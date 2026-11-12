# -----------------------------------------------------------------------------
# PLSsemLab
# A jamovi module for partial least squares structural equation modeling and
# evidence-based methodological decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of PLSsemLab.
#
# PLSsemLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# PLSsemLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PLSsemLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Shared helpers.
# ES: Funciones auxiliares compartidas.
#
# Description
#   Every small function used by more than one part of the module, with the
#   `.pl_` prefix (CODE_STYLE.md): translation, HTML blocks, table rows,
#   cSEM error capture, jamovi notices, and the core of the module: reading
#   the construct boxes, validating the model, preparing the indicators and
#   building the cSEM syntax with internal names.
#
# Responsibilities
#   - One place for each rule, so a fix is never made in one copy only.
#   - Every message the user reads comes from R/texts.R.
#
# ES: Descripción
#   Cada función pequeña que usa más de una parte del módulo, con el prefijo
#   `.pl_`: traducción, bloques HTML, filas de tablas, captura de errores de
#   cSEM, avisos de jamovi, y el núcleo del módulo: leer las cajas de
#   constructos, validar el modelo, preparar los indicadores y armar la
#   sintaxis de cSEM con nombres internos.
#
# ES: Responsabilidades
#   - Un solo lugar para cada regla, para que un arreglo nunca quede en una
#     sola copia.
#   - Cada mensaje que lee el usuario sale de R/texts.R.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .pl_lang()
#
# Input: options - self$options of the running analysis.
# Output: an environment with the translator of this run (`tr`) and the text
#   table, built on first use. jamovi sends its language with every run, so
#   a change of language reaches the next run (ARCHITECTURE.md §12).
# ES: Salida: un entorno con el traductor de esta ejecución (`tr`) y la tabla
#   de textos, que se arma al primer uso.
# -----------------------------------------------------------------------------
# Separator of the two constructs of an interaction term, used as is in
# every table and text ("A × B"); cmap maps the term to cSEM's C1.C2.
# ES: Separador de los dos constructos de una interacción, tal cual en tablas
# y textos ("A × B"); cmap lleva el término al C1.C2 de cSEM.
.PL_X <- " \u00d7 "

.pl_lang <- function(options) {
    lang <- new.env(parent = emptyenv())
    lang$tr <- function(text) {
        if (is.null(text) || length(text) != 1 || is.na(text) || !nzchar(text))
            return(text)
        tryCatch(options$translate(text), error = function(e) text)
    }
    lang$texts <- NULL
    lang
}

# -----------------------------------------------------------------------------
# .pl_html_escape() / .pl_html_block() / .pl_html_list()
#
# Small HTML builders for the Html results.
# ES: Constructores HTML pequeños para los resultados Html.
# -----------------------------------------------------------------------------
.pl_html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
}

.pl_html_block <- function(paragraphs, title = NULL) {
    body <- paste0("<p>", .pl_html_escape(paragraphs), "</p>", collapse = "")
    if (!is.null(title))
        body <- paste0("<h3>", .pl_html_escape(title), "</h3>", body)
    paste0("<div style=\"text-align: justify;\">", body, "</div>")
}

.pl_html_list <- function(items, ordered = FALSE) {
    tag <- if (ordered) "ol" else "ul"
    paste0("<", tag, ">", paste0("<li>", .pl_html_escape(items), "</li>", collapse = ""), "</", tag, ">")
}

# -----------------------------------------------------------------------------
# .pl_capture()
#
# Input: expr - a cSEM call.
# Output: list(value, error, warnings): errors are caught, warnings collected
#   (unique, whitespace-normalised) and console output discarded, so cSEM
#   messages become jamovi notices and never a raw R error.
# ES: Salida: list(value, error, warnings): los errores se atrapan, las
#   advertencias se recogen y la salida de consola se descarta, para que los
#   mensajes de cSEM sean avisos de jamovi y nunca un error crudo de R.
# -----------------------------------------------------------------------------
.pl_capture <- function(expr) {
    warnings <- character(0)
    value <- NULL
    utils::capture.output(
        value <- withCallingHandlers(
            tryCatch(expr, error = function(e) structure(list(message = conditionMessage(e)), class = "pl_error")),
            warning = function(w) {
                warnings <<- c(warnings, trimws(gsub("[[:space:]]+", " ", conditionMessage(w))))
                invokeRestart("muffleWarning")
            }
        )
    )
    if (inherits(value, "pl_error"))
        return(list(value = NULL, error = trimws(gsub("[[:space:]]+", " ", value$message)),
                    warnings = unique(warnings)))
    list(value = value, error = NULL, warnings = unique(warnings))
}

# -----------------------------------------------------------------------------
# .pl_insert_notices()
#
# Input: results - the analysis' results group; options; messages - text;
#   type - a jmvcore::NoticeType.
# Output: one jmvcore Notice per message at the top of the results, where
#   jamovi shows warnings in every analysis (jamovi's audit of MokkenLab).
# ES: Salida: un Notice de jmvcore por mensaje, arriba de los resultados.
# -----------------------------------------------------------------------------
.pl_insert_notices <- function(results, options, messages, type = jmvcore::NoticeType$WARNING,
                               prefix = ".plssemNote") {
    messages <- unique(trimws(messages[nzchar(trimws(messages))]))
    for (k in rev(seq_along(messages)))
        results$insert(1, jmvcore::Notice$new(options, name = paste0(prefix, k),
                                              type = type, content = messages[k]))
    invisible(NULL)
}

# -----------------------------------------------------------------------------
# .pl_constructs()
#
# Input: options - with `reflective` and `formative` (lists of
#   list(label, vars)).
# Output: list(blocks, empty): blocks is one list(label, vars, type) per
#   construct with at least one indicator, reflective first; empty holds the
#   names of constructs with no indicators yet (left out, with a notice;
#   the box always starts with an empty "Construct 1").
# ES: Salida: list(blocks, empty): blocks tiene un list(label, vars, type)
#   por constructo con al menos un indicador, reflectivos primero; empty
#   guarda los nombres de los constructos aún sin indicadores (se omiten con
#   un aviso; la caja siempre empieza con un "Construct 1" vacío).
# -----------------------------------------------------------------------------
.pl_constructs <- function(options) {
    blocks <- list()
    empty <- character(0)
    for (type in c("reflective", "formative")) {
        for (item in options[[type]]) {
            label <- trimws(if (is.null(item$label)) "" else as.character(item$label))
            vars <- unlist(item$vars)
            if (length(vars) == 0) {
                if (nzchar(label)) empty <- c(empty, label)
                next
            }
            blocks[[length(blocks) + 1]] <- list(label = label, vars = as.character(vars), type = type)
        }
    }
    list(blocks = blocks, empty = empty)
}

# -----------------------------------------------------------------------------
# .pl_paths()
#
# Input: structural - list of Terms, one row per construct, in the order of
#   .pl_structural_labels() (the rule of constructNames() in
#   jamovi/js/plssem.events.js: a name and at least one indicator, reflective
#   first, no repeated names); names - those labels.
# Output: data.frame(from, to) with one row per predictor term, interaction
#   terms joined by " × " (rows beyond the constructs are ignored: the panel
#   removes them on its next update).
# ES: Entrada: structural - lista de Terms, una fila por constructo, en el
#   orden de .pl_structural_labels() (la misma regla que constructNames() en
#   el JS). Salida: data.frame(from, to) con una fila por término predictor,
#   las interacciones unidas por " × ".
# -----------------------------------------------------------------------------
.pl_structural_labels <- function(blocks, hocs = list()) {
    labels <- c(vapply(blocks, `[[`, "", "label"),
                vapply(Filter(function(h) length(h$components) > 0, hocs), `[[`, "", "label"))
    unique(labels[nzchar(labels)])
}

.pl_paths <- function(structural, names) {
    rows <- list()
    for (i in seq_along(structural)) {
        if (i > length(names))
            break
        for (term in structural[[i]]) {
            term <- as.character(unlist(term))
            if (length(term) == 0)
                next
            rows[[length(rows) + 1]] <- data.frame(from = paste(term, collapse = .PL_X), to = names[i],
                                                   stringsAsFactors = FALSE)
        }
    }
    if (length(rows) == 0)
        return(data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE))
    unique(do.call(rbind, rows))
}

# -----------------------------------------------------------------------------
# .pl_find_loop()
#
# Input: paths - data.frame(from, to).
# Output: NULL, or the constructs of one loop in order (A, B, ..., A).
#   PLS-PM with OLS paths needs a recursive model (experiments, E4: cSEM
#   alone says only "Reordering the structural equations was not
#   succesful").
# ES: Salida: NULL, o los constructos de un ciclo en orden (A, B, ..., A).
# -----------------------------------------------------------------------------
.pl_find_loop <- function(paths) {
    adj <- split(paths$to, paths$from)
    state <- list()
    stack <- character(0)
    found <- NULL
    visit <- function(node) {
        if (!is.null(found)) return(invisible())
        state[[node]] <<- 1
        stack <<- c(stack, node)
        for (nxt in adj[[node]]) {
            if (identical(state[[nxt]], 1)) {
                found <<- c(stack[match(nxt, stack):length(stack)], nxt)
                return(invisible())
            }
            if (is.null(state[[nxt]])) visit(nxt)
            if (!is.null(found)) return(invisible())
        }
        stack <<- stack[-length(stack)]
        state[[node]] <<- 2
    }
    for (node in unique(paths$from))
        if (is.null(state[[node]]) && is.null(found)) visit(node)
    found
}

# -----------------------------------------------------------------------------
# .pl_validate_model()
#
# Input: blocks (from .pl_constructs()); paths (from .pl_paths()); lang.
# Output: character vector of Category A messages (empty when the model is
#   valid). Each names the construct or indicator at fault (D5 in
#   docs/DESIGN.md; the seven cases of experiment E4).
# ES: Salida: vector de mensajes de Categoría A (vacío si el modelo es
#   válido). Cada uno nombra el constructo o indicador con problemas.
# -----------------------------------------------------------------------------
.pl_validate_model <- function(blocks, paths, lang) {
    txt <- function(key) .pl_text(lang, "errors", key)
    errors <- character(0)
    labels <- vapply(blocks, `[[`, "", "label")
    if (any(!nzchar(labels)))
        errors <- c(errors, txt("no_name"))
    dup <- unique(labels[duplicated(labels) & nzchar(labels)])
    errors <- c(errors, vapply(dup, function(x) sprintf(txt("duplicated"), x), ""))
    used <- unlist(lapply(blocks, `[[`, "vars"))
    twice <- unique(used[duplicated(used)])
    errors <- c(errors, vapply(twice, function(x) sprintf(txt("twice"), x), ""))
    if (length(errors))
        return(unname(errors))

    # Interactions (phase 4): two-way terms of constructs, with both main
    # effects in the same equation (a product term without its components
    # is not interpretable as moderation) and neither component being the
    # outcome itself.
    # ES: Interacciones (fase 4): términos de dos vías entre constructos, con
    # ambos efectos principales en la misma ecuación y sin que un componente
    # sea el propio resultado.
    inter <- paths[grepl(.PL_X, paths$from, fixed = TRUE), , drop = FALSE]
    for (k in seq_len(nrow(inter))) {
        parts <- strsplit(inter$from[k], .PL_X, fixed = TRUE)[[1]]
        shown <- inter$from[k]
        if (length(parts) != 2) {
            errors <- c(errors, sprintf(txt("interaction_order"), shown))
            next
        }
        bad <- setdiff(parts, labels)
        if (length(bad)) {
            errors <- c(errors, vapply(bad, function(x) sprintf(txt("unknown"), x), ""))
            next
        }
        if (inter$to[k] %in% parts)
            errors <- c(errors, sprintf(txt("interaction_self"), shown, inter$to[k]))
        missing_main <- setdiff(parts, paths$from[paths$to == inter$to[k]])
        if (length(missing_main))
            errors <- c(errors, sprintf(txt("interaction_main"), shown, inter$to[k],
                                        paste(missing_main, collapse = ", "), inter$to[k]))
    }
    simple <- paths[!grepl(.PL_X, paths$from, fixed = TRUE), , drop = FALSE]
    unknown <- setdiff(unique(c(simple$from, simple$to)), labels)
    errors <- c(errors, vapply(unknown, function(x) sprintf(txt("unknown"), x), ""))
    self <- unique(simple$from[simple$from == simple$to])
    errors <- c(errors, vapply(self, function(x) sprintf(txt("self"), x), ""))
    if (length(errors))
        return(unname(errors))

    loop <- .pl_find_loop(simple)
    if (!is.null(loop))
        errors <- c(errors, sprintf(txt("loop"), paste(loop, collapse = " \u2192 ")))
    isolated <- setdiff(labels, c(simple$from, simple$to))
    errors <- c(errors, vapply(isolated, function(x) sprintf(txt("isolated"), x), ""))
    unname(errors)
}

# -----------------------------------------------------------------------------
# .pl_sentinel()
#
# Input: x - a numeric indicator.
# Output: NULL, or list(code, count, low, high) for the first value among the
#   usual missing-data codes (-99, -999, -9, 99, 999, 9999) that lies beyond
#   the range of the other values by more than that range's width. Experiment
#   E1: the reference data code missing answers as -99, and estimated as is
#   one construct's alpha fell from 0.831 to 0.421.
# ES: Salida: NULL, o list(code, count, low, high) para el primer valor entre
#   los códigos habituales de perdidos que queda fuera del rango de los demás
#   valores por más que el ancho de ese rango (E1: alfa 0,831 -> 0,421).
# -----------------------------------------------------------------------------
.pl_sentinel <- function(x) {
    x <- x[is.finite(x)]
    for (code in c(-99, -999, -9, 99, 999, 9999)) {
        hit <- x == code
        if (!any(hit) || all(hit))
            next
        other <- x[!hit]
        low <- min(other); high <- max(other)
        width <- max(high - low, 1)
        if (code < low - width || code > high + width)
            return(list(code = code, count = sum(hit), low = low, high = high))
    }
    NULL
}

# -----------------------------------------------------------------------------
# .pl_prepare_indicators()
#
# Input: data - self$data; vars - indicator names; missing - "listwise" or
#   "mean"; lang.
# Output: list(data, errors, n_total, n_used, n_dropped, n_replaced,
#   n_cases_replaced): the numeric indicators after the missing-data rule,
#   and Category A messages for non-numeric, constant or sentinel-coded
#   indicators, or too few complete cases.
# ES: Salida: los indicadores numéricos tras la regla de perdidos, y
#   mensajes de Categoría A para indicadores no numéricos, constantes o con
#   códigos centinela, o muy pocos casos completos.
# -----------------------------------------------------------------------------
.pl_prepare_indicators <- function(data, vars, missing, lang) {
    txt <- function(key) .pl_text(lang, "errors", key)
    errors <- character(0)
    out <- data.frame(row.names = seq_len(nrow(data)))
    for (v in vars) {
        x <- data[[v]]
        if (is.factor(x)) x <- jmvcore::toNumeric(x)
        if (!is.numeric(x)) {
            errors <- c(errors, sprintf(txt("not_numeric"), v))
            next
        }
        x <- as.numeric(x)
        obs <- x[is.finite(x)]
        if (length(unique(obs)) <= 1) {
            errors <- c(errors, sprintf(txt("constant"), v))
            next
        }
        s <- .pl_sentinel(x)
        if (!is.null(s))
            errors <- c(errors, sprintf(txt("sentinel"), v, s$count, format(s$code),
                                        format(s$low), format(s$high)))
        out[[v]] <- x
    }
    res <- list(data = NULL, errors = errors, n_total = nrow(data), n_used = 0L,
                n_dropped = 0L, n_replaced = 0L, n_cases_replaced = 0L)
    if (length(errors))
        return(res)

    incomplete <- !stats::complete.cases(out)
    if (missing == "mean") {
        res$n_replaced <- sum(is.na(out))
        res$n_cases_replaced <- sum(incomplete)
        for (v in names(out))
            out[[v]][is.na(out[[v]])] <- mean(out[[v]], na.rm = TRUE)
    } else {
        res$n_dropped <- sum(incomplete)
        out <- out[!incomplete, , drop = FALSE]
    }
    res$n_used <- nrow(out)
    if (res$n_used < 10)
        res$errors <- sprintf(txt("few_cases"), res$n_used)
    res$data <- out
    res
}

# -----------------------------------------------------------------------------
# .pl_build_syntax()
#
# Input: blocks; paths (simple paths only); estimator - "pls" or "plsc".
# Output: list(syntax, modes, cmap, vmap, readable): the cSEM model with
#   internal names C1..Ck and V1..Vp (cSEM's parser drops backticks, so a
#   variable name with a space always fails and an accented construct name
#   fails in a non-UTF-8 locale: experiment E4), the PLS modes, the maps back
#   to the user's names, and the readable model for the report.
#   With PLS, reflective constructs are Mode A composites (<~): the same
#   estimates as =~, and the only form IPMA and MICOM accept (E6). With
#   PLSc, reflective constructs are common factors (=~).
# ES: Salida: el modelo de cSEM con nombres internos, los modos de PLS, los
#   mapas de vuelta a los nombres del usuario y el modelo legible. Con PLS,
#   los reflectivos son compuestos de Modo A (<~): mismas estimaciones que
#   =~ y la única forma que aceptan IPMA y MICOM (E6). Con PLSc, son factores
#   comunes (=~).
# -----------------------------------------------------------------------------
.pl_build_syntax <- function(blocks, paths, estimator) {
    labels <- vapply(blocks, `[[`, "", "label")
    vars <- unlist(lapply(blocks, `[[`, "vars"))
    cmap <- stats::setNames(paste0("C", seq_along(labels)), labels)
    vmap <- stats::setNames(paste0("V", seq_along(vars)), vars)
    modes <- list()
    lines <- character(0)
    readable <- character(0)
    for (b in blocks) {
        op <- if (b$type == "formative" || estimator == "pls") "<~" else "=~"
        lines <- c(lines, sprintf("%s %s %s", cmap[[b$label]], op, paste(vmap[b$vars], collapse = " + ")))
        readable <- c(readable, sprintf("%s %s %s", b$label, op, paste(b$vars, collapse = " + ")))
        if (op == "<~")
            modes[[cmap[[b$label]]]] <- if (b$type == "formative") "modeB" else "modeA"
    }
    # An interaction term A:B is the product C1.C2 in cSEM's syntax; it is
    # added to cmap so that every lookup by term works the same way.
    # ES: Un término A:B es el producto C1.C2 en la sintaxis de cSEM; se
    # añade a cmap para que toda búsqueda por término funcione igual.
    for (term in unique(paths$from[grepl(.PL_X, paths$from, fixed = TRUE)]))
        cmap[[term]] <- paste(cmap[strsplit(term, .PL_X, fixed = TRUE)[[1]]], collapse = ".")
    for (to in unique(paths$to)) {
        from <- paths$from[paths$to == to]
        lines <- c(lines, sprintf("%s ~ %s", cmap[[to]], paste(cmap[from], collapse = " + ")))
        readable <- c(readable, sprintf("%s ~ %s", to, paste(gsub(.PL_X, ".", from, fixed = TRUE), collapse = " + ")))
    }
    list(syntax = paste(lines, collapse = "\n"), modes = modes, cmap = cmap, vmap = vmap,
         readable = readable, nonlinear = any(grepl(.PL_X, paths$from, fixed = TRUE)))
}

# -----------------------------------------------------------------------------
# .pl_fmt()
#
# Input: x - a number. Output: text with 3 decimals ("" for NA), for the
#   narrative; table cells stay numeric so they follow jamovi's settings.
# ES: Salida: texto con 3 decimales, para la narrativa; las celdas de las
#   tablas siguen siendo numéricas.
# -----------------------------------------------------------------------------
# Short label of an indicator for running text: the item code when the name
# starts with one ("E02.- Actualmente..." -> "E02"), otherwise the name cut
# at 30 characters. Tables always keep the full name.
# ES: Etiqueta corta de un indicador para el texto corrido: el código del ítem
# si el nombre empieza con uno, si no el nombre cortado a 30 caracteres. Las
# tablas conservan siempre el nombre completo.
.pl_short <- function(x) {
    vapply(x, function(v) {
        code <- regmatches(v, regexpr("^[[:alnum:]_]{1,12}(?=[.:)\\-]\\s|\\s*[.:)\\-]{1,2}\\s)", v, perl = TRUE))
        if (length(code) == 1 && nzchar(code) && nchar(v) > nchar(code) + 3) return(code)
        if (nchar(v) > 30) paste0(substr(v, 1, 29), "\u2026") else v
    }, "", USE.NAMES = FALSE)
}

.pl_fmt <- function(x) {
    if (length(x) == 0 || is.na(x)) return("")
    formatC(x, format = "f", digits = 3)
}
