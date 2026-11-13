# -----------------------------------------------------------------------------
# PLSsemLab
# A jamovi module for partial least squares structural equation modeling and
# evidence-based methodological decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of PLSsemLab, licensed under the GNU General Public
# License, version 3 or later. See https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Higher-order constructs (phase 4): the disjoint two-stage approach of
# Sarstedt et al. (2019) and Hair et al. (2021), estimated in two plain cSEM
# calls so that the second stage keeps the user's choice of Mode A
# (reflective higher-order construct) or Mode B (formative). cSEM's own
# second-order routine always estimates its second stage with
# `.PLS_modes = NULL` and forces `.disattenuate = TRUE` (cSEM 0.7.1,
# calculate2ndStage), so a reflective higher-order construct would come out
# as a common factor corrected by consistent PLS even when PLS was chosen.
#
#   Stage 1: every first-order construct, the higher-order construct
#            replaced in the structural model by its lower-order constructs
#            (each of them takes the higher-order construct's paths).
#   Stage 2: the scores of stage 1 become indicators: the lower-order
#            scores measure the higher-order construct; every other
#            construct keeps a single indicator, its own score.
#
# Internal names are shared by both stages (C1..Ck first-order, H1..Hm
# higher-order, S_Cx for a stage-1 score used as an indicator), so every
# lookup by name works on either fit.
# ES: Constructos de orden superior (fase 4): el enfoque disjunto en dos
# etapas de Sarstedt et al. (2019) y Hair et al. (2021), estimado con dos
# llamadas simples a cSEM para que la segunda etapa respete el Modo A
# (orden superior reflectivo) o el Modo B (formativo). La rutina de segundo
# orden de cSEM estima siempre su segunda etapa con `.PLS_modes = NULL` y
# fuerza `.disattenuate = TRUE`, así que un orden superior reflectivo saldría
# como factor común corregido por PLS consistente aunque se eligiera PLS.
# Etapa 1: todos los constructos de primer orden, con el de orden superior
# reemplazado en el modelo estructural por sus constructos de orden inferior.
# Etapa 2: las puntuaciones de la etapa 1 son los indicadores.
# -----------------------------------------------------------------------------

# Input: options. Output: list of list(label, components, type), components
#   being first-order construct names (reflective higher-order first).
# ES: Salida: lista de constructos de orden superior con sus componentes.
.pl_hocs <- function(options) {
    out <- list()
    for (type in c("reflective", "formative")) {
        for (item in options[[paste0(type, "Hoc")]]) {
            label <- trimws(if (is.null(item$label)) "" else as.character(item$label))
            comps <- unique(as.character(unlist(item$components)))
            if (!nzchar(label) && !length(comps)) next
            out[[length(out) + 1]] <- list(label = label, components = comps, type = type)
        }
    }
    out
}

# Category A checks of the higher-order part. Returns messages.
# ES: Comprobaciones de Categoría A de la parte de orden superior.
.pl_hoc_validate <- function(blocks, hocs, paths, options, lang) {
    if (!length(hocs)) return(character(0))
    txt <- function(key) .pl_text(lang, "errors", key)
    first <- vapply(blocks, `[[`, "", "label")
    errors <- character(0)
    hl <- vapply(hocs, `[[`, "", "label")
    if (any(!nzchar(hl))) errors <- c(errors, txt("hoc_no_name"))
    clash <- intersect(hl, first)
    errors <- c(errors, vapply(clash, function(x) sprintf(txt("hoc_clash"), x), ""))
    for (h in hocs) {
        if (length(h$components) < 2)
            errors <- c(errors, sprintf(txt("hoc_two"), h$label))
        bad <- setdiff(h$components, first)
        errors <- c(errors, vapply(bad, function(x) sprintf(txt("hoc_unknown"), x, h$label), ""))
    }
    comps <- unlist(lapply(hocs, `[[`, "components"))
    twice <- unique(comps[duplicated(comps)])
    errors <- c(errors, vapply(twice, function(x) sprintf(txt("hoc_twice"), x), ""))
    if (options$estimator == "plsc")
        errors <- c(errors, txt("hoc_plsc"))
    if (length(errors)) return(unname(errors))
    # paths name the higher-order construct, never its dimensions
    # ES: las rutas nombran al constructo de orden superior, no a sus
    # dimensiones
    parts <- unique(unlist(strsplit(c(paths$from, paths$to), .PL_X, fixed = TRUE)))
    for (h in hocs) for (cpt in intersect(h$components, parts))
        errors <- c(errors, sprintf(txt("hoc_path_loc"), cpt, h$label, h$label))
    inter <- paths$from[grepl(.PL_X, paths$from, fixed = TRUE)]
    hit <- unique(unlist(strsplit(inter, .PL_X, fixed = TRUE)))
    errors <- c(errors, vapply(intersect(hit, hl), function(x) sprintf(txt("hoc_interaction"), x), ""))
    unname(errors)
}

# Stage-1 paths: each path from or to a higher-order construct is repeated
# for each of its lower-order constructs.
# ES: Rutas de la etapa 1: cada ruta desde o hacia un constructo de orden
# superior se repite para cada uno de sus constructos de orden inferior.
.pl_stage1_paths <- function(paths, hocs) {
    comps <- stats::setNames(lapply(hocs, `[[`, "components"), vapply(hocs, `[[`, "", "label"))
    expand <- function(x) if (x %in% names(comps)) comps[[x]] else x
    rows <- list()
    for (k in seq_len(nrow(paths)))
        for (f in expand(paths$from[k])) for (t in expand(paths$to[k]))
            rows[[length(rows) + 1]] <- data.frame(from = f, to = t, stringsAsFactors = FALSE)
    unique(do.call(rbind, rows))
}

# Stage-2 model. Two approaches (option hocApproach):
#   disjoint - the disjoint two-stage approach: indicators S_Cx (stage-1
#              scores), the higher-order construct in Mode A (reflective) or
#              Mode B (formative), no disattenuation;
#   csem     - cSEM's own second-order routine reproduced with a plain call:
#              indicators named as the stage-1 scores (Cx), the other
#              constructs renamed Cx_temp, a reflective higher-order
#              construct as a common factor (=~), .PLS_modes = NULL and
#              .disattenuate = TRUE, as calculate2ndStage() does (cSEM 0.7.1;
#              with composite first-order constructs its reliability
#              adjustments are 1, so the plain call gives the same values,
#              test-hoc.R).
# Returns list(syntax, modes, disattenuate, hmap, cmap2, ind(x), prefix,
#   readable): cmap2 maps every construct of the structural model to its
#   stage-2 name, ind(x) gives the stage-2 indicator of a lower-order
#   construct x.
# ES: Modelo de la etapa 2, con dos enfoques: disjunto (indicadores S_Cx,
# Modo A o B, sin desatenuación) o la rutina de cSEM reproducida con una
# llamada simple (constructos Cx_temp, orden superior reflectivo como factor
# común, .PLS_modes = NULL y .disattenuate = TRUE).
.pl_stage2_model <- function(hocs, paths, cmap, approach = "disjoint") {
    csem_like <- identical(approach, "csem")
    hmap <- stats::setNames(paste0("H", seq_along(hocs)), vapply(hocs, `[[`, "", "label"))
    prefix <- if (csem_like) "" else "S_"
    ind <- function(x) paste0(prefix, cmap[[x]])
    others <- setdiff(unique(c(paths$from, paths$to)), names(hmap))
    cmap2 <- c(stats::setNames(if (csem_like) paste0(cmap[others], "_temp") else unname(cmap[others]), others), hmap)
    lines <- character(0)
    readable <- character(0)
    modes <- list()
    for (h in hocs) {
        op <- if (csem_like && h$type == "reflective") "=~" else "<~"
        lines <- c(lines, sprintf("%s %s %s", hmap[[h$label]], op, paste(vapply(h$components, ind, ""), collapse = " + ")))
        readable <- c(readable, sprintf("%s %s %s", h$label, op, paste(h$components, collapse = " + ")))
        modes[[hmap[[h$label]]]] <- if (h$type == "reflective") "modeA" else "modeB"
    }
    for (x in others) {
        lines <- c(lines, sprintf("%s <~ %s", cmap2[[x]], ind(x)))
        modes[[cmap2[[x]]]] <- "modeA"
    }
    for (to in unique(paths$to)) {
        from <- paths$from[paths$to == to]
        lines <- c(lines, sprintf("%s ~ %s", cmap2[[to]], paste(cmap2[from], collapse = " + ")))
        readable <- c(readable, sprintf("%s ~ %s", to, paste(from, collapse = " + ")))
    }
    list(syntax = paste(lines, collapse = "\n"), modes = if (csem_like) NULL else modes,
         disattenuate = csem_like, hmap = hmap, cmap2 = cmap2, ind = ind, prefix = prefix,
         approach = approach, readable = readable)
}

# Stage-2 data from stage-1 construct scores.
# ES: Datos de la etapa 2 a partir de las puntuaciones de la etapa 1.
.pl_stage2_data <- function(scores, m2) {
    d <- as.data.frame(scores)
    names(d) <- paste0(m2$prefix, colnames(scores))
    d
}

.pl_stage2_fit <- function(scores, m2, options) {
    suppressWarnings(cSEM::csem(.data = .pl_stage2_data(scores, m2), .model = m2$syntax, .PLS_modes = m2$modes,
                                .disattenuate = m2$disattenuate, .PLS_weight_scheme_inner = options$innerScheme))
}

# cSEM's second-order routine (hocApproach = "csem"): the model in cSEM's
# syntax, estimated once to obtain cSEM's stage 1 (where the lower-order
# constructs also get paths among themselves); stage 2 then follows
# .pl_stage2_model(). Returns list(fit1, paths1) or list(error).
# ES: La rutina de segundo orden de cSEM: el modelo en la sintaxis de cSEM,
# estimado una vez para obtener su etapa 1 (donde los constructos de orden
# inferior también tienen rutas entre sí).
.pl_csem_stage1 <- function(d, blocks, hocs, paths, model, options) {
    hmap <- stats::setNames(paste0("H", seq_along(hocs)), vapply(hocs, `[[`, "", "label"))
    all <- c(model$cmap, hmap)
    lines <- strsplit(model$syntax, "\n", fixed = TRUE)[[1]]
    lines <- lines[!grepl(" ~ ", lines, fixed = TRUE)]
    for (h in hocs)
        lines <- c(lines, sprintf("%s %s %s", hmap[[h$label]], if (h$type == "reflective") "=~" else "<~",
                                  paste(unname(model$cmap[h$components]), collapse = " + ")))
    for (to in unique(paths$to))
        lines <- c(lines, sprintf("%s ~ %s", all[[to]], paste(all[paths$from[paths$to == to]], collapse = " + ")))
    syntax <- paste(lines, collapse = "\n")
    f <- suppressWarnings(cSEM::csem(.data = d, .model = syntax,
                                     .PLS_modes = if (length(model$modes)) model$modes else NULL,
                                     .disattenuate = FALSE, .PLS_weight_scheme_inner = options$innerScheme))
    fit1 <- f$First_stage
    S <- fit1$Information$Model$structural
    inv <- stats::setNames(names(model$cmap), model$cmap)
    idx <- which(S == 1, arr.ind = TRUE)
    paths1 <- data.frame(from = unname(inv[colnames(S)[idx[, 2]]]), to = unname(inv[rownames(S)[idx[, 1]]]),
                         stringsAsFactors = FALSE)
    list(fit1 = fit1, paths1 = paths1, syntax = syntax, second = f$Second_stage)
}

# Measurement of the higher-order constructs in stage 2: loadings and
# weights of the lower-order scores, reliability and AVE of the reflective
# ones, VIF of the formative ones (diagonal of the inverse correlation of
# the lower-order scores).
# ES: Medida de los constructos de orden superior en la etapa 2.
.pl_hoc_quality <- function(fit2, hocs, m2) {
    hmap <- m2$hmap
    est <- fit2$Estimates
    rows <- list()
    rel <- list()
    for (h in hocs) {
        hh <- hmap[[h$label]]
        ids <- vapply(h$components, m2$ind, "")
        l <- est$Loading_estimates[hh, ids]
        w <- est$Weight_estimates[hh, ids]
        vif <- if (h$type == "formative") {
            S <- est$Indicator_VCV[ids, ids, drop = FALSE]
            tryCatch(diag(solve(S)), error = function(e) rep(NA_real_, length(ids)))
        } else rep(NA_real_, length(ids))
        rows[[length(rows) + 1]] <- data.frame(hoc = h$label, type = h$type, component = h$components,
                                               loading = unname(l), weight = unname(w), vif = unname(vif),
                                               stringsAsFactors = FALSE)
        if (h$type == "reflective") {
            rhoC <- sum(l)^2 / (sum(l)^2 + sum(1 - l^2))
            ave <- mean(l^2)
            S <- est$Indicator_VCV[ids, ids, drop = FALSE]
            k <- length(ids)
            alpha <- k / (k - 1) * (1 - k / sum(S))
            rel[[length(rel) + 1]] <- data.frame(hoc = h$label, alpha = alpha, rhoC = rhoC, ave = ave,
                                                 stringsAsFactors = FALSE)
        }
    }
    list(measurement = do.call(rbind, rows), reliability = if (length(rel)) do.call(rbind, rel) else NULL)
}
