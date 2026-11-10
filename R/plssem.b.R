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
# PLS-SEM analysis.
# ES: Análisis PLS-SEM.
#
# Description
#   Phase 1 of docs/DESIGN.md §8: the model is read from the construct boxes
#   and the structural rows, validated (Category A messages that name the
#   construct or indicator at fault), estimated with cSEM, and reported as
#   model summary, loadings and weights, path coefficients and explained
#   variance, with guides, interpretation, key findings and the equivalent
#   R code.
#
# Workflow
#   1. Read the boxes; while the model is incomplete, show the introduction
#   2. Validate the model and prepare the indicators (Category A)
#   3. Build the cSEM syntax with internal names and estimate
#   4. Tables (rows laid out in .init(), filled here)
#   5. Notices, interpretation, key findings, choices, reproducibility
#
# ES: Descripción
#   Fase 1 de docs/DESIGN.md §8: el modelo se lee de las cajas de
#   constructos y de las filas estructurales, se valida (mensajes de
#   Categoría A que nombran el constructo o indicador con problemas), se
#   estima con cSEM y se informa como resumen del modelo, cargas y pesos,
#   coeficientes de ruta y varianza explicada, con guías, interpretación,
#   hallazgos clave y el código R equivalente.
#
# ES: Flujo
#   1. Leer las cajas; mientras el modelo esté incompleto, mostrar la
#      introducción
#   2. Validar el modelo y preparar los indicadores (Categoría A)
#   3. Armar la sintaxis de cSEM con nombres internos y estimar
#   4. Tablas (filas definidas en .init(), llenadas aquí)
#   5. Avisos, interpretación, hallazgos clave, elecciones, reproducibilidad
# -----------------------------------------------------------------------------

plssemClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "plssemClass",
    inherit = plssemBase,
    private = list(

        # ---------------------------------------------------------------------
        # Lay out the rows that depend only on the options, so every table
        # keeps its shape before the results arrive (jamovi's review of
        # MokkenLab).
        # ES: Definir las filas que dependen solo de las opciones, para que
        # cada tabla conserve su forma antes de los resultados.
        # ---------------------------------------------------------------------
        .init = function() {
            blocks <- .pl_constructs(self$options)$blocks
            labels <- vapply(blocks, `[[`, "", "label")
            paths <- .pl_paths(self$options$structural, labels)
            for (i in seq_along(blocks)) {
                self$results$model$addRow(rowKey = i, values = list(construct = blocks[[i]]$label))
                for (v in blocks[[i]]$vars)
                    self$results$measurement$addRow(rowKey = paste(i, v, sep = "::"),
                                                    values = list(construct = blocks[[i]]$label, indicator = v))
            }
            for (k in seq_len(nrow(paths)))
                self$results$paths$addRow(rowKey = k, values = list(from = paths$from[k], arrow = "\u2192",
                                                                    to = paths$to[k]))
            for (to in unique(paths$to))
                self$results$r2$addRow(rowKey = to, values = list(construct = to))
        },

        .run = function() {
            lang <- .pl_lang(self$options)
            txt <- function(section, key) .pl_text(lang, section, key)

            # 1. Read the boxes.
            # ES: 1. Leer las cajas.
            cons <- .pl_constructs(self$options)
            blocks <- cons$blocks
            labels <- vapply(blocks, `[[`, "", "label")
            paths <- .pl_paths(self$options$structural, labels)
            if (length(blocks) < 2 || nrow(paths) == 0) {
                private$.showIntro(lang, if (length(blocks) < 2) "need_constructs" else "need_paths")
                return()
            }
            self$results$intro$setVisible(FALSE)

            # 2. Validate and prepare (Category A).
            # ES: 2. Validar y preparar (Categoría A).
            errors <- .pl_validate_model(blocks, paths, lang)
            if (length(errors))
                jmvcore::reject(paste(errors, collapse = "\n"))
            vars <- unlist(lapply(blocks, `[[`, "vars"))
            prep <- .pl_prepare_indicators(self$data, vars, self$options$missing, lang)
            if (length(prep$errors))
                jmvcore::reject(paste(prep$errors, collapse = "\n"))

            # 3. Build and estimate.
            # ES: 3. Armar y estimar.
            model <- .pl_build_syntax(blocks, paths, self$options$estimator)
            d <- prep$data[, names(model$vmap), drop = FALSE]
            names(d) <- unname(model$vmap)
            fit <- .pl_capture(cSEM::csem(
                .data = d, .model = model$syntax,
                .PLS_modes = if (length(model$modes)) model$modes else NULL,
                .disattenuate = self$options$estimator == "plsc",
                .PLS_weight_scheme_inner = self$options$innerScheme))
            if (!is.null(fit$error))
                jmvcore::reject(sprintf(txt("errors", "estimation"), fit$error))
            est <- fit$value$Estimates
            admissible <- !any(unlist(cSEM::verify(fit$value)))

            # 4. Tables.
            # ES: 4. Tablas.
            inv <- stats::setNames(names(model$cmap), model$cmap)
            private$.fillModel(blocks, paths, txt)
            loadings <- private$.fillMeasurement(blocks, model, est)
            path_tab <- private$.fillPaths(paths, model, est)
            r2_tab <- private$.fillR2(paths, model, est)

            # 5. Notices, interpretation, summaries, reproducibility.
            # ES: 5. Avisos, interpretación, resúmenes, reproducibilidad.
            warn <- c(vapply(cons$empty, function(x) sprintf(txt("notices", "empty_construct"), x), ""),
                      unlist(lapply(blocks, function(b)
                          if (b$type == "reflective" && length(b$vars) == 1)
                              sprintf(txt("notices", "single_reflective"), b$label))),
                      fit$warnings)
            if (!admissible) {
                failed <- names(which(unlist(cSEM::verify(fit$value))))
                warn <- c(warn, sprintf(txt("notices", "inadmissible"), paste(failed, collapse = ", ")))
            }
            info <- character(0)
            if (prep$n_dropped > 0)
                info <- c(info, sprintf(txt("notices", "dropped"), prep$n_dropped, prep$n_used))
            if (prep$n_replaced > 0)
                info <- c(info, sprintf(txt("notices", "replaced"), prep$n_replaced, prep$n_cases_replaced))
            .pl_insert_notices(self$results, self$options, info, jmvcore::NoticeType$INFO, ".plssemInfo")
            .pl_insert_notices(self$results, self$options, warn)

            private$.interpretation(lang, blocks, loadings, path_tab, r2_tab)
            private$.keySummary(lang, blocks, vars, paths, prep, loadings, r2_tab, admissible)
            private$.choices(lang, prep)
            private$.guides(lang)
            private$.reproducibility(lang, model, prep)
        },

        # ---------------------------------------------------------------------
        # Introduction while the model is incomplete.
        # ES: Introducción mientras el modelo está incompleto.
        # ---------------------------------------------------------------------
        .showIntro = function(lang, need) {
            txt <- function(key) .pl_text(lang, "intro", key)
            html <- paste0(
                .pl_html_block(txt("what")),
                .pl_html_list(c(txt("step1"), txt("step2"), txt("step3")), ordered = TRUE),
                "<p><strong>", .pl_html_escape(txt(need)), "</strong></p>")
            self$results$intro$setContent(html)
            for (name in c("keySummary", "choicesGuide", "modelGuide", "measurementGuide", "pathsGuide",
                           "interpretation", "reproducibility", "reproducibilityCode"))
                self$results[[name]]$setVisible(FALSE)
        },

        .fillModel = function(blocks, paths, txt) {
            for (i in seq_along(blocks)) {
                b <- blocks[[i]]
                preds <- paths$from[paths$to == b$label]
                self$results$model$setRow(rowKey = i, values = list(
                    construct = b$label,
                    measurement = txt("labels", b$type),
                    indicators = length(b$vars),
                    role = txt("labels", if (length(preds)) "endogenous" else "exogenous"),
                    predictors = if (length(preds)) paste(preds, collapse = ", ") else txt("labels", "none")))
            }
        },

        # Returns the long table of loadings and weights, reused by the
        # interpretation (one object, computed once: ARCHITECTURE.md §6).
        # ES: Devuelve la tabla larga de cargas y pesos, reutilizada por la
        # interpretación.
        .fillMeasurement = function(blocks, model, est) {
            out <- list()
            for (i in seq_along(blocks)) {
                b <- blocks[[i]]
                cc <- model$cmap[[b$label]]
                for (v in b$vars) {
                    vv <- model$vmap[[v]]
                    loading <- est$Loading_estimates[cc, vv]
                    weight <- est$Weight_estimates[cc, vv]
                    self$results$measurement$setRow(rowKey = paste(i, v, sep = "::"), values = list(
                        construct = b$label, indicator = v, loading = loading, weight = weight))
                    out[[length(out) + 1]] <- data.frame(construct = b$label, type = b$type, indicator = v,
                                                         loading = loading, weight = weight,
                                                         stringsAsFactors = FALSE)
                }
            }
            do.call(rbind, out)
        },

        .fillPaths = function(paths, model, est) {
            paths$estimate <- NA_real_
            for (k in seq_len(nrow(paths))) {
                paths$estimate[k] <- est$Path_estimates[model$cmap[[paths$to[k]]], model$cmap[[paths$from[k]]]]
                self$results$paths$setRow(rowKey = k, values = list(
                    from = paths$from[k], arrow = "\u2192", to = paths$to[k], estimate = paths$estimate[k]))
            }
            paths
        },

        .fillR2 = function(paths, model, est) {
            out <- data.frame(construct = unique(paths$to), r2 = NA_real_, r2adj = NA_real_,
                              stringsAsFactors = FALSE)
            for (k in seq_len(nrow(out))) {
                cc <- model$cmap[[out$construct[k]]]
                out$r2[k] <- est$R2[[cc]]
                out$r2adj[k] <- est$R2adj[[cc]]
                self$results$r2$setRow(rowKey = out$construct[k], values = list(
                    construct = out$construct[k], r2 = out$r2[k], r2adj = out$r2adj[k]))
            }
            out
        },

        # ---------------------------------------------------------------------
        # Interpretation: what happened, why, what it means, what to do.
        # ES: Interpretación: qué ocurrió, por qué, qué significa, qué hacer.
        # ---------------------------------------------------------------------
        .interpretation = function(lang, blocks, loadings, paths, r2) {
            txt <- function(key) .pl_text(lang, "interp", key)
            refl <- private$.multiReflective(loadings)
            meas <- character(0)
            if (nrow(refl) == 0) {
                meas <- txt("no_reflective")
            } else {
                low <- refl[refl$loading < 0.708, , drop = FALSE]
                meas <- if (nrow(low))
                    sprintf(txt("low_loadings"), nrow(low), nrow(refl),
                            paste(sprintf("%s %s", low$indicator, vapply(low$loading, .pl_fmt, "")), collapse = ", "))
                else sprintf(txt("good_loadings"), nrow(refl))
            }
            form <- loadings[loadings$type == "formative", , drop = FALSE]
            if (nrow(form)) {
                neg <- unique(form$construct[form$weight < 0])
                meas <- c(meas, if (length(neg)) sprintf(txt("formative_negative"), paste(neg, collapse = ", "))
                                else txt("formative_ok"))
            }
            k <- which.max(abs(paths$estimate))
            band <- function(x) txt(if (x >= 0.75) "r2_substantial" else if (x >= 0.50) "r2_moderate"
                                    else if (x >= 0.25) "r2_weak" else "r2_very_weak")
            r2_lines <- vapply(seq_len(nrow(r2)), function(i)
                sprintf(txt("r2_line"), r2$construct[i], .pl_fmt(r2$r2[i]), band(r2$r2[i])), "")
            struct <- c(sprintf(txt("strongest"), paths$from[k], paths$to[k], .pl_fmt(paths$estimate[k])),
                        r2_lines, txt("r2_note"))
            html <- paste0(.pl_html_block(meas, txt("title_measurement")),
                           .pl_html_block(struct, txt("title_structural")),
                           .pl_html_block(txt("next_steps"), txt("title_next")))
            self$results$interpretation$setContent(html)
        },

        # Indicators of reflective constructs with two or more indicators: a
        # single indicator has a loading of 1 by definition, so it never
        # enters a count of loadings (key findings and interpretation agree).
        # ES: Indicadores de reflectivos con dos o más indicadores: uno solo
        # carga 1 por definición y nunca entra en un conteo de cargas.
        .multiReflective = function(loadings) {
            n <- table(loadings$construct)
            loadings[loadings$type == "reflective" & n[loadings$construct] > 1, , drop = FALSE]
        },

        .keySummary = function(lang, blocks, vars, paths, prep, loadings, r2, admissible) {
            if (!self$options$showKeySummary) return()
            txt <- function(key) .pl_text(lang, "key", key)
            types <- vapply(blocks, `[[`, "", "type")
            est_name <- if (self$options$estimator == "plsc") "PLSc" else "PLS"
            refl <- private$.multiReflective(loadings)
            findings <- c(sprintf(txt("model"), length(blocks), sum(types == "reflective"),
                                  sum(types == "formative"), length(vars), nrow(paths), prep$n_used, est_name))
            if (nrow(refl))
                findings <- c(findings, sprintf(txt("loadings"), sum(refl$loading >= 0.708), nrow(refl)))
            k <- which.max(r2$r2)
            findings <- c(findings, sprintf(txt("r2"), .pl_fmt(r2$r2[k]), r2$construct[k]))
            warnings <- if (admissible) character(0) else txt("admissible_no")
            section <- function(title, items)
                paste0("<p><strong>", .pl_html_escape(title), "</strong></p>",
                       .pl_html_list(if (length(items)) items else txt("none")))
            self$results$keySummary$setContent(paste0(
                section(txt("findings"), findings), section(txt("uncertain"), txt("no_inference")),
                section(txt("warnings"), warnings)))
        },

        .choices = function(lang, prep) {
            if (!self$options$showChoicesGuide) return()
            txt <- function(key) .pl_text(lang, "choices", key)
            scheme <- c(path = "path", centroid = "centroid", factorial = "factorial")[[self$options$innerScheme]]
            paras <- c(txt(self$options$estimator), sprintf(txt("scheme"), scheme),
                       if (self$options$missing == "mean") sprintf(txt("mean"), prep$n_replaced)
                       else sprintf(txt("listwise"), prep$n_used, prep$n_total))
            self$results$choicesGuide$setContent(.pl_html_block(paras))
        },

        .guides = function(lang) {
            if (!self$options$showGuides) return()
            txt <- function(key) .pl_text(lang, "guide", key)
            self$results$modelGuide$setContent(.pl_html_block(txt("model")))
            self$results$measurementGuide$setContent(.pl_html_block(txt("measurement")))
            self$results$pathsGuide$setContent(.pl_html_block(txt("paths")))
        },

        # ---------------------------------------------------------------------
        # Reproducibility: versions and the equivalent cSEM code (MokkenLab
        # pattern), with the internal names mapped back in comments.
        # ES: Reproducibilidad: versiones y el código cSEM equivalente, con los
        # nombres internos traducidos en comentarios.
        # ---------------------------------------------------------------------
        .reproducibility = function(lang, model, prep) {
            if (!self$options$showRepro) return()
            txt <- function(key) .pl_text(lang, "repro", key)
            version <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
            self$results$reproducibility$setContent(.pl_html_block(c(
                sprintf(txt("versions"), version("PLSsemLab"), version("cSEM"), R.version.string,
                        format(Sys.Date())),
                txt("code"))))
            q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
            map_c <- paste0("#   ", model$cmap, " = ", names(model$cmap))
            map_v <- paste0("#   ", model$vmap, " = ", names(model$vmap))
            modes <- if (length(model$modes))
                paste0("list(", paste(sprintf("%s = \"%s\"", names(model$modes), unlist(model$modes)), collapse = ", "), ")")
            else "NULL"
            missing_line <- if (self$options$missing == "mean")
                "for (v in names(d)) d[[v]][is.na(d[[v]])] <- mean(d[[v]], na.rm = TRUE)"
            else "d <- d[complete.cases(d), ]"
            code <- c(
                "# Model as defined in jamovi:",
                paste0("#   ", model$readable),
                "# Internal names:", map_c, map_v,
                sprintf("d <- data[, c(%s)]", paste(q(names(model$vmap)), collapse = ", ")),
                sprintf("names(d) <- c(%s)", paste(q(unname(model$vmap)), collapse = ", ")),
                missing_line,
                "model <- \"",
                model$syntax,
                "\"",
                sprintf("fit <- cSEM::csem(d, model, .PLS_modes = %s, .disattenuate = %s,", modes,
                        if (self$options$estimator == "plsc") "TRUE" else "FALSE"),
                sprintf("                  .PLS_weight_scheme_inner = \"%s\")", self$options$innerScheme),
                "cSEM::summarize(fit)")
            self$results$reproducibilityCode$setContent(paste(code, collapse = "\n"))
        }
    )
)
