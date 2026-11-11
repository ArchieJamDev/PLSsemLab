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

            # Measurement-model quality (phase 2): rows follow the constructs
            # in the boxes; a table that does not apply to the model (no
            # reflective pair, no formative construct) is hidden with its
            # guide, and the interpretation says why.
            # ES: Calidad del modelo de medida: las filas siguen a los
            # constructos de las cajas; una tabla que no aplica al modelo se
            # oculta con su guía y la interpretación explica por qué.
            refl <- labels[vapply(blocks, function(b) b$type == "reflective", TRUE)]
            for (r in refl)
                self$results$reliability$addRow(rowKey = r, values = list(construct = r))
            if (length(refl) >= 2) {
                pairs <- utils::combn(refl, 2)
                for (k in seq_len(ncol(pairs)))
                    self$results$htmt$addRow(rowKey = paste(pairs[, k], collapse = "::"),
                                             values = list(c1 = pairs[1, k], sep = "–", c2 = pairs[2, k]))
            }
            for (i in seq_along(refl)) {
                self$results$fornell$addColumn(name = paste0("c", i), title = refl[i], type = "number")
                self$results$fornell$addRow(rowKey = refl[i], values = list(construct = refl[i]))
            }
            for (i in seq_along(blocks)) {
                if (blocks[[i]]$type != "formative") next
                for (v in blocks[[i]]$vars)
                    self$results$vif$addRow(rowKey = paste(i, v, sep = "::"),
                                            values = list(construct = blocks[[i]]$label, indicator = v))
            }
            hide <- c(if (length(refl) == 0) c("reliability", "reliabilityGuide", "fornell"),
                      if (length(refl) < 2) c("htmt", "htmtGuide"),
                      if (!any(vapply(blocks, function(b) b$type == "formative", TRUE))) c("vif", "vifGuide"))
            for (name in hide)
                self$results[[name]]$setVisible(FALSE)
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
            quality <- .pl_measurement_quality(fit$value, blocks, paths, model, d, self$options)
            private$.fillReliability(quality, txt)
            private$.fillHtmt(quality, txt)
            private$.fillFornell(quality, txt)
            private$.fillVif(quality, txt)

            # 5. Notices, interpretation, summaries, reproducibility.
            # ES: 5. Avisos, interpretación, resúmenes, reproducibilidad.
            warn <- c(vapply(cons$empty, function(x) sprintf(txt("notices", "empty_construct"), x), ""),
                      unlist(lapply(blocks, function(b)
                          if (b$type == "reflective" && length(b$vars) == 1)
                              sprintf(txt("notices", "single_reflective"), b$label))),
                      fit$warnings, quality$warnings)
            if (!admissible) {
                failed <- names(which(unlist(cSEM::verify(fit$value))))
                warn <- c(warn, sprintf(txt("notices", "inadmissible"), paste(failed, collapse = ", ")))
            }
            info <- character(0)
            if (prep$n_dropped > 0)
                info <- c(info, sprintf(txt("notices", "dropped"), prep$n_dropped, prep$n_used))
            if (prep$n_replaced > 0)
                info <- c(info, sprintf(txt("notices", "replaced"), prep$n_replaced, prep$n_cases_replaced))

            types <- vapply(blocks, `[[`, "", "type")
            if (!any(types == "reflective"))
                info <- c(info, txt("notices", "no_reflective_quality"))
            else if (sum(types == "reflective") == 1)
                info <- c(info, txt("notices", "one_reflective_htmt"))
            .pl_insert_notices(self$results, self$options, info, jmvcore::NoticeType$INFO, ".plssemInfo")
            .pl_insert_notices(self$results, self$options, warn)

            private$.interpretation(lang, blocks, loadings, path_tab, r2_tab, quality)
            private$.keySummary(lang, blocks, vars, paths, prep, loadings, r2_tab, admissible, quality)
            private$.choices(lang, prep)
            private$.guides(lang)
            private$.reproducibility(lang, model, prep, blocks, paths)
        },

        # ---------------------------------------------------------------------
        # Measurement-model quality tables. Empty numeric cells carry a
        # footnote with the specific reason (Category B: the row stays).
        # ES: Tablas de calidad del modelo de medida. Las celdas numéricas
        # vacías llevan una nota al pie con la razón específica.
        # ---------------------------------------------------------------------
        .fillReliability = function(quality, txt) {
            rel <- quality$reliability
            if (is.null(rel)) return()
            table <- self$results$reliability
            if (self$options$estimator == "pls")
                table$setNote("rhoA", txt("footnotes", "rho_a_pls"))
            for (k in seq_len(nrow(rel))) {
                r <- rel[k, ]
                if (r$single) {
                    table$setRow(rowKey = r$construct, values = list(
                        construct = r$construct, assessment = txt("labels", "not_computable")))
                    table$addFootnote(rowKey = r$construct, col = "assessment",
                                      txt("footnotes", "single_reliability"))
                    next
                }
                values <- list(construct = r$construct)
                for (col in c("alpha", "rhoA", "rhoC", "ave"))
                    if (!is.na(r[[col]])) values[[col]] <- r[[col]]
                parts <- c(.pl_band_reliability(r$rhoC),
                           if (!is.na(r$ave)) if (r$ave >= 0.50) "ave_ok" else "ave_low")
                parts <- parts[!is.na(parts)]
                values$assessment <- if (length(parts))
                    paste(vapply(parts, function(p) txt("labels", p), ""), collapse = "; ")
                else txt("labels", "not_computable")
                table$setRow(rowKey = r$construct, values = values)
                for (col in c("alpha", "rhoA", "rhoC", "ave")) {
                    if (is.na(r[[col]])) {
                        reason <- quality$errors[[col]]
                        table$addFootnote(rowKey = r$construct, col = col, sprintf(
                            txt("footnotes", "failed"), if (is.null(reason)) "NaN" else reason))
                    }
                }
                if (!is.na(r$alpha) && r$alpha < 0)
                    table$addFootnote(rowKey = r$construct, col = "alpha", txt("footnotes", "alpha_negative"))
            }
        },

        .fillHtmt = function(quality, txt) {
            h <- quality$htmt
            if (is.null(h)) return()
            table <- self$results$htmt
            for (k in seq_len(nrow(h))) {
                key <- paste(h$c1[k], h$c2[k], sep = "::")
                band <- .pl_band_htmt(h$htmt[k])
                values <- list(c1 = h$c1[k], sep = "–", c2 = h$c2[k],
                               assessment = if (is.na(band)) txt("labels", "not_computable")
                                            else txt("labels", band))
                if (!is.na(h$htmt[k])) values$htmt <- h$htmt[k]
                if (!is.na(h$htmt2[k])) values$htmt2 <- h$htmt2[k]
                table$setRow(rowKey = key, values = values)
                single <- intersect(c(h$c1[k], h$c2[k]), quality$reliability$construct[quality$reliability$single])
                if (length(single) && !is.na(h$htmt[k]))
                    table$addFootnote(rowKey = key, col = "htmt", sprintf(txt("footnotes", "htmt_single"), single[1]))
                for (col in c("htmt", "htmt2")) {
                    if (!is.na(h[[col]][k])) next
                    reason <- quality$errors[[col]]
                    table$addFootnote(rowKey = key, col = col, if (is.null(reason)) txt("footnotes", "htmt_signs")
                                                               else sprintf(txt("footnotes", "failed"), reason))
                }
            }
        },

        .fillFornell = function(quality, txt) {
            if (!self$options$fornellLarcker || is.null(quality$fl)) return()
            table <- self$results$fornell
            cr <- quality$fl$cor
            s <- quality$fl$sqrt_ave
            n <- nrow(cr)
            table$setNote("fl", txt("footnotes", "fl_note"))
            for (i in seq_len(n)) {
                values <- list(construct = rownames(cr)[i])
                if (!is.na(s[i])) values[[paste0("c", i)]] <- s[[i]]
                for (j in seq_len(i - 1)) values[[paste0("c", j)]] <- cr[i, j]
                table$setRow(rowKey = rownames(cr)[i], values = values)
                for (j in seq_len(i - 1)) {
                    worst <- min(s[[i]], s[[j]])
                    if (!is.na(worst) && abs(cr[i, j]) > worst)
                        table$addFootnote(rowKey = rownames(cr)[i], col = paste0("c", j),
                                          sprintf(txt("footnotes", "fl_fails"), rownames(cr)[j]))
                }
                if (is.na(s[i]))
                    table$addFootnote(rowKey = rownames(cr)[i], col = paste0("c", i),
                                      txt("footnotes", "single_reliability"))
            }
        },

        .fillVif = function(quality, txt) {
            v <- quality$vif
            if (is.null(v)) return()
            table <- self$results$vif
            keys <- vapply(seq_len(nrow(v)), function(k) {
                i <- which(vapply(private$.blocks(), `[[`, "", "label") == v$construct[k])
                paste(i, v$indicator[k], sep = "::")
            }, "")
            for (k in seq_len(nrow(v))) {
                band <- .pl_band_vif(v$vif[k])
                values <- list(construct = v$construct[k], indicator = v$indicator[k],
                               assessment = if (is.na(band)) txt("labels", "not_computable")
                                            else txt("labels", band))
                if (!is.na(v$vif[k])) values$vif <- v$vif[k]
                table$setRow(rowKey = keys[k], values = values)
                if (is.na(v$vif[k])) {
                    note <- if (v$single[k]) txt("footnotes", "single_vif")
                            else sprintf(txt("footnotes", "failed"),
                                         if (is.null(quality$errors$vif)) "NaN" else quality$errors$vif)
                    table$addFootnote(rowKey = keys[k], col = "vif", note)
                }
            }
        },

        .blocks = function() .pl_constructs(self$options)$blocks,

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
            for (name in c("keySummary", "choicesGuide", "modelGuide", "measurementGuide", "reliabilityGuide",
                           "htmtGuide", "vifGuide", "pathsGuide", "interpretation", "reproducibility",
                           "reproducibilityCode"))
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
        .interpretation = function(lang, blocks, loadings, paths, r2, quality) {
            txt <- function(key) .pl_text(lang, "interp", key)
            listed <- function(names, values)
                paste(sprintf("%s (%s)", names, vapply(values, .pl_fmt, "")), collapse = ", ")
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

            # Reliability, convergent and discriminant validity, collinearity:
            # the same counts as the tables (one band function per table).
            # ES: Fiabilidad, validez convergente y discriminante, colinealidad:
            # los mismos conteos que las tablas.
            rel <- quality$reliability
            if (!is.null(rel)) {
                ok <- rel[!rel$single & !is.na(rel$rhoC), , drop = FALSE]
                if (nrow(ok)) {
                    low <- ok[ok$rhoC < 0.70, , drop = FALSE]
                    high <- ok[ok$rhoC > 0.95, , drop = FALSE]
                    if (nrow(low)) meas <- c(meas, sprintf(txt("reliability_low"), listed(low$construct, low$rhoC)))
                    if (nrow(high)) meas <- c(meas, sprintf(txt("reliability_high"), listed(high$construct, high$rhoC)))
                    if (!nrow(low) && !nrow(high)) meas <- c(meas, sprintf(txt("reliability_ok"), nrow(ok)))
                }
                a <- rel[!rel$single & !is.na(rel$ave), , drop = FALSE]
                if (nrow(a)) {
                    low <- a[a$ave < 0.50, , drop = FALSE]
                    meas <- c(meas, if (nrow(low)) sprintf(txt("ave_low"), listed(low$construct, low$ave))
                                    else sprintf(txt("ave_ok"), nrow(a)))
                }
            }
            h <- quality$htmt
            if (!is.null(h)) {
                pair <- paste(h$c1, h$c2, sep = "–")
                high <- !is.na(h$htmt) & h$htmt >= 0.85
                if (any(high)) meas <- c(meas, sprintf(txt("htmt_high"), listed(pair[high], h$htmt[high])))
                else if (!anyNA(h$htmt)) meas <- c(meas, sprintf(txt("htmt_ok"), length(unique(c(h$c1, h$c2)))))
                if (anyNA(h$htmt)) meas <- c(meas, sprintf(txt("htmt_na"), paste(pair[is.na(h$htmt)], collapse = ", ")))
            }
            v <- quality$vif
            if (!is.null(v) && any(!is.na(v$vif))) {
                high <- !is.na(v$vif) & v$vif >= 3
                meas <- c(meas, if (any(high)) sprintf(txt("vif_high"), listed(v$indicator[high], v$vif[high]))
                                else txt("vif_ok"))
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

        .keySummary = function(lang, blocks, vars, paths, prep, loadings, r2, admissible, quality) {
            if (!self$options$showKeySummary) return()
            txt <- function(key) .pl_text(lang, "key", key)
            types <- vapply(blocks, `[[`, "", "type")
            est_name <- if (self$options$estimator == "plsc") "PLSc" else "PLS"
            refl <- private$.multiReflective(loadings)
            findings <- c(sprintf(txt("model"), length(blocks), sum(types == "reflective"),
                                  sum(types == "formative"), length(vars), nrow(paths), prep$n_used, est_name))
            if (nrow(refl))
                findings <- c(findings, sprintf(txt("loadings"), sum(refl$loading >= 0.708), nrow(refl)))
            rel <- quality$reliability
            if (!is.null(rel) && any(!rel$single)) {
                multi <- rel[!rel$single, , drop = FALSE]
                rc <- multi$rhoC[!is.na(multi$rhoC)]
                av <- multi$ave[!is.na(multi$ave)]
                findings <- c(findings, sprintf(txt("reliability"), sum(rc >= 0.70 & rc <= 0.95), length(rc),
                                                sum(av >= 0.50), length(av)))
            }
            h <- quality$htmt
            if (!is.null(h))
                findings <- c(findings, sprintf(txt("htmt"), sum(!is.na(h$htmt) & h$htmt < 0.85), nrow(h)))
            v <- quality$vif
            if (!is.null(v) && any(!is.na(v$vif))) {
                j <- which.max(v$vif)
                findings <- c(findings, sprintf(txt("vif"), .pl_fmt(v$vif[j]), v$indicator[j]))
            }
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
            self$results$reliabilityGuide$setContent(.pl_html_block(txt("reliability")))
            self$results$htmtGuide$setContent(.pl_html_block(txt("htmt")))
            self$results$vifGuide$setContent(.pl_html_block(txt("vif")))
            self$results$pathsGuide$setContent(.pl_html_block(txt("paths")))
        },

        # ---------------------------------------------------------------------
        # Reproducibility: versions and the equivalent cSEM code (MokkenLab
        # pattern), with the internal names mapped back in comments.
        # ES: Reproducibilidad: versiones y el código cSEM equivalente, con los
        # nombres internos traducidos en comentarios.
        # ---------------------------------------------------------------------
        .reproducibility = function(lang, model, prep, blocks, paths) {
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
            modes_of <- function(m) if (length(m))
                paste0("list(", paste(sprintf("%s = \"%s\"", names(m), unlist(m)), collapse = ", "), ")")
            else "NULL"
            modes <- modes_of(model$modes)
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
            types <- vapply(blocks, `[[`, "", "type")
            if (any(types == "reflective")) {
                # Quality criteria, each called separately as the module does.
                # ES: Criterios de calidad, cada uno por separado, como el módulo.
                code <- c(code, "",
                    "# Measurement model: reliability, convergent and discriminant validity",
                    "cSEM::calculateRhoT(fit, .only_common_factors = FALSE)  # Cronbach's alpha",
                    "cSEM::calculateRhoC(fit, .only_common_factors = FALSE)",
                    "cSEM::calculateAVE(fit, .only_common_factors = FALSE)")
                if (self$options$estimator == "plsc") {
                    code <- c(code, "fit$Estimates$Reliabilities  # rho_A")
                } else {
                    plsc <- .pl_build_syntax(blocks, paths, "plsc")
                    code <- c(code,
                        "# rho_A: reliabilities of a consistent PLS estimation of the same model",
                        "model_c <- \"", plsc$syntax, "\"",
                        sprintf("fit_c <- cSEM::csem(d, model_c, .PLS_modes = %s, .disattenuate = TRUE,",
                                modes_of(plsc$modes)),
                        sprintf("                    .PLS_weight_scheme_inner = \"%s\")", self$options$innerScheme),
                        "fit_c$Estimates$Reliabilities  # rho_A")
                }
                if (sum(types == "reflective") >= 2)
                    code <- c(code,
                        "cSEM::calculateHTMT(fit, .only_common_factors = FALSE)$htmts",
                        "cSEM::calculateHTMT(fit, .type_htmt = \"htmt2\", .only_common_factors = FALSE)$htmts")
                if (self$options$fornellLarcker)
                    code <- c(code, "fit$Estimates$Construct_VCV  # correlations; diagonal of Fornell-Larcker: sqrt(AVE)")
            }
            if (any(types == "formative"))
                code <- c(code, "", "# Collinearity of formative indicators", "cSEM::calculateVIFModeB(fit)")
            self$results$reproducibilityCode$setContent(paste(code, collapse = "\n"))
        }
    )
)
