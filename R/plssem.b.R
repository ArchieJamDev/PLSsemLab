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
            paths <- .pl_paths(self$options$structural, .pl_structural_labels(blocks))
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
                                             values = list(c1 = pairs[1, k], sep = "\u2013", c2 = pairs[2, k]))
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
            # Structural tables (phase 3): f² and VIF per path, effects per
            # pair of constructs joined by at least one path.
            # ES: Tablas estructurales (fase 3): f² y VIF por ruta, efectos
            # por par de constructos unidos por al menos una ruta.
            for (k in seq_len(nrow(paths))) {
                row <- list(from = paths$from[k], arrow = "\u2192", to = paths$to[k])
                self$results$f2$addRow(rowKey = k, values = row)
                self$results$vifStruct$addRow(rowKey = k, values = row)
            }
            pairs_eff <- .pl_effect_pairs(paths)
            for (k in seq_len(nrow(pairs_eff)))
                self$results$effects$addRow(rowKey = paste(pairs_eff$from[k], pairs_eff$to[k], sep = "::"),
                                            values = list(from = pairs_eff$from[k], arrow = "\u2192",
                                                          to = pairs_eff$to[k]))

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
            paths <- .pl_paths(self$options$structural, .pl_structural_labels(blocks))
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
            ordinal <- self$options$indicatorScale == "ordinal"
            if (ordinal && self$options$missing == "mean")
                jmvcore::reject(txt("errors", "ordinal_mean"))
            vars <- unlist(lapply(blocks, `[[`, "vars"))
            prep <- .pl_prepare_indicators(self$data, vars, self$options$missing, lang)
            if (length(prep$errors))
                jmvcore::reject(paste(prep$errors, collapse = "\n"))

            # 3. Build and estimate.
            # ES: 3. Armar y estimar.
            model <- .pl_build_syntax(blocks, paths, self$options$estimator)
            d <- prep$data[, names(model$vmap), drop = FALSE]
            names(d) <- unname(model$vmap)
            # Ordinal indicators go to cSEM as ordered factors, which makes it
            # use polychoric correlations (ordinal PLS).
            # ES: Los ordinales van a cSEM como factores ordenados, con lo que
            # usa correlaciones policóricas (PLS ordinal).
            categories <- stats::setNames(vapply(d, function(x) length(unique(x)), 0), names(model$vmap))
            if (ordinal)
                for (v in names(d)) d[[v]] <- ordered(d[[v]])
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
            structural <- private$.fillStructural(fit$value, paths, model, txt)

            # Bootstrap (phase 3): only when "Run bootstrap" is ticked.
            # ES: Bootstrap (fase 3): solo con "Ejecutar bootstrap" marcado.
            boot <- NULL
            if (self$options$bootstrap) {
                boot <- private$.bootstrap(fit$value, blocks, model, d, txt)
                if (!is.null(boot$summary))
                    private$.fillBootstrap(boot$summary, blocks, paths, model, txt)
            }
            private$.prepareDiagram(blocks, loadings, path_tab, r2_tab, boot$summary, model)
            private$.preparePlots(loadings, quality, boot$summary, model, txt)

            # 5. Notices, interpretation, summaries, reproducibility.
            # ES: 5. Avisos, interpretación, resúmenes, reproducibilidad.
            warn <- c(vapply(cons$empty, function(x) sprintf(txt("notices", "empty_construct"), x), ""),
                      unlist(lapply(blocks, function(b)
                          if (b$type == "reflective" && length(b$vars) == 1)
                              sprintf(txt("notices", "single_reflective"), b$label))),
                      fit$warnings, quality$warnings,
                      private$.negativeLoadings(loadings, txt),
                      if (self$options$bootstrap && is.null(boot$summary))
                          sprintf(txt("boot", "failed"), paste(boot$error, collapse = "; ")),
                      if (!is.null(boot$summary) && length(boot$error))
                          sprintf(txt("boot", "some_failed"), paste(boot$error, collapse = "; ")))
            if (!admissible) {
                failed <- names(which(unlist(cSEM::verify(fit$value))))
                warn <- c(warn, sprintf(txt("notices", "inadmissible"), paste(failed, collapse = ", ")))
            }
            info <- character(0)
            if (prep$n_dropped > 0)
                info <- c(info, sprintf(txt("notices", "dropped"), prep$n_dropped, prep$n_used))
            if (prep$n_replaced > 0)
                info <- c(info, sprintf(txt("notices", "replaced"), prep$n_replaced, prep$n_cases_replaced))

            few <- names(categories)[categories <= 5]
            many <- names(categories)[categories > 10]
            if (!ordinal && length(few))
                info <- c(info, sprintf(txt("notices", "few_categories"), paste(few, collapse = ", ")))
            if (ordinal && length(many))
                warn <- c(warn, sprintf(txt("notices", "many_categories"), paste(many, collapse = ", ")))
            types <- vapply(blocks, `[[`, "", "type")
            if (!any(types == "reflective"))
                info <- c(info, txt("notices", "no_reflective_quality"))
            else if (sum(types == "reflective") == 1)
                info <- c(info, txt("notices", "one_reflective_htmt"))
            .pl_insert_notices(self$results, self$options, info, jmvcore::NoticeType$INFO, ".plssemInfo")
            .pl_insert_notices(self$results, self$options, warn)

            private$.interpretation(lang, blocks, loadings, path_tab, r2_tab, quality, structural, boot$summary, model)
            private$.keySummary(lang, blocks, vars, paths, prep, loadings, r2_tab, admissible, quality, boot$summary,
                                model)
            private$.choices(lang, prep, boot)
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
            if (self$options$indicatorScale == "ordinal")
                table$setNote("ordinal", txt("footnotes", "ordinal_note"))
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
                for (col in c("alpha", "rhoA", "rhoC", "ave", "omega"))
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
                if (self$options$omegaCfa && is.na(r$omega)) {
                    reason <- r$omega_reason
                    note <- if (identical(reason, "two")) txt("footnotes", "omega_two")
                            else if (identical(reason, "nonconverged")) txt("footnotes", "omega_nonconverged")
                            else if (identical(reason, "heywood")) txt("footnotes", "omega_heywood")
                            else sprintf(txt("footnotes", "failed"), reason)
                    table$addFootnote(rowKey = r$construct, col = "omega", note)
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
                values <- list(c1 = h$c1[k], sep = "\u2013", c2 = h$c2[k],
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

        # ---------------------------------------------------------------------
        # Structural model (phase 3): effect size f², collinearity of the
        # predictors (VIF) and direct, indirect and total effects. Point
        # estimates; their intervals come from the bootstrap.
        # ES: Modelo estructural (fase 3): tamaño del efecto f², colinealidad
        # de los predictores (VIF) y efectos directos, indirectos y totales.
        # ---------------------------------------------------------------------
        .fillStructural = function(fit, paths, model, txt) {
            cc <- function(x) model$cmap[[x]]
            f2 <- .pl_capture(cSEM::calculatef2(fit))
            vifs <- unlist(fit$Estimates$VIF)
            eff <- .pl_capture(cSEM::summarize(fit)$Estimates$Effect_estimates)
            out_f2 <- rep(NA_real_, nrow(paths))
            out_vif <- rep(NA_real_, nrow(paths))
            for (k in seq_len(nrow(paths))) {
                to <- cc(paths$to[k])
                from <- cc(paths$from[k])
                f <- if (is.null(f2$value)) NA_real_ else f2$value[to, from]
                out_f2[k] <- if (is.finite(f)) f else NA_real_
                band <- .pl_band_f2(out_f2[k])
                values <- list(from = paths$from[k], arrow = "\u2192", to = paths$to[k],
                               assessment = if (is.na(band)) txt("labels", "not_computable") else txt("labels", band))
                if (!is.na(out_f2[k])) values$f2 <- out_f2[k]
                self$results$f2$setRow(rowNo = k, values = values)
                if (is.na(out_f2[k]))
                    self$results$f2$addFootnote(rowNo = k, col = "f2", sprintf(txt("footnotes", "failed"),
                        if (is.null(f2$error)) "NaN" else f2$error))
                v <- vifs[paste(to, from, sep = ".")]
                out_vif[k] <- if (length(v) && is.finite(v)) unname(v) else NA_real_
                band <- .pl_band_vif(out_vif[k])
                values <- list(from = paths$from[k], arrow = "\u2192", to = paths$to[k],
                               assessment = if (sum(paths$to == paths$to[k]) == 1) txt("labels", "vif_single_predictor")
                                            else if (is.na(band)) txt("labels", "not_computable") else txt("labels", band))
                if (!is.na(out_vif[k])) values$vif <- out_vif[k]
                self$results$vifStruct$setRow(rowNo = k, values = values)
            }
            pairs <- .pl_effect_pairs(paths)
            pick <- function(df, from, to) {
                if (is.null(df)) return(NA_real_)
                v <- df$Estimate[as.character(df$Name) == paste(cc(to), "~", cc(from))]
                if (length(v) && is.finite(v[1]) && v[1] != 0) v[1] else NA_real_
            }
            eff_rows <- list()
            for (k in seq_len(nrow(pairs))) {
                from <- pairs$from[k]
                to <- pairs$to[k]
                direct <- paths$from == from & paths$to == to
                values <- list(from = from, arrow = "\u2192", to = to)
                d_est <- if (any(direct)) fit$Estimates$Path_estimates[cc(to), cc(from)] else NA_real_
                i_est <- pick(eff$value$Indirect_effect, from, to)
                t_est <- pick(eff$value$Total_effect, from, to)
                if (!is.na(d_est)) values$direct <- d_est
                if (!is.na(i_est)) values$indirect <- i_est
                if (!is.na(t_est)) values$total <- t_est
                self$results$effects$setRow(rowKey = paste(from, to, sep = "::"), values = values)
                eff_rows[[k]] <- data.frame(from = from, to = to, direct = d_est, indirect = i_est, total = t_est,
                                            stringsAsFactors = FALSE)
            }
            list(f2 = cbind(paths[, c("from", "to")], f2 = out_f2), vif = cbind(paths[, c("from", "to")], vif = out_vif),
                 effects = if (length(eff_rows)) do.call(rbind, eff_rows) else NULL)
        },

        # ---------------------------------------------------------------------
        # Bootstrap: cached, in blocks, with progress and a checkpoint after
        # each block (stops when an option changes).
        # ES: Bootstrap: en caché, por bloques, con avance y un checkpoint
        # después de cada bloque (se detiene si cambia una opción).
        # ---------------------------------------------------------------------
        .bootstrap = function(fit, blocks, model, d, txt) {
            reps <- as.integer(self$options$bootReps)
            seed <- as.integer(self$options$bootSeed)
            level <- as.numeric(self$options$ciLevel)
            status <- self$results$bootStatus
            spec <- .pl_boot_spec(blocks, model, fit, self$options)
            key <- list("plssem-boot-1", model$syntax, model$modes, d, self$options$estimator,
                        self$options$innerScheme, reps, seed, level,
                        tryCatch(as.character(utils::packageVersion("cSEM")), error = function(e) ""))
            res <- .pl_cached_bootstrap(self$results$cacheStore, key, function() {
                run <- .pl_run_bootstrap(fit, spec$funs, reps, seed, progress = function(done, total) {
                    status$setContent(.pl_html_block(sprintf(txt("boot", "running"), done, total)))
                    private$.checkpoint()
                })
                sm <- if (!is.null(run$value)) .pl_capture(.pl_boot_summary(run$value, level, spec)) else NULL
                list(summary = sm$value, error = c(run$errors, sm$error), warnings = run$warnings,
                     seconds = run$seconds)
            })
            if (!is.null(res$summary)) {
                status$setContent(.pl_html_block(sprintf(txt("boot", "done"), res$summary$runs, res$summary$admissible,
                    seed, .PL_BLOCK, round(res$seconds), format(100 * level))))
            } else {
                status$setContent(.pl_html_block(txt("boot", "not_done")))
            }
            res
        },

        # Writes the bootstrap columns of every table, by internal name.
        # ES: Escribe las columnas del bootstrap de cada tabla, por nombre
        # interno.
        .fillBootstrap = function(sm, blocks, paths, model, txt) {
            cc <- function(x) model$cmap[[x]]
            vv <- function(x) model$vmap[[x]]
            set <- function(table, values, rowNo = NA, rowKey = NULL) {
                for (col in names(values)) {
                    v <- values[[col]]
                    if (length(v) == 1 && is.finite(v))
                        table$setCell(col = col, value = v, rowNo = rowNo, rowKey = rowKey)
                }
            }
            row_of <- function(df, name) if (is.null(df)) NULL else df[df$name == name, , drop = FALSE]
            pct <- format(100 * as.numeric(self$options$ciLevel))
            note <- sprintf(txt("boot", "ci_note"), pct, sm$admissible)

            # Paths: SE, t, p and interval (cSEM's summarize).
            # ES: Rutas: EE, t, p e intervalo (summarize de cSEM).
            for (k in seq_len(nrow(paths))) {
                r <- row_of(sm$paths, paste(cc(paths$to[k]), "~", cc(paths$from[k])))
                if (nrow(r)) {
                    set(self$results$paths, list(se = r$se, t = r$t, p = r$p, lower = r$lower, upper = r$upper), rowNo = k)
                    # p (normal theory on the bootstrap SE) and the percentile
                    # interval can disagree on borderline paths; the interval
                    # decides, and the row says so.
                    # ES: p y el intervalo percentil pueden discrepar en rutas
                    # límite; decide el intervalo y la fila lo dice.
                    excl <- is.finite(r$lower) && (r$lower > 0 || r$upper < 0)
                    sig_p <- is.finite(r$p) && r$p < 1 - as.numeric(self$options$ciLevel)
                    if (is.finite(r$p) && excl != sig_p)
                        self$results$paths$addFootnote(rowNo = k, col = "p", txt("boot", "p_ci_disagree"))
                }
            }
            self$results$paths$setNote("ci", paste(note, txt("boot", "p_note")))

            # Loadings and weights.
            # ES: Cargas y pesos.
            for (i in seq_along(blocks)) for (v in blocks[[i]]$vars) {
                key <- paste(i, v, sep = "::")
                l <- row_of(sm$loadings, paste(cc(blocks[[i]]$label), "=~", vv(v)))
                w <- row_of(sm$weights, paste(cc(blocks[[i]]$label), "<~", vv(v)))
                if (!is.null(l) && nrow(l)) set(self$results$measurement, list(loadLower = l$lower, loadUpper = l$upper), rowKey = key)
                if (!is.null(w) && nrow(w)) set(self$results$measurement, list(weightLower = w$lower, weightUpper = w$upper,
                                                                              weightP = w$p), rowKey = key)
            }
            self$results$measurement$setNote("ci", note)

            for (to in unique(paths$to)) {
                r <- row_of(sm$R2, cc(to))
                if (!is.null(r) && nrow(r)) set(self$results$r2, list(r2Lower = r$lower, r2Upper = r$upper), rowKey = to)
            }
            self$results$r2$setNote("ci", note)

            for (b in Filter(function(b) b$type == "reflective" && length(b$vars) > 1, blocks)) {
                rc <- row_of(sm$rhoC, cc(b$label))
                av <- row_of(sm$AVE, cc(b$label))
                if (!is.null(rc) && nrow(rc)) set(self$results$reliability, list(rhoCLower = rc$lower, rhoCUpper = rc$upper), rowKey = b$label)
                if (!is.null(av) && nrow(av)) set(self$results$reliability, list(aveLower = av$lower, aveUpper = av$upper), rowKey = b$label)
            }
            self$results$reliability$setNote("ci", note)

            # HTMT: the one-sided 95% upper bound decides the assessment.
            # ES: HTMT: el límite superior unilateral al 95% decide la
            # valoración.
            if (!is.null(sm$HTMT)) {
                refl <- vapply(Filter(function(b) b$type == "reflective", blocks), `[[`, "", "label")
                pr <- utils::combn(refl, 2)
                for (k in seq_len(ncol(pr))) {
                    r <- row_of(sm$HTMT, paste(cc(pr[1, k]), cc(pr[2, k]), sep = "~"))
                    key <- paste(pr[, k], collapse = "::")
                    if (!is.null(r) && nrow(r) && is.finite(r$upper)) {
                        self$results$htmt$setCell(col = "htmtUpper", value = r$upper, rowKey = key)
                        band <- if (r$upper < 0.85) "htmt_ok_ci" else if (r$upper < 0.90) "htmt_similar_ci" else "htmt_doubt_ci"
                        self$results$htmt$setCell(col = "assessment", value = txt("labels", band), rowKey = key)
                    }
                }
                self$results$htmt$setNote("ci", sprintf(txt("boot", "htmt_note"), sm$admissible))
            }

            for (i in seq_along(blocks)) {
                if (blocks[[i]]$type != "formative" || length(blocks[[i]]$vars) < 2) next
                for (v in blocks[[i]]$vars) {
                    r <- row_of(sm$VIFB, paste(cc(blocks[[i]]$label), vv(v), sep = "~"))
                    if (!is.null(r) && nrow(r)) set(self$results$vif, list(vifLower = r$lower, vifUpper = r$upper),
                                                    rowKey = paste(i, v, sep = "::"))
                }
            }
            self$results$vif$setNote("ci", note)

            for (k in seq_len(nrow(paths))) {
                r <- row_of(sm$VIFS, paste(cc(paths$to[k]), cc(paths$from[k]), sep = "."))
                if (!is.null(r) && nrow(r)) set(self$results$vifStruct, list(vifLower = r$lower, vifUpper = r$upper), rowNo = k)
            }
            self$results$vifStruct$setNote("ci", note)

            pairs <- .pl_effect_pairs(paths)
            for (k in seq_len(nrow(pairs))) {
                key <- paste(pairs$from[k], pairs$to[k], sep = "::")
                nm <- paste(cc(pairs$to[k]), "~", cc(pairs$from[k]))
                ri <- row_of(sm$indirect, nm)
                rt <- row_of(sm$total, nm)
                if (!is.null(ri) && nrow(ri) && ri$estimate != 0)
                    set(self$results$effects, list(indLower = ri$lower, indUpper = ri$upper), rowKey = key)
                if (!is.null(rt) && nrow(rt))
                    set(self$results$effects, list(totLower = rt$lower, totUpper = rt$upper), rowKey = key)
            }
            self$results$effects$setNote("ci", note)
        },

        # ---------------------------------------------------------------------
        # Path diagram: layout computed here and kept in image$state; the
        # render function only draws it.
        # ES: Diagrama de rutas: la disposición se calcula aquí y se guarda en
        # image$state; la función de dibujo solo la dibuja.
        # ---------------------------------------------------------------------
        .prepareDiagram = function(blocks, loadings, path_tab, r2_tab, sm, model) {
            image <- self$results$pathDiagram
            if (!self$options$pathDiagram) return()
            lang <- .pl_lang(self$options)
            caption <- if (is.null(sm)) .pl_text(lang, "labels", "diagram_caption")
                       else sprintf(.pl_text(lang, "labels", "diagram_caption_boot"),
                                    format(100 * as.numeric(self$options$ciLevel)))
            st <- .pl_sem_state(blocks, loadings, path_tab, r2_tab, sm, model,
                                indicators = isTRUE(self$options$diagramIndicators), caption = caption)
            # Size: wider with more layers of constructs, taller with more
            # indicators per layer (left to right) or the reverse.
            # ES: Tamaño según capas de constructos e indicadores.
            # Size from the extent of the layout (about 50 px per indicator
            # step, 95 px per unit across), so boxes keep their size.
            # ES: Tamaño según la extensión de la disposición.
            span_x <- diff(range(st$x)) + 1.6
            span_y <- diff(range(st$y)) + 1
            long <- max(560, min(1100, 75 * span_x + 120))
            wide <- max(360, min(1100, 62 * span_y + (if (isTRUE(self$options$diagramCaption)) 170 else 110)))
            if (identical(self$options$diagramLayout, "treeTB")) {
                tx <- diff(range(st$x_tb)) + 1.6
                ty <- diff(range(st$y_tb)) + 1
                image$setSize(max(560, min(1100, 62 * tx + 120)), max(360, min(1100, 75 * ty + 170)))
            } else image$setSize(long, wide)
            image$setState(st)
        },

        # Loadings and HTMT plots: data in image$state; a plot that does not
        # apply to the model (no reflective construct with two indicators, no
        # reflective pair) is hidden.
        # ES: Gráficos de cargas y HTMT: datos en image$state; un gráfico que
        # no aplica al modelo se oculta.
        .preparePlots = function(loadings, quality, sm, model, txt) {
            pct <- format(100 * as.numeric(self$options$ciLevel))
            if (self$options$loadingsPlot) {
                st <- .pl_loadings_state(loadings, sm, model, if (is.null(sm)) txt("labels", "loadings_caption")
                                         else sprintf(txt("labels", "loadings_caption_boot"), pct))
                if (is.null(st)) self$results$loadingsPlot$setVisible(FALSE)
                else {
                    self$results$loadingsPlot$setSize(600, max(260, 26 * nrow(st$data) + 30 * length(unique(st$data$construct)) + 70))
                    self$results$loadingsPlot$setState(st)
                }
            }
            if (self$options$htmtPlot) {
                st <- .pl_htmt_state(quality, sm, model, if (is.null(sm)) txt("labels", "htmt_caption")
                                     else txt("labels", "htmt_caption_boot"))
                if (is.null(st)) self$results$htmtPlot$setVisible(FALSE)
                else {
                    n <- length(st$levels)
                    self$results$htmtPlot$setSize(max(380, 95 * n + 140), max(340, 85 * n + 120))
                    self$results$htmtPlot$setState(st)
                }
            }
        },

        .plotLoadings = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$data)) return(FALSE)
            print(.pl_plot_loadings(image$state, ggtheme, theme))
            TRUE
        },

        .plotHtmt = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$data)) return(FALSE)
            print(.pl_plot_htmt(image$state, ggtheme, theme))
            TRUE
        },

        .plotPaths = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$pars)) return(FALSE)
            .pl_plot_sem(image$state, self$options, theme, width = image$width, height = image$height)
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
                           "htmtGuide", "vifGuide", "pathsGuide", "structuralGuide", "interpretation", "reproducibility",
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
        .interpretation = function(lang, blocks, loadings, paths, r2, quality, structural = NULL, sm = NULL,
                                   model = NULL) {
            txt <- function(key) .pl_text(lang, "interp", key)
            listed <- function(names, values)
                paste(sprintf("%s (%s)", .pl_short(names), vapply(values, .pl_fmt, "")), collapse = ", ")
            refl <- private$.multiReflective(loadings)
            meas <- character(0)
            if (nrow(refl) == 0) {
                meas <- txt("no_reflective")
            } else {
                low <- refl[refl$loading < 0.708, , drop = FALSE]
                meas <- if (nrow(low))
                    sprintf(txt("low_loadings"), nrow(low), nrow(refl),
                            paste(sprintf("%s %s", .pl_short(low$indicator), vapply(low$loading, .pl_fmt, "")), collapse = ", "))
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
            if (!is.null(rel) && self$options$omegaCfa) {
                both <- rel[!is.na(rel$rhoC) & !is.na(rel$omega), , drop = FALSE]
                if (nrow(both)) {
                    gap <- mean(both$rhoC - both$omega)
                    meas <- c(meas, sprintf(txt(if (self$options$estimator == "pls") "omega_gap_pls" else "omega_gap_plsc"),
                                            .pl_fmt(gap)))
                }
            }
            h <- quality$htmt
            if (!is.null(h)) {
                pair <- paste(h$c1, h$c2, sep = "\u2013")
                high <- !is.na(h$htmt) & h$htmt >= 0.85
                if (any(high)) meas <- c(meas, sprintf(txt("htmt_high"), listed(pair[high], h$htmt[high])))
                else if (!anyNA(h$htmt)) meas <- c(meas, sprintf(txt("htmt_ok"), length(unique(c(h$c1, h$c2)))))
                if (anyNA(h$htmt)) meas <- c(meas, sprintf(txt("htmt_na"), paste(pair[is.na(h$htmt)], collapse = ", ")))
                # With the bootstrap, the test is the upper bound (Henseler et
                # al., 2015), not the estimate.
                # ES: Con bootstrap, la prueba es el límite superior.
                if (!is.null(sm$HTMT)) {
                    up <- vapply(seq_len(nrow(h)), function(k) {
                        r <- sm$HTMT$upper[sm$HTMT$name == paste(model$cmap[[h$c1[k]]], model$cmap[[h$c2[k]]], sep = "~")]
                        if (length(r)) r[1] else NA_real_
                    }, 0)
                    bad <- !is.na(up) & up >= 0.85
                    meas <- c(meas, if (any(bad)) sprintf(txt("htmt_ci_high"), listed(pair[bad], up[bad]))
                                    else sprintf(txt("htmt_ci_ok"), sum(!is.na(up))))
                }
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
                # Non-significant weights: kept when the loading is 0.50 or
                # more (absolute contribution), otherwise candidates for
                # removal on theoretical grounds (Hair et al., 2019).
                # ES: Pesos no significativos: se conservan si la carga es de
                # 0.50 o más; si no, son candidatos a quitarse por teoría.
                if (!is.null(sm$weights)) {
                    ns <- vapply(seq_len(nrow(form)), function(k) {
                        r <- sm$weights[sm$weights$name == paste(model$cmap[[form$construct[k]]], "<~",
                                                                 model$vmap[[form$indicator[k]]]), , drop = FALSE]
                        nrow(r) == 1 && is.finite(r$lower) && r$lower <= 0 && r$upper >= 0
                    }, TRUE)
                    keep <- form[ns & form$loading >= 0.5, , drop = FALSE]
                    drop <- form[ns & form$loading < 0.5, , drop = FALSE]
                    if (nrow(keep)) meas <- c(meas, sprintf(txt("weights_ns_keep"), listed(keep$indicator, keep$loading)))
                    if (nrow(drop)) meas <- c(meas, sprintf(txt("weights_ns_drop"), listed(drop$indicator, drop$loading)))
                    if (!any(ns)) meas <- c(meas, txt("weights_all_sig"))
                }
            }
            k <- which.max(abs(paths$estimate))
            band <- function(x) txt(if (x >= 0.75) "r2_substantial" else if (x >= 0.50) "r2_moderate"
                                    else if (x >= 0.25) "r2_weak" else "r2_very_weak")
            r2_lines <- vapply(seq_len(nrow(r2)), function(i)
                sprintf(txt("r2_line"), r2$construct[i], .pl_fmt(r2$r2[i]), band(r2$r2[i])), "")
            struct <- c(sprintf(txt("strongest"), paths$from[k], paths$to[k], .pl_fmt(paths$estimate[k])))
            if (!is.null(sm)) {
                pct <- format(100 * as.numeric(self$options$ciLevel))
                sig <- vapply(seq_len(nrow(paths)), function(i) {
                    r <- sm$paths[sm$paths$name == paste(model$cmap[[paths$to[i]]], "~", model$cmap[[paths$from[i]]]), ,
                                  drop = FALSE]
                    nrow(r) == 1 && is.finite(r$lower) && (r$lower > 0 || r$upper < 0)
                }, TRUE)
                lab <- paste(paths$from, paths$to, sep = " \u2192 ")
                struct <- c(struct, if (all(sig)) sprintf(txt("paths_all_sig"), nrow(paths), pct)
                                    else sprintf(txt("paths_sig"), sum(sig), nrow(paths), pct,
                                                 listed(lab[!sig], paths$estimate[!sig])))
            }
            struct <- c(struct, r2_lines, txt("r2_note"))
            if (!is.null(structural)) {
                f <- structural$f2
                big <- !is.na(f$f2) & f$f2 >= 0.15
                if (any(big)) struct <- c(struct, sprintf(txt("f2_big"), listed(paste(f$from[big], f$to[big], sep = " \u2192 "), f$f2[big])))
                tiny <- !is.na(f$f2) & f$f2 < 0.02
                if (any(tiny)) struct <- c(struct, sprintf(txt("f2_tiny"), listed(paste(f$from[tiny], f$to[tiny], sep = " \u2192 "), f$f2[tiny])))
                vs <- structural$vif
                hv <- !is.na(vs$vif) & vs$vif >= 3
                struct <- c(struct, if (any(hv)) sprintf(txt("vifs_high"), listed(paste(vs$from[hv], vs$to[hv], sep = " \u2192 "), vs$vif[hv]))
                                    else txt("vifs_ok"))
            }
            html <- paste0(.pl_html_block(meas, txt("title_measurement")),
                           .pl_html_block(struct, txt("title_structural")),
                           .pl_html_block(txt(if (is.null(sm)) "next_steps" else "next_steps_boot"), txt("title_next")))
            self$results$interpretation$setContent(html)
        },

        # Reflective indicators with a negative loading: almost always items
        # worded in the opposite direction that were not recoded. Everything
        # downstream (alpha, rho_C, AVE, HTMT) is distorted until they are, so
        # the report says it as a warning, by name.
        # ES: Indicadores reflectivos con carga negativa: casi siempre ítems
        # redactados en sentido opuesto que no se recodificaron. Todo lo que
        # sigue queda distorsionado hasta corregirlo, así que el informe lo
        # advierte, con nombres.
        .negativeLoadings = function(loadings, txt) {
            refl <- private$.multiReflective(loadings)
            neg <- refl[!is.na(refl$loading) & refl$loading < 0, , drop = FALSE]
            if (!nrow(neg)) return(character(0))
            sprintf(txt("notices", "negative_loadings"),
                    paste(sprintf("%s (%s, %s)", .pl_short(neg$indicator), neg$construct,
                                  vapply(neg$loading, .pl_fmt, "")), collapse = "; "))
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

        .keySummary = function(lang, blocks, vars, paths, prep, loadings, r2, admissible, quality, sm = NULL,
                               model = NULL) {
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
            uncertain <- txt("no_inference")
            if (!is.null(sm)) {
                sig <- vapply(seq_len(nrow(paths)), function(i) {
                    rr <- sm$paths[sm$paths$name == paste(model$cmap[[paths$to[i]]], "~", model$cmap[[paths$from[i]]]), ,
                                   drop = FALSE]
                    nrow(rr) == 1 && is.finite(rr$lower) && (rr$lower > 0 || rr$upper < 0)
                }, TRUE)
                pct <- format(100 * as.numeric(self$options$ciLevel))
                findings <- c(findings, sprintf(txt("paths_sig"), sum(sig), nrow(paths), pct))
                uncertain <- sprintf(txt("boot_basis"), sm$admissible, sm$runs, as.integer(self$options$bootSeed))
            }
            warnings <- c(if (!admissible) txt("admissible_no"),
                          private$.negativeLoadings(loadings, function(section, key) .pl_text(lang, section, key)))
            section <- function(title, items)
                paste0("<p><strong>", .pl_html_escape(title), "</strong></p>",
                       .pl_html_list(if (length(items)) items else txt("none")))
            self$results$keySummary$setContent(paste0(
                section(txt("findings"), findings), section(txt("uncertain"), uncertain),
                section(txt("warnings"), warnings)))
        },

        .choices = function(lang, prep, boot = NULL) {
            if (!self$options$showChoicesGuide) return()
            txt <- function(key) .pl_text(lang, "choices", key)
            scheme <- c(path = "path", centroid = "centroid", factorial = "factorial")[[self$options$innerScheme]]
            paras <- c(txt(self$options$estimator), sprintf(txt("scheme"), scheme), txt(self$options$indicatorScale),
                       if (self$options$missing == "mean") sprintf(txt("mean"), prep$n_replaced)
                       else sprintf(txt("listwise"), prep$n_used, prep$n_total),
                       if (self$options$bootstrap)
                           sprintf(txt("bootstrap"), as.integer(self$options$bootReps), as.integer(self$options$bootSeed),
                                   format(100 * as.numeric(self$options$ciLevel)))
                       else txt("no_bootstrap"))
            self$results$choicesGuide$setContent(.pl_html_block(paras))
        },

        .guides = function(lang) {
            if (!self$options$showGuides) return()
            txt <- function(key) .pl_text(lang, "guide", key)
            self$results$modelGuide$setContent(.pl_html_block(txt("model")))
            self$results$measurementGuide$setContent(.pl_html_block(txt("measurement")))
            self$results$reliabilityGuide$setContent(.pl_html_block(c(txt("reliability"), txt("reliability_omega"))))
            self$results$htmtGuide$setContent(.pl_html_block(txt("htmt")))
            self$results$vifGuide$setContent(.pl_html_block(txt("vif")))
            self$results$structuralGuide$setContent(.pl_html_block(c(txt("f2"), txt("vif_struct"), txt("effects"))))
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
            # Versions go into the code as a comment, not into the report
            # text (Archie's request).
            # ES: Las versiones van como comentario del código, no en el
            # texto del informe (a pedido de Archie).
            self$results$reproducibility$setContent(.pl_html_block(txt("code")))
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
                sprintf("# PLSsemLab %s, cSEM %s, %s", version("PLSsemLab"), version("cSEM"), R.version.string),
                "# Model as defined in jamovi:",
                paste0("#   ", model$readable),
                "# Internal names:", map_c, map_v,
                sprintf("d <- data[, c(%s)]", paste(q(names(model$vmap)), collapse = ", ")),
                sprintf("names(d) <- c(%s)", paste(q(unname(model$vmap)), collapse = ", ")),
                missing_line,
                if (self$options$indicatorScale == "ordinal")
                    "for (v in names(d)) d[[v]] <- ordered(d[[v]])  # ordinal: polychoric correlations",
                "model <- \"",
                model$syntax,
                "\"",
                sprintf("fit <- cSEM::csem(d, model, .PLS_modes = %s, .disattenuate = %s,", modes,
                        if (self$options$estimator == "plsc") "TRUE" else "FALSE"),
                sprintf("                  .PLS_weight_scheme_inner = \"%s\")", self$options$innerScheme),
                "cSEM::summarize(fit)")
            if (self$options$bootstrap) {
                # The same blocks and seeds as the module: up to 200 resamples
                # this is a single cSEM call with the seed.
                # ES: Los mismos bloques y semillas que el módulo.
                code <- c(code, "",
                    sprintf("# Bootstrap: %d resamples in blocks of %d; block k uses seed %d + k - 1",
                            as.integer(self$options$bootReps), .PL_BLOCK, as.integer(self$options$bootSeed)),
                    sprintf("R <- %d; seed <- %d", as.integer(self$options$bootReps), as.integer(self$options$bootSeed)),
                    sprintf("parts <- lapply(seq_len(ceiling(R / %1$d)), function(k) cSEM::resamplecSEMResults(fit, .R = min(%1$d, R - (k - 1) * %1$d), .seed = seed + k - 1, .handle_inadmissibles = \"drop\"))", .PL_BLOCK),
                    "boot <- parts[[1]]",
                    "q <- setdiff(names(boot$Estimates$Estimates_resample$Estimates1), c(\"Indicator_correlation\", \"Exo_construct_correlation\"))",
                    "boot$Estimates$Estimates_resample$Estimates1 <- boot$Estimates$Estimates_resample$Estimates1[q]",
                    "for (n in q) boot$Estimates$Estimates_resample$Estimates1[[n]]$Resampled <- do.call(rbind, lapply(parts, function(o) o$Estimates$Estimates_resample$Estimates1[[n]]$Resampled))",
                    "boot$Information$Information_resample$Number_of_admissibles <- sum(sapply(parts, function(o) o$Information$Information_resample$Number_of_admissibles))",
                    sprintf("cSEM::summarize(boot, .alpha = %s, .ci = \"CI_percentile\")", format(1 - as.numeric(self$options$ciLevel))),
                    "# Intervals of R2, reliability, AVE, HTMT and VIF: the same resampling with .user_funs (see R/bootstrap.R in PLSsemLab)")
            }
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
            if (any(types == "reflective") && self$options$omegaCfa) {
                code <- c(code, "",
                    "# McDonald's omega from a one-factor CFA of each reflective construct (3+ indicators)",
                    "omega <- function(x, ordinal) {",
                    "    f <- lavaan::cfa(paste(\"F =~\", paste(names(x), collapse = \" + \")), data = x, std.lv = TRUE,",
                    "                     ordered = if (ordinal) names(x) else NULL)",
                    "    l <- lavaan::lavInspect(f, \"std\")$lambda[, 1]",
                    "    sum(l)^2 / (sum(l)^2 + sum(1 - l^2))",
                    "}")
                for (b in blocks) if (b$type == "reflective" && length(b$vars) >= 3)
                    code <- c(code, sprintf("omega(d[, c(%s)], %s)  # %s", paste(q(unname(model$vmap[b$vars])), collapse = ", "),
                                            if (self$options$indicatorScale == "ordinal") "TRUE" else "FALSE",
                                            unname(model$cmap[[b$label]])))
            }
            if (any(types == "formative")) {
                # VIF from the indicator correlations the model used (polychoric
                # when ordinal); with continuous indicators it equals
                # cSEM::calculateVIFModeB(fit).
                # ES: VIF desde las correlaciones que usó el modelo.
                code <- c(code, "", "# Collinearity of formative indicators: diag(solve(R)) of each block")
                for (b in blocks) if (b$type == "formative" && length(b$vars) >= 2)
                    code <- c(code, sprintf("diag(solve(fit$Estimates$Indicator_VCV[c(%1$s), c(%1$s)]))  # %2$s",
                                            paste(q(unname(model$vmap[b$vars])), collapse = ", "),
                                            unname(model$cmap[[b$label]])))
            }
            esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
            self$results$reproducibilityCode$setContent(paste0(
                "<pre style=\"white-space: pre-wrap; font-family: monospace; font-size: 0.85em;\">",
                esc(paste(code, collapse = "\n")), "</pre>"))
        }
    )
)
