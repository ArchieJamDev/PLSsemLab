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

        # Checkpoint inside a long cSEM resampling call (every resample,
        # throttled in .pl_with_ticks()): without flushing the results, so
        # it is cheap; it stops the run when jamovi asks for a restart.
        # ES: Checkpoint dentro de una llamada larga de remuestreo de cSEM
        # (cada remuestra, con freno en .pl_with_ticks()): sin enviar los
        # resultados, así que es barato; detiene la ejecución si jamovi pide
        # reiniciar.
        .tick = function() private$.checkpoint(flush = FALSE),

        # Parts of the report that do not apply to this model, found by
        # .init(): list(section, before, key, args). .run() explains each
        # one with a jamovi notice placed before its table or plot, which
        # stays in place (jamovi's review: never hide, explain).
        # ES: Partes del informe que no aplican a este modelo, halladas por
        # .init(); .run() explica cada una con un aviso de jamovi delante de
        # su tabla o gráfico, que se queda en su sitio.
        .na = NULL,

        # ---------------------------------------------------------------------
        # Lay out the rows that depend only on the options, so every table
        # keeps its shape before the results arrive (jamovi's review of
        # MokkenLab).
        # ES: Definir las filas que dependen solo de las opciones, para que
        # cada tabla conserve su forma antes de los resultados.
        # ---------------------------------------------------------------------
        .init = function() {
            blocks <- .pl_constructs(self$options)$blocks
            hocs <- .pl_hocs(self$options)
            paths <- .pl_paths(self$options$structural, .pl_structural_labels(blocks, hocs))
            private$.na <- list()
            private$.initMeasurement(blocks, hocs, paths)
            private$.initStructural(blocks, paths)
            private$.initOptional(blocks, paths)
            private$.initVisibility(blocks, hocs, paths)
        },

        # Records a part of the report that does not apply (see .na).
        # ES: Registra una parte del informe que no aplica (ver .na).
        .notApply = function(section, before, key, args = list())
            private$.na[[length(private$.na) + 1]] <- list(section = section, before = before, key = key, args = args),

        # Rows of the measurement-model tables, higher-order constructs
        # included, known from the boxes.
        # ES: Filas de las tablas del modelo de medida, conocidas por las cajas.
        .initMeasurement = function(blocks, hocs, paths) {
            labels <- vapply(blocks, `[[`, "", "label")
            for (i in seq_along(blocks)) {
                self$results$measurementSection$model$addRow(rowKey = i, values = list(construct = blocks[[i]]$label))
                for (v in blocks[[i]]$vars)
                    self$results$measurementSection$measurement$addRow(rowKey = paste(i, v, sep = "::"),
                                                    values = list(construct = blocks[[i]]$label, indicator = v))
            }
            # Higher-order constructs (phase 4): a row in the model summary
            # and one per lower-order construct in their measurement table.
            # ES: Constructos de orden superior (fase 4).
            for (j in seq_along(hocs)) {
                h <- hocs[[j]]
                self$results$measurementSection$model$addRow(rowKey = paste0("hoc", j), values = list(construct = h$label))
                for (cpt in h$components)
                    self$results$measurementSection$hocMeasurement$addRow(rowKey = paste(h$label, cpt, sep = "::"),
                                                       values = list(hoc = h$label, component = cpt))
                if (h$type == "reflective")
                    self$results$measurementSection$hocReliability$addRow(rowKey = h$label, values = list(hoc = h$label))
            }
            hp <- if (length(hocs)) .pl_hoc_htmt_pairs(hocs, blocks, paths, NULL) else NULL
            for (k in seq_len(if (is.null(hp)) 0 else nrow(hp)))
                self$results$measurementSection$hocHtmt$addRow(rowKey = paste(hp$hoc[k], hp$other[k], sep = "::"),
                                            values = list(c1 = hp$hoc[k], sep = "\u2013", c2 = hp$other[k]))
            # A feature the model does not use is hidden (see .initVisibility);
            # one that applies but cannot be computed keeps its place and a notice.
            # ES: Una función que el modelo no usa se oculta; una que aplica pero
            # no se puede calcular conserva su sitio y un aviso.
            if (length(hocs) && !any(vapply(hocs, function(h) h$type == "reflective", TRUE)))
                private$.notApply("measurementSection", "hocReliability", "na_hoc_reliability")
            if (length(hocs) && is.null(hp))
                private$.notApply("measurementSection", "hocHtmt", "na_hoc_htmt")

            # Measurement-model quality (phase 2): rows follow the constructs
            # in the boxes; a table that does not apply to the model (no
            # reflective pair, no formative construct) stays in place, and a
            # notice says why (see .na).
            # ES: Calidad del modelo de medida: las filas siguen a los
            # constructos de las cajas; una tabla que no aplica al modelo se
            # queda en su sitio y un aviso explica por qué.
            refl <- labels[vapply(blocks, function(b) b$type == "reflective", TRUE)]
            for (r in refl)
                self$results$measurementSection$reliability$addRow(rowKey = r, values = list(construct = r))
            if (length(refl) >= 2) {
                pairs <- utils::combn(refl, 2)
                for (k in seq_len(ncol(pairs)))
                    self$results$measurementSection$htmt$addRow(rowKey = paste(pairs[, k], collapse = "::"),
                                             values = list(c1 = pairs[1, k], sep = "\u2013", c2 = pairs[2, k]))
            }
            for (i in seq_along(refl)) {
                self$results$measurementSection$fornell$addColumn(name = paste0("c", i), title = refl[i], type = "number")
                self$results$measurementSection$fornell$addRow(rowKey = refl[i], values = list(construct = refl[i]))
            }
            for (i in seq_along(blocks)) {
                if (blocks[[i]]$type != "formative") next
                for (v in blocks[[i]]$vars)
                    self$results$measurementSection$vif$addRow(rowKey = paste(i, v, sep = "::"),
                                            values = list(construct = blocks[[i]]$label, indicator = v))
            }
            # A single reflective construct has no HTMT: that table stays with its notice.
            # ES: Con un solo constructo reflectivo no hay HTMT: esa tabla conserva su aviso.
            if (length(refl) < 2)
                private$.notApply("measurementSection", "htmt", "na_htmt", list(length(refl)))
        },

        # Shows the optional tables and guides only when the model uses them
        # (jamovi's review: the layout must not carry empty sections). Called
        # from .init(), so the layout is stable from the first frame.
        # ES: Muestra las tablas y guías opcionales solo cuando el modelo las
        # usa (revisión de jamovi). Se llama desde .init(), así el diseño es
        # estable desde el primer fotograma.
        .initVisibility = function(blocks, hocs, paths) {
            ms <- self$results$measurementSection
            ss <- self$results$structuralSection
            refl <- vapply(blocks, function(b) b$type == "reflective", TRUE)
            formative <- vapply(blocks, function(b) b$type == "formative", TRUE)
            has_hoc <- length(hocs) > 0
            has_chain <- nrow(.pl_mediation_chains(paths)) > 0
            has_inter <- any(grepl(.PL_X, paths$from, fixed = TRUE))
            ms$hocGuide$setVisible(has_hoc)
            ms$hocMeasurement$setVisible(has_hoc)
            ms$hocReliability$setVisible(has_hoc)
            ms$hocHtmt$setVisible(has_hoc)
            ms$reliabilityGuide$setVisible(any(refl))
            ms$reliability$setVisible(any(refl))
            ms$vifGuide$setVisible(any(formative))
            ms$vif$setVisible(any(formative))
            ss$mediationGuide$setVisible(has_chain)
            ss$mediation$setVisible(has_chain)
            ss$moderationGuide$setVisible(has_inter)
            ss$simpleSlopes$setVisible(has_inter)
            ss$moderationPlots$setVisible(has_inter)
        },

        # Rows of the structural tables: paths, R², f², VIF, effects,
        # mediation chains and simple slopes.
        # ES: Filas de las tablas estructurales.
        .initStructural = function(blocks, paths) {
            for (k in seq_len(nrow(paths)))
                self$results$structuralSection$paths$addRow(rowKey = k, values = list(from = paths$from[k], arrow = "\u2192",
                                                                    to = paths$to[k]))
            for (to in unique(paths$to))
                self$results$structuralSection$r2$addRow(rowKey = to, values = list(construct = to))
            # Structural tables (phase 3): f² and VIF per path, effects per
            # pair of constructs joined by at least one path.
            # ES: Tablas estructurales (fase 3): f² y VIF por ruta, efectos
            # por par de constructos unidos por al menos una ruta.
            for (k in seq_len(nrow(paths))) {
                row <- list(from = paths$from[k], arrow = "\u2192", to = paths$to[k])
                self$results$structuralSection$f2$addRow(rowKey = k, values = row)
                self$results$structuralSection$vifStruct$addRow(rowKey = k, values = row)
            }
            pairs_eff <- .pl_effect_pairs(paths)
            for (k in seq_len(nrow(pairs_eff)))
                self$results$structuralSection$effects$addRow(rowKey = paste(pairs_eff$from[k], pairs_eff$to[k], sep = "::"),
                                            values = list(from = pairs_eff$from[k], arrow = "\u2192",
                                                          to = pairs_eff$to[k]))

            # Mediation (phase 6): one row per specific indirect effect.
            # ES: Mediación (fase 6): una fila por efecto indirecto específico.
            chains <- .pl_mediation_chains(paths)
            for (k in seq_len(nrow(chains)))
                self$results$structuralSection$mediation$addRow(rowKey = chains$label[k], values = list(chain = chains$label[k]))

            # Moderation (phase 4): three rows (moderator at -1 SD, the mean,
            # +1 SD) and one plot per interaction.
            # ES: Moderación (fase 4): tres filas y un gráfico por interacción.
            inter <- paths[grepl(.PL_X, paths$from, fixed = TRUE), , drop = FALSE]
            lang <- .pl_lang(self$options)
            lv <- c(.pl_text(lang, "moderation", "minus_sd"), .pl_text(lang, "moderation", "mean"),
                    .pl_text(lang, "moderation", "plus_sd"))
            for (k in seq_len(nrow(inter))) {
                parts <- strsplit(inter$from[k], .PL_X, fixed = TRUE)[[1]]
                for (j in 1:3)
                    self$results$structuralSection$simpleSlopes$addRow(rowKey = paste(k, j, sep = "::"), values = list(
                        term = paste(inter$from[k], "\u2192", inter$to[k]),
                        effect = paste(parts[1], "\u2192", inter$to[k]),
                        level = sprintf(lv[j], parts[2])))
                self$results$structuralSection$moderationPlots$addItem(key = paste(inter$from[k], "\u2192", inter$to[k]))
            }
        },

        # Rows of the optional sections: overall fit, prediction, IPMA and
        # multigroup analysis.
        # ES: Filas de las secciones opcionales.
        .initOptional = function(blocks, paths) {
            labels <- vapply(blocks, `[[`, "", "label")
            # overall fit: 4 discrepancies x estimated and saturated model
            # ES: ajuste global: 4 discrepancias x modelo estimado y saturado
            ltxt <- function(key) .pl_text(.pl_lang(self$options), "labels", key)
            for (mdl in c("estimated", "saturated"))
                for (m in names(.PL_FIT))
                    self$results$fitSection$fit$addRow(rowKey = paste(mdl, m, sep = "::"),
                                            values = list(model = ltxt(paste0("fit_", mdl)), measure = .PL_FIT[[m]]))
            self$results$fitSection$fit$getColumn("critical")$setTitle(
                sprintf(ltxt("fit_critical"), format(100 * as.numeric(self$options$fitLevel))))
            # Prediction (phase 6): one row per indicator of an endogenous
            # construct; CVPAT: all endogenous constructs and each one, against
            # IA and LM.
            # ES: Predicción (fase 6): una fila por indicador de un constructo
            # endógeno; CVPAT: todos y cada uno, frente a IA y LM.
            ptxt <- function(key) .pl_text(.pl_lang(self$options), "labels", key)
            endo <- unique(paths$to)
            for (cn in endo) {
                b <- Filter(function(x) identical(x$label, cn), blocks)
                for (v in if (length(b)) b[[1]]$vars else character(0))
                    self$results$predictSection$predict$addRow(rowKey = paste(cn, v, sep = "::"), values = list(construct = cn, indicator = v))
            }
            alt_on <- nrow(.pl_cvpat_drop(self$options$cvpatDrop, paths)) > 0
            bm_on <- self$options$predictBenchmark != "none"
            for (col in c("rmseBench", "maeBench")) {
                cc <- self$results$predictSection$predict$getColumn(col)
                cc$setVisible(bm_on)
                if (bm_on) cc$setTitle(ptxt(paste0("approach_", self$options$predictBenchmark)))
            }
            for (cn in c("", endo)) for (bn in c("IA", "LM", if (alt_on) "ALT", if (bm_on) "BM"))
                self$results$predictSection$cvpat$addRow(rowKey = paste(cn, bn, sep = "::"),
                                          values = list(construct = if (nzchar(cn)) cn else ptxt("cvpat_all"),
                                                        bench = if (bn == "BM") ptxt(paste0("approach_", self$options$predictBenchmark))
                                                                else ptxt(paste0("bench_", bn))))

            # Model comparison (1.2.0): per endogenous construct of the main
            # model, R², adjusted R² and the information criteria; one row of
            # SRMR for the whole model; one row per path of the main model.
            # ES: Comparación de modelos (1.2.0): por constructo endógeno del
            # modelo principal, R², R² ajustado y los criterios de
            # información; una fila de SRMR; una fila por ruta.
            if (self$options$compareRun) {
                ct2 <- self$results$compareSection$compare
                for (cn in endo) for (cr in .PL_COMPARE)
                    ct2$addRow(rowKey = paste(cn, cr, sep = "::"), values = list(construct = cn, criterion = ptxt(paste0("crit_", cr))))
                ct2$addRow(rowKey = "::SRMR", values = list(construct = ptxt("cvpat_all"), criterion = "SRMR"))
                for (k in seq_len(nrow(paths)))
                    self$results$compareSection$comparePaths$addRow(rowKey = paste(paths$from[k], paths$to[k], sep = "::"),
                        values = list(from = paths$from[k], arrow = "\u2192", to = paths$to[k]))
            }

            # IPMA (phase 6): each endogenous construct and every construct
            # with an effect on it; one map per target.
            # ES: IPMA (fase 6): cada constructo endógeno y cada constructo con
            # efecto sobre él; un mapa por objetivo.
            ipairs <- .pl_effect_pairs(paths)
            for (tg in unique(paths$to[!grepl(.PL_X, paths$from, fixed = TRUE)])) {
                k <- which(ipairs$to == tg)
                for (j in k)
                    self$results$ipmaSection$ipmaTable$addRow(rowKey = paste(tg, ipairs$from[j], sep = "::"),
                                                  values = list(target = tg, construct = ipairs$from[j]))
                if (length(k)) self$results$ipmaSection$ipmaPlots$addItem(key = tg)
            }

            # NCA (1.1.0): per outcome (endogenous construct), two rows per
            # condition (CE-FDH, CR-FDH), one combined row, a bottleneck
            # table with a column per condition and one plot per condition.
            # ES: NCA (1.1.0): por resultado, dos filas por condición, una
            # fila combinada, una tabla de cuellos de botella y un gráfico por
            # condición.
            npairs <- .pl_nca_pairs(paths)
            nsec <- self$results$ncaSection
            for (tg in unique(npairs$to)) {
                conds <- npairs$from[npairs$to == tg]
                for (cn in conds) {
                    for (ce in c("ce", "cr"))
                        nsec$ncaTable$addRow(rowKey = paste(tg, cn, ce, sep = "::"),
                                             values = list(target = tg, condition = cn, ceiling = ptxt(paste0("nca_", ce))))
                    nsec$ncaCombined$addRow(rowKey = paste(tg, cn, sep = "::"), values = list(target = tg, condition = cn))
                    if (self$options$ncaPlots) nsec$ncaPlots$addItem(key = paste(cn, "\u2192", tg))
                }
                if (self$options$ncaBottleneck) {
                    bt <- nsec$ncaBottlenecks$addItem(key = tg)
                    bt$setTitle(sprintf(ptxt("nca_bn_title"), tg, ptxt(paste0("nca_", substr(self$options$ncaCeiling, 1, 2)))))
                    for (j in seq_along(conds)) bt$addColumn(name = paste0("c", j), title = conds[j], type = "text")
                    for (lv in .PL_NCA_STEPS) bt$addRow(rowKey = lv, values = list(level = lv))
                }
            }

            # Multigroup analysis (phase 7): one row per group, per pair of
            # groups and construct (MICOM) and per pair and path. With two
            # groups the coefficient columns carry the group names.
            # ES: Análisis multigrupo (fase 7): una fila por grupo, por par y
            # constructo (MICOM) y por par y ruta.
            gv <- self$options$groupVar
            if (self$options$mgaRun && !is.null(gv) && nzchar(gv))
                private$.mgaRows(.pl_mga_levels(self$data[[gv]]), labels, paths, ptxt("mga_vs"))

        },

        # Notices for the parts that do not apply (see .na), placed before
        # their table or plot.
        # ES: Avisos de las partes que no aplican, delante de su tabla o
        # gráfico.
        .notApplyNotices = function(txt) {
            for (x in private$.na)
                .pl_notice_before(self$results$get(x$section), self$options, x$before,
                                  do.call(sprintf, c(list(txt("notapply", x$key)), x$args)))
        },

        .run = function() {
            lang <- .pl_lang(self$options)
            txt <- function(section, key) .pl_text(lang, section, key)

            # 1. Read the boxes.
            # ES: 1. Leer las cajas.
            cons <- .pl_constructs(self$options)
            blocks <- cons$blocks
            hocs <- .pl_hocs(self$options)
            paths <- .pl_paths(self$options$structural, .pl_structural_labels(blocks, hocs))
            if (length(blocks) < 2 || nrow(paths) == 0) {
                private$.showIntro(lang, if (length(blocks) < 2) "need_constructs" else "need_paths")
                return()
            }
            self$results$intro$setVisible(FALSE)
            private$.notApplyNotices(txt)

            # 2-3. Validate, prepare and estimate; 4. the tables of every
            # model; the optional sections; 5. notices and the texts.
            # ES: 2-3. Validar, preparar y estimar; 4. las tablas de todo
            # modelo; las secciones opcionales; 5. avisos y textos.
            e <- private$.estimate(blocks, hocs, paths, lang, txt)
            t <- private$.fillTables(e, blocks, paths, hocs, txt)
            t$quality$mardia <- private$.fillMardia(e, txt)
            private$.saveScores(e, blocks, hocs)
            o <- private$.optionalSections(e, t, blocks, paths, hocs, txt)
            private$.reportNotices(e, t, o, cons, blocks, hocs, txt)
            private$.interpretation(lang, blocks, t$loadings, t$path_tab, t$r2_tab, t$quality, t$structural,
                                    o$boot$summary, e$smodel, o$mod, t$hocq, o$omf, o$med, o$prd, o$ipm, o$mga, o$nca, o$cmp)
            private$.keySummary(lang, blocks, e$vars, paths, e$prep, t$loadings, t$r2_tab, e$admissible, t$quality,
                                o$boot$summary, e$smodel, t$hocq, o$omf, o$med, o$prd, o$mga, o$nca, o$cmp)
            private$.choices(lang, e$prep, o$boot)
            private$.guides(lang)
            private$.reproducibility(lang, e$smodel, e$prep, blocks, paths)
        },

        # ---------------------------------------------------------------------
        # Steps 2-3 of .run(): validation (Category A: reject with the real
        # reason), data preparation and estimation, with stage 2 for
        # higher-order constructs. Returns everything the later steps read.
        # ES: Pasos 2-3 de .run(): validación (Categoría A), preparación de
        # los datos y estimación, con la etapa 2 del orden superior.
        # ---------------------------------------------------------------------
        .estimate = function(blocks, hocs, paths, lang, txt) {
            # 2. Validate and prepare (Category A).
            # ES: 2. Validar y preparar (Categoría A).
            # Higher-order constructs (phase 4): checked first; the
            # first-order model is then validated with the stage-1 paths,
            # where each higher-order construct is replaced by its
            # lower-order constructs.
            # ES: Constructos de orden superior (fase 4): se revisan primero;
            # el modelo de primer orden se valida con las rutas de la etapa 1.
            errors <- .pl_hoc_validate(blocks, hocs, paths, self$options, lang)
            if (length(errors))
                jmvcore::reject(paste(errors, collapse = "\n"))
            approach <- if (is.null(self$options$hocApproach)) "disjoint" else self$options$hocApproach
            paths1 <- if (length(hocs)) .pl_stage1_paths(paths, hocs) else paths
            errors <- .pl_validate_model(blocks, paths1, lang)
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
            model <- .pl_build_syntax(blocks, paths1, self$options$estimator)
            # Weighting approach (1.2.0): combinations cSEM cannot estimate
            # stop here with the reason (Category A).
            # ES: Enfoque de ponderación (1.2.0): las combinaciones que cSEM
            # no puede estimar se detienen aquí con la razón (Categoría A).
            ar <- .pl_approach_reject(self$options, blocks, hocs, isTRUE(model$nonlinear))
            if (!is.null(ar))
                jmvcore::reject(sprintf(txt("errors", ar), txt("labels", paste0("approach_", self$options$weightApproach))))
            d <- prep$data[, names(model$vmap), drop = FALSE]
            names(d) <- unname(model$vmap)
            # Ordinal indicators go to cSEM as ordered factors, which makes it
            # use polychoric correlations (ordinal PLS).
            # ES: Los ordinales van a cSEM como factores ordenados, con lo que
            # usa correlaciones policóricas (PLS ordinal).
            categories <- stats::setNames(vapply(d, function(x) length(unique(x)), 0), names(model$vmap))
            if (ordinal)
                for (v in names(d)) d[[v]] <- ordered(d[[v]])
            fit <- if (length(hocs) && approach == "csem") {
                # cSEM's routine: its own stage 1 (the paths among the
                # dimensions it adds are read back into paths1)
                # ES: la rutina de cSEM: su propia etapa 1
                c1 <- .pl_capture(.pl_csem_stage1(d, blocks, hocs, paths, model, self$options))
                if (is.null(c1$error)) {
                    paths1 <- c1$value$paths1
                    model$csem_syntax <- c1$value$syntax
                }
                list(value = c1$value$fit1, error = c1$error, warnings = c1$warnings)
            } else .pl_capture(do.call(cSEM::csem, c(list(.data = d, .model = model$syntax),
                                                       .pl_csem_args(self$options, model$modes))))
            if (!is.null(fit$error))
                jmvcore::reject(sprintf(txt("errors", "estimation"), fit$error))
            est <- fit$value$Estimates
            admissible <- !any(unlist(cSEM::verify(fit$value)))

            # Stage 2 of a higher-order model: the structural model is read
            # from it; smodel adds the higher-order names to the maps.
            # ES: Etapa 2 de un modelo de orden superior: el modelo
            # estructural se lee de ella; smodel añade los nombres de orden
            # superior a los mapas.
            sfit <- fit$value
            smodel <- model
            if (length(hocs)) {
                m2 <- .pl_stage2_model(hocs, paths, model$cmap, approach, model)
                fit2 <- .pl_capture(.pl_stage2_fit(est$Construct_scores, m2, self$options, fit$value$Information$Data))
                if (!is.null(fit2$error))
                    jmvcore::reject(sprintf(txt("errors", "estimation"), fit2$error))
                sfit <- fit2$value
                smodel$cmap <- c(m2$cmap2, model$cmap[setdiff(names(model$cmap), names(m2$cmap2))])
                smodel$mcmap <- model$cmap
                smodel$m2 <- m2
                smodel$hocs <- hocs
                smodel$blocks <- blocks
                admissible <- admissible && !any(unlist(cSEM::verify(sfit)))
            }
            sest <- sfit$Estimates

            list(approach = approach, paths1 = paths1, model = model, d = d, categories = categories,
                 ordinal = ordinal, fit = fit, est = est, admissible = admissible, sfit = sfit,
                 smodel = smodel, prep = prep, vars = vars)
        },

        # ---------------------------------------------------------------------
        # Step 4 of .run(): the tables every model has.
        # ES: Paso 4 de .run(): las tablas que tiene todo modelo.
        # ---------------------------------------------------------------------
        .fillTables = function(e, blocks, paths, hocs, txt) {
            fit <- e$fit
            model <- e$model
            smodel <- e$smodel
            sest <- e$sfit$Estimates
            private$.fillModel(blocks, paths, txt, hocs)
            loadings <- private$.fillMeasurement(blocks, model, e$est)
            path_tab <- private$.fillPaths(paths, smodel, sest)
            r2_tab <- private$.fillR2(paths, smodel, sest)
            quality <- .pl_measurement_quality(fit$value, blocks, e$paths1, model, e$d, self$options)
            private$.fillReliability(quality, txt)
            private$.fillHtmt(quality, txt)
            private$.fillFornell(quality, txt)
            private$.fillVif(quality, txt)
            hocq <- if (length(hocs)) private$.fillHoc(e$sfit, hocs, smodel, txt, paths) else NULL
            structural <- private$.fillStructural(e$sfit, paths, smodel, txt)

            list(loadings = loadings, path_tab = path_tab, r2_tab = r2_tab, quality = quality, hocq = hocq,
                 structural = structural)
        },

        # ---------------------------------------------------------------------
        # The optional sections, each only when its box is ticked: bootstrap,
        # mediation and moderation, overall fit, IPMA, prediction,
        # multigroup; then the plots' data.
        # ES: Las secciones opcionales, cada una solo con su casilla marcada;
        # luego los datos de los gráficos.
        # ---------------------------------------------------------------------
        .optionalSections = function(e, t, blocks, paths, hocs, txt) {
            fit <- e$fit
            model <- e$model
            smodel <- e$smodel
            sfit <- e$sfit
            d <- e$d
            admissible <- e$admissible
            # Bootstrap (phase 3): only when "Run bootstrap" is ticked.
            # ES: Bootstrap (fase 3): solo con "Ejecutar bootstrap" marcado.
            boot <- NULL
            if (self$options$bootstrap) {
                boot <- private$.bootstrap(fit$value, blocks, model, d, txt, smodel, paths)
                if (!is.null(boot$summary)) {
                    private$.fillBootstrap(boot$summary, blocks, paths, smodel, txt)
                    if (length(hocs)) private$.fillHocBootstrap(boot$summary, hocs, smodel, txt)
                }
            }
            med <- private$.fillMediation(sfit, paths, smodel, boot$summary, txt)
            mod <- if (isTRUE(model$nonlinear))
                private$.fillModeration(sfit, paths, smodel, boot$summary, txt) else NULL

            # Overall fit (phase 5): only when "Run overall fit test" is ticked.
            # ES: Ajuste global (fase 5): solo con su casilla marcada.
            omf <- if (self$options$fitTest)
                private$.fitTest(fit$value, model, d, hocs, admissible, txt) else NULL
            ipm <- if (self$options$ipma)
                private$.fillIpma(fit$value, model, d, blocks, paths, hocs, txt, boot$summary) else NULL
            nca <- if (self$options$ncaRun)
                private$.fillNca(fit$value, model, d, blocks, paths, hocs, txt, t$path_tab, boot$summary) else NULL
            prd <- if (self$options$predictRun)
                private$.prediction(fit$value, model, d, e$prep$raw[, names(model$vmap), drop = FALSE], blocks, paths, hocs, admissible, txt) else NULL
            cmp <- if (self$options$compareRun)
                private$.compareModels(fit$value, model, d, blocks, paths, hocs, txt) else NULL
            gv <- self$options$groupVar
            mga <- if (self$options$mgaRun && !is.null(gv) && nzchar(gv))
                private$.multigroup(model, d, blocks, paths, hocs, txt) else NULL
            private$.prepareDiagram(blocks, t$loadings, t$path_tab, t$r2_tab, boot$summary, smodel, t$hocq)
            private$.preparePlots(t$loadings, t$quality, boot$summary, model, txt)

            list(boot = boot, med = med, mod = mod, omf = omf, ipm = ipm, nca = nca, prd = prd, mga = mga, cmp = cmp)
        },

        # ---------------------------------------------------------------------
        # Step 5 of .run(): cSEM's messages and the data notes as jamovi
        # notices (warnings and information).
        # ES: Paso 5 de .run(): los mensajes de cSEM y las notas de los datos
        # como avisos de jamovi.
        # ---------------------------------------------------------------------
        .reportNotices = function(e, t, o, cons, blocks, hocs, txt) {
            fit <- e$fit
            prep <- e$prep
            boot <- o$boot
            omf <- o$omf
            prd <- o$prd
            mga <- o$mga
            model <- e$model
            ordinal <- e$ordinal
            categories <- e$categories
            approach <- e$approach
            warn <- c(vapply(cons$empty, function(x) sprintf(txt("notices", "empty_construct"), x), ""),
                      unlist(lapply(blocks, function(b)
                          if (b$type == "reflective" && length(b$vars) == 1)
                              sprintf(txt("notices", "single_reflective"), b$label))),
                      fit$warnings, t$quality$warnings,
                      private$.negativeLoadings(t$loadings, txt),
                      if (self$options$bootstrap && is.null(boot$summary))
                          sprintf(txt("boot", "failed"), paste(boot$error, collapse = "; ")),
                      if (!is.null(boot$summary) && length(boot$error))
                          sprintf(txt("boot", "some_failed"), paste(boot$error, collapse = "; ")),
                      if (!is.null(omf$error)) sprintf(txt("notices", "fit_failed"), paste(omf$error, collapse = "; ")),
                      if (!is.null(prd$error)) sprintf(txt("notices", "predict_failed"), paste(prd$error, collapse = "; ")),
                      if (isTRUE(prd$inadmissible > 0)) sprintf(txt("notices", "predict_inadmissible_folds"), prd$inadmissible),
                      if (!is.null(mga$error)) sprintf(txt("notices", "mga_failed"), paste(mga$error, collapse = "; ")),
                      omf$warnings, mga$warnings)
            if (!e$admissible) {
                failed <- c(names(which(unlist(cSEM::verify(fit$value)))),
                            if (length(hocs)) names(which(unlist(cSEM::verify(e$sfit)))))
                warn <- c(warn, sprintf(txt("notices", "inadmissible"), paste(failed, collapse = ", ")))
            }
            info <- if (length(hocs)) sprintf(txt("notices", if (approach == "csem") "hoc_csem" else "hoc_two_stage"),
                                              paste(vapply(hocs, `[[`, "", "label"), collapse = ", ")) else character(0)
            if (prep$n_dropped > 0)
                info <- c(info, sprintf(txt("notices", "dropped"), prep$n_dropped, prep$n_used))
            if (prep$n_replaced > 0)
                info <- c(info, sprintf(txt("notices", "replaced"), prep$n_replaced, prep$n_cases_replaced))
            if (isTRUE(mga$n_missing > 0))
                info <- c(info, sprintf(txt("notices", "mga_missing_group"), mga$n_missing, self$options$groupVar))

            few <- names(categories)[categories <= 5]
            many <- names(categories)[categories > 10]
            if (!ordinal && length(few))
                info <- c(info, sprintf(txt("notices", "few_categories"), paste(few, collapse = ", ")))
            if (ordinal && length(many))
                warn <- c(warn, sprintf(txt("notices", "many_categories"), paste(many, collapse = ", ")))
            types <- vapply(blocks, `[[`, "", "type")
            if (isTRUE(model$nonlinear) && any(types == "reflective"))
                info <- c(info, txt("notices", "rhoc_loadings"))
            if (!any(types == "reflective"))
                info <- c(info, txt("notices", "no_reflective_quality"))
            else if (sum(types == "reflective") == 1)
                info <- c(info, txt("notices", "one_reflective_htmt"))
            .pl_insert_notices(self$results, self$options, info, jmvcore::NoticeType$INFO, ".plssemInfo")
            .pl_insert_notices(self$results, self$options, warn)

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
            table <- self$results$measurementSection$reliability
            if (self$options$weightApproach != "pls")
                table$setNote("rhoA", sprintf(txt("footnotes", "rho_a_approach"),
                                              txt("labels", paste0("approach_", self$options$weightApproach))))
            else if (self$options$estimator == "pls")
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
            table <- self$results$measurementSection$htmt
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
            table <- self$results$measurementSection$fornell
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
            table <- self$results$measurementSection$vif
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
                # cSEM's calculatef2() drops the "_temp" suffix of the
                # second-order routine's stage 2; look up by name.
                # ES: calculatef2() quita el sufijo "_temp"; se busca por nombre.
                fm <- f2$value
                to2 <- sub("_temp$", "", to)
                from2 <- sub("_temp$", "", from)
                f <- if (is.null(fm) || !(to2 %in% rownames(fm)) || !(from2 %in% colnames(fm)))
                    NA_real_ else fm[to2, from2]
                out_f2[k] <- if (is.finite(f)) f else NA_real_
                # interaction terms are judged on Kenny's (2018) smaller
                # benchmarks (Hair et al., 2021, ch. 8)
                # ES: las interacciones se juzgan con los umbrales menores de
                # Kenny (2018)
                band <- if (grepl(.PL_X, paths$from[k], fixed = TRUE)) .pl_band_f2_int(out_f2[k])
                        else .pl_band_f2(out_f2[k])
                values <- list(from = paths$from[k], arrow = "\u2192", to = paths$to[k],
                               assessment = if (is.na(band)) txt("labels", "not_computable") else txt("labels", band))
                if (!is.na(out_f2[k])) values$f2 <- out_f2[k]
                self$results$structuralSection$f2$setRow(rowNo = k, values = values)
                if (is.na(out_f2[k]))
                    self$results$structuralSection$f2$addFootnote(rowNo = k, col = "f2", sprintf(txt("footnotes", "failed"),
                        if (is.null(f2$error)) "NaN" else f2$error))
                v <- vifs[paste(to, from, sep = ".")]
                out_vif[k] <- if (length(v) && is.finite(v)) unname(v) else NA_real_
                band <- .pl_band_vif(out_vif[k])
                values <- list(from = paths$from[k], arrow = "\u2192", to = paths$to[k],
                               assessment = if (sum(paths$to == paths$to[k]) == 1) txt("labels", "vif_single_predictor")
                                            else if (is.na(band)) txt("labels", "not_computable") else txt("labels", band))
                if (!is.na(out_vif[k])) values$vif <- out_vif[k]
                self$results$structuralSection$vifStruct$setRow(rowNo = k, values = values)
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
                self$results$structuralSection$effects$setRow(rowKey = paste(from, to, sep = "::"), values = values)
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
        .bootstrap = function(fit, blocks, model, d, txt, smodel = model, paths = NULL) {
            reps <- as.integer(self$options$bootReps)
            seed <- as.integer(self$options$bootSeed)
            level <- as.numeric(self$options$ciLevel)
            status <- self$results$bootStatus
            status$setVisible(TRUE)
            spec <- .pl_boot_spec(blocks, model, fit, self$options, smodel, paths)
            key <- list("plssem-boot-3", self$options$weightApproach, self$options$resampleMethod, model$syntax, model$modes, smodel$m2$syntax, d, self$options$estimator,
                        self$options$innerScheme, reps, seed, level,
                        tryCatch(as.character(utils::packageVersion("cSEM")), error = function(e) ""))
            res <- .pl_cached_bootstrap(self$results$cacheStore, key, function() {
                jack <- self$options$resampleMethod == "jackknife"
                run <- .pl_run_bootstrap(fit, spec$funs, reps, seed, progress = function(done, total) {
                    if (!jack) status$setContent(.pl_html_block(sprintf(txt("boot", "running"), done, total)))
                    private$.checkpoint()
                }, tick = private$.tick, method = self$options$resampleMethod)
                sm <- if (!is.null(run$value)) .pl_capture(.pl_boot_summary(run$value, level, spec)) else NULL
                list(summary = sm$value, error = c(run$errors, sm$error), warnings = run$warnings,
                     seconds = run$seconds)
            }, max_entries = .PL_CACHE_BOOT$entries, max_bytes = .PL_CACHE_BOOT$bytes)
            # the record also as numbers; a failed bootstrap keeps the row
            # with what is known and a footnote
            # ES: el registro también en números; si el bootstrap falla la
            # fila queda con lo conocido y una nota
            bs <- self$results$bootSummary
            known <- list(requested = reps, seed = seed, block = .PL_BLOCK, level = level)
            if (!is.null(res$summary) && identical(res$summary$method, "jackknife")) {
                # jackknife (1.2.0): n leave-one-out estimations, no seed and no blocks
                # ES: jackknife (1.2.0): n estimaciones dejando un caso fuera, sin semilla ni bloques
                status$setVisible(FALSE)
                bs$setRow(rowNo = 1, values = list(requested = res$summary$runs, drawn = res$summary$runs,
                    admissible = res$summary$admissible, seed = "\u2014", block = "\u2014", level = level))
                bs$setNote("jk", txt("boot", "jk_summary_note"))
                bs$setNote("time", jmvcore::format(txt("boot", "time_note"), seconds = round(res$seconds)))
                if (isTRUE(res$cached)) bs$setNote("cached", txt("boot", "cached_note"))
            } else if (!is.null(res$summary)) {
                status$setVisible(FALSE)
                bs$setRow(rowNo = 1, values = c(known, list(drawn = res$summary$runs,
                    admissible = res$summary$admissible)))
                bs$setNote("time", jmvcore::format(txt("boot", "time_note"), seconds = round(res$seconds)))
                if (isTRUE(res$cached)) bs$setNote("cached", txt("boot", "cached_note"))
            } else {
                status$setVisible(FALSE)
                bs$setRow(rowNo = 1, values = known)
                bs$setNote("failed", txt("boot", "not_done"))
            }
            res
        },

        # ---------------------------------------------------------------------
        # Mardia's multivariate skewness and kurtosis of the indicators
        # (1.2.0), on the data the model was estimated with.
        # ES: Asimetría y curtosis multivariadas de Mardia de los indicadores
        # (1.2.0), con los datos con que se estimó el modelo.
        # ---------------------------------------------------------------------
        .fillMardia = function(e, txt) {
            if (!self$options$mardia) return(NULL)
            table <- self$results$measurementSection$mardia
            lab <- function(k) txt("labels", k)
            r <- .pl_capture(.pl_mardia(.pl_numeric(e$prep$data)))
            if (is.null(r$value)) {
                for (k in 1:2) table$setRow(rowNo = k, values = list(measure = lab(c("mardia_skew", "mardia_kurt")[k]),
                                                                     reading = lab("not_computable")))
                table$setNote("reason", txt("footnotes", "mardia_singular"))
                return(NULL)
            }
            m <- r$value
            for (k in 1:2)
                table$setRow(rowNo = k, values = list(
                    measure = lab(c("mardia_skew", "mardia_kurt")[k]), value = m$value[k], statistic = m$statistic[k],
                    df = if (k == 1) m$df[1] else "", p = m$p[k],
                    reading = lab(if (m$p[k] < 0.05) "mardia_departs" else "mardia_consistent")))
            table$setNote("what", sprintf(txt("footnotes", "mardia_note"), m$k[1], m$n[1], m$k[1] * (m$k[1] + 2)))
            m
        },

        # ---------------------------------------------------------------------
        # Construct scores as new columns of the data set (1.2.0): the
        # standardized scores of the estimated model (stage 2 for the
        # higher-order constructs), in the rows the model used.
        # ES: Puntuaciones de los constructos como columnas nuevas del
        # conjunto de datos (1.2.0): las puntuaciones estandarizadas del
        # modelo estimado, en las filas que usó el modelo.
        # ---------------------------------------------------------------------
        .saveScores = function(e, blocks, hocs) {
            out <- self$results$scoresOut
            if (!self$options$scoresOut || !out$isNotFilled()) return()
            S <- e$est$Construct_scores
            labels <- vapply(blocks, `[[`, "", "label")
            cols <- list()
            for (lb in labels) {
                id <- e$model$cmap[[lb]]
                if (!is.null(id) && id %in% colnames(S)) cols[[lb]] <- S[, id]
            }
            if (length(hocs)) {
                S2 <- e$sest$Construct_scores
                for (h in hocs) {
                    id <- e$smodel$cmap[[h$label]]
                    if (!is.null(id) && id %in% colnames(S2)) cols[[h$label]] <- S2[, id]
                }
            }
            if (!length(cols)) return()
            lang <- .pl_lang(self$options)
            keys <- paste0("score", seq_along(cols))
            out$set(keys = keys, titles = paste0(names(cols), "_score"),
                    descriptions = sprintf(.pl_text(lang, "labels", "score_desc"), names(cols)),
                    measureTypes = rep("continuous", length(cols)))
            out$setRowNums(as.integer(rownames(e$prep$data)))
            for (k in seq_along(cols)) out$setValues(index = k, cols[[k]])
        },

        # ---------------------------------------------------------------------
        # Mediation: specific indirect effects, their percentile bootstrap
        # interval (from the draws of the path coefficients) and the type of
        # mediation. Without the bootstrap only the estimates.
        # ES: Mediación: efectos indirectos específicos, su intervalo
        # bootstrap percentil y el tipo de mediación. Sin bootstrap, solo
        # las estimaciones.
        # ---------------------------------------------------------------------
        .fillMediation = function(fit, paths, model, sm, txt) {
            chains <- .pl_mediation_chains(paths)
            if (!nrow(chains)) return(NULL)
            table <- self$results$structuralSection$mediation
            B <- fit$Estimates$Path_estimates
            level <- as.numeric(self$options$ciLevel)
            med <- .pl_mediation(chains, B, model$cmap, draws = sm$path_draws, level = level, direct_ci = sm$paths,
                                 method = if (is.null(sm$method)) "bootstrap" else sm$method)
            for (k in seq_len(nrow(med))) {
                v <- list(chain = med$label[k], indirect = med$estimate[k])
                if (!is.na(med$direct[k])) v$direct <- med$direct[k] else v$direct <- "\u2014"
                if (!is.na(med$lower[k])) { v$se <- med$se[k]; v$lower <- med$lower[k]; v$upper <- med$upper[k] }
                v$type <- if (is.na(med$type[k])) txt("labels", "med_needs_boot") else txt("labels", paste0("med_", med$type[k]))
                table$setRow(rowKey = med$label[k], values = v)
            }
            if (any(grepl(.PL_X, paths$from, fixed = TRUE)))
                table$setNote("inter", txt("footnotes", "med_interaction"))
            if (!is.null(sm)) table$setNote("ci", sprintf(txt("boot", paste0("med_note", if (identical(sm$method, "jackknife")) "_jk" else "")), format(100 * level)))
            med
        },

        # ---------------------------------------------------------------------
        # IPMA: importance (unstandardized total effects) and performance
        # (0-100) per target; the maps are drawn from image$state.
        # ES: IPMA: importancia y desempeño (0-100) por objetivo; los mapas se
        # dibujan desde image$state.
        # ---------------------------------------------------------------------
        .fillIpma = function(fit, model, d, blocks, paths, hocs, txt, sm = NULL) {
            table <- self$results$ipmaSection$ipmaTable
            reason <- .pl_ipma_reason(model, hocs, self$options)
            if (!is.null(reason)) {
                for (k in seq_len(table$rowCount)) table$setCell(rowNo = k, col = "quadrant", value = txt("labels", "not_computable"))
                table$setNote("reason", txt("footnotes", reason))
                return(list(reason = txt("footnotes", reason)))
            }
            pairs <- .pl_effect_pairs(paths)
            pairs <- pairs[pairs$to %in% paths$to[!grepl(.PL_X, paths$from, fixed = TRUE)], , drop = FALSE]
            range <- private$.scaleRange(model, d, blocks)
            if (!(range[2] > range[1])) {
                table$setNote("reason", txt("footnotes", "ipma_range"))
                return(list(reason = txt("footnotes", "ipma_range")))
            }
            r <- .pl_capture(.pl_ipma(fit, pairs, model$cmap, range, sm$total))
            if (is.null(r$value)) {
                table$setNote("reason", sprintf(txt("footnotes", "ipma_error"), paste(r$error, collapse = "; ")))
                return(list(error = r$error))
            }
            ip <- r$value$table
            for (k in seq_len(nrow(ip)))
                table$setRow(rowKey = paste(ip$to[k], ip$from[k], sep = "::"),
                             values = list(importance = ip$importance[k], perfRaw = ip$perf_raw[k],
                                           performance = ip$performance[k],
                                           # whole labels, one per quadrant and significance (translatable as one sentence)
                                           # ES: etiquetas completas, una por cuadrante y significación
                                           quadrant = txt("labels", paste0(ip$quadrant[k], if (isFALSE(ip$sig[k])) "_ns" else ""))))
            if (is.null(sm)) table$setNote("sig", txt("footnotes", "ipma_no_boot"))
            else if (any(!is.na(ip$sig) & !ip$sig))
                table$setNote("sig", if (identical(sm$method, "jackknife")) gsub("bootstrap", "jackknife", txt("footnotes", "ipma_ns"), fixed = TRUE)
                                     else txt("footnotes", "ipma_ns"))
            table$setNote("range", sprintf(txt("boot", "ipma_note"), .pl_fmt(range[1]), .pl_fmt(range[2]),
                                           txt("labels", paste0("ipma_range_", self$options$ipmaRange))))
            if (isTRUE(r$value$negative)) table$setNote("negative", txt("footnotes", "ipma_negative"))
            lang <- .pl_lang(self$options)
            for (tg in unique(ip$to)) {
                item <- self$results$ipmaSection$ipmaPlots$get(key = tg)
                k <- ip$to == tg
                item$setState(list(points = data.frame(construct = ip$from[k], importance = ip$importance[k],
                                                       performance = ip$performance[k], stringsAsFactors = FALSE),
                                   title = sprintf(.pl_text(lang, "labels", "ipma_title"), tg),
                                   x_lab = .pl_text(lang, "labels", "ipma_x"), y_lab = .pl_text(lang, "labels", "ipma_y"),
                                   caption = .pl_text(lang, "labels", "ipma_caption")))
            }
            list(table = ip, range = range, negative = r$value$negative, boot = !is.null(sm))
        },

        # The range of the scale shared by the IPMA and the NCA: from the
        # data (observed minimum and maximum of the indicators) or set by the
        # user. ES: El rango de la escala que comparten el IPMA y el NCA.
        .scaleRange = function(model, d, blocks) {
            if (self$options$ipmaRange == "manual")
                return(c(as.numeric(self$options$ipmaMin), as.numeric(self$options$ipmaMax)))
            vars <- unlist(lapply(blocks, `[[`, "vars"))
            vals <- unlist(lapply(d[, unname(model$vmap[vars]), drop = FALSE], function(x) as.numeric(x)))
            range(vals, na.rm = TRUE)
        },

        # ---------------------------------------------------------------------
        # NCA (1.1.0): the effect sizes and permutation tests of every
        # outcome-condition pair, the combined reading with the PLS-SEM
        # paths, the bottleneck tables and the ceiling plots. Long: cached,
        # with checkpoints during the permutations.
        # ES: NCA (1.1.0): tamaños del efecto y pruebas de permutación de cada
        # par resultado-condición, la lectura combinada con las rutas de
        # PLS-SEM, las tablas de cuellos de botella y los gráficos de techo.
        # ---------------------------------------------------------------------
        .fillNca = function(fit, model, d, blocks, paths, hocs, txt, path_tab, sm = NULL) {
            sec <- self$results$ncaSection
            tab <- sec$ncaTable
            comb <- sec$ncaCombined
            notcomp <- function(why) {
                for (k in seq_len(tab$rowCount)) tab$setCell(rowNo = k, col = "verdict", value = txt("labels", "not_computable"))
                for (k in seq_len(comb$rowCount)) comb$setCell(rowNo = k, col = "reading", value = txt("labels", "not_computable"))
                tab$setNote("reason", why)
                comb$setNote("reason", why)
                list(reason = why)
            }
            reason <- .pl_nca_reason(model, hocs, self$options)
            if (!is.null(reason)) return(notcomp(txt("footnotes", reason)))
            range <- private$.scaleRange(model, d, blocks)
            if (!(range[2] > range[1])) return(notcomp(txt("footnotes", "ipma_range")))
            pairs <- .pl_nca_pairs(paths)
            reps <- as.integer(self$options$ncaPerm)
            seed <- as.integer(self$options$bootSeed)
            scores <- .pl_capture(.pl_nca_scores(fit, d, range))
            if (is.null(scores$value)) return(notcomp(sprintf(txt("footnotes", "nca_error"), paste(scores$error, collapse = "; "))))
            sc <- scores$value
            key <- list("plssem-nca-2", self$options$weightApproach, model$syntax, model$modes, d, self$options$estimator, self$options$innerScheme,
                        range, reps, seed, pairs)
            res <- .pl_cached_bootstrap(self$results$ncaCacheStore, key, function() {
                t0 <- proc.time()[["elapsed"]]
                r <- .pl_capture(.pl_nca_run(sc, pairs, model$cmap, reps, seed, tick = private$.tick))
                list(summary = r$value, error = r$error, seconds = proc.time()[["elapsed"]] - t0)
            }, max_entries = .PL_CACHE_BOOT$entries, max_bytes = .PL_CACHE_BOOT$bytes)
            if (is.null(res$summary)) return(notcomp(sprintf(txt("footnotes", "nca_error"), paste(res$error, collapse = "; "))))
            verdict_ceiling <- substr(self$options$ncaCeiling, 1, 2)
            lab <- function(k) txt("labels", k)
            rows <- list()
            for (k in seq_len(nrow(pairs))) {
                tg <- pairs$to[k]
                cn <- pairs$from[k]
                r <- res$summary[[tg]][[model$cmap[[cn]]]]
                for (ce in c("ce", "cr")) {
                    dd <- r$effects[[ce]]
                    pp <- r$p[[ce]]
                    v <- .pl_nca_verdict(dd, pp)
                    tab$setRow(rowKey = paste(tg, cn, ce, sep = "::"),
                               values = list(d = dd, size = if (is.na(dd)) "" else lab(.pl_nca_size(dd)), p = pp,
                                             accuracy = if (ce == "cr") r$accuracy else 100,
                                             verdict = if (is.na(v)) lab("not_computable") else lab(v)))
                    if (is.na(dd)) tab$addFootnote(rowKey = paste(tg, cn, ce, sep = "::"), col = "d", txt("footnotes", "nca_negative_slope"))
                }
                # combined reading: necessity (NCA) and sufficiency in degree
                # (a significant PLS-SEM path)
                # ES: lectura combinada: necesidad (NCA) y suficiencia en grado
                # (ruta de PLS-SEM significativa)
                dd <- r$effects[[verdict_ceiling]]
                pp <- r$p[[verdict_ceiling]]
                nec <- identical(.pl_nca_verdict(dd, pp), "nca_necessary")
                est <- path_tab$estimate[path_tab$from == cn & path_tab$to == tg][1]
                sig <- NA
                if (!is.null(sm)) {
                    pr <- sm$paths[sm$paths$name == paste(model$cmap[[tg]], "~", model$cmap[[cn]]), , drop = FALSE]
                    if (nrow(pr) == 1 && is.finite(pr$lower)) sig <- pr$lower > 0 || pr$upper < 0
                }
                reading <- if (is.na(sig)) (if (nec) "nca_must_noboot" else "nca_not_must_noboot")
                           else if (nec && sig) "nca_both" else if (nec) "nca_must" else if (sig) "nca_should" else "nca_neither"
                comb$setRow(rowKey = paste(tg, cn, sep = "::"),
                            values = list(path = est, pathSig = if (is.na(sig)) "\u2014" else lab(if (sig) "yes" else "no"),
                                          d = dd, p = pp, reading = lab(reading)))
                rows[[k]] <- data.frame(to = tg, from = cn, d_ce = r$effects[["ce"]], d_cr = r$effects[["cr"]],
                                        p_ce = r$p[["ce"]], p_cr = r$p[["cr"]], d = dd, p = pp, necessary = nec,
                                        path = est, sig = sig, reading = reading, stringsAsFactors = FALSE)
            }
            tab$setNote("rule", sprintf(txt("footnotes", "nca_note"), .pl_fmt(range[1]), .pl_fmt(range[2]),
                                        txt("labels", paste0("ipma_range_", self$options$ipmaRange)), reps, seed))
            cnote <- sprintf(txt("footnotes", "nca_combined_note"), lab(paste0("nca_", verdict_ceiling)))
            if (identical(sm$method, "jackknife")) cnote <- gsub("bootstrap", "jackknife", cnote, fixed = TRUE)
            comb$setNote("ceiling", cnote)
            if (is.null(sm)) comb$setNote("boot", txt("footnotes", "nca_no_boot"))

            # bottleneck tables and plots
            # ES: tablas de cuellos de botella y gráficos
            lang <- .pl_lang(self$options)
            bn <- list()
            for (tg in unique(pairs$to)) {
                conds <- pairs$from[pairs$to == tg]
                if (self$options$ncaBottleneck) {
                    bt <- sec$ncaBottlenecks$get(key = tg)
                    for (j in seq_along(conds)) {
                        b <- res$summary[[tg]][[model$cmap[[conds[j]]]]]$bottleneck[[verdict_ceiling]]
                        for (i in seq_along(.PL_NCA_STEPS))
                            bt$setCell(rowKey = .PL_NCA_STEPS[i], col = paste0("c", j),
                                       value = .pl_nca_bn_cell(b[i], lab("nca_nn"), lab("nca_na")))
                    }
                    bt$setNote("read", txt("footnotes", "nca_bn_note"))
                }
                for (cn in conds) {
                    r <- res$summary[[tg]][[model$cmap[[cn]]]]
                    bn[[paste(tg, cn, sep = "::")]] <- r$bottleneck[[verdict_ceiling]]
                    if (!self$options$ncaPlots) next
                    ok <- stats::complete.cases(sc[[model$cmap[[cn]]]], sc[[model$cmap[[tg]]]])
                    item <- sec$ncaPlots$get(key = paste(cn, "\u2192", tg))
                    item$setState(list(x = sc[[model$cmap[[cn]]]][ok], y = sc[[model$cmap[[tg]]]][ok], peers = unname(r$peers),
                                       scope = r$scope, intercept = r$intercept, slope = r$slope,
                                       labels = list(ce = lab("nca_ce"), cr = lab("nca_cr")),
                                       title = sprintf(.pl_text(lang, "labels", "nca_title"), cn, tg),
                                       x_lab = sprintf(.pl_text(lang, "labels", "nca_axis"), cn),
                                       y_lab = sprintf(.pl_text(lang, "labels", "nca_axis"), tg),
                                       caption = .pl_text(lang, "labels", "nca_caption")))
                }
            }
            list(table = do.call(rbind, rows), bottleneck = bn, reps = reps, seed = seed, range = range,
                 boot = !is.null(sm), ceiling = verdict_ceiling, seconds = res$seconds)
        },

        .plotNca = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$x)) return(FALSE)
            print(.pl_plot_nca(image$state, ggtheme, theme))
            TRUE
        },

        .plotIpma = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$points)) return(FALSE)
            print(.pl_plot_ipma(image$state, ggtheme, theme))
            TRUE
        },

        # ---------------------------------------------------------------------
        # PLSpredict and CVPAT (cached), from one repeated cross-validation.
        # Category B: interactions, higher-order constructs, inadmissible
        # estimates.
        # ES: PLSpredict y CVPAT (en caché), de una validación cruzada
        # repetida. Categoría B: interacciones, orden superior, estimaciones
        # inadmisibles.
        # ---------------------------------------------------------------------
        .prediction = function(fit, model, d, raw, blocks, paths, hocs, admissible, txt) {
            pt <- self$results$predictSection$predict
            ct <- self$results$predictSection$cvpat
            reason <- .pl_predict_reason(model, hocs, admissible)
            notcomp <- function(why) {
                for (tab in list(pt, ct)) {
                    col <- if (identical(tab, pt)) "lower" else "decision"
                    for (k in seq_len(tab$rowCount)) tab$setCell(rowNo = k, col = col, value = txt("labels", "not_computable"))
                    tab$setNote("reason", why)
                }
                list(reason = why)
            }
            if (!is.null(reason)) return(notcomp(txt("footnotes", reason)))
            folds <- as.integer(self$options$predictFolds)
            reps <- as.integer(self$options$predictReps)
            seed <- as.integer(self$options$bootSeed)
            if (folds > nrow(d)) return(notcomp(txt("footnotes", "predict_small")))
            refit_with <- function(syntax, approach = self$options$weightApproach) function(data)
                suppressWarnings(do.call(cSEM::csem, c(list(.data = data, .model = syntax),
                                                       .pl_csem_args(self$options, model$modes, approach))))
            refit <- refit_with(model$syntax)
            pm <- private$.predictModels(model, blocks, paths, hocs, d, txt, refit_with)
            drop <- pm$drop; alt_refit <- pm$alt_refit; alt_reason <- pm$alt_reason; alt_syntax <- pm$alt_syntax
            bench <- pm$bench; bench_refit <- pm$bench_refit; bench_reason <- pm$bench_reason
            key <- list("plssem-predict-4", self$options$weightApproach, self$options$predictBenchmark, model$syntax, model$modes, d, raw, self$options$missing, self$options$estimator,
                        self$options$innerScheme, folds, reps, seed, alt_syntax,
                        tryCatch(as.character(utils::packageVersion("cSEM")), error = function(e) ""))
            res <- .pl_cached_bootstrap(self$results$predictCacheStore, key, function() {
                t0 <- proc.time()[["elapsed"]]
                r <- .pl_capture(local({
                    cv <- .pl_cv_residuals(fit, d, refit, folds, reps, seed, alt_refit = alt_refit,
                                           checkpoint = function() private$.checkpoint(), bench_refit = bench_refit,
                                           raw = raw)
                    vinv <- stats::setNames(names(model$vmap), model$vmap)
                    endo <- unique(paths$to)
                    ind_of <- function(cn) {
                        b <- Filter(function(x) identical(x$label, cn), blocks)
                        unname(model$vmap[b[[1]]$vars])
                    }
                    groups <- c(list(all = unique(unlist(lapply(endo, ind_of)))),
                                stats::setNames(lapply(endo, ind_of), endo))
                    m <- .pl_predict_metrics(cv)
                    m$indicator <- vinv[m$indicator]
                    list(metrics = m, cvpat = .pl_cvpat(cv, groups), inadmissible = cv$inadmissible)
                }))
                list(summary = r$value, error = r$error, warnings = r$warnings,
                     seconds = proc.time()[["elapsed"]] - t0)
            })
            if (is.null(res$summary)) {
                notcomp(sprintf(txt("footnotes", "predict_error"), paste(res$error, collapse = "; ")))
                return(list(error = res$error))
            }
            private$.fillPredict(res$summary$metrics, paths, blocks, pt, txt)
            private$.fillCvpat(res$summary$cvpat, paths, pt, ct, pm, txt)
            note <- sprintf(txt("boot", "predict_note"), folds, reps, seed, round(res$seconds))
            pt$setNote("cv", note)
            pt$setNote("metric", txt("boot", "predict_metric_note"))
            pt$setNote("q2", txt("boot", "q2_not_blindfolding"))
            # sample size for the cross-validation
            # ES: tamaño de muestra para la validación cruzada
            n_exog <- length(unique(unlist(lapply(setdiff(unique(paths$from), unique(paths$to)), function(cn) {
                b <- Filter(function(x) identical(x$label, cn), blocks)
                if (length(b)) b[[1]]$vars else character(0)
            }))))
            small <- .pl_predict_small(nrow(d), folds, n_exog)
            if (small$small) {
                msg <- sprintf(txt("footnotes", "predict_small_sample"), nrow(d), small$test, small$train, small$n_exog)
                pt$setNote("small", msg)
                ct$setNote("small", msg)
            }
            ct$setNote("cv", paste(note, txt("boot", "cvpat_note")))
            c(res$summary, list(folds = folds, reps = reps, seed = seed, warnings = res$warnings, small = small,
                                bench = if (bench != "none" && is.null(bench_reason)) txt("labels", paste0("approach_", bench)),
                                drop = if (nrow(drop)) paste(drop$from, "\u2192", drop$to, collapse = ", ")))
        },

        # Competing model and extra benchmark of PLSpredict and CVPAT. Each is
        # checked on the whole sample first (it must estimate and be
        # admissible); otherwise its rows say why. Split from .prediction.
        # ES: Modelo competidor y referencia adicional de PLSpredict y CVPAT. Cada
        # uno se comprueba antes con toda la muestra. Separado de .prediction.
        .predictModels = function(model, blocks, paths, hocs, d, txt, refit_with) {
            # Competing model: the same constructs without the left-out paths;
            # checked on the whole sample first (it must estimate and be
            # admissible), otherwise its rows say why.
            # ES: Modelo competidor: los mismos constructos sin las rutas
            # excluidas; se comprueba antes con toda la muestra.
            drop <- .pl_cvpat_drop(self$options$cvpatDrop, paths)
            alt_refit <- NULL
            alt_reason <- NULL
            alt_syntax <- NULL
            if (nrow(drop)) {
                keep <- paths[!(paste(paths$from, paths$to) %in% paste(drop$from, drop$to)), , drop = FALSE]
                if (!nrow(keep)) alt_reason <- txt("footnotes", "cvpat_alt_empty")
                else {
                    alt_syntax <- .pl_build_syntax(blocks, keep, self$options$estimator)$syntax
                    chk <- .pl_capture(refit_with(alt_syntax)(d))
                    if (is.null(chk$value)) alt_reason <- sprintf(txt("footnotes", "cvpat_alt_error"), paste(chk$error, collapse = "; "))
                    else if (sum(cSEM::verify(chk$value)) != 0) alt_reason <- txt("footnotes", "cvpat_alt_inadmissible")
                    else alt_refit <- refit_with(alt_syntax)
                }
            }
            # Extra benchmark (1.2.0): the same model with another weighting
            # approach; checked on the whole sample first.
            # ES: Referencia adicional (1.2.0): el mismo modelo con otro
            # enfoque de ponderación; se comprueba antes con toda la muestra.
            bench <- self$options$predictBenchmark
            bench_refit <- NULL
            bench_reason <- NULL
            if (bench != "none") {
                ar <- .pl_approach_reject(list(weightApproach = bench, estimator = self$options$estimator), blocks, hocs,
                                          isTRUE(model$nonlinear))
                if (bench == self$options$weightApproach) bench_reason <- txt("footnotes", "bench_same")
                else if (!is.null(ar)) bench_reason <- sprintf(txt("errors", ar), txt("labels", paste0("approach_", bench)))
                else {
                    chk <- .pl_capture(refit_with(model$syntax, bench)(d))
                    if (is.null(chk$value)) bench_reason <- sprintf(txt("footnotes", "bench_error"), paste(chk$error, collapse = "; "))
                    else bench_refit <- refit_with(model$syntax, bench)
                }
            }
            list(drop = drop, alt_refit = alt_refit, alt_reason = alt_reason, alt_syntax = alt_syntax,
                 bench = bench, bench_refit = bench_refit, bench_reason = bench_reason)
        },

        # Rows of PLSpredict: one per indicator of an endogenous construct, with
        # the benchmark columns when an extra benchmark was asked for.
        # ES: Filas de PLSpredict: una por indicador de un constructo endógeno.
        .fillPredict = function(m, paths, blocks, pt, txt) {
            endo <- unique(paths$to)
            for (cn in endo) {
                b <- Filter(function(x) identical(x$label, cn), blocks)
                for (v in b[[1]]$vars) {
                    r <- m[m$indicator == v, ]
                    if (!nrow(r)) next
                    lower <- if (r$tie) txt("labels", "equal") else if (r$pls_better) "PLS" else "LM"
                    vals <- list(q2 = r$q2, rmsePls = r$rmse_pls, rmseLm = r$rmse_lm,
                                 maePls = r$mae_pls, maeLm = r$mae_lm, skew = r$skew,
                                 metric = r$metric, lower = lower)
                    if (!is.null(r$rmse_bm)) { vals$rmseBench <- r$rmse_bm; vals$maeBench <- r$mae_bm }
                    pt$setRow(rowKey = paste(cn, v, sep = "::"), values = vals)
                }
            }
        },

        # Rows of CVPAT and their notes: the competing model and the benchmark
        # say why when they could not be estimated. Split from .prediction.
        # ES: Filas de CVPAT y sus notas: el modelo competidor y la referencia
        # explican por qué no se pudieron estimar. Separado de .prediction.
        .fillCvpat = function(cv, paths, pt, ct, pm, txt) {
            drop <- pm$drop; alt_reason <- pm$alt_reason; bench <- pm$bench; bench_reason <- pm$bench_reason
            for (k in seq_len(nrow(cv))) {
                cn <- if (cv$group[k] == "all") "" else cv$group[k]
                dec <- if (!cv$bench[k] %in% c("ALT", "BM")) { if (cv$p[k] < 0.05 && cv$diff[k] < 0) "yes" else "no" }
                       else if (cv$p[k] >= 0.05) "alt_same" else if (cv$diff[k] < 0) "yes"
                       else if (cv$bench[k] == "BM") "bm_better" else "alt_better"
                ct$setRow(rowKey = paste(cn, cv$bench[k], sep = "::"),
                          values = list(lossPls = cv$loss_pls[k], lossBench = cv$loss_bench[k], diff = cv$diff[k],
                                        t = cv$t[k], p = cv$p[k], decision = txt("labels", dec)))
            }
            if (nrow(drop)) {
                if (!is.null(alt_reason)) {
                    for (cn in c("", unique(paths$to)))
                        ct$setCell(rowKey = paste(cn, "ALT", sep = "::"), col = "decision", value = txt("labels", "not_computable"))
                    ct$setNote("alt", alt_reason)
                } else
                    ct$setNote("alt", sprintf(txt("boot", "cvpat_alt_note"), paste(drop$from, "\u2192", drop$to, collapse = ", ")))
            }
            if (bench != "none") {
                blab <- txt("labels", paste0("approach_", bench))
                if (!is.null(bench_reason)) {
                    for (cn in c("", unique(paths$to)))
                        ct$setCell(rowKey = paste(cn, "BM", sep = "::"), col = "decision", value = txt("labels", "not_computable"))
                    ct$setNote("bm", bench_reason)
                    pt$setNote("bm", bench_reason)
                } else {
                    ct$setNote("bm", sprintf(txt("footnotes", "bench_note"), blab))
                    pt$setNote("bm", sprintf(txt("footnotes", "bench_note"), blab))
                }
            }
        },

        # ---------------------------------------------------------------------
        # Model comparison (1.2.0): the main model and the competing model
        # (the main model without the paths listed in "Left out of the
        # competing model"), estimated with the same settings and compared
        # by R², adjusted R², the information criteria of each endogenous
        # construct (Sharma et al., 2019, 2021: BIC and GM for PLS-SEM) and
        # SRMR. No resampling.
        # ES: Comparación de modelos (1.2.0): el modelo principal y el
        # competidor, estimados con los mismos ajustes y comparados por R²,
        # R² ajustado, los criterios de información de cada constructo
        # endógeno y el SRMR. Sin remuestreo.
        # ---------------------------------------------------------------------
        .compareModels = function(fit, model, d, blocks, paths, hocs, txt) {
            tab <- self$results$compareSection$compare
            ptab <- self$results$compareSection$comparePaths
            lab <- function(k) txt("labels", k)
            notcomp <- function(why) {
                for (k in seq_len(tab$rowCount)) tab$setCell(rowNo = k, col = "preferred", value = lab("not_computable"))
                tab$setNote("reason", why)
                list(reason = why)
            }
            if (length(hocs)) return(notcomp(txt("footnotes", "compare_hoc")))
            drop <- .pl_cvpat_drop(self$options$cvpatDrop, paths)
            if (!nrow(drop)) return(notcomp(txt("footnotes", "compare_nodrop")))
            keep <- paths[!(paste(paths$from, paths$to) %in% paste(drop$from, drop$to)), , drop = FALSE]
            if (!nrow(keep)) return(notcomp(txt("footnotes", "cvpat_alt_empty")))
            alt <- .pl_build_syntax(blocks, keep, self$options$estimator)
            f2 <- .pl_capture(do.call(cSEM::csem, c(list(.data = d, .model = alt$syntax), .pl_csem_args(self$options, model$modes))))
            if (is.null(f2$value)) return(notcomp(sprintf(txt("footnotes", "cvpat_alt_error"), paste(f2$error, collapse = "; "))))
            r <- .pl_capture(.pl_compare(fit, f2$value, model$cmap, unique(paths$to)))
            if (is.null(r$value)) return(notcomp(sprintf(txt("footnotes", "compare_error"), paste(r$error, collapse = "; "))))
            cm <- r$value
            for (k in seq_len(nrow(cm$table))) {
                row <- cm$table[k, ]
                tab$setRow(rowKey = paste(row$construct, row$criterion, sep = "::"), values = list(
                    main = row$main, competing = if (is.na(row$competing)) "\u2014" else row$competing,
                    preferred = if (is.na(row$preferred)) "\u2014" else lab(paste0("pref_", row$preferred))))
            }
            sr <- cm$srmr
            tab$setRow(rowKey = "::SRMR", values = list(main = if (is.na(sr[1])) "\u2014" else sr[1],
                competing = if (is.na(sr[2])) "\u2014" else sr[2],
                preferred = if (anyNA(sr)) "\u2014" else lab(paste0("pref_", if (sr[1] <= sr[2]) "main" else "competing"))))
            if (anyNA(sr)) tab$setNote("srmr", txt("footnotes", "compare_srmr"))
            tab$setNote("drop", sprintf(txt("footnotes", "compare_note"), paste(drop$from, "\u2192", drop$to, collapse = ", ")))
            if (sum(cSEM::verify(f2$value)) != 0) tab$setNote("inadm", txt("footnotes", "cvpat_alt_inadmissible"))
            B1 <- fit$Estimates$Path_estimates
            B2 <- f2$value$Estimates$Path_estimates
            for (k in seq_len(nrow(paths))) {
                fr <- model$cmap[[paths$from[k]]]
                to <- model$cmap[[paths$to[k]]]
                dropped <- paste(paths$from[k], paths$to[k]) %in% paste(drop$from, drop$to)
                ptab$setRow(rowKey = paste(paths$from[k], paths$to[k], sep = "::"), values = list(
                    main = B1[to, fr], competing = if (dropped || !(to %in% rownames(B2))) "\u2014" else B2[to, fr]))
            }
            ptab$setNote("drop", txt("footnotes", "compare_paths_note"))
            c(cm, list(drop = paste(drop$from, "\u2192", drop$to, collapse = ", "), changed = unique(drop$to)))
        },

        # A result item by name, wherever it sits: at the top of the report or
        # inside one of its sections (Groups that give the report headings).
        # ES: Un elemento de resultados por nombre, esté arriba o dentro de una
        # de las secciones del informe.
        .item = function(name) {
            r <- self$results
            top <- r$get(name)
            if (!is.null(top)) return(top)
            for (g in c("measurementSection", "structuralSection", "fitSection", "predictSection", "compareSection",
                        "ipmaSection", "ncaSection", "mgaSection", "plotsSection")) {
                it <- r$get(g)$get(name)
                if (!is.null(it)) return(it)
            }
            stop("unknown result item: ", name)
        },

        # Rows of the multigroup tables: one per group, per pair of groups
        # and construct (MICOM) and per pair and path. With two groups the
        # coefficient columns carry the group names. Called by .init() (with
        # the header of the data) and again by .run() when the levels with
        # cases differ.
        # ES: Filas de las tablas multigrupo: una por grupo, por par y
        # constructo (MICOM) y por par y ruta. La llama .init() (con la
        # cabecera de los datos) y de nuevo .run() si los niveles con casos
        # son otros.
        .mgaRows = function(lev, labels, paths, vs) {
            for (g in lev) self$results$mgaSection$mgaGroups$addRow(rowKey = g, values = list(group = g))
            # MGCFA (1.1.0): one row per model, whatever the groups
            # ES: MGCFA (1.1.0): una fila por modelo, sean cuales sean los grupos
            mc <- self$results$mgaSection$mgcfa
            if (self$options$mgcfa && mc$rowCount == 0)
                for (st in .PL_MGCFA_STEPS)
                    mc$addRow(rowKey = st, values = list(step = .pl_text(.pl_lang(self$options), "labels", paste0("mgcfa_", st))))
            if (length(lev) < 2 || length(lev) > .PL_MGA_MAX_GROUPS) return(invisible(NULL))
            prs <- utils::combn(lev, 2, simplify = FALSE)
            for (k in seq_along(prs)) {
                cmp <- sprintf(vs, prs[[k]][1], prs[[k]][2])
                for (cn in labels)
                    self$results$mgaSection$micom$addRow(rowKey = paste(k, cn, sep = "::"),
                                              values = list(comparison = cmp, construct = cn))
                for (j in seq_len(nrow(paths)))
                    self$results$mgaSection$mga$addRow(rowKey = paste(k, j, sep = "::"), values = list(
                        comparison = cmp, from = paths$from[j], arrow = "\u2192", to = paths$to[j]))
            }
            if (length(lev) == 2) {
                self$results$mgaSection$mga$getColumn("est1")$setTitle(lev[1])
                self$results$mgaSection$mga$getColumn("est2")$setTitle(lev[2])
            }
        },

        # ---------------------------------------------------------------------
        # Multigroup analysis (cached): the model in each group, MICOM and the
        # path differences. Category B: when it cannot run, the rows stay with
        # the reason; MICOM alone can be "Not computable" (PLSc, ordinal) while
        # the path differences are still computed.
        # ES: Análisis multigrupo (en caché): el modelo en cada grupo, MICOM y
        # las diferencias de rutas. Categoría B: si no puede correr, las filas
        # se quedan con la razón; MICOM solo puede quedar "No calculable"
        # mientras las diferencias sí se calculan.
        # ---------------------------------------------------------------------
        .multigroup = function(model, d, blocks, paths, hocs, txt) {
            gt <- self$results$mgaSection$mgaGroups
            mt <- self$results$mgaSection$micom
            dt <- self$results$mgaSection$mga
            lab <- function(key) txt("labels", key)
            gv <- self$options$groupVar
            lev <- .pl_mga_levels(self$data[[gv]])
            if (!identical(as.character(unlist(gt$rowKeys)), lev)) {
                for (tab in list(gt, mt, dt)) tab$deleteRows()
                private$.mgaRows(lev, vapply(blocks, `[[`, "", "label"), paths, lab("mga_vs"))
            }
            g <- self$data[[gv]][as.integer(rownames(d))]
            mg <- .pl_mga_data(d, g, lev)
            n <- stats::setNames(unname(mg$n), lev)
            for (k in seq_along(lev)) gt$setRow(rowKey = lev[k], values = list(n = n[[k]]))
            notcomp <- function(why, tables = list(mt, dt), groups = FALSE) {
                for (tab in tables) {
                    col <- if (identical(tab, mt)) "invariance" else "decision"
                    for (k in seq_len(tab$rowCount)) tab$setCell(rowNo = k, col = col, value = lab("not_computable"))
                    tab$setNote("reason", why)
                }
                # the per-group table keeps its N and says why there is no
                # estimate, instead of an empty cell
                # ES: la tabla por grupo conserva su N y dice por qué no hay
                # estimación, en lugar de una celda vacía
                if (groups) {
                    for (k in seq_along(lev)) gt$setRow(rowKey = lev[k], values = list(admissible = lab("not_computable")))
                    gt$setNote("reason", why)
                }
            }
            r <- .pl_mga_reason(hocs, gv, unlist(lapply(blocks, `[[`, "vars")), n)
            if (!is.null(r)) {
                why <- do.call(sprintf, c(list(txt("footnotes", r$key)), r$args))
                notcomp(why, tables = if (self$options$mgcfa) list(mt, dt, self$results$mgaSection$mgcfa) else list(mt, dt),
                        groups = TRUE)
                return(list(reason = why, n_missing = mg$n_missing))
            }
            # MGCFA (1.1.0): lavaan on the same groups; it does not need the
            # PLS estimates of each group.
            # ES: MGCFA (1.1.0): lavaan con los mismos grupos; no necesita las
            # estimaciones PLS de cada grupo.
            mgc <- private$.fillMgcfa(mg, model, blocks, txt)
            # MAXVAR fixes the weights only up to a common sign, which can
            # differ between groups: MICOM and the path differences would
            # compare composites of opposite sign (1.2.0).
            # ES: MAXVAR fija los pesos solo hasta un signo común, que puede
            # diferir entre grupos (1.2.0).
            if (self$options$weightApproach == "maxvar") {
                why <- txt("footnotes", "mga_maxvar")
                notcomp(why, groups = TRUE)
                return(list(reason = why, n_missing = mg$n_missing, mgcfa = mgc))
            }
            fit <- .pl_capture(do.call(cSEM::csem, c(list(.data = mg$data, .model = model$syntax, .id = "PLgroup"),
                                                     .pl_csem_args(self$options, model$modes))))
            if (!is.null(fit$error)) {
                why <- sprintf(txt("footnotes", "mga_error"), paste(fit$error, collapse = "; "))
                notcomp(why, groups = TRUE)
                return(list(reason = why, error = fit$error, n_missing = mg$n_missing, mgcfa = mgc))
            }
            fg <- fit$value
            adm <- vapply(mg$ids, function(id) sum(cSEM::verify(fg[[id]])) == 0, TRUE)
            for (k in seq_along(lev))
                gt$setRow(rowKey = lev[k], values = list(admissible = lab(if (adm[[k]]) "yes" else "no")))
            gt$setNote("est", txt("boot", "mga_groups_note"))
            if (!all(adm)) {
                why <- sprintf(txt("footnotes", "mga_inadmissible"), paste(lev[!adm], collapse = ", "))
                notcomp(why)
                return(list(reason = why, n_missing = mg$n_missing, mgcfa = mgc))
            }
            perms <- as.integer(self$options$mgaPerm)
            boots <- as.integer(self$options$mgaBoot)
            seed <- as.integer(self$options$bootSeed)
            adjust <- self$options$mgaAdjust
            micom_reason <- .pl_micom_reason(self$options$estimator, self$options$indicatorScale)
            compare <- .pl_mga_compare(paths, model$cmap)
            key <- list("plssem-mga-2", self$options$weightApproach, model$syntax, model$modes, mg$data, self$options$estimator,
                        self$options$innerScheme, perms, boots, seed, micom_reason,
                        tryCatch(as.character(utils::packageVersion("cSEM")), error = function(e) ""))
            res <- .pl_cached_bootstrap(self$results$mgaCacheStore, key, function() {
                out <- list()
                warn <- character(0)
                if (is.null(micom_reason)) {
                    private$.checkpoint()
                    t0 <- proc.time()[["elapsed"]]
                    m <- .pl_capture(.pl_micom(fg, perms, seed, tick = private$.tick))
                    out$micom <- m$value
                    out$micom_error <- m$error
                    out$s_micom <- proc.time()[["elapsed"]] - t0
                    warn <- c(warn, m$warnings)
                }
                private$.checkpoint()
                t0 <- proc.time()[["elapsed"]]
                m <- .pl_capture(.pl_mgd(fg, compare, perms, boots, seed, tick = private$.tick))
                out$mgd <- m$value
                out$mgd_error <- m$error
                out$s_mgd <- proc.time()[["elapsed"]] - t0
                list(summary = out, warnings = unique(c(warn, m$warnings)))
            })
            s <- res$summary
            cinv <- stats::setNames(names(model$cmap), unname(model$cmap))
            prs <- .pl_mga_pairs(mg$ids)
            cmp_of <- function(k) sprintf(lab("mga_vs"), mg$labels[[prs[[k]][1]]], mg$labels[[prs[[k]][2]]])
            out <- list(group = gv, labels = lev, n = n, n_missing = mg$n_missing, adjust = adjust,
                        warnings = res$warnings, perms = perms, boots = boots, seed = seed)
            out$mgcfa <- mgc

            ctx <- list(txt = txt, lab = lab, notcomp = notcomp, cmp_of = cmp_of, cinv = cinv, mg = mg,
                        perms = perms, boots = boots, seed = seed, adjust = adjust, mt = mt, dt = dt)
            out <- private$.fillMicom(out, s, micom_reason, blocks, ctx)
            private$.fillMgd(out, s, model, paths, ctx)
        },

        # MGCFA rows (configural, metric, scalar) or the reason they are not
        # computable. ES: Filas del MGCFA o la razón por la que no se calculan.
        .fillMgcfa = function(mg, model, blocks, txt) {
            if (!self$options$mgcfa) return(NULL)
            tab <- self$results$mgaSection$mgcfa
            lab <- function(k) txt("labels", k)
            notcomp <- function(why) {
                for (k in seq_len(tab$rowCount)) tab$setCell(rowNo = k, col = "decision", value = lab("not_computable"))
                tab$setNote("reason", why)
                list(reason = why)
            }
            syn <- .pl_mgcfa_syntax(blocks, model$cmap, model$vmap)
            reason <- .pl_mgcfa_reason(syn, self$options$indicatorScale)
            if (!is.null(reason)) return(notcomp(txt("footnotes", reason)))
            est <- self$options$mgcfaEstimator
            r <- .pl_capture(.pl_mgcfa(.pl_numeric(mg$data), syn, est))
            if (is.null(r$value)) return(notcomp(sprintf(txt("footnotes", "mgcfa_error"), paste(r$error, collapse = "; "))))
            t <- r$value
            for (k in seq_len(nrow(t)))
                tab$setRow(rowKey = t$step[k], values = list(
                    chisq = t$chisq[k], df = t$df[k], p = t$p[k], cfi = t$cfi[k], rmsea = t$rmsea[k], srmr = t$srmr[k],
                    dchisq = if (k > 1) t$dchisq[k] else "", ddf = if (k > 1) t$ddf[k] else "",
                    dp = if (k > 1) t$dp[k] else "", dcfi = if (k > 1) t$dcfi[k] else "",
                    drmsea = if (k > 1) t$drmsea[k] else "", dsrmr = if (k > 1) t$dsrmr[k] else "",
                    decision = lab(t$decision[k])))
            tab$setNote("rule", sprintf(txt("footnotes", "mgcfa_note"), lab(paste0("mgcfa_est_", est)), length(mg$ids)))
            single <- vapply(Filter(function(b) identical(b$type, "reflective") && length(b$vars) == 1, blocks), `[[`, "", "label")
            form <- vapply(Filter(function(b) !identical(b$type, "reflective"), blocks), `[[`, "", "label")
            if (length(c(single, form)))
                tab$setNote("left", sprintf(txt("footnotes", "mgcfa_left"), paste(c(single, form), collapse = ", ")))
            # configural RMSEA above .08 while CFI and SRMR hold: say why
            # RMSEA does not decide (Kenny et al., 2015)
            # ES: RMSEA configural sobre .08 con CFI y SRMR aceptables
            if (private$.rmseaOverruled(t))
                tab$addFootnote(rowKey = "configural", col = "rmsea",
                                sprintf(txt("footnotes", "mgcfa_rmsea_cell"), .pl_fmt(t$rmsea[1]), format(round(t$df[1]))))
            if (!all(t$converged))
                tab$setNote("conv", sprintf(txt("footnotes", "mgcfa_nonconv"),
                                            paste(lab(paste0("mgcfa_", t$step[!t$converged])), collapse = ", ")))
            if (length(r$warnings))
                tab$setNote("warn", sprintf(txt("footnotes", "mgcfa_warnings"), paste(unique(r$warnings), collapse = "; ")))
            list(table = t, estimator = est, warnings = r$warnings)
        },

        # MICOM rows (steps 2 and 3) or the reason they are not computable.
        # ES: Filas de MICOM (pasos 2 y 3) o la razón por la que no se calculan.
        .fillMicom = function(out, s, micom_reason, blocks, ctx) {
            txt <- ctx$txt; lab <- ctx$lab; notcomp <- ctx$notcomp; cmp_of <- ctx$cmp_of; cinv <- ctx$cinv
            mt <- ctx$mt; perms <- ctx$perms; seed <- ctx$seed
            # MICOM
            # ES: MICOM
            if (!is.null(micom_reason)) {
                out$why_micom <- txt("footnotes", micom_reason)
                notcomp(out$why_micom, list(mt))
            } else if (is.null(s[["micom"]])) {
                out$why_micom <- sprintf(txt("footnotes", "micom_error"), paste(s$micom_error, collapse = "; "))
                notcomp(out$why_micom, list(mt))
            } else {
                mi <- s[["micom"]]$table
                mi$label <- unname(cinv[mi$construct])
                mi$comparison <- vapply(mi$pair, cmp_of, "")
                for (k in seq_len(nrow(mi)))
                    mt$setRow(rowKey = paste(mi$pair[k], mi$label[k], sep = "::"), values = list(
                        c = mi$c[k], c5 = mi$c5[k], pC = mi$p_c[k], meanDiff = mi$mean[k], pMean = mi$p_mean[k],
                        logVar = mi$var[k], pVar = mi$p_var[k], invariance = lab(mi$verdict[k])))
                mt$setNote("micom", sprintf(txt("boot", "micom_note"), perms, s[["micom"]]$admissible, seed, round(s$s_micom)))
                mt$setNote("inv", txt("boot", "micom_invariance_note"))
                single <- vapply(Filter(function(b) length(b$vars) == 1, blocks), `[[`, "", "label")
                if (length(single))
                    mt$setNote("single", sprintf(txt("boot", "micom_single"), paste(single, collapse = ", ")))
                out$micom <- mi
            }

            out
        },

        # Path differences between groups (permutation test and PLS-MGA),
        # with the paths whose constructs lack compositional invariance.
        # ES: Diferencias de rutas entre grupos, con las rutas cuyos
        # constructos no tienen invarianza composicional.
        .fillMgd = function(out, s, model, paths, ctx) {
            txt <- ctx$txt; lab <- ctx$lab; notcomp <- ctx$notcomp; cmp_of <- ctx$cmp_of; mg <- ctx$mg
            dt <- ctx$dt; perms <- ctx$perms; boots <- ctx$boots; seed <- ctx$seed; adjust <- ctx$adjust
            # Path differences
            # ES: Diferencias de rutas
            if (is.null(s[["mgd"]])) {
                why <- sprintf(txt("footnotes", "mga_error"), paste(s$mgd_error, collapse = "; "))
                notcomp(why, list(dt))
                out$reason <- why
                out$error <- s$mgd_error
                return(out)
            }
            tab <- .pl_mgd_adjust(s[["mgd"]]$table, adjust)
            tab$decision <- .pl_mgd_decision(tab$p_perm, tab$p_hens)
            names_j <- vapply(seq_len(nrow(paths)), function(j)
                paste(model$cmap[[paths$to[j]]], "~", model$cmap[[paths$from[j]]]), "")
            tab$j <- match(tab$name, names_j)
            tab$label <- paste(paths$from[tab$j], "→", paths$to[tab$j])
            tab$comparison <- vapply(tab$pair, cmp_of, "")
            tab$l1 <- unname(mg$labels[tab$g1])
            tab$l2 <- unname(mg$labels[tab$g2])
            for (k in seq_len(nrow(tab)))
                dt$setRow(rowKey = paste(tab$pair[k], tab$j[k], sep = "::"), values = list(
                    est1 = tab$est1[k], est2 = tab$est2[k], diff = tab$diff[k], pPerm = tab$p_perm[k],
                    pOne = tab$p_one[k], pHens = tab$p_hens[k], decision = lab(tab$decision[k])))
            dt$setNote("mgd", sprintf(txt("boot", "mgd_note"), perms, s[["mgd"]]$admissible_perm, boots, seed, round(s$s_mgd)))
            dt$setNote("smartpls", txt("boot", "mgd_smartpls"))
            if (adjust != "none")
                dt$setNote("adjust", sprintf(txt("boot", "mgd_adjust"), nrow(tab), lab(paste0("adjust_", adjust))))
            # Paths that involve a construct without compositional invariance
            # in that pair of groups (MICOM step 2).
            # ES: Rutas con un constructo sin invarianza composicional.
            if (!is.null(out$micom)) {
                bad <- out$micom[out$micom$verdict == "inv_none", , drop = FALSE]
                if (nrow(bad)) {
                    parts <- vapply(unique(bad$pair), function(k)
                        sprintf("%s (%s)", paste(bad$label[bad$pair == k], collapse = ", "), cmp_of(k)), "")
                    out$fail_micom <- sprintf(txt("boot", "mgd_micom_fail"), paste(parts, collapse = "; "))
                    dt$setNote("micom", out$fail_micom)
                }
            }
            out$mgd <- tab
            out
        },

        # Interpretation of the multigroup analysis: the groups, MICOM per
        # pair of groups, the path differences and the cautions.
        # ES: Interpretación del análisis multigrupo.
        .mgaLines = function(txt, mga) {
            if (is.null(mga)) return(character(0))
            mcl <- private$.mgcfaLines(txt, mga$mgcfa)
            if (!is.null(mga$reason) && is.null(mga$mgd)) return(c(sprintf(txt("mga_none"), mga$reason), mcl))
            out <- sprintf(txt("mga_groups"), mga$group,
                           paste(sprintf("%s (n = %d)", mga$labels, mga$n), collapse = ", "))
            if (!is.null(mga$micom)) {
                mi <- mga$micom
                for (cmp in unique(mi$comparison)) {
                    r <- mi[mi$comparison == cmp, , drop = FALSE]
                    none <- r$label[r$verdict == "inv_none"]
                    part <- r$label[r$verdict == "inv_partial"]
                    out <- c(out, if (length(none)) sprintf(txt("micom_none"), cmp, paste(none, collapse = ", "))
                                  else if (length(part)) sprintf(txt("micom_partial"), cmp, paste(part, collapse = ", "))
                                  else sprintf(txt("micom_full"), cmp))
                }
            } else out <- c(out, sprintf(txt("micom_not"), mga$why_micom))
            out <- c(out, mcl)
            t <- mga$mgd
            tests <- c(mgd_both = "mgd_tests_both", mgd_perm_only = "mgd_tests_perm", mgd_hens_only = "mgd_tests_hens")
            for (cmp in unique(t$comparison)) {
                r <- t[t$comparison == cmp, , drop = FALSE]
                sig <- r$decision != "mgd_none"
                if (!any(sig)) out <- c(out, sprintf(txt("mgd_no_diff"), cmp))
                for (k in which(sig))
                    out <- c(out, sprintf(txt("mgd_sig"), cmp, r$label[k], txt(tests[[r$decision[k]]]),
                                          .pl_fmt(r$est1[k]), r$l1[k], .pl_fmt(r$est2[k]), r$l2[k]))
            }
            dis <- t$decision %in% c("mgd_perm_only", "mgd_hens_only")
            if (any(dis)) {
                many <- length(unique(t$comparison)) > 1
                items <- if (many) sprintf("%s (%s)", t$label[dis], t$comparison[dis]) else t$label[dis]
                out <- c(out, sprintf(txt("mgd_disagree"), paste(items, collapse = ", ")))
            }
            if (!is.null(mga$fail_micom)) out <- c(out, mga$fail_micom)
            if (min(mga$n) < 100) out <- c(out, sprintf(txt("mgd_small"), min(mga$n)))
            if (mga$adjust == "none" && nrow(t) > 1) out <- c(out, sprintf(txt("mgd_multiple"), nrow(t)))
            c(out, txt("mga_caution"), txt("mga_next"))
        },

        # TRUE when the configural RMSEA alone (> .08) would have called the
        # fit poor while CFI and SRMR call it good or acceptable.
        # ES: TRUE cuando solo el RMSEA configural (> .08) habría calificado
        # el ajuste de pobre y CFI y SRMR lo aceptan.
        .rmseaOverruled = function(t) {
            is.finite(t$rmsea[1]) && t$rmsea[1] > 0.08 && t$decision[1] %in% c("mgcfa_good", "mgcfa_acceptable")
        },

        # Interpretation of the MGCFA: how far the invariance of the
        # reflective constructs as common factors goes, and how it relates to
        # MICOM. ES: Interpretación del MGCFA y su relación con MICOM.
        .mgcfaLines = function(txt, mgc) {
            if (is.null(mgc)) return(character(0))
            # changes rounded to three decimals, without a negative zero
            # ES: cambios con tres decimales, sin cero negativo
            fd <- function(v) formatC(if (abs(v) < 5e-4) 0 else v, format = "f", digits = 3)
            if (!is.null(mgc$reason)) return(sprintf(txt("mgcfa_none"), mgc$reason))
            t <- mgc$table
            lab <- function(k) .pl_text(.pl_lang(self$options), "labels", k)
            out <- sprintf(txt("mgcfa_configural"), tolower(lab(t$decision[1])), .pl_fmt(t$cfi[1]), .pl_fmt(t$srmr[1]))
            if (private$.rmseaOverruled(t))
                out <- c(out, sprintf(txt("mgcfa_rmsea"), .pl_fmt(t$rmsea[1]), format(round(t$df[1])),
                                      .pl_fmt(t$cfi[1]), .pl_fmt(t$srmr[1])))
            if (t$decision[2] == "mgcfa_supported") {
                out <- c(out, sprintf(txt("mgcfa_metric_ok"), fd(t$dcfi[2]), fd(t$drmsea[2])))
                out <- c(out, if (t$decision[3] == "mgcfa_supported")
                                  sprintf(txt("mgcfa_scalar_ok"), fd(t$dcfi[3]), fd(t$drmsea[3]))
                              else sprintf(txt("mgcfa_scalar_fail"), fd(t$dcfi[3]), fd(t$drmsea[3])))
            } else if (t$decision[2] == "mgcfa_not_supported") {
                out <- c(out, sprintf(txt("mgcfa_metric_fail"), fd(t$dcfi[2]), fd(t$drmsea[2])))
            } else {
                out <- c(out, txt("mgcfa_configural_fail"))
            }
            # a significant chi-square difference under supported indices
            # ES: diferencia de chi cuadrado significativa con índices que sí
            # sostienen el paso
            k <- which(t$decision == "mgcfa_supported" & is.finite(t$dp) & t$dp < 0.05)
            if (length(k))
                out <- c(out, sprintf(txt("mgcfa_chisq"), paste(tolower(lab(paste0("mgcfa_", t$step[k]))), collapse = ", "),
                                      paste(vapply(t$dp[k], .pl_fmt_p, ""), collapse = ", ")))
            c(out, txt("mgcfa_vs_micom"))
        },

        # ---------------------------------------------------------------------
        # Overall fit test (cached). Category B: with interactions, higher-order
        # constructs or inadmissible estimates the rows stay, with the reason.
        # ES: Prueba de ajuste global (en caché). Categoría B: con
        # interacciones, orden superior o estimaciones inadmisibles las filas
        # se quedan, con la razón.
        # ---------------------------------------------------------------------
        .fitTest = function(fit, model, d, hocs, admissible, txt) {
            table <- self$results$fitSection$fit
            reason <- .pl_fit_reason(model, hocs, admissible)
            notcomp <- function(why) {
                for (k in seq_len(table$rowCount)) {
                    table$setCell(rowNo = k, col = "decision", value = txt("labels", "not_computable"))
                }
                # a table note (keyed), not a cell footnote: the analysis can
                # run several times and footnotes would pile up
                # ES: una nota de la tabla (con clave), no una nota de celda:
                # el análisis puede correr varias veces y se acumularían
                table$setNote("reason", why)
                list(reason = why)
            }
            if (!is.null(reason)) return(notcomp(txt("footnotes", reason)))
            reps <- as.integer(self$options$fitReps)
            seed <- as.integer(self$options$bootSeed)
            level <- as.numeric(self$options$fitLevel)
            key <- list("plssem-fit-2", self$options$weightApproach, model$syntax, model$modes, d, self$options$estimator, self$options$innerScheme,
                        reps, seed, level, tryCatch(as.character(utils::packageVersion("cSEM")), error = function(e) ""))
            res <- .pl_cached_bootstrap(self$results$fitCacheStore, key, function() {
                t0 <- proc.time()[["elapsed"]]
                r <- .pl_capture(.pl_fit_test(fit, reps, seed, level, checkpoint = function() private$.checkpoint(),
                                              tick = private$.tick))
                list(summary = r$value, error = r$error, warnings = r$warnings,
                     seconds = proc.time()[["elapsed"]] - t0)
            })
            if (is.null(res$summary)) {
                notcomp(sprintf(txt("footnotes", "fit_error"), paste(res$error, collapse = "; ")))
                return(list(error = res$error))
            }
            ft <- res$summary$table
            for (k in seq_len(nrow(ft))) {
                table$setRow(rowKey = paste(ft$model[k], ft$measure[k], sep = "::"),
                             values = list(value = ft$value[k], critical = ft$critical[k], p = ft$p[k],
                                           decision = txt("labels", if (ft$reject[k]) "fit_reject" else "fit_keep")))
            }
            adm <- res$summary$admissible
            table$setNote("boot", sprintf(txt("boot", "fit_note"), reps, adm[["estimated"]], adm[["saturated"]], seed,
                                          format(100 * level), round(res$seconds)))
            same <- isTRUE(all.equal(ft$value[ft$model == "estimated"], ft$value[ft$model == "saturated"]))
            if (same) table$setNote("same", txt("boot", "fit_same"))
            c(res$summary, list(same = same, level = level, warnings = res$warnings, seed = seed))
        },

        # Writes the bootstrap columns of every table, by internal name.
        # ES: Escribe las columnas del bootstrap de cada tabla, por nombre
        # interno.
        .fillBootstrap = function(sm, blocks, paths, model, txt) {
            cc <- function(x) model$cmap[[x]]
            # measurement rows use the stage-1 names (they differ from the
            # stage-2 ones with cSEM's second-order routine)
            # ES: las filas de medida usan los nombres de la etapa 1
            mc <- function(x) (if (is.null(model$mcmap)) model$cmap else model$mcmap)[[x]]
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
            note <- sprintf(txt("boot", paste0("ci_note", if (identical(sm$method, "jackknife")) "_jk" else "")), pct, sm$admissible)

            # Paths: SE, t, p and interval (cSEM's summarize).
            # ES: Rutas: EE, t, p e intervalo (summarize de cSEM).
            for (k in seq_len(nrow(paths))) {
                r <- row_of(sm$paths, paste(cc(paths$to[k]), "~", cc(paths$from[k])))
                if (nrow(r)) {
                    set(self$results$structuralSection$paths, list(se = r$se, t = r$t, p = r$p, lower = r$lower, upper = r$upper), rowNo = k)
                    # p (normal theory on the bootstrap SE) and the percentile
                    # interval can disagree on borderline paths; the interval
                    # decides, and the row says so.
                    # ES: p y el intervalo percentil pueden discrepar en rutas
                    # límite; decide el intervalo y la fila lo dice.
                    excl <- is.finite(r$lower) && (r$lower > 0 || r$upper < 0)
                    sig_p <- is.finite(r$p) && r$p < 1 - as.numeric(self$options$ciLevel)
                    if (is.finite(r$p) && excl != sig_p)
                        self$results$structuralSection$paths$addFootnote(rowNo = k, col = "p", txt("boot", "p_ci_disagree"))
                }
            }
            self$results$structuralSection$paths$setNote("ci", paste(note, txt("boot", paste0("p_note", if (identical(sm$method, "jackknife")) "_jk" else ""))))

            # Loadings and weights.
            # ES: Cargas y pesos.
            for (i in seq_along(blocks)) for (v in blocks[[i]]$vars) {
                key <- paste(i, v, sep = "::")
                l <- row_of(sm$loadings, paste(mc(blocks[[i]]$label), "=~", vv(v)))
                w <- row_of(sm$weights, paste(mc(blocks[[i]]$label), "<~", vv(v)))
                if (!is.null(l) && nrow(l)) set(self$results$measurementSection$measurement, list(loadLower = l$lower, loadUpper = l$upper), rowKey = key)
                if (!is.null(w) && nrow(w)) set(self$results$measurementSection$measurement, list(weightLower = w$lower, weightUpper = w$upper,
                                                                              weightP = w$p), rowKey = key)
            }
            self$results$measurementSection$measurement$setNote("ci", note)

            for (to in unique(paths$to)) {
                r <- row_of(sm$R2, cc(to))
                if (!is.null(r) && nrow(r)) set(self$results$structuralSection$r2, list(r2Lower = r$lower, r2Upper = r$upper), rowKey = to)
            }
            self$results$structuralSection$r2$setNote("ci", note)

            for (b in Filter(function(b) b$type == "reflective" && length(b$vars) > 1, blocks)) {
                rc <- row_of(sm$rhoC, mc(b$label))
                av <- row_of(sm$AVE, mc(b$label))
                if (!is.null(rc) && nrow(rc)) set(self$results$measurementSection$reliability, list(rhoCLower = rc$lower, rhoCUpper = rc$upper), rowKey = b$label)
                if (!is.null(av) && nrow(av)) set(self$results$measurementSection$reliability, list(aveLower = av$lower, aveUpper = av$upper), rowKey = b$label)
            }
            self$results$measurementSection$reliability$setNote("ci", note)

            # HTMT: the one-sided 95% upper bound decides the assessment.
            # ES: HTMT: el límite superior unilateral al 95% decide la
            # valoración.
            if (!is.null(sm$HTMT)) {
                refl <- vapply(Filter(function(b) b$type == "reflective", blocks), `[[`, "", "label")
                pr <- utils::combn(refl, 2)
                for (k in seq_len(ncol(pr))) {
                    r <- row_of(sm$HTMT, paste(mc(pr[1, k]), mc(pr[2, k]), sep = "~"))
                    key <- paste(pr[, k], collapse = "::")
                    if (!is.null(r) && nrow(r) && is.finite(r$upper)) {
                        self$results$measurementSection$htmt$setCell(col = "htmtUpper", value = r$upper, rowKey = key)
                        band <- if (r$upper < 0.85) "htmt_ok_ci" else if (r$upper < 0.90) "htmt_similar_ci" else "htmt_doubt_ci"
                        self$results$measurementSection$htmt$setCell(col = "assessment", value = txt("labels", band), rowKey = key)
                    }
                }
                self$results$measurementSection$htmt$setNote("ci", sprintf(txt("boot", paste0("htmt_note", if (identical(sm$method, "jackknife")) "_jk" else "")), sm$admissible))
            }

            for (i in seq_along(blocks)) {
                if (blocks[[i]]$type != "formative" || length(blocks[[i]]$vars) < 2) next
                for (v in blocks[[i]]$vars) {
                    r <- row_of(sm$VIFB, paste(mc(blocks[[i]]$label), vv(v), sep = "~"))
                    if (!is.null(r) && nrow(r)) set(self$results$measurementSection$vif, list(vifLower = r$lower, vifUpper = r$upper),
                                                    rowKey = paste(i, v, sep = "::"))
                }
            }
            self$results$measurementSection$vif$setNote("ci", note)

            for (k in seq_len(nrow(paths))) {
                r <- row_of(sm$VIFS, paste(cc(paths$to[k]), cc(paths$from[k]), sep = "."))
                if (!is.null(r) && nrow(r)) set(self$results$structuralSection$vifStruct, list(vifLower = r$lower, vifUpper = r$upper), rowNo = k)
            }
            self$results$structuralSection$vifStruct$setNote("ci", note)

            pairs <- .pl_effect_pairs(paths)
            for (k in seq_len(nrow(pairs))) {
                key <- paste(pairs$from[k], pairs$to[k], sep = "::")
                nm <- paste(cc(pairs$to[k]), "~", cc(pairs$from[k]))
                ri <- row_of(sm$indirect, nm)
                rt <- row_of(sm$total, nm)
                if (!is.null(ri) && nrow(ri) && ri$estimate != 0)
                    set(self$results$structuralSection$effects, list(indLower = ri$lower, indUpper = ri$upper), rowKey = key)
                if (!is.null(rt) && nrow(rt))
                    set(self$results$structuralSection$effects, list(totLower = rt$lower, totUpper = rt$upper), rowKey = key)
            }
            self$results$structuralSection$effects$setNote("ci", note)
        },

        # ---------------------------------------------------------------------
        # Path diagram: layout computed here and kept in image$state; the
        # render function only draws it.
        # ES: Diagrama de rutas: la disposición se calcula aquí y se guarda en
        # image$state; la función de dibujo solo la dibuja.
        # ---------------------------------------------------------------------
        # Simple effects at -1 SD, the mean and +1 SD of the moderator, and
        # the plot state of each interaction.
        # ES: Efectos simples en -1 DE, la media y +1 DE del moderador, y el
        # estado del gráfico de cada interacción.
        .fillModeration = function(fit, paths, model, sm, txt) {
            level <- as.numeric(self$options$ciLevel)
            mod <- .pl_moderation(fit, paths, model, sm, level)
            pct <- format(100 * level)
            for (k in seq_along(mod)) {
                md <- mod[[k]]
                for (j in 1:3) {
                    r <- md$slopes[j, ]
                    values <- list(slope = r$slope)
                    if (md$boot) values <- c(values, list(se = r$se, lower = r$lower, upper = r$upper))
                    self$results$structuralSection$simpleSlopes$setRow(rowKey = paste(k, j, sep = "::"), values = values)
                }
                if (self$options$moderationPlot) {
                    item <- self$results$structuralSection$moderationPlots$get(key = paste(md$term, "\u2192", md$y))
                    labels <- list(levels = c(txt("moderation", "minus_sd_short"), txt("moderation", "mean_short"),
                                              txt("moderation", "plus_sd_short")),
                                   x_axis = txt("moderation", "x_axis"), y_axis = txt("moderation", "y_axis"),
                                   m_axis = txt("moderation", "m_axis"), effect_axis = txt("moderation", "effect_axis"),
                                   slopes = txt("moderation", "slopes_title"), floodlight = txt("moderation", "flood_title"))
                    caption <- if (md$boot) sprintf(txt("moderation", paste0("caption_boot", if (identical(sm$method, "jackknife")) "_jk" else "")), pct) else txt("moderation", "caption")
                    item$setState(.pl_moderation_state(md, labels, caption))
                }
            }
            note <- if (!is.null(sm)) sprintf(txt("moderation", paste0("table_note_boot", if (identical(sm$method, "jackknife")) "_jk" else "")), pct, sm$admissible)
                    else txt("moderation", "table_note")
            self$results$structuralSection$simpleSlopes$setNote("mod", note)
            mod
        },

        .plotModeration = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$lines)) return(FALSE)
            print(.pl_plot_moderation(image$state, ggtheme, theme))
            TRUE
        },

        .prepareDiagram = function(blocks, loadings, path_tab, r2_tab, sm, model, hocq = NULL) {
            image <- self$results$plotsSection$pathDiagram
            if (!self$options$pathDiagram) return()
            lang <- .pl_lang(self$options)
            caption <- if (is.null(sm)) .pl_text(lang, "labels", "diagram_caption")
                       else sprintf(.pl_text(lang, "labels", paste0("diagram_caption_boot", if (identical(sm$method, "jackknife")) "_jk" else "")),
                                    format(100 * as.numeric(self$options$ciLevel)))
            if (any(grepl(.PL_X, path_tab$from, fixed = TRUE)))
                caption <- paste(caption, .pl_text(lang, "labels", "diagram_moderation"))
            st <- .pl_sem_state(blocks, loadings, path_tab, r2_tab, sm, model,
                                indicators = isTRUE(self$options$diagramIndicators), caption = caption,
                                hocq = hocq)
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
                                         else sprintf(txt("labels", paste0("loadings_caption_boot", if (identical(sm$method, "jackknife")) "_jk" else "")), pct))
                if (is.null(st))
                    .pl_notice_before(self$results$plotsSection, self$options, "loadingsPlot",
                                      txt("notapply", "na_loadings_plot"))
                else {
                    self$results$plotsSection$loadingsPlot$setSize(600, max(260, 26 * nrow(st$data) + 30 * length(unique(st$data$construct)) + 70))
                    self$results$plotsSection$loadingsPlot$setState(st)
                }
            }
            if (self$options$htmtPlot) {
                st <- .pl_htmt_state(quality, sm, model, if (is.null(sm)) txt("labels", "htmt_caption")
                                     else txt("labels", paste0("htmt_caption_boot", if (identical(sm$method, "jackknife")) "_jk" else "")))
                if (is.null(st))
                    .pl_notice_before(self$results$plotsSection, self$options, "htmtPlot",
                                      txt("notapply", "na_htmt_plot"))
                else {
                    n <- length(st$levels)
                    self$results$plotsSection$htmtPlot$setSize(max(380, 95 * n + 140), max(340, 85 * n + 120))
                    self$results$plotsSection$htmtPlot$setState(st)
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
                private$.item(name)$setVisible(FALSE)
        },

        .fillModel = function(blocks, paths, txt, hocs = list()) {
            dims <- stats::setNames(rep(vapply(hocs, `[[`, "", "label"), vapply(hocs, function(h) length(h$components), 0)),
                                    unlist(lapply(hocs, `[[`, "components")))
            for (i in seq_along(blocks)) {
                b <- blocks[[i]]
                preds <- paths$from[paths$to == b$label]
                role <- if (b$label %in% names(dims)) sprintf(txt("labels", "dimension_of"), dims[[b$label]])
                        else txt("labels", if (length(preds)) "endogenous" else "exogenous")
                self$results$measurementSection$model$setRow(rowKey = i, values = list(
                    construct = b$label,
                    measurement = txt("labels", b$type),
                    indicators = length(b$vars),
                    role = role,
                    predictors = if (length(preds)) paste(preds, collapse = ", ") else txt("labels", "none")))
            }
            for (j in seq_along(hocs)) {
                h <- hocs[[j]]
                preds <- paths$from[paths$to == h$label]
                self$results$measurementSection$model$setRow(rowKey = paste0("hoc", j), values = list(
                    construct = h$label,
                    measurement = txt("labels", paste0(h$type, "_hoc")),
                    indicators = length(h$components),
                    role = txt("labels", if (length(preds)) "endogenous" else "exogenous"),
                    predictors = if (length(preds)) paste(preds, collapse = ", ") else txt("labels", "none")))
            }
        },

        # Measurement of the higher-order constructs (stage 2): one row per
        # lower-order construct, and reliability of the reflective ones.
        # ES: Medida de los constructos de orden superior (etapa 2).
        .fillHoc = function(sfit, hocs, smodel, txt, paths) {
            q <- .pl_hoc_quality(sfit, hocs, smodel$m2)
            q$htmt <- private$.fillHocHtmt(sfit, hocs, smodel, txt, paths)
            for (k in seq_len(nrow(q$measurement))) {
                r <- q$measurement[k, ]
                band <- if (r$type == "formative") .pl_band_vif(r$vif) else NA_character_
                values <- list(hoc = r$hoc, component = r$component, loading = r$loading, weight = r$weight,
                               vifAssessment = if (is.na(band)) "" else txt("labels", band))
                if (is.finite(r$vif)) values$vif <- r$vif
                self$results$measurementSection$hocMeasurement$setRow(rowKey = paste(r$hoc, r$component, sep = "::"), values = values)
            }
            rel <- q$reliability
            for (k in seq_len(if (is.null(rel)) 0 else nrow(rel))) {
                r <- rel[k, ]
                a <- c(.pl_band_reliability(r$rhoC), if (r$ave >= 0.5) "ave_ok" else "ave_low")
                self$results$measurementSection$hocReliability$setRow(rowKey = r$hoc, values = list(
                    hoc = r$hoc, alpha = r$alpha, rhoC = r$rhoC, ave = r$ave,
                    assessment = paste(vapply(a, function(x) txt("labels", x), ""), collapse = "; ")))
            }
            q
        },

        # Stage-2 HTMT of the higher-order constructs. cSEM's routine measures
        # the other constructs by a single score in stage 2, so the HTMT is
        # not defined there (Category B: rows stay, footnote).
        # ES: HTMT de la etapa 2 de los constructos de orden superior. La
        # rutina de cSEM mide los otros constructos con una sola puntuación,
        # así que ahí el HTMT no está definido (las filas se quedan).
        .fillHocHtmt = function(sfit, hocs, smodel, txt, paths) {
            hp <- .pl_hoc_htmt_pairs(hocs, smodel$blocks, paths, smodel$m2)
            if (is.null(hp)) return(NULL)
            table <- self$results$measurementSection$hocHtmt
            csem_like <- !identical(smodel$m2$approach, "disjoint")
            hp$htmt <- if (csem_like) NA_real_ else .pl_hoc_htmt(sfit, hp)
            hp$htmt2 <- if (csem_like) NA_real_ else .pl_hoc_htmt(sfit, hp, "htmt2")
            single <- vapply(smodel$blocks, function(b) length(b$vars) == 1, TRUE)
            single <- vapply(smodel$blocks, `[[`, "", "label")[single]
            for (k in seq_len(nrow(hp))) {
                key <- paste(hp$hoc[k], hp$other[k], sep = "::")
                band <- .pl_band_htmt(hp$htmt[k])
                values <- list(c1 = hp$hoc[k], sep = "\u2013", c2 = hp$other[k],
                               assessment = if (is.na(band)) txt("labels", "not_computable") else txt("labels", band))
                if (!is.na(hp$htmt[k])) values$htmt <- hp$htmt[k]
                if (!is.na(hp$htmt2[k])) values$htmt2 <- hp$htmt2[k]
                table$setRow(rowKey = key, values = values)
                if (csem_like)
                    table$addFootnote(rowKey = key, col = "htmt", txt("footnotes", "hoc_htmt_csem"))
                else if (is.na(hp$htmt[k]))
                    table$addFootnote(rowKey = key, col = "htmt", txt("footnotes", "htmt_signs"))
                else if (hp$other[k] %in% single)
                    table$addFootnote(rowKey = key, col = "htmt", sprintf(txt("footnotes", "htmt_single"), hp$other[k]))
            }
            hp
        },

        .fillHocBootstrap = function(sm, hocs, smodel, txt) {
            if (!is.null(sm$hoc_htmt))
                for (k in seq_len(nrow(sm$hoc_htmt))) {
                    ab <- strsplit(sm$hoc_htmt$name[k], "~", fixed = TRUE)[[1]]
                    inv <- stats::setNames(names(smodel$m2$cmap2), smodel$m2$cmap2)
                    key <- paste(inv[[ab[1]]], inv[[ab[2]]], sep = "::")
                    u <- sm$hoc_htmt$upper[k]
                    if (!is.finite(u)) next
                    self$results$measurementSection$hocHtmt$setCell(rowKey = key, col = "htmtUpper", value = u)
                    band <- if (u < 0.85) "htmt_ok_ci" else if (u < 0.90) "htmt_similar_ci" else "htmt_doubt_ci"
                    self$results$measurementSection$hocHtmt$setCell(rowKey = key, col = "assessment", value = txt("labels", band))
                    self$results$measurementSection$hocHtmt$setNote("ci", sprintf(txt("boot", paste0("htmt_note", if (identical(sm$method, "jackknife")) "_jk" else "")), sm$admissible))
                }
            set <- function(table, key, values) for (col in names(values)) {
                v <- values[[col]]
                if (length(v) == 1 && is.finite(v)) table$setCell(rowKey = key, col = col, value = v)
            }
            row_of <- function(df, name) if (is.null(df)) NULL else df[df$name == name, , drop = FALSE]
            for (h in hocs) {
                hh <- smodel$m2$hmap[[h$label]]
                for (cpt in h$components) {
                    ind <- smodel$m2$ind(cpt)
                    key <- paste(h$label, cpt, sep = "::")
                    l <- row_of(sm$hoc_loadings, paste(hh, "=~", ind))
                    w <- row_of(sm$hoc_weights, paste(hh, "<~", ind))
                    v <- row_of(sm$hoc_vif, paste(hh, ind, sep = "~"))
                    if (!is.null(l) && nrow(l)) set(self$results$measurementSection$hocMeasurement, key, list(loadLower = l$lower, loadUpper = l$upper))
                    if (!is.null(w) && nrow(w)) set(self$results$measurementSection$hocMeasurement, key, list(weightLower = w$lower,
                                                                                            weightUpper = w$upper, weightP = w$p))
                    if (!is.null(v) && nrow(v)) set(self$results$measurementSection$hocMeasurement, key, list(vifLower = v$lower, vifUpper = v$upper))
                }
                if (h$type == "reflective") {
                    r <- row_of(sm$hoc_rhoC, hh)
                    a <- row_of(sm$hoc_ave, hh)
                    if (!is.null(r) && nrow(r)) set(self$results$measurementSection$hocReliability, h$label, list(rhoCLower = r$lower, rhoCUpper = r$upper))
                    if (!is.null(a) && nrow(a)) set(self$results$measurementSection$hocReliability, h$label, list(aveLower = a$lower, aveUpper = a$upper))
                }
            }
            pct <- format(100 * as.numeric(self$options$ciLevel))
            note <- sprintf(txt("boot", paste0("ci_note", if (identical(sm$method, "jackknife")) "_jk" else "")), pct, sm$admissible)
            self$results$measurementSection$hocMeasurement$setNote("ci", note)
            self$results$measurementSection$hocReliability$setNote("ci", note)
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
                    self$results$measurementSection$measurement$setRow(rowKey = paste(i, v, sep = "::"), values = list(
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
                self$results$structuralSection$paths$setRow(rowKey = k, values = list(
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
                self$results$structuralSection$r2$setRow(rowKey = out$construct[k], values = list(
                    construct = out$construct[k], r2 = out$r2[k], r2adj = out$r2adj[k]))
            }
            out
        },

        # ---------------------------------------------------------------------
        # Interpretation: what happened, why, what it means, what to do.
        # ES: Interpretación: qué ocurrió, por qué, qué significa, qué hacer.
        # ---------------------------------------------------------------------
        .interpretation = function(lang, blocks, loadings, paths, r2, quality, structural = NULL, sm = NULL,
                                   model = NULL, mod = NULL, hocq = NULL, omf = NULL, med = NULL, prd = NULL, ipm = NULL,
                                   mga = NULL, nca = NULL, cmp = NULL) {
            txt <- function(key) .pl_text(lang, "interp", key)
            listed <- function(names, values)
                paste(sprintf("%s (%s)", .pl_short(names), vapply(values, .pl_fmt, "")), collapse = ", ")
            meas <- private$.measurementLines(txt, loadings, quality, sm, model, listed)
            struct <- private$.structuralLines(txt, paths, r2, structural, sm, model, listed)
            mlines <- private$.moderationLines(lang, mod, structural, sm, model)
            hlines <- if (is.null(hocq)) character(0) else private$.hocLines(txt, hocq, sm, model, listed)
            flines <- private$.fitLines(txt, omf)
            dlines <- private$.mediationLines(txt, med)
            plines <- private$.predictLines(txt, prd)
            ilines <- private$.ipmaLines(txt, ipm)
            nlines <- private$.ncaLines(txt, nca)
            glines <- private$.mgaLines(txt, mga)
            clines <- private$.compareLines(txt, cmp)
            # Jackknife (1.2.0): the sections whose intervals come from the
            # main resampling name it; the term is the same loanword in
            # English and Spanish. The multigroup tests and the overall fit
            # test always bootstrap, so their lines are left as they are.
            # ES: Jackknife (1.2.0): las secciones cuyos intervalos vienen
            # del remuestreo principal lo nombran; el término es el mismo
            # préstamo en inglés y español. Las pruebas multigrupo y la de
            # ajuste global siempre usan bootstrap, así que no se tocan.
            if (identical(sm$method, "jackknife")) {
                jk <- function(x) gsub("bootstrap", "jackknife", x, fixed = TRUE)
                meas <- jk(meas); struct <- jk(struct); mlines <- jk(mlines); hlines <- jk(hlines)
                dlines <- jk(dlines); ilines <- jk(ilines); nlines <- jk(nlines)
            }
            html <- paste0(.pl_html_block(meas, txt("title_measurement")),
                           if (length(hlines)) .pl_html_block(hlines, txt("title_hoc")) else "",
                           .pl_html_block(c(struct, mlines), txt("title_structural")),
                           if (length(dlines)) .pl_html_block(dlines, txt("title_mediation")) else "",
                           if (length(plines)) .pl_html_block(plines, txt("title_predict")) else "",
                           if (length(clines)) .pl_html_block(clines, txt("title_compare")) else "",
                           if (length(ilines)) .pl_html_block(ilines, txt("title_ipma")) else "",
                           if (length(nlines)) .pl_html_block(nlines, txt("title_nca")) else "",
                           if (length(glines)) .pl_html_block(glines, txt("title_mga")) else "",
                           if (length(flines)) .pl_html_block(flines, txt("title_fit")) else "",
                           .pl_html_block(txt(if (is.null(sm)) "next_steps" else if (identical(sm$method, "jackknife"))
                                                  "next_steps_jk" else "next_steps_boot"), txt("title_next")))
            self$results$interpretation$setContent(html)
        },

        # Interpretation of the measurement model: loadings, reliability,
        # convergent and discriminant validity, collinearity and weights.
        # ES: Interpretación del modelo de medida.
        .measurementLines = function(txt, loadings, quality, sm, model, listed) {
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
                        mm <- if (is.null(model$mcmap)) model$cmap else model$mcmap
                        r <- sm$HTMT$upper[sm$HTMT$name == paste(mm[[h$c1[k]]], mm[[h$c2[k]]], sep = "~")]
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
                        r <- sm$weights[sm$weights$name == paste((if (is.null(model$mcmap)) model$cmap else model$mcmap)[[form$construct[k]]], "<~",
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
            # Mardia (1.2.0)
            mt <- quality$mardia
            if (!is.null(mt)) {
                dep <- mt$p < 0.05
                meas <- c(meas, sprintf(txt(if (any(dep)) "mardia_dep" else "mardia_ok"),
                                        .pl_fmt(mt$value[1]), .pl_fmt_p(mt$p[1]), .pl_fmt(mt$value[2]),
                                        .pl_fmt(mt$statistic[2]), .pl_fmt_p(mt$p[2])))
            }
            meas
        },

        # Interpretation of the structural model: strongest path,
        # significance, R², f² and collinearity.
        # ES: Interpretación del modelo estructural.
        .structuralLines = function(txt, paths, r2, structural, sm, model, listed) {
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
                f <- f[!grepl(.PL_X, f$from, fixed = TRUE), , drop = FALSE]
                big <- !is.na(f$f2) & f$f2 >= 0.15
                if (any(big)) struct <- c(struct, sprintf(txt("f2_big"), listed(paste(f$from[big], f$to[big], sep = " \u2192 "), f$f2[big])))
                tiny <- !is.na(f$f2) & f$f2 < 0.02
                if (any(tiny)) struct <- c(struct, sprintf(txt("f2_tiny"), listed(paste(f$from[tiny], f$to[tiny], sep = " \u2192 "), f$f2[tiny])))
                vs <- structural$vif
                hv <- !is.na(vs$vif) & vs$vif >= 3
                struct <- c(struct, if (any(hv)) sprintf(txt("vifs_high"), listed(paste(vs$from[hv], vs$to[hv], sep = " \u2192 "), vs$vif[hv]))
                                    else txt("vifs_ok"))
            }
            struct
        },

        # Interpretation of each interaction: coefficient, f², significance
        # and Johnson-Neyman region.
        # ES: Interpretación de cada interacción.
        .moderationLines = function(lang, mod, structural, sm, model) {
            mtxt <- function(key) .pl_text(lang, "moderation", key)
            txt_band <- function(v) .pl_text(lang, "labels", .pl_band_f2_int(v))
            mlines <- character(0)
            for (md in mod) {
                head <- sprintf(mtxt("line"), md$term, md$y, .pl_fmt(md$bxm), md$x, md$y, md$m)
                f2i <- structural$f2$f2[structural$f2$from == md$term & structural$f2$to == md$y]
                if (length(f2i) && is.finite(f2i[1]))
                    head <- paste(head, sprintf(mtxt("f2_line"), .pl_fmt(f2i[1]), txt_band(f2i[1])))
                if (md$boot) {
                    r <- sm$paths[sm$paths$name == paste(model$cmap[[md$y]], "~", model$cmap[[md$term]]), , drop = FALSE]
                    if (nrow(r) == 1 && is.finite(r$lower))
                        head <- paste(head, if (r$lower > 0 || r$upper < 0) sprintf(mtxt("int_sig"), md$m, md$x, md$y)
                                            else sprintf(mtxt("int_ns"), md$m, md$x, md$y))
                    reg <- .pl_jn_region(md)
                    tail <- switch(reg$key,
                        jn_all = sprintf(mtxt("jn_all"), md$x, md$y, md$m),
                        jn_none = sprintf(mtxt("jn_none"), md$x, md$y, md$m),
                        jn_some = sprintf(mtxt("jn_some"), md$x, md$y, md$m,
                                          paste(formatC(reg$points, format = "f", digits = 2), collapse = ", ")))
                    head <- paste(head, tail)
                } else head <- paste(head, mtxt("no_boot"))
                mlines <- c(mlines, head)
            }
            mlines
        },

        # Interpretation of the mediation: each specific indirect effect with
        # its interval and what its type means.
        # ES: Interpretación de la mediación.
        .mediationLines = function(txt, med) {
            if (is.null(med) || !nrow(med)) return(character(0))
            if (all(is.na(med$type))) return(txt("med_no_boot"))
            out <- character(0)
            for (k in seq_len(nrow(med))) {
                ci <- sprintf("[%s; %s]", .pl_fmt(med$lower[k]), .pl_fmt(med$upper[k]))
                out <- c(out, paste(sprintf(txt("med_line"), med$label[k], .pl_fmt(med$estimate[k]), ci),
                                    txt(paste0("med_", med$type[k]))))
            }
            multi <- any(duplicated(paste(med$from, med$to)))
            c(out, if (multi) txt("med_multiple"), txt("med_caution"), txt("med_next"))
        },

        # Interpretation of the IPMA: per target, the constructs to improve
        # first (above-average importance, below-average performance).
        # ES: Interpretación del IPMA: por objetivo, qué mejorar primero.
        .ipmaLines = function(txt, ipm) {
            if (is.null(ipm)) return(character(0))
            if (!is.null(ipm$reason)) return(sprintf(txt("ipma_none"), ipm$reason))
            if (is.null(ipm$table)) return(character(0))
            ip <- ipm$table
            out <- character(0)
            for (tg in unique(ip$to)) {
                r <- ip[ip$to == tg, ]
                if (nrow(r) == 1) next
                ns <- !is.na(r$sig) & !r$sig
                pr <- r$from[r$quadrant == "q_priority" & !ns]
                pr_ns <- r$from[r$quadrant == "q_priority" & ns]
                top <- r$from[which.max(r$importance)]
                out <- c(out, if (length(pr)) sprintf(txt("ipma_priority"), tg, paste(pr, collapse = ", "))
                              else sprintf(txt("ipma_no_priority"), tg),
                         if (length(pr_ns)) sprintf(txt("ipma_priority_ns"), paste(pr_ns, collapse = ", ")),
                         sprintf(txt("ipma_top"), top, .pl_fmt(max(r$importance)), tg))
            }
            c(out, if (!isTRUE(ipm$boot)) txt("ipma_no_boot_interp"), txt("ipma_caution"), txt("ipma_next"))
        },

        # Interpretation of the NCA: which constructs are necessary for each
        # outcome, the bottleneck that matters, the combined reading with the
        # PLS-SEM paths, cautions and next step.
        # ES: Interpretación del NCA: qué constructos son necesarios para cada
        # resultado, el cuello de botella relevante, la lectura combinada con
        # las rutas de PLS-SEM, precauciones y siguiente paso.
        .ncaLines = function(txt, nca) {
            if (is.null(nca)) return(character(0))
            if (!is.null(nca$reason)) return(sprintf(txt("nca_none"), nca$reason))
            if (is.null(nca$table)) return(character(0))
            tb <- nca$table
            cl <- .pl_text(.pl_lang(self$options), "labels", paste0("nca_", nca$ceiling))
            out <- character(0)
            for (k in which(tb$necessary)) {
                out <- c(out, sprintf(txt("nca_line_nec"), tb$from[k], tb$to[k], cl, .pl_fmt(tb$d[k]), .pl_fmt_p(tb$p[k])))
                b <- nca$bottleneck[[paste(tb$to[k], tb$from[k], sep = "::")]]
                j <- which(is.finite(b) & b > 0)
                if (length(j)) {
                    j <- max(j)
                    out <- c(out, sprintf(txt("nca_line_bn"), tb$to[k], format(.PL_NCA_STEPS[j]), tb$from[k],
                                                  formatC(b[j], format = "f", digits = 1)))
                }
            }
            if (!any(tb$necessary)) out <- c(out, sprintf(txt("nca_line_none"), cl))
            small <- !tb$necessary & !is.na(tb$d) & tb$d < 0.1 & !is.na(tb$p) & tb$p < 0.05
            if (any(small))
                out <- c(out, sprintf(txt("nca_line_small"), paste(sprintf("%s \u2192 %s", tb$from[small], tb$to[small]), collapse = ", ")))
            if (isTRUE(nca$boot)) {
                must <- tb$reading == "nca_must"
                both <- tb$reading == "nca_both"
                should <- tb$reading == "nca_should"
                lst <- function(i) paste(sprintf("%s \u2192 %s", tb$from[i], tb$to[i]), collapse = ", ")
                if (any(both)) out <- c(out, sprintf(txt("nca_combo_both"), lst(both)))
                if (any(must)) out <- c(out, sprintf(txt("nca_combo_must"), lst(must)))
                if (any(should)) out <- c(out, sprintf(txt("nca_combo_should"), lst(should)))
            } else {
                out <- c(out, txt("nca_no_boot_interp"))
            }
            c(out, txt("nca_caution"), txt("nca_next"))
        },

        # Interpretation of PLSpredict (Shmueli et al., 2019: Q2predict > 0,
        # then the share of indicators where PLS has a lower RMSE than LM)
        # and CVPAT (the overall test against IA and LM).
        # ES: Interpretación de PLSpredict y CVPAT.
        .predictLines = function(txt, prd) {
            if (is.null(prd)) return(character(0))
            if (!is.null(prd$reason)) return(sprintf(txt("predict_none"), prd$reason))
            if (is.null(prd$metrics)) return(character(0))
            m <- prd$metrics
            out <- character(0)
            neg <- m$q2 <= 0
            out <- c(out, if (any(neg)) sprintf(txt("q2_neg"), paste(m$indicator[neg], collapse = ", "))
                          else sprintf(txt("q2_pos"), nrow(m)))
            k <- sum(m$pls_better); n <- nrow(m)
            level <- if (k == n) "power_high" else if (k >= n / 2) "power_medium" else if (k > 0) "power_low" else "power_none"
            out <- c(out, sprintf(txt(level), k, n))
            asym <- m$indicator[m$metric == "MAE"]
            if (length(asym)) out <- c(out, sprintf(txt("predict_mae_used"), paste(asym, collapse = ", ")))
            cv <- prd$cvpat[prd$cvpat$group == "all", ]
            for (b in c("IA", "LM")) {
                r <- cv[cv$bench == b, ]
                out <- c(out, sprintf(txt(if (r$p < 0.05 && r$diff < 0) "cvpat_better" else "cvpat_not"),
                                      txt(paste0("bench_long_", b)), .pl_fmt(r$diff), .pl_fmt(r$t), .pl_fmt_p(r$p)))
            }
            r <- cv[cv$bench == "ALT", ]
            if (nrow(r) == 1) {
                key <- if (r$p >= 0.05) "cvpat_alt_same" else if (r$diff < 0) "cvpat_alt_worse" else "cvpat_alt_better"
                out <- c(out, sprintf(txt(key), prd$drop, .pl_fmt(r$diff), .pl_fmt(r$t), .pl_fmt_p(r$p)))
            }
            r <- cv[cv$bench == "BM", ]
            if (nrow(r) == 1 && !is.null(prd$bench)) {
                key <- if (r$p >= 0.05) "cvpat_bm_same" else if (r$diff < 0) "cvpat_bm_worse" else "cvpat_bm_better"
                out <- c(out, sprintf(txt(key), prd$bench, .pl_fmt(r$diff), .pl_fmt(r$t), .pl_fmt_p(r$p)))
            }
            if (isTRUE(prd$small$small))
                out <- c(out, sprintf(txt("predict_small_interp"), prd$small$test, prd$small$train, prd$small$n_exog))
            ia <- cv[cv$bench == "IA", ]
            c(out, txt("predict_caution"),
              txt(if (any(m$q2 <= 0) || !(ia$p < 0.05 && ia$diff < 0)) "predict_next_weak" else "predict_next"))
        },

        # Interpretation of the model comparison (1.2.0): what BIC and GM
        # say for each outcome. ES: Interpretación de la comparación de
        # modelos (1.2.0): qué dicen BIC y GM para cada resultado.
        .compareLines = function(txt, cmp) {
            if (is.null(cmp)) return(character(0))
            if (!is.null(cmp$reason)) return(sprintf(txt("compare_none"), cmp$reason))
            tb <- cmp$table
            lang <- .pl_lang(self$options)
            pl <- function(k) tolower(.pl_text(lang, "labels", paste0("pref_", k)))
            out <- character(0)
            # only the outcomes whose equation loses a path: the others
            # differ only through the weights
            # ES: solo los resultados cuya ecuación pierde una ruta
            for (cn in intersect(unique(tb$construct), cmp$changed)) {
                b <- tb$preferred[tb$construct == cn & tb$criterion == "BIC"]
                g <- tb$preferred[tb$construct == cn & tb$criterion == "GM"]
                if (is.na(b) || is.na(g)) next
                out <- c(out, if (b == g && b != "equal")
                    sprintf(txt("compare_both"), cn, pl(b), cmp$drop, txt(if (b == "main") "compare_drop_worse" else "compare_drop_better"))
                else sprintf(txt("compare_split"), cn, pl(b), pl(g)))
            }
            c(out, txt("compare_next"))
        },

        # Interpretation of the overall fit test: exact fit kept or rejected
        # for each model, the SRMR against the descriptive 0.08 rule, and
        # what to do next.
        # ES: Interpretación de la prueba de ajuste global.
        .fitLines = function(txt, omf) {
            if (is.null(omf)) return(character(0))
            if (!is.null(omf$reason)) return(sprintf(txt("fit_none"), omf$reason))
            if (is.null(omf$table)) return(character(0))
            ft <- omf$table
            lvl <- format(100 * omf$level)
            line <- function(mdl) {
                r <- ft[ft$model == mdl, , drop = FALSE]
                rej <- r$measure[r$reject]
                srmr <- r[r$measure == "SRMR", ]
                head <- if (!length(rej)) sprintf(txt(paste0("fit_keep_", mdl)), lvl)
                        else sprintf(txt(paste0("fit_reject_", mdl)), paste(.PL_FIT[rej], collapse = ", "), lvl)
                paste(head, sprintf(txt("fit_srmr"), .pl_fmt(srmr$value), .pl_fmt(srmr$critical)))
            }
            out <- c(line("estimated"), if (!isTRUE(omf$same)) line("saturated") else txt("fit_same_interp"))
            est <- ft[ft$model == "estimated", ]; sat <- ft[ft$model == "saturated", ]
            srmr_e <- est$value[est$measure == "SRMR"]
            if (any(est$reject)) {
                out <- c(out, if (!any(sat$reject) && !isTRUE(omf$same)) txt("fit_structural_blame")
                              else txt("fit_measurement_blame"))
                if (srmr_e < 0.08) out <- c(out, txt("fit_srmr_low_reject"))
            }
            out
        },

        # Interpretation of the higher-order constructs (stage 2): loadings,
        # reliability and AVE of the reflective ones, weights and VIF of the
        # formative ones, and their HTMT with the other reflective constructs.
        # ES: Interpretación de los constructos de orden superior (etapa 2).
        .hocLines = function(txt, hocq, sm, model, listed) {
            m2 <- model$m2
            q <- hocq$measurement
            out <- character(0)
            ci_of <- function(df, name) {
                r <- if (is.null(df)) NULL else df[df$name == name, , drop = FALSE]
                if (!is.null(r) && nrow(r) == 1 && is.finite(r$lower)) r else NULL
            }
            for (h in model$hocs) {
                r <- q[q$hoc == h$label, , drop = FALSE]
                hh <- m2$hmap[[h$label]]
                if (h$type == "reflective") {
                    low <- r$loading < 0.708
                    out <- c(out, if (any(low)) sprintf(txt("hoc_loadings_low"), h$label, listed(r$component[low], r$loading[low]))
                                  else sprintf(txt("hoc_loadings_ok"), h$label, listed(r$component, r$loading)))
                    if (!is.null(sm)) {
                        ns <- vapply(r$component, function(cpt) {
                            ci <- ci_of(sm$hoc_loadings, paste(hh, "=~", m2$ind(cpt)))
                            !is.null(ci) && ci$lower <= 0 && ci$upper >= 0
                        }, TRUE)
                        if (any(ns)) out <- c(out, sprintf(txt("hoc_loadings_ns"), listed(r$component[ns], r$loading[ns])))
                    }
                    rel <- hocq$reliability[hocq$reliability$hoc == h$label, , drop = FALSE]
                    if (nrow(rel)) {
                        rc <- rel$rhoC[1]; av <- rel$ave[1]
                        out <- c(out, if (rc > 0.95) sprintf(txt("hoc_rel_high"), h$label, .pl_fmt(rc))
                                      else if (rc < 0.70) sprintf(txt("hoc_rel_low"), h$label, .pl_fmt(rc))
                                      else if (av >= 0.50) sprintf(txt("hoc_rel_ok"), h$label, .pl_fmt(rc), .pl_fmt(av)))
                        if (av < 0.50) out <- c(out, sprintf(txt("hoc_ave_low"), h$label, .pl_fmt(av)))
                    }
                } else {
                    out <- c(out, sprintf(txt("hoc_weights"), h$label, listed(r$component, r$weight)))
                    if (!is.null(sm)) {
                        ns <- vapply(r$component, function(cpt) {
                            ci <- ci_of(sm$hoc_weights, paste(hh, "<~", m2$ind(cpt)))
                            !is.null(ci) && ci$lower <= 0 && ci$upper >= 0
                        }, TRUE)
                        if (any(ns)) out <- c(out, sprintf(txt("hoc_weights_ns"), listed(r$component[ns], r$loading[ns])))
                    }
                    hv <- !is.na(r$vif) & r$vif >= 3
                    if (any(hv)) out <- c(out, sprintf(txt("hoc_vif_high"), h$label, listed(r$component[hv], r$vif[hv])))
                }
            }
            hp <- hocq$htmt
            if (!is.null(hp) && any(!is.na(hp$htmt))) {
                pair <- paste(hp$hoc, hp$other, sep = "\u2013")
                ok <- !is.na(hp$htmt)
                high <- ok & hp$htmt >= 0.85
                out <- c(out, if (any(high)) sprintf(txt("hoc_htmt_high"), listed(pair[high], hp$htmt[high]))
                              else sprintf(txt("hoc_htmt_ok"), listed(pair[ok], hp$htmt[ok])))
                if (!is.null(sm$hoc_htmt)) {
                    up <- vapply(seq_len(nrow(hp)), function(k) {
                        u <- sm$hoc_htmt$upper[sm$hoc_htmt$name == paste(hp$a[k], hp$b[k], sep = "~")]
                        if (length(u) && is.finite(u[1])) u[1] else NA_real_
                    }, 0)
                    bad <- !is.na(up) & up >= 0.85
                    out <- c(out, if (any(bad)) sprintf(txt("hoc_htmt_ci_high"), listed(pair[bad], up[bad]))
                                  else sprintf(txt("hoc_htmt_ci_ok"), sum(!is.na(up))))
                }
            }
            out
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
                               model = NULL, hocq = NULL, omf = NULL, med = NULL, prd = NULL, mga = NULL,
                               nca = NULL, cmp = NULL) {
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
            hq <- hocq
            if (!is.null(hq$reliability) && nrow(hq$reliability)) {
                rc <- hq$reliability$rhoC
                findings <- c(findings, sprintf(txt("hoc_rel"), sum(rc >= 0.70 & rc <= 0.95), length(rc)))
            }
            if (!is.null(hq$htmt) && any(!is.na(hq$htmt$htmt)))
                findings <- c(findings, sprintf(txt("hoc_htmt"), sum(!is.na(hq$htmt$htmt) & hq$htmt$htmt < 0.85),
                                                sum(!is.na(hq$htmt$htmt))))
            k <- which.max(r2$r2)
            findings <- c(findings, sprintf(txt("r2"), .pl_fmt(r2$r2[k]), r2$construct[k]))
            if (!is.null(med) && nrow(med) && any(!is.na(med$type))) {
                sig <- !is.na(med$lower) & (med$lower > 0 | med$upper < 0)
                findings <- c(findings, sprintf(txt("med"), sum(sig), nrow(med)))
            }
            if (!is.null(prd$metrics)) {
                m <- prd$metrics
                findings <- c(findings, sprintf(txt("predict"), sum(m$q2 > 0), nrow(m), sum(m$pls_better)))
            }
            if (!is.null(mga$mgd)) {
                both <- sum(mga$mgd$decision == "mgd_both")
                findings <- c(findings, if (!is.null(mga$micom))
                    sprintf(txt("mga"), mga$group, sum(mga$micom$verdict != "inv_none"), nrow(mga$micom), both, nrow(mga$mgd))
                else sprintf(txt("mga_no_micom"), mga$group, both, nrow(mga$mgd)))
            }
            # NCA and MGCFA (1.1.0)
            # ES: NCA y AFCMG (1.1.0)
            if (!is.null(nca$table)) {
                nt <- nca$table
                findings <- c(findings, if (any(nt$necessary))
                    sprintf(txt("nca"), sum(nt$necessary), nrow(nt), .pl_text(lang, "labels", paste0("nca_", nca$ceiling)),
                            paste(sprintf("%s \u2192 %s", nt$from[nt$necessary], nt$to[nt$necessary]), collapse = ", "))
                else sprintf(txt("nca_none"), nrow(nt), .pl_text(lang, "labels", paste0("nca_", nca$ceiling))))
            }
            if (!is.null(cmp$table)) {
                ct <- cmp$table
                both <- function(w) sum(vapply(intersect(unique(ct$construct), cmp$changed), function(cn)
                    identical(ct$preferred[ct$construct == cn & ct$criterion == "BIC"], w) &&
                    identical(ct$preferred[ct$construct == cn & ct$criterion == "GM"], w), TRUE))
                findings <- c(findings, sprintf(txt("compare"), both("main"), length(intersect(unique(ct$construct), cmp$changed)), cmp$drop,
                                                both("competing")))
            }
            mt <- mga$mgcfa$table
            if (!is.null(mt)) {
                held <- c("mgcfa_good", "mgcfa_acceptable", "mgcfa_supported")
                level <- if (mt$decision[3] %in% held) "mgcfa_scalar" else if (mt$decision[2] %in% held) "mgcfa_metric"
                         else if (mt$decision[1] %in% held) "mgcfa_configural" else NA
                findings <- c(findings, if (is.na(level)) txt("mgcfa_none")
                                        else sprintf(txt("mgcfa"), tolower(.pl_text(lang, "labels", level))))
            }
            if (!is.null(omf$table)) {
                e <- omf$table[omf$table$model == "estimated", ]
                lv <- format(100 * omf$level)
                sr <- .pl_fmt(e$value[e$measure == "SRMR"])
                findings <- c(findings, if (any(e$reject)) sprintf(txt("fit_reject"), sum(e$reject), lv, sr)
                                        else sprintf(txt("fit_keep"), lv, sr))
            }
            uncertain <- txt("no_inference")
            if (!is.null(sm)) {
                sig <- vapply(seq_len(nrow(paths)), function(i) {
                    rr <- sm$paths[sm$paths$name == paste(model$cmap[[paths$to[i]]], "~", model$cmap[[paths$from[i]]]), ,
                                   drop = FALSE]
                    nrow(rr) == 1 && is.finite(rr$lower) && (rr$lower > 0 || rr$upper < 0)
                }, TRUE)
                pct <- format(100 * as.numeric(self$options$ciLevel))
                findings <- c(findings, sprintf(txt("paths_sig"), sum(sig), nrow(paths), pct))
                uncertain <- if (identical(sm$method, "jackknife")) sprintf(txt("jk_basis"), sm$admissible, sm$runs)
                             else sprintf(txt("boot_basis"), sm$admissible, sm$runs, as.integer(self$options$bootSeed))
            }
            warnings <- c(if (!admissible) txt("admissible_no"),
                          private$.negativeLoadings(loadings, function(section, key) .pl_text(lang, section, key)),
                          private$.htmtWarning(txt, quality, sm, model, hocq))
            section <- function(title, items)
                paste0("<p><strong>", .pl_html_escape(title), "</strong></p>",
                       .pl_html_list(if (length(items)) items else txt("none")))
            self$results$keySummary$setContent(paste0(
                section(txt("findings"), findings), section(txt("uncertain"), uncertain),
                section(txt("warnings"), warnings)))
        },

        # Pairs without discriminant validity even on the lenient threshold
        # (0.90; Henseler et al., 2015): with the bootstrap the upper bound,
        # otherwise the estimate. First-order pairs and the stage-2 pairs of
        # the higher-order constructs.
        # ES: Pares sin validez discriminante ni con el umbral permisivo
        # (0.90): con bootstrap, el límite superior; si no, la estimación.
        .htmtWarning = function(txt, quality, sm, model, hocq) {
            boot <- !is.null(sm)
            lab <- character(0); val <- numeric(0)
            h <- quality$htmt
            mm <- if (is.null(model$mcmap)) model$cmap else model$mcmap
            for (k in seq_len(if (is.null(h)) 0 else nrow(h))) {
                v <- if (boot) {
                    r <- sm$HTMT$upper[sm$HTMT$name == paste(mm[[h$c1[k]]], mm[[h$c2[k]]], sep = "~")]
                    if (length(r)) r[1] else NA_real_
                } else h$htmt[k]
                if (is.finite(v) && v >= 0.90) { lab <- c(lab, paste(h$c1[k], h$c2[k], sep = "\u2013")); val <- c(val, v) }
            }
            hp <- hocq$htmt
            for (k in seq_len(if (is.null(hp)) 0 else nrow(hp))) {
                v <- if (boot) {
                    r <- sm$hoc_htmt$upper[sm$hoc_htmt$name == paste(hp$a[k], hp$b[k], sep = "~")]
                    if (length(r)) r[1] else NA_real_
                } else hp$htmt[k]
                if (is.finite(v) && v >= 0.90) { lab <- c(lab, paste(hp$hoc[k], hp$other[k], sep = "\u2013")); val <- c(val, v) }
            }
            if (!length(lab)) return(character(0))
            sprintf(txt(if (boot) "htmt_fail_ci" else "htmt_fail"),
                    paste(sprintf("%s (%s)", lab, vapply(val, .pl_fmt, "")), collapse = ", "))
        },

        .choices = function(lang, prep, boot = NULL) {
            if (!self$options$showChoicesGuide) return()
            txt <- function(key) .pl_text(lang, "choices", key)
            scheme <- c(path = "path", centroid = "centroid", factorial = "factorial")[[self$options$innerScheme]]
            wa <- self$options$weightApproach
            paras <- c(txt(self$options$estimator),
                       if (wa != "pls") txt(paste0("approach_", wa)) else sprintf(txt("scheme"), scheme),
                       txt(self$options$indicatorScale),
                       if (self$options$missing == "mean") sprintf(txt("mean"), prep$n_replaced)
                       else sprintf(txt("listwise"), prep$n_used, prep$n_total),
                       if (self$options$bootstrap && self$options$resampleMethod == "jackknife")
                           sprintf(txt("jackknife"), prep$n_used, format(100 * as.numeric(self$options$ciLevel)))
                       else if (self$options$bootstrap)
                           sprintf(txt("bootstrap"), as.integer(self$options$bootReps), as.integer(self$options$bootSeed),
                                   format(100 * as.numeric(self$options$ciLevel)))
                       else txt("no_bootstrap"),
                       if (self$options$ncaRun) sprintf(txt(paste0("nca_", self$options$ncaCeiling)), as.integer(self$options$ncaPerm)),
                       if (self$options$mgaRun && self$options$mgcfa) txt(paste0("mgcfa_", self$options$mgcfaEstimator)))
            self$results$choicesGuide$setContent(.pl_html_block(paras))
        },

        .guides = function(lang) {
            if (!self$options$showGuides) return()
            txt <- function(key) .pl_text(lang, "guide", key)
            self$results$measurementSection$modelGuide$setContent(.pl_html_block(txt("model")))
            self$results$measurementSection$measurementGuide$setContent(.pl_html_block(txt("measurement")))
            self$results$measurementSection$reliabilityGuide$setContent(.pl_html_block(c(txt("reliability"), txt("reliability_omega"))))
            self$results$measurementSection$htmtGuide$setContent(.pl_html_block(txt("htmt")))
            self$results$measurementSection$vifGuide$setContent(.pl_html_block(txt("vif")))
            self$results$structuralSection$structuralGuide$setContent(.pl_html_block(c(txt("f2"), txt("vif_struct"), txt("effects"))))
            self$results$structuralSection$pathsGuide$setContent(.pl_html_block(txt("paths")))
            self$results$structuralSection$moderationGuide$setContent(.pl_html_block(c(txt("moderation"), txt("moderation_f2"))))
            self$results$measurementSection$hocGuide$setContent(.pl_html_block(c(txt("hoc"), txt("hoc_htmt"))))
            self$results$structuralSection$mediationGuide$setContent(.pl_html_block(c(txt("mediation"), txt("mediation_types"))))
            self$results$ipmaSection$ipmaGuide$setContent(.pl_html_block(c(txt("ipma"), txt("ipma_reading"))))
            self$results$compareSection$compareGuide$setContent(.pl_html_block(txt("compare")))
            self$results$ncaSection$ncaGuide$setContent(.pl_html_block(c(txt("nca"), txt("nca_reading"), txt("nca_bottleneck"),
                                                                         txt("nca_combined"))))
            self$results$predictSection$predictGuide$setContent(.pl_html_block(c(txt("predict"), txt("predict_metrics"), txt("q2_clarify"),
                                                                 txt("predict_power"), txt("cvpat"))))
            self$results$mgaSection$mgaGuide$setContent(.pl_html_block(c(txt("mga"), txt("micom"), txt("mgcfa"), txt("mgd"),
                                                                                    txt("mga_cautions"))))
            self$results$fitSection$fitGuide$setContent(.pl_html_block(c(txt("fit"), txt("fit_saturated"), txt("fit_not_shown"))))
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
            m2 <- model$m2
            if (!is.null(model$hocs)) paths <- .pl_stage1_paths(paths, model$hocs)
            version <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
            # Versions go into the code as a comment, not into the report
            # text (Archie's request).
            # ES: Las versiones van como comentario del código, no en el
            # texto del informe (a pedido de Archie).
            self$results$reproducibility$setContent(.pl_html_block(txt("code")))
            q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
            cm <- model$cmap[!grepl(.PL_X, names(model$cmap), fixed = TRUE)]
            map_c <- paste0("#   ", cm, " = ", names(cm))
            map_v <- paste0("#   ", model$vmap, " = ", names(model$vmap))
            modes_of <- function(m) if (length(m))
                paste0("list(", paste(sprintf("%s = \"%s\"", names(m), unlist(m)), collapse = ", "), ")")
            else "NULL"
            modes <- modes_of(model$modes)
            # Weighting approach (1.2.0): PLS modes only under PLS-PM
            # ES: Enfoque de ponderación (1.2.0): modos PLS solo con PLS-PM
            wa <- .pl_approach(self$options$weightApproach)
            if (wa != "PLS-PM") modes <- sprintf("NULL, .approach_weights = \"%s\"", wa)
            missing_line <- if (self$options$missing == "mean")
                "for (v in names(d)) d[[v]][is.na(d[[v]])] <- mean(d[[v]], na.rm = TRUE)"
            else "d <- d[complete.cases(d), ]"
            code <- c(
                sprintf("# PLSsemLab %s, cSEM %s, %s", version("PLSsemLab"), version("cSEM"), R.version.string),
                "# Model as defined in jamovi:",
                paste0("#   ", if (is.null(m2)) model$readable else c(m2$readable, "# Stage 1 (higher-order constructs replaced by their dimensions):", paste0("#   ", model$readable))),
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
            if (!is.null(m2) && identical(m2$approach, "csem")) {
                # cSEM's second-order routine: one call; stage 1 is its
                # First_stage, stage 2 its Second_stage.
                # ES: La rutina de segundo orden de cSEM: una llamada.
                code <- c(code, "",
                    "# cSEM's second-order routine: stage 1 = fit_2nd$First_stage, stage 2 = fit_2nd$Second_stage",
                    "model_2nd <- \"", model$csem_syntax, "\"",
                    sprintf("fit_2nd <- cSEM::csem(d, model_2nd, .PLS_modes = %s, .PLS_weight_scheme_inner = \"%s\")  # cSEM sets .disattenuate = TRUE",
                            modes, self$options$innerScheme),
                    "cSEM::summarize(fit_2nd)",
                    "# The same stage 2 as one plain call (used for the bootstrap):",
                    "d2 <- as.data.frame(fit_2nd$First_stage$Estimates$Construct_scores)",
                    "model2 <- \"", m2$syntax, "\"",
                    sprintf("fit2 <- cSEM::csem(d2, model2, .disattenuate = TRUE, .PLS_weight_scheme_inner = \"%s\")", self$options$innerScheme))
            } else if (!is.null(m2)) {
                # Stage 2 of the disjoint two-stage approach.
                # ES: Etapa 2 del enfoque disjunto en dos etapas.
                code <- c(code, "",
                    "# Stage 2: the stage-1 scores of the dimensions measure the higher-order construct;",
                    "# the other constructs keep their own indicators (Sarstedt et al., 2019)",
                    "d2 <- as.data.frame(fit$Estimates$Construct_scores)",
                    "names(d2) <- paste0(\"S_\", names(d2))",
                    if (length(m2$own)) c(
                        sprintf("own <- as.data.frame(fit$Information$Data[, c(%s)])",
                                paste0("\"", m2$own, "\"", collapse = ", ")),
                        if (self$options$indicatorScale == "ordinal")
                            "for (v in names(own)) own[[v]] <- ordered(own[[v]])  # ordinal: polychoric correlations",
                        "d2 <- cbind(own, d2)"),
                    "model2 <- \"", m2$syntax, "\"",
                    sprintf("fit2 <- cSEM::csem(d2, model2, .PLS_modes = %s, .disattenuate = FALSE,", modes_of(m2$modes)),
                    sprintf("                   .PLS_weight_scheme_inner = \"%s\")", self$options$innerScheme),
                    "cSEM::summarize(fit2)",
                    "# Bootstrap of a higher-order model: in each resample of stage 1, stage 2 is re-estimated from that resample (.user_funs; see R/bootstrap.R in PLSsemLab)")
            }
            code <- c(code, private$.reproOptional(model, paths, q, modes, blocks),
                      private$.reproQuality(model, blocks, paths, q, modes_of))
            self$results$reproducibilityCode$setContent(paste(code, collapse = "\n"))
        },

        # Equivalent R code of the optional sections that ran: bootstrap,
        # IPMA, prediction, multigroup analysis and overall fit.
        # ES: Código R equivalente de las secciones opcionales que corrieron.
        .reproOptional = function(model, paths, q, modes, blocks = list()) {
            code <- character(0)
            if (self$options$mardia)
                code <- c(code, "",
                    "# Mardia's multivariate skewness and kurtosis (covariance with denominator n, as Mardia, 1970)",
                    "X <- as.matrix(d); X <- X[complete.cases(X), ]; n <- nrow(X); p <- ncol(X); Z <- scale(X, scale = FALSE)",
                    "D <- Z %*% solve(crossprod(Z) / n) %*% t(Z); b1 <- sum(D^3) / n^2; b2 <- sum(diag(D)^2) / n",
                    "c(b1 = b1, p_skew = pchisq(n * b1 / 6, p * (p + 1) * (p + 2) / 6, lower.tail = FALSE),",
                    "  b2 = b2, z_kurt = (b2 - p * (p + 2)) / sqrt(8 * p * (p + 2) / n))")
            if (self$options$bootstrap && self$options$resampleMethod == "jackknife") {
                code <- c(code, "",
                    "# Jackknife: one estimation per case left out; t intervals estimate +/- t(n - 1) * jackknife SE",
                    "jk <- cSEM::resamplecSEMResults(fit, .resample_method = \"jackknife\", .handle_inadmissibles = \"drop\")",
                    "pe <- jk$Estimates$Estimates_resample$Estimates1$Path_estimates; n <- nrow(pe$Resampled)",
                    "se <- sqrt((n - 1) / n * colSums(sweep(pe$Resampled, 2, colMeans(pe$Resampled))^2))",
                    sprintf("cbind(estimate = pe$Original, se = se, lower = pe$Original - qt(%s, n - 1) * se, upper = pe$Original + qt(%s, n - 1) * se)",
                            format(1 - (1 - as.numeric(self$options$ciLevel)) / 2), format(1 - (1 - as.numeric(self$options$ciLevel)) / 2)),
                    "# cSEM's summarize(.ci = \"CI_standard_t\") subtracts the bootstrap-style bias mean - estimate; PLSsemLab does not")
            } else if (self$options$bootstrap) {
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
                    "# Intervals of R2, reliability, AVE, HTMT and VIF: the same resampling with .user_funs (see R/bootstrap.R in PLSsemLab)",
                    if (self$options$weightApproach == "maxvar")
                        "# MAXVAR: PLSsemLab turns each construct of each draw back to the sign of its original loadings (R/weights.R); cSEM does not")
            }
            if (self$options$compareRun) {
                drop <- .pl_cvpat_drop(self$options$cvpatDrop, paths)
                if (nrow(drop) && !length(model$hocs)) {
                    keep <- paths[!(paste(paths$from, paths$to) %in% paste(drop$from, drop$to)), , drop = FALSE]
                    alt <- .pl_build_syntax(blocks, keep, self$options$estimator)$syntax
                    code <- c(code, "",
                        sprintf("# Model comparison: the competing model leaves out %s", paste(drop$from, "->", drop$to, collapse = ", ")),
                        sprintf("model_alt <- \"%s\"", gsub("\n", "\\\\n", alt)),
                        sprintf("fit_alt <- cSEM::csem(d, model_alt, .PLS_modes = %s, .disattenuate = %s, .PLS_weight_scheme_inner = \"%s\")",
                                modes, if (self$options$estimator == "plsc") "TRUE" else "FALSE", self$options$innerScheme),
                        "sapply(list(main = fit, competing = fit_alt), function(f) unlist(cSEM::calculateModelSelectionCriteria(f, .by_equation = TRUE, .only_structural = TRUE)[c(\"BIC\", \"GM\")]))",
                        "c(main = cSEM::calculateSRMR(fit), competing = cSEM::calculateSRMR(fit_alt))")
                }
            }
            if (self$options$ipma && is.null(.pl_ipma_reason(model, model$hocs, self$options))) {
                code <- c(code, "",
                    "# IPMA: importance = unstandardized total effects (rows: targets); performance rescaled to 0-100",
                    "ip <- cSEM::doIPMA(fit)",
                    "ip$Construct$Importance",
                    if (self$options$ipmaRange == "manual")
                        sprintf("100 * (ip$Construct$Performance - %s) / (%s - %s)", format(self$options$ipmaMin),
                                format(self$options$ipmaMax), format(self$options$ipmaMin))
                    else "rg <- range(unlist(lapply(d, as.numeric)), na.rm = TRUE); 100 * (ip$Construct$Performance - rg[1]) / diff(rg)")
            }
            if (self$options$ncaRun && is.null(.pl_nca_reason(model, model$hocs, self$options))) {
                np <- .pl_nca_pairs(paths)
                code <- c(code, "",
                    "# NCA: unstandardized construct scores (the IPMA's) rescaled to 0-100 with the range of the scale",
                    "W <- fit$Estimates$Weight_estimates; X <- as.matrix(d[, colnames(W)])",
                    "S <- sapply(rownames(W), function(c) { w <- W[c, ] / apply(X, 2, sd); X %*% (w / sum(w)) })",
                    if (self$options$ipmaRange == "manual")
                        sprintf("S <- as.data.frame(100 * (S - %s) / (%s - %s))", format(self$options$ipmaMin),
                                format(self$options$ipmaMax), format(self$options$ipmaMin))
                    else "rg <- range(unlist(lapply(d, as.numeric)), na.rm = TRUE); S <- as.data.frame(100 * (S - rg[1]) / diff(rg))",
                    "# The NCA package (Dul) gives the same d, accuracy and bottlenecks; its permutations draw other",
                    "# random numbers than PLSsemLab's, so the p values agree within Monte Carlo error",
                    sprintf("set.seed(%d)", as.integer(self$options$bootSeed)))
                for (tg in unique(np$to))
                    code <- c(code, sprintf("NCA::nca_analysis(S, c(%s), \"%s\", ceilings = c(\"ce_fdh\", \"cr_fdh\"), test.rep = %d)",
                                            paste(sprintf("\"%s\"", unname(unlist(model$cmap[np$from[np$to == tg]]))), collapse = ", "),
                                            model$cmap[[tg]], as.integer(self$options$ncaPerm)))
            }
            if (self$options$predictRun && is.null(.pl_predict_reason(model, model$hocs, TRUE))) {
                code <- c(code, "",
                    "# PLSpredict: each repetition uses cSEM's folds with seed + repetition - 1 (PLS columns of cSEM::predict)",
                    sprintf("pred <- lapply(seq_len(%d), function(r) cSEM::predict(fit, .benchmark = \"lm\", .cv_folds = %d, .r = 1, .seed = %d + r - 1))",
                            as.integer(self$options$predictReps), as.integer(self$options$predictFolds),
                            as.integer(self$options$bootSeed)),
                    "Reduce(`+`, lapply(pred, function(p) p$Prediction_metrics[, c(\"Q2_predict\", \"RMSE_target\", \"MAE_target\")])) / length(pred)",
                    if (self$options$predictBenchmark != "none")
                        sprintf("# Extra benchmark: the same folds and cSEM::predict(.benchmark = \"%s\") (RMSE_benchmark, MAE_benchmark columns)",
                                .pl_approach(self$options$predictBenchmark)),
                    "# LM benchmark (Shmueli et al., 2019): each endogenous indicator regressed WITH an intercept on the",
                    "# exogenous indicators, on the same folds; cSEM::predict(.benchmark = \"lm\") omits the intercept",
                    "exo <- colnames(fit$Information$Model$measurement)[colSums(fit$Information$Model$measurement[rowSums(fit$Information$Model$structural) == 0, , drop = FALSE]) > 0]",
                    "endo <- as.character(pred[[1]]$Prediction_metrics$Name)",
                    sprintf("lm_rmse <- sapply(seq_len(%d), function(r) {", as.integer(self$options$predictReps)),
                    sprintf("    folds <- cSEM::resampleData(fit, .resample_method = \"cross-validation\", .cv_folds = %d, .R = 1, .seed = %d + r - 1)[[1]]",
                            as.integer(self$options$predictFolds), as.integer(self$options$bootSeed)),
                    "    res <- do.call(rbind, lapply(folds, function(test) { train <- d[setdiff(rownames(d), rownames(test)), ]",
                    "        b <- qr.coef(qr(cbind(1, as.matrix(train[, exo]))), as.matrix(train[, endo]))",
                    "        as.matrix(test[, endo]) - cbind(1, as.matrix(test[, exo])) %*% b }))",
                    "    sqrt(colMeans(res^2)) })",
                    "rowMeans(lm_rmse)  # RMSE of LM (MAE likewise with mean(abs(res)))",
                    "# CVPAT against IA and LM: paired t test on the per-case losses of the same residuals (see R/predict.R in PLSsemLab)")
            }
            gv <- self$options$groupVar
            if (self$options$mgaRun && !is.null(gv) && nzchar(gv) && is.null(model$hocs)) {
                # The same internal group names as the module (G01..Gk in the
                # order of the levels), so the sign of each difference matches.
                # ES: Los mismos nombres internos de grupo que el módulo.
                lev <- .pl_mga_levels(self$data[[gv]])
                perms <- as.integer(self$options$mgaPerm)
                seed <- as.integer(self$options$bootSeed)
                code <- c(code, "",
                    "# Multigroup analysis: the model in each group; groups named G01..Gk in the order of the levels",
                    sprintf("lev <- c(%s)", paste(q(lev), collapse = ", ")),
                    sprintf("k <- match(as.character(data[[%s]])[as.integer(rownames(d))], lev)", q(gv)),
                    "dg <- d[!is.na(k), ]",
                    "dg$PLgroup <- sprintf(\"G%02d\", k[!is.na(k)])",
                    sprintf("fit_g <- cSEM::csem(dg, model, .id = \"PLgroup\", .PLS_modes = %s, .disattenuate = %s,", modes,
                            if (self$options$estimator == "plsc") "TRUE" else "FALSE"),
                    sprintf("                    .PLS_weight_scheme_inner = \"%s\")", self$options$innerScheme),
                    if (is.null(.pl_micom_reason(self$options$estimator, self$options$indicatorScale)))
                        sprintf("cSEM::testMICOM(fit_g, .R = %d, .seed = %d)", perms, seed),
                    sprintf("mgd <- cSEM::testMGD(fit_g, .approach_mgd = c(\"Henseler\", \"Chin\"), .R_permutation = %d, .R_bootstrap = %d, .seed = %d,",
                            perms, as.integer(self$options$mgaBoot), seed),
                    sprintf("                     .parameters_to_compare = \"%s\")", gsub("\n", "\\\\n", .pl_mga_compare(paths, model$cmap))),
                    "mgd$Chin$P_value$none  # permutation p",
                    "lapply(mgd$Henseler$P_value$none, function(P) 2 * pmin(P, 1 - P))  # PLS-MGA, two-sided p",
                    if (self$options$mgaAdjust != "none")
                        sprintf("# correction: stats::p.adjust(p, method = \"%s\") over every path and pair, each test on its own",
                                self$options$mgaAdjust))
                syn <- .pl_mgcfa_syntax(blocks, model$cmap, model$vmap)
                if (self$options$mgcfa && is.null(.pl_mgcfa_reason(syn, self$options$indicatorScale))) {
                    est <- self$options$mgcfaEstimator
                    code <- c(code, "",
                        "# Measurement invariance of the reflective constructs as common factors (multigroup CFA, lavaan)",
                        sprintf("cfa_model <- \"%s\"", gsub("\n", "\\\\n", syn)),
                        sprintf("mi <- lapply(list(configural = character(0), metric = \"loadings\", scalar = c(\"loadings\", \"intercepts\")), function(eq) lavaan::cfa(cfa_model, data = dg, group = \"PLgroup\", estimator = \"%s\", meanstructure = TRUE, group.equal = eq))", est),
                        sprintf("sapply(mi, lavaan::fitMeasures, c(%s))", if (est == "MLR")
                            "\"chisq.scaled\", \"df.scaled\", \"pvalue.scaled\", \"cfi.robust\", \"rmsea.robust\", \"srmr\""
                            else "\"chisq\", \"df\", \"pvalue\", \"cfi\", \"rmsea\", \"srmr\""),
                        "lavaan::lavTestLRT(mi$configural, mi$metric); lavaan::lavTestLRT(mi$metric, mi$scalar)")
                }
            }
            if (self$options$fitTest && is.null(.pl_fit_reason(model, model$hocs, TRUE))) {
                args <- sprintf(".R = %d, .seed = %d, .alpha = %s", as.integer(self$options$fitReps),
                                as.integer(self$options$bootSeed), format(1 - as.numeric(self$options$fitLevel)))
                code <- c(code, "",
                    "# Overall model fit: bootstrap test of exact fit, estimated and saturated model",
                    sprintf("cSEM::testOMF(fit, %s, .saturated = FALSE)", args),
                    sprintf("cSEM::testOMF(fit, %s, .saturated = TRUE)", args))
            }
            code
        },

        # Equivalent R code of the measurement-model criteria, each called
        # separately as the module does.
        # ES: Código R equivalente de los criterios del modelo de medida.
        .reproQuality = function(model, blocks, paths, q, modes_of) {
            code <- character(0)
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
            code
        }
    )
)
