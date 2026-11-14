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
# Importance-performance map analysis (phase 6; Ringle & Sarstedt, 2016):
# for each endogenous construct (the target), the importance of every
# construct that affects it is its unstandardised total effect on the
# target, and its performance is the mean of its unstandardised scores
# rescaled to 0-100 with the range of the scale. cSEM::doIPMA() gives both
# on the metric of the indicators; the module rescales.
# ES: Análisis del mapa importancia-desempeño (fase 6): para cada constructo
# endógeno (el objetivo), la importancia de cada constructo que lo afecta es
# su efecto total no estandarizado sobre el objetivo, y su desempeño la media
# de sus puntuaciones no estandarizadas reescalada a 0-100 con el rango de
# la escala. cSEM::doIPMA() da ambos en la métrica de los indicadores; el
# módulo reescala.
# -----------------------------------------------------------------------------

# Why IPMA cannot run for this model (NULL when it can). Category B.
# ES: Por qué el IPMA no puede correr con este modelo (NULL si puede).
.pl_ipma_reason <- function(model, hocs, options) {
    if (identical(options$estimator, "plsc")) return("ipma_plsc")
    if (isTRUE(model$nonlinear)) return("ipma_nonlinear")
    if (length(hocs)) return("ipma_hoc")
    NULL
}

# Importance and performance per target and predictor. range: c(min, max)
# of the scale. Returns a data.frame (target, construct, importance,
# perf_raw, performance, quadrant) and whether some weight is negative.
# ES: Importancia y desempeño por objetivo y predictor.
.pl_ipma <- function(fit, pairs, cmap, range, total_ci = NULL) {
    ip <- suppressWarnings(cSEM::doIPMA(fit))
    imp <- ip$Construct$Importance
    perf <- ip$Construct$Performance
    out <- pairs
    out$importance <- mapply(function(t, f) imp[cmap[[t]], cmap[[f]]], pairs$to, pairs$from)
    out$perf_raw <- unname(perf[cmap[pairs$from]])
    out$performance <- 100 * (out$perf_raw - range[1]) / (range[2] - range[1])
    out$quadrant <- NA_character_
    for (t in unique(out$to)) {
        k <- out$to == t
        # one construct alone has no "above or below average"
        # ES: un solo constructo no está "por encima o por debajo del promedio"
        if (sum(k) == 1) { out$quadrant[k] <- "q_single"; next }
        hi_imp <- out$importance[k] >= mean(out$importance[k])
        hi_perf <- out$performance[k] >= mean(out$performance[k])
        out$quadrant[k] <- ifelse(hi_imp & !hi_perf, "q_priority",
                                  ifelse(hi_imp & hi_perf, "q_keep",
                                         ifelse(!hi_imp & hi_perf, "q_overkill", "q_low")))
    }
    # significance of the importance: the bootstrap interval of the total
    # effect (NA without the bootstrap)
    # ES: significación de la importancia: el intervalo bootstrap del efecto
    # total (NA sin bootstrap)
    out$sig <- NA
    if (!is.null(total_ci))
        out$sig <- mapply(function(t, f) {
            r <- total_ci[total_ci$name == paste(cmap[[t]], "~", cmap[[f]]), , drop = FALSE]
            if (nrow(r) == 1 && is.finite(r$lower)) r$lower > 0 || r$upper < 0 else NA
        }, pairs$to, pairs$from)
    w <- fit$Estimates$Weight_estimates
    list(table = out, negative = any(w < 0))
}

.pl_plot_ipma <- function(state, ggtheme, theme) {
    d <- state$points
    ink <- theme$color[1]
    accent <- theme$color[2]
    ggplot2::ggplot(d, ggplot2::aes(x = importance, y = performance, label = construct)) +
        ggplot2::geom_vline(xintercept = mean(d$importance), linetype = "dashed", colour = ink, alpha = 0.6) +
        ggplot2::geom_hline(yintercept = mean(d$performance), linetype = "dashed", colour = ink, alpha = 0.6) +
        ggplot2::geom_point(size = 3, colour = accent) +
        ggplot2::geom_text(vjust = -1, colour = ink, size = 3.6) +
        ggplot2::scale_y_continuous(limits = c(min(0, d$performance), max(100, d$performance))) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
        ggplot2::labs(x = state$x_lab, y = state$y_lab, title = state$title, caption = state$caption) +
        ggtheme
}
