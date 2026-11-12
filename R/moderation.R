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
# Moderation (phase 4): for each two-way interaction X × M -> Y, the simple
# effect of X on Y at chosen values of M, the floodlight analysis over the
# observed range of M and the Johnson-Neyman points, where the interval of
# the simple effect starts or stops including 0. The first construct of the
# term is the predictor and the second the moderator (the order in which the
# term was built in the panel).
#
# cSEM standardizes the construct scores and enters the product of the
# standardized scores, so the simple effect at M = m (in standard deviations)
# is b_X + b_XM * m; its interval comes from the same bootstrap draws as
# every other interval of the report. Up to cSEM's own grid, the values
# equal cSEM::doNonlinearEffectsAnalysis() (test-interaction.R).
# ES: Moderación (fase 4): para cada interacción X × M -> Y, el efecto simple
# de X sobre Y en valores elegidos de M, el análisis floodlight sobre el
# rango observado de M y los puntos de Johnson-Neyman, donde el intervalo
# del efecto simple empieza o deja de incluir el 0. El primer constructo del
# término es el predictor y el segundo el moderador. El efecto simple en
# M = m (en desviaciones estándar) es b_X + b_XM * m, con intervalos de las
# mismas réplicas bootstrap del resto del informe.
# -----------------------------------------------------------------------------

.PL_LEVELS <- c(-1, 0, 1)

# Input: fit (cSEMResults); paths (from, to); model (cmap); sm - bootstrap
#   summary or NULL (path_draws used); level - confidence level.
# Output: a list, one element per interaction term.
# ES: Salida: una lista, un elemento por término de interacción.
.pl_moderation <- function(fit, paths, model, sm, level) {
    inter <- paths[grepl(.PL_X, paths$from, fixed = TRUE), , drop = FALSE]
    est <- fit$Estimates
    alpha <- 1 - level
    lapply(seq_len(nrow(inter)), function(k) {
        parts <- strsplit(inter$from[k], .PL_X, fixed = TRUE)[[1]]
        x <- parts[1]
        m <- parts[2]
        y <- inter$to[k]
        cy <- model$cmap[[y]]
        name_x <- paste(cy, "~", model$cmap[[x]])
        name_m <- paste(cy, "~", model$cmap[[m]])
        name_xm <- paste(cy, "~", model$cmap[[inter$from[k]]])
        bx <- est$Path_estimates[cy, model$cmap[[x]]]
        bm <- est$Path_estimates[cy, model$cmap[[m]]]
        bxm <- est$Path_estimates[cy, model$cmap[[inter$from[k]]]]
        scores <- est$Construct_scores[, model$cmap[[m]]]
        draws <- sm$path_draws
        has_draws <- !is.null(draws) && all(c(name_x, name_xm) %in% colnames(draws))
        effect_at <- function(v) {
            out <- data.frame(value = v, slope = bx + bxm * v, se = NA_real_, lower = NA_real_, upper = NA_real_)
            if (has_draws) {
                d <- outer(draws[, name_x], rep(1, length(v))) + outer(draws[, name_xm], v)
                out$se <- apply(d, 2, stats::sd)
                q <- apply(d, 2, stats::quantile, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
                out$lower <- q[1, ]
                out$upper <- q[2, ]
            }
            out
        }
        grid <- seq(min(scores), max(scores), length.out = 200)
        flood <- effect_at(grid)
        jn <- numeric(0)
        if (has_draws) {
            # where a bound crosses 0, by linear interpolation on the grid
            # ES: donde un límite cruza el 0, por interpolación lineal
            for (b in c("lower", "upper")) {
                v <- flood[[b]]
                cross <- which(diff(sign(v)) != 0)
                jn <- c(jn, vapply(cross, function(i) grid[i] - v[i] * (grid[i + 1] - grid[i]) / (v[i + 1] - v[i]), 0))
            }
            jn <- sort(jn)
        }
        list(term = inter$from[k], x = x, m = m, y = y, bx = bx, bm = bm, bxm = bxm,
             slopes = effect_at(.PL_LEVELS), flood = flood, jn = jn, range = range(scores),
             boot = has_draws)
    })
}

# Text describing where the simple effect is significant along the observed
# range of the moderator. Returns one of the keys of the "moderation" text
# section with its sprintf arguments.
# ES: Texto que describe dónde es significativo el efecto simple a lo largo
# del rango observado del moderador.
.pl_jn_region <- function(mod) {
    f <- mod$flood
    sig <- (f$lower > 0 | f$upper < 0)
    if (all(sig)) return(list(key = "jn_all"))
    if (!any(sig)) return(list(key = "jn_none"))
    list(key = "jn_some", points = mod$jn)
}

# Plot data for one interaction: simple slopes (predicted Y over X at M =
# -1, 0, +1 SD; standardized, so the intercept is 0) and floodlight.
# ES: Datos del gráfico de una interacción: pendientes simples y floodlight.
.pl_moderation_state <- function(mod, labels, caption) {
    xs <- seq(-2, 2, length.out = 41)
    lines <- do.call(rbind, lapply(.PL_LEVELS, function(v)
        data.frame(x = xs, y = mod$bx * xs + mod$bm * v + mod$bxm * xs * v, level = v)))
    list(lines = lines, flood = mod$flood, jn = mod$jn, boot = mod$boot, x = mod$x, m = mod$m, y = mod$y,
         labels = labels, caption = caption)
}

.pl_plot_moderation <- function(state, ggtheme, theme) {
    ink <- theme$color[1]
    accent <- theme$color[2]
    lab <- state$labels
    ln <- state$lines
    ln$level <- factor(ln$level, levels = .PL_LEVELS, labels = lab$levels)
    p1 <- ggplot2::ggplot(ln, ggplot2::aes(x = x, y = y, linetype = level)) +
        ggplot2::geom_line(colour = ink, linewidth = 0.8) +
        ggplot2::scale_linetype_manual(values = c("dotted", "solid", "dashed"), name = state$m) +
        ggplot2::labs(x = sprintf(lab$x_axis, state$x), y = sprintf(lab$y_axis, state$y), title = lab$slopes) +
        ggtheme + ggplot2::theme(legend.position = "bottom")
    f <- state$flood
    p2 <- ggplot2::ggplot(f, ggplot2::aes(x = value, y = slope))
    if (state$boot)
        p2 <- p2 + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = accent, alpha = 0.25)
    p2 <- p2 + ggplot2::geom_hline(yintercept = 0, colour = ink, linetype = "dashed", alpha = 0.7) +
        ggplot2::geom_line(colour = ink, linewidth = 0.8)
    if (length(state$jn))
        p2 <- p2 + ggplot2::geom_vline(xintercept = state$jn, colour = accent, linewidth = 0.7)
    p2 <- p2 + ggplot2::labs(x = sprintf(lab$m_axis, state$m), y = sprintf(lab$effect_axis, state$x, state$y),
                             title = lab$floodlight) +
        ggtheme
    cap <- paste(strwrap(state$caption, 110), collapse = "\n")
    patchwork::wrap_plots(p1, p2, nrow = 1) +
        patchwork::plot_annotation(caption = cap,
                                   theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9)))
}
