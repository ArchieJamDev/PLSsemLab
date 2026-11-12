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
# Measurement-model plots (phase 3): loadings and HTMT. The data are built in
# .run() and stored in image$state (an export renders from a fresh
# instance); colours come only from jamovi's theme (color[1] ink, color[2]
# accent, fill[1] background), as jmv's own plots.
# ES: Gráficos del modelo de medida (fase 3): cargas y HTMT. Los datos se
# arman en .run() y se guardan en image$state; los colores salen solo del
# tema de jamovi, como en los gráficos de jmv.
# -----------------------------------------------------------------------------

# Loadings of indicators of reflective constructs with two or more
# indicators, with their bootstrap interval when there is one.
# ES: Cargas de indicadores de reflectivos con dos o más indicadores, con su
# intervalo bootstrap cuando lo hay.
.pl_loadings_state <- function(loadings, sm, model, caption) {
    n <- table(loadings$construct)
    d <- loadings[loadings$type == "reflective" & n[loadings$construct] > 1, , drop = FALSE]
    if (!nrow(d)) return(NULL)
    d$lower <- NA_real_
    d$upper <- NA_real_
    if (!is.null(sm)) {
        for (k in seq_len(nrow(d))) {
            r <- sm$loadings[sm$loadings$name == paste(model$cmap[[d$construct[k]]], "=~", model$vmap[[d$indicator[k]]]), ,
                             drop = FALSE]
            if (nrow(r)) {
                d$lower[k] <- r$lower
                d$upper[k] <- r$upper
            }
        }
    }
    list(data = data.frame(construct = d$construct, item = .pl_short(d$indicator), loading = d$loading,
                           lower = d$lower, upper = d$upper, stringsAsFactors = FALSE),
         caption = caption)
}

.pl_plot_loadings <- function(state, ggtheme, theme) {
    d <- state$data
    d$construct <- factor(d$construct, levels = unique(d$construct))
    d$item <- factor(d$item, levels = rev(unique(d$item)))
    d$low <- d$loading < 0.708
    p <- ggplot2::ggplot(d, ggplot2::aes(x = loading, y = item)) +
        ggplot2::geom_vline(xintercept = 0.708, linetype = "dashed", colour = theme$color[1], alpha = 0.6)
    if (any(is.finite(d$lower)))
        p <- p + ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.25,
                                         colour = theme$color[1], alpha = 0.7, na.rm = TRUE)
    p <- p +
        ggplot2::geom_point(ggplot2::aes(shape = low), size = 2.6, colour = theme$color[1], fill = theme$color[2]) +
        ggplot2::scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 24), guide = "none") +
        ggplot2::facet_grid(construct ~ ., scales = "free_y", space = "free_y") +
        ggplot2::scale_x_continuous(limits = c(min(0, d$lower, d$loading, na.rm = TRUE), 1)) +
        ggplot2::labs(x = NULL, y = NULL, caption = paste(strwrap(state$caption, 95), collapse = "\n")) +
        ggtheme +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9),
                       strip.text.y = ggplot2::element_text(angle = 0))
    p
}

# HTMT of every pair of reflective constructs, in a lower-triangle grid.
# ES: HTMT de cada par de reflectivos, en una cuadrícula triangular.
.pl_htmt_state <- function(quality, sm, model, caption) {
    h <- quality$htmt
    if (is.null(h) || !nrow(h)) return(NULL)
    h$upper <- NA_real_
    if (!is.null(sm$HTMT))
        for (k in seq_len(nrow(h))) {
            r <- sm$HTMT[sm$HTMT$name == paste(model$cmap[[h$c1[k]]], model$cmap[[h$c2[k]]], sep = "~"), , drop = FALSE]
            if (nrow(r)) h$upper[k] <- r$upper
        }
    list(data = data.frame(c1 = h$c1, c2 = h$c2, htmt = h$htmt, upper = h$upper, stringsAsFactors = FALSE),
         levels = unique(c(h$c1, h$c2)), caption = caption)
}

.pl_plot_htmt <- function(state, ggtheme, theme) {
    d <- state$data
    lv <- state$levels
    d$x <- factor(d$c1, levels = lv)
    d$y <- factor(d$c2, levels = rev(lv))
    d$value <- ifelse(is.finite(d$upper), d$upper, d$htmt)
    d$label <- ifelse(is.na(d$htmt), "\u2014",
                      ifelse(is.finite(d$upper), sprintf("%.2f\n(\u2264 %.2f)", d$htmt, d$upper), sprintf("%.2f", d$htmt)))
    d$band <- ifelse(is.na(d$value), "na", ifelse(d$value < 0.85, "ok", ifelse(d$value < 0.90, "similar", "doubt")))
    alpha <- c(ok = 0.12, similar = 0.45, doubt = 0.85, na = 0)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_tile(ggplot2::aes(alpha = band), fill = theme$color[2], colour = theme$color[1], linewidth = 0.3) +
        ggplot2::geom_text(ggplot2::aes(label = label), size = 3.3, colour = theme$color[1], lineheight = 0.9) +
        ggplot2::scale_alpha_manual(values = alpha, guide = "none") +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_discrete(drop = FALSE) +
        ggplot2::coord_equal() +
        ggplot2::labs(x = NULL, y = NULL, caption = paste(strwrap(state$caption, 95), collapse = "\n")) +
        ggtheme +
        ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                       axis.line = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 9))
    p
}
