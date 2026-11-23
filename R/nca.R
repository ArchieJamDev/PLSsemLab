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
# Necessary condition analysis (NCA; Dul, 2016) combined with PLS-SEM
# (Richter et al., 2020, 2023). For each endogenous construct (the outcome),
# every construct with a direct path to it is tested as a necessary
# condition on the unstandardized construct scores rescaled to 0-100 (the
# scores of the IPMA): the ceiling lines CE-FDH (step function through the
# upper-left peers) and CR-FDH (OLS line through those peers), the effect
# size d (ceiling zone / scope), the accuracy of CR-FDH, the bottleneck table
# and the permutation test of Dul, van der Laan and Kuik (2020).
#
# The definitions are those of the NCA package (Dul, version 5.1.0) with its
# defaults (empirical scope, no flips, bottlenecks in percentage of the
# range, cutoff 0): validation/nca_equivalence.R checks every value against
# NCA::nca_analysis(). The module computes them itself because the package
# finds the peers with an R loop, which makes 10 000 permutations take many
# minutes and cannot be interrupted, and because it brings plotting and
# database dependencies the module does not use. The permutations follow the
# package's scheme (one offset per condition from the session's generator,
# set.seed(offset + i) for permutation i), so the same seed gives the same p.
#
# ES: Análisis de condición necesaria combinado con PLS-SEM. Para cada
# constructo endógeno, cada constructo con una ruta directa hacia él se
# prueba como condición necesaria con las puntuaciones no estandarizadas en
# 0-100 (las del IPMA): techos CE-FDH y CR-FDH, tamaño del efecto d,
# precisión de CR-FDH, tabla de cuellos de botella y prueba de permutación.
# Las definiciones son las del paquete NCA 5.1.0 con sus opciones por
# defecto, comprobadas en validation/nca_equivalence.R; el módulo las calcula
# por su cuenta porque el paquete busca los picos con un bucle de R (10 000
# permutaciones tardan muchos minutos y no se pueden interrumpir) y trae
# dependencias de gráficos y bases de datos que el módulo no usa.
# -----------------------------------------------------------------------------

.PL_NCA_STEPS <- seq(0, 100, 10)
.PL_NCA_EPS <- 1e-10

# Why NCA cannot run for this model (NULL when it can). Category B: the
# scores of the IPMA are needed (composites estimated by PLS, no product
# terms, first-order constructs).
# ES: Por qué no puede correr el NCA (NULL si puede). Categoría B.
.pl_nca_reason <- function(model, hocs, options) {
    if (identical(options$estimator, "plsc")) return("nca_plsc")
    if (isTRUE(model$nonlinear)) return("nca_nonlinear")
    if (length(hocs)) return("nca_hoc")
    NULL
}

# Unstandardized construct scores on the metric of the indicators (the
# scores whose means are cSEM::doIPMA()'s performances), rescaled to 0-100
# with the range of the scale. Returns a data.frame, one column per
# construct (internal names).
# ES: Puntuaciones no estandarizadas en la métrica de los indicadores (sus
# medias son los desempeños de doIPMA()), reescaladas a 0-100.
.pl_nca_scores <- function(fit, data, range) {
    W <- fit$Estimates$Weight_estimates
    X <- as.matrix(.pl_numeric(data[, colnames(W), drop = FALSE]))
    sdx <- apply(X, 2, stats::sd)
    out <- vapply(rownames(W), function(cn) {
        w <- W[cn, ] / sdx
        w[!is.finite(w)] <- 0
        w <- w / sum(w)
        drop(X %*% w)
    }, numeric(nrow(X)))
    out <- matrix(out, nrow = nrow(X), dimnames = list(NULL, rownames(W)))
    as.data.frame(100 * (out - range[1]) / (range[2] - range[1]))
}

# The NCA package's equality test (relative tolerance 1e-6).
# ES: La prueba de igualdad del paquete NCA (tolerancia relativa 1e-6).
.pl_nca_equal <- function(a, b) abs(a - b) <= pmin(abs(a), abs(b)) / 1e6

# Peers of the CE-FDH ceiling: the upper-left staircase. x sorted ascending
# (ties by y ascending), a point is a peer when its y exceeds the last
# peer's y; a later point with the same x and a larger y replaces the peers
# at that x, and a point equal to the last peer is kept as a duplicate (it
# weighs in the CR-FDH regression). Same rules as NCA:::p_peers().
# ES: Picos del techo CE-FDH: la escalera superior izquierda, con las mismas
# reglas que NCA:::p_peers(), sin su bucle sobre cada caso.
.pl_nca_peers <- function(x, y, ord = order(x, y)) {
    xs <- x[ord]
    ys <- y[ord]
    n <- length(xs)
    if (n < 2) return(cbind(X = xs, Y = ys))
    # groups of equal x (consecutive, NCA's tolerance)
    # ES: grupos de x iguales (consecutivos, con la tolerancia del paquete)
    newg <- c(TRUE, !.pl_nca_equal(xs[-1], xs[-n]))
    g <- cumsum(newg)
    gmax <- as.numeric(tapply(ys, g, max))
    prev <- c(-Inf, cummax(gmax)[-length(gmax)])
    record <- gmax > prev
    record[1] <- TRUE
    keep <- record[g] & .pl_nca_equal(ys, gmax[g])
    keep[is.na(keep)] <- FALSE
    # a group whose maximum is 0 compares 0 with 0 exactly
    # ES: un grupo cuyo máximo es 0 se compara exactamente
    keep <- keep | (record[g] & ys == gmax[g])
    cbind(X = xs[keep], Y = ys[keep])
}

# Area of the ceiling zone above the CR-FDH line inside the scope (empirical
# scope: c(xmin, xmax, ymin, ymax)). As NCA:::p_ceiling() without flips.
# ES: Área de la zona techo sobre la recta CR-FDH dentro del alcance.
.pl_nca_cr_area <- function(slope, intercept, scope, area) {
    if (is.na(slope) || is.na(intercept) || slope < 0) return(NaN)
    pl <- c(scope[1], slope * scope[1] + intercept)
    pr <- c(scope[2], slope * scope[2] + intercept)
    if (pl[2] > scope[4] && pr[2] > scope[4]) return(NaN)
    if (pl[2] < scope[3] && pr[2] < scope[3]) return(area)
    if (slope > 0) {
        if (pl[2] < scope[3]) pl <- c((scope[3] - intercept) / slope, scope[3])
        if (pr[2] > scope[4]) pr <- c((scope[4] - intercept) / slope, scope[4])
        above <- 0.5 * (pr[1] - pl[1]) * (pr[2] - pl[2]) +
            (scope[2] - scope[1]) * (scope[4] - pr[2]) +
            (pl[1] - scope[1]) * (scope[4] - scope[3]) -
            (pl[1] - scope[1]) * (scope[4] - pr[2])
    } else {
        above <- (scope[2] - scope[1]) * (scope[4] - intercept)
    }
    above
}

# Effect sizes of both ceilings for one condition (and, optionally, the
# lines and the accuracy). ES: Tamaños del efecto de ambos techos.
.pl_nca_effects <- function(x, y, scope, area, ord = order(x, y), full = FALSE) {
    pk <- .pl_nca_peers(x, y, ord)
    np <- nrow(pk)
    uniq <- np > 1 && nrow(unique(pk)) > 1
    ce <- if (uniq) sum(abs((pk[-1, 1] - scope[1]) * diff(pk[, 2]))) else 0
    # a single distinct peer: both ceiling zones are 0 (no line), as in the
    # package. ES: un solo pico distinto: ambas zonas techo valen 0.
    out <- c(ce = ce / area, cr = 0)
    if (uniq) {
        b <- stats::lm.fit(cbind(1, pk[, 1]), pk[, 2])$coefficients
        out[["cr"]] <- .pl_nca_cr_area(b[[2]], b[[1]], scope, area) / area
    } else {
        b <- c(NA_real_, NA_real_)
    }
    if (!full) return(out)
    above <- if (!uniq) 0 else if (is.na(b[2]) || b[2] < 0) NaN else sum(y - (b[[2]] * x + b[[1]]) > 1e-07, na.rm = TRUE)
    list(effects = out, peers = pk, intercept = unname(b[1]), slope = unname(b[2]),
         accuracy = 100 * (length(x) - above) / length(x))
}

# Bottleneck: the level of the condition (percentage of its range) needed
# for each level of the outcome (percentage of its range); -Inf = "NN" (not
# necessary), NA = "NA" (level above the observed range). As
# NCA:::p_bottleneck_fdh() and p_bottleneck() with cutoff 0.
# ES: Cuello de botella: nivel de la condición (en % de su rango) necesario
# para cada nivel del resultado; -Inf = "NN", NA = "NA".
.pl_nca_bottleneck <- function(e, scope) {
    mpy <- scope[3] + .PL_NCA_STEPS / 100 * (scope[4] - scope[3])
    edge <- function(m) {
        m[!is.na(m) & m < scope[1] + .PL_NCA_EPS] <- -Inf
        m[!is.na(m) & is.finite(m) & m > scope[2]] <- NA
        m
    }
    ce <- vapply(mpy, function(v) {
        i <- which(e$peers[, 2] > v - .PL_NCA_EPS)[1]
        if (is.na(i)) NA_real_ else e$peers[i, 1]
    }, 0)
    cr <- if (is.na(e$slope)) rep(Inf, length(mpy)) else (mpy - e$intercept) / e$slope
    pct <- function(m) ifelse(is.finite(m), 100 * (m - scope[1]) / (scope[2] - scope[1]), m)
    list(ce = pct(edge(ce)), cr = pct(edge(cr)))
}

# The permutation test of one condition (Dul, van der Laan & Kuik, 2020):
# the outcome is permuted, the effect size recomputed, p = (#{d* >= d} + 1) /
# (R + 1), never below 1 / R; ties within 1e-10 count as d* >= d. Permutation i uses set.seed(offset + i), with
# the offset drawn from the session's generator, as the NCA package does;
# the session's state after drawing the offset is restored. tick(): jamovi's
# checkpoint, every `every` seconds.
# ES: Prueba de permutación de una condición: se permuta el resultado, se
# recalcula d y p = (#{d* >= d} + 1) / (R + 1), nunca menor que 1 / R.
.pl_nca_permutation <- function(x, y, scope, area, observed, reps, tick = NULL, every = 0.25,
                               offset = sample.int(2^24, 1), return_sims = FALSE) {
    h <- length(y)
    force(offset)
    state <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", state, envir = globalenv()), add = TRUE)
    perms <- lapply(seq_len(reps), function(i) { set.seed(offset + i); sample.int(h, h) })
    if (reps <= 720) {
        while (length(unique(perms)) < reps) {
            set.seed(offset + length(perms))
            perms[[length(perms) + 1]] <- sample.int(h, h)
        }
        perms <- unique(perms)
    }
    sims <- matrix(NA_real_, length(perms), 2, dimnames = list(NULL, c("ce", "cr")))
    last <- proc.time()[["elapsed"]]
    for (i in seq_along(perms)) {
        yp <- y[perms[[i]]]
        sims[i, ] <- .pl_nca_effects(x, yp, scope, area, order(x, yp))
        if (!is.null(tick) && proc.time()[["elapsed"]] - last >= every) {
            last <- proc.time()[["elapsed"]]
            tick()
        }
    }
    # Permutations often rebuild the same peers, so many effects equal the
    # observed one exactly; they count as "at least as large" with a
    # tolerance of 1e-10 (the package compares the raw floating-point
    # values, so rounding noise can drop some of those ties).
    # ES: Las permutaciones suelen rehacer los mismos picos, así que muchos
    # efectos son exactamente iguales al observado; cuentan como "al menos
    # igual de grandes" con una tolerancia de 1e-10.
    p <- vapply(c("ce", "cr"), function(k) {
        s <- sims[, k]
        s <- s[!is.na(s)]
        if (is.na(observed[[k]]) || !length(s)) return(NA_real_)
        max((sum(s >= observed[[k]] - 1e-10) + 1) / (length(s) + 1), 1 / reps)
    }, 0)
    if (return_sims) list(p = p, sims = sims) else p
}

# The whole analysis for one outcome and its direct predictors. scores:
# data.frame of 0-100 scores; returns one entry per condition with effects,
# p values, accuracy, lines and bottlenecks. The conditions are tested in
# order, each drawing its offset from the session's generator (seeded once
# by the caller), as NCA::nca_analysis() does for several conditions.
# ES: El análisis completo de un resultado y sus predictores directos.
.pl_nca_outcome <- function(scores, outcome, conditions, reps, tick = NULL) {
    y <- scores[[outcome]]
    out <- list()
    for (cn in conditions) {
        x <- scores[[cn]]
        ok <- stats::complete.cases(x, y)
        xx <- x[ok]
        yy <- y[ok]
        scope <- c(min(xx), max(xx), min(yy), max(yy))
        area <- (scope[2] - scope[1]) * (scope[4] - scope[3])
        e <- .pl_nca_effects(xx, yy, scope, area, full = TRUE)
        p <- if (reps > 0) .pl_nca_permutation(xx, yy, scope, area, e$effects, reps, tick) else c(ce = NA, cr = NA)
        out[[cn]] <- list(effects = e$effects, p = p, accuracy = e$accuracy, intercept = e$intercept,
                          slope = e$slope, peers = e$peers, scope = scope,
                          bottleneck = .pl_nca_bottleneck(e, scope), n = sum(ok))
    }
    out
}

# Dul's (2016) benchmarks for the effect size d.
# ES: Referencias de Dul (2016) para el tamaño del efecto d.
.pl_nca_size <- function(d) {
    if (is.na(d)) return(NA_character_)
    if (d < 0.1) "nca_small" else if (d < 0.3) "nca_medium" else if (d < 0.5) "nca_large" else "nca_very_large"
}

# The verdict of one ceiling: necessary when d >= 0.1 and p < .05 (Dul,
# 2016; Richter et al., 2020). ES: El veredicto de un techo.
.pl_nca_verdict <- function(d, p, alpha = 0.05) {
    if (is.na(d)) return(NA_character_)
    if (d < 0.1) return("nca_negligible")
    if (is.na(p) || p >= alpha) return("nca_not_sig")
    "nca_necessary"
}

# A bottleneck value as the table shows it: a number, or "NN" / "NA".
# ES: Un valor de cuello de botella como lo muestra la tabla.
.pl_nca_bn_cell <- function(v, nn, na) {
    if (is.na(v)) return(na)
    if (is.infinite(v)) return(nn)
    v
}

# Scatter plot of one condition with both ceiling lines, from image$state.
# Colours follow jamovi's theme: the points and CR-FDH (dashed) in the ink
# colour, CE-FDH in the accent colour.
# ES: Diagrama de dispersión de una condición con ambos techos, desde
# image$state; los colores siguen el tema de jamovi.
.pl_plot_nca <- function(state, ggtheme, theme) {
    s <- state$scope
    pk <- state$peers
    pts <- data.frame(x = state$x, y = state$y)
    ink <- theme$color[1]
    pal <- c(theme$color[2], ink)
    # CE-FDH: a step function from the lower-left corner through the peers
    # ES: CE-FDH: una escalera desde la esquina inferior izquierda
    sx <- c(as.vector(rbind(pk[, 1], pk[, 1])), s[2])
    sy <- c(as.vector(rbind(c(s[3], pk[-nrow(pk), 2]), pk[, 2])), pk[nrow(pk), 2])
    ce <- data.frame(x = sx, y = sy, line = state$labels[["ce"]])
    lines <- ce
    if (is.finite(state$slope) && state$slope > 0) {
        xs <- seq(s[1], s[2], length.out = 200)
        yc <- state$intercept + state$slope * xs
        keep <- yc >= s[3] & yc <= s[4]
        if (any(keep))
            lines <- rbind(lines, data.frame(x = xs[keep], y = yc[keep], line = state$labels[["cr"]]))
    }
    lines$line <- factor(lines$line, levels = c(state$labels[["ce"]], state$labels[["cr"]]))
    ggplot2::ggplot() +
        ggplot2::annotate("rect", xmin = s[1], xmax = s[2], ymin = s[3], ymax = s[4],
                          fill = NA, colour = ink, alpha = 0.4, linetype = "dotted") +
        ggplot2::geom_point(data = pts, ggplot2::aes(x = x, y = y), colour = ink, alpha = 0.45, size = 1.6) +
        ggplot2::geom_path(data = lines, ggplot2::aes(x = x, y = y, colour = line, linetype = line), linewidth = 1) +
        ggplot2::scale_colour_manual(values = stats::setNames(pal, levels(lines$line)), drop = FALSE, name = NULL) +
        ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "dashed"), levels(lines$line)),
                                       drop = FALSE, name = NULL) +
        ggplot2::labs(x = state$x_lab, y = state$y_lab, title = state$title, caption = state$caption) +
        ggtheme +
        ggplot2::theme(legend.position = "bottom")
}

# Outcome-condition pairs of the NCA: every direct path between two
# constructs (interaction terms left out). ES: Pares resultado-condición.
.pl_nca_pairs <- function(paths) {
    p <- paths[!grepl(.PL_X, paths$from, fixed = TRUE), c("from", "to"), drop = FALSE]
    rownames(p) <- NULL
    p
}

# Every outcome of the model, under the default generator with the seed set
# once (the session's generator is restored afterwards). cmap: user label ->
# internal name. ES: Todos los resultados del modelo, con el generador por
# defecto y la semilla fijada una vez.
.pl_nca_run <- function(scores, pairs, cmap, reps, seed, tick = NULL) {
    restore_rng <- .pl_rng_guard()
    on.exit(restore_rng(), add = TRUE)
    set.seed(seed)
    out <- list()
    for (tg in unique(pairs$to))
        out[[tg]] <- .pl_nca_outcome(scores, cmap[[tg]], unname(unlist(cmap[pairs$from[pairs$to == tg]])), reps, tick)
    out
}
