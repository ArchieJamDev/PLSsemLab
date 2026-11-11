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
# SEM path diagram drawn with semPlot (as SEMLj): constructs as ellipses,
# indicators as rectangles; reflective constructs point to their indicators
# (loadings), formative indicators point to their construct (weights). The
# PLS estimates are handed to semPlot as a semPlotModel built here, because
# semPlot reads lavaan-type fits and cSEM's is not one. The data are built in
# .run() and stored in image$state; the render function only draws.
# ES: Diagrama de rutas SEM con semPlot (como SEMLj): constructos como
# elipses, indicadores como rectángulos; los reflectivos apuntan a sus
# indicadores (cargas) y los indicadores formativos a su constructo (pesos).
# Las estimaciones de PLS se entregan a semPlot como un semPlotModel armado
# aquí, porque semPlot lee ajustes tipo lavaan y el de cSEM no lo es.
# -----------------------------------------------------------------------------

# Input: blocks; loadings (construct, type, indicator, loading, weight);
#   paths (from, to, estimate); r2 (construct, r2); sm - bootstrap summary
#   or NULL; model - internal names; indicators - show them; caption.
# Output: list(vars, pars, caption, n_layers, n_rows) for image$state.
# ES: Salida: lista con variables, parámetros y leyenda para image$state.
.pl_sem_state <- function(blocks, loadings, paths, r2, sm, model, indicators = TRUE, caption = "") {
    lab <- vapply(blocks, `[[`, "", "label")
    r2v <- stats::setNames(r2$r2, r2$construct)
    # Construct node names carry their R² (semPlot draws the name as label).
    # ES: El nombre del nodo lleva su R² (semPlot dibuja el nombre).
    node <- stats::setNames(vapply(lab, function(x)
        if (!is.null(r2v[x]) && is.finite(r2v[x])) paste0(x, "\nR\u00b2 = ", formatC(r2v[[x]], format = "f", digits = 2)) else x, ""), lab)
    sig_of <- function(name, tab) {
        if (is.null(sm) || is.null(sm[[tab]])) return(NA)
        r <- sm[[tab]][sm[[tab]]$name == name, , drop = FALSE]
        if (nrow(r) != 1 || !is.finite(r$lower)) return(NA)
        r$lower > 0 || r$upper < 0
    }
    pars <- list()
    add <- function(lhs, rhs, est, kind, sig = NA)
        pars[[length(pars) + 1]] <<- data.frame(lhs = lhs, rhs = rhs, est = est, kind = kind, sig = sig,
                                                stringsAsFactors = FALSE)
    man <- character(0)
    if (indicators) {
        for (i in seq_len(nrow(loadings))) {
            l <- loadings[i, ]
            v <- l$indicator
            # an indicator named like a construct gets a trailing space
            # ES: un indicador con el nombre de un constructo recibe un espacio
            vn <- if (v %in% node) paste0(v, " ") else v
            man <- c(man, vn)
            cc <- model$cmap[[l$construct]]
            vv <- model$vmap[[v]]
            if (l$type == "formative")
                add(vn, node[[l$construct]], l$weight, "weight", sig_of(paste(cc, "<~", vv), "weights"))
            else
                add(node[[l$construct]], vn, l$loading, "loading")
        }
    }
    simple <- paths[!grepl(":", paths$from, fixed = TRUE), , drop = FALSE]
    for (k in seq_len(nrow(simple)))
        add(node[[simple$from[k]]], node[[simple$to[k]]], simple$estimate[k], "path",
            sig_of(paste(model$cmap[[simple$to[k]]], "~", model$cmap[[simple$from[k]]]), "paths"))
    pars <- if (length(pars)) do.call(rbind, pars) else
        data.frame(lhs = character(0), rhs = character(0), est = numeric(0), kind = character(0), sig = logical(0))
    endo <- unique(simple$to)
    vars <- data.frame(name = c(unique(man), unname(node)),
                       manifest = c(rep(TRUE, length(unique(man))), rep(FALSE, length(node))),
                       exogenous = c(rep(NA, length(unique(man))), !(lab %in% endo)),
                       stringsAsFactors = FALSE)
    lay <- .pl_diagram_layout(blocks, paths, paths, r2, NULL)
    list(vars = vars, pars = pars, caption = caption,
         n_layers = length(unique(lay$nodes$layer)), n_rows = max(table(lay$nodes$layer)),
         n_ind = length(unique(man)), boot = !is.null(sm))
}

# A semPlotModel from the state; slots are filled by name so that a semPlot
# release with more slots still gets a valid object.
# ES: Un semPlotModel a partir del estado; las ranuras se llenan por nombre.
.pl_sem_model <- function(state) {
    p <- state$pars
    pars <- data.frame(label = "", lhs = p$lhs, edge = rep("->", nrow(p)), rhs = p$rhs, est = p$est,
                       std = p$est, group = "", fixed = FALSE, par = seq_len(nrow(p)),
                       stringsAsFactors = FALSE)
    cl <- methods::getClass("semPlotModel", where = asNamespace("semPlot"))
    slots <- cl@slots
    args <- list(Class = cl)
    for (s in names(slots)) {
        args[[s]] <- switch(s,
            Pars = pars, Vars = state$vars, Computed = TRUE,
            switch(slots[[s]], data.frame = data.frame(), list = list(), logical = TRUE,
                   character = character(0), numeric = numeric(0), NULL))
    }
    args <- args[!vapply(args, is.null, TRUE)]
    do.call(methods::new, args)
}

# Draws the diagram (base graphics, through qgraph). opts: the analysis
# options; theme: jamovi's theme (color[1] ink, fill[1] background, fill[2]
# soft fill).
# ES: Dibuja el diagrama (gráficos base, por qgraph).
.pl_plot_sem <- function(state, opts, theme) {
    p <- state$pars
    if (!nrow(p)) return(FALSE)
    ink <- theme$color[1]
    soft <- theme$fill[2]
    back <- theme$fill[1]
    pick <- function(x, default) if (is.null(x) || identical(x, "theme")) default else x
    edge_col <- pick(opts$edgeColor, ink)
    m <- .pl_sem_model(state)
    digits <- if (is.null(opts$edgeDigits)) 2 else as.integer(opts$edgeDigits)
    labels <- formatC(p$est, format = "f", digits = digits)
    labels[!is.na(p$sig) & p$sig] <- paste0(labels[!is.na(p$sig) & p$sig], "*")
    if (isFALSE(opts$edgeLabels)) labels <- rep("", length(labels))
    # Non-significant paths and weights are drawn dashed (semPlot draws
    # "fixed" parameters with fixedStyle).
    # ES: Rutas y pesos no significativos van discontinuos.
    dashed <- !is.na(p$sig) & !p$sig & !isFALSE(opts$nonsigDashed)
    m@Pars$fixed <- dashed
    layout <- switch(if (is.null(opts$diagramLayout)) "treeLR" else opts$diagramLayout,
                     treeLR = list("tree2", 2), treeTB = list("tree2", 1),
                     circle = list("circle2", 1), spring = list("spring", 1))
    by_est <- !identical(opts$edgeWidth, "uniform")
    semPlot::semPaths(m, what = if (by_est) "est" else "paths", whatLabels = "est", edgeLabels = labels,
                      layout = layout[[1]], rotation = layout[[2]],
                      sizeLat = if (is.null(opts$latentSize)) 11 else opts$latentSize,
                      sizeLat2 = 0.62 * (if (is.null(opts$latentSize)) 11 else opts$latentSize),
                      sizeMan = if (is.null(opts$manifestSize)) 6 else opts$manifestSize,
                      sizeMan2 = 0.5 * (if (is.null(opts$manifestSize)) 6 else opts$manifestSize),
                      nCharNodes = 0, label.cex = if (is.null(opts$nodeLabelSize)) 1 else opts$nodeLabelSize,
                      label.scale = FALSE,
                      edge.label.cex = if (is.null(opts$edgeLabelSize)) 0.8 else opts$edgeLabelSize,
                      residuals = FALSE, intercepts = FALSE, thresholds = FALSE, exoCov = FALSE,
                      color = list(lat = pick(opts$latentFill, soft), man = pick(opts$manifestFill, back)),
                      border.color = ink, label.color = ink, edge.color = edge_col,
                      edge.label.color = ink, edge.label.bg = back,
                      fixedStyle = c(edge_col, 2), freeStyle = c(edge_col, 1),
                      fade = FALSE, esize = if (by_est) 6 else 1.6, asize = 2.6,
                      curvePivot = TRUE, mar = c(5, 4, if (nzchar(opts$diagramTitle %or% "")) 6 else 3, 4),
                      bg = back, DoNotPlot = FALSE)
    if (nzchar(opts$diagramTitle %or% ""))
        graphics::title(main = opts$diagramTitle, col.main = ink, font.main = 1, cex.main = 1.1, line = 0.5)
    if (!isFALSE(opts$diagramCaption) && nzchar(state$caption))
        graphics::mtext(state$caption, side = 1, line = 1, adj = 0, cex = 0.75, col = ink)
    TRUE
}

`%or%` <- function(a, b) if (is.null(a)) b else a
