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
.pl_sem_state <- function(blocks, loadings, paths, r2, sm, model, indicators = TRUE, caption = "", hocq = NULL) {
    # Higher-order constructs are drawn as constructs whose lower-order
    # constructs stand before them, linked by loadings (reflective) or
    # weights (formative).
    # ES: Los constructos de orden superior se dibujan con sus constructos de
    # orden inferior delante, unidos por cargas o pesos.
    hocs <- if (is.null(model$hocs)) list() else model$hocs
    blocks_all <- c(blocks, lapply(hocs, function(h) list(label = h$label, vars = character(0), type = "hoc")))
    lab <- vapply(blocks_all, `[[`, "", "label")
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
            cc <- (if (is.null(model$mcmap)) model$cmap else model$mcmap)[[l$construct]]
            vv <- model$vmap[[v]]
            if (l$type == "formative")
                add(vn, node[[l$construct]], l$weight, "weight", sig_of(paste(cc, "<~", vv), "weights"))
            else
                add(node[[l$construct]], vn, l$loading, "loading")
        }
    }
    simple <- paths[!grepl(.PL_X, paths$from, fixed = TRUE), , drop = FALSE]
    for (k in seq_len(nrow(simple)))
        add(node[[simple$from[k]]], node[[simple$to[k]]], simple$estimate[k], "path",
            sig_of(paste(model$cmap[[simple$to[k]]], "~", model$cmap[[simple$from[k]]]), "paths"))
    # Each interaction term is a node of its own ("A × B") with its path to
    # the outcome, drawn in the column of its components, below them.
    # ES: Cada interacción es un nodo propio ("A × B") con su ruta al
    # resultado, en la columna de sus componentes, debajo de ellos.
    inter <- paths[grepl(.PL_X, paths$from, fixed = TRUE), , drop = FALSE]
    for (term in unique(inter$from)) node[[term]] <- term
    for (k in seq_len(nrow(inter)))
        add(node[[inter$from[k]]], node[[inter$to[k]]], inter$estimate[k], "path",
            sig_of(paste(model$cmap[[inter$to[k]]], "~", model$cmap[[inter$from[k]]]), "paths"))
    for (h in hocs) {
        hh <- model$m2$hmap[[h$label]]
        for (cpt in h$components) {
            r <- hocq$measurement[hocq$measurement$hoc == h$label & hocq$measurement$component == cpt, , drop = FALSE]
            ind <- model$m2$ind(cpt)
            if (h$type == "formative")
                add(node[[cpt]], node[[h$label]], r$weight, "hoc", sig_of(paste(hh, "<~", ind), "hoc_weights"))
            else
                add(node[[h$label]], node[[cpt]], r$loading, "hoc", sig_of(paste(hh, "=~", ind), "hoc_loadings"))
        }
    }
    pars <- if (length(pars)) do.call(rbind, pars) else
        data.frame(lhs = character(0), rhs = character(0), est = numeric(0), kind = character(0), sig = logical(0))
    endo <- unique(simple$to)
    man_u <- unique(man)
    # formative indicators are exogenous (they point to their construct)
    # ES: los indicadores formativos son exógenos (apuntan a su constructo)
    man_exo <- man_u %in% pars$lhs[pars$kind == "weight"]
    vars <- data.frame(name = c(man_u, unname(node)),
                       manifest = c(rep(TRUE, length(man_u)), rep(FALSE, length(node))),
                       exogenous = c(man_exo, !(names(node) %in% endo)),
                       stringsAsFactors = FALSE)
    # layout paths: the user's, plus each lower-order construct before its
    # higher-order construct
    # ES: rutas de la disposición: las del usuario y cada constructo de orden
    # inferior antes del suyo de orden superior
    lay_paths <- paths[, c("from", "to")]
    for (h in hocs) lay_paths <- rbind(lay_paths, data.frame(from = h$components, to = h$label, stringsAsFactors = FALSE))
    lay <- .pl_diagram_layout(blocks_all, lay_paths, paths, r2, NULL)
    for (term in unique(inter$from)) {
        parts <- strsplit(term, .PL_X, fixed = TRUE)[[1]]
        lay$nodes <- rbind(lay$nodes[, c("label", "type", "layer")], data.frame(
            label = term, type = "interaction", layer = max(lay$nodes$layer[lay$nodes$label %in% parts]),
            stringsAsFactors = FALSE))
    }
    simple_lay <- lay_paths[!grepl(.PL_X, lay_paths$from, fixed = TRUE), , drop = FALSE]
    xy <- .pl_sem_coords(lay$nodes, simple_lay, node, pars, vars)
    # top to bottom: indicator boxes side by side need a wider step
    # ES: de arriba abajo, las cajas lado a lado necesitan más separación
    tb <- .pl_sem_coords(lay$nodes, simple_lay, node, pars, vars, gap = 1.3, step = 0.6, side = 1.7,
                         row_off = 2.5, row_extra = 3.1, min_body = 2.3, layer = 1.4)
    list(vars = vars, pars = pars, caption = caption, x = xy$x, y = xy$y, x_tb = -tb$y, y_tb = -tb$x,
         n_layers = length(unique(lay$nodes$layer)), n_rows = max(table(lay$nodes$layer)),
         n_ind = length(unique(man)), boot = !is.null(sm))
}

# Coordinates for the left-to-right tree (SmartPLS-like): constructs in
# columns by depth, ordered by the mean position of their predictors;
# formative indicators and those of exogenous reflective constructs in a
# column to the left, those of reflective constructs that predict nothing in
# a column to the right, and those of reflective constructs in the middle in
# a row above, so no indicator sits on a structural arrow.
# ES: Coordenadas del árbol de izquierda a derecha (como SmartPLS):
# constructos en columnas por profundidad; indicadores formativos y de
# reflectivos exógenos a la izquierda, de reflectivos terminales a la
# derecha y de reflectivos intermedios en una fila arriba.
.pl_sem_coords <- function(nodes, simple, node, pars, vars, gap = 0.62, step = 1.4, side = 2.8,
                           row_off = 1.5, row_extra = 1.8, min_body = 1.5, layer = 1.6) {
    x <- stats::setNames(rep(0, nrow(vars)), vars$name)
    y <- x
    sides <- lapply(nodes$label, function(cl) {
        nm <- node[[cl]]
        into <- pars$lhs[pars$rhs == nm & pars$kind == "weight"]
        outof <- pars$rhs[pars$lhs == nm & pars$kind == "loading"]
        exo <- !(cl %in% simple$to)
        term <- !(cl %in% simple$from)
        list(left = c(into, if (exo) outof), right = if (!exo && term) outof else character(0),
             top = if (!exo && !term) outof else character(0))
    })
    names(sides) <- nodes$label
    body <- vapply(sides, function(sd) max(min_body, gap * length(sd$left), gap * length(sd$right)), 0)
    order_y <- stats::setNames(rep(0, nrow(nodes)), nodes$label)
    # Column positions: each column is as wide as what it holds (indicator
    # columns at the sides, rows above or below), plus `layer` between
    # columns for the path labels.
    # ES: Cada columna es tan ancha como lo que contiene, más `layer` entre
    # columnas para las etiquetas de las rutas.
    layers <- sort(unique(nodes$layer))
    ext <- function(l, which) {
        cls <- nodes$label[nodes$layer == l]
        has <- any(vapply(sides[cls], function(sd) length(sd[[which]]) > 0, TRUE))
        row <- max(0, vapply(sides[cls], function(sd) (length(sd$top) - 1) * step / 2 + 0.6, 0))
        max(1.1, row, if (has) side + 0.6 else 0)
    }
    col_x <- stats::setNames(numeric(length(layers)), layers)
    for (k in seq_along(layers)[-1])
        col_x[k] <- col_x[k - 1] + ext(layers[k - 1], "right") + layer + ext(layers[k], "left")
    for (l in layers) {
        idx <- which(nodes$layer == l)
        if (l > 0) {
            key <- vapply(nodes$label[idx], function(cl) {
                pr <- simple$from[simple$to == cl]
                if (length(pr)) mean(order_y[pr]) else 0
            }, 0)
            idx <- idx[order(-key)]
        } else {
            # first column: constructs that feed the same constructs stay
            # together (e.g. the dimensions of a higher-order construct), so
            # no other path runs between them
            # ES: primera columna: los constructos que alimentan a los mismos
            # quedan juntos (p. ej. las dimensiones de un constructo de orden
            # superior), para que ninguna otra ruta pase entre ellos
            succ <- vapply(nodes$label[idx], function(cl) paste(sort(simple$to[simple$from == cl]), collapse = "|"), "")
            idx <- idx[order(match(succ, unique(succ)))]
        }
        idx <- c(idx[nodes$type[idx] != "interaction"], idx[nodes$type[idx] == "interaction"])
        # the first construct of a column gets its row of indicators above,
        # the others below, so the rows stay outside the column
        # ES: el primer constructo de la columna lleva su fila arriba, los
        # demás abajo
        row_above <- seq_along(idx) == 1
        extra <- vapply(nodes$label[idx], function(cl) if (length(sides[[cl]]$top)) row_extra else 0, 0)
        total <- sum(body[nodes$label[idx]] + extra) + 0.8 * (length(idx) - 1)
        top <- total / 2
        # A path that skips this column (e.g. Qual -> Loy over Trust) runs
        # near the middle: the column moves down and its rows go below, so
        # the path passes above instead of through the indicators.
        # ES: Una ruta que salta esta columna pasa por el centro: la columna
        # baja y sus filas van abajo, para que la ruta pase por encima.
        lay_of <- stats::setNames(nodes$layer, nodes$label)
        if (any(lay_of[simple$from] < l & lay_of[simple$to] > l)) {
            row_above <- rep(FALSE, length(idx))
            top <- total / 2 - 1.4
        }
        for (j in seq_along(idx)) {
            cl <- nodes$label[idx[j]]
            sd <- sides[[cl]]
            nm <- node[[cl]]
            above <- if (row_above[j]) extra[[j]] else 0
            cy <- top - above - body[[cl]] / 2
            cx <- col_x[[as.character(l)]]
            x[[nm]] <- cx
            y[[nm]] <- cy
            order_y[[cl]] <- cy
            col <- function(v, xx) if (length(v)) {
                x[v] <<- xx
                y[v] <<- cy + rev(seq_along(v) - (length(v) + 1) / 2) * gap
            }
            col(sd$left, cx - side)
            col(sd$right, cx + side)
            if (length(sd$top)) {
                x[sd$top] <- cx + (seq_along(sd$top) - (length(sd$top) + 1) / 2) * step
                y[sd$top] <- cy + if (row_above[j]) row_off else -row_off
            }
            top <- top - body[[cl]] - extra[[j]] - 0.8
        }
    }
    list(x = unname(x), y = unname(y))
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
.pl_plot_sem <- function(state, opts, theme, width = 900, height = 600) {
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
    # "fixed" parameters with fixedStyle). The styles are line types only:
    # semPlot reads any string with a digit as a line type, so a hex colour
    # there would break the plot; the colour goes in edge.color.
    # ES: Rutas y pesos no significativos van discontinuos.
    dashed <- !is.na(p$sig) & !p$sig & !isFALSE(opts$nonsigDashed)
    m@Pars$fixed <- dashed
    xy <- cbind(state$x, state$y)
    layout <- switch(if (is.null(opts$diagramLayout)) "treeLR" else opts$diagramLayout,
                     treeLR = list(xy, 1), treeTB = list(cbind(state$x_tb, state$y_tb), 1),
                     circle = list("circle2", 1), spring = list("spring", 1))
    if (is.null(state$x)) layout[[1]] <- "tree2"
    by_est <- !identical(opts$edgeWidth, "uniform")
    # qgraph sizes nodes relative to the device diagonal (its Cent2Edge with
    # normalize = TRUE gives 0.00749 * sqrt(width^2 + height^2) px per size
    # unit); the defaults give boxes of about 70 x 24 px and ellipses of
    # 115 x 56 px at any size; the size options scale them.
    # ES: qgraph mide los nodos respecto a la diagonal del dispositivo; por
    # defecto, cajas de unos 70 x 24 px y elipses de 115 x 56 px.
    unit <- 0.00749 * sqrt(width^2 + height^2)
    kl <- (if (is.null(opts$latentSize)) 11 else opts$latentSize) / 11
    km <- (if (is.null(opts$manifestSize)) 6 else opts$manifestSize) / 6
    # When the layout is tight for the image, nodes shrink to fit their
    # spacing (pixels per layout unit, after qgraph's margins).
    # ES: Si la disposición queda apretada, los nodos se achican.
    tb <- identical(opts$diagramLayout, "treeTB")
    gx <- if (tb) state$x_tb else state$x
    gy <- if (tb) state$y_tb else state$y
    geo <- if (tb) list(side = 1.7, step = 1.3, gap = 0.6) else list(side = 2.8, step = 1.4, gap = 0.62)
    ppx <- 0.84 * width / (diff(range(gx)) + 1.6)
    ppy <- 0.80 * height / (diff(range(gy)) + 1)
    man_w <- max(58, min(70 * km, 0.85 * (if (tb) geo$step * ppx else min(geo$step * ppx, 0.75 * geo$side * ppx))))
    man_h <- max(16, min(24 * km, 0.75 * (if (tb) 0.6 else geo$gap) * ppy))
    lat_w <- max(70, min(115 * kl, 1.0 * (if (tb) 2.3 else geo$side) * ppx))
    lat_h <- max(34, min(56 * kl, 0.9 * ppy))
    # Coefficients near the indicator on measurement arrows (where arrows
    # converge on the construct the labels would pile up), midway on paths.
    # ES: Coeficientes cerca del indicador en las flechas de medida, al
    # medio en las rutas.
    lab_pos <- ifelse(p$kind == "weight", 0.36, ifelse(p$kind == "loading", 0.64, 0.5))
    # paths into the same construct alternate their label position so the
    # labels of converging paths do not stack
    # ES: las rutas hacia un mismo constructo alternan la posición de su
    # etiqueta para que no se apilen
    for (to in unique(p$rhs[p$kind == "path"])) {
        k <- which(p$kind == "path" & p$rhs == to)
        if (length(k) > 1) lab_pos[k] <- rep(c(0.4, 0.6), length.out = length(k))
    }
    semPlot::semPaths(m, what = if (by_est) "est" else "paths", whatLabels = "est", edgeLabels = labels,
                      layout = layout[[1]], rotation = layout[[2]], curve = 0,
                      sizeLat = lat_w / unit, sizeLat2 = lat_h / unit,
                      sizeMan = man_w / unit, sizeMan2 = man_h / unit,
                      nCharNodes = 0, label.cex = if (is.null(opts$nodeLabelSize)) 1 else opts$nodeLabelSize,
                      label.scale = FALSE,
                      edge.label.cex = (if (is.null(opts$edgeLabelSize)) 0.8 else opts$edgeLabelSize) *
                          ifelse(p$kind == "path", 1, 0.9),
                      residuals = FALSE, intercepts = FALSE, thresholds = FALSE, exoCov = FALSE,
                      color = list(lat = pick(opts$latentFill, soft), man = pick(opts$manifestFill, back)),
                      border.color = ink, label.color = ink, edge.color = edge_col,
                      edge.label.color = ink, edge.label.bg = back,
                      fixedStyle = 2, freeStyle = 1,
                      fade = FALSE, esize = if (by_est) 6 else 1.6, asize = 2.6,
                      edge.label.position = lab_pos,
                      curvePivot = TRUE, mar = c(if (!isFALSE(opts$diagramCaption)) 7 else 3, 3, if (nzchar(opts$diagramTitle %or% "")) 6 else 3, 3),
                      bg = back, DoNotPlot = FALSE)
    if (nzchar(opts$diagramTitle %or% ""))
        graphics::title(main = opts$diagramTitle, col.main = ink, font.main = 1, cex.main = 1.1, line = 0.5)
    if (!isFALSE(opts$diagramCaption) && nzchar(state$caption))
        graphics::mtext(paste(strwrap(state$caption, width = max(40, round(width / 7))), collapse = "\n"),
                        side = 1, line = 1, adj = 0, cex = 0.75, col = ink, padj = 1)
    TRUE
}

`%or%` <- function(a, b) if (is.null(a)) b else a
