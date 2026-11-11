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
# Bootstrap in blocks (phase 3 of docs/DESIGN.md §8; deficiency D4).
# ES: Bootstrap por bloques (fase 3; deficiencia D4).
#
# cSEM::resamplecSEMResults() runs in blocks of 200 resamples; block k uses
# seed + k - 1. A block with seed s draws the same resamples as the start of
# a longer run with seed s (checked), so up to 200 resamples the result is
# identical to a direct cSEM call. The blocks are merged into one cSEM
# object and cSEM's own summarize() / infer() compute standard errors, t, p
# and percentile intervals, so every number is cSEM's. Quality criteria (R²,
# reliability, AVE, HTMT, VIF) ride along as .user_funs, each returning a
# vector of fixed length (NA when a draw cannot compute it), as cSEM needs.
#
# ES: cSEM::resamplecSEMResults() corre en bloques de 200 réplicas; el bloque
# k usa semilla + k - 1. Un bloque con semilla s produce las mismas réplicas
# que el inicio de una corrida más larga con semilla s (comprobado), así que
# hasta 200 réplicas el resultado es idéntico a una llamada directa a cSEM.
# Los bloques se combinan en un objeto de cSEM y summarize() / infer() de
# cSEM calculan errores estándar, t, p e intervalos percentiles: cada número
# es de cSEM. Los criterios de calidad viajan como .user_funs, cada uno con
# un vector de largo fijo (NA cuando una réplica no puede calcularlo).
# -----------------------------------------------------------------------------

.PL_BLOCK <- 200L

# What the bootstrap computes besides cSEM's own quantities, with the labels
# needed to read the result back.
# ES: Lo que el bootstrap calcula además de las cantidades de cSEM, con las
# etiquetas para leer el resultado.
.pl_boot_spec <- function(blocks, model, fit, options) {
    est <- fit$Estimates
    refl <- Filter(function(b) b$type == "reflective", blocks)
    multi <- Filter(function(b) length(b$vars) > 1, refl)
    rint <- unname(model$cmap[vapply(multi, `[[`, "", "label")])
    rall <- unname(model$cmap[vapply(refl, `[[`, "", "label")])
    pairs <- if (length(rall) >= 2) utils::combn(rall, 2) else matrix(character(0), 2, 0)
    form <- Filter(function(b) b$type == "formative" && length(b$vars) > 1, blocks)
    form_ids <- lapply(form, function(b) unname(model$vmap[b$vars]))
    names(form_ids) <- unname(model$cmap[vapply(form, `[[`, "", "label")])
    r2names <- names(est$R2)
    vifs <- unlist(est$VIF)
    plsc <- options$estimator == "plsc"

    safe <- function(f, n) {
        force(f)
        force(n)
        function(.object) {
            v <- tryCatch(suppressWarnings(f(.object)), error = function(e) rep(NA_real_, n))
            v <- unname(as.numeric(v))
            if (length(v) != n) rep(NA_real_, n) else v
        }
    }
    funs <- list(R2 = safe(function(o) o$Estimates$R2[r2names], length(r2names)),
                 R2adj = safe(function(o) o$Estimates$R2adj[r2names], length(r2names)))
    if (length(rint)) {
        funs$rhoC <- safe(function(o) cSEM::calculateRhoC(o, .only_common_factors = FALSE)[rint], length(rint))
        funs$AVE <- safe(function(o) cSEM::calculateAVE(o, .only_common_factors = FALSE)[rint], length(rint))
        if (plsc)
            funs$rhoA <- safe(function(o) o$Estimates$Reliabilities[rint], length(rint))
    }
    if (ncol(pairs))
        funs$HTMT <- safe(function(o) {
            m <- cSEM::calculateHTMT(o, .only_common_factors = FALSE, .absolute = FALSE)$htmts
            apply(pairs, 2, function(p) {
                ia <- match(p[1], rownames(m))
                ib <- match(p[2], rownames(m))
                if (ia > ib) m[ia, ib] else m[ib, ia]
            })
        }, ncol(pairs))
    if (length(form_ids)) {
        nB <- sum(lengths(form_ids))
        funs$VIFB <- safe(function(o) {
            S <- o$Estimates$Indicator_VCV
            unlist(lapply(form_ids, function(ids) diag(solve(S[ids, ids, drop = FALSE]))))
        }, nB)
    }
    if (length(vifs))
        funs$VIFS <- safe(function(o) unlist(o$Estimates$VIF)[names(vifs)], length(vifs))

    list(funs = funs, rint = rint, pairs = pairs, form_ids = form_ids, r2names = r2names,
         vifnames = names(vifs), plsc = plsc)
}

# Runs the blocks, calling progress(done, total) after each one (the caller
# flushes the results and checks for a restart there).
# ES: Corre los bloques y llama a progress(hechas, total) después de cada
# uno (quien llama envía los resultados y revisa si hay que reiniciar).
.pl_run_bootstrap <- function(fit, funs, reps, seed, progress = NULL) {
    n_blocks <- ceiling(reps / .PL_BLOCK)
    objs <- list()
    errors <- character(0)
    warnings <- character(0)
    t0 <- Sys.time()
    for (k in seq_len(n_blocks)) {
        R <- min(.PL_BLOCK, reps - (k - 1L) * .PL_BLOCK)
        r <- .pl_capture(cSEM::resamplecSEMResults(fit, .R = R, .seed = seed + k - 1L, .user_funs = funs,
                                                   .handle_inadmissibles = "drop"))
        warnings <- c(warnings, r$warnings)
        if (!is.null(r$error)) errors <- c(errors, r$error)
        else objs[[length(objs) + 1L]] <- r$value
        if (!is.null(progress))
            progress(min(k * .PL_BLOCK, reps), reps)
    }
    seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (!length(objs))
        return(list(value = NULL, errors = unique(errors), warnings = unique(warnings), seconds = seconds))
    list(value = .pl_merge_blocks(objs), errors = unique(errors), warnings = unique(warnings),
         seconds = seconds)
}

# One cSEM resample object from several: the resampled draws of every
# quantity stacked, the counts summed. The indicator and construct
# correlation draws are dropped (large and not reported).
# ES: Un objeto de remuestreo de cSEM a partir de varios: las réplicas de cada
# cantidad apiladas y los conteos sumados. Se descartan las réplicas de
# correlaciones de indicadores y constructos (grandes y no informadas).
.pl_merge_blocks <- function(objs) {
    m <- objs[[1]]
    q <- setdiff(names(m$Estimates$Estimates_resample$Estimates1),
                 c("Indicator_correlation", "Exo_construct_correlation"))
    m$Estimates$Estimates_resample$Estimates1 <- m$Estimates$Estimates_resample$Estimates1[q]
    for (n in q)
        m$Estimates$Estimates_resample$Estimates1[[n]]$Resampled <-
            do.call(rbind, lapply(objs, function(o) o$Estimates$Estimates_resample$Estimates1[[n]]$Resampled))
    ir <- m$Information$Information_resample
    ir$Number_of_admissibles <- sum(vapply(objs, function(o) o$Information$Information_resample$Number_of_admissibles, 0))
    ir$Number_of_runs <- sum(vapply(objs, function(o) o$Information$Information_resample$Number_of_runs, 0))
    m$Information$Information_resample <- ir
    m
}

# Everything the tables need, computed by cSEM from the merged draws: one
# small list (cached), keyed by internal names.
# ES: Todo lo que necesitan las tablas, calculado por cSEM a partir de las
# réplicas combinadas: una lista pequeña (en caché), con nombres internos.
.pl_boot_summary <- function(merged, level, spec) {
    alpha <- 1 - level
    s <- cSEM::summarize(merged, .alpha = alpha, .ci = "CI_percentile")$Estimates
    inf <- cSEM::infer(merged, .quantity = c("sd", "CI_percentile"), .alpha = alpha)
    bounds <- function(df) {
        lo <- grep("L$", names(df), value = TRUE)[1]
        up <- grep("U$", names(df), value = TRUE)[1]
        data.frame(name = as.character(df$Name), estimate = df$Estimate, se = df$Std_err, t = df$t_stat,
                   p = df$p_value, lower = df[[lo]], upper = df[[up]], stringsAsFactors = FALSE)
    }
    ci <- function(q, names) {
        m <- inf[[q]]$CI_percentile
        if (is.null(m)) return(NULL)
        data.frame(name = names, lower = unname(m[1, ]), upper = unname(m[2, ]), stringsAsFactors = FALSE)
    }
    out <- list(
        paths = bounds(s$Path_estimates),
        loadings = bounds(s$Loading_estimates),
        weights = bounds(s$Weight_estimates),
        indirect = bounds(s$Effect_estimates$Indirect_effect),
        total = bounds(s$Effect_estimates$Total_effect),
        R2 = ci("R2", spec$r2names),
        rhoC = if (length(spec$rint)) ci("rhoC", spec$rint),
        AVE = if (length(spec$rint)) ci("AVE", spec$rint),
        VIFB = if (length(spec$form_ids)) ci("VIFB", unlist(lapply(names(spec$form_ids), function(cc)
            paste(cc, spec$form_ids[[cc]], sep = "~")))),
        VIFS = if (length(spec$vifnames)) ci("VIFS", spec$vifnames),
        admissible = merged$Information$Information_resample$Number_of_admissibles,
        runs = merged$Information$Information_resample$Number_of_runs)
    # HTMT: the one-sided 95% upper bound of Henseler et al. (2015), the 95th
    # percentile, read from cSEM's 90% two-sided interval.
    # ES: HTMT: el límite superior unilateral al 95% de Henseler et al.
    # (2015), el percentil 95, leído del intervalo bilateral al 90% de cSEM.
    if (ncol(spec$pairs)) {
        h <- cSEM::infer(merged, .quantity = "CI_percentile", .alpha = 0.10)$HTMT$CI_percentile
        out$HTMT <- data.frame(name = apply(spec$pairs, 2, paste, collapse = "~"), upper = unname(h[2, ]),
                               stringsAsFactors = FALSE)
    }
    out
}

# Cached run: the key covers the data, the model and every bootstrap option,
# so a rerun for a report option (guides, key findings) costs nothing.
# ES: Corrida en caché: la clave cubre los datos, el modelo y cada opción del
# bootstrap, así que volver a ejecutar por una opción del informe no cuesta.
.pl_cached_bootstrap <- function(store, key_parts, compute, max_entries = 4, max_bytes = 2 * 1024^2) {
    key <- tryCatch(rlang::hash(key_parts), error = function(e) NULL)
    state <- tryCatch(store$state, error = function(e) NULL)
    if (!is.null(key) && is.list(state) && !is.null(state[[key]]))
        return(c(state[[key]], cached = TRUE))
    result <- compute()
    if (!is.null(key) && !is.null(result$summary)) {
        if (!is.list(state)) state <- list()
        state[[key]] <- result
        if (length(state) > max_entries)
            state <- state[seq.int(length(state) - max_entries + 1, length(state))]
        while (length(state) > 0 && length(serialize(state, NULL)) > max_bytes)
            state <- state[-1]
        tryCatch(store$setState(state), error = function(e) NULL)
    }
    c(result, cached = FALSE)
}

.pl_band_f2 <- function(f2) {
    ifelse(is.na(f2), NA_character_,
    ifelse(f2 < 0.02, "f2_none", ifelse(f2 < 0.15, "f2_small", ifelse(f2 < 0.35, "f2_medium", "f2_large"))))
}

# Pairs of constructs joined by at least one path (directly or through
# others), in the order of the paths: the rows of the effects table.
# ES: Pares de constructos unidos por al menos una ruta (directa o a través
# de otros): las filas de la tabla de efectos.
.pl_effect_pairs <- function(paths) {
    simple <- paths[!grepl(":", paths$from, fixed = TRUE), , drop = FALSE]
    if (!nrow(simple)) return(data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE))
    nodes <- unique(c(simple$from, simple$to))
    reach <- function(start) {
        seen <- character(0)
        frontier <- start
        while (length(frontier)) {
            nxt <- unique(simple$to[simple$from %in% frontier])
            nxt <- setdiff(nxt, seen)
            seen <- c(seen, nxt)
            frontier <- nxt
        }
        seen
    }
    rows <- list()
    for (n in nodes) for (t in reach(n))
        rows[[length(rows) + 1]] <- data.frame(from = n, to = t, stringsAsFactors = FALSE)
    if (!length(rows)) return(data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE))
    do.call(rbind, rows)
}

# -----------------------------------------------------------------------------
# Path diagram (phase 3): layout by layers, exogenous constructs on the left
# (a construct's layer = 1 + the deepest layer of its predictors), drawn with
# ggplot2 on jamovi's theme. The layout is computed in .run() and stored in
# image$state, so an export renders the same picture from a fresh instance.
# ES: Diagrama de rutas (fase 3): disposición por capas, exógenos a la
# izquierda (capa = 1 + la capa más profunda de sus predictores), dibujado
# con ggplot2 sobre el tema de jamovi. La disposición se calcula en .run()
# y se guarda en image$state.
# -----------------------------------------------------------------------------
.pl_diagram_layout <- function(blocks, paths, path_tab, r2_tab, sig = NULL) {
    labels <- vapply(blocks, `[[`, "", "label")
    simple <- paths[!grepl(":", paths$from, fixed = TRUE), , drop = FALSE]
    depth <- stats::setNames(rep(NA_integer_, length(labels)), labels)
    layer_of <- function(n, seen = character(0)) {
        if (!is.na(depth[[n]])) return(depth[[n]])
        preds <- setdiff(simple$from[simple$to == n], seen)
        d <- if (!length(preds)) 0L else 1L + max(vapply(preds, layer_of, 0L, seen = c(seen, n)))
        depth[[n]] <<- d
        d
    }
    for (n in labels) layer_of(n)
    nodes <- data.frame(label = labels, type = vapply(blocks, `[[`, "", "type"), layer = unname(depth),
                        stringsAsFactors = FALSE)
    nodes$y <- 0
    for (l in unique(nodes$layer)) {
        idx <- which(nodes$layer == l)
        nodes$y[idx] <- rev(seq_along(idx) - (length(idx) + 1) / 2) * 1.2
    }
    nodes$x <- nodes$layer * 3
    nodes$r2 <- r2_tab$r2[match(nodes$label, r2_tab$construct)]
    edges <- merge(path_tab[, c("from", "to", "estimate")], nodes[, c("label", "x", "y")], by.x = "from", by.y = "label")
    names(edges)[names(edges) %in% c("x", "y")] <- c("x1", "y1")
    edges <- merge(edges, nodes[, c("label", "x", "y")], by.x = "to", by.y = "label")
    names(edges)[names(edges) %in% c("x", "y")] <- c("x2", "y2")
    edges$sig <- if (is.null(sig)) NA else sig[paste(edges$from, edges$to, sep = "->")]
    list(nodes = nodes, edges = edges)
}

