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
# Mediation (phase 6): every specific indirect effect (a chain X -> M1 -> ...
# -> Y of the structural model) as the product of its path coefficients, with
# the percentile bootstrap interval of that product (Preacher & Hayes, 2008;
# Nitzl et al., 2016), and the type of mediation of Zhao et al. (2010) from
# the indirect effect and the direct path X -> Y.
# ES: Mediación (fase 6): cada efecto indirecto específico (una cadena
# X -> M1 -> ... -> Y del modelo estructural) como producto de sus
# coeficientes de ruta, con el intervalo bootstrap percentil de ese producto,
# y el tipo de mediación de Zhao et al. (2010) a partir del efecto indirecto
# y de la ruta directa X -> Y.
# -----------------------------------------------------------------------------

.PL_ARROW <- " → "

# Every chain of two or more structural paths (interaction terms left out).
# Returns a data.frame (from, to, via, label) with via the mediators joined
# by "|".
# ES: Cada cadena de dos o más rutas estructurales (sin términos de
# interacción). via son los mediadores unidos por "|".
.pl_mediation_chains <- function(paths) {
    simple <- paths[!grepl(.PL_X, paths$from, fixed = TRUE), , drop = FALSE]
    empty <- data.frame(from = character(0), to = character(0), via = character(0), label = character(0),
                        stringsAsFactors = FALSE)
    if (nrow(simple) < 2) return(empty)
    rows <- list()
    walk <- function(route) {
        last <- route[length(route)]
        for (nx in simple$to[simple$from == last]) {
            if (nx %in% route) next
            r <- c(route, nx)
            if (length(r) >= 3)
                rows[[length(rows) + 1]] <<- data.frame(from = r[1], to = nx, via = paste(r[-c(1, length(r))], collapse = "|"),
                                                        label = paste(r, collapse = .PL_ARROW), stringsAsFactors = FALSE)
            walk(r)
        }
    }
    for (n in unique(simple$from)) walk(n)
    if (length(rows)) do.call(rbind, rows) else empty
}

# Estimates (and bootstrap intervals when draws are given) of the chains.
# B is the path matrix (rows = outcomes) with internal names; draws has one
# column per path named "to ~ from" in internal names. Returns the chains
# with estimate, se, lower, upper, the direct effect X -> Y (NA when not in
# the model), its interval, and the Zhao et al. (2010) type key.
# ES: Estimaciones (e intervalos bootstrap si hay réplicas) de las cadenas.
.pl_mediation <- function(chains, B, cmap, draws = NULL, level = 0.95, direct_ci = NULL) {
    if (!nrow(chains)) return(chains)
    a <- (1 - level) / 2
    out <- chains
    out$estimate <- out$se <- out$lower <- out$upper <- NA_real_
    out$direct <- out$dlower <- out$dupper <- NA_real_
    out$type <- NA_character_
    for (k in seq_len(nrow(chains))) {
        nodes <- c(chains$from[k], if (nzchar(chains$via[k])) strsplit(chains$via[k], "|", fixed = TRUE)[[1]], chains$to[k])
        ids <- unname(cmap[nodes])
        steps <- paste(ids[-1], "~", ids[-length(ids)])
        out$estimate[k] <- prod(mapply(function(to, from) B[to, from], ids[-1], ids[-length(ids)]))
        x <- ids[1]; y <- ids[length(ids)]
        if (B[y, x] != 0) out$direct[k] <- B[y, x]
        if (!is.null(draws) && all(steps %in% colnames(draws))) {
            d <- apply(draws[, steps, drop = FALSE], 1, prod)
            d <- d[is.finite(d)]
            out$se[k] <- stats::sd(d)
            q <- stats::quantile(d, c(a, 1 - a), names = FALSE)
            out$lower[k] <- q[1]; out$upper[k] <- q[2]
            dn <- paste(y, "~", x)
            if (!is.null(direct_ci) && dn %in% direct_ci$name) {
                out$dlower[k] <- direct_ci$lower[direct_ci$name == dn]
                out$dupper[k] <- direct_ci$upper[direct_ci$name == dn]
            }
            out$type[k] <- .pl_mediation_type(out$estimate[k], out$lower[k], out$upper[k],
                                              out$direct[k], out$dlower[k], out$dupper[k])
        }
    }
    out
}

# Zhao, Lynch and Chen (2010): the type of mediation from the specific
# indirect effect and the direct effect (significant when the interval
# excludes 0). Keys: complementary, competitive, indirect_only, direct_only,
# no_effect; indirect_nodirect / none_nodirect when the model has no direct
# path X -> Y (full mediation cannot be told apart from an omitted path).
# ES: Zhao, Lynch y Chen (2010): el tipo de mediación a partir del efecto
# indirecto específico y del directo.
.pl_mediation_type <- function(ind, lo, up, dir, dlo, dup) {
    ind_sig <- is.finite(lo) && (lo > 0 || up < 0)
    if (is.na(dir)) return(if (ind_sig) "indirect_nodirect" else "none_nodirect")
    dir_sig <- is.finite(dlo) && (dlo > 0 || dup < 0)
    if (ind_sig && dir_sig) return(if (sign(ind) == sign(dir)) "complementary" else "competitive")
    if (ind_sig) return("indirect_only")
    if (dir_sig) return("direct_only")
    "no_effect"
}
