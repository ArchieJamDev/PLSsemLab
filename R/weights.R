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
# Weighting approaches (1.2.0): the option values and cSEM's names, the
# arguments every cSEM::csem() call of the module shares, which combinations
# cannot be estimated (Category A) and the sign correction MAXVAR needs in
# the resamples.
# ES: Enfoques de ponderación (1.2.0): los valores de la opción y los nombres
# de cSEM, los argumentos que comparten todas las llamadas a cSEM::csem() del
# módulo, qué combinaciones no pueden estimarse (Categoría A) y la
# corrección de signo que necesita MAXVAR en los remuestreos.
# -----------------------------------------------------------------------------

.PL_APPROACHES <- c(pls = "PLS-PM", gsca = "GSCA", sumcorr = "SUMCORR", maxvar = "MAXVAR", pca = "PCA",
                    unit = "unit")

.pl_approach <- function(option) {
    a <- .PL_APPROACHES[[if (is.null(option)) "pls" else option]]
    if (is.null(a)) "PLS-PM" else a
}

# The arguments of cSEM::csem() that follow the estimation options: the
# weighting approach, the PLS modes and inner scheme (PLS-PM only) and the
# disattenuation (PLSc, or GSCAm when the approach is GSCA).
# ES: Los argumentos de cSEM::csem() que siguen a las opciones de estimación.
.pl_csem_args <- function(options, modes = NULL, approach = options$weightApproach) {
    a <- .pl_approach(approach)
    pls <- identical(a, "PLS-PM")
    list(.approach_weights = a,
         .PLS_modes = if (pls && length(modes)) modes else NULL,
         .disattenuate = identical(options$estimator, "plsc"),
         .PLS_weight_scheme_inner = options$innerScheme)
}

# Why the model cannot be estimated with this approach (NULL when it can):
# a key of the "errors" text section. Category A: the analysis stops.
# ES: Por qué el modelo no puede estimarse con este enfoque (NULL si puede).
# Categoría A: el análisis se detiene.
.pl_approach_reject <- function(options, blocks, hocs, nonlinear) {
    a <- options$weightApproach
    if (is.null(a) || a == "pls") return(NULL)
    if (identical(options$estimator, "plsc") && a != "gsca") return("approach_plsc")
    if (length(hocs)) return("approach_hoc")
    if (nonlinear && a == "gsca") return("approach_gsca_nonlinear")
    if (a == "pca" && any(vapply(blocks, function(b) length(b$vars) == 1, TRUE))) return("approach_pca_single")
    NULL
}

# MAXVAR fixes the weights only up to a common sign, and cSEM 0.7.1 does not
# correct it in the resamples (its .sign_change_option has no effect for
# MAXVAR): about one draw in five reverses every loading and weight. Each
# construct of each draw is turned back to the sign of its loadings in the
# original estimation, and paths and effects take the product of the signs
# of their two constructs (Tenenhaus et al., 2005, construct-level
# correction). Any other approach is returned unchanged.
# ES: MAXVAR fija los pesos solo hasta un signo común, y cSEM 0.7.1 no lo
# corrige en los remuestreos: cerca de una réplica de cada cinco invierte
# todas las cargas y pesos. Cada constructo de cada réplica vuelve al signo
# de sus cargas en la estimación original, y rutas y efectos toman el
# producto de los signos de sus dos constructos.
.pl_fix_signs <- function(merged) {
    if (!identical(merged$Information$Arguments$.approach_weights, "MAXVAR")) return(merged)
    E <- merged$Estimates$Estimates_resample$Estimates1
    L <- E$Loading_estimates
    if (is.null(L)) return(merged)
    lc <- sub(" =~ .*$", "", names(L$Original))
    cons <- unique(lc)
    ref <- vapply(cons, function(cc) sign(sum(L$Original[lc == cc])), 0)
    S <- vapply(cons, function(cc) sign(rowSums(L$Resampled[, lc == cc, drop = FALSE])) * ref[[cc]], numeric(nrow(L$Resampled)))
    S <- matrix(S, nrow = nrow(L$Resampled), dimnames = list(NULL, cons))
    S[S == 0] <- 1
    flip_by <- function(q, cons_of) {
        if (is.null(q) || is.null(q$Resampled)) return(q)
        k <- cons_of(names(q$Original))
        for (j in seq_along(k)) if (all(k[[j]] %in% cons))
            q$Resampled[, j] <- q$Resampled[, j] * apply(S[, k[[j]], drop = FALSE], 1, prod)
        q
    }
    one <- function(sep) function(nm) lapply(nm, function(x) sub(paste0(" ", sep, " .*$"), "", x))
    two <- function(nm) lapply(nm, function(x) strsplit(x, " ~ ", fixed = TRUE)[[1]])
    E$Loading_estimates <- flip_by(L, one("=~"))
    E$Weight_estimates <- flip_by(E$Weight_estimates, one("<~"))
    E$Path_estimates <- flip_by(E$Path_estimates, two)
    for (eff in c("Total_effect", "Indirect_effect"))
        if (!is.null(E[[eff]])) E[[eff]] <- flip_by(E[[eff]], two)
    merged$Estimates$Estimates_resample$Estimates1 <- E
    merged
}
