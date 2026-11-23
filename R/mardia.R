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
# Mardia's (1970) multivariate skewness and kurtosis of the indicators (1.2.0).
# PLS-SEM does not assume normality, but the measures describe how far the
# data are from it, which matters for the maximum-likelihood steps of the
# module (the multigroup CFA) and for reading bootstrap intervals; Cain,
# Zhang and Yuan (2017) recommend reporting them.
#   b1 = (1 / n^2) sum_ij (z_i' S^-1 z_j)^3,  n b1 / 6 ~ chi2 with
#        p (p + 1) (p + 2) / 6 df
#   b2 = (1 / n) sum_i (z_i' S^-1 z_i)^2,     (b2 - p (p + 2)) /
#        sqrt(8 p (p + 2) / n) ~ N(0, 1)
# S is the covariance matrix with denominator n (Mardia's definition), as in
# MVN::mvn(); psych::mardia() uses denominator n - 1, so its b1 and b2 are
# ours times ((n - 1) / n)^3 and ((n - 1) / n)^2 (the identity checked in
# validation/csem_equivalence.R).
#
# ES: Asimetría y curtosis multivariadas de Mardia (1970) de los indicadores
# (1.2.0). PLS-SEM no supone normalidad, pero estas medidas describen cuánto
# se alejan los datos de ella, lo que importa en los pasos de máxima
# verosimilitud del módulo (el AFC multigrupo) y al leer los intervalos
# bootstrap; Cain, Zhang y Yuan (2017) recomiendan informarlas.
# -----------------------------------------------------------------------------
.pl_mardia <- function(x) {
    x <- as.matrix(x)
    x <- x[stats::complete.cases(x), , drop = FALSE]
    n <- nrow(x)
    p <- ncol(x)
    z <- scale(x, scale = FALSE)
    S <- crossprod(z) / n
    Sinv <- tryCatch(solve(S), error = function(e) NULL)
    if (is.null(Sinv)) return(NULL)
    D <- z %*% Sinv %*% t(z)
    b1 <- sum(D^3) / n^2
    b2 <- sum(diag(D)^2) / n
    df <- p * (p + 1) * (p + 2) / 6
    skew_stat <- n * b1 / 6
    kurt_z <- (b2 - p * (p + 2)) / sqrt(8 * p * (p + 2) / n)
    data.frame(measure = c("skewness", "kurtosis"), value = c(b1, b2),
               statistic = c(skew_stat, kurt_z), df = c(df, NA),
               p = c(stats::pchisq(skew_stat, df, lower.tail = FALSE), 2 * stats::pnorm(-abs(kurt_z))),
               n = n, k = p, stringsAsFactors = FALSE)
}
