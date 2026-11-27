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
# Measurement invariance of the reflective constructs as common factors
# (1.1.0): multigroup confirmatory factor analysis with lavaan, configural,
# metric (equal loadings) and scalar (equal loadings and intercepts) models
# across the groups of the grouping variable (Vandenberg & Lance, 2000;
# Putnick & Bornstein, 2016). Each step is compared with the previous one by
# the chi-square difference test and by the change in CFI, RMSEA and SRMR
# (Cheung & Rensvold, 2002; Chen, 2007). It sits next to MICOM: MICOM tests
# the composites PLS estimates; MGCFA tests the common-factor reading of the
# reflective constructs (the one PLSc estimates).
#
# ES: Invarianza de medida de los constructos reflectivos como factores
# comunes (1.1.0): análisis factorial confirmatorio multigrupo con lavaan,
# modelos configural, métrico (cargas iguales) y escalar (cargas e
# interceptos iguales) entre los grupos. Cada paso se compara con el anterior
# por la diferencia de chi cuadrado y por el cambio en CFI, RMSEA y SRMR. Se
# ubica junto a MICOM: MICOM prueba los compuestos que estima PLS; el MGCFA
# prueba la lectura de factor común de los constructos reflectivos.
# -----------------------------------------------------------------------------

.PL_MGCFA_STEPS <- c("configural", "metric", "scalar")

# The reflective constructs that enter the CFA (two or more indicators: a
# single-indicator factor is not identified) as lavaan syntax in internal
# names; NULL when there is none. ES: Los constructos reflectivos que entran
# en el AFC (dos o más indicadores), como sintaxis de lavaan.
.pl_mgcfa_syntax <- function(blocks, cmap, vmap) {
    b <- Filter(function(b) identical(b$type, "reflective") && length(b$vars) > 1, blocks)
    if (!length(b)) return(NULL)
    paste(vapply(b, function(x) paste(cmap[[x$label]], "=~", paste(unname(unlist(vmap[x$vars])), collapse = " + ")), ""),
          collapse = "\n")
}

# Why the MGCFA cannot run (NULL when it can). Category B.
# ES: Por qué no puede correr el MGCFA (NULL si puede). Categoría B.
.pl_mgcfa_reason <- function(syntax, scale) {
    if (is.null(syntax)) return("mgcfa_none")
    if (identical(scale, "ordinal")) return("mgcfa_ordinal")
    NULL
}

# Fits the three models and compares each with the previous one. estimator:
# "ML" or "MLR" (robust: scaled chi-square difference of Satorra & Bentler,
# 2001, and robust CFI and RMSEA). Returns a data.frame with one row per
# step, or stops with lavaan's error.
# ES: Ajusta los tres modelos y compara cada uno con el anterior.
.pl_mgcfa <- function(data, syntax, estimator = "MLR") {
    fit <- function(eq) lavaan::cfa(syntax, data = data, group = "PLgroup", estimator = estimator,
                                    meanstructure = TRUE, group.equal = eq)
    fits <- list(configural = fit(character(0)), metric = fit("loadings"), scalar = fit(c("loadings", "intercepts")))
    conv <- vapply(fits, lavaan::lavInspect, TRUE, what = "converged")
    robust <- identical(estimator, "MLR")
    m <- if (robust) c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "rmsea.robust", "srmr")
         else c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr")
    fm <- t(vapply(fits, function(f) unname(lavaan::fitMeasures(f, m)), numeric(6)))
    out <- data.frame(step = .PL_MGCFA_STEPS, chisq = fm[, 1], df = fm[, 2], p = fm[, 3], cfi = fm[, 4],
                      rmsea = fm[, 5], srmr = fm[, 6], dchisq = NA_real_, ddf = NA_real_, dp = NA_real_,
                      dcfi = NA_real_, drmsea = NA_real_, dsrmr = NA_real_, converged = conv,
                      stringsAsFactors = FALSE)
    for (k in 2:3) {
        lrt <- suppressWarnings(lavaan::lavTestLRT(fits[[k - 1]], fits[[k]]))
        out$dchisq[k] <- lrt[["Chisq diff"]][2]
        out$ddf[k] <- lrt[["Df diff"]][2]
        out$dp[k] <- lrt[["Pr(>Chisq)"]][2]
        out$dcfi[k] <- out$cfi[k] - out$cfi[k - 1]
        out$drmsea[k] <- out$rmsea[k] - out$rmsea[k - 1]
        out$dsrmr[k] <- out$srmr[k] - out$srmr[k - 1]
    }
    out$decision <- .pl_mgcfa_decision(out)
    out
}

# Decision per step. Configural: the fit of the model in every group, with
# Hu and Bentler's (1999) two-index combination (CFI >= .95 and SRMR <= .08
# good; CFI >= .90 and SRMR <= .10 acceptable); RMSEA is reported but left
# out of the rule, because with few degrees of freedom it overstates misfit. Metric and scalar (Chen, 2007): supported when CFI drops
# by no more than .010 and RMSEA rises by no more than .015 (SRMR by no more
# than .030 for metric, .010 for scalar); a step is only judged when the
# previous one holds.
# ES: Decisión por paso: configural por el ajuste (Hu y Bentler, 1999);
# métrica y escalar por los cambios de Chen (2007); un paso solo se juzga si
# el anterior se sostiene.
.pl_mgcfa_decision <- function(t) {
    out <- character(3)
    out[1] <- if (!is.finite(t$cfi[1]) || !is.finite(t$srmr[1])) "mgcfa_na"
              else if (t$cfi[1] >= 0.95 && t$srmr[1] <= 0.08) "mgcfa_good"
              else if (t$cfi[1] >= 0.90 && t$srmr[1] <= 0.10) "mgcfa_acceptable" else "mgcfa_poor"
    srmr_cut <- c(NA, 0.030, 0.010)
    for (k in 2:3) {
        prev_ok <- out[k - 1] %in% c("mgcfa_good", "mgcfa_acceptable", "mgcfa_supported")
        if (!prev_ok) { out[k] <- "mgcfa_not_judged"; next }
        ok <- is.finite(t$dcfi[k]) && t$dcfi[k] >= -0.010 && t$drmsea[k] <= 0.015 &&
              (!is.finite(t$dsrmr[k]) || t$dsrmr[k] <= srmr_cut[k])
        out[k] <- if (ok) "mgcfa_supported" else "mgcfa_not_supported"
    }
    out
}
