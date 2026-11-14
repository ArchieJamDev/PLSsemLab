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
# Overall model fit (phase 5): the bootstrap test of exact fit of Beran and
# Srivastava (1985), as Dijkstra and Henseler (2015) apply it to PLS, through
# cSEM::testOMF(). Each discrepancy between the observed and the
# model-implied indicator correlation matrix (SRMR, d_G, d_L, d_ML) is
# compared with a quantile of its bootstrap distribution under the null
# hypothesis of exact fit. Done for the estimated model and for the model
# with a saturated structural part (only the measurement model is tested).
# ES: Ajuste global (fase 5): la prueba bootstrap de ajuste exacto de Beran y
# Srivastava (1985), como la aplican Dijkstra y Henseler (2015) a PLS, con
# cSEM::testOMF(). Cada discrepancia entre la matriz de correlaciones
# observada y la implicada por el modelo se compara con un cuantil de su
# distribución bootstrap bajo la hipótesis nula de ajuste exacto. Se hace
# para el modelo estimado y para el modelo con la parte estructural saturada.
# -----------------------------------------------------------------------------

# Display order and labels: SRMR first, as most readers know it.
# ES: Orden y etiquetas: el SRMR primero, el más conocido.
.PL_FIT <- c(SRMR = "SRMR", dG = "d_G", dL = "d_L", dML = "d_ML")

# Why the test cannot run for this model (NULL when it can). Category B: the
# rows stay with this reason.
# ES: Por qué la prueba no puede correr con este modelo (NULL si puede).
.pl_fit_reason <- function(model, hocs, admissible) {
    if (isTRUE(model$nonlinear)) return("fit_nonlinear")
    if (length(hocs)) return("fit_hoc")
    if (!admissible) return("fit_inadmissible")
    NULL
}

# Runs testOMF() for the estimated and the saturated model. Returns a
# data.frame (model, measure, value, critical, p, reject) and the numbers of
# admissible resamples; p is (number of admissible bootstrap values at
# least as large as the observed one + 1) / (admissible values + 1), the
# usual bootstrap p-value that is never 0 (Davison & Hinkley, 1997).
# ES: Corre testOMF() para el modelo estimado y el saturado. p es (valores
# bootstrap admisibles al menos tan grandes como el observado + 1) /
# (valores admisibles + 1), que nunca es 0.
.pl_fit_test <- function(fit, reps, seed, level, checkpoint = function() NULL) {
    rows <- list()
    adm <- c()
    for (sat in c(FALSE, TRUE)) {
        checkpoint()
        t <- cSEM::testOMF(fit, .R = reps, .seed = seed, .alpha = 1 - level, .saturated = sat)
        boot <- do.call(rbind, t$Information$Bootstrap_values)
        key <- if (sat) "saturated" else "estimated"
        adm[[key]] <- t$Information$Number_admissibles
        for (m in names(.PL_FIT)) {
            v <- unname(t$Test_statistic[m])
            cv <- unname(t$Critical_value[m, 1])
            rows[[length(rows) + 1]] <- data.frame(model = key, measure = m, value = v, critical = cv,
                                                   p = (sum(boot[, m] >= v) + 1) / (nrow(boot) + 1), reject = v > cv,
                                                   stringsAsFactors = FALSE)
        }
    }
    list(table = do.call(rbind, rows), admissible = adm, reps = reps)
}
