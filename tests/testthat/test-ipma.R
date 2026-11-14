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

# IPMA (phase 6): importance equals cSEM::doIPMA()'s, and performance is its
# performance rescaled to 0-100 with the scale range.
# ES: IPMA (fase 6): la importancia es la de cSEM::doIPMA() y el desempeño es
# el suyo reescalado a 0-100 con el rango de la escala.

test_that("IPMA equals cSEM::doIPMA rescaled to 0-100", {
    a <- runPlssem(corpRepData(), simpleModel(), ipma = TRUE, ipmaRange = "manual", ipmaMin = 1, ipmaMax = 7)
    tab <- a$results$ipmaTable$asDF
    env <- new.env(); env$data <- corpRepData()
    utils::capture.output(eval(parse(text = reproCode(a)), envir = env))
    ip <- cSEM::doIPMA(env$fit)
    cm <- c(COMP = "C1", LIKE = "C2", CUSA = "C3", CUSL = "C4")
    for (k in seq_len(nrow(tab))) {
        expect_equal(tab$importance[k], ip$Construct$Importance[cm[[tab$target[k]]], cm[[tab$construct[k]]]],
                     tolerance = 1e-10)
        expect_equal(tab$performance[k], 100 * (ip$Construct$Performance[[cm[[tab$construct[k]]]]] - 1) / 6,
                     tolerance = 1e-10)
    }
    expect_equal(length(a$results$ipmaPlots$items), 2)
})

test_that("IPMA with consistent PLS stays as not computable", {
    a <- runPlssem(corpRepData(), simpleModel(), ipma = TRUE, estimator = "plsc")
    expect_true(all(a$results$ipmaTable$asDF$quadrant == "Not computable"))
})
