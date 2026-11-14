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

# Overall fit (phase 5): the table equals direct cSEM::testOMF() calls with
# the same seed, for the estimated and the saturated model; models where the
# test is not defined keep their rows with the reason.
# ES: Ajuste global (fase 5): la tabla es igual a llamadas directas a
# cSEM::testOMF() con la misma semilla; los modelos donde la prueba no está
# definida conservan sus filas con la razón.

test_that("overall fit equals cSEM::testOMF() for the estimated and the saturated model", {
    d <- cSEM::satisfaction[, c(paste0("imag", 1:5), paste0("sat", 1:4), paste0("loy", 1:4))]
    m <- list(reflective = list(block("IMAG", paste0("imag", 1:5)), block("SAT", paste0("sat", 1:4)),
                                block("LOY", paste0("loy", 1:4))),
              formative = list(),
              structural = list(srow("SAT", "IMAG"), srow("LOY", "SAT")))
    a <- runPlssem(d, m, fitTest = TRUE, fitReps = 99, bootSeed = 7)
    tab <- a$results$fit$asDF
    expect_equal(nrow(tab), 8)

    model <- "IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
              SAT <~ sat1 + sat2 + sat3 + sat4
              LOY <~ loy1 + loy2 + loy3 + loy4
              SAT ~ IMAG
              LOY ~ SAT"
    f <- cSEM::csem(d, model, .PLS_modes = list(IMAG = "modeA", SAT = "modeA", LOY = "modeA"),
                    .disattenuate = FALSE)
    for (sat in c(FALSE, TRUE)) {
        t <- cSEM::testOMF(f, .R = 99, .seed = 7, .alpha = 0.05, .saturated = sat)
        rows <- tab[(if (sat) 5:8 else 1:4), ]
        expect_equal(rows$value, unname(t$Test_statistic[c("SRMR", "dG", "dL", "dML")]), tolerance = 1e-10)
        expect_equal(rows$critical, unname(t$Critical_value[c("SRMR", "dG", "dL", "dML"), 1]), tolerance = 1e-10)
    }
    # LOY ~ IMAG is left out: the two tests differ
    # ES: falta LOY ~ IMAG: las dos pruebas difieren
    expect_false(isTRUE(all.equal(tab$value[1:4], tab$value[5:8])))
    expect_match(gsub("<[^>]+>", " ", a$results$interpretation$content), "Overall model fit")
    expect_match(a$results$keySummary$content, "Overall fit:")
})

test_that("with an interaction the fit rows stay as not computable", {
    d <- cSEM::satisfaction[, c(paste0("imag", 1:5), paste0("sat", 1:4), paste0("loy", 1:4))]
    m <- list(reflective = list(block("IMAG", paste0("imag", 1:5)), block("SAT", paste0("sat", 1:4)),
                                block("LOY", paste0("loy", 1:4))),
              formative = list(),
              structural = list(list(label = "LOY", predictors = list("SAT", "IMAG", c("SAT", "IMAG")))))
    a <- runPlssem(d, m, fitTest = TRUE, fitReps = 99)
    tab <- a$results$fit$asDF
    expect_equal(nrow(tab), 8)
    expect_true(all(tab$decision == "Not computable"))
})
