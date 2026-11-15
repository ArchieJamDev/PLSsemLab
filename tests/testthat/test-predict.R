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

# PLSpredict and CVPAT (phase 6). With one repetition the module's
# cross-validation uses cSEM's folds, so its metrics must equal
# cSEM::predict(.r = 1, .seed = seed), and its CVPAT of two models must equal
# cSEM::testCVPAT(.seed = seed).
# ES: PLSpredict y CVPAT (fase 6). Con una repetición la validación cruzada
# del módulo usa las particiones de cSEM: sus métricas deben ser iguales a
# las de cSEM::predict(.r = 1, .seed = semilla), y su CVPAT de dos modelos
# igual a cSEM::testCVPAT(.seed = semilla).

satModel <- function() list(
    d = cSEM::satisfaction[, c(paste0("imag", 1:5), paste0("sat", 1:4), paste0("loy", 1:4))],
    m = list(reflective = list(block("IMAG", paste0("imag", 1:5)), block("SAT", paste0("sat", 1:4)),
                               block("LOY", paste0("loy", 1:4))),
             formative = list(),
             structural = list(srow("SAT", "IMAG"), srow("LOY", "SAT", "IMAG"))),
    syntax = "IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
              SAT <~ sat1 + sat2 + sat3 + sat4
              LOY <~ loy1 + loy2 + loy3 + loy4
              SAT ~ IMAG
              LOY ~ SAT + IMAG",
    modes = list(IMAG = "modeA", SAT = "modeA", LOY = "modeA"))

test_that("PLSpredict equals cSEM::predict with one repetition", {
    s <- satModel()
    a <- runPlssem(s$d, s$m, predictRun = TRUE, predictFolds = 10, predictReps = 1, bootSeed = 11)
    tab <- a$results$predict$asDF
    f <- cSEM::csem(s$d, s$syntax, .PLS_modes = s$modes, .disattenuate = FALSE)
    p <- withDefaultRng(suppressWarnings(cSEM::predict(f, .benchmark = "lm", .cv_folds = 10, .r = 1, .seed = 11)))$Prediction_metrics
    p <- p[match(tab$indicator, p$Name), ]
    expect_equal(tab$q2, p$Q2_predict, tolerance = 1e-8)
    expect_equal(tab$rmsePls, p$RMSE_target, tolerance = 1e-8)
    expect_equal(tab$rmseLm, p$RMSE_benchmark, tolerance = 1e-8)
    expect_equal(tab$maePls, p$MAE_target, tolerance = 1e-8)
    expect_equal(tab$maeLm, p$MAE_benchmark, tolerance = 1e-8)
    expect_equal(nrow(a$results$cvpat$asDF), 6)
    expect_match(a$results$keySummary$content, "Prediction:")
})

test_that("CVPAT of two models equals cSEM::testCVPAT", {
    s <- satModel()
    f1 <- cSEM::csem(s$d, s$syntax, .PLS_modes = s$modes, .disattenuate = FALSE)
    alt <- sub("LOY ~ SAT + IMAG", "LOY ~ SAT", s$syntax, fixed = TRUE)
    f2 <- cSEM::csem(s$d, alt, .PLS_modes = s$modes, .disattenuate = FALSE)
    ref <- withDefaultRng(suppressWarnings(cSEM::testCVPAT(f1, f2, .seed = 11, .cv_folds = 10)))
    refit <- function(data, syn) cSEM::csem(data, syn, .PLS_modes = s$modes, .disattenuate = FALSE)
    cv <- PLSsemLab:::.pl_cv_residuals(f1, s$d, function(x) refit(x, s$syntax), 10, 1, 11,
                                       alt_refit = function(x) refit(x, alt))
    ends <- colnames(cv$res[[1]]$PLS)
    out <- PLSsemLab:::.pl_cvpat(cv, list(all = ends))
    r <- out[out$bench == "ALT", ]
    # cSEM: D = L(model 2) - L(model 1); the module: L(model 1) - L(model 2)
    # ES: cSEM: D = L(modelo 2) - L(modelo 1); el módulo, al revés
    expect_equal(r$t, -unname(ref$test_statistic), tolerance = 1e-8)
    expect_equal(r$p, unname(ref$p_value), tolerance = 1e-8)
})

test_that("with an interaction prediction stays as not computable", {
    s <- satModel()
    s$m$structural <- list(list(label = "LOY", predictors = list("SAT", "IMAG", c("SAT", "IMAG"))))
    a <- runPlssem(s$d, s$m, predictRun = TRUE, predictReps = 1)
    expect_true(all(a$results$predict$asDF$lower == "Not computable"))
})

test_that("CVPAT against a competing model through the options equals cSEM::testCVPAT", {
    s <- satModel()
    a <- runPlssem(s$d, s$m, predictRun = TRUE, predictFolds = 10, predictReps = 1, bootSeed = 11,
                   cvpatDrop = list("IMAG → LOY"))
    tab <- a$results$cvpat$asDF
    r <- tab[tab$bench == "Competing model" & tab$construct == "All endogenous constructs", ]
    f1 <- cSEM::csem(s$d, s$syntax, .PLS_modes = s$modes, .disattenuate = FALSE)
    f2 <- cSEM::csem(s$d, sub("LOY ~ SAT + IMAG", "LOY ~ SAT", s$syntax, fixed = TRUE), .PLS_modes = s$modes,
                     .disattenuate = FALSE)
    ref <- withDefaultRng(suppressWarnings(cSEM::testCVPAT(f1, f2, .seed = 11, .cv_folds = 10)))
    expect_equal(r$t, -unname(ref$test_statistic), tolerance = 1e-8)
    expect_equal(r$p, unname(ref$p_value), tolerance = 1e-8)
    expect_equal(nrow(tab), 9)
})

test_that("the comparison with LM uses the MAE when the errors are asymmetric", {
    s <- satModel()
    a <- runPlssem(s$d, s$m, predictRun = TRUE, predictReps = 2, bootSeed = 3)
    tab <- a$results$predict$asDF
    expect_true(all(tab$metric == ifelse(abs(tab$skew) > 1, "MAE", "RMSE")))
    better <- ifelse(tab$metric == "MAE", tab$maePls < tab$maeLm, tab$rmsePls < tab$rmseLm)
    expect_equal(tab$lower, ifelse(better, "PLS", "LM"))
    expect_true(all(c("metric", "q2") %in% names(a$results$predict$notes)))
    # 250 cases, 10 folds: 25 per fold and 225 training cases for 5 predictors of LM
    # ES: 250 casos, 10 particiones: 25 por partición, 225 de entrenamiento
    expect_false("small" %in% names(a$results$predict$notes))
    expect_true(PLSsemLab:::.pl_predict_small(120, 10, 11)$small)
})
