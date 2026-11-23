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

# Version 1.2.0: weighting approaches, jackknife, Mardia, construct scores,
# the extra PLSpredict benchmark and the comparison of the main and the
# competing model, each against direct calls or its definition.
# ES: Versión 1.2.0: enfoques de ponderación, jackknife, Mardia,
# puntuaciones, referencia adicional de PLSpredict y comparación de modelos,
# cada uno frente a llamadas directas o a su definición.

pl <- function(name) get(name, envir = asNamespace("PLSsemLab"))
csemCorp <- function(approach) {
    d <- corpRepData()
    v <- c(paste0("comp_", 1:3), paste0("like_", 1:3), "cusa", paste0("cusl_", 1:3))
    d <- d[stats::complete.cases(d[, v]), v]
    m <- "COMP <~ comp_1 + comp_2 + comp_3\nLIKE <~ like_1 + like_2 + like_3\nCUSA <~ cusa\nCUSL <~ cusl_1 + cusl_2 + cusl_3\nCUSA ~ COMP + LIKE\nCUSL ~ COMP + LIKE + CUSA"
    list(d = d, fit = cSEM::csem(d, m, .approach_weights = approach, .disattenuate = FALSE,
                                 .PLS_modes = if (approach == "PLS-PM") list(COMP = "modeA", LIKE = "modeA", CUSA = "modeA", CUSL = "modeA")))
}

test_that("every weighting approach reproduces cSEM's paths and loadings", {
    for (a in c("gsca", "sumcorr", "maxvar", "unit")) {
        x <- runPlssem(corpRepData(), simpleModel(), weightApproach = a)
        ref <- csemCorp(pl(".pl_approach")(a))$fit$Estimates
        p <- x$results$structuralSection$paths$asDF
        # SUMCORR and MAXVAR iterate to cSEM's tolerance (1e-5): the module
        # and a direct call can stop a few iterations apart
        # ES: SUMCORR y MAXVAR iteran hasta la tolerancia de cSEM (1e-5)
        expect_equal(p$estimate[p$from == "CUSA" & p$to == "CUSL"], ref$Path_estimates["CUSL", "CUSA"], tolerance = 1e-4, info = a)
        l <- x$results$measurementSection$measurement$asDF
        expect_equal(l$loading[l$indicator == "comp_1"], ref$Loading_estimates["COMP", "comp_1"], tolerance = 1e-4, info = a)
        # rho_A is PLS-PM only
        # ES: rho_A es solo de PLS-PM
        expect_true(all(is.na(suppressWarnings(as.numeric(x$results$measurementSection$reliability$asDF$rhoA)))), info = a)
    }
})

test_that("combinations cSEM cannot estimate stop with the reason", {
    expect_error(runPlssem(corpRepData(), simpleModel(), weightApproach = "pca"), "single indicator")
    expect_error(runPlssem(corpRepData(), simpleModel(), weightApproach = "sumcorr", estimator = "plsc"), "Consistent estimation")
    # GSCA with consistent estimation is GSCAm: it runs
    # ES: GSCA con estimación consistente es GSCAm: corre
    x <- runPlssem(corpRepData(), simpleModel(), weightApproach = "gsca", estimator = "plsc")
    expect_true(nrow(x$results$structuralSection$paths$asDF) == 5)
})

test_that("IPMA and NCA are not computable outside PLS-PM, with the reason", {
    x <- runPlssem(corpRepData(), simpleModel(), weightApproach = "gsca", ipma = TRUE, ncaRun = TRUE, ncaPerm = 99)
    expect_match(x$results$ipmaSection$ipmaTable$notes$reason$note, "PLS-PM")
    expect_true(all(x$results$ncaSection$ncaTable$asDF$verdict == "Not computable"))
})

test_that("MAXVAR draws are turned back to the original sign", {
    r <- csemCorp("MAXVAR")
    b <- withDefaultRng(cSEM::resamplecSEMResults(r$fit, .R = 200, .seed = 3))
    L0 <- b$Estimates$Estimates_resample$Estimates1$Loading_estimates$Resampled
    expect_gt(sum(L0 < 0), 0)
    fixed <- pl(".pl_fix_signs")(b)$Estimates$Estimates_resample$Estimates1
    expect_equal(sum(fixed$Loading_estimates$Resampled < 0), 0)
    # a global flip leaves the paths as they were
    # ES: una inversión global deja las rutas como estaban
    expect_equal(fixed$Path_estimates$Resampled, b$Estimates$Estimates_resample$Estimates1$Path_estimates$Resampled)
    x <- runPlssem(corpRepData(), simpleModel(), weightApproach = "maxvar", bootstrap = TRUE, bootReps = 200, bootSeed = 3)
    expect_gt(min(x$results$measurementSection$measurement$asDF$loadLower), 0)
})

test_that("jackknife intervals are estimate +/- t(n - 1) x jackknife SE", {
    x <- runPlssem(corpRepData(), simpleModel(), bootstrap = TRUE, resampleMethod = "jackknife")
    p <- x$results$structuralSection$paths$asDF
    r <- csemCorp("PLS-PM")
    j <- cSEM::resamplecSEMResults(r$fit, .resample_method = "jackknife")
    pe <- j$Estimates$Estimates_resample$Estimates1$Path_estimates
    n <- nrow(pe$Resampled)
    se <- sqrt((n - 1) / n * colSums(sweep(pe$Resampled, 2, colMeans(pe$Resampled))^2))
    expect_equal(p$se, unname(se), tolerance = 1e-10)
    expect_equal(p$lower, unname(pe$Original - stats::qt(0.975, n - 1) * se), tolerance = 1e-10)
    # cSEM's own standard error agrees
    # ES: el error estándar de cSEM coincide
    expect_equal(unname(cSEM::infer(j, .quantity = "sd")$Path_estimates$sd), unname(se), tolerance = 1e-10)
    expect_equal(x$results$bootSummary$asDF$drawn, n)
    # specific indirect effects: jackknife of the products
    # ES: efectos indirectos específicos: jackknife de los productos
    med <- x$results$structuralSection$mediation$asDF
    prod <- pe$Resampled[, "CUSA ~ COMP"] * pe$Resampled[, "CUSL ~ CUSA"]
    se_m <- sqrt((n - 1) / n * sum((prod - mean(prod))^2))
    expect_equal(med$se[1], se_m, tolerance = 1e-10)
    expect_match(gsub("<[^>]+>", " ", x$results$interpretation$content), "With the jackknife")
})

test_that("Mardia's measures follow the definition (psych uses denominator n - 1)", {
    d <- csemCorp("PLS-PM")$d
    m <- pl(".pl_mardia")(d)
    ps <- psych::mardia(d, plot = FALSE)
    n <- nrow(d)
    expect_equal(m$value[1] * ((n - 1) / n)^3, ps$b1p, tolerance = 1e-10)
    expect_equal(m$value[2] * ((n - 1) / n)^2, ps$b2p, tolerance = 1e-10)
    expect_equal(m$df[1], 10 * 11 * 12 / 6)
    x <- runPlssem(corpRepData(), simpleModel(), mardia = TRUE)
    tab <- x$results$measurementSection$mardia$asDF
    expect_equal(tab$value, m$value, tolerance = 1e-10)
})

test_that("the model comparison equals cSEM's criteria of both models", {
    x <- runPlssem(corpRepData(), simpleModel(), compareRun = TRUE, cvpatDrop = list("COMP → CUSL"))
    tab <- x$results$compareSection$compare$asDF
    r <- csemCorp("PLS-PM")
    alt <- cSEM::csem(r$d, "COMP <~ comp_1 + comp_2 + comp_3\nLIKE <~ like_1 + like_2 + like_3\nCUSA <~ cusa\nCUSL <~ cusl_1 + cusl_2 + cusl_3\nCUSA ~ COMP + LIKE\nCUSL ~ LIKE + CUSA",
                      .PLS_modes = list(COMP = "modeA", LIKE = "modeA", CUSA = "modeA", CUSL = "modeA"), .disattenuate = FALSE)
    c1 <- cSEM::calculateModelSelectionCriteria(r$fit, .by_equation = TRUE, .only_structural = TRUE)
    c2 <- cSEM::calculateModelSelectionCriteria(alt, .by_equation = TRUE, .only_structural = TRUE)
    k <- tab$construct == "CUSL" & tab$criterion == "BIC (recommended)"
    expect_equal(tab$main[k], unname(c1$BIC["CUSL"]), tolerance = 1e-10)
    expect_equal(as.numeric(tab$competing[k]), unname(c2$BIC["CUSL"]), tolerance = 1e-10)
    expect_equal(tab$preferred[k], "Competing")
    s <- tab[tab$criterion == "SRMR", ]
    expect_equal(s$main, unname(cSEM::calculateSRMR(r$fit)), tolerance = 1e-10)
    pp <- x$results$compareSection$comparePaths$asDF
    expect_equal(pp$competing[pp$from == "COMP" & pp$to == "CUSL"], "—")
    # without left-out paths the table says why
    # ES: sin rutas excluidas la tabla dice por qué
    y <- runPlssem(corpRepData(), simpleModel(), compareRun = TRUE)
    expect_true(all(y$results$compareSection$compare$asDF$preferred == "Not computable"))
})

test_that("the extra PLSpredict benchmark equals cSEM's benchmark on the same folds", {
    # cSEM 0.7.1's predict(.test_data = ...) returns the target residuals as
    # the benchmark's (the benchmark is not applied); its cross-validation
    # mode does apply it, so the reference is that mode with the same seed.
    # ES: predict(.test_data = ...) de cSEM 0.7.1 devuelve como residuos de
    # la referencia los del modelo; su modo de validación cruzada sí la
    # aplica, así que la referencia es ese modo con la misma semilla.
    r <- csemCorp("PLS-PM")
    ref <- withDefaultRng(cSEM::predict(r$fit, .benchmark = "GSCA", .cv_folds = 5, .r = 1, .seed = 2026))$Prediction_metrics
    x <- runPlssem(corpRepData(), simpleModel(), predictRun = TRUE, predictFolds = 5, predictReps = 1,
                   predictBenchmark = "gsca", bootSeed = 2026)
    pt <- x$results$predictSection$predict$asDF
    expect_equal(pt$rmseBench, ref$RMSE_benchmark[match(pt$indicator, ref$Name)], tolerance = 1e-8)
    expect_equal(pt$maeBench, ref$MAE_benchmark[match(pt$indicator, ref$Name)], tolerance = 1e-8)
    expect_equal(pt$rmsePls, ref$RMSE_target[match(pt$indicator, ref$Name)], tolerance = 1e-8)
    cv <- x$results$predictSection$cvpat$asDF
    expect_true("GSCA" %in% cv$bench)
})
