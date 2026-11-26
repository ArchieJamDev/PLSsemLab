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

# Changes requested in jamovi's review of 1.0.0: sections the model does not
# use are hidden from the first frame; the diagram follows the theme; the
# prose has no inline styles; the bootstrap record is the table.
# ES: Cambios pedidos en la revisión de jamovi de la 1.0.0: las secciones que
# el modelo no usa se ocultan desde el primer fotograma; el diagrama sigue el
# tema; el texto no tiene estilos en línea; el registro del bootstrap es la tabla.

test_that("a plain model hides the higher-order, formative, interaction tables and guides", {
    a <- runPlssem(corpRepData(), simpleModel())
    ms <- a$results$measurementSection
    ss <- a$results$structuralSection
    for (item in list(ms$hocGuide, ms$hocMeasurement, ms$hocReliability, ms$hocHtmt, ms$vif, ms$vifGuide,
                      ss$simpleSlopes, ss$moderationGuide, ss$moderationPlots))
        expect_false(item$visible)
    # the simple model has the chain COMP -> CUSA -> CUSL, and reflective constructs with HTMT
    # ES: el modelo simple tiene la cadena COMP -> CUSA -> CUSL y constructos reflectivos con HTMT
    expect_true(ss$mediation$visible)
    expect_true(ms$htmt$visible)
    expect_true(ms$reliability$visible)
})

test_that("the formative VIF table shows when the model has formative constructs", {
    a <- runPlssem(corpRepData(), extendedModel())
    expect_true(a$results$measurementSection$vif$visible)
    expect_false(a$results$measurementSection$hocMeasurement$visible)
    expect_false(a$results$structuralSection$simpleSlopes$visible)
})

test_that("the bootstrap record is the table, with the time as a footnote and no seconds column", {
    a <- runPlssem(corpRepData(), simpleModel(), bootstrap = TRUE, bootReps = 100, bootSeed = 7)
    bs <- a$results$bootSummary$asDF
    expect_null(bs$seconds)
    expect_equal(bs$seed, 7)
    expect_false(a$results$bootStatus$visible)
})

test_that("the diagram takes its colours from jamovi's theme only", {
    a <- runPlssem(corpRepData(), simpleModel())
    expect_null(a$options$latentFill)
    expect_null(a$options$manifestFill)
    expect_null(a$options$edgeColor)
})

test_that("the prose blocks carry no inline styles", {
    block <- get(".pl_html_block", envir = asNamespace("PLSsemLab"))
    expect_false(grepl("style=", block("A paragraph."), fixed = TRUE))
    expect_false(grepl("style=", block("A paragraph.", title = "A title"), fixed = TRUE))
})

test_that("the equivalent R code is plain text, not escaped HTML", {
    a <- runPlssem(corpRepData(), simpleModel(), showRepro = TRUE)
    code <- a$results$reproducibilityCode$content
    expect_false(grepl("&lt;", code, fixed = TRUE))
    expect_false(grepl("<pre", code, fixed = TRUE))
    expect_match(code, "cSEM::csem", fixed = TRUE)
})

test_that("mean replacement fills each fold from its training cases only", {
    fill <- get(".pl_fold_fill", envir = asNamespace("PLSsemLab"))
    raw <- data.frame(a = c(1, NA, 3, NA, 5), b = c(2, 2, NA, 4, 6))
    f <- fill(raw[1:3, ], raw[4:5, ])
    # the training mean of a is (1 + 3) / 2 = 2, not the full-sample mean 3
    # ES: la media de entrenamiento de a es (1 + 3) / 2 = 2, no la de toda la muestra
    expect_equal(f$test$a, c(2, 5))
    expect_equal(f$train$a, c(1, 2, 3))
    expect_equal(f$train$b, c(2, 2, 2))
    expect_equal(f$test$b, c(4, 6))
})

test_that("PLSpredict runs with mean replacement and missing values", {
    d <- corpRepData()
    d$comp_1[1:5] <- NA
    a <- runPlssem(d, simpleModel(), missing = "mean", predictRun = TRUE, predictFolds = 5, predictReps = 1)
    expect_gt(nrow(a$results$predictSection$predict$asDF), 0)
})
