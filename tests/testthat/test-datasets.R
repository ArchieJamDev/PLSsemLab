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
# The bundled datasets, end to end: each row of the "Which dataset for which
# option" table of data/README.md runs, and the simulated learning data show
# what they were built to show.
# ES: Los conjuntos incluidos de principio a fin: cada fila de la tabla "qué
# conjunto para qué opción" de data/README.md corre, y los datos simulados
# muestran lo que se construyó para mostrar.
# -----------------------------------------------------------------------------

learnHoc <- function() list(
    reflective = lapply(c("LEAD", "AUT", "VIG", "DED", "ABS", "PERF", "INT"), learnBlock),
    formative = list(learnBlock("RES")),
    structural = list(list(label = "ENG", predictors = list("LEAD", "RES", "AUT", c("LEAD", "AUT"))),
                      srow("PERF", "ENG", "LEAD"), srow("INT", "ENG", "LEAD")))

learnFirst <- function() list(
    reflective = lapply(c("LEAD", "AUT", "ENG", "PERF", "INT"), learnBlock),
    formative = list(learnBlock("RES")),
    structural = list(srow("ENG", "LEAD", "RES", "AUT"), srow("PERF", "ENG", "LEAD"), srow("INT", "ENG", "LEAD")))

# runPlssem() with higher-order constructs: the structural rows follow the
# panel's order (first-order constructs, then the higher-order ones).
# ES: runPlssem() con constructos de orden superior.
runHocModel <- function(data, m, hocs, type = "reflective", ...) {
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    labels <- c(vapply(c(m$reflective, m$formative), `[[`, "", "label"), vapply(hocs, `[[`, "", "label"))
    rows <- lapply(labels, function(n) {
        r <- Filter(function(x) identical(x$label, n), m$structural)
        if (length(r)) r[[1]]$predictors else list()
    })
    args <- list(reflective = m$reflective, formative = m$formative, structural = rows, ...)
    args[[if (type == "reflective") "reflectiveHoc" else "formativeHoc"]] <- hocs
    a <- cls$new(options = do.call(opts$new, args), data = data)
    a$init()
    a$run()
    a
}

test_that("the learning data: a reflective higher-order construct with a moderation and bootstrap", {
    d <- bundled("plssemlab_learning.csv")
    a <- runHocModel(d, learnHoc(), list(list(label = "ENG", components = list("VIG", "DED", "ABS"))),
                     missing = "listwise", bootstrap = TRUE, bootReps = 200, bootSeed = 1)
    p <- res(a, "paths")$asDF
    inter <- p[p$from == "LEAD × AUT", ]
    expect_gt(inter$estimate, 0)
    expect_true(all(is.finite(p$lower)))
    expect_true(all(res(a, "hocMeasurement")$asDF$loading > 0.8))
    expect_equal(nrow(res(a, "simpleSlopes")$asDF), 3)
    expect_true(all(is.finite(res(a, "simpleSlopes")$asDF$lower)))
    expect_true(nrow(res(a, "mediation")$asDF) > 0)
})

test_that("the learning data: a formative higher-order construct, both two-stage approaches", {
    d <- bundled("plssemlab_learning.csv")
    m <- list(reflective = lapply(c("LEAD", "AUT", "ENG", "PERF"), learnBlock), formative = list(),
              structural = list(srow("ENG", "SUP"), srow("PERF", "ENG", "SUP")))
    for (ap in c("disjoint", "csem")) {
        a <- runHocModel(d, m, list(list(label = "SUP", components = list("LEAD", "AUT"))), type = "formative",
                         hocApproach = ap, missing = "mean")
        expect_true(all(is.finite(res(a, "hocMeasurement")$asDF$weight)), info = ap)
        expect_true(all(is.finite(res(a, "paths")$asDF$estimate)), info = ap)
    }
})

test_that("the learning data: the sector difference, and no difference between regions", {
    skip_on_cran()
    d <- bundled("plssemlab_learning.csv")
    a <- runPlssem(d, learnFirst(), missing = "listwise", groupVar = "sector", mgaRun = TRUE,
                   mgaPerm = 999, mgaBoot = 499, bootSeed = 2026)
    expect_true(all(res(a, "micom")$asDF$invariance == "Full"))
    g <- res(a, "mga")$asDF
    expect_equal(g$decision[g$from == "LEAD" & g$to == "ENG"], "Yes (both tests)")
    expect_true(all(g$decision[!(g$from == "LEAD" & g$to == "ENG")] == "No"))
    r <- runPlssem(d, learnFirst(), missing = "listwise", groupVar = "region", mgaRun = TRUE,
                   mgaPerm = 499, mgaBoot = 299, bootSeed = 2026)
    expect_equal(nrow(res(r, "mga")$asDF), 3 * 7)
    # 21 comparisons: no difference in both tests; one borderline result of
    # a single test, gone with the Holm correction
    # ES: 21 comparaciones: ninguna diferencia en ambas pruebas; un resultado
    # límite de una sola prueba, que desaparece con Holm
    expect_false(any(res(r, "mga")$asDF$decision == "Yes (both tests)"))
    r <- runPlssem(d, learnFirst(), missing = "listwise", groupVar = "region", mgaRun = TRUE,
                   mgaPerm = 499, mgaBoot = 299, bootSeed = 2026, mgaAdjust = "holm")
    expect_true(all(res(r, "mga")$asDF$decision == "No"))
})

test_that("the learning data: prediction, fit test and importance-performance map", {
    d <- bundled("plssemlab_learning.csv")
    a <- runPlssem(d, learnFirst(), missing = "listwise", predictRun = TRUE, predictReps = 1,
                   fitTest = TRUE, fitReps = 99, ipma = TRUE, bootSeed = 3)
    expect_true(all(res(a, "predict")$asDF$q2 > 0))
    expect_true(all(is.finite(res(a, "fit")$asDF$p)))
    ip <- res(a, "ipmaTable")$asDF
    expect_true(all(ip$performance > 0 & ip$performance < 100))
})

test_that("the bank data compare women and men", {
    d <- bundled("plssemlab_bank.csv")
    d$gender <- factor(d$gender)
    m <- list(reflective = list(block("TRUST", paste0("trust_", 1:4)), block("LOY", paste0("loy_", 1:3))),
              formative = list(block("QUAL", c("qual_1", "qual_2", "qual_4", "qual_5", "qual_6", "qual_7"))),
              structural = list(srow("TRUST", "QUAL"), srow("LOY", "TRUST", "QUAL")))
    a <- runPlssem(d, m, groupVar = "gender", mgaRun = TRUE, mgaPerm = 99, mgaBoot = 99)
    expect_equal(sort(res(a, "mgaGroups")$asDF$n), c(282L, 393L))
    expect_true(all(is.finite(res(a, "mga")$asDF$pPerm)))
})

test_that("the satisfaction data: moderation SAT x IMAG -> LOY and PLSc", {
    d <- bundled("plssemlab_satisfaction.csv")
    m <- list(reflective = list(block("IMAG", paste0("imag", 1:5)), block("SAT", paste0("sat", 1:4)),
                                block("LOY", paste0("loy", 1:4))), formative = list(),
              structural = list(list(label = "LOY", predictors = list("SAT", "IMAG", c("SAT", "IMAG")))))
    a <- runPlssem(d, m)
    expect_true(any(res(a, "paths")$asDF$from == "SAT × IMAG"))
    m$structural <- list(srow("SAT", "IMAG"), srow("LOY", "SAT", "IMAG"))
    a <- runPlssem(d, m, estimator = "plsc")
    expect_true(all(is.finite(res(a, "paths")$asDF$estimate)))
})
