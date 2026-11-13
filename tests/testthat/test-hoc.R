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
# Phase 4: higher-order constructs (model A of experiment E2, satisfaction
# data: QUAL formed by IMAG and EXPE). The disjoint two-stage estimates
# equal two direct cSEM calls; the bootstrap resamples both stages together;
# the equivalent R code reproduces the estimates.
# ES: Fase 4: constructos de orden superior (modelo A del experimento E2).
# Las estimaciones en dos etapas son iguales a dos llamadas directas a cSEM;
# el bootstrap remuestrea ambas etapas juntas; el código R equivalente
# reproduce las estimaciones.
# -----------------------------------------------------------------------------

hocData <- function() cSEM::satisfaction

runHoc <- function(type = "reflective", ...) {
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    hoc <- list(list(label = "QUAL", components = list("IMAG", "EXPE")))
    args <- list(reflective = list(block("SAT", paste0("sat", 1:4)), block("LOY", paste0("loy", 1:4))),
                 formative = list(block("IMAG", paste0("imag", 1:5)), block("EXPE", paste0("expe", 1:5))),
                 structural = list(list("QUAL"), list("SAT", "QUAL"), list(), list(), list()), ...)
    args[[paste0(type, "Hoc")]] <- hoc
    a <- cls$new(options = do.call(opts$new, args), data = hocData())
    a$init()
    a$run()
    a
}

# Stage 1: IMAG and EXPE take QUAL's paths; stage 2: scores as indicators.
# ES: Etapa 1: IMAG y EXPE toman las rutas de QUAL; etapa 2: puntuaciones.
directTwoStage <- function(mode) {
    d <- hocData()
    m1 <- "SAT <~ sat1 + sat2 + sat3 + sat4
           LOY <~ loy1 + loy2 + loy3 + loy4
           IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
           EXPE <~ expe1 + expe2 + expe3 + expe4 + expe5
           SAT ~ IMAG + EXPE
           LOY ~ SAT + IMAG + EXPE"
    f1 <- cSEM::csem(d, m1, .PLS_modes = list(SAT = "modeA", LOY = "modeA", IMAG = "modeB", EXPE = "modeB"),
                     .disattenuate = FALSE)
    s <- as.data.frame(f1$Estimates$Construct_scores)
    names(s) <- paste0("s_", names(s))
    m2 <- "QUAL <~ s_IMAG + s_EXPE
           SAT <~ s_SAT
           LOY <~ s_LOY
           SAT ~ QUAL
           LOY ~ SAT + QUAL"
    f2 <- cSEM::csem(s, m2, .PLS_modes = list(QUAL = mode, SAT = "modeA", LOY = "modeA"), .disattenuate = FALSE)
    list(f1 = f1, f2 = f2)
}

test_that("a reflective higher-order construct equals two direct cSEM stages (Mode A)", {
    a <- runHoc("reflective")
    ref <- directTwoStage("modeA")
    p <- a$results$paths$asDF
    B <- ref$f2$Estimates$Path_estimates
    expect_equal(p$estimate[p$from == "QUAL" & p$to == "SAT"], B["SAT", "QUAL"], tolerance = 1e-10)
    expect_equal(p$estimate[p$from == "QUAL" & p$to == "LOY"], B["LOY", "QUAL"], tolerance = 1e-10)
    h <- a$results$hocMeasurement$asDF
    expect_equal(h$loading, unname(ref$f2$Estimates$Loading_estimates["QUAL", c("s_IMAG", "s_EXPE")]), tolerance = 1e-10)
    expect_equal(h$weight, unname(ref$f2$Estimates$Weight_estimates["QUAL", c("s_IMAG", "s_EXPE")]), tolerance = 1e-10)
    r <- a$results$hocReliability$asDF
    expect_equal(r$rhoC, unname(cSEM::calculateRhoC(ref$f2, .only_common_factors = FALSE)["QUAL"]), tolerance = 1e-10)
    expect_equal(r$ave, unname(cSEM::calculateAVE(ref$f2, .only_common_factors = FALSE)["QUAL"]), tolerance = 1e-10)
    expect_equal(a$results$r2$asDF$r2, unname(ref$f2$Estimates$R2[c("SAT", "LOY")]), tolerance = 1e-10)
    # first-order measurement comes from stage 1
    # ES: la medida de primer orden sale de la etapa 1
    m <- a$results$measurement$asDF
    expect_equal(m$weight[m$construct == "IMAG"], unname(ref$f1$Estimates$Weight_estimates["IMAG", paste0("imag", 1:5)]),
                 tolerance = 1e-10)
    expect_match(a$results$model$asDF$role[a$results$model$asDF$construct == "IMAG"], "Dimension of QUAL")
    expect_true(any(grepl("two-stage", noticesOf(a))))
})

test_that("a formative higher-order construct uses Mode B weights with VIF", {
    a <- runHoc("formative")
    ref <- directTwoStage("modeB")
    h <- a$results$hocMeasurement$asDF
    expect_equal(h$weight, unname(ref$f2$Estimates$Weight_estimates["QUAL", c("s_IMAG", "s_EXPE")]), tolerance = 1e-10)
    S <- ref$f2$Estimates$Indicator_VCV[c("s_IMAG", "s_EXPE"), c("s_IMAG", "s_EXPE")]
    expect_equal(h$vif, unname(diag(solve(S))), tolerance = 1e-10)
    expect_false(a$results$hocReliability$visible)
})

test_that("the bootstrap resamples both stages and the R code reproduces the model", {
    a <- runHoc("reflective", bootstrap = TRUE, bootReps = 120, bootSeed = 3)
    ref <- directTwoStage("modeA")
    fun <- function(.object) {
        s <- as.data.frame(.object$Estimates$Construct_scores)
        names(s) <- paste0("s_", names(s))
        f2 <- cSEM::csem(s, "QUAL <~ s_IMAG + s_EXPE
                             SAT <~ s_SAT
                             LOY <~ s_LOY
                             SAT ~ QUAL
                             LOY ~ SAT + QUAL", .PLS_modes = list(QUAL = "modeA", SAT = "modeA", LOY = "modeA"),
                         .disattenuate = FALSE)
        f2$Estimates$Path_estimates["LOY", "QUAL"]
    }
    b <- cSEM::resamplecSEMResults(ref$f1, .R = 120, .seed = 3, .user_funs = list(QL = fun),
                                   .handle_inadmissibles = "drop")
    ci <- cSEM::infer(b, .quantity = "CI_percentile", .alpha = 0.05)$QL$CI_percentile
    p <- a$results$paths$asDF
    row <- p$from == "QUAL" & p$to == "LOY"
    expect_equal(c(p$lower[row], p$upper[row]), unname(ci[, 1]), tolerance = 1e-10)
    expect_true(all(is.finite(a$results$hocMeasurement$asDF$loadLower)))
    expect_true(all(is.finite(a$results$hocReliability$asDF$rhoCLower)))
    expect_true(all(is.finite(a$results$effects$asDF$totLower)))
    env <- new.env()
    env$data <- hocData()
    utils::capture.output(eval(parse(text = reproCode(a)), envir = env))
    expect_equal(env$fit2$Estimates$Path_estimates["C2", "H1"], p$estimate[row], tolerance = 1e-10)
    dg <- a$results$pathDiagram$state
    expect_equal(sum(dg$pars$kind == "hoc"), 2)
    expect_true(any(grepl("^QUAL", dg$vars$name)))
})

test_that("higher-order mistakes are rejected with the reason", {
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    base <- list(reflective = list(block("SAT", paste0("sat", 1:4)), block("LOY", paste0("loy", 1:4))),
                 formative = list(block("IMAG", paste0("imag", 1:5)), block("EXPE", paste0("expe", 1:5))))
    run <- function(...) {
        a <- cls$new(options = do.call(opts$new, c(base, list(...))), data = hocData())
        a$init()
        a$run()
    }
    expect_error(run(reflectiveHoc = list(list(label = "QUAL", components = list("IMAG"))),
                     structural = list(list("QUAL"), list("SAT"), list(), list(), list())), "at least two")
    expect_error(run(reflectiveHoc = list(list(label = "QUAL", components = list("IMAG", "EXPE"))),
                     structural = list(list("QUAL", "IMAG"), list("SAT"), list(), list(), list())),
                 "'IMAG' is a dimension")
    expect_error(run(reflectiveHoc = list(list(label = "QUAL", components = list("IMAG", "EXPE"))),
                     structural = list(list("QUAL"), list("SAT"), list(), list(), list()), estimator = "plsc"),
                 "consistent PLS with higher-order")
    expect_error(run(reflectiveHoc = list(list(label = "Q1", components = list("IMAG", "EXPE")),
                                          list(label = "Q2", components = list("IMAG", "SAT"))),
                     structural = list(list("Q1"), list("Q2"), list(), list(), list(), list())), "two higher-order")
})

test_that("cSEM's second-order routine, as an option, equals cSEM's own estimates", {
    a <- runHoc("reflective", hocApproach = "csem", bootstrap = TRUE, bootReps = 100, bootSeed = 2)
    d <- hocData()
    m <- "SAT <~ sat1 + sat2 + sat3 + sat4
          LOY <~ loy1 + loy2 + loy3 + loy4
          IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
          EXPE <~ expe1 + expe2 + expe3 + expe4 + expe5
          QUAL =~ IMAG + EXPE
          SAT ~ QUAL
          LOY ~ SAT + QUAL"
    f <- suppressWarnings(cSEM::csem(d, m, .PLS_modes = list(SAT = "modeA", LOY = "modeA", IMAG = "modeB", EXPE = "modeB")))
    B <- f$Second_stage$Estimates$Path_estimates
    p <- a$results$paths$asDF
    expect_equal(p$estimate[p$from == "QUAL" & p$to == "SAT"], B["SAT_temp", "QUAL"], tolerance = 1e-8)
    expect_equal(p$estimate[p$from == "QUAL" & p$to == "LOY"], B["LOY_temp", "QUAL"], tolerance = 1e-8)
    expect_equal(a$results$hocMeasurement$asDF$loading,
                 unname(f$Second_stage$Estimates$Loading_estimates["QUAL", c("IMAG", "EXPE")]), tolerance = 1e-8)
    m1 <- a$results$measurement$asDF
    expect_equal(m1$weight[m1$construct == "IMAG"],
                 unname(f$First_stage$Estimates$Weight_estimates["IMAG", paste0("imag", 1:5)]), tolerance = 1e-8)
    expect_true(all(is.finite(p$lower)))
    expect_true(any(grepl("second-order routine", noticesOf(a))))
    # the disjoint approach differs (no paths among the dimensions in stage 1)
    # ES: el enfoque disjunto difiere
    b <- runHoc("reflective")
    pb <- b$results$paths$asDF
    expect_gt(abs(pb$estimate[pb$from == "QUAL" & pb$to == "SAT"] - p$estimate[p$from == "QUAL" & p$to == "SAT"]), 1e-4)
})
