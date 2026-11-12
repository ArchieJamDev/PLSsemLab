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
# Phase 4: two-way interactions (model B of experiment E2, satisfaction
# data). Estimates, f², structural VIF and intervals equal direct cSEM
# calls; the simple effects and the floodlight bounds equal
# cSEM::doNonlinearEffectsAnalysis() with the same draws.
# ES: Fase 4: interacciones de dos vías (modelo B del experimento E2). Las
# estimaciones, f², VIF estructural e intervalos son iguales a cSEM; los
# efectos simples y los límites floodlight son iguales a
# doNonlinearEffectsAnalysis() con las mismas réplicas.
# -----------------------------------------------------------------------------

satData <- function() cSEM::satisfaction[, c(paste0("imag", 1:5), paste0("sat", 1:4), paste0("loy", 1:4))]

interactionModel <- function(predictors = list("SAT", "IMAG", c("SAT", "IMAG"))) list(
    reflective = list(block("IMAG", paste0("imag", 1:5)), block("SAT", paste0("sat", 1:4)),
                      block("LOY", paste0("loy", 1:4))),
    formative = list(),
    structural = list(list(label = "LOY", predictors = predictors)))

directFit <- function(d) {
    m <- "C1 <~ imag1 + imag2 + imag3 + imag4 + imag5
          C2 <~ sat1 + sat2 + sat3 + sat4
          C3 <~ loy1 + loy2 + loy3 + loy4
          C3 ~ C2 + C1 + C2.C1"
    cSEM::csem(d, m, .PLS_modes = list(C1 = "modeA", C2 = "modeA", C3 = "modeA"), .disattenuate = FALSE)
}

test_that("an interaction without its main effects is rejected with the reason", {
    expect_error(runPlssem(satData(), interactionModel(list("SAT", c("SAT", "IMAG")))), "IMAG is not also a predictor")
    m <- interactionModel()
    m$structural <- list(list(label = "LOY", predictors = list("SAT", "IMAG")),
                         list(label = "SAT", predictors = list("IMAG", "LOY", c("SAT", "IMAG"))))
    expect_error(runPlssem(satData(), m), "one of its own components")
})

test_that("estimates, f² and structural VIF equal cSEM with an interaction", {
    d <- satData()
    a <- runPlssem(d, interactionModel())
    f <- directFit(d)
    p <- a$results$paths$asDF
    term <- paste0("SAT", " × ", "IMAG")
    expect_equal(p$estimate[p$from == term], f$Estimates$Path_estimates["C3", "C2.C1"], tolerance = 1e-10)
    expect_equal(p$estimate[p$from == "SAT"], f$Estimates$Path_estimates["C3", "C2"], tolerance = 1e-10)
    f2 <- a$results$f2$asDF
    expect_equal(f2$f2[f2$from == term], cSEM::calculatef2(f)["C3", "C2.C1"], tolerance = 1e-10)
    expect_match(f2$assessment[f2$from == term], "^Interaction")
    v <- a$results$vifStruct$asDF
    expect_equal(v$vif[v$from == term], unname(f$Estimates$VIF$C3["C2.C1"]), tolerance = 1e-10)
    expect_equal(a$results$r2$asDF$r2, unname(f$Estimates$R2), tolerance = 1e-10)
    expect_true(all(is.finite(a$results$reliability$asDF$rhoC)))
    expect_true(any(grepl("computed from the loadings", noticesOf(a))))
    expect_equal(nrow(a$results$simpleSlopes$asDF), 3)
})

test_that("rho_C from the loadings equals cSEM's in a linear model", {
    f <- cSEM::csem(satData(), "C1 <~ imag1 + imag2 + imag3 + imag4 + imag5
                                 C2 <~ sat1 + sat2 + sat3 + sat4
                                 C2 ~ C1", .PLS_modes = list(C1 = "modeA", C2 = "modeA"), .disattenuate = FALSE)
    own <- get(".pl_rhoC_loadings", envir = asNamespace("PLSsemLab"))(f, c("C1", "C2"))
    expect_equal(unname(own), unname(cSEM::calculateRhoC(f, .only_common_factors = FALSE)[c("C1", "C2")]),
                 tolerance = 1e-12)
})

test_that("simple effects and floodlight equal doNonlinearEffectsAnalysis()", {
    d <- satData()
    a <- runPlssem(d, interactionModel(), bootstrap = TRUE, bootReps = 150, bootSeed = 7)
    b <- cSEM::resamplecSEMResults(directFit(d), .R = 150, .seed = 7, .handle_inadmissibles = "drop")
    p <- a$results$paths$asDF
    s <- cSEM::summarize(b, .ci = "CI_percentile")$Estimates$Path_estimates
    term <- "SAT × IMAG"
    expect_equal(p$upper[p$from == term], s[["CI_percentile.95%U"]][s$Name == "C3 ~ C2.C1"], tolerance = 1e-10)
    nl <- cSEM::doNonlinearEffectsAnalysis(b, .dependent = "C3", .independent = "C2", .moderator = "C1")
    fl <- as.data.frame(nl$out_floodlight$out)
    # the module's own slopes at cSEM's grid, from the same draws
    # ES: las pendientes del módulo en la rejilla de cSEM, de las mismas réplicas
    st <- a$results$moderationPlots$items[[1]]$state
    expect_equal(range(st$flood$value), range(fl$value_z), tolerance = 1e-10)
    ss <- a$results$simpleSlopes$asDF
    pe <- f <- directFit(d)$Estimates$Path_estimates
    expect_equal(ss$slope, pe["C3", "C2"] + pe["C3", "C2.C1"] * c(-1, 0, 1), tolerance = 1e-10)
    dr <- b$Estimates$Estimates_resample$Estimates1$Path_estimates$Resampled
    colnames(dr) <- names(b$Estimates$Estimates_resample$Estimates1$Path_estimates$Original)
    for (z in fl$value_z[c(1, 50, 100)]) {
        v <- dr[, "C3 ~ C2"] + dr[, "C3 ~ C2.C1"] * z
        k <- which.min(abs(fl$value_z - z))
        expect_equal(unname(stats::quantile(v, c(0.025, 0.975))), c(fl$lb[k], fl$ub[k]), tolerance = 1e-10)
    }
    expect_equal(ss$lower[2], p$lower[p$from == "SAT"], tolerance = 1e-10)
    expect_match(a$results$interpretation$content, "no evidence that IMAG moderates")
    dg <- a$results$pathDiagram$state
    expect_true(term %in% dg$vars$name)
    expect_equal(sum(dg$pars$kind == "path"), 3)
})
