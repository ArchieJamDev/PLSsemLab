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

# Necessary condition analysis (1.1.0): the module's own computations (peers,
# effect sizes, accuracy, bottlenecks, permutation test) against their
# definitions, and the jamovi tables. validation/nca_equivalence.R compares
# every value with the NCA package.
# ES: Análisis de condición necesaria (1.1.0): los cálculos propios del
# módulo frente a sus definiciones, y las tablas de jamovi.
# validation/nca_equivalence.R compara cada valor con el paquete NCA.

nca <- function(name) get(name, envir = asNamespace("PLSsemLab"))

test_that("the peers are the upper-left staircase, with ties as the NCA package", {
    peers <- nca(".pl_nca_peers")
    x <- c(1, 1, 2, 2, 3, 4, 4, 5)
    y <- c(1, 3, 2, 3, 5, 4, 5, 5)
    # x = 1 keeps its highest y; (2, 3) repeats the last peer's y: not a
    # peer; (4, 5) and (5, 5) do not exceed 5
    # ES: x = 1 conserva su y más alta; (2, 3) no supera la y del último pico
    expect_equal(unname(peers(x, y)), cbind(c(1, 3), c(3, 5)))
    # a point equal to the last peer is kept as a duplicate
    # ES: un punto igual al último pico se conserva como duplicado
    expect_equal(unname(peers(c(1, 2, 2), c(1, 2, 2))), cbind(c(1, 2, 2), c(1, 2, 2)))
})

test_that("CE-FDH and CR-FDH effect sizes follow their definitions", {
    eff <- nca(".pl_nca_effects")
    # peers (0, 0.5), (0.5, 1): CE ceiling zone = 0.5 * 0.5 = 0.25 of a unit scope
    # ES: zona techo CE = 0.5 * 0.5 = 0.25 de un alcance unitario
    x <- c(0, 0.5, 1, 0.2, 0.8, 1)
    y <- c(0.5, 1, 0, 0.1, 0.3, 0.6)
    e <- eff(x, y, c(0, 1, 0, 1), 1, full = TRUE)
    expect_equal(e$effects[["ce"]], 0.25)
    # CR-FDH: line y = 0.5 + x through both peers, zone above it inside the
    # scope: triangle 0.5 * 0.5 * 0.5
    # ES: CR-FDH: recta y = 0.5 + x, zona sobre ella: triángulo 0.125
    expect_equal(e$slope, 1)
    expect_equal(e$intercept, 0.5)
    expect_equal(e$effects[["cr"]], 0.125)
    expect_equal(e$accuracy, 100)
    # one distinct peer: no ceiling zone
    # ES: un solo pico distinto: sin zona techo
    expect_equal(unname(eff(c(0, 1), c(1, 0), c(0, 1, 0, 1), 1)), c(0, 0))
})

test_that("bottlenecks mark levels that need nothing (NN) and levels beyond the data (NA)", {
    eff <- nca(".pl_nca_effects")
    bn <- nca(".pl_nca_bottleneck")
    x <- c(0, 0.5, 1, 0.2, 0.8, 1)
    y <- c(0.5, 1, 0, 0.1, 0.3, 0.6)
    b <- bn(eff(x, y, c(0, 1, 0, 1), 1, full = TRUE), c(0, 1, 0, 1))
    # CE: up to 50% of y needs nothing; above it, x = 0.5 (50%)
    # ES: CE: hasta el 50% de y no hace falta nada; por encima, x = 0.5
    expect_true(all(is.infinite(b$ce[1:6])))
    expect_equal(b$ce[7:11], rep(50, 5))
    # CR: y = 0.5 + x -> x = y - 0.5
    expect_equal(b$cr[7:11], c(10, 20, 30, 40, 50))
})

test_that("the permutation p value follows the NCA package's rule and seeds", {
    perm <- nca(".pl_nca_permutation")
    eff <- nca(".pl_nca_effects")
    set.seed(5)
    x <- runif(40); y <- pmin(x + runif(40, 0, 0.6), 1)
    sc <- c(range(x), range(y)); ar <- diff(sc[1:2]) * diff(sc[3:4])
    obs <- eff(x, y, sc, ar)
    p <- perm(x, y, sc, ar, obs, 200, offset = 77)
    sims <- t(vapply(1:200, function(i) { set.seed(77 + i); yp <- y[sample(1:40, 40)]; eff(x, yp, sc, ar) }, c(ce = 0, cr = 0)))
    expect_equal(p[["ce"]], max((sum(sims[, "ce"] >= obs[["ce"]] - 1e-10) + 1) / 201, 1 / 200))
    expect_equal(p[["cr"]], max((sum(sims[, "cr"] >= obs[["cr"]] - 1e-10, na.rm = TRUE) + 1) / (sum(!is.na(sims[, "cr"])) + 1), 1 / 200))
})

test_that("the NCA tables fill on corp_rep and the scores are the IPMA's", {
    a <- runPlssem(corpRepData(), simpleModel(), ncaRun = TRUE, ncaPerm = 199, ipmaRange = "manual",
                   ipmaMin = 1, ipmaMax = 7, bootSeed = 11)
    s <- a$results$ncaSection
    tab <- s$ncaTable$asDF
    expect_equal(nrow(tab), 2 * 5)
    expect_true(all(is.finite(tab$d)) && all(is.finite(tab$p)))
    expect_true(all(tab$accuracy[tab$ceiling == "CE-FDH"] == 100))
    # CUSA is the one necessary condition for CUSL (d about 0.23)
    # ES: CUSA es la única condición necesaria para CUSL
    k <- tab$target == "CUSL" & tab$condition == "CUSA" & tab$ceiling == "CR-FDH"
    expect_equal(tab$verdict[k], "Necessary condition")
    expect_gt(tab$d[k], 0.2)
    expect_equal(sum(tab$verdict == "Necessary condition"), 2)
    expect_equal(nrow(s$ncaCombined$asDF), 5)
    expect_true("boot" %in% names(s$ncaCombined$notes))
    expect_equal(length(s$ncaBottlenecks$items), 2)
    expect_equal(nrow(s$ncaBottlenecks$items[[2]]$asDF), 11)
    expect_equal(length(s$ncaPlots$items), 5)
    # mean of the 0-100 scores = IPMA performance
    # ES: media de las puntuaciones 0-100 = desempeño del IPMA
    b <- runPlssem(corpRepData(), simpleModel(), ipma = TRUE, ipmaRange = "manual", ipmaMin = 1, ipmaMax = 7)
    ip <- b$results$ipmaSection$ipmaTable$asDF
    st <- s$ncaPlots$items[[1]]$state
    expect_equal(mean(st$x), ip$performance[ip$target == "CUSA" & ip$construct == "COMP"], tolerance = 1e-10)
    # same seed, same p values (the cache is per instance; a new run recomputes)
    # ES: misma semilla, mismos valores p
    a2 <- runPlssem(corpRepData(), simpleModel(), ncaRun = TRUE, ncaPerm = 199, ipmaRange = "manual",
                    ipmaMin = 1, ipmaMax = 7, bootSeed = 11)
    expect_equal(a2$results$ncaSection$ncaTable$asDF$p, tab$p)
})

test_that("the NCA combined reading uses the bootstrap and the chosen ceiling", {
    a <- runPlssem(corpRepData(), simpleModel(), ncaRun = TRUE, ncaPerm = 199, bootstrap = TRUE, bootReps = 100,
                   bootSeed = 3, ncaCeiling = "ce_fdh")
    cb <- a$results$ncaSection$ncaCombined$asDF
    expect_equal(cb$reading[cb$target == "CUSL" & cb$condition == "CUSA"], "Must-have and should-have")
    expect_equal(cb$reading[cb$target == "CUSL" & cb$condition == "COMP"], "Neither")
    tab <- a$results$ncaSection$ncaTable$asDF
    expect_equal(cb$d[cb$condition == "CUSA"], tab$d[tab$condition == "CUSA" & tab$ceiling == "CE-FDH"])
    html <- gsub("<[^>]+>", " ", a$results$interpretation$content)
    expect_match(html, "Necessary condition analysis")
    expect_match(html, "Must-have and should-have: CUSA")
})

test_that("NCA with consistent PLS or an interaction stays as not computable", {
    a <- runPlssem(corpRepData(), simpleModel(), ncaRun = TRUE, ncaPerm = 99, estimator = "plsc")
    expect_true(all(a$results$ncaSection$ncaTable$asDF$verdict == "Not computable"))
    expect_true(all(a$results$ncaSection$ncaCombined$asDF$reading == "Not computable"))
})

test_that("the NCA restores the session's random number generator", {
    RNGkind("L'Ecuyer-CMRG"); set.seed(1)
    on.exit(RNGkind("default", "default", "default"), add = TRUE)
    before <- .Random.seed
    runPlssem(corpRepData(), simpleModel(), ncaRun = TRUE, ncaPerm = 99)
    expect_equal(RNGkind()[1], "L'Ecuyer-CMRG")
    expect_identical(.Random.seed, before)
})
