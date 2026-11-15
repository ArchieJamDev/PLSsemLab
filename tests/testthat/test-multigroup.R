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
# Phase 7: multigroup analysis. MICOM and the path differences equal direct
# cSEM::testMICOM() and cSEM::testMGD() calls on the same groups; the
# comparison by type of service of experiment E6 is reproduced; the cases
# where it cannot run keep their rows with the reason; the equivalent R code
# reproduces the numbers.
# ES: Fase 7: análisis multigrupo. MICOM y las diferencias de rutas son
# iguales a llamadas directas a testMICOM() y testMGD() con los mismos
# grupos; se reproduce la comparación por tipo de servicio del E6; los casos
# en que no puede correr conservan sus filas con la razón; el código R
# equivalente reproduce los números.
# -----------------------------------------------------------------------------

simpleSyntax <- "COMP <~ comp_1 + comp_2 + comp_3
                 LIKE <~ like_1 + like_2 + like_3
                 CUSA <~ cusa
                 CUSL <~ cusl_1 + cusl_2 + cusl_3
                 CUSA ~ COMP + LIKE
                 CUSL ~ COMP + LIKE + CUSA"
simpleModes <- list(COMP = "modeA", LIKE = "modeA", CUSA = "modeA", CUSL = "modeA")
simpleVars <- c(paste0("comp_", 1:3), paste0("like_", 1:3), "cusa", paste0("cusl_", 1:3))

meanReplaced <- function(d, vars) {
    for (v in vars) d[[v]][is.na(d[[v]])] <- mean(d[[v]], na.rm = TRUE)
    d
}

test_that("MICOM and the path differences equal direct cSEM calls", {
    d <- corpRepData()
    d$servicetype <- factor(d$servicetype)
    a <- runPlssem(d, simpleModel(), missing = "mean", groupVar = "servicetype", mgaRun = TRUE,
                   mgaPerm = 99, mgaBoot = 99, bootSeed = 11)
    dd <- meanReplaced(d, simpleVars)
    dd$PLgroup <- ifelse(dd$servicetype == "Contract", "G01", "G02")
    fg <- cSEM::csem(dd[, c(simpleVars, "PLgroup")], simpleSyntax, .id = "PLgroup", .PLS_modes = simpleModes,
                     .disattenuate = FALSE)
    mi <- withDefaultRng(suppressWarnings(cSEM::testMICOM(fg, .R = 99, .seed = 11, .verbose = FALSE)))
    tab <- a$results$mgaSection$micom$asDF
    expect_equal(tab$construct, c("COMP", "LIKE", "CUSA", "CUSL"))
    expect_equal(tab$c, unname(mi$Step2$Test_statistic[[1]]), tolerance = 1e-10)
    expect_equal(tab$pC, unname(mi$Step2$P_value$none[[1]]), tolerance = 1e-10)
    expect_equal(tab$meanDiff, unname(mi$Step3$Mean$Test_statistic[[1]]), tolerance = 1e-10)
    expect_equal(tab$pVar, unname(mi$Step3$Var$P_value$none[[1]]), tolerance = 1e-10)
    mg <- withDefaultRng(suppressWarnings(cSEM::testMGD(fg, .approach_mgd = c("Henseler", "Chin"), .R_permutation = 99,
                                                        .R_bootstrap = 99, .seed = 11, .verbose = FALSE,
                                                        .parameters_to_compare = "CUSA ~ COMP + LIKE\nCUSL ~ COMP + LIKE + CUSA")))
    dt <- a$results$mgaSection$mga$asDF
    expect_equal(paste(dt$from, dt$to), c("COMP CUSA", "LIKE CUSA", "COMP CUSL", "LIKE CUSL", "CUSA CUSL"))
    expect_equal(dt$diff, unname(mg$Chin$Test_statistic[[1]]), tolerance = 1e-10)
    expect_equal(dt$est1 - dt$est2, dt$diff, tolerance = 1e-10)
    expect_equal(dt$pPerm, unname(mg$Chin$P_value$none[[1]]), tolerance = 1e-10)
    expect_equal(dt$pOne, unname(mg$Henseler$P_value$none[[1]]), tolerance = 1e-10)
    expect_equal(dt$pHens, pmin(1, 2 * pmin(dt$pOne, 1 - dt$pOne)), tolerance = 1e-10)
    # two groups: the coefficient columns carry the group names
    # ES: dos grupos: las columnas de coeficientes llevan los nombres
    expect_equal(a$results$mgaSection$mga$getColumn("est1")$title, "Contract")
    expect_equal(a$results$mgaSection$mgaGroups$asDF$n, c(219L, 125L))
    expect_match(a$results$interpretation$content, "Groups compared by servicetype")
    expect_match(a$results$keySummary$content, "Multigroup \\(servicetype\\)")
    # the equivalent R code gives the same groups and differences
    # ES: el código R equivalente da los mismos grupos y diferencias
    env <- new.env()
    env$data <- d
    withDefaultRng(utils::capture.output(suppressWarnings(eval(parse(text = reproCode(a)), envir = env))))
    expect_equal(unname(env$mgd$Chin$Test_statistic[[1]]), dt$diff, tolerance = 1e-10)
    expect_equal(unname(env$mgd$Chin$P_value$none[[1]]), dt$pPerm, tolerance = 1e-10)
})

test_that("the comparison by type of service of experiment E6 is reproduced", {
    skip_on_cran()
    d <- corpRepRaw()
    d[d == -99] <- NA
    d$servicetype <- factor(d$servicetype)
    a <- runPlssem(d, simpleModel(), missing = "mean", groupVar = "servicetype", mgaRun = TRUE,
                   mgaPerm = 499, mgaBoot = 499, bootSeed = 20261110)
    # experiments/design-v1/results/E6_D6_missing_metrics.txt (cSEM 0.7.1)
    tab <- a$results$mgaSection$micom$asDF
    expect_equal(a$results$mgaSection$mgaGroups$asDF$n, c(125L, 219L))
    expect_equal(round(tab$c, 4), c(0.9931, 0.9996, 1, 0.9999))
    expect_equal(round(tab$pC, 4), c(0.1563, 0.6493, 1, 0.7655))
    expect_equal(round(tab$meanDiff, 4), c(0.0575, -0.0541, -0.0967, -0.0597))
    expect_equal(round(tab$pVar, 4), c(0.3567, 0.9078, 0.2124, 0.7275))
    dt <- a$results$mgaSection$mga$asDF
    expect_equal(round(dt$diff, 3), c(0.093, -0.102, 0.184, -0.213, 0.159))
    expect_equal(round(dt$pPerm, 3), c(0.506, 0.412, 0.136, 0.064, 0.084))
    expect_equal(round(dt$pOne, 3), c(0.247, 0.803, 0.052, 0.975, 0.022))
})

test_that("MICOM is not computable with PLSc or ordinal indicators; the differences still are", {
    # PLSc: satisfaction data split in two halves (PLSc is not admissible in
    # one of the service-type groups of the corporate reputation data)
    # ES: PLSc: datos de satisfacción en dos mitades
    s <- cSEM::satisfaction
    s$grp <- factor(rep(c("a", "b"), length.out = nrow(s)))
    m <- list(reflective = list(block("IMAG", paste0("imag", 1:5)), block("SAT", paste0("sat", 1:4)),
                                block("LOY", paste0("loy", 1:4))),
              formative = list(), structural = list(srow("SAT", "IMAG"), srow("LOY", "SAT", "IMAG")))
    a <- runPlssem(s, m, estimator = "plsc", groupVar = "grp", mgaRun = TRUE, mgaPerm = 99, mgaBoot = 99, bootSeed = 3)
    expect_true(all(a$results$mgaSection$micom$asDF$invariance == "Not computable"))
    expect_match(a$results$mgaSection$micom$notes$reason$note, "consistent PLS")
    expect_true(all(is.finite(a$results$mgaSection$mga$asDF$pPerm)))
    expect_match(a$results$interpretation$content, "MICOM was not computed")
    # ordinal indicators
    # ES: indicadores ordinales
    d <- corpRepData()
    d$servicetype <- factor(d$servicetype)
    a <- runPlssem(d, simpleModel(), indicatorScale = "ordinal", groupVar = "servicetype", mgaRun = TRUE,
                   mgaPerm = 99, mgaBoot = 99, bootSeed = 3)
    expect_true(all(a$results$mgaSection$micom$asDF$invariance == "Not computable"))
    expect_match(a$results$mgaSection$micom$notes$reason$note, "ordinal")
    expect_true(all(is.finite(a$results$mgaSection$mga$asDF$pPerm)))
})

test_that("inadmissible estimates in a group stop the comparison, with the group named", {
    d <- corpRepData()
    d$servicetype <- factor(d$servicetype)
    a <- runPlssem(d, simpleModel(), estimator = "plsc", missing = "mean", groupVar = "servicetype", mgaRun = TRUE)
    expect_equal(a$results$mgaSection$mgaGroups$asDF$admissible, c("No", "Yes"))
    expect_true(all(a$results$mgaSection$mga$asDF$decision == "Not computable"))
    expect_match(a$results$mgaSection$mga$notes$reason$note, "not admissible in Contract")
})

test_that("a model that cannot be compared keeps its rows with the reason", {
    d <- corpRepData()
    d$servicetype <- factor(d$servicetype)
    # one group with cases
    # ES: un solo grupo con casos
    one <- d[d$servicetype == "Contract", ]
    a <- runPlssem(one, simpleModel(), missing = "mean", groupVar = "servicetype", mgaRun = TRUE)
    expect_equal(nrow(a$results$mgaSection$mga$asDF), 0)
    expect_match(a$results$mgaSection$mga$notes$reason$note, "fewer than two groups")
    # a group below the minimum
    # ES: un grupo bajo el mínimo
    small <- rbind(d[d$servicetype == "Contract", ], d[d$servicetype == "Prepaid", ][1:6, ])
    a <- runPlssem(small, simpleModel(), missing = "mean", groupVar = "servicetype", mgaRun = TRUE)
    expect_true(all(a$results$mgaSection$mga$asDF$decision == "Not computable"))
    expect_match(a$results$mgaSection$mga$notes$reason$note, "Prepaid \\(n = 6\\)")
    # higher-order constructs
    # ES: constructos de orden superior
    s <- cSEM::satisfaction
    s$grp <- factor(rep(c("a", "b"), length.out = nrow(s)))
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    h <- cls$new(options = opts$new(
        reflective = list(block("SAT", paste0("sat", 1:4)), block("LOY", paste0("loy", 1:4))),
        formative = list(block("IMAG", paste0("imag", 1:5)), block("EXPE", paste0("expe", 1:5))),
        structural = list(list("QUAL"), list("SAT", "QUAL"), list(), list(), list()),
        formativeHoc = list(list(label = "QUAL", components = list("IMAG", "EXPE"))),
        groupVar = "grp", mgaRun = TRUE), data = s)
    h$init(); h$run()
    expect_true(all(h$results$mgaSection$micom$asDF$invariance == "Not computable"))
    expect_match(h$results$mgaSection$mga$notes$reason$note, "higher-order")
    # the per-group table says why, instead of an empty cell
    # ES: la tabla por grupo dice por qué, en lugar de una celda vacía
    expect_equal(h$results$mgaSection$mgaGroups$asDF$admissible, c("Not computable", "Not computable"))
    expect_match(h$results$mgaSection$mgaGroups$notes$reason$note, "higher-order")
    expect_true(all(h$results$mgaSection$mgaGroups$asDF$n > 0))
})

test_that("a bootstrap earlier in the session does not change the permutations", {
    d <- corpRepData()
    d$servicetype <- factor(d$servicetype)
    run <- function() runPlssem(d, simpleModel(), missing = "mean", groupVar = "servicetype", mgaRun = TRUE,
                                mgaPerm = 99, mgaBoot = 99, bootSeed = 5)$results$mgaSection$micom$asDF$pC
    before <- run()
    kind <- RNGkind()
    runPlssem(d, simpleModel(), missing = "mean", bootstrap = TRUE, bootReps = 100)
    expect_identical(RNGkind(), kind)
    # even with L'Ecuyer-CMRG set by someone else
    # ES: incluso con L'Ecuyer-CMRG fijado por otro
    RNGkind("L'Ecuyer-CMRG")
    after <- run()
    expect_equal(RNGkind()[1], "L'Ecuyer-CMRG")
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    expect_identical(after, before)
})

test_that("verdicts, decisions and corrections follow their rules", {
    v <- PLSsemLab:::.pl_micom_verdict(c(0.01, 0.20, 0.20, 0.20), c(0.5, 0.5, 0.01, 0.5), c(0.5, 0.5, 0.5, 0.04))
    expect_equal(v, c("inv_none", "inv_full", "inv_partial", "inv_partial"))
    dec <- PLSsemLab:::.pl_mgd_decision(c(0.01, 0.01, 0.20, 0.20), c(0.01, 0.20, 0.01, 0.20))
    expect_equal(dec, c("mgd_both", "mgd_perm_only", "mgd_hens_only", "mgd_none"))
    tab <- data.frame(p_perm = c(0.01, 0.04), p_hens = c(0.02, 0.03))
    expect_equal(PLSsemLab:::.pl_mgd_adjust(tab, "holm")$p_perm, stats::p.adjust(c(0.01, 0.04), "holm"))
    expect_identical(PLSsemLab:::.pl_mgd_adjust(tab, "none"), tab)
    expect_equal(PLSsemLab:::.pl_mga_levels(factor(c("b", "a", NA), levels = c("b", "a", "c"))), c("b", "a"))
    expect_equal(PLSsemLab:::.pl_mga_reason(list(), "g", "x", c(a = 30, b = 9))$key, "mga_small_group")
    expect_equal(PLSsemLab:::.pl_mga_reason(list(), "x", "x", c(a = 30, b = 30))$key, "mga_group_indicator")
    expect_null(PLSsemLab:::.pl_mga_reason(list(), "g", "x", c(a = 30, b = 30)))
})

test_that("rows are built when jamovi runs .init() with the header of the data only", {
    # jamovi's .init() reads the data with headerOnly = TRUE (no rows): the
    # rows of the groups must still exist when .run() fills them.
    # ES: el .init() de jamovi lee los datos solo con la cabecera.
    d <- corpRepData()
    d$servicetype <- factor(d$servicetype, levels = c("Contract", "Prepaid", "Unused"))
    m <- simpleModel()
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    a <- cls$new(options = opts$new(reflective = m$reflective, formative = m$formative, structural = asRows(m),
                                    missing = "mean", groupVar = "servicetype", mgaRun = TRUE,
                                    mgaPerm = 99, mgaBoot = 99, bootSeed = 2), data = d[0, ])
    a$init()
    # header only: every level of the factor, including the unused one
    # ES: solo cabecera: todos los niveles, incluido el no usado
    expect_equal(a$results$mgaSection$mgaGroups$rowCount, 3)
    a$.__enclos_env__$private$.data <- d
    expect_error(a$run(), NA)
    expect_equal(a$results$mgaSection$mgaGroups$asDF$group, c("Contract", "Prepaid"))
    expect_equal(a$results$mgaSection$mgaGroups$asDF$n, c(219L, 125L))
    expect_equal(nrow(a$results$mgaSection$micom$asDF), 4)
    expect_true(all(is.finite(a$results$mgaSection$mga$asDF$pPerm)))
    expect_equal(a$results$mgaSection$mga$getColumn("est1")$title, "Contract")
})
