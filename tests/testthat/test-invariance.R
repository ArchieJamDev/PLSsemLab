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

# Measurement invariance of the reflective constructs as common factors
# (1.1.0): the MGCFA table equals direct lavaan calls (the equivalent R code
# of the report), and the decisions follow Chen's (2007) limits.
# ES: Invarianza de medida de los constructos reflectivos como factores
# comunes (1.1.0): la tabla del AFCMG es igual a llamadas directas a lavaan.

test_that("the MGCFA table equals lavaan on corp_rep by service type", {
    a <- runPlssem(corpRepData(), simpleModel(), groupVar = "servicetype", mgaRun = TRUE, mgcfa = TRUE,
                   mgaPerm = 99, mgaBoot = 99)
    tab <- a$results$mgaSection$mgcfa$asDF
    expect_equal(nrow(tab), 3)
    env <- new.env(); env$data <- corpRepData()
    utils::capture.output(withDefaultRng(eval(parse(text = reproCode(a)), envir = env)))
    fm <- sapply(env$mi, lavaan::fitMeasures, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "rmsea.robust", "srmr"))
    expect_equal(tab$chisq, unname(fm["chisq.scaled", ]), tolerance = 1e-10)
    expect_equal(tab$df, unname(fm["df.scaled", ]))
    expect_equal(tab$cfi, unname(fm["cfi.robust", ]), tolerance = 1e-10)
    expect_equal(tab$rmsea, unname(fm["rmsea.robust", ]), tolerance = 1e-10)
    expect_equal(tab$srmr, unname(fm["srmr", ]), tolerance = 1e-10)
    lrt <- lavaan::lavTestLRT(env$mi$configural, env$mi$metric)
    expect_equal(tab$dchisq[2], lrt[["Chisq diff"]][2], tolerance = 1e-10)
    expect_equal(tab$dcfi[3], tab$cfi[3] - tab$cfi[2], tolerance = 1e-12)
    # CUSA has one indicator: left out, and the note says so
    # ES: CUSA tiene un indicador: queda fuera y la nota lo dice
    expect_true("left" %in% names(a$results$mgaSection$mgcfa$notes))
    expect_match(gsub("<[^>]+>", " ", a$results$interpretation$content), "Multigroup CFA of the reflective constructs")
})

test_that("MGCFA decisions follow the configural fit and Chen's limits", {
    dec <- get(".pl_mgcfa_decision", envir = asNamespace("PLSsemLab"))
    t <- data.frame(cfi = c(0.96, 0.955, 0.94), rmsea = c(0.05, 0.05, 0.06), srmr = c(0.04, 0.05, 0.055),
                    dcfi = c(NA, -0.005, -0.015), drmsea = c(NA, 0, 0.01), dsrmr = c(NA, 0.01, 0.005))
    expect_equal(dec(t), c("mgcfa_good", "mgcfa_supported", "mgcfa_not_supported"))
    t$cfi[1] <- 0.85
    expect_equal(dec(t), c("mgcfa_poor", "mgcfa_not_judged", "mgcfa_not_judged"))
    t$cfi[1] <- 0.92
    t$srmr[1] <- 0.09
    expect_equal(dec(t)[1], "mgcfa_acceptable")
})

test_that("MGCFA is not computable without reflective constructs of two or more indicators, or with ordinal data", {
    rs <- get(".pl_mgcfa_reason", envir = asNamespace("PLSsemLab"))
    sy <- get(".pl_mgcfa_syntax", envir = asNamespace("PLSsemLab"))
    b <- list(list(label = "A", vars = c("a1", "a2"), type = "reflective"),
              list(label = "B", vars = "b1", type = "reflective"),
              list(label = "F", vars = c("f1", "f2"), type = "formative"))
    cm <- list(A = "C1", B = "C2", F = "C3")
    vm <- list(a1 = "V1", a2 = "V2", b1 = "V3", f1 = "V4", f2 = "V5")
    expect_equal(sy(b, cm, vm), "C1 =~ V1 + V2")
    expect_null(sy(b[2:3], cm, vm))
    expect_equal(rs(NULL, "continuous"), "mgcfa_none")
    expect_equal(rs("C1 =~ V1 + V2", "ordinal"), "mgcfa_ordinal")
    expect_null(rs("C1 =~ V1 + V2", "continuous"))
})
