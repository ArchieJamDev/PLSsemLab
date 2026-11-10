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
# Every number the module reports equals a direct cSEM call, for PLS and
# PLSc, and the reference model gives the path coefficients and R² reported
# for it in the design experiments (E1, mean replacement).
# ES: Cada número que informa el módulo es igual a una llamada directa a
# cSEM, para PLS y PLSc, y el modelo de referencia da las rutas y R² de los
# experimentos de diseño.
# -----------------------------------------------------------------------------

directFit <- function(data, syntax, estimator) {
    d <- data
    for (v in names(d)) if (is.numeric(d[[v]])) d[[v]][is.na(d[[v]])] <- mean(d[[v]], na.rm = TRUE)
    cSEM::csem(d, syntax, .disattenuate = estimator == "plsc")
}

pathTable <- function(a) {
    p <- a$results$paths$asDF
    stats::setNames(p$estimate, paste(p$from, p$to, sep = "->"))
}

test_that("PLS estimates of the extended model equal a direct cSEM call", {
    data <- corpRepData()
    a <- runPlssem(data, extendedModel(), missing = "mean")
    syntax <- "
      QUAL <~ qual_1 + qual_2 + qual_3 + qual_4 + qual_5 + qual_6 + qual_7 + qual_8
      PERF <~ perf_1 + perf_2 + perf_3 + perf_4 + perf_5
      CSOR <~ csor_1 + csor_2 + csor_3 + csor_4 + csor_5
      ATTR <~ attr_1 + attr_2 + attr_3
      COMP =~ comp_1 + comp_2 + comp_3
      LIKE =~ like_1 + like_2 + like_3
      CUSA =~ cusa
      CUSL =~ cusl_1 + cusl_2 + cusl_3
      COMP ~ QUAL + PERF + CSOR + ATTR
      LIKE ~ QUAL + PERF + CSOR + ATTR
      CUSA ~ COMP + LIKE
      CUSL ~ COMP + LIKE + CUSA"
    ref <- directFit(data[, -c(1, 2)], syntax, "pls")$Estimates
    got <- pathTable(a)
    for (nm in names(got)) {
        ft <- strsplit(nm, "->", fixed = TRUE)[[1]]
        expect_equal(unname(got[nm]), ref$Path_estimates[ft[2], ft[1]], tolerance = 1e-10, info = nm)
    }
    m <- a$results$measurement$asDF
    for (k in seq_len(nrow(m))) {
        expect_equal(m$loading[k], ref$Loading_estimates[m$construct[k], m$indicator[k]], tolerance = 1e-10)
        expect_equal(m$weight[k], ref$Weight_estimates[m$construct[k], m$indicator[k]], tolerance = 1e-10)
    }
    r2 <- a$results$r2$asDF
    expect_equal(r2$r2, unname(ref$R2[r2$construct]), tolerance = 1e-10)

    # Values of the design experiments (E1/E3), rounded to 3 decimals.
    # ES: Valores de los experimentos de diseño, redondeados a 3 decimales.
    expect_equal(round(unname(got[c("QUAL->COMP", "LIKE->CUSA", "CUSA->CUSL")]), 3), c(0.430, 0.436, 0.505))
    expect_equal(round(r2$r2, 3), c(0.631, 0.558, 0.292, 0.562))
})

test_that("PLSc estimates of the simple model equal a direct cSEM call", {
    data <- corpRepData()
    a <- runPlssem(data, simpleModel(), missing = "mean", estimator = "plsc")
    syntax <- "
      COMP =~ comp_1 + comp_2 + comp_3
      LIKE =~ like_1 + like_2 + like_3
      CUSA =~ cusa
      CUSL =~ cusl_1 + cusl_2 + cusl_3
      CUSA ~ COMP + LIKE
      CUSL ~ COMP + LIKE + CUSA"
    ref <- directFit(data[, -c(1, 2)], syntax, "plsc")$Estimates
    got <- pathTable(a)
    for (nm in names(got)) {
        ft <- strsplit(nm, "->", fixed = TRUE)[[1]]
        expect_equal(unname(got[nm]), ref$Path_estimates[ft[2], ft[1]], tolerance = 1e-10, info = nm)
    }
})

test_that("listwise deletion is the default and is reported", {
    a <- runPlssem(corpRepData(), simpleModel())
    expect_true(any(grepl("8 case\\(s\\) with missing values were excluded; 336 case\\(s\\) remain", noticesOf(a))))
})

test_that("the report blocks are filled and the reproducibility code runs", {
    data <- corpRepData()
    a <- runPlssem(data, simpleModel(), showGuides = TRUE)
    for (name in c("keySummary", "choicesGuide", "modelGuide", "measurementGuide", "pathsGuide",
                   "interpretation", "reproducibility"))
        expect_true(nzchar(a$results[[name]]$content), info = name)
    expect_false(a$results$intro$visible)

    # The equivalent R code reproduces the module's path coefficients.
    # ES: El código R equivalente reproduce las rutas del módulo.
    env <- new.env()
    env$data <- data
    eval(parse(text = a$results$reproducibilityCode$content), envir = env)
    p <- env$fit$Estimates$Path_estimates
    expect_equal(sort(unname(p[p != 0])), sort(a$results$paths$asDF$estimate), tolerance = 1e-10)
})
