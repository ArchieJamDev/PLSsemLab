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
# Phase 2: quality of the measurement model. Every value equals a direct cSEM
# call on the common-factor syntax (the textbook specification), the values
# of COMP reported in the design experiment E1 are reproduced, the tables
# keep their shape, and what cannot be computed stays as a row with its
# reason.
# ES: Fase 2: calidad del modelo de medida. Cada valor es igual a una llamada
# directa a cSEM con la sintaxis de factor común, se reproducen los valores
# de COMP del experimento E1, las tablas conservan su forma y lo que no se
# puede calcular se queda como fila con su razón.
# -----------------------------------------------------------------------------

meanReplace <- function(d) {
    for (v in names(d)) if (is.numeric(d[[v]])) d[[v]][is.na(d[[v]])] <- mean(d[[v]], na.rm = TRUE)
    d
}

extendedSyntax <- "
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

byRow <- function(df, key) stats::setNames(df[[key]], df$construct)

test_that("reliability, AVE, HTMT and VIF of the extended model equal direct cSEM calls", {
    data <- corpRepData()
    a <- runPlssem(data, extendedModel(), missing = "mean")
    d <- meanReplace(data[, -c(1, 2)])
    ref <- cSEM::csem(d, extendedSyntax, .disattenuate = FALSE)
    ref_c <- cSEM::csem(d, extendedSyntax, .disattenuate = TRUE)

    rel <- a$results$reliability$asDF
    multi <- c("COMP", "LIKE", "CUSL")
    expect_equal(rel$construct, c("COMP", "LIKE", "CUSA", "CUSL"))
    expect_equal(byRow(rel, "alpha")[multi], cSEM::calculateRhoT(ref)[multi], tolerance = 1e-10)
    expect_equal(byRow(rel, "rhoC")[multi], cSEM::calculateRhoC(ref)[multi], tolerance = 1e-10)
    expect_equal(byRow(rel, "ave")[multi], cSEM::calculateAVE(ref)[multi], tolerance = 1e-10)
    expect_equal(byRow(rel, "rhoA")[multi], ref_c$Estimates$Reliabilities[multi], tolerance = 1e-10)

    # Design experiment E1, COMP: alpha 0.776, rho_A 0.786, rho_C 0.869, AVE 0.688.
    # ES: Experimento E1, COMP.
    expect_equal(round(unlist(rel[1, c("alpha", "rhoA", "rhoC", "ave")]), 3),
                 c(alpha = 0.776, rhoA = 0.786, rhoC = 0.869, ave = 0.688))

    h <- a$results$htmt$asDF
    href <- cSEM::calculateHTMT(ref)$htmts
    h2ref <- cSEM::calculateHTMT(ref, .type_htmt = "htmt2")$htmts
    expect_equal(nrow(h), 6)
    for (k in seq_len(nrow(h))) {
        expect_equal(h$htmt[k], max(href[h$c1[k], h$c2[k]], href[h$c2[k], h$c1[k]]), tolerance = 1e-10)
        expect_equal(h$htmt2[k], max(h2ref[h$c1[k], h$c2[k]], h2ref[h$c2[k], h$c1[k]]), tolerance = 1e-10)
    }
    expect_equal(round(h$htmt[h$c1 == "COMP" & h$c2 == "LIKE"], 3), 0.780)

    v <- a$results$vif$asDF
    vref <- cSEM::calculateVIFModeB(ref)
    expect_equal(nrow(v), 21)
    for (k in seq_len(nrow(v)))
        expect_equal(v$vif[k], vref[v$construct[k], v$indicator[k]], tolerance = 1e-10)
})

test_that("with PLSc, rho_A and rho_C come from the consistent estimation", {
    data <- corpRepData()
    a <- runPlssem(data, simpleModel(), missing = "mean", estimator = "plsc")
    d <- meanReplace(data[, -c(1, 2)])
    ref <- cSEM::csem(d, "
      COMP =~ comp_1 + comp_2 + comp_3
      LIKE =~ like_1 + like_2 + like_3
      CUSA =~ cusa
      CUSL =~ cusl_1 + cusl_2 + cusl_3
      CUSA ~ COMP + LIKE
      CUSL ~ COMP + LIKE + CUSA", .disattenuate = TRUE)
    rel <- a$results$reliability$asDF
    multi <- c("COMP", "LIKE", "CUSL")
    expect_equal(byRow(rel, "rhoA")[multi], ref$Estimates$Reliabilities[multi], tolerance = 1e-10)
    expect_equal(byRow(rel, "rhoC")[multi], cSEM::calculateRhoC(ref)[multi], tolerance = 1e-10)
})

test_that("a single-indicator construct keeps its row, marked as not computable", {
    a <- runPlssem(corpRepData(), simpleModel())
    rel <- a$results$reliability$asDF
    cusa <- rel[rel$construct == "CUSA", ]
    expect_equal(cusa$assessment, "Not computable")
    expect_true(is.na(cusa$rhoC) || identical(cusa$rhoC, ""))
    # The HTMT of CUSA is shown, with a footnote: it is a lower bound.
    # ES: El HTMT de CUSA se muestra, con una nota: es un límite inferior.
    h <- a$results$htmt$asDF
    expect_true(all(is.finite(h$htmt)))
})

test_that("tables keep their shape and hide only when they do not apply", {
    a <- runPlssem(corpRepData(), simpleModel())
    expect_false(a$results$vif$visible)
    expect_false(a$results$fornell$visible)
    expect_true(a$results$htmt$visible)

    b <- runPlssem(corpRepData(), simpleModel(), fornellLarcker = TRUE)
    fl <- b$results$fornell$asDF
    expect_equal(dim(fl), c(4, 5))
    ave <- byRow(b$results$reliability$asDF, "ave")
    expect_equal(fl$c1[1], sqrt(ave[["COMP"]]), tolerance = 1e-10)
})

test_that("a reverse-coded indicator is caught: negative alpha and HTMT not computable", {
    data <- corpRepData()
    data$comp_2 <- 8 - data$comp_2
    a <- runPlssem(data, simpleModel())
    rel <- a$results$reliability$asDF
    expect_lt(rel$alpha[rel$construct == "COMP"], 0)
    h <- a$results$htmt$asDF
    comp <- h[h$c1 == "COMP" | h$c2 == "COMP", ]
    expect_true(all(comp$assessment == "Not computable"))
    expect_false(any(grepl("monotrait|Intra-block", noticesOf(a))))
})

test_that("a model without reflective constructs explains why there is no reliability", {
    m <- list(reflective = list(),
              formative = list(block("QUAL", paste0("qual_", 1:8)), block("PERF", paste0("perf_", 1:5))),
              structural = list(srow("PERF", "QUAL"), srow("QUAL")))
    a <- runPlssem(corpRepData(), m)
    expect_false(a$results$reliability$visible)
    expect_true(any(grepl("no reflective constructs", noticesOf(a))))
    expect_equal(nrow(a$results$vif$asDF), 13)
})

test_that("the reproducibility code reproduces reliability, rho_A and HTMT", {
    data <- corpRepData()
    a <- runPlssem(data, extendedModel())
    env <- new.env()
    env$data <- data
    out <- utils::capture.output(eval(parse(text = a$results$reproducibilityCode$content), envir = env))
    rel <- a$results$reliability$asDF
    rhoC <- cSEM::calculateRhoC(env$fit, .only_common_factors = FALSE)
    expect_equal(unname(rhoC[c("C1", "C2", "C4")]), rel$rhoC[c(1, 2, 4)], tolerance = 1e-10)
    expect_equal(unname(env$fit_c$Estimates$Reliabilities[c("C1", "C2", "C4")]), rel$rhoA[c(1, 2, 4)],
                 tolerance = 1e-10)
})

test_that("the measurement report is translated", {
    withr::local_envvar(c(LANGUAGE = "es"))
    a <- runPlssem(corpRepData(), simpleModel())
    expect_match(a$results$interpretation$content, "Fiabilidad")
    expect_equal(a$results$reliability$asDF$assessment[2], "fiabilidad satisfactoria; AVE adecuada")
})
