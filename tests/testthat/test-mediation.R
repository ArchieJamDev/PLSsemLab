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

# Mediation (phase 6). A chain that is the only route between two constructs
# must reproduce cSEM's indirect effect and its percentile interval (same
# resamples); the estimate of every chain is the product of cSEM's paths.
# ES: Mediación (fase 6). Una cadena que es la única ruta entre dos
# constructos debe reproducir el efecto indirecto de cSEM y su intervalo.

test_that("chains are found and typed (Zhao et al., 2010)", {
    paths <- data.frame(from = c("A", "A", "M", "M", "N", "A"), to = c("M", "N", "Y", "N", "Y", "Y"),
                        stringsAsFactors = FALSE)
    ch <- PLSsemLab:::.pl_mediation_chains(paths)
    expect_setequal(ch$label, c("A → M → Y", "A → M → N", "A → N → Y",
                                "A → M → N → Y", "M → N → Y"))
    type <- PLSsemLab:::.pl_mediation_type
    expect_equal(type(0.2, 0.1, 0.3, 0.3, 0.1, 0.5), "complementary")
    expect_equal(type(0.2, 0.1, 0.3, -0.3, -0.5, -0.1), "competitive")
    expect_equal(type(0.2, 0.1, 0.3, 0.05, -0.1, 0.2), "indirect_only")
    expect_equal(type(0.01, -0.1, 0.1, 0.3, 0.1, 0.5), "direct_only")
    expect_equal(type(0.01, -0.1, 0.1, 0.01, -0.1, 0.2), "no_effect")
    expect_equal(type(0.2, 0.1, 0.3, NA, NA, NA), "indirect_nodirect")
})

test_that("a single chain reproduces cSEM's indirect effect and interval", {
    a <- runPlssem(corpRepData(), simpleModel(), bootstrap = TRUE, bootReps = 200, bootSeed = 1)
    med <- a$results$mediation$asDF
    eff <- a$results$effects$asDF
    expect_equal(nrow(med), 2)
    for (x in c("COMP", "LIKE")) {
        m <- med[med$chain == paste(x, "→ CUSA → CUSL"), ]
        e <- eff[eff$from == x & eff$to == "CUSL", ]
        expect_equal(m$indirect, e$indirect, tolerance = 1e-10)
        expect_equal(c(m$lower, m$upper), c(e$indLower, e$indUpper), tolerance = 1e-6)
    }
    expect_equal(med$type[med$chain == "LIKE → CUSA → CUSL"], "Complementary (partial)")
    expect_match(a$results$keySummary$content, "Mediation: 2 of 2")
})

test_that("without the bootstrap the chains show estimates and ask for it", {
    a <- runPlssem(corpRepData(), simpleModel())
    med <- a$results$mediation$asDF
    expect_true(all(med$type == "Needs the bootstrap"))
    expect_true(all(is.finite(med$indirect)))
})

test_that("mediation through a higher-order construct uses the stage-2 draws", {
    d <- cSEM::satisfaction
    m <- list(reflective = list(block("SAT", paste0("sat", 1:4)), block("LOY", paste0("loy", 1:4))),
              formative = list(block("IMAG", paste0("imag", 1:5)), block("EXPE", paste0("expe", 1:5))),
              structural = list(srow("SAT", "QUAL"), srow("LOY", "SAT", "QUAL")))
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    a <- cls$new(options = opts$new(reflective = m$reflective, formative = m$formative,
                                    reflectiveHoc = list(list(label = "QUAL", components = list("IMAG", "EXPE"))),
                                    structural = list(list("QUAL"), list("SAT", "QUAL"), list(), list(), list()),
                                    bootstrap = TRUE, bootReps = 100, bootSeed = 3), data = d)
    a$init(); a$run()
    med <- a$results$mediation$asDF
    eff <- a$results$effects$asDF
    expect_equal(med$chain, "QUAL → SAT → LOY")
    e <- eff[eff$from == "QUAL" & eff$to == "LOY", ]
    expect_equal(med$indirect, e$indirect, tolerance = 1e-10)
    expect_equal(c(med$lower, med$upper), c(e$indLower, e$indUpper), tolerance = 1e-6)
})
