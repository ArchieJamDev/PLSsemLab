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
# Stress tests (jamovi's library review asks for missing data, special
# characters and empty datasets). Every case must finish, or stop with a
# PLSsemLab message, never with a raw R error.
# ES: Pruebas de esfuerzo (la revisión de la biblioteca de jamovi pide datos
# ausentes, caracteres especiales y conjuntos vacíos). Cada caso debe
# terminar o detenerse con un mensaje de PLSsemLab, nunca con un error crudo
# de R.
# -----------------------------------------------------------------------------

# Messages of R itself that must never reach the user.
# ES: Mensajes del propio R que nunca deben llegar al usuario.
rawR <- paste("subscript out of bounds", "non-conformable", "object '.*' not found", "argument is of length zero",
              "missing value where TRUE/FALSE needed", "undefined columns", "replacement has", "invalid 'times'",
              "arguments imply differing number of rows", "rowKey .* not found", "\\$ operator is invalid", sep = "|")

# Runs an analysis; returns "ok", or the message of a stop that is not a
# raw R error (fails the test if it is one).
# ES: Corre un análisis; devuelve "ok" o el mensaje de una detención que no
# es un error crudo de R.
survive <- function(expr) {
    out <- tryCatch({ force(expr); "ok" }, error = function(e) conditionMessage(e))
    expect_false(grepl(rawR, out), info = out)
    out
}

accented <- function() {
    d <- bundled("plssemlab_learning.csv")
    names(d)[names(d) == "lead_1"] <- "liderazgo 1"
    names(d)[names(d) == "perf_2"] <- "desempeño 2"
    d$sector <- factor(ifelse(d$sector == "Public", "Sector público", "Sector privado"))
    d$sector[c(3, 50, 77)] <- NA
    d
}

accentedModel <- function() list(
    reflective = list(block("Liderazgo", c("liderazgo 1", paste0("lead_", 2:4))),
                      block("Compromiso laboral", c(paste0("vig_", 1:3), paste0("ded_", 1:3))),
                      block("Desempeño", c("perf_1", "desempeño 2", "perf_3"))),
    formative = list(),
    structural = list(srow("Compromiso laboral", "Liderazgo"), srow("Desempeño", "Compromiso laboral", "Liderazgo")))

test_that("names with spaces and accents run through every long option", {
    d <- accented()
    a <- NULL
    expect_equal(survive(a <- runPlssem(d, accentedModel(), missing = "listwise", bootstrap = TRUE, bootReps = 100,
                                        fitTest = TRUE, fitReps = 99, predictRun = TRUE, predictReps = 1, ipma = TRUE,
                                        groupVar = "sector", mgaRun = TRUE, mgaPerm = 99, mgaBoot = 99)), "ok")
    expect_true("Compromiso laboral" %in% res(a, "paths")$asDF$to)
    expect_equal(res(a, "mgaGroups")$asDF$group, c("Sector privado", "Sector público"))
    expect_true(all(is.finite(res(a, "mga")$asDF$pPerm)))
    expect_true(any(grepl("3 case\\(s\\) without a value of sector", noticesOf(a))))
    expect_true(all(is.finite(res(a, "predict")$asDF$q2)))
    expect_true(all(is.finite(res(a, "ipmaTable")$asDF$importance)))
})

test_that("an empty dataset stops with a message", {
    d <- bundled("plssemlab_learning.csv")[0, ]
    msg <- survive(runPlssem(d, accentedModel()))
    expect_false(identical(msg, "ok"))
})

test_that("a small sample stops or explains, in every section", {
    d <- accented()[1:25, ]
    survive(runPlssem(d, accentedModel(), bootstrap = TRUE, bootReps = 50, fitTest = TRUE, fitReps = 99,
                      predictRun = TRUE, predictReps = 1, groupVar = "sector", mgaRun = TRUE, mgaPerm = 99, mgaBoot = 99))
    d <- accented()[1:12, ]
    survive(runPlssem(d, accentedModel(), predictRun = TRUE, predictReps = 1))
})

test_that("an indicator constant inside one group keeps the rows with a reason", {
    d <- accented()
    d$perf_3[which(d$sector == "Sector público")] <- 4
    a <- NULL
    survive(a <- runPlssem(d, accentedModel(), missing = "listwise", groupVar = "sector", mgaRun = TRUE,
                           mgaPerm = 99, mgaBoot = 99))
    if (!is.null(a)) {
        dec <- res(a, "mga")$asDF$decision
        expect_true(all(dec == "Not computable") || all(dec %in% c("No", "Yes (both tests)", "Permutation test only", "PLS-MGA only")))
    }
})

test_that("an indicator with no observed value stops with a message", {
    d <- accented()
    d$perf_1 <- NA
    msg <- survive(runPlssem(d, accentedModel(), missing = "listwise"))
    expect_false(identical(msg, "ok"))
})

test_that("a grouping variable with numeric codes and one tiny group", {
    d <- bundled("plssemlab_learning.csv")
    d$code <- factor(ifelse(seq_len(nrow(d)) <= 5, 3, ifelse(d$sector == "Public", 1, 2)))
    a <- NULL
    survive(a <- runPlssem(d, accentedModel()[c("formative")] |> c(list(
        reflective = list(block("LEAD", paste0("lead_", 1:4)), block("PERF", paste0("perf_", 1:3))),
        structural = list(srow("PERF", "LEAD")))), missing = "listwise", groupVar = "code", mgaRun = TRUE))
    expect_match(res(a, "mga")$notes$reason$note, "3 \\(n = [0-9]\\)")
})
