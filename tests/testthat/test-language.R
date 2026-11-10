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
# The report follows jamovi's language through the native catalog, and a
# change of language reaches the next run in the same process (ARCHITECTURE.md
# §12; the lesson of MokkenLab 1.0.0).
# ES: El informe sigue el idioma de jamovi a través del catálogo nativo, y un
# cambio de idioma llega a la siguiente ejecución en el mismo proceso.
# -----------------------------------------------------------------------------

runIn <- function(lang, ...) {
    withr::local_envvar(c(LANGUAGE = lang))
    runPlssem(...)
}

test_that("the report is in Spanish, then English, then Spanish in one process", {
    data <- corpRepData()
    es1 <- runIn("es", data, simpleModel())
    en <- runIn("en", data, simpleModel())
    es2 <- runIn("es", data, simpleModel())
    expect_match(es1$results$keySummary$content, "Hallazgos principales")
    expect_match(en$results$keySummary$content, "Main findings")
    expect_match(es2$results$keySummary$content, "Hallazgos principales")
    expect_equal(es2$results$model$asDF$measurement[1], "Reflectivo")
})

test_that("Category A messages are translated", {
    withr::local_envvar(c(LANGUAGE = "es"))
    expect_error(runPlssem(corpRepRaw(), simpleModel()), "tiene 1 valor\\(es\\) de -99")
})
