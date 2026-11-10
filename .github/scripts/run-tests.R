# -----------------------------------------------------------------------------
# PLSsemLab
# A jamovi module for partial least squares structural equation modeling and
# evidence-based methodological decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of PLSsemLab.
#
# PLSsemLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# PLSsemLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PLSsemLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# CI test runner.
# ES: Ejecutor de pruebas para la CI.
#
# Runs the edge-case suite and, for every failed or errored expectation,
# prints a GitHub Actions error annotation with the test name and message,
# so failures are readable from the PR's Checks tab. Exits with status 1 if
# anything failed.
#
# ES: Ejecuta la suite de casos límite y, por cada expectativa fallida o con
# error, imprime una anotación de error de GitHub Actions con el nombre de la
# prueba y el mensaje, para poder leer los fallos desde la pestaña Checks del
# PR. Termina con estado 1 si algo falló.
# -----------------------------------------------------------------------------

escape <- function(x) {
    x <- gsub("%", "%25", x, fixed = TRUE)
    x <- gsub("\r", "%0D", x, fixed = TRUE)
    gsub("\n", "%0A", x, fixed = TRUE)
}

results <- testthat::test_dir(
    "tests/testthat",
    reporter = testthat::ProgressReporter$new(show_praise = FALSE),
    stop_on_failure = FALSE
)

failed <- 0L
for (test in results) {
    for (r in test$results) {
        if (inherits(r, c("expectation_failure", "expectation_error"))) {
            failed <- failed + 1L
            cat(sprintf("::error title=%s::%s\n",
                        escape(paste0(test$file, ": ", test$test)),
                        escape(substr(conditionMessage(r), 1, 3000))))
        }
    }
}

if (failed > 0L)
    quit(status = 1)
