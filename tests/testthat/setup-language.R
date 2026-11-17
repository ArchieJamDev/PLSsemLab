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

# Tests read English messages unless a test sets another language.
# ES: Las pruebas leen los mensajes en inglés salvo que una prueba fije otro idioma.
withr::local_envvar(c(LANGUAGE = "en"), .local_envir = testthat::teardown_env())
