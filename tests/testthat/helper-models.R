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
# Test helpers: the reference data, the reference model as the option boxes
# hold it, and a runner that goes through the R6 class as jamovi does.
# ES: Ayudantes de prueba: los datos de referencia, el modelo de referencia
# tal como lo guardan las cajas de opciones, y un ejecutor que pasa por la
# clase R6 como lo hace jamovi.
# -----------------------------------------------------------------------------

corpRepData <- function() {
    path <- system.file("extdata", "plssemlab_corp_rep.csv", package = "PLSsemLab")
    if (!nzchar(path))
        path <- testthat::test_path("..", "..", "data", "plssemlab_corp_rep.csv")
    utils::read.csv(path)
}

# The corporate reputation data with the original -99 codes (cSEM's copy).
# ES: Los datos de reputación corporativa con los -99 originales.
corpRepRaw <- function() {
    utils::data("corp_rep_data", package = "cSEM", envir = environment())
    corp_rep_data
}

block <- function(label, vars) list(label = label, vars = vars)
srow <- function(label, ...) list(label = label, predictors = lapply(c(...), function(x) x))

# Extended model of Hair et al. (four formative, four reflective constructs).
# ES: Modelo extendido de Hair et al. (cuatro formativos, cuatro reflectivos).
extendedModel <- function() list(
    reflective = list(block("COMP", paste0("comp_", 1:3)), block("LIKE", paste0("like_", 1:3)),
                      block("CUSA", "cusa"), block("CUSL", paste0("cusl_", 1:3))),
    formative = list(block("QUAL", paste0("qual_", 1:8)), block("PERF", paste0("perf_", 1:5)),
                     block("CSOR", paste0("csor_", 1:5)), block("ATTR", paste0("attr_", 1:3))),
    structural = list(srow("COMP", "QUAL", "PERF", "CSOR", "ATTR"), srow("LIKE", "QUAL", "PERF", "CSOR", "ATTR"),
                      srow("CUSA", "COMP", "LIKE"), srow("CUSL", "COMP", "LIKE", "CUSA"),
                      srow("QUAL"), srow("PERF"), srow("CSOR"), srow("ATTR")))

# Simple reflective model of Hair et al.
# ES: Modelo reflectivo simple de Hair et al.
simpleModel <- function() list(
    reflective = list(block("COMP", paste0("comp_", 1:3)), block("LIKE", paste0("like_", 1:3)),
                      block("CUSA", "cusa"), block("CUSL", paste0("cusl_", 1:3))),
    formative = list(),
    structural = list(srow("CUSA", "COMP", "LIKE"), srow("CUSL", "COMP", "LIKE", "CUSA"),
                      srow("COMP"), srow("LIKE")))

# The structural option as jamovi stores it: one list of terms per construct,
# in the order of the constructs with a name and indicators (reflective
# first), as jamovi/js/plssem.events.js builds it. Tests write rows by name
# (srow) and this puts them in that order.
# ES: La opción estructural como la guarda jamovi: una lista de términos por
# constructo, en el orden de los constructos con nombre e indicadores.
asRows <- function(model) {
    blocks <- c(model$reflective, model$formative)
    names <- unique(vapply(Filter(function(b) length(b$vars) > 0 && nzchar(trimws(b$label)), blocks),
                           function(b) trimws(b$label), ""))
    lapply(names, function(n) {
        row <- Filter(function(r) identical(r$label, n), model$structural)
        if (length(row)) row[[1]]$predictors else list()
    })
}

runPlssem <- function(data, model, ...) {
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    analysis <- cls$new(options = opts$new(reflective = model$reflective, formative = model$formative,
                                           structural = asRows(model), ...),
                        data = data)
    analysis$init()
    analysis$run()
    analysis
}

# The analysis' notices (cSEM warnings and data notes), as text.
# ES: Los avisos del análisis, como texto.
noticesOf <- function(analysis) {
    items <- analysis$results$items
    unlist(lapply(items[grepl("^\\.plssem", names(items))], function(n) n$content))
}

# The equivalent R code as plain text (the output is Html with a <pre>).
# ES: El código R equivalente como texto (la salida es Html con <pre>).
reproCode <- function(a) {
    code <- gsub("<[^>]+>", "", a$results$reproducibilityCode$content)
    gsub("&gt;", ">", gsub("&lt;", "<", gsub("&amp;", "&", code, fixed = TRUE), fixed = TRUE), fixed = TRUE)
}
