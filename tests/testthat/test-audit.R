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
# Regression tests for the findings of jamovi's audits of the earlier modules
# of the suite (EGAnetLab, MokkenLab), applied to PLSsemLab before its own
# review: the restart signal is never swallowed, long resampling calls stop
# in the middle, tables keep their shape, whole translatable sentences.
# ES: Pruebas de regresión de los hallazgos de las auditorías de jamovi a
# los módulos anteriores de la suite, aplicados a PLSsemLab antes de su
# propia revisión.
# -----------------------------------------------------------------------------

# jamovi's checkpoint serializes the results with RProtoBuf when it flushes;
# outside jamovi a stub is enough.
# ES: El checkpoint de jamovi serializa con RProtoBuf; fuera de jamovi basta
# un sustituto.
RProtoBuf_serialize <- function(...) raw(0)
assign("RProtoBuf_serialize", RProtoBuf_serialize, envir = globalenv())

learnFirstModel <- function() list(
    reflective = lapply(c("LEAD", "AUT", "ENG", "PERF", "INT"), learnBlock),
    formative = list(learnBlock("RES")),
    structural = list(srow("ENG", "LEAD", "RES", "AUT"), srow("PERF", "ENG", "LEAD"), srow("INT", "ENG", "LEAD")))

# Runs .run() with a checkpoint that asks for a restart after `after` calls.
# ES: Corre .run() con un checkpoint que pide reiniciar tras `after` llamadas.
runWithRestart <- function(extra, after = 1) {
    m <- learnFirstModel()
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    a <- cls$new(options = do.call(opts$new, c(list(reflective = m$reflective, formative = m$formative,
                                                    structural = asRows(m)), extra)),
                 data = bundled("plssemlab_learning.csv"))
    a$init()
    calls <- 0
    a$.__enclos_env__$private$.checkpointCB <- function(results) {
        calls <<- calls + 1
        if (calls >= after) "restart" else NULL
    }
    err <- tryCatch({ a$run(); NULL }, error = function(e) e)
    list(error = err, calls = calls, analysis = a)
}

test_that(".pl_capture() lets jamovi's restart signal through", {
    restart <- jmvcore::createError("restarting", "restart")
    expect_error(PLSsemLab:::.pl_capture(stop(restart)), "restarting")
    r <- PLSsemLab:::.pl_capture(stop("a cSEM failure"))
    expect_equal(r$error, "a cSEM failure")
})

test_that("every long computation stops when jamovi asks for a restart", {
    skip_on_cran()
    cases <- list(
        bootstrap = list(bootstrap = TRUE, bootReps = 400),
        fit = list(fitTest = TRUE, fitReps = 199),
        predict = list(predictRun = TRUE, predictFolds = 5, predictReps = 2),
        multigroup = list(groupVar = "sector", mgaRun = TRUE, mgaPerm = 199, mgaBoot = 199))
    for (nm in names(cases)) {
        r <- runWithRestart(cases[[nm]])
        expect_true(inherits(r$error, "error"), info = nm)
        expect_true(PLSsemLab:::.pl_is_restart(r$error), info = nm)
    }
})

test_that("a restarted run is not cached", {
    skip_on_cran()
    r <- runWithRestart(list(fitTest = TRUE, fitReps = 199))
    st <- r$analysis$results$fitCacheStore$state
    expect_true(is.null(st) || length(st) == 0)
})

test_that("long cSEM resampling calls check in at every resample, with the same results", {
    skip_on_cran()
    d <- stats::na.omit(corpRepData())
    model <- "COMP <~ comp_1 + comp_2 + comp_3\nLIKE <~ like_1 + like_2 + like_3\nCUSL <~ cusl_1 + cusl_2 + cusl_3\nCUSL ~ COMP + LIKE"
    fit <- withDefaultRng(cSEM::csem(d, model, .PLS_modes = "modeA", .disattenuate = FALSE))
    ticks <- 0
    with_ticks <- withDefaultRng(PLSsemLab:::.pl_with_ticks(cSEM::testOMF(fit, .R = 60, .seed = 3),
                                                            function() ticks <<- ticks + 1, every = 0))
    plain <- withDefaultRng(cSEM::testOMF(fit, .R = 60, .seed = 3))
    expect_equal(ticks, 60)
    expect_identical(with_ticks$Information$Bootstrap_values, plain$Information$Bootstrap_values)
    expect_identical(with_ticks$Critical_value, plain$Critical_value)
    # a restart thrown from a tick stops cSEM in the middle
    # ES: un reinicio lanzado desde un tick detiene a cSEM a mitad de camino
    n <- 0
    expect_error(PLSsemLab:::.pl_with_ticks(cSEM::testOMF(fit, .R = 200, .seed = 3), function() {
        n <<- n + 1
        if (n == 5) stop(jmvcore::createError("restarting", "restart"))
    }, every = 0), "restarting")
    expect_lt(n, 200)
    # progressr's options are restored
    # ES: las opciones de progressr se restauran
    expect_null(getOption("progressr.handlers"))
})

test_that("tables have their final rows when .init() sees only the header of the data", {
    skip_on_cran()
    m <- learnFirstModel()
    d <- bundled("plssemlab_learning.csv")
    cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
    args <- list(reflective = m$reflective, formative = m$formative, structural = asRows(m),
                 bootstrap = TRUE, bootReps = 100, fitTest = TRUE, fitReps = 99,
                 predictRun = TRUE, predictFolds = 5, predictReps = 1, ipma = TRUE,
                 groupVar = "sector", mgaRun = TRUE, mgaPerm = 99, mgaBoot = 99)
    tables <- function(g, path = "") {
        out <- list()
        for (it in g$items) {
            nm <- paste0(path, it$name)
            if (inherits(it, "Table")) out[[nm]] <- it$rowCount
            else if (inherits(it, "Group")) out <- c(out, tables(it, paste0(nm, "/")))
        }
        out
    }
    a0 <- cls$new(options = do.call(opts$new, args), data = d[0, , drop = FALSE])
    a0$init()
    a1 <- cls$new(options = do.call(opts$new, args), data = d)
    a1$init()
    a1$run()
    expect_equal(tables(a0$results), tables(a1$results))
})

test_that("the IPMA quadrant is one translatable sentence, also when not significant", {
    keys <- c("q_priority", "q_keep", "q_overkill", "q_low")
    lang <- PLSsemLab:::.pl_lang(get("plssemOptions", envir = asNamespace("PLSsemLab"))$new())
    for (k in keys) {
        whole <- PLSsemLab:::.pl_text(lang, "labels", paste0(k, "_ns"))
        expect_match(whole, "total effect not significant", fixed = TRUE)
        expect_true(startsWith(whole, PLSsemLab:::.pl_text(lang, "labels", k)))
    }
    src <- testthat::test_path("..", "..", "R", "plssem.b.R")
    if (file.exists(src)) expect_false(any(grepl("ipma_ns_suffix", readLines(src), fixed = TRUE)))
})

test_that("the reading guides are off by default and plot settings wait for their plot", {
    opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))$new()
    expect_false(opts$showGuides)
    skip_if_not_installed("yaml")
    ui <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "plssem.u.yaml"))
    flat <- function(x) if (is.list(x)) c(list(x), unlist(lapply(x, flat), recursive = FALSE)) else list()
    nodes <- Filter(function(n) is.list(n) && !is.null(n$name), flat(ui))
    diagram <- c("diagramLayout", "diagramTitle", "diagramIndicators", "latentSize", "edgeLabels")
    for (n in nodes) if (isTRUE(n$name %in% diagram)) expect_match(n$enable, "pathDiagram", info = n$name)
})

test_that("the bootstrap record is also a numeric table", {
    skip_on_cran()
    m <- learnFirstModel()
    a <- runPlssem(bundled("plssemlab_learning.csv"), m, bootstrap = TRUE, bootReps = 300, bootSeed = 11)
    bs <- a$results$bootSummary$asDF
    expect_equal(nrow(bs), 1)
    expect_equal(bs$requested, 300)
    expect_equal(bs$seed, 11)
    expect_equal(bs$block, 200)
    expect_equal(bs$level, 0.95)
    expect_true(bs$admissible <= bs$drawn && bs$admissible > 0)
    expect_false(a$results$bootStatus$visible)  # the record is the table (jamovi review)
})

test_that("the caches stay small: entries and bytes are capped", {
    fake <- new.env()
    fake$state <- NULL
    fake$setState <- function(x) fake$state <- x
    big <- function(i) function() list(summary = list(i = i, x = stats::runif(40000)))  # ~0.3 MB each
    for (i in 1:6) PLSsemLab:::.pl_cached_bootstrap(fake, list("k", i), big(i),
                                                    max_entries = PLSsemLab:::.PL_CACHE_BOOT$entries,
                                                    max_bytes = PLSsemLab:::.PL_CACHE_BOOT$bytes)
    expect_lte(length(fake$state), 3)
    expect_lte(length(serialize(fake$state, NULL)), 1.5 * 1024^2)
    # the newest entries are the ones kept
    # ES: se conservan las entradas más recientes
    expect_equal(fake$state[[length(fake$state)]]$summary$i, 6)
    expect_lte(PLSsemLab:::.PL_CACHE_BOOT$bytes + 3 * PLSsemLab:::.PL_CACHE_SMALL$bytes, 2 * 1024^2)
})

test_that("column super-titles follow jamovi's language", {
    skip_on_cran()
    withr::local_envvar(c(LANGUAGE = "es"))
    a <- runPlssem(corpRepData(), simpleModel(), bootstrap = TRUE, bootReps = 100)
    cols <- a$results$measurementSection$measurement$columns
    st <- unlist(lapply(cols, function(c) if (length(c$superTitle)) c$superTitle))
    expect_true("IC de la carga" %in% st)
    expect_false("Loading CI" %in% st)
})

test_that("every reference the module cites is in docs/Bibliography.md", {
    refs_file <- testthat::test_path("..", "..", "jamovi", "00refs.yaml")
    bib_file <- testthat::test_path("..", "..", "docs", "Bibliography.md")
    skip_if_not(file.exists(refs_file) && file.exists(bib_file))
    skip_if_not_installed("yaml")
    refs <- yaml::read_yaml(refs_file)$refs
    bib <- tolower(paste(readLines(bib_file, encoding = "UTF-8"), collapse = " "))
    doi <- vapply(refs, function(r) tolower(sub("^https?://(dx\\.)?doi\\.org/", "", r$url)), "")
    expect_true(all(vapply(doi, function(d) grepl(d, bib, fixed = TRUE), TRUE)),
                info = paste(names(doi)[!vapply(doi, function(d) grepl(d, bib, fixed = TRUE), TRUE)], collapse = ", "))
})
