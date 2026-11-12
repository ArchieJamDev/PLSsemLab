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
# D5: the model is checked before estimation, and every Category A message
# names the construct or indicator at fault (the seven mistakes of
# experiment E4, plus the data checks).
# ES: D5: el modelo se revisa antes de estimar, y cada mensaje de Categoría A
# nombra el constructo o indicador con problemas.
# -----------------------------------------------------------------------------

test_that("an incomplete model shows the introduction, not an error", {
    data <- corpRepData()
    one <- list(reflective = list(block("COMP", paste0("comp_", 1:3))), formative = list(), structural = list())
    a <- runPlssem(data, one)
    expect_true(a$results$intro$visible)
    expect_match(a$results$intro$content, "at least two constructs")

    no_paths <- simpleModel()
    no_paths$structural <- list()
    a <- runPlssem(data, no_paths)
    expect_match(a$results$intro$content, "drag at least one predictor")
})

test_that("the seven model mistakes of E4 are rejected with a message that names the culprit", {
    data <- corpRepData()
    m <- simpleModel()

    twice <- m; twice$reflective[[2]]$vars <- c("like_1", "like_2", "comp_1")
    expect_error(runPlssem(data, twice), "'comp_1' is an indicator of more than one construct")

    dup <- m; dup$reflective[[2]]$label <- "COMP"
    expect_error(runPlssem(data, dup), "The name 'COMP' is used for more than one construct")

    noname <- m; noname$reflective[[2]]$label <- ""
    expect_error(runPlssem(data, noname), "needs a name")

    loop <- m; loop$structural[[3]] <- srow("COMP", "CUSL")
    expect_error(runPlssem(data, loop), "loop \\(")

    iso <- m; iso$reflective[[5]] <- block("SWITCH", paste0("switch_", 1:4))
    expect_error(runPlssem(data, iso), "'SWITCH' is not connected")

    self <- m; self$structural[[1]] <- srow("CUSA", "COMP", "CUSA")
    expect_error(runPlssem(data, self), "'CUSA' cannot predict itself")

    # two-way interactions are estimated (phase 4); three-way ones are not
    # ES: las interacciones de dos vías se estiman (fase 4); las de tres no
    inter <- m; inter$structural[[2]]$predictors <- list("COMP", "LIKE", "CUSA", c("COMP", "LIKE", "CUSA"))
    expect_error(runPlssem(data, inter), "more than two constructs")
})

test_that("an empty construct is left out with a notice, not an error", {
    m <- simpleModel()
    m$formative <- list(block("Construct 5", character(0)))
    a <- runPlssem(corpRepData(), m)
    expect_true(any(grepl("'Construct 5' has no indicators yet", noticesOf(a))))
})

test_that("missing-data codes such as -99 are caught before estimation (E1)", {
    expect_error(runPlssem(corpRepRaw(), simpleModel()),
                 "'cusa' has 1 value\\(s\\) of -99, far outside the range of its other values")
})

test_that("constant indicators and too few cases are rejected", {
    data <- corpRepData()
    data$comp_1 <- 4
    expect_error(runPlssem(data, simpleModel()), "'comp_1' has the same value for every case")

    small <- corpRepData()[1:8, ]
    expect_error(runPlssem(small, simpleModel()), "Only 8 cases have complete data")
})

test_that("names with spaces and accents work (internal names, E4)", {
    data <- corpRepData()
    names(data)[names(data) == "comp_1"] <- "competencia 1"
    m <- simpleModel()
    m$reflective[[1]] <- block("Competencia percibida", c("competencia 1", "comp_2", "comp_3"))
    m$reflective[[2]]$label <- "Simpatía"
    m$structural <- list(srow("CUSA", "Competencia percibida", "Simpatía"),
                         srow("CUSL", "Competencia percibida", "Simpatía", "CUSA"),
                         srow("Competencia percibida"), srow("Simpatía"))
    a <- runPlssem(data, m)
    paths <- a$results$paths$asDF
    expect_true("Competencia percibida" %in% paths$from)
    expect_true(all(is.finite(paths$estimate)))
})
