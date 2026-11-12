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
# Phase 3: bootstrap in blocks, structural effect sizes and effects. Up to
# 200 resamples the intervals are identical to a direct cSEM call with the
# same seed; with more, they equal the module's equivalent R code (the same
# blocks and seeds), and every criterion keeps its row.
# ES: Fase 3: bootstrap por bloques, tamaños de efecto y efectos. Hasta 200
# réplicas los intervalos son idénticos a una llamada directa a cSEM con la
# misma semilla; con más, iguales al código R equivalente del módulo.
# -----------------------------------------------------------------------------

reproEnv <- function(a, data) {
    env <- new.env()
    env$data <- data
    utils::capture.output(eval(parse(text = reproCode(a)), envir = env))
    env
}

test_that("one block equals a direct cSEM bootstrap with the same seed", {
    data <- corpRepData()
    a <- runPlssem(data, simpleModel(), bootstrap = TRUE, bootReps = 150, bootSeed = 11)
    env <- reproEnv(a, data)
    direct <- cSEM::resamplecSEMResults(env$fit, .R = 150, .seed = 11, .handle_inadmissibles = "drop")
    s <- cSEM::summarize(direct, .alpha = 0.05, .ci = "CI_percentile")$Estimates$Path_estimates
    p <- a$results$paths$asDF
    key <- paste(p$to, p$from)
    inv <- c(COMP = "C1", LIKE = "C2", CUSA = "C3", CUSL = "C4")
    for (k in seq_len(nrow(p))) {
        r <- s[as.character(s$Name) == paste(inv[[p$to[k]]], "~", inv[[p$from[k]]]), ]
        expect_equal(p$se[k], r$Std_err, tolerance = 1e-10, info = key[k])
        expect_equal(p$p[k], r$p_value, tolerance = 1e-10, info = key[k])
        expect_equal(p$lower[k], r[["CI_percentile.95%L"]], tolerance = 1e-10, info = key[k])
        expect_equal(p$upper[k], r[["CI_percentile.95%U"]], tolerance = 1e-10, info = key[k])
    }
    expect_match(a$results$bootStatus$content, "150 resamples drawn")
    expect_match(a$results$bootStatus$content, "seed 11")
})

test_that("several blocks equal the equivalent R code and stay close to one run", {
    data <- corpRepData()
    a <- runPlssem(data, simpleModel(), bootstrap = TRUE, bootReps = 450, bootSeed = 5, ciLevel = 0.90)
    env <- reproEnv(a, data)
    s <- cSEM::summarize(env$boot, .alpha = 0.10, .ci = "CI_percentile")$Estimates$Path_estimates
    p <- a$results$paths$asDF
    expect_equal(sort(p$lower), sort(s[["CI_percentile.90%L"]]), tolerance = 1e-10)
    expect_equal(sort(p$upper), sort(s[["CI_percentile.90%U"]]), tolerance = 1e-10)
    one <- cSEM::summarize(cSEM::resamplecSEMResults(env$fit, .R = 450, .seed = 5, .handle_inadmissibles = "drop"),
                           .alpha = 0.10, .ci = "CI_percentile")$Estimates$Path_estimates
    expect_lt(max(abs(sort(p$upper) - sort(one[["CI_percentile.90%U"]]))), 0.05)
})

test_that("every table gets its intervals and the HTMT assessment uses the upper bound", {
    a <- runPlssem(corpRepData(), extendedModel(), bootstrap = TRUE, bootReps = 120, bootSeed = 3)
    rel <- a$results$reliability$asDF
    multi <- rel[rel$construct != "CUSA", ]
    expect_true(all(multi$rhoCLower <= multi$rhoC & multi$rhoC <= multi$rhoCUpper))
    expect_true(all(multi$aveLower <= multi$ave & multi$ave <= multi$aveUpper))
    h <- a$results$htmt$asDF
    expect_true(all(h$htmtUpper >= h$htmt - 1e-8))
    expect_true(all(grepl("^Upper bound", h$assessment)))
    m <- a$results$measurement$asDF
    expect_true(all(is.finite(m$loadLower)))
    expect_true(all(is.finite(m$weightP[m$construct %in% c("QUAL", "PERF", "CSOR", "ATTR")])))
    r2 <- a$results$r2$asDF
    expect_true(all(r2$r2Lower <= r2$r2 & r2$r2 <= r2$r2Upper))
    expect_true(all(is.finite(a$results$vif$asDF$vifUpper)))
    v <- a$results$vifStruct$asDF
    expect_true(all(is.finite(v$vifLower[v$to %in% c("COMP", "LIKE", "CUSL")])))
    e <- a$results$effects$asDF
    expect_true(all(is.finite(e$totLower)))
    expect_match(a$results$interpretation$content, "With the bootstrap")
})

test_that("without the bootstrap the structural tables still show estimates", {
    data <- corpRepData()
    a <- runPlssem(data, extendedModel(), missing = "mean")
    d <- data[, -c(1, 2)]
    for (v in names(d)) d[[v]][is.na(d[[v]])] <- mean(d[[v]], na.rm = TRUE)
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
    ref <- cSEM::csem(d, syntax, .disattenuate = FALSE)
    f2 <- cSEM::calculatef2(ref)
    tab <- a$results$f2$asDF
    for (k in seq_len(nrow(tab)))
        expect_equal(tab$f2[k], f2[tab$to[k], tab$from[k]], tolerance = 1e-8)
    vifs <- unlist(ref$Estimates$VIF)
    v <- a$results$vifStruct$asDF
    for (k in seq_len(nrow(v)))
        expect_equal(v$vif[k], unname(vifs[paste(v$to[k], v$from[k], sep = ".")]), tolerance = 1e-8)
    eff <- cSEM::summarize(ref)$Estimates$Effect_estimates$Total_effect
    e <- a$results$effects$asDF
    row <- e[e$from == "QUAL" & e$to == "CUSL", ]
    expect_equal(row$total, eff$Estimate[as.character(eff$Name) == "CUSL ~ QUAL"], tolerance = 1e-8)
    expect_equal(nrow(e), 21)
    expect_false(a$results$bootStatus$visible)
})

test_that("the diagram and the measurement plots keep their data in the image state", {
    a <- runPlssem(corpRepData(), simpleModel(), loadingsPlot = TRUE, htmtPlot = TRUE)
    st <- a$results$pathDiagram$state
    # 4 constructs and 10 indicators; 10 loadings and 5 paths
    # ES: 4 constructos y 10 indicadores; 10 cargas y 5 rutas
    expect_equal(sum(!st$vars$manifest), 4)
    expect_equal(sum(st$vars$manifest), 10)
    expect_equal(sum(st$pars$kind == "loading"), 10)
    expect_equal(sum(st$pars$kind == "path"), 5)
    expect_true(any(grepl("R\u00b2", st$vars$name)))
    expect_equal(nrow(a$results$loadingsPlot$state$data), 9)
    expect_equal(nrow(a$results$htmtPlot$state$data), 6)
    theme <- list(color = c("#333333", "#6B9DE8"), fill = c("#FFFFFF", "#E6E6E6"))
    fun <- function(name) get(name, envir = asNamespace("PLSsemLab"))
    for (p in list(fun(".pl_plot_loadings")(a$results$loadingsPlot$state, ggplot2::theme_minimal(), theme),
                   fun(".pl_plot_htmt")(a$results$htmtPlot$state, ggplot2::theme_minimal(), theme)))
        expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
    without <- runPlssem(corpRepData(), simpleModel(), diagramIndicators = FALSE)$results$pathDiagram$state
    expect_equal(sum(without$vars$manifest), 0)
    expect_equal(nrow(without$pars), 5)
})

test_that("formative indicators point to their construct in the diagram", {
    st <- runPlssem(corpRepData(), extendedModel())$results$pathDiagram$state
    w <- st$pars[st$pars$kind == "weight", ]
    expect_true(all(w$lhs %in% st$vars$name[st$vars$manifest]))
    expect_true(all(grepl("^QUAL|^PERF|^CSOR|^ATTR", w$rhs)))
})

test_that("the SEM diagram renders with semPlot", {
    skip_if_not_installed("semPlot", minimum_version = "1.0")
    a <- runPlssem(corpRepData(), extendedModel(), bootstrap = TRUE, bootReps = 100)
    theme <- list(color = c("#333333", "#6B9DE8"), fill = c("#FFFFFF", "#E6E6E6"))
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 900, 600)
    ok <- get(".pl_plot_sem", envir = asNamespace("PLSsemLab"))(a$results$pathDiagram$state, a$options, theme)
    grDevices::dev.off()
    expect_true(ok)
    expect_gt(file.size(f), 5000)
})
