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
# Comparison with the values published by Hair et al. for the corporate
# reputation model (bundled dataset, -99 codes replaced by the indicator
# mean, as in the books).
# ES: Comparación con los valores publicados por Hair et al. para el modelo
# de reputación corporativa (con reemplazo por la media, como en los libros).
#
# Sources (values printed in the text, three decimals):
#   H21 = Hair, Hult, Ringle, Sarstedt, Danks & Ray (2021). Partial least
#         squares structural equation modeling (PLS-SEM) using R. Springer,
#         chapters 4 and 5 (open access; estimated with SEMinR).
#   H22 = Hair, Hult, Ringle & Sarstedt (2022). A primer on partial least
#         squares structural equation modeling (3rd ed.). Sage; case study of
#         chapter 6 (estimated with SmartPLS 4).
# Point estimates are deterministic and must agree to the three printed
# decimals (|difference| <= 0.0005). Bootstrap intervals and PLSpredict
# depend on random draws and folds: they are reported as informative rows
# (tolerance 0.03) and never decide the result.
# Writes validation/hair_published_results.csv and stops if a deterministic
# value disagrees. Run from the package root after installing PLSsemLab:
#   Rscript validation/hair_published.R
#
# ES: Las estimaciones puntuales son deterministas y deben coincidir en los
# tres decimales publicados; los intervalos bootstrap y PLSpredict dependen
# de sorteos y particiones: se informan como filas orientativas.
# -----------------------------------------------------------------------------

suppressMessages(library(PLSsemLab))

quiet <- function(expr) { out <- NULL; utils::capture.output(out <- suppressWarnings(expr)); out }
cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
runModule <- function(data, ...) {
    a <- cls$new(options = opts$new(...), data = data)
    a$init()
    quiet(a$run())
    a
}
block <- function(label, vars) list(label = label, vars = vars)
tab <- function(a, section, name) a$results$get(section)$get(name)$asDF

path <- system.file("extdata", "plssemlab_corp_rep.csv", package = "PLSsemLab")
if (!nzchar(path)) path <- file.path("data", "plssemlab_corp_rep.csv")
corp <- utils::read.csv(path, stringsAsFactors = TRUE)

refl <- list(block("COMP", paste0("comp_", 1:3)), block("LIKE", paste0("like_", 1:3)),
             block("CUSA", "cusa"), block("CUSL", paste0("cusl_", 1:3)))
form <- list(block("QUAL", paste0("qual_", 1:8)), block("PERF", paste0("perf_", 1:5)),
             block("CSOR", paste0("csor_", 1:5)), block("ATTR", paste0("attr_", 1:3)))
simple <- runModule(corp, reflective = refl, formative = list(), missing = "mean",
                    structural = list(list(), list(), list("COMP", "LIKE"), list("COMP", "LIKE", "CUSA")))
extended <- runModule(corp, reflective = refl, formative = form, missing = "mean",
                      structural = list(list("QUAL", "PERF", "CSOR", "ATTR"), list("QUAL", "PERF", "CSOR", "ATTR"),
                                        list("COMP", "LIKE"), list("COMP", "LIKE", "CUSA"),
                                        list(), list(), list(), list()),
                      bootstrap = TRUE, bootReps = 2000, bootSeed = 123,
                      predictRun = TRUE, predictFolds = 10, predictReps = 10)

rows <- list()
add <- function(source, model, quantity, item, published, ours, informative = FALSE, note = "") {
    tol <- if (informative) 0.03 else 0.0005 + 1e-9
    rows[[length(rows) + 1]] <<- data.frame(source = source, model = model, quantity = quantity, item = item,
                                            published = published, plssemlab = round(ours, 4),
                                            difference = round(ours - published, 4), tolerance = tol,
                                            informative = informative, pass = abs(ours - published) <= tol,
                                            note = note)
}

# Simple model: reliability, AVE, loadings, Fornell-Larcker (H21, ch. 4).
# ES: Modelo simple: fiabilidad, AVE, cargas, Fornell-Larcker (H21, cap. 4).
rel <- tab(simple, "measurementSection", "reliability")
pub <- data.frame(construct = c("COMP", "CUSL", "LIKE"), alpha = c(0.776, 0.831, 0.831),
                  rhoC = c(0.865, 0.899, 0.899), rhoA = c(0.832, 0.839, 0.836), ave = c(0.681, 0.748, 0.747))
for (k in seq_len(nrow(pub))) {
    r <- rel[rel$construct == pub$construct[k], ]
    add("H21 ch. 4", "simple", "Cronbach's alpha", pub$construct[k], pub$alpha[k], r$alpha)
    add("H21 ch. 4", "simple", "rho_C", pub$construct[k], pub$rhoC[k], r$rhoC)
    add("H21 ch. 4", "simple", "rho_A", pub$construct[k], pub$rhoA[k], r$rhoA)
    add("H21 ch. 4", "simple", "AVE", pub$construct[k], pub$ave[k], r$ave)
    add("H21 ch. 4", "simple", "sqrt(AVE), Fornell-Larcker diagonal", pub$construct[k],
        c(COMP = 0.825, CUSL = 0.865, LIKE = 0.864)[[pub$construct[k]]], sqrt(r$ave))
}
m <- tab(simple, "measurementSection", "measurement")
add("H21 ch. 4", "simple", "loading", "comp_2", 0.798, m$loading[m$indicator == "comp_2"])
add("H21 ch. 4", "simple", "loading", "cusl_2", 0.917, m$loading[m$indicator == "cusl_2"])
add("H21 ch. 4", "simple", "indicator reliability (loading squared)", "comp_2", 0.638, m$loading[m$indicator == "comp_2"]^2)
add("H21 ch. 4", "simple", "indicator reliability (loading squared)", "cusl_2", 0.841, m$loading[m$indicator == "cusl_2"]^2)

# Extended model: formative indicators (H21, ch. 5).
# ES: Modelo extendido: indicadores formativos (H21, cap. 5).
v <- tab(extended, "measurementSection", "vif")
add("H21 ch. 5", "extended", "VIF (highest)", "qual_3", 2.269, v$vif[v$indicator == "qual_3"])
add("H21 ch. 5", "extended", "VIF (highest of all formative indicators)", "max", 2.269, max(v$vif))
me <- tab(extended, "measurementSection", "measurement")
add("H21 ch. 5", "extended", "loading", "qual_2", 0.570, me$loading[me$indicator == "qual_2"])

# Extended model: paths, R², f², total effects (H22, ch. 6 case study).
# ES: Modelo extendido: rutas, R², f², efectos totales (H22, cap. 6).
p <- tab(extended, "structuralSection", "paths")
pubPaths <- c("ATTR COMP" = 0.086, "ATTR LIKE" = 0.167, "COMP CUSA" = 0.146, "COMP CUSL" = 0.006,
              "CSOR COMP" = 0.059, "CSOR LIKE" = 0.178, "CUSA CUSL" = 0.505, "LIKE CUSA" = 0.436,
              "LIKE CUSL" = 0.344, "PERF COMP" = 0.295, "PERF LIKE" = 0.117, "QUAL COMP" = 0.430,
              "QUAL LIKE" = 0.380)
for (nm in names(pubPaths)) {
    ft <- strsplit(nm, " ")[[1]]
    k <- which(p$from == ft[1] & p$to == ft[2])
    add("H22 ch. 6", "extended", "path coefficient", paste(ft, collapse = " → "), pubPaths[[nm]], p$estimate[k])
}
r2 <- tab(extended, "structuralSection", "r2")
for (cn in c(COMP = 0.631, CUSL = 0.562, LIKE = 0.558, CUSA = 0.292) |> names())
    add("H22 ch. 6", "extended", "R2", cn, c(COMP = 0.631, CUSL = 0.562, LIKE = 0.558, CUSA = 0.292)[[cn]],
        r2$r2[r2$construct == cn])
f2 <- tab(extended, "structuralSection", "f2")
for (nm in c("LIKE CUSA", "LIKE CUSL", "COMP CUSA", "COMP CUSL")) {
    ft <- strsplit(nm, " ")[[1]]
    add("H22 ch. 6", "extended", "f2", paste(ft, collapse = " → "),
        c("LIKE CUSA" = 0.159, "LIKE CUSL" = 0.138, "COMP CUSA" = 0.018, "COMP CUSL" = 0.000)[[nm]],
        f2$f2[f2$from == ft[1] & f2$to == ft[2]])
}
ef <- tab(extended, "structuralSection", "effects")
pubTot <- c("ATTR CUSA" = 0.086, "ATTR CUSL" = 0.101, "COMP CUSL" = 0.074, "CSOR CUSA" = 0.086,
            "CSOR CUSL" = 0.105, "LIKE CUSL" = 0.220, "PERF CUSA" = 0.094, "PERF CUSL" = 0.089,
            "QUAL CUSA" = 0.228, "QUAL CUSL" = 0.248)
# The book's "total effects" table lists the indirect paths: for COMP and
# LIKE on CUSL the printed value is the indirect effect through CUSA.
# ES: La tabla de "efectos totales" del libro lista los efectos por rutas
# indirectas: para COMP y LIKE sobre CUSL el valor es el indirecto.
# ATTR -> CUSA: the printed 0.086 does not follow from the source's own path
# coefficients (0.086 x 0.146 + 0.167 x 0.436 = 0.0854), so that row is
# informative.
# ES: ATTR -> CUSA: el 0.086 impreso no se deduce de las rutas de la propia
# fuente (0.0854), así que esa fila es orientativa.
inconsistent <- "ATTR CUSA"
for (nm in names(pubTot)) {
    ft <- strsplit(nm, " ")[[1]]
    r <- ef[ef$from == ft[1] & ef$to == ft[2], ]
    ours <- if (nm %in% c("COMP CUSL", "LIKE CUSL")) r$indirect else r$total
    add("H22 ch. 6", "extended", if (nm %in% c("COMP CUSL", "LIKE CUSL")) "indirect effect" else "total effect",
        paste(ft, collapse = " \u2192 "), pubTot[[nm]], ours, informative = nm %in% inconsistent,
        note = if (nm %in% inconsistent) "printed value inconsistent with the source's own paths (0.086 x 0.146 + 0.167 x 0.436 = 0.0854)" else "")
}

# Random draws and folds: informative only.
# ES: Sorteos y particiones aleatorias: solo orientativo.
pubCI <- list("CUSA CUSL" = c(0.420, 0.584), "LIKE CUSA" = c(0.316, 0.545), "QUAL COMP" = c(0.299, 0.558))
for (nm in names(pubCI)) {
    ft <- strsplit(nm, " ")[[1]]
    k <- which(p$from == ft[1] & p$to == ft[2])
    add("H22 ch. 6", "extended", "95% bootstrap lower bound", paste(ft, collapse = " → "), pubCI[[nm]][1], p$lower[k], TRUE)
    add("H22 ch. 6", "extended", "95% bootstrap upper bound", paste(ft, collapse = " → "), pubCI[[nm]][2], p$upper[k], TRUE)
}
pr <- tab(extended, "predictSection", "predict")
for (it in c(cusl_1 = 1.300, cusl_2 = 1.523, cusl_3 = 1.531) |> names())
    add("H22 ch. 6", "extended", "RMSE PLS (10 folds x 10 repetitions)", it,
        c(cusl_1 = 1.300, cusl_2 = 1.523, cusl_3 = 1.531)[[it]], pr$rmsePls[pr$indicator == it], TRUE)
for (it in c(cusl_1 = 1.310, cusl_2 = 1.542, cusl_3 = 1.569) |> names())
    add("H22 ch. 6", "extended", "RMSE LM (10 folds x 10 repetitions)", it,
        c(cusl_1 = 1.310, cusl_2 = 1.542, cusl_3 = 1.569)[[it]], pr$rmseLm[pr$indicator == it], TRUE)

out <- do.call(rbind, rows)
utils::write.csv(out, file.path("validation", "hair_published_results.csv"), row.names = FALSE)
det <- out[!out$informative, ]
cat(sprintf("%d published values: %d deterministic (%d agree to 3 decimals), %d informative (%d within 0.03)\n",
            nrow(out), nrow(det), sum(det$pass), sum(out$informative), sum(out$pass[out$informative])))
if (any(!det$pass)) {
    print(det[!det$pass, ], row.names = FALSE)
    stop("a deterministic value differs from the published one")
}
