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
# Numerical equivalence with the NCA package (Dul).
# ES: Equivalencia numérica con el paquete NCA (Dul).
#
# PLSsemLab computes the necessary condition analysis itself (R/nca.R): the
# NCA package finds the peers with an R loop, so 10 000 permutations take
# many minutes and cannot be interrupted in jamovi. This script checks that
# every number equals NCA::nca_analysis() with its defaults: the 0-100
# construct scores (against scores built here by hand from a direct cSEM
# fit), the CE-FDH and CR-FDH effect sizes, the accuracy of CR-FDH, both
# bottleneck tables and the permutation p values. The package draws one
# offset per condition from the session's generator and seeds permutation i
# with offset + i; its purity metrics also draw random numbers, so the same
# seed does not give it the same offsets as the module. The script reads the
# package's offsets (trace on its internal p_test) and gives them to the
# module's permutation test: with the same permutations both p values must be
# identical. The module's own p values (its offsets) are checked to repeat
# with the seed. Three scenarios: corporate reputation (Hair et al.), cSEM's
# satisfaction data (10-point items, many ties) and 300 random data sets with
# heavy ties for the peers, effects, accuracy and bottlenecks. Writes
# validation/nca_equivalence_results.csv and stops with an error if a check
# fails. Run from the package root after installing PLSsemLab and NCA:
#   Rscript validation/nca_equivalence.R
#
# ES: PLSsemLab calcula el NCA por su cuenta. Este script comprueba que cada
# número sea igual al de NCA::nca_analysis() con sus opciones por defecto:
# puntuaciones 0-100, tamaños del efecto CE-FDH y CR-FDH, precisión de
# CR-FDH, ambas tablas de cuellos de botella y los valores p de permutación.
# El paquete toma un desplazamiento por condición del generador de la sesión
# y siembra la permutación i con desplazamiento + i; sus métricas de pureza
# también consumen números aleatorios, así que la misma semilla no le da los
# mismos desplazamientos que al módulo. El script lee los desplazamientos del
# paquete y se los da a la prueba de permutación del módulo: con las mismas
# permutaciones ambos valores p deben ser idénticos.
# -----------------------------------------------------------------------------

suppressMessages({
    library(cSEM)
    library(NCA)
    library(PLSsemLab)
})
Sys.setenv(max.cores = 1, NCA_SKIP_PURITY = TRUE)

quiet <- function(expr) { out <- NULL; utils::capture.output(out <- suppressWarnings(expr)); out }
results <- list()
record <- function(scenario, check, ours, theirs, tol = 1e-10) {
    ours <- as.numeric(unlist(ours))
    theirs <- as.numeric(unlist(theirs))
    problem <- ""
    if (length(ours) != length(theirs)) {
        problem <- sprintf("length %d vs %d", length(ours), length(theirs))
    } else if (!identical(is.na(ours), is.na(theirs))) {
        problem <- "different missing positions"
    } else if (sum(!is.na(ours)) == 0 || any(!is.finite(ours[!is.na(ours)]))) {
        problem <- "non-finite or empty values"
    }
    diff <- if (nzchar(problem)) NA_real_ else max(abs(ours - theirs), na.rm = TRUE)
    results[[length(results) + 1]] <<- data.frame(scenario = scenario, check = check, n = length(ours),
                                                  max_abs_diff = signif(diff, 3), tolerance = tol,
                                                  pass = !nzchar(problem) && diff <= tol, problem = problem)
}
ns <- asNamespace("PLSsemLab")
mod <- function(name) get(name, envir = ns)

# The package's offsets, one per condition, read when p_test() returns.
# ES: Los desplazamientos del paquete, uno por condición.
OFFSETS <- integer(0)
suppressMessages(trace("p_test", where = asNamespace("NCA"), print = FALSE,
                       exit = quote(assign("OFFSETS", c(get("OFFSETS", globalenv()), offset), globalenv()))))

# Bottleneck values as numbers on both sides: NN -> -1, NA -> -2.
# ES: Cuellos de botella como números en ambos lados: NN -> -1, NA -> -2.
bnPkg <- function(col) {
    v <- as.character(col)
    out <- suppressWarnings(as.numeric(v))
    out[v == "NN"] <- -1
    out[v == "NA" | is.na(v)] <- -2
    out
}
bnMod <- function(v) { v[is.infinite(v)] <- -1; v[is.na(v)] <- -2; v }

# One outcome: the package with its defaults against the module's helpers.
# ES: Un resultado: el paquete con sus opciones por defecto frente al módulo.
compareOutcome <- function(sc, scores, outcome, conditions, reps, seed) {
    OFFSETS <<- integer(0)
    set.seed(seed)
    r <- quiet(nca_analysis(scores, conditions, outcome, ceilings = c("ce_fdh", "cr_fdh"), test.rep = reps))
    y <- scores[[outcome]]
    for (k in seq_along(conditions)) {
        cn <- conditions[k]
        x <- scores[[cn]]
        s <- c(range(x), range(y))
        area <- diff(s[1:2]) * diff(s[3:4])
        e <- mod(".pl_nca_effects")(x, y, s, area, full = TRUE)
        P <- r$summaries[[cn]]$params
        lab <- sprintf("%s -> %s", cn, outcome)
        record(sc, paste(lab, "effect size CE-FDH"), e$effects[["ce"]], P["Effect size", "ce_fdh"])
        record(sc, paste(lab, "effect size CR-FDH"), e$effects[["cr"]], P["Effect size", "cr_fdh"])
        record(sc, paste(lab, "accuracy CR-FDH"), e$accuracy, P["Ceiling accuracy", "cr_fdh"])
        b <- mod(".pl_nca_bottleneck")(e, s)
        # the package's bottleneck table prints one decimal
        # ES: la tabla de cuellos de botella del paquete imprime un decimal
        record(sc, paste(lab, "bottlenecks CE-FDH (package: 1 decimal)"), bnMod(b$ce),
               bnPkg(r$bottlenecks$ce_fdh[[cn]]), tol = 0.05 + 1e-9)
        record(sc, paste(lab, "bottlenecks CR-FDH (package: 1 decimal)"), bnMod(b$cr),
               bnPkg(r$bottlenecks$cr_fdh[[cn]]), tol = 0.05 + 1e-9)
        if (reps > 0) {
            pm <- mod(".pl_nca_permutation")(x, y, s, area, e$effects, reps, offset = OFFSETS[k], return_sims = TRUE)
            for (ce in c("ce", "cr")) {
                tst <- r$tests[[cn]][[paste0(ce, "_fdh")]]
                ms <- pm$sims[, ce]
                ms <- ms[!is.na(ms)]
                record(sc, sprintf("%s permuted effect sizes %s-FDH", lab, toupper(ce)), ms, tst$data, tol = 1e-12)
                # the p value from the package's own permuted effects with ties
                # within 1e-10 counted as at least as large (the package
                # compares raw floating-point values, so rounding noise can
                # drop exact ties)
                # ES: el valor p con los efectos permutados del propio
                # paquete, contando los empates dentro de 1e-10
                pref <- max((sum(tst$data >= tst$observed - 1e-10) + 1) / (length(tst$data) + 1), 1 / reps)
                record(sc, sprintf("%s p %s-FDH, same permutations", lab, toupper(ce)), pm$p[[ce]], pref)
                record(sc, sprintf("%s p %s-FDH, package's own count (informative)", lab, toupper(ce)), pm$p[[ce]],
                       P["p-value", paste0(ce, "_fdh")], tol = Inf)
            }
        }
    }
}

# The module through its R6 class, as jamovi runs it.
# ES: El módulo por su clase R6, como lo ejecuta jamovi.
cls <- get("plssemClass", envir = ns)
opts <- get("plssemOptions", envir = ns)
runModule <- function(data, ...) {
    a <- cls$new(options = opts$new(...), data = data)
    a$init()
    quiet(a$run())
    a
}
block <- function(label, vars) list(label = label, vars = vars)

# Unstandardized scores by hand: weights over the indicator SDs, normalized
# to sum 1, rescaled to 0-100 with the range of the scale.
# ES: Puntuaciones no estandarizadas a mano, reescaladas a 0-100.
handScores <- function(fit, data, range) {
    W <- fit$Estimates$Weight_estimates
    X <- as.matrix(data[, colnames(W)])
    S <- sapply(rownames(W), function(c) { w <- W[c, ] / apply(X, 2, stats::sd); X %*% (w / sum(w)) })
    as.data.frame(100 * (S - range[1]) / (range[2] - range[1]))
}

# -----------------------------------------------------------------------------
# 1. Corporate reputation (Hair et al.), the simple model, range 1-7.
# ES: 1. Reputación corporativa, el modelo simple, rango 1-7.
# -----------------------------------------------------------------------------
sc <- "1 corporate reputation"
path <- system.file("extdata", "plssemlab_corp_rep.csv", package = "PLSsemLab")
if (!nzchar(path)) path <- file.path("data", "plssemlab_corp_rep.csv")
corp <- utils::read.csv(path, stringsAsFactors = TRUE)
vars <- setdiff(names(corp), c("servicetype", "serviceprovider"))
corpMean <- corp[, vars]
for (v in names(corpMean)) corpMean[[v]][is.na(corpMean[[v]])] <- mean(corpMean[[v]], na.rm = TRUE)
refl <- list(block("COMP", paste0("comp_", 1:3)), block("LIKE", paste0("like_", 1:3)),
             block("CUSA", "cusa"), block("CUSL", paste0("cusl_", 1:3)))
a <- runModule(corp, reflective = refl, formative = list(),
               structural = list(list(), list(), list("COMP", "LIKE"), list("COMP", "LIKE", "CUSA")),
               missing = "mean", ncaRun = TRUE, ncaPerm = 499, ipmaRange = "manual", ipmaMin = 1, ipmaMax = 7,
               bootSeed = 2026)
f <- cSEM::csem(corpMean, "COMP <~ comp_1 + comp_2 + comp_3
                           LIKE <~ like_1 + like_2 + like_3
                           CUSA <~ cusa
                           CUSL <~ cusl_1 + cusl_2 + cusl_3
                           CUSA ~ COMP + LIKE
                           CUSL ~ COMP + LIKE + CUSA",
                .PLS_modes = list(COMP = "modeA", LIKE = "modeA", CUSA = "modeA", CUSL = "modeA"), .disattenuate = FALSE)
S <- handScores(f, corpMean, c(1, 7))
st <- a$results$ncaSection$ncaPlots$items
record(sc, "0-100 scores of every condition-outcome plot",
       unlist(lapply(st, function(i) c(i$state$x, i$state$y))),
       unlist(lapply(st, function(i) {
           k <- strsplit(i$key, " → ", fixed = TRUE)[[1]]
           c(S[[k[1]]], S[[k[2]]])
       })))
tb <- a$results$ncaSection$ncaTable$asDF
compareOutcome(sc, S, "CUSA", c("COMP", "LIKE"), 499, 2026)
compareOutcome(sc, S, "CUSL", c("COMP", "LIKE", "CUSA"), 499, 2026)
# the module's tables against the package's effect sizes (no permutations)
# ES: las tablas del módulo frente a los tamaños del efecto del paquete
dpk <- unlist(lapply(seq_len(nrow(tb)), function(k) {
    r <- quiet(nca_analysis(S, tb$condition[k], tb$target[k], ceilings = c("ce_fdh", "cr_fdh")))
    r$summaries[[tb$condition[k]]]$params["Effect size", if (tb$ceiling[k] == "CE-FDH") "ce_fdh" else "cr_fdh"]
}))
record(sc, "table: effect sizes", tb$d, dpk)
# the module's own p values repeat with the seed
# ES: los valores p del módulo se repiten con la semilla
a2 <- runModule(corp, reflective = refl, formative = list(),
                structural = list(list(), list(), list("COMP", "LIKE"), list("COMP", "LIKE", "CUSA")),
                missing = "mean", ncaRun = TRUE, ncaPerm = 499, ipmaRange = "manual", ipmaMin = 1, ipmaMax = 7,
                bootSeed = 2026)
record(sc, "table: p values repeat with the seed", a2$results$ncaSection$ncaTable$asDF$p, tb$p, tol = 0)

# -----------------------------------------------------------------------------
# 2. cSEM's satisfaction data (ECSI, 10-point items): many ties.
# ES: 2. Datos de satisfacción de cSEM (ítems de 10 puntos): muchos empates.
# -----------------------------------------------------------------------------
sc <- "2 satisfaction"
sat <- cSEM::satisfaction
fs <- cSEM::csem(sat, "IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
                       EXPE <~ expe1 + expe2 + expe3
                       QUAL <~ qual1 + qual2 + qual3 + qual4 + qual5
                       VAL <~ val1 + val2 + val3 + val4
                       SAT <~ sat1 + sat2 + sat3 + sat4
                       LOY <~ loy1 + loy2 + loy3 + loy4
                       EXPE ~ IMAG
                       QUAL ~ EXPE
                       VAL ~ EXPE + QUAL
                       SAT ~ IMAG + EXPE + QUAL + VAL
                       LOY ~ IMAG + SAT",
                 .PLS_modes = list(IMAG = "modeA", EXPE = "modeA", QUAL = "modeA", VAL = "modeA", SAT = "modeA",
                                   LOY = "modeA"), .disattenuate = FALSE)
S2 <- handScores(fs, sat, range(unlist(sat[, colnames(fs$Estimates$Weight_estimates)])))
compareOutcome(sc, S2, "SAT", c("IMAG", "EXPE", "QUAL", "VAL"), 299, 7)
compareOutcome(sc, S2, "LOY", c("IMAG", "SAT"), 299, 8)

# -----------------------------------------------------------------------------
# 3. 300 random data sets with heavy ties (5- to 7-point items, n 5 to 80):
# peers, effects, accuracy and bottlenecks; permutations in the first 20.
# ES: 3. 300 conjuntos aleatorios con muchos empates: picos, efectos,
# precisión y cuellos de botella; permutaciones en los primeros 20.
# -----------------------------------------------------------------------------
sc <- "3 random data with ties"
set.seed(2026)
for (i in 1:300) {
    n <- sample(5:80, 1)
    k <- sample(5:7, 1)
    x <- sample(1:k, n, replace = TRUE) + 0
    y <- pmin(k, pmax(1, x + sample(-k:2, n, replace = TRUE))) + 0
    if (length(unique(y)) < 2 || length(unique(x)) < 2) next
    d <- data.frame(X = x, Y = y)
    st <- .Random.seed
    compareOutcome(sprintf("%s (%d)", sc, i), d, "Y", "X", if (i <= 20) 99 else 0, i)
    assign(".Random.seed", st, envir = globalenv())
}

# -----------------------------------------------------------------------------
# Results: random data sets summarized per check.
# ES: Resultados: los conjuntos aleatorios resumidos por verificación.
# -----------------------------------------------------------------------------
out <- do.call(rbind, results)
rnd <- grepl("^3 ", out$scenario)
if (any(rnd)) {
    out$check[rnd] <- sub("^X -> Y ", "", out$check[rnd])
    agg <- do.call(rbind, lapply(split(out[rnd, ], out$check[rnd]), function(g)
        data.frame(scenario = sprintf("3 random data with ties (%d data sets)", nrow(g)), check = g$check[1],
                   n = sum(g$n), max_abs_diff = suppressWarnings(max(g$max_abs_diff, na.rm = TRUE)),
                   tolerance = g$tolerance[1], pass = all(g$pass),
                   problem = paste(unique(g$problem[nzchar(g$problem)]), collapse = "; "))))
    out <- rbind(out[!rnd, ], agg)
}
utils::write.csv(out, file.path("validation", "nca_equivalence_results.csv"), row.names = FALSE)
cat(sprintf("%d checks, %d values, %d failed (NCA %s, PLSsemLab %s)\n", nrow(out), sum(out$n), sum(!out$pass),
            utils::packageVersion("NCA"), utils::packageVersion("PLSsemLab")))
if (any(!out$pass)) {
    print(out[!out$pass, ], row.names = FALSE)
    stop("numerical equivalence with the NCA package failed")
}
