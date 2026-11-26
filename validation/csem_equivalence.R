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
# Numerical equivalence with cSEM.
# ES: Equivalencia numérica con cSEM.
#
# Every number PLSsemLab reports must equal the one cSEM returns for the same
# data and options. This script runs the module through its R6 class, as
# jamovi does, and the same analysis with direct cSEM calls written here by
# hand (not taken from the module), on the bundled corporate reputation data
# and on cSEM's satisfaction data. It writes
# validation/csem_equivalence_results.csv, one row per check with the largest
# absolute difference, and stops with an error if any check fails. Run from
# the package root after installing PLSsemLab:
#   Rscript validation/csem_equivalence.R
#
# ES: Cada número que informa PLSsemLab debe ser igual al que devuelve cSEM
# para los mismos datos y opciones. El script ejecuta el módulo por su clase
# R6, como jamovi, y el mismo análisis con llamadas directas a cSEM escritas
# aquí a mano (no tomadas del módulo). Escribe
# validation/csem_equivalence_results.csv, una fila por verificación con la
# mayor diferencia absoluta, y termina con error si alguna falla.
# -----------------------------------------------------------------------------

suppressMessages({
    library(cSEM)
    library(PLSsemLab)
})

quiet <- function(expr) { out <- NULL; utils::capture.output(out <- suppressWarnings(expr)); out }
results <- list()

# A check passes only when both sides have the same length, the same missing
# positions and finite values that agree within the tolerance; a length or
# NA mismatch is a failure, never a silently ignored difference (MokkenLab's
# rule).
# ES: Una verificación pasa solo si ambos lados tienen la misma longitud,
# las mismas posiciones perdidas y valores finitos dentro de la tolerancia.
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

# cSEM's bootstrap leaves L'Ecuyer-CMRG as the session's generator; the
# module restores R's default around every resampling routine, so direct
# calls run under the default generator too.
# ES: Las llamadas directas corren con el generador por defecto de R.
withDefaultRng <- function(expr) {
    old <- RNGkind()
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    on.exit(do.call(RNGkind, as.list(old)))
    expr
}

# The module through its R6 class, as jamovi runs it.
# ES: El módulo por su clase R6, como lo ejecuta jamovi.
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
meanReplaced <- function(d) {
    for (v in names(d)) if (is.numeric(d[[v]])) d[[v]][is.na(d[[v]])] <- mean(d[[v]], na.rm = TRUE)
    d
}
corpVars <- setdiff(names(corp), c("servicetype", "serviceprovider"))
corpMean <- meanReplaced(corp[, corpVars])
sat <- cSEM::satisfaction

# The corporate reputation models of Hair et al. as the option boxes hold them
# (the structural option: one list of predictors per construct, in the order
# of the constructs, reflective first).
# ES: Los modelos de reputación corporativa como los guardan las cajas.
reflSimple <- list(block("COMP", paste0("comp_", 1:3)), block("LIKE", paste0("like_", 1:3)),
                   block("CUSA", "cusa"), block("CUSL", paste0("cusl_", 1:3)))
formExt <- list(block("QUAL", paste0("qual_", 1:8)), block("PERF", paste0("perf_", 1:5)),
                block("CSOR", paste0("csor_", 1:5)), block("ATTR", paste0("attr_", 1:3)))
structSimple <- list(list(), list(), list("COMP", "LIKE"), list("COMP", "LIKE", "CUSA"))
structExt <- list(list("QUAL", "PERF", "CSOR", "ATTR"), list("QUAL", "PERF", "CSOR", "ATTR"),
                  list("COMP", "LIKE"), list("COMP", "LIKE", "CUSA"), list(), list(), list(), list())
syntaxSimple <- "COMP <~ comp_1 + comp_2 + comp_3
                 LIKE <~ like_1 + like_2 + like_3
                 CUSA <~ cusa
                 CUSL <~ cusl_1 + cusl_2 + cusl_3
                 CUSA ~ COMP + LIKE
                 CUSL ~ COMP + LIKE + CUSA"
modesSimple <- list(COMP = "modeA", LIKE = "modeA", CUSA = "modeA", CUSL = "modeA")
syntaxExt <- "QUAL <~ qual_1 + qual_2 + qual_3 + qual_4 + qual_5 + qual_6 + qual_7 + qual_8
              PERF <~ perf_1 + perf_2 + perf_3 + perf_4 + perf_5
              CSOR <~ csor_1 + csor_2 + csor_3 + csor_4 + csor_5
              ATTR <~ attr_1 + attr_2 + attr_3
              COMP <~ comp_1 + comp_2 + comp_3
              LIKE <~ like_1 + like_2 + like_3
              CUSA <~ cusa
              CUSL <~ cusl_1 + cusl_2 + cusl_3
              COMP ~ QUAL + PERF + CSOR + ATTR
              LIKE ~ QUAL + PERF + CSOR + ATTR
              CUSA ~ COMP + LIKE
              CUSL ~ COMP + LIKE + CUSA"
modesExt <- c(modesSimple, list(QUAL = "modeB", PERF = "modeB", CSOR = "modeB", ATTR = "modeB"))
pathsOf <- function(B, p) vapply(seq_len(nrow(p)), function(k) B[p$to[k], p$from[k]], 0)

# -----------------------------------------------------------------------------
# 1. Extended model, PLS: every estimate and quality criterion.
# ES: 1. Modelo extendido, PLS: cada estimación y criterio de calidad.
# -----------------------------------------------------------------------------
sc <- "1 extended, PLS"
a <- runModule(corp, reflective = reflSimple, formative = formExt, structural = structExt, missing = "mean")
f <- cSEM::csem(corpMean, syntaxExt, .PLS_modes = modesExt, .disattenuate = FALSE)
fc <- cSEM::csem(corpMean, gsub("(COMP|LIKE|CUSA|CUSL) <~", "\\1 =~", syntaxExt), .PLS_modes = modesExt[5:8],
                 .disattenuate = TRUE)
E <- f$Estimates
p <- tab(a, "structuralSection", "paths")
record(sc, "path coefficients", p$estimate, pathsOf(E$Path_estimates, p))
m <- tab(a, "measurementSection", "measurement")
record(sc, "loadings", m$loading, mapply(function(c, i) E$Loading_estimates[c, i], m$construct, m$indicator))
record(sc, "weights", m$weight, mapply(function(c, i) E$Weight_estimates[c, i], m$construct, m$indicator))
r2 <- tab(a, "structuralSection", "r2")
record(sc, "R2", r2$r2, E$R2[r2$construct])
record(sc, "adjusted R2", r2$r2adj, E$R2adj[r2$construct])
rel <- tab(a, "measurementSection", "reliability")
multi <- rel$construct != "CUSA"
record(sc, "Cronbach's alpha", rel$alpha[multi], cSEM::calculateRhoT(f, .only_common_factors = FALSE)[rel$construct[multi]])
record(sc, "rho_C", rel$rhoC[multi], cSEM::calculateRhoC(f, .only_common_factors = FALSE)[rel$construct[multi]])
record(sc, "AVE", rel$ave[multi], cSEM::calculateAVE(f, .only_common_factors = FALSE)[rel$construct[multi]])
record(sc, "rho_A (from the consistent estimation)", rel$rhoA[multi], fc$Estimates$Reliabilities[rel$construct[multi]])
h <- tab(a, "measurementSection", "htmt")
H <- cSEM::calculateHTMT(f, .only_common_factors = FALSE)$htmts
H2 <- cSEM::calculateHTMT(f, .type_htmt = "htmt2", .only_common_factors = FALSE)$htmts
hv <- function(M) mapply(function(x, y) max(M[x, y], M[y, x]), h$c1, h$c2)
record(sc, "HTMT", h$htmt, hv(H))
record(sc, "HTMT2", h$htmt2, hv(H2))
v <- tab(a, "measurementSection", "vif")
V <- cSEM::calculateVIFModeB(f)
record(sc, "VIF of formative indicators", v$vif, mapply(function(c, i) V[c, i], v$construct, v$indicator))
f2 <- tab(a, "structuralSection", "f2")
F2 <- cSEM::calculatef2(f)
record(sc, "f2", f2$f2, pathsOf(F2, f2))
vs <- tab(a, "structuralSection", "vifStruct")
record(sc, "structural VIF", vs$vif, mapply(function(fr, to) E$VIF[[to]][[fr]], vs$from, vs$to))
ef <- tab(a, "structuralSection", "effects")
eff <- quiet(cSEM::summarize(f))$Estimates$Effect_estimates
tot <- stats::setNames(eff$Total_effect$Estimate, as.character(eff$Total_effect$Name))
ind <- stats::setNames(eff$Indirect_effect$Estimate, as.character(eff$Indirect_effect$Name))
record(sc, "total effects", ef$total, tot[paste(ef$to, "~", ef$from)])
hasInd <- paste(ef$to, "~", ef$from) %in% names(ind)
record(sc, "indirect effects", ef$indirect[hasInd], ind[paste(ef$to, "~", ef$from)[hasInd]])

# -----------------------------------------------------------------------------
# 2. Simple model, consistent PLS.
# ES: 2. Modelo simple, PLS consistente.
# -----------------------------------------------------------------------------
sc <- "2 simple, PLSc"
a <- runModule(corp, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean",
               estimator = "plsc")
fc <- cSEM::csem(corpMean, gsub("<~", "=~", syntaxSimple), .disattenuate = TRUE)
p <- tab(a, "structuralSection", "paths")
record(sc, "path coefficients", p$estimate, pathsOf(fc$Estimates$Path_estimates, p))
m <- tab(a, "measurementSection", "measurement")
record(sc, "loadings", m$loading, mapply(function(c, i) fc$Estimates$Loading_estimates[c, i], m$construct, m$indicator))
rel <- tab(a, "measurementSection", "reliability")
multi <- rel$construct != "CUSA"
record(sc, "rho_A", rel$rhoA[multi], fc$Estimates$Reliabilities[rel$construct[multi]])
record(sc, "rho_C", rel$rhoC[multi], cSEM::calculateRhoC(fc)[rel$construct[multi]])

# -----------------------------------------------------------------------------
# 3. Bootstrap in blocks of 200 (block k uses seed + k - 1): the intervals of
# the paths equal the same blocks run directly and pooled.
# ES: 3. Bootstrap por bloques de 200 (el bloque k usa semilla + k - 1).
# -----------------------------------------------------------------------------
sc <- "3 simple, bootstrap 450 in blocks"
a <- runModule(corp, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean",
               bootstrap = TRUE, bootReps = 450, bootSeed = 5, ciLevel = 0.90)
f <- cSEM::csem(corpMean, syntaxSimple, .PLS_modes = modesSimple, .disattenuate = FALSE)
R <- 450
draws <- withDefaultRng(do.call(rbind, lapply(seq_len(ceiling(R / 200)), function(k) {
    b <- cSEM::resamplecSEMResults(f, .R = min(200, R - (k - 1) * 200), .seed = 5 + k - 1, .handle_inadmissibles = "drop")
    b$Estimates$Estimates_resample$Estimates1$Path_estimates$Resampled
})))
p <- tab(a, "structuralSection", "paths")
cols <- paste(p$to, "~", p$from)
record(sc, "number of draws", nrow(draws), R, tol = 0)
record(sc, "path SE", p$se, apply(draws[, cols, drop = FALSE], 2, stats::sd))
record(sc, "path 90% lower bound", p$lower, apply(draws[, cols, drop = FALSE], 2, stats::quantile, 0.05))
record(sc, "path 90% upper bound", p$upper, apply(draws[, cols, drop = FALSE], 2, stats::quantile, 0.95))

# -----------------------------------------------------------------------------
# 4. Mediation: the specific indirect effect of a single chain equals cSEM's
# indirect effect for that pair.
# ES: 4. Mediación: el efecto indirecto de una cadena única es el de cSEM.
# -----------------------------------------------------------------------------
sc <- "4 simple, mediation"
med <- tab(a, "structuralSection", "mediation")
eff <- quiet(cSEM::summarize(f))$Estimates$Effect_estimates$Indirect_effect
ind <- stats::setNames(eff$Estimate, as.character(eff$Name))
ends <- strsplit(med$chain, " → ", fixed = TRUE)
record(sc, "specific indirect effects", med$indirect,
       vapply(ends, function(x) prod(vapply(seq_len(length(x) - 1), function(k)
           f$Estimates$Path_estimates[x[k + 1], x[k]], 0)), 0))
single <- vapply(ends, function(x) sum(vapply(ends, function(y) y[1] == x[1] && y[length(y)] == x[length(x)], TRUE)) == 1, TRUE)
record(sc, "single chains equal cSEM's indirect effect", med$indirect[single],
       vapply(ends[single], function(x) ind[[paste(x[length(x)], "~", x[1])]], 0))

# -----------------------------------------------------------------------------
# 5. Overall model fit: testOMF() for the estimated and the saturated model.
# ES: 5. Ajuste global: testOMF() del modelo estimado y del saturado.
# -----------------------------------------------------------------------------
sc <- "5 satisfaction, overall fit"
dsat <- sat[, c(paste0("imag", 1:5), paste0("sat", 1:4), paste0("loy", 1:4))]
reflSat <- list(block("IMAG", paste0("imag", 1:5)), block("SAT", paste0("sat", 1:4)), block("LOY", paste0("loy", 1:4)))
syntaxSat <- "IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
              SAT <~ sat1 + sat2 + sat3 + sat4
              LOY <~ loy1 + loy2 + loy3 + loy4
              SAT ~ IMAG
              LOY ~ SAT + IMAG"
modesSat <- list(IMAG = "modeA", SAT = "modeA", LOY = "modeA")
a <- runModule(dsat, reflective = reflSat, formative = list(), structural = list(list(), list("IMAG"), list("SAT", "IMAG")),
               fitTest = TRUE, fitReps = 199, bootSeed = 7)
fs <- cSEM::csem(dsat, syntaxSat, .PLS_modes = modesSat, .disattenuate = FALSE)
ft <- tab(a, "fitSection", "fit")
for (s in c(FALSE, TRUE)) {
    t <- withDefaultRng(quiet(cSEM::testOMF(fs, .R = 199, .seed = 7, .alpha = 0.05, .saturated = s)))
    rows <- ft[if (s) 5:8 else 1:4, ]
    lab <- if (s) "saturated" else "estimated"
    record(sc, paste("discrepancies,", lab), rows$value, t$Test_statistic[c("SRMR", "dG", "dL", "dML")])
    record(sc, paste("critical values,", lab), rows$critical, t$Critical_value[c("SRMR", "dG", "dL", "dML"), 1])
}

# -----------------------------------------------------------------------------
# 6. PLSpredict: one repetition equals cSEM::predict with the same seed.
# ES: 6. PLSpredict: una repetición es cSEM::predict con la misma semilla.
# -----------------------------------------------------------------------------
sc <- "6 satisfaction, PLSpredict"
a <- runModule(dsat, reflective = reflSat, formative = list(), structural = list(list(), list("IMAG"), list("SAT", "IMAG")),
               predictRun = TRUE, predictFolds = 10, predictReps = 1, bootSeed = 11)
pt <- tab(a, "predictSection", "predict")
pm <- withDefaultRng(quiet(cSEM::predict(fs, .benchmark = "lm", .cv_folds = 10, .r = 1, .seed = 11)))$Prediction_metrics
pm <- pm[match(pt$indicator, pm$Name), ]
record(sc, "Q2 predict", pt$q2, pm$Q2_predict, tol = 1e-8)
record(sc, "RMSE PLS", pt$rmsePls, pm$RMSE_target, tol = 1e-8)
record(sc, "MAE PLS", pt$maePls, pm$MAE_target, tol = 1e-8)
# LM benchmark (Shmueli et al., 2019): stats::lm with an intercept on the
# exogenous indicators, on cSEM's folds. cSEM 0.7.1's predict(.benchmark =
# "lm") omits the intercept, so the module computes LM itself.
# ES: Referencia LM: stats::lm con intercepto en las particiones de cSEM;
# predict(.benchmark = "lm") de cSEM 0.7.1 omite el intercepto.
folds <- withDefaultRng(cSEM::resampleData(fs, .resample_method = "cross-validation", .cv_folds = 10, .R = 1,
                                           .seed = 11))[[1]]
endo <- c(paste0("sat", 1:4), paste0("loy", 1:4))
res <- matrix(NA, nrow(dsat), length(endo), dimnames = list(NULL, endo))
for (k in seq_along(folds)) {
    te <- match(rownames(folds[[k]]), rownames(dsat))
    fit_k <- stats::lm(as.matrix(dsat[-te, endo]) ~ ., data = dsat[-te, paste0("imag", 1:5)])
    res[te, ] <- as.matrix(dsat[te, endo]) - stats::predict(fit_k, newdata = dsat[te, paste0("imag", 1:5)])
}
record(sc, "RMSE LM (stats::lm with intercept)", pt$rmseLm, sqrt(colMeans(res^2))[pt$indicator], tol = 1e-8)
record(sc, "MAE LM (stats::lm with intercept)", pt$maeLm, colMeans(abs(res))[pt$indicator], tol = 1e-8)

# -----------------------------------------------------------------------------
# 7. IPMA: importance is doIPMA()'s; performance its rescaling to 0-100.
# ES: 7. IPMA: la importancia es la de doIPMA(); el desempeño, en 0-100.
# -----------------------------------------------------------------------------
sc <- "7 simple, IPMA"
a <- runModule(corp, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean",
               ipma = TRUE, ipmaRange = "manual", ipmaMin = 1, ipmaMax = 7)
f <- cSEM::csem(corpMean, syntaxSimple, .PLS_modes = modesSimple, .disattenuate = FALSE)
ip <- quiet(cSEM::doIPMA(f))
it <- tab(a, "ipmaSection", "ipmaTable")
record(sc, "importance", it$importance, mapply(function(t, c) ip$Construct$Importance[t, c], it$target, it$construct))
record(sc, "performance (0-100)", it$performance, 100 * (ip$Construct$Performance[it$construct] - 1) / 6)

# -----------------------------------------------------------------------------
# 8. Multigroup analysis: MICOM and the path differences by service type.
# ES: 8. Análisis multigrupo: MICOM y diferencias de rutas por tipo de servicio.
# -----------------------------------------------------------------------------
sc <- "8 simple, multigroup"
dg <- corp
dg$servicetype <- factor(dg$servicetype)
a <- runModule(dg, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean",
               groupVar = "servicetype", mgaRun = TRUE, mgaPerm = 199, mgaBoot = 199, bootSeed = 11, mgcfa = TRUE)
dd <- meanReplaced(dg[, c(corpVars, "servicetype")])
dd$PLgroup <- sprintf("G%02d", as.integer(dd$servicetype))
dd$servicetype <- NULL
fg <- cSEM::csem(dd[, c(paste0("comp_", 1:3), paste0("like_", 1:3), "cusa", paste0("cusl_", 1:3), "PLgroup")],
                 syntaxSimple, .id = "PLgroup", .PLS_modes = modesSimple, .disattenuate = FALSE)
mi <- withDefaultRng(quiet(cSEM::testMICOM(fg, .R = 199, .seed = 11, .verbose = FALSE)))
mt <- tab(a, "mgaSection", "micom")
record(sc, "MICOM c", mt$c, mi$Step2$Test_statistic[[1]][mt$construct])
record(sc, "MICOM p of c", mt$pC, mi$Step2$P_value$none[[1]][mt$construct])
record(sc, "MICOM mean difference", mt$meanDiff, mi$Step3$Mean$Test_statistic[[1]][mt$construct])
record(sc, "MICOM log variance ratio", mt$logVar, mi$Step3$Var$Test_statistic[[1]][mt$construct])
mg <- withDefaultRng(quiet(cSEM::testMGD(fg, .approach_mgd = c("Henseler", "Chin"), .R_permutation = 199,
                                         .R_bootstrap = 199, .seed = 11, .verbose = FALSE,
                                         .parameters_to_compare = "CUSA ~ COMP + LIKE\nCUSL ~ COMP + LIKE + CUSA")))
dt <- tab(a, "mgaSection", "mga")
record(sc, "path differences", dt$diff, mg$Chin$Test_statistic[[1]])
record(sc, "permutation p", dt$pPerm, mg$Chin$P_value$none[[1]])
record(sc, "PLS-MGA P(first <= second)", dt$pOne, mg$Henseler$P_value$none[[1]])
record(sc, "PLS-MGA two-sided p = 2 min(P, 1 - P)", dt$pHens, pmin(1, 2 * pmin(dt$pOne, 1 - dt$pOne)))
# Multigroup CFA of the reflective constructs with two or more indicators
# (1.1.0), lavaan with MLR, written here by hand
# ES: AFC multigrupo de los constructos reflectivos (1.1.0), lavaan con MLR
cfaSyn <- "COMP =~ comp_1 + comp_2 + comp_3\nLIKE =~ like_1 + like_2 + like_3\nCUSL =~ cusl_1 + cusl_2 + cusl_3"
cf <- lapply(list(character(0), "loadings", c("loadings", "intercepts")), function(eq)
    lavaan::cfa(cfaSyn, data = dd, group = "PLgroup", estimator = "MLR", meanstructure = TRUE, group.equal = eq))
ct <- tab(a, "mgaSection", "mgcfa")
fmc <- sapply(cf, lavaan::fitMeasures, c("chisq.scaled", "df.scaled", "cfi.robust", "rmsea.robust", "srmr"))
record(sc, "MGCFA scaled chi-square", ct$chisq, fmc["chisq.scaled", ])
record(sc, "MGCFA df", ct$df, fmc["df.scaled", ])
record(sc, "MGCFA robust CFI, robust RMSEA, SRMR", c(ct$cfi, ct$rmsea, ct$srmr),
       c(fmc["cfi.robust", ], fmc["rmsea.robust", ], fmc["srmr", ]))
record(sc, "MGCFA scaled chi-square differences",
       ct$dchisq[2:3], c(lavaan::lavTestLRT(cf[[1]], cf[[2]])[["Chisq diff"]][2],
                         lavaan::lavTestLRT(cf[[2]], cf[[3]])[["Chisq diff"]][2]))

# -----------------------------------------------------------------------------
# 9. Higher-order construct, disjoint two-stage approach (Sarstedt et al.,
# 2019): stage 2 equals two direct cSEM stages.
# ES: 9. Constructo de orden superior, enfoque disjunto en dos etapas.
# -----------------------------------------------------------------------------
sc <- "9 satisfaction, higher-order (disjoint)"
a <- runModule(sat, reflective = list(block("SAT", paste0("sat", 1:4)), block("LOY", paste0("loy", 1:4))),
               formative = list(block("IMAG", paste0("imag", 1:5)), block("EXPE", paste0("expe", 1:5))),
               reflectiveHoc = list(list(label = "QUAL", components = list("IMAG", "EXPE"))),
               structural = list(list("QUAL"), list("SAT", "QUAL"), list(), list(), list()))
f1 <- cSEM::csem(sat, "SAT <~ sat1 + sat2 + sat3 + sat4
                       LOY <~ loy1 + loy2 + loy3 + loy4
                       IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
                       EXPE <~ expe1 + expe2 + expe3 + expe4 + expe5
                       SAT ~ IMAG + EXPE
                       LOY ~ SAT + IMAG + EXPE",
                 .PLS_modes = list(SAT = "modeA", LOY = "modeA", IMAG = "modeB", EXPE = "modeB"), .disattenuate = FALSE)
s2 <- as.data.frame(f1$Estimates$Construct_scores)
names(s2) <- paste0("s_", names(s2))
s2 <- cbind(sat[, c(paste0("sat", 1:4), paste0("loy", 1:4))], s2)
f2 <- cSEM::csem(s2, "QUAL <~ s_IMAG + s_EXPE
                      SAT <~ sat1 + sat2 + sat3 + sat4
                      LOY <~ loy1 + loy2 + loy3 + loy4
                      SAT ~ QUAL
                      LOY ~ SAT + QUAL",
                 .PLS_modes = list(QUAL = "modeA", SAT = "modeA", LOY = "modeA"), .disattenuate = FALSE)
p <- tab(a, "structuralSection", "paths")
record(sc, "stage-2 paths", p$estimate, pathsOf(f2$Estimates$Path_estimates, p))
hm <- tab(a, "measurementSection", "hocMeasurement")
record(sc, "higher-order loadings", hm$loading, f2$Estimates$Loading_estimates["QUAL", c("s_IMAG", "s_EXPE")])
record(sc, "higher-order weights", hm$weight, f2$Estimates$Weight_estimates["QUAL", c("s_IMAG", "s_EXPE")])
hr <- tab(a, "measurementSection", "hocReliability")
record(sc, "higher-order rho_C", hr$rhoC, cSEM::calculateRhoC(f2, .only_common_factors = FALSE)["QUAL"])
record(sc, "higher-order AVE", hr$ave, cSEM::calculateAVE(f2, .only_common_factors = FALSE)["QUAL"])
record(sc, "R2", tab(a, "structuralSection", "r2")$r2, f2$Estimates$R2[c("SAT", "LOY")])

# -----------------------------------------------------------------------------
# 10. Two-way interaction (two-stage product term, cSEM's default).
# ES: 10. Interacción de dos vías (término producto en dos etapas).
# -----------------------------------------------------------------------------
sc <- "10 satisfaction, interaction"
a <- runModule(dsat, reflective = reflSat, formative = list(),
               structural = list(list(), list(), list("SAT", "IMAG", c("SAT", "IMAG"))))
fi <- cSEM::csem(dsat, "IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
                        SAT <~ sat1 + sat2 + sat3 + sat4
                        LOY <~ loy1 + loy2 + loy3 + loy4
                        LOY ~ SAT + IMAG + SAT.IMAG",
                 .PLS_modes = modesSat, .disattenuate = FALSE)
p <- tab(a, "structuralSection", "paths")
from <- sub(" × ", ".", p$from, fixed = TRUE)
record(sc, "paths with the interaction term", p$estimate,
       mapply(function(fr, to) fi$Estimates$Path_estimates[to, fr], from, p$to))
f2t <- tab(a, "structuralSection", "f2")
record(sc, "f2 with the interaction term", f2t$f2,
       mapply(function(fr, to) cSEM::calculatef2(fi)[to, fr], sub(" × ", ".", f2t$from, fixed = TRUE), f2t$to))
record(sc, "R2", tab(a, "structuralSection", "r2")$r2, fi$Estimates$R2["LOY"])

# -----------------------------------------------------------------------------
# 11. Version 1.2.0: weighting approaches (GSCA, unit weights), jackknife
# intervals, Mardia's measures, the comparison with a competing model and the
# extra PLSpredict benchmark, against direct calls.
# ES: 11. Versión 1.2.0: enfoques de ponderación, intervalos jackknife,
# medidas de Mardia, comparación con un modelo competidor y referencia
# adicional de PLSpredict, frente a llamadas directas.
# -----------------------------------------------------------------------------
for (ap in c("gsca", "unit")) {
    sc <- sprintf("11 simple, weighting approach %s", ap)
    a <- runModule(corp, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean",
                   weightApproach = ap)
    f <- cSEM::csem(corpMean, syntaxSimple, .approach_weights = if (ap == "gsca") "GSCA" else "unit", .disattenuate = FALSE)
    pt <- tab(a, "structuralSection", "paths")
    record(sc, "path coefficients", pt$estimate, mapply(function(fr, to) f$Estimates$Path_estimates[to, fr], pt$from, pt$to))
    lt <- tab(a, "measurementSection", "measurement")
    record(sc, "loadings", lt$loading, mapply(function(cn, v) f$Estimates$Loading_estimates[cn, v], lt$construct, lt$indicator))
}
sc <- "11 simple, jackknife"
a <- runModule(corp, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean",
               bootstrap = TRUE, resampleMethod = "jackknife")
f <- cSEM::csem(corpMean, syntaxSimple, .PLS_modes = modesSimple, .disattenuate = FALSE)
j <- quiet(cSEM::resamplecSEMResults(f, .resample_method = "jackknife"))
pt <- tab(a, "structuralSection", "paths")
record(sc, "jackknife SE of the paths = cSEM's sd", pt$se, unname(cSEM::infer(j, .quantity = "sd")$Path_estimates$sd))
pe <- j$Estimates$Estimates_resample$Estimates1$Path_estimates
nj <- nrow(pe$Resampled)
record(sc, "t interval: estimate - t(n - 1) SE", pt$lower,
       unname(pe$Original - stats::qt(0.975, nj - 1) * cSEM::infer(j, .quantity = "sd")$Path_estimates$sd))
sc <- "11 simple, Mardia"
a <- runModule(corp, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean", mardia = TRUE)
mt <- tab(a, "measurementSection", "mardia")
X <- corpMean[, c(paste0("comp_", 1:3), paste0("like_", 1:3), "cusa", paste0("cusl_", 1:3))]
ps <- psych::mardia(X, plot = FALSE)
nm <- nrow(X)
record(sc, "b1 and b2 (psych::mardia, denominator n - 1, rescaled)", mt$value * ((nm - 1) / nm)^c(3, 2), c(ps$b1p, ps$b2p))
sc <- "11 simple, model comparison"
a <- runModule(corp, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean",
               compareRun = TRUE, cvpatDrop = list("COMP \u2192 CUSL"))
ct <- tab(a, "compareSection", "compare")
f2 <- cSEM::csem(corpMean, sub("CUSL ~ COMP + LIKE + CUSA", "CUSL ~ LIKE + CUSA", syntaxSimple, fixed = TRUE),
                 .PLS_modes = modesSimple, .disattenuate = FALSE)
c1 <- cSEM::calculateModelSelectionCriteria(f, .by_equation = TRUE, .only_structural = TRUE)
c2 <- cSEM::calculateModelSelectionCriteria(f2, .by_equation = TRUE, .only_structural = TRUE)
k <- ct$construct == "CUSL" & grepl("^(BIC|GM)", ct$criterion)
record(sc, "BIC and GM of CUSL, main model", ct$main[k], c(c1$BIC["CUSL"], c1$GM["CUSL"]))
record(sc, "BIC and GM of CUSL, competing model", as.numeric(ct$competing[k]), c(c2$BIC["CUSL"], c2$GM["CUSL"]))
record(sc, "SRMR of both models", c(ct$main[ct$criterion == "SRMR"], as.numeric(ct$competing[ct$criterion == "SRMR"])),
       c(cSEM::calculateSRMR(f), cSEM::calculateSRMR(f2)))
sc <- "11 simple, PLSpredict GSCA benchmark"
# Same data as cSEM (corpMean, no missing values): since 1.2.0 the module fills each
# fold from its training means, so the raw data with NAs would not be comparable.
# ES: Mismos datos que cSEM (corpMean, sin NA): desde 1.2.0 el módulo imputa cada
# partición con las medias de su entrenamiento, así que los datos con NA no son comparables.
a <- runModule(corpMean, reflective = reflSimple, formative = list(), structural = structSimple, missing = "mean",
               predictRun = TRUE, predictFolds = 5, predictReps = 1, predictBenchmark = "gsca", bootSeed = 2026)
pr <- tab(a, "predictSection", "predict")
# cSEM 0.7.1 applies the benchmark only in its cross-validation mode
# (with .test_data it returns the target residuals as the benchmark's)
# ES: cSEM 0.7.1 aplica la referencia solo en su modo de validación cruzada
ref <- withDefaultRng(quiet(cSEM::predict(f, .benchmark = "GSCA", .cv_folds = 5, .r = 1, .seed = 2026)))$Prediction_metrics
record(sc, "RMSE and MAE of the GSCA benchmark", c(pr$rmseBench, pr$maeBench),
       c(ref$RMSE_benchmark[match(pr$indicator, ref$Name)], ref$MAE_benchmark[match(pr$indicator, ref$Name)]), tol = 1e-8)

# -----------------------------------------------------------------------------
# Results.
# ES: Resultados.
# -----------------------------------------------------------------------------
out <- do.call(rbind, results)
utils::write.csv(out, file.path("validation", "csem_equivalence_results.csv"), row.names = FALSE)
cat(sprintf("%d checks, %d values, %d failed (cSEM %s, PLSsemLab %s)\n", nrow(out), sum(out$n), sum(!out$pass),
            utils::packageVersion("cSEM"), utils::packageVersion("PLSsemLab")))
if (any(!out$pass)) {
    print(out[!out$pass, ], row.names = FALSE)
    stop("numerical equivalence with cSEM failed")
}
