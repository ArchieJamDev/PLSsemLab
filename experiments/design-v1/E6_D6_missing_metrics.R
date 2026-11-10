# -----------------------------------------------------------------------------
# E6 — D6: measures JASP's documentation does not show.
# f2, Q2/PLSpredict, CVPAT, mediation (indirect effects with bootstrap CI),
# IPMA, and multigroup comparison with MICOM. Source: JASP's help file
# (Oct 2024); to be confirmed in JASP 0.99.
# Question: does each one run on the reference data and return what the
# report needs?
# ES: E6 — D6: medidas que la documentación de JASP no muestra.
# ES: Pregunta: ¿cada una funciona con los datos de referencia y devuelve lo
# que necesita el informe?
# -----------------------------------------------------------------------------
source("00_setup.R")
fit <- csem(corp_meanrep, model_extended, .disattenuate = FALSE)

# 1. f2 effect sizes.
# ES: 1. Tamaños de efecto f2.
r <- run_case("f2", calculatef2(fit))
if (r$ok) { idx <- which(r$value != 0, arr.ind = TRUE)
  print(data.frame(path = paste(colnames(r$value)[idx[, 2]], "->", rownames(r$value)[idx[, 1]]),
                   f2 = round(r$value[idx], 3))) }

# 2. PLSpredict: Q2_predict, RMSE and MAE against the linear-model benchmark.
# ES: 2. PLSpredict: Q2_predict, RMSE y MAE frente al modelo lineal de referencia.
# FINDING: cSEM accepts a seed only with one repetition (.r = 1). For several
# repetitions the module would set the seed itself before the call.
# ES: HALLAZGO: cSEM acepta semilla solo con una repetición (.r = 1). Para
# varias, el módulo fijaría la semilla antes de la llamada.
run_case("predict with .r = 10 and a seed", predict(fit, .r = 10, .seed = SEED))
p <- run_case("predict (10 folds, 10 repetitions, set.seed outside)",
              { set.seed(SEED); predict(fit, .benchmark = "lm", .cv_folds = 10, .r = 10) })
if (p$ok) { pm <- p$value$Prediction_metrics
  cat("  columns:", paste(names(pm), collapse = ", "), "\n"); print(head(pm, 12)) }

# 3. CVPAT: does the established model predict better than a competing one?
# ES: 3. CVPAT: ¿el modelo establecido predice mejor que uno alternativo?
model_competing <- sub("CUSL ~ COMP \\+ LIKE \\+ CUSA", "CUSL ~ CUSA", model_extended)
fit_comp <- csem(corp_meanrep, model_competing, .disattenuate = FALSE)
cv <- run_case("testCVPAT established vs competing",
               testCVPAT(fit, fit_comp, .seed = SEED, .cv_folds = 10))
if (cv$ok) print(cv$value)

# 4. Mediation: indirect effects with bootstrap percentile CI.
# ES: 4. Mediación: efectos indirectos con IC percentil bootstrap.
fb <- csem(corp_meanrep, model_extended, .disattenuate = FALSE,
           .resample_method = "bootstrap", .R = 999, .seed = SEED)
s <- run_case("indirect + total effects (R = 999)", summarize(fb))
if (s$ok) { ie <- s$value$Estimates$Effect_estimates$Indirect_effect
  cat("  columns:", paste(names(ie), collapse = ", "), "\n")
  print(ie[ie$Name %in% c("CUSL ~ COMP", "CUSL ~ LIKE", "CUSA ~ QUAL"),
           c("Name", "Estimate", "Std_err", grep("CI_percentile", names(ie), value = TRUE))]) }

# 5. IPMA: importance (total effects, unstandardized) x performance (0-100).
# ES: 5. IPMA: importancia (efectos totales) x desempeño (0-100).
ip <- run_case("doIPMA", doIPMA(fit))
# FINDING: performance comes on the items' own metric (here 1-7), not 0-100;
# the module rescales with the scale range the user declares (as MokkenLab's
# score range option). Importance = unstandardized total effects on the target.
# ES: HALLAZGO: el desempeño viene en la métrica de los ítems (aquí 1-7), no
# 0-100; el módulo reescala con el rango que declare el usuario.
if (ip$ok) {
  perf <- ip$value$Construct$Performance
  imp  <- ip$value$Construct$Importance["CUSL", ]
  tab <- data.frame(construct = names(perf), importance_on_CUSL = round(imp[names(perf)], 3),
                    performance_raw = round(perf, 2),
                    performance_0_100 = round((perf - 1) / (7 - 1) * 100, 1))
  print(tab[tab$importance_on_CUSL != 0, ], row.names = FALSE)
}

# 6. Multigroup: MICOM (steps 2 and 3) and MGD, by type of service.
# ES: 6. Multigrupo: MICOM (pasos 2 y 3) y MGD, por tipo de servicio.
dg <- corp_meanrep
fg <- csem(dg, model_simple, .id = "servicetype", .disattenuate = FALSE)
cat("Groups:", names(fg), "| n per group:", paste(table(dg$servicetype), collapse = " / "), "\n")
mi <- run_case("testMICOM (R = 499)", testMICOM(fg, .R = 499, .seed = SEED))
if (mi$ok) print(mi$value)
# FINDING: .output_type = "structured" fails in cSEM 0.7.1 ("Column `CI_type`
# not found"); the default "complete" output works and is read directly.
# ES: HALLAZGO: .output_type = "structured" falla en cSEM 0.7.1; la salida
# "complete" (por defecto) funciona y se lee directamente.
run_case("testMGD structured output (known failure)",
         testMGD(fg, .approach_mgd = "Chin", .R_permutation = 49, .R_bootstrap = 49,
                 .seed = SEED, .output_type = "structured", .verbose = FALSE))
mg <- run_case("testMGD Henseler + Chin (R = 499)",
               testMGD(fg, .approach_mgd = c("Henseler", "Chin"), .R_permutation = 499,
                       .R_bootstrap = 499, .seed = SEED, .verbose = FALSE))
if (mg$ok) {
  ch <- mg$value$Chin
  print(data.frame(path = names(ch$Test_statistic$`1_2`),
                   difference = round(ch$Test_statistic$`1_2`, 3),
                   p_Chin = round(ch$P_value$none$`1_2`, 3),
                   p_Henseler = round(mg$value$Henseler$P_value$none$`1_2`[names(ch$Test_statistic$`1_2`)], 3)),
        row.names = FALSE)
}
