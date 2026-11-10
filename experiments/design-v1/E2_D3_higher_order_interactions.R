# -----------------------------------------------------------------------------
# E2 — D3: higher-order constructs and interactions between constructs.
# User reports in JASP: issue #2955 (errors with files attached), #3007.
# Root cause named by a user: cSEM's assess() refuses nonlinear terms.
# Question: which quality criteria still work if each one is computed on its
# own, for a second-order model and for a model with an interaction?
# ES: E2 — D3: constructos de orden superior e interacciones.
# ES: Pregunta: ¿qué criterios de calidad siguen funcionando si se calcula
# cada uno por separado, en un modelo de segundo orden y con interacción?
# -----------------------------------------------------------------------------
source("00_setup.R")

# Each quality criterion the v1 report needs, computed one by one.
# ES: Cada criterio de calidad que necesita el informe de la v1, uno por uno.
criteria <- list(
  R2        = function(f) f$Estimates$R2,
  f2        = function(f) calculatef2(f),
  rhoC      = function(f) calculateRhoC(f),
  alpha     = function(f) calculateRhoT(f),
  AVE       = function(f) calculateAVE(f),
  HTMT      = function(f) calculateHTMT(f)$htmts,
  VIF       = function(f) assess(f, .quality_criterion = "vif")$VIF,
  VIFModeB  = function(f) calculateVIFModeB(f),
  SRMR      = function(f) calculateSRMR(f),
  dG        = function(f) calculateDG(f),
  dL        = function(f) calculateDL(f),
  effects   = function(f) summarize(f)$Estimates$Effect_estimates$Indirect_effect,
  assess_all = function(f) assess(f))

check_all <- function(fit, label) {
  cat("\n==", label, "== class:", paste(class(fit), collapse = ", "), "\n")
  # For a 2nd-order model cSEM returns two stages; criteria apply to each.
  # ES: Para segundo orden cSEM devuelve dos etapas; los criterios aplican a cada una.
  targets <- if (inherits(fit, "cSEMResults_2ndorder"))
    list(first_stage = fit$First_stage, second_stage = fit$Second_stage) else list(model = fit)
  res <- list()
  for (tn in names(targets)) for (cn in names(criteria)) {
    r <- run_case(paste(tn, cn, sep = " / "), criteria[[cn]](targets[[tn]]))
    res[[paste(tn, cn)]] <- r$ok
  }
  # Also the whole object, as JASP passes it.
  # ES: También el objeto completo, como lo pasa JASP.
  if (length(targets) > 1) run_case("whole object / assess_all", assess(fit))
  invisible(res)
}

# A. Second-order construct (two-stage approach), satisfaction data.
# ES: A. Constructo de segundo orden (enfoque en dos etapas), datos satisfaction.
model_2nd <- "
  IMAG <~ imag1 + imag2 + imag3 + imag4 + imag5
  EXPE <~ expe1 + expe2 + expe3 + expe4 + expe5
  QUAL =~ IMAG + EXPE
  SAT  =~ sat1 + sat2 + sat3 + sat4
  LOY  =~ loy1 + loy2 + loy3 + loy4
  SAT ~ QUAL
  LOY ~ SAT + QUAL
"
f2nd <- run_case("Estimate 2nd-order model", csem(satisfaction, model_2nd))
if (f2nd$ok) {
  cat("Admissible:", all(!unlist(verify(f2nd$value))), "\n")
  print(round(f2nd$value$Second_stage$Estimates$Path_estimates, 3))
  check_all(f2nd$value, "second-order")
  bs <- run_case("Bootstrap 2nd-order (R = 199)",
                 resamplecSEMResults(f2nd$value, .R = 199, .seed = SEED))
  if (bs$ok) print(summarize(bs$value)$Second_stage$Estimates$Path_estimates[, c("Name", "Estimate", "p_value")])
}

# B. Interaction (two-way, product-indicator-free: cSEM's "sequential" approach).
# ES: B. Interacción de dos vías (enfoque secuencial de cSEM).
model_int <- "
  IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
  SAT  =~ sat1 + sat2 + sat3 + sat4
  LOY  =~ loy1 + loy2 + loy3 + loy4
  LOY ~ IMAG + SAT + IMAG.SAT
"
fint <- run_case("Estimate interaction model", csem(satisfaction, model_int))
if (fint$ok) {
  print(round(fint$value$Estimates$Path_estimates, 3))
  check_all(fint$value, "interaction")
  # Simple slopes / floodlight analysis for the teaching plot.
  # ES: Pendientes simples / análisis floodlight para el gráfico didáctico.
  bi <- run_case("Bootstrap interaction (R = 199)",
                 csem(satisfaction, model_int, .resample_method = "bootstrap", .R = 199, .seed = SEED))
  if (bi$ok) {
    nl <- run_case("doNonlinearEffectsAnalysis (floodlight)",
                   doNonlinearEffectsAnalysis(bi$value, .dependent = "LOY",
                                              .independent = "SAT", .moderator = "IMAG"))
    if (nl$ok) {
      # Simple effect of SAT on LOY at IMAG = -2..+2 SD: one row per moderator value.
      # ES: Efecto simple de SAT sobre LOY con IMAG = -2..+2 DE.
      fl <- nl$value$out_floodlight
      out <- as.data.frame(fl$out)
      cat("  floodlight columns:", paste(names(out), collapse = ", "), "\n")
      print(round(out[round(seq(1, nrow(out), length.out = 5)), ], 3))
      cat("  Johnson-Neyman points:\n"); print(fl$Johnson_Neyman_points)
    }
  }

  # C. Workarounds for the criteria that fail with nonlinear terms.
  # ES: C. Alternativas para los criterios que fallan con términos no lineales.
  f <- fint$value
  run_case("rhoC with .model_implied = FALSE", print(round(calculateRhoC(f, .model_implied = FALSE), 3)))
  # Structural VIF from construct scores: regress each predictor of LOY on the others.
  # ES: VIF estructural desde las puntuaciones: cada predictor de LOY sobre los demás.
  run_case("Structural VIF from construct scores", {
    sc <- as.data.frame(f$Estimates$Construct_scores)
    sc$IMAG.SAT <- sc$IMAG * sc$SAT
    preds <- c("IMAG", "SAT", "IMAG.SAT")
    print(round(sapply(preds, function(p) {
      r2 <- summary(lm(reformulate(setdiff(preds, p), p), data = sc))$r.squared
      1 / (1 - r2) }), 3))
  })
  cat("  SRMR, d_G, d_L: no model-implied matrix exists for nonlinear models;",
      "the module reports them as 'Not computable' with this reason.\n")
}
