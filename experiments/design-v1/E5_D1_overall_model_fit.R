# -----------------------------------------------------------------------------
# E5 — D1: a correct test of overall model fit.
# An ML-based chi-square test of fit does not apply to PLS; the bootstrap test
# of Beran & Srivastava (1985) / Dijkstra & Henseler (2015), testOMF(), is the
# right one.
# Questions: (1) does testOMF() run on the reference models, with what time?
# (2) how different is the ML chi-square of covariance-based SEM? (3) saturated vs
# estimated model: what changes?
# ES: E5 — D1: una prueba correcta del ajuste global del modelo.
# ES: Preguntas: (1) ¿testOMF() funciona en los modelos de referencia y cuánto
# tarda? (2) ¿qué tan distinto es el chi-cuadrado ML del SEM basado en covarianzas?
# (3) modelo saturado vs estimado: ¿qué cambia?
# -----------------------------------------------------------------------------
source("00_setup.R")

omf_table <- function(t) {
  # testOMF returns the statistics and their bootstrap critical values.
  # ES: testOMF devuelve los estadísticos y sus valores críticos bootstrap.
  # Critical values come as a column per alpha level, in the same order.
  # ES: Los valores críticos vienen como una columna por nivel alfa, en el mismo orden.
  ts <- t$Test_statistic; cv <- t$Critical_value[, 1]
  data.frame(measure = names(ts), value = round(ts, 4), critical_95 = round(cv, 4),
             decision = ifelse(ts <= cv, "do not reject exact fit", "reject exact fit"),
             row.names = NULL)
}

for (case in list(list(name = "simple reflective (PLSc)", model = model_simple, dis = TRUE),
                  list(name = "extended, composites (PLS)", model = model_extended, dis = FALSE))) {
  cat("\n====", case$name, "\n")
  fit <- csem(corp_meanrep, case$model, .disattenuate = case$dis)
  for (sat in c(FALSE, TRUE)) {
    r <- run_case(sprintf("testOMF R=499 saturated=%s", sat),
                  testOMF(fit, .R = 499, .seed = SEED, .saturated = sat, .alpha = 0.05))
    if (r$ok) { print(omf_table(r$value), row.names = FALSE)
                cat("  admissible bootstrap samples:", r$value$Information$Number_admissibles, "\n") }
  }
  # The covariance-based SEM report: an ML chi-square and its p-value.
  # ES: Lo que reporta el SEM basado en covarianzas: un chi-cuadrado ML y su p.
  run_case("ML chi-square (covariance-based SEM)", {
    chi <- calculateChiSquare(fit); df <- calculateDf(fit)
    cat(sprintf("  chi2 = %.2f, df = %d, p = %.3g\n", chi, df, pchisq(chi, df, lower.tail = FALSE)))
  })
  run_case("Descriptive fit indices", {
    print(round(unlist(assess(fit, .quality_criterion = c("srmr", "dg", "dl", "dml", "rms_theta",
                                                            "nfi", "cfi", "gof"))[c("SRMR", "DG", "DL", "DML", "RMS_theta", "NFI", "CFI", "GoF")]), 4))
  })
}
