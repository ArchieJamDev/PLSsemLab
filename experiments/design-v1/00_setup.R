# -----------------------------------------------------------------------------
# PLSsemLab — design experiments
# Shared setup: models and data used by every experiment script.
# ES: Configuración común: modelos y datos que usan todos los experimentos.
#
# Data: corp_rep_data (Hair, Hult, Ringle & Sarstedt, PLS-SEM Primer; bundled
# in cSEM, n = 344, with servicetype for multigroup analysis).
# ES: Datos: corp_rep_data (Primer de PLS-SEM de Hair et al.; incluido en cSEM,
# n = 344, con servicetype para el análisis multigrupo).
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(cSEM))
data(corp_rep_data)
data(satisfaction)
data(dgp_2ndorder_cf_of_c)

SEED <- 20261110

# FINDING (E1): corp_rep_data codes missing answers as -99 (cusa, cusl_1..3).
# Run as is, PLS treats -99 as a real answer: CUSL alpha falls to 0.42 and HTMT
# warns about mixed signs. PLSsemLab must flag out-of-range sentinel codes
# (-99, -9, 999...) before estimating. Here they become NA.
# ES: HALLAZGO (E1): corp_rep_data codifica las respuestas faltantes como -99.
# Tal cual, PLS trata -99 como respuesta real: el alfa de CUSL cae a 0,42 y el
# HTMT advierte signos mezclados. PLSsemLab debe detectar códigos centinela
# fuera de rango antes de estimar. Aquí se convierten en NA.
corp_raw <- corp_rep_data
corp_na  <- corp_rep_data
corp_na[corp_na == -99] <- NA
corp_complete <- corp_na[complete.cases(corp_na), ]          # listwise
corp_meanrep  <- corp_na                                      # mean replacement
for (v in names(corp_meanrep))                                # (Hair et al. Primer)
  corp_meanrep[[v]][is.na(corp_meanrep[[v]])] <- mean(corp_meanrep[[v]], na.rm = TRUE)

# Simple reflective model (Hair et al. "simple model").
# ES: Modelo reflectivo simple (modelo simple de Hair et al.).
model_simple <- "
  COMP =~ comp_1 + comp_2 + comp_3
  LIKE =~ like_1 + like_2 + like_3
  CUSA =~ cusa
  CUSL =~ cusl_1 + cusl_2 + cusl_3
  CUSA ~ COMP + LIKE
  CUSL ~ COMP + LIKE + CUSA
"

# Extended model: four formative composites (<~) plus reflective constructs.
# ES: Modelo extendido: cuatro compuestos formativos (<~) y constructos reflectivos.
model_extended <- "
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
  CUSL ~ COMP + LIKE + CUSA
"

# Helper: run an expression, time it, and never let an error stop the script.
# ES: Ayudante: ejecuta, mide el tiempo y nunca deja que un error detenga el script.
run_case <- function(label, expr) {
  t0 <- Sys.time()
  warns <- character(0)
  out <- tryCatch(withCallingHandlers(expr,
           warning = function(w) {
             warns <<- c(warns, gsub("\\s+", " ", conditionMessage(w)))
             invokeRestart("muffleWarning")
           }),
           error = function(e) structure(conditionMessage(e), class = "exp_error"))
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  status <- if (inherits(out, "exp_error")) paste("ERROR:", gsub("\\s+", " ", out)) else "OK"
  cat(sprintf("[%s] %s (%.2f s)\n", label, status, secs))
  # Unique warnings with their count: what the module would turn into notices.
  # ES: Advertencias únicas con su conteo: lo que el módulo convertiría en avisos.
  if (length(warns)) {
    tab <- table(warns)
    for (w in names(tab)) cat(sprintf("    warning x%d: %s\n", tab[[w]], substr(w, 1, 160)))
  }
  invisible(list(value = out, secs = secs, ok = !inherits(out, "exp_error"), warnings = warns))
}
