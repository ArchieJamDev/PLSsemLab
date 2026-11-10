# -----------------------------------------------------------------------------
# E3 — D4: bootstrap speed and robustness.
# User reports in JASP: issue #3642 (crash with >= 2000 samples, fixed),
# #4599 (open, Oct 2026: runs for a day), forum 2023 (disattenuation always on
# -> many inadmissible samples redrawn with "replace").
# Questions: (1) how long does each R take? (2) what does disattenuation cost?
# (3) does R >= 2000 still break anything? (4) can the bootstrap run in chunks,
# so jamovi's checkpoint can stop it, with the same result as one run?
# ES: E3 — D4: velocidad y robustez del bootstrap.
# ES: Preguntas: (1) ¿cuánto tarda cada R? (2) ¿qué cuesta la desatenuación?
# (3) ¿R >= 2000 sigue rompiendo algo? (4) ¿se puede correr por bloques, para
# que el checkpoint de jamovi lo detenga, con el mismo resultado?
# -----------------------------------------------------------------------------
source("00_setup.R")
dat <- corp_meanrep

# (1) + (2) Time and admissible samples, PLS vs PLSc, drop vs replace.
# ES: (1) + (2) Tiempo y muestras admisibles, PLS vs PLSc, drop vs replace.
grid <- expand.grid(R = c(500, 2000), disattenuate = c(FALSE, TRUE),
                    handle = c("drop", "replace"), stringsAsFactors = FALSE)
grid$secs <- NA; grid$admissible <- NA; grid$runs <- NA
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  r <- run_case(sprintf("R=%d PLSc=%s %s", g$R, g$disattenuate, g$handle),
    csem(dat, model_extended, .disattenuate = g$disattenuate,
         .resample_method = "bootstrap", .R = g$R, .seed = SEED,
         .handle_inadmissibles = g$handle))
  grid$secs[i] <- r$secs
  if (r$ok) { grid$admissible[i] <- r$value$Information$Information_resample$Number_of_admissibles
              grid$runs[i] <- r$value$Information$Information_resample$Number_of_runs }
}
print(grid, row.names = FALSE)

# (3) The step that crashed JASP at >= 2000: model selection criteria on a
# resampled object.
# ES: (3) El paso que rompía JASP con >= 2000: criterios de selección de modelo
# sobre un objeto remuestreado.
big <- csem(dat, model_extended, .resample_method = "bootstrap", .R = 2000, .seed = SEED,
            .disattenuate = FALSE)
run_case("calculateModelSelectionCriteria on R = 2000 object",
         print(round(sapply(calculateModelSelectionCriteria(big, .only_structural = TRUE), `[`, "CUSL"), 2)))
run_case("summarize on R = 2000 object", invisible(summarize(big)))

# (4) Chunked bootstrap: 10 blocks of 200 draws with derived seeds, pooled,
# versus one run of 2000. Percentile CIs of the paths should agree closely
# (Monte Carlo error), and each block is a natural checkpoint for jamovi.
# ES: (4) Bootstrap por bloques: 10 bloques de 200 con semillas derivadas,
# combinados, frente a una corrida de 2000. Los IC percentiles deberían
# coincidir salvo error Monte Carlo; cada bloque es un checkpoint natural.
fit0 <- csem(dat, model_extended, .disattenuate = FALSE)
draws_of <- function(obj) obj$Estimates$Estimates_resample$Estimates1$Path_estimates$Resampled
t0 <- Sys.time()
chunks <- lapply(1:10, function(k) {
  # Here the module would call private$.checkpoint() and update progress.
  # ES: Aquí el módulo llamaría a private$.checkpoint() y actualizaría el progreso.
  draws_of(resamplecSEMResults(fit0, .R = 200, .seed = SEED + k))
})
pooled <- do.call(rbind, chunks)
cat(sprintf("Chunked 10 x 200: %.1f s, %d draws\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")), nrow(pooled)))
single <- draws_of(big)
ci <- function(m) t(apply(m, 2, quantile, probs = c(.025, .975)))
paths <- colnames(single)
cmp <- data.frame(path = paths,
                  single_lo = ci(single)[, 1], pooled_lo = ci(pooled)[, 1],
                  single_hi = ci(single)[, 2], pooled_hi = ci(pooled)[, 2])
cmp[, -1] <- round(cmp[, -1], 3)
print(cmp, row.names = FALSE)
cat("Largest absolute difference between CI bounds:",
    round(max(abs(c(cmp$single_lo - cmp$pooled_lo, cmp$single_hi - cmp$pooled_hi))), 3), "\n")

# (5) Same seed, same result: reproducibility check.
# ES: (5) Misma semilla, mismo resultado: verificación de reproducibilidad.
a <- draws_of(resamplecSEMResults(fit0, .R = 100, .seed = SEED))
b <- draws_of(resamplecSEMResults(fit0, .R = 100, .seed = SEED))
cat("Identical draws with the same seed:", identical(a, b), "\n")
