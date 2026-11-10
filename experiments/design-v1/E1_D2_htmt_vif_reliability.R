# -----------------------------------------------------------------------------
# E1 — D2: HTMT, VIF and reliability with bootstrap intervals.
# User reports in JASP: issues #1982, #2567; forum 2023; Rogers & Barboza 2024.
# Question: can every measure be computed, with a bootstrap CI where the
# literature asks for one, for reflective AND formative constructs?
# ES: E1 — D2: HTMT, VIF y fiabilidad con intervalos bootstrap.
# ES: Pregunta: ¿se puede calcular cada medida, con IC bootstrap donde la
# literatura lo pide, para constructos reflectivos Y formativos?
# -----------------------------------------------------------------------------
source("00_setup.R")
cat("cSEM", as.character(packageVersion("cSEM")), "\n\n")

# 0. The -99 trap: same model on the raw file and on the cleaned file.
# ES: 0. La trampa del -99: mismo modelo con el archivo crudo y el depurado.
reliab <- function(f) data.frame(rhoC = calculateRhoC(f), alpha = calculateRhoT(f),
                                 AVE = calculateAVE(f))
raw <- run_case("Raw data with -99 codes", csem(corp_raw, model_extended, .disattenuate = FALSE))
if (raw$ok) { cat("  Reliability on RAW data (wrong):\n"); print(round(reliab(raw$value), 3)) }
cat(sprintf("  Cases: raw %d | listwise after -99 -> NA: %d\n\n",
            nrow(corp_raw), nrow(corp_complete)))

fit <- csem(corp_meanrep, model_extended, .PLS_weight_scheme_inner = "path",
            .disattenuate = FALSE)
cat("Admissible (mean replacement, as in Hair et al.):", all(verify(fit) == FALSE), "\n\n")

# 1. Reliability and convergent validity (reflective constructs only).
# rho_A only exists as the PLSc correction factor: cSEM reports it in
# $Reliabilities only when .disattenuate = TRUE; with plain PLS it is 1.
# ES: 1. Fiabilidad y validez convergente (solo constructos reflectivos).
# rho_A solo existe como factor de corrección de PLSc: cSEM lo da en
# $Reliabilities solo con .disattenuate = TRUE; con PLS simple vale 1.
fit_c <- csem(corp_meanrep, model_extended, .disattenuate = TRUE)
r <- run_case("rho_C / rho_A / alpha / AVE", {
  out <- reliab(fit)
  out$rhoA <- fit_c$Estimates$Reliabilities[rownames(out)]
  out$rhoA_if_PLS <- fit$Estimates$Reliabilities[rownames(out)]
  out
})
if (r$ok) print(round(r$value, 3))

# 2. HTMT and HTMT2 with bootstrap percentile CI (Henseler et al., 2015).
# cSEM warns to use .absolute = FALSE when resampling.
# ES: 2. HTMT y HTMT2 con IC percentil bootstrap; cSEM pide .absolute = FALSE.
h <- run_case("HTMT + bootstrap CI (R = 499)", {
  calculateHTMT(fit, .type_htmt = "htmt", .inference = "bootstrap", .absolute = FALSE,
                .R = 499, .seed = SEED, .alpha = 0.10)
})
# FINDING: with inference, cSEM packs both numbers in one matrix — estimate in
# the lower triangle, upper CI bound in the upper triangle. The module must
# unpack it into a long table (pair, HTMT, lower, upper, verdict).
# ES: HALLAZGO: con inferencia, cSEM mete ambos números en una matriz —
# estimación en el triángulo inferior, límite superior en el superior. El
# módulo debe desplegarlo en una tabla larga (par, HTMT, inferior, superior).
if (h$ok) {
  m <- h$value$htmts; q <- h$value$quantiles
  pairs <- which(lower.tri(m), arr.ind = TRUE)
  tab <- data.frame(pair = paste(rownames(m)[pairs[, 1]], colnames(m)[pairs[, 2]], sep = " - "),
                    HTMT = m[pairs], upper95 = t(m)[pairs])
  tab$verdict <- ifelse(tab$upper95 < 0.85, "below .85 (strict)",
                 ifelse(tab$upper95 < 0.90, "below .90 (liberal)", "discriminant validity in doubt"))
  tab$HTMT <- round(tab$HTMT, 3); tab$upper95 <- round(tab$upper95, 3)
  print(tab, row.names = FALSE)
  cat("Admissible bootstrap samples:", h$value$nr_admissibles, "\n")
}
h2 <- run_case("HTMT2", calculateHTMT(fit, .type_htmt = "htmt2"))
if (h2$ok) print(round(h2$value$htmts, 3))

# 3. HTMT for composites: cSEM refuses by default (conceptually tied to the
# common factor model). The module must say why, not show an empty table.
# ES: 3. HTMT para compuestos: cSEM lo rechaza por defecto. El módulo debe
# explicar por qué, no mostrar una tabla vacía.
hc <- run_case("HTMT including composites", calculateHTMT(fit, .only_common_factors = FALSE))
if (hc$ok) print(round(hc$value$htmts, 3))

# 4. Collinearity: indicator VIF for formative (Mode B) composites and VIF of
# the predictors of each structural equation.
# ES: 4. Colinealidad: VIF de indicadores formativos (Modo B) y VIF de los
# predictores de cada ecuación estructural.
v1 <- run_case("VIF Mode B (formative indicators)", calculateVIFModeB(fit))
# Wide matrix (construct x indicator, 0 = not in block) -> long table.
# ES: Matriz ancha (constructo x indicador, 0 = no pertenece) -> tabla larga.
long_vif <- function(m) { idx <- which(m != 0, arr.ind = TRUE)
  data.frame(block = rownames(m)[idx[, 1]], variable = colnames(m)[idx[, 2]],
             VIF = round(m[idx], 2))[order(rownames(m)[idx[, 1]]), ] }
if (v1$ok) print(long_vif(v1$value), row.names = FALSE)
v2 <- run_case("VIF structural (assess)", assess(fit, .quality_criterion = "vif")$VIF)
if (v2$ok) print(long_vif(v2$value), row.names = FALSE)

# Fixed positions of the structural VIFs, so every bootstrap sample returns a
# vector of the same length and order.
# ES: Posiciones fijas de los VIF estructurales, para que cada muestra
# bootstrap devuelva un vector de igual largo y orden.
# Names are taken from the same which() call, in the same (column-major) order:
# taking them from the sorted display table mislabels the columns.
# ES: Los nombres salen de la misma llamada a which(), en el mismo orden;
# tomarlos de la tabla ordenada para mostrar etiqueta mal las columnas.
vif_idx   <- which(v2$value != 0, arr.ind = TRUE)
vif_pos   <- which(v2$value != 0)
vif_names <- paste(colnames(v2$value)[vif_idx[, 2]], "->", rownames(v2$value)[vif_idx[, 1]])

# 5. Bootstrap CI for VIF and reliability via .user_funs (JASP issue #2567
# asks for CI of VIF; SmartPLS 4.1.1.2 added them in April 2025).
# ES: 5. IC bootstrap para VIF y fiabilidad vía .user_funs.
b <- run_case("Bootstrap with user functions (R = 499)", {
  resamplecSEMResults(fit, .R = 499, .seed = SEED,
    .user_funs = list(
      VIF_struct = function(.object) {
        v <- c(assess(.object, .quality_criterion = "vif")$VIF)[vif_pos]
        names(v) <- vif_names; v },
      rhoC = function(.object) calculateRhoC(.object),
      AVE  = function(.object) calculateAVE(.object)))
})
if (b$ok) {
  inf <- infer(b$value, .quantity = c("mean", "CI_percentile"))
  for (q in c("VIF_struct", "rhoC", "AVE")) {
    cat("\n", q, ":\n", sep = "")
    print(round(t(rbind(estimate = b$value$Estimates$Estimates_resample$Estimates1[[q]]$Original,
                        inf[[q]]$CI_percentile)), 3))
  }
}

# 6. Fornell-Larcker, still requested by reviewers although HTMT replaces it.
# ES: 6. Fornell-Larcker, todavía pedido por revisores aunque HTMT lo reemplaza.
fl <- run_case("Fornell-Larcker", calculateFLCriterion(fit))
if (fl$ok) print(round(fl$value, 3))
