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
# Simulated learning dataset (plssemlab_learning.csv). Employees answer
# 7-point items about leadership, autonomy, job resources, work engagement
# (vigor, dedication, absorption), performance and turnover intention. The
# population model is known, so a user can see whether each option of the
# module recovers it, and every option has something to show:
#   - reflective constructs (LEAD, AUT, VIG, DED, ABS, PERF, INT) and one
#     formative construct (RES, Mode B weights .50, .30, .30, .20);
#   - a reflective-reflective higher-order construct: engagement (ENG) with
#     the dimensions VIG, DED, ABS (loadings .85 each);
#   - a moderation: AUT strengthens LEAD -> ENG (interaction .15);
#   - a partial mediation: LEAD -> ENG -> PERF, with a small direct path;
#   - a multigroup difference: LEAD -> ENG is .20 in the public sector and
#     .50 in the private sector, with the same measurement model in both
#     (MICOM should find invariance and the tests a difference); region has
#     three groups and no differences;
#   - constructs with different mean levels, for the importance-performance
#     map, and about 1% of answers missing at random in some items.
# Synthetic data, no real persons: released under CC0 1.0.
# ES: Datos simulados para aprender. El modelo poblacional es conocido, así
# que el usuario puede ver si cada opción del módulo lo recupera, y cada
# opción tiene algo que mostrar: constructos reflectivos y uno formativo, un
# constructo de orden superior (compromiso, con vigor, dedicación y
# absorción), una moderación, una mediación parcial, una diferencia entre
# sectores con el mismo modelo de medida, una región sin diferencias,
# niveles medios distintos para el IPMA y un 1% de respuestas perdidas.
# Datos sintéticos, sin personas reales: CC0 1.0.
# -----------------------------------------------------------------------------

# Seed: the population model is fixed above; among seeds 1-12 this one gives
# a sample whose chance fluctuations do not hide what the data are meant to
# show (no MICOM step that fails by chance, no chance difference between
# regions, the sector difference and the interaction visible). The choice is
# stated here and in data/README.md.
# ES: Semilla: el modelo poblacional está fijado; entre las semillas 1-12 esta
# da una muestra cuyas fluctuaciones al azar no ocultan lo que los datos
# deben mostrar. La elección se declara aquí y en data/README.md.
set.seed(8)
n <- 600

# Exogenous variables: leadership, autonomy and the four resource indicators,
# moderately correlated.
# ES: Variables exógenas: liderazgo, autonomía y los cuatro indicadores de
# recursos, moderadamente correlacionados.
sigma <- matrix(0.30, 6, 6)
sigma[1, 2] <- sigma[2, 1] <- 0.30
sigma[1, 3:6] <- sigma[3:6, 1] <- 0.20
sigma[2, 3:6] <- sigma[3:6, 2] <- 0.15
diag(sigma) <- 1
exo <- MASS::mvrnorm(n, rep(0, 6), sigma)
lead <- exo[, 1]
aut <- exo[, 2]
res_x <- exo[, 3:6]
res <- as.numeric(scale(res_x %*% c(0.50, 0.30, 0.30, 0.20)))

sector <- sample(rep(c("Public", "Private"), each = n / 2))
region <- sample(c("North", "Central", "South"), n, replace = TRUE)

# Endogenous constructs; each is standardised after adding its disturbance.
# ES: Constructos endógenos; cada uno se estandariza tras sumar su residuo.
endo <- function(core, r2) {
    e <- stats::rnorm(n)
    as.numeric(scale(core / stats::sd(core) * sqrt(r2) + e * sqrt(1 - r2)))
}
b_lead <- ifelse(sector == "Private", 0.50, 0.20)
eng_core <- b_lead * lead + 0.30 * res + 0.20 * aut + 0.15 * lead * aut
eng <- endo(eng_core, 0.45)
vig <- endo(eng, 0.85^2)
ded <- endo(eng, 0.85^2)
abs_ <- endo(eng, 0.85^2)
perf <- endo(0.45 * eng + 0.10 * lead, 0.25)
int <- endo(-0.40 * eng - 0.15 * lead, 0.22)

# 7-point items: loading x construct + error, shifted by the construct's mean
# level (higher = better performance on the map), cut at fixed thresholds.
# ES: Las cargas son iguales dentro de cada constructo (.80), para que los
# pesos de PLS no dependan del modelo estructural y la diferencia entre
# sectores no se convierta en una diferencia en los compuestos.
# The loadings are equal within each construct (.80): PLS weights then do not
# depend on the structural model, so the difference between sectors in
# LEAD -> ENG does not turn into a difference in the composites (with unequal
# loadings, MICOM step 2 can fail for that reason alone).
# ES: Ítems de 7 puntos: carga x constructo + error, desplazados por el nivel
# medio del constructo, cortados en umbrales fijos.
cuts <- c(-Inf, -1.6, -0.9, -0.3, 0.3, 0.9, 1.6, Inf)
likert <- function(z) as.integer(cut(z, cuts))
items <- function(eta, loadings, shift, prefix) {
    out <- sapply(loadings, function(l) likert(l * eta + sqrt(1 - l^2) * stats::rnorm(n) + shift))
    colnames(out) <- paste0(prefix, "_", seq_along(loadings))
    out
}
dat <- data.frame(
    sector = sector, region = region,
    items(lead, rep(0.80, 4), 0.20, "lead"),
    items(aut, rep(0.80, 3), 0.50, "aut"),
    stats::setNames(as.data.frame(sapply(1:4, function(k) likert(res_x[, k] - 0.20))), paste0("res_", 1:4)),
    items(vig, rep(0.80, 3), 0.30, "vig"),
    items(ded, rep(0.80, 3), 0.40, "ded"),
    items(abs_, rep(0.80, 3), 0.10, "abs"),
    items(perf, rep(0.80, 3), 0.60, "perf"),
    items(int, rep(0.80, 3), -0.40, "int"))

# About 1% of the answers missing at random in performance, intention and
# one dedication item.
# ES: Alrededor del 1% de respuestas perdidas al azar.
for (v in c(paste0("perf_", 1:3), paste0("int_", 1:3), "ded_2")) dat[[v]][stats::runif(n) < 0.01] <- NA

utils::write.csv(dat, "data/plssemlab_learning.csv", row.names = FALSE, na = "")
