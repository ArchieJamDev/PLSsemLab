# Validation

Two scripts check the numbers PLSsemLab reports. Unlike `tests/` (which also
checks behavior on difficult input), they answer one question each and write
a table of results next to them. Both run in CI on every push
(`.github/workflows/jamovi-check.yml`) and stop with an error if a check
fails. Run them from the package root after installing PLSsemLab:

```
Rscript validation/csem_equivalence.R
Rscript validation/hair_published.R
```

## `csem_equivalence.R`: identical to cSEM

The module runs through its R6 class, as jamovi runs it, and the same
analysis is computed with direct cSEM calls written by hand in the script.
Ten scenarios on the bundled corporate reputation data and on cSEM's
satisfaction data, 54 checks and 346 values
(`csem_equivalence_results.csv`): estimates and quality criteria of the
extended model with PLS (paths, loadings, weights, R², adjusted R², alpha,
rho_C, AVE, rho_A, HTMT, HTMT2, VIF, f², structural VIF, total and indirect
effects), consistent PLS, the bootstrap in blocks, mediation, overall fit
(testOMF), PLSpredict, IPMA, multigroup analysis (MICOM, permutation test,
PLS-MGA), a higher-order construct with the disjoint two-stage approach and
a two-way interaction. Largest difference: 1.3e-15.

The PLSpredict benchmark LM is checked against `stats::lm()` with an
intercept on cSEM's folds: cSEM 0.7.1's `predict(.benchmark = "lm")`
regresses through the origin (no intercept), which inflates the benchmark's
errors and favors PLS, so the module computes LM itself as Shmueli et al.
(2019) define it.

## `hair_published.R`: the published values

The corporate reputation model of Hair et al., with the -99 codes replaced by
the indicator mean as in the books, against the values printed in
(`hair_published_results.csv`):

- Hair, Hult, Ringle, Sarstedt, Danks & Ray (2021), *Partial least squares
  structural equation modeling (PLS-SEM) using R*, chapters 4 and 5:
  alpha, rho_C, rho_A, AVE, square root of the AVE, loadings, indicator
  reliability and VIF.
- Hair, Hult, Ringle & Sarstedt (2022), *A primer on partial least squares
  structural equation modeling* (3rd ed.), case study of chapter 6: the 13
  path coefficients, R², f², total and indirect effects.

The 52 deterministic values agree to the three printed decimals. Bootstrap
intervals and PLSpredict errors depend on random draws and folds and are
informative rows (13, all within 0.03). One printed total effect
(ATTR → CUSA, 0.086) does not follow from the source's own path coefficients
(0.086 × 0.146 + 0.167 × 0.436 = 0.0854; PLSsemLab: 0.0853) and is kept as an
informative row with that note.

---

# Validación (ES)

Dos scripts comprueban los números de PLSsemLab y escriben una tabla de
resultados junto a ellos; ambos corren en CI en cada push y terminan con
error si una verificación falla.

- `csem_equivalence.R`: el módulo, ejecutado como lo ejecuta jamovi, frente
  a llamadas directas a cSEM escritas a mano: diez escenarios, 54
  verificaciones y 346 valores, diferencia máxima 1.3e-15. La referencia LM
  de PLSpredict se contrasta con `stats::lm()` con intercepto, porque
  `predict(.benchmark = "lm")` de cSEM 0.7.1 omite el intercepto (regresión
  por el origen) y favorece a PLS; el módulo calcula LM como lo definen
  Shmueli et al. (2019).
- `hair_published.R`: el modelo de reputación corporativa frente a los
  valores publicados por Hair et al. (2021, caps. 4 y 5; 2022, caso del
  cap. 6). Los 52 valores deterministas coinciden en los tres decimales
  publicados; los intervalos bootstrap y los errores de PLSpredict son filas
  orientativas (13, todas dentro de 0.03). Un efecto total impreso
  (ATTR → CUSA, 0.086) no se deduce de las rutas de la propia fuente
  (0.0854) y queda como fila orientativa con esa nota.
