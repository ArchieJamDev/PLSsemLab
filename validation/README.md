# Validation

Three scripts check the numbers PLSsemLab reports. Unlike `tests/` (which also
checks behavior on difficult input), they answer one question each and write
a table of results next to them. All run in CI on every push
(`.github/workflows/jamovi-check.yml`) and stop with an error if a check
fails. Run them from the package root after installing PLSsemLab:

```
Rscript validation/csem_equivalence.R
Rscript validation/hair_published.R
Rscript validation/nca_equivalence.R   # needs the NCA package
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

## `nca_equivalence.R`: identical to the NCA package

PLSsemLab computes the necessary condition analysis itself (the NCA
package's peers come from an R loop: 10 000 permutations take many minutes
and cannot be cancelled in jamovi). The script checks every number against
`NCA::nca_analysis()` 5.1.0 with its defaults: the 0-100 construct scores
(against scores built by hand from a direct cSEM fit), CE-FDH and CR-FDH
effect sizes, CR-FDH accuracy, both bottleneck tables and the permuted
effect sizes and p values with the package's own permutations (its offsets
are read with a trace on its internal `p_test`). Three scenarios: corporate
reputation, cSEM's satisfaction data and 300 random data sets with heavy
ties; 135 checks and 23 897 values, largest difference 4.9e-16 (bottlenecks
within 0.05: the package prints one decimal).

Permutations often rebuild the same peers, so many permuted effects equal
the observed one exactly. The module counts those ties as "at least as
large" with a tolerance of 1e-10; the package compares raw floating-point
values, and rounding noise drops some ties, so its own p can be lower (by up
to 0.08 in the random data sets with very few distinct values; 0.012 on
corporate reputation). The script reports that difference as an informative
row and checks the module's p against the p computed from the package's
permuted effects with the ties counted. The module's own p values (its own
offsets from the seed) repeat exactly with the same seed.

---

# Validación (ES)

Tres scripts comprueban los números de PLSsemLab y escriben una tabla de
resultados junto a ellos; todos corren en CI en cada push y terminan con
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
- `nca_equivalence.R`: el NCA del módulo frente a `NCA::nca_analysis()`:
  puntuaciones 0-100, tamaños del efecto, precisión, cuellos de botella y
  valores p con las mismas permutaciones del paquete; 135 verificaciones y
  23 897 valores, diferencia máxima 4.9e-16. El módulo cuenta como empates
  los efectos permutados iguales al observado (tolerancia 1e-10); el paquete
  pierde algunos por ruido de redondeo, así que su propio p puede ser menor
  (hasta 0.08 con datos de muy pocos valores distintos), lo que queda como
  fila orientativa.
