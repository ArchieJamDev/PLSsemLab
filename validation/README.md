# Validation

Scripts that will check PLSsemLab against cSEM itself. Unlike `tests/`
(which checks graceful behavior on pathological input), these scripts check
that the numbers are identical to a direct cSEM call, and that they match
the published values of Hair et al. for the corporate reputation model.

| Script | Checks |
| --- | --- |
| *(to be written with the first analysis)* | Loadings, weights, paths, rho_A, rho_C, alpha, AVE, HTMT, VIF, f², R² and bootstrap intervals of `plssem()` vs. direct `cSEM::csem()` calls on `corp_rep_data`, PLS and PLSc |

The design-stage experiments in `experiments/design-v1/` already give, with
cSEM 0.7.1 and mean replacement of the -99 codes, COMP: alpha 0.776, rho_A
0.786, rho_C 0.869, AVE 0.688. The formal comparison with the tables
published by Hair et al. is the first job of this folder.

Results will be written next to each script (`*_results.csv`), and the
scripts will run in CI on every push, as in MokkenLab.

---

# Validación (ES)

Scripts que comprobarán PLSsemLab contra el propio cSEM: que los números
sean idénticos a una llamada directa a cSEM y que coincidan con los valores
publicados por Hair et al. para el modelo de reputación corporativa. Se
escriben junto con el primer análisis y correrán en CI en cada push, como en
MokkenLab.
