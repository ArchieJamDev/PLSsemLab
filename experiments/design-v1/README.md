# Design experiments for PLSsemLab v1

Each script checks, with cSEM (the engine JASP also uses), that one documented
shortcoming of JASP's PLS-SEM can be solved in PLSsemLab's first version. The
scripts are deliberately small and self-contained: they are evidence for
design decisions, not module code.

Data: `corp_rep_data` (Hair, Hult, Ringle & Sarstedt, *A Primer on PLS-SEM*;
bundled in cSEM, n = 344) and `satisfaction` (ECSI, bundled in cSEM).

## How to run

On GitHub (no local R needed): **Actions → design-experiments → Run
workflow**. Leave the field blank to run all, or type e.g. `E1 E5`. The
`results/` folder is attached to the run as an artifact.

Locally: `Rscript install_deps.R`, then `Rscript run_all.R` from this folder.
`results/session.txt` records the R and cSEM versions used.

## Experiments and findings

| Script | Shortcoming in JASP | Result | What the design takes from it |
| --- | --- | --- | --- |
| `E1_D2_htmt_vif_reliability.R` | D2: no HTMT/VIF, no bootstrap CI (issues #1982, #2567) | Solved. rho_A, rho_C, alpha, AVE, HTMT/HTMT2 with CI, VIF of formative indicators and of structural predictors, all with bootstrap CI | rho_A exists only under PLSc (`.disattenuate = TRUE`); cSEM packs HTMT and its upper bound in one matrix (lower/upper triangle), to be unpacked into a long table |
| `E2_D3_higher_order_interactions.R` | D3: errors with second-order constructs and interactions (#2955, #3007) | Solved by computing each criterion separately. Second order: every criterion works per stage. Interaction: R², f², alpha, AVE, HTMT, VIF Mode B, effects work; rho_C works with `.model_implied = FALSE`; structural VIF from construct scores; floodlight / Johnson-Neyman works | SRMR, d_G, d_L have no model-implied matrix with nonlinear terms: shown as *Not computable* with that reason (Category B). Second-stage names carry a `_temp` suffix that must be mapped back |
| `E3_D4_bootstrap_performance.R` | D4: crash at >= 2000 samples (#3642), runs that never end (#4599) | About 19 ms per bootstrap draw for the extended model (n = 344), linear in R. No error at R = 2000. Ten blocks of 200 draws give the same percentile CIs as one run of 2000 (largest difference 0.016). Same seed, identical draws | Bootstrap in blocks with `private$.checkpoint()` between them, progress shown, seed in the panel and in the report |
| `E4_D5_model_builder_validation.R` | D5: uninformative errors on wrong syntax (forum 2023) | A builder from option boxes plus a validator catches the 7 common mistakes tested, each with a teaching message; cSEM alone gives messages such as "input string 1 is invalid" | Constructs and indicators are renamed internally (C1..Ck, V1..Vp): cSEM fails with spaces in variable names always, and with accented construct names depending on the system locale |
| `E5_D1_overall_model_fit.R` | D1: ML chi-square test, wrong for PLS (#2955) | Solved. `testOMF()` gives SRMR, d_G, d_L, d_ML with bootstrap critical values in 5–7 s at R = 499 | The report separates the exact-fit test from descriptive SRMR and says why the ML chi-square is not shown |
| `E6_D6_missing_metrics.R` | D6: f², Q²/PLSpredict, CVPAT, mediation, IPMA, MICOM not documented | Solved. f², PLSpredict (Q²_predict, RMSE, MAE against the LM benchmark), CVPAT (established vs competing model), indirect effects with bootstrap CI, IPMA, MICOM and multigroup differences (Chin, Henseler) all run | cSEM takes a seed only with one prediction repetition, so the module sets it itself; IPMA performance comes on the item scale and is rescaled to 0–100 with the scale range the user declares; `testMGD(.output_type = "structured")` fails in cSEM 0.7.1, the default output works; Henseler's p is one-sided (near 0 or near 1 both signal a difference) |

### Design decision forced by E6: what "reflective" means

In cSEM, `=~` means a **common factor** and `<~` a **composite**. IPMA and
MICOM warn that their results are trustworthy only for composites, yet the
PLS-SEM literature (Hair et al.) calls Mode A constructs "reflective" while
estimating them as composites. PLSsemLab must therefore separate two choices
the user makes: the measurement model (reflective / formative, as taught) and
the estimator (PLS, composites in Mode A/B; or PLSc, common factors). The
mapping to cSEM syntax is done by the module and explained in the report. With plain PLS, `=~` and `<~` in Mode A
give identical paths, weights and loadings (difference 0, checked in E6), but
only the composite form clears IPMA's "common factor" warning.

### Data trap found on the reference data

`corp_rep_data` codes missing answers as **-99** (cusa, cusl_1..3). Estimated
as is, CUSL's alpha falls from 0.831 to 0.421 and HTMT warns about mixed
signs. PLSsemLab checks every indicator for out-of-range sentinel codes before
estimating and asks the user to declare them as missing in jamovi.

---

# Experimentos de diseño para PLSsemLab v1 (ES)

Cada script comprueba, con cSEM (el mismo motor de JASP), que una deficiencia
documentada del PLS-SEM de JASP se puede resolver en la primera versión de
PLSsemLab. Son scripts pequeños y autónomos: evidencia para decisiones de
diseño, no código del módulo.

## Cómo ejecutarlos

En GitHub (sin R local): **Actions → design-experiments → Run workflow**.
Dejar el campo vacío para correr todos, o escribir p. ej. `E1 E5`. La carpeta
`results/` queda adjunta a la ejecución como artefacto.

En local: `Rscript install_deps.R` y luego `Rscript run_all.R` desde esta
carpeta.

La tabla de hallazgos está arriba, en inglés, igual que el resto de la
documentación de la suite.
