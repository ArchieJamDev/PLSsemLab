# PLSsemLab 0.0.0 (development)

## Phase 1: platform and first estimates

* **PLS-SEM analysis** (`plssem`): the model is built from boxes, never typed.
  Reflective and formative constructs are named boxes with their indicators
  dragged in (as EGAnetLab's *Theoretical structure* and jamovi's CFA); the
  structural model has one row per construct, kept in step with the boxes by
  `jamovi/js/plssem.events.js` and matched by name, into which the
  constructs that predict it are dragged (SEMLj's supplier pattern).
* **Validation before estimation** (D5): duplicated or missing names, an
  indicator in two constructs, unknown predictors, a construct predicting
  itself, loops, isolated constructs, interactions (not yet available),
  non-numeric or constant indicators, missing-data codes such as -99 and too
  few complete cases each stop the analysis with a message that names the
  construct or indicator at fault. Constructs without indicators are left
  out with a notice.
* **Estimation** with cSEM, PLS or consistent PLS, path/centroid/factorial
  inner weighting, listwise deletion or mean replacement. Internal names
  (C1.., V1..) so that spaces and accents in names never break cSEM; with
  PLS, reflective constructs are Mode A composites.
* **Results:** model summary, loadings and weights, path coefficients,
  explained variance; "How to read" guides, About your choices, applied
  interpretation, key findings, notices, and the equivalent cSEM code.
* Report in English and Spanish through jamovi's catalog (120 strings).
* Bundled datasets: corporate reputation (Hair et al., -99 declared as
  missing) and ECSI satisfaction.
* Tests (114 expectations): the seven model mistakes of experiment E4, data
  checks, every number equal to a direct cSEM call for PLS and PLSc, the
  reproducibility code, Spanish-English-Spanish in one process. CI with
  `jmvtools::check()` and the tests on Linux, Windows and macOS.

## Phase 0: design

* Design experiments (`experiments/design-v1/`): six R scripts that check
  with cSEM 0.7.1 that every shortcoming documented for JASP's PLS-SEM (D1–D6)
  can be solved in the first version, with their results and a GitHub
  Actions workflow to run them without a local R installation.
* Design document (`docs/DESIGN.md`).
* Project documents adopted from MokkenLab, EGAnetLab and AssumptionsLab and
  adapted to PLS-SEM: `ARCHITECTURE.md`, `DEVELOPER_GUIDE.md`,
  `CODE_STYLE.md`, `CONTRIBUTING.md`, `docs/Bibliography.md`, citation files.
