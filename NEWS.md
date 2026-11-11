# PLSsemLab 0.0.0 (development)

## Phase 2: quality of the measurement model (D2)

* **Reliability and convergent validity:** Cronbach's alpha, ρA, ρC and
  AVE for every reflective construct, with a verbal assessment (thresholds
  of Hair et al., 2019). ρA is computed in both estimators: with PLS it
  comes from a consistent PLS estimation of the same model, and a table note
  says so. A negative alpha is flagged as a sign of reverse-coded items.
* **Discriminant validity:** HTMT and HTMT2 for every pair of reflective
  constructs, in a long table with the assessment against 0.85 and 0.90.
  Mixed-sign correlations leave the pair "Not computable" with a footnote
  that points to reverse-coded items; a pair with a single-indicator
  construct carries a footnote saying its HTMT is a lower bound.
* **Fornell-Larcker criterion** (optional, off by default): square table with
  the square root of the AVE on the diagonal and footnotes on the pairs that
  fail.
* **Collinearity of formative indicators:** VIF of every indicator of a
  formative construct, with the assessment against 3 and 5.
* Every criterion is computed separately (never `assess()`), each in its own
  capture: what fails stays as a row with its reason (Category B). Tables
  keep their shape from `.init()`; a table that does not apply to the model
  is hidden with its guide and a notice says why.
* Teaching: three new "How to read" guides, interpretation and key findings
  of reliability, AVE, HTMT and VIF; the reproducibility code adds every
  cSEM call.
* **McDonald's ω (one-factor CFA)**, optional: a one-factor CFA per
  reflective construct with lavaan, outside the PLS model, as the
  psychometric reference for ρC. The guide explains that ρC is an ω
  computed with the PLS loadings (an upper bound with PLS, close to the CFA
  ω with PLSc), and the interpretation reports the gap. Two-indicator
  constructs, non-convergence and Heywood cases are "Not computable" with
  their reason.
* **Ordinal indicators** (Estimation > Indicators): polychoric correlations
  through cSEM (ordinal PLS; Schuberth et al., 2018), for the whole model.
  The reliability table notes that values refer to the latent responses
  (Chalmers, 2018); mean replacement is refused with ordinal indicators;
  notices point out indicators with 5 categories or fewer treated as
  continuous, and more than 10 treated as ordinal.
* VIF of formative indicators computed from the indicator correlations the
  model used: equal to `cSEM::calculateVIFModeB()` with continuous
  indicators, and with ordinal ones it uses the polychoric correlations
  (cSEM's function regresses the raw category codes).
* Spanish catalog: 193 strings. Tests: 193 expectations, including equality
  of every value with direct cSEM calls on the common-factor syntax, the
  values of COMP of experiment E1, and PLSc.

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
