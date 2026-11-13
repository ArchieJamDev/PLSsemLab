# PLSsemLab 0.0.0 (development)

## Phase 4: interactions and higher-order constructs (D3)

* **Two-way interactions:** drag two constructs together into a predictor
  box. The report adds the simple effects at −1, 0 and +1 SD of the
  moderator, a floodlight plot with the Johnson-Neyman bounds (from the
  bootstrap) and the f² of the interaction on Kenny's (2018) benchmarks.
  SRMR-type criteria are not defined with interactions; ρC is computed from
  the loadings.
* **Higher-order constructs:** a *Higher-Order Constructs* box builds
  reflective or formative second-order constructs from first-order ones.
  * Default: the disjoint two-stage approach (Sarstedt et al., 2019), with
    both stages resampled together in the bootstrap.
  * Option: cSEM's own second-order routine, as JASP uses it, whose
    estimates equal a direct `cSEM::csem()` call. A notice explains the two
    differences (paths among lower-order constructs in stage 1; a
    reflective higher-order construct as a common factor with consistent
    PLS in stage 2).
* **Path diagram:** drawn with semPlot, with interaction and higher-order
  nodes, and a *Path Diagram* settings box.

## Phase 3: structural model and bootstrap (D2, D4)

* **Structural model:** path coefficients and R² with adjusted R², plus
  three new tables. The f² effect size is banded against 0.02, 0.15 and
  0.35. The structural VIF has an assessment against 3 and 5, and an
  outcome with a single predictor keeps its row and says why. The direct,
  indirect and total effects table has one row for every pair of
  constructs linked by a route. Each table has a "How to read" guide.
* **Bootstrap in blocks (D4):** an explicit *Run bootstrap* checkbox, off
  by default. The resamples (default 2000), seed and confidence level are
  in the panel and printed in the report.
  * Resamples are drawn in blocks of 200; block k uses seed + k − 1.
    `private$.checkpoint()` runs between blocks, so changing an option stops
    the run, and the status line reports progress.
  * Up to 200 resamples, the intervals are identical to a direct
    `resamplecSEMResults()` call with the same seed. With more, they equal
    the module's equivalent R code.
  * Inadmissible resamples are dropped and counted, and blocks that fail
    are reported as a notice.
  * Results are cached (small and capped) by a hash of the model, data and
    bootstrap options.
* **Intervals for every measure:** percentile CIs, standard errors and p
  values for paths, loadings, weights and effects. There are CIs for R², ρC,
  AVE, the formative and structural VIF, and the one-sided 95% upper bound
  of HTMT.
  * With the bootstrap, the HTMT assessment uses the upper bound (Henseler
    et al., 2015).
  * Formative weights are assessed by significance and loading (Hair et
    al., 2019).
  * When p and the interval disagree, a footnote explains that the decision
    follows the interval.
* **Plots** (colours from jamovi's theme only, data in `image$state`):
  * a path diagram (on by default) with R² in each outcome, line width
    following the coefficient, and dashed lines for non-significant paths;
  * a loadings plot with the 0.708 reference line and bootstrap error bars;
  * an HTMT grid shaded by the upper bound.
* Teaching: interpretation and key findings of significance, f², structural
  VIF and HTMT upper bounds. "What to do now" changes once the bootstrap has
  run, and the reproducibility code adds the blocks and their merge.
* Fixes from the first test in jamovi:
  * each row of the structural model shows its construct (`Like ←`), and
    the header stays after predictors are added or removed;
  * the .jmo bundles cSEM 0.7.1, not the compiler snapshot's 0.6.1;
  * a bootstrap of the bank model that ended the analysis in jamovi now
    runs (2000 resamples in about 20-40 s in jamovi's docker image).
* Tests: the bootstrap is checked against direct cSEM calls to 1e-10, and f²,
  structural VIF and total effects against cSEM. 2000 resamples of the
  extended corporate-reputation model ran in 52 s with all 2000 admissible.

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
* A warning names the reflective indicators with a negative loading
  (reverse-coded items not recoded), also in the key findings; running
  text shows item codes instead of long item wordings.
* Options panel fixed for jus 3.0 (jamovi 2.7): helpers through jamovi's
  global utils, construct names declared as custom variables, structural
  model as SEMLj's Array of Terms; tested in the official jamovi 2.7.4
  docker image with a headless browser and in jamovi 2.7.27 by the author.
* New bundled dataset: Bank customers (Damberg, 2023, CC BY 4.0), with
  formative and reflective constructs and demographics.
* Spanish catalog: 194 strings. Tests: 199 expectations, including equality
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
