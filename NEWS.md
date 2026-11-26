# PLSsemLab 1.2.0

* Changes requested in jamovi's review of 1.0.0:
  * Sections the model does not use (higher-order constructs, mediation,
    simple slopes, formative VIF) are hidden; a section that applies but
    cannot be computed keeps its place and a notice.
  * The random seed has its own box, which lists the analyses that use it.
  * The Path Diagram box is inside Plots, next to the option that switches
    the diagram on.
  * Result headings use Title Case.
  * The diagram follows jamovi's theme: the fixed colour options (construct
    fill, indicator fill, arrow colour) are removed.
  * Prose blocks carry no inline styles, and the equivalent R code is a
    Preformatted item. If PDF export drops its line breaks, that is a jamovi
    issue to report there.
  * The bootstrap status is hidden when the bootstrap ends; the elapsed time
    is a footnote of the bootstrap table, not a column.
  * Separators are not translated, and the texts of the hidden notices are
    removed from the catalogue.
  * The diagram drawing and the prediction code are split into helper
    functions; the drawing arguments are unchanged (checked on five models).

* **Weighting approaches** (Estimation box): besides PLS-PM, GSCA
  (Hwang & Takane, 2004; GSCAm with consistent estimation), SUMCORR and
  MAXVAR (generalized canonical correlation; Kettenring, 1971), PCA and
  unit weights (sum scores), through cSEM. Combinations cSEM cannot estimate
  stop with the reason (consistent estimation other than PLS-PM or GSCA,
  higher-order constructs outside PLS-PM, GSCA with interactions, PCA with
  single-indicator constructs); rho_A, the IPMA and the NCA, defined for
  PLS-PM weights, say so with a note. SSQCORR and MINVAR are left out: in
  cSEM 0.7.1 they return weights with reversed signs. MAXVAR weights are
  fixed only up to a common sign and cSEM does not correct it in the
  resamples (about one draw in five reversed every loading): the module turns
  each construct of each draw back to its original sign, and the multigroup
  tests are not computed with MAXVAR.
* **Jackknife** as an alternative to the bootstrap (Bootstrap box): one
  estimation per case left out, t intervals from the jackknife standard
  error (Efron & Tibshirani, 1993) for every quantity of the report,
  including specific indirect effects, simple effects and the HTMT upper
  bound. cSEM's own jackknife interval subtracts the bootstrap-style bias;
  the module reports the plain t interval. The report explains that the
  bootstrap remains the standard of PLS-SEM.
* **Mardia's multivariate skewness and kurtosis** of the indicators
  (Measurement Model box; Mardia, 1970; Cain et al., 2017).
* **Construct scores saved to the data set** (Save box): the standardized
  scores of the estimated model, one new column per construct.
* **PLSpredict: extra benchmark**: the same model with another weighting
  approach (PLS-PM, GSCA, SUMCORR, MAXVAR, PCA or unit weights),
  cross-validated on the same folds, with RMSE and MAE columns and a
  two-sided CVPAT row. cSEM 0.7.1's predict() with test data returns the
  model's residuals as the benchmark's, so the module estimates the
  benchmark itself; the values equal cSEM's cross-validation mode.
* **Model comparison** (Prediction box, "Compare the main and the competing
  model"): the main model and the competing model (without the paths left
  out) side by side, with R², adjusted R², AIC, AICu, BIC, GM and HQ of each
  structural equation, SRMR and both sets of path coefficients; the reading
  follows BIC and GM (Sharma et al., 2019, 2021).
* `validation/csem_equivalence.R` adds the new options (69 checks, 0 failed).

# PLSsemLab 1.1.0

* Necessary condition analysis (NCA) combined with PLS-SEM (Dul, 2016;
  Richter et al., 2020, 2023), suggested by a user. A new
  *Necessary Condition Analysis* box, with its own Run checkbox, tests every
  construct with a direct path to an endogenous construct as a necessary
  condition, on the unstandardized construct scores of the IPMA rescaled to
  0-100 (same range of the scale). Tables: CE-FDH and CR-FDH effect sizes d
  with Dul's benchmarks, permutation p values (Dul, van der Laan & Kuik,
  2020; 10 000 permutations by default, the bootstrap's seed), accuracy and a
  verdict (necessary when d >= 0.1 and p < .05); the combined reading with
  the PLS-SEM paths (must-have, should-have, both or neither); bottleneck
  tables per outcome; ceiling plots; interpretation, guide and the
  equivalent R code. The module computes the NCA itself, because the NCA
  package's peers come from an R loop that makes 10 000 permutations take
  many minutes without a way to cancel them: here they take seconds and can
  be cancelled. `validation/nca_equivalence.R` checks in CI that the scores,
  effect sizes, accuracy, bottlenecks and p values (with the same
  permutations) equal `NCA::nca_analysis()`. Not computable (with the reason)
  with PLSc, interactions or higher-order constructs, as the IPMA.

* Measurement invariance of the reflective constructs as common factors,
  suggested by a user: in the *Multigroup Analysis* box, a
  multigroup CFA with lavaan (configural, metric and scalar models; MLR by
  default, ML as an option) next to MICOM. Each step is compared with the
  previous one by the chi-square difference (scaled with MLR) and by the
  change in CFI, RMSEA and SRMR, with Chen's (2007) limits; the configural
  model is judged by CFI and SRMR (Hu & Bentler, 1999). The interpretation
  reads MICOM (composites) and the MGCFA (common factors) together.
  Reflective constructs with one indicator and formative constructs are left
  out with a note; not computable with ordinal indicators in this version.
  `validation/csem_equivalence.R` checks the table against direct lavaan
  calls.
* Ribbon menu icon for jamovi (`menuIcons` in `jamovi/0000.yaml`):
  `jamovi/icons/plssemlab.svg`, 74 x 44, jamovi's palette, strokes of 2-4
  units, plain SVG without metadata, mapped to the "PLS-SEM Lab" menu group.
  jamovi ignores it until its support for module icons is released.

# PLSsemLab 1.0.1 (released 2026-11-18, DOI 10.5281/zenodo.24341176)

* New module icon and logo, in the visual line of the suite (MokkenLab,
  EGAnetLab): two constructs (blue circles) predict a third (amber circle).
  The icon is `jamovi/assets/plssemlab-icon.png`; `docs/logo/` holds its SVG,
  the light and dark logos of the README and `analysis-plssemlab.svg`, a
  74 x 44 icon for jamovi's Analyses ribbon. `docs/logo/build_logo.py` builds
  them all.

# PLSsemLab 1.0.0 (released 2026-11-17, DOI 10.5281/zenodo.24309058)

Version prepared for the jamovi library. It covers every section of the
design (requirements D1–D6) and the lessons of jamovi's audits of the
earlier modules of the suite; numbers identical to direct cSEM calls and to
the values published by Hair et al. (`validation/`, in CI on every push).
The changes since 0.7.6 are listed under 0.7.7.

# PLSsemLab 0.7.7

Lessons from jamovi's audits of EGAnetLab and MokkenLab, applied before
PLSsemLab's own review.

## Changes visible to users

* **PLSpredict's linear-model benchmark (LM) has its intercept.** cSEM
  0.7.1's `predict(.benchmark = "lm")` regresses the indicators through the
  origin, which inflates LM's errors and makes PLS look better than it is,
  in PLSpredict and in CVPAT against LM. PLSsemLab now fits LM itself as
  Shmueli et al. (2019) define it (OLS with an intercept on the exogenous
  indicators, same folds), and reproduces the RMSE published by Hair et al.
  (2022). The equivalent R code shows the same computation.
* **Every long computation can be interrupted.** The overall fit test,
  PLSpredict/CVPAT, MICOM and the multigroup tests stop as soon as an option
  changes: jamovi's restart signal is no longer caught as if it were an
  error of cSEM, and cSEM's resampling calls (bootstrap, testOMF, testMICOM,
  testMGD) check in with jamovi at every resample. The results are the same.
* **Bootstrap resamples table:** requested, drawn and admissible resamples,
  seed, block size, confidence level and time as numbers, next to the text
  record.
* **"How to read each table" is off by default**, so the report stays
  compact; the guides are one click away in Report Content.
* The IPMA quadrant of a construct whose total effect is not significant is
  one whole label (translatable as a sentence), not a label with a clause
  appended.
* Panel subheading "Nodes and Labels" in title case.

## Internal changes

* `.pl_capture()` rethrows jamovi's restart condition (`.pl_is_restart()`);
  `.pl_with_ticks()` installs a silent progressr handler that calls
  `private$.checkpoint(flush = FALSE)` at most every 0.25 s.
* `graphics` and `progressr` declared in Imports.
* Long methods split into named steps: `.run()` into `.estimate()`,
  `.fillTables()`, `.optionalSections()` and `.reportNotices()`; `.init()`
  into `.initMeasurement()`, `.initStructural()` and `.initOptional()`;
  `.interpretation()`, `.reproducibility()` and `.multigroup()` likewise.
  No method is longer than about 125 lines.
* The four result caches together stay under 2 MB (bootstrap: 3 entries,
  1.5 MB; overall fit, prediction and multigroup: 4 entries, 0.1 MB each,
  their measured entries being a few KB).
* Column super-titles are translated at run time by jmvcore from the
  catalog; a test checks them in Spanish.
* **Validation (phase 8):** `validation/csem_equivalence.R` (54 checks, 346
  values, identical to direct cSEM calls) and `validation/hair_published.R`
  (52 published values of Hair et al., 2021 and 2022, reproduced to three
  decimals) run in CI on every push.
* Regression tests in `tests/testthat/test-audit.R`: restart through every
  long computation, restarted runs not cached, one tick per resample with
  identical results, tables with their final rows when `.init()` sees only
  the header of the data, whole IPMA labels, guides off by default, plot
  settings enabled only with their plot, numeric bootstrap table.

# PLSsemLab 0.7.6

* The per-group table of the multigroup analysis says "Not computable", with
  the reason, when the comparison cannot run (for example, with a
  higher-order construct), instead of leaving the cell empty.
* Documentation: README with features, installation from the latest release,
  DOI and citation; design documents worded as requirements D1–D6.

# PLSsemLab 0.7.5

* The path diagram draws a moderation as it is drawn in the literature: an
  arrow from the moderator that ends on the path it moderates (predictor →
  outcome), labelled with the coefficient of the interaction term and dashed
  when it is not significant. The interaction term is no longer a separate
  node, and the caption explains how to read the arrow.
* The indicators of an endogenous formative construct no longer sit between
  its predictors and the construct, where they covered the path labels:
  they go in a row above (when the construct predicts others) or in a column
  to the right (when it does not), like those of reflective constructs.

# PLSsemLab 0.7.4

* Nothing is hidden because it does not apply to the model: the tables and
  plots stay in place and a jamovi notice in front of each one says why
  (no higher-order construct, formative higher-order constructs and their
  reliability, no higher-order HTMT pair, no mediation chain, no interaction,
  no reflective construct, fewer than two reflective constructs for HTMT, no
  formative construct, and the loadings and HTMT plots), as jamovi's review
  asked of EGAnetLab. The introduction is still shown only while the model
  is incomplete.

# PLSsemLab 0.7.3

## Phase 8, first part: datasets, stress tests and jamovi's requirements

* New bundled dataset **Learning data** (`plssemlab_learning.csv`, n = 600,
  simulated, CC0) with a known population model, so that every option has
  something to show: reflective and formative constructs, a higher-order
  construct (work engagement), a moderation, a mediation, a difference
  between sectors (MICOM: full invariance; leadership → engagement differs
  in both tests) and none between regions, the importance-performance map
  and missing answers. `data-raw/simulate_learning_data.R` rebuilds it; the
  seed choice is stated. `data/README.md` has a table of which dataset and
  model to use for each option.
* Fix: a higher-order construct together with an interaction stopped with
  "non-conformable arrays" (the stage-2 path matrix has one column per
  interaction term) and the moderation table looked for the higher-order
  construct in the stage-1 fit. Both now use stage 2.
* New tests: every row of the dataset table end to end
  (`test-datasets.R`) and stress tests (`test-stress.R`: names with spaces
  and accents through every long option, a grouping variable with missing
  values and accented labels, an empty dataset, small samples, an indicator
  constant inside one group, an indicator with no observed value, a tiny
  group). Each case finishes or stops with a PLSsemLab message.
* jamovi's library conventions: American spelling in the report and the
  panel, Title Case for every target box, no "Show" in checkbox titles, the
  module and analysis descriptions list every section, README with the
  datasets and the Windows build. The Spanish catalog has no untranslated
  string.

# PLSsemLab 0.7.2

* The report is organised in sections with headings, so the reader can find
  each part: Measurement model, Structural model, Overall model fit,
  Out-of-sample prediction, Importance-performance map, Multigroup analysis
  and Plots (jamovi Groups; the optional sections appear only when they
  are run).

# PLSsemLab 0.7.1

* Fix: ticking Run multigroup analysis in jamovi stopped with "rowKey not
  found". jamovi runs `.init()` with the header of the data only (no rows),
  so no group was found and no rows were laid out; `.init()` now uses the
  levels of the factor and `.run()` rebuilds the rows when a level has no
  cases. A test reproduces jamovi's header-only `.init()`.

# PLSsemLab 0.7.0

## Phase 7: multigroup analysis (D6)

* **Multigroup Analysis** box (its own Run checkbox): a grouping variable,
  permutations, bootstrap resamples per group and an optional correction
  for multiple comparisons (Holm, Bonferroni). The seed is the bootstrap's.
* **Groups** table: cases per group and whether each group's estimates are
  admissible (cases without a group value are left out, with a notice).
* **MICOM** (Henseler, Ringle & Sarstedt, 2016): step 1 guaranteed by the
  module; step 2 (c, its 5% permutation quantile and p) and step 3 (mean
  difference and log variance ratio with permutation p) for every pair of
  groups and construct, with the verdict none / partial / full. Equal to
  `cSEM::testMICOM()`; not computed with PLSc (common factors) or ordinal
  indicators, where the path differences are still computed.
* **Path differences**: the coefficient in each group, the difference, the
  permutation test (Chin & Dibbern, 2010) and PLS-MGA (Henseler et al.,
  2009) with cSEM's one-sided P(first ≤ second) and its two-sided p, and a
  decision that says when only one test finds a difference. Equal to
  `cSEM::testMGD()`. Notes explain the one-sided value, SmartPLS's
  .05/.95 rule (a two-sided .10 level) and which paths involve constructs
  without compositional invariance.
* Guide, interpretation (MICOM per pair, significant differences with both
  coefficients, disagreement between tests, small groups, multiple
  comparisons, observed groups), key finding and equivalent R code.
* Not computable (rows kept, with the reason) with higher-order constructs,
  fewer than two groups, more than 10 groups, a group under 10 cases or
  inadmissible estimates in a group.
* The comparison by type of service of experiment E6 is reproduced exactly.
* **Reproducibility fix (also phases 5 and 6):** cSEM's bootstrap leaves
  the L'Ecuyer-CMRG generator set in the R session, and jamovi keeps one
  session, so after a bootstrap the permutations and the cross-validation
  folds no longer matched the equivalent R code. Every resampling routine
  now runs with R's default generator and restores the previous one.

# PLSsemLab 0.6.1

## Phase 6: mediation, prediction and IPMA (D6)

* *How to read each table* is now ticked by default.
* PLSpredict compares PLS with LM with the RMSE, or with the MAE when the
  prediction errors are clearly asymmetric (|skewness| > 1); the table shows
  both, the skewness and the metric used, and the guide explains the choice.
* The report says that Q²predict is not the Stone-Geisser Q² of
  blindfolding, and warns when the sample is small for the cross-validation.
* IPMA: with the bootstrap, constructs whose total effect is not significant
  are marked and do not set priorities; without it, a warning.
* Mediation with several chains between two constructs, and a next step in
  each phase-6 block of the interpretation.

* **Mediation:** every specific indirect effect (a chain of two or more
  paths) with its percentile bootstrap interval, computed from the same
  resamples as the paths, and the type of mediation of Zhao et al. (2010):
  complementary, competitive, indirect-only, direct-only or no effect. A
  chain that is the only route between two constructs reproduces cSEM's
  indirect effect and interval; works through higher-order constructs.
* **PLSpredict** (*Prediction* box, its own Run checkbox): Q²predict, RMSE
  and MAE of PLS and of the linear-model benchmark for every indicator of an
  endogenous construct, from a k-fold cross-validation repeated r times with
  cSEM's folds (one repetition equals `cSEM::predict()`), and the predictive
  power reading of Shmueli et al. (2019).
* **CVPAT** from the same cross-validation: one-sided tests against the
  indicator averages (IA) and the linear model (LM), for all endogenous
  constructs and for each one (Sharma et al., 2023); and, if you leave out
  paths in the Prediction box, a two-sided test against that competing model
  (equal to `cSEM::testCVPAT()`).
* **IPMA** (*Importance-Performance Map* box): importance (unstandardised
  total effects) and performance (0-100, with the scale range from the data
  or set by hand) for every endogenous construct, a map per target and a
  reading of what to improve first. Equal to `cSEM::doIPMA()`; not computed
  with PLSc.

## Phase 5: overall model fit (D1)

* **Overall fit test** (*Model Fit* box, its own Run checkbox): the
  bootstrap test of exact fit (Beran & Srivastava, 1985; Dijkstra &
  Henseler, 2015) with `cSEM::testOMF()`. SRMR, d_G, d_L and d_ML against
  their critical values, a bootstrap p-value and the decision, for the
  estimated and the saturated model together. The interpretation says where
  the misfit lies (structural or measurement model) and how to read a
  rejection with a low SRMR.
* The guide explains why there is no ML chi-square test, CFI or RMSEA.
* With interactions, higher-order constructs or an inadmissible solution the
  rows stay as "Not computable" with the reason.

## Phase 4: interactions and higher-order constructs (D3), closed

* **Two-way interactions:** drag two constructs together into a predictor
  box. The report adds the simple effects at −1, 0 and +1 SD of the
  moderator, a floodlight plot with the Johnson-Neyman bounds (from the
  bootstrap) and the f² of the interaction on Kenny's (2018) benchmarks.
  SRMR-type criteria are not defined with interactions; ρC is computed from
  the loadings.
* **Higher-order constructs:** a *Higher-Order Constructs* box builds
  reflective or formative second-order constructs from first-order ones.
  * Default: the disjoint two-stage approach (Sarstedt et al., 2019): in
    stage 2 the dimensions' scores measure the higher-order construct and
    every other construct keeps its own indicators. Both stages are
    resampled together in the bootstrap.
  * Option: cSEM's own second-order routine, whose
    estimates equal a direct `cSEM::csem()` call. A notice explains the three
    differences (paths among lower-order constructs in stage 1; other
    constructs measured by their scores in stage 2; a reflective
    higher-order construct as a common factor with consistent PLS in
    stage 2).
  * Discriminant validity of each reflective higher-order construct with the
    other reflective constructs: HTMT, HTMT2 and the bootstrap upper bound in
    stage 2, never against its own dimensions. With cSEM's routine the rows
    stay as "Not computable".
  * The interpretation and the key findings report the higher-order
    constructs (loadings or weights, reliability, AVE, VIF, HTMT).
* **Warnings in the key findings:** pairs of constructs without
  discriminant validity even on the lenient threshold (bootstrap upper bound
  of the HTMT of 0.90 or more; the estimate without the bootstrap), first
  order and higher order.
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
  with cSEM 0.7.1 that every requirement of the first version (D1–D6)
  can be solved in the first version, with their results and a GitHub
  Actions workflow to run them without a local R installation.
* Design document (`docs/DESIGN.md`).
* Project documents adopted from MokkenLab, EGAnetLab and AssumptionsLab and
  adapted to PLS-SEM: `ARCHITECTURE.md`, `DEVELOPER_GUIDE.md`,
  `CODE_STYLE.md`, `CONTRIBUTING.md`, `docs/Bibliography.md`, citation files.
