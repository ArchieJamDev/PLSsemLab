# PLSsemLab — project context for Claude

PLSsemLab is a jamovi module for PLS-SEM by Arquímedes De León Chacón
(Archie), part of his "Lab" suite: AssumptionsLab, EGAnetLab, MokkenLab,
FiabilityLab (all at github.com/ArchieJamDev). Status: 1.0.1 submitted to
the jamovi library (main stays at 1.0.1 until jamovi answers); 1.1.0 (NCA,
MGCFA) released 2026-11-23 from branch v1.1-nca-invariance, DOI
10.5281/zenodo.24585551 (all versions: 10.5281/zenodo.24259622); 1.2.0
(weighting approaches, jackknife, Mardia, scores, benchmarks, model
comparison) on branch v1.2-weights-compare.

## Goal of v1

Be a complete PLS-SEM module for jamovi and meet, from the first release, six
requirements of a complete PLS-SEM analysis (priority = most requested by
users first): D2 HTMT/VIF with bootstrap CI · D3 higher-order constructs and
interactions · D4 bootstrap speed/robustness · D5 syntax errors and help ·
D1 wrong (ML) overall fit test · D6 f², Q²/PLSpredict, CVPAT, mediation,
IPMA, MICOM. Engine: cSEM. Design: `docs/DESIGN.md`. Evidence for each fix:
`experiments/design-v1/` (see its README). Suite rules: `ARCHITECTURE.md`,
`DEVELOPER_GUIDE.md`, `CODE_STYLE.md` (adopted from MokkenLab, the most
recent module after jamovi's review; helper prefix `.pl_`).

## Non-negotiable philosophy

- Teach, not only compute: every result answers what happened, why, what it
  means and what to do next (AssumptionsLab's "Applied Interpretation").
- Academic audience; methodological honesty over convenience.
- Interface like EGAnetLab: models are built from boxes (ListBox of named
  constructs with drag-and-drop indicators, as EGAnetLab's *Theoretical
  structure* and jamovi's CFA), never typed syntax. Construct names for the
  structural model come from a JS-populated Supplier (SEMLj's pattern,
  `semljgui.u.yaml` + `gui.events.js`).

## Rules learned from jamovi's reviews of the earlier modules

Read EGAnetLab's ARCHITECTURE.md, DEVELOPER_GUIDE.md and CODE_STYLE.md; they
carry every rule below in full.

- One language mechanism: jamovi i18n (`.()` + `jamovi/i18n/es.po`). No
  per-analysis report-language option.
- Plot colours follow jamovi's theme/palette (`ggtheme`, `theme`); no module
  palette option.
- Plot data go through `image$state` (export renders from a fresh instance).
- Backtick/rename every user name: cSEM fails with spaces in variable names
  and, depending on locale, with accents — use internal C1..Ck / V1..Vp.
- Category A failures -> `jmvcore::reject()` with the real reason; Category B
  -> the row stays, "Not computable" + specific footnote. Never drop rows.
- Tables keep their shape: rows/columns known from options are declared in
  `.r.yaml` or `.init()`; optional sections that fail keep their place and
  explain why with a jamovi notice.
- Package warnings become jamovi notices, not an end-of-report block.
- Numeric table columns follow jamovi's decimal and p-value settings.
- Long computations: explicit "Run" checkbox off by default, strict
  `clearWith`, cached results (small, capped), `private$.checkpoint()`,
  seed exposed in the panel and printed in the report.
- Version 1.0.0 everywhere for a library submission; module icon; bundled
  datasets with source and licence; Title Case for option boxes.
- Reproducibility record with the equivalent R code (MokkenLab pattern).
- `validation/` script checking numerical equivalence with direct cSEM
  calls, run in CI.

## Findings from the design experiments (cSEM 0.7.1)

- Reference data `corp_rep_data` codes missing as -99: detect sentinel codes
  before estimating (CUSL alpha 0.831 -> 0.421 if ignored).
- rho_A exists only under PLSc; HTMT with inference is packed in one matrix
  (estimate lower triangle, upper bound upper triangle).
- `assess()` refuses nonlinear models: compute every criterion separately;
  SRMR/d_G/d_L are not defined with interactions (Category B).
- Bootstrap ≈ 19 ms/draw (n = 344, extended model); blocks of 200 with
  derived seeds reproduce a single run (max CI difference 0.016).
- cSEM `=~` = common factor, `<~` = composite; IPMA/MICOM are meant for
  composites. Measurement model (reflective/formative) and estimator
  (PLS/PLSc) are separate user choices; the module maps them to syntax.
- `testMGD(.output_type = "structured")` fails in cSEM 0.7.1; use default.
- With PLS the reflective constructs are `<~` Mode A composites: call the
  quality functions with `.only_common_factors = FALSE` (identical values).
  `cSEM::csem()` defaults to `.disattenuate = TRUE`: always pass it.
- HTMT of a single-indicator construct takes its monotrait correlation as 1
  (lower bound); mixed signs give NaN (reverse-coded items).
- Ordered-factor indicators make cSEM use polychoric correlations (alpha,
  HTMT, paths), but `calculateVIFModeB()` regresses the raw codes: compute
  VIF as `diag(solve(Indicator_VCV[block, block]))` (equal for Pearson).
- rho_C is omega with the model's loadings: PLS inflates it (~+0.07 on
  corp_rep); a one-factor lavaan CFA omega ~ rho_C of PLSc.
- Disjoint two-stage HOC (Sarstedt et al., 2019, Fig. 4): in stage 2 only
  the HOC is measured by stage-1 scores; every other construct keeps its
  own indicators. cSEM's `calculate2ndStage` uses scores for all (and adds
  paths among LOCs in stage 1): it is the optional "cSEM routine" approach.
- cSEM accepts a path to one dimension of a second-order construct without
  warning and drops it from the reported stage 2: the module rejects it.
- D1 is about reading and completeness: testOMF() for the estimated and the
  saturated model together, with decision and reading; no ML chi-square nor
  CFI/GFI/RMSEA-type indices.
- `testOMF()` returns `Critical_value` columns in its own order and keeps
  admissible draws in `Information$Bootstrap_values`; `"<~"` without
  `.PLS_modes` is Mode B (pass Mode A in reference calls).
- Multigroup (phase 7): `testMGD()`'s Henseler p is
  P(first ≤ second), one-sided; the module reports 2·min(P, 1−P) (cSEM's
  own decision rule); SmartPLS's .05/.95 rule is a two-sided .10 level.
  The group column is `PLgroup` with labels G01..Gk (cSEM rejects dots;
  sign of differences = level order). `testMICOM()` stops with ordinal
  indicators; PLSc is inadmissible in corp_rep's Contract group. E6 is
  reproduced exactly only with its group order (1 = 125 cases first).
- cSEM's bootstrap (future.seed) leaves `RNGkind()` = L'Ecuyer-CMRG in the
  session; jamovi keeps one R session, so later set.seed() + sample()
  (MICOM, Chin, CV folds) changed. Every resampling routine runs under
  `.pl_rng_guard()` (default generator, previous one restored); tests wrap
  direct cSEM calls in `withDefaultRng()`.
- jamovi runs `.init()` with `readDataset(headerOnly = TRUE)`: zero rows.
  Rows that depend on data values (group levels) must come from the factor
  levels in `.init()` and be rebuilt in `.run()` if they differ; tests that
  pass the full data to `.init()` hide this (see the header-only test).
- jamovi's `private$.checkpoint()` signals a restart with an error whose
  `code` is "restart": every `tryCatch` that may run a checkpoint must
  rethrow it (`.pl_capture()` does; `.pl_is_restart()`), or the run goes on
  with the old options (MokkenLab's second audit, MEDIUM). cSEM reports each
  resample through progressr inside its own `with_progress()`: a handler set
  in `options(progressr.handlers = ..., progressr.enable = TRUE)` is the only
  way to see them (`.pl_with_ticks()`); results are identical.
- cSEM 0.7.1's `predict(.benchmark = "lm")` solves (X'X)^-1 X'y on the raw
  data without an intercept (regression through the origin): LM errors are
  inflated. The module fits LM itself with an intercept
  (`.pl_lm_residuals()`), which reproduces Hair et al.'s (2022) RMSE.
- R's `$` partially matches list names (`s$micom` returns `s$micom_error`
  when `micom` is absent): read cached summaries with `[[ ]]`.
- The .jmo must match the user's jamovi R version exactly (`rVersion` in
  jamovi.yaml vs jamovi's `JAMOVI_R_VERSION`), else "Installed version
  needs to be updated".
- Archie uses jamovi on Windows: build with `build-jmo-windows.yml`
  (PLSsemLab_<v>_win64.jmo, release dev-build-windows), which also runs the
  engine probe in that R. The Linux .jmo bundles Linux .so files (expm,
  RcppArmadillo link libRlapack/libRblas, absent in jamovi's Linux R too):
  testOMF() fails with it.
- `jmc --prepare` deletes `inst/i18n/es.json`; rebuild it with
  `--i18n . --update`, fill `es.po`, then `--build . --skip-deps`.
- NCA (1.1.0, R/nca.R) is computed by the module: NCA 5.1.0's `p_peers()`
  loops in R (10 000 permutations of 3 conditions: >15 min, not
  cancellable; the module: ~11 s with checkpoints). Same definitions as
  `nca_analysis()` defaults; validated in `validation/nca_equivalence.R`.
  Details: a single distinct peer gives CR-FDH effect 0 (not NaN); the
  package's purity metrics draw random numbers, so its permutation offsets
  differ from the module's for the same seed (validation traces `p_test`
  for the offsets); permutations often rebuild the same peers, so effects
  tie with the observed one exactly and the package's raw `>=` drops some
  ties by rounding noise: the module counts ties within 1e-10; the
  package's bottleneck table prints one decimal. Scores = IPMA scores
  (W / sd(x), normalized, 0-100 with the IPMA range). NCA is not in
  DESCRIPTION (jmc bundles Suggests into the .jmo); CI installs it for the
  validation only.
- MGCFA (1.1.0, R/invariance.R): lavaan configural/metric/scalar on the
  reflective constructs with 2+ indicators; MLR default; configural judged
  by CFI and SRMR (RMSEA inflates with few df), steps by Chen (2007).
- Weighting approaches (1.2.0, R/weights.R): every csem() call goes through
  `.pl_csem_args()`. cSEM 0.7.1: SSQCORR/MINVAR give reversed signs (left
  out); MAXVAR flips all signs in ~20% of bootstrap draws and
  `.sign_change_option` does nothing for it (`.pl_fix_signs()` corrects per
  construct); doIPMA is PLS-PM only; GSCA has no nonlinear terms; PCA fails
  with single-indicator constructs; MAXVAR + disattenuation is absurd.
- Jackknife (1.2.0): cSEM's sd is the jackknife SE, but its
  CI_standard_t subtracts mean - estimate (bootstrap bias); the module uses
  estimate +/- t(n-1) SE. Number_of_runs keeps 499; use
  Number_of_observations.
- cSEM 0.7.1 `predict(.test_data = ...)` returns the target residuals as
  the benchmark's (benchmark not applied); its CV mode applies it. The
  module refits the benchmark approach on each training fold.
- Keep private users' names out of the repository (credit only with their
  consent).

## Working conventions

- No mention of, or comparison with, JASP in any file of the repository, the
  module texts, commits or releases. Comparisons with JASP are internal only
  and stay outside GitHub.
- Talk to Archie in Spanish, always with "tú" — never voseo (tenés, podés,
  querés...). Check every -ás/-és/-ís verb before sending.
- Source comments: English first, then a `# ES:` Spanish block.
- Archie's computer is limited: heavy runs go to GitHub Actions
  (`.github/workflows/design-experiments.yml`) or a cloud session.
- Validate statements against real runs; do not claim a fix without running it.
