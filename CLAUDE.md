# PLSsemLab — project context for Claude

PLSsemLab is a jamovi module for PLS-SEM by Arquímedes De León Chacón
(Archie), part of his "Lab" suite: AssumptionsLab, EGAnetLab, MokkenLab,
FiabilityLab (all at github.com/ArchieJamDev). Status: design stage.

## Goal of v1

Be the first PLS-SEM module for jamovi and fix, from the first release, the
six shortcomings documented for JASP's PLS-SEM (priority = reported by users
first): D2 HTMT/VIF with bootstrap CI · D3 higher-order constructs and
interactions · D4 bootstrap speed/robustness · D5 syntax errors and help ·
D1 wrong (ML) overall fit test · D6 f², Q²/PLSpredict, CVPAT, mediation,
IPMA, MICOM. Engine: cSEM (same as JASP; lets results be checked against
JASP). Design: `docs/DESIGN.md`. Evidence for each fix:
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
  paths among LOCs in stage 1): it is the optional "as JASP" approach.
- cSEM accepts a path to one dimension of a second-order construct without
  warning and drops it from the reported stage 2: the module rejects it.
- The .jmo must match the user's jamovi R version exactly (`rVersion` in
  jamovi.yaml vs jamovi's `JAMOVI_R_VERSION`), else "Installed version
  needs to be updated".
- `jmc --prepare` deletes `inst/i18n/es.json`; rebuild it with
  `--i18n . --update`, fill `es.po`, then `--build . --skip-deps`.

## Working conventions

- Talk to Archie in Spanish, always with "tú" — never voseo (tenés, podés,
  querés...). Check every -ás/-és/-ís verb before sending.
- Source comments: English first, then a `# ES:` Spanish block.
- Archie's computer is limited: heavy runs go to GitHub Actions
  (`.github/workflows/design-experiments.yml`) or a cloud session.
- Validate statements against real runs; do not claim a fix without running it.
