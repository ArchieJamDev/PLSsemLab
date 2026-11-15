<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/logo/plssemlab-logo-dark.svg">
    <img src="docs/logo/plssemlab-logo.svg" alt="PLSsemLab" width="560">
  </picture>
</p>

# PLSsemLab

A jamovi module for partial least squares structural equation modeling
(PLS-SEM), with guided interpretation in the style of AssumptionsLab,
EGAnetLab and MokkenLab.

**Status:** in development (phase 8 of 10: validation). Not yet in the
jamovi Library; it can be built from source for testing.

## What is here now

- `docs/DESIGN.md` — the design of version 1 (in Spanish): the six
  shortcomings of JASP's PLS-SEM it fixes, the engine, the interface, the
  architecture, validation and open decisions.
- `ARCHITECTURE.md`, `DEVELOPER_GUIDE.md`, `CODE_STYLE.md`, `CONTRIBUTING.md`
  — the suite's rules, adopted from MokkenLab, EGAnetLab and AssumptionsLab
  and adapted to PLS-SEM with cSEM.
- `docs/Bibliography.md` — APA 7th edition references.
- `experiments/design-v1/` — reproducible R scripts that check, with cSEM,
  that each shortcoming documented for JASP's PLS-SEM can be solved in the
  first version. See its README for the findings.
- `.github/workflows/design-experiments.yml` — runs those scripts on GitHub's
  machines (Actions tab → *design-experiments* → *Run workflow*); results are
  attached to the run as an artifact.

## Try it (development version)

**Windows (jamovi 2.7):** download `PLSsemLab_<version>_win64.jmo` from the
[dev-build-windows pre-release](https://github.com/ArchieJamDev/PLSsemLab/releases/tag/dev-build-windows)
and, in jamovi, open **⋮ › Modules › Install from file**. **Linux (jamovi
from Flathub):** the same from the
[dev-build pre-release](https://github.com/ArchieJamDev/PLSsemLab/releases/tag/dev-build).
They are rebuilt by the *Build .jmo* workflows; a `.jmo` only loads in the
jamovi (R version) it was built for.

**Any system, from source:** with jamovi installed, clone this repository
and, in R, from its folder:

```r
install.packages("jmvtools", repos = c("https://repo.jamovi.org", "https://cloud.r-project.org"))
jmvtools::install()
```

Then open a bundled dataset in jamovi (Open > Data Library > PLS-SEM Lab)
and run *PLS-SEM Lab > PLS-SEM*.

## Datasets

Four datasets ship with the module (Open > Data Library > PLS-SEM Lab):

- **Corporate reputation** (n = 344), the example of Hair et al.'s PLS-SEM
  primer: reflective and formative constructs, missing answers, service type
  for a multigroup analysis.
- **Customer satisfaction index ECSI** (n = 250): image, expectation,
  quality, value, satisfaction and loyalty; a moderation and PLSc.
- **Bank customers** (n = 675, Damberg, 2023, CC BY 4.0): four formative and
  five reflective constructs with demographics.
- **Learning data** (n = 600, simulated, CC0): a known population model in
  which every option has something to show: a higher-order construct, a
  moderation, a mediation, a difference between sectors and none between
  regions, the importance-performance map and missing answers.

`data/README.md` gives the source and licence of each one and a table of
which dataset and model to use to try each option.

## Roadmap

| Phase | Content | Status |
|---|---|---|
| 0 | Design, experiments with cSEM for every JASP shortcoming, suite documents, logo | ✅ Done |
| 1 | Platform, model built from boxes, model validation, first estimates | ✅ Done |
| 2 | Measurement model: reliability, AVE, HTMT, HTMT2, VIF, Fornell-Larcker, ω, ordinal indicators | ✅ Done |
| 3 | Structural model (f², VIF, effects), bootstrap in blocks with intervals for every measure, path diagram, loadings and HTMT plots | ✅ Done |
| 4 | Two-way interactions (simple effects, floodlight, Johnson-Neyman) and higher-order constructs (disjoint two-stage by default; cSEM's routine as an option), with stage-2 discriminant validity | ✅ Done |
| 5 | Bootstrap test of overall model fit (estimated and saturated model, with decision and reading) | ✅ Done |
| 6 | Mediation (specific indirect effects), PLSpredict, CVPAT (IA, LM and a competing model) and IPMA | ✅ Done |
| 7 | Multigroup analysis: MICOM (steps 1-3) and path differences (permutation test and PLS-MGA) | ✅ Done |
| 8 | Validation against cSEM, Hair et al. and JASP; stress tests | 🗓 Planned |
| 9 | First public release (0.1.0) with Zenodo DOI | 🗓 Planned |
| 10 | Version 1.0.0 and jamovi Library submission | 🗓 Planned |

Details of each phase and its completion criterion: `docs/DESIGN.md` §8.

---

# PLSsemLab (ES)

Un módulo de jamovi para modelos de ecuaciones estructurales por mínimos
cuadrados parciales (PLS-SEM), con interpretación guiada al estilo de
AssumptionsLab, EGAnetLab y MokkenLab.

**Estado:** en desarrollo (fase 8 de 10: validación). Todavía no está en
la biblioteca de jamovi; se puede compilar desde el código fuente para
probarlo.

**Conjuntos de datos:** el módulo incluye cuatro (Abrir > Biblioteca de
datos > PLS-SEM Lab): reputación corporativa (Hair et al.), índice de
satisfacción ECSI, clientes de bancos (Damberg, 2023) y unos datos simulados
para aprender, con un modelo poblacional conocido en el que cada opción
tiene algo que mostrar. `data/README.md` indica la fuente, la licencia y qué
conjunto usar para probar cada opción.

## Qué hay ahora

- `docs/DESIGN.md` — el diseño de la versión 1: las seis deficiencias del
  PLS-SEM de JASP que corrige, el motor, la interfaz, la arquitectura, la
  validación y las decisiones abiertas.
- `ARCHITECTURE.md`, `DEVELOPER_GUIDE.md`, `CODE_STYLE.md`, `CONTRIBUTING.md`
  — las reglas de la suite, adoptadas de MokkenLab, EGAnetLab y
  AssumptionsLab y adaptadas a PLS-SEM con cSEM.
- `docs/Bibliography.md` — referencias en APA 7.ª edición.
- `experiments/design-v1/` — scripts de R reproducibles que comprueban, con
  cSEM, que cada deficiencia documentada del PLS-SEM de JASP se puede resolver
  en la primera versión. Su README resume los hallazgos.
- `.github/workflows/design-experiments.yml` — ejecuta esos scripts en las
  máquinas de GitHub (pestaña Actions → *design-experiments* → *Run
  workflow*); los resultados quedan adjuntos a la ejecución como artefacto.

## License

GNU General Public License v3.0 or later.
