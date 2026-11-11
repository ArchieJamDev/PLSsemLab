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

**Status:** in development (phase 2 of 10). Not yet in the jamovi Library;
it can be built from source for testing.

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

With jamovi installed, clone this repository and, in R, from its folder:

```r
install.packages("jmvtools", repos = c("https://repo.jamovi.org", "https://cloud.r-project.org"))
jmvtools::install()
```

Then open the bundled *Corporate reputation* data in jamovi (Open > Data
Library > PLS-SEM Lab) and run *PLS-SEM Lab > PLS-SEM*.

## Roadmap

| Phase | Content | Status |
|---|---|---|
| 0 | Design, experiments with cSEM for every JASP shortcoming, suite documents, logo | ✅ Done |
| 1 | Platform, model built from boxes, model validation, first estimates | 🔄 Built, CI green — awaiting test in jamovi |
| 2 | Measurement model: reliability, AVE, HTMT, HTMT2, VIF, Fornell-Larcker | 🔄 Built, CI pending — awaiting test in jamovi |
| 3 | Structural model, bootstrap in blocks, path diagram | 🗓 Planned |
| 4 | Higher-order constructs and interactions | 🗓 Planned |
| 5 | Bootstrap test of overall model fit | 🗓 Planned |
| 6 | Prediction (PLSpredict, CVPAT), mediation, IPMA | 🗓 Planned |
| 7 | Multigroup analysis with MICOM | 🗓 Planned |
| 8 | Validation against cSEM, Hair et al. and JASP; stress tests | 🗓 Planned |
| 9 | First public release (0.1.0) with Zenodo DOI | 🗓 Planned |
| 10 | Version 1.0.0 and jamovi Library submission | 🗓 Planned |

Details of each phase and its completion criterion: `docs/DESIGN.md` §8.

---

# PLSsemLab (ES)

Un módulo de jamovi para modelos de ecuaciones estructurales por mínimos
cuadrados parciales (PLS-SEM), con interpretación guiada al estilo de
AssumptionsLab, EGAnetLab y MokkenLab.

**Estado:** en desarrollo (fase 1 de 10). Todavía no está en la biblioteca
de jamovi; se puede compilar desde el código fuente para probarlo.

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
