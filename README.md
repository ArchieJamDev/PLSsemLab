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

**Status:** design stage. No analysis is available in jamovi yet.

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

---

# PLSsemLab (ES)

Un módulo de jamovi para modelos de ecuaciones estructurales por mínimos
cuadrados parciales (PLS-SEM), con interpretación guiada al estilo de
AssumptionsLab, EGAnetLab y MokkenLab.

**Estado:** fase de diseño. Todavía no hay ningún análisis disponible en jamovi.

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
