# PLSsemLab

A jamovi module for partial least squares structural equation modeling
(PLS-SEM), with guided interpretation in the style of AssumptionsLab,
EGAnetLab and MokkenLab.

**Status:** design stage. No analysis is available in jamovi yet.

## What is here now

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

- `experiments/design-v1/` — scripts de R reproducibles que comprueban, con
  cSEM, que cada deficiencia documentada del PLS-SEM de JASP se puede resolver
  en la primera versión. Su README resume los hallazgos.
- `.github/workflows/design-experiments.yml` — ejecuta esos scripts en las
  máquinas de GitHub (pestaña Actions → *design-experiments* → *Run
  workflow*); los resultados quedan adjuntos a la ejecución como artefacto.

## License

GNU General Public License v3.0 or later.
