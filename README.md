<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/logo/plssemlab-logo-dark.svg">
    <img src="docs/logo/plssemlab-logo.svg" alt="PLSsemLab" width="560">
  </picture>
</p>

# PLSsemLab

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24259623.svg)](https://doi.org/10.5281/zenodo.24259623)

A jamovi module for partial least squares structural equation modeling
(PLS-SEM) with the cSEM R package, with guided interpretation in the style
of AssumptionsLab, EGAnetLab and MokkenLab: every result says what happened,
why, what it means and what to do next, in English and Spanish.

## What it does

- **Models built from boxes**, no syntax: reflective and formative
  constructs, structural paths, higher-order constructs and interactions,
  with a validator that explains each modeling mistake.
- **Estimators:** PLS and consistent PLS (PLSc); continuous or ordinal
  indicators (polychoric correlations).
- **Measurement model:** Cronbach's alpha, rho_A, rho_C, AVE, HTMT and HTMT2
  with bootstrap intervals, Fornell-Larcker, indicator VIF, loadings and
  weights.
- **Structural model:** paths, R², f², VIF, direct, indirect and total
  effects; bootstrap in blocks with intervals for every measure, cancellable
  and with a visible seed; path diagram, loadings and HTMT plots.
- **Higher-order constructs** (disjoint two-stage approach by default; cSEM's
  second-order routine as an option) and **two-way interactions** with simple
  effects, floodlight and Johnson-Neyman.
- **Overall model fit:** bootstrap test with SRMR, d_G, d_L and d_ML for the
  estimated and the saturated model, with decision and reading.
- **Prediction and more:** PLSpredict, CVPAT (against the indicator average,
  a linear model and a competing model), mediation with specific indirect
  effects, and the importance-performance map (IPMA).
- **Multigroup analysis:** MICOM (steps 1-3) and path differences
  (permutation test and PLS-MGA).
- **Reproducibility:** the equivalent R code of every analysis.

## Install

**Windows (jamovi 2.7.41):** download `PLSsemLab_<version>_win64.jmo` from
the [latest release](https://github.com/ArchieJamDev/PLSsemLab/releases/latest)
and, in jamovi, open **⋮ › Modules › Install from file**. A `.jmo` only loads
in the jamovi (R version) it was built for.

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

## Documentation

- `docs/DESIGN.md` — the design (in Spanish): engine, interface,
  architecture and validation.
- `ARCHITECTURE.md`, `DEVELOPER_GUIDE.md`, `CODE_STYLE.md`, `CONTRIBUTING.md`
  — the suite's rules, adapted to PLS-SEM with cSEM.
- `docs/Bibliography.md` — APA 7th edition references.
- `experiments/design-v1/` — reproducible R scripts with the evidence behind
  each design decision.
- `NEWS.md` — changes in each version.

## Citation

If you use PLSsemLab, please cite it:

> Chacón Chacón, A. D. L. (2026). *PLSsemLab: A jamovi module for partial
> least squares structural equation modeling* (Version 0.7.5) [Computer
> software]. Zenodo. https://doi.org/10.5281/zenodo.24259623

The metadata are also in `CITATION.cff` (GitHub's *Cite this repository*
button). Please cite cSEM and the methods you
report as listed in `docs/Bibliography.md`.

---

# PLSsemLab (ES)

Un módulo de jamovi para modelos de ecuaciones estructurales por mínimos
cuadrados parciales (PLS-SEM) con el paquete cSEM de R, con interpretación
guiada al estilo de AssumptionsLab, EGAnetLab y MokkenLab: cada resultado
dice qué pasó, por qué, qué significa y qué hacer después, en español e
inglés.

**Qué hace:** modelos armados con cajas, sin sintaxis; PLS y PLS consistente;
fiabilidad y validez (HTMT con intervalos bootstrap); modelo estructural con
bootstrap por bloques; constructos de orden superior e interacciones con
pendientes simples y Johnson-Neyman; ajuste global por bootstrap (SRMR, d_G,
d_L); PLSpredict, CVPAT, mediación, IPMA y análisis multigrupo (MICOM, PLS-MGA
y prueba de permutación); y el código R equivalente de cada análisis.

**Instalación:** en Windows (jamovi 2.7.41), descargar
`PLSsemLab_<versión>_win64.jmo` de la
[última versión](https://github.com/ArchieJamDev/PLSsemLab/releases/latest)
y abrirlo en jamovi con **⋮ › Módulos › Instalar desde archivo**.

**Conjuntos de datos:** el módulo incluye cuatro (Abrir > Biblioteca de
datos > PLS-SEM Lab): reputación corporativa (Hair et al.), índice de
satisfacción ECSI, clientes de bancos (Damberg, 2023) y unos datos simulados
para aprender, con un modelo poblacional conocido en el que cada opción
tiene algo que mostrar. `data/README.md` indica la fuente, la licencia y qué
conjunto usar para probar cada opción.

**Cita:** Chacón Chacón, A. D. L. (2026). *PLSsemLab: A jamovi module for
partial least squares structural equation modeling* (Versión 0.7.5)
[Software]. Zenodo. https://doi.org/10.5281/zenodo.24259623. Cite también cSEM y los métodos que reporte según `docs/Bibliography.md`.

## License

GNU General Public License v3.0 or later.
