# PLSsemLab Architecture

**Version:** 0.1 (2026-11-10: adopted from MokkenLab's ARCHITECTURE.md v1.0,
itself adopted from EGAnetLab's v1.0 and AssumptionsLab's v1.2, keeping every
architectural rule that came out of jamovi's reviews of those modules;
sections 1, 4, 6, 10, 14 and 16 rewritten for PLS-SEM with cSEM, from the
design experiments in `experiments/design-v1/`)
**Project:** PLSsemLab
**License:** GNU General Public License v3.0
**Author:** Arquímedes De León Chacón Chacón

---

# Table of Contents

1. Introduction
2. Architectural Philosophy
3. High-Level Architecture
4. Directory Structure
5. Module Architecture
6. Data Flow
7. Analysis Lifecycle
8. Report Generation Architecture
9. Interpretation Engine
10. Graphical Architecture
11. Methodological Library
12. Internationalization
13. Documentation Architecture
14. Future Expansion
15. Design Principles

16. cSEM-Specific Architecture

---

# 1. Introduction

PLSsemLab has been designed as a modular scientific software platform for
estimating and evaluating structural equation models with partial least
squares (PLS-SEM, composite-based SEM), and supporting methodological
decision making.

The architecture prioritizes

• scientific rigor;

• modularity;

• maintainability;

• educational value;

• reproducibility.

Every component has a clearly defined responsibility.

-------------------------------------------------------------------------------

# Introducción

PLSsemLab ha sido diseñado como una plataforma modular para la estimación y
evaluación de modelos de ecuaciones estructurales por mínimos cuadrados
parciales (PLS-SEM, SEM basado en compuestos) y el apoyo a la toma de
decisiones metodológicas.

La arquitectura prioriza

• rigor científico;

• modularidad;

• mantenibilidad;

• valor educativo;

• reproducibilidad.

Cada componente posee una responsabilidad claramente definida.

-------------------------------------------------------------------------------

# 2. Architectural Philosophy

The architecture follows five fundamental principles.

### Separation of responsibilities

Each file performs one well-defined task.

### Progressive workflow

Every analysis follows the same logical sequence.

### Educational software

The software explains statistical decisions rather than merely producing
results.

### Scientific transparency

Every methodological decision can be identified inside the source code.

### Scalability

New analyses should integrate without modifying the existing architecture.

-------------------------------------------------------------------------------

# Filosofía arquitectónica

Cada nuevo módulo debe adaptarse a la arquitectura existente.

La arquitectura nunca debe adaptarse a un módulo específico.

-------------------------------------------------------------------------------

# 3. High-Level Architecture

```
                USER

                  │

                  ▼

        User Interface (.yaml)

                  │

                  ▼

        Analysis Engine (.R)

                  │

                  ▼

    Statistical Computations

                  │

                  ▼

      Diagnostic Evaluation

                  │

                  ▼

 Methodological Interpretation

                  │

                  ▼

        Report Generation

                  │

                  ▼

            Final Output
```

-------------------------------------------------------------------------------

# Arquitectura general

Toda información sigue un flujo unidireccional.

No existen ciclos innecesarios.

-------------------------------------------------------------------------------

# 4. Directory Structure

```
PLSsemLab/

│

├── R/

├── jamovi/

│   └── assets/

├── inst/

│   └── assets/

├── data/

├── data-raw/

├── docs/

├── tests/

├── .github/

├── README.md

├── LICENSE

├── CODE_STYLE.md

├── DEVELOPER_GUIDE.md

├── ARCHITECTURE.md

├── NEWS.md

├── DESCRIPTION

└── NAMESPACE
```

-------------------------------------------------------------------------------

## Responsibilities

### R/

Implements every statistical algorithm. `R/shared-helpers.R` holds every
small function (`.pl_*()`) duplicated identically across two or more
analysis files — translation, HTML block builders, row-adding wrappers,
number cleaning, plot palettes and shared plot settings, indicator
preparation (numeric check, sentinel missing-data codes, missing data), the
model builder (option boxes -> cSEM syntax with internal names) and cSEM
error capture.
A new duplicate belongs there, not copy-pasted into a third file. See
DEVELOPER_GUIDE.md §13.

### jamovi/

Defines analysis options and user interfaces. Also holds `jamovi/assets/`,
the module's own icon.

### inst/

Package-installed resources: icons/logos and the plain-text `CITATION` file.

### data/ and data-raw/

`data/` ships the bundled example datasets shown inside jamovi (registered
under `datasets:` in `jamovi/0000.yaml`); `data-raw/` holds the script that
produces them reproducibly from the cSEM package (corporate reputation data of
Hair et al., ECSI satisfaction data), with the -99 missing-data codes of the
original declared as missing.

### docs/

Contains project documentation, including the design document
(`docs/DESIGN.md`) and the APA 7th edition bibliography.

### experiments/

Design-stage evidence: small R scripts that checked, with cSEM, every
decision of the first version before any module code was written. They are
not module code and are not shipped in the `.jmo`.

### tests/

Stores validation procedures.

-------------------------------------------------------------------------------

# 5. Module Architecture

Every analysis consists of four primary components.

```
analysis.a.yaml

↓

analysis.u.yaml

↓

analysis.r.yaml

↓

analysis.b.R
```

-------------------------------------------------------------------------------

## analysis.a.yaml

Defines

• options

• variables

• controls

• defaults

-------------------------------------------------------------------------------

## analysis.u.yaml

Defines

• layout

• groups

• visibility

• interface organization

-------------------------------------------------------------------------------

## analysis.r.yaml

Defines

• result tables

• images

• HTML outputs

• textual reports

-------------------------------------------------------------------------------

## analysis.b.R

Implements

• validation

• computations

• diagnostics

• graphics

• interpretations

• reporting

-------------------------------------------------------------------------------

# 6. Data Flow

```
Model boxes (reflective constructs, formative constructs, higher-order
constructs, structural paths, interactions)

↓

Model Validation (Category A: >= 2 constructs, every construct has a name and
at least one indicator, no indicator in two blocks, no duplicated name, no
unknown construct in a path, no loop, no isolated construct)

↓

Indicator Preparation (numeric, not constant, sentinel codes such as -99
flagged before estimating, missing-data rule chosen by the user and reported)

↓

Model Builder (internal names C1..Ck and V1..Vp, cSEM syntax, maps back to
the user's names; readable syntax for the reproducibility record)

↓

cSEM estimation wrapped in .pl_capture() (errors -> reject, warnings ->
jamovi notices), admissibility checked with verify()

↓

Measurement model (loadings/weights, reliability, AVE, HTMT, VIF)

↓

Structural model (paths, R², f², VIF, effects), then optional sections:
bootstrap inference, overall fit test, prediction, mediation, IPMA,
multigroup

↓

Plots (state = what is drawn only), Interpretation Engine (four questions),
key findings, reproducibility record

↓

Output
```

-------------------------------------------------------------------------------

# Flujo de datos

Cada etapa depende únicamente de la anterior.

No deben existir cálculos redundantes: cada objeto devuelto por cSEM se
calcula una sola vez por ejecución y se reutiliza para tablas, gráficos e
interpretación. El usuario nunca escribe sintaxis: el modelo se arma desde
las cajas del panel, se valida antes de llamar a cSEM y se traduce a
nombres internos, porque cSEM falla con espacios en los nombres de variables
y, según la configuración regional, con acentos en los de constructos.

-------------------------------------------------------------------------------

# 7. Analysis Lifecycle

Every module should follow exactly the same lifecycle.

```
Initialization

↓

Validation

↓

Preparation

↓

Analysis

↓

Diagnostics

↓

Interpretation

↓

Recommendations

↓

Report

↓

Finish
```

This workflow must remain consistent throughout the project.

-------------------------------------------------------------------------------

# Ciclo de vida del análisis

La experiencia del usuario debe ser idéntica en todos los módulos.

-------------------------------------------------------------------------------

# 8. Report Generation Architecture

Reports are generated after every statistical computation has been completed.

The report is composed of

Introduction

↓

Descriptive Statistics

↓

Assumption Diagnostics

↓

Interpretation

↓

Recommendations

↓

References

Each section should be generated independently.

A report is only ever fully blocked (`jmvcore::reject()`) by a condition
that prevents the whole analysis from running. A single failed diagnostic
test never blocks the report — its row always renders, tiered "Not
computable" with a specific footnote. See CODE_STYLE.md §19.1.

-------------------------------------------------------------------------------

# Arquitectura del informe

La generación del informe nunca debe mezclarse con los cálculos estadísticos.

Un informe solo queda completamente bloqueado (`jmvcore::reject()`) por
una condición que impide ejecutar todo el análisis. Una única prueba
diagnóstica fallida nunca bloquea el informe — su fila siempre se
renderiza, como "No computable" con una nota al pie específica. Ver
CODE_STYLE.md §19.1.

-------------------------------------------------------------------------------

# 9. Interpretation Engine

The interpretation engine represents one of the most important components of
PLSsemLab.

Its objective is to transform statistical outputs into methodological
recommendations.

```
Statistical Results

↓

Methodological Rules

↓

Interpretation Templates

↓

Educational Explanation

↓

Recommendations
```

The engine should never merely reproduce numerical values.

-------------------------------------------------------------------------------

# Motor de interpretación

Toda interpretación debe responder

¿Qué ocurrió?

¿Por qué?

¿Qué significa?

¿Qué debe hacer ahora el investigador?

-------------------------------------------------------------------------------

# 10. Graphical Architecture

Graphs are organized according to methodological purpose.

```
Path diagram (constructs, indicators, estimates and significance)

↓

Measurement quality (loadings/weights with intervals, HTMT with upper bounds)

↓

Structural results (paths with intervals, f²)

↓

Applied analyses (simple slopes / floodlight of an interaction, IPMA map,
prediction errors against the benchmark, group differences)
```

cSEM draws path diagrams with DiagrammeR (Graphviz), which jamovi cannot
display. PLSsemLab draws its own path diagram in ggplot from the estimated
matrices, so it follows jamovi's Theme/Palette through `ggtheme` and survives
export (render functions read only `image$state`, which holds the layout and
the numbers to draw, never the cSEM object). Every other plot is built on
`+ ggtheme`; series colours come from jamovi's palette through
`jmvcore::colorPalette()`; there is no module palette option.

-------------------------------------------------------------------------------

# Arquitectura gráfica

Los gráficos constituyen herramientas metodológicas.

No elementos decorativos.

cSEM dibuja los diagramas de ruta con DiagrammeR (Graphviz), que jamovi no
puede mostrar. PLSsemLab dibuja su propio diagrama de ruta en ggplot a partir
de las matrices estimadas, para que siga el Tema/Paleta de jamovi vía
`ggtheme` y sobreviva a la exportación (las funciones de render leen solo
`image$state`, con la disposición y los números a dibujar, nunca el objeto de
cSEM). Los demás gráficos se construyen sobre `+ ggtheme`; los colores de las
series vienen de la paleta de jamovi mediante `jmvcore::colorPalette()`; no
hay opción de paleta propia.

-------------------------------------------------------------------------------

# 11. Methodological Library

Teaching lives inside every analysis rather than in a separate library
analysis: each table is preceded by a "How to read" block (option "How to
read each table" in the Report Content box, off by default so the output
stays compact) that says what the statistic measures, how
it works, what values to expect and the most common misreading; an "About
your choices" block explains every decision currently selected and when an
alternative fits better; and the interpretation answers the four questions
of §9. All of this is text in `texts.R` (sections `guide`, `choices` and one
per analysis), wrapped in `.()` and translated through jamovi's catalog
(§12).

Per-analysis citations use jamovi's native mechanism (`jamovi/00refs.yaml` +
`refs:` on each `Table`). The complete APA 7th edition reference list lives in
`docs/Bibliography.md`. There is no Bibliography analysis in the menu.

-------------------------------------------------------------------------------

# Biblioteca metodológica

La enseñanza vive dentro de cada análisis y no en un análisis de biblioteca
aparte: cada tabla va precedida de un bloque "Cómo leer" (opción de la caja Contenido del informe,
desactivada por defecto para que la salida sea compacta) que dice qué mide el estadístico, cómo funciona, qué valores esperar
y la lectura errónea más común; un bloque "Acerca de sus elecciones" explica
cada decisión seleccionada y cuándo conviene otra; y la interpretación
responde las cuatro preguntas de §9. Todo es texto de `texts.R`, envuelto en
`.()` y traducido con el catálogo de jamovi (§12).

Las citas por análisis usan el mecanismo nativo de jamovi
(`jamovi/00refs.yaml` + `refs:` en cada `Table`). La lista completa en APA 7.ª
edición vive en `docs/Bibliography.md`.

-------------------------------------------------------------------------------

# 12. Internationalization

PLSsemLab follows a bilingual philosophy, implemented as two separate
mechanisms that must not be confused with one another.

```
Source code                    Interface and report
comments                       (jamovi i18n)

English, then a Spanish        Follows jamovi's own language setting.
"# ES:" block below            jamovi/i18n/catalog.pot + es.po hold:
(CODE_STYLE.md)                 - every title/description/label in
                                  .a.yaml/.u.yaml/.r.yaml/0000.yaml,
                                  extracted with no markup;
                                - every .("...") string in R/*.R
                                  (texts.R, *.b.R, shared-helpers.R).
                               inst/i18n/es.json is built from es.po.
```

Both the interface and the report use jamovi's own catalog, not a custom
reimplementation. The compiler extracts the yaml strings and every `.()`
literal in the R sources; `jmvtools::i18nUpdate("es")` re-syncs `es.po` as
strings change, and `jmvtools::prepare()` writes `inst/i18n/es.json`. Because
gettext's model is one source string to one translation, every source string
is written in English; when one English word needs two Spanish translations,
it carries a context (`.("Mode A [inline]")`).

Report text lives in `R/texts.R` as `.pl_text_table(.)`, a function of the
translator, so every entry is a `.()` literal the compiler can see. Each run
builds it once with `.pl_lang(options)` and reads it with `.pl_text(lang,
section, key)`.

`.()` does not freeze. `jmvcore::Options$translate()` creates its translator
for each `Options` object, from the language jamovi sends with each run, and
`createTranslator()` caches one translator per package and language. A change
of jamovi's language therefore reaches the next run without a restart.
The tests run the same analysis in Spanish, English and Spanish again in
one process. (MokkenLab, from which this section is adopted, first kept a
per-analysis `reportLang` option on the belief that `.()` freezes; reading
jmvcore 2.7.38 showed that was wrong. PLSsemLab never has that option.)

A future language needs only one more `.po` file.

The module has no language option of its own: to read the results in
another language, the user changes jamovi's language in its settings.

-------------------------------------------------------------------------------

# Internacionalización

La arquitectura de PLSsemLab sigue una filosofía bilingüe, implementada como
dos mecanismos separados que no deben confundirse entre sí.

```
Comentarios de                 Interfaz e informe
código fuente                  (i18n de jamovi)

Inglés, seguido de un          Sigue el ajuste de idioma de jamovi.
bloque "# ES:" en español      jamovi/i18n/catalog.pot + es.po guardan:
(CODE_STYLE.md)                 - cada title/description/label de
                                  .a.yaml/.u.yaml/.r.yaml/0000.yaml,
                                  extraídos sin ninguna marca;
                                - cada string .("...") de R/*.R
                                  (texts.R, *.b.R, shared-helpers.R).
                               inst/i18n/es.json se genera desde es.po.
```

La interfaz y el informe usan el catálogo propio de jamovi, no una
reimplementación. El compilador extrae los strings del yaml y cada literal
`.()` de los fuentes R; `jmvtools::i18nUpdate("es")` resincroniza `es.po`
cuando cambian los strings, y `jmvtools::prepare()` escribe
`inst/i18n/es.json`. Como el modelo de gettext es un string fuente por cada
traducción, todo string fuente se escribe en inglés; cuando una palabra
inglesa necesita dos traducciones, lleva un contexto
(`.("Mode A [inline]")`).

El texto del informe vive en `R/texts.R` como `.pl_text_table(.)`, una
función del traductor, así que cada entrada es un literal `.()` visible para
el compilador. Cada ejecución lo construye una vez con `.pl_lang(options)` y
lo lee con `.pl_text(lang, section, key)`.

`.()` no se congela. `jmvcore::Options$translate()` crea su traductor para
cada objeto `Options`, con el idioma que jamovi envía en cada ejecución, y
`createTranslator()` guarda un traductor por paquete e idioma. Un cambio del
idioma de jamovi llega a la siguiente ejecución sin reiniciar.
Las pruebas ejecutan el mismo análisis en español, inglés y de nuevo
español en un solo proceso. (MokkenLab, de donde se adopta esta sección,
mantuvo al principio una opción `reportLang` por análisis creyendo que `.()`
se congela; la lectura de jmvcore 2.7.38 mostró que era un error. PLSsemLab
nunca tiene esa opción.)

Un idioma futuro solo necesita un archivo `.po` más.

El módulo no tiene opción de idioma propia: para leer los resultados en otro
idioma, el usuario cambia el idioma de jamovi en sus preferencias.

-------------------------------------------------------------------------------

# 13. Documentation Architecture

Documentation exists at four levels.

```
Repository

↓

Module

↓

Source File

↓

Function
```

Each level should answer progressively more detailed questions.

Repository

What is PLSsemLab?

Module

What analysis is implemented?

Source File

How is the analysis organized?

Function

How is each task performed?

-------------------------------------------------------------------------------

# Arquitectura documental

La documentación constituye un componente de la arquitectura.

No un elemento accesorio.

-------------------------------------------------------------------------------

# 14. Future Expansion

Version 1 covers the six shortcomings documented for JASP's PLS-SEM
(`docs/DESIGN.md`). Planned analyses, in release order:

| Phase | Analysis | cSEM functions |
| --- | --- | --- |
| 1 | PLS-SEM (estimation and evaluation) | `csem`, `verify`, `calculateRhoC`, `calculateRhoT`, `calculateAVE`, `calculateHTMT`, `calculateVIFModeB`, `calculatef2`, `summarize`, `infer`, `resamplecSEMResults`, `testOMF` |
| 1 | Prediction (in PLS-SEM) | `predict`, `testCVPAT` |
| 1 | Mediation, moderation, IPMA (in PLS-SEM) | effects of `summarize`, `doNonlinearEffectsAnalysis`, `doIPMA` |
| 1 | Multigroup PLS-SEM | `testMICOM`, `testMGD` |
| 2 | Other composite estimators | GSCA, GSCAm, PCA, MAXVAR (`.approach_weights`) |
| 3 | Unobserved heterogeneity and endogeneity | not in cSEM (FIMIX-PLS, PLS-POS: seminrExtras; Gaussian copula) — to be evaluated |

A new capability is added as options and results of the analysis it belongs
to, or as a new analysis, without structural changes.

-------------------------------------------------------------------------------

# Expansión futura

La versión 1 cubre las seis deficiencias documentadas del PLS-SEM de JASP
(`docs/DESIGN.md`). Cada capacidad nueva se agrega como opciones y
resultados del análisis al que pertenece, o como un análisis nuevo, sin
cambios estructurales.

-------------------------------------------------------------------------------

# 15. Design Principles

Every architectural decision should satisfy the following principles.

### Scientific

Algorithms should faithfully implement accepted statistical procedures.

### Educational

Users should understand every methodological decision.

### Modular

Components should remain independent whenever possible.

### Transparent

Every important decision should be documented.

### Maintainable

Future developers should understand the architecture without external guidance.

### Consistent

Every module should behave as part of the same software ecosystem.

### Reproducible

Analyses should produce reproducible results from identical data.

-------------------------------------------------------------------------------

# Principios de diseño

La arquitectura de PLSsemLab pretende equilibrar

ingeniería del software,

metodología estadística,

experiencia del usuario

y

valor educativo.

Estos principios deberán preservarse durante toda la evolución del proyecto.

-------------------------------------------------------------------------------

# 16. cSEM-Specific Architecture

Five conditions shape every PLSsemLab analysis. Each was found or confirmed
in the design experiments (`experiments/design-v1/`, cSEM 0.7.1).

**Names.** cSEM's parser drops backticks, so a variable name with a space
always fails, and an accented construct name fails in a non-UTF-8 locale.
The model builder renames constructs to C1..Ck and indicators to V1..Vp, keeps
the maps, and translates every table and plot back to the user's names. A
second-order model adds a `_temp` suffix to second-stage names, also mapped
back.

**Measurement model and estimator are two choices.** In cSEM, `=~` is a
common factor and `<~` a composite, while the PLS-SEM literature calls Mode A
constructs "reflective" and estimates them as composites. The panel asks the
user for the measurement model as it is taught (reflective or formative) and
for the estimator (PLS or consistent PLS), and the builder maps the pair to
cSEM's operators and `.disattenuate`. Measures that only exist under one
estimator (rho_A under PLSc) or are meant for composites only (IPMA, MICOM)
say so in the report instead of showing a misleading number.

**Quality criteria one by one.** `assess()` refuses models with nonlinear
terms, so every criterion is computed with its own `calculate*()` function,
each inside its own `tryCatch` (Category B). With an interaction, SRMR, d_G
and d_L have no model-implied matrix and are shown as "Not computable" with
that reason; rho_C is computed with `.model_implied = FALSE`; the structural
VIF is computed from construct scores.

**Computation time.** One bootstrap draw of the reference model (n = 344, 8
constructs) takes about 19 ms, linear in the number of draws, and jamovi
re-runs `.run()` on every option change. Every resampling section has a
"Run" checkbox off by default and strict `clearWith`; the bootstrap runs in
blocks (200 draws each, derived seeds) with `private$.checkpoint()` and
progress between blocks — pooled blocks reproduce one long run within Monte
Carlo error (largest CI difference 0.016 for 2000 draws). Results are cached
(small, capped, as PLSsemLab) so a cosmetic change recomputes nothing.

**Known quirks of cSEM 0.7.1, handled explicitly.** With inference,
`calculateHTMT()` packs the estimate (lower triangle) and the upper bound
(upper triangle) in one matrix, unpacked into a long table. VIF matrices
mark non-members with 0. `testOMF()` returns one critical-value column per
alpha level. `predict()` accepts a seed only with one repetition, so the
module sets the seed itself. `testMGD(.output_type = "structured")` fails;
the default output is read instead. IPMA performance comes on the items'
metric and is rescaled to 0–100 with the scale range the user declares.

-------------------------------------------------------------------------------

# Arquitectura específica de cSEM

Cinco condiciones dan forma a cada análisis de PLSsemLab: nombres internos
(cSEM falla con espacios y, según la configuración regional, con acentos),
modelo de medida y estimador como dos elecciones separadas (en cSEM `=~` es
factor común y `<~` compuesto), criterios de calidad calculados uno por uno
(`assess()` rechaza términos no lineales), tiempo de cómputo (bootstrap por
bloques con checkpoint y caché) y peculiaridades conocidas de cSEM 0.7.1
tratadas explícitamente.

-------------------------------------------------------------------------------

# Final Statement

PLSsemLab is designed to make PLS-SEM teachable: every measurement and
structural decision it reports can be traced to its source literature and
explained in plain language.

-------------------------------------------------------------------------------

# Declaración final

PLSsemLab está diseñado para que el PLS-SEM pueda enseñarse: cada decisión
sobre el modelo de medida y el estructural que informa puede rastrearse hasta
su literatura de origen y explicarse en lenguaje claro.

-------------------------------------------------------------------------------


**End of document**
