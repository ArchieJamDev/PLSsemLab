# PLSsemLab Code Style Guide

Version: 0.1 (2026-11-10: adopted from MokkenLab's CODE_STYLE.md v1.0, itself
adopted from EGAnetLab's v1.0 and AssumptionsLab's CODE_STYLE.md v1.2,
including every lesson from jamovi's official module review of
AssumptionsLab - Category A/B error handling, native plot themes, native
i18n catalog, `refs:` only on `Table` results. Helper prefix is `.pl_`
instead of `.al_`/`.el_`/`.mk_`. Examples that mention AssumptionsLab analyses are kept
as the precedent that motivated each rule.)
ES: Versión 0.1 (2026-11-10: adoptada del CODE_STYLE.md v1.0 de MokkenLab,
a su vez adoptado de EGAnetLab y del v1.2 de AssumptionsLab, con todas las lecciones de la revisión oficial de jamovi -
manejo de errores de Categoría A/B, temas de gráfico nativos, catálogo i18n
nativo, `refs:` solo en resultados `Table`. El prefijo de helpers es `.pl_`
en lugar de `.al_`/`.el_`/`.mk_`. Los ejemplos que mencionan análisis de AssumptionsLab se
conservan como el precedente que motivó cada regla.)
Project: PLSsemLab
License: GNU GPL v3
Author: Arquímedes De León Chacón Chacón

---

# 1. Philosophy

PLSsemLab is more than a statistical software project.

It is an educational, methodological and scientific software project.

The source code must therefore be understandable, maintainable and educational.

Every source file should be readable as a technical document rather than merely
as executable code.

Our objective is not only to build software.

Our objective is to teach methodology through software.

---

# Filosofía

PLSsemLab es mucho más que un proyecto de software estadístico.

Es un proyecto científico, metodológico y educativo.

Por ello, el código fuente debe ser comprensible, mantenible y didáctico.

Cada archivo debe poder leerse como un documento técnico y no únicamente como
código ejecutable.

Nuestro objetivo no es solamente desarrollar software.

Nuestro objetivo es enseñar metodología mediante software.

-------------------------------------------------------------------------------

# 2. General Principles

The following principles govern every source file.

• Readability over cleverness.
• Consistency over personal preference.
• Documentation over assumptions.
• Simplicity over unnecessary complexity.
• Scientific rigor over shortcuts.

-------------------------------------------------------------------------------

# Principios generales

Los siguientes principios gobiernan todos los archivos fuente.

• Legibilidad antes que ingenio.
• Consistencia antes que preferencias personales.
• Documentación antes que suposiciones.
• Simplicidad antes que complejidad innecesaria.
• Rigor científico antes que atajos.

-------------------------------------------------------------------------------

# 3. Source File Structure

Every source file must follow the same structure.

1. GPL copyright
2. File description
3. Responsibilities
4. Workflow
5. Code

-------------------------------------------------------------------------------

Example

GPL Header

↓

File Description

↓

Responsibilities

↓

Workflow

↓

Functions

↓

Implementation

-------------------------------------------------------------------------------

# 4. File Description

Every file must begin with a bilingual description.

Example

# -----------------------------------------------------------------------------
# Independent Groups Analysis
# ES: Análisis de grupos independientes.
#
# This file implements the complete analytical workflow for independent-group
# assumption diagnostics.
#
# ES: Este archivo implementa el flujo completo para el diagnóstico de
# supuestos en grupos independientes.
# -----------------------------------------------------------------------------

-------------------------------------------------------------------------------

# 5. Workflow

Large source files must include a workflow section.

Example

Workflow

1. Validate input
2. Prepare data
3. Compute descriptives
4. Detect outliers
5. Evaluate assumptions
6. Generate graphics
7. Build interpretations
8. Assemble report

Every workflow must also include the Spanish translation.

-------------------------------------------------------------------------------

# 6. Section Headers

All sections must use the following separator.

# -----------------------------------------------------------------------------
# Compute descriptive statistics.
# ES: Calcular estadísticos descriptivos.
# -----------------------------------------------------------------------------

Do not use

########################

========================

******

or similar separators.

-------------------------------------------------------------------------------

# 7. Function Documentation

Every non-trivial function must be documented.

Documentation must explain

• What the function does.
• Why it exists.
• Inputs.
• Outputs.
• Methodological notes when appropriate.

Example

# -----------------------------------------------------------------------------
# Compute Cook's distance.
# ES: Calcular la distancia de Cook.
#
# Identifies potentially influential observations by measuring the impact of
# each case on the fitted regression model.
#
# ES: Identifica observaciones potencialmente influyentes midiendo el impacto
# de cada caso sobre el modelo ajustado.
# -----------------------------------------------------------------------------

-------------------------------------------------------------------------------

# 8. Comment Philosophy

Never describe obvious code.

Bad

x <- mean(values)

# Compute mean.

Good

# The mean is reported because it is required by both the descriptive table
# and the methodological interpretation.

ES:

# La media se informa porque es utilizada tanto por la tabla descriptiva como
# por la interpretación metodológica.

-------------------------------------------------------------------------------

# 9. Bilingual Documentation

English is the primary language.

Spanish is always provided immediately below.

Example

# Generate QQ plot.
# ES: Generar gráfico QQ.

Never mix both languages on the same line.

-------------------------------------------------------------------------------

# 10. Methodological Comments

Whenever an algorithm reflects a methodological decision, explain the reason.

Example

# Welch's correction is preferred because it provides more reliable Type I
# error control when variances are unequal.

# ES:
# La corrección de Welch se prefiere porque proporciona un mejor control del
# error Tipo I cuando las varianzas son desiguales.

-------------------------------------------------------------------------------

# 11. Variable Names

Variable names must be descriptive.

Preferred

groupMeans

pooledVariance

cookDistance

studentizedResiduals

Avoid

a

b

tmp

aux

x1

-------------------------------------------------------------------------------

# 12. Function Names

Functions should describe actions.

Preferred

computeNormality()

buildInterpretation()

createQQPlot()

assembleReport()

Avoid

test()

run()

execute()

process()

-------------------------------------------------------------------------------

# 13. Line Length

Recommended maximum

100 characters.

Long comments should be wrapped.

-------------------------------------------------------------------------------

# 14. Empty Lines

Separate logical blocks with one empty line.

Do not insert multiple consecutive blank lines.

-------------------------------------------------------------------------------

# 15. Indentation

Use four spaces.

Do not mix tabs and spaces.

-------------------------------------------------------------------------------

# 16. Code Blocks

Large functions should be divided into logical blocks.

Example

Validation

↓

Preparation

↓

Statistics

↓

Diagnostics

↓

Interpretation

↓

Output

-------------------------------------------------------------------------------

# 17. Report Generation

Report construction must follow the same logical order as the statistical
workflow.

Never generate report sections in arbitrary order.

-------------------------------------------------------------------------------

# 18. Graphics

Graphics should be grouped according to methodological purpose.

Distribution

Normality

Variance

Influence

Correlation

Residuals

Appearance

Never by plotting library.

Never reimplement jamovi's own Theme/Palette controls. Every render function
receives `ggtheme`/`theme` from jamovi itself — build on `+ ggtheme`, never
a parallel `plotStyle` option. Series colours come from jamovi's own
palette through `jmvcore::colorPalette()` (`.pl_global_palette()`); the
module has no palette option of its own.

A manual `scale_color_manual()`/`scale_fill_manual()` must be added to the
plot *after* `+ ggtheme`, never before — ggtheme carries its own discrete
scale, and whichever scale is added last to the ggplot object wins.

-------------------------------------------------------------------------------

# ES:

Nunca reimplementar los propios controles de Tema/Paleta de jamovi. Cada
función de render recibe `ggtheme`/`theme` desde el propio jamovi —
construir sobre `+ ggtheme`, nunca una opción `plotStyle` paralela. Los
colores de las series vienen de la paleta de jamovi mediante
`jmvcore::colorPalette()` (`.pl_global_palette()`); el módulo no tiene
opción de paleta propia.

Un `scale_color_manual()`/`scale_fill_manual()` manual debe agregarse al
gráfico *después* de `+ ggtheme`, nunca antes — ggtheme trae su propia
escala discreta, y para una misma estética gana la escala agregada al
final del objeto ggplot.

-------------------------------------------------------------------------------

# 19. Error Messages

Error messages should

• explain the problem;

• explain why it occurred;

• indicate how to solve it.

## 19.1 Category A vs. Category B

Not every failure is the same failure. Distinguish the two before writing a
`tryCatch`.

**Category A** — a condition that blocks the entire analysis (no dependent
variable, wrong factor-level count, no predictors, a model that fails to
fit). Call `jmvcore::reject(message)` with the real reason, including
`conditionMessage(e)` where relevant. Never let the analysis render a
completed-looking report with the explanation buried in the intro text.

**Category B** — one diagnostic test fails while the rest of the analysis
succeeds (too few observations, a constant series, a near-singular
sub-model). The row must always render — never silently vanish — tiered
"Not computable," with a footnote naming the *specific* methodological
reason. A generic "could not be computed" is barely better than a vanished
row.

A sharp trap for Category B: many R statistical functions do not throw on
degenerate input — they warn and return a result with a non-finite
statistic or p-value instead. `!is.null(result)` is not enough; also check
`is.finite(result$p.value)` before treating a result as real.

-------------------------------------------------------------------------------

# ES: 19.1 Categoría A vs. Categoría B

No toda falla es la misma falla. Distinguir las dos antes de escribir un
`tryCatch`.

**Categoría A** — una condición que bloquea todo el análisis (falta la
variable dependiente, número incorrecto de niveles del factor, sin
predictores, un modelo que no logra ajustarse). Llamar a
`jmvcore::reject(mensaje)` con la razón real, incluyendo
`conditionMessage(e)` cuando corresponda. Nunca dejar que el análisis
muestre un informe con apariencia completa mientras la explicación queda
enterrada en el texto introductorio.

**Categoría B** — una prueba diagnóstica falla mientras el resto del
análisis se completa (muy pocas observaciones, una serie constante, un
submodelo casi singular). La fila siempre debe renderizarse — nunca
desaparecer en silencio — como "No computable", con una nota al pie que
nombre la razón metodológica *específica*. Un mensaje genérico de "no se
pudo calcular" apenas es mejor que una fila desaparecida.

Una trampa frecuente para la Categoría B: muchas funciones estadísticas de
R no lanzan error ante datos degenerados — advierten y devuelven un
resultado con estadístico o p-valor no finito. `!is.null(resultado)` no
basta; verificar también `is.finite(resultado$p.value)` antes de tratar un
resultado como real.

-------------------------------------------------------------------------------

# 20. YAML Files

YAML comments should explain

• why an option exists;

• what analysis it affects;

• how it is used.

Avoid comments that merely repeat field names.

Every `title`/`description`/`label`/`menuTitle`/`content`/`notes` value in
`.a.yaml`, `.u.yaml`, `.r.yaml`, and `0000.yaml` is automatically extracted
by jamovi's own compiler into its translation catalog — no `.()` markup
needed. Write these fields in English only. A string in any other
language there is not translated on the fly; it becomes the untranslatable
source text jamovi's catalog serves to every language, including English.
Regenerate the catalog (`jmvtools::i18nUpdate("es")`) whenever such text
changes.

An `Html` result's static `content:` field (resolved natively, like
`title`) is the right place for text that never changes once compiled —
`AssumptionsLab's Assumption Library`'s category guides are the precedent.
Text assembled in R (per row, per computed value, per dataset) is written
as an English `.("...")` literal: report text goes in `R/texts.R`
(`.pl_text_table(.)`), short labels may sit in `.b.R`. The compiler
extracts every literal into the same catalog, and the text follows
jamovi's language. Never build the argument of `.()` with `paste()` or
`sprintf()`: translate the template, then fill it
(`sprintf(.("%d items"), n)`).

-------------------------------------------------------------------------------

# ES:

Evitar comentarios que solo repiten nombres de campo.

Todo valor `title`/`description`/`label`/`menuTitle`/`content`/`notes` en
`.a.yaml`, `.u.yaml`, `.r.yaml` y `0000.yaml` es extraído automáticamente
por el propio compilador de jamovi hacia su catálogo de traducción — no se
necesita ninguna marca `.()`. Escribir estos campos solo en inglés. Un
texto en otro idioma ahí no se traduce sobre la marcha; se convierte en el
texto fuente no traducible que el catálogo de jamovi sirve a todo idioma,
incluido el inglés. Regenerar el catálogo
(`jmvtools::i18nUpdate("es")`) cada vez que ese texto cambie.

El campo `content:` estático de un resultado `Html` (resuelto de forma
nativa, igual que `title`) es el lugar correcto para texto que nunca
cambia una vez compilado — las guías por categoría de `Assumption
Library` son el precedente. El texto que se arma en R (por fila, por
valor calculado, por dataset) se escribe como literal `.("...")` en
inglés: el texto del informe va en `R/texts.R` (`.pl_text_table(.)`), y
las etiquetas cortas pueden quedar en `.b.R`. El compilador extrae cada
literal al mismo catálogo, y el texto sigue el idioma de jamovi. Nunca
construir el argumento de `.()` con `paste()` o `sprintf()`: traducir la
plantilla y luego llenarla (`sprintf(.("%d items"), n)`).

-------------------------------------------------------------------------------

# 21. Scientific Integrity

Never modify calculations to obtain expected results.

Never suppress warnings without justification.

Never remove observations automatically.

Always document methodological decisions.

PLSsemLab always cites in APA 7th edition in `docs/Bibliography.md`,
the module's single authoritative reference list. This is fixed, not a
user preference — there is no citation-style or reference-style selector
anywhere in the interface.

Per-analysis citations shown *inside* jamovi (via `00refs.yaml`/`refs:`)
render in jamovi's own numbered, order-of-appearance format — a
constraint of jamovi's own reference renderer, not a PLSsemLab
choice, and the same for every module in the jamovi ecosystem. Never try
to write a specific bracket number (`[7]`) anywhere in source text —
jamovi assigns it at render time and it is not stable across runs. Where
report prose mentions a source by name (e.g. "Levene, 1960"), append
"see references" / "ver referencias", never a number.

`refs:` only ever renders visibly for `Table`-type results — confirmed
by reading jamovi's Electron client source directly: the reference-list
rendering logic lives exclusively inside the Table renderer, and an
`Html`-type result's renderer never touches it, even though the `refs:`
data does reach the client correctly through `jmvcore`. Never add
`refs:` to an `Html`/`Image` result expecting it to display — it will
compile and install without error, but nothing will ever show. Assumption
Library's category sections learned this the hard way: they are all
`Html`, so they carry no `refs:` at all and instead point to
`docs/Bibliography.md` as plain text.

---

# Integridad científica

Nunca modificar los cálculos para obtener resultados esperados.

Nunca suprimir advertencias sin justificación.

Nunca eliminar observaciones automáticamente.

Documentar siempre las decisiones metodológicas.

PLSsemLab siempre cita en APA 7.ª edición en `docs/Bibliography.md`,
la lista de referencias única y oficial del módulo. Esto es fijo, no una
preferencia del usuario — no existe ningún selector de estilo de citación
o de referencia en ninguna parte de la interfaz.

Las citas por análisis mostradas *dentro* de jamovi (vía
`00refs.yaml`/`refs:`) se renderizan en el propio formato numerado por
orden de aparición de jamovi — una restricción del propio renderizador de
referencias de jamovi, no una elección de PLSsemLab, y la misma para
cualquier módulo del ecosistema jamovi. Nunca escribir un número de
corchete específico (`[7]`) en ningún texto fuente — jamovi lo asigna en
tiempo de render y no es estable entre corridas. Donde el texto del
informe mencione una fuente por su nombre (p. ej. "Levene, 1960"),
agregar "see references" / "ver referencias", nunca un número.

`refs:` solo se renderiza visiblemente en resultados tipo `Table` —
confirmado leyendo directamente el código fuente del cliente Electron
de jamovi: la lógica de renderizado de la lista de referencias vive
exclusivamente dentro del renderer de Table, y el renderer de un
resultado tipo `Html`/`Image` nunca la usa, aunque los datos de `refs:`
sí llegan correctamente al cliente a través de `jmvcore`. Nunca agregar
`refs:` a un resultado `Html`/`Image` esperando que se muestre —
compila e instala sin error, pero nunca aparece nada. Las secciones de
categoría de AssumptionsLab's Assumption Library lo aprendieron de la forma difícil: son
todas `Html`, así que no llevan ningún `refs:` y en cambio apuntan a
`docs/Bibliography.md` como texto simple.

-------------------------------------------------------------------------------

# 22. Backward Compatibility

Documentation improvements must never modify

• algorithms;

• outputs;

• numerical results;

• statistical decisions.

Documentation is editorial.

It is never functional.

-------------------------------------------------------------------------------

# 23. Golden Rule

Every comment must answer at least one question that the code alone cannot
answer.

If a comment merely repeats the code,

remove it.

If a comment explains

• why,

• when,

• where,

• methodological implications,

keep it.

-------------------------------------------------------------------------------

# 24. PLSsemLab Identity

The source code must reflect the same philosophy as the software.

Readable.

Scientific.

Educational.

Consistent.

Maintainable.

International.

Accessible to the Spanish-speaking scientific community.

Useful to future developers.

-------------------------------------------------------------------------------

End of document.
