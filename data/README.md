# Bundled datasets

Four datasets ship with PLSsemLab. Three are public data, so its results
can be checked against published PLS-SEM examples: the first two come from
the cSEM R package (GPL-3) and the third from Mendeley Data (CC BY 4.0);
`data-raw/prepare_dataset.R` rebuilds them. The fourth is simulated from a
known population model, so that every option of the module has something
to show; `data-raw/simulate_learning_data.R` rebuilds it. In jamovi they
are under ☰ › Open › Data Library › PLS-SEM Lab.

| File | Cases | Variables | Source |
| --- | --- | --- | --- |
| `plssemlab_corp_rep.csv` | 344 | 41 | Corporate reputation data of Hair, Hult, Ringle and Sarstedt (*A Primer on PLS-SEM*), as bundled in cSEM (from seminr 2.4.0). Service type (prepaid / contract) allows the multigroup analysis. |
| `plssemlab_satisfaction.csv` | 250 | 27 | European Customer Satisfaction Index (ECSI), mobile phone industry, as bundled in cSEM: image, expectation, quality, value, satisfaction and loyalty. |
| `plssemlab_learning.csv` | 600 | 28 | Simulated (CC0 1.0): employees' leadership, autonomy, job resources, work engagement (vigor, dedication, absorption), performance and turnover intention, with sector and region. See below. |
| `plssemlab_bank.csv` | 675 | 41 | Bank Customer Perceptions: customers of German cooperative banks, 2020. Formative QUAL (6 items), PERF (5), CSOR (5), ATTR (4); reflective LIKE (2), COMP (3), SAT (3), LOY (3), TRUST (4); gender, age group, education, employment, income, family status. Damberg, S. (2023), Mendeley Data, V3, https://doi.org/10.17632/4wyg7vdzzm.3, CC BY 4.0. |

**Bank data, changes from the source.** Variable names are in lower case,
`QUA_7` is renamed `qual_7`, and the demographic columns are renamed
(`gender`, `age_group`, `education`, `employment`, `income`, `family`).
Gender is coded 0/1 in the file while the codebook lists 1 = male,
2 = female; the article (Damberg, 2023, *Data in Brief*, 48, 109187)
reports 393 women and 282 men, which matches 1 = female, so it is labelled
Male (0) / Female (1). Age is given in groups only (18-24 to >65).
Education, employment and income keep their numeric codes (see the source
codebook; some cells hold 0, a code the codebook does not list). Item
`csor_5` has 3 missing answers, kept as empty cells. Items QUAL_3, ATTR_5
and LIKE_3 of the questionnaire are not in the published data.

**Missing values.** The original corporate reputation data code 11 missing
answers as -99 (in `cusa` and `cusl_1` to `cusl_3`). They are written as
empty cells, which jamovi reads as missing. Estimated with the -99 codes as
if they were answers, the alpha of loyalty falls from 0.831 to 0.421
(`experiments/design-v1/`, E1); PLSsemLab stops with a message when it
finds such codes in a user's data. Nothing else is recoded, imputed or
removed.

**Learning data (simulated).** 600 employees answer 7-point items. The
population model, written in `data-raw/simulate_learning_data.R`, is:

- Reflective constructs with three or four items (loading .80 each):
  leadership (`lead_1`-`lead_4`), autonomy (`aut_1`-`aut_3`), vigor
  (`vig_1`-`vig_3`), dedication (`ded_1`-`ded_3`), absorption
  (`abs_1`-`abs_3`), performance (`perf_1`-`perf_3`) and turnover intention
  (`int_1`-`int_3`). One formative construct, job resources
  (`res_1`-`res_4`, weights .50, .30, .30, .20).
- Work engagement (ENG) is a higher-order construct with the dimensions
  vigor, dedication and absorption (loadings .85).
- ENG is predicted by leadership, resources and autonomy, and autonomy
  strengthens the effect of leadership (interaction .15).
- ENG mediates the effect of leadership on performance (partial mediation:
  a small direct path) and on turnover intention (negative).
- The path leadership → ENG is .50 in the private sector and .20 in the
  public sector, with the same measurement model in both; region (North,
  Central, South) has no differences.
- The constructs have different mean levels, for the importance-performance
  map; about 1% of the answers to `perf_*`, `int_*` and `ded_2` are missing
  at random.

The seed (8) was chosen among seeds 1-12 so that chance does not hide what
the data are meant to show: with it, MICOM finds full invariance by sector,
both tests find the sector difference in leadership → ENG, and no path
differs between regions in both tests (among the 21 regional comparisons
one test alone gives a borderline result, which the Holm correction
removes: a live example of why multiple comparisons need care). Turnover intention is the only construct where
higher means worse: do not use it as the target of the importance-performance
map without reversing it. Synthetic data, no real persons: CC0 1.0.

## Which dataset for which option

| Option | Dataset and model |
| --- | --- |
| Reflective and formative constructs, VIF of formative indicators | `corp_rep`: COMP, LIKE, CUSA, CUSL reflective; QUAL, PERF, CSOR, ATTR formative (extended model of Hair et al.). `bank`: the same design with demographics. |
| Consistent PLS (PLSc) | `corp_rep` or `satisfaction`, reflective constructs. |
| Missing values (listwise or mean replacement) | `corp_rep` (11 missing answers in `cusa`, `cusl_*`), `learning` (about 1%), `bank` (`csor_5`). |
| Ordinal indicators | `learning` or `bank` (Likert items). |
| Higher-order construct, reflective | `learning`: ENG with VIG, DED and ABS; ENG → PERF and INT, predicted by LEAD, RES and AUT. |
| Higher-order construct, formative | `learning`: a formative SUP (support) formed by LEAD and AUT, predicting ENG. |
| Disjoint or "as cSEM" two-stage approach | Either model above, switching the approach. |
| Moderation (interaction) | `learning`: LEAD × AUT → ENG (ENG as a higher-order construct or measured by its nine items); `satisfaction`: SAT × IMAG → LOY. |
| Mediation | `learning`: LEAD → ENG → PERF; `corp_rep`: COMP and LIKE → CUSA → CUSL. |
| Bootstrap, overall fit test, PLSpredict and CVPAT | Any dataset; `learning` with ENG measured by its nine items (prediction and the fit test are not computed with higher-order constructs). |
| Importance-performance map | `learning` with target PERF (all items on the same 1-7 scale, higher = better). |
| Multigroup analysis | `learning`: sector (two groups, one real difference) and region (three groups, no real differences; try the Holm correction); `corp_rep`: service type; `bank`: gender. Use first-order constructs: multigroup analysis is not computed with higher-order constructs. |

---

# Conjuntos de datos incluidos (ES)

PLSsemLab incluye cuatro conjuntos de datos. Tres son datos públicos, para
que sus resultados puedan contrastarse con ejemplos publicados de PLS-SEM:
los dos primeros vienen del paquete de R cSEM (GPL-3) y el tercero de
Mendeley Data (CC BY 4.0, Damberg, 2023, con constructos formativos y
reflectivos y variables demográficas); `data-raw/prepare_dataset.R` los
reconstruye. El cuarto, `plssemlab_learning.csv`, está simulado a partir de
un modelo poblacional conocido (CC0 1.0), para que cada opción del módulo
tenga algo que mostrar: constructos reflectivos y uno formativo, un
constructo de orden superior (compromiso, con vigor, dedicación y
absorción), una moderación, una mediación parcial, una diferencia real
entre sectores y ninguna entre regiones, niveles medios distintos para el
mapa importancia-desempeño y un 1% de respuestas perdidas;
`data-raw/simulate_learning_data.R` lo reconstruye. La semilla (8) se eligió
entre las semillas 1-12 para que el azar no oculte lo que los datos deben
mostrar. En jamovi están en ☰ › Abrir › Biblioteca de datos › PLS-SEM Lab.
La tabla de arriba indica qué conjunto y qué modelo usar para probar cada
opción.

**Valores perdidos.** Los datos originales de reputación corporativa
codifican 11 respuestas faltantes como -99. Se escriben como celdas vacías,
que jamovi lee como perdidos. No se recodifica, imputa ni elimina nada más.
