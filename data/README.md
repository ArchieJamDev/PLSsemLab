# Bundled datasets

Three public datasets ship with PLSsemLab, so its results can be checked
against published PLS-SEM examples. The first two come from the cSEM R
package (GPL-3) and the third from Mendeley Data (CC BY 4.0);
`data-raw/prepare_dataset.R` rebuilds them.

| File | Cases | Variables | Source |
| --- | --- | --- | --- |
| `plssemlab_corp_rep.csv` | 344 | 41 | Corporate reputation data of Hair, Hult, Ringle and Sarstedt (*A Primer on PLS-SEM*), as bundled in cSEM (from seminr 2.4.0). Service type (prepaid / contract) allows the multigroup analysis. |
| `plssemlab_satisfaction.csv` | 250 | 27 | European Customer Satisfaction Index (ECSI), mobile phone industry, as bundled in cSEM: image, expectation, quality, value, satisfaction and loyalty. |
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

---

# Conjuntos de datos incluidos (ES)

PLSsemLab incluye tres conjuntos de datos públicos, para que sus resultados
puedan contrastarse con ejemplos publicados de PLS-SEM. Los dos primeros
vienen del paquete de R cSEM (GPL-3) y el tercero de Mendeley Data (CC BY
4.0, Damberg, 2023, con constructos formativos y reflectivos y variables
demográficas); `data-raw/prepare_dataset.R` los reconstruye.

**Valores perdidos.** Los datos originales de reputación corporativa
codifican 11 respuestas faltantes como -99. Se escriben como celdas vacías,
que jamovi lee como perdidos. No se recodifica, imputa ni elimina nada más.
