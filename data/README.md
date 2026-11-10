# Bundled datasets

Two public datasets ship with PLSsemLab, so its results can be checked
against published PLS-SEM examples. Both come from the cSEM R package
(GPL-3); `data-raw/prepare_dataset.R` rebuilds them.

| File | Cases | Variables | Source |
| --- | --- | --- | --- |
| `plssemlab_corp_rep.csv` | 344 | 41 | Corporate reputation data of Hair, Hult, Ringle and Sarstedt (*A Primer on PLS-SEM*), as bundled in cSEM (from seminr 2.4.0). Service type (prepaid / contract) allows the multigroup analysis. |
| `plssemlab_satisfaction.csv` | 250 | 27 | European Customer Satisfaction Index (ECSI), mobile phone industry, as bundled in cSEM: image, expectation, quality, value, satisfaction and loyalty. |

**Missing values.** The original corporate reputation data code 11 missing
answers as -99 (in `cusa` and `cusl_1` to `cusl_3`). They are written as
empty cells, which jamovi reads as missing. Estimated with the -99 codes as
if they were answers, the alpha of loyalty falls from 0.831 to 0.421
(`experiments/design-v1/`, E1); PLSsemLab stops with a message when it
finds such codes in a user's data. Nothing else is recoded, imputed or
removed.

---

# Conjuntos de datos incluidos (ES)

PLSsemLab incluye dos conjuntos de datos públicos, para que sus resultados
puedan contrastarse con ejemplos publicados de PLS-SEM. Ambos vienen del
paquete de R cSEM (GPL-3); `data-raw/prepare_dataset.R` los reconstruye.

**Valores perdidos.** Los datos originales de reputación corporativa
codifican 11 respuestas faltantes como -99. Se escriben como celdas vacías,
que jamovi lee como perdidos. No se recodifica, imputa ni elimina nada más.
