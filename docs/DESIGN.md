# PLSsemLab v1 — Documento de diseño

**Versión:** 0.1 (2026-11-10)  
**Autor:** Arquímedes De León Chacón Chacón  
**Estado:** diseño. Cada decisión técnica de este documento se comprobó con un
script de `experiments/design-v1/` (cSEM 0.7.1, R 4.3.3); los resultados
están en `experiments/design-v1/results/`.

---

## 1. Propósito y alcance

PLSsemLab v1 será el primer módulo de PLS-SEM para jamovi y resolverá desde
su primera versión las seis deficiencias documentadas del PLS-SEM de JASP. No
busca superar a JASP en el motor de estimación —usa el mismo, cSEM—, sino en
lo que JASP calcula pero no muestra, en lo que falla y en cómo se interpreta.

- **Dentro de la v1:** constructos reflectivos y formativos, estimadores PLS
  y PLSc, evaluación completa del modelo de medida y del estructural, prueba
  correcta del ajuste global, constructos de segundo orden, interacciones de
  dos vías, predicción, mediación, IPMA y comparación multigrupo con MICOM.
- **Fuera de la v1:** GSCA y otros estimadores de compuestos, segmentación
  latente (FIMIX-PLS, PLS-POS), endogeneidad (cópulas gaussianas) y efectos
  no lineales más allá de interacciones de dos vías.
- **Rasgo distintivo:** enseñar y no solo calcular, con la filosofía de
  AssumptionsLab y las mejoras de EGAnetLab y MokkenLab: bloques "Cómo leer",
  "Acerca de sus elecciones", interpretación que responde qué ocurrió, por
  qué, qué significa y qué hacer, hallazgos clave y registro de
  reproducibilidad. La interpretación es determinista y citada; no depende de
  una clave de API (JASP 0.98 añadió IA opcional que sí la requiere).
- **Público:** académico (docencia e investigación).

## 2. Priorización

Primero van las deficiencias reportadas por usuarios (errores o solicitudes
no cumplidas), luego la detectada por el propio equipo de JASP y al final la
que se infiere de su documentación. Las seis entran en la v1; el orden fija
qué se construye y se valida antes.

| Prioridad | Deficiencia | Tipo | Evidencia en JASP | Estado conocido |
| --- | --- | --- | --- | --- |
| 1 | D2 HTMT, VIF e IC bootstrap | Solicitud no cumplida | Issues #1982, #2567; foro 2023; tutorial de Rogers y Barboza (2024) | Sin confirmar en JASP 0.99; su ayuda no lo menciona |
| 2 | D3 Orden superior e interacciones | Error | Issues #2955, #3007 (con archivos de usuarios) | Solo se añadió manejo de excepciones |
| 3 | D4 Rendimiento del bootstrap | Error | Issues #3642, #4599; foro 2023 | #3642 corregido; #4599 abierto (oct. 2026) |
| 4 | D5 Errores de sintaxis y ayuda escasa | Usabilidad | Foro 2023; issue #2955 | Sin cambios documentados |
| 5 | D1 Prueba de ajuste incorrecta | Error metodológico | Issue #2955, abierto por el equipo de JASP | Abierto |
| 6 | D6 f², Q², mediación, CVPAT, IPMA, MICOM | Ausencia inferida | Ayuda oficial de JASP (oct. 2024) | Por confirmar en JASP 0.99 |

Pendiente: abrir JASP 0.99 con un modelo cualquiera y confirmar HTMT, VIF,
f², la tabla de ajuste y el diagrama de rutas, para saber qué deficiencias
siguen vigentes.

## 3. Motor: cSEM

El motor es cSEM (Rademaker y Schuberth), el mismo de JASP. Cubre las seis
deficiencias con funciones propias y permite validar los números contra JASP
en los mismos datos. Versión probada: 0.7.1 (2026-09-08); licencia GPL-3,
compatible con la suite. Arrastra unas 76 dependencias; JASP ya lo empaqueta,
así que es viable, pero el `.jmo` será grande.

| Deficiencia | Funciones de cSEM | Comprobado en |
| --- | --- | --- |
| D1 Ajuste | `testOMF()` (SRMR, d_G, d_L, d_ML con valores críticos bootstrap) | E5 |
| D2 Fiabilidad | `calculateRhoC()`, `calculateRhoT()`, `calculateAVE()`, rho_A en `$Reliabilities` (solo PLSc) | E1 |
| D2 HTMT | `calculateHTMT(.inference = "bootstrap")`, HTMT2 | E1 |
| D2 VIF | `calculateVIFModeB()` (indicadores formativos), VIF estructural de `assess()` | E1 |
| D2 IC de cualquier medida | `resamplecSEMResults(.user_funs = ...)` + `infer()` | E1 |
| D3 Segundo orden | `csem()` en dos etapas; criterios por etapa | E2 |
| D3 Interacciones | `csem()` con `A.B`; `doNonlinearEffectsAnalysis()` (floodlight, Johnson-Neyman) | E2 |
| D4 Bootstrap | `resamplecSEMResults()` por bloques con semillas derivadas | E3 |
| D5 Modelo | constructor y validador propios; cSEM solo recibe sintaxis válida | E4 |
| D6 f² | `calculatef2()` | E6 |
| D6 Predicción | `predict()` (PLSpredict), `testCVPAT()` | E6 |
| D6 Mediación | efectos indirectos y totales de `summarize()` con IC bootstrap | E6 |
| D6 IPMA | `doIPMA()` | E6 |
| D6 Multigrupo | `testMICOM()`, `testMGD()` (Chin, Henseler) | E6 |

## 4. Las seis deficiencias: solución y evidencia

### D2 — HTMT, VIF y fiabilidad con intervalos (E1)

**Solución.** Tabla de fiabilidad (alfa, rho_A, rho_C, AVE) con IC bootstrap;
tabla larga de HTMT por par (estimación, límite superior, veredicto frente a
.85 y .90); HTMT2; VIF de indicadores formativos y de predictores
estructurales, ambos con IC; Fornell-Larcker como tabla opcional, porque los
revisores aún la piden aunque HTMT la reemplaza.

**Evidencia.** Con el modelo extendido de reputación corporativa: COMP alfa
0,776, rho_A 0,786, rho_C 0,869, AVE 0,688; HTMT LIKE–COMP 0,780 con límite
superior 0,835; VIF estructural máximo 3,49 (QUAL). Bootstrap de 499 réplicas
para HTMT en unos 10 s.

**Decisiones.**
- rho_A solo existe como factor de corrección de PLSc: con PLS simple cSEM lo
  deja en 1. El informe lo calcula siempre con PLSc y lo explica.
- Con inferencia, cSEM mete la estimación del HTMT en el triángulo inferior y
  el límite superior en el superior; el módulo lo despliega en tabla larga.
- HTMT para compuestos se muestra solo si el usuario lo pide, con la
  advertencia de cSEM: el HTMT está ligado al modelo de factor común.
- Con PLS el módulo arma los reflectivos como compuestos (`<~`, Modo A), así
  que llama a cada función con `.only_common_factors = FALSE`; para los
  reflectivos da exactamente lo mismo que la sintaxis de factor común
  (diferencia 0, comprobado en la fase 2).
- Con un constructo de un solo indicador, cSEM calcula el HTMT tomando como 1
  su correlación interna (fiabilidad perfecta), lo que lo vuelve un límite
  inferior; el informe lo dice en una nota. Correlaciones con signos mezclados
  dejan el par como "No computable" con una nota que apunta a ítems invertidos.
- ω de McDonald opcional (AFC de un factor con lavaan, fuera del modelo
  PLS) como referencia psicométrica de ρC, e indicadores ordinales
  (policóricas, PLS ordinal) como opción de estimación. El VIF formativo se
  calcula con las correlaciones del modelo, porque `calculateVIFModeB()` usa
  los códigos crudos aun con ordinales.
- Los nombres de las columnas del bootstrap se toman de la misma llamada a
  `which()` que las posiciones; tomarlos de una tabla ordenada las etiqueta
  mal (error que el propio experimento detectó y corrigió).

### D3 — Orden superior e interacciones (E2)

**Solución.** Cada criterio de calidad se calcula por separado, nunca con
`assess()`, cada uno en su propio `tryCatch` (Categoría B).

**Evidencia.** Segundo orden: todos los criterios funcionan por etapa
(primera y segunda); el objeto completo omite HTMT y Fornell-Larcker.
Interacción: funcionan R², f², alfa, AVE, HTMT, VIF de Modo B y efectos;
rho_C funciona con `.model_implied = FALSE`; el VIF estructural sale de las
puntuaciones de los constructos (1,84 / 2,00 / 1,27); el análisis floodlight
da el efecto simple de SAT sobre LOY en todo el rango de IMAG.

**Decisiones.**
- SRMR, d_G y d_L no tienen matriz implicada con términos no lineales: se
  muestran como "No computable" con esa razón.
- En segundo orden, cSEM añade el sufijo `_temp` a los nombres de la segunda
  etapa; el módulo los traduce de vuelta.
- Gráfico didáctico de pendientes simples y floodlight con la región de
  Johnson-Neyman.

### D4 — Rendimiento y robustez del bootstrap (E3)

**Solución.** Casilla "Run" desactivada por defecto, `clearWith` estricto,
bootstrap por bloques de 200 réplicas con `private$.checkpoint()` y progreso
entre bloques, semilla visible en el panel y en el informe, resultados en
caché (pequeña y con tope, como MokkenLab).

**Evidencia.** Unos 19 ms por réplica para el modelo extendido (n = 344),
lineal: 500 réplicas en 10 s, 2000 en 38 s, igual con PLS o PLSc y con
`drop` o `replace`. Con R = 2000 nada falla (el error #3642 de JASP no se
reproduce con cSEM 0.7.1). Diez bloques de 200 dan los mismos IC percentiles
que una corrida de 2000 (diferencia máxima 0,016). La misma semilla da
réplicas idénticas.

**Decisiones.**
- El informe muestra cuántas réplicas fueron admisibles y qué se hizo con las
  inadmisibles. En E5, la prueba de ajuste del modelo simple con PLSc tuvo
  410 de 499 réplicas admisibles: la desatenuación sí produce inadmisibles en
  algunos modelos, y la opción debe ser real (en JASP no tenía efecto).
- 2000 réplicas por defecto para el informe final, con aviso de tiempo
  estimado a partir de la primera tanda.

### D5 — Construcción del modelo y mensajes (E4)

**Solución.** El usuario nunca escribe sintaxis: arma el modelo con cajas
(sección 5). Un validador revisa las cajas antes de llamar a cSEM y un
constructor genera la sintaxis con nombres internos.

**Evidencia.** El validador detecta los siete errores comunes probados, cada
uno con un mensaje que nombra el constructo o el indicador: constructo vacío,
indicador en dos bloques, nombre duplicado, ciclo, constructo aislado,
constructo desconocido en una ruta y modelo estructural vacío. cSEM solo da
mensajes como "input string 1 is invalid" o "Reordering the structural
equations was not succesful". Además detecta códigos centinela: los -99 del
dataset de referencia salen como cuatro mensajes claros.

**Decisiones.**
- Nombres internos C1..Ck (constructos) y V1..Vp (indicadores). cSEM quita
  las comillas invertidas, así que un nombre de variable con espacio falla
  siempre, y un nombre de constructo con acento falla en una configuración
  regional que no sea UTF-8 (riesgo real en Windows).
- La sintaxis legible, con los nombres del usuario, va al registro de
  reproducibilidad.
- Ayuda completa en los bloques "Cómo leer" de cada tabla y en
  `docs/Bibliography.md`.

### D1 — Prueba de ajuste global (E5)

**Solución.** `testOMF()` (Beran y Srivastava, 1985; Dijkstra y Henseler,
2015): SRMR, d_G, d_L y d_ML con su valor crítico bootstrap al nivel elegido,
modelo estimado y saturado. El chi-cuadrado ML no se muestra, y el informe
explica por qué.

**Evidencia.** 3–7 s con 499 réplicas. Modelo simple (PLSc): SRMR 0,056
frente a un valor crítico de 0,040, se rechaza el ajuste exacto aunque el
SRMR está bajo la regla descriptiva de 0,08. Una prueba chi-cuadrado ML, el
tipo de prueba que muestra JASP, da para el mismo modelo 191,6 con 29 gl.

**Decisión.** El informe separa la prueba de ajuste exacto de los índices
descriptivos y enseña la diferencia: rechazar el ajuste exacto con un SRMR
bajo es un resultado frecuente y debe leerse junto al tamaño de la muestra.

### D6 — Medidas ausentes (E6)

**Solución y evidencia.**
- f² por ruta (CUSA → CUSL 0,412; COMP → CUSL 0,000).
- PLSpredict: Q²_predict, RMSE y MAE por indicador frente al modelo lineal,
  10 particiones y 10 repeticiones en 2,4 s.
- CVPAT: el modelo establecido predice mejor que uno alternativo sin las
  rutas COMP → CUSL y LIKE → CUSL (estadístico 2,88; p = 0,004).
- Mediación: efectos indirectos con IC percentil (CUSL ~ LIKE: 0,220
  [0,152; 0,286]).
- IPMA: importancia (efectos totales no estandarizados) y desempeño
  reescalado a 0–100.
- Multigrupo: MICOM (pasos 1 a 3) y diferencias por ruta con las pruebas de
  Chin y de Henseler, por tipo de servicio (125 / 219).

**Decisiones.**
- cSEM solo acepta semilla en `predict()` con una repetición; el módulo fija
  la semilla antes de la llamada.
- El desempeño del IPMA viene en la métrica de los ítems (1–7); el módulo lo
  reescala con el rango de la escala que declare el usuario (como la opción
  de rango de MokkenLab).
- `testMGD(.output_type = "structured")` falla en cSEM 0.7.1; se lee la
  salida completa.
- La p de Henseler es unilateral: valores cerca de 0 o de 1 indican
  diferencia. El informe lo explica para no inducir a error.
- CVPAT necesita un modelo alternativo. Propuesta: el usuario marca las rutas
  que quiere quitar y el módulo compara ambos modelos (decisión abierta).

### Hallazgo transversal: qué significa "reflectivo"

En cSEM, `=~` es un **factor común** y `<~` un **compuesto**; IPMA y MICOM
advierten que sus resultados solo son confiables con compuestos. La
literatura de PLS-SEM (Hair et al.) llama "reflectivos" a los constructos de
Modo A y los estima como compuestos. Por eso el panel separa dos elecciones:

1. **Modelo de medida**, como se enseña: reflectivo o formativo.
2. **Estimador:** PLS (compuestos, Modo A o B) o PLS consistente (factores
   comunes en los reflectivos).

El constructor traduce cada combinación a los operadores de cSEM y a
`.disattenuate`, y el informe explica la consecuencia de cada elección. Con
PLS simple, `=~` con `.disattenuate = FALSE` y `<~` con Modo A dan
exactamente las mismas rutas, pesos y cargas (diferencia 0, comprobado en
E6), pero los avisos de IPMA y MICOM dependen del operador: con `<~` y Modo A
desaparece el aviso de "factor común". Por eso el constructor usa `<~` con
Modo A cuando el estimador es PLS y `=~` cuando es PLSc.

### Trampa de datos

El dataset de referencia de Hair et al. codifica los perdidos como -99. Si no
se detecta, el alfa de CUSL cae de 0,831 a 0,421 y el HTMT advierte signos
mezclados. El módulo revisa cada indicador antes de estimar y pide declarar
esos valores como perdidos en la vista de datos de jamovi; el dataset que se
distribuya con el módulo los traerá ya declarados.

## 5. Interfaz

Sigue el patrón de EGAnetLab (*Estructura teórica*) y del CFA de jamovi:
cajas con nombre y arrastrar indicadores. Para las rutas estructurales, un
Supplier con los constructos ya definidos, llenado desde `.events.js`, como
hace SEMLj (`semljgui.u.yaml` + `gui.events.js`, GPL).

**Cajas de variables (arriba del panel):**

| Caja | Contenido |
| --- | --- |
| Reflective Constructs | ListBox: nombre del constructo + indicadores arrastrados |
| Formative Constructs | ListBox: nombre + indicadores |
| Grouping Variable | Una variable, para el análisis multigrupo |

**Secciones plegables (CollapseBox, Title Case):**

| Sección | Opciones principales |
| --- | --- |
| Structural Model | Supplier de constructos → para cada constructo dependiente, sus predictores |
| Higher-Order Constructs | Nombre + constructos de primer orden (desde el Supplier); dos etapas |
| Interactions | Par de constructos (desde el Supplier) y constructo dependiente |
| Estimation | Estimador (PLS / PLS consistente), esquema interno (path, centroide, factorial), datos perdidos, rango de la escala |
| Bootstrap | Run, réplicas (2000), semilla, tipo de IC, nivel |
| Overall Model Fit | Run, réplicas, modelo estimado o saturado |
| Prediction | Run PLSpredict, particiones, repeticiones; CVPAT frente a un modelo sin las rutas marcadas |
| Mediation and Moderation | Efectos indirectos específicos; pendientes simples y floodlight |
| Importance-Performance Map | Constructo objetivo |
| Multigroup | Run MICOM, prueba de diferencias (Henseler, Chin), réplicas |
| Plots | Diagrama de rutas, cargas, HTMT, IPMA, pendientes simples |
| Report Content | Hallazgos clave, Acerca de sus elecciones, Cómo leer cada tabla (desactivado por defecto), registro de reproducibilidad |

Reglas de la revisión de jamovi que aplican: Title Case en las cajas,
opciones en mayúscula inicial, casillas sin "Show"; las tablas conservan su
forma desde `.init()`; una sección opcional que falla se queda en su lugar y
explica por qué con un aviso nativo.

## 6. Arquitectura del módulo

- **Un análisis principal**, `plssem` (PLS-SEM), con todas las secciones de
  la v1. El multigrupo se activa con la variable de agrupación, sin un
  análisis aparte.
- **Archivos:** `jamovi/plssem.a.yaml`, `.u.yaml`, `.r.yaml`;
  `jamovi/js/plssem.events.js`; `R/plssem.b.R` dividido en pasos privados;
  `R/shared-helpers.R` (`.pl_build_model()`, `.pl_prepare_indicators()`,
  `.pl_capture()`, `.pl_bootstrap_blocks()`, `.pl_global_palette()`,
  `.pl_repro_block()`); `R/texts.R` con todo el texto del informe en `.()`;
  `R/path-diagram.R` con el diagrama propio en ggplot.
- **Idioma:** un solo mecanismo, el catálogo i18n de jamovi
  (`jamovi/i18n/es.po`). Sin opción de idioma por análisis.
- **Gráficos:** sobre `+ ggtheme`, colores de `jmvcore::colorPalette()`, datos
  en `image$state`. El diagrama de rutas lo dibuja el módulo porque el de
  cSEM usa DiagrammeR, que jamovi no muestra.
- **Errores:** Categoría A (`jmvcore::reject()` con la razón real) y
  Categoría B (la fila se queda, "No computable" y nota específica); los
  avisos de cSEM pasan a avisos nativos de jamovi.
- Detalle completo en `ARCHITECTURE.md` §16.

## 7. Plan de validación

1. `validation/` comprueba que cada número del módulo es idéntico a la
   llamada directa a cSEM, en PLS y PLSc, y corre en CI en cada push.
2. Comparación con las tablas publicadas por Hair et al. para el modelo de
   reputación corporativa.
3. Comparación con JASP 0.99 en los mismos datos y opciones; cada diferencia
   se explica.
4. Pruebas de estrés (`tests/`): perdidos, muestras pequeñas, nombres con
   espacios y acentos, un solo indicador, modelos casi singulares, idioma
   español-inglés-español en un mismo proceso.

## 8. Plan de desarrollo por fases

Cada fase termina con algo que compila en jamovi, con sus pruebas y su
validación contra cSEM, y con la enseñanza de lo que añade (guías "Cómo
leer", interpretación, hallazgos clave): la parte didáctica se construye con
cada tabla, no al final. El orden sigue la prioridad de la sección 2, salvo
la fase 1, que es la base de todas las demás.

| Fase | Contenido | Deficiencia | Criterio de cierre |
| --- | --- | --- | --- |
| 0 | Diseño, experimentos con cSEM, documentos de la suite, logo | — | Hecho |
| 1 | Plataforma: paquete, `0000.yaml`, catálogo i18n, CI, datasets incluidos (perdidos -99 ya declarados); interfaz de cajas con el Supplier en JS; constructor y validador del modelo; estimación con cargas, pesos y rutas | D5 | El modelo se arma con cajas, los 7 errores del E4 dan su mensaje y los números coinciden con cSEM |
| 2 | Modelo de medida: estimador PLS/PLSc, fiabilidad (alfa, rho_A, rho_C), AVE, HTMT y HTMT2, VIF de indicadores formativos, Fornell-Larcker | D2 | Valores de COMP del E1 reproducidos; tablas con forma fija desde `.init()` |
| 3 | Modelo estructural y bootstrap: R², f², VIF estructural, efectos directos, indirectos y totales; bootstrap por bloques con checkpoint, caché y semilla; IC de todas las medidas; diagrama de rutas, gráfico de cargas y cuadrícula HTMT en ggplot | D2, D4 | 2000 réplicas sin fallo, cancelables; IC iguales a los de cSEM |
| 4 | Constructos de segundo orden e interacciones de dos vías, con pendientes simples, floodlight y Johnson-Neyman | D3 | Los dos modelos del E2 corren completos; lo no calculable sale como "No computable" con su razón |
| 5 | Ajuste global: `testOMF()` con SRMR, d_G, d_L y d_ML, modelo estimado y saturado | D1 | Resultados del E5 reproducidos; explicación de por qué no hay chi-cuadrado ML |
| 6 | Predicción (PLSpredict, CVPAT), mediación e IPMA | D6 | Resultados del E6 reproducidos; rango de la escala para IPMA |
| 7 | Multigrupo: MICOM y diferencias por ruta (Henseler, Chin) | D6 | Comparación por tipo de servicio del E6 reproducida |
| 8 | Validación completa: contra cSEM en CI, contra las tablas de Hair et al. y contra JASP; pruebas de estrés; catálogo en español completo | — | `validation/` en CI sin fallos; cada diferencia con JASP explicada |
| 9 | Primera versión pública 0.1.0 con DOI de Zenodo | — | `.jmo` instalado en Windows, macOS y Linux |
| 10 | Versión 1.0.0 y envío a la biblioteca de jamovi | — | Revisión de jamovi atendida |

## 9. Riesgos y decisiones abiertas

| Tema | Riesgo o pregunta | Propuesta |
| --- | --- | --- |
| Supplier en JS | Es la pieza más compleja de la interfaz | Partir del código de SEMLj (GPL) y probar primero un prototipo mínimo |
| Tamaño y compilación | cSEM trae unas 76 dependencias; TruncatedNormal, spacefillr y qrng compilan C++ | Construir el `.jmo` en Windows, macOS y Linux desde la primera versión de prueba |
| Versión de R | Los experimentos usaron R 4.3.3; jamovi usa su propia versión | Repetir los experimentos en GitHub Actions con R actual y luego en jamovi |
| Diagrama de rutas | Distribución automática legible con muchos constructos | Disposición por capas (exógenos a la izquierda), probada con los modelos de referencia |
| CVPAT | Requiere un modelo alternativo | Rutas marcadas para quitar; confirmar con Archie |
| IPMA | Requiere el rango de la escala | Opción de rango (mínimo y máximo) con detección automática como valor por defecto |
| Estado de JASP | Lo de D2 y D6 puede haber cambiado en 0.99 | Revisarlo en JASP antes de cerrar la lista de requisitos |
| Volumen de la v1 | Cubrir D1–D6 a la vez es mucho | Construir en el orden de prioridad de la sección 2 y validar cada parte antes de la siguiente |

## 10. Fuentes

Las referencias metodológicas están en `docs/Bibliography.md`. Reportes de
JASP usados en este documento:

- [JASP Release Notes](https://jasp-stats.org/release-notes/)
- [Ayuda de PLS-SEM de JASP (jaspSem)](https://rdrr.io/github/jasp-stats/jaspSem/f/inst/help/PLSSEM.md)
- [Issue #2955: ajuste, AVE/HTMT, segundo orden, interacciones](https://github.com/jasp-stats/jasp-issues/issues/2955)
- [Issue #1982: VIF, HTMT, AVE](https://github.com/jasp-stats/jasp-issues/issues/1982)
- [Issue #2567: mejoras de PLS-SEM](https://github.com/jasp-stats/jasp-issues/issues/2567)
- [Issue #3642: fallo del bootstrap con 2000 o más réplicas](https://github.com/jasp-stats/jasp-issues/issues/3642)
- [Issue #4599: PLS-SEM extremadamente lento](https://github.com/jasp-stats/jasp-issues/issues/4599)
- [Rogers y Barboza: tutorial de PLS-SEM en JASP](https://www.researchgate.net/publication/377656241_Unlocking_VB-SEM_Practical_PLS-SEM_Tutorial_Using_JASP_for_Variance-Based_Structural_Equation_Modeling)
- [Foro de JASP: sintaxis de PLS-SEM](https://forum.cogsci.nl/discussion/8657/pls-sem-model-sintax)
- [SEMLj (interfaz de modelos en jamovi)](https://github.com/semlj/semlj)
