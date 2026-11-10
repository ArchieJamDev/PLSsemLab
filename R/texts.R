# -----------------------------------------------------------------------------
# PLSsemLab
# A jamovi module for partial least squares structural equation modeling and
# evidence-based methodological decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of PLSsemLab.
#
# PLSsemLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# PLSsemLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PLSsemLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Text repository.
# ES: Repositorio de textos.
#
# Every user-facing report string assembled at runtime lives here, keyed by
# (section, key), written in English inside jamovi's `.()` so the jamovi
# compiler extracts it into the same catalog as the interface text
# (jamovi/i18n/es.po). Report text follows jamovi's own language; there is no
# language option (ARCHITECTURE.md §12). Variable parts are %s / %d
# placeholders filled with sprintf() after translation.
#
# ES: Todo texto del informe que se arma en tiempo de ejecución vive aquí,
# indexado por (sección, clave), escrito en inglés dentro de `.()` de jamovi
# para que el compilador lo extraiga al mismo catálogo que el texto de la
# interfaz (jamovi/i18n/es.po). El texto sigue el idioma de jamovi; no hay
# opción de idioma (ARCHITECTURE.md §12). Las partes variables son
# marcadores %s / %d que se llenan con sprintf() después de traducir.
#
# Writing rule: every interpretation answers four questions (what happened,
# why, what it means, what the researcher should do now). Never write a
# bracketed reference number; append "see references" after an author-year
# mention (CODE_STYLE.md §21).
#
# ES: Regla de redacción: cada interpretación responde cuatro preguntas (qué
# ocurrió, por qué, qué significa, qué hacer ahora). Nunca escribir un
# número de referencia entre corchetes; agregar "see references" después de
# una mención autor-año (CODE_STYLE.md §21).
# -----------------------------------------------------------------------------

.pl_text_table <- function(.) {
    list(

        # Introduction, shown while the model is not complete.
        # ES: Introducción, visible mientras el modelo no está completo.
        intro = list(
            what = .("PLS-SEM estimates relationships between constructs (concepts measured by several indicators) and how well each construct is measured. It builds each construct as a weighted combination of its indicators and is oriented to explaining and predicting the outcomes of the model."),
            step1 = .("Define each construct in Reflective Constructs or Formative Constructs: give it a name and drag its indicators into it. Reflective: the construct causes its indicators, which should correlate highly. Formative: the indicators together form the construct and need not correlate."),
            step2 = .("Open Structural Model: every construct has a row. Drag into a construct's row the constructs that predict it. A construct whose row stays empty is exogenous (only a predictor)."),
            step3 = .("The model is checked before estimation; if something is missing or inconsistent, the message names the construct or indicator at fault."),
            need_constructs = .("Define at least two constructs, each with at least one indicator."),
            need_paths = .("Add the structural model: drag at least one predictor into the row of a construct.")
        ),

        # Category A messages (jmvcore::reject).
        # ES: Mensajes de Categoría A (jmvcore::reject).
        errors = list(
            no_name = .("Every construct needs a name: one of the constructs has indicators but an empty name."),
            duplicated = .("The name '%s' is used for more than one construct. Give each construct a different name."),
            twice = .("'%s' is an indicator of more than one construct. In PLS-SEM each indicator belongs to one construct only; keep it in one of them."),
            unknown = .("'%s' appears in the structural model but is not defined as a construct with indicators."),
            self = .("'%s' cannot predict itself."),
            loop = .("The structural model has a loop (%s). PLS-SEM estimates recursive models only, in which no construct predicts itself through others: remove one path of the loop."),
            isolated = .("'%s' is not connected to any other construct. Drag it into the row of the construct it predicts, or drag its predictors into its row, or remove it."),
            interaction = .("The structural model includes an interaction (%s). Interactions between constructs will be available in a later version of PLSsemLab; remove it for now."),
            not_numeric = .("'%s' is not numeric. Indicators must be numeric variables."),
            constant = .("'%s' has the same value for every case, so it carries no information. Remove it from its construct."),
            sentinel = .("'%s' has %d value(s) of %s, far outside the range of its other values (%s to %s). If this is a missing-data code, declare it as missing in jamovi's data view (Setup > Missing values) and run the analysis again."),
            few_cases = .("Only %d cases have complete data on the indicators, too few to estimate the model. Check the missing values or use mean replacement."),
            estimation = .("cSEM could not estimate the model: %s")
        ),

        # Notices (Category B, never block).
        # ES: Avisos (Categoría B, nunca bloquean).
        notices = list(
            empty_construct = .("'%s' has no indicators yet and was left out of the model."),
            single_reflective = .("'%s' has a single indicator: its loading is fixed at 1 and its measurement quality cannot be evaluated."),
            inadmissible = .("The estimates are not admissible (%s). Results are shown, but should not be interpreted until the cause is fixed: check indicators with very low or negative correlations, or very small samples."),
            dropped = .("%d case(s) with missing values were excluded; %d case(s) remain."),
            replaced = .("%d missing value(s) were replaced by the mean of their indicator (%d case(s) affected).")
        ),

        # Table cells and labels.
        # ES: Celdas y etiquetas de las tablas.
        labels = list(
            reflective = .("Reflective"),
            formative = .("Formative"),
            exogenous = .("Exogenous"),
            endogenous = .("Endogenous"),
            none = .("\u2014"),
            interaction_sep = .(" \u00d7 ")
        ),

        # "How to read" guides.
        # ES: Guías "Cómo leer".
        guide = list(
            model = .("Each row is one construct: how it is measured (reflective or formative), how many indicators it has, and its role in the structural model. Exogenous constructs are only predictors; endogenous constructs are predicted by at least one other construct. Check that this table describes the model you intended before reading any number."),
            measurement = .("Loadings are the correlations between each indicator and its construct; weights are the contribution of each indicator to the construct score. For reflective constructs read the loadings: values of 0.708 or more mean the construct explains at least half of the indicator's variance (Hair et al., 2019, see references). For formative constructs read the weights: they show the relative contribution of each indicator, and a small or negative weight is not by itself a reason to remove an indicator. Common misreading: judging formative indicators by their loadings as if they were reflective."),
            paths = .("Each coefficient is the change in the outcome construct, in standard deviations, when the predictor increases by one standard deviation and the other predictors of the same outcome stay constant. R\u00b2 is the share of the outcome's variance explained by its predictors. Common misreading: treating a coefficient as significant. This version reports estimates only; significance needs bootstrap confidence intervals, which arrive in a later version.")
        ),

        # About your choices.
        # ES: Acerca de sus elecciones.
        choices = list(
            pls = .("Estimator: PLS. Every construct is estimated as a weighted combination of its indicators (a composite): Mode A weights for reflective constructs and Mode B for formative ones. This is the PLS-SEM of the primer of Hair et al. Choose consistent PLS when your reflective constructs are meant as common factors and you want estimates corrected for measurement error."),
            plsc = .("Estimator: consistent PLS (PLSc). Reflective constructs are treated as common factors and their estimates are corrected for measurement error (Dijkstra & Henseler, 2015, see references); formative constructs stay composites. PLSc can produce inadmissible solutions in small samples; the report warns when that happens."),
            scheme = .("Inner weighting scheme: %s. The scheme decides how neighbouring constructs inform each other while the weights are estimated; the path scheme is the usual default and the results rarely depend much on this choice."),
            listwise = .("Missing values: cases with a missing value on any indicator are excluded (%d of %d cases used). This adds no information to the data; if many cases are lost, consider why the values are missing."),
            mean = .("Missing values: each missing value is replaced by the mean of its indicator (%d value(s) replaced). This keeps every case but shrinks the variance of the indicator; use it only when few values are missing.")
        ),

        # Interpretation fragments.
        # ES: Fragmentos de interpretación.
        interp = list(
            title_measurement = .("Measurement model"),
            title_structural = .("Structural model"),
            title_next = .("What to do now"),
            low_loadings = .("%d of %d indicators of reflective constructs have a loading below 0.708 (%s). Indicators with loadings between 0.40 and 0.70 are usually kept unless removing them improves the reliability and validity of their construct, which will be assessed in the next version; indicators below 0.40 are candidates for removal."),
            good_loadings = .("All %d indicators of reflective constructs have a loading of 0.708 or more: each construct explains at least half of the variance of its indicators."),
            no_reflective = .("The model has no reflective constructs, so loadings are not used to judge indicator quality."),
            formative_negative = .("Formative constructs with negative weights: %s. A negative weight often reflects collinearity among the indicators of the construct; collinearity (VIF) and the significance of the weights will be assessed in later versions before any indicator is removed."),
            formative_ok = .("All weights of the formative constructs are positive."),
            strongest = .("The largest path is %s \u2192 %s (%s)."),
            r2_line = .("%s: R\u00b2 = %s (%s)."),
            r2_substantial = .("substantial"),
            r2_moderate = .("moderate"),
            r2_weak = .("weak"),
            r2_very_weak = .("very weak"),
            r2_note = .("As a rough guide, R\u00b2 values of 0.75, 0.50 and 0.25 are described as substantial, moderate and weak (Hair et al., 2019, see references), but what counts as high depends on the field: in consumer behaviour an R\u00b2 of 0.20 can be high."),
            next_steps = .("Read the loadings and weights before the paths: a construct that is poorly measured makes its paths hard to interpret. The next versions of PLSsemLab add reliability, convergent and discriminant validity, collinearity and bootstrap significance; until then, treat these estimates as descriptive.")
        ),

        # Key findings.
        # ES: Hallazgos clave.
        key = list(
            findings = .("Main findings"),
            uncertain = .("Uncertainties"),
            warnings = .("Warnings"),
            none = .("None."),
            model = .("Model: %d construct(s) (%d reflective, %d formative), %d indicator(s), %d structural path(s), %d case(s), estimated with %s."),
            loadings = .("Measurement model: %d of %d reflective indicators load 0.708 or more."),
            r2 = .("Structural model: the highest R\u00b2 is %s for %s."),
            no_inference = .("No significance or confidence intervals yet: this version reports estimates only."),
            admissible_no = .("The estimates are not admissible; do not interpret them until the cause is fixed.")
        ),

        # Reproducibility record.
        # ES: Registro de reproducibilidad.
        repro = list(
            versions = .("PLSsemLab %s, cSEM %s, %s; run on %s."),
            code = .("The R code below reproduces this analysis with cSEM directly, from the data set loaded in jamovi (called data). Constructs and indicators get internal names (C1, C2, ... and V1, V2, ...) because cSEM fails with spaces or accents in names; the comments map them back to the names used in jamovi.")
        )
    )
}

.pl_text <- function(lang, section, key) {
    if (is.null(lang$texts))
        lang$texts <- .pl_text_table(lang$tr)
    value <- lang$texts[[section]][[key]]
    if (is.null(value))
        value <- paste0("[missing text: ", section, ".", key, "]")
    value
}
