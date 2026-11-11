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
            ordinal_mean = .("Mean replacement cannot be used with ordinal indicators: the mean of an ordinal indicator is usually not one of its categories. Choose Exclude cases listwise in Missing values, or treat the indicators as continuous."),
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
            replaced = .("%d missing value(s) were replaced by the mean of their indicator (%d case(s) affected)."),
            few_categories = .("Indicators with 5 categories or fewer are treated as continuous: %s. With so few categories Pearson correlations underestimate the relationships; consider Ordinal (polychoric correlations) in Estimation."),
            many_categories = .("Indicators with more than 10 categories are treated as ordinal: %s. Polychoric correlations are meant for a few ordered categories; with many categories, treating the indicators as continuous is usually adequate and more stable."),
            no_reflective_quality = .("The model has no reflective constructs, so reliability, convergent validity (AVE) and discriminant validity (HTMT) do not apply. Formative constructs are judged by the collinearity and the weights of their indicators."),
            one_reflective_htmt = .("Discriminant validity (HTMT) compares pairs of reflective constructs; the model has only one.")
        ),

        # Footnotes of cells that cannot be computed (Category B).
        # ES: Notas al pie de celdas que no se pueden calcular (Categoría B).
        footnotes = list(
            single_reliability = .("A single indicator has no internal consistency to estimate: its reliability and AVE are 1 by definition."),
            single_vif = .("A construct with a single indicator has no collinearity among its indicators."),
            ordinal_note = .("Computed from polychoric correlations (ordinal indicators): the values refer to the reliability of the latent continuous responses assumed behind the categories, not to the observed sum score, which is usually lower (Chalmers, 2018, see references)."),
            omega_two = .("A one-factor model with two indicators is not identified, so \u03c9 cannot be estimated; with two indicators, alpha (Spearman-Brown) is the usual reliability estimate."),
            omega_nonconverged = .("The one-factor CFA did not converge, so \u03c9 cannot be estimated."),
            omega_heywood = .("The one-factor CFA gave a standardized loading of 1 or more (a Heywood case), so \u03c9 is not reported."),
            htmt_single = .("%s has a single indicator: cSEM takes its within-construct correlation as 1 (perfect reliability), so this HTMT is a lower bound of the overlap between the two constructs."),
            htmt_signs = .("Not computable: the indicators of these constructs correlate with mixed signs. Check for reverse-coded items and recode them before the analysis."),
            failed = .("Not computable: %s"),
            rho_a_pls = .("\u03c1A comes from a consistent PLS estimation of the same model (same weights): it is the reliability that consistent PLS uses to correct for measurement error (Dijkstra & Henseler, 2015, see references)."),
            alpha_negative = .("A negative alpha means that some indicators of the construct correlate negatively: check for reverse-coded items."),
            fl_note = .("Diagonal: square root of the AVE. Below the diagonal: correlations between constructs. Discriminant validity holds for a pair when both square roots exceed their correlation."),
            fl_fails = .("The correlation with %s exceeds the square root of the AVE of one of the two constructs.")
        ),

        # Table cells and labels.
        # ES: Celdas y etiquetas de las tablas.
        labels = list(
            reflective = .("Reflective"),
            formative = .("Formative"),
            exogenous = .("Exogenous"),
            endogenous = .("Endogenous"),
            none = .("\u2014"),
            interaction_sep = .(" \u00d7 "),
            not_computable = .("Not computable"),
            rel_too_low = .("reliability too low"),
            rel_exploratory = .("reliability acceptable only in exploratory research"),
            rel_ok = .("reliability satisfactory"),
            rel_too_high = .("reliability too high: redundant indicators?"),
            ave_ok = .("AVE adequate"),
            ave_low = .("AVE below 0.50"),
            htmt_ok = .("Below 0.85: distinct constructs"),
            htmt_similar = .("0.85 to 0.90: acceptable only for conceptually similar constructs"),
            htmt_doubt = .("0.90 or more: discriminant validity in doubt"),
            vif_ok = .("Below 3: no collinearity problem"),
            vif_possible = .("3 to 5: possible collinearity"),
            vif_critical = .("5 or more: critical collinearity")
        ),

        # "How to read" guides.
        # ES: Guías "Cómo leer".
        guide = list(
            model = .("Each row is one construct: how it is measured (reflective or formative), how many indicators it has, and its role in the structural model. Exogenous constructs are only predictors; endogenous constructs are predicted by at least one other construct. Check that this table describes the model you intended before reading any number."),
            measurement = .("Loadings are the correlations between each indicator and its construct; weights are the contribution of each indicator to the construct score. For reflective constructs read the loadings: values of 0.708 or more mean the construct explains at least half of the indicator's variance (Hair et al., 2019, see references). For formative constructs read the weights: they show the relative contribution of each indicator, and a small or negative weight is not by itself a reason to remove an indicator. Common misreading: judging formative indicators by their loadings as if they were reflective."),
            reliability = .("One row per reflective construct. Cronbach's alpha is a lower bound of reliability and composite reliability (\u03c1C) an upper bound; \u03c1A usually lies between them and is the most exact of the three (Hair et al., 2019, see references). Values between 0.70 and 0.95 are satisfactory; 0.60 to 0.70 is acceptable only in exploratory research; above 0.95 the indicators are probably redundant (the same question asked several times). The average variance extracted (AVE) is the share of indicator variance the construct explains on average: 0.50 or more means convergent validity. Common misreading: reading only alpha, which tends to underestimate reliability when loadings differ."),
            reliability_omega = .("\u03c1C is McDonald's \u03c9 computed with the loadings of the PLS model. With PLS these are composite loadings, which are inflated, so \u03c1C is an upper bound of reliability; with consistent PLS they are common-factor loadings and \u03c1C is close to the \u03c9 of a factor analysis. The optional \u03c9 (CFA) column estimates a one-factor confirmatory factor analysis of each construct, outside the PLS model (McDonald, 1999, see references): it is the reference used in psychometrics. Common misreading: reporting \u03c1C from plain PLS as if it were the \u03c9 of a factor analysis."),
            htmt = .("One row per pair of reflective constructs. The heterotrait-monotrait ratio (HTMT) compares the correlations between the indicators of two different constructs with the correlations within each construct (Henseler et al., 2015, see references). Values near 1 mean the two constructs are not distinguished by the data. Below 0.85 the constructs are distinct; up to 0.90 is acceptable when they are conceptually similar (for example, two kinds of satisfaction). HTMT2 uses geometric instead of arithmetic means and is more accurate when loadings differ (Roemer et al., 2021, see references). Common misreading: treating HTMT as a correlation between the constructs; it is a ratio of indicator correlations. A formal test needs the bootstrap confidence interval, which arrives in the next version."),
            vif = .("One row per indicator of a formative construct. The variance inflation factor (VIF) measures how much an indicator overlaps with the other indicators of the same construct. Because formative indicators compete to explain the construct, high overlap makes their weights unstable and can turn them negative. VIF of 5 or more is critical; 3 to 5 may be a problem (Hair et al., 2019, see references). Common misreading: removing an indicator only because its VIF is high; a formative indicator carries part of the content of the construct, so first consider merging indicators or building a higher-order construct."),
            paths = .("Each coefficient is the change in the outcome construct, in standard deviations, when the predictor increases by one standard deviation and the other predictors of the same outcome stay constant. R\u00b2 is the share of the outcome's variance explained by its predictors. Common misreading: treating a coefficient as significant. This version reports estimates only; significance needs bootstrap confidence intervals, which arrive in a later version.")
        ),

        # About your choices.
        # ES: Acerca de sus elecciones.
        choices = list(
            pls = .("Estimator: PLS. Every construct is estimated as a weighted combination of its indicators (a composite): Mode A weights for reflective constructs and Mode B for formative ones. This is the PLS-SEM of the primer of Hair et al. Choose consistent PLS when your reflective constructs are meant as common factors and you want estimates corrected for measurement error."),
            plsc = .("Estimator: consistent PLS (PLSc). Reflective constructs are treated as common factors and their estimates are corrected for measurement error (Dijkstra & Henseler, 2015, see references); formative constructs stay composites. PLSc can produce inadmissible solutions in small samples; the report warns when that happens."),
            continuous = .("Indicators: continuous. Correlations between indicators are Pearson correlations, as in classic PLS-SEM. With rating scales of 7 or more categories this is usually adequate; with 5 categories or fewer, or very skewed answers, Pearson correlations underestimate the relationships and ordinal treatment is worth considering."),
            ordinal = .("Indicators: ordinal. Correlations between indicators are polychoric: they assume that each category comes from cutting a continuous latent response, and estimate the correlation between those latent responses (ordinal PLS; Schuberth et al., 2018, see references). This changes the whole model, not only the reliability. Reliability and validity then describe the latent responses; Chalmers (2018, see references) argues that this overstates the reliability of the scores people actually obtain, so report which correlations you used."),
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
            low_loadings = .("%d of %d indicators of reflective constructs have a loading below 0.708 (%s). Indicators with loadings between 0.40 and 0.70 are usually kept unless removing them raises the composite reliability or the AVE of their construct above its threshold; indicators below 0.40 are candidates for removal."),
            good_loadings = .("All %d indicators of reflective constructs have a loading of 0.708 or more: each construct explains at least half of the variance of its indicators."),
            no_reflective = .("The model has no reflective constructs, so loadings are not used to judge indicator quality."),
            formative_negative = .("Formative constructs with negative weights: %s. A negative weight can come from collinearity among the indicators (see their VIF) or from an indicator that adds little beyond the others of its construct. Wait for the significance of the weights (next version) before removing any indicator: a formative indicator carries part of the content of the construct."),
            formative_ok = .("All weights of the formative constructs are positive."),
            reliability_ok = .("Reliability: composite reliability is between 0.70 and 0.95 in all %d reflective construct(s) with two or more indicators."),
            reliability_low = .("Reliability: composite reliability is below 0.70 in %s. These indicators share too little variance to measure the construct consistently; look at the lowest loadings of each construct and at reverse-coded items."),
            reliability_high = .("Reliability: composite reliability is above 0.95 in %s. Such high values usually mean the indicators are near copies of each other, which inflates reliability without adding content."),
            ave_ok = .("Convergent validity: the AVE is 0.50 or more in all %d reflective construct(s) with two or more indicators."),
            ave_low = .("Convergent validity: the AVE is below 0.50 in %s: each construct explains less than half of the variance of its indicators on average. Removing the indicators with the lowest loadings usually raises the AVE, but check that the construct keeps its content."),
            omega_gap_pls = .("On average \u03c1C exceeds the \u03c9 of the one-factor CFA by %s: with PLS, composite loadings are inflated and \u03c1C overstates reliability. Report \u03c9 or \u03c1A alongside \u03c1C, or use consistent PLS."),
            omega_gap_plsc = .("On average \u03c1C differs from the \u03c9 of the one-factor CFA by %s: with consistent PLS both rest on common-factor loadings, so they should be close."),
            htmt_ok = .("Discriminant validity: every HTMT is below 0.85, so the data distinguish the %d reflective constructs from each other."),
            htmt_high = .("Discriminant validity: HTMT is 0.85 or more for %s. The data barely distinguish these constructs; ask whether they measure the same concept, whether some indicators belong to both, or whether they should be merged."),
            htmt_na = .("Discriminant validity: HTMT could not be computed for %s (see the footnotes of the HTMT table)."),
            vif_ok = .("Collinearity of formative indicators: every VIF is below 3."),
            vif_high = .("Collinearity of formative indicators: VIF of 3 or more for %s. Their weights may be unstable; interpret them with care."),
            strongest = .("The largest path is %s \u2192 %s (%s)."),
            r2_line = .("%s: R\u00b2 = %s (%s)."),
            r2_substantial = .("substantial"),
            r2_moderate = .("moderate"),
            r2_weak = .("weak"),
            r2_very_weak = .("very weak"),
            r2_note = .("As a rough guide, R\u00b2 values of 0.75, 0.50 and 0.25 are described as substantial, moderate and weak (Hair et al., 2019, see references), but what counts as high depends on the field: in consumer behaviour an R\u00b2 of 0.20 can be high."),
            next_steps = .("Settle the measurement model before reading the paths: a construct that is poorly measured, or not distinct from another, makes its paths hard to interpret. Any indicator you remove changes every number, so remove one at a time and run the analysis again. The next version of PLSsemLab adds bootstrap confidence intervals, which turn the HTMT thresholds into a test and give the significance of paths and weights; until then, treat these estimates as descriptive.")
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
            reliability = .("Reliability and convergent validity: %d of %d reflective construct(s) have composite reliability between 0.70 and 0.95, and %d of %d have AVE of 0.50 or more."),
            htmt = .("Discriminant validity: %d of %d pair(s) of reflective constructs have HTMT below 0.85."),
            vif = .("Formative indicators: the largest VIF is %s (%s)."),
            no_inference = .("No significance or confidence intervals yet: this version reports estimates only, so the HTMT thresholds are compared with the estimate and not with its upper bound."),
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
