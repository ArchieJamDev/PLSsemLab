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
            hoc_no_name = .("A higher-order construct has no name. Give each higher-order construct a name."),
            hoc_clash = .("'%s' is the name of a first-order construct and of a higher-order construct. Use a different name for each."),
            hoc_two = .("The higher-order construct '%s' needs at least two lower-order constructs. Drag two or more first-order constructs into it."),
            hoc_unknown = .("'%s' in the higher-order construct '%s' is not a first-order construct with indicators."),
            hoc_twice = .("'%s' is a dimension of two higher-order constructs. Each lower-order construct can belong to one higher-order construct only."),
            hoc_plsc = .("Higher-order constructs are estimated with PLS in two stages; consistent PLS with higher-order constructs is not available in this version. Choose PLS as the estimator."),
            hoc_path_loc = .("'%s' is a dimension of the higher-order construct '%s', so it cannot have structural paths of its own. Draw the paths to or from '%s' instead."),
            hoc_interaction = .("The higher-order construct '%s' is part of an interaction. Interactions with higher-order constructs are not available in this version."),
            interaction_order = .("The interaction %s has more than two constructs. PLSsemLab estimates two-way interactions; build it from two constructs."),
            interaction_self = .("The interaction %s predicts %s, one of its own components. An interaction must predict a different construct."),
            interaction_main = .("The interaction %s predicts %s, but %s is not also a predictor of %s. Add the main effect of each component to the same construct: without it the interaction term mixes the moderation with the main effect and cannot be read as moderation."),
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
            hoc_csem = .("Higher-order constructs (%s): cSEM's second-order routine, as JASP uses it. It differs from the disjoint two-stage approach in two ways: in stage 1 the lower-order constructs also get structural paths among themselves, and in stage 2 a reflective higher-order construct is a common factor corrected by consistent PLS (cSEM sets .PLS_modes = NULL and .disattenuate = TRUE there), even when PLS was chosen. Choose the disjoint approach to follow Sarstedt et al. (2019) and Hair et al. (2021)."),
            hoc_two_stage = .("Higher-order constructs (%s): disjoint two-stage approach (Sarstedt et al., 2019). Stage 1 estimates every first-order construct, with each higher-order construct replaced in the structural model by its lower-order constructs; stage 2 uses the stage-1 scores as indicators. The measurement tables of first-order constructs come from stage 1; the structural model and the higher-order constructs from stage 2. With the bootstrap, both stages are resampled together."),
            rhoc_loadings = .("With an interaction in the model, cSEM cannot build the model-implied matrix used for \u03c1C, so \u03c1C is computed from the loadings, (\u03a3\u03bb)\u00b2 / ((\u03a3\u03bb)\u00b2 + \u03a3(1 \u2212 \u03bb\u00b2)); in models without interactions this gives the same value as cSEM."),
            empty_construct = .("'%s' has no indicators yet and was left out of the model."),
            single_reflective = .("'%s' has a single indicator: its loading is fixed at 1 and its measurement quality cannot be evaluated."),
            inadmissible = .("The estimates are not admissible (%s). Results are shown, but should not be interpreted until the cause is fixed: check indicators with very low or negative correlations, or very small samples."),
            dropped = .("%d case(s) with missing values were excluded; %d case(s) remain."),
            replaced = .("%d missing value(s) were replaced by the mean of their indicator (%d case(s) affected)."),
            negative_loadings = .("Reflective indicators with a negative loading: %s. They are almost certainly items worded in the opposite direction that were not recoded. Recode them in jamovi (for a scale from 1 to k, the new value is k + 1 minus the old one) and run the analysis again: until then, the reliability, AVE and HTMT of their constructs are distorted."),
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

        # Bootstrap status and table notes.
        # ES: Estado del bootstrap y notas de las tablas.
        boot = list(
            running = .("Bootstrap running: %d of %d resamples done. Changing any option stops it."),
            done = .("Bootstrap: %d resamples drawn, %d admissible and used; seed %d; drawn in blocks of %d (block k uses the seed + k - 1); %d s. Intervals are percentile intervals at %s%%."),
            not_done = .("The bootstrap could not be completed; see the notice at the top of the results."),
            failed = .("The bootstrap could not be computed, so the tables show estimates without intervals: %s"),
            some_failed = .("Some bootstrap blocks failed and were left out: %s"),
            ci_note = .("Intervals: %s%% percentile bootstrap intervals from %d admissible resamples."),
            p_ci_disagree = .("The p value and the percentile interval disagree for this path (a borderline case). The decision follows the interval, as recommended for PLS-SEM (Hair et al., 2019, see references); report both, and check with more resamples or another seed."),
            p_note = .("p values come from cSEM: t = estimate / bootstrap standard error."),
            htmt_note = .("Upper bound: the one-sided 95%% bootstrap upper bound (the 95th percentile of %d admissible resamples). Discriminant validity holds when it stays below the threshold (Henseler et al., 2015, see references).")
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
            vif_critical = .("5 or more: critical collinearity"),
            vif_single_predictor = .("Single predictor: no collinearity"),
            f2_none = .("Below 0.02: no effect"),
            f2_small = .("0.02 to 0.15: small"),
            f2_medium = .("0.15 to 0.35: medium"),
            f2_large = .("0.35 or more: large"),
            dimension_of = .("Dimension of %s"),
            reflective_hoc = .("Reflective (higher-order)"),
            formative_hoc = .("Formative (higher-order)"),
            f2i_none = .("Interaction below 0.005: no effect"),
            f2i_small = .("Interaction 0.005 to 0.01: small"),
            f2i_medium = .("Interaction 0.01 to 0.025: medium"),
            f2i_large = .("Interaction 0.025 or more: large"),
            htmt_ok_ci = .("Upper bound below 0.85: distinct constructs"),
            htmt_similar_ci = .("Upper bound 0.85 to 0.90: acceptable only for conceptually similar constructs"),
            htmt_doubt_ci = .("Upper bound 0.90 or more: discriminant validity not established"),
            loadings_caption = .("Loadings of reflective indicators; dashed line at 0.708. Triangles: below 0.708."),
            loadings_caption_boot = .("Loadings of reflective indicators with their %s%% bootstrap interval; dashed line at 0.708. Triangles: below 0.708."),
            htmt_caption = .("HTMT per pair of reflective constructs; darker cells reach 0.85 or 0.90."),
            htmt_caption_boot = .("HTMT per pair, with its one-sided 95% bootstrap upper bound in brackets; the shade follows the upper bound (darker: 0.85 or 0.90 reached)."),
            diagram_caption = .("Path coefficients; line width follows the coefficient. Run the bootstrap to mark which paths are significant."),
            diagram_caption_boot = .("Path coefficients; * the %s%% bootstrap interval excludes 0 (solid line); dashed line: not significant.")
        ),

        # "How to read" guides.
        # ES: Guías "Cómo leer".
        guide = list(
            hoc = .("A higher-order construct is measured by other constructs, its dimensions (for example, psychological capital by hope, efficacy, resilience and optimism). In the disjoint two-stage approach, stage 1 estimates the dimensions as ordinary constructs, linked to the constructs the higher-order construct is related to, and stage 2 treats their scores as the indicators of the higher-order construct (Sarstedt et al., 2019, see references). Read this table as a measurement table one level up: for a reflective higher-order construct, the loadings of its dimensions, its reliability and its AVE, judged on the usual thresholds; for a formative one, the weights of its dimensions, their significance and their VIF. The dimensions themselves are assessed in the first-order tables. Common misreading: drawing structural paths from the dimensions as well as from the higher-order construct; the dimensions only reach the rest of the model through it."),
            moderation = .("An interaction X \u00d7 M asks whether the effect of X on the outcome depends on the moderator M. Its coefficient is the change in that effect when M rises by one standard deviation. The table gives the simple effect of X at three values of M (one standard deviation below the mean, the mean and one above), and the plot draws the same idea twice: the simple slopes (left) and the floodlight analysis (right), the simple effect over the whole observed range of M with its bootstrap interval. The Johnson-Neyman points mark where that interval starts or stops including 0. The first construct of the term is the predictor and the second the moderator, in the order the term was built in the panel. Common misreading: concluding there is moderation because the simple effect is significant at one value of M; moderation is the interaction coefficient itself, and its interval must exclude 0."),
            moderation_f2 = .("The f\u00b2 of an interaction is judged on smaller benchmarks than other paths: 0.005, 0.01 and 0.025 for small, medium and large (Kenny, 2018; Hair et al., 2021, see references), because moderating effects are typically small."),
            model = .("Each row is one construct: how it is measured (reflective or formative), how many indicators it has, and its role in the structural model. Exogenous constructs are only predictors; endogenous constructs are predicted by at least one other construct. Check that this table describes the model you intended before reading any number."),
            measurement = .("Loadings are the correlations between each indicator and its construct; weights are the contribution of each indicator to the construct score. For reflective constructs read the loadings: values of 0.708 or more mean the construct explains at least half of the indicator's variance (Hair et al., 2019, see references). For formative constructs read the weights: they show the relative contribution of each indicator, and a small or negative weight is not by itself a reason to remove an indicator. Common misreading: judging formative indicators by their loadings as if they were reflective."),
            reliability = .("One row per reflective construct. Cronbach's alpha is a lower bound of reliability and composite reliability (\u03c1C) an upper bound; \u03c1A usually lies between them and is the most exact of the three (Hair et al., 2019, see references). Values between 0.70 and 0.95 are satisfactory; 0.60 to 0.70 is acceptable only in exploratory research; above 0.95 the indicators are probably redundant (the same question asked several times). The average variance extracted (AVE) is the share of indicator variance the construct explains on average: 0.50 or more means convergent validity. Common misreading: reading only alpha, which tends to underestimate reliability when loadings differ."),
            reliability_omega = .("\u03c1C is McDonald's \u03c9 computed with the loadings of the PLS model. With PLS these are composite loadings, which are inflated, so \u03c1C is an upper bound of reliability; with consistent PLS they are common-factor loadings and \u03c1C is close to the \u03c9 of a factor analysis. The optional \u03c9 (CFA) column estimates a one-factor confirmatory factor analysis of each construct, outside the PLS model (McDonald, 1999, see references): it is the reference used in psychometrics. Common misreading: reporting \u03c1C from plain PLS as if it were the \u03c9 of a factor analysis."),
            htmt = .("One row per pair of reflective constructs. The heterotrait-monotrait ratio (HTMT) compares the correlations between the indicators of two different constructs with the correlations within each construct (Henseler et al., 2015, see references). Values near 1 mean the two constructs are not distinguished by the data. Below 0.85 the constructs are distinct; up to 0.90 is acceptable when they are conceptually similar (for example, two kinds of satisfaction). HTMT2 uses geometric instead of arithmetic means and is more accurate when loadings differ (Roemer et al., 2021, see references). Common misreading: treating HTMT as a correlation between the constructs; it is a ratio of indicator correlations. A formal test needs the bootstrap (Bootstrap box): discriminant validity holds when the upper bound of HTMT stays below the threshold."),
            vif = .("One row per indicator of a formative construct. The variance inflation factor (VIF) measures how much an indicator overlaps with the other indicators of the same construct. Because formative indicators compete to explain the construct, high overlap makes their weights unstable and can turn them negative. VIF of 5 or more is critical; 3 to 5 may be a problem (Hair et al., 2019, see references). Common misreading: removing an indicator only because its VIF is high; a formative indicator carries part of the content of the construct, so first consider merging indicators or building a higher-order construct."),
            f2 = .("The effect size f\u00b2 tells how much the R\u00b2 of the outcome drops when that predictor is removed: f\u00b2 = (R\u00b2 with \u2212 R\u00b2 without) / (1 \u2212 R\u00b2 with). Values of 0.02, 0.15 and 0.35 are read as small, medium and large (Hair et al., 2019, see references). A significant path can have a negligible f\u00b2 in a large sample: read both."),
            vif_struct = .("The structural VIF measures how much the predictors of the same outcome overlap. Above 3, and certainly above 5, their coefficients become unstable and can even change sign (Hair et al., 2019, see references). With a single predictor there is no collinearity to assess."),
            effects = .("The direct effect is the path itself; the indirect effect is the sum of all routes through other constructs (the product of the paths along each route); the total effect is their sum. An indirect effect whose bootstrap interval excludes 0 means mediation: whether it is full or partial depends on the direct effect, as the mediation section of a later version explains. Common misreading: testing mediation by the significance of each path separately instead of the indirect effect itself."),
            paths = .("Each coefficient is the change in the outcome construct, in standard deviations, when the predictor increases by one standard deviation and the other predictors of the same outcome stay constant. R\u00b2 is the share of the outcome's variance explained by its predictors. Common misreading: treating a large coefficient as significant. Significance needs the bootstrap (Bootstrap box): a path is significant at the chosen level when its interval excludes 0.")
        ),

        # About your choices.
        # ES: Acerca de sus elecciones.
        choices = list(
            pls = .("Estimator: PLS. Every construct is estimated as a weighted combination of its indicators (a composite): Mode A weights for reflective constructs and Mode B for formative ones. This is the PLS-SEM of the primer of Hair et al. Choose consistent PLS when your reflective constructs are meant as common factors and you want estimates corrected for measurement error."),
            plsc = .("Estimator: consistent PLS (PLSc). Reflective constructs are treated as common factors and their estimates are corrected for measurement error (Dijkstra & Henseler, 2015, see references); formative constructs stay composites. PLSc can produce inadmissible solutions in small samples; the report warns when that happens."),
            continuous = .("Indicators: continuous. Correlations between indicators are Pearson correlations, as in classic PLS-SEM. With rating scales of 7 or more categories this is usually adequate; with 5 categories or fewer, or very skewed answers, Pearson correlations underestimate the relationships and ordinal treatment is worth considering."),
            ordinal = .("Indicators: ordinal. Correlations between indicators are polychoric: they assume that each category comes from cutting a continuous latent response, and estimate the correlation between those latent responses (ordinal PLS; Schuberth et al., 2018, see references). This changes the whole model, not only the reliability. Reliability and validity then describe the latent responses; Chalmers (2018, see references) argues that this overstates the reliability of the scores people actually obtain, so report which correlations you used."),
            bootstrap = .("Bootstrap: %d resamples with seed %d; percentile intervals at %s%%. Each resample redraws the cases with replacement and re-estimates the model, so the spread of the estimates shows their sampling uncertainty. The seed makes the result reproducible: the same seed and resamples give the same intervals. Inadmissible resamples are dropped and counted in the bootstrap note. 2000 or more resamples are recommended for a final report (Hair et al., 2019, see references)."),
            no_bootstrap = .("Bootstrap: not run. The tables show estimates without significance; tick Run bootstrap in the Bootstrap box to add intervals, standard errors and p values."),
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
            formative_negative = .("Formative constructs with negative weights: %s. A negative weight can come from collinearity among the indicators (see their VIF) or from an indicator that adds little beyond the others of its construct. Check the significance of the weights with the bootstrap before removing any indicator: a formative indicator carries part of the content of the construct."),
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
            htmt_ci_ok = .("With the bootstrap, the upper bound of HTMT stays below 0.85 for all %d pairs: discriminant validity is established."),
            htmt_ci_high = .("With the bootstrap, the upper bound of HTMT reaches 0.85 or more for %s: discriminant validity is not established for these pairs, even if the estimate is lower."),
            weights_ns_keep = .("Formative indicators with a non-significant weight but a loading of 0.50 or more: %s. Keep them: their absolute contribution to the construct is important even if their relative contribution is not (Hair et al., 2019, see references)."),
            weights_ns_drop = .("Formative indicators with a non-significant weight and a loading below 0.50: %s. Consider removing them only on theoretical grounds: a formative indicator carries part of the content of the construct."),
            weights_all_sig = .("Every weight of the formative constructs is significant."),
            paths_all_sig = .("With the bootstrap, all %d paths are significant at %s%%: their intervals exclude 0."),
            paths_sig = .("With the bootstrap, %d of %d paths are significant at %s%%. Not significant (interval includes 0): %s."),
            f2_big = .("Effect sizes: f\u00b2 of 0.15 or more for %s, the predictors that matter most for their outcome."),
            f2_tiny = .("Effect sizes: f\u00b2 below 0.02 for %s: removing these predictors barely changes the R\u00b2 of their outcome."),
            vifs_ok = .("Collinearity of predictors: every structural VIF is below 3."),
            vifs_high = .("Collinearity of predictors: structural VIF of 3 or more for %s; their coefficients may be unstable."),
            next_steps_boot = .("Read the measurement model first, then the significant paths and their effect sizes. Report each path with its coefficient, interval and f\u00b2, and say how many bootstrap resamples and which seed you used. Mediation analysis, prediction and group comparisons come in later versions of PLSsemLab."),
            vif_high = .("Collinearity of formative indicators: VIF of 3 or more for %s. Their weights may be unstable; interpret them with care."),
            strongest = .("The largest path is %s \u2192 %s (%s)."),
            r2_line = .("%s: R\u00b2 = %s (%s)."),
            r2_substantial = .("substantial"),
            r2_moderate = .("moderate"),
            r2_weak = .("weak"),
            r2_very_weak = .("very weak"),
            r2_note = .("As a rough guide, R\u00b2 values of 0.75, 0.50 and 0.25 are described as substantial, moderate and weak (Hair et al., 2019, see references), but what counts as high depends on the field: in consumer behaviour an R\u00b2 of 0.20 can be high."),
            next_steps = .("Settle the measurement model before reading the paths: a construct that is poorly measured, or not distinct from another, makes its paths hard to interpret. Any indicator you remove changes every number, so remove one at a time and run the analysis again. Run the bootstrap (Bootstrap box) to turn the HTMT thresholds into a test and to get the significance of paths and weights; until then, treat these estimates as descriptive.")
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
            no_inference = .("No significance or confidence intervals: the bootstrap was not run, so the HTMT thresholds are compared with the estimate and not with its upper bound. Tick Run bootstrap in the Bootstrap box."),
            paths_sig = .("Structural model: %d of %d paths are significant at %s%%."),
            boot_basis = .("Intervals from %d admissible of %d bootstrap resamples (seed %d); a path whose interval only just excludes 0 can change with another seed."),
            admissible_no = .("The estimates are not admissible; do not interpret them until the cause is fixed.")
        ),

        # Reproducibility record.
        # ES: Registro de reproducibilidad.
        moderation = list(
            minus_sd = .("\u22121 SD of %s"),
            mean = .("Mean of %s"),
            plus_sd = .("+1 SD of %s"),
            minus_sd_short = .("\u22121 SD"),
            mean_short = .("Mean"),
            plus_sd_short = .("+1 SD"),
            x_axis = .("%s (standardized)"),
            y_axis = .("Predicted %s (standardized)"),
            m_axis = .("%s, moderator (standardized)"),
            effect_axis = .("Effect of %s on %s"),
            slopes_title = .("Simple slopes"),
            flood_title = .("Floodlight"),
            caption = .("Left: predicted outcome over the predictor at three values of the moderator. Right: the simple effect over the observed range of the moderator. Run the bootstrap to add its interval and the Johnson-Neyman points."),
            caption_boot = .("Left: predicted outcome over the predictor at three values of the moderator. Right: the simple effect over the observed range of the moderator with its %s%% bootstrap interval (band); vertical lines: Johnson-Neyman points, where the interval starts or stops including 0."),
            table_note = .("Simple effect of the predictor at each value of the moderator: b(X) + b(X \u00d7 M) \u00d7 M, with M in standard deviations. Run the bootstrap for intervals."),
            table_note_boot = .("Simple effect of the predictor at each value of the moderator: b(X) + b(X \u00d7 M) \u00d7 M, with M in standard deviations; %s%% percentile bootstrap intervals from %d admissible resamples."),
            line = .("Moderation: the interaction %s on %s is %s, the change in the effect of %s on %s when %s rises by one standard deviation."),
            f2_line = .("Its f\u00b2 is %s (%s)."),
            int_sig = .("Its bootstrap interval excludes 0: %s moderates the effect of %s on %s."),
            int_ns = .("Its bootstrap interval includes 0: there is no evidence that %s moderates the effect of %s on %s, whatever the simple effects show."),
            jn_all = .("Along the whole observed range of %3$s, the effect of %1$s on %2$s stays significant."),
            jn_none = .("Along the whole observed range of %3$s, the interval of the effect of %1$s on %2$s includes 0."),
            jn_some = .("The effect of %1$s on %2$s is significant only for part of the range of %3$s; Johnson-Neyman points (in standard deviations of %3$s): %4$s."),
            no_boot = .("Run the bootstrap to know whether the interaction and the simple effects are significant.")
        ),

        repro = list(
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
