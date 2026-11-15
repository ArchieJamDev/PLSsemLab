# -----------------------------------------------------------------------------
# PLSsemLab
# A jamovi module for partial least squares structural equation modeling and
# evidence-based methodological decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of PLSsemLab, licensed under the GNU General Public
# License, version 3 or later. See https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Multigroup analysis (phase 7). The model is estimated in each group of a
# grouping variable (cSEM's .id). MICOM (Henseler, Ringle & Sarstedt, 2016)
# checks first that the composites mean the same in every group: step 1
# (configural) is guaranteed by the module, step 2 (compositional, the
# correlation c between the scores built with each group's weights, tested
# by permutation) and step 3 (equal means and variances, by permutation)
# come from cSEM::testMICOM(). The paths are then compared pair by pair with
# cSEM::testMGD(): the permutation test (Chin & Dibbern, 2010) and PLS-MGA
# (Henseler et al., 2009; Sarstedt et al., 2011), whose bootstrap
# probability P(first <= second) cSEM returns one-sided; the module turns it
# into the two-sided p = 2 min(P, 1 - P), the same decision as cSEM's rule.
# Group labels go to cSEM as internal names G01..Gk (accents and spaces are
# safe, and the order is the order of the levels).
# ES: Análisis multigrupo (fase 7). El modelo se estima en cada grupo de una
# variable de agrupación. MICOM comprueba primero que los compuestos
# signifiquen lo mismo en todos los grupos: el paso 1 (configural) lo
# garantiza el módulo; los pasos 2 (composicional, correlación c entre las
# puntuaciones hechas con los pesos de cada grupo, por permutación) y 3
# (medias y varianzas iguales, por permutación) vienen de testMICOM(). Luego
# las rutas se comparan por pares con testMGD(): la prueba de permutación y
# PLS-MGA, cuya probabilidad bootstrap P(primero <= segundo) cSEM da
# unilateral; el módulo la convierte en la p bilateral 2 min(P, 1 - P), la
# misma decisión que la regla de cSEM. Las etiquetas de los grupos van a
# cSEM como nombres internos G01..Gk.
# -----------------------------------------------------------------------------

# Fewest cases a group may have, and most groups (45 pairs) the module
# compares. Rules of the module, stated as such in the report.
# ES: Mínimo de casos por grupo y máximo de grupos. Reglas del módulo.
.PL_MGA_MIN <- 10
.PL_MGA_MAX_GROUPS <- 10

# Levels of the grouping variable that have at least one case, in the order
# of the factor levels (jamovi's order) or, for other types, sorted. jamovi
# runs .init() with the header of the data only (no rows): then every level
# of the factor is returned, and .run() rebuilds the rows if a level turns
# out to have no cases.
# ES: Niveles de la variable de agrupación con al menos un caso. jamovi
# corre .init() solo con la cabecera de los datos (sin filas): entonces se
# devuelven todos los niveles del factor, y .run() rehace las filas si un
# nivel resulta no tener casos.
.pl_mga_levels <- function(g) {
    if (is.null(g)) return(character(0))
    if (is.factor(g)) {
        lev <- levels(g)
        if (!length(g)) return(lev)
        return(lev[lev %in% as.character(g[!is.na(g)])])
    }
    sort(unique(as.character(g[!is.na(g)])))
}

# Why the multigroup analysis cannot run (NULL when it can): list(key, args)
# for a sprintf() of the footnote. Category B: the rows stay with the reason.
# ES: Por qué el análisis multigrupo no puede correr (NULL si puede).
.pl_mga_reason <- function(hocs, group, indicators, n_by_group) {
    if (length(hocs)) return(list(key = "mga_hoc"))
    if (group %in% indicators) return(list(key = "mga_group_indicator", args = list(group)))
    if (length(n_by_group) < 2) return(list(key = "mga_one_group", args = list(group)))
    if (length(n_by_group) > .PL_MGA_MAX_GROUPS)
        return(list(key = "mga_many_groups", args = list(group, length(n_by_group), .PL_MGA_MAX_GROUPS)))
    small <- n_by_group[n_by_group < .PL_MGA_MIN]
    if (length(small))
        return(list(key = "mga_small_group",
                    args = list(paste(sprintf("%s (n = %d)", names(small), small), collapse = ", "), .PL_MGA_MIN)))
    NULL
}

# Why MICOM in particular cannot run (NULL when it can). MICOM is defined for
# composites (Henseler, Ringle & Sarstedt, 2016): with PLSc the reflective
# constructs are common factors. With ordinal indicators cSEM's testMICOM()
# builds the scores from the raw codes and stops.
# ES: Por qué MICOM no puede correr (NULL si puede): MICOM se define para
# compuestos; con indicadores ordinales testMICOM() se detiene.
.pl_micom_reason <- function(estimator, scale) {
    if (identical(estimator, "plsc")) return("micom_plsc")
    if (identical(scale, "ordinal")) return("micom_ordinal")
    NULL
}

# The data with the internal group column (PLgroup = G01..Gk), without the
# cases that have no group value.
# ES: Los datos con la columna interna de grupo, sin los casos sin grupo.
.pl_mga_data <- function(d, g, lev) {
    k <- match(as.character(g), lev)
    keep <- !is.na(k)
    ids <- sprintf("G%02d", seq_along(lev))
    dg <- d[keep, , drop = FALSE]
    dg$PLgroup <- ids[k[keep]]
    list(data = dg, ids = ids, labels = stats::setNames(lev, ids),
         n = stats::setNames(as.integer(table(factor(dg$PLgroup, levels = ids))), ids),
         n_missing = sum(!keep))
}

# The structural part of the model, in internal names, for
# .parameters_to_compare (only the paths are compared).
# ES: La parte estructural del modelo, en nombres internos.
.pl_mga_compare <- function(paths, cmap) {
    tos <- unique(paths$to)
    paste(vapply(tos, function(to)
        paste(cmap[[to]], "~", paste(vapply(paths$from[paths$to == to], function(f) cmap[[f]], ""), collapse = " + ")),
        ""), collapse = "\n")
}

# The pairs of groups, as cSEM forms them (utils::combn of the group names).
# ES: Los pares de grupos, como los forma cSEM.
.pl_mga_pairs <- function(ids) utils::combn(ids, 2, simplify = FALSE)

# MICOM steps 2 and 3. Returns one row per pair and construct: c, the 5%
# quantile of its permutation distribution, the one-sided permutation p of
# step 2 (share of permutations with c at or below the observed one), and the
# difference of means and the log of the ratio of variances of the pooled
# standardised scores (first group minus second) with their two-sided
# permutation p.
# ES: Pasos 2 y 3 de MICOM: una fila por par y constructo.
.pl_micom <- function(fg, reps, seed) {
    restore_rng <- .pl_rng_guard()
    on.exit(restore_rng(), add = TRUE)
    mi <- cSEM::testMICOM(fg, .R = reps, .seed = seed, .verbose = FALSE)
    pairs <- .pl_mga_pairs(names(fg))
    rows <- list()
    for (k in seq_along(pairs)) {
        st <- mi$Step2$Test_statistic[[k]]
        cons <- names(st)
        perm <- do.call(rbind, lapply(mi$Step2$Bootstrap_values, function(b) b[[k]][cons]))
        c5 <- apply(perm, 2, stats::quantile, probs = 0.05, na.rm = TRUE, names = FALSE)
        rows[[k]] <- data.frame(
            pair = k, g1 = pairs[[k]][1], g2 = pairs[[k]][2], construct = cons,
            c = unname(st), c5 = unname(c5[cons]), p_c = unname(mi$Step2$P_value$none[[k]][cons]),
            mean = unname(mi$Step3$Mean$Test_statistic[[k]][cons]),
            p_mean = unname(mi$Step3$Mean$P_value$none[[k]][cons]),
            var = unname(mi$Step3$Var$Test_statistic[[k]][cons]),
            p_var = unname(mi$Step3$Var$P_value$none[[k]][cons]),
            stringsAsFactors = FALSE)
    }
    tab <- do.call(rbind, rows)
    tab$verdict <- .pl_micom_verdict(tab$p_c, tab$p_mean, tab$p_var)
    list(table = tab, admissible = mi$Information$Number_admissibles, runs = mi$Information$Total_runs)
}

# MICOM verdict per construct (Henseler, Ringle & Sarstedt, 2016): no
# compositional invariance (step 2 fails), partial (step 2 holds), full
# (step 2 and both parts of step 3 hold). alpha = .05.
# ES: Veredicto MICOM por constructo: ninguna, parcial o total.
.pl_micom_verdict <- function(p_c, p_mean, p_var, alpha = 0.05) {
    ifelse(p_c < alpha, "inv_none", ifelse(p_mean >= alpha & p_var >= alpha, "inv_full", "inv_partial"))
}

# Path differences, pair by pair. est1/est2: the path in each group;
# diff = est1 - est2 (Chin's statistic); p_perm: permutation p, (count + 1) /
# (R + 1); p_one: PLS-MGA's P(first <= second); p_hens: its two-sided form.
# adjust: a stats::p.adjust() method applied over every path and pair, to
# both tests.
# ES: Diferencias de rutas por par de grupos; p_one es la P(primero <=
# segundo) de PLS-MGA y p_hens su forma bilateral.
.pl_mgd <- function(fg, compare, perms, boots, seed) {
    restore_rng <- .pl_rng_guard()
    on.exit(restore_rng(), add = TRUE)
    mg <- cSEM::testMGD(fg, .approach_mgd = c("Henseler", "Chin"), .R_permutation = perms,
                        .R_bootstrap = boots, .seed = seed, .verbose = FALSE,
                        .parameters_to_compare = compare)
    pairs <- .pl_mga_pairs(names(fg))
    coef <- function(g, name) {
        p <- strsplit(name, " ~ ", fixed = TRUE)[[1]]
        fg[[g]]$Estimates$Path_estimates[p[1], p[2]]
    }
    rows <- list()
    for (k in seq_along(pairs)) {
        st <- mg$Chin$Test_statistic[[k]]
        prm <- names(st)
        g1 <- pairs[[k]][1]; g2 <- pairs[[k]][2]
        p_one <- unname(mg$Henseler$P_value$none[[k]][prm])
        rows[[k]] <- data.frame(
            pair = k, g1 = g1, g2 = g2, name = prm,
            est1 = vapply(prm, function(x) coef(g1, x), 0, USE.NAMES = FALSE),
            est2 = vapply(prm, function(x) coef(g2, x), 0, USE.NAMES = FALSE),
            diff = unname(st), p_perm = unname(mg$Chin$P_value$none[[k]][prm]),
            p_one = p_one, p_hens = pmin(1, 2 * pmin(p_one, 1 - p_one)),
            stringsAsFactors = FALSE)
    }
    ip <- mg$Information$Information_permutation
    ib <- mg$Information$Information_bootstrap
    list(table = do.call(rbind, rows),
         admissible_perm = ip$Number_admissibles, runs_perm = ip$Total_runs,
         admissible_boot = unlist(ib$Number_admissibles), runs_boot = unlist(ib$Total_runs))
}

# Correction for multiple comparisons over every path and pair, each test
# on its own; "none" leaves the p values as they are.
# ES: Corrección por comparaciones múltiples sobre todas las rutas y pares.
.pl_mgd_adjust <- function(tab, method) {
    if (is.null(method) || method == "none") return(tab)
    tab$p_perm <- stats::p.adjust(tab$p_perm, method = method)
    tab$p_hens <- stats::p.adjust(tab$p_hens, method = method)
    tab
}

# Decision per path from the two tests at alpha = .05.
# ES: Decisión por ruta a partir de las dos pruebas.
.pl_mgd_decision <- function(p_perm, p_hens, alpha = 0.05) {
    a <- p_perm < alpha; b <- p_hens < alpha
    ifelse(a & b, "mgd_both", ifelse(a, "mgd_perm_only", ifelse(b, "mgd_hens_only", "mgd_none")))
}
