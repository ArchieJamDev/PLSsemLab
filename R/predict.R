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
# Out-of-sample prediction (phase 6): one k-fold cross-validation, repeated
# r times, feeds both PLSpredict (Shmueli et al., 2016, 2019) and CVPAT
# (Liengaard et al., 2021; Sharma et al., 2023). The folds are cSEM's own
# (resampleData, seed + repetition - 1), so with one repetition the residuals
# equal cSEM::predict(.r = 1, .seed = seed). For each fold the model is
# estimated on the training part and cSEM::predict(.test_data) predicts the
# indicators of the endogenous constructs of the test part (PLS); the
# linear-model benchmark LM is the regression on the exogenous indicators
# with an intercept (.pl_lm_residuals()), and the indicator-average
# benchmark IA predicts each indicator by its training mean.
# ES: Predicción fuera de la muestra (fase 6): una validación cruzada de k
# particiones, repetida r veces, alimenta PLSpredict y CVPAT. Las
# particiones son las de cSEM, así que con una repetición los residuos son
# iguales a cSEM::predict(.r = 1, .seed = semilla). En cada partición el
# modelo se estima con la parte de entrenamiento y cSEM::predict(.test_data)
# predice los indicadores de los constructos endógenos de la parte de prueba
# (PLS y el modelo lineal LM); la referencia IA predice cada indicador con
# su media de entrenamiento.
# -----------------------------------------------------------------------------

# Why prediction cannot run for this model (NULL when it can). Category B.
# ES: Por qué la predicción no puede correr con este modelo (NULL si puede).
.pl_predict_reason <- function(model, hocs, admissible) {
    if (isTRUE(model$nonlinear)) return("predict_nonlinear")
    if (length(hocs)) return("predict_hoc")
    if (!admissible) return("predict_inadmissible")
    NULL
}

# Cross-validated residuals. fit: the full-sample cSEM fit; refit(data)
# estimates the same model on other data (alt_refit, optional, a competing
# model for CVPAT). Returns, per repetition, n x p matrices (rows in the
# order of the data) of residuals for PLS, LM, IA and ALT.
# ES: Residuos de validación cruzada, por repetición, para PLS, LM, IA y ALT.
.pl_cv_residuals <- function(fit, d, refit, folds, reps, seed, alt_refit = NULL,
                             checkpoint = function() NULL, bench_refit = NULL) {
    restore_rng <- .pl_rng_guard()
    on.exit(restore_rng(), add = TRUE)
    ids <- rownames(d)
    out <- list()
    inadmissible <- 0
    one <- function(f, test) {
        p <- suppressWarnings(cSEM::predict(f, .test_data = test, .benchmark = "NA"))
        get1 <- function(x) if (is.list(x)) x[[1]] else x
        list(t = get1(p$Residuals_target))
    }
    exo <- .pl_exo_indicators(fit)
    for (r in seq_len(reps)) {
        checkpoint()
        cv <- cSEM::resampleData(fit, .resample_method = "cross-validation", .cv_folds = folds, .R = 1,
                                 .seed = seed + r - 1)[[1]]
        rows <- list()
        for (k in seq_along(cv)) {
            te <- match(rownames(cv[[k]]), ids)
            train <- d[-te, , drop = FALSE]
            test <- d[te, , drop = FALSE]
            fk <- refit(train)
            if (sum(cSEM::verify(fk)) != 0) inadmissible <- inadmissible + 1
            pk <- one(fk, test)
            pk$b <- .pl_lm_residuals(train, test, exo, colnames(pk$t))
            ia <- sweep(as.matrix(.pl_numeric(test[, colnames(pk$t), drop = FALSE])), 2,
                        colMeans(as.matrix(.pl_numeric(train[, colnames(pk$t), drop = FALSE]))))
            alt <- NULL
            if (!is.null(alt_refit)) {
                ak <- one(alt_refit(train), test)
                alt <- ak$t[, colnames(pk$t), drop = FALSE]
            }
            # extra benchmark (1.2.0): the same model with another
            # weighting approach, on the same folds
            # ES: referencia adicional (1.2.0): el mismo modelo con otro
            # enfoque de ponderación, en los mismos pliegues
            bm <- NULL
            if (!is.null(bench_refit)) {
                bk <- one(bench_refit(train), test)
                bm <- bk$t[, colnames(pk$t), drop = FALSE]
            }
            rows[[k]] <- list(te = te, t = pk$t, b = pk$b, ia = ia, alt = alt, bm = bm)
        }
        te <- unlist(lapply(rows, `[[`, "te"))
        o <- order(te)
        stack <- function(x) {
            if (is.null(rows[[1]][[x]])) return(NULL)
            m <- do.call(rbind, lapply(rows, function(z) unname(as.matrix(z[[x]]))))
            if (is.null(m)) return(NULL)
            m <- m[o, , drop = FALSE]
            colnames(m) <- colnames(rows[[1]]$t)
            m
        }
        out[[r]] <- list(PLS = stack("t"), LM = stack("b"), IA = stack("ia"), ALT = stack("alt"), BM = stack("bm"))
    }
    list(res = out, inadmissible = inadmissible, folds = folds, reps = reps, seed = seed)
}

# The linear-model benchmark LM of PLSpredict (Shmueli et al., 2019): each
# indicator of an endogenous construct regressed, with an intercept, on the
# indicators of the exogenous constructs, estimated on the training part and
# applied to the test part. cSEM 0.7.1's predict(.benchmark = "lm") solves
# (X'X)^-1 X'y on the uncentered data without an intercept (a regression
# through the origin), which inflates the benchmark's errors and favors PLS;
# its results match the published ones only with the intercept. Ordinal
# indicators are predicted on their codes and rounded, as cSEM does.
# ES: La referencia LM de PLSpredict: cada indicador de un constructo
# endógeno regresado, con intercepto, sobre los indicadores de los
# constructos exógenos. predict(.benchmark = "lm") de cSEM 0.7.1 resuelve
# (X'X)^-1 X'y sin intercepto (regresión por el origen), lo que infla los
# errores de la referencia y favorece a PLS.
.pl_exo_indicators <- function(fit) {
    m <- fit$Information$Model
    exo <- rownames(m$structural)[rowSums(m$structural) == 0]
    colnames(m$measurement)[colSums(m$measurement[exo, , drop = FALSE]) > 0]
}

.pl_lm_residuals <- function(train, test, exo, endo) {
    ordinal <- vapply(test[, endo, drop = FALSE], is.factor, TRUE)
    xtr <- cbind(1, as.matrix(.pl_numeric(train[, exo, drop = FALSE])))
    xte <- cbind(1, as.matrix(.pl_numeric(test[, exo, drop = FALSE])))
    ytr <- as.matrix(.pl_numeric(train[, endo, drop = FALSE]))
    yte <- as.matrix(.pl_numeric(test[, endo, drop = FALSE]))
    beta <- qr.coef(qr(xtr), ytr)
    beta[is.na(beta)] <- 0
    pred <- xte %*% beta
    if (any(ordinal)) pred[, ordinal] <- round(pred[, ordinal])
    res <- yte - pred
    colnames(res) <- endo
    res
}

# Ordered factors back to their codes (IA works on the indicator metric).
# ES: Factores ordenados a sus códigos.
.pl_numeric <- function(df) {
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.numeric(df[[v]])
    df
}

# PLSpredict metrics per indicator, averaged over the repetitions:
# Q2_predict = 1 - SS(PLS residuals) / SS(IA residuals), with the residuals
# centred as cSEM's calculateq2() does (the difference from the uncentred
# formula is of the order of 1e-6), RMSE and MAE for PLS and LM.
# ES: Métricas de PLSpredict por indicador, promediadas sobre repeticiones;
# Q2_predict con los residuos centrados, como calculateq2() de cSEM.
.pl_predict_metrics <- function(cv) {
    skew <- function(e) apply(e, 2, function(x) { x <- x - mean(x); mean(x^3) / mean(x^2)^1.5 })
    per <- lapply(cv$res, function(x) data.frame(
        indicator = colnames(x$PLS),
        q2 = 1 - colSums(sweep(x$PLS, 2, colMeans(x$PLS))^2) / colSums(sweep(x$IA, 2, colMeans(x$IA))^2),
        rmse_pls = sqrt(colMeans(x$PLS^2)), rmse_lm = sqrt(colMeans(x$LM^2)),
        mae_pls = colMeans(abs(x$PLS)), mae_lm = colMeans(abs(x$LM)),
        skew = skew(x$PLS),
        stringsAsFactors = FALSE))
    if (!is.null(cv$res[[1]]$BM))
        for (r in seq_along(per)) {
            per[[r]]$rmse_bm <- sqrt(colMeans(cv$res[[r]]$BM^2))
            per[[r]]$mae_bm <- colMeans(abs(cv$res[[r]]$BM))
        }
    m <- per[[1]]
    for (v in intersect(c("q2", "rmse_pls", "rmse_lm", "mae_pls", "mae_lm", "skew", "rmse_bm", "mae_bm"), names(m)))
        m[[v]] <- rowMeans(sapply(per, `[[`, v))
    # The metric that compares PLS with LM (Shmueli et al., 2019): RMSE, or
    # MAE when the prediction errors are clearly asymmetric (|skewness| > 1,
    # the module's rule of thumb), since the RMSE then weighs a few large
    # errors too much.
    # ES: La métrica que compara PLS con LM: RMSE, o MAE si los errores de
    # predicción son claramente asimétricos (|asimetría| > 1).
    m$metric <- ifelse(abs(m$skew) > 1, "MAE", "RMSE")
    m$pls_better <- ifelse(m$metric == "MAE", m$mae_pls < m$mae_lm, m$rmse_pls < m$rmse_lm)
    m$tie <- ifelse(m$metric == "MAE", m$mae_pls == m$mae_lm, m$rmse_pls == m$rmse_lm)
    rownames(m) <- NULL
    m
}

# Whether the sample is small for the cross-validation: fewer than 20 cases
# in each held-out fold, or fewer than 10 training cases per predictor of
# the linear-model benchmark (the indicators of the exogenous constructs).
# Rules of thumb of the module, stated as such in the report.
# ES: Si la muestra es pequeña para la validación cruzada: menos de 20 casos
# por partición de prueba o menos de 10 casos de entrenamiento por predictor
# del modelo lineal. Reglas prácticas del módulo.
.pl_predict_small <- function(n, folds, n_exog) {
    test <- floor(n / folds)
    train <- n - ceiling(n / folds)
    list(small = test < 20 || train < 10 * n_exog, test = test, train = train, n_exog = n_exog)
}

# CVPAT: the loss of each case is its squared prediction error averaged over
# the indicators of a construct (or of all endogenous constructs) and over
# the repetitions; D = loss(PLS) - loss(benchmark), SmartPLS's sign (negative
# = PLS predicts better), is tested with a paired t test (Liengaard et al.,
# 2021). Against IA and LM the test is one-sided (H1: D < 0; Sharma et al.,
# 2023); against a competing model it is two-sided, as cSEM::testCVPAT().
# groups: list of indicator vectors (names = construct keys; "" = all).
# ES: CVPAT: la pérdida de cada caso es su error cuadrático promedio en los
# indicadores de un constructo (o de todos) y en las repeticiones; se prueba
# D = pérdida(PLS) - pérdida(referencia) (negativa = PLS predice mejor) con
# una t pareada. Frente a IA y LM la prueba es unilateral; frente a un
# modelo competidor, bilateral.
.pl_cvpat <- function(cv, groups) {
    loss <- function(which, ind) {
        rowMeans(sapply(cv$res, function(x) rowMeans(x[[which]][, ind, drop = FALSE]^2)))
    }
    benches <- c("IA", "LM", if (!is.null(cv$res[[1]]$ALT)) "ALT", if (!is.null(cv$res[[1]]$BM)) "BM")
    rows <- list()
    for (g in names(groups)) for (b in benches) {
        lp <- loss("PLS", groups[[g]])
        lb <- loss(b, groups[[g]])
        D <- lp - lb
        n <- length(D)
        tt <- mean(D) / sqrt(stats::var(D) / n)
        # two-sided against a competing model or another weighting approach
        # ES: bilateral frente a un modelo competidor u otro enfoque
        p <- if (b %in% c("ALT", "BM")) 2 * stats::pt(-abs(tt), n - 1) else stats::pt(tt, n - 1)
        rows[[length(rows) + 1]] <- data.frame(group = g, bench = b, loss_pls = mean(lp), loss_bench = mean(lb),
                                               diff = mean(D), t = tt, p = p, stringsAsFactors = FALSE)
    }
    do.call(rbind, rows)
}

# The paths that the competing model of CVPAT leaves out, from the option
# (terms "Predictor → Outcome"); only paths of the model are kept.
# ES: Las rutas que el modelo competidor de CVPAT deja fuera.
.pl_cvpat_drop <- function(terms, paths) {
    labs <- paste(paths$from, "\u2192", paths$to)
    sel <- unique(unlist(lapply(terms, function(t) if (length(t) == 1) t else NULL)))
    paths[labs %in% sel, , drop = FALSE]
}

# Model comparison (1.2.0). Criteria per endogenous construct: R² and
# adjusted R² (higher is better) and cSEM's information criteria of the
# structural equations (lower is better): AIC, AICu, BIC, GM and HQ; Sharma
# et al. (2019, 2021) recommend BIC and GM for choosing among PLS-SEM models.
# SRMR of both models (NA when cSEM cannot compute it, e.g. with an
# interaction). A construct the competing model leaves without predictors
# has no criteria there (NA).
# ES: Comparación de modelos (1.2.0). Criterios por constructo endógeno: R²
# y R² ajustado (mayor es mejor) y los criterios de información de las
# ecuaciones estructurales (menor es mejor): AIC, AICu, BIC, GM y HQ.
.PL_COMPARE <- c("R2", "R2adj", "AIC", "AICu", "BIC", "GM", "HQ")

.pl_compare <- function(fit1, fit2, cmap, endo) {
    crit <- function(f) cSEM::calculateModelSelectionCriteria(f, .by_equation = TRUE, .only_structural = TRUE)
    c1 <- crit(fit1)
    c2 <- crit(fit2)
    val <- function(f, cc, k, id) {
        v <- switch(k, R2 = f$Estimates$R2[id], R2adj = f$Estimates$R2adj[id], cc[[k]][id])
        if (is.null(v) || length(v) == 0) NA_real_ else unname(v)
    }
    rows <- list()
    for (cn in endo) {
        id <- cmap[[cn]]
        for (k in .PL_COMPARE) {
            a <- val(fit1, c1, k, id)
            b <- val(fit2, c2, k, id)
            higher <- k %in% c("R2", "R2adj")
            pref <- if (is.na(a) || is.na(b)) NA_character_
                    else if (isTRUE(all.equal(a, b))) "equal"
                    else if ((a > b) == higher) "main" else "competing"
            rows[[length(rows) + 1]] <- data.frame(construct = cn, criterion = k, main = a, competing = b,
                                                   preferred = pref, stringsAsFactors = FALSE)
        }
    }
    srmr <- function(f) tryCatch(unname(cSEM::calculateSRMR(f)), error = function(e) NA_real_)
    list(table = do.call(rbind, rows), srmr = c(srmr(fit1), srmr(fit2)))
}
