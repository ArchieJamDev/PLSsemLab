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
# Quality criteria of the measurement model (phase 2 of docs/DESIGN.md §8).
# ES: Criterios de calidad del modelo de medida (fase 2).
#
# Every criterion is computed separately, never with cSEM::assess(), and each
# in its own capture: one that fails leaves its cells "Not computable" with
# the reason and never stops the others (Category B; DESIGN.md D3).
#
# With the PLS estimator the module builds reflective constructs as Mode A
# composites (`<~`), so every cSEM function is called with
# `.only_common_factors = FALSE`; the values for the reflective constructs
# are then identical to those of the common-factor syntax (difference 0,
# checked on the extended corporate reputation model). rho_A only exists as
# the correction factor of consistent PLS: with PLS it comes from a PLSc
# estimation of the same model, whose weights are the PLS weights.
#
# ES: Cada criterio se calcula por separado, nunca con cSEM::assess(), y cada
# uno en su propia captura: el que falla deja sus celdas como "No computable"
# con la razón y nunca detiene a los demás (Categoría B).
#
# ES: Con el estimador PLS el módulo arma los reflectivos como compuestos de
# Modo A (`<~`), así que cada función de cSEM se llama con
# `.only_common_factors = FALSE`; los valores de los reflectivos son
# idénticos a los de la sintaxis de factor común (diferencia 0, comprobado
# con el modelo extendido de reputación corporativa). rho_A solo existe como
# factor de corrección de PLS consistente: con PLS sale de una estimación
# PLSc del mismo modelo, cuyos pesos son los de PLS.
# -----------------------------------------------------------------------------

# Returns
#   reliability  data.frame(construct, single, alpha, rhoA, rhoC, ave)
#   htmt         data.frame(c1, c2, htmt, htmt2)
#   fl           list(matrix of construct correlations, sqrt of the AVE)
#   vif          data.frame(construct, indicator, single, vif)
#   errors       named list of messages of criteria that failed entirely
#   warnings     cSEM warnings not already explained by a footnote
# ES: Devuelve las tablas largas, los errores de criterios que fallaron por
# completo y los avisos de cSEM que no explica ya una nota al pie.
.pl_measurement_quality <- function(fit, blocks, paths, model, data, options) {
    refl <- Filter(function(b) b$type == "reflective", blocks)
    form <- Filter(function(b) b$type == "formative", blocks)
    rlab <- vapply(refl, `[[`, "", "label")
    rint <- unname(model$cmap[rlab])
    errors <- list()
    warnings <- character(0)
    explained <- "monotrait|Intra-block|NaNs produced|different signs"
    grab <- function(name, expr) {
        r <- .pl_capture(expr)
        warnings <<- c(warnings, r$warnings[!grepl(explained, r$warnings)])
        if (!is.null(r$error)) errors[[name]] <<- r$error
        r$value
    }
    pick <- function(v, ids) {
        if (is.null(v)) return(rep(NA_real_, length(ids)))
        out <- unname(v[ids])
        out[!is.finite(out)] <- NA_real_
        out
    }

    out <- list(reliability = NULL, htmt = NULL, fl = NULL, vif = NULL)
    if (length(refl)) {
        alpha <- grab("alpha", cSEM::calculateRhoT(fit, .only_common_factors = FALSE))
        # With an interaction cSEM cannot build the model-implied matrix
        # calculateRhoC() relies on; rho_C is then computed from the
        # loadings, (sum l)^2 / ((sum l)^2 + sum(1 - l^2)), which equals
        # calculateRhoC() in linear models (test-interaction.R).
        # ES: Con una interacción cSEM no puede armar la matriz implicada que
        # usa calculateRhoC(); rho_C sale entonces de las cargas, igual que
        # calculateRhoC() en modelos lineales.
        rhoC <- if (isTRUE(model$nonlinear)) grab("rhoC", .pl_rhoC_loadings(fit, rint))
                else grab("rhoC", cSEM::calculateRhoC(fit, .only_common_factors = FALSE))
        ave <- grab("ave", cSEM::calculateAVE(fit, .only_common_factors = FALSE))
        rhoA <- if (options$estimator == "plsc") {
            fit$Estimates$Reliabilities
        } else {
            plsc <- .pl_build_syntax(blocks, paths, "plsc")
            refit <- grab("rhoA", cSEM::csem(
                .data = data, .model = plsc$syntax,
                .PLS_modes = if (length(plsc$modes)) plsc$modes else NULL,
                .disattenuate = TRUE, .PLS_weight_scheme_inner = options$innerScheme))
            if (is.null(refit)) NULL else refit$Estimates$Reliabilities
        }
        single <- vapply(refl, function(b) length(b$vars) == 1, TRUE)
        out$reliability <- data.frame(construct = rlab, single = single,
                                      alpha = pick(alpha, rint), rhoA = pick(rhoA, rint),
                                      rhoC = pick(rhoC, rint), ave = pick(ave, rint),
                                      stringsAsFactors = FALSE)
        # A single indicator has no internal consistency: alpha is NaN in cSEM
        # and rho_C, rho_A and AVE are 1 by definition. They are not shown.
        # ES: Un solo indicador no tiene consistencia interna: no se muestran.
        out$reliability[single, c("alpha", "rhoA", "rhoC", "ave")] <- NA_real_

        # McDonald's omega from a one-factor CFA per construct (optional).
        # ES: Omega de McDonald de un AFC de un factor por constructo.
        out$reliability$omega <- NA_real_
        out$reliability$omega_reason <- NA_character_
        if (isTRUE(options$omegaCfa)) {
            for (k in which(!single)) {
                vars <- unname(model$vmap[refl[[k]]$vars])
                o <- .pl_omega_cfa(data[, vars, drop = FALSE], options$indicatorScale == "ordinal")
                out$reliability$omega[k] <- o$value
                out$reliability$omega_reason[k] <- o$reason
            }
        }

        vcv <- fit$Estimates$Construct_VCV[rint, rint, drop = FALSE]
        dimnames(vcv) <- list(rlab, rlab)
        out$fl <- list(cor = vcv, sqrt_ave = stats::setNames(sqrt(out$reliability$ave), rlab))
    }

    if (length(refl) >= 2) {
        h <- grab("htmt", cSEM::calculateHTMT(fit, .only_common_factors = FALSE))
        h2 <- grab("htmt2", cSEM::calculateHTMT(fit, .type_htmt = "htmt2", .only_common_factors = FALSE))
        # Without inference cSEM returns the ratio in the lower triangle, in the
        # order of the constructs of the fit.
        # ES: Sin inferencia cSEM da la razón en el triángulo inferior, en el
        # orden de los constructos del ajuste.
        cell <- function(m, a, b) {
            if (is.null(m)) return(NA_real_)
            m <- if (is.list(m)) m$htmts else m
            ia <- match(a, rownames(m))
            ib <- match(b, rownames(m))
            v <- if (ia > ib) m[ia, ib] else m[ib, ia]
            if (is.finite(v)) v else NA_real_
        }
        pairs <- utils::combn(seq_along(refl), 2)
        out$htmt <- data.frame(
            c1 = rlab[pairs[1, ]], c2 = rlab[pairs[2, ]],
            htmt = vapply(seq_len(ncol(pairs)), function(k) cell(h, rint[pairs[1, k]], rint[pairs[2, k]]), 0),
            htmt2 = vapply(seq_len(ncol(pairs)), function(k) cell(h2, rint[pairs[1, k]], rint[pairs[2, k]]), 0),
            stringsAsFactors = FALSE)
    }

    # VIF of formative indicators: the diagonal of the inverse of the
    # indicator correlation matrix of each block, the matrix the model was
    # estimated with. With continuous indicators it equals
    # cSEM::calculateVIFModeB() (difference below 1e-15); with ordinal ones
    # it uses the polychoric correlations, while calculateVIFModeB() would
    # regress the raw category codes (Pearson), inconsistent with the model.
    # ES: VIF de los indicadores formativos: la diagonal de la inversa de la
    # matriz de correlaciones de cada bloque, la misma con que se estimó el
    # modelo. Con indicadores continuos es igual a calculateVIFModeB(); con
    # ordinales usa las policóricas, mientras que calculateVIFModeB()
    # regresaría los códigos crudos (Pearson), inconsistente con el modelo.
    if (length(form)) {
        S <- fit$Estimates$Indicator_VCV
        v <- lapply(form, function(b) {
            if (length(b$vars) < 2) return(NULL)
            ids <- unname(model$vmap[b$vars])
            r <- grab("vif", diag(solve(S[ids, ids, drop = FALSE])))
            if (is.null(r)) NULL else stats::setNames(r, ids)
        })
        names(v) <- vapply(form, `[[`, "", "label")
        rows <- list()
        for (b in form) for (x in b$vars) {
            vb <- v[[b$label]]
            value <- if (is.null(vb)) NA_real_ else vb[[model$vmap[[x]]]]
            rows[[length(rows) + 1]] <- data.frame(construct = b$label, indicator = x,
                                                   single = length(b$vars) == 1,
                                                   vif = if (is.finite(value)) value else NA_real_,
                                                   stringsAsFactors = FALSE)
        }
        out$vif <- do.call(rbind, rows)
    }
    out$errors <- errors
    out$warnings <- unique(warnings)
    out
}

# rho_C from the standardized loadings of each construct in ids.
# ES: rho_C a partir de las cargas estandarizadas de cada constructo.
.pl_rhoC_loadings <- function(fit, ids) {
    L <- fit$Estimates$Loading_estimates
    vapply(stats::setNames(ids, ids), function(cn) {
        l <- L[cn, L[cn, ] != 0]
        sum(l)^2 / (sum(l)^2 + sum(1 - l^2))
    }, 0)
}

# McDonald's omega (total) of a one-factor CFA with lavaan: standardized
# loadings, omega = (sum l)^2 / ((sum l)^2 + sum(1 - l^2)). Ordinal
# indicators are declared as ordered (WLSMV on polychoric correlations), so
# the value refers to the latent continuous responses. Three or more
# indicators are needed for the one-factor model to be identified.
# Returns list(value, reason), reason one of NA, "two", "nonconverged",
# "heywood" or an error message.
# ES: Omega de McDonald (total) de un AFC de un factor con lavaan, con cargas
# estandarizadas. Los ordinales se declaran como ordenados (WLSMV sobre
# policóricas), así que el valor se refiere a las respuestas latentes
# continuas. Se necesitan tres o más indicadores para identificar el modelo.
.pl_omega_cfa <- function(x, ordinal) {
    if (ncol(x) < 3) return(list(value = NA_real_, reason = "two"))
    spec <- paste("F =~", paste(names(x), collapse = " + "))
    r <- .pl_capture(lavaan::cfa(spec, data = x, std.lv = TRUE,
                                 ordered = if (ordinal) names(x) else NULL))
    if (!is.null(r$error)) return(list(value = NA_real_, reason = r$error))
    if (!isTRUE(lavaan::lavInspect(r$value, "converged")))
        return(list(value = NA_real_, reason = "nonconverged"))
    l <- lavaan::lavInspect(r$value, "std")$lambda[, 1]
    if (any(!is.finite(l)) || any(abs(l) >= 1))
        return(list(value = NA_real_, reason = "heywood"))
    list(value = sum(l)^2 / (sum(l)^2 + sum(1 - l^2)), reason = NA_character_)
}

# Verbal assessment of each row, with the thresholds of Hair et al. (2019)
# and Henseler et al. (2015). One function per table, so the interpretation
# and the key findings count exactly what the tables say.
# ES: Valoración verbal de cada fila, con los umbrales de Hair et al. (2019)
# y Henseler et al. (2015). Una función por tabla, para que la interpretación
# y los hallazgos clave cuenten exactamente lo que dicen las tablas.
.pl_band_reliability <- function(rhoC) {
    ifelse(is.na(rhoC), NA_character_,
    ifelse(rhoC < 0.60, "rel_too_low",
    ifelse(rhoC < 0.70, "rel_exploratory",
    ifelse(rhoC <= 0.95, "rel_ok", "rel_too_high"))))
}

.pl_band_htmt <- function(h) {
    ifelse(is.na(h), NA_character_,
    ifelse(h < 0.85, "htmt_ok", ifelse(h < 0.90, "htmt_similar", "htmt_doubt")))
}

.pl_band_vif <- function(v) {
    ifelse(is.na(v), NA_character_,
    ifelse(v < 3, "vif_ok", ifelse(v < 5, "vif_possible", "vif_critical")))
}
