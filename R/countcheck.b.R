# -----------------------------------------------------------------------------
# AssumptionsLab
# A Jamovi module for statistical assumptions assessment and methodological
# decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of AssumptionsLab.
#
# AssumptionsLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# AssumptionsLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with AssumptionsLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Count and Rate Regression (Poisson / Negative Binomial).
# ES: Regresión de recuentos y tasas (Poisson / binomial negativa).
#
# This file implements countCheck: an assumption-diagnostic module for
# regression models of a count outcome, Poisson and negative binomial
# (MASS::glm.nb), with an optional exposure offset that turns the model
# into a model of rates. It reports the diagnostics that decide whether the
# model's rate ratios and their standard errors can be trusted:
# overdispersion, excess zeros, residuals, linearity on the log scale,
# multicollinearity and influential cases.
#
# ES: Este archivo implementa countCheck: un módulo de diagnóstico de
# supuestos para modelos de regresión con un desenlace de recuento, Poisson
# y binomial negativa (MASS::glm.nb), con un offset de exposición opcional
# que convierte el modelo en un modelo de tasas. Informa los diagnósticos
# que deciden si se puede confiar en las razones de tasas del modelo y en
# sus errores estándar: sobredispersión, exceso de ceros, residuos,
# linealidad en la escala logarítmica, multicolinealidad y casos
# influyentes.
#
# Responsibilities
# 1. Validate the count outcome and the exposure, and fit both the Poisson
#    and the negative binomial model (count-helpers.R).
# 2. Report the overdispersion tests and the Poisson-versus-negative-
#    binomial comparison, which always concern both models.
# 3. Diagnose the model the user chose: zeros, residuals, linearity,
#    multicollinearity, influence and incidence rate ratios.
# 4. Render the diagnostic plots and the applied interpretation of every
#    area, in jamovi's language.
#
# ES: Responsabilidades
# 1. Validar el desenlace de recuento y la exposición, y ajustar tanto el
#    modelo de Poisson como el binomial negativo (count-helpers.R).
# 2. Informar las pruebas de sobredispersión y la comparación Poisson
#    frente a binomial negativa, que siempre conciernen a ambos modelos.
# 3. Diagnosticar el modelo elegido por el usuario: ceros, residuos,
#    linealidad, multicolinealidad, influencia y razones de tasas de
#    incidencia.
# 4. Renderizar los gráficos diagnósticos y la interpretación aplicada de
#    cada área, en el idioma de jamovi.
#
# Workflow
# 1. Validate: outcome, predictors and exposure (Category A rejections).
# 2. Fit: Poisson and negative binomial on the same complete cases.
# 3. Diagnose: dispersion, zeros, residuals, linearity, collinearity,
#    influence.
# 4. Interpret: rate ratios, notes and recommendation.
# 5. Plot: hand each image only the state its render function reads.
#
# ES: Flujo de trabajo
# 1. Validar: desenlace, predictores y exposición (rechazos de Categoría A).
# 2. Ajustar: Poisson y binomial negativa sobre los mismos casos completos.
# 3. Diagnosticar: dispersión, ceros, residuos, linealidad, colinealidad,
#    influencia.
# 4. Interpretar: razones de tasas, notas y recomendación.
# 5. Graficar: entregar a cada imagen solo el estado que lee su función de
#    render.
# -----------------------------------------------------------------------------

countCheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "countCheckClass",
    inherit = countCheckBase,
    private = list(

        .run = function() {

            # -----------------------------------------------------------------------------
            # Initial setup and shared helpers (AssumptionsLab standard).
            # ES: Configuración inicial y helpers compartidos (estándar AssumptionsLab).
            # -----------------------------------------------------------------------------
            lang <- .al_lang(self$options)
            . <- lang$tr

            txt <- function(key) .al_text(lang, "count", key)
            html_block <- .al_html_block
            html_guide <- function(title, key) {
                html_block(title, .al_html_list(txt(key)), raw = TRUE)
            }
            fmt_num <- function(x, digits = 3) {
                x <- .al_clean_num(x)
                if (is.na(x))
                    return(.("Not computed"))
                format(round(x, digits), nsmall = digits)
            }
            add_row <- .al_add_row
            qname <- .al_qname

            dep_var <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors
            exposure_var <- self$options$exposure
            exposure_log <- identical(self$options$exposureScale, "log")
            family <- self$options$family
            if (is.null(exposure_var) || !nzchar(exposure_var))
                exposure_var <- NULL

            # -----------------------------------------------------------------------------
            # Validate the input (Category A: each problem blocks the analysis).
            # ES: Validar la entrada (Categoría A: cada problema bloquea el análisis).
            # -----------------------------------------------------------------------------
            data <- self$data
            if (is.null(data) || nrow(data) == 0)
                jmvcore::reject(.("No data are available."))
            if (is.null(dep_var) || !(dep_var %in% names(data)))
                jmvcore::reject(.("Dependent variable not selected."))

            predictors <- c(covs, factors)
            used <- unique(c(dep_var, predictors, exposure_var))
            model_data <- data[, used, drop = FALSE]
            model_data <- model_data[stats::complete.cases(model_data), , drop = FALSE]

            n_total <- nrow(data)
            n_complete <- nrow(model_data)
            if (n_complete < 3)
                jmvcore::reject(sprintf(.("Only %s complete case(s) are available; a count model needs more cases than parameters."), n_complete))

            y <- model_data[[dep_var]]
            if (is.factor(y))
                y <- jmvcore::toNumeric(y)
            problem <- .al_count_outcome_problem(y)
            if (!is.null(problem)) {
                jmvcore::reject(switch(problem,
                    nonnumeric = .("The dependent variable must be numeric: a count of events per case."),
                    negative = .("The dependent variable has negative values, and a count cannot be negative. Check how the variable is coded."),
                    noninteger = .("The dependent variable has non-integer values, and Poisson and negative binomial models need whole-number counts. If it is a rate, enter the count as the dependent variable and the time or population at risk as the exposure."),
                    allzero = .("Every value of the dependent variable is 0, so there is nothing to model."),
                    .("The dependent variable takes a single value, so there is nothing to model.")
                ))
            }
            model_data$al_count_y_ <- round(y)

            offset_term <- NULL
            if (!is.null(exposure_var)) {
                expo <- model_data[[exposure_var]]
                if (!exposure_log && any(expo <= 0))
                    jmvcore::reject(sprintf(.("The exposure variable has zero or negative values in %s case(s), and its logarithm is not defined. Correct those values, or choose 'Already on the log scale' if the variable is already a logarithm."), sum(expo <= 0)))
                model_data[[.al_count_offset_col]] <- if (exposure_log) expo else log(expo)
                offset_term <- paste0("offset(", .al_count_offset_col, ")")
            }

            rhs_terms <- if (length(predictors) > 0) qname(predictors) else character(0)
            rhs <- paste(c(if (length(rhs_terms) > 0) rhs_terms else "1", offset_term), collapse = " + ")
            formula <- stats::as.formula(paste("al_count_y_ ~", rhs))

            # -----------------------------------------------------------------------------
            # Fit both models.
            # ES: Ajustar ambos modelos.
            # -----------------------------------------------------------------------------
            fits <- .al_count_fit(formula, model_data)
            if (is.null(fits$pois))
                jmvcore::reject(sprintf(.("Error fitting the Poisson model: %s"), fits$pois_error))
            if (identical(family, "negbin") && is.null(fits$nb))
                jmvcore::reject(sprintf(.("The negative binomial model could not be fitted (%s). Choose the Poisson model to see the overdispersion checks."), fits$nb_error))

            fit <- if (identical(family, "negbin")) fits$nb else fits$pois
            theta <- if (identical(family, "negbin")) fits$nb$theta else NULL
            model_label <- if (identical(family, "negbin")) .("Negative binomial") else .("Poisson")

            # -----------------------------------------------------------------------------
            # Introduction and design.
            # ES: Introducción y diseño.
            # -----------------------------------------------------------------------------
            n_zeros <- sum(y == 0)
            self$results$intro$setContent(sprintf(.("<div style=\"max-width: 7.25in; width: 100%%; box-sizing: border-box;\"><p style=\"font-weight: 700; margin: 0 0 0.10em 0; line-height: 1.25;\">AssumptionsLab</p><p style=\"margin: 0 0 0.35em 0; line-height: 1.25;\">Assumption check for count and rate regression</p><p style=\"margin: 0 0 0.25em 0;\"> </p><p style=\"margin: 0 0 0.55em 0; line-height: 1.35;\">Use this analysis when you want to review whether a Poisson or negative binomial regression model has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.</p><p style=\"margin: 0 0 0.25em 0;\"> </p>%s</div>"), html_block(NULL, c(
                sprintf(.("<b>Model diagnosed:</b> %s | <b>Exposure (offset):</b> %s"), model_label, if (is.null(exposure_var)) .("none") else .al_html_escape(exposure_var)),
                sprintf(.("<b>Complete cases:</b> %s | <b>Mean count:</b> %s | <b>Variance:</b> %s | <b>Zeros:</b> %s"), n_complete, fmt_num(mean(y), 2), fmt_num(stats::var(y), 2), n_zeros)
            ), paragraphs = TRUE, escape = FALSE)))

            self$results$designGuide$setContent(html_guide(.("Model design"), "designGuide"))
            design_rows <- list(
                list(.("Total cases"), as.character(n_total)),
                list(.("Complete cases"), as.character(n_complete)),
                list(.("Mean count"), fmt_num(mean(y), 3)),
                list(.("Variance of the count"), fmt_num(stats::var(y), 3)),
                list(.("Variance / mean (raw data)"), fmt_num(stats::var(y) / mean(y), 2)),
                list(.("Zeros"), sprintf("%s (%s%%)", n_zeros, fmt_num(100 * n_zeros / n_complete, 1))),
                list(.("Maximum count"), as.character(max(y))),
                list(.("Exposure (offset)"), if (is.null(exposure_var)) .("None") else if (exposure_log) sprintf(.("%s (already on the log scale)"), exposure_var) else sprintf(.("log(%s)"), exposure_var)),
                list(.("Model diagnosed"), model_label)
            )
            for (i in seq_along(design_rows))
                add_row(self$results$design, paste0("design_", i), list(element = design_rows[[i]][[1]], value = design_rows[[i]][[2]]))

            disp <- private$.checkDispersion(., html_block, html_guide, fmt_num, fits, family)
            zeros <- private$.checkZeros(., html_block, html_guide, fmt_num, fits, family)
            resid <- private$.checkResiduals(., html_block, html_guide, fmt_num, fit, theta, family, disp)
            lin <- private$.checkLinearity(., html_block, html_guide, fmt_num, fit, covs, rhs_terms, offset_term, model_data, family, disp)
            max_gvif <- private$.checkMulticollinearity(., html_block, html_guide, fmt_num, fit, predictors)
            infl <- private$.checkInfluence(., html_block, html_guide, fmt_num, fit, n_complete)
            private$.fillRateRatios(., html_block, html_guide, fmt_num, fit, family, disp, exposure_var)
            private$.fillNotes(., html_block, fmt_num, family, disp, zeros, lin, max_gvif, infl, n_complete)
            private$.setPlotStates(fits, fit, theta, resid, lin, infl)
        },

        .checkDispersion = function(., html_block, html_guide, fmt_num, fits, family) {
            # -----------------------------------------------------------------------------
            # Overdispersion and the Poisson-versus-negative-binomial comparison.
            # ES: Sobredispersión y comparación Poisson frente a binomial negativa.
            # -----------------------------------------------------------------------------
            self$results$dispersionGuide$setContent(html_guide(.("Overdispersion"), "dispersionGuide"))

            d <- .al_count_dispersion(fits$pois)
            lr <- .al_count_lr_test(fits$pois, fits$nb)
            table <- self$results$dispersion
            nb_reason <- if (is.null(fits$nb)) sprintf(.("Not computable: the negative binomial model could not be fitted (%s)."), fits$nb_error) else NULL

            table$addRow(rowKey = "pearson", values = list(test = .("Pearson χ² / df (Poisson)"), statistic = d$pearson, df = d$df, ratio = d$pearson_ratio, p = d$pearson_p, pSig = .al_p_sig(d$pearson_p)))
            table$addRow(rowKey = "deviance", values = list(test = .("Deviance / df (Poisson)"), statistic = d$deviance, df = d$df, ratio = d$deviance_ratio, p = d$deviance_p, pSig = .al_p_sig(d$deviance_p)))
            table$addRow(rowKey = "dl", values = list(test = .("Dean-Lawless score test (z)"), statistic = d$dl_z, df = NA_real_, ratio = NA_real_, p = d$dl_p, pSig = .al_p_sig(d$dl_p)))
            table$addRow(rowKey = "ct", values = list(test = .("Cameron-Trivedi auxiliary regression (z)"), statistic = d$ct_z, df = NA_real_, ratio = NA_real_, p = d$ct_p, pSig = .al_p_sig(d$ct_p)))
            if (is.na(d$ct_z))
                table$addFootnote(rowKey = "ct", col = "statistic", note = .("Not computable: the auxiliary regression has no residual variation (too few cases or identical fitted values)."))
            table$addRow(rowKey = "lr", values = list(test = .("Likelihood ratio, Poisson vs negative binomial (χ²)"), statistic = lr$lr, df = 1, ratio = NA_real_, p = lr$p, pSig = .al_p_sig(lr$p)))
            if (!is.null(nb_reason))
                table$addFootnote(rowKey = "lr", col = "statistic", note = nb_reason)
            else
                table$addFootnote(rowKey = "lr", col = "p", note = .("Half the χ²(1) tail, because the Poisson model lies on the boundary of the negative binomial (α = 0)."))

            d_nb <- NULL
            if (identical(family, "negbin")) {
                pearson_nb <- sum(stats::residuals(fits$nb, type = "pearson")^2)
                df_nb <- fits$nb$df.residual
                d_nb <- list(ratio = pearson_nb / df_nb, p = stats::pchisq(pearson_nb, df_nb, lower.tail = FALSE))
                table$addRow(rowKey = "pearsonNb", values = list(test = .("Pearson χ² / df (negative binomial)"), statistic = pearson_nb, df = df_nb, ratio = d_nb$ratio, p = d_nb$p, pSig = .al_p_sig(d_nb$p)))
            }
            table$setNote("onesided", .("The Dean-Lawless and Cameron-Trivedi tests are one-sided (H1: overdispersion of the negative binomial form, var = μ + α·μ²)."))

            comp <- self$results$modelComparison
            self$results$modelComparisonGuide$setContent(html_guide(.("Poisson vs negative binomial"), "modelComparisonGuide"))
            comp$addRow(rowKey = "pois", values = list(model = .("Poisson"), logLik = as.numeric(stats::logLik(fits$pois)), k = length(stats::coef(fits$pois)), aic = stats::AIC(fits$pois), bic = stats::BIC(fits$pois), theta = NA_real_, alpha = 0))
            if (!is.null(fits$nb)) {
                comp$addRow(rowKey = "nb", values = list(model = .("Negative binomial"), logLik = as.numeric(stats::logLik(fits$nb)), k = length(stats::coef(fits$nb)) + 1L, aic = stats::AIC(fits$nb), bic = stats::BIC(fits$nb), theta = fits$nb$theta, alpha = 1 / fits$nb$theta))
                if (length(fits$nb_warnings) > 0)
                    comp$addFootnote(rowKey = "nb", col = "theta", note = sprintf(.("glm.nb() reported a convergence warning (%s). This usually means θ is growing without bound because the data show little or no overdispersion."), paste(fits$nb_warnings, collapse = "; ")))
            } else {
                comp$addRow(rowKey = "nb", values = list(model = .("Negative binomial"), logLik = NA_real_, k = NA_integer_, aic = NA_real_, bic = NA_real_, theta = NA_real_, alpha = NA_real_))
                comp$addFootnote(rowKey = "nb", col = "logLik", note = nb_reason)
            }

            over <- isTRUE(d$pearson_ratio > 1) && (isTRUE(lr$p < .05) || isTRUE(d$dl_p < .05) || isTRUE(d$ct_p < .05))
            under <- isTRUE(d$df > 0) && isTRUE(stats::pchisq(d$pearson, d$df) < .05)

            interp <- c(
                sprintf(.("Pearson dispersion ratio under the Poisson model: %s (Cameron & Trivedi, 2013; see references). Under the Poisson model it should be close to 1."), fmt_num(d$pearson_ratio, 2)),
                if (over) {
                    sprintf(.("The tests indicate overdispersion: the counts vary more than the Poisson model allows (Dean-Lawless p %s; likelihood ratio p %s). With a Poisson model, standard errors would be about %s times too small."), private$.pText(d$dl_p, fmt_num), private$.pText(lr$p, fmt_num), fmt_num(sqrt(max(d$pearson_ratio, 1)), 2))
                } else if (under) {
                    .("The Pearson ratio is clearly below 1, which suggests underdispersion: the counts vary less than the Poisson model expects. Poisson standard errors are then conservative (too wide); the negative binomial model does not help, because it only allows more variance, not less.")
                } else {
                    .("There is no clear evidence of overdispersion: the variance of the counts is compatible with the Poisson model.")
                },
                if (!is.null(fits$nb))
                    sprintf(.("The negative binomial model estimates θ = %s (α = 1/θ = %s); AIC is %s for Poisson and %s for the negative binomial."), fmt_num(fits$nb$theta, 3), fmt_num(1 / fits$nb$theta, 3), fmt_num(stats::AIC(fits$pois), 1), fmt_num(stats::AIC(fits$nb), 1)),
                if (!is.null(d_nb))
                    sprintf(.("Pearson ratio of the negative binomial model: %s. A value close to 1 means its variance function, μ + μ²/θ, describes the data; a value well above 1 means overdispersion remains even after allowing for it."), fmt_num(d_nb$ratio, 2)),
                .("Common error: judging overdispersion from the raw variance and mean of the outcome. What matters is the variance conditional on the predictors, which is what the Pearson ratio and the tests measure; predictors can explain part of the raw extra variance."),
                sprintf(.("Sample-size caveat: with %s cases, the tests can flag a trivial excess of variance in large samples and miss a relevant one in small samples; read the ratio and the mean-variance plot, not only the p-values."), length(fits$pois$y))
            )
            self$results$dispersionInterpretation$setContent(html_block(.("Applied Interpretation"), interp, paragraphs = TRUE))

            list(d = d, lr = lr, over = over, under = under, d_nb = d_nb)
        },

        .pText = function(p, fmt_num) {
            if (is.na(.al_clean_num(p)))
                return("= NA")
            if (p < .001) "< .001" else paste0("= ", fmt_num(p, 3))
        },

        .checkZeros = function(., html_block, html_guide, fmt_num, fits, family) {
            # -----------------------------------------------------------------------------
            # Excess zeros.
            # ES: Exceso de ceros.
            # -----------------------------------------------------------------------------
            self$results$zerosGuide$setContent(html_guide(.("Excess zeros"), "zerosGuide"))

            z <- .al_count_zeros(fits$pois, fits$nb)
            table <- self$results$zeros
            ratio_pois <- if (z$expected_pois > 0) z$observed / z$expected_pois else NA_real_
            ratio_nb <- if (isTRUE(z$expected_nb > 0)) z$observed / z$expected_nb else NA_real_

            table$addRow(rowKey = "pois", values = list(model = .("Poisson"), observed = z$observed, expected = z$expected_pois, ratio = ratio_pois, statistic = z$score, p = z$p, pSig = .al_p_sig(z$p)))
            if (is.na(z$score))
                table$addFootnote(rowKey = "pois", col = "statistic", note = .("Not computable: the score test's variance is not positive for these fitted values (usually very few zeros are expected and observed)."))
            table$addRow(rowKey = "nb", values = list(model = .("Negative binomial"), observed = z$observed, expected = z$expected_nb, ratio = ratio_nb, statistic = NA_real_, p = NA_real_, pSig = ""))
            table$addFootnote(rowKey = "nb", col = "statistic", note = .("van den Broek's score test is defined against the Poisson model only; for the negative binomial, compare observed and expected zeros."))
            table$setNote("score", .("Score test: van den Broek (1995), zero-inflated Poisson vs Poisson, χ²(1)."))

            excess_pois <- isTRUE(z$p < .05) && isTRUE(ratio_pois > 1)
            excess_nb <- isTRUE(ratio_nb > 1.15) && isTRUE(z$observed - z$expected_nb >= 5)

            interp <- c(
                sprintf(.("Observed zeros: %s. The Poisson model expects %s and the negative binomial model expects %s."), z$observed, fmt_num(z$expected_pois, 1), fmt_num(z$expected_nb, 1)),
                if (excess_pois && !excess_nb && !is.na(ratio_nb)) {
                    .("The Poisson model under-predicts the zeros, but the negative binomial model reproduces them: the extra zeros are explained by overdispersion, and a zero-inflated model is usually unnecessary.")
                } else if (excess_nb) {
                    sprintf(.("Even the negative binomial model under-predicts the zeros (observed / expected = %s). Consider a zero-inflated or hurdle model, and justify it substantively: is there a group of cases that cannot experience the event at all?"), fmt_num(ratio_nb, 2))
                } else if (excess_pois) {
                    .("The Poisson model under-predicts the zeros. The negative binomial model could not be fitted, so it is not possible to tell whether the extra zeros come from overdispersion or from a separate process.")
                } else {
                    .("The number of zeros is compatible with the model; there is no sign of zero inflation.")
                },
                .("The comparison of the negative binomial's expected zeros uses a heuristic (observed at least 15% above expected and at least 5 zeros more), not a formal test."),
                .("Common error: choosing a zero-inflated model only because the outcome has many zeros. Many zeros are expected when the mean is small; what matters is whether there are more zeros than the model predicts.")
            )
            self$results$zerosInterpretation$setContent(html_block(.("Applied Interpretation"), interp, paragraphs = TRUE))

            list(z = z, excess_pois = excess_pois, excess_nb = excess_nb, ratio_nb = ratio_nb)
        },

        .checkResiduals = function(., html_block, html_guide, fmt_num, fit, theta, family, disp) {
            # -----------------------------------------------------------------------------
            # Pearson, deviance and randomized quantile residuals.
            # ES: Residuos de Pearson, de devianza y cuantílicos aleatorizados.
            # -----------------------------------------------------------------------------
            self$results$residualsGuide$setContent(html_guide(.("Residuals"), "residualsGuide"))

            r_pearson <- as.numeric(stats::residuals(fit, type = "pearson"))
            r_deviance <- as.numeric(stats::residuals(fit, type = "deviance"))
            r_quantile <- .al_count_quantile_residuals(fit$y, stats::fitted(fit), theta)

            summarise <- function(r) list(
                mean = mean(r), sd = stats::sd(r), min = min(r), max = max(r),
                over2 = 100 * mean(abs(r) > 2), over3 = 100 * mean(abs(r) > 3)
            )
            rows <- list(
                pearson = c(list(type = .("Pearson")), summarise(r_pearson)),
                deviance = c(list(type = .("Deviance")), summarise(r_deviance)),
                quantile = c(list(type = .("Randomized quantile")), summarise(r_quantile))
            )
            for (key in names(rows))
                self$results$residuals$addRow(rowKey = key, values = rows[[key]])
            self$results$residuals$setNote("quantile", .("Randomized quantile residuals use a fixed seed (20260704), so the same data always give the same values. Under a correct model they are standard normal: mean 0, SD 1, about 4.6% beyond ±2 and 0.3% beyond ±3."))

            sw <- NULL
            if (length(r_quantile) >= 3 && length(r_quantile) <= 5000)
                sw <- tryCatch(stats::shapiro.test(r_quantile), error = function(e) NULL)

            q <- rows$quantile
            interp <- c(
                sprintf(.("Randomized quantile residuals (Dunn & Smyth, 1996; see references): SD = %s and %s%% of cases beyond ±2, against 1 and about 4.6%% expected under a correct model."), fmt_num(q$sd, 2), fmt_num(q$over2, 1)),
                if (!is.null(sw)) {
                    sprintf(.("Shapiro-Wilk test on these residuals: p %s. %s"), private$.pText(sw$p.value, fmt_num), if (sw$p.value < .05) .("They depart from the standard normal distribution, which points to a misspecified distribution (for example, overdispersion or excess zeros) or a misspecified mean.") else .("Compatible with the distribution the model assumes."))
                } else {
                    .("The Shapiro-Wilk test is not computed on these residuals (it needs between 3 and 5000 cases); use the Q-Q plot.")
                },
                if (identical(family, "poisson") && isTRUE(disp$over))
                    .("With overdispersed data, Poisson residuals are too spread out: a SD clearly above 1 and many cases beyond ±2 are the residual-side view of the overdispersion reported above."),
                .("Common error: judging a count model with the Q-Q plot of its Pearson or deviance residuals. Those residuals are not normal for counts even when the model is right; use the quantile residuals for that purpose, and the Pearson and deviance residuals to look for patterns against the fitted values.")
            )
            self$results$residualsInterpretation$setContent(html_block(.("Applied Interpretation"), interp, paragraphs = TRUE))

            list(pearson = r_pearson, deviance = r_deviance, quantile = r_quantile, eta = as.numeric(stats::predict(fit, type = "link")))
        },

        .checkLinearity = function(., html_block, html_guide, fmt_num, fit, covs, rhs_terms, offset_term, model_data, family, disp) {
            # -----------------------------------------------------------------------------
            # Linearity on the log scale (spline LR test and partial residuals).
            # ES: Linealidad en la escala logarítmica (prueba LR con spline y
            # residuos parciales).
            # -----------------------------------------------------------------------------
            self$results$linearityGuide$setContent(html_guide(.("Linearity on the log scale"), "linearityGuide"))
            table <- self$results$linearity

            n_tested <- 0
            n_significant <- 0
            flagged <- character(0)
            partial <- list()
            coefs <- stats::coef(fit)
            work_resid <- stats::residuals(fit, type = "working")

            if (length(covs) == 0) {
                table$addRow(rowKey = "lin_none", values = list(predictor = .("Not applicable"), statistic = NA_real_, df = NA_integer_, p = NA_real_, pSig = ""))
            }

            for (predictor in covs) {
                term <- .al_qname(predictor)
                x <- model_data[[predictor]]
                res <- .al_count_spline_test(fit, rhs_terms, term, x, model_data, family, offset_term)
                table$addRow(rowKey = paste0("lin_", predictor), values = list(predictor = predictor, statistic = res$lr, df = 2L, p = res$p, pSig = .al_p_sig(res$p)))
                if (!is.null(res$reason)) {
                    table$addFootnote(rowKey = paste0("lin_", predictor), col = "statistic", note = if (identical(res$reason, "fewvalues")) .("Not tested: the predictor has fewer than 5 distinct values, too few for a 3-df spline.") else .("Not computable: the model with the spline term could not be fitted."))
                } else {
                    n_tested <- n_tested + 1
                    if (res$p < .05) {
                        n_significant <- n_significant + 1
                        flagged <- c(flagged, predictor)
                    }
                }

                b <- coefs[term]
                if (is.na(b))
                    b <- coefs[predictor]
                if (!is.na(b))
                    partial[[predictor]] <- data.frame(predictor = predictor, x = x, partial = as.numeric(work_resid + b * x))
            }

            interp <- if (length(covs) == 0) {
                .("No numeric predictors were included, so the linearity check on the log scale does not apply.")
            } else if (n_tested == 0) {
                .("The spline test could not be computed for any numeric predictor; use the partial residual plot.")
            } else {
                c(
                    sprintf(.("%s of %s numeric predictor(s) showed a significant departure from linearity on the log scale (spline likelihood ratio test; Harrell, 2015; see references)%s"), n_significant, n_tested, if (length(flagged) > 0) sprintf(.(": %s."), paste(flagged, collapse = ", ")) else "."),
                    if (identical(family, "poisson") && isTRUE(disp$over))
                        .("With overdispersed data, the Poisson spline test rejects linearity too often, because its likelihood ratio assumes the Poisson variance; check linearity again with the negative binomial model before acting on a significant result."),
                    .("Common error: checking linearity on the raw count scale. The model is linear in log(μ), so a curved scatterplot of the count against the predictor is expected even when the assumption holds; the partial residual plot is the right view."),
                    .("If non-linearity is confirmed, keep the predictor as a spline or a polynomial term, or transform it (for example, its logarithm), and report the choice.")
                )
            }
            self$results$linearityInterpretation$setContent(html_block(.("Applied Interpretation"), interp, paragraphs = TRUE))

            list(n_significant = n_significant, flagged = flagged, partial = if (length(partial) > 0) do.call(rbind, partial) else NULL)
        },

        .checkMulticollinearity = function(., html_block, html_guide, fmt_num, fit, predictors) {
            # -----------------------------------------------------------------------------
            # Multicollinearity (generalized VIF, car::vif).
            # ES: Multicolinealidad (VIF generalizado, car::vif).
            # -----------------------------------------------------------------------------
            self$results$multicollinearityGuide$setContent(html_guide(.("Multicollinearity"), "multicollinearityGuide"))
            table <- self$results$multicollinearity

            if (length(predictors) < 2) {
                table$addRow(rowKey = "mc_none", values = list(predictor = .("Not applicable"), gvif = NA_real_, df = NA_integer_, gvifAdj = NA_real_))
                self$results$multicollinearityInterpretation$setContent(html_block(.("Applied Interpretation"), .("Multicollinearity needs at least two predictors, so it does not apply to this model."), paragraphs = TRUE))
                return(NA_real_)
            }

            v <- tryCatch(suppressWarnings(car::vif(fit)), error = function(e) e)
            if (inherits(v, "error")) {
                table$addRow(rowKey = "mc_fail", values = list(predictor = .("All predictors"), gvif = NA_real_, df = NA_integer_, gvifAdj = NA_real_))
                table$addFootnote(rowKey = "mc_fail", col = "gvif", note = sprintf(.("Not computable: %s"), conditionMessage(v)))
                self$results$multicollinearityInterpretation$setContent(html_block(.("Applied Interpretation"), .("The variance inflation factors could not be computed; this usually means an exact linear dependency among predictors (an aliased coefficient)."), paragraphs = TRUE))
                return(NA_real_)
            }
            if (is.null(dim(v)))
                v <- cbind(GVIF = v, Df = 1, `GVIF^(1/(2*Df))` = sqrt(v))

            for (i in seq_len(nrow(v))) {
                table$addRow(rowKey = paste0("mc_", i), values = list(predictor = .al_strip_qname(rownames(v)[i]), gvif = v[i, 1], df = as.integer(v[i, 2]), gvifAdj = v[i, 3]))
            }
            table$setNote("scale", .("GVIF^(1/(2·df)) above √5 ≈ 2.24 (VIF above 5) is a common warning threshold; above √10 ≈ 3.16, a strong one."))

            max_adj <- max(v[, 3], na.rm = TRUE)
            worst <- .al_strip_qname(rownames(v)[which.max(v[, 3])])
            self$results$multicollinearityInterpretation$setContent(html_block(.("Applied Interpretation"), c(
                sprintf(.("Largest adjusted GVIF: %s (%s), equivalent to a VIF of %s (Fox & Monette, 1992; see references)."), fmt_num(max_adj, 2), worst, fmt_num(max_adj^2, 2)),
                if (max_adj^2 >= 5) .("There is relevant collinearity: the rate ratios of the predictors involved are unstable and their intervals wide. Consider combining or removing redundant predictors.") else .("Collinearity is low; it does not compromise the interpretation of individual rate ratios.")
            ), paragraphs = TRUE))

            max_adj
        },

        .checkInfluence = function(., html_block, html_guide, fmt_num, fit, n_complete) {
            # -----------------------------------------------------------------------------
            # Influential cases.
            # ES: Casos influyentes.
            # -----------------------------------------------------------------------------
            self$results$influenceGuide$setContent(html_guide(.("Influential cases"), "influenceGuide"))

            cooks_d <- as.numeric(stats::cooks.distance(fit))
            leverage <- as.numeric(stats::hatvalues(fit))
            std_resid <- as.numeric(suppressWarnings(stats::rstandard(fit)))
            n_params <- length(stats::coef(fit))
            cook_cut <- 4 / n_complete
            lev_cut <- 2 * n_params / n_complete

            flag <- (is.finite(cooks_d) & cooks_d > cook_cut) | (is.finite(leverage) & leverage > lev_cut)
            flagged <- which(flag)
            n_influential <- length(flagged)
            shown <- flagged[order(-cooks_d[flagged])][seq_len(min(30, n_influential))]

            table <- self$results$influence
            if (n_influential == 0) {
                table$addRow(rowKey = "none", values = list(case = NA_integer_, cooksD = NA_real_, leverage = NA_real_, stdResid = NA_real_))
                table$addFootnote(rowKey = "none", col = "case", note = .("No case exceeds the Cook's D or leverage cutoffs."))
            }
            for (i in shown)
                table$addRow(rowKey = paste0("case_", i), values = list(case = i, cooksD = .al_clean_num(cooks_d[i]), leverage = .al_clean_num(leverage[i]), stdResid = .al_clean_num(std_resid[i])))
            table$setNote("cutoffs", sprintf(.("Cases flagged by Cook's D > 4/n = %s or leverage > 2p/n = %s; at most 30 are listed, ordered by Cook's D. Case numbers refer to the complete cases."), fmt_num(cook_cut, 4), fmt_num(lev_cut, 4)))

            pct <- 100 * n_influential / n_complete
            max_idx <- which.max(cooks_d)
            self$results$influenceInterpretation$setContent(html_block(.("Applied Interpretation"), c(
                sprintf(.("%s of %s cases (%s%%) exceed the Cook's D or leverage cutoffs used here (Pregibon, 1981; see references)."), n_influential, n_complete, fmt_num(pct, 1)),
                if (length(max_idx) == 1) sprintf(.("The most influential case is case %s, with Cook's D = %s and an observed count of %s against a fitted %s."), max_idx, fmt_num(cooks_d[max_idx], 3), fit$y[max_idx], fmt_num(stats::fitted(fit)[max_idx], 2)),
                .("Common error: removing every flagged case automatically. With cutoffs this permissive some flags are expected even in well-behaved data; treat them as a shortlist to inspect, and if you refit without a case, report both models.")
            ), paragraphs = TRUE))

            list(cooks_d = cooks_d, leverage = leverage, n_params = n_params, n_influential = n_influential, pct = pct)
        },

        .fillRateRatios = function(., html_block, html_guide, fmt_num, fit, family, disp, exposure_var) {
            # -----------------------------------------------------------------------------
            # Incidence rate ratios.
            # ES: Razones de tasas de incidencia.
            # -----------------------------------------------------------------------------
            self$results$rateRatiosGuide$setContent(html_guide(.("Incidence rate ratios"), "rateRatiosGuide"))

            coefs <- stats::coef(fit)
            coef_table <- summary(fit)$coefficients
            ci <- tryCatch(suppressMessages(stats::confint.default(fit)), error = function(e) NULL)
            p_by_name <- stats::setNames(rep(NA_real_, length(coefs)), names(coefs))
            matched <- intersect(names(coefs), rownames(coef_table))
            p_by_name[matched] <- coef_table[matched, 4]

            for (i in seq_along(coefs)) {
                nm <- names(coefs)[i]
                self$results$rateRatios$addRow(rowKey = paste0("irr_", i), values = list(
                    predictor = .al_strip_qname(nm),
                    irr = unname(exp(coefs[i])),
                    ciLower = if (!is.null(ci) && nm %in% rownames(ci)) exp(ci[nm, 1]) else NA_real_,
                    ciUpper = if (!is.null(ci) && nm %in% rownames(ci)) exp(ci[nm, 2]) else NA_real_,
                    p = unname(p_by_name[i]),
                    pSig = .al_p_sig(p_by_name[i])
                ))
            }
            self$results$rateRatios$setNote("wald", .("Wald 95% confidence intervals. The intercept row is the baseline expected count (or rate, with an offset) when every predictor is 0 or at its reference category."))

            p_slopes <- p_by_name[names(p_by_name) != "(Intercept)"]
            interp <- c(
                if (length(p_slopes) > 0 && any(!is.na(p_slopes))) {
                    k <- names(p_slopes)[which.min(p_slopes)]
                    sprintf(.("Strongest association: %s, IRR = %s (p %s)."), .al_strip_qname(k), fmt_num(exp(coefs[k]), 2), private$.pText(p_slopes[k], fmt_num))
                } else {
                    .("The model has no predictors, so only the baseline count is estimated.")
                },
                if (!is.null(exposure_var))
                    sprintf(.("Because %s enters as an offset, each IRR compares rates per unit of exposure, not raw counts."), exposure_var),
                if (identical(family, "poisson") && isTRUE(disp$over))
                    .("Caution: the data are overdispersed and these intervals come from the Poisson model, so they are too narrow and the p-values too small. Prefer the negative binomial model before reporting them.")
                else
                    .("Read the confidence interval, not only the point estimate: if it includes 1, the effect is not statistically distinguishable from no association."),
                .("Common error: reading an IRR as a difference in counts. An IRR of 1.5 means a 50% higher rate, whatever the baseline; report the baseline rate too, so the reader can judge the absolute size of the effect.")
            )
            self$results$rateRatiosInterpretation$setContent(html_block(.("Applied Interpretation"), interp, paragraphs = TRUE))

            invisible(NULL)
        },

        .fillNotes = function(., html_block, fmt_num, family, disp, zeros, lin, max_gvif, infl, n_complete) {
            # -----------------------------------------------------------------------------
            # Notes and recommendation: every problem found, in workflow order.
            # ES: Notas y recomendación: cada problema encontrado, en el orden del
            # flujo de trabajo.
            # -----------------------------------------------------------------------------
            decisions <- character(0)
            if (identical(family, "poisson") && isTRUE(disp$over))
                decisions <- c(decisions, .("There is overdispersion: the Poisson standard errors are too small. Prefer the negative binomial model (choose it under 'Model to diagnose'), or at least use robust standard errors."))
            if (isTRUE(disp$under))
                decisions <- c(decisions, .("There is underdispersion: Poisson standard errors are conservative. Consider a generalized Poisson or Conway-Maxwell-Poisson model if precision matters."))
            if (identical(family, "negbin") && isTRUE(disp$lr$p >= .05))
                decisions <- c(decisions, .("The negative binomial model does not improve clearly on the Poisson model (likelihood ratio test not significant); the simpler Poisson model may be enough."))
            if (identical(family, "negbin") && isTRUE(disp$d_nb$ratio > 1.5))
                decisions <- c(decisions, .("Overdispersion remains in the negative binomial model; review omitted predictors, clustering of cases, or excess zeros."))
            if (isTRUE(zeros$excess_nb))
                decisions <- c(decisions, .("There are more zeros than the negative binomial model expects; consider a zero-inflated or hurdle model, with a substantive justification."))
            if (lin$n_significant > 0)
                decisions <- c(decisions, sprintf(.("At least one numeric predictor is not linear on the log scale (%s); consider a spline, a polynomial or a transformation."), paste(lin$flagged, collapse = ", ")))
            if (!is.na(max_gvif) && max_gvif^2 >= 5)
                decisions <- c(decisions, .("There is relevant multicollinearity; review redundant predictors before interpreting individual rate ratios."))
            if (infl$n_influential > 0 && infl$pct >= 10)
                decisions <- c(decisions, .("There are many outlying or influential cases; inspect them before making decisions about the model."))
            if (length(decisions) == 0)
                decisions <- .("No diagnostic points to a serious problem; the model can be interpreted, reporting the diagnostics.")

            self$results$notes$setContent(html_block(.("Notes and recommendation"), c(
                sprintf(.("Suggested decision: %s"), decisions[1]),
                if (length(decisions) > 1) decisions[-1],
                sprintf(.("Model fitted with %s cases."), n_complete)
            ), paragraphs = TRUE))

            invisible(NULL)
        },

        .setPlotStates = function(fits, fit, theta, resid, lin, infl) {
            # -----------------------------------------------------------------------------
            # State for the plots: each image gets only the fields its own render
            # function reads (jamovi review: repeated state inflates saved files).
            # ES: Estado para los gráficos: cada imagen recibe solo los campos que lee
            # su propia función de render (revisión de jamovi: repetir el estado
            # infla los archivos guardados).
            # -----------------------------------------------------------------------------
            y <- fits$pois$y
            mu_pois <- as.numeric(stats::fitted(fits$pois))

            # Mean and variance of the observed counts in groups of similar
            # fitted values (up to 10 quantile groups of at least 2 cases).
            # ES: Media y varianza de los recuentos observados en grupos de
            # valores ajustados parecidos (hasta 10 grupos de al menos 2 casos).
            mv <- NULL
            breaks <- unique(stats::quantile(mu_pois, probs = seq(0, 1, length.out = 11), names = FALSE))
            if (length(breaks) >= 3) {
                grp <- cut(mu_pois, breaks = breaks, include.lowest = TRUE)
                n_grp <- as.numeric(tapply(y, grp, length))
                mv <- data.frame(mean = as.numeric(tapply(y, grp, mean)), variance = as.numeric(tapply(y, grp, stats::var)), n = n_grp)
                mv <- mv[mv$n >= 2 & is.finite(mv$variance), , drop = FALSE]
            }
            self$results$meanVariancePlot$setState(list(mv = mv, theta = if (!is.null(fits$nb)) fits$nb$theta else NULL))

            self$results$frequencyPlot$setState(list(freq = .al_count_frequencies(
                y, mu_pois,
                if (!is.null(fits$nb)) as.numeric(stats::fitted(fits$nb)) else NULL,
                if (!is.null(fits$nb)) fits$nb$theta else NULL
            )))
            self$results$residualPlot$setState(list(eta = resid$eta, pearson = resid$pearson, deviance = resid$deviance))
            self$results$quantileResidualPlot$setState(list(quantile = resid$quantile))
            self$results$linearityPlot$setState(list(partial = lin$partial))
            self$results$influencePlot$setState(list(cooks_d = infl$cooks_d, leverage = infl$leverage, n = length(infl$cooks_d), p = infl$n_params))

            show <- isTRUE(self$options$showPlots)
            visibility <- c(
                meanVariancePlot = isTRUE(self$options$dispShowPlots),
                frequencyPlot = isTRUE(self$options$zeroShowPlots),
                residualPlot = isTRUE(self$options$residShowPlots),
                quantileResidualPlot = isTRUE(self$options$residShowPlots),
                linearityPlot = isTRUE(self$options$linShowPlots) && !is.null(lin$partial),
                influencePlot = isTRUE(self$options$influenceShowPlots)
            )
            for (item_name in names(visibility))
                self$results[[item_name]]$setVisible(show && isTRUE(visibility[[item_name]]))

            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # Plot helpers: colors come from jamovi's own theme (see logcheck.b.R for
        # the full rationale); reference lines stay a neutral gray.
        # ES: Auxiliares de gráficos: los colores vienen del propio tema de jamovi
        # (ver logcheck.b.R para el razonamiento completo); las líneas de
        # referencia quedan en un gris neutro.
        # -----------------------------------------------------------------------------
        .plotColors = function(theme) {
            base_color <- if (!is.null(theme$color) && length(theme$color) >= 1) theme$color[1] else "#333333"
            accent_color <- if (!is.null(theme$color) && length(theme$color) >= 2) theme$color[2] else "#2C7FB8"
            accent_fill <- if (!is.null(theme$fill) && length(theme$fill) >= 2) theme$fill[2] else "#A6CEE3"
            list(point = base_color, line = base_color, ref = "gray50", alert = accent_color, smooth = accent_color, fill = accent_fill)
        },

        .emptyPlot = function(message) {
            graphics::plot.new()
            graphics::par(mar = c(1, 1, 1, 1))
            graphics::text(x = 0.5, y = 0.55, labels = message, cex = 0.95)
            graphics::text(x = 0.5, y = 0.40, labels = "AssumptionsLab", cex = 0.80)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Mean-variance relationship (overdispersion).
        # ES: Relación media-varianza (sobredispersión).
        # -----------------------------------------------------------------------------
        .plotMeanVariance = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            st <- image$state
            if (is.null(st))
                return(FALSE)
            mv <- st$mv
            if (is.null(mv) || nrow(mv) < 2)
                return(private$.emptyPlot(.("Not enough variation in fitted values to group the cases.")))
            pal <- private$.plotColors(theme)

            grid <- seq(0, max(mv$mean) * 1.05, length.out = 100)
            curves <- data.frame(mean = grid, variance = grid, model = .("Poisson: var = μ"))
            if (!is.null(st$theta))
                curves <- rbind(curves, data.frame(mean = grid, variance = grid + grid^2 / st$theta, model = .("Negative binomial: var = μ + μ²/θ")))

            p <- ggplot2::ggplot() +
                ggplot2::geom_line(data = curves, ggplot2::aes(x = mean, y = variance, color = model, linetype = model), linewidth = 0.7) +
                ggplot2::geom_point(data = mv, ggplot2::aes(x = mean, y = variance), color = pal$point, size = 2.6) +
                ggplot2::labs(x = .("Mean count in the group"), y = .("Variance in the group"), color = NULL, linetype = NULL) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::margin(4, 6, 4, 6))
            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Observed vs expected frequency of each count (zeros).
        # ES: Frecuencia observada vs esperada de cada recuento (ceros).
        # -----------------------------------------------------------------------------
        .plotFrequencies = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            st <- image$state
            if (is.null(st) || is.null(st$freq))
                return(FALSE)
            pal <- private$.plotColors(theme)
            f <- st$freq

            expected <- data.frame(count = f$count, frequency = f$poisson, model = .("Poisson"))
            if (any(is.finite(f$negbin)))
                expected <- rbind(expected, data.frame(count = f$count, frequency = f$negbin, model = .("Negative binomial")))

            p <- ggplot2::ggplot() +
                ggplot2::geom_col(data = f, ggplot2::aes(x = count, y = observed), fill = pal$fill, color = pal$line, linewidth = 0.2, width = 0.8) +
                ggplot2::geom_line(data = expected, ggplot2::aes(x = count, y = frequency, color = model), linewidth = 0.7) +
                ggplot2::geom_point(data = expected, ggplot2::aes(x = count, y = frequency, color = model), size = 1.8) +
                ggplot2::labs(x = .("Count value"), y = .("Number of cases"), color = .("Expected by")) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::margin(4, 6, 4, 6))
            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Pearson and deviance residuals vs the linear predictor.
        # ES: Residuos de Pearson y de devianza vs el predictor lineal.
        # -----------------------------------------------------------------------------
        .plotResiduals = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            st <- image$state
            if (is.null(st))
                return(FALSE)
            pal <- private$.plotColors(theme)

            df <- rbind(
                data.frame(eta = st$eta, residual = st$pearson, type = .("Pearson")),
                data.frame(eta = st$eta, residual = st$deviance, type = .("Deviance"))
            )
            df$type <- factor(df$type, levels = c(.("Pearson"), .("Deviance")))

            p <- ggplot2::ggplot(df, ggplot2::aes(x = eta, y = residual)) +
                ggplot2::geom_hline(yintercept = 0, color = pal$ref, linewidth = 0.5) +
                ggplot2::geom_hline(yintercept = c(-2, 2), color = pal$ref, linetype = "dashed", linewidth = 0.4) +
                ggplot2::geom_point(color = pal$point, alpha = 0.6, size = 1.4)
            if (isTRUE(self$options$residSmoother) && length(unique(st$eta)) > 4)
                p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = pal$smooth, linewidth = 0.7)
            p <- p +
                ggplot2::facet_wrap(~type, scales = "free_y") +
                ggplot2::labs(x = .("Linear predictor (log of the fitted count)"), y = .("Residual")) +
                ggtheme +
                ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))
            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Q-Q plot of randomized quantile residuals.
        # ES: Gráfico Q-Q de los residuos cuantílicos aleatorizados.
        # -----------------------------------------------------------------------------
        .plotQuantileResiduals = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            st <- image$state
            if (is.null(st) || length(st$quantile) < 3)
                return(FALSE)
            pal <- private$.plotColors(theme)

            p <- ggplot2::ggplot(data.frame(r = st$quantile), ggplot2::aes(sample = r)) +
                ggplot2::geom_abline(intercept = 0, slope = 1, color = pal$ref, linetype = "dashed", linewidth = 0.5) +
                ggplot2::stat_qq(color = pal$point, size = 1.4, alpha = 0.7) +
                ggplot2::labs(x = .("Theoretical quantiles (standard normal)"), y = .("Randomized quantile residuals")) +
                ggtheme +
                ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))
            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Partial residuals vs each numeric predictor (linearity).
        # ES: Residuos parciales vs cada predictor numérico (linealidad).
        # -----------------------------------------------------------------------------
        .plotLinearity = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            st <- image$state
            if (is.null(st) || is.null(st$partial))
                return(FALSE)
            pal <- private$.plotColors(theme)

            p <- ggplot2::ggplot(st$partial, ggplot2::aes(x = x, y = partial)) +
                ggplot2::geom_point(color = pal$point, alpha = 0.5, size = 1.3) +
                ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = pal$ref, linetype = "dashed", linewidth = 0.6)
            if (identical(self$options$linSmoother, "loess"))
                p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = pal$smooth, linewidth = 0.7)
            p <- p +
                ggplot2::facet_wrap(~predictor, scales = "free") +
                ggplot2::labs(x = .("Predictor"), y = .("Partial residual (log scale)")) +
                ggtheme +
                ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))
            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Cook's D per case (influence).
        # ES: Cook's D por caso (influencia).
        # -----------------------------------------------------------------------------
        .plotInfluence = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            st <- image$state
            if (is.null(st))
                return(FALSE)
            pal <- private$.plotColors(theme)

            n <- st$n
            p_count <- if (is.null(st$p) || !is.finite(st$p) || st$p < 1) 1 else st$p
            cooks_threshold <- 4 / n
            leverage_threshold <- 2 * p_count / n
            df <- data.frame(case = seq_along(st$cooks_d), cooksD = st$cooks_d, leverage = st$leverage)
            df <- df[is.finite(df$cooksD), , drop = FALSE]
            df$flag <- df$cooksD > cooks_threshold

            p <- ggplot2::ggplot(df, ggplot2::aes(x = case, y = cooksD))
            if (isTRUE(self$options$influenceShowThreshold))
                p <- p + ggplot2::geom_hline(yintercept = cooks_threshold, linetype = "dashed", linewidth = 0.5, color = pal$ref)
            p <- p +
                ggplot2::geom_segment(ggplot2::aes(xend = case, y = 0, yend = cooksD), color = pal$point, linewidth = 0.3) +
                ggplot2::geom_point(ggplot2::aes(color = flag), size = 1.6, show.legend = FALSE)

            lab <- switch(self$options$influenceLabelMode,
                cooks = df[df$cooksD > cooks_threshold, , drop = FALSE],
                leverage = df[df$leverage > leverage_threshold, , drop = FALSE],
                top5 = df[order(-df$cooksD), , drop = FALSE][seq_len(min(5, nrow(df))), , drop = FALSE],
                NULL
            )
            if (!is.null(lab) && nrow(lab) > 0)
                p <- p + ggplot2::geom_text(data = lab, ggplot2::aes(x = case, y = cooksD, label = case), vjust = -0.6, size = 3, check_overlap = TRUE, color = pal$alert)

            p <- p +
                ggplot2::labs(x = .("Case"), y = "Cook's D") +
                ggtheme +
                ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6)) +
                # After ggtheme: the last scale added wins (CODE_STYLE §18).
                # ES: después de ggtheme: gana la última escala agregada.
                ggplot2::scale_color_manual(values = c(`FALSE` = pal$point, `TRUE` = pal$alert))
            print(p)
            TRUE
        }
    )
)
