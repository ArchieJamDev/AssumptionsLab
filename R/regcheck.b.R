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
# Simple & Multiple Regression.
# ES: Regresión Simple y Múltiple.
#
# This file implements regCheck: an assumption-diagnostic module for linear
# regression (one numeric dependent variable, one or more numeric and/or
# categorical predictors). It reports the assumption diagnostics that
# decide whether a linear model's coefficients are trustworthy (linearity,
# residual normality, homoscedasticity, error independence,
# multicollinearity, and outlying/influential cases), plus a
# Pearson/dCor/copula-entropy dependence-structure analysis and exploratory
# outcome transformations.
#
# ES: Este archivo implementa regCheck: un módulo de diagnóstico de
# supuestos para regresión lineal (una variable dependiente numérica, uno
# o más predictores numéricos y/o categóricos). Reporta los diagnósticos
# de supuestos que deciden si los coeficientes de un modelo lineal son
# confiables (linealidad, normalidad de residuos, homoscedasticidad,
# independencia de errores, multicolinealidad, y casos atípicos/
# influyentes), además de un análisis de estructura de dependencia
# Pearson/dCor/entropía copular y transformaciones exploratorias de la
# variable dependiente.
#
# Responsibilities
# 1. Fit the linear regression model from the user's selected dependent
#    variable and predictors.
# 2. Compute and report the full assumption-diagnostic battery (linearity
#    per predictor and for the model as a whole via RESET, residual
#    normality, homoscedasticity, error independence, multicollinearity,
#    outlying/influential cases).
# 3. Compute the Pearson/dCor/copula-entropy correlation structure among
#    numeric variables and flag pairs with a notable linear/non-linear
#    discordance.
# 4. Render the diagnostic plots, grouped by methodological area
#    (linearity, normality, homoscedasticity, correlations, influence)
#    and parametrized per the user's per-area options.
# 5. Explore log/sqrt/inverse transformations of the dependent variable as
#    a quick sensitivity check, and assemble the applied-interpretation
#    text for every diagnostic area in the user's selected report
#    language.
#
# ES: Responsabilidades
# 1. Ajustar el modelo de regresión lineal a partir de la variable
#    dependiente y predictores seleccionados por el usuario.
# 2. Calcular y reportar la batería completa de diagnósticos de supuestos
#    (linealidad por predictor y del modelo completo vía RESET,
#    normalidad de residuos, homoscedasticidad, independencia de errores,
#    multicolinealidad, casos atípicos/influyentes).
# 3. Calcular la estructura de correlación Pearson/dCor/entropía copular
#    entre variables numéricas y señalar pares con discordancia notable
#    lineal/no lineal.
# 4. Renderizar los gráficos diagnósticos, agrupados por área
#    metodológica (linealidad, normalidad, homoscedasticidad,
#    correlaciones, influencia) y parametrizados según las opciones por
#    área del usuario.
# 5. Explorar transformaciones log/sqrt/inversa de la variable dependiente
#    como verificación de sensibilidad rápida, y ensamblar el texto de
#    interpretación aplicada para cada área diagnóstica en el idioma de
#    informe seleccionado por el usuario.
#
# Workflow
# 1. Fit: build and estimate the linear regression model from the user's
#    variable selection.
# 2. Diagnose: compute linearity, residual normality, homoscedasticity,
#    error independence, multicollinearity, and outlying/influential
#    cases.
# 3. Correlate: compute the Pearson/dCor/copula-entropy structure and flag
#    discordant pairs.
# 4. Plot: render the diagnostic plots belonging to each area, per the
#    user's selected options.
# 5. Transform: explore log/sqrt/inverse transformations as a sensitivity
#    check.
# 6. Interpret: build the applied-interpretation text for every
#    diagnostic area.
#
# ES: Flujo de trabajo
# 1. Ajustar: construir y estimar el modelo de regresión lineal a partir
#    de la selección de variables del usuario.
# 2. Diagnosticar: calcular linealidad, normalidad de residuos,
#    homoscedasticidad, independencia de errores, multicolinealidad y
#    casos atípicos/influyentes.
# 3. Correlacionar: calcular la estructura Pearson/dCor/entropía copular y
#    señalar pares discordantes.
# 4. Graficar: renderizar los gráficos diagnósticos de cada área, según
#    las opciones seleccionadas por el usuario.
# 5. Transformar: explorar transformaciones log/sqrt/inversa como
#    verificación de sensibilidad.
# 6. Interpretar: construir el texto de interpretación aplicada para cada
#    área diagnóstica.
# -----------------------------------------------------------------------------

regCheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "regCheckClass",
    inherit = regCheckBase,
    private = list(
        .plotData = NULL,
        .numericBoxplotData = NULL,
        .predictorEffectsData = NULL,
        .pModel = NULL,
        .corrPairResults = NULL,
        .corrData = NULL,
        .corrMatVars = NULL,

        .init = function() {
            private$.initCorrelationMatrix()
        },

        .initCorrelationMatrix = function() {
            dep <- self$options$dep
            covs <- self$options$covs
            vars <- c(if (!is.null(dep) && nzchar(dep)) dep else character(0), covs)
            for (tableName in c("pearsonMatrixTable", "dcorMatrixTable")) {
                table <- tryCatch(self$results[[tableName]], error = function(e) NULL)
                if (is.null(table)) next()
                for (i in seq_along(vars)) {
                    colName <- paste0("c", i)
                    existing <- tryCatch(table$getColumn(colName), error = function(e) NULL)
                    if (is.null(existing)) {
                        tryCatch(
                            table$addColumn(name = colName, title = as.character(i), type = "text"),
                            error = function(e) NULL
                        )
                    }
                }
            }
            invisible(TRUE)
        },

        .run = function() {

            lang <- .al_lang(self$options)
            . <- lang$tr

            txt <- function(section, key) {
                .al_text(lang, section, key)
            }

            txt_block <- function(section, key) {
                .al_text_block(lang, section, key)
            }

            html_escape <- .al_html_escape

            html_block <- .al_html_block

            html_guide <- function(title, section, key) {
                html_block(title, .al_html_list(.al_text(lang, section, key)), raw = TRUE)
            }

            plot_guide <- function(title, objective, x_axis, y_axis,
                                   expected, warning, decision) {

                html_block(
                    title,
                    .al_html_list(c(
                        sprintf(.("Objective: %s"), objective),
                        sprintf(.("X axis: %s"), x_axis),
                        sprintf(.("Y axis: %s"), y_axis),
                        sprintf(.("Expected pattern: %s"), expected),
                        sprintf(.("Warning sign: %s"), warning),
                        sprintf(.("Possible decision: %s"), decision)
                    )),
                    raw = TRUE
                )
            }





            clean_num <- function(x) {
                if (length(x) == 0)
                    return(NA_real_)
                x <- suppressWarnings(as.numeric(x[1]))
                if (is.na(x) || is.nan(x) || is.infinite(x))
                    return(NA_real_)
                x
            }

            # p_sig(): identical logic (via clean_num) in every module,
            # consolidated in shared-helpers.R (.al_p_sig).
            # ES: idéntica (vía clean_num) en todos los módulos,
            # consolidada en shared-helpers.R.
            p_sig <- .al_p_sig

            fmt_p <- function(p) {
                p <- clean_num(p)

                if (is.na(p))
                    return("")

                if (p < .001)
                    return("<.001")

                out <- formatC(p, format = "f", digits = 3)
                sub("^0", "", out)
            }

            fmt_num <- function(x, digits = 4) {
                x <- clean_num(x)
                if (is.na(x))
                    return(.("Not computed"))
                format(round(x, digits), nsmall = digits)
            }

            # dcor_stat()/dcor_pvalue(): byte-identical (after
            # whitespace/name normalization) in every module that has them
            # (anovaCheck, logCheck, regCheck, relatedCheck), consolidated
            # in shared-helpers.R (.al_dcor_stat/.al_dcor_test), same
            # pattern already used for copentTest below. Fixed B=199/
            # seed=20260704 preserved unchanged.
            # ES: idénticas (tras normalizar espacios/nombres) en todos los
            # módulos que las tienen, consolidadas en shared-helpers.R
            # (.al_dcor_stat/.al_dcor_test), mismo patrón ya usado para
            # copentTest más abajo. B=199/semilla=20260704 fijos,
            # preservados sin cambio.
            dcor_stat <- .al_dcor_stat
            dcor_pvalue <- function(x, y) .al_dcor_test(x, y)$p

            # copentTest(): byte-identical in every module that has it,
            # consolidated in shared-helpers.R (.al_copent_test).
            # ES: idéntica en todos los módulos que la usan, consolidada
            # en shared-helpers.R.
            copentTest <- .al_copent_test

            # qname(): identical logic in every module that had it,
            # consolidated in shared-helpers.R (.al_qname) - the fix for
            # the formula-breaking-on-spaces crash jamovi's own module
            # review reported, Sep 2026.
            # ES: lógica idéntica en cada módulo que la tenía, consolidada
            # en shared-helpers.R (.al_qname) - el arreglo para el choque
            # de fórmula-rota-con-espacios que reportó la revisión oficial
            # del módulo de jamovi, sep. 2026.
            qname <- .al_qname

            safe_key <- function(x) {
                x <- gsub("[^A-Za-z0-9_]+", "_", x)
                x <- gsub("_+", "_", x)
                x
            }

            ul <- function(x) {
                chars <- strsplit(x, "", fixed = TRUE)[[1]]
                paste0(paste0(chars, "\u0332"), collapse = "")
            }

            add_table_row <- function(table, key, values) {
                table$addRow(rowKey = key, values = values)
            }

            if (is.null(self$options$dep) || self$options$dep == "") {
                jmvcore::reject(.("Select a numeric dependent variable."))
            }

            dep <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors
            predictors <- c(covs, factors)

            if (length(predictors) == 0) {
                jmvcore::reject(.("Select at least one numeric or categorical predictor."))
            }

            data <- self$data
            vars <- c(dep, predictors)
            dat <- data[, vars, drop = FALSE]

            complete <- stats::complete.cases(dat)
            dat2 <- dat[complete, , drop = FALSE]

            n_total <- nrow(dat)
            n_used <- nrow(dat2)
            n_excluded <- n_total - n_used

            if (n_used < 5) {
                jmvcore::reject(.("There are not enough complete cases to fit the model."))
            }

            formula_text <- paste(
                qname(dep),
                "~",
                paste(vapply(predictors, qname, character(1)), collapse = " + ")
            )

            model_formula <- stats::as.formula(formula_text)

            fit_error <- NULL
            fit <- tryCatch(
                stats::lm(model_formula, data = dat2),
                error = function(e) {
                    fit_error <<- conditionMessage(e)
                    NULL
                }
            )

            if (is.null(fit)) {
                jmvcore::reject(sprintf(.("It was not possible to fit the linear regression model: %s"), fit_error))
            }

            model_sum <- summary(fit)

            fitted_values <- tryCatch(stats::fitted(fit), error = function(e) NA_real_)
            residuals_raw <- tryCatch(stats::residuals(fit), error = function(e) NA_real_)
            residuals_stud <- tryCatch(stats::rstudent(fit), error = function(e) NA_real_)
            leverage <- tryCatch(stats::hatvalues(fit), error = function(e) NA_real_)
            cooks_d <- tryCatch(stats::cooks.distance(fit), error = function(e) NA_real_)
            dffits <- tryCatch(stats::dffits(fit), error = function(e) NA_real_)

            p_model <- length(stats::coef(fit))
            predictor_count <- length(predictors)
            private$.pModel <- p_model

            std_resid_plot <- tryCatch(
                stats::rstandard(fit),
                error = function(e) residuals_raw / stats::sd(residuals_raw, na.rm = TRUE)
            )

            y_plot <- dat2[[dep]]

            private$.plotData <- data.frame(
                case = seq_along(y_plot),
                observed = as.numeric(y_plot),
                fitted = as.numeric(fitted_values),
                residual = as.numeric(residuals_raw),
                stdResidual = as.numeric(std_resid_plot),
                studResidual = as.numeric(residuals_stud),
                absStdResidual = sqrt(abs(as.numeric(std_resid_plot))),
                leverage = as.numeric(leverage),
                cooksD = as.numeric(cooks_d),
                dffits = as.numeric(dffits),
                stringsAsFactors = FALSE
            )

            # jamovi re-renders Image results in a fresh execution context
            # when exporting to PDF/image; private$ fields (populated only
            # in this live .run() instance) are empty there, but data
            # placed in image$state via setState() survives into it. This
            # is the fix for the jamovi review team's HIGH-severity report
            # that this module's plots exported blank. Every Image in this
            # file gets its own setState() call below and at its
            # corresponding private$ assignment; render functions read
            # image$state instead of private$ fields.
            # ES: jamovi vuelve a renderizar los resultados Image en un
            # contexto de ejecución nuevo al exportar a PDF/imagen; los
            # campos private$ (poblados solo en esta instancia viva de
            # .run()) están vacíos allí, pero los datos colocados en
            # image$state mediante setState() sí sobreviven. Esta es la
            # corrección al hallazgo de severidad ALTA del equipo de
            # revisión de jamovi de que los gráficos de este módulo se
            # exportaban en blanco. Cada Image de este archivo recibe su
            # propio setState() más abajo y en su asignación private$
            # correspondiente; las funciones de render leen image$state en
            # lugar de campos private$.
            reg_plot_state <- list(plotData = private$.plotData, pModel = p_model)

            self$results$residualsFittedPlot$setState(reg_plot_state)
            self$results$qqResidualsPlot$setState(reg_plot_state)
            self$results$residualHistogramPlot$setState(reg_plot_state)
            self$results$residualNormalCurvePlot$setState(reg_plot_state)
            self$results$scaleLocationPlot$setState(reg_plot_state)
            self$results$residualsLeveragePlot$setState(reg_plot_state)
            self$results$cooksDPlot$setState(reg_plot_state)
            self$results$observedPredictedPlot$setState(reg_plot_state)

            box_vars <- unique(c(dep, self$options$covs))
            box_vars <- box_vars[box_vars %in% names(dat2)]

            box_list <- lapply(box_vars, function(v) {
                vals <- suppressWarnings(as.numeric(dat2[[v]]))
                data.frame(
                    variable = v,
                    value = vals,
                    stringsAsFactors = FALSE
                )
            })

            private$.numericBoxplotData <- if (length(box_list) > 0) {
                stats::na.omit(do.call(rbind, box_list))
            } else {
                NULL
            }

            self$results$numericBoxplotsPlot$setState(private$.numericBoxplotData)

            make_mode <- function(x) {
                x <- x[!is.na(x)]
                if (length(x) == 0)
                    return(NA)
                names(sort(table(x), decreasing = TRUE))[1]
            }

            effect_covs <- self$options$covs
            effect_covs <- effect_covs[effect_covs %in% names(dat2)]

            effect_list <- list()

            if (length(effect_covs) > 0) {
                base_row <- dat2[1, , drop = FALSE]

                for (nm in names(dat2)) {
                    if (is.numeric(dat2[[nm]]) || is.integer(dat2[[nm]])) {
                        base_row[[nm]] <- mean(dat2[[nm]], na.rm = TRUE)
                    } else {
                        base_row[[nm]] <- make_mode(dat2[[nm]])
                        if (is.factor(dat2[[nm]]))
                            base_row[[nm]] <- factor(base_row[[nm]], levels = levels(dat2[[nm]]))
                    }
                }

                for (v in effect_covs) {
                    vals <- suppressWarnings(as.numeric(dat2[[v]]))
                    vals <- vals[is.finite(vals)]

                    if (length(vals) < 3)
                        next

                    v_mean <- mean(vals, na.rm = TRUE)
                    v_sd <- stats::sd(vals, na.rm = TRUE)

                    if (!is.finite(v_sd) || v_sd <= 0)
                        next

                    z_grid <- seq(-2, 2, length.out = 80)
                    x_grid <- v_mean + z_grid * v_sd

                    newdat <- base_row[rep(1, length(x_grid)), , drop = FALSE]
                    newdat[[v]] <- x_grid

                    pred <- tryCatch(
                        stats::predict(fit, newdata = newdat),
                        error = function(e) rep(NA_real_, length(x_grid))
                    )

                    effect_list[[v]] <- data.frame(
                        predictor = v,
                        predictorZ = z_grid,
                        predicted = as.numeric(pred),
                        stringsAsFactors = FALSE
                    )
                }
            }

            private$.predictorEffectsData <- if (length(effect_list) > 0) {
                stats::na.omit(do.call(rbind, effect_list))
            } else {
                NULL
            }

            self$results$predictorEffectsPlot$setState(private$.predictorEffectsData)

            r2 <- clean_num(model_sum$r.squared)
            adj_r2 <- clean_num(model_sum$adj.r.squared)
            sigma <- clean_num(model_sum$sigma)

            fstat <- model_sum$fstatistic
            model_p <- NA_real_

            if (!is.null(fstat) && length(fstat) == 3) {
                model_p <- tryCatch(
                    stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE),
                    error = function(e) NA_real_
                )
            }

            self$results$intro$setContent(paste0(
                "<div style=\"max-width: 7.25in; width: 100%; box-sizing: border-box;\">",
                "<p style=\"font-weight: 700; margin: 0 0 0.10em 0; line-height: 1.25;\">",
                "AssumptionsLab",
                "</p>",
                "<p style=\"margin: 0 0 0.35em 0; line-height: 1.25;\">",
                .("Assumption check for simple &amp; multiple regression"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">\u00A0</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("Use this analysis when you want to review whether a linear regression model has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data."),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">\u00A0</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("<b>Dependent variable:</b> "), html_escape(dep),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("<b>Numeric predictors:</b> "),
                html_escape(ifelse(length(covs) == 0, .("None"), paste(covs, collapse = ", "))),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("<b>Categorical predictors:</b> "),
                html_escape(ifelse(length(factors) == 0, .("None"), paste(factors, collapse = ", "))),
                "</p>",
                "</div>"
            ))

            ms_i <- 1
            add_ms <- function(item, value) {
                add_table_row(
                    self$results$modelSummary,
                    paste0("ms_", ms_i),
                    list(item = item, value = value)
                )
                ms_i <<- ms_i + 1
            }

            add_ms(.("Model"), gsub("`", "", formula_text))
            add_ms(.("Cases used"), as.character(n_used))
            add_ms(.("Selected predictors"), as.character(predictor_count))
            add_ms(.("Estimated parameters"), as.character(p_model))
            add_ms("R²", fmt_num(r2))
            add_ms(.("Adjusted R²"), fmt_num(adj_r2))
            add_ms(.("Residual standard error"), fmt_num(sigma))
            add_ms(
                .("Model p"),
                ifelse(is.na(model_p), .("Not computed"),
                       format.pval(model_p, digits = 3, eps = .001))
            )

            miss_i <- 1
            add_miss <- function(item, value) {
                add_table_row(
                    self$results$missingSummary,
                    paste0("miss_", miss_i),
                    list(item = item, value = value)
                )
                miss_i <<- miss_i + 1
            }

            add_miss(.("Total rows"), as.character(n_total))
            add_miss(.("Complete cases used"), as.character(n_used))
            add_miss(.("Cases excluded due to missing data"), as.character(n_excluded))
            add_miss(
                .("Excluded percentage"),
                paste0(round(100 * n_excluded / max(1, n_total), 2), "%")
            )

            self$results$designGuide$setContent(html_guide(.("Regression model"), "regression", "designGuide"))

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # General flags for recommendations.
            # ES: Banderas generales para recomendaciones
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            linearity_problem <- FALSE
            normality_problem <- FALSE
            homo_problem <- FALSE
            auto_problem <- FALSE
            col_problem <- FALSE
            severe_col_problem <- FALSE
            influence_problem <- FALSE

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Linearity per predictor and overall model linearity.
            # ES: Linealidad por predictor y linealidad global
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            lin_pred_i <- 1
            lin_model_i <- 1

            add_linearity_predictor <- function(predictor, test, statistic, value, p_value, reason = NULL) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05 &&
                    test %in% c(
                        .("Exploratory quadratic term"),
                        .("Exploratory Box-Tidwell")
                    )) {
                    linearity_problem <<- TRUE
                }

                key <- paste0("lin_pred_", lin_pred_i)
                add_table_row(
                    self$results$linearityPredictor,
                    key,
                    list(
                        predictor = predictor,
                        dependent = dep,
                        test = test,
                        statistic = statistic,
                        value = clean_num(value),
                        p = clean_num(p_value),
                        pSig = p_sig(p_value)
                    )
                )
                if (!is.null(reason) && is.na(clean_num(value)) && is.na(clean_num(p_value)))
                    tryCatch(
                        self$results$linearityPredictor$addFootnote(col = "value", note = reason, rowKey = key),
                        error = function(e) invisible(NULL)
                    )

                lin_pred_i <<- lin_pred_i + 1
            }

            add_linearity_model <- function(scope, test, statistic, value, p_value) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05)
                    linearity_problem <<- TRUE

                add_table_row(
                    self$results$linearityModel,
                    paste0("lin_model_", lin_model_i),
                    list(
                        scope = scope,
                        dependent = dep,
                        test = test,
                        statistic = statistic,
                        value = clean_num(value),
                        p = clean_num(p_value),
                        pSig = p_sig(p_value)
                    )
                )

                lin_model_i <<- lin_model_i + 1
            }

            if (length(covs) == 0) {
                add_linearity_predictor(
                    "No aplicable",
                    .("Numeric predictors"),
                    "",
                    NA_real_,
                    NA_real_
                )
            }

            for (xname in covs) {

                x <- dat2[[xname]]
                y <- dat2[[dep]]

                cor_val <- tryCatch(
                    stats::cor(x, y, use = "complete.obs"),
                    error = function(e) NA_real_
                )

                cor_p <- tryCatch(
                    stats::cor.test(x, y)$p.value,
                    error = function(e) NA_real_
                )

                add_linearity_predictor(
                    xname,
                    .("Bivariate correlation"),
                    "r",
                    cor_val,
                    cor_p
                )

                quad_p <- tryCatch({
                    quadDat <- dat2
                    quadDat[[".x_sq_tmp"]] <- x^2

                    fit_quad <- stats::lm(
                        stats::as.formula(paste(
                            qname(dep), "~",
                            paste(vapply(predictors, qname, character(1)), collapse = " + "),
                            "+ .x_sq_tmp"
                        )),
                        data = quadDat
                    )

                    coef(summary(fit_quad))[".x_sq_tmp", "Pr(>|t|)"]
                }, error = function(e) NA_real_)

                add_linearity_predictor(
                    xname,
                    .("Exploratory quadratic term"),
                    "p",
                    quad_p,
                    quad_p
                )

                bt_needs_positive <- any(is.na(x)) || any(x <= 0)
                bt_p <- if (bt_needs_positive) NA_real_ else tryCatch({
                    boxTidwellDat <- dat2
                    boxTidwellDat[[".bt_tmp"]] <- x * log(x)

                    fit_bt <- stats::lm(
                        stats::as.formula(paste(
                            qname(dep), "~",
                            paste(vapply(predictors, qname, character(1)), collapse = " + "),
                            "+ .bt_tmp"
                        )),
                        data = boxTidwellDat
                    )

                    coef(summary(fit_bt))[".bt_tmp", "Pr(>|t|)"]
                }, error = function(e) NA_real_)

                bt_reason <- if (bt_needs_positive) .("Box-Tidwell requires strictly positive values (it uses x*log(x)); this predictor has zero, negative, or missing values.") else if (is.na(clean_num(bt_p))) .("Could not be computed for this predictor.") else NULL

                add_linearity_predictor(
                    xname,
                    .("Exploratory Box-Tidwell"),
                    "p",
                    bt_p,
                    bt_p,
                    reason = bt_reason
                )

                dcor_val <- tryCatch(dcor_stat(x, y), error = function(e) NA_real_)
                dcor_p <- tryCatch(dcor_pvalue(x, y), error = function(e) NA_real_)

                if (!is.na(dcor_val) && !is.na(cor_val) && !is.na(dcor_p) &&
                    (dcor_val - abs(cor_val)) > 0.10 && dcor_p < .05) {
                    linearity_problem <<- TRUE
                }

                add_linearity_predictor(
                    xname,
                    .("Distance correlation (dCor)"),
                    "dCor",
                    dcor_val,
                    dcor_p
                )

                ce_res <- tryCatch(copentTest(x, y), error = function(e) NULL)

                if (!is.null(ce_res)) {
                    add_linearity_predictor(
                        xname,
                        .("Copula entropy (copent)"),
                        "CE",
                        ce_res$ce,
                        ce_res$p
                    )
                } else {
                    ce_reason <- if (!requireNamespace("copent", quietly = TRUE)) .("The 'copent' package is not installed.") else .("Could not be computed for this predictor pair (copula entropy needs enough distinct values to estimate a copula density).")
                    add_linearity_predictor(
                        xname,
                        .("Copula entropy (copent)"),
                        "CE", NA_real_, NA_real_,
                        reason = ce_reason
                    )
                }
            }

            reset_p <- tryCatch({
                resetDat <- dat2
                resetDat[[".fit2_tmp"]] <- fitted_values^2
                resetDat[[".fit3_tmp"]] <- fitted_values^3

                fit_reset <- stats::lm(
                    stats::as.formula(paste(
                        qname(dep), "~",
                        paste(vapply(predictors, qname, character(1)), collapse = " + "),
                        "+ .fit2_tmp + .fit3_tmp"
                    )),
                    data = resetDat
                )

                resetAnovaTable <- stats::anova(fit, fit_reset)
                resetAnovaTable$`Pr(>F)`[2]
            }, error = function(e) NA_real_)

            add_linearity_model(
                .("Full model"),
                .("Approximate Ramsey RESET"),
                "p",
                reset_p,
                reset_p
            )

            rainbow_p <- tryCatch({
                ord <- order(fitted_values)
                n_rb <- length(ord)

                if (n_rb < 20)
                    stop("Muestra pequeña.")

                central_n <- floor(n_rb * 0.5)
                start_i <- floor((n_rb - central_n) / 2) + 1
                central_idx <- ord[start_i:(start_i + central_n - 1)]

                fit_central <- stats::lm(
                    model_formula,
                    data = dat2[central_idx, , drop = FALSE]
                )

                rss_full <- sum(stats::residuals(fit)^2, na.rm = TRUE)
                rss_central <- sum(stats::residuals(fit_central)^2, na.rm = TRUE)

                df_full <- stats::df.residual(fit)
                df_central <- stats::df.residual(fit_central)

                if (df_full <= df_central)
                    stop("Grados de libertad insuficientes.")

                f_val <- ((rss_full - rss_central) / (df_full - df_central)) /
                    (rss_central / df_central)

                stats::pf(f_val, df_full - df_central, df_central, lower.tail = FALSE)
            }, error = function(e) NA_real_)

            add_linearity_model(
                .("Full model"),
                .("Approximate Rainbow test"),
                "p",
                rainbow_p,
                rainbow_p
            )

            coherence_note <- function(anchor_sig, other_pvalues) {
                other_pvalues <- other_pvalues[!is.na(other_pvalues)]
                if (length(other_pvalues) == 0) return(NULL)
                other_sig <- other_pvalues < .05
                agree <- sum(other_sig == anchor_sig)
                total <- length(other_pvalues)
                list(agree = agree, total = total, all_agree = (agree == total))
            }

            coherence_sentence <- function(coh) {
                if (is.null(coh)) return(NULL)
                if (coh$all_agree) {
                    sprintf(.("The other %s test(s) in the table agree with this conclusion."), coh$total)
                } else {
                    sprintf(.("%s of the other %s tests in the table agree with this conclusion; check the table to see which one(s) differ before drawing a final conclusion."), coh$agree, coh$total)
                }
            }

            self$results$linearityGuide$setContent(html_guide(.("Linearity"), "regression", "linearityGuide"))

            self$results$linearityInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                c(
                    sprintf(.("Ramsey RESET: p = %s. Rainbow test: p = %s%s"), fmt_p(reset_p), fmt_p(rainbow_p), sprintf(.(" (n = %s). Both tests check whether the model is missing curvature that a straight line cannot capture."), n_used)),
                    .("A significant result (p < .05) does not mean the model is useless: it means the linear form may be missing part of the relationship, which can bias coefficients and predictions in specific ranges of the predictors."),
                    .("Common error: treating a non-significant RESET/Rainbow test as proof of perfect linearity. These tests have limited power in small samples (they may miss real curvature) and can flag trivial, practically irrelevant curvature in very large samples. Always read them together with residual-vs-fitted plots."),
                    .("If there is evidence of non-linearity, consider adding polynomial terms, transforming variables, using splines, or reviewing whether a different model family fits the relationship better."),
                    if (length(covs) > 0) .("Distance correlation (dCor): unlike Pearson's r, which only detects linear association, dCor can detect any form of statistical dependence, including non-monotonic or curved relationships, and equals zero only when the variables are independent. When dCor is notably higher than |Pearson's r| for a predictor, it signals dependence the linear model may be missing.") else "",
                    if (length(covs) > 0) .("Common error: treating a significant dCor by itself as proof that the relationship is non-linear. dCor is also significant for purely linear relationships; the real diagnostic signal is the size of the gap between dCor and |Pearson's r|, not the significance of dCor alone.") else ""
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # Correlation matrices (Pearson and dCor, APA 7 format).
            # ES: Matrices de correlaciones (Pearson y dCor, formato APA 7).
            # ------------------------------------------------------------

            self$results$correlationMatrixGuide$setContent(html_guide(.("Correlation Matrix"), "regression", "correlationMatrixGuide"))

            pearsonTable <- self$results$pearsonMatrixTable
            pearsonTable$deleteRows()
            dcorTable <- self$results$dcorMatrixTable
            dcorTable$deleteRows()

            matVars <- c(dep, covs)

            # fmtR()/apaCell(): identical in every module that has them,
            # consolidated in shared-helpers.R (.al_fmt_r/.al_apa_cell).
            # ES: idénticas en todos los módulos que las usan, consolidadas
            # en shared-helpers.R.
            fmtR <- .al_fmt_r
            apaCell <- .al_apa_cell

            k <- length(matVars)
            pairResults <- list()

            self$results$correlationMatrixGuide$setVisible(k >= 2)
            self$results$pearsonMatrixTable$setVisible(k >= 2)
            self$results$dcorMatrixTable$setVisible(k >= 2)
            self$results$correlationMatrixNote$setVisible(k >= 2)
            self$results$correlationComparisonGuide$setVisible(k >= 2)
            self$results$correlationComparisonTable$setVisible(k >= 2)
            self$results$correlationComparisonInterpretation$setVisible(k >= 2)

            corr_show_individual <- tryCatch(isTRUE(self$options$corrShowIndividual), error = function(e) TRUE)
            corr_show_comparative <- tryCatch(isTRUE(self$options$corrShowComparative), error = function(e) TRUE)
            self$results$correlationIndividualPlot$setVisible(k >= 2 && corr_show_individual)
            self$results$correlationIndividualGuide$setVisible(k >= 2 && corr_show_individual)
            self$results$correlationComparativePlot$setVisible(k >= 2 && corr_show_comparative)
            self$results$correlationComparativeGuide$setVisible(k >= 2 && corr_show_comparative)

            private$.corrPairResults <- NULL
            private$.corrData <- NULL
            private$.corrMatVars <- NULL

            self$results$correlationIndividualPlot$setState(NULL)
            self$results$correlationComparativePlot$setState(NULL)

            if (k >= 2) {
                for (i in seq_len(k)) {
                    for (j in seq_len(k)) {
                        if (j >= i) next()
                        v1 <- matVars[i]; v2 <- matVars[j]
                        x <- dat2[[v1]]; y <- dat2[[v2]]
                        ct <- tryCatch(stats::cor.test(x, y, method = "pearson"), error = function(e) NULL)
                        dcv <- tryCatch(dcor_stat(x, y), error = function(e) NA_real_)
                        dcp <- tryCatch(dcor_pvalue(x, y), error = function(e) NA_real_)
                        ceRes <- tryCatch(copentTest(x, y), error = function(e) NULL)

                        pairResults[[paste(v1, v2, sep = "|")]] <- list(
                            v1 = v1, v2 = v2,
                            pearsonR = if (!is.null(ct)) unname(ct$estimate) else NA_real_,
                            pearsonP = if (!is.null(ct)) ct$p.value else NA_real_,
                            dcor = dcv,
                            dcorP = dcp,
                            ce = if (!is.null(ceRes)) ceRes$ce else NA_real_,
                            ceP = if (!is.null(ceRes)) ceRes$p else NA_real_
                        )
                    }
                }

                private$.corrPairResults <- pairResults
                private$.corrData <- dat2[, matVars, drop = FALSE]
                private$.corrMatVars <- matVars

                # correlationComparativePlot only ever reads corrPairResults
                # (summary statistics per pair), never the raw columns, so it
                # gets its own smaller state instead of a second copy of
                # corrData - jamovi's review flagged storing more than a
                # plot draws as unnecessary bloat in every exported/saved
                # results file.
                # ES: correlationComparativePlot solo lee corrPairResults
                # (estadísticas resumen por par), nunca las columnas crudas,
                # así que recibe su propio estado más pequeño en vez de una
                # segunda copia de corrData - la revisión de jamovi señaló
                # que guardar más de lo que un gráfico dibuja es peso
                # innecesario en cada archivo de resultados exportado o
                # guardado.
                self$results$correlationIndividualPlot$setState(list(
                    corrPairResults = pairResults, corrData = private$.corrData
                ))
                self$results$correlationComparativePlot$setState(list(
                    corrPairResults = pairResults
                ))

                for (i in seq_len(k)) {
                    rowVar <- matVars[i]
                    pearsonVals <- list(var = sprintf("%d. %s", i, rowVar))
                    dcorVals <- list(var = sprintf("%d. %s", i, rowVar))
                    for (j in seq_len(k)) {
                        colName <- paste0("c", j)
                        if (j > i) {
                            pearsonVals[[colName]] <- ""
                            dcorVals[[colName]] <- ""
                        } else if (j == i) {
                            pearsonVals[[colName]] <- "\u2014"
                            dcorVals[[colName]] <- "\u2014"
                        } else {
                            pr <- pairResults[[paste(rowVar, matVars[j], sep = "|")]]
                            pearsonVals[[colName]] <- if (!is.null(pr)) apaCell(pr$pearsonR, pr$pearsonP) else ""
                            dcorVals[[colName]] <- if (!is.null(pr)) apaCell(pr$dcor, pr$dcorP) else ""
                        }
                    }
                    pearsonTable$addRow(rowKey = rowVar, values = pearsonVals)
                    dcorTable$addRow(rowKey = rowVar, values = dcorVals)
                }

                self$results$correlationMatrixNote$setContent(html_block(NULL,
                    c(
                        sprintf(.("%s * p < .05, ** p < .01, *** p < .001"), sprintf("N = %d.", n_used)),
                        .al_dcor_na_note(lang)
                    ),
                    paragraphs = FALSE
                ))
            } else {
                self$results$correlationMatrixNote$setContent(html_block(NULL,
                    .("Not applicable: fewer than two numeric model variables."),
                    paragraphs = FALSE
                ))
            }

            self$results$correlationComparisonGuide$setContent(html_block(
                .("Pearson / dCor / Copula Entropy Discordance Analysis"),
                .al_html_list(c(.("Because Pearson's r only captures linear association while dCor captures both linear and non-linear association, a pair whose dCor is notably larger than its Pearson |r| is a signal (not proof) of a non-linear relationship."), .("Pairs are flagged in the \"Pairs with a Notable Difference between Pearson and dCor\" table when the gap (dCor minus |Pearson r|) is greater than .10."), .("The copula entropy (CE, copent()) result for the same pair is shown alongside as a second, distribution-free line of evidence."), .("This threshold is a heuristic, not a formal test; always inspect a scatterplot of any flagged pair before concluding the relationship is non-linear."), .al_permutation_note(lang, 199, 20260704))),
                raw = TRUE
            ))

            compTable <- self$results$correlationComparisonTable
            compTable$deleteRows()
            gapThreshold <- 0.10
            flaggedPairs <- character(0)

            for (key in names(pairResults)) {
                pr <- pairResults[[key]]
                if (is.na(pr$pearsonR) || is.na(pr$dcor)) next()
                gap <- pr$dcor - abs(pr$pearsonR)
                if (gap <= gapThreshold) next()
                flaggedPairs <- c(flaggedPairs, sprintf("%s-%s", pr$v1, pr$v2))
                compTable$addRow(rowKey = key, values = list(
                    var1 = pr$v1,
                    var2 = pr$v2,
                    pearson = apaCell(pr$pearsonR, pr$pearsonP),
                    dcor = apaCell(pr$dcor, pr$dcorP),
                    gap = gap,
                    ce = pr$ce,
                    ceP = pr$ceP,
                    ceSig = p_sig(pr$ceP),
                    flag = .("Yes")
                ))
            }

            if (length(flaggedPairs) == 0) {
                self$results$correlationComparisonInterpretation$setContent(html_block(
                    .("Applied Interpretation"),
                    if (k < 2)
                        .("Not applicable: fewer than two numeric model variables.")
                    else
                        sprintf(.("No pair shows a Pearson/dCor gap greater than %.2f; there is no indication of unmodeled non-linear association among the model's numeric variables."), gapThreshold),
                    paragraphs = FALSE
                ))
            } else {
                self$results$correlationComparisonInterpretation$setContent(html_block(
                    .("Applied Interpretation"),
                    sprintf(.("%d pair(s) show a Pearson/dCor gap greater than %.2f: %s. Inspect a scatterplot of each flagged pair; when copula entropy is also significant for a pair, this reinforces the suspicion of an unmodeled non-linear dependency."), length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", ")),
                    paragraphs = FALSE
                ))
            }

            self$results$correlationIndividualGuide$setContent(
                plot_guide(
                    .("Guide: Pairwise relationship (Pearson / dCor)"),
                    .("visually compare what Pearson r and dCor detect in the relationship between two variables."),
                    .("values of the first variable in the pair."),
                    .("values of the second variable in the pair."),
                    .("points aligned around the linear fit line when r is high; a clear curve when dCor notably exceeds |r|."),
                    .("the smoothed fit separates markedly from the linear line, especially in pairs flagged in the discordance table."),
                    .("review whether a non-linear term or transformation is warranted, or simply confirm the relationship is linear.")
                )
            )

            self$results$correlationComparativeGuide$setContent(
                plot_guide(
                    .("Guide: Comparison of Pearson, dCor, and copula entropy across pairs"),
                    .("compare at a glance the three association coefficients (Pearson, dCor, copula entropy) across all pairs of numeric variables in the model."),
                    .("pairs of numeric variables in the model."),
                    .("coefficient magnitude (Pearson r, dCor)."),
                    .("Pearson and dCor with similar values for most pairs."),
                    .("a large gap between Pearson and dCor for some pair, especially if it coincides with significant copula entropy."),
                    .("review that pair's individual chart and consider a non-linear term if the gap is confirmed.")
                )
            )

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Residual normality.
            # ES: Normalidad de residuos
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            add_res_norm <- function(test, statistic, value, p_value, reason = NULL) {
                key <- paste0("norm_", safe_key(test))
                add_table_row(
                    self$results$residualNormality,
                    key,
                    list(
                        test = test,
                        statistic = statistic,
                        value = clean_num(value),
                        p = clean_num(p_value),
                        pSig = p_sig(p_value)
                    )
                )
                if (!is.null(reason) && is.na(clean_num(p_value)))
                    tryCatch(
                        self$results$residualNormality$addFootnote(col = "p", note = reason, rowKey = key),
                        error = function(e) invisible(NULL)
                    )
            }

            res <- residuals_raw
            res <- res[!is.na(res)]

            # Shapiro-Wilk / Jarque-Bera / skewness / kurtosis: guard unified
            # suite-wide (n>=3,<=5000 & sd>0 for SW; n>=8 & sd>0 for the rest)
            # per Archie's decision, Aug 2026 - see .al_norm_core_battery().
            # ES: guarda unificada para toda la suite - ver
            # .al_norm_core_battery() en shared-helpers.R.
            .nc_res <- .al_norm_core_battery(res)
            sw <- .nc_res$sw

            if (!is.null(sw)) {
                if (!is.na(clean_num(sw$p.value)) && clean_num(sw$p.value) < .05)
                    normality_problem <- TRUE
                add_res_norm("Shapiro-Wilk", "W", sw$statistic[[1]], sw$p.value)
            } else
                add_res_norm("Shapiro-Wilk", "W", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_res$reasons$sw, .))

            # Lilliefors / Anderson-Darling / Cramer-von Mises / Shapiro-Francia /
            # Pearson chi-square: identical tryCatch calls in every module,
            # consolidated in shared-helpers.R (.al_nortest_battery).
            # ES: idénticas en todos los módulos, consolidadas en shared-helpers.R.
            .nt_res <- .al_nortest_battery(res)
            li <- .nt_res$li; ad <- .nt_res$ad; cvm <- .nt_res$cvm; sf <- .nt_res$sf

            if (!is.null(li))
                add_res_norm(.("Lilliefors (corrected K-S)"), "D", li$statistic[[1]], li$p.value)
            else
                add_res_norm(.("Lilliefors (corrected K-S)"), "D", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$li, .))

            if (!is.null(ad))
                add_res_norm("Anderson-Darling", "A²", ad$statistic[[1]], ad$p.value)
            else
                add_res_norm("Anderson-Darling", "A²", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$ad, .))

            if (!is.null(cvm))
                add_res_norm("Cramer-von Mises", "W²", cvm$statistic[[1]], cvm$p.value)
            else
                add_res_norm("Cramer-von Mises", "W²", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$cvm, .))

            if (!is.null(sf))
                add_res_norm("Shapiro-Francia", "W'", sf$statistic[[1]], sf$p.value)
            else
                add_res_norm("Shapiro-Francia", "W'", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$sf, .))

            pt <- .nt_res$pt

            if (!is.null(pt))
                add_res_norm(.("Pearson chi-square"), "P", pt$statistic[[1]], pt$p.value)
            else
                add_res_norm(.("Pearson chi-square"), "P", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$pt, .))

            jb <- .nc_res$jb

            if (!is.null(jb))
                add_res_norm("Jarque-Bera", "JB", jb$value, jb$p)
            else
                add_res_norm("Jarque-Bera", "JB", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_res$reasons$jb, .))

            skew_test <- .nc_res$skew

            if (!is.null(skew_test))
                add_res_norm(.("Skewness test"), "z", skew_test$value, skew_test$p)
            else
                add_res_norm(.("Skewness test"), "z", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_res$reasons$skew, .))

            kurt_test <- .nc_res$kurt

            if (!is.null(kurt_test))
                add_res_norm(.("Kurtosis test"), "z", kurt_test$value, kurt_test$p)
            else
                add_res_norm(.("Kurtosis test"), "z", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_res$reasons$kurt, .))

            self$results$residualNormalityGuide$setContent(html_guide(.("Residual normality"), "regression", "residualNormalityGuide"))

            self$results$residualNormalityInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                c(
                    paste0(
                        .("Shapiro-Wilk (primary test): "),
                        if (!is.null(sw)) paste0("p = ", fmt_p(sw$p.value)) else .("not computed"),
                        sprintf(.(" (n = %s)."), n_used),
                        .(" This test looks at the residuals of the model, not the raw dependent variable.")
                    ),
                    .("Shapiro-Wilk is used as the primary test because it retains the highest statistical power among common normality tests across nearly the full range of sample sizes (Razali & Wah, 2011; see references). A widespread recommendation to switch to Kolmogorov-Smirnov once n exceeds ~50 is a leftover from old software limitations, not a statistical reason: in practice, Kolmogorov-Smirnov is generally one of the weakest of these tests when the mean and standard deviation are estimated from the same data, as is always the case here."),
                    (function() {
                        coh <- coherence_note(
                            !is.null(sw) && !is.na(sw$p.value) && sw$p.value < .05,
                            c(
                                if (!is.null(li)) li$p.value else NA_real_,
                                if (!is.null(ad)) ad$p.value else NA_real_,
                                if (!is.null(cvm)) cvm$p.value else NA_real_,
                                if (!is.null(sf)) sf$p.value else NA_real_,
                                if (!is.null(pt)) pt$p.value else NA_real_,
                                if (!is.null(jb)) jb$p else NA_real_,
                                if (!is.null(skew_test)) skew_test$p else NA_real_,
                                if (!is.null(kurt_test)) kurt_test$p else NA_real_
                            )
                        )
                        coherence_sentence(coh)
                    })(),
                    .("In practice, residual normality mainly affects the precision of classical inference (confidence intervals and p-values) in small samples. Point estimates of the coefficients remain valid even with moderate non-normality, and by the Central Limit Theorem, inference becomes more robust as sample size grows."),
                    .("Common error: assuming that a significant normality test invalidates the whole regression. It does not: it is a signal to inspect the shape of the residuals (skewness, heavy tails, outliers) before deciding whether classical inference is still trustworthy."),
                    .("Sample-size caveat: Shapiro-Wilk becomes very sensitive with large samples (roughly n > 300), often flagging small, practically irrelevant deviations as statistically significant. With small samples (n < 30) it may lack power to detect real departures from normality.")
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Homoscedasticity and heteroscedasticity.
            # ES: Homoscedasticidad y heterocedasticidad
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            add_homo <- function(family, test, statistic, value, df, p_value, reason = NULL) {
                key <- paste0("homo_", safe_key(test))
                add_table_row(
                    self$results$homoscedasticity,
                    key,
                    list(
                        family = family,
                        test = test,
                        statistic = statistic,
                        value = clean_num(value),
                        df = ifelse(is.na(clean_num(df)), NA_integer_, as.integer(df)),
                        p = clean_num(p_value),
                        pSig = p_sig(p_value)
                    )
                )
                if (!is.null(reason) && is.na(clean_num(p_value)))
                    tryCatch(
                        self$results$homoscedasticity$addFootnote(col = "p", note = reason, rowKey = key),
                        error = function(e) invisible(NULL)
                    )
            }

            bp <- .al_bptest(fit)

            if (!is.null(bp)) {
                if (!is.na(clean_num(bp$p.value)) && clean_num(bp$p.value) < .05)
                    homo_problem <- TRUE
                add_homo(.("Heteroscedasticity"), "Breusch-Pagan (lmtest)",
                         "LM", unname(bp$statistic), unname(bp$parameter), bp$p.value)
            } else
                add_homo(.("Heteroscedasticity"), "Breusch-Pagan (lmtest)",
                         "LM", NA_real_, NA_integer_, NA_real_, reason = .("Could not be computed - the auxiliary regression of squared residuals on the predictors is likely singular (too few residual degrees of freedom, or near-perfect collinearity among predictors)."))

            white <- tryCatch({
                e2 <- residuals_raw^2
                k <- length(predictors)
                if (k >= 1) {
                    terms <- paste0("`", predictors, "`")
                    sq_terms <- if (length(covs) > 0) paste0("I(`", covs, "`^2)") else character(0)
                    int_terms <- character(0)
                    if (k >= 2) {
                        combos <- utils::combn(predictors, 2, simplify = FALSE)
                        int_terms <- vapply(combos, function(pr) paste0("`", pr[1], "`:`", pr[2], "`"), character(1))
                    }
                    rhs <- paste(c(terms, sq_terms, int_terms), collapse = " + ")
                    auxForm <- stats::as.formula(paste("e2 ~", rhs))
                    auxData <- dat2
                    auxData$e2 <- e2
                    auxFit <- stats::lm(auxForm, data = auxData)
                    aux_coefs <- stats::coef(auxFit)
                    dfW <- sum(!is.na(aux_coefs)) - 1
                    lm_val <- if (dfW > 0) length(e2) * summary(auxFit)$r.squared else NA_real_

                    if (is.finite(lm_val) && is.finite(dfW) && dfW > 0)
                        list(value = lm_val, df = dfW, p = stats::pchisq(lm_val, df = dfW, lower.tail = FALSE))
                    else
                        NULL
                } else NULL
            }, error = function(e) NULL)

            if (!is.null(white))
                add_homo(.("Heteroscedasticity"), .("White (general)"),
                         "LM", white$value, white$df, white$p)
            else
                add_homo(.("Heteroscedasticity"), .("White (general)"),
                         "LM", NA_real_, NA_integer_, NA_real_, reason = .("Could not be computed - White's auxiliary regression adds every predictor's square and cross-product, so it needs more residual degrees of freedom than Breusch-Pagan; there are likely too many predictors relative to the number of complete cases."))

            gq <- tryCatch(lmtest::gqtest(fit, order.by = fitted_values), error = function(e) NULL)

            if (!is.null(gq))
                add_homo(.("Heteroscedasticity"), "Goldfeld-Quandt (lmtest)",
                         "F", unname(gq$statistic), min(unname(gq$parameter)), gq$p.value)
            else
                add_homo(.("Heteroscedasticity"), "Goldfeld-Quandt (lmtest)",
                         "F", NA_real_, NA_integer_, NA_real_, reason = .("Could not be computed - Goldfeld-Quandt splits the sample in two (ordered by fitted values) and fits a separate regression to each half; the sample is likely too small, relative to the number of predictors, for both halves to be estimable."))

            fitted_groups <- tryCatch({
                r <- rank(fitted_values, ties.method = "first")
                br <- stats::quantile(r, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
                br <- unique(br)

                if (length(br) < 4)
                    stop("No hay suficientes grupos distintos.")

                cut(
                    r,
                    breaks = br,
                    include.lowest = TRUE,
                    labels = c("Bajo", "Medio", "Alto")
                )
            }, error = function(e) NULL)

            # Manual Levene (mean-centered) / Brown-Forsythe (median-centered):
            # identical algorithm in anovaCheck and regCheck, consolidated in
            # shared-helpers.R (.al_levene_manual). groupCheck's car::leveneTest
            # rows are a separate implementation, left untouched.
            # ES: idéntico en anovaCheck y regCheck, consolidado en
            # shared-helpers.R. Las filas de groupCheck (car::leveneTest) son
            # una implementación aparte, sin tocar.
            lev_mean <- tryCatch(
                .al_levene_manual(residuals_raw, fitted_groups, "mean"),
                error = function(e) NULL
            )

            if (!is.null(lev_mean)) {
                add_homo(
                    .("Variance equality"),
                    .("Levene by fitted values"),
                    "F",
                    lev_mean$value,
                    lev_mean$df,
                    lev_mean$p
                )
            } else {
                add_homo(
                    .("Variance equality"),
                    .("Levene by fitted values"),
                    "F",
                    NA_real_,
                    NA_integer_,
                    NA_real_
                )
            }

            lev_median <- tryCatch(
                .al_levene_manual(residuals_raw, fitted_groups, "median"),
                error = function(e) NULL
            )

            if (!is.null(lev_median)) {
                add_homo(
                    .("Variance equality"),
                    .("Brown-Forsythe by fitted values"),
                    "F",
                    lev_median$value,
                    lev_median$df,
                    lev_median$p
                )
            } else {
                add_homo(
                    .("Variance equality"),
                    .("Brown-Forsythe by fitted values"),
                    "F",
                    NA_real_,
                    NA_integer_,
                    NA_real_
                )
            }

            fligner <- tryCatch({
                if (is.null(fitted_groups))
                    stop("No hay grupos.")

                dat_fk <- data.frame(
                    res = residuals_raw,
                    group = fitted_groups
                )

                dat_fk <- dat_fk[stats::complete.cases(dat_fk), , drop = FALSE]

                if (length(unique(dat_fk$group)) < 2)
                    stop("No hay suficientes grupos.")

                stats::fligner.test(res ~ group, data = dat_fk)
            }, error = function(e) NULL)

            if (!is.null(fligner)) {
                add_homo(
                    .("Variance equality"),
                    .("Fligner-Killeen by fitted values"),
                    "χ²",
                    fligner$statistic[[1]],
                    fligner$parameter[[1]],
                    fligner$p.value
                )
            } else {
                add_homo(
                    .("Variance equality"),
                    .("Fligner-Killeen by fitted values"),
                    "χ²",
                    NA_real_,
                    NA_integer_,
                    NA_real_
                )
            }

            spearman_abs <- tryCatch({
                stats::cor.test(
                    abs(residuals_raw),
                    fitted_values,
                    method = "spearman",
                    exact = FALSE
                )
            }, error = function(e) NULL)

            if (!is.null(spearman_abs)) {
                add_homo(
                    .("Heteroscedasticity"),
                    .("Spearman |residuals| vs fitted"),
                    "ρ",
                    spearman_abs$estimate[[1]],
                    NA_integer_,
                    spearman_abs$p.value
                )
            } else {
                add_homo(
                    .("Heteroscedasticity"),
                    .("Spearman |residuals| vs fitted"),
                    "ρ",
                    NA_real_,
                    NA_integer_,
                    NA_real_
                )
            }

            self$results$homoscedasticityGuide$setContent(html_guide(.("Homoscedasticity and heteroscedasticity"), "regression", "homoscedasticityGuide"))

            self$results$homoscedasticityInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                c(
                    paste0(
                        .("Breusch-Pagan (primary test): "),
                        if (!is.null(bp)) paste0("p = ", fmt_p(bp$p.value)) else .("not computed"),
                        sprintf(.(" (n = %s)."), n_used),
                        " "
                    ),
                    .("Breusch-Pagan is used as the primary test because it was designed specifically to detect heteroscedasticity in regression residuals (Breusch & Pagan, 1979; see references), unlike Levene, Brown-Forsythe, and Fligner-Killeen, which are group-variance tests borrowed from ANOVA and applied here only approximately, by artificially splitting fitted values into bins. The White test (reported alongside) generalizes Breusch-Pagan to detect more complex, non-linear forms of heteroscedasticity (White, 1980; see references)."),
                    (function() {
                        coh <- coherence_note(
                            !is.null(bp) && !is.na(bp$p.value) && bp$p.value < .05,
                            c(
                                if (!is.null(white)) white$p else NA_real_,
                                if (!is.null(gq)) gq$p.value else NA_real_,
                                if (!is.null(lev_mean)) lev_mean$p else NA_real_,
                                if (!is.null(lev_median)) lev_median$p else NA_real_,
                                if (!is.null(fligner)) fligner$p.value else NA_real_,
                                if (!is.null(spearman_abs)) spearman_abs$p.value else NA_real_
                            )
                        )
                        coherence_sentence(coh)
                    })(),
                    .("Heteroscedasticity does not bias the regression coefficients themselves, but it does make the standard errors, confidence intervals, and p-values unreliable, because the classical formulas assume constant error variance across all predicted values."),
                    .("Common error: concluding that a significant heteroscedasticity test means the model must be discarded. In most cases, the practical remedy is not a different model but heteroscedasticity-robust standard errors (also called White or Huber-White standard errors)."),
                    .("Sample-size note: Breusch-Pagan and White rely on a large-sample (asymptotic) approximation and can be unreliable with very small samples; in that situation, cross-check with Levene, Fligner-Killeen, or the Spearman correlation between |residuals| and fitted values shown here.")
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Error independence.
            # ES: Independencia de errores
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            ind_i <- 1

            add_independence <- function(test, statistic, value, p_value) {
                p_clean <- clean_num(p_value)

                add_table_row(
                    self$results$independence,
                    paste0("ind_", ind_i),
                    list(
                        test = test,
                        statistic = statistic,
                        value = clean_num(value),
                        p = if (is.na(p_clean)) "" else fmt_p(p_clean),
                        pSig = p_sig(p_clean)
                    )
                )

                ind_i <<- ind_i + 1
            }

            dw <- tryCatch({
                sum(diff(residuals_raw)^2) / sum(residuals_raw^2)
            }, error = function(e) NA_real_)

            if (!is.na(clean_num(dw)) && (dw < 1.5 || dw > 2.5))
                auto_problem <- TRUE

            add_independence("Durbin-Watson", "DW", dw, NA_real_)

            bg_lag <- min(2, max(1, floor(n_used / 10)))

            bg <- tryCatch({
                e <- residuals_raw
                nbg <- length(e)
                aux_dat <- data.frame(e = e)

                for (lag in seq_len(bg_lag)) {
                    aux_dat[[paste0("lag", lag)]] <-
                        c(rep(NA_real_, lag), e[seq_len(nbg - lag)])
                }

                X_model <- stats::model.matrix(fit)
                X_model <- X_model[, colnames(X_model) != "(Intercept)", drop = FALSE]

                if (ncol(X_model) > 0) {
                    X_aux <- as.data.frame(X_model)
                    names(X_aux) <- paste0("x", seq_len(ncol(X_aux)))
                    aux_dat <- cbind(aux_dat, X_aux)
                }

                aux_dat <- aux_dat[stats::complete.cases(aux_dat), , drop = FALSE]
                aux_fit <- stats::lm(e ~ ., data = aux_dat)

                lm_val <- nrow(aux_dat) * summary(aux_fit)$r.squared
                p <- stats::pchisq(lm_val, df = bg_lag, lower.tail = FALSE)

                list(value = lm_val, p = p, df = bg_lag)
            }, error = function(e) NULL)

            if (!is.null(bg)) {
                if (!is.na(clean_num(bg$p)) && clean_num(bg$p) < .05)
                    auto_problem <- TRUE
                add_independence(sprintf(.("Breusch-Godfrey, lags = %s"), bg$df),
                                 "LM", bg$value, bg$p)
            } else
                add_independence("Breusch-Godfrey", "LM", NA_real_, NA_real_)

            lb_lag <- min(10, max(1, floor(n_used / 5)))

            lb <- tryCatch({
                stats::Box.test(
                    residuals_raw,
                    lag = lb_lag,
                    type = "Ljung-Box",
                    fitdf = min(p_model, lb_lag - 1)
                )
            }, error = function(e) NULL)

            if (!is.null(lb))
                add_independence(sprintf(.("Ljung-Box, lags = %s"), lb_lag),
                                 "Q", lb$statistic[[1]], lb$p.value)
            else
                add_independence("Ljung-Box", "Q", NA_real_, NA_real_)

            runs <- tryCatch({
                signs <- residuals_raw
                signs <- signs[!is.na(signs)]
                signs <- signs[signs != 0]

                if (length(signs) < 5)
                    stop("Muy pocos residuos.")

                sgn <- ifelse(signs > 0, 1, 0)
                n1 <- sum(sgn == 1)
                n2 <- sum(sgn == 0)

                if (n1 == 0 || n2 == 0)
                    stop("No hay ambos signos.")

                R <- 1 + sum(sgn[-1] != sgn[-length(sgn)])
                mu_R <- 1 + (2 * n1 * n2) / (n1 + n2)

                var_R <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) /
                    (((n1 + n2)^2) * (n1 + n2 - 1))

                z <- (R - mu_R) / sqrt(var_R)
                p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)

                list(value = z, p = p)
            }, error = function(e) NULL)

            if (!is.null(runs))
                add_independence("Runs test", "z", runs$value, runs$p)
            else
                add_independence("Runs test", "z", NA_real_, NA_real_)

            self$results$independenceGuide$setContent(html_guide(.("Error independence"), "regression", "independenceGuide"))

            self$results$independenceInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                c(
                    paste0(
                        .("Breusch-Godfrey (primary test): "),
                        if (!is.null(bg)) paste0("p = ", fmt_p(bg$p)) else .("not computed"),
                        sprintf(.(" (n = %s). Durbin-Watson (reported for reference): DW = "), n_used),
                        fmt_num(dw, 3),
                        .(". Values close to 2 are compatible with independent errors.")
                    ),
                    .("Breusch-Godfrey is used as the primary test because it generalizes Durbin-Watson to any lag order and does not have Durbin-Watson's \"inconclusive zone\", a range of values where the classical DW test cannot reject or accept the null hypothesis (Breusch, 1978; Godfrey, 1978; see references). Durbin-Watson is still reported because it remains the most widely recognized statistic in applied practice (Durbin & Watson, 1951; see references)."),
                    (function() {
                        coh <- coherence_note(
                            !is.null(bg) && !is.na(bg$p) && bg$p < .05,
                            c(
                                if (!is.null(lb)) lb$p.value else NA_real_,
                                if (!is.null(runs)) runs$p else NA_real_
                            )
                        )
                        coherence_sentence(coh)
                    })(),
                    .("Common error: running and interpreting these tests on data that has no meaningful order (for example, a cross-sectional survey where row order is arbitrary). They are only informative when the order of observations reflects a real sequence: time, spatial position, or measurement order."),
                    .("If your data does have a meaningful order (repeated measurements over time, data collected sequentially, or clustered/nested observations), significant autocorrelation means the standard errors of the model are likely underestimated, making results look more precise than they really are."),
                    .("If there is a real dependency structure (repeated measures, nested groups, time series), consider mixed-effects models, generalized least squares, or explicitly time-series-aware methods instead of relying only on this diagnostic.")
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Multicollinearity.
            # ES: Multicolinealidad
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            multi_i <- 1
            max_vif <- NA_real_
            max_vif_name <- NA_character_
            max_ci <- NA_real_
            non_estimable <- NA_real_

            add_multi <- function(diagnostic, item, statistic, value, reason = NULL) {
                key <- paste0("multi_", multi_i)
                add_table_row(
                    self$results$multicollinearity,
                    key,
                    list(
                        diagnostic = diagnostic,
                        item = item,
                        statistic = statistic,
                        value = clean_num(value)
                    )
                )
                if (!is.null(reason) && is.na(clean_num(value)))
                    tryCatch(
                        self$results$multicollinearity$addFootnote(col = "value", note = reason, rowKey = key),
                        error = function(e) invisible(NULL)
                    )

                multi_i <<- multi_i + 1
            }

            X <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)

            if (!is.null(X)) {
                X_no_intercept <- X[, colnames(X) != "(Intercept)", drop = FALSE]
            } else {
                X_no_intercept <- NULL
            }

            mc_visible <- !is.null(X_no_intercept) && ncol(X_no_intercept) > 1
            self$results$multicollinearityGuide$setVisible(mc_visible)
            self$results$multicollinearity$setVisible(mc_visible)
            self$results$multicollinearityInterpretation$setVisible(mc_visible)

            if (!is.null(X_no_intercept) && ncol(X_no_intercept) > 1) {

                for (j in seq_len(ncol(X_no_intercept))) {
                    target <- X_no_intercept[, j]
                    others <- X_no_intercept[, -j, drop = FALSE]

                    vif <- tryCatch({
                        vifAuxFit <- stats::lm(target ~ others)
                        r2j <- summary(vifAuxFit)$r.squared
                        1 / (1 - r2j)
                    }, error = function(e) NA_real_)

                    tol <- ifelse(is.na(clean_num(vif)), NA_real_, 1 / vif)

                    if (!is.na(clean_num(vif))) {
                        if (is.na(max_vif) || vif > max_vif) {
                            max_vif <- vif
                            max_vif_name <- colnames(X_no_intercept)[j]
                        }
                    }

                    vif_reason <- if (is.na(clean_num(vif))) .("Could not be computed - regressing this predictor on the others is likely singular (more predictors than complete cases, or an exact linear dependency among predictors).") else NULL
                    add_multi("VIF", colnames(X_no_intercept)[j], "VIF", vif, reason = vif_reason)
                    add_multi(.("Tolerance"), colnames(X_no_intercept)[j], "1/VIF", tol, reason = vif_reason)
                }

                eig <- tryCatch({
                    X_scaled <- scale(X_no_intercept)
                    R <- stats::cor(X_scaled, use = "pairwise.complete.obs")
                    eigen(R, symmetric = TRUE)$values
                }, error = function(e) NULL)

                if (is.null(eig)) {
                    eig_reason <- .("Could not be computed - the predictors' correlation matrix could not be decomposed (likely too few complete cases relative to the number of predictors).")
                    add_multi(.("Minimum eigenvalue"), .("Design matrix"), .("minimum λ"), NA_real_, reason = eig_reason)
                    add_multi(.("Condition index"), .("Design matrix"), "CI", NA_real_, reason = eig_reason)
                    add_multi(.("Determinant"), .("Correlation matrix"), "det(R)", NA_real_, reason = eig_reason)
                }

                if (!is.null(eig)) {
                    min_eig <- max(min(eig, na.rm = TRUE), .Machine$double.eps)
                    max_eig <- max(eig, na.rm = TRUE)
                    ci <- sqrt(max_eig / min_eig)
                    max_ci <- ci

                    det_r <- tryCatch(
                        det(stats::cor(scale(X_no_intercept),
                                       use = "pairwise.complete.obs")),
                        error = function(e) NA_real_
                    )

                    add_multi(.("Minimum eigenvalue"), .("Design matrix"), .("minimum λ"), min_eig)
                    add_multi(.("Condition index"), .("Design matrix"), "CI", ci)
                    add_multi(.("Determinant"), .("Correlation matrix"), "det(R)", det_r)
                }
            } else {
                add_multi("VIF / tolerancia", "No aplicable", "", NA_real_)
            }

            if (length(covs) >= 2) {
                cor_mat <- tryCatch(
                    stats::cor(dat2[, covs, drop = FALSE], use = "pairwise.complete.obs"),
                    error = function(e) NULL
                )

                if (!is.null(cor_mat)) {
                    cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA_real_
                    max_cor <- suppressWarnings(max(abs(cor_mat), na.rm = TRUE))
                    add_multi(.("Maximum correlation"), .("Numeric predictors"), .("maximum |r|"), max_cor)
                }
            }

            model_rank <- tryCatch(qr(stats::model.matrix(fit))$rank, error = function(e) NA_real_)
            model_cols <- tryCatch(ncol(stats::model.matrix(fit)), error = function(e) NA_real_)
            non_estimable <- clean_num(model_cols - model_rank)

            add_multi(.("Model rank"), .("Design matrix"), .("rank"), model_rank)
            add_multi(.("Non-estimable parameters"), .("Design matrix"),
                      .("columns - rank"), non_estimable)

            if (!is.na(clean_num(max_vif)) && max_vif >= 5)
                col_problem <- TRUE

            if (!is.na(clean_num(max_vif)) && max_vif >= 10)
                severe_col_problem <- TRUE

            if (!is.na(clean_num(max_ci)) && max_ci > 15)
                col_problem <- TRUE

            if (!is.na(clean_num(non_estimable)) && non_estimable > 0)
                col_problem <- TRUE

            self$results$multicollinearityGuide$setContent(html_guide(.("Multicollinearity"), "regression", "multicollinearityGuide"))

            self$results$multicollinearityInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                c(
                    sprintf(.("Maximum VIF: %s%s. By convention, values between 5 and 10 raise a moderate concern, and values above 10 are considered a severe problem (a threshold traceable to Marquardt, 1970, and widely adopted in reference texts such as Kutner et al.'s Applied Linear Statistical Models and Hair et al.'s Multivariate Data Analysis)."), fmt_num(max_vif, 2), if (!is.na(max_vif_name)) sprintf(.(" (predictor: %s)"), max_vif_name) else ""),
                    sprintf(.("In practical terms, a VIF of %s%s"), fmt_num(max_vif, 2), sprintf(.(" means the standard error of that coefficient is about %s times larger than it would be without collinearity. This does not invalidate the model as a whole, but it does make that specific coefficient's confidence interval much less precise."), fmt_num(sqrt(max_vif), 2))),
                    .("Common error: assuming a high VIF means the predictor should be removed or that the model \"does not work\". The model can still predict well overall; the issue is isolating that specific predictor's individual effect from the others. Removing a substantively relevant variable only because of a high VIF can bias the model more than the collinearity itself did."),
                    sprintf(.("Sample-size caveat: this threshold is a convention, not a statistical law. With small samples (n < 100; here n = %s), even moderate collinearity can make coefficients quite unstable; with larger samples, a similar VIF is usually more tolerable."), n_used),
                    .("If collinearity is a real concern, consider combining the most redundant predictors, centering variables involved in interactions, or reporting the finding and discussing its effect on that coefficient's precision rather than discarding the full model.")
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # Outlying and influential cases.
            # ES: Casos atípicos e influyentes.
            # ------------------------------------------------------------

            n <- n_used

            # Threshold computation, per-case flagging, and top-20 ranking:
            # byte-identical logic in anovaCheck and regCheck, consolidated
            # in shared-helpers.R (.al_influence_diagnostics). logCheck,
            # pathCheck, and groupCheck each have a genuinely different
            # design for their own model type and are not part of this.
            # ES: cómputo de umbrales, marcado por caso y ranking top-20 -
            # lógica idéntica en anovaCheck y regCheck, consolidada en
            # shared-helpers.R. logCheck, pathCheck y groupCheck tienen
            # cada uno un diseño genuinamente distinto para su tipo de
            # modelo y no forman parte de esto.
            .infl <- .al_influence_diagnostics(n, p_model, residuals_stud, leverage, cooks_d, dffits)
            cook_cut <- .infl$cook_cut
            lev_cut <- .infl$lev_cut
            dffits_cut <- .infl$dffits_cut
            stud_cut <- .infl$stud_cut
            criteria <- .infl$criteria
            influence_problem <- .infl$problem
            ord <- .infl$ord
            complete_indices <- which(complete)

            for (k in seq_along(ord)) {
                i <- ord[k]

                add_table_row(
                    self$results$influence,
                    paste0("case_", k),
                    list(
                        case = complete_indices[i],
                        fitted = clean_num(fitted_values[i]),
                        residual = clean_num(residuals_raw[i]),
                        studResidual = clean_num(residuals_stud[i]),
                        leverage = clean_num(leverage[i]),
                        cooksD = clean_num(cooks_d[i]),
                        dffits = clean_num(dffits[i]),
                        criteria = criteria[i]
                    )
                )
            }

            self$results$influenceGuide$setContent(html_guide(.("Outlying and influential cases"), "regression", "influenceGuide"))

            n_flagged_influence <- sum(nzchar(criteria), na.rm = TRUE)

            self$results$influenceInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                c(
                    paste0(
                        sprintf(.("%s of %s cases were flagged by at least one influence criterion (Cook's D > %s, leverage > %s, |DFFITS| > %s, or |studentized residual| > %s)."), n_flagged_influence, n, fmt_num(cook_cut, 4), fmt_num(lev_cut, 4), fmt_num(dffits_cut, 4), stud_cut)
                    ),
                    .("These criteria measure different aspects of the same question: how unusual a case is (leverage), how large its residual is (studentized residual), and how much it would change the model's coefficients if removed (Cook's D, DFFITS). A flagged case is not automatically an error."),
                    .("Common error: deleting flagged cases automatically to \"improve\" the model. This can hide real, substantively important observations and produce a model that only fits the well-behaved majority of the data, misrepresenting the population it is meant to describe."),
                    sprintf(.("Sample-size caveat: the Cook's D cutoff of 4/n used here (n = %s) becomes stricter as the sample shrinks, so in small samples it is common to flag several cases that are not truly problematic. Treat the number of flagged cases as a starting point for inspection, not as a fixed quota to remove."), n),
                    .("For each flagged case, check whether it is a data-entry error, a valid but extreme case, or a signal that the model does not represent all subgroups equally well; compare the model's conclusions with and without the case before deciding.")
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Exploratory transformations of the dependent variable.
            # ES: Transformaciones exploratorias de la variable dependiente
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            # ------------------------------------------------------------
            # Plot visibility according to the selected plot set.
            # ES: Visibilidad de gráficos según el conjunto seleccionado.
            # ------------------------------------------------------------

            # ------------------------------------------------------------
            # Plot visibility per diagnostic area.
            # ES: Visibilidad de gráficos por área.
            # ------------------------------------------------------------

            lin_show <- tryCatch(isTRUE(self$options$linShowPlots), error = function(e) TRUE)
            norm_show <- tryCatch(isTRUE(self$options$normShowPlots), error = function(e) TRUE)
            homo_show <- tryCatch(isTRUE(self$options$homoShowPlots), error = function(e) TRUE)
            influence_show <- tryCatch(isTRUE(self$options$influenceShowPlots), error = function(e) TRUE)

            area_visibility <- c(
                residualsFittedPlot = lin_show, residualsFittedGuide = lin_show,
                observedPredictedPlot = lin_show, observedPredictedGuide = lin_show,
                predictorEffectsPlot = lin_show, predictorEffectsGuide = lin_show,

                qqResidualsPlot = norm_show, qqResidualsGuide = norm_show,
                residualHistogramPlot = norm_show, residualHistogramGuide = norm_show,
                residualNormalCurvePlot = norm_show, residualNormalCurveGuide = norm_show,

                scaleLocationPlot = homo_show, scaleLocationGuide = homo_show,

                numericBoxplotsPlot = influence_show, numericBoxplotsGuide = influence_show,
                residualsLeveragePlot = influence_show, residualsLeverageGuide = influence_show,
                cooksDPlot = influence_show, cooksDGuide = influence_show
            )

            for (item_name in names(area_visibility)) {
                item <- tryCatch(self$results[[item_name]], error = function(e) NULL)

                if (!is.null(item)) {
                    item$setVisible(
                        isTRUE(self$options$showPlots) && isTRUE(area_visibility[[item_name]])
                    )
                }
            }

            self$results$diagnosticPlotsGuide$setVisible(
                isTRUE(self$options$showPlots) &&
                    (lin_show || norm_show || homo_show || influence_show)
            )

            if (isTRUE(self$options$showPlots)) {
                self$results$diagnosticPlotsGuide$setContent(
                    html_block(
                        .("Regression diagnostic plots"),
                        c(
                            .("Diagnostic plots complement statistical tests and help evaluate patterns that a p-value may hide."),
                            .("Use these plots to review linearity, residual normality, homoscedasticity, influence, and predictive quality.")
                        )
                    )
                )

                self$results$residualsFittedGuide$setContent(
                    plot_guide(
                        .("Guide: Residuals vs fitted values"),
                        .("evaluate linearity and constant residual variance."),
                        .("fitted or predicted values from the model for each case."),
                        .("residuals, that is, the difference between observed and predicted values."),
                        .("a random cloud around zero, without curvature or funnel shape."),
                        .("curvature, bands, funnel shape, or separated groups."),
                        .("review nonlinear terms, transformation, or robust standard errors.")
                    )
                )

                self$results$predictorEffectsGuide$setContent(
                    plot_guide(
                        .("Guide: Visual effects of numeric predictors"),
                        .("compare the direction and relative strength of numeric predictors."),
                        .("standardized value of the evaluated predictor, from low to high."),
                        .("predicted dependent-variable value while holding the other predictors constant."),
                        .("approximately straight lines with slopes consistent with the model."),
                        .("very different slopes, flat lines, or effects that are difficult to interpret."),
                        .("review effect size and direction, linearity, interactions, or nonlinear terms.")
                    )
                )

                self$results$qqResidualsGuide$setContent(
                    plot_guide(
                        .("Guide: Residual Q-Q plot"),
                        .("evaluate whether standardized residuals approximate a normal distribution."),
                        .("theoretical quantiles expected under normality."),
                        .("observed quantiles of the standardized residuals."),
                        .("points close to the diagonal line."),
                        .("strong deviations at the extremes, S-shape, or heavy tails."),
                        .("interpret classical inferences cautiously and review outliers or transformations.")
                    )
                )

                self$results$residualHistogramGuide$setContent(
                    plot_guide(
                        .("Guide: Residual histogram"),
                        .("examine the shape, symmetry, and concentration of standardized residuals."),
                        .("standardized residuals."),
                        .("frequency or number of cases."),
                        .("an approximately symmetric distribution centered near zero."),
                        .("marked skewness, multiple peaks, or very long tails."),
                        .("complement normality tests and review extreme cases.")
                    )
                )

                self$results$residualNormalCurveGuide$setContent(
                    plot_guide(
                        .("Guide: Observed distribution vs theoretical normal curve"),
                        .("compare the observed residual distribution with a theoretical normal curve."),
                        .("standardized residuals from the model."),
                        .("density, that is, the relative concentration of cases."),
                        .("an empirical curve centered near zero and similar to the normal bell curve."),
                        .("skewness, heavy tails, multiple peaks, or too many extreme cases."),
                        .("read classical tests cautiously and review outliers, transformation, or specification.")
                    )
                )

                self$results$scaleLocationGuide$setContent(
                    plot_guide(
                        .("Guide: Scale-location plot"),
                        .("evaluate whether residual dispersion remains stable."),
                        .("fitted or predicted values from the model."),
                        .("square root of the absolute standardized residuals."),
                        .("a horizontal band with similar dispersion across the X axis."),
                        .("an increasing trend, decreasing trend, or funnel shape."),
                        .("consider robust errors, transformation, or an alternative model.")
                    )
                )

                self$results$numericBoxplotsGuide$setContent(
                    plot_guide(
                        .("Guide: Numeric variable boxplots"),
                        .("detect univariate outliers in the dependent variable and numeric predictors."),
                        .("numeric variables included in the diagnostic."),
                        .("observed values of each variable on its original scale."),
                        .("compact boxes, reasonable whiskers, and few extreme points."),
                        .("many points outside the whiskers, strong skewness, or very distant values."),
                        .("review extreme values before interpreting multivariate influence or residuals.")
                    )
                )

                self$results$residualsLeverageGuide$setContent(
                    plot_guide(
                        .("Guide: Studentized residuals vs leverage"),
                        .("identify cases combining extreme residuals and high leverage."),
                        .("leverage, or the case distance from the center of the predictor space."),
                        .("studentized residual, which indicates how extreme the case error is."),
                        .("most cases near zero and with low leverage."),
                        .("cases with large studentized residuals, high leverage, or elevated Cook's D."),
                        .("review influential data points and compare sensitivity analyses.")
                    )
                )

                self$results$cooksDGuide$setContent(
                    plot_guide(
                        .("Guide: Cook's D by case"),
                        .("detect observations that may change model fit."),
                        .("case number or position in the data set."),
                        .("Cook's distance, an indicator of global influence."),
                        .("small bars below the reference line."),
                        .("cases with Cook's D above the visual threshold."),
                        .("check case plausibility and document analyses with and without influential cases.")
                    )
                )

                self$results$observedPredictedGuide$setContent(
                    plot_guide(
                        .("Guide: Observed vs predicted values"),
                        .("evaluate the correspondence between observed values and model predictions."),
                        .("values predicted by the model."),
                        .("observed values of the dependent variable."),
                        .("points close to the diagonal line of perfect prediction."),
                        .("wide dispersion, curved patterns, or systematic bias."),
                        .("review predictive ability, model specification, and possible additional terms.")
                    )
                )
            } else {
                self$results$diagnosticPlotsGuide$setContent("")
                self$results$residualsFittedGuide$setContent("")
                self$results$predictorEffectsGuide$setContent("")
                self$results$qqResidualsGuide$setContent("")
                self$results$residualHistogramGuide$setContent("")
                self$results$residualNormalCurveGuide$setContent("")
                self$results$scaleLocationGuide$setContent("")
                self$results$numericBoxplotsGuide$setContent("")
                self$results$residualsLeverageGuide$setContent("")
                self$results$cooksDGuide$setContent("")
                self$results$observedPredictedGuide$setContent("")
            }

            self$results$transformationsGuide$setContent(html_guide(.("Exploratory transformations"), "regression", "transformationsGuide"))

            trans_i <- 1
            trans_results <- list()

            add_transformation <- function(transformation, applicability, suggested_criterion,
                                           shapiro_p, ks_p, ad_p, jb_p, skew_p, kurt_p,
                                           bp_p, adj_r2, norm_eval, fit_eval) {

                trans_results[[trans_i]] <<- list(
                    transformation = transformation,
                    applicability = applicability,
                    suggestedCriterion = suggested_criterion,
                    shapiroP = clean_num(shapiro_p),
                    ksP = clean_num(ks_p),
                    adP = clean_num(ad_p),
                    jbP = clean_num(jb_p),
                    skewP = clean_num(skew_p),
                    kurtP = clean_num(kurt_p),
                    bpP = clean_num(bp_p),
                    adjR2 = clean_num(adj_r2),
                    normEval = norm_eval,
                    fitEval = fit_eval
                )

                trans_i <<- trans_i + 1
            }

            get_residual_diagnostics <- function(res_t, fit_t) {

                res_t <- res_t[!is.na(res_t)]

                shapiro_p <- tryCatch({
                    if (length(res_t) >= 3 && length(res_t) <= 5000)
                        stats::shapiro.test(res_t)$p.value
                    else
                        NA_real_
                }, error = function(e) NA_real_)

                ks_p <- tryCatch({
                    z <- (res_t - mean(res_t)) / stats::sd(res_t)
                    stats::ks.test(z, "pnorm")$p.value
                }, error = function(e) NA_real_)

                ad_p <- tryCatch({
                    z <- sort((res_t - mean(res_t)) / stats::sd(res_t))
                    n_ad <- length(z)
                    i <- seq_len(n_ad)
                    pz <- stats::pnorm(z)
                    pz <- pmin(pmax(pz, 1e-12), 1 - 1e-12)

                    a2 <- -n_ad - mean((2 * i - 1) *
                        (log(pz) + log(1 - rev(pz))))

                    a2c <- a2 * (1 + 0.75 / n_ad + 2.25 / n_ad^2)

                    if (a2c < 0.2) {
                        1 - exp(-13.436 + 101.14 * a2c - 223.73 * a2c^2)
                    } else if (a2c < 0.34) {
                        1 - exp(-8.318 + 42.796 * a2c - 59.938 * a2c^2)
                    } else if (a2c < 0.6) {
                        exp(0.9177 - 4.279 * a2c - 1.38 * a2c^2)
                    } else {
                        exp(1.2937 - 5.709 * a2c + 0.0186 * a2c^2)
                    }
                }, error = function(e) NA_real_)

                jb_p <- tryCatch({
                    n_jb <- length(res_t)
                    m <- mean(res_t)
                    ss <- stats::sd(res_t)
                    skew <- mean((res_t - m)^3) / ss^3
                    kurt <- mean((res_t - m)^4) / ss^4
                    jb_val <- n_jb / 6 * (skew^2 + ((kurt - 3)^2 / 4))
                    stats::pchisq(jb_val, df = 2, lower.tail = FALSE)
                }, error = function(e) NA_real_)

                skew_p <- tryCatch({
                    n_sk <- length(res_t)
                    m <- mean(res_t)
                    ss <- stats::sd(res_t)
                    skew <- mean((res_t - m)^3) / ss^3
                    z <- skew / sqrt(6 / n_sk)
                    2 * stats::pnorm(abs(z), lower.tail = FALSE)
                }, error = function(e) NA_real_)

                kurt_p <- tryCatch({
                    n_ku <- length(res_t)
                    m <- mean(res_t)
                    ss <- stats::sd(res_t)
                    kurt <- mean((res_t - m)^4) / ss^4
                    z <- (kurt - 3) / sqrt(24 / n_ku)
                    2 * stats::pnorm(abs(z), lower.tail = FALSE)
                }, error = function(e) NA_real_)

                bp_p <- tryCatch({
                    e2 <- stats::residuals(fit_t)^2
                    fv <- stats::fitted(fit_t)
                    bpAuxFit <- stats::lm(e2 ~ fv)
                    lm_val <- length(e2) * summary(bpAuxFit)$r.squared
                    stats::pchisq(lm_val, df = 1, lower.tail = FALSE)
                }, error = function(e) NA_real_)

                list(
                    shapiro_p = shapiro_p,
                    ks_p = ks_p,
                    ad_p = ad_p,
                    jb_p = jb_p,
                    skew_p = skew_p,
                    kurt_p = kurt_p,
                    bp_p = bp_p
                )
            }

            eval_transformed_model <- function(y_new, label, applicability,
                                               suggested_criterion) {

                if (any(is.na(y_new)) || length(y_new) != nrow(dat2)) {
                    add_transformation(
                        label, applicability, suggested_criterion,
                        NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                        NA_real_, NA_real_,
                        .("Not computed"), .("Not computed")
                    )
                    return(invisible(NULL))
                }

                transDat <- dat2
                transDat[[".y_trans_tmp"]] <- y_new

                f_trans <- stats::as.formula(paste(
                    ".y_trans_tmp ~",
                    paste(vapply(predictors, qname, character(1)), collapse = " + ")
                ))

                fit_t <- tryCatch(stats::lm(f_trans, data = transDat), error = function(e) NULL)

                if (is.null(fit_t)) {
                    add_transformation(
                        label, applicability, suggested_criterion,
                        NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                        NA_real_, NA_real_,
                        .("Not computed"), .("Not computed")
                    )
                    return(invisible(NULL))
                }

                res_t <- tryCatch(stats::residuals(fit_t), error = function(e) NA_real_)
                adj_r2_t <- tryCatch(summary(fit_t)$adj.r.squared, error = function(e) NA_real_)

                diag <- get_residual_diagnostics(res_t, fit_t)

                normality_ps <- c(
                    diag$shapiro_p,
                    diag$ks_p,
                    diag$ad_p,
                    diag$jb_p,
                    diag$skew_p,
                    diag$kurt_p
                )

                normality_total <- sum(!is.na(normality_ps))
                normality_ok <- sum(!is.na(normality_ps) & normality_ps >= .05)
                variance_ok <- !is.na(diag$bp_p) && diag$bp_p >= .05

                norm_eval <- if (label == "Original") {
                    .("Reference")
                } else if (normality_total > 0 &&
                           normality_ok >= ceiling(normality_total / 2)) {
                    .("Possible improvement")
                } else {
                    .("No clear improvement")
                }

                fit_eval <- if (label == "Original") {
                    .("Reference")
                } else if (variance_ok) {
                    .("Compatible with constant variance")
                } else {
                    .("Review residual variance")
                }

                add_transformation(
                    label,
                    applicability,
                    suggested_criterion,
                    diag$shapiro_p,
                    diag$ks_p,
                    diag$ad_p,
                    diag$jb_p,
                    diag$skew_p,
                    diag$kurt_p,
                    diag$bp_p,
                    adj_r2_t,
                    norm_eval,
                    fit_eval
                )
            }

            y_original <- dat2[[dep]]

            eval_transformed_model(
                y_original,
                "Original",
                .("Always applicable"),
                .("Reference model")
            )

            if (all(y_original > 0, na.rm = TRUE)) {
                eval_transformed_model(
                    log(y_original),
                    paste0("log(", dep, ")"),
                    sprintf(.("Applicable: %s > 0"), dep),
                    .("Marked/severe positive skewness")
                )
            } else {
                add_transformation(
                    paste0("log(", dep, ")"),
                    paste0("No aplicable: ", dep, " debe ser > 0"),
                    .("Marked/severe positive skewness"),
                    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                    NA_real_, NA_real_,
                    .("Not computed"), .("Not computed")
                )
            }

            if (all(y_original >= 0, na.rm = TRUE)) {
                eval_transformed_model(
                    sqrt(y_original),
                    paste0("sqrt(", dep, ")"),
                    sprintf(.("Applicable: %s >= 0"), dep),
                    .("Mild/moderate positive skewness")
                )
            } else {
                add_transformation(
                    paste0("sqrt(", dep, ")"),
                    paste0("No aplicable: ", dep, " debe ser >= 0"),
                    .("Mild/moderate positive skewness"),
                    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                    NA_real_, NA_real_,
                    .("Not computed"), .("Not computed")
                )
            }

            if (all(y_original != 0, na.rm = TRUE)) {
                eval_transformed_model(
                    1 / y_original,
                    paste0("1/", dep),
                    sprintf(.("Applicable: %s != 0"), dep),
                    .("Very severe positive skewness")
                )
            } else {
                add_transformation(
                    paste0("1/", dep),
                    paste0("No aplicable: ", dep, " no debe contener ceros"),
                    .("Very severe positive skewness"),
                    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                    NA_real_, NA_real_,
                    .("Not computed"), .("Not computed")
                )
            }

            transformation_value <- function(index, field, digits = 3) {
                if (length(trans_results) < index)
                    return(.("Not computed"))

                value <- trans_results[[index]][[field]]

                if (is.null(value))
                    return(.("Not computed"))

                if (is.numeric(value)) {
                    value <- clean_num(value)

                    if (is.na(value))
                        return(.("Not computed"))

                    if (field %in% c("shapiroP", "ksP", "adP", "jbP", "skewP", "kurtP", "bpP")) {
                        if (value < .001)
                            return("<.001")

                        return(sub("^0", "", format(round(value, digits), nsmall = digits)))
                    }

                    return(format(round(value, digits), nsmall = digits))
                }

                as.character(value)
            }

            add_transformation_row <- function(table, key, criterion, field, digits = 3) {
                add_table_row(
                    table,
                    key,
                    list(
                        criterion = criterion,
                        original = transformation_value(1, field, digits),
                        logTrans = transformation_value(2, field, digits),
                        sqrtTrans = transformation_value(3, field, digits),
                        inverseTrans = transformation_value(4, field, digits)
                    )
                )
            }

            add_transformation_row(
                self$results$transformationNormality,
                "norm_applicability",
                .("Applicability"),
                "applicability"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_criterion",
                .("Suggested criterion"),
                "suggestedCriterion"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_shapiro",
                "Shapiro-Wilk p",
                "shapiroP"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_ks",
                "Kolmogorov-Smirnov p",
                "ksP"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_ad",
                "Anderson-Darling p",
                "adP"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_jb",
                "Jarque-Bera p",
                "jbP"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_skew",
                "Skewness p",
                "skewP"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_kurt",
                "Kurtosis p",
                "kurtP"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_eval",
                .("Evaluation"),
                "normEval"
            )

            add_transformation_row(
                self$results$transformationFit,
                "fit_applicability",
                .("Applicability"),
                "applicability"
            )

            add_transformation_row(
                self$results$transformationFit,
                "fit_criterion",
                .("Suggested criterion"),
                "suggestedCriterion"
            )

            add_transformation_row(
                self$results$transformationFit,
                "fit_bp",
                "Breusch-Pagan p",
                "bpP"
            )

            add_transformation_row(
                self$results$transformationFit,
                "fit_adj_r2",
                .("Adjusted R²"),
                "adjR2"
            )

            add_transformation_row(
                self$results$transformationFit,
                "fit_eval",
                .("Evaluation"),
                "fitEval"
            )

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Robust and resampling options.
            # ES: Opciones robustas y de remuestreo
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            self$results$robustOptionsGuide$setContent(html_guide(.("Robust and resampling options"), "regression", "robustOptionsGuide"))

            robust_text <- character()

            if (exists("homo_problem") && isTRUE(homo_problem)) {
                robust_text <- c(
                    robust_text,
                    .("Nonconstant residual variance. One or more tests suggest heteroscedasticity or changes in residual variance. This does not automatically invalidate the model, but it may affect standard errors, confidence intervals and p-values. Consider HC3 robust standard errors, transforming the dependent variable or an alternative model if the conclusion changes.")
                )
            }

            if (exists("linearity_problem") && isTRUE(linearity_problem)) {
                robust_text <- c(
                    robust_text,
                    .("Possible nonlinearity. At least one diagnostic suggests that the relationship between predictors and the outcome may not be fully linear. Review residual plots and consider polynomial terms, transformations, splines or a nonlinear model if the curvature is substantively meaningful.")
                )
            }

            if (exists("influence_problem") && isTRUE(influence_problem)) {
                robust_text <- c(
                    robust_text,
                    .("Outlying or influential cases. At least one case activated numerical influence or outlier criteria. The recommendation is not automatic deletion, but reviewing the original value, documenting its plausibility and comparing sensitivity analyses with and without those cases.")
                )
            }

            if (length(robust_text) == 0) {
                robust_text <- c(
                    robust_text,
                    .("No strong signals were detected that force abandoning the classical linear model. Even so, the decision should consider plots, study design, sample size and the substantive meaning of the coefficients.")
                )
            }

            self$results$robustOptionsText$setContent(
                html_block(
                    .("Methodological interpretation of robust options"),
                    robust_text
                )
            )

            recommendation <- .("The linear regression model can be interpreted cautiously, while reviewing the reported diagnostics.")

            if (homo_problem) {
                recommendation <- .("There is evidence of nonconstant residual variance; consider robust standard errors, transforming the dependent variable, or using an alternative model.")
            }

            if (auto_problem) {
                recommendation <- .("There are signs of autocorrelation or residual patterning; consider temporal structure, HAC/Newey-West errors, or GLS models.")
            }

            if (severe_col_problem) {
                recommendation <- .("There is severe multicollinearity; review redundant predictors before interpreting coefficients.")
            }

            # Residual non-normality mainly threatens the precision of classical
            # inference (CIs, p-values on coefficients), not the point estimates
            # themselves, and the Central Limit Theorem makes that threat fade as
            # n grows - the same n>=30 convention groupCheck uses for its own
            # normality-driven branch. Below that threshold, flag it explicitly
            # rather than leaving normality_problem computed but unused.
            # ES: la no-normalidad residual amenaza principalmente la precisión
            # de la inferencia clásica (IC, p de los coeficientes), no las
            # estimaciones puntuales, y el Teorema Central del Límite atenúa esa
            # amenaza a medida que crece n - la misma convención n>=30 que usa
            # groupCheck en su propia rama de normalidad. Por debajo de ese
            # umbral, señalarlo explícitamente en vez de dejar normality_problem
            # calculada pero sin uso.
            normality_small_n_problem <- normality_problem && n_used < 30

            if (normality_small_n_problem && !homo_problem && !auto_problem &&
                !severe_col_problem) {
                recommendation <- .("There is evidence of non-normal residuals in a small sample, where the Central Limit Theorem cannot yet be relied on; consider a bootstrap confidence interval, a nonparametric alternative, or reporting classical inference with explicit caution.")
            }

            if (influence_problem && !homo_problem && !auto_problem &&
                !severe_col_problem && !normality_small_n_problem) {
                recommendation <- .("There are outlying or influential cases; inspect them before making decisions about the model.")
            }

            self$results$notes$setContent(
                html_block(
                    .("Notes and recommendation"),
                    c(
                        sprintf(.("Suggested decision: %s"), recommendation),
                        .("Significance codes: * p < .05, ** p < .01, *** p < .001."),
                        .("Statistical symbols are kept in international format: n, R², p, df, VIF.")
                    )
                )

            )

        },

        .requirePlotData = function(image) {
            . <- .al_lang(self$options)$tr
            if (!isTRUE(self$options$showPlots))
                return(FALSE)

            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                image$setError(.("The ggplot2 package is required to draw diagnostic plots."))
                return(FALSE)
            }

            # image$state, not private$.plotData, is the export-safe check
            # (see the setState() comment in .run()).
            # ES: image$state, no private$.plotData, es la verificación
            # segura para exportación (ver el comentario de setState() en
            # .run()).
            if (is.null(image$state)) {
                image$setError(.("No diagnostic plot data are available."))
                return(FALSE)
            }

            TRUE
        },

        .requireCorrPlotData = function(image) {
            . <- .al_lang(self$options)$tr
            if (!isTRUE(self$options$showPlots))
                return(FALSE)

            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                image$setError(.("The ggplot2 package is required to draw diagnostic plots."))
                return(FALSE)
            }

            if (is.null(image$state) || length(image$state$corrPairResults) == 0) {
                image$setError(.("Not enough numeric variables are available for this plot."))
                return(FALSE)
            }

            TRUE
        },


        # Plot theme/palette migrated to jamovi's native ggtheme/theme
        # mechanism - see logcheck.b.R's .plotColors()/.plotSeriesColors()
        # (the pilot for this change, 2026-09-17) for the full rationale,
        # and anovacheck.b.R's .plotThemeExtra() for the "small adjustments
        # layered after ggtheme" pattern used here too.
        # ES: Tema/paleta de gráficos migrado al mecanismo nativo
        # ggtheme/theme de jamovi - ver .plotColors()/.plotSeriesColors()
        # de logcheck.b.R (el piloto de este cambio, 2026-09-17) para el
        # razonamiento completo, y .plotThemeExtra() de anovacheck.b.R
        # para el patrón de "ajustes pequeños colocados después de
        # ggtheme" usado también aquí.
        .plotColors = function(theme) {
            base_color <- if (!is.null(theme$color) && length(theme$color) >= 1) theme$color[1] else "#333333"
            accent_color <- if (!is.null(theme$color) && length(theme$color) >= 2) theme$color[2] else "#2C7FB8"
            accent_fill <- if (!is.null(theme$fill) && length(theme$fill) >= 2) theme$fill[2] else "#A6CEE3"

            list(
                point = base_color, line = base_color, ref = "gray50",
                alert = accent_color, smooth = accent_color, fill = accent_fill
            )
        },

        .plotSeriesColors = function() {
            choice <- tryCatch(self$options$plotPalette, error = function(e) "jamovi")
            if (is.null(choice) || length(choice) == 0 || !nzchar(choice) || identical(choice, "jamovi"))
                return(NULL)
            .al_plot_series_palette(choice)
        },

        .plotThemeExtra = function() {
            ggplot2::theme(
                plot.title = ggplot2::element_blank(),
                plot.subtitle = ggplot2::element_text(size = 9.5),
                axis.title = ggplot2::element_text(size = 9.5),
                axis.text = ggplot2::element_text(size = 8.5),
                legend.title = ggplot2::element_text(size = 9),
                legend.text = ggplot2::element_text(size = 8.5),
                legend.position = "bottom",
                panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "grey90"),
                plot.margin = ggplot2::margin(4, 6, 4, 6)
            )
        },

        .addSmoother = function(plot, x, y, smoother = "loess", showBand = FALSE, pal) {
            if (identical(smoother, "loess")) {
                plot + ggplot2::geom_smooth(
                    ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                    method = "loess",
                    se = isTRUE(showBand),
                    linewidth = 0.6,
                    color = pal$smooth,
                    fill = pal$smooth
                )
            } else if (identical(smoother, "linear")) {
                plot + ggplot2::geom_smooth(
                    ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                    method = "lm",
                    se = isTRUE(showBand),
                    linewidth = 0.6,
                    color = pal$smooth,
                    fill = pal$smooth
                )
            } else {
                plot
            }
        },

        .labelPlotCases = function(d, p_count = NULL) {
            label_mode <- tryCatch(
                self$options$influenceLabelMode,
                error = function(e) "top5"
            )

            if (is.null(label_mode) || length(label_mode) == 0 || !nzchar(label_mode))
                label_mode <- "top5"

            if (identical(label_mode, "none"))
                return(d[FALSE, , drop = FALSE])

            if (is.null(d) || nrow(d) == 0)
                return(d[FALSE, , drop = FALSE])

            if (!("case" %in% names(d)))
                d$case <- seq_len(nrow(d))

            if (!("cooksD" %in% names(d)))
                d$cooksD <- NA_real_

            if (!("leverage" %in% names(d)))
                d$leverage <- NA_real_

            if (!("studResidual" %in% names(d)))
                d$studResidual <- NA_real_

            n_plot <- max(1, nrow(d))
            if (is.null(p_count) || !is.finite(p_count) || p_count < 1)
                p_count <- max(1, length(self$options$covs) + length(self$options$factors) + 1)

            cook_cut <- 4 / n_plot
            lev_cut <- 2 * p_count / n_plot
            stud_cut <- 3

            keep <- rep(FALSE, n_plot)

            if (identical(label_mode, "cooks")) {
                keep <- is.finite(d$cooksD) & d$cooksD > cook_cut

            } else if (identical(label_mode, "leverage")) {
                keep <- is.finite(d$leverage) & d$leverage > lev_cut

            } else if (identical(label_mode, "studres")) {
                keep <- is.finite(d$studResidual) & abs(d$studResidual) > stud_cut

            } else if (identical(label_mode, "top5")) {
                score <- rep(0, n_plot)
                score <- score + ifelse(is.finite(d$cooksD), d$cooksD, 0)
                score <- score + ifelse(is.finite(d$leverage), d$leverage, 0)
                score <- score + ifelse(is.finite(d$studResidual), abs(d$studResidual) / 10, 0)

                ord <- order(score, decreasing = TRUE, na.last = TRUE)
                ord <- ord[seq_len(min(5, length(ord)))]
                keep[ord] <- score[ord] > 0
            }

            out <- d[keep, , drop = FALSE]

            if (nrow(out) == 0)
                return(out)

            out$caseLabel <- as.character(out$case)
            out
        },

        .plotResidualsFitted = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state$plotData
            d <- d[is.finite(d$fitted) & is.finite(d$residual), , drop = FALSE]

            show_ref <- tryCatch(isTRUE(self$options$linRefLine), error = function(e) TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = residual))

            if (show_ref) {
                plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                      color = pal$ref)
            }

            plot <- plot +
                ggplot2::geom_point(alpha = 0.75, size = 1.6,
                                    color = pal$point) +
                ggplot2::labs(
                    x = .("Fitted values"),
                    y = .("Residuals")
                ) +
                ggtheme + private$.plotThemeExtra()

            lin_smoother <- tryCatch(self$options$linSmoother, error = function(e) "loess")
            lin_band <- tryCatch(isTRUE(self$options$linShowBand), error = function(e) FALSE)
            plot <- private$.addSmoother(plot, "fitted", "residual", lin_smoother, lin_band, pal)
            print(plot)
        },

        .plotPredictorEffects = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state

            if (is.null(d) || nrow(d) == 0) {
                image$setError(.("No numeric predictor effects are available."))
                return()
            }

            d <- d[is.finite(d$predictorZ) & is.finite(d$predicted), , drop = FALSE]

            if (nrow(d) == 0) {
                image$setError(.("No finite predictor effects are available."))
                return()
            }

            series_colors <- private$.plotSeriesColors()
            plot <- ggplot2::ggplot(
                    d,
                    ggplot2::aes(x = predictorZ, y = predicted, color = predictor)
                ) +
                ggplot2::geom_line(linewidth = 0.8) +
                ggplot2::geom_vline(
                    xintercept = 0,
                    linetype = "dashed",
                    color = pal$ref
                ) +
                ggplot2::labs(
                    x = .("Predictor value in standard deviations"),
                    y = .("Predicted dependent variable"),
                    color = .("Predictor")
                ) +
                ggtheme + private$.plotThemeExtra()

            if (!is.null(series_colors)) {
                predictors <- unique(as.character(d$predictor))
                colors <- rep(series_colors, length.out = max(1, length(predictors)))
                plot <- plot + ggplot2::scale_color_manual(values = stats::setNames(colors, predictors))
            }

            print(plot)
        },

        .plotQQResiduals = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state$plotData
            d <- d[is.finite(d$stdResidual), , drop = FALSE]
            n <- nrow(d)

            show_band <- tryCatch(isTRUE(self$options$normQQBand), error = function(e) TRUE)
            flag_outliers <- tryCatch(isTRUE(self$options$normFlagOutliers), error = function(e) TRUE)

            d <- d[order(d$stdResidual), , drop = FALSE]
            d$theoretical <- stats::qnorm(stats::ppoints(n))
            d$isExtreme <- flag_outliers & is.finite(d$stdResidual) & abs(d$stdResidual) > 2.5

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = theoretical, y = stdResidual))

            if (show_band && n >= 3) {
                p <- stats::ppoints(n)
                se <- sqrt(p * (1 - p) / n) / stats::dnorm(d$theoretical)
                band <- data.frame(
                    theoretical = d$theoretical,
                    lower = d$theoretical - stats::qnorm(0.975) * se,
                    upper = d$theoretical + stats::qnorm(0.975) * se
                )
                plot <- plot + ggplot2::geom_ribbon(
                    data = band,
                    ggplot2::aes(x = theoretical, ymin = lower, ymax = upper),
                    inherit.aes = FALSE,
                    alpha = 0.15,
                    fill = pal$line
                )
            }

            plot <- plot +
                ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.6,
                                      color = pal$line) +
                ggplot2::geom_point(
                    ggplot2::aes(color = isExtreme),
                    alpha = 0.75, size = 1.6, show.legend = FALSE
                ) +
                ggplot2::labs(
                    x = .("Theoretical quantiles"),
                    y = .("Standardized residuals")
                ) +
                ggtheme + private$.plotThemeExtra()

            # scale_color_manual must be added AFTER ggtheme - ggtheme carries
            # its own discrete colour scale, and whichever scale is added
            # last wins for a given aesthetic. This flag is always the
            # base/accent pair, regardless of plotPalette choice.
            # ES: scale_color_manual debe agregarse DESPUÉS de ggtheme -
            # ggtheme trae su propia escala discreta de color, y para una
            # misma estética gana la escala agregada al final. Este flag
            # siempre usa el par base/acento, sin importar plotPalette.
            plot <- plot + ggplot2::scale_color_manual(values = c(
                "FALSE" = pal$point,
                "TRUE" = pal$alert
            ))

            print(plot)
        },

        .plotResidualHistogram = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state$plotData
            d <- d[is.finite(d$stdResidual), , drop = FALSE]

            bin_method <- tryCatch(self$options$normHistBins, error = function(e) "sturges")
            n_bins <- tryCatch({
                switch(bin_method,
                    scott = grDevices::nclass.scott(d$stdResidual),
                    fd = grDevices::nclass.FD(d$stdResidual),
                    grDevices::nclass.Sturges(d$stdResidual)
                )
            }, error = function(e) 30)
            n_bins <- max(5, n_bins)

            show_curve <- tryCatch(isTRUE(self$options$normCurveOverlay), error = function(e) TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = stdResidual)) +
                ggplot2::geom_histogram(bins = n_bins, alpha = 0.85,
                                          fill = pal$fill,
                                          color = pal$line) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                                    color = pal$ref)

            if (show_curve && nrow(d) >= 3) {
                bin_width <- diff(range(d$stdResidual)) / n_bins
                mu <- mean(d$stdResidual)
                sigma <- stats::sd(d$stdResidual)
                plot <- plot + ggplot2::stat_function(
                    fun = function(x) stats::dnorm(x, mean = mu, sd = sigma) * nrow(d) * bin_width,
                    color = pal$smooth,
                    linewidth = 0.7
                )
            }

            plot <- plot +
                ggplot2::labs(
                    x = .("Standardized residuals"),
                    y = .("Count")
                ) +
                ggtheme + private$.plotThemeExtra()

            print(plot)
        },

        .plotResidualNormalCurve = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state$plotData
            d <- d[is.finite(d$stdResidual), , drop = FALSE]

            if (nrow(d) < 3) {
                image$setError(.("At least three residuals are required for this plot."))
                return()
            }

            x_min <- min(-4, min(d$stdResidual, na.rm = TRUE))
            x_max <- max(4, max(d$stdResidual, na.rm = TRUE))
            x_grid <- seq(x_min, x_max, length.out = 300)

            normal_df <- data.frame(
                stdResidual = x_grid,
                density = stats::dnorm(x_grid, mean = 0, sd = 1),
                curve = .("Theoretical normal N(0, 1)"),
                stringsAsFactors = FALSE
            )

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = stdResidual)) +
                ggplot2::geom_density(
                    linewidth = 0.8,
                    color = pal$smooth
                ) +
                ggplot2::geom_line(
                    data = normal_df,
                    ggplot2::aes(x = stdResidual, y = density, linetype = curve),
                    linewidth = 0.6,
                    color = pal$alert
                ) +
                ggplot2::geom_vline(
                    xintercept = 0,
                    linetype = "dashed",
                    color = pal$ref
                ) +
                ggplot2::scale_linetype_manual(values = "dashed", name = NULL) +
                ggplot2::labs(
                    x = .("Standardized residuals"),
                    y = .("Density")
                ) +
                ggtheme + private$.plotThemeExtra()

            print(plot)
        },

        .plotScaleLocation = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state$plotData
            d <- d[is.finite(d$fitted) & is.finite(d$absStdResidual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = absStdResidual)) +
                ggplot2::geom_point(alpha = 0.75, size = 1.6,
                                    color = pal$point) +
                ggplot2::labs(
                    x = .("Fitted values"),
                    y = .("sqrt(|standardized residuals|)")
                ) +
                ggtheme + private$.plotThemeExtra()

            plot <- private$.addSmoother(
                plot, "fitted", "absStdResidual",
                tryCatch(self$options$homoSmoother, error = function(e) "loess"),
                tryCatch(isTRUE(self$options$homoShowBand), error = function(e) FALSE),
                pal
            )
            print(plot)
        },

        .plotNumericBoxplots = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state

            if (is.null(d) || nrow(d) == 0) {
                image$setError(.("No numeric variables are available for boxplots."))
                return()
            }

            d <- d[is.finite(d$value), , drop = FALSE]

            if (nrow(d) == 0) {
                image$setError(.("No finite numeric values are available for boxplots."))
                return()
            }

            split_values <- split(d$value, d$variable)

            span_df <- do.call(
                rbind,
                lapply(names(split_values), function(v) {
                    x <- split_values[[v]]
                    x <- x[is.finite(x)]

                    if (length(x) == 0) {
                        return(data.frame(
                            variable = v,
                            min = NA_real_,
                            max = NA_real_,
                            iqr = NA_real_,
                            stringsAsFactors = FALSE
                        ))
                    }

                    data.frame(
                        variable = v,
                        min = min(x, na.rm = TRUE),
                        max = max(x, na.rm = TRUE),
                        iqr = stats::IQR(x, na.rm = TRUE),
                        stringsAsFactors = FALSE
                    )
                })
            )

            span_df$span <- span_df$max - span_df$min

            pos_span <- span_df$span[is.finite(span_df$span) & span_df$span > 0]
            pos_iqr  <- span_df$iqr[is.finite(span_df$iqr) & span_df$iqr > 0]

            similar_scale <- TRUE

            if (length(pos_span) > 1) {
                span_ratio <- max(pos_span) / max(min(pos_span), 1e-8)
                similar_scale <- span_ratio <= 3
            }

            if (similar_scale && length(pos_iqr) > 1) {
                iqr_ratio <- max(pos_iqr) / max(min(pos_iqr), 1e-8)
                similar_scale <- iqr_ratio <= 3
            }

            base_plot <- ggplot2::ggplot(d, ggplot2::aes(x = variable, y = value)) +
                ggplot2::geom_boxplot(
                    width = 0.65,
                    outlier.colour = pal$alert,
                    outlier.alpha = 0.85,
                    outlier.size = 1.6,
                    fill = pal$fill,
                    color = pal$line
                ) +
                ggtheme + private$.plotThemeExtra()

            if (similar_scale) {
                plot <- base_plot +
                    ggplot2::labs(
                        title = .("Boxplots of dependent variable and numeric predictors"),
                        subtitle = .("Common Y scale for direct comparison across variables."),
                        x = .("Variable"),
                        y = .("Observed values")
                    ) +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
                    )
            } else {
                plot <- base_plot +
                    ggplot2::facet_wrap(
                        stats::as.formula("~ variable"),
                        scales = "free_y"
                    ) +
                    ggplot2::labs(
                        title = .("Boxplots of dependent variable and numeric predictors"),
                        subtitle = .("Free Y scales are used because the variables have notably different magnitudes."),
                        x = NULL,
                        y = .("Observed values in original scale")
                    ) +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        strip.text = ggplot2::element_text(face = "bold")
                    )
            }

            print(plot)
        },

        .plotResidualsLeverage = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state$plotData
            d <- d[is.finite(d$leverage) & is.finite(d$studResidual), , drop = FALSE]

            p_count <- image$state$pModel
            if (is.null(p_count) || !is.finite(p_count) || p_count < 1)
                p_count <- max(1, length(self$options$covs) + length(self$options$factors) + 1)
            lev_cut <- 2 * p_count / max(1, nrow(d))

            show_threshold <- tryCatch(isTRUE(self$options$influenceShowThreshold), error = function(e) TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = leverage, y = studResidual))

            if (show_threshold) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = c(-3, 0, 3), linetype = "dashed",
                                          color = pal$ref) +
                    ggplot2::geom_vline(xintercept = lev_cut, linetype = "dashed",
                                        color = pal$alert)
            } else {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                          color = pal$ref)
            }

            plot <- plot +
                ggplot2::geom_point(ggplot2::aes(size = cooksD), alpha = 0.75,
                                    color = pal$point) +
                ggplot2::scale_size_continuous(name = "Cook's D", range = c(1.2, 4)) +
                ggplot2::labs(
                    x = .("Leverage"),
                    y = .("Studentized residuals")
                ) +
                ggtheme + private$.plotThemeExtra()

            lab <- private$.labelPlotCases(d, image$state$pModel)

            if (!is.null(lab) && nrow(lab) > 0) {
                plot <- plot +
                    ggplot2::geom_text(
                        data = lab,
                        ggplot2::aes(
                            x = leverage,
                            y = studResidual,
                            label = caseLabel
                        ),
                        inherit.aes = FALSE,
                        size = 3,
                        hjust = -0.1,
                        vjust = -0.5,
                        check_overlap = TRUE,
                        color = pal$alert
                    )
            }

            print(plot)
        },

        .plotCooksD = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state$plotData
            d <- d[is.finite(d$case) & is.finite(d$cooksD), , drop = FALSE]
            cut <- 4 / max(1, nrow(d))
            show_threshold <- tryCatch(isTRUE(self$options$influenceShowThreshold), error = function(e) TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = case, y = cooksD)) +
                ggplot2::geom_col(alpha = 0.85,
                                    fill = pal$fill,
                                    color = pal$line)

            if (show_threshold) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = cut, linetype = "dashed",
                                        color = pal$alert)
            }

            plot <- plot +
                ggplot2::labs(
                    x = .("Case"),
                    y = .("Cook's D")
                ) +
                ggtheme + private$.plotThemeExtra()

            lab <- private$.labelPlotCases(d, image$state$pModel)

            if (!is.null(lab) && nrow(lab) > 0) {
                plot <- plot +
                    ggplot2::geom_text(
                        data = lab,
                        ggplot2::aes(
                            x = case,
                            y = cooksD,
                            label = caseLabel
                        ),
                        inherit.aes = FALSE,
                        size = 3,
                        vjust = -0.6,
                        check_overlap = TRUE,
                        color = pal$alert
                    )
            }

            print(plot)
        },

        .plotCorrelationIndividual = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requireCorrPlotData(image))
                return()

            pal <- private$.plotColors(theme)
            pr <- image$state$corrPairResults
            dat <- image$state$corrData
            show_fit <- tryCatch(isTRUE(self$options$corrIndividualFit), error = function(e) TRUE)
            highlight <- tryCatch(isTRUE(self$options$corrHighlightDiscordant), error = function(e) TRUE)

            z <- function(v) {
                v <- as.numeric(v)
                s <- stats::sd(v, na.rm = TRUE)
                if (!is.finite(s) || s == 0) return(v - mean(v, na.rm = TRUE))
                (v - mean(v, na.rm = TRUE)) / s
            }

            fmt2 <- function(x) {
                if (!is.finite(x)) return("NA")
                sprintf("%.2f", x)
            }

            rows <- lapply(pr, function(p) {
                if (is.null(dat[[p$v1]]) || is.null(dat[[p$v2]]))
                    return(NULL)
                gap <- p$dcor - abs(p$pearsonR)
                discordant <- isTRUE(is.finite(gap) && gap > 0.10)
                data.frame(
                    pairLabel = sprintf(
                        "%s \u2013 %s  (r=%s, dCor=%s)",
                        p$v1, p$v2, fmt2(p$pearsonR), fmt2(p$dcor)
                    ),
                    x = z(dat[[p$v1]]),
                    y = z(dat[[p$v2]]),
                    discordant = discordant,
                    stringsAsFactors = FALSE
                )
            })
            rows <- rows[!vapply(rows, is.null, logical(1))]

            if (length(rows) == 0) {
                image$setError(.("Not enough data to draw this plot."))
                return()
            }

            d <- do.call(rbind, rows)
            d$discordant <- factor(
                if (highlight) d$discordant else rep(FALSE, nrow(d)),
                levels = c(FALSE, TRUE)
            )

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
                ggplot2::geom_point(
                    ggplot2::aes(color = discordant),
                    alpha = 0.7, size = 1.4, show.legend = highlight
                )

            if (show_fit) {
                plot <- plot +
                    ggplot2::geom_smooth(
                        method = "lm", se = FALSE, linewidth = 0.55, linetype = "dashed",
                        color = pal$line
                    ) +
                    ggplot2::geom_smooth(
                        method = "loess", se = FALSE, linewidth = 0.6,
                        color = pal$smooth
                    )
            }

            plot <- plot +
                ggplot2::facet_wrap(~pairLabel, scales = "free") +
                ggplot2::labs(
                    x = .("Standardized value (variable 1)"),
                    y = .("Standardized value (variable 2)")
                ) +
                ggtheme + private$.plotThemeExtra() +
                ggplot2::theme(strip.text = ggplot2::element_text(size = 8))

            # scale_color_manual must be added AFTER ggtheme - ggtheme carries
            # its own discrete colour scale, and whichever scale is added
            # last wins for a given aesthetic. This flag is always the
            # base/accent pair, regardless of plotPalette choice.
            # ES: scale_color_manual debe agregarse DESPUÉS de ggtheme -
            # ggtheme trae su propia escala discreta de color, y para una
            # misma estética gana la escala agregada al final. Este flag
            # siempre usa el par base/acento, sin importar plotPalette.
            plot <- plot + ggplot2::scale_color_manual(
                values = c(
                    "FALSE" = pal$point,
                    "TRUE" = pal$alert
                ),
                labels = c(
                    "FALSE" = .("Concordant"),
                    "TRUE" = .("Notable gap")
                ),
                name = .("Pearson/dCor gap")
            )

            print(plot)
        },

        .plotCorrelationComparative = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requireCorrPlotData(image))
                return()

            pal <- private$.plotColors(theme)
            pr <- image$state$corrPairResults
            style <- tryCatch(self$options$corrComparativeStyle, error = function(e) "dumbbell")
            highlight <- tryCatch(isTRUE(self$options$corrHighlightDiscordant), error = function(e) TRUE)

            rows <- lapply(pr, function(p) {
                gap <- p$dcor - abs(p$pearsonR)
                data.frame(
                    pairLabel = sprintf("%s \u2013 %s", p$v1, p$v2),
                    pearson = abs(p$pearsonR),
                    dcor = p$dcor,
                    discordant = isTRUE(is.finite(gap) && gap > 0.10),
                    stringsAsFactors = FALSE
                )
            })
            d <- do.call(rbind, rows)
            d <- d[is.finite(d$pearson) & is.finite(d$dcor), , drop = FALSE]

            if (nrow(d) == 0) {
                image$setError(.("Not enough data to draw this plot."))
                return()
            }

            d <- d[order(d$dcor), , drop = FALSE]
            d$pairLabel <- factor(d$pairLabel, levels = d$pairLabel)
            if (!highlight) d$discordant <- FALSE

            # Pearson/dCor are two fixed semantic series (not a
            # user-varying group count), but still follow the same
            # jamovi-vs-override choice as every other multi-series plot:
            # by default no manual scale is set below, so ggtheme's own
            # 2-color discrete scale colors them; a non-default plotPalette
            # choice provides an explicit 2-color override instead.
            # ES: Pearson/dCor son dos series semánticas fijas (no una
            # cantidad de grupos que varía por usuario), pero igual siguen
            # la misma elección jamovi-vs-anulación que cualquier otro
            # gráfico multi-serie: por defecto no se fija ninguna escala
            # manual abajo, así que la propia escala discreta de 2 colores
            # de ggtheme las colorea; una elección no predeterminada de
            # plotPalette da en cambio una anulación explícita de 2 colores.
            series_colors <- private$.plotSeriesColors()
            two_colors <- if (!is.null(series_colors))
                c("Pearson |r|" = series_colors[1], "dCor" = if (length(series_colors) >= 2) series_colors[2] else pal$smooth)
            else
                NULL

            if (identical(style, "bar")) {
                long <- rbind(
                    data.frame(pairLabel = d$pairLabel, metric = "Pearson |r|", value = d$pearson),
                    data.frame(pairLabel = d$pairLabel, metric = "dCor", value = d$dcor)
                )
                plot <- ggplot2::ggplot(long, ggplot2::aes(x = pairLabel, y = value, fill = metric)) +
                    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        x = NULL, fill = NULL,
                        y = .("Coefficient magnitude")
                    ) +
                    ggtheme + private$.plotThemeExtra()

                if (!is.null(two_colors))
                    plot <- plot + ggplot2::scale_fill_manual(values = two_colors, name = NULL)
            } else {
                plot <- ggplot2::ggplot(d, ggplot2::aes(y = pairLabel)) +
                    ggplot2::geom_segment(
                        ggplot2::aes(x = pearson, xend = dcor, yend = pairLabel),
                        color = ifelse(d$discordant, pal$alert, pal$ref),
                        linewidth = ifelse(d$discordant, 1, 0.6)
                    ) +
                    ggplot2::geom_point(ggplot2::aes(x = pearson, color = "Pearson |r|"), size = 2.4) +
                    ggplot2::geom_point(ggplot2::aes(x = dcor, color = "dCor"), size = 2.4) +
                    ggplot2::labs(
                        x = .("Coefficient magnitude"),
                        y = NULL, color = NULL
                    ) +
                    ggtheme + private$.plotThemeExtra()

                if (!is.null(two_colors))
                    plot <- plot + ggplot2::scale_color_manual(values = two_colors, name = NULL)
            }

            print(plot)
        },

        .plotObservedPredicted = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image))
                return()

            pal <- private$.plotColors(theme)
            d <- image$state$plotData
            d <- d[is.finite(d$observed) & is.finite(d$fitted), , drop = FALSE]
            lims <- range(c(d$observed, d$fitted), na.rm = TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = observed)) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                                      color = pal$ref) +
                ggplot2::geom_point(alpha = 0.75, size = 1.6,
                                    color = pal$point) +
                ggplot2::coord_equal(xlim = lims, ylim = lims) +
                ggplot2::labs(
                    x = .("Predicted values"),
                    y = .("Observed values")
                ) +
                ggtheme + private$.plotThemeExtra()

            print(plot)
        }
    )
)
