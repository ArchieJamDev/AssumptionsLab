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
# ANOVA/ANCOVA.
# ES: ANOVA/ANCOVA.
#
# This file implements anovaCheck: an assumption-diagnostic module for
# between-groups ANOVA and ANCOVA designs (one dependent variable, one or
# more factors, and optional covariates). It reports the assumption
# diagnostics that decide whether an ANOVA/ANCOVA comparison of group means
# is trustworthy (residual normality, homogeneity of variances, homogeneity
# of regression slopes for ANCOVA, covariate linearity, multicollinearity
# among covariates, and outlying/influential cases), and recommends a
# specific test given the pattern of results.
#
# ES: Este archivo implementa anovaCheck: un módulo de diagnóstico de
# supuestos para diseños ANOVA y ANCOVA entre grupos (una variable
# dependiente, uno o más factores, y covariables opcionales). Reporta los
# diagnósticos de supuestos que deciden si una comparación de medias de
# grupo por ANOVA/ANCOVA es confiable (normalidad de residuos,
# homogeneidad de varianzas, homogeneidad de pendientes de regresión para
# ANCOVA, linealidad de covariables, multicolinealidad entre covariables, y
# casos atípicos/influyentes), y recomienda una prueba específica según el
# patrón de resultados.
#
# Responsibilities
# 1. Fit the ANOVA/ANCOVA model from the user's selected dependent
#    variable, factor(s), and covariate(s).
# 2. Compute and report the full assumption-diagnostic battery for the
#    fitted model (normality, homogeneity of variances, homogeneity of
#    slopes, covariate linearity, multicollinearity, outliers/influence).
# 3. Render the diagnostic plots (residuals, Q-Q, histograms, boxplots,
#    leverage, Cook's D), grouped by methodological purpose and
#    parametrized per the user's plot-set selection.
# 4. Assemble the applied-interpretation text for every diagnostic area,
#    in jamovi's language, and recommend a specific
#    test/alternative given the pattern of results.
#
# ES: Responsabilidades
# 1. Ajustar el modelo ANOVA/ANCOVA a partir de la variable dependiente,
#    factor(es) y covariable(s) seleccionados por el usuario.
# 2. Calcular y reportar la batería completa de diagnósticos de supuestos
#    para el modelo ajustado (normalidad, homogeneidad de varianzas,
#    homogeneidad de pendientes, linealidad de covariables,
#    multicolinealidad, atípicos/influencia).
# 3. Renderizar los gráficos diagnósticos (residuos, Q-Q, histogramas,
#    diagramas de caja, leverage, Cook's D), agrupados por propósito
#    metodológico y parametrizados según el conjunto de gráficos
#    seleccionado por el usuario.
# 4. Ensamblar el texto de interpretación aplicada para cada área
#    diagnóstica, en el idioma de jamovi, y
#    recomendar una prueba/alternativa específica según el patrón de
#    resultados.
#
# Workflow
# 1. Fit: build and estimate the ANOVA/ANCOVA model from the user's
#    variable selection.
# 2. Diagnose: compute residual normality, homogeneity of variances,
#    homogeneity of slopes (ANCOVA only), covariate linearity,
#    multicollinearity, and outlying/influential cases.
# 3. Plot: render the diagnostic plots belonging to the user's selected
#    plot set (basic/normality/influence/complete).
# 4. Interpret: build the applied-interpretation text for every diagnostic
#    area.
# 5. Recommend: assemble a dynamic recommendation (which test/alternative
#    fits the observed assumption pattern) into the notes section.
#
# ES: Flujo de trabajo
# 1. Ajustar: construir y estimar el modelo ANOVA/ANCOVA a partir de la
#    selección de variables del usuario.
# 2. Diagnosticar: calcular normalidad de residuos, homogeneidad de
#    varianzas, homogeneidad de pendientes (solo ANCOVA), linealidad de
#    covariables, multicolinealidad y casos atípicos/influyentes.
# 3. Graficar: renderizar los gráficos diagnósticos que pertenecen al
#    conjunto de gráficos seleccionado por el usuario
#    (básico/normalidad/influencia/completo).
# 4. Interpretar: construir el texto de interpretación aplicada para cada
#    área diagnóstica.
# 5. Recomendar: ensamblar una recomendación dinámica (qué prueba/
#    alternativa se ajusta al patrón de supuestos observado) en la
#    sección de notas.
# -----------------------------------------------------------------------------

anovaCheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "anovaCheckClass",
    inherit = anovaCheckBase,
    private = list(
        .plotData = NULL,
        .groupPlotData = NULL,

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
                            table$addColumn(name = colName, title = as.character(i), type = "number"),
                            error = function(e) NULL
                        )
                    }
                }
            }
            invisible(TRUE)
        },

        .run = function() {
            # Hide the fixed guides when "Explanations" is off (see
            # .al_apply_guides()). ES: oculta las guías fijas si está desactivado.
            on.exit(.al_apply_guides(self), add = TRUE)

            lang <- .al_lang(self$options)
            . <- lang$tr





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

            fmt_num <- function(x, digits = 4) {
                x <- clean_num(x)
                if (is.na(x))
                    return(.("Not computed"))
                format(round(x, digits), nsmall = digits)
            }

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
                gsub("_+", "_", x)
            }

            ul <- function(x) {
                chars <- strsplit(x, "", fixed = TRUE)[[1]]
                paste0(paste0(chars, "\u0332"), collapse = "")
            }

            wrap_text <- function(..., width = 96) {
                txt <- paste(..., collapse = "")

                # Convert literal \\n text inherited from previous patches into real line breaks.
                # ES: Convertir texto literal \\n heredado de parches previos a saltos reales.
                txt <- gsub("\\\\n", "\n", txt)
                txt <- gsub("\\\\t", " ", txt)

                txt <- gsub("[ \t\r]+", " ", txt)
                txt <- trimws(txt)

                if (is.na(txt) || txt == "")
                    return("")

                paste(strwrap(txt, width = width), collapse = "\n")
            }

            wrap_paragraphs <- function(x, width = 96) {
                x <- paste(x, collapse = "")

                # Convert literal \\n text inherited from previous patches into real line breaks.
                # ES: Convertir texto literal \\n heredado de parches previos a saltos reales.
                x <- gsub("\\\\n", "\n", x)
                x <- gsub("\\\\t", " ", x)

                # Normalize whitespace around line breaks while preserving blank lines.
                # ES: Normalizar espacios alrededor de saltos y conservar líneas vacías.
                x <- gsub("[ \t]+\n", "\n", x)
                x <- gsub("\n[ \t]+", "\n", x)

                parts <- unlist(strsplit(x, "\n[ \t]*\n", perl = TRUE))
                parts <- trimws(parts)
                parts <- parts[nzchar(parts)]

                parts <- vapply(
                    parts,
                    function(z) wrap_text(z, width = width),
                    character(1)
                )

                paste(parts, collapse = "\n\n")
            }

            html_escape <- .al_html_escape

            block96 <- function(...) {
                raw <- c(...)
                raw <- raw[!is.na(raw)]

                expand_arg <- function(z) {
                    z <- gsub("\r\n", "\n", z)
                    chunks <- unlist(strsplit(z, "\n[ \t]*\n", perl = TRUE))
                    chunks <- vapply(chunks, function(cc) {
                        cc <- gsub("\n", " ", cc)
                        gsub("[ \t]+", " ", trimws(cc))
                    }, character(1))
                    chunks[nzchar(chunks)]
                }

                parts <- unlist(lapply(raw, expand_arg))
                parts <- parts[nzchar(parts)]

                if (length(parts) == 0)
                    return("")

                heading_labels <- c(.("Quick guide"), .("Applied Interpretation"))

                p_style <- "margin: 0 0 0.55em 0; line-height: 1.35; text-align: justify;"
                h_style <- "margin: 0.35em 0 0.45em 0; line-height: 1.25;"
                div_style <- "max-width: 7.25in; width: 100%; box-sizing: border-box; page-break-inside: avoid; break-inside: avoid; text-align: justify;"

                rendered <- vapply(parts, function(pp) {
                    if (pp %in% heading_labels) {
                        paste0("<h3 style=\"", h_style, "\">", html_escape(pp), "</h3>")
                    } else {
                        paste0("<p style=\"", p_style, "\">", html_escape(pp), "</p>")
                    }
                }, character(1))

                paste0("<div style=\"", div_style, "\">\n",
                       paste(rendered, collapse = "\n"), "\n</div>")
            }

            # html_list_guide(): sibling of block96() for "Quick guide"
            # checklist-style content (short, independent tip sentences).
            # Reuses block96's own argument-collection logic (each `...`
            # argument becomes one part; the first part is the title), but
            # renders the remaining parts as a real bulleted <ul><li> via
            # .al_html_list() instead of one <p> per item - block96() never
            # produced actual bullets for any of its callers, guide or
            # interpretation alike, and had no text-align: justify anywhere,
            # unlike every other module's guide rendering (which already
            # goes through .al_html_block()/.al_html_list() in
            # shared-helpers.R). Only call sites whose content is genuinely
            # a short independent-tip checklist (matching the convention
            # already used for relatedCheck's "Brief guide" boxes) were
            # switched to this function; call sites with flowing narrative
            # prose (Applied Interpretation, Notes, and the
            # correlationComparisonGuide shared with logCheck/pathCheck,
            # which keeps its own topic title and paragraph structure for
            # consistency with those modules) were left as block96() calls,
            # now justified along with everything else.
            #
            # ES: hermana de block96() para contenido tipo checklist de
            # "Quick guide" (tips cortos e independientes). Reutiliza la
            # misma lógica de recolección de argumentos de block96 (cada
            # argumento de `...` se vuelve una parte; la primera parte es el
            # título), pero renderiza las partes restantes como una lista
            # <ul><li> real vía .al_html_list() en vez de un <p> por ítem -
            # block96() nunca produjo viñetas reales para ningún llamador,
            # guía o interpretación por igual, y no tenía text-align:
            # justify en ningún lado, a diferencia del resto de los módulos
            # (que ya pasan por .al_html_block()/.al_html_list() en
            # shared-helpers.R). Solo se cambiaron a esta función los sitios
            # cuyo contenido es genuinamente un checklist de tips cortos e
            # independientes (misma convención que ya usan las cajas "Brief
            # guide" de relatedCheck); los sitios con prosa narrativa
            # continua (Interpretación Aplicada, Notas, y
            # correlationComparisonGuide, compartida con logCheck/pathCheck,
            # que conserva su propio título de tema y estructura de párrafo
            # por consistencia con esos módulos) se dejaron como llamadas a
            # block96(), ahora justificadas junto con todo lo demás.
            html_list_guide <- function(...) {
                raw <- c(...)
                raw <- raw[!is.na(raw)]

                expand_arg <- function(z) {
                    z <- gsub("\r\n", "\n", z)
                    chunks <- unlist(strsplit(z, "\n[ \t]*\n", perl = TRUE))
                    chunks <- vapply(chunks, function(cc) {
                        cc <- gsub("\n", " ", cc)
                        gsub("[ \t]+", " ", trimws(cc))
                    }, character(1))
                    chunks[nzchar(chunks)]
                }

                parts <- unlist(lapply(raw, expand_arg))
                parts <- parts[nzchar(parts)]

                if (length(parts) == 0)
                    return("")

                title <- parts[1]
                items <- parts[-1]

                if (length(items) == 0)
                    return("")

                .al_html_block(title, .al_html_list(items), raw = TRUE)
            }

            plot_guide <- function(objective, x_axis, y_axis, expected, warning_txt, decision) {
                html_list_guide(
                    .("Quick guide"),
                    sprintf(.("Objective: %s"), objective),
                    sprintf(.("X axis: %s"), x_axis),
                    sprintf(.("Y axis: %s"), y_axis),
                    sprintf(.("Expected pattern: %s"), expected),
                    sprintf(.("Warning sign: %s"), warning_txt),
                    sprintf(.("Possible decision: %s"), decision)
                )
            }

            format_p <- function(p) {
                p <- clean_num(p)
                if (is.na(p))
                    return(.("not computed"))
                if (p < .001)
                    return("< .001")
                sub("^0", "", sprintf("%.3f", p))
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

            interpret_normality <- function(test, p_value, n, scope) {
                p_value <- clean_num(p_value)

                if (is.na(p_value))
                    return(sprintf(.("%s: not computed."), test))

                if (p_value < .05)
                    return(sprintf(.("%s: p = %s, n = %s. Significant deviation from normality."), test, format_p(p_value), n))

                sprintf(.("%s: p = %s, n = %s. Compatible with approximate normality."), test, format_p(p_value), n)
            }

            interpret_variance <- function(test, p_value, n, groups, family) {
                p_value <- clean_num(p_value)

                if (test == "Hartley Fmax")
                    return(.("Hartley Fmax: descriptive variance ratio. High values suggest checking heterogeneity."))

                if (is.na(p_value))
                    return(sprintf(.("%s: not computed."), test))

                if (p_value < .05)
                    return(sprintf(.("%s: p = %s, n = %s. Significant differences between variances."), test, format_p(p_value), n))

                sprintf(.("%s: p = %s, n = %s. Compatible with homogeneous variances."), test, format_p(p_value), n)
            }

            interpret_slope <- function(fac, cov, p_value, n) {
                p_value <- clean_num(p_value)

                if (fac == .("Not applicable"))
                    return(.("Not applicable: requires covariates."))

                if (is.na(p_value))
                    return(sprintf(.("%s × %s: not computed."), fac, cov))

                if (p_value < .05)
                    return(sprintf(.("%s × %s: p = %s. Different slopes between groups."), fac, cov, format_p(p_value)))

                sprintf(.("%s × %s: p = %s. Compatible with homogeneous slopes."), fac, cov, format_p(p_value))
            }

            interpret_covariate_linearity <- function(cov, test, p_value, n, extra_r = NA_real_, stat_value = NA_real_) {
                p_value <- clean_num(p_value)

                if (cov == .("Not applicable"))
                    return(.("Not applicable: no covariates were selected."))

                if (test == .("Distance correlation (dCor)")) {
                    dcor_val <- clean_num(stat_value)
                    pearson_r <- clean_num(extra_r)

                    if (is.na(dcor_val))
                        return(sprintf(.("%s on %s: not computed."), test, cov))

                    gap <- if (is.na(pearson_r)) NA_real_ else dcor_val - abs(pearson_r)

                    if (!is.na(gap) && gap > 0.10 && !is.na(p_value) && p_value < .05) {
                        return(sprintf(.("%s on %s: dCor = %s, Pearson r = %s. dCor detects dependence that Pearson's r does not fully capture; inspect a scatterplot and consider a non-linear term or transformation for this covariate."), test, cov, format(round(dcor_val, 3), nsmall = 3), format(round(pearson_r, 3), nsmall = 3)))
                    }

                    return(sprintf(.("%s on %s: dCor = %s. Consistent with the linear association already shown by Pearson's r; no additional non-linear structure is evident."), test, cov, format(round(dcor_val, 3), nsmall = 3)))
                }

                if (is.na(p_value))
                    return(sprintf(.("%s on %s: not computed."), test, cov))

                if (test == .("Bivariate correlation"))
                    return(sprintf(.("%s on %s: p = %s. Simple linear association."), test, cov, format_p(p_value)))

                if (p_value < .05)
                    return(sprintf(.("%s on %s: p = %s. Possible deviation from linearity."), test, cov, format_p(p_value)))

                sprintf(.("%s on %s: p = %s. Compatible with approximate linearity."), test, cov, format_p(p_value))
            }

            add_table_row <- function(table, key, values) {
                table$addRow(rowKey = key, values = values)
            }

            dep <- self$options$dep
            factors <- self$options$factors
            covs <- self$options$covs

            # Not configured yet (nothing selected): return quietly and leave the
            # empty results skeleton in place; reject() is kept for real
            # problems the user can fix.
            # ES: Aún sin configurar (nada seleccionado): volver en silencio y
            # dejar el esqueleto vacío; reject() queda para errores reales.
            if (is.null(dep) || dep == "" || length(factors) == 0)
                return()

            vars <- c(dep, factors, covs)
            dat <- self$data[, vars, drop = FALSE]
            complete <- stats::complete.cases(dat)
            dat2 <- dat[complete, , drop = FALSE]

            n_total <- nrow(dat)
            n_used <- nrow(dat2)
            n_excluded <- n_total - n_used

            if (n_used < 5) {
                jmvcore::reject(.("There are not enough complete cases to fit the model."))
            }

            for (f in factors)
                dat2[[f]] <- as.factor(dat2[[f]])

            factor_terms <- paste(vapply(factors, qname, character(1)), collapse = " + ")
            cov_terms <- if (length(covs) > 0)
                paste(vapply(covs, qname, character(1)), collapse = " + ")
            else
                ""

            rhs <- if (length(covs) > 0)
                paste(factor_terms, cov_terms, sep = " + ")
            else
                factor_terms

            formula_text <- paste(qname(dep), "~", rhs)
            model_formula <- stats::as.formula(formula_text)

            fit_error <- NULL
            fit <- tryCatch(stats::lm(model_formula, data = dat2), error = function(e) {
                fit_error <<- conditionMessage(e)
                NULL
            })

            if (is.null(fit)) {
                jmvcore::reject(sprintf(.("It was not possible to fit the ANOVA/ANCOVA model: %s"), fit_error))
            }

            residuals_raw <- tryCatch(stats::residuals(fit), error = function(e) NA_real_)
            fitted_values <- tryCatch(stats::fitted(fit), error = function(e) NA_real_)
            residuals_stud <- tryCatch(stats::rstudent(fit), error = function(e) NA_real_)
            leverage <- tryCatch(stats::hatvalues(fit), error = function(e) NA_real_)
            cooks_d <- tryCatch(stats::cooks.distance(fit), error = function(e) NA_real_)
            dffits <- tryCatch(stats::dffits(fit), error = function(e) NA_real_)

            p_model <- length(stats::coef(fit))

            private$.setResidualPlots(., plot_guide, n_used, fit, residuals_raw, fitted_values, residuals_stud, leverage, cooks_d, dffits, p_model)

            normality_problem <- FALSE
            variance_problem <- FALSE
            slopes_problem <- FALSE
            linearity_problem <- FALSE
            influence_problem <- FALSE
            design_problem <- FALSE
            dcor_problem <- FALSE

            normality_texts <- character()
            variance_texts <- character()
            slope_texts <- character()
            covlin_texts <- character()

            norm_sig_count <- 0L
            norm_total_count <- 0L

            recommendation <- ""

            cell_factor <- tryCatch({
                interaction(dat2[, factors, drop = FALSE], drop = TRUE, sep = " | ")
            }, error = function(e) NULL)

            if (is.null(cell_factor)) {
                self$results$intro$setContent(
                    .("It was not possible to build the design cells.")
                )
                return()
            }

            cell_n <- table(cell_factor)
            min_cell <- min(cell_n)
            max_cell <- max(cell_n)
            balance_ratio <- max_cell / max(1, min_cell)

            if (min_cell < 5 || balance_ratio > 2)
                design_problem <- TRUE

            y <- private$.fillDesign(., clean_num, fmt_num, html_escape, block96, html_list_guide, plot_guide, add_table_row, dep, factors, covs, dat2, n_total, n_used, n_excluded, formula_text, design_problem, cell_factor, cell_n, min_cell, max_cell, balance_ratio)


            normality_problem <- private$.checkNormality(., clean_num, p_sig, block96, html_list_guide, interpret_normality, add_table_row, residuals_raw, normality_texts, norm_sig_count, norm_total_count, normality_problem)

            variance_problem <- private$.checkHomogeneity(., clean_num, p_sig, block96, html_list_guide, interpret_variance, add_table_row, n_used, variance_texts, cell_factor, cell_n, y, variance_problem)

            slopes_problem <- private$.checkSlopes(., clean_num, p_sig, qname, block96, html_list_guide, interpret_slope, add_table_row, dep, factors, covs, dat2, n_used, rhs, fit, slope_texts, slopes_problem)

            step <- private$.checkCovariateLinearity(., clean_num, p_sig, qname, block96, html_list_guide, dcor_stat, dcor_pvalue, copentTest, interpret_covariate_linearity, add_table_row, dep, covs, dat2, n_used, rhs, covlin_texts, y, linearity_problem, dcor_problem)
            linearity_problem <- step$linearity_problem
            dcor_problem <- step$dcor_problem

            private$.checkMulticollinearity(., clean_num, block96, html_list_guide, add_table_row, covs, dat2, fit)

            private$.fillCorrelations(lang, ., p_sig, block96, html_list_guide, dcor_stat, dcor_pvalue, copentTest, dep, covs, dat2, n_used)

            influence_problem <- private$.checkInfluence(., clean_num, block96, html_list_guide, plot_guide, add_table_row, complete, n_used, residuals_raw, fitted_values, residuals_stud, leverage, cooks_d, dffits, p_model)


            private$.fillAlternatives(., html_list_guide, add_table_row, normality_problem, variance_problem, slopes_problem, linearity_problem, influence_problem, design_problem, dcor_problem)

            private$.setPlotVisibility(., block96, html_list_guide, normality_problem, variance_problem, slopes_problem, linearity_problem, influence_problem, design_problem, dcor_problem)
        },

        .setResidualPlots = function(., plot_guide, n_used, fit, residuals_raw, fitted_values, residuals_stud, leverage, cooks_d, dffits, p_model) {
            std_resid_anova <- tryCatch(
                stats::rstandard(fit),
                error = function(e) residuals_raw / stats::sd(residuals_raw, na.rm = TRUE)
            )

            private$.plotData <- data.frame(
                case = seq_len(n_used),
                fitted = as.numeric(fitted_values),
                residual = as.numeric(residuals_raw),
                stdResidual = as.numeric(std_resid_anova),
                studResidual = as.numeric(residuals_stud),
                absStdResidual = sqrt(abs(as.numeric(std_resid_anova))),
                leverage = as.numeric(leverage),
                cooksD = as.numeric(cooks_d),
                dffits = as.numeric(dffits),
                pModel = p_model,
                stringsAsFactors = FALSE
            )

            # jamovi's PDF/image export renders each Image's render function in a
            # fresh context (not the live results-panel instance), where private$
            # fields are already empty; setState() serializes the plot data into
            # the results tree instead, which does survive into that export
            # context. Without this, these plots render fine on screen but export
            # blank (jamovi review finding).
            # ES: La exportación a PDF/imagen de jamovi renderiza la función de
            # cada Image en un contexto nuevo (no la instancia en vivo del panel
            # de resultados), donde los campos private$ ya están vacíos;
            # setState() serializa los datos del gráfico en el árbol de
            # resultados, y ese sí sobrevive a ese contexto de exportación. Sin
            # esto, estos gráficos se ven bien en pantalla pero exportan en
            # blanco (hallazgo de la revisión de jamovi).
            self$results$residualsFittedPlot$setState(private$.plotData)
            self$results$qqResidualsPlot$setState(private$.plotData)
            self$results$residualHistogramPlot$setState(private$.plotData)
            self$results$residualNormalCurvePlot$setState(private$.plotData)
            self$results$residualsLeveragePlot$setState(private$.plotData)
            self$results$cooksDPlot$setState(private$.plotData)

            self$results$residualsFittedGuide$setContent(plot_guide(
                .("check for residual patterns not captured by group or cell means."),
                .("fitted values (group/cell means, adjusted for covariates) for each case."),
                .("residuals, the difference between the observed and fitted value."),
                .("a random cloud around zero, without curvature or funnel shape."),
                .("curvature, bands, funnel shape, or separated groups."),
                .("review whether a covariate, interaction, or non-linear term is missing.")
            ))

            self$results$qqResidualsGuide$setContent(plot_guide(
                .("evaluate whether standardized residuals approximate a normal distribution."),
                .("theoretical quantiles expected under normality."),
                .("observed quantiles of the standardized residuals."),
                .("points close to the diagonal reference line."),
                .("strong deviations at the extremes, S-shape, or heavy tails."),
                .("interpret classical inference with caution and review outliers or transformations.")
            ))

            self$results$residualHistogramGuide$setContent(plot_guide(
                .("examine the shape, symmetry, and concentration of standardized residuals."),
                .("standardized residuals."),
                .("count of cases."),
                .("an approximately symmetric distribution centered near zero."),
                .("marked skew, multiple peaks, or very long tails."),
                .("complement normality tests and review extreme cases.")
            ))

            self$results$residualNormalCurveGuide$setContent(plot_guide(
                .("compare the observed residual distribution to a theoretical normal curve."),
                .("standardized residuals."),
                .("density, i.e. the relative concentration of cases."),
                .("an empirical curve centered near zero and similar to the normal bell shape."),
                .("skew, heavy tails, several peaks, or an excess of extreme cases."),
                .("read classical tests with caution and review outliers, transformation, or specification.")
            ))

            invisible(NULL)
        },

        .fillDesign = function(., clean_num, fmt_num, html_escape, block96, html_list_guide, plot_guide, add_table_row, dep, factors, covs, dat2, n_total, n_used, n_excluded, formula_text, design_problem, cell_factor, cell_n, min_cell, max_cell, balance_ratio) {
            self$results$intro$setContent(paste0(
                "<div style=\"max-width: 7.25in; width: 100%; box-sizing: border-box;\">",
                "<p style=\"font-weight: 700; margin: 0 0 0.10em 0; line-height: 1.25;\">",
                "AssumptionsLab",
                "</p>",
                "<p style=\"margin: 0 0 0.35em 0; line-height: 1.25;\">",
                .("Assumption check for ANOVA/ANCOVA"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">\u00A0</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("Use this analysis when you want to review whether an ANOVA/ANCOVA has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data."),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">\u00A0</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("<b>Dependent variable:</b> "), html_escape(dep),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("<b>Factors:</b> "), html_escape(paste(factors, collapse = ", ")),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("<b>Covariates:</b> "),
                html_escape(ifelse(length(covs) == 0, .("None"), paste(covs, collapse = ", "))),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                .("<b>Analysis type:</b> "),
                ifelse(length(covs) == 0, "ANOVA", "ANCOVA"),
                "</p>",
                "</div>"
            ))

            ds_i <- 1
            add_ds <- function(item, value) {
                add_table_row(
                    self$results$designSummary,
                    paste0("ds_", ds_i),
                    list(item = item, value = value)
                )
                ds_i <<- ds_i + 1
            }

            add_ds(.("Model"), gsub("`", "", formula_text))
            add_ds(.("Total cases"), as.character(n_total))
            add_ds(.("Complete cases used"), as.character(n_used))
            add_ds(.("Cases excluded for missing data"), as.character(n_excluded))
            add_ds(.("Number of factors"), as.character(length(factors)))
            add_ds(.("Number of covariates"), as.character(length(covs)))
            add_ds(.("Number of observed cells"), as.character(length(cell_n)))
            add_ds(.("Minimum cell size"), as.character(min_cell))
            add_ds(.("Maximum cell size"), as.character(max_cell))
            add_ds(.("Max/min ratio"), fmt_num(balance_ratio))

            cs_i <- 1
            y <- dat2[[dep]]

            private$.groupPlotData <- data.frame(
                cell = as.character(cell_factor),
                value = as.numeric(y),
                stringsAsFactors = FALSE
            )

            # Same export-context issue as private$.plotData above: setState()
            # keeps this boxplot's data available when jamovi renders it in a
            # fresh export context.
            # ES: Mismo problema de contexto de exportación que private$.plotData
            # arriba: setState() mantiene disponibles los datos de este boxplot
            # cuando jamovi lo renderiza en un contexto de exportación nuevo.
            self$results$groupBoxplotsPlot$setState(private$.groupPlotData)

            self$results$groupBoxplotsGuide$setContent(plot_guide(
                .("compare central tendency, spread, and univariate outliers across groups or cells."),
                .("groups / cells defined by the selected factor(s)."),
                .("observed values of the dependent variable."),
                .("boxes of similar size and reasonable whiskers across groups."),
                .("very different spreads, many outliers, or strong asymmetry in one group."),
                .("review those groups before trusting homogeneity of variance results.")
            ))

            for (cell in names(cell_n)) {
                idx <- cell_factor == cell
                yy <- y[idx]

                add_table_row(
                    self$results$cellSummary,
                    paste0("cell_", cs_i),
                    list(
                        cell = cell,
                        n = as.integer(sum(idx)),
                        mean = clean_num(mean(yy, na.rm = TRUE)),
                        sd = clean_num(stats::sd(yy, na.rm = TRUE)),
                        variance = clean_num(stats::var(yy, na.rm = TRUE))
                    )
                )

                cs_i <- cs_i + 1
            }

            self$results$designGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("ANOVA compares means across groups; ANCOVA adds numeric covariates."),
                .("Check cell sizes, balance, missing data, and design coherence."),
                .("The full explanation of these criteria is in Assumption Library.")
            ))

            small_cells <- names(cell_n)[cell_n < 5]

            design_finding <- if (!design_problem) {
                sprintf(.("Cell sizes range from %d to %d (max/min ratio %.2f), which is compatible with a reasonably balanced design."), min_cell, max_cell, balance_ratio)
            } else if (length(small_cells) > 0) {
                sprintf(.("%d of %d cell(s) have fewer than 5 cases (%s); the max/min ratio is %.2f."), length(small_cells), length(cell_n), paste(small_cells, collapse = ", "), balance_ratio)
            } else {
                sprintf(.("No cell has fewer than 5 cases, but the max/min ratio is %.2f, above the balance threshold."), balance_ratio)
            }

            self$results$designInterpretation$setContent(
                block96(
                    .("Applied Interpretation"),
                    design_finding,
                    .("Why it matters: unequal cell sizes interact with variance heterogeneity. When the cell with the smallest n also has the largest variance, the classical F-test becomes anti-conservative (actual Type I error above the nominal level); when the smallest cell has the smallest variance, the test becomes conservative instead."),
                    .("Common error: treating any degree of design imbalance as automatically invalidating the ANOVA. With homogeneous variances, moderate imbalance has limited practical impact; the real risk appears specifically when imbalance and variance heterogeneity occur together, so this result should be read alongside the \"Homogeneity of Variances\" section (Maxwell & Delaney, 2004; see references).")
                )
            )

            y
        },

        .checkNormality = function(., clean_num, p_sig, block96, html_list_guide, interpret_normality, add_table_row, residuals_raw, normality_texts, norm_sig_count, norm_total_count, normality_problem) {
            # -----------------------------------------------------------------------------
            # Residual normality.
            # ES: Normalidad de residuos.
            # -----------------------------------------------------------------------------

            rn_i <- 1
            add_norm <- function(test, statistic, value, p_value, reason = NULL) {
                if (!is.na(clean_num(p_value))) {
                    norm_total_count <<- norm_total_count + 1L
                    if (clean_num(p_value) < .05) {
                        normality_problem <<- TRUE
                        norm_sig_count <<- norm_sig_count + 1L
                    }
                }

                key <- paste0("norm_", rn_i)
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

                normality_texts <<- c(
                    normality_texts,
                    interpret_normality(test, p_value, length(res), "los residuos del modelo")
                )

                rn_i <<- rn_i + 1
            }

            res <- residuals_raw[!is.na(residuals_raw)]

            # Shapiro-Wilk / Jarque-Bera / skewness / kurtosis: guard unified
            # suite-wide (n>=3,<=5000 & sd>0 for SW; n>=8 & sd>0 for the rest)
            # per Archie's decision, Aug 2026 - see .al_norm_core_battery().
            # ES: guarda unificada para toda la suite - ver
            # .al_norm_core_battery() en shared-helpers.R.
            .nc_res <- .al_norm_core_battery(res)
            sw <- .nc_res$sw

            if (!is.null(sw))
                add_norm("Shapiro-Wilk", "W", sw$statistic[[1]], sw$p.value)
            else
                add_norm("Shapiro-Wilk", "W", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_res$reasons$sw, .))

            # Lilliefors / Anderson-Darling / Cramér-von Mises / Shapiro-Francia /
            # Pearson chi-square: identical tryCatch calls in every module,
            # consolidated in shared-helpers.R (.al_nortest_battery).
            # ES: idénticas en todos los módulos, consolidadas en shared-helpers.R.
            .nt_res <- .al_nortest_battery(res)
            li <- .nt_res$li; ad <- .nt_res$ad; cvm <- .nt_res$cvm; sf <- .nt_res$sf

            if (!is.null(li))
                add_norm(.("Lilliefors (corrected K-S)"), "D", li$statistic[[1]], li$p.value)
            else
                add_norm(.("Lilliefors (corrected K-S)"), "D", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$li, .))

            if (!is.null(ad))
                add_norm("Anderson-Darling", "A²", ad$statistic[[1]], ad$p.value)
            else
                add_norm("Anderson-Darling", "A²", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$ad, .))

            if (!is.null(cvm))
                add_norm("Cram\u00e9r-von Mises", "W²", cvm$statistic[[1]], cvm$p.value)
            else
                add_norm("Cram\u00e9r-von Mises", "W²", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$cvm, .))

            if (!is.null(sf))
                add_norm("Shapiro-Francia", "W'", sf$statistic[[1]], sf$p.value)
            else
                add_norm("Shapiro-Francia", "W'", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$sf, .))

            pt <- .nt_res$pt

            if (!is.null(pt))
                add_norm(.("Pearson chi-square"), "P", pt$statistic[[1]], pt$p.value)
            else
                add_norm(.("Pearson chi-square"), "P", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_res$reasons$pt, .))

            jb <- .nc_res$jb

            if (!is.null(jb))
                add_norm("Jarque-Bera", "JB", jb$value, jb$p)
            else
                add_norm("Jarque-Bera", "JB", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_res$reasons$jb, .))

            skew_test <- .nc_res$skew

            if (!is.null(skew_test))
                add_norm(.("Skewness test"), "z", skew_test$value, skew_test$p)
            else
                add_norm(.("Skewness test"), "z", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_res$reasons$skew, .))

            kurt_test <- .nc_res$kurt

            if (!is.null(kurt_test))
                add_norm(.("Kurtosis test"), "z", kurt_test$value, kurt_test$p)
            else
                add_norm(.("Kurtosis test"), "z", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_res$reasons$kurt, .))

            self$results$residualNormalityGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("Normality is evaluated on the model's residuals, not on the raw variable."),
                .("p >= .05 is compatible with approximate normality."),
                .("p < .05 suggests a significant deviation from normality."),
                .("See methodological details in Assumption Library.")
            ))


            normality_integration <- if (norm_total_count == 0) {
                ""
            } else if (norm_sig_count == 0) {
                sprintf(.("None of the %d normality tests computed reached significance: consistent evidence of approximate normality."), norm_total_count)
            } else if (norm_sig_count == norm_total_count) {
                sprintf(.("All %d normality tests computed reached significance: consistent evidence of a real deviation from normality."), norm_total_count)
            } else if (norm_sig_count == 1) {
                sprintf(.("Only 1 of %d normality tests reached significance. An isolated signal like this, uncorroborated by the rest of the battery, deserves more caution than a result confirmed across multiple tests."), norm_total_count)
            } else {
                sprintf(.("%d of %d normality tests reached significance. This level of agreement across multiple tests strengthens the case for a real deviation from normality, beyond what a single isolated test would indicate."), norm_sig_count, norm_total_count)
            }

            self$results$residualNormalityInterpretation$setContent(
                block96(paste(
                    .("Applied Interpretation"),
                    paste(normality_texts, collapse = "\n"),
                    normality_integration,
                    .("Shapiro-Wilk is used because it retains the highest statistical power among common normality tests across nearly the full range of sample sizes (Razali & Wah, 2011; see references). A widespread recommendation to prefer Kolmogorov-Smirnov once n grows is a leftover from old software limitations, not a statistical reason."),
                    .("Why it matters: residual normality mainly affects the precision of classical inference (confidence intervals, p-values) in small samples; point estimates of group means remain valid even with moderate non-normality."),
                    .("Common error: assuming a significant normality test invalidates the whole ANOVA. It is a signal to inspect the shape of residuals (skewness, heavy tails, outliers) before deciding whether classical inference is still trustworthy."),
                    sep = "\n\n"
                ))
            )

            normality_problem
        },

        .checkHomogeneity = function(., clean_num, p_sig, block96, html_list_guide, interpret_variance, add_table_row, n_used, variance_texts, cell_factor, cell_n, y, variance_problem) {
            # -----------------------------------------------------------------------------
            # Homogeneity of variances.
            # ES: Homogeneidad de varianzas.
            # -----------------------------------------------------------------------------

            vt_i <- 1
            add_var <- function(family, test, statistic, value, df, p_value, reason = NULL) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05)
                    variance_problem <<- TRUE

                key <- paste0("var_", vt_i)
                add_table_row(
                    self$results$varianceTests,
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
                        self$results$varianceTests$addFootnote(col = "p", note = reason, rowKey = key),
                        error = function(e) invisible(NULL)
                    )

                variance_texts <<- c(
                    variance_texts,
                    interpret_variance(test, p_value, n_used, length(cell_n), family)
                )

                vt_i <<- vt_i + 1
            }

            # Manual Levene (mean-centered) / Brown-Forsythe (median-centered):
            # identical algorithm in anovaCheck and regCheck, consolidated in
            # shared-helpers.R (.al_levene_manual). groupCheck's car::leveneTest
            # rows are a separate implementation, left untouched.
            # ES: idéntico en anovaCheck y regCheck, consolidado en
            # shared-helpers.R. Las filas de groupCheck (car::leveneTest) son
            # una implementación aparte, sin tocar.
            # All four variance-equality tests below share the same basic
            # precondition - every cell (factor-level combination) needs at
            # least 2 observations to contribute a within-cell spread - so a
            # failure in any of them is almost always explained by the
            # smallest cell, not a random computational glitch.
            # ES: las cuatro pruebas de igualdad de varianzas comparten la
            # misma precondición básica - cada celda (combinación de niveles
            # de factor) necesita al menos 2 observaciones para aportar una
            # dispersión intra-celda - así que un fallo en cualquiera de
            # ellas casi siempre se explica por la celda más pequeña, no por
            # un fallo computacional aleatorio.
            variance_test_reason <- if (length(cell_n) > 0 && min(cell_n) < 2) sprintf(.("Could not be computed - the smallest factor-level combination has only %s observation(s); at least 2 are needed in every cell."), min(cell_n)) else .("Could not be computed for this design.")

            lev <- tryCatch(.al_levene_manual(y, cell_factor, "mean"), error = function(e) NULL)

            if (!is.null(lev))
                add_var(.("Equality of variances"), "Levene", "F", lev$value, lev$df, lev$p)
            else
                add_var(.("Equality of variances"), "Levene", "F", NA_real_, NA_integer_, NA_real_, reason = variance_test_reason)

            bf <- tryCatch(.al_levene_manual(y, cell_factor, "median"), error = function(e) NULL)

            if (!is.null(bf))
                add_var(.("Equality of variances"), "Brown-Forsythe", "F", bf$value, bf$df, bf$p)
            else
                add_var(.("Equality of variances"), "Brown-Forsythe", "F", NA_real_, NA_integer_, NA_real_, reason = variance_test_reason)

            bart <- tryCatch(stats::bartlett.test(y ~ cell_factor), error = function(e) NULL)

            if (!is.null(bart))
                add_var(.("Equality of variances"), "Bartlett", "K²",
                        bart$statistic[[1]], bart$parameter[[1]], bart$p.value)
            else
                add_var(.("Equality of variances"), "Bartlett", "K²",
                        NA_real_, NA_integer_, NA_real_, reason = variance_test_reason)

            flig <- tryCatch(stats::fligner.test(y ~ cell_factor), error = function(e) NULL)

            if (!is.null(flig))
                add_var(.("Equality of variances"), "Fligner-Killeen", "χ²",
                        flig$statistic[[1]], flig$parameter[[1]], flig$p.value)
            else
                add_var(.("Equality of variances"), "Fligner-Killeen", "χ²",
                        NA_real_, NA_integer_, NA_real_, reason = variance_test_reason)

            hartley <- tryCatch({
                vars <- tapply(y, cell_factor, stats::var, na.rm = TRUE)
                vars <- vars[!is.na(vars) & vars > 0]
                max(vars) / min(vars)
            }, error = function(e) NA_real_)

            add_var(.("Variance ratio"), "Hartley Fmax", "Fmax",
                    hartley, NA_integer_, NA_real_)

            self$results$varianceGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("These tests check whether variances are similar across groups or cells."),
                .("p >= .05 is compatible with approximately homogeneous variances."),
                .("p < .05 suggests significant differences between variances."),
                .("See methodological details in Assumption Library.")
            ))


            self$results$varianceInterpretation$setContent(
                block96(paste(
                    .("Applied Interpretation"),
                    paste(variance_texts, collapse = "\n"),
                    .("Levene's test is treated as the primary reference because it is the classical, most widely recognized test for equality of variances across groups (Levene, 1960; see references); Brown-Forsythe and Bartlett are reported alongside for comparison."),
                    .("Why it matters: unequal variances mainly threaten the classical F-test when group sizes are also unequal; with equal group sizes, ANOVA is fairly robust to moderate variance differences."),
                    .("Common error: treating a significant variance test as reason to abandon ANOVA entirely. The direct remedy is usually switching to Welch's ANOVA, which does not assume equal variances, rather than discarding the analysis."),
                    sep = "\n\n"
                ))
            )

            variance_problem
        },

        .checkSlopes = function(., clean_num, p_sig, qname, block96, html_list_guide, interpret_slope, add_table_row, dep, factors, covs, dat2, n_used, rhs, fit, slope_texts, slopes_problem) {
            # -----------------------------------------------------------------------------
            # ANCOVA homogeneity of slopes.
            # ES: Homogeneidad de pendientes ANCOVA.
            # -----------------------------------------------------------------------------

            st_i <- 1
            add_slope <- function(fac, cov, inter, statistic, value, p_value) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05)
                    slopes_problem <<- TRUE

                add_table_row(
                    self$results$slopesTests,
                    paste0("slope_", st_i),
                    list(
                        factor = fac,
                        covariate = cov,
                        interaction = inter,
                        statistic = statistic,
                        value = clean_num(value),
                        p = clean_num(p_value),
                        pSig = p_sig(p_value)
                    )
                )

                slope_texts <<- c(
                    slope_texts,
                    interpret_slope(fac, cov, p_value, n_used)
                )

                st_i <<- st_i + 1
            }

            if (length(covs) == 0) {
                add_slope(.("Not applicable"), .("No covariates"), "", "", NA_real_, NA_real_)
            } else {
                for (fac in factors) {
                    for (cv in covs) {
                        int_term <- paste0(qname(fac), ":", qname(cv))
                        f_int <- stats::as.formula(paste(
                            qname(dep), "~", rhs, "+", int_term
                        ))

                        fit_int <- tryCatch(stats::lm(f_int, data = dat2), error = function(e) NULL)

                        p_int <- tryCatch({
                            modelComparisonAnova <- stats::anova(fit, fit_int)
                            modelComparisonAnova$`Pr(>F)`[2]
                        }, error = function(e) NA_real_)

                        add_slope(fac, cv, paste0(fac, " × ", cv), "p", p_int, p_int)
                    }
                }
            }

            self$results$slopesGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("This assumption applies to ANCOVA when covariates are present."),
                .("p >= .05 is compatible with approximately homogeneous slopes."),
                .("p < .05 suggests the slope changes across groups."),
                .("See methodological details in Assumption Library.")
            ))


            self$results$slopesInterpretation$setContent(
                block96(
                    .("Applied Interpretation"),
                    paste(slope_texts, collapse = "\n"),
                    if (length(covs) > 0) .("Why it matters: if slopes are not homogeneous, a single averaged covariate adjustment misrepresents the group differences, because the covariate-response relationship itself differs by group (Huitema, 2011; see references).") else "",
                    if (length(covs) > 0) .("Common error: running standard ANCOVA anyway and reporting adjusted means as if the covariate effect were the same for every group, when the interaction test says otherwise.") else "",
                    if (length(covs) > 0) .("Sample-size nuance: this interaction test has limited power in small or unbalanced cells, so a non-significant result should not be read as strong proof of homogeneous slopes; with very large samples, even a trivial slope difference can become significant.") else ""
                )
            )

            slopes_problem
        },

        .checkCovariateLinearity = function(., clean_num, p_sig, qname, block96, html_list_guide, dcor_stat, dcor_pvalue, copentTest, interpret_covariate_linearity, add_table_row, dep, covs, dat2, n_used, rhs, covlin_texts, y, linearity_problem, dcor_problem) {
            # -----------------------------------------------------------------------------
            # Covariate linearity.
            # ES: Linealidad de covariables.
            # -----------------------------------------------------------------------------

            cl_i <- 1
            add_covlin <- function(cv, test, statistic, value, p_value, extra_r = NA_real_, reason = NULL) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05 &&
                    test %in% c(.("Exploratory quadratic term"),
                                .("Exploratory Box-Tidwell"))) {
                    linearity_problem <<- TRUE
                }

                key <- paste0("covlin_", cl_i)
                add_table_row(
                    self$results$covariateLinearity,
                    key,
                    list(
                        covariate = cv,
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
                        self$results$covariateLinearity$addFootnote(col = "p", note = reason, rowKey = key),
                        error = function(e) invisible(NULL)
                    )

                covlin_texts <<- c(
                    covlin_texts,
                    interpret_covariate_linearity(
                        cv, test, p_value, n_used,
                        extra_r = extra_r, stat_value = value
                    )
                )

                cl_i <<- cl_i + 1
            }

            if (length(covs) == 0) {
                add_covlin(.("Not applicable"), .("No covariates"), "", NA_real_, NA_real_)
            } else {
                for (cv in covs) {
                    # Let jamovi stop the permutation tests when an option changes.
                    # ES: Permite que jamovi detenga las permutaciones si cambia una opción.
                    private$.checkpoint(flush = FALSE)
                    x <- dat2[[cv]]

                    cor_val <- tryCatch(stats::cor(x, y, use = "complete.obs"),
                                        error = function(e) NA_real_)
                    cor_p <- tryCatch(stats::cor.test(x, y)$p.value,
                                      error = function(e) NA_real_)

                    add_covlin(cv, .("Bivariate correlation"), "r", cor_val, cor_p)

                    quad_p <- tryCatch({
                        quadDat <- dat2
                        quadDat[[".x_sq_tmp"]] <- x^2
                        f_quad <- stats::as.formula(paste(
                            qname(dep), "~", rhs, "+ .x_sq_tmp"
                        ))
                        fit_quad <- stats::lm(f_quad, data = quadDat)
                        coef(summary(fit_quad))[".x_sq_tmp", "Pr(>|t|)"]
                    }, error = function(e) NA_real_)

                    add_covlin(cv, .("Exploratory quadratic term"), "p", quad_p, quad_p)

                    bt_needs_positive <- any(x <= 0, na.rm = TRUE)
                    bt_p <- if (bt_needs_positive) NA_real_ else tryCatch({
                        boxTidwellDat <- dat2
                        boxTidwellDat[[".bt_tmp"]] <- x * log(x)
                        f_bt <- stats::as.formula(paste(
                            qname(dep), "~", rhs, "+ .bt_tmp"
                        ))
                        fit_bt <- stats::lm(f_bt, data = boxTidwellDat)
                        coef(summary(fit_bt))[".bt_tmp", "Pr(>|t|)"]
                    }, error = function(e) NA_real_)

                    bt_reason <- if (bt_needs_positive) .("Box-Tidwell requires strictly positive values (it uses x*log(x)); this covariate has zero or negative values.") else if (is.na(clean_num(bt_p))) .("Could not be computed for this covariate.") else NULL

                    add_covlin(cv, .("Exploratory Box-Tidwell"), "p", bt_p, bt_p, reason = bt_reason)

                    dcor_val <- tryCatch(dcor_stat(x, y), error = function(e) NA_real_)
                    dcor_p <- tryCatch(dcor_pvalue(x, y), error = function(e) NA_real_)

                    if (!is.na(dcor_val) && !is.na(cor_val) && !is.na(dcor_p) &&
                        (dcor_val - abs(cor_val)) > 0.10 && dcor_p < .05) {
                        dcor_problem <<- TRUE
                    }

                    add_covlin(cv, .("Distance correlation (dCor)"), "dCor", dcor_val, dcor_p, extra_r = cor_val)

                    ce_res <- tryCatch(copentTest(x, y), error = function(e) NULL)

                    if (!is.null(ce_res)) {
                        add_covlin(cv, .("Copula entropy (copent)"), "CE", ce_res$ce, ce_res$p)
                    } else {
                        ce_reason <- .("Could not be computed for this covariate (copula entropy needs enough distinct values to estimate a copula density).")
                        add_covlin(cv, .("Copula entropy (copent)"), "CE", NA_real_, NA_real_, reason = ce_reason)
                    }
                }
            }

            self$results$covariateLinearityGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("Linearity applies to numeric covariates, not to categorical factors."),
                .("p >= .05 is compatible with approximate linearity."),
                .("p < .05 suggests possible curvature or misspecification."),
                .("See methodological details in Assumption Library.")
            ))


            self$results$covariateLinearityInterpretation$setContent(
                block96(
                    .("Applied Interpretation"),
                    paste(covlin_texts, collapse = "\n"),
                    if (length(covs) > 0) .("Distance correlation (dCor): unlike Pearson's r, which only detects linear association, dCor can detect any form of statistical dependence, including non-monotonic or curved relationships, and equals zero only when the variables are independent.") else "",
                    if (length(covs) > 0) .("Common error: treating a significant dCor by itself as proof that the relationship is non-linear. dCor is also significant for purely linear relationships; the real diagnostic signal here is the size of the gap between dCor and |Pearson's r|, not the significance of dCor alone.") else "",
                    if (length(covs) > 0) .("Sample-size nuance: the permutation test used for dCor has limited resolution in small samples, so a moderate gap should be treated as exploratory rather than conclusive.") else ""
                )
            )

            list(linearity_problem = linearity_problem, dcor_problem = dcor_problem)
        },

        .checkMulticollinearity = function(., clean_num, block96, html_list_guide, add_table_row, covs, dat2, fit) {
            # -----------------------------------------------------------------------------
            # Multicollinearity.
            # ES: Multicolinealidad.
            # -----------------------------------------------------------------------------

            multi_i <- 1
            max_vif <- NA_real_
            max_vif_name <- NA_character_

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

            if (length(covs) > 0 && !is.null(X_no_intercept) && ncol(X_no_intercept) > 1) {

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

                    vif_reason <- if (is.na(clean_num(vif))) .("Could not be computed - regressing this term on the others is likely singular (more terms than complete cases, or an exact linear dependency among predictors).") else NULL
                    add_multi("VIF", colnames(X_no_intercept)[j], "VIF", vif, reason = vif_reason)
                    add_multi(.("Tolerance"), colnames(X_no_intercept)[j], "1/VIF", tol, reason = vif_reason)
                }

                eig <- tryCatch({
                    X_scaled <- scale(X_no_intercept)
                    R <- stats::cor(X_scaled, use = "pairwise.complete.obs")
                    eigen(R, symmetric = TRUE)$values
                }, error = function(e) NULL)

                if (is.null(eig)) {
                    eig_reason <- .("Could not be computed - the design matrix's correlation matrix could not be decomposed (likely too few complete cases relative to the number of terms).")
                    add_multi(.("Minimum eigenvalue"), .("Design matrix"), .("minimum λ"), NA_real_, reason = eig_reason)
                    add_multi(.("Condition index"), .("Design matrix"), "CI", NA_real_, reason = eig_reason)
                    add_multi(.("Determinant"), .("Correlation matrix"), "det(R)", NA_real_, reason = eig_reason)
                }

                if (!is.null(eig)) {
                    min_eig <- max(min(eig, na.rm = TRUE), .Machine$double.eps)
                    max_eig <- max(eig, na.rm = TRUE)
                    ci <- sqrt(max_eig / min_eig)

                    det_r <- tryCatch(
                        det(stats::cor(scale(X_no_intercept), use = "pairwise.complete.obs")),
                        error = function(e) NA_real_
                    )

                    add_multi(.("Minimum eigenvalue"), .("Design matrix"), .("minimum λ"), min_eig)
                    add_multi(.("Condition index"), .("Design matrix"), "CI", ci)
                    add_multi(.("Determinant"), .("Correlation matrix"), "det(R)", det_r)
                }
            } else {
                add_multi(.("VIF / tolerance"), .("Not applicable"), "", NA_real_)
            }

            if (length(covs) >= 2) {
                cor_mat <- tryCatch(
                    stats::cor(dat2[, covs, drop = FALSE], use = "pairwise.complete.obs"),
                    error = function(e) NULL
                )

                if (!is.null(cor_mat)) {
                    cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA_real_
                    max_cor <- suppressWarnings(max(abs(cor_mat), na.rm = TRUE))
                    add_multi(.("Maximum correlation"), .("Numeric covariates"), .("maximum |r|"), max_cor)
                }
            }

            self$results$multicollinearityGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("This check applies when covariates are present (ANCOVA); VIF among a single factor's own dummy-coded levels is not an actionable diagnostic and is not shown for a covariate-free ANOVA."),
                .("VIF and tolerance describe how much each model term overlaps with the others; the eigenvalue/condition index diagnose collinearity across the whole design matrix at once."),
                .("VIF >= 5 is a moderate concern; VIF >= 10 is a severe problem."),
                .("See methodological details in Assumption Library.")
            ))

            self$results$multicollinearityInterpretation$setContent(block96(
                .("Applied Interpretation"),
                if (length(covs) > 0) sprintf(.("Maximum VIF: %s%s. By convention, values between 5 and 10 raise a moderate concern, and values above 10 are considered a severe problem (Marquardt, 1970; see references)."), format(round(clean_num(max_vif), 2), nsmall = 2), if (!is.na(max_vif_name)) sprintf(.(" (term: %s)"), max_vif_name) else "") else .("Not applicable: requires covariates."),
                if (length(covs) > 0) .("Common error: assuming a high VIF means a covariate or factor should be removed. The model can still predict well overall; the issue is isolating that specific term's individual effect from the others.") else "",
                if (length(covs) > 0) .("If collinearity is a real concern with multiple covariates, consider combining the most redundant ones or centering variables involved in interactions, rather than discarding the full model.") else ""
            ))

            invisible(NULL)
        },

        .fillCorrelations = function(lang, ., p_sig, block96, html_list_guide, dcor_stat, dcor_pvalue, copentTest, dep, covs, dat2, n_used) {
            # -----------------------------------------------------------------------------
            # Correlation matrices (Pearson and dCor, APA 7 format) + copent discordance.
            # ES: Matrices de correlación (Pearson y dCor, formato APA 7) + discordancia con copent.
            # -----------------------------------------------------------------------------

            self$results$correlationMatrixGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("These two tables complement the multicollinearity checks with an overview of the association between the dependent variable and all numeric covariates, each in APA 7 format (lower triangle, numbered variables)."),
                .("The first reports conventional Pearson correlation (linear association only); the second reports distance correlation (dCor, Szekely et al., 2007), which detects linear and non-linear association alike.")
            ))

            {
                # fmtR()/apaCell(): identical in every module that has them,
                # consolidated in shared-helpers.R (.al_fmt_r/.al_apa_cell).
                # ES: idénticas en todos los módulos que las usan,
                # consolidadas en shared-helpers.R.
                fmtR <- .al_fmt_r
                apaCell <- .al_apa_cell

                pearsonTable <- self$results$pearsonMatrixTable
                pearsonTable$deleteRows()
                dcorTable <- self$results$dcorMatrixTable
                dcorTable$deleteRows()

                matVars <- c(dep, covs)
                k <- length(matVars)
                pairResults <- list()

                if (k >= 2) {
                    for (i in seq_len(k)) {
                        for (j in seq_len(k)) {
                            if (j >= i) next()
                            # Let jamovi stop the permutation tests when an option changes.
                            # ES: Permite que jamovi detenga las permutaciones si cambia una opción.
                            private$.checkpoint(flush = FALSE)
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
                                dcor = dcv, dcorP = dcp,
                                ce = if (!is.null(ceRes)) ceRes$ce else NA_real_,
                                ceP = if (!is.null(ceRes)) ceRes$p else NA_real_
                            )
                        }
                    }

                    for (i in seq_len(k)) {
                        rowVar <- matVars[i]
                        pearsonVals <- list(var = sprintf("%d. %s", i, rowVar))
                        pearsonSyms <- list(); dcorSyms <- list()
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
                                pearsonVals[[colName]] <- if (!is.null(pr) && !is.na(pr$pearsonR)) pr$pearsonR else ""
                                pearsonSyms[[colName]] <- if (!is.null(pr)) .al_p_sig(pr$pearsonP) else ""
                                dcorVals[[colName]] <- if (!is.null(pr) && !is.na(pr$dcor)) pr$dcor else ""
                                dcorSyms[[colName]] <- if (!is.null(pr)) .al_p_sig(pr$dcorP) else ""
                            }
                        }
                        pearsonTable$addRow(rowKey = rowVar, values = pearsonVals)
                        for (cn in names(pearsonSyms)) if (nzchar(pearsonSyms[[cn]])) pearsonTable$addSymbol(rowNo = pearsonTable$rowCount, cn, pearsonSyms[[cn]])
                        dcorTable$addRow(rowKey = rowVar, values = dcorVals)
                        for (cn in names(dcorSyms)) if (nzchar(dcorSyms[[cn]])) dcorTable$addSymbol(rowNo = dcorTable$rowCount, cn, dcorSyms[[cn]])
                    }

                    self$results$correlationMatrixNote$setContent(block96(
                        sprintf(.("%s * p < .05, ** p < .01, *** p < .001"), sprintf("N = %d.", n_used)),
                        .al_dcor_na_note(lang)
                    ))
                } else {
                    self$results$correlationMatrixNote$setContent(block96(
                        .("Not applicable: fewer than two numeric model variables.")
                    ))
                }

                self$results$correlationComparisonGuide$setContent(block96(
                    .("Pearson / dCor / Copula Entropy Discordance Analysis"),
                    .("Because Pearson's r only captures linear association while dCor captures both linear and non-linear association, a pair whose dCor is notably larger than its Pearson |r| is a signal (not proof) of a non-linear relationship."),
                    .("Pairs are flagged in the \"Pairs with a Notable Difference between Pearson and dCor\" table when the gap (dCor minus |Pearson r|) is greater than .10. The copula entropy (CE, copent()) result for the same pair is shown alongside as a second, distribution-free line of evidence."),
                    .al_permutation_note(lang, 199, 20260704)
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
                        var1 = pr$v1, var2 = pr$v2,
                        pearson = pr$pearsonR, pearsonSig = .al_p_sig(pr$pearsonP),
                        dcor = pr$dcor, dcorSig = .al_p_sig(pr$dcorP),
                        gap = gap, ce = pr$ce, ceP = pr$ceP,
                        ceSig = p_sig(pr$ceP),
                        flag = .("Yes")
                    ))
                }

                if (length(flaggedPairs) == 0) {
                    self$results$correlationComparisonInterpretation$setContent(block96(
                        .("Applied Interpretation"),
                        if (k < 2)
                            .("Not applicable: fewer than two numeric model variables.")
                        else
                            sprintf(.("No pair shows a Pearson/dCor gap greater than %.2f; there is no indication of unmodeled non-linear association among the model's numeric variables."), gapThreshold)
                    ))
                } else {
                    self$results$correlationComparisonInterpretation$setContent(block96(
                        .("Applied Interpretation"),
                        sprintf(.("%d pair(s) show a Pearson/dCor gap greater than %.2f: %s. Inspect a scatterplot of each flagged pair; when copula entropy is also significant for a pair, this reinforces the suspicion of an unmodeled non-linear dependency."), length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", "))
                    ))
                }
            }

            invisible(NULL)
        },

        .checkInfluence = function(., clean_num, block96, html_list_guide, plot_guide, add_table_row, complete, n_used, residuals_raw, fitted_values, residuals_stud, leverage, cooks_d, dffits, p_model) {
            # -----------------------------------------------------------------------------
            # Outlying and influential cases.
            # ES: Casos atípicos e influyentes.
            # -----------------------------------------------------------------------------

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

            influence_count <- sum(nzchar(criteria), na.rm = TRUE)

            influence_text <- if (influence_count == 0) {
                .("No influential cases were detected under the main numeric criteria. This result is compatible with the absence of clearly influential cases according to the thresholds used. Even so, it is worth reviewing the table and plots if any cases show relatively high values.")
            } else {
                sprintf(.("%s case(s) were detected with at least one criterion activated. This suggests reviewing those cases before interpreting the model. A flagged case should not be removed automatically; first check whether it reflects a data error, a valid extreme case, or substantive information."), influence_count)
            }

            self$results$influenceInterpretation$setContent(
                block96(
                    .("Applied Interpretation"),
                    influence_text,
                    .("Why it matters: in ANOVA/ANCOVA, an influential case can shift group means, cell variances, or the covariate slope enough to change which post hoc comparisons look significant."),
                    .("Common error: deleting every flagged case to \"clean up\" the model. This can hide genuine subgroup differences and produce a model that only fits the well-behaved majority of the data."),
                    .("Sample-size nuance: the 4/n threshold used for Cook's D becomes stricter as the sample shrinks, so in small designs it is common to flag several cases that are not truly problematic.")
                )
            )

            self$results$influenceGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("These diagnostics identify outlying or influential cases in the model."),
                .("A flagged case should not be removed automatically."),
                .("Check the data, compare a sensitivity analysis, and justify decisions."),
                .("See methodological details in Assumption Library.")
            ))

            self$results$residualsLeverageGuide$setContent(plot_guide(
                .("identify cases with a combination of extreme residual and high leverage."),
                .("leverage, i.e. how far a case sits from the center of the predictor space."),
                .("studentized residual, how extreme the case's error is."),
                .("most cases near zero and with low leverage."),
                .("cases with a large studentized residual, high leverage, or a large Cook's D."),
                .("review influential cases and compare a sensitivity analysis.")
            ))

            self$results$cooksDGuide$setContent(plot_guide(
                .("detect observations that can change the model's fit."),
                .("case number or position in the dataset."),
                .("Cook's distance, an indicator of overall influence."),
                .("small bars, below the reference line."),
                .("cases with Cook's D above the visual threshold."),
                .("verify the case's plausibility and document analyses with/without influential cases.")
            ))


            self$results$independenceGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("Independence depends mainly on the study design."),
                .("Repeated measures, paired data, or nested data require a different approach."),
                .("Use Related Groups or mixed models if independence is not defensible."),
                .("Why it matters: dependent observations underestimate standard errors, making p-values look more significant than the data actually support."),
                .("Common error: assuming independence is satisfied simply because no diagnostic flagged it. Independence is a property of how the data were collected, not something a single test can confirm after the fact.")
            ))

            influence_problem
        },

        .fillAlternatives = function(., html_list_guide, add_table_row, normality_problem, variance_problem, slopes_problem, linearity_problem, influence_problem, design_problem, dcor_problem) {
            # -----------------------------------------------------------------------------
            # Methodological alternatives.
            # ES: Alternativas metodológicas.
            # -----------------------------------------------------------------------------

            alt_i <- 1
            alt_when_texts <- character()
            add_alt <- function(problem, evidence, suggestion, when) {
                add_table_row(
                    self$results$alternatives,
                    paste0("alt_", alt_i),
                    list(
                        problem = problem,
                        evidence = evidence,
                        suggestion = suggestion
                    )
                )
                alt_when_texts <<- c(
                    alt_when_texts,
                    paste0(suggestion, ": ", when)
                )
                alt_i <<- alt_i + 1
            }

            if (design_problem) {
                add_alt(
                    .("Unbalanced design or small cells"),
                    .("There are small cells or an elevated max/min ratio."),
                    .("Review design, cell sizes, and robustness."),
                    .("When some cells have very few cases or the design is unbalanced.")
                )
            }

            if (variance_problem) {
                add_alt(
                    .("Different variances between groups"),
                    .("One or more tests suggest unequal variances."),
                    .("Welch ANOVA, Brown-Forsythe ANOVA, or robust methods."),
                    .("When homogeneity of variances is not defensible.")
                )
            }

            if (normality_problem) {
                add_alt(
                    .("Non-normality of residuals"),
                    .("One or more tests suggest a deviation from normality."),
                    .("Transformation, bootstrap, robust method, or non-parametric alternative."),
                    .("When non-normality is strong or combined with outliers.")
                )
            }

            if (slopes_problem) {
                add_alt(
                    .("Non-homogeneous slopes"),
                    .("Some factor × covariate interaction was relevant."),
                    .("Model the interaction or use a regression approach with interaction."),
                    .("When the covariate-response relationship changes across groups.")
                )
            }

            if (linearity_problem) {
                add_alt(
                    .("Non-linearity of covariates"),
                    .("A covariate shows possible curvature."),
                    .("Polynomial terms, splines, or covariate transformation."),
                    .("When the covariate-response relationship does not appear linear.")
                )
            }

            if (influence_problem) {
                add_alt(
                    .("Outlying or influential cases"),
                    .("Some case activated numeric criteria."),
                    .("Sensitivity analysis or robust method."),
                    .("When some cases may dominate the results.")
                )
            }

            if (dcor_problem) {
                add_alt(
                    .("Non-linear covariate dependence (dCor)"),
                    .("Distance correlation is notably higher than Pearson's r for at least one covariate."),
                    .("Inspect a scatterplot; consider a non-linear term or transformation for that covariate."),
                    .("When dCor detects dependence that a linear covariate adjustment would miss.")
                )
            }

            if (!design_problem && !variance_problem && !normality_problem &&
                !slopes_problem && !linearity_problem && !influence_problem && !dcor_problem) {
                add_alt(
                    .("No primary alert"),
                    .("The diagnostics show no strong signals in the assumptions evaluated."),
                    .("Standard ANOVA/ANCOVA."),
                    .("When the design and assumptions are reasonably defensible.")
                )
            }

            if (length(alt_when_texts) > 0) {
                self$results$alternativesWhen$setContent(.al_html_list(alt_when_texts))
            } else {
                self$results$alternativesWhen$setContent("")
            }

            self$results$alternativesGuide$setContent(html_list_guide(
                .("Quick guide"),
                .("This table translates the diagnostics into possible methodological options."),
                .("Do not switch methods because of a single isolated test."),
                .("The decision should combine evidence, design, and research question.")
            ))

            invisible(NULL)
        },

        .setPlotVisibility = function(., block96, html_list_guide, normality_problem, variance_problem, slopes_problem, linearity_problem, influence_problem, design_problem, dcor_problem) {
            # -----------------------------------------------------------------------------
            # Plot visibility by selected plot set.
            # ES: Visibilidad de gráficos según el conjunto seleccionado.
            # -----------------------------------------------------------------------------

            plot_set <- tryCatch(self$options$plotSet, error = function(e) "complete")
            if (is.null(plot_set) || length(plot_set) == 0 || !nzchar(plot_set))
                plot_set <- "complete"

            all_plot_result_items <- c(
                "diagnosticPlotsGuide",
                "groupBoxplotsPlot", "groupBoxplotsGuide",
                "residualsFittedPlot", "residualsFittedGuide",
                "qqResidualsPlot", "qqResidualsGuide",
                "residualHistogramPlot", "residualHistogramGuide",
                "residualNormalCurvePlot", "residualNormalCurveGuide",
                "residualsLeveragePlot", "residualsLeverageGuide",
                "cooksDPlot", "cooksDGuide"
            )

            visible_plot_result_items <- switch(
                plot_set,
                basic = c(
                    "diagnosticPlotsGuide",
                    "groupBoxplotsPlot", "groupBoxplotsGuide",
                    "residualsFittedPlot", "residualsFittedGuide"
                ),
                normality = c(
                    "diagnosticPlotsGuide",
                    "qqResidualsPlot", "qqResidualsGuide",
                    "residualHistogramPlot", "residualHistogramGuide",
                    "residualNormalCurvePlot", "residualNormalCurveGuide"
                ),
                influence = c(
                    "diagnosticPlotsGuide",
                    "residualsLeveragePlot", "residualsLeverageGuide",
                    "cooksDPlot", "cooksDGuide"
                ),
                complete = all_plot_result_items,
                all_plot_result_items
            )

            for (item_name in all_plot_result_items) {
                item <- tryCatch(self$results[[item_name]], error = function(e) NULL)
                if (!is.null(item)) {
                    item$setVisible(
                        isTRUE(self$options$showPlots) &&
                            item_name %in% visible_plot_result_items
                    )
                }
            }

            if (isTRUE(self$options$showPlots)) {
                self$results$diagnosticPlotsGuide$setContent(html_list_guide(
                    .("ANOVA/ANCOVA diagnostic plots"),
                    .("Diagnostic plots complement statistical tests and help evaluate patterns that a p-value may hide."),
                    .("Use these plots to review group balance, residual normality, and influential cases.")
                ))
            }

            problem_count <- sum(
                design_problem, variance_problem, normality_problem,
                slopes_problem, linearity_problem, influence_problem, dcor_problem
            )

            recommendation <- if (problem_count == 0) {
                .("No diagnostic raised a relevant concern; the ANOVA/ANCOVA model can be interpreted under its standard assumptions.")
            } else {
                flagged <- c(
                    if (design_problem) .("design balance"),
                    if (variance_problem) .("homogeneity of variances"),
                    if (normality_problem) .("residual normality"),
                    if (slopes_problem) .("homogeneity of slopes"),
                    if (linearity_problem) .("covariate linearity"),
                    if (influence_problem) .("influential cases"),
                    if (dcor_problem) .("non-linear covariate dependence")
                )
                sprintf(.("%d assumption(s) raised a concern (%s); review the suggested methodological options in this section before relying on the standard ANOVA/ANCOVA F-test."), problem_count, paste(flagged, collapse = ", "))
            }

            self$results$notes$setContent(block96(
                sprintf(.("Suggested decision: %s"), recommendation),
                .("Significance codes: * p < .05, ** p < .01, *** p < .001.")
            ))

            invisible(NULL)
        },

        .requirePlotData = function(image) {
            . <- .al_lang(self$options)$tr
            if (!isTRUE(self$options$showPlots))
                return(FALSE)

            # image$state, not private$.plotData: state is what survives into the
            # fresh render context jamovi uses for PDF/image export.
            # ES: image$state, no private$.plotData: el state es lo que
            # sobrevive al contexto de renderizado nuevo que usa jamovi al
            # exportar a PDF/imagen.
            if (is.null(image$state) || nrow(image$state) == 0) {
                image$setError(.("No diagnostic plot data are available."))
                return(FALSE)
            }
            TRUE
        },

        .requireGroupPlotData = function(image) {
            . <- .al_lang(self$options)$tr
            if (!isTRUE(self$options$showPlots))
                return(FALSE)

            if (is.null(image$state) || nrow(image$state) == 0) {
                image$setError(.("No group plot data are available."))
                return(FALSE)
            }
            TRUE
        },


        # Plot theme/palette migrated to jamovi's native ggtheme/theme
        # mechanism - see logcheck.b.R's .plotColors()/.plotSeriesColors()
        # (the pilot for this change, 2026-09-17) for the full rationale.
        # ES: Tema/paleta de gráficos migrado al mecanismo nativo
        # ggtheme/theme de jamovi - ver .plotColors()/.plotSeriesColors()
        # de logcheck.b.R (el piloto de este cambio, 2026-09-17) para el
        # razonamiento completo.
        .plotColors = function(theme) {
            base_color <- if (!is.null(theme$color) && length(theme$color) >= 1) theme$color[1] else "#333333"
            accent_color <- if (!is.null(theme$color) && length(theme$color) >= 2) theme$color[2] else "#2C7FB8"
            accent_fill <- if (!is.null(theme$fill) && length(theme$fill) >= 2) theme$fill[2] else "#A6CEE3"

            list(
                point = base_color, line = base_color, ref = "gray50",
                alert = accent_color, smooth = accent_color, fill = accent_fill
            )
        },

        # NULL means "let ggtheme's own discrete fill scale handle it" -
        # used by .plotGroupBoxplots(); n is unused (kept only so the
        # existing call site does not need updating) since the color count
        # is no longer relevant when delegating to ggtheme.
        # ES: NULL significa "que la escala de relleno discreta propia de
        # ggtheme se encargue" - usado por .plotGroupBoxplots(); n no se
        # usa (se conserva solo para no tener que actualizar el sitio de
        # llamada existente) ya que la cantidad de colores deja de ser
        # relevante al delegar en ggtheme.
        .categoricalPalette = function(n) {
            NULL
        },

        .addSmoother = function(plot, x, y, smooth_color) {
            smoother <- tryCatch(self$options$residSmoother, error = function(e) "none")
            if (is.null(smoother) || length(smoother) == 0 || !nzchar(smoother))
                smoother <- "none"

            show_band <- isTRUE(self$options$residShowBand)

            if (identical(smoother, "loess")) {
                plot + ggplot2::geom_smooth(
                    ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                    method = "loess", se = show_band, linewidth = 0.6,
                    color = smooth_color,
                    fill = smooth_color
                )
            } else if (identical(smoother, "linear")) {
                plot + ggplot2::geom_smooth(
                    ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                    method = "lm", se = show_band, linewidth = 0.6,
                    color = smooth_color,
                    fill = smooth_color
                )
            } else {
                plot
            }
        },

        .labelPlotCases = function(d) {
            label_mode <- tryCatch(self$options$influenceLabelMode, error = function(e) "top5")
            if (is.null(label_mode) || length(label_mode) == 0 || !nzchar(label_mode))
                label_mode <- "top5"

            if (identical(label_mode, "none"))
                return(d[FALSE, , drop = FALSE])

            if (is.null(d) || nrow(d) == 0)
                return(d[FALSE, , drop = FALSE])

            if (!("case" %in% names(d))) d$case <- seq_len(nrow(d))
            if (!("cooksD" %in% names(d))) d$cooksD <- NA_real_
            if (!("leverage" %in% names(d))) d$leverage <- NA_real_
            if (!("studResidual" %in% names(d))) d$studResidual <- NA_real_

            n_plot <- max(1, nrow(d))
            # Use the true model parameter count (length(coef(fit)), computed in .run()
            # and carried over in d$pModel) instead of recounting selected variables:
            # length(covs) + length(factors) + 1 undercounts the real parameter count as
            # soon as a factor has more than 2 levels (each extra level adds a parameter
            # via dummy coding). See the recurring bug across AssumptionsLab: confusing
            # "selected variables" with "real parameters".
            # ES: Usar el conteo real de parámetros del modelo (length(coef(fit)),
            # calculado en .run() y transportado en d$pModel) en vez de recontar
            # variables seleccionadas: length(covs) + length(factors) + 1 subestima el
            # número real de parámetros apenas un factor tiene más de 2 niveles (cada
            # nivel extra agrega un parámetro por codificación dummy). Ver bug
            # recurrente en AssumptionsLab: confundir "variables seleccionadas" con
            # "parámetros reales".
            p_count <- if ("pModel" %in% names(d) && length(d$pModel) > 0 && is.finite(d$pModel[1]))
                d$pModel[1]
            else
                max(1, length(self$options$covs) + length(self$options$factors) + 1)

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
            if (nrow(out) == 0) return(out)
            out$caseLabel <- as.character(out$case)
            out
        },

        # A composed theme addition shared by every plot in this file: a
        # small, deliberate set of adjustments layered on top of jamovi's
        # own ggtheme (smaller subtitle/axis/legend text, faint major
        # gridlines, tighter margins) - not a competing theme choice, just
        # the kind of tweak jamovi's own plot-theme guide explicitly
        # allows after `+ ggtheme`.
        # ES: Un agregado de tema compuesto, compartido por cada gráfico de
        # este archivo: un conjunto pequeño y deliberado de ajustes
        # colocado sobre el propio ggtheme de jamovi (texto de
        # subtítulo/eje/leyenda más chico, grillas mayores tenues,
        # márgenes más ajustados) - no una elección de tema que compite,
        # solo el tipo de ajuste que la propia guía de temas de gráficos de
        # jamovi permite explícitamente después de `+ ggtheme`.
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

        .plotResidualsFitted = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image)) return()
            pal <- private$.plotColors(theme)
            d <- image$state
            d <- d[is.finite(d$fitted) & is.finite(d$residual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = residual))

            if (isTRUE(self$options$residRefLine)) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = pal$ref)
            }

            plot <- plot +
                ggplot2::geom_point(alpha = 0.75, size = 1.6, color = pal$point)

            if (length(unique(d$fitted)) >= 5) {
                plot <- private$.addSmoother(plot, "fitted", "residual", pal$smooth)
            }

            plot <- plot +
                ggplot2::labs(
                    x = .("Fitted values"),
                    y = .("Residuals")
                ) +
                ggtheme + private$.plotThemeExtra()

            print(plot)
        },

        .plotQQResiduals = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image)) return()
            pal <- private$.plotColors(theme)
            d <- image$state
            d <- d[is.finite(d$stdResidual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(sample = stdResidual)) +
                ggplot2::stat_qq(alpha = 0.75, size = 1.6, color = pal$point) +
                ggplot2::stat_qq_line(linewidth = 0.6, color = pal$line) +
                ggplot2::labs(
                    x = .("Theoretical quantiles"),
                    y = .("Standardized residuals")
                ) +
                ggtheme + private$.plotThemeExtra()

            print(plot)
        },

        .plotResidualHistogram = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image)) return()
            pal <- private$.plotColors(theme)
            d <- image$state
            d <- d[is.finite(d$stdResidual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = stdResidual)) +
                ggplot2::geom_histogram(bins = 30, alpha = 0.85,
                                         fill = pal$fill,
                                         color = pal$line) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = pal$ref) +
                ggplot2::labs(
                    x = .("Standardized residuals"),
                    y = .("Count")
                ) +
                ggtheme + private$.plotThemeExtra()

            print(plot)
        },

        .plotResidualNormalCurve = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image)) return()
            pal <- private$.plotColors(theme)
            d <- image$state
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
                ggplot2::geom_density(linewidth = 0.8, color = pal$smooth) +
                ggplot2::geom_line(
                    data = normal_df,
                    ggplot2::aes(x = stdResidual, y = density, linetype = curve),
                    linewidth = 0.6,
                    color = pal$alert
                ) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = pal$ref) +
                ggplot2::scale_linetype_manual(values = "dashed", name = NULL) +
                ggplot2::labs(
                    x = .("Standardized residuals"),
                    y = .("Density")
                ) +
                ggtheme + private$.plotThemeExtra()

            print(plot)
        },

        .plotGroupBoxplots = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requireGroupPlotData(image)) return()
            colors <- private$.plotColors(theme)
            d <- image$state
            d <- d[is.finite(d$value), , drop = FALSE]
            d$cell <- factor(d$cell, levels = unique(d$cell))
            fillPal <- private$.categoricalPalette(nlevels(d$cell))

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = cell, y = value, fill = cell)) +
                ggplot2::geom_boxplot(
                    width = 0.65,
                    outlier.colour = colors$alert,
                    outlier.alpha = 0.85,
                    outlier.size = 1.6,
                    color = colors$line,
                    show.legend = FALSE
                ) +
                ggplot2::labs(
                    x = .("Group / cell"),
                    y = .("Observed values")
                ) +
                ggtheme + private$.plotThemeExtra() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))

            if (!is.null(fillPal))
                plot <- plot + ggplot2::scale_fill_manual(values = fillPal)

            print(plot)
        },

        .plotResidualsLeverage = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image)) return()
            pal <- private$.plotColors(theme)
            d <- image$state
            d <- d[is.finite(d$leverage) & is.finite(d$studResidual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = leverage, y = studResidual))

            if (isTRUE(self$options$influenceShowThreshold)) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = c(-3, 0, 3), linetype = "dashed", color = pal$ref)
            }

            plot <- plot +
                ggplot2::geom_point(ggplot2::aes(size = cooksD), alpha = 0.75, color = pal$point) +
                ggplot2::scale_size_continuous(name = "Cook's D", range = c(1.2, 4)) +
                ggplot2::labs(
                    x = .("Leverage"),
                    y = .("Studentized residuals")
                ) +
                ggtheme + private$.plotThemeExtra()

            lab <- private$.labelPlotCases(d)
            if (!is.null(lab) && nrow(lab) > 0) {
                plot <- plot + ggplot2::geom_text(
                    data = lab,
                    ggplot2::aes(x = leverage, y = studResidual, label = caseLabel),
                    inherit.aes = FALSE, size = 3, hjust = -0.1, vjust = -0.5,
                    check_overlap = TRUE, color = pal$alert
                )
            }

            print(plot)
        },

        .plotCooksD = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            if (!private$.requirePlotData(image)) return()
            pal <- private$.plotColors(theme)
            d <- image$state
            d <- d[is.finite(d$case) & is.finite(d$cooksD), , drop = FALSE]
            cut <- 4 / max(1, nrow(d))

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = case, y = cooksD)) +
                ggplot2::geom_col(alpha = 0.85, fill = pal$fill, color = pal$line)

            if (isTRUE(self$options$influenceShowThreshold)) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = cut, linetype = "dashed", color = pal$alert)
            }

            plot <- plot +
                ggplot2::labs(
                    x = .("Case"),
                    y = .("Cook's D")
                ) +
                ggtheme + private$.plotThemeExtra()

            lab <- private$.labelPlotCases(d)
            if (!is.null(lab) && nrow(lab) > 0) {
                plot <- plot + ggplot2::geom_text(
                    data = lab,
                    ggplot2::aes(x = case, y = cooksD, label = caseLabel),
                    inherit.aes = FALSE, size = 3, vjust = -0.6,
                    check_overlap = TRUE, color = pal$alert
                )
            }

            print(plot)
        }
    )
)
