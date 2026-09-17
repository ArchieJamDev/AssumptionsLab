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
# Logistic Regression.
# ES: Regresión Logística.
#
# This file implements logCheck: an assumption-diagnostic module for binary
# logistic regression (one binary dependent variable, numeric and/or
# categorical predictors). It reports the assumption diagnostics that
# decide whether a logistic regression's coefficients and odds ratios are
# trustworthy (complete separation, linearity in the logit via Box-Tidwell,
# goodness of fit, discrimination via ROC/AUC, multicollinearity among
# predictors, and influential cases).
#
# ES: Este archivo implementa logCheck: un módulo de diagnóstico de
# supuestos para regresión logística binaria (una variable dependiente
# binaria, predictores numéricos y/o categóricos). Reporta los
# diagnósticos de supuestos que deciden si los coeficientes y odds ratios
# de una regresión logística son confiables (separación completa,
# linealidad en el logit vía Box-Tidwell, bondad de ajuste, discriminación
# vía ROC/AUC, multicolinealidad entre predictores, y casos influyentes).
#
# Responsibilities
# 1. Fit the binary logistic regression model from the user's selected
#    dependent variable and predictors, and detect complete/quasi-complete
#    separation before trusting the fit.
# 2. Compute and report the full assumption-diagnostic battery (linearity
#    in the logit, goodness of fit, discrimination, multicollinearity,
#    influential cases).
# 3. Render the diagnostic plots (empirical logit, calibration, ROC,
#    Cook's D), parametrized per area and per the user's plot-set
#    selection.
# 4. Assemble the odds-ratio table and the applied-interpretation text for
#    every diagnostic area, in the user's selected report language.
#
# ES: Responsabilidades
# 1. Ajustar el modelo de regresión logística binaria a partir de la
#    variable dependiente y predictores seleccionados por el usuario, y
#    detectar separación completa/cuasi-completa antes de confiar en el
#    ajuste.
# 2. Calcular y reportar la batería completa de diagnósticos de supuestos
#    (linealidad en el logit, bondad de ajuste, discriminación,
#    multicolinealidad, casos influyentes).
# 3. Renderizar los gráficos diagnósticos (logit empírico, calibración,
#    ROC, Cook's D), parametrizados por área y según la selección del
#    usuario.
# 4. Ensamblar la tabla de odds ratios y el texto de interpretación
#    aplicada para cada área diagnóstica, en el idioma de informe
#    seleccionado por el usuario.
#
# Workflow
# 1. Fit: build and estimate the logistic regression model; check for
#    complete separation before proceeding.
# 2. Diagnose: compute linearity in the logit (Box-Tidwell), goodness of
#    fit, discrimination (ROC/AUC), multicollinearity, and influential
#    cases.
# 3. Plot: render the diagnostic plots belonging to each area, per the
#    user's selected options.
# 4. Interpret: build the applied-interpretation text for every
#    diagnostic area and the odds-ratio table.
#
# ES: Flujo de trabajo
# 1. Ajustar: construir y estimar el modelo de regresión logística;
#    verificar separación completa antes de continuar.
# 2. Diagnosticar: calcular linealidad en el logit (Box-Tidwell), bondad
#    de ajuste, discriminación (ROC/AUC), multicolinealidad y casos
#    influyentes.
# 3. Graficar: renderizar los gráficos diagnósticos de cada área, según
#    las opciones seleccionadas por el usuario.
# 4. Interpretar: construir el texto de interpretación aplicada para cada
#    área diagnóstica y la tabla de odds ratios.
# -----------------------------------------------------------------------------

logCheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "logCheckClass",
    inherit = logCheckBase,
    private = list(
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

            # -----------------------------------------------------------------------------
            # Initial setup and shared helpers (AssumptionsLab standard).
            # ES: Configuración inicial y helpers compartidos (estándar AssumptionsLab).
            # -----------------------------------------------------------------------------
            lang <- .al_normalize_lang(self$options$reportLang)

            tr <- function(en, es) .al_tr(lang, en, es)

            txt <- function(key) {
                .al_text(lang, "logistic", key)
            }

            html_escape <- .al_html_escape

            html_block <- function(title = NULL, text, paragraphs = TRUE, escape = TRUE, raw = FALSE) {
                .al_html_block(title, text, paragraphs = paragraphs, escape = escape, raw = raw)
            }

            html_guide <- function(title, key) {
                html_block(title, .al_html_list(txt(key)), raw = TRUE)
            }

            clean_num <- function(x) {
                if (length(x) == 0)
                    return(NA_real_)
                x <- suppressWarnings(as.numeric(x[1]))
                if (is.na(x) || is.nan(x) || is.infinite(x))
                    return(NA_real_)
                x
            }

            fmt_num <- function(x, digits = 3) {
                x <- clean_num(x)
                if (is.na(x))
                    return(tr("Not computed", "No calculado"))
                format(round(x, digits), nsmall = digits)
            }

            # p_sig(): identical logic (via clean_num) in every module,
            # consolidated in shared-helpers.R (.al_p_sig).
            # ES: idéntica (vía clean_num) en todos los módulos,
            # consolidada en shared-helpers.R.
            p_sig <- .al_p_sig

            add_row <- function(table, key, values) {
                table$addRow(rowKey = key, values = values)
            }

            # Backtick-quotes a variable name before it goes into a formula
            # string. Without this, a predictor name containing a space or
            # another character that isn't valid in a bare R identifier
            # (common in jamovi datasets, e.g. imported from spreadsheet
            # column headers) breaks as.formula()'s parser - this was
            # reported as a real crash by jamovi's own module review (Sep
            # 2026) and mirrors the qname() helper already used in
            # ordcheck.b.R for the same reason.
            # ES: cita con comillas invertidas un nombre de variable antes
            # de usarlo en una fórmula. Sin esto, un predictor con espacio u
            # otro carácter no válido en un identificador de R (común en
            # datasets de jamovi importados de encabezados de hoja de
            # cálculo) rompe el parser de as.formula() - esto se reportó
            # como un crash real en la revisión del módulo de jamovi (sep.
            # 2026) y replica el helper qname() ya usado en ordcheck.b.R por
            # la misma razón.
            qname <- function(x) {
                paste0("`", gsub("`", "", x), "`")
            }

            # The inverse of qname(), for display only: coefficient/rowname
            # labels pulled from a fitted model (e.g. rownames(coef_table),
            # names(coef(model))) carry qname()'s backticks whenever the
            # original name needed them to parse, and that's correct for
            # re-use in another formula/regex, but showing the backticks to
            # the user in a results table is just visual noise.
            # ES: la inversa de qname(), solo para mostrar: las etiquetas de
            # coeficiente/nombre de fila tomadas de un modelo ajustado
            # llevan las comillas invertidas de qname() cuando el nombre
            # original las necesitó para analizarse, y eso es correcto para
            # reutilizarlas en otra fórmula/regex, pero mostrarle esas
            # comillas al usuario en una tabla de resultados es solo ruido
            # visual.
            strip_qname <- function(x) gsub("^`|`$", "", x)

            set_result_titles <- function() {

                set_title_safe <- function(name, en, es) {
                    element <- tryCatch(
                        self$results[[name]],
                        error = function(e) NULL
                    )

                    if (is.null(element))
                        return(invisible(FALSE))

                    tryCatch(
                        element$setTitle(tr(en, es)),
                        error = function(e) invisible(FALSE)
                    )

                    invisible(TRUE)
                }

                titles <- list(
                    c("intro", "Introduction", "Introducción"),
                    c("designGuide", "Model design", "Diseño del modelo"),
                    c("design", "Design summary", "Resumen del diseño"),

                    c("separationGuide", "Complete separation", "Separación completa"),
                    c("separation", "Complete separation", "Separación completa"),
                    c("separationInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("linearityGuide", "Linearity in the logit", "Linealidad en el logit"),
                    c("linearity",
                      "Linearity in the logit (Box-Tidwell)",
                      "Linealidad en el logit (Box-Tidwell)"),
                    c("linearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("linearityPlot",
                      "Empirical logit vs predictor",
                      "Logit empírico vs predictor"),

                    c("goodnessOfFitGuide", "Goodness of fit", "Bondad de ajuste"),
                    c("goodnessOfFit", "Goodness of fit", "Bondad de ajuste"),
                    c("goodnessOfFitInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("calibrationPlot",
                      "Calibration by probability deciles",
                      "Calibración por deciles de probabilidad"),

                    c("discriminationGuide", "Discrimination", "Discriminación"),
                    c("discrimination", "Discrimination", "Discriminación"),
                    c("discriminationInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("discriminationPlot", "ROC curve", "Curva ROC"),

                    c("multicollinearityGuide", "Multicollinearity", "Multicolinealidad"),
                    c("multicollinearity", "Multicollinearity", "Multicolinealidad"),
                    c("multicollinearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("correlationMatrixGuide", "Correlation Matrix", "Matriz de Correlaciones"),
                    c("pearsonMatrixTable",
                      "Pearson correlation matrix (APA 7 format)",
                      "Matriz de correlaciones de Pearson (formato APA 7)"),
                    c("dcorMatrixTable",
                      "Distance correlation matrix (dCor, APA 7 format)",
                      "Matriz de correlación de distancia (dCor, formato APA 7)"),
                    c("correlationComparisonGuide",
                      "Pearson / dCor / Copula Entropy Discordance Analysis",
                      "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                    c("correlationComparisonTable",
                      "Pairs with a notable difference between Pearson and dCor",
                      "Pares con diferencia notable entre Pearson y dCor"),
                    c("correlationComparisonInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("influenceGuide", "Influential cases", "Casos influyentes"),
                    c("influence", "Influential cases", "Casos influyentes"),
                    c("influenceInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("influencePlot", "Cook's D by case", "Cook's D por caso"),

                    c("oddsRatiosGuide", "Odds Ratios", "Odds Ratios"),
                    c("oddsRatios", "Odds Ratios", "Odds Ratios"),
                    c("oddsRatiosInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("notes", "Notes and recommendation", "Notas y recomendación")
                )

                for (item in titles)
                    set_title_safe(item[[1]], item[[2]], item[[3]])
            }

            set_result_titles()

            set_table_column_titles <- function() {

                set_col_title_safe <- function(table_name, col_name, en, es) {
                    table <- tryCatch(
                        self$results[[table_name]],
                        error = function(e) NULL
                    )

                    if (is.null(table))
                        return(invisible(FALSE))

                    column <- tryCatch(
                        table$getColumn(col_name),
                        error = function(e) NULL
                    )

                    if (is.null(column))
                        return(invisible(FALSE))

                    tryCatch(
                        column$setTitle(tr(en, es)),
                        error = function(e) invisible(FALSE)
                    )

                    invisible(TRUE)
                }

                cols <- list(
                    c("design", "element", "Element", "Elemento"),
                    c("design", "value", "Value", "Valor"),

                    c("separation", "predictor", "Predictor", "Predictor"),
                    c("separation", "type", "Type", "Tipo"),
                    c("separation", "coefficient", "Coefficient", "Coeficiente"),

                    c("linearity", "predictor", "Predictor", "Predictor"),
                    c("linearity", "statistic", "Statistic", "Estadístico"),
                    c("linearity", "pSig", "Sig.", "Sig."),

                    c("goodnessOfFit", "test", "Test", "Prueba"),
                    c("goodnessOfFit", "statistic", "Statistic", "Estadístico"),
                    c("goodnessOfFit", "pSig", "Sig.", "Sig."),

                    c("discrimination", "metric", "Metric", "Métrica"),
                    c("discrimination", "value", "Value", "Valor"),

                    c("multicollinearity", "diagnostic", "Diagnostic", "Diagnóstico"),
                    c("multicollinearity", "item", "Item", "Elemento"),
                    c("multicollinearity", "statistic", "Statistic", "Estadístico"),
                    c("multicollinearity", "value", "Value", "Valor"),

                    c("pearsonMatrixTable", "var", "Variable", "Variable"),
                    c("dcorMatrixTable", "var", "Variable", "Variable"),

                    c("correlationComparisonTable", "var1", "Variable 1", "Variable 1"),
                    c("correlationComparisonTable", "var2", "Variable 2", "Variable 2"),
                    c("correlationComparisonTable", "pearson", "Pearson r", "Pearson r"),
                    c("correlationComparisonTable", "dcor", "dCor", "dCor"),
                    c("correlationComparisonTable", "gap", "Gap (dCor - |r|)", "Brecha (dCor − |r|)"),
                    c("correlationComparisonTable", "ce", "Copula entropy", "Entropía copular"),
                    c("correlationComparisonTable", "ceP", "p (CE)", "p (CE)"),
                    c("correlationComparisonTable", "ceSig", "Sig. (CE)", "Sig. (CE)"),
                    c("correlationComparisonTable", "flag", "Notable difference", "Diferencia notable"),

                    c("influence", "case", "Case", "Caso"),
                    c("influence", "cooksD", "Cook's D", "Cook's D"),
                    c("influence", "leverage", "Leverage", "Leverage"),

                    c("oddsRatios", "predictor", "Predictor", "Predictor"),
                    c("oddsRatios", "or", "OR", "OR"),
                    c("oddsRatios", "ciLower", "95% CI lower", "IC 95% inferior"),
                    c("oddsRatios", "ciUpper", "95% CI upper", "IC 95% superior"),
                    c("oddsRatios", "pSig", "Sig.", "Sig.")
                )

                for (item in cols)
                    set_col_title_safe(item[[1]], item[[2]], item[[3]], item[[4]])
            }

            set_table_column_titles()

            dep_var <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors
            show_plots <- self$options$showPlots

            data <- self$data
            if (is.null(data) || nrow(data) == 0) {
                jmvcore::reject(tr("No data are available.", "No hay datos disponibles."))
            }

            if (is.null(dep_var) || !(dep_var %in% names(data))) {
                jmvcore::reject(tr("Dependent variable not selected.", "Variable dependiente no seleccionada."))
            }

            dep_data <- data[[dep_var]]

            # ------------------------------------------------------------
            # Conversion to binary, making explicit which level is the "event".
            # ES: Conversión a binaria, dejando explícito cuál nivel es "evento".
            # ------------------------------------------------------------
            event_label <- NULL
            reference_label <- NULL

            if (is.factor(dep_data)) {
                levels_dep <- levels(dep_data)
                if (length(levels_dep) != 2) {
                    jmvcore::reject(tr(
                        "The variable must have exactly 2 levels.",
                        "La variable debe tener 2 niveles."
                    ))
                }
                reference_label <- levels_dep[1]
                event_label <- levels_dep[2]
                dep_binary <- as.integer(dep_data == levels_dep[2])
            } else if (is.numeric(dep_data)) {
                unique_vals <- sort(unique(na.omit(dep_data)))
                if (length(unique_vals) != 2) {
                    jmvcore::reject(tr(
                        "The variable must have exactly 2 unique values.",
                        "La variable debe tener 2 valores únicos."
                    ))
                }
                reference_label <- as.character(unique_vals[1])
                event_label <- as.character(unique_vals[2])
                dep_binary <- as.integer(dep_data == unique_vals[2])
            } else {
                jmvcore::reject(tr(
                    "The variable must be binary or a 2-level factor.",
                    "Variable debe ser binaria o factor de 2 niveles."
                ))
            }

            predictors <- c(covs, factors)
            if (length(predictors) == 0) {
                jmvcore::reject(tr("Select at least one predictor.", "Seleccione al menos un predictor."))
            }

            model_data <- data
            model_data$dep_binary <- dep_binary
            complete_cases <- complete.cases(model_data[, c("dep_binary", predictors), drop = FALSE])
            model_data_complete <- model_data[complete_cases, ]

            n_total <- nrow(data)
            n_complete <- nrow(model_data_complete)
            n_events <- sum(model_data_complete$dep_binary)
            n_non_events <- n_complete - n_events
            n_predictors <- length(predictors)
            epv <- min(n_events, n_non_events) / max(n_predictors, 1)

            formula_str <- paste("dep_binary ~", paste(qname(predictors), collapse = " + "))
            formula <- as.formula(formula_str)

            model_error <- NULL
            model <- tryCatch({
                glm(formula, data = model_data_complete, family = binomial(link = "logit"))
            }, error = function(e) {
                model_error <<- conditionMessage(e)
                NULL
            })

            if (is.null(model)) {
                jmvcore::reject(paste0(
                    tr("Error fitting the model: ", "Error al ajustar el modelo: "),
                    model_error
                ))
            }

            model_summary <- summary(model)
            coef_table <- model_summary$coefficients

            # -----------------------------------------------------------------------------
            # Report introduction.
            # ES: Introducción del informe.
            # -----------------------------------------------------------------------------
            self$results$intro$setContent(paste0(
                "<div style=\"max-width: 7.25in; width: 100%; box-sizing: border-box;\">",
                "<p style=\"font-weight: 700; margin: 0 0 0.10em 0; line-height: 1.25;\">",
                "AssumptionsLab",
                "</p>",
                "<p style=\"margin: 0 0 0.35em 0; line-height: 1.25;\">",
                tr("Assumption check for logistic regression", "Revisión de supuestos para regresión logística"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">\u00A0</p>",
                "<p style=\"margin: 0 0 0.55em 0; line-height: 1.35;\">",
                tr(
                    "Use this analysis when you want to review whether a logistic regression model has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.",
                    "Use este análisis cuando quiera revisar si un modelo de regresión logística tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."
                ),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">\u00A0</p>",
                html_block(NULL, c(
                paste0(
                    tr("<b>Reference level (0):</b> ", "<b>Nivel de referencia (0):</b> "),
                    html_escape(reference_label),
                    tr(" | <b>Event (1):</b> ", " | <b>Evento (1):</b> "),
                    html_escape(event_label)
                ),
                paste0(
                    tr("<b>Complete cases:</b> ", "<b>Casos completos:</b> "), n_complete,
                    tr(" | <b>Events:</b> ", " | <b>Eventos:</b> "), n_events,
                    tr(" | <b>Non-events:</b> ", " | <b>No eventos:</b> "), n_non_events
                ),
                paste0(tr("<b>EPV (least-frequent category):</b> ", "<b>EPV (categoría menos frecuente):</b> "), fmt_num(epv, 1))
            ), paragraphs = TRUE, escape = FALSE),
                "</div>"
            ))

            # -----------------------------------------------------------------------------
            # Model design.
            # ES: Diseño del modelo.
            # -----------------------------------------------------------------------------
            self$results$designGuide$setContent(html_guide(tr("Model design", "Diseño del modelo"), "designGuide"))

            add_row(self$results$design, "design_1", list(element = tr("Total cases", "Casos totales"), value = as.character(n_total)))
            add_row(self$results$design, "design_2", list(element = tr("Complete cases", "Casos completos"), value = as.character(n_complete)))
            add_row(self$results$design, "design_3", list(element = tr("Events", "Eventos"), value = as.character(n_events)))
            add_row(self$results$design, "design_4", list(element = tr("EPV (least-frequent category)", "EPV (categoría menos frecuente)"), value = fmt_num(epv, 2)))

            # -----------------------------------------------------------------------------
            # Complete separation.
            # ES: Separación completa.
            # -----------------------------------------------------------------------------
            self$results$separationGuide$setContent(html_guide(tr("Complete separation", "Separación completa"), "separationGuide"))

            n_separations <- 0
            if (nrow(coef_table) > 1) {
                for (i in 2:nrow(coef_table)) {
                    predictor_name <- strip_qname(rownames(coef_table)[i])
                    coef_val <- coef_table[i, 1]
                    se_val <- coef_table[i, 2]

                    if (abs(coef_val) > 5 && se_val > 2) {
                        n_separations <- n_separations + 1
                        add_row(self$results$separation, paste0("sep_", n_separations), list(
                            predictor = predictor_name,
                            type = tr("Complete separation", "Separación completa"),
                            coefficient = coef_val
                        ))
                    }
                }
            }
            if (n_separations == 0) {
                add_row(self$results$separation, "sep_none", list(
                    predictor = tr("Not detected", "No detectada"),
                    type = "-",
                    coefficient = NA
                ))
            }

            self$results$separationInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (n_separations > 0) {
                    c(
                        tr(
                            paste0(n_separations, " of ", n_predictors, " predictor(s) show coefficients and standard errors consistent with complete or quasi-complete separation. Estimates for these predictors are not reliable and should not be interpreted at face value."),
                            paste0(n_separations, " de ", n_predictors, " predictor(es) muestran coeficientes y errores estándar consistentes con separación completa o cuasi-completa. Las estimaciones de esos predictores no son confiables y no deben interpretarse literalmente.")
                        ),
                        tr(
                            "Common error: reporting the odds ratio for a separated predictor as if it were a normal, stable estimate; a coefficient of, say, 8 with a standard error of 4 does not mean a huge, precise effect - it means the algorithm could not find a finite solution.",
                            "Error común: reportar el odds ratio de un predictor separado como si fuera una estimación estable normal; un coeficiente de, por ejemplo, 8 con un error estándar de 4 no significa un efecto enorme y preciso - significa que el algoritmo no pudo encontrar una solución finita."
                        ),
                        tr(
                            paste0("Sample-size context: with ", n_events, " events and ", n_non_events, " non-events, small or unevenly split subgroups within a predictor are the most common cause of this pattern."),
                            paste0("Contexto de tamaño de muestra: con ", n_events, " eventos y ", n_non_events, " no eventos, los subgrupos pequeños o muy desiguales dentro de un predictor son la causa más frecuente de este patrón.")
                        ),
                        tr(
                            "Consider Firth's (1993) penalized logistic regression, which corrects the maximum-likelihood bias and yields finite estimates under separation, or combine rare categories before trusting these coefficients.",
                            "Considere la regresión logística penalizada de Firth (1993), que corrige el sesgo de máxima verosimilitud y produce estimaciones finitas bajo separación, o combine categorías poco frecuentes antes de confiar en estos coeficientes."
                        )
                    )
                } else {
                    c(
                        tr(
                            "No coefficient/standard-error pattern consistent with complete separation was detected.",
                            "No se detectó ningún patrón de coeficiente/error estándar consistente con separación completa."
                        ),
                        tr(
                            "This check only screens for the extreme coefficient/standard-error signature; it does not guarantee that every category combination has adequate cell counts, which is worth a quick manual check with sparse categorical predictors.",
                            "Esta revisión solo detecta la firma extrema de coeficiente/error estándar; no garantiza que cada combinación de categorías tenga conteos de celda adecuados, algo que vale la pena revisar manualmente con predictores categóricos poco frecuentes."
                        )
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Linearity in the logit (Box-Tidwell).
            # ES: Linealidad en el logit (Box-Tidwell).
            # -----------------------------------------------------------------------------
            self$results$linearityGuide$setContent(html_guide(tr("Linearity in the logit", "Linealidad en el logit"), "linearityGuide"))

            bt_tested <- 0
            bt_significant <- 0
            bt_skipped <- character(0)

            if (length(covs) == 0) {
                self$results$linearity$addRow(rowKey = "lin_none", values = list(
                    predictor = tr("Not applicable", "No aplicable"),
                    statistic = NA_real_,
                    p = NA_real_,
                    pSig = ""
                ))
            }

            if (length(covs) > 0) {
                for (predictor in covs) {
                    x <- model_data_complete[[predictor]]

                    if (!is.numeric(x) || any(x <= 0, na.rm = TRUE)) {
                        bt_skipped <- c(bt_skipped, predictor)
                        next
                    }

                    bt_formula <- as.formula(paste0(
                        "dep_binary ~ ", paste(qname(predictors), collapse = " + "),
                        " + ", qname(predictor), ":log(", qname(predictor), ")"
                    ))

                    bt_model <- tryCatch(
                        glm(bt_formula, data = model_data_complete, family = binomial(link = "logit")),
                        error = function(e) NULL
                    )

                    if (is.null(bt_model))
                        next

                    bt_coefs <- summary(bt_model)$coefficients

                    # The interaction term was built with qname() above, so
                    # its coefficient name comes back from glm() with the
                    # same backticks (R preserves them whenever a term
                    # needed quoting to parse), and the resulting rowname is
                    # deterministic - a plain "`x`:log(`x`)" string, verified
                    # empirically - so this looks it up by exact match
                    # instead of building a regex from the predictor name:
                    # jamovi's review team flagged that a name containing
                    # regex metacharacters (parentheses, ".", "+", etc. -
                    # e.g. "Income (2024)") would silently break a
                    # regex-based lookup even after backtick-quoting, since
                    # quoting makes a term parse as R syntax but does not
                    # escape it for grep().
                    # ES: el término de interacción se construyó con
                    # qname() arriba, así que su nombre de coeficiente
                    # vuelve de glm() con las mismas comillas invertidas (R
                    # las conserva siempre que un término las necesitó para
                    # analizarse), y el nombre resultante es determinístico
                    # - una cadena simple "`x`:log(`x`)", verificado
                    # empíricamente - así que aquí se busca por coincidencia
                    # exacta en vez de construir un regex a partir del
                    # nombre del predictor: el equipo de revisión de jamovi
                    # señaló que un nombre con metacaracteres de regex
                    # (paréntesis, ".", "+", etc. - p. ej. "Income (2024)")
                    # rompería silenciosamente una búsqueda basada en regex
                    # incluso después de citarlo con comillas invertidas, ya
                    # que citarlo lo hace válido como sintaxis de R pero no
                    # lo escapa para grep().
                    bt_term_expected <- paste0(
                        qname(predictor), ":log(", qname(predictor), ")"
                    )
                    idx <- match(bt_term_expected, rownames(bt_coefs))
                    idx <- if (is.na(idx)) integer(0) else idx

                    if (length(idx) == 1) {
                        bt_tested <- bt_tested + 1
                        stat_val <- bt_coefs[idx, "z value"]
                        p_val <- bt_coefs[idx, "Pr(>|z|)"]
                        if (!is.na(p_val) && p_val < .05)
                            bt_significant <- bt_significant + 1

                        self$results$linearity$addRow(rowKey = paste0("lin_", predictor), values = list(
                            predictor = predictor,
                            statistic = stat_val,
                            p = p_val,
                            pSig = p_sig(p_val)
                        ))
                    }
                }
            }

            linearity_interp <- character(0)
            if (length(covs) == 0) {
                linearity_interp <- tr(
                    "No numeric predictors were included, so the Box-Tidwell linearity-in-the-logit check does not apply.",
                    "No se incluyeron predictores numéricos, por lo que la prueba de linealidad en el logit (Box-Tidwell) no aplica."
                )
            } else if (bt_tested == 0) {
                linearity_interp <- tr(
                    "The Box-Tidwell test could not be computed for the numeric predictors (it requires strictly positive values).",
                    "No fue posible calcular la prueba de Box-Tidwell para los predictores numéricos (requiere valores estrictamente positivos)."
                )
            } else {
                linearity_interp <- c(
                    paste0(
                        tr(
                            paste0(bt_significant, " of ", bt_tested, " numeric predictor(s) showed a significant Box-Tidwell interaction (Box & Tidwell, 1962), suggesting the relationship with the logit may not be linear for those predictors."),
                            paste0(bt_significant, " de ", bt_tested, " predictor(es) numérico(s) mostraron una interacción de Box-Tidwell significativa (Box & Tidwell, 1962), lo que sugiere que la relación con el logit podría no ser lineal para esos predictores.")
                        )
                    ),
                    tr(
                        "Common error: applying this check to categorical predictors, or concluding non-linearity from the raw scatterplot of the outcome against the predictor. Linearity in the logit is only meaningful for numeric predictors and is properly assessed with the interaction test or the \"Empirical Logit vs Predictor\" chart, not with the raw 0/1 scatter.",
                        "Error común: aplicar esta revisión a predictores categóricos, o concluir no linealidad a partir del diagrama de dispersión crudo de la variable dependiente contra el predictor. La linealidad en el logit solo tiene sentido para predictores numéricos y se evalúa correctamente con la prueba de interacción o el gráfico \"Logit empírico vs predictor\", no con la dispersión cruda 0/1."
                    ),
                    tr(
                        paste0("Sample-size caveat: with ", n_complete, " complete cases, the Box-Tidwell interaction term has less power to detect real curvature than the main effects do; a non-significant result here is weaker evidence of linearity than a non-significant result would be with a much larger sample."),
                        paste0("Matiz de tamaño de muestra: con ", n_complete, " casos completos, el término de interacción de Box-Tidwell tiene menos poder para detectar curvatura real que los efectos principales; un resultado no significativo aquí es evidencia más débil de linealidad de lo que sería con una muestra mucho más grande.")
                    ),
                    tr(
                        "If non-linearity is confirmed, consider adding a quadratic or spline term for that predictor, or categorizing it into clinically or theoretically meaningful groups.",
                        "Si se confirma la no linealidad, considere agregar un término cuadrático o spline para ese predictor, o categorizarlo en grupos con sentido clínico o teórico."
                    )
                )
            }

            if (length(bt_skipped) > 0) {
                linearity_interp <- c(linearity_interp, paste0(
                    tr(
                        "Not tested (non-positive or non-numeric values): ",
                        "No evaluado (valores no positivos o no numéricos): "
                    ),
                    paste(bt_skipped, collapse = ", ")
                ))
            }

            self$results$linearityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                linearity_interp,
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Goodness of fit.
            # ES: Bondad de ajuste.
            # -----------------------------------------------------------------------------
            self$results$goodnessOfFitGuide$setContent(html_guide(tr("Goodness of fit", "Bondad de ajuste"), "goodnessOfFitGuide"))

            deviance_val <- model$deviance
            deviance_df <- model$df.residual
            deviance_p <- pchisq(deviance_val, deviance_df, lower.tail = FALSE)

            self$results$goodnessOfFit$addRow(rowKey = "gof_deviance", values = list(
                test = "Deviance",
                statistic = deviance_val,
                p = deviance_p,
                pSig = p_sig(deviance_p)
            ))

            hl_test <- NULL
            hl_reason <- NULL
            if (!requireNamespace("ResourceSelection", quietly = TRUE)) {
                hl_reason <- tr(
                    "The 'ResourceSelection' package is not installed.",
                    "El paquete 'ResourceSelection' no está instalado."
                )
            } else {
                hl_test <- tryCatch({
                    ResourceSelection::hoslem.test(model_data_complete$dep_binary, fitted(model), g = 10)
                }, error = function(e) NULL)

                # hoslem.test() does not always throw on a degenerate input -
                # when the data cannot support 10 distinct bins it instead
                # warns ("did not allow for the requested number of bins")
                # and returns a result with p.value = NA, the same
                # non-throwing "soft failure" gotcha already found in
                # tseries/nortest/relatedCheck's manual skewness test - so a
                # result is only accepted when its p-value is actually
                # finite, not merely non-NULL.
                # ES: hoslem.test() no siempre lanza error ante una entrada
                # degenerada - cuando los datos no soportan 10 bins distintos,
                # en cambio avisa ("did not allow for the requested number of
                # bins") y devuelve un resultado con p.value = NA, el mismo
                # "fallo blando" sin lanzar error ya encontrado en tseries/
                # nortest/la prueba manual de asimetría de relatedCheck - así
                # que un resultado solo se acepta cuando su p-valor es
                # realmente finito, no solo por no ser NULL.
                if (!is.null(hl_test) && !is.finite(hl_test$p.value))
                    hl_test <- NULL

                if (is.null(hl_test))
                    hl_reason <- tr(
                        "Could not be computed - the predicted probabilities are not spread out enough to form 10 distinct groups (too few observations or too little variation in fitted probabilities).",
                        "No se pudo calcular - las probabilidades predichas no están suficientemente dispersas para formar 10 grupos distintos (muy pocas observaciones o muy poca variación en las probabilidades ajustadas)."
                    )
            }

            self$results$goodnessOfFit$addRow(rowKey = "gof_hl", values = list(
                test = "Hosmer-Lemeshow",
                statistic = if (!is.null(hl_test)) hl_test$statistic else NA_real_,
                p = if (!is.null(hl_test)) hl_test$p.value else NA_real_,
                pSig = if (!is.null(hl_test)) p_sig(hl_test$p.value) else ""
            ))
            if (!is.null(hl_reason))
                tryCatch(
                    self$results$goodnessOfFit$addFootnote(col = "p", note = hl_reason, rowKey = "gof_hl"),
                    error = function(e) invisible(NULL)
                )

            self$results$goodnessOfFitInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    paste0(
                        tr("Deviance / df ratio: ", "Razón devianza / gl: "), fmt_num(deviance_val / deviance_df, 2),
                        tr(
                            ". Values well above 1 suggest overdispersion or poor fit, though this ratio is only a rough signal.",
                            ". Valores muy por encima de 1 sugieren sobredispersión o ajuste pobre, aunque esta razón es solo una señal aproximada."
                        )
                    ),
                    tr(
                        "Caveat: the residual-deviance chi-square approximation is only strictly valid when data are grouped (repeated covariate patterns); with individual-level data, as here, it is an approximate reference, not a precise test.",
                        "Matiz: la aproximación de ji-cuadrado sobre la devianza residual es estrictamente válida solo con datos agrupados (patrones de covariables repetidos); con datos a nivel individual, como aquí, es una referencia aproximada, no una prueba precisa."
                    ),
                    if (!is.null(hl_test)) {
                        hl_p_str <- if (hl_test$p.value < .001) "< .001" else paste0("= ", fmt_num(hl_test$p.value, 3))
                        paste0(
                            "Hosmer-Lemeshow (1980): p ", hl_p_str, ". ",
                            tr(
                                if (hl_test$p.value >= .05)
                                    "Compatible with adequate fit across probability groups."
                                else
                                    "Suggests the model does not fit well in some probability range; review the \"Calibration by Probability Decile\" chart.",
                                if (hl_test$p.value >= .05)
                                    "Compatible con un ajuste adecuado entre grupos de probabilidad."
                                else
                                    "Sugiere que el modelo no ajusta bien en algún rango de probabilidades; revise el gráfico \"Calibración por deciles de probabilidad\"."
                            )
                        )
                    } else {
                        paste0(
                            tr("Hosmer-Lemeshow: ", "Hosmer-Lemeshow: "),
                            hl_reason
                        )
                    },
                    tr(
                        "Common error: treating a non-significant Hosmer-Lemeshow test as proof the model fits well. The test loses power with very large samples and its result depends on the number of groups chosen (commonly 10); always read it together with the calibration plot rather than in isolation.",
                        "Error común: tratar una prueba de Hosmer-Lemeshow no significativa como prueba de que el modelo ajusta bien. La prueba pierde poder con muestras muy grandes y su resultado depende del número de grupos elegido (habitualmente 10); léala siempre junto con el gráfico de calibración, no de forma aislada."
                    )
                ),
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Discrimination.
            # ES: Discriminación.
            # -----------------------------------------------------------------------------
            self$results$discriminationGuide$setContent(html_guide(tr("Discrimination", "Discriminación"), "discriminationGuide"))

            auc_val <- NA_real_
            roc_obj <- NULL
            auc_reason <- NULL
            if (!requireNamespace("pROC", quietly = TRUE)) {
                auc_reason <- tr(
                    "The 'pROC' package is not installed.",
                    "El paquete 'pROC' no está instalado."
                )
            } else {
                roc_obj <- tryCatch({
                    pROC::roc(model_data_complete$dep_binary, fitted(model), quiet = TRUE)
                }, error = function(e) NULL)

                if (!is.null(roc_obj))
                    auc_val <- as.numeric(pROC::auc(roc_obj))

                # Same non-throwing "soft failure" gotcha as hoslem.test():
                # accept a result only when it is actually finite.
                # ES: mismo "fallo blando" sin lanzar error que
                # hoslem.test(): un resultado solo se acepta cuando es
                # realmente finito.
                if (!is.finite(auc_val)) {
                    auc_val <- NA_real_
                    auc_reason <- tr(
                        "Could not be computed for this model's predicted probabilities.",
                        "No se pudo calcular para las probabilidades predichas de este modelo."
                    )
                }
            }
            self$results$discrimination$addRow(rowKey = "disc_auc", values = list(metric = "AUC", value = auc_val))
            if (!is.null(auc_reason))
                tryCatch(
                    self$results$discrimination$addFootnote(col = "value", note = auc_reason, rowKey = "disc_auc"),
                    error = function(e) invisible(NULL)
                )

            self$results$discriminationInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (!is.na(auc_val)) {
                    auc_q1 <- auc_val / (2 - auc_val)
                    auc_q2 <- (2 * auc_val^2) / (1 + auc_val)
                    auc_se <- sqrt(
                        (auc_val * (1 - auc_val) +
                            (n_events - 1) * (auc_q1 - auc_val^2) +
                            (n_non_events - 1) * (auc_q2 - auc_val^2)) /
                            (n_events * n_non_events)
                    )
                    auc_ci_lo <- max(0, auc_val - 1.96 * auc_se)
                    auc_ci_hi <- min(1, auc_val + 1.96 * auc_se)

                    c(
                        paste0(
                            tr("AUC = ", "AUC = "), fmt_num(auc_val, 3),
                            tr(" (approx. 95% CI ", " (IC 95% aprox. "), fmt_num(auc_ci_lo, 3), "-", fmt_num(auc_ci_hi, 3), "; Hanley & McNeil, 1982). ",
                            tr(
                                if (auc_val >= .8) "Good discrimination between events and non-events."
                                else if (auc_val >= .7) "Acceptable discrimination."
                                else if (auc_val >= .6) "Weak discrimination; interpret with caution."
                                else "Discrimination close to chance level.",
                                if (auc_val >= .8) "Buena discriminación entre eventos y no eventos."
                                else if (auc_val >= .7) "Discriminación aceptable."
                                else if (auc_val >= .6) "Discriminación débil; interprete con cautela."
                                else "Discriminación cercana al nivel de azar."
                            )
                        ),
                        if (auc_val < .5) {
                            tr(
                                paste0("An AUC below .50 means the predicted probability ranking runs opposite to the event category as coded; reversing the classification rule would give an equivalent discrimination of approximately ", fmt_num(1 - auc_val, 3), ", still close to chance."),
                                paste0("Un AUC por debajo de .50 significa que el orden de las probabilidades predichas va en sentido opuesto a la categoría de evento tal como está codificada; invertir la regla de clasificación daría una discriminación equivalente de aproximadamente ", fmt_num(1 - auc_val, 3), ", todavía cercana al azar.")
                            )
                        },
                        tr(
                            "Common error: treating AUC as evidence of good calibration, or as a measure of overall model accuracy. A model can discriminate well and still produce poorly calibrated probabilities; use the \"Calibration by Probability Decile\" chart to check that separately.",
                            "Error común: tratar el AUC como evidencia de buena calibración, o como una medida de exactitud global del modelo. Un modelo puede discriminar bien y aun así producir probabilidades mal calibradas; use el gráfico \"Calibración por deciles de probabilidad\" para revisar eso por separado."
                        ),
                        tr(
                            paste0("Sample-size caveat: with only ", n_events, " events and ", n_non_events, " non-events, the AUC confidence interval reported in the Discrimination table reflects real sampling uncertainty in this estimate, not just a point value."),
                            paste0("Matiz de tamaño de muestra: con solo ", n_events, " eventos y ", n_non_events, " no eventos, el intervalo de confianza del AUC reportado en la tabla de Discriminación refleja incertidumbre real de muestreo en esta estimación, no solo un valor puntual.")
                        )
                    )
                } else {
                    paste0(tr("AUC: ", "AUC: "), auc_reason)
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Multicollinearity.
            # ES: Multicolinealidad.
            # -----------------------------------------------------------------------------
            self$results$multicollinearityGuide$setContent(html_guide(tr("Multicollinearity", "Multicolinealidad"), "multicollinearityGuide"))

            max_vif <- NA_real_
            max_vif_name <- NA_character_
            max_ci <- NA_real_
            multi_i <- 0

            add_multi <- function(diagnostic, item, statistic, value, reason = NULL) {
                multi_i <<- multi_i + 1
                key <- paste0("multi_", multi_i)
                self$results$multicollinearity$addRow(rowKey = key, values = list(
                    diagnostic = diagnostic, item = item, statistic = statistic,
                    value = if (is.na(value)) NA_real_ else value
                ))
                if (!is.null(reason) && is.na(value))
                    tryCatch(
                        self$results$multicollinearity$addFootnote(col = "value", note = reason, rowKey = key),
                        error = function(e) invisible(NULL)
                    )
            }

            X <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
            X_no_intercept <- if (!is.null(X)) X[, colnames(X) != "(Intercept)", drop = FALSE] else NULL

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
                        1 / (1 - summary(vifAuxFit)$r.squared)
                    }, error = function(e) NA_real_)

                    tol <- if (is.na(vif)) NA_real_ else 1 / vif
                    pClean <- gsub("^`|`$", "", colnames(X_no_intercept)[j])

                    vif_reason <- if (is.na(vif)) tr(
                        "Could not be computed - regressing this predictor on the others is likely singular (more predictors than complete cases, or an exact linear dependency among predictors).",
                        "No se pudo calcular - regresar este predictor sobre los demás es probablemente singular (más predictores que casos completos, o una dependencia lineal exacta entre predictores)."
                    ) else NULL
                    add_multi("VIF", pClean, "VIF", vif, reason = vif_reason)
                    add_multi(tr("Tolerance", "Tolerancia"), pClean, "1/VIF", tol, reason = vif_reason)

                    if (!is.na(vif) && (is.na(max_vif) || vif > max_vif)) {
                        max_vif <- vif
                        max_vif_name <- pClean
                    }
                }

                eig <- tryCatch({
                    R <- stats::cor(scale(X_no_intercept), use = "pairwise.complete.obs")
                    eigen(R, symmetric = TRUE)$values
                }, error = function(e) NULL)

                if (!is.null(eig)) {
                    min_eig <- max(min(eig, na.rm = TRUE), .Machine$double.eps)
                    max_eig <- max(eig, na.rm = TRUE)
                    ci <- sqrt(max_eig / min_eig)
                    max_ci <- ci

                    det_r <- tryCatch(
                        det(stats::cor(scale(X_no_intercept), use = "pairwise.complete.obs")),
                        error = function(e) NA_real_
                    )

                    add_multi(tr("Minimum eigenvalue", "Eigenvalue mínimo"), tr("Design matrix", "Matriz de diseño"), tr("minimum λ", "λ mínimo"), min_eig)
                    add_multi(tr("Condition index", "Índice de condición"), tr("Design matrix", "Matriz de diseño"), "CI", ci)
                    add_multi(tr("Determinant", "Determinante"), tr("Correlation matrix", "Matriz de correlación"), "det(R)", det_r)
                }
            } else {
                add_multi(tr("VIF / tolerance", "VIF / tolerancia"), tr("Not applicable", "No aplicable"), "", NA_real_)
            }

            self$results$multicollinearityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (!is.na(max_vif)) {
                    c(
                        paste0(
                            tr("Maximum VIF: ", "VIF máximo: "), fmt_num(max_vif, 2),
                            tr(" (predictor: ", " (predictor: "), max_vif_name, ")",
                            tr(
                                ". Values between 5 and 10 raise a moderate concern, and values above 10 are considered a severe problem (Marquardt, 1970).",
                                ". Valores entre 5 y 10 encienden una alerta moderada, y valores por encima de 10 se consideran un problema severo (Marquardt, 1970)."
                            )
                        ),
                        paste0(
                            tr("In practical terms, a VIF of ", "En términos prácticos, un VIF de "), fmt_num(max_vif, 2),
                            tr(
                                paste0(" means that predictor's standard error is about ", fmt_num(sqrt(max_vif), 2), " times larger than it would be without collinearity - which widens its confidence interval and odds-ratio range accordingly."),
                                paste0(" significa que el error estándar de ese predictor es aproximadamente ", fmt_num(sqrt(max_vif), 2), " veces más grande de lo que sería sin colinealidad - lo que amplía su intervalo de confianza y su rango de odds ratio en la misma proporción.")
                            )
                        ),
                        if (!is.na(max_ci))
                            tr(
                                paste0("Condition index (whole design matrix): ", fmt_num(max_ci, 2), ". Values above 15 raise a moderate concern and above 30 a severe one, complementing VIF by diagnosing collinearity across the whole matrix at once."),
                                paste0("Índice de condición (toda la matriz de diseño): ", fmt_num(max_ci, 2), ". Valores por encima de 15 encienden una alerta moderada y por encima de 30 una severa, complementando al VIF al diagnosticar la colinealidad de toda la matriz a la vez.")
                            )
                        else "",
                        tr(
                            "Common error: assuming a high VIF means the predictor should be removed. The model may still predict and discriminate well overall; the issue is isolating that predictor's individual effect from the others, not the model's usefulness as a whole.",
                            "Error común: pensar que un VIF alto significa que hay que eliminar el predictor. El modelo puede seguir prediciendo y discriminando bien en conjunto; el problema es aislar el efecto individual de ese predictor frente a los demás, no la utilidad del modelo en su conjunto."
                        ),
                        tr(
                            paste0("Sample-size caveat: this threshold is a convention, not a statistical law. With ", n_complete, " complete cases here, even moderate collinearity can make coefficients noticeably unstable; with much larger samples a similar VIF is usually more tolerable."),
                            paste0("Matiz de tamaño de muestra: este umbral es una convención, no una ley estadística. Con ", n_complete, " casos completos aquí, incluso una colinealidad moderada puede volver los coeficientes notablemente inestables; con muestras mucho más grandes, un VIF similar suele ser más tolerable.")
                        )
                    )
                } else if (n_predictors <= 1) {
                    tr(
                        "Multicollinearity requires at least two predictors; it does not apply here.",
                        "La multicolinealidad requiere al menos dos predictores; no aplica en este caso."
                    )
                } else {
                    tr(
                        "VIF could not be computed for this model matrix.",
                        "No fue posible calcular el VIF para esta matriz de diseño."
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Correlation matrices (Pearson and dCor, APA 7 format) + copent.
            # ES: Matrices de correlación (Pearson y dCor, formato APA 7) + copent.
            # -----------------------------------------------------------------------------
            self$results$correlationMatrixGuide$setContent(html_guide(tr("Correlation Matrix", "Matriz de Correlaciones"), "correlationMatrixGuide"))

            # dcor_stat()/dcor_pvalue(): equivalent to every other module's
            # version (regCheck/anovaCheck/relatedCheck), consolidated in
            # shared-helpers.R (.al_dcor_stat/.al_dcor_test), same pattern
            # already used for copentTest below. Fixed B=199/seed=20260704
            # preserved unchanged. The shared version's denom guard
            # (!is.finite(denom)) is marginally more defensive than this
            # module's previous is.na(denom) check (also catches an
            # infinite denominator, not just NaN) - no change for any
            # realistic finite input.
            # ES: equivalentes a la versión de los demás módulos
            # (regCheck/anovaCheck/relatedCheck), consolidadas en
            # shared-helpers.R (.al_dcor_stat/.al_dcor_test), mismo patrón
            # ya usado para copentTest más abajo. B=199/semilla=20260704
            # fijos, preservados sin cambio. La guarda de denominador
            # compartida (!is.finite(denom)) es marginalmente más
            # defensiva que la anterior is.na(denom) de este módulo
            # (también detecta un denominador infinito, no solo NaN) - sin
            # cambio para ninguna entrada finita realista.
            dcor_stat <- .al_dcor_stat
            dcor_pvalue <- function(x, y) .al_dcor_test(x, y)$p

            # copentTest(): byte-identical in every module that has it,
            # consolidated in shared-helpers.R (.al_copent_test).
            # ES: idéntica en todos los módulos que la usan, consolidada
            # en shared-helpers.R.
            copentTest <- .al_copent_test

            # fmtR()/apaCell(): identical in every module that has them,
            # consolidated in shared-helpers.R (.al_fmt_r/.al_apa_cell).
            # ES: idénticas en todos los módulos que las usan, consolidadas
            # en shared-helpers.R.
            fmtR <- .al_fmt_r
            apaCell <- .al_apa_cell

            pearsonTable <- self$results$pearsonMatrixTable
            pearsonTable$deleteRows()
            dcorTable <- self$results$dcorMatrixTable
            dcorTable$deleteRows()

            corrData <- model_data_complete[, covs, drop = FALSE]
            corrData[[dep_var]] <- model_data_complete$dep_binary
            matVars <- c(dep_var, covs)
            k <- length(matVars)
            pairResults <- list()

            self$results$correlationMatrixGuide$setVisible(k >= 2)
            self$results$pearsonMatrixTable$setVisible(k >= 2)
            self$results$dcorMatrixTable$setVisible(k >= 2)
            self$results$correlationMatrixNote$setVisible(k >= 2)
            self$results$correlationComparisonGuide$setVisible(k >= 2)
            self$results$correlationComparisonTable$setVisible(k >= 2)
            self$results$correlationComparisonInterpretation$setVisible(k >= 2)

            if (k >= 2) {
                for (i in seq_len(k)) {
                    for (j in seq_len(k)) {
                        if (j >= i) next()
                        v1 <- matVars[i]; v2 <- matVars[j]
                        x <- corrData[[v1]]; y <- corrData[[v2]]
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
                        paste0(sprintf("N = %d.", n_complete), " ", tr("* p < .05, ** p < .01, *** p < .001 (dependent variable coded 0/1: point-biserial correlation).", "* p < .05, ** p < .01, *** p < .001 (variable dependiente codificada 0/1: correlación biserial-puntual).")),
                        .al_dcor_na_note(lang)
                    ),
                    paragraphs = FALSE
                ))
            } else {
                self$results$correlationMatrixNote$setContent(html_block(NULL,
                    tr("Not applicable: fewer than two numeric model variables.", "No aplica: menos de dos variables numéricas del modelo."),
                    paragraphs = FALSE
                ))
            }

            self$results$correlationComparisonGuide$setContent(html_block(
                tr("Pearson / dCor / Copula Entropy Discordance Analysis", "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                .al_html_list(tr(c(
                    "Because Pearson's r only captures linear association while dCor captures both linear and non-linear association, a pair whose dCor is notably larger than its Pearson |r| is a signal (not proof) of a non-linear relationship.",
                    "Pairs are flagged in the \"Pairs with a Notable Difference between Pearson and dCor\" table when the gap (dCor minus |Pearson r|) is greater than .10.",
                    "The copula entropy (CE, copent()) result for the same pair is shown alongside as a second, distribution-free line of evidence.",
                    "This threshold is a heuristic, not a formal test; always inspect a scatterplot of any flagged pair before concluding the relationship is non-linear.",
                    .al_permutation_note(lang, 199, 20260704)
                ), c(
                    "Dado que la r de Pearson solo capta asociación lineal mientras que dCor capta asociación lineal y no lineal por igual, un par cuyo dCor sea notablemente mayor que su |r| de Pearson es una señal (no una prueba) de una relación no lineal.",
                    "Se señalan en la tabla \"Pares con diferencia notable entre Pearson y dCor\" los pares con una brecha (dCor menos |r| de Pearson) mayor a .10.",
                    "El resultado de la prueba de entropía copular (CE, copent()) para el mismo par se muestra al lado como una segunda línea de evidencia libre de supuestos distribucionales.",
                    "Este umbral es una heurística, no una prueba formal; siempre revise un diagrama de dispersión de cualquier par señalado antes de concluir que la relación es no lineal.",
                    .al_permutation_note(lang, 199, 20260704)
                ))),
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
                    var1 = pr$v1, var2 = pr$v2,
                    pearson = apaCell(pr$pearsonR, pr$pearsonP),
                    dcor = apaCell(pr$dcor, pr$dcorP),
                    gap = gap, ce = pr$ce, ceP = pr$ceP,
                    ceSig = p_sig(pr$ceP),
                    flag = tr("Yes", "Sí")
                ))
            }

            if (length(flaggedPairs) == 0) {
                self$results$correlationComparisonInterpretation$setContent(html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    if (k < 2)
                        tr("Not applicable: fewer than two numeric model variables.", "No aplica: menos de dos variables numéricas del modelo.")
                    else
                        tr(sprintf("No pair shows a Pearson/dCor gap greater than %.2f; there is no indication of unmodeled non-linear association among the model's numeric variables.", gapThreshold),
                           sprintf("Ningún par muestra una brecha Pearson/dCor mayor a %.2f; no hay indicios de asociación no lineal no modelada entre las variables numéricas del modelo.", gapThreshold)),
                    paragraphs = FALSE
                ))
            } else {
                self$results$correlationComparisonInterpretation$setContent(html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    tr(sprintf("%d pair(s) show a Pearson/dCor gap greater than %.2f: %s. Inspect a scatterplot of each flagged pair (recall the dependent variable is binary, so its correlations are point-biserial).",
                               length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", ")),
                       sprintf("%d par(es) muestran una brecha Pearson/dCor mayor a %.2f: %s. Revise un diagrama de dispersión de cada par señalado (recuerde que la variable dependiente es binaria, así que sus correlaciones son biserial-puntuales).",
                               length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", "))),
                    paragraphs = FALSE
                ))
            }

            # -----------------------------------------------------------------------------
            # Influence.
            # ES: Influencia.
            # -----------------------------------------------------------------------------
            self$results$influenceGuide$setContent(html_guide(tr("Influential cases", "Casos influyentes"), "influenceGuide"))

            cooks_d <- cooks.distance(model)
            leverage_vals <- hatvalues(model)
            n_influential <- 0

            # Use the actual number of estimated model parameters (intercept +
            # expanded factor dummies), not the raw count of user-selected
            # predictor variables (n_predictors), which understates leverage's
            # denominator whenever a factor predictor has more than 2 levels.
            n_model_params <- length(coef(model))

            for (i in seq_len(n_complete)) {

                # A saturated or near-saturated fit (e.g. as few complete cases as
                # model parameters) can leave cooks.distance()/hatvalues() returning
                # NaN for a given case; comparing NaN with `>` yields NA, and NA in an
                # `if()` condition is a fatal R error rather than a skipped case. Each
                # term is therefore only evaluated once its value is confirmed finite,
                # mirroring the guard already used by .al_influence_diagnostics() in
                # shared-helpers.R for anovaCheck/regCheck's equivalent loop.
                # ES: un ajuste saturado o casi saturado (p. ej. tan pocos casos
                # completos como parámetros del modelo) puede dejar que
                # cooks.distance()/hatvalues() devuelvan NaN para un caso dado;
                # comparar NaN con `>` produce NA, y un NA en una condición `if()` es
                # un error fatal de R en lugar de un caso simplemente omitido. Por eso
                # cada término solo se evalúa una vez confirmado que su valor es
                # finito, replicando la protección que ya usa
                # .al_influence_diagnostics() en shared-helpers.R para el bucle
                # equivalente de anovaCheck/regCheck.
                cooks_ok <- !is.na(clean_num(cooks_d[i])) && cooks_d[i] > 4 / n_complete
                leverage_ok <- !is.na(clean_num(leverage_vals[i])) && leverage_vals[i] > 2 * n_model_params / n_complete

                if (cooks_ok || leverage_ok) {
                    n_influential <- n_influential + 1
                    self$results$influence$addRow(rowKey = paste0("case_", i), values = list(
                        case = i,
                        cooksD = clean_num(cooks_d[i]),
                        leverage = clean_num(leverage_vals[i])
                    ))
                }
            }

            max_cooks_idx <- which.max(cooks_d)
            max_cooks_val <- cooks_d[max_cooks_idx]
            pct_influential <- 100 * n_influential / n_complete

            self$results$influenceInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    paste0(
                        tr(
                            paste0(n_influential, " of ", n_complete, " cases (", fmt_num(pct_influential, 1), "%) exceed the Cook's D or leverage cutoffs used here (Pregibon, 1981)."),
                            paste0(n_influential, " de ", n_complete, " casos (", fmt_num(pct_influential, 1), "%) superan los umbrales de Cook's D o leverage usados aquí (Pregibon, 1981).")
                        )
                    ),
                    paste0(
                        tr("The single most influential observation is case ", "El caso más influyente es el caso "), max_cooks_idx,
                        tr(paste0(", with Cook's D = ", fmt_num(max_cooks_val, 3), "."), paste0(", con Cook's D = ", fmt_num(max_cooks_val, 3), ".")
                        )
                    ),
                    tr(
                        "Common error: removing every flagged case automatically to 'clean' the model. With cutoffs this permissive, some flags are expected even in well-behaved data; treat this as a shortlist to inspect, not a deletion list.",
                        "Error común: eliminar automáticamente todos los casos marcados para 'limpiar' el modelo. Con umbrales tan permisivos, es esperable marcar algunos casos incluso en datos bien comportados; trate esto como una lista corta para inspeccionar, no una lista para eliminar."
                    ),
                    tr(
                        "An influential case should not be removed automatically; review whether it is a recording error, a valid but extreme case, or a sign the model does not represent all subgroups well. If you do remove or refit without a case, report both versions of the model.",
                        "Un caso influyente no debe eliminarse automáticamente; revise si es un error de registro, un caso válido pero extremo, o una señal de que el modelo no representa bien a todos los subgrupos. Si elimina un caso o reajusta sin él, reporte ambas versiones del modelo."
                    )
                ),
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Odds ratios.
            # ES: Odds ratios.
            # -----------------------------------------------------------------------------
            self$results$oddsRatiosGuide$setContent(html_guide("Odds Ratios", "oddsRatiosGuide"))

            ors <- exp(coef(model))
            ci <- tryCatch(exp(confint(model)), error = function(e) {
                matrix(NA_real_, nrow = length(ors), ncol = 2)
            })

            # coef(model) keeps one entry per predictor even when a predictor is
            # aliased (e.g. a constant/collinear covariate), reporting NA for it;
            # summary()$coefficients (coef_table), however, DROPS aliased rows
            # entirely, so it can have fewer rows than length(ors). Looking up each
            # p-value by coefficient name - defaulting to NA when the name is absent
            # from coef_table - keeps ors/p-value pairing correct instead of reading
            # coef_table by raw row position, which silently misaligns or throws
            # "subscript out of bounds" once a coefficient has been dropped.
            # ES: coef(model) conserva una entrada por predictor incluso cuando un
            # predictor está confundido/alias (p. ej. una covariable constante o
            # colineal), reportando NA para él; summary()$coefficients (coef_table),
            # en cambio, ELIMINA por completo las filas confundidas, por lo que puede
            # tener menos filas que length(ors). Buscar cada valor p por el nombre
            # del coeficiente - usando NA cuando el nombre no está en coef_table -
            # mantiene correcta la relación ors/valor p, en lugar de leer coef_table
            # por posición de fila cruda, lo que desalinea en silencio o produce
            # "subíndice fuera de los límites" en cuanto se elimina un coeficiente.
            p_values_by_name <- stats::setNames(rep(NA_real_, length(ors)), names(ors))
            matched_names <- intersect(names(ors), rownames(coef_table))
            p_values_by_name[matched_names] <- coef_table[matched_names, "Pr(>|z|)"]

            for (i in seq_along(ors)) {
                p_val <- p_values_by_name[i]
                self$results$oddsRatios$addRow(rowKey = paste0("or_", i), values = list(
                    predictor = strip_qname(names(ors)[i]),
                    or = ors[i],
                    ciLower = if (!is.null(ci) && nrow(ci) >= i) ci[i, 1] else NA,
                    ciUpper = if (!is.null(ci) && nrow(ci) >= i) ci[i, 2] else NA,
                    p = p_val,
                    pSig = p_sig(p_val)
                ))
            }

            event_prevalence <- 100 * n_events / n_complete

            strongest_idx <- NA_integer_
            if (length(p_values_by_name) > 1) {
                p_vals_no_intercept <- p_values_by_name[2:length(p_values_by_name)]
                if (any(!is.na(p_vals_no_intercept))) {
                    strongest_idx <- which.min(p_vals_no_intercept) + 1
                }
            }

            self$results$oddsRatiosInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    if (!is.na(strongest_idx)) {
                        # p_values_by_name (not coef_table) is the correct lookup here:
                        # strongest_idx indexes into ors/names(ors)/coef(model), and
                        # coef_table can have fewer rows than that whenever a
                        # coefficient was dropped as aliased (see the p_values_by_name
                        # construction above), which would otherwise throw "subscript
                        # out of bounds" or silently misalign for a strongest-predictor
                        # index beyond coef_table's own row count.
                        # ES: p_values_by_name (no coef_table) es la búsqueda correcta
                        # aquí: strongest_idx indexa dentro de ors/names(ors)/
                        # coef(model), y coef_table puede tener menos filas que eso
                        # cuando un coeficiente fue eliminado por estar confundido/alias
                        # (ver la construcción de p_values_by_name más arriba), lo que
                        # de otro modo produciría "subíndice fuera de los límites" o un
                        # desalineamiento silencioso para un índice de predictor más
                        # fuerte que exceda el propio número de filas de coef_table.
                        strongest_p <- p_values_by_name[strongest_idx]
                        paste0(
                            tr("Strongest association: ", "Asociación más fuerte: "), strip_qname(names(ors)[strongest_idx]),
                            tr(paste0(", OR = ", fmt_num(ors[strongest_idx], 2), " (p ", if (!is.na(strongest_p) && strongest_p < .001) "< .001" else paste0("= ", fmt_num(strongest_p, 3)), ")."),
                               paste0(", OR = ", fmt_num(ors[strongest_idx], 2), " (p ", if (!is.na(strongest_p) && strongest_p < .001) "< .001" else paste0("= ", fmt_num(strongest_p, 3)), ").")
                            )
                        )
                    } else {
                        tr("No predictor reached conventional significance in this model.", "Ningún predictor alcanzó significancia convencional en este modelo.")
                    },
                    tr(
                        "Read the confidence interval, not only the point estimate: if it includes 1, the effect is not statistically distinguishable from no association.",
                        "Lea el intervalo de confianza, no solo el estimador puntual: si incluye 1, el efecto no es estadísticamente distinguible de la ausencia de asociación."
                    ),
                    paste0(
                        tr(
                            paste0("Common error: interpreting the OR as a relative risk. They coincide only when the event is rare; here the event prevalence is ", fmt_num(event_prevalence, 1), "%, so with common events like this the OR systematically overstates the real relative effect (Zhang & Yu, 1998)."),
                            paste0("Error común: interpretar el OR como un riesgo relativo. Coinciden solo cuando el evento es poco frecuente; aquí la prevalencia del evento es ", fmt_num(event_prevalence, 1), "%, así que con eventos frecuentes como este el OR exagera sistemáticamente el efecto relativo real (Zhang & Yu, 1998).")
                        )
                    ),
                    tr(
                        "For audiences unfamiliar with odds ratios, consider also reporting predicted probabilities at representative predictor values, which are easier to communicate than the OR itself.",
                        "Para audiencias no familiarizadas con odds ratios, considere reportar también probabilidades predichas en valores representativos de los predictores, que son más fáciles de comunicar que el OR en sí."
                    )
                ),
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Notes and recommendation.
            # ES: Notas y recomendación.
            # -----------------------------------------------------------------------------
            epv_note <- if (epv >= 10) {
                tr("Adequate.", "Adecuado.")
            } else if (epv >= 5) {
                tr("Acceptable with caution.", "Aceptable con cautela.")
            } else {
                tr("Insufficient (Peduzzi et al., 1996, recommend EPV >= 10).", "Insuficiente (Peduzzi et al., 1996, recomiendan EPV >= 10).")
            }

            recommendation <- paste(
                tr("The logistic regression model can be interpreted cautiously,",
                   "El modelo de regresión logística puede interpretarse con cautela,"),
                tr("while reviewing the reported diagnostics.",
                   "revisando los diagnósticos reportados.")
            )

            if (epv < 10) {
                recommendation <- paste(
                    tr("EPV is below the recommended threshold; coefficients and standard errors may be unstable,",
                       "El EPV está por debajo del umbral recomendado; los coeficientes y errores estándar pueden ser inestables,"),
                    tr("consider Firth's penalized logistic regression or combining sparse categories.",
                       "considere regresión logística penalizada de Firth o combinar categorías poco frecuentes.")
                )
            }

            if (!is.na(max_vif) && max_vif >= 5) {
                recommendation <- paste(
                    tr("There is relevant multicollinearity among predictors; review redundant predictors",
                       "Hay multicolinealidad relevante entre predictores; revisar predictores redundantes"),
                    tr("before interpreting individual odds ratios.",
                       "antes de interpretar los odds ratios individuales.")
                )
            }

            if (exists("bt_significant") && bt_significant > 0) {
                recommendation <- paste(
                    tr("At least one numeric predictor shows a non-linear relationship with the logit; consider a",
                       "Al menos un predictor numérico muestra una relación no lineal con el logit; considere un"),
                    tr("polynomial term, spline, or categorization for that predictor.",
                       "término polinomial, spline o categorización para ese predictor.")
                )
            }

            if (n_influential > 0 && pct_influential >= 10) {
                recommendation <- paste(
                    tr("There are outlying or influential cases; inspect them before",
                       "Existen casos atípicos o influyentes; inspeccionarlos antes de"),
                    tr("making decisions about the model.",
                       "tomar decisiones sobre el modelo.")
                )
            }

            self$results$notes$setContent(html_block(
                tr("Notes and recommendation", "Notas y recomendación"),
                c(
                    paste0(tr("Suggested decision: ", "Decisión sugerida: "), recommendation),
                    paste0(
                        tr("Model fitted with ", "Modelo ajustado con "), n_complete,
                        tr(" cases.", " casos.")
                    ),
                    paste0(tr("EPV (least-frequent category): ", "EPV (categoría menos frecuente): "), fmt_num(epv, 1), " - ", epv_note)
                ),
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # State for the plots (defensive pattern: language and data persist with the
            # image, so they do not depend on .run() being re-triggered). Each image gets
            # only the fields its own render function reads - not one shared list with
            # every field for every plot - since jamovi's review flagged that repeating
            # the same (potentially large) state across images multiplies what gets
            # written into every exported/saved results file. In particular, the plotted
            # ROC curve only needs the sensitivity/specificity vectors, not the full pROC
            # `roc` object, which keeps its own copies of the predictor and response data.
            # ES: Estado para los gráficos (patrón defensivo: idioma y datos persisten con
            # la imagen, no dependen de que .run() se repita). Cada imagen recibe solo los
            # campos que su propia función de render lee - no una lista compartida con
            # todos los campos para todos los gráficos - ya que la revisión de jamovi
            # señaló que repetir el mismo estado (potencialmente grande) en varias
            # imágenes multiplica lo que se escribe en cada archivo de resultados
            # exportado o guardado. En particular, la curva ROC graficada solo necesita
            # los vectores de sensibilidad/especificidad, no el objeto `roc` completo de
            # pROC, que guarda sus propias copias de los datos de predictor y respuesta.
            # -----------------------------------------------------------------------------
            self$results$linearityPlot$setState(list(
                reportLang = lang,
                covs = covs,
                model_data = model_data_complete,
                dep_binary = model_data_complete$dep_binary
            ))
            self$results$calibrationPlot$setState(list(
                reportLang = lang,
                fitted = fitted(model),
                dep_binary = model_data_complete$dep_binary
            ))
            self$results$discriminationPlot$setState(list(
                reportLang = lang,
                roc_specificities = if (!is.null(roc_obj)) roc_obj$specificities else NULL,
                roc_sensitivities = if (!is.null(roc_obj)) roc_obj$sensitivities else NULL,
                auc_val = auc_val
            ))
            self$results$influencePlot$setState(list(
                reportLang = lang,
                cooks_d = cooks_d,
                leverage = leverage_vals,
                n = n_complete,
                p = n_model_params
            ))

            lin_show <- tryCatch(isTRUE(self$options$linShowPlots), error = function(e) TRUE)
            cali_show <- tryCatch(isTRUE(self$options$caliShowPlots), error = function(e) TRUE)
            roc_show <- tryCatch(isTRUE(self$options$rocShowPlots), error = function(e) TRUE)
            influence_show <- tryCatch(isTRUE(self$options$influenceShowPlots), error = function(e) TRUE)

            area_visibility <- c(
                linearityPlot = lin_show,
                calibrationPlot = cali_show,
                discriminationPlot = roc_show,
                influencePlot = influence_show
            )

            for (item_name in names(area_visibility)) {
                item <- tryCatch(self$results[[item_name]], error = function(e) NULL)
                if (!is.null(item)) {
                    item$setVisible(
                        isTRUE(self$options$showPlots) && isTRUE(area_visibility[[item_name]])
                    )
                }
            }
        },

        .emptyLogPlot = function(message) {
            graphics::plot.new()
            graphics::par(mar = c(1, 1, 1, 1))
            graphics::text(x = 0.5, y = 0.55, labels = message, cex = 0.95)
            graphics::text(x = 0.5, y = 0.40, labels = "AssumptionsLab", cex = 0.80)
            TRUE
        },

        .plotTr = function(en, es, image = NULL) {
            plot_lang <- tryCatch({
                st <- if (!is.null(image)) image$state else NULL
                if (!is.null(st) && is.list(st) && !is.null(st$reportLang) && nzchar(st$reportLang)) {
                    st$reportLang
                } else {
                    self$options$reportLang
                }
            }, error = function(e) "es")

            plot_lang <- tryCatch(.al_normalize_lang(plot_lang), error = function(e) "es")

            if (identical(plot_lang, "es")) es else en
        },

        # -----------------------------------------------------------------------------
        # Plot theme/palette - migrated to jamovi's native mechanism (jamovi
        # review team feedback, 2026-09-16; see
        # https://dev.jamovi.org/tutorial/tuts0302-plot-themes). Every
        # render function now declares `ggtheme`/`theme` parameters, which
        # jamovi always passes (jmvcore::Analysis$.createPlotObject() calls
        # every render function with `theme = t$theme, ggtheme = t$ggtheme`
        # regardless of whether the function names them - they were
        # previously just absorbed into `...` and ignored). `ggtheme` is a
        # list of ggplot2 layers (a base theme + discrete color/fill scales
        # already matching the user's jamovi-wide palette choice, verified
        # against jmvcore::getGGTheme()/ggPalette()) added directly via
        # `plot + ggtheme`; `theme` is a plain list with `$color`/`$fill`
        # (2-color base+accent vectors) for elements that are not mapped to
        # a discrete aesthetic. `private$.plotStyle()`/`.plotTheme()` (a
        # module-specific theme_bw()/theme_classic()/theme_minimal() choice)
        # are removed entirely, since that duplicated jamovi's own Theme
        # preference one-for-one.
        # ES: Tema/paleta de gráficos - migrado al mecanismo nativo de
        # jamovi (retroalimentación del equipo de revisión de jamovi,
        # 2026-09-16; ver
        # https://dev.jamovi.org/tutorial/tuts0302-plot-themes). Cada
        # función de render ahora declara parámetros `ggtheme`/`theme`, que
        # jamovi siempre pasa (jmvcore::Analysis$.createPlotObject() llama a
        # cada función de render con `theme = t$theme, ggtheme = t$ggtheme`
        # sin importar si la función los nombra - antes simplemente se
        # absorbían en `...` y se ignoraban). `ggtheme` es una lista de
        # capas ggplot2 (un tema base + escalas de color/relleno discretas
        # que ya coinciden con la paleta elegida por el usuario a nivel de
        # jamovi, verificado contra jmvcore::getGGTheme()/ggPalette())
        # agregada directamente vía `plot + ggtheme`; `theme` es una lista
        # simple con `$color`/`$fill` (vectores de 2 colores base+acento)
        # para elementos que no están mapeados a una estética discreta.
        # `private$.plotStyle()`/`.plotTheme()` (una elección propia del
        # módulo entre theme_bw()/theme_classic()/theme_minimal()) se
        # eliminan por completo, ya que duplicaban uno a uno la preferencia
        # de Tema propia de jamovi.
        # -----------------------------------------------------------------------------
        .plotColors = function(theme) {
            base_color <- if (!is.null(theme$color) && length(theme$color) >= 1) theme$color[1] else "#333333"
            accent_color <- if (!is.null(theme$color) && length(theme$color) >= 2) theme$color[2] else "#2C7FB8"
            accent_fill <- if (!is.null(theme$fill) && length(theme$fill) >= 2) theme$fill[2] else "#A6CEE3"

            list(
                point = base_color, line = base_color,
                # A reference/guide line (e.g. a calibration diagonal) is a
                # structural comparison element, not data - kept a fixed
                # neutral gray independent of the user's palette, the
                # conventional treatment for this role in every theme.
                # ES: una línea de referencia/guía (p. ej. la diagonal de
                # calibración) es un elemento estructural de comparación,
                # no datos - se mantiene en un gris neutro fijo,
                # independiente de la paleta del usuario, el tratamiento
                # convencional de este rol en cualquier tema.
                ref = "gray50",
                alert = accent_color, smooth = accent_color, fill = accent_fill
            )
        },

        # NULL return means "let ggtheme's own discrete scale handle it" -
        # the default and recommended path, since it already matches
        # jamovi's global palette for any number of groups. A manual vector
        # is returned only for the two palettes jamovi does not offer
        # (verified against jmvcore::colorPalette(), which silently falls
        # back to its default colors for unrecognized names like
        # "colorblind"/"viridis").
        # ES: un retorno NULL significa "que la escala discreta propia de
        # ggtheme se encargue" - la ruta por defecto y recomendada, ya que
        # coincide con la paleta global de jamovi para cualquier número de
        # grupos. Se devuelve un vector manual solo para las dos paletas
        # que jamovi no ofrece (verificado contra jmvcore::colorPalette(),
        # que cae en silencio a sus colores por defecto ante nombres no
        # reconocidos como "colorblind"/"viridis").
        .plotSeriesColors = function() {
            choice <- tryCatch(self$options$plotPalette, error = function(e) "jamovi")
            if (is.null(choice) || length(choice) == 0 || !nzchar(choice) || identical(choice, "jamovi"))
                return(NULL)
            .al_plot_series_palette(choice)
        },

        # -----------------------------------------------------------------------------
        # Empirical logit vs predictor (linearity in the logit).
        # ES: Logit empírico vs predictor (linealidad en el logit).
        # -----------------------------------------------------------------------------
        .plotLinearity = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st))
                return(FALSE)

            tr_p <- function(en, es) private$.plotTr(en, es, image)
            covs <- st$covs

            if (length(covs) == 0) {
                return(private$.emptyLogPlot(tr_p(
                    "No numeric predictors to plot.",
                    "No hay predictores numéricos para graficar."
                )))
            }

            bins <- tryCatch(as.integer(self$options$linBins), error = function(e) 8L)
            if (is.null(bins) || is.na(bins) || bins < 4) bins <- 8L

            emp_logit_df <- function(x, y, bins = 8) {
                ok <- !is.na(x) & !is.na(y)
                x <- x[ok]; y <- y[ok]
                if (length(unique(x)) < 3)
                    return(NULL)

                probs <- seq(0, 1, length.out = bins + 1)
                qs <- unique(stats::quantile(x, probs = probs, na.rm = TRUE))
                if (length(qs) < 3)
                    return(NULL)

                grp <- cut(x, breaks = qs, include.lowest = TRUE)
                mean_x <- tapply(x, grp, mean)
                n_grp <- tapply(y, grp, length)
                events_grp <- tapply(y, grp, sum)
                p_hat <- (events_grp + 0.5) / (n_grp + 1)
                logit_hat <- log(p_hat / (1 - p_hat))

                data.frame(
                    mean_x = as.numeric(mean_x),
                    logit = as.numeric(logit_hat),
                    n = as.numeric(n_grp)
                )
            }

            plot_rows <- list()
            for (predictor in covs) {
                x <- st$model_data[[predictor]]
                df <- emp_logit_df(x, st$dep_binary, bins = bins)
                if (!is.null(df)) {
                    df$predictor <- predictor
                    plot_rows[[predictor]] <- df
                }
            }

            if (length(plot_rows) == 0) {
                return(private$.emptyLogPlot(tr_p(
                    "Not enough variation to plot linearity in the logit.",
                    "No hay suficiente variación para graficar la linealidad en el logit."
                )))
            }

            plot_df <- do.call(rbind, plot_rows)
            predictors_unique <- unique(as.character(plot_df$predictor))
            series_colors <- private$.plotSeriesColors()
            if (!is.null(series_colors)) {
                series_colors <- rep(series_colors, length.out = max(1, length(predictors_unique)))
                series_colors <- stats::setNames(series_colors, predictors_unique)
            }

            smoother <- tryCatch(self$options$linSmoother, error = function(e) "none")

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = mean_x, y = logit))

            if (identical(smoother, "linear")) {
                p <- p + ggplot2::geom_smooth(
                    ggplot2::aes(color = predictor),
                    method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5,
                    show.legend = FALSE
                )
            } else if (identical(smoother, "loess")) {
                p <- p + ggplot2::geom_smooth(
                    ggplot2::aes(color = predictor),
                    method = "loess", se = FALSE, linewidth = 0.5,
                    show.legend = FALSE
                )
            }

            p <- p +
                ggplot2::geom_point(ggplot2::aes(size = n, color = predictor), show.legend = c(size = TRUE, color = FALSE)) +
                ggplot2::facet_wrap(~predictor, scales = "free_x") +
                ggplot2::labs(
                    x = tr_p("Predictor (binned mean)", "Predictor (media por grupo)"),
                    y = tr_p("Empirical logit", "Logit empírico"),
                    size = tr_p("Group n", "n del grupo")
                ) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::margin(4, 6, 4, 6))

            if (!is.null(series_colors))
                p <- p + ggplot2::scale_color_manual(values = series_colors)

            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Calibration: predicted probability vs observed proportion by decile.
        # ES: Calibración: probabilidad predicha vs proporción observada por decil.
        # -----------------------------------------------------------------------------
        .plotCalibration = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st))
                return(FALSE)

            tr_p <- function(en, es) private$.plotTr(en, es, image)
            pal <- private$.plotColors(theme)

            fitted_vals <- st$fitted
            dep_binary <- st$dep_binary

            bins <- tryCatch(as.integer(self$options$caliBins), error = function(e) 10L)
            if (is.null(bins) || is.na(bins) || bins < 4) bins <- 10L
            show_ref <- tryCatch(isTRUE(self$options$caliRefLine), error = function(e) TRUE)

            probs <- seq(0, 1, length.out = bins + 1)
            qs <- unique(stats::quantile(fitted_vals, probs = probs, na.rm = TRUE))

            if (length(qs) < 3) {
                return(private$.emptyLogPlot(tr_p(
                    "Not enough variation in predicted probabilities to plot calibration.",
                    "No hay suficiente variación en las probabilidades predichas para graficar la calibración."
                )))
            }

            grp <- cut(fitted_vals, breaks = qs, include.lowest = TRUE)
            mean_pred <- as.numeric(tapply(fitted_vals, grp, mean))
            mean_obs <- as.numeric(tapply(dep_binary, grp, mean))

            plot_df <- data.frame(mean_pred = mean_pred, mean_obs = mean_obs)
            plot_df <- plot_df[order(plot_df$mean_pred), ]

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = mean_pred, y = mean_obs))

            if (show_ref) {
                p <- p + ggplot2::geom_abline(
                    intercept = 0, slope = 1, linetype = "dashed",
                    linewidth = 0.5, color = pal$ref
                )
            }

            p <- p +
                ggplot2::geom_line(color = pal$line, linewidth = 0.5) +
                ggplot2::geom_point(color = pal$point, size = 2.4) +
                ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
                ggplot2::labs(
                    x = tr_p("Mean predicted probability (decile)", "Probabilidad predicha media (decil)"),
                    y = tr_p("Observed proportion", "Proporción observada")
                ) +
                ggtheme +
                ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))

            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # ROC curve (discrimination).
        # ES: Curva ROC (discriminación).
        # -----------------------------------------------------------------------------
        .plotROC = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st))
                return(FALSE)

            tr_p <- function(en, es) private$.plotTr(en, es, image)
            pal <- private$.plotColors(theme)

            specificities <- st$roc_specificities
            sensitivities <- st$roc_sensitivities
            if (is.null(specificities) || is.null(sensitivities)) {
                return(private$.emptyLogPlot(tr_p(
                    "ROC curve unavailable ('pROC' package required).",
                    "Curva ROC no disponible (se requiere el paquete 'pROC')."
                )))
            }

            show_ref <- tryCatch(isTRUE(self$options$rocRefLine), error = function(e) TRUE)
            show_auc <- tryCatch(isTRUE(self$options$rocShowAUCLabel), error = function(e) TRUE)

            plot_df <- data.frame(
                fpr = 1 - specificities,
                tpr = sensitivities
            )
            plot_df <- plot_df[order(plot_df$fpr), ]

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = fpr, y = tpr))

            if (show_ref) {
                p <- p + ggplot2::geom_abline(
                    intercept = 0, slope = 1, linetype = "dashed",
                    linewidth = 0.5, color = pal$ref
                )
            }

            p <- p + ggplot2::geom_line(color = pal$line, linewidth = 0.7)

            if (show_auc) {
                auc_label <- paste0("AUC = ", format(round(st$auc_val, 3), nsmall = 3))
                p <- p + ggplot2::annotate(
                    "text", x = 0.65, y = 0.10, label = auc_label,
                    size = 3.4, color = pal$alert
                )
            }

            p <- p +
                ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
                ggplot2::labs(
                    x = tr_p("1 - Specificity", "1 - Especificidad"),
                    y = tr_p("Sensitivity", "Sensibilidad")
                ) +
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
            st <- image$state
            if (is.null(st))
                return(FALSE)

            tr_p <- function(en, es) private$.plotTr(en, es, image)
            pal <- private$.plotColors(theme)

            cooks_d <- st$cooks_d
            leverage <- st$leverage
            n <- st$n
            p_count <- st$p
            if (is.null(p_count) || !is.finite(p_count) || p_count < 1) p_count <- 2
            cooks_threshold <- 4 / n
            leverage_threshold <- 2 * p_count / n

            show_threshold <- tryCatch(isTRUE(self$options$influenceShowThreshold), error = function(e) TRUE)
            label_mode <- tryCatch(self$options$influenceLabelMode, error = function(e) "top5")

            plot_df <- data.frame(
                case = seq_along(cooks_d),
                cooksD = as.numeric(cooks_d),
                leverage = as.numeric(leverage)
            )
            plot_df$flag <- plot_df$cooksD > cooks_threshold

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = case, y = cooksD))

            if (show_threshold) {
                p <- p + ggplot2::geom_hline(
                    yintercept = cooks_threshold, linetype = "dashed",
                    linewidth = 0.5, color = pal$ref
                )
            }

            p <- p +
                ggplot2::geom_segment(
                    ggplot2::aes(xend = case, y = 0, yend = cooksD),
                    color = pal$point, linewidth = 0.3
                ) +
                ggplot2::geom_point(
                    ggplot2::aes(color = flag), size = 1.6, show.legend = FALSE
                )

            lab <- switch(label_mode,
                cooks = plot_df[plot_df$cooksD > cooks_threshold, , drop = FALSE],
                leverage = plot_df[plot_df$leverage > leverage_threshold, , drop = FALSE],
                top5 = plot_df[order(-plot_df$cooksD), , drop = FALSE][seq_len(min(5, nrow(plot_df))), , drop = FALSE],
                NULL
            )

            if (!is.null(lab) && nrow(lab) > 0) {
                p <- p + ggplot2::geom_text(
                    data = lab,
                    ggplot2::aes(x = case, y = cooksD, label = case),
                    vjust = -0.6, size = 3, check_overlap = TRUE,
                    color = pal$alert
                )
            }

            p <- p +
                ggplot2::labs(
                    x = tr_p("Case", "Caso"),
                    y = "Cook's D"
                ) +
                ggtheme +
                ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))

            # scale_color_manual must be added AFTER ggtheme - ggtheme carries
            # its own discrete colour scale, and whichever scale is added
            # last wins for a given aesthetic. This flag is always the
            # base/accent pair, regardless of plotPalette choice.
            # ES: scale_color_manual debe agregarse DESPUÉS de ggtheme -
            # ggtheme trae su propia escala discreta de color, y para una
            # misma estética gana la escala agregada al final. Este flag
            # siempre usa el par base/acento, sin importar plotPalette.
            p <- p + ggplot2::scale_color_manual(values = c(`FALSE` = pal$point, `TRUE` = pal$alert))

            print(p)
            TRUE
        }
    )
)

