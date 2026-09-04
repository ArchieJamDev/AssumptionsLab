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
# Ordinal Logistic Regression.
# ES: Regresión Logística Ordinal.
#
# This file implements ordCheck: an assumption-diagnostic module for the
# proportional-odds (cumulative logit) model (one ordinal dependent variable
# with 3+ ordered categories, numeric and/or categorical predictors). It is
# the ordinal sibling of logCheck (binary) and mlogCheck (multinomial,
# planned) - built Sep 2026 after logCheck was found too binary-specific
# (ROC/AUC, Hosmer-Lemeshow, and per-event odds ratios all assume exactly
# two outcome categories) for the multinomial/ordinal dependent variables
# common in People Analytics and survey work.
#
# NEW relative to logCheck's diagnostic battery: the proportional-odds
# ("parallel lines") assumption itself - the one assumption unique to this
# model family, tested via Brant's (1990) test - and ordinal-specific
# goodness-of-fit tests (Lipsitz; Pulkstenis-Robinson), since
# Hosmer-Lemeshow does not generalize to an ordinal response. Discrimination
# is reported as pseudo-R² (McFadden, Nagelkerke) rather than AUC, per
# Archie's decision (Sep 2026): there is no single ROC curve once the
# outcome has more than two ordered levels.
#
# REUSED from logCheck/shared-helpers.R unchanged: the manual VIF/tolerance
# computation (a property of the predictors' own design matrix, independent
# of the response family - see the Multicollinearity section below), the
# full Pearson/dCor/copula-entropy correlation-matrix machinery
# (.al_dcor_stat/.al_dcor_test/.al_copent_test/.al_dcor_na_note/
# .al_permutation_note), p_sig()/fmt_num()/html_block()/html_list() plot
# palette helpers, and the general .run()/.plotTr()/.plotPalette()/
# .plotTheme() scaffolding pattern.
#
# ES: Este archivo implementa ordCheck: un módulo de diagnóstico de
# supuestos para el modelo de momios proporcionales (logit acumulativo)
# (una variable dependiente ordinal con 3+ categorías ordenadas,
# predictores numéricos y/o categóricos). Es el hermano ordinal de
# logCheck (binario) y mlogCheck (multinomial, planeado) - construido en
# sep 2026 después de encontrar que logCheck era demasiado específico de lo
# binario (ROC/AUC, Hosmer-Lemeshow y odds ratios por evento asumen
# exactamente dos categorías de desenlace) para las variables dependientes
# multinomiales/ordinales comunes en People Analytics y trabajo de
# encuestas.
#
# NUEVO respecto a la batería de logCheck: el supuesto de momios
# proporcionales ("líneas paralelas") en sí mismo - el único supuesto
# propio de esta familia de modelos, probado con la prueba de Brant
# (1990) - y pruebas de bondad de ajuste específicas para lo ordinal
# (Lipsitz; Pulkstenis-Robinson), ya que Hosmer-Lemeshow no generaliza a
# un desenlace ordinal. La discriminación se reporta como pseudo-R²
# (McFadden, Nagelkerke) en vez de AUC, por decisión de Archie (sep 2026):
# no hay una única curva ROC una vez que el desenlace tiene más de dos
# niveles ordenados.
#
# REUTILIZADO de logCheck/shared-helpers.R sin cambios: el cálculo manual
# de VIF/tolerancia (una propiedad de la matriz de diseño de los
# predictores, independiente de la familia del desenlace - ver la sección
# de Multicolinealidad más abajo), toda la maquinaria de matriz de
# correlaciones Pearson/dCor/entropía copular
# (.al_dcor_stat/.al_dcor_test/.al_copent_test/.al_dcor_na_note/
# .al_permutation_note), los helpers p_sig()/fmt_num()/html_block()/
# html_list()/paleta de gráficos, y el patrón general de andamiaje
# .run()/.plotTr()/.plotPalette()/.plotTheme().
#
# Soft dependencies (Sep 2026, per Archie's decision - same pattern as
# copent): 'brant' (Brant's proportional-odds test) and 'generalhoslem'
# (Lipsitz and Pulkstenis-Robinson goodness-of-fit tests). Both are CORE
# sections here (not a secondary "second line of evidence" like copula
# entropy in the correlation matrix), so - unlike copentTest(), which
# silently degrades a table cell to NA when the package is missing - a
# missing brant/generalhoslem is surfaced as an explicit note in the
# corresponding section, so the user cannot mistake "package not
# installed" for "no evidence of assumption violation".
#
# ES: Dependencias opcionales (sep 2026, por decisión de Archie - mismo
# patrón que copent): 'brant' (prueba de momios proporcionales de Brant) y
# 'generalhoslem' (pruebas de bondad de ajuste de Lipsitz y
# Pulkstenis-Robinson). Ambas son secciones CENTRALES aquí (no una
# "segunda línea de evidencia" secundaria como la entropía copular en la
# matriz de correlaciones), así que - a diferencia de copentTest(), que
# degrada en silencio una celda de tabla a NA cuando falta el paquete -
# un brant/generalhoslem faltante se muestra como una nota explícita en
# la sección correspondiente, para que el usuario no confunda "paquete no
# instalado" con "no hay evidencia de violación del supuesto".
#
# Case-influence approach (Sep 2026, per Archie's decision): polr() does
# not expose Cook's D/leverage/DFFITS the way glm()/lm() do (no single
# established statistic exists in the literature the way it does for
# binary/linear models - see Liu et al., 2009, for a graphical treatment
# instead). Implemented here: a per-case Pearson-type residual comparing
# the model's fitted probability for the case's OWN OBSERVED category
# against a perfect fit (r_i = (1 - p_obs_i) / sqrt(p_obs_i * (1 -
# p_obs_i))), flagged at |r_i| > 2.5 (consistent with pathCheck's
# residFlagOutliers convention elsewhere in the suite). This is a
# reasonable, transparent approximation, not a single canonical published
# statistic - the guide text says so explicitly and points to Liu et al.
# (2009) for a fuller graphical treatment.
#
# ES: Enfoque de casos influyentes (sep 2026, por decisión de Archie):
# polr() no expone Cook's D/leverage/DFFITS como sí lo hacen glm()/lm()
# (no existe un único estadístico establecido en la literatura como sí
# existe para modelos binarios/lineales - ver Liu et al., 2009, para un
# tratamiento gráfico en cambio). Implementado aquí: un residuo tipo
# Pearson por caso que compara la probabilidad ajustada del modelo para
# la CATEGORÍA REALMENTE OBSERVADA del caso contra un ajuste perfecto
# (r_i = (1 - p_obs_i) / sqrt(p_obs_i * (1 - p_obs_i))), marcado en |r_i|
# > 2.5 (consistente con la convención residFlagOutliers de pathCheck en
# otra parte de la suite). Es una aproximación razonable y transparente,
# no un único estadístico canónico publicado - el texto de la guía lo
# dice explícitamente y remite a Liu et al. (2009) para un tratamiento
# gráfico más completo.
#
# Workflow
# 1. Validate the dependent variable and predictors, then fit the
#    proportional-odds (cumulative logit) model.
# 2. Summarize the model design.
# 3. Screen for complete or quasi-complete separation.
# 4. Test linearity in the cumulative logit (Box-Tidwell).
# 5. Test the proportional-odds assumption (Brant test).
# 6. Evaluate goodness of fit (Lipsitz and Pulkstenis-Robinson tests).
# 7. Report pseudo-R² discrimination.
# 8. Assess multicollinearity among the predictors.
# 9. Build the correlation matrices (Pearson / dCor / copula entropy).
# 10. Detect influential cases.
# 11. Report coefficients, odds ratios, and cutpoints.
# 12. Assemble the recommendation and notes.
#
# ES: Flujo de trabajo
# 1. Validar la variable dependiente y los predictores, y ajustar el
#    modelo de momios proporcionales (logit acumulativo).
# 2. Resumir el diseño del modelo.
# 3. Detectar separación completa o casi completa.
# 4. Evaluar la linealidad en el logit acumulativo (Box-Tidwell).
# 5. Probar el supuesto de momios proporcionales (prueba de Brant).
# 6. Evaluar la bondad de ajuste (pruebas de Lipsitz y
#    Pulkstenis-Robinson).
# 7. Reportar la discriminación mediante pseudo-R².
# 8. Evaluar la multicolinealidad entre los predictores.
# 9. Construir las matrices de correlación (Pearson / dCor / entropía
#    copular).
# 10. Detectar casos influyentes.
# 11. Reportar coeficientes, odds ratios y puntos de corte.
# 12. Elaborar la recomendación y las notas.
# -----------------------------------------------------------------------------

ordCheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "ordCheckClass",
    inherit = ordCheckBase,
    private = list(
        .init = function() {
            private$.initCorrelationMatrix()
        },

        # dcor_stat/dcor_pvalue's column-per-variable dynamic setup:
        # byte-identical pattern to logCheck's .initCorrelationMatrix().
        # ES: patrón idéntico al .initCorrelationMatrix() de logCheck.
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

            html_escape <- .al_html_escape

            html_block <- function(title = NULL, text, paragraphs = TRUE, escape = TRUE, raw = FALSE) {
                .al_html_block(title, text, paragraphs = paragraphs, escape = escape, raw = raw)
            }

            html_guide <- function(title, items) {
                html_block(title, .al_html_list(items), raw = TRUE)
            }

            clean_num <- .al_clean_num

            fmt_num <- function(x, digits = 3) {
                x <- clean_num(x)
                if (is.na(x))
                    return(tr("Not computed", "No calculado"))
                format(round(x, digits), nsmall = digits)
            }

            p_sig <- .al_p_sig

            add_row <- function(table, key, values) {
                table$addRow(rowKey = key, values = values)
            }

            qname <- function(x) {
                paste0("`", gsub("`", "", x), "`")
            }

            # dcor_stat()/dcor_pvalue(): consolidated in shared-helpers.R
            # (.al_dcor_stat/.al_dcor_test), same pattern as the other four
            # modules that have them. Fixed B=199/seed=20260704.
            # ES: consolidadas en shared-helpers.R (.al_dcor_stat/
            # .al_dcor_test), mismo patrón que los otros cuatro módulos que
            # las tienen. B=199/semilla=20260704 fijos.
            dcor_stat <- .al_dcor_stat
            dcor_pvalue <- function(x, y) .al_dcor_test(x, y)$p

            # copentTest(): byte-identical in every module that has it,
            # consolidated in shared-helpers.R (.al_copent_test).
            # ES: idéntica en todos los módulos que la usan, consolidada
            # en shared-helpers.R.
            copentTest <- .al_copent_test

            apaCell <- function(r, p, digits = 3) .al_apa_cell(r, p, digits)

            set_result_titles <- function() {

                set_title_safe <- function(name, en, es) {
                    element <- tryCatch(self$results[[name]], error = function(e) NULL)
                    if (is.null(element)) return(invisible(FALSE))
                    tryCatch(element$setTitle(tr(en, es)), error = function(e) invisible(FALSE))
                    invisible(TRUE)
                }

                titles <- list(
                    c("intro", "Introduction", "Introducción"),
                    c("designGuide", "Model design", "Diseño del modelo"),
                    c("design", "Design summary", "Resumen del diseño"),
                    c("categoryDistribution", "Category distribution", "Distribución por categoría"),

                    c("separationGuide", "Complete separation", "Separación completa"),
                    c("separation", "Complete separation", "Separación completa"),
                    c("separationInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("linearityGuide", "Linearity in the cumulative logit", "Linealidad en el logit acumulativo"),
                    c("linearity",
                      "Linearity in the cumulative logit (Box-Tidwell)",
                      "Linealidad en el logit acumulativo (Box-Tidwell)"),
                    c("linearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("linearityPlot", "Empirical logit vs predictor", "Logit empírico vs predictor"),

                    c("proportionalOddsGuide",
                      "Proportional odds (parallel lines)",
                      "Momios proporcionales (líneas paralelas)"),
                    c("proportionalOdds", "Brant test", "Prueba de Brant"),
                    c("proportionalOddsInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("goodnessOfFitGuide", "Goodness of fit", "Bondad de ajuste"),
                    c("goodnessOfFit", "Goodness of fit", "Bondad de ajuste"),
                    c("goodnessOfFitInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("discriminationGuide", "Pseudo-R\u00b2", "Pseudo-R\u00b2"),
                    c("discrimination", "Pseudo-R\u00b2", "Pseudo-R\u00b2"),
                    c("discriminationInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

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
                    c("influencePlot", "Pearson residual by case", "Residuo de Pearson por caso"),

                    c("coefficientsGuide",
                      "Coefficients and odds ratios (proportional odds)",
                      "Coeficientes y odds ratios (momios proporcionales)"),
                    c("coefficients", "Coefficients and odds ratios", "Coeficientes y odds ratios"),
                    c("coefficientsInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("cutpoints", "Cutpoints (intercepts)", "Puntos de corte (intercepts)"),

                    c("notes", "Notes and recommendation", "Notas y recomendación")
                )

                for (item in titles) set_title_safe(item[[1]], item[[2]], item[[3]])
            }

            set_result_titles()

            set_table_column_titles <- function() {

                set_col_title_safe <- function(table_name, col_name, en, es) {
                    table <- tryCatch(self$results[[table_name]], error = function(e) NULL)
                    if (is.null(table)) return(invisible(FALSE))
                    column <- tryCatch(table$getColumn(col_name), error = function(e) NULL)
                    if (is.null(column)) return(invisible(FALSE))
                    tryCatch(column$setTitle(tr(en, es)), error = function(e) invisible(FALSE))
                    invisible(TRUE)
                }

                cols <- list(
                    c("design", "element", "Element", "Elemento"),
                    c("design", "value", "Value", "Valor"),

                    c("categoryDistribution", "category", "Category", "Categoría"),
                    c("categoryDistribution", "n", "n", "n"),
                    c("categoryDistribution", "proportion", "Proportion", "Proporción"),

                    c("separation", "predictor", "Predictor", "Predictor"),
                    c("separation", "type", "Type", "Tipo"),
                    c("separation", "coefficient", "Coefficient", "Coeficiente"),

                    c("linearity", "predictor", "Predictor", "Predictor"),
                    c("linearity", "statistic", "Statistic", "Estadístico"),
                    c("linearity", "pSig", "Sig.", "Sig."),

                    c("proportionalOdds", "term", "Term", "Término"),
                    c("proportionalOdds", "chiSq", "X\u00b2", "X\u00b2"),
                    c("proportionalOdds", "df", "df", "gl"),
                    c("proportionalOdds", "pSig", "Sig.", "Sig."),

                    c("goodnessOfFit", "test", "Test", "Prueba"),
                    c("goodnessOfFit", "statistic", "Statistic", "Estadístico"),
                    c("goodnessOfFit", "df", "df", "gl"),
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
                    c("correlationComparisonTable", "gap", "Gap (dCor - |r|)", "Brecha (dCor \u2212 |r|)"),
                    c("correlationComparisonTable", "ce", "Copula entropy", "Entropía copular"),
                    c("correlationComparisonTable", "ceP", "p (CE)", "p (CE)"),
                    c("correlationComparisonTable", "ceSig", "Sig. (CE)", "Sig. (CE)"),
                    c("correlationComparisonTable", "flag", "Notable difference", "Diferencia notable"),

                    c("influence", "case", "Case", "Caso"),
                    c("influence", "observed", "Observed category", "Categoría observada"),
                    c("influence", "pearsonStat", "Pearson statistic", "Estadístico de Pearson"),
                    c("influence", "p", "p", "p"),

                    c("coefficients", "predictor", "Predictor", "Predictor"),
                    c("coefficients", "or", "OR", "OR"),
                    c("coefficients", "ciLower", "95% CI lower", "IC 95% inferior"),
                    c("coefficients", "ciUpper", "95% CI upper", "IC 95% superior"),
                    c("coefficients", "pSig", "Sig.", "Sig."),

                    c("cutpoints", "cutpoint", "Cutpoint", "Punto de corte"),
                    c("cutpoints", "estimate", "Estimate", "Estimación"),
                    c("cutpoints", "se", "SE", "EE")
                )

                for (item in cols) set_col_title_safe(item[[1]], item[[2]], item[[3]], item[[4]])
            }

            set_table_column_titles()

            # -----------------------------------------------------------------------------
            # Data / dependent-variable checks.
            # ES: Datos / verificaciones de la variable dependiente.
            # -----------------------------------------------------------------------------
            dep_var <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors

            data <- self$data
            if (is.null(data) || nrow(data) == 0) {
                self$results$intro$setContent(html_block(NULL, tr(
                    "No data are available.", "No hay datos disponibles."
                ), paragraphs = FALSE))
                return()
            }

            if (is.null(dep_var) || !nzchar(dep_var) || !(dep_var %in% names(data))) {
                self$results$intro$setContent(html_block(NULL, tr(
                    "Dependent variable not selected.", "Variable dependiente no seleccionada."
                ), paragraphs = FALSE))
                return()
            }

            dep_raw <- data[[dep_var]]
            if (!is.factor(dep_raw)) {
                self$results$intro$setContent(html_block(NULL, tr(
                    "The dependent variable must be an ordinal (ordered factor) variable.",
                    "La variable dependiente debe ser una variable ordinal (factor ordenado)."
                ), paragraphs = FALSE))
                return()
            }

            dep_levels <- levels(droplevels(dep_raw))
            n_levels <- length(dep_levels)

            if (n_levels < 3) {
                self$results$intro$setContent(html_block(NULL, tr(
                    "The dependent variable has fewer than 3 categories. Use logCheck for a binary (2-category) dependent variable instead - it gives richer binary-specific diagnostics (ROC/AUC, Hosmer-Lemeshow, odds ratios).",
                    "La variable dependiente tiene menos de 3 categorías. Use logCheck en cambio para una variable dependiente binaria (2 categorías) - da diagnósticos binarios más ricos (ROC/AUC, Hosmer-Lemeshow, odds ratios)."
                ), paragraphs = FALSE))
                return()
            }

            predictors <- c(covs, factors)
            if (length(predictors) == 0) {
                self$results$intro$setContent(html_block(NULL, tr(
                    "Select at least one predictor.", "Seleccione al menos un predictor."
                ), paragraphs = FALSE))
                return()
            }

            model_data <- data
            model_data$dep_ord <- factor(dep_raw, levels = dep_levels, ordered = TRUE)
            complete_cases <- stats::complete.cases(model_data[, c("dep_ord", predictors), drop = FALSE])
            model_data_complete <- model_data[complete_cases, ]

            n_total <- nrow(data)
            n_complete <- nrow(model_data_complete)
            n_predictors <- length(predictors)

            cat_counts <- table(model_data_complete$dep_ord)
            min_cat_n <- min(cat_counts)
            min_cat_name <- names(cat_counts)[which.min(cat_counts)]

            formula_str <- paste("dep_ord ~", paste(qname(predictors), collapse = " + "))
            formula <- stats::as.formula(formula_str)
            null_formula <- stats::as.formula("dep_ord ~ 1")

            model <- tryCatch({
                MASS::polr(formula, data = model_data_complete, Hess = TRUE, method = "logistic")
            }, error = function(e) NULL)

            if (is.null(model)) {
                self$results$notes$setContent(html_block(NULL, tr(
                    "Error fitting the model. Check for near-perfect separation or a predictor with an unused level in a subset of the data.",
                    "Error al ajustar el modelo. Revise si hay cuasi-separación o un predictor con un nivel no usado en un subconjunto de los datos."
                ), paragraphs = FALSE))
                return()
            }

            null_model <- tryCatch({
                MASS::polr(null_formula, data = model_data_complete, Hess = TRUE, method = "logistic")
            }, error = function(e) NULL)

            model_summary <- summary(model)
            coef_table_full <- model_summary$coefficients
            n_cut <- n_levels - 1L
            n_slope_rows <- nrow(coef_table_full) - n_cut
            coef_table <- coef_table_full[seq_len(max(n_slope_rows, 0)), , drop = FALSE]
            cut_table <- coef_table_full[seq_len(n_cut) + n_slope_rows, , drop = FALSE]

            # z-based two-sided p-values: polr's summary() does not include
            # them (it only reports the t/z value), unlike glm()'s summary().
            # ES: valores p bilaterales basados en z: summary() de polr no
            # los incluye (solo reporta el valor t/z), a diferencia de
            # summary() de glm().
            slope_p <- if (nrow(coef_table) > 0) 2 * stats::pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) else numeric(0)

            n_model_params <- length(coef(model)) + n_cut

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
                tr("Assumption check for ordinal logistic regression (proportional odds)", "Revisión de supuestos para regresión logística ordinal (momios proporcionales)"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                "<p style=\"margin: 0 0 0.55em 0; line-height: 1.35;\">",
                tr(
                    "Use this analysis when you want to review whether a proportional-odds (ordinal logistic) model has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.",
                    "Use este análisis cuando quiera revisar si un modelo de momios proporcionales (regresión logística ordinal) tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."
                ),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                html_block(NULL, c(
                    paste0(
                        tr("<b>Categories (low to high):</b> ", "<b>Categorías (de menor a mayor):</b> "),
                        html_escape(paste(dep_levels, collapse = " < "))
                    ),
                    paste0(
                        tr("<b>Complete cases:</b> ", "<b>Casos completos:</b> "), n_complete,
                        tr(" | <b>Smallest category:</b> ", " | <b>Categoría más pequeña:</b> "),
                        html_escape(min_cat_name), " (n = ", min_cat_n, ")"
                    ),
                    paste0(
                        tr("<b>Estimated parameters:</b> ", "<b>Parámetros estimados:</b> "),
                        n_model_params,
                        tr(" (predictor coefficients + cutpoints)", " (coeficientes de predictores + puntos de corte)")
                    )
                ), paragraphs = TRUE, escape = FALSE),
                "</div>"
            ))

            # -----------------------------------------------------------------------------
            # Model design.
            # ES: Diseño del modelo.
            # -----------------------------------------------------------------------------
            self$results$designGuide$setContent(html_guide(tr("Model design", "Diseño del modelo"), tr(
                c(
                    "The proportional-odds model treats the dependent variable's categories as ordered but not equally spaced, and models the cumulative logit - log-odds of being at or below each cutpoint - as a linear function of the predictors.",
                    "Before interpreting the model, review that every category has a reasonable number of cases and that the number of estimated parameters (predictor coefficients plus k-1 cutpoints) is small relative to the smallest category.",
                    "A model can be computed even when some categories are sparse, but coefficient estimates and standard errors become unstable as the smallest category shrinks relative to the number of parameters."
                ),
                c(
                    "El modelo de momios proporcionales trata las categorías de la variable dependiente como ordenadas pero no necesariamente equiespaciadas, y modela el logit acumulativo - el log-momios de estar en o por debajo de cada punto de corte - como una función lineal de los predictores.",
                    "Antes de interpretar el modelo, revise que cada categoría tenga un número razonable de casos y que el número de parámetros estimados (coeficientes de predictores más k-1 puntos de corte) sea pequeño frente a la categoría más pequeña.",
                    "Un modelo puede calcularse incluso cuando alguna categoría es escasa, pero las estimaciones de coeficientes y sus errores estándar se vuelven inestables a medida que la categoría más pequeña se achica frente al número de parámetros."
                )
            )))

            add_row(self$results$design, "design_1", list(element = tr("Total cases", "Casos totales"), value = as.character(n_total)))
            add_row(self$results$design, "design_2", list(element = tr("Complete cases", "Casos completos"), value = as.character(n_complete)))
            add_row(self$results$design, "design_3", list(element = tr("Number of categories", "Número de categorías"), value = as.character(n_levels)))
            add_row(self$results$design, "design_4", list(element = tr("Smallest category", "Categoría más pequeña"), value = paste0(html_escape(min_cat_name), " (n = ", min_cat_n, ")")))
            add_row(self$results$design, "design_5", list(element = tr("Estimated parameters", "Parámetros estimados"), value = as.character(n_model_params)))

            for (i in seq_along(cat_counts)) {
                add_row(self$results$categoryDistribution, paste0("cat_", i), list(
                    category = names(cat_counts)[i],
                    n = as.integer(cat_counts[i]),
                    proportion = as.numeric(cat_counts[i]) / n_complete
                ))
            }

            # -----------------------------------------------------------------------------
            # Complete / quasi-complete separation (same coefficient/SE
            # screen as logCheck, applied only to the slope rows - the
            # cutpoint rows are excluded since a large cutpoint estimate
            # with a large SE is common and not itself a separation signal).
            # ES: Separación completa/cuasi-completa (mismo cribado de
            # coeficiente/EE que logCheck, aplicado solo a las filas de
            # pendientes - las filas de puntos de corte se excluyen porque
            # una estimación de punto de corte grande con EE grande es
            # común y no es en sí una señal de separación).
            # -----------------------------------------------------------------------------
            self$results$separationGuide$setContent(html_guide(tr("Complete separation", "Separación completa"), tr(
                c(
                    "Complete or quasi-complete separation occurs when a predictor (or combination of predictors) perfectly or nearly perfectly predicts which side of a cutpoint a case falls on.",
                    "Very large coefficients together with very large standard errors are the typical signal of this problem.",
                    "This check only screens for the extreme coefficient/standard-error signature in the predictor (slope) rows; it does not guarantee adequate cell counts across every category combination."
                ),
                c(
                    "La separación completa o cuasi-completa ocurre cuando un predictor (o combinación de predictores) predice perfecta o casi perfectamente de qué lado de un punto de corte cae un caso.",
                    "Coeficientes muy grandes junto con errores estándar muy grandes son la señal típica de este problema.",
                    "Esta revisión solo detecta la firma extrema de coeficiente/error estándar en las filas de predictores (pendientes); no garantiza conteos de celda adecuados en cada combinación de categorías."
                )
            )))

            n_separations <- 0
            if (nrow(coef_table) > 0) {
                for (i in seq_len(nrow(coef_table))) {
                    predictor_name <- rownames(coef_table)[i]
                    coef_val <- coef_table[i, "Value"]
                    se_val <- coef_table[i, "Std. Error"]
                    if (!is.na(coef_val) && !is.na(se_val) && abs(coef_val) > 5 && se_val > 2) {
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
                    predictor = tr("Not detected", "No detectada"), type = "-", coefficient = NA
                ))
            }

            self$results$separationInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (n_separations > 0) {
                    tr(
                        paste0(n_separations, " of ", n_predictors, " predictor(s) show coefficients and standard errors consistent with complete or quasi-complete separation. Estimates for these predictors are not reliable and should not be interpreted at face value."),
                        paste0(n_separations, " de ", n_predictors, " predictor(es) muestran coeficientes y errores estándar consistentes con separación completa o cuasi-completa. Las estimaciones de esos predictores no son confiables y no deben interpretarse literalmente.")
                    )
                } else {
                    tr(
                        "No coefficient/standard-error pattern consistent with complete separation was detected in the predictor (slope) rows.",
                        "No se detectó ningún patrón de coeficiente/error estándar consistente con separación completa en las filas de predictores (pendientes)."
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Linearity in the cumulative logit (Box-Tidwell-style):
            # a numeric_predictor * log(numeric_predictor) interaction
            # added to the ordinal model; a significant interaction term
            # suggests the predictor's relationship with the cumulative
            # logit is not linear.
            # ES: Linealidad en el logit acumulativo (estilo Box-Tidwell):
            # una interacción predictor_numérico * log(predictor_numérico)
            # agregada al modelo ordinal; un término de interacción
            # significativo sugiere que la relación del predictor con el
            # logit acumulativo no es lineal.
            # -----------------------------------------------------------------------------
            self$results$linearityGuide$setContent(html_guide(tr("Linearity in the cumulative logit", "Linealidad en el logit acumulativo"), tr(
                c(
                    "The proportional-odds model does not assume the outcome relates linearly to the predictors, but that the cumulative logit does.",
                    "This assumption is checked with a Box-Tidwell-style procedure, adding an interaction term between each numeric predictor and its own logarithm to the ordinal model.",
                    "A significant term suggests the real relationship is not linear in the cumulative logit and the predictor may need a transformation or a nonlinear term.",
                    "This check only applies to numeric predictors; categorical predictors do not have this assumption."
                ),
                c(
                    "El modelo de momios proporcionales no asume que el desenlace se relacione linealmente con los predictores, sino que el logit acumulativo lo haga.",
                    "Este supuesto se revisa con un procedimiento estilo Box-Tidwell, agregando un término de interacción entre cada predictor numérico y su propio logaritmo al modelo ordinal.",
                    "Un término significativo sugiere que la relación real no es lineal en el logit acumulativo y el predictor podría necesitar una transformación o un término no lineal.",
                    "Esta revisión solo aplica a predictores numéricos; los predictores categóricos no tienen este supuesto."
                )
            )))

            bt_tested <- 0
            bt_significant <- 0
            if (length(covs) > 0) {
                for (predictor in covs) {
                    x <- model_data_complete[[predictor]]
                    if (!is.numeric(x) || any(x <= 0, na.rm = TRUE)) next()

                    bt_data <- model_data_complete
                    bt_data$.bt_log_int <- x * log(x)
                    bt_formula <- stats::as.formula(paste(
                        "dep_ord ~", paste(qname(predictors), collapse = " + "), "+", qname(".bt_log_int")
                    ))

                    bt_model <- tryCatch(MASS::polr(bt_formula, data = bt_data, Hess = TRUE), error = function(e) NULL)
                    if (is.null(bt_model)) next()

                    bt_summary <- summary(bt_model)
                    bt_coefs <- bt_summary$coefficients
                    if (!(".bt_log_int" %in% rownames(bt_coefs))) next()

                    bt_t <- bt_coefs[".bt_log_int", "t value"]
                    bt_p <- 2 * stats::pnorm(abs(bt_t), lower.tail = FALSE)

                    bt_tested <- bt_tested + 1
                    if (!is.na(bt_p) && bt_p < 0.05) bt_significant <- bt_significant + 1

                    add_row(self$results$linearity, paste0("lin_", predictor), list(
                        predictor = predictor, statistic = bt_t, p = bt_p, pSig = p_sig(bt_p)
                    ))
                }
            }

            self$results$linearityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (bt_tested == 0) {
                    tr("No numeric predictors were available to test for linearity in the cumulative logit.",
                       "No había predictores numéricos disponibles para revisar la linealidad en el logit acumulativo.")
                } else {
                    c(
                        tr(
                            paste0(bt_significant, " of ", bt_tested, " numeric predictor(s) showed a significant Box-Tidwell-style interaction, suggesting the relationship with the cumulative logit may not be linear for those predictors."),
                            paste0(bt_significant, " de ", bt_tested, " predictor(es) numérico(s) mostraron una interacción estilo Box-Tidwell significativa, lo que sugiere que la relación con el logit acumulativo podría no ser lineal para esos predictores.")
                        ),
                        tr(
                            "If non-linearity is confirmed, consider adding a quadratic or spline term for that predictor, or categorizing it into meaningful groups.",
                            "Si se confirma la no linealidad, considere agregar un término cuadrático o spline para ese predictor, o categorizarlo en grupos con sentido."
                        )
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Proportional odds (Brant test) - soft dependency on 'brant'.
            # ES: Momios proporcionales (prueba de Brant) - dependencia
            # opcional del paquete 'brant'.
            # -----------------------------------------------------------------------------
            self$results$proportionalOddsGuide$setContent(html_guide(
                tr("Proportional odds (parallel lines)", "Momios proporcionales (líneas paralelas)"),
                tr(
                    c(
                        "The proportional-odds model assumes each predictor has the SAME effect on every cutpoint of the outcome - the 'parallel lines' or 'proportional odds' assumption, and the one assumption unique to this model family.",
                        "Brant's (1990) test compares the coefficients from separate binary logits fit at each cutpoint against the single, pooled coefficient the proportional-odds model reports; a significant term means that predictor's effect is not constant across cutpoints.",
                        "If the omnibus test is significant but only one or two predictors drive it, consider a partial-proportional-odds model that lets just those predictors vary by cutpoint, rather than abandoning the ordinal approach entirely.",
                        "This test has limited power in small samples or with sparse categories; a non-significant result there is weaker evidence of proportionality than the same result with a larger, better-balanced sample."
                    ),
                    c(
                        "El modelo de momios proporcionales asume que cada predictor tiene el MISMO efecto en todos los puntos de corte del desenlace - el supuesto de \"líneas paralelas\" o \"momios proporcionales\", y el único supuesto propio de esta familia de modelos.",
                        "La prueba de Brant (1990) compara los coeficientes de logits binarios separados ajustados en cada punto de corte contra el coeficiente único y agrupado que reporta el modelo de momios proporcionales; un término significativo significa que el efecto de ese predictor no es constante entre puntos de corte.",
                        "Si la prueba ómnibus es significativa pero solo uno o dos predictores la explican, considere un modelo de momios parcialmente proporcionales que deje variar solo esos predictores por punto de corte, en vez de abandonar el enfoque ordinal por completo.",
                        "Esta prueba tiene poder limitado en muestras pequeñas o con categorías escasas; un resultado no significativo ahí es evidencia más débil de proporcionalidad que el mismo resultado con una muestra más grande y balanceada."
                    )
                )
            ))

            brant_available <- requireNamespace("brant", quietly = TRUE)
            brant_res <- if (brant_available) tryCatch(brant::brant(model), error = function(e) NULL) else NULL

            if (!brant_available) {
                add_row(self$results$proportionalOdds, "pom_missing", list(
                    term = tr("Package 'brant' not installed", "Paquete 'brant' no instalado"),
                    chiSq = NA, df = NA, p = NA, pSig = ""
                ))
            } else if (is.null(brant_res)) {
                add_row(self$results$proportionalOdds, "pom_error", list(
                    term = tr("Test could not be computed", "No se pudo calcular la prueba"),
                    chiSq = NA, df = NA, p = NA, pSig = ""
                ))
            } else {
                brant_terms <- rownames(brant_res)
                for (i in seq_along(brant_terms)) {
                    term_label <- if (identical(brant_terms[i], "Omnibus")) tr("Omnibus", "Ómnibus") else brant_terms[i]
                    add_row(self$results$proportionalOdds, paste0("pom_", i), list(
                        term = term_label,
                        chiSq = brant_res[i, "X2"],
                        df = brant_res[i, "df"],
                        p = brant_res[i, "probability"],
                        pSig = p_sig(brant_res[i, "probability"])
                    ))
                }
            }

            pom_omnibus_p <- if (!is.null(brant_res) && "Omnibus" %in% rownames(brant_res)) brant_res["Omnibus", "probability"] else NA_real_

            self$results$proportionalOddsInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (!brant_available) {
                    tr(
                        "The 'brant' package is not installed, so the proportional-odds assumption could not be tested. Install it (install.packages(\"brant\")) to enable this check - it is the central assumption of this model family and should not be skipped in a final analysis.",
                        "El paquete 'brant' no está instalado, así que no se pudo probar el supuesto de momios proporcionales. Instálelo (install.packages(\"brant\")) para habilitar esta revisión - es el supuesto central de esta familia de modelos y no debería omitirse en un análisis final."
                    )
                } else if (is.null(brant_res)) {
                    tr(
                        "Brant's test could not be computed for this model, most often because a category or predictor combination is too sparse.",
                        "La prueba de Brant no pudo calcularse para este modelo, con mayor frecuencia porque alguna combinación de categoría o predictor es demasiado escasa."
                    )
                } else if (!is.na(pom_omnibus_p) && pom_omnibus_p < 0.05) {
                    c(
                        tr(
                            paste0("The omnibus test is significant (p = ", fmt_num(pom_omnibus_p, 3), "), indicating at least one predictor's effect is not constant across cutpoints - the proportional-odds assumption is not well supported for this model as specified."),
                            paste0("La prueba ómnibus es significativa (p = ", fmt_num(pom_omnibus_p, 3), "), lo que indica que al menos un predictor tiene un efecto que no es constante entre puntos de corte - el supuesto de momios proporcionales no está bien respaldado para este modelo tal como está especificado.")
                        ),
                        tr(
                            "Check the per-term rows above to see which predictor(s) drive the violation before deciding between a partial-proportional-odds model, a multinomial model, or reporting cutpoint-specific effects.",
                            "Revise las filas por término de arriba para ver qué predictor(es) impulsan la violación antes de decidir entre un modelo de momios parcialmente proporcionales, un modelo multinomial, o reportar efectos específicos por punto de corte."
                        )
                    )
                } else {
                    tr(
                        "The omnibus test is not significant, compatible with the proportional-odds assumption holding across the predictors in this model.",
                        "La prueba ómnibus no es significativa, compatible con que el supuesto de momios proporcionales se sostenga entre los predictores de este modelo."
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Goodness of fit - soft dependency on 'generalhoslem'.
            # ES: Bondad de ajuste - dependencia opcional del paquete
            # 'generalhoslem'.
            # -----------------------------------------------------------------------------
            self$results$goodnessOfFitGuide$setContent(html_guide(tr("Goodness of fit", "Bondad de ajuste"), tr(
                c(
                    "Goodness of fit evaluates whether the model, as a whole, reasonably reproduces the observed category frequencies.",
                    "The Hosmer-Lemeshow test does not generalize directly to an ordinal response; the Lipsitz (1996) and Pulkstenis-Robinson (2004) tests are the ordinal-specific analogues used here instead.",
                    "The Pulkstenis-Robinson test needs at least one categorical predictor to partition cases into covariate patterns; it is not shown when only numeric predictors are selected. It also needs at least as many distinct covariate patterns as groups to be valid (Fagerland & Hosmer, 2013) - with very few categorical predictors or very few levels, treat a result from this test with extra caution.",
                    "As with any chi-square-based fit test, these lose power with very small samples and can flag trivially small lack of fit with very large ones - read them together with the proportional-odds result and substantive knowledge of the data."
                ),
                c(
                    "La bondad de ajuste evalúa si el modelo, en conjunto, reproduce razonablemente las frecuencias de categoría observadas.",
                    "La prueba de Hosmer-Lemeshow no generaliza directamente a un desenlace ordinal; las pruebas de Lipsitz (1996) y Pulkstenis-Robinson (2004) son los análogos específicos para lo ordinal usados aquí en cambio.",
                    "La prueba de Pulkstenis-Robinson necesita al menos un predictor categórico para particionar los casos en patrones de covariables; no se muestra cuando solo hay predictores numéricos seleccionados. También necesita al menos tantos patrones de covariables distintos como grupos para ser válida (Fagerland & Hosmer, 2013) - con muy pocos predictores categóricos o muy pocos niveles, trate un resultado de esta prueba con cautela adicional.",
                    "Como cualquier prueba de ajuste basada en ji-cuadrado, estas pierden poder con muestras muy pequeñas y pueden marcar una falta de ajuste trivial con muestras muy grandes - léalas junto con el resultado de momios proporcionales y el conocimiento sustantivo de los datos."
                )
            )))

            gof_available <- requireNamespace("generalhoslem", quietly = TRUE)

            if (!gof_available) {
                add_row(self$results$goodnessOfFit, "gof_missing", list(
                    test = tr("Package 'generalhoslem' not installed", "Paquete 'generalhoslem' no instalado"),
                    statistic = NA, df = NA, p = NA, pSig = ""
                ))
            } else {
                lipsitz_res <- tryCatch(generalhoslem::lipsitz.test(model), error = function(e) NULL)
                if (!is.null(lipsitz_res)) {
                    add_row(self$results$goodnessOfFit, "gof_lipsitz", list(
                        test = "Lipsitz",
                        statistic = unname(lipsitz_res$statistic),
                        df = unname(lipsitz_res$parameter),
                        p = lipsitz_res$p.value,
                        pSig = p_sig(lipsitz_res$p.value)
                    ))
                }

                if (length(factors) > 0) {
                    pr_chisq <- tryCatch(generalhoslem::pulkrob.chisq(model, catvars = factors), error = function(e) NULL)
                    if (!is.null(pr_chisq)) {
                        add_row(self$results$goodnessOfFit, "gof_pr_chisq", list(
                            test = tr("Pulkstenis-Robinson (chi-squared)", "Pulkstenis-Robinson (ji-cuadrado)"),
                            statistic = unname(pr_chisq$statistic),
                            df = unname(pr_chisq$parameter),
                            p = pr_chisq$p.value,
                            pSig = p_sig(pr_chisq$p.value)
                        ))
                    }
                    pr_dev <- tryCatch(generalhoslem::pulkrob.deviance(model, catvars = factors), error = function(e) NULL)
                    if (!is.null(pr_dev)) {
                        add_row(self$results$goodnessOfFit, "gof_pr_dev", list(
                            test = tr("Pulkstenis-Robinson (deviance)", "Pulkstenis-Robinson (devianza)"),
                            statistic = unname(pr_dev$statistic),
                            df = unname(pr_dev$parameter),
                            p = pr_dev$p.value,
                            pSig = p_sig(pr_dev$p.value)
                        ))
                    }
                }

                if (self$results$goodnessOfFit$rowCount == 0) {
                    add_row(self$results$goodnessOfFit, "gof_none", list(
                        test = tr("No test could be computed", "No se pudo calcular ninguna prueba"),
                        statistic = NA, df = NA, p = NA, pSig = ""
                    ))
                }
            }

            gof_any_sig <- FALSE
            if (gof_available) {
                gof_rows_df <- tryCatch(self$results$goodnessOfFit$asDF, error = function(e) NULL)
                if (!is.null(gof_rows_df) && "p" %in% names(gof_rows_df))
                    gof_any_sig <- any(!is.na(gof_rows_df$p) & gof_rows_df$p < 0.05)
            }

            self$results$goodnessOfFitInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (!gof_available) {
                    tr(
                        "The 'generalhoslem' package is not installed, so goodness of fit could not be tested. Install it (install.packages(\"generalhoslem\")) to enable this check.",
                        "El paquete 'generalhoslem' no está instalado, así que no se pudo probar la bondad de ajuste. Instálelo (install.packages(\"generalhoslem\")) para habilitar esta revisión."
                    )
                } else if (gof_any_sig) {
                    tr(
                        "At least one goodness-of-fit test is significant, suggesting the model does not reproduce the observed category frequencies well in some region of the predictor space. Consider additional predictors, interactions, or reviewing category coding before trusting model-based predictions.",
                        "Al menos una prueba de bondad de ajuste es significativa, lo que sugiere que el modelo no reproduce bien las frecuencias de categoría observadas en alguna región del espacio de predictores. Considere predictores adicionales, interacciones, o revisar la codificación de categorías antes de confiar en predicciones basadas en el modelo."
                    )
                } else {
                    tr(
                        "The available goodness-of-fit tests are compatible with adequate fit.",
                        "Las pruebas de bondad de ajuste disponibles son compatibles con un ajuste adecuado."
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Pseudo-R² (discrimination replacement, per Archie's decision:
            # no single ROC/AUC once the outcome has 3+ ordered levels).
            # ES: Pseudo-R² (reemplazo de discriminación, por decisión de
            # Archie: no hay un único ROC/AUC una vez que el desenlace
            # tiene 3+ niveles ordenados).
            # -----------------------------------------------------------------------------
            self$results$discriminationGuide$setContent(html_guide(tr("Pseudo-R\u00b2", "Pseudo-R\u00b2"), tr(
                c(
                    "There is no single ROC curve or AUC once the outcome has more than two ordered categories, so overall model quality is summarized here with pseudo-R\u00b2 measures instead.",
                    "McFadden's pseudo-R\u00b2 compares the fitted model's log-likelihood to an intercept-only model; values of .20-.40 are often described as a good fit, but this rule of thumb is much less established than R\u00b2 conventions for linear regression.",
                    "Nagelkerke's pseudo-R\u00b2 rescales a related measure so it can reach 1.0, making it more directly comparable across models, but it is not on the same numeric scale as a linear-regression R\u00b2 and should not be interpreted as \"percent of variance explained\" in that sense."
                ),
                c(
                    "No hay una única curva ROC ni AUC una vez que el desenlace tiene más de dos categorías ordenadas, así que aquí la calidad general del modelo se resume con medidas de pseudo-R\u00b2 en cambio.",
                    "El pseudo-R\u00b2 de McFadden compara la log-verosimilitud del modelo ajustado contra un modelo solo con intercepto; valores de .20-.40 suelen describirse como un buen ajuste, pero esta regla es mucho menos establecida que las convenciones de R\u00b2 en regresión lineal.",
                    "El pseudo-R\u00b2 de Nagelkerke reescala una medida relacionada para que pueda llegar a 1.0, haciéndolo más directamente comparable entre modelos, pero no está en la misma escala numérica que un R\u00b2 de regresión lineal y no debería interpretarse como \"porcentaje de varianza explicada\" en ese sentido."
                )
            )))

            ll_model <- as.numeric(stats::logLik(model))
            ll_null <- if (!is.null(null_model)) as.numeric(stats::logLik(null_model)) else NA_real_

            mcfadden_r2 <- if (!is.na(ll_null)) 1 - (ll_model / ll_null) else NA_real_
            nagelkerke_r2 <- if (!is.na(ll_null)) {
                num <- 1 - exp((2 / n_complete) * (ll_null - ll_model))
                den <- 1 - exp((2 / n_complete) * ll_null)
                num / den
            } else NA_real_

            add_row(self$results$discrimination, "disc_1", list(metric = "McFadden R\u00b2", value = mcfadden_r2))
            add_row(self$results$discrimination, "disc_2", list(metric = "Nagelkerke R\u00b2", value = nagelkerke_r2))

            self$results$discriminationInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (is.na(mcfadden_r2)) {
                    tr("Pseudo-R\u00b2 could not be computed (the null model failed to converge).",
                       "El pseudo-R\u00b2 no pudo calcularse (el modelo nulo no convergió).")
                } else {
                    tr(
                        paste0("McFadden R\u00b2 = ", fmt_num(mcfadden_r2, 3), "; Nagelkerke R\u00b2 = ", fmt_num(nagelkerke_r2, 3), "."),
                        paste0("McFadden R\u00b2 = ", fmt_num(mcfadden_r2, 3), "; Nagelkerke R\u00b2 = ", fmt_num(nagelkerke_r2, 3), ".")
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Multicollinearity: manual VIF/tolerance from the predictors'
            # own design matrix - identical computation to logCheck's/
            # regCheck's block (a property of the predictors alone, entirely
            # independent of the response family, so it needs no adaptation
            # here beyond the predictor list itself).
            # ES: Multicolinealidad: VIF/tolerancia manual desde la propia
            # matriz de diseño de los predictores - cálculo idéntico al
            # bloque de logCheck/regCheck (una propiedad solo de los
            # predictores, totalmente independiente de la familia del
            # desenlace, así que no necesita adaptación aquí más allá de la
            # lista de predictores misma).
            # -----------------------------------------------------------------------------
            self$results$multicollinearityGuide$setContent(html_guide(tr("Multicollinearity", "Multicolinealidad"), tr(
                c(
                    "Multicollinearity evaluates whether predictors carry very redundant information.",
                    "VIF values close to 1 suggest low collinearity; high values indicate a predictor is too well explained by other predictors.",
                    "If meaningful collinearity is present, consider combining variables, removing redundant predictors, centering variables, or changing the analytic question."
                ),
                c(
                    "La multicolinealidad evalúa si los predictores contienen información muy redundante.",
                    "VIF cercanos a 1 sugieren baja colinealidad; valores altos indican que un predictor se explica demasiado bien por otros predictores.",
                    "Si hay colinealidad importante, considere combinar variables, eliminar predictores redundantes, centrar variables, o cambiar la pregunta analítica."
                )
            )))

            mc_visible <- length(predictors) >= 2
            self$results$multicollinearityGuide$setVisible(mc_visible)
            self$results$multicollinearity$setVisible(mc_visible)
            self$results$multicollinearityInterpretation$setVisible(mc_visible)

            max_vif <- NA_real_
            max_vif_name <- NA_character_

            if (mc_visible) {
                multi_i <- 0
                add_multi <- function(diagnostic, item, statistic, value) {
                    multi_i <<- multi_i + 1
                    add_row(self$results$multicollinearity, paste0("multi_", multi_i), list(
                        diagnostic = diagnostic, item = item, statistic = statistic, value = value
                    ))
                }

                design_formula <- stats::as.formula(paste("~", paste(qname(predictors), collapse = " + "), "- 1"))
                design_mat <- tryCatch(stats::model.matrix(design_formula, data = model_data_complete), error = function(e) NULL)

                if (!is.null(design_mat) && ncol(design_mat) >= 2) {
                    for (pName in colnames(design_mat)) {
                        target <- design_mat[, pName]
                        others <- design_mat[, setdiff(colnames(design_mat), pName), drop = FALSE]
                        pClean <- gsub("^`|`$", "", pName)

                        vif <- tryCatch({
                            vifAuxFit <- stats::lm(target ~ others)
                            1 / (1 - summary(vifAuxFit)$r.squared)
                        }, error = function(e) NA_real_)
                        tol <- if (is.na(vif)) NA_real_ else 1 / vif

                        add_multi("VIF", pClean, "VIF", vif)
                        add_multi(tr("Tolerance", "Tolerancia"), pClean, "1/VIF", tol)

                        if (!is.na(vif) && (is.na(max_vif) || vif > max_vif)) {
                            max_vif <- vif
                            max_vif_name <- pClean
                        }
                    }

                    eig <- tryCatch(eigen(stats::cor(design_mat), only.values = TRUE)$values, error = function(e) NA_real_)
                    min_eig <- if (all(is.na(eig))) NA_real_ else min(eig, na.rm = TRUE)
                    max_eig <- if (all(is.na(eig))) NA_real_ else max(eig, na.rm = TRUE)
                    cond_index <- if (!is.na(min_eig) && min_eig > 0) sqrt(max_eig / min_eig) else NA_real_
                    det_r <- tryCatch(det(stats::cor(design_mat)), error = function(e) NA_real_)

                    add_multi(tr("Minimum eigenvalue", "Eigenvalue mínimo"), tr("Design matrix", "Matriz de diseño"), "\u03bb min", min_eig)
                    add_multi(tr("Condition index", "Índice de condición"), tr("Design matrix", "Matriz de diseño"), "CI", cond_index)
                    add_multi(tr("Determinant", "Determinante"), tr("Correlation matrix", "Matriz de correlación"), "det(R)", det_r)
                } else {
                    add_multi(tr("VIF / tolerance", "VIF / tolerancia"), tr("Not applicable", "No aplicable"), "", NA_real_)
                }

                self$results$multicollinearityInterpretation$setContent(html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    if (!is.na(max_vif)) {
                        tr(
                            paste0("Maximum VIF: ", fmt_num(max_vif, 2), " (predictor: ", max_vif_name, "). Values between 5 and 10 raise a moderate concern, and values above 10 are considered a severe problem (Marquardt, 1970)."),
                            paste0("VIF máximo: ", fmt_num(max_vif, 2), " (predictor: ", max_vif_name, "). Valores entre 5 y 10 encienden una alerta moderada, y valores por encima de 10 se consideran un problema severo (Marquardt, 1970).")
                        )
                    } else {
                        tr("Multicollinearity could not be assessed for this predictor set.",
                           "No se pudo evaluar la multicolinealidad para este conjunto de predictores.")
                    },
                    paragraphs = TRUE
                ))
            }

            # -----------------------------------------------------------------------------
            # Correlation matrices (Pearson / dCor / copula entropy) -
            # identical machinery to logCheck's block; the dependent
            # variable enters via its integer category codes (as.integer())
            # as a rough interval approximation, not a formal
            # rank/polyserial correlation - flagged explicitly in the note
            # below so it is not over-read as more precise than it is.
            # ES: Matrices de correlación (Pearson / dCor / entropía
            # copular) - maquinaria idéntica al bloque de logCheck; la
            # variable dependiente entra vía sus códigos enteros de
            # categoría (as.integer()) como una aproximación de intervalo
            # aproximada, no una correlación de rango/poliserial formal -
            # señalado explícitamente en la nota de abajo para que no se
            # sobre-interprete como más precisa de lo que es.
            # -----------------------------------------------------------------------------
            matVars <- c(dep_var, covs)
            k <- length(matVars)

            corrData <- model_data_complete
            corrData[[dep_var]] <- as.integer(corrData$dep_ord)

            self$results$correlationMatrixGuide$setContent(html_guide(tr("Correlation Matrix", "Matriz de Correlaciones"), tr(
                c(
                    "These two tables complement the multicollinearity checks with an overview of the association between the dependent variable and all numeric predictors, each in APA 7 format (lower triangle, numbered variables).",
                    "The first reports conventional Pearson correlation (linear association only); the second reports distance correlation (Sz\u00e9kely et al., 2007), which detects linear and non-linear association alike.",
                    "The dependent variable enters this matrix as its integer category code (1, 2, 3, ...), a rough interval approximation - not a formal rank or polyserial correlation - so read its row/column as an approximate signal, not a precise effect size."
                ),
                c(
                    "Estas dos tablas complementan las pruebas de multicolinealidad con una vista general de la asociación entre la variable dependiente y todos los predictores numéricos, cada una en formato APA 7 (triángulo inferior, variables numeradas).",
                    "La primera reporta la correlación de Pearson convencional (solo detecta asociación lineal); la segunda reporta la correlación de distancia (Sz\u00e9kely et al., 2007), que detecta asociación lineal y no lineal por igual.",
                    "La variable dependiente entra en esta matriz como su código entero de categoría (1, 2, 3, ...), una aproximación de intervalo aproximada - no una correlación de rango o poliserial formal - así que lea su fila/columna como una señal aproximada, no un tamaño de efecto preciso."
                )
            )))

            self$results$correlationMatrixGuide$setVisible(k >= 2)
            self$results$pearsonMatrixTable$setVisible(k >= 2)
            self$results$dcorMatrixTable$setVisible(k >= 2)
            self$results$correlationMatrixNote$setVisible(k >= 2)
            self$results$correlationComparisonGuide$setVisible(k >= 2)
            self$results$correlationComparisonTable$setVisible(k >= 2)
            self$results$correlationComparisonInterpretation$setVisible(k >= 2)

            if (k >= 2) {
                pairResults <- list()
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
                    add_row(self$results$pearsonMatrixTable, paste0("pm_", i), pearsonVals)
                    add_row(self$results$dcorMatrixTable, paste0("dm_", i), dcorVals)
                }

                self$results$correlationMatrixNote$setContent(html_block(NULL, paste0(
                    tr("N = ", "N = "), n_complete, ". ",
                    tr("* p < .05, ** p < .01, *** p < .001. ", "* p < .05, ** p < .01, *** p < .001. "),
                    .al_dcor_na_note(lang)
                ), paragraphs = FALSE))

                notable_pairs <- 0
                for (key in names(pairResults)) {
                    pr <- pairResults[[key]]
                    if (is.na(pr$dcor) || is.na(pr$pearsonR)) next()
                    gap <- pr$dcor - abs(pr$pearsonR)
                    if (!is.na(gap) && gap > 0.10) {
                        notable_pairs <- notable_pairs + 1
                        add_row(self$results$correlationComparisonTable, paste0("cc_", notable_pairs), list(
                            var1 = pr$v1, var2 = pr$v2,
                            pearson = fmt_num(pr$pearsonR, 3), dcor = fmt_num(pr$dcor, 3),
                            gap = gap, ce = pr$ce, ceP = pr$ceP, ceSig = p_sig(pr$ceP),
                            flag = tr("Yes", "Sí")
                        ))
                    }
                }

                self$results$correlationComparisonGuide$setContent(html_block(
                    tr("Pearson / dCor / Copula Entropy Discordance Analysis", "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                    .al_html_list(tr(c(
                        "Because Pearson's r only captures linear association while dCor captures both linear and non-linear association, a pair whose dCor is notably larger than its Pearson |r| is a signal (not proof) of a non-linear relationship.",
                        "Pairs are flagged in the table below when the gap (dCor minus |Pearson r|) is greater than .10. The copula entropy (CE, copent()) result for the same pair is shown alongside as a second, distribution-free line of evidence.",
                        .al_permutation_note(lang, 199, 20260704)
                    ), c(
                        "Dado que la r de Pearson solo capta asociación lineal mientras que dCor capta asociación lineal y no lineal por igual, un par cuyo dCor sea notablemente mayor que su |r| de Pearson es una señal (no una prueba) de una relación no lineal.",
                        "Se señalan en la tabla de abajo los pares con una brecha (dCor menos |r| de Pearson) mayor a .10. El resultado de la prueba de entropía copular (CE, copent()) para el mismo par se muestra al lado como una segunda línea de evidencia libre de supuestos distribucionales.",
                        .al_permutation_note(lang, 199, 20260704)
                    ))),
                    raw = TRUE
                ))

                self$results$correlationComparisonInterpretation$setContent(html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    if (notable_pairs == 0) {
                        tr("No pair shows a Pearson/dCor gap greater than 0.10; there is no indication of unmodeled non-linear association among the model's numeric variables.",
                           "Ningún par muestra una brecha Pearson/dCor mayor a 0.10; no hay indicios de asociación no lineal no modelada entre las variables numéricas del modelo.")
                    } else {
                        tr(paste0(notable_pairs, " pair(s) show a notable Pearson/dCor gap; inspect a scatterplot before concluding the relationship is non-linear."),
                           paste0(notable_pairs, " par(es) muestran una brecha Pearson/dCor notable; revise un diagrama de dispersión antes de concluir que la relación es no lineal."))
                    },
                    paragraphs = TRUE
                ))
            }

            # -----------------------------------------------------------------------------
            # Influential cases: per-case Pearson-type residual on the
            # observed category's fitted probability (see file header for
            # the rationale and its limits).
            # ES: Casos influyentes: residuo tipo Pearson por caso sobre la
            # probabilidad ajustada de la categoría observada (ver el
            # encabezado del archivo para el razonamiento y sus límites).
            # -----------------------------------------------------------------------------
            self$results$influenceGuide$setContent(html_guide(tr("Influential cases", "Casos influyentes"), tr(
                c(
                    "polr() does not expose Cook's D or leverage the way a linear or binary logistic model does, so influence here is screened with a simpler, case-level Pearson-type residual instead: how far the model's fitted probability for the case's OWN observed category falls from a perfect fit.",
                    "Cases are flagged at |residual| > 2.5, the same convention used elsewhere in this suite for standardized-residual screening.",
                    "This is a reasonable approximation, not a single canonical published statistic for this model family - for a fuller graphical treatment, see Liu et al. (2009).",
                    "A flagged case should not be removed automatically; check whether it is a recording error, a valid but extreme case, or a sign the model does not represent all subgroups well."
                ),
                c(
                    "polr() no expone Cook's D ni leverage como sí lo hace un modelo lineal o logístico binario, así que aquí la influencia se criba con un residuo tipo Pearson por caso más simple en cambio: qué tan lejos está la probabilidad ajustada del modelo para la categoría REALMENTE OBSERVADA del caso de un ajuste perfecto.",
                    "Los casos se marcan en |residuo| > 2.5, la misma convención usada en otra parte de esta suite para el cribado de residuos estandarizados.",
                    "Esta es una aproximación razonable, no un único estadístico canónico publicado para esta familia de modelos - para un tratamiento gráfico más completo, ver Liu et al. (2009).",
                    "Un caso marcado no debe eliminarse automáticamente; revise si es un error de registro, un caso válido pero extremo, o una señal de que el modelo no representa bien a todos los subgrupos."
                )
            )))

            fitted_probs <- tryCatch(stats::predict(model, type = "probs"), error = function(e) NULL)

            n_influential <- 0
            if (!is.null(fitted_probs)) {
                obs_idx <- as.integer(model_data_complete$dep_ord)
                p_obs <- vapply(seq_len(n_complete), function(i) {
                    row <- fitted_probs[i, ]
                    row[obs_idx[i]]
                }, numeric(1))

                p_obs <- pmin(pmax(p_obs, 1e-6), 1 - 1e-6)
                pearson_resid <- (1 - p_obs) / sqrt(p_obs * (1 - p_obs))
                resid_p <- 2 * stats::pnorm(abs(pearson_resid), lower.tail = FALSE)

                threshold <- 2.5
                flagged <- which(abs(pearson_resid) > threshold)
                for (i in flagged) {
                    n_influential <- n_influential + 1
                    add_row(self$results$influence, paste0("case_", i), list(
                        case = i,
                        observed = as.character(model_data_complete$dep_ord[i]),
                        pearsonStat = pearson_resid[i],
                        p = resid_p[i]
                    ))
                }
            }

            pct_influential <- if (n_complete > 0) 100 * n_influential / n_complete else NA_real_

            self$results$influenceInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (is.null(fitted_probs)) {
                    tr("Fitted probabilities could not be computed for this model.",
                       "No se pudieron calcular las probabilidades ajustadas para este modelo.")
                } else if (n_influential == 0) {
                    tr("No cases exceed the |residual| > 2.5 screening threshold.",
                       "Ningún caso supera el umbral de cribado |residuo| > 2.5.")
                } else {
                    tr(
                        paste0(n_influential, " of ", n_complete, " cases (", fmt_num(pct_influential, 1), "%) exceed the |residual| > 2.5 screening threshold."),
                        paste0(n_influential, " de ", n_complete, " casos (", fmt_num(pct_influential, 1), "%) superan el umbral de cribado |residuo| > 2.5.")
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Coefficients / odds ratios (single OR per predictor, since
            # under proportional odds the coefficient is constant across
            # cutpoints) and cutpoints table.
            # ES: Coeficientes / odds ratios (un solo OR por predictor, ya
            # que bajo momios proporcionales el coeficiente es constante
            # entre puntos de corte) y tabla de puntos de corte.
            # -----------------------------------------------------------------------------
            self$results$coefficientsGuide$setContent(html_guide(
                tr("Coefficients and odds ratios (proportional odds)", "Coeficientes y odds ratios (momios proporcionales)"),
                tr(
                    c(
                        "Under proportional odds, each predictor has a single odds ratio that applies to every cutpoint: it is the multiplicative change in the odds of being at or above any given category, for a one-unit increase in the predictor (or for belonging to a category versus the reference category).",
                        "Read the confidence interval, not only the point estimate: if it includes 1, the effect is not statistically distinguishable from no association.",
                        "This single-OR interpretation is only valid to the extent the proportional-odds assumption holds - if the Brant test above flags a predictor, its odds ratio here should not be read as constant across cutpoints."
                    ),
                    c(
                        "Bajo momios proporcionales, cada predictor tiene un único odds ratio que aplica a todos los puntos de corte: es el cambio multiplicativo en los momios de estar en o por encima de cualquier categoría dada, por un aumento de una unidad en el predictor (o por pertenecer a una categoría frente a la de referencia).",
                        "Lea el intervalo de confianza, no solo el estimador puntual: si incluye 1, el efecto no es estadísticamente distinguible de la ausencia de asociación.",
                        "Esta interpretación de OR único solo es válida en la medida en que se sostenga el supuesto de momios proporcionales - si la prueba de Brant de arriba marca un predictor, su odds ratio aquí no debería leerse como constante entre puntos de corte."
                    )
                )
            ))

            ors <- exp(coef(model))
            ci <- tryCatch(exp(stats::confint(model)), error = function(e) {
                matrix(NA_real_, nrow = length(ors), ncol = 2)
            })
            if (is.null(dim(ci))) ci <- matrix(ci, nrow = 1)

            for (i in seq_along(ors)) {
                p_val <- if (i <= length(slope_p)) slope_p[i] else NA_real_
                add_row(self$results$coefficients, paste0("coef_", i), list(
                    predictor = names(ors)[i],
                    or = ors[i],
                    ciLower = ci[i, 1],
                    ciUpper = ci[i, 2],
                    p = p_val,
                    pSig = p_sig(p_val)
                ))
            }

            for (i in seq_len(n_cut)) {
                add_row(self$results$cutpoints, paste0("cut_", i), list(
                    cutpoint = rownames(cut_table)[i],
                    estimate = cut_table[i, "Value"],
                    se = cut_table[i, "Std. Error"]
                ))
            }

            self$results$coefficientsInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (length(ors) == 0) {
                    tr("No predictor coefficients were estimated.", "No se estimaron coeficientes de predictores.")
                } else {
                    strongest_idx <- which.max(abs(log(ors)))
                    tr(
                        paste0("Strongest association: ", names(ors)[strongest_idx], ", OR = ", fmt_num(ors[strongest_idx], 2), "."),
                        paste0("Asociación más fuerte: ", names(ors)[strongest_idx], ", OR = ", fmt_num(ors[strongest_idx], 2), ".")
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Notes and recommendation.
            # ES: Notas y recomendación.
            # -----------------------------------------------------------------------------
            recommendations <- c()
            if (!is.na(pom_omnibus_p) && pom_omnibus_p < 0.05) {
                recommendations <- c(recommendations, tr(
                    "The proportional-odds assumption is not well supported; consider a partial-proportional-odds or multinomial model.",
                    "El supuesto de momios proporcionales no está bien respaldado; considere un modelo de momios parcialmente proporcionales o multinomial."
                ))
            }
            if (bt_significant > 0) {
                recommendations <- c(recommendations, tr(
                    "At least one numeric predictor shows a non-linear relationship with the cumulative logit; consider a transformation or categorization.",
                    "Al menos un predictor numérico muestra una relación no lineal con el logit acumulativo; considere una transformación o categorización."
                ))
            }
            if (n_influential > 0) {
                recommendations <- c(recommendations, tr(
                    "There are cases with a large Pearson-type residual; inspect them before relying on the model's predictions.",
                    "Hay casos con un residuo tipo Pearson grande; inspecciónelos antes de confiar en las predicciones del modelo."
                ))
            }

            self$results$notes$setContent(html_block(NULL, c(
                paste0(
                    tr("Suggested decision: ", "Decisión sugerida: "),
                    if (length(recommendations) > 0) paste(recommendations, collapse = " ") else tr("No major assumption concerns were flagged; the model's coefficients appear reasonably defensible.", "No se marcaron preocupaciones importantes de supuestos; los coeficientes del modelo parecen razonablemente defendibles.")
                ),
                paste0(tr("Model fitted with ", "Modelo ajustado con "), n_complete, tr(" cases.", " casos."))
            ), paragraphs = TRUE))

            # -----------------------------------------------------------------------------
            # State for the plots.
            # ES: Estado para los gráficos.
            # -----------------------------------------------------------------------------
            mid_cut <- ceiling(n_levels / 2)
            dep_binary_mid <- as.integer(as.integer(model_data_complete$dep_ord) > mid_cut)

            plot_state <- list(
                reportLang = lang,
                model_data = model_data_complete,
                dep_binary_mid = dep_binary_mid,
                covs = covs,
                n = n_complete,
                p = n_model_params,
                pearson_resid = if (!is.null(fitted_probs)) pearson_resid else rep(NA_real_, n_complete),
                obs_category = as.character(model_data_complete$dep_ord)
            )

            self$results$linearityPlot$setState(plot_state)
            self$results$influencePlot$setState(plot_state)

            lin_show <- tryCatch(isTRUE(self$options$linShowPlots), error = function(e) TRUE)
            infl_show <- tryCatch(isTRUE(self$options$influenceShowPlots), error = function(e) TRUE)
            show_plots_master <- tryCatch(isTRUE(self$options$showPlots), error = function(e) TRUE)

            self$results$linearityPlot$setVisible(show_plots_master && lin_show && length(covs) > 0)
            self$results$influencePlot$setVisible(show_plots_master && infl_show)
        },

        # -----------------------------------------------------------------------------
        # Plot helpers (identical pattern to logCheck: .emptyLogPlot,
        # .plotTr, .plotStyle, .plotPalette, .plotTheme).
        # ES: Helpers de gráficos (patrón idéntico a logCheck).
        # -----------------------------------------------------------------------------
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

        .plotStyle = function() {
            style <- tryCatch(self$options$plotStyle, error = function(e) "clean")
            if (is.null(style) || length(style) == 0 || !nzchar(style)) style <- "clean"
            style
        },

        .plotPalette = function() {
            style <- private$.plotStyle()
            base <- .al_plot_palette_base(style)

            palette_choice <- tryCatch(self$options$plotPalette, error = function(e) "blueOrange")
            if (is.null(palette_choice) || length(palette_choice) == 0 || !nzchar(palette_choice))
                palette_choice <- "blueOrange"

            base$series <- .al_plot_series_palette(palette_choice)
            base
        },

        .plotTheme = function() {
            style <- private$.plotStyle()
            base <- if (identical(style, "bw")) {
                ggplot2::theme_bw(base_size = 10.5)
            } else if (identical(style, "contrast")) {
                ggplot2::theme_classic(base_size = 10.5)
            } else {
                ggplot2::theme_minimal(base_size = 10.5)
            }
            base + ggplot2::theme(
                legend.position = "bottom",
                panel.grid.minor = ggplot2::element_blank(),
                plot.margin = ggplot2::margin(4, 6, 4, 6)
            )
        },

        # -----------------------------------------------------------------------------
        # Empirical logit vs predictor, at the model's midpoint cutpoint
        # (the closest ordinal analogue of logCheck's binary empirical
        # logit): adapted from logCheck's .plotLinearity by substituting
        # dep_binary (the single binary outcome) for dep_binary_mid (the
        # ordinal outcome collapsed at its median cutpoint) - everything
        # else is unchanged.
        # ES: Logit empírico vs predictor, en el punto de corte medio del
        # modelo (el análogo ordinal más cercano al logit empírico binario
        # de logCheck): adaptado del .plotLinearity de logCheck sustituyendo
        # dep_binary (el único desenlace binario) por dep_binary_mid (el
        # desenlace ordinal colapsado en su punto de corte mediano) - todo
        # lo demás queda igual.
        # -----------------------------------------------------------------------------
        .plotLinearity = function(image, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)

            tr_p <- function(en, es) private$.plotTr(en, es, image)
            pal <- private$.plotPalette()
            covs <- st$covs

            if (length(covs) == 0) {
                return(private$.emptyLogPlot(tr_p(
                    "No numeric predictors to plot.", "No hay predictores numéricos para graficar."
                )))
            }

            bins <- tryCatch(as.integer(self$options$linBins), error = function(e) 8L)
            if (is.null(bins) || is.na(bins) || bins < 4) bins <- 8L

            emp_logit_df <- function(x, y, bins = 8) {
                ok <- !is.na(x) & !is.na(y)
                x <- x[ok]; y <- y[ok]
                if (length(unique(x)) < 3) return(NULL)

                probs <- seq(0, 1, length.out = bins + 1)
                qs <- unique(stats::quantile(x, probs = probs, na.rm = TRUE))
                if (length(qs) < 3) return(NULL)

                grp <- cut(x, breaks = qs, include.lowest = TRUE)
                mean_x <- tapply(x, grp, mean)
                n_grp <- tapply(y, grp, length)
                events_grp <- tapply(y, grp, sum)
                p_hat <- (events_grp + 0.5) / (n_grp + 1)
                logit_hat <- log(p_hat / (1 - p_hat))

                data.frame(mean_x = as.numeric(mean_x), logit = as.numeric(logit_hat), n = as.numeric(n_grp))
            }

            plot_rows <- list()
            for (predictor in covs) {
                x <- st$model_data[[predictor]]
                df <- emp_logit_df(x, st$dep_binary_mid, bins = bins)
                if (!is.null(df)) {
                    df$predictor <- predictor
                    plot_rows[[predictor]] <- df
                }
            }

            if (length(plot_rows) == 0) {
                return(private$.emptyLogPlot(tr_p(
                    "Not enough variation to plot linearity in the cumulative logit.",
                    "No hay suficiente variación para graficar la linealidad en el logit acumulativo."
                )))
            }

            plot_df <- do.call(rbind, plot_rows)
            predictors_unique <- unique(as.character(plot_df$predictor))
            series_colors <- rep(pal$series, length.out = max(1, length(predictors_unique)))
            series_colors <- stats::setNames(series_colors, predictors_unique)

            smoother <- tryCatch(self$options$linSmoother, error = function(e) "none")

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = mean_x, y = logit))

            if (identical(smoother, "linear")) {
                p <- p + ggplot2::geom_smooth(
                    ggplot2::aes(color = predictor), method = "lm", se = FALSE,
                    linetype = "dashed", linewidth = 0.5, show.legend = FALSE
                )
            } else if (identical(smoother, "loess")) {
                p <- p + ggplot2::geom_smooth(
                    ggplot2::aes(color = predictor), method = "loess", se = FALSE,
                    linewidth = 0.5, show.legend = FALSE
                )
            }

            p <- p +
                ggplot2::geom_point(ggplot2::aes(size = n, color = predictor), show.legend = c(size = TRUE, color = FALSE)) +
                ggplot2::scale_color_manual(values = series_colors) +
                ggplot2::facet_wrap(~predictor, scales = "free_x") +
                ggplot2::labs(
                    x = tr_p("Predictor (binned mean)", "Predictor (media por grupo)"),
                    y = tr_p("Empirical cumulative logit (median cutpoint)", "Logit acumulativo empírico (punto de corte mediano)"),
                    size = tr_p("Group n", "n del grupo")
                ) +
                private$.plotTheme()

            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Pearson-type residual by case.
        # ES: Residuo tipo Pearson por caso.
        # -----------------------------------------------------------------------------
        .plotInfluence = function(image, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)

            tr_p <- function(en, es) private$.plotTr(en, es, image)
            pal <- private$.plotPalette()

            resid_vals <- st$pearson_resid
            n <- st$n
            threshold <- 2.5

            if (all(is.na(resid_vals))) {
                return(private$.emptyLogPlot(tr_p(
                    "Residuals could not be computed for this model.",
                    "No se pudieron calcular los residuos para este modelo."
                )))
            }

            show_threshold <- tryCatch(isTRUE(self$options$influenceShowThreshold), error = function(e) TRUE)
            label_mode <- tryCatch(self$options$influenceLabelMode, error = function(e) "top5")

            plot_df <- data.frame(case = seq_along(resid_vals), resid = as.numeric(resid_vals))
            plot_df$flag <- abs(plot_df$resid) > threshold

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = case, y = resid))

            if (show_threshold) {
                p <- p +
                    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", linewidth = 0.5, color = pal$ref) +
                    ggplot2::geom_hline(yintercept = -threshold, linetype = "dashed", linewidth = 0.5, color = pal$ref)
            }

            p <- p +
                ggplot2::geom_segment(ggplot2::aes(xend = case, y = 0, yend = resid), color = pal$point, linewidth = 0.3) +
                ggplot2::geom_point(ggplot2::aes(color = flag), size = 1.6, show.legend = FALSE) +
                ggplot2::scale_color_manual(values = c(`FALSE` = pal$point, `TRUE` = pal$alert))

            lab <- switch(label_mode,
                studres = plot_df[abs(plot_df$resid) > threshold, , drop = FALSE],
                top5 = plot_df[order(-abs(plot_df$resid)), , drop = FALSE][seq_len(min(5, nrow(plot_df))), , drop = FALSE],
                NULL
            )

            if (!is.null(lab) && nrow(lab) > 0) {
                p <- p + ggplot2::geom_text(
                    data = lab, ggplot2::aes(x = case, y = resid, label = case),
                    vjust = -0.6, size = 3, check_overlap = TRUE, color = pal$alert
                )
            }

            p <- p +
                ggplot2::labs(x = tr_p("Case", "Caso"), y = tr_p("Pearson-type residual", "Residuo tipo Pearson")) +
                private$.plotTheme()

            print(p)
            TRUE
        }
    )
)
