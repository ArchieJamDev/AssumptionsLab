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

            lang <- .al_normalize_lang(self$options$reportLang)

            txt <- function(section, key) {
                .al_text(lang, section, key)
            }

            txt_block <- function(section, key) {
                .al_text_block(lang, section, key)
            }

            html_escape <- .al_html_escape

            html_block <- function(title = NULL, text, paragraphs = TRUE, raw = FALSE) {
                .al_html_block(title, text, paragraphs = paragraphs, raw = raw)
            }

            html_guide <- function(title, section, key) {
                html_block(title, .al_html_list(.al_text(lang, section, key)), raw = TRUE)
            }

            plot_guide <- function(title, objective, x_axis, y_axis,
                                   expected, warning, decision) {

                lab <- function(en, es) {
                    if (identical(lang, "es"))
                        es
                    else
                        en
                }

                pg_tr <- function(x) {
                    x <- as.character(x)

                    if (identical(lang, "es"))
                        return(x)

                    map <- c(
                        "Guía: Residuos vs valores ajustados" =
                            "Guide: Residuals vs fitted values",
                        "Guía: Valores observados vs predichos" =
                            "Guide: Observed vs predicted values",
                        "Guía: Efectos visuales de predictores numéricos" =
                            "Guide: Visual effects of numeric predictors",
                        "Guía: Q-Q plot de residuos" =
                            "Guide: Residual Q-Q plot",
                        "Guía: Histograma de residuos" =
                            "Guide: Residual histogram",
                        "Guía: Distribución observada vs normal teórica" =
                            "Guide: Observed distribution vs theoretical normal curve",
                        "Guía: Scale-location plot" =
                            "Guide: Scale-location plot",
                        "Guía: Boxplots de variables numéricas" =
                            "Guide: Numeric variable boxplots",
                        "Guía: Residuos studentizados vs leverage" =
                            "Guide: Studentized residuals vs leverage",
                        "Guía: Cook's D por caso" =
                            "Guide: Cook's D by case",

                        "evaluar linealidad y varianza residual constante." =
                            "evaluate linearity and constant residual variance.",
                        "valores ajustados o predichos por el modelo para cada caso." =
                            "fitted or predicted values from the model for each case.",
                        "residuos, es decir, diferencia entre valor observado y valor predicho." =
                            "residuals, that is, the difference between observed and predicted values.",
                        "nube aleatoria alrededor de cero, sin forma curva ni embudo." =
                            "a random cloud around zero, without curvature or funnel shape.",
                        "curvatura, bandas, embudo o grupos separados." =
                            "curvature, bands, funnel shape, or separated groups.",
                        "revisar términos no lineales, transformación o errores estándar robustos." =
                            "review nonlinear terms, transformation, or robust standard errors.",

                        "evaluar la correspondencia entre valores reales y predicciones del modelo." =
                            "evaluate the correspondence between observed values and model predictions.",
                        "valores predichos por el modelo." =
                            "values predicted by the model.",
                        "valores observados de la variable dependiente." =
                            "observed values of the dependent variable.",
                        "puntos cercanos a la línea diagonal de predicción perfecta." =
                            "points close to the diagonal line of perfect prediction.",
                        "dispersión amplia, patrones curvos o sesgo sistemático." =
                            "wide dispersion, curved patterns, or systematic bias.",
                        "revisar capacidad predictiva, especificación del modelo y posibles términos adicionales." =
                            "review predictive ability, model specification, and possible additional terms.",

                        "comparar la dirección e intensidad relativa de los predictores numéricos." =
                            "compare the direction and relative strength of numeric predictors.",
                        "valor estandarizado del predictor evaluado, de bajo a alto." =
                            "standardized value of the evaluated predictor, from low to high.",
                        "valor predicho de la variable dependiente manteniendo constantes los demás predictores." =
                            "predicted dependent-variable value while holding the other predictors constant.",
                        "líneas aproximadamente rectas y pendientes coherentes con el modelo." =
                            "approximately straight lines with slopes consistent with the model.",
                        "pendientes muy distintas, líneas planas o efectos difíciles de interpretar." =
                            "very different slopes, flat lines, or effects that are difficult to interpret.",
                        "revisar tamaño y dirección de efectos, linealidad, interacciones o términos no lineales." =
                            "review effect size and direction, linearity, interactions, or nonlinear terms.",

                        "evaluar si los residuos estandarizados se aproximan a una distribución normal." =
                            "evaluate whether standardized residuals approximate a normal distribution.",
                        "cuantiles teóricos esperados bajo normalidad." =
                            "theoretical quantiles expected under normality.",
                        "cuantiles observados de los residuos estandarizados." =
                            "observed quantiles of the standardized residuals.",
                        "puntos cercanos a la línea diagonal." =
                            "points close to the diagonal line.",
                        "desviaciones fuertes en extremos, forma de S o colas pesadas." =
                            "strong deviations at the extremes, S-shape, or heavy tails.",
                        "interpretar inferencias clásicas con cautela y revisar atípicos o transformaciones." =
                            "interpret classical inferences cautiously and review outliers or transformations.",

                        "examinar forma, simetría y concentración de los residuos estandarizados." =
                            "examine the shape, symmetry, and concentration of standardized residuals.",
                        "residuos estandarizados." =
                            "standardized residuals.",
                        "frecuencia o número de casos." =
                            "frequency or number of cases.",
                        "distribución aproximadamente simétrica y centrada en cero." =
                            "an approximately symmetric distribution centered near zero.",
                        "asimetría marcada, múltiples picos o colas muy largas." =
                            "marked skewness, multiple peaks, or very long tails.",
                        "complementar pruebas de normalidad y revisar casos extremos." =
                            "complement normality tests and review extreme cases.",

                        "comparar la distribución observada de los residuos con una normal teórica." =
                            "compare the observed residual distribution with a theoretical normal curve.",
                        "residuos estandarizados del modelo." =
                            "standardized residuals from the model.",
                        "densidad, es decir, concentración relativa de casos." =
                            "density, that is, the relative concentration of cases.",
                        "curva empírica centrada cerca de cero y similar a la campana normal." =
                            "an empirical curve centered near zero and similar to the normal bell curve.",
                        "asimetría, colas pesadas, varios picos o exceso de casos extremos." =
                            "skewness, heavy tails, multiple peaks, or too many extreme cases.",
                        "leer pruebas clásicas con cautela y revisar atípicos, transformación o especificación." =
                            "read classical tests cautiously and review outliers, transformation, or specification.",

                        "evaluar si la dispersión de los residuos se mantiene estable." =
                            "evaluate whether residual dispersion remains stable.",
                        "raíz cuadrada del valor absoluto de los residuos estandarizados." =
                            "square root of the absolute standardized residuals.",
                        "banda horizontal con dispersión similar a lo largo del eje X." =
                            "a horizontal band with similar dispersion across the X axis.",
                        "tendencia ascendente, descendente o forma de embudo." =
                            "an increasing trend, decreasing trend, or funnel shape.",
                        "considerar errores robustos, transformación o modelo alternativo." =
                            "consider robust errors, transformation, or an alternative model.",

                        "detectar valores atípicos univariados en la variable dependiente y predictores numéricos." =
                            "detect univariate outliers in the dependent variable and numeric predictors.",
                        "variables numéricas incluidas en el diagnóstico." =
                            "numeric variables included in the diagnostic.",
                        "valores observados de cada variable en su escala original." =
                            "observed values of each variable on its original scale.",
                        "cajas compactas, bigotes razonables y pocos puntos extremos." =
                            "compact boxes, reasonable whiskers, and few extreme points.",
                        "muchos puntos fuera de bigotes, asimetría fuerte o valores muy alejados." =
                            "many points outside the whiskers, strong skewness, or very distant values.",
                        "revisar datos extremos antes de interpretar influencia multivariada o residuos." =
                            "review extreme values before interpreting multivariate influence or residuals.",

                        "identificar casos con combinación de residuo extremo y alto leverage." =
                            "identify cases combining extreme residuals and high leverage.",
                        "leverage, o distancia del caso respecto al centro del espacio de predictores." =
                            "leverage, or the case distance from the center of the predictor space.",
                        "residuo studentizado, que expresa qué tan extremo es el error del caso." =
                            "studentized residual, which indicates how extreme the case error is.",
                        "la mayoría de casos cerca de cero y con leverage bajo." =
                            "most cases near zero and with low leverage.",
                        "casos con residuo studentizado grande, leverage alto o Cook's D elevado." =
                            "cases with large studentized residuals, high leverage, or elevated Cook's D.",
                        "revisar datos influyentes y comparar análisis de sensibilidad." =
                            "review influential data points and compare sensitivity analyses.",

                        "detectar observaciones que pueden cambiar el ajuste del modelo." =
                            "detect observations that may change model fit.",
                        "número o posición del caso en la base de datos." =
                            "case number or position in the data set.",
                        "distancia de Cook, indicador de influencia global." =
                            "Cook's distance, an indicator of global influence.",
                        "barras pequeñas y por debajo de la línea de referencia." =
                            "small bars below the reference line.",
                        "casos con Cook's D por encima del umbral visual." =
                            "cases with Cook's D above the visual threshold.",
                        "verificar plausibilidad del caso y documentar análisis con/sin casos influyentes." =
                            "check case plausibility and document analyses with and without influential cases.",

                        "Guía: Relación por par (Pearson / dCor)" =
                            "Guide: Pairwise relationship (Pearson / dCor)",
                        "comparar visualmente lo que Pearson r y dCor detectan en la relación entre dos variables." =
                            "visually compare what Pearson r and dCor detect in the relationship between two variables.",
                        "valores de la primera variable del par." =
                            "values of the first variable in the pair.",
                        "valores de la segunda variable del par." =
                            "values of the second variable in the pair.",
                        "puntos alineados en torno a la línea de ajuste lineal cuando r es alto; una curva clara cuando dCor supera notablemente a |r|." =
                            "points aligned around the linear fit line when r is high; a clear curve when dCor notably exceeds |r|.",
                        "el ajuste suavizado se separa marcadamente de la línea lineal, sobre todo en pares señalados en la tabla de discordancia." =
                            "the smoothed fit separates markedly from the linear line, especially in pairs flagged in the discordance table.",
                        "revisar si conviene un término no lineal, una transformación, o simplemente confirmar que la relación es lineal." =
                            "review whether a non-linear term or transformation is warranted, or simply confirm the relationship is linear.",

                        "Guía: Comparación de Pearson, dCor y entropía copular entre pares" =
                            "Guide: Comparison of Pearson, dCor, and copula entropy across pairs",
                        "comparar de un vistazo los tres coeficientes de asociación (Pearson, dCor, entropía copular) entre todos los pares de variables numéricas del modelo." =
                            "compare at a glance the three association coefficients (Pearson, dCor, copula entropy) across all pairs of numeric variables in the model.",
                        "pares de variables numéricas del modelo." =
                            "pairs of numeric variables in the model.",
                        "magnitud del coeficiente (Pearson r, dCor)." =
                            "coefficient magnitude (Pearson r, dCor).",
                        "Pearson y dCor con valores similares para la mayoría de los pares." =
                            "Pearson and dCor with similar values for most pairs.",
                        "una brecha grande entre Pearson y dCor en algún par, especialmente si coincide con entropía copular significativa." =
                            "a large gap between Pearson and dCor for some pair, especially if it coincides with significant copula entropy.",
                        "revisar el gráfico individual de ese par y considerar un término no lineal si la brecha se confirma." =
                            "review that pair's individual chart and consider a non-linear term if the gap is confirmed."
                    )

                    if (x %in% names(map))
                        unname(map[[x]])
                    else
                        x
                }

                html_block(
                    pg_tr(title),
                    .al_html_list(c(
                        paste0(lab("Objective: ", "Objetivo: "), pg_tr(objective)),
                        paste0(lab("X axis: ", "Eje X: "), pg_tr(x_axis)),
                        paste0(lab("Y axis: ", "Eje Y: "), pg_tr(y_axis)),
                        paste0(lab("Expected pattern: ", "Patrón esperado: "), pg_tr(expected)),
                        paste0(lab("Warning sign: ", "Señal de alerta: "), pg_tr(warning)),
                        paste0(lab("Possible decision: ", "Decisión posible: "), pg_tr(decision))
                    )),
                    raw = TRUE
                )
            }
            tr <- function(en, es) .al_tr(lang, en, es)

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
                    c("intro",
                      "AssumptionsLab — Simple & Multiple Regression",
                      "AssumptionsLab — Simple & Multiple Regression"),
                    c("designGuide", "Regression model", "Modelo de regresión"),
                    c("modelSummary", "Model summary", "Resumen del modelo"),
                    c("missingSummary", "Missing data summary", "Resumen de datos faltantes"),

                    c("linearityGuide", "Linearity", "Linealidad"),
                    c("linearityPredictor",
                      "Linearity by numeric predictor",
                      "Linealidad por predictor numérico"),
                    c("linearityModel",
                      "Global model linearity",
                      "Linealidad global del modelo"),
                    c("linearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("correlationMatrixGuide", "Correlation matrix", "Matriz de correlaciones"),
                    c("pearsonMatrixTable",
                      "Pearson correlation matrix (APA 7 format)",
                      "Matriz de correlaciones de Pearson (formato APA 7)"),
                    c("dcorMatrixTable",
                      "Distance correlation matrix (dCor, APA 7 format)",
                      "Matriz de correlación de distancia (dCor, formato APA 7)"),
                    c("correlationMatrixNote", " ", " "),
                    c("correlationComparisonGuide",
                      "Pearson / dCor / Copula Entropy Discordance Analysis",
                      "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                    c("correlationComparisonTable",
                      "Pairs with a notable difference between Pearson and dCor",
                      "Pares con diferencia notable entre Pearson y dCor"),
                    c("correlationComparisonInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("correlationIndividualPlot",
                      "Pairwise relationship (Pearson / dCor)",
                      "Relación por par (Pearson / dCor)"),
                    c("correlationIndividualGuide",
                      "Guide: Pairwise relationship (Pearson / dCor)",
                      "Guía: Relación por par (Pearson / dCor)"),
                    c("correlationComparativePlot",
                      "Comparison of Pearson, dCor, and copula entropy across pairs",
                      "Comparación de Pearson, dCor y entropía copular entre pares"),
                    c("correlationComparativeGuide",
                      "Guide: Comparison of Pearson, dCor, and copula entropy",
                      "Guía: Comparación de Pearson, dCor y entropía copular"),

                    c("diagnosticPlotsGuide",
                      "Regression diagnostic plots",
                      "Gráficos diagnósticos de regresión"),
                    c("residualsFittedPlot",
                      "Residuals vs fitted values",
                      "Residuos vs valores ajustados"),
                    c("residualsFittedGuide",
                      "Guide: Residuals vs fitted values",
                      "Guía: Residuos vs valores ajustados"),
                    c("observedPredictedPlot",
                      "Observed vs predicted values",
                      "Valores observados vs predichos"),
                    c("observedPredictedGuide",
                      "Guide: Observed vs predicted values",
                      "Guía: Valores observados vs predichos"),
                    c("predictorEffectsPlot",
                      "Visual effects of numeric predictors on the dependent variable",
                      "Efectos visuales de predictores numéricos sobre la dependiente"),
                    c("predictorEffectsGuide",
                      "Guide: Visual effects of numeric predictors",
                      "Guía: Efectos visuales de predictores numéricos"),

                    c("residualNormalityGuide",
                      "Residual normality",
                      "Normalidad de residuos"),
                    c("residualNormality",
                      "Residual normality",
                      "Normalidad de residuos"),
                    c("residualNormalityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("qqResidualsPlot",
                      "Residual Q-Q plot",
                      "Q-Q plot de residuos"),
                    c("qqResidualsGuide",
                      "Guide: Residual Q-Q plot",
                      "Guía: Q-Q plot de residuos"),
                    c("residualHistogramPlot",
                      "Residual histogram",
                      "Histograma de residuos"),
                    c("residualHistogramGuide",
                      "Guide: Residual histogram",
                      "Guía: Histograma de residuos"),
                    c("residualNormalCurvePlot",
                      "Observed residual distribution vs theoretical normal curve",
                      "Distribución observada de residuos vs curva normal teórica"),
                    c("residualNormalCurveGuide",
                      "Guide: Observed distribution vs theoretical normal curve",
                      "Guía: Distribución observada vs normal teórica"),

                    c("homoscedasticityGuide",
                      "Homoscedasticity and heteroscedasticity",
                      "Homoscedasticidad y heterocedasticidad"),
                    c("homoscedasticity",
                      "Homoscedasticity and heteroscedasticity",
                      "Homoscedasticidad y heterocedasticidad"),
                    c("homoscedasticityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("scaleLocationPlot",
                      "Scale-location plot",
                      "Scale-location plot"),
                    c("scaleLocationGuide",
                      "Guide: Scale-location plot",
                      "Guía: Scale-location plot"),

                    c("independenceGuide",
                      "Error independence",
                      "Independencia de errores"),
                    c("independence",
                      "Error independence",
                      "Independencia de errores"),
                    c("independenceInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("multicollinearityGuide",
                      "Multicollinearity",
                      "Multicolinealidad"),
                    c("multicollinearity",
                      "Multicollinearity",
                      "Multicolinealidad"),
                    c("multicollinearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("influenceGuide",
                      "Outlying and influential cases",
                      "Casos atípicos e influyentes"),
                    c("influence",
                      "Outlying and influential cases",
                      "Casos atípicos e influyentes"),
                    c("influenceCases",
                      "Outlying and influential cases",
                      "Casos atípicos e influyentes"),
                    c("influenceInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("numericBoxplotsPlot",
                      "Boxplots of dependent variable and numeric predictors",
                      "Boxplots de la variable dependiente y predictores numéricos"),
                    c("numericBoxplotsGuide",
                      "Guide: Numeric variable boxplots",
                      "Guía: Boxplots de variables numéricas"),
                    c("residualsLeveragePlot",
                      "Studentized residuals vs leverage",
                      "Residuos studentizados vs leverage"),
                    c("residualsLeverageGuide",
                      "Guide: Studentized residuals vs leverage",
                      "Guía: Residuos studentizados vs leverage"),
                    c("cooksDPlot",
                      "Cook's D by case",
                      "Cook's D por caso"),
                    c("cooksDGuide",
                      "Guide: Cook's D by case",
                      "Guía: Cook's D por caso"),

                    c("transformGuide",
                      "Exploratory transformations",
                      "Transformaciones exploratorias"),
                    c("transformationsGuide",
                      "Exploratory transformations",
                      "Transformaciones exploratorias"),
                    c("transformNormality",
                      "Residual normality by transformation",
                      "Normalidad de residuos por transformación"),
                    c("transformationNormality",
                      "Residual normality by transformation",
                      "Normalidad de residuos por transformación"),
                    c("transformFit",
                      "Residual fit and variance by transformation",
                      "Ajuste y varianza residual por transformación"),
                    c("transformationFit",
                      "Residual fit and variance by transformation",
                      "Ajuste y varianza residual por transformación"),
                    c("robustGuide",
                      "Robust and resampling options",
                      "Opciones robustas y de remuestreo"),
                    c("robustOptionsGuide",
                      "Robust and resampling options",
                      "Opciones robustas y de remuestreo"),
                    c("robustInterpretation",
                      "Methodological interpretation of robust options",
                      "Interpretación metodológica de opciones robustas"),
                    c("robustOptionsText",
                      "Methodological interpretation of robust options",
                      "Interpretación metodológica de opciones robustas"),
                    c("notes",
                      "Notes and recommendation",
                      "Notas y recomendación")
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
                    c("modelSummary", "item", "Item", "Elemento"),
                    c("modelSummary", "value", "Value", "Valor"),
                    c("missingSummary", "item", "Item", "Elemento"),
                    c("missingSummary", "value", "Value", "Valor"),

                    c("linearityPredictor", "predictor", "Predictor", "Predictor"),
                    c("linearityPredictor", "dependent", "Dependent variable", "Variable dependiente"),
                    c("linearityPredictor", "test", "Test / criterion", "Prueba / criterio"),
                    c("linearityPredictor", "statistic", "Statistic", "Estadístico"),
                    c("linearityPredictor", "value", "Value", "Valor"),
                    c("linearityPredictor", "pSig", "Sig.", "Sig."),

                    c("linearityModel", "scope", "Scope", "Alcance"),
                    c("linearityModel", "dependent", "Dependent variable", "Variable dependiente"),
                    c("linearityModel", "test", "Test / criterion", "Prueba / criterio"),
                    c("linearityModel", "statistic", "Statistic", "Estadístico"),
                    c("linearityModel", "value", "Value", "Valor"),
                    c("linearityModel", "pSig", "Sig.", "Sig."),

                    c("residualNormality", "test", "Test", "Prueba"),
                    c("residualNormality", "statistic", "Statistic", "Estadístico"),
                    c("residualNormality", "value", "Value", "Valor"),
                    c("residualNormality", "pSig", "Sig.", "Sig."),

                    c("homoscedasticity", "family", "Family", "Familia"),
                    c("homoscedasticity", "test", "Test", "Prueba"),
                    c("homoscedasticity", "statistic", "Statistic", "Estadístico"),
                    c("homoscedasticity", "value", "Value", "Valor"),
                    c("homoscedasticity", "pSig", "Sig.", "Sig."),

                    c("independence", "test", "Test", "Prueba"),
                    c("independence", "statistic", "Statistic", "Estadístico"),
                    c("independence", "value", "Value", "Valor"),
                    c("independence", "pSig", "Sig.", "Sig."),

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
                    c("influence", "fitted", "Fitted", "Ajustado"),
                    c("influence", "residual", "Residual", "Residuo"),
                    c("influence", "studResidual", "Studentized residual", "Residuo studentizado"),
                    c("influence", "leverage", "Leverage", "Leverage"),
                    c("influence", "cooksD", "Cook's D", "Cook's D"),
                    c("influence", "dffits", "DFFITS", "DFFITS"),
                    c("influence", "criteria", "Triggered criteria", "Criterios activados"),

                    c("transformationNormality", "criterion", "Criterion / test", "Criterio / prueba"),
                    c("transformationNormality", "original", "Original", "Original"),
                    c("transformationNormality", "logTrans", "log(Y)", "log(Y)"),
                    c("transformationNormality", "sqrtTrans", "sqrt(Y)", "sqrt(Y)"),
                    c("transformationNormality", "inverseTrans", "1/Y", "1/Y"),

                    c("transformationFit", "criterion", "Criterion / test", "Criterio / prueba"),
                    c("transformationFit", "original", "Original", "Original"),
                    c("transformationFit", "logTrans", "log(Y)", "log(Y)"),
                    c("transformationFit", "sqrtTrans", "sqrt(Y)", "sqrt(Y)"),
                    c("transformationFit", "inverseTrans", "1/Y", "1/Y")
                )

                for (item in cols)
                    set_col_title_safe(item[[1]], item[[2]], item[[3]], item[[4]])
            }

            set_table_column_titles()

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
                    return(tr("Not computed", "No calculado"))
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

            qname <- function(x) {
                paste0("`", gsub("`", "", x), "`")
            }

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
                self$results$intro$setContent(
                    "Seleccione una variable dependiente numérica."
                )
                return()
            }

            dep <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors
            predictors <- c(covs, factors)

            if (length(predictors) == 0) {
                self$results$intro$setContent(
                    "Seleccione al menos un predictor numérico o categórico."
                )
                return()
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
                self$results$intro$setContent(
                    "No hay suficientes casos completos para ajustar el modelo."
                )
                return()
            }

            formula_text <- paste(
                qname(dep),
                "~",
                paste(vapply(predictors, qname, character(1)), collapse = " + ")
            )

            model_formula <- stats::as.formula(formula_text)

            fit <- tryCatch(
                stats::lm(model_formula, data = dat2),
                error = function(e) NULL
            )

            if (is.null(fit)) {
                self$results$intro$setContent(
                    "No fue posible ajustar el modelo de regresión lineal."
                )
                return()
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
                tr("Assumption check for simple &amp; multiple regression", "Revisión de supuestos para regresión simple y múltiple"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr(
                    "Use this analysis when you want to review whether a linear regression model has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.",
                    "Use este análisis cuando quiera revisar si un modelo de regresión lineal tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."
                ),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr("<b>Dependent variable:</b> ", "<b>Variable dependiente:</b> "), html_escape(dep),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr("<b>Numeric predictors:</b> ", "<b>Predictores numéricos:</b> "),
                html_escape(ifelse(length(covs) == 0, tr("None", "Ninguno"), paste(covs, collapse = ", "))),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr("<b>Categorical predictors:</b> ", "<b>Predictores categóricos:</b> "),
                html_escape(ifelse(length(factors) == 0, tr("None", "Ninguno"), paste(factors, collapse = ", "))),
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

            add_ms(tr("Model", "Modelo"), gsub("`", "", formula_text))
            add_ms(tr("Cases used", "Casos usados"), as.character(n_used))
            add_ms(tr("Selected predictors", "Predictores seleccionados"), as.character(predictor_count))
            add_ms(tr("Estimated parameters", "Parámetros estimados"), as.character(p_model))
            add_ms("R²", fmt_num(r2))
            add_ms(tr("Adjusted R²", tr("Adjusted R²", "R² ajustado")), fmt_num(adj_r2))
            add_ms(tr("Residual standard error", "Error estándar residual"), fmt_num(sigma))
            add_ms(
                tr("Model p", "p del modelo"),
                ifelse(is.na(model_p), tr("Not computed", "No calculado"),
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

            add_miss(tr("Total rows", "Filas totales"), as.character(n_total))
            add_miss(tr("Complete cases used", "Casos completos usados"), as.character(n_used))
            add_miss(tr("Cases excluded due to missing data", "Casos excluidos por datos faltantes"), as.character(n_excluded))
            add_miss(
                tr("Excluded percentage", "Porcentaje excluido"),
                paste0(round(100 * n_excluded / max(1, n_total), 2), "%")
            )

            self$results$designGuide$setContent(html_guide(tr("Regression model", "Modelo de regresión"), "regression", "designGuide"))

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

            add_linearity_predictor <- function(predictor, test, statistic, value, p_value) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05 &&
                    test %in% c(
                        tr("Exploratory quadratic term", "Término cuadrático exploratorio"),
                        tr("Exploratory Box-Tidwell", "Box-Tidwell exploratorio")
                    )) {
                    linearity_problem <<- TRUE
                }

                add_table_row(
                    self$results$linearityPredictor,
                    paste0("lin_pred_", lin_pred_i),
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
                    tr("Numeric predictors", "Predictores numéricos"),
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
                    tr("Bivariate correlation", "Correlación bivariada"),
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
                    tr("Exploratory quadratic term", "Término cuadrático exploratorio"),
                    "p",
                    quad_p,
                    quad_p
                )

                bt_p <- tryCatch({
                    if (any(is.na(x)) || any(x <= 0))
                        stop("Box-Tidwell requiere valores positivos.")

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

                add_linearity_predictor(
                    xname,
                    tr("Exploratory Box-Tidwell", "Box-Tidwell exploratorio"),
                    "p",
                    bt_p,
                    bt_p
                )

                dcor_val <- tryCatch(dcor_stat(x, y), error = function(e) NA_real_)
                dcor_p <- tryCatch(dcor_pvalue(x, y), error = function(e) NA_real_)

                if (!is.na(dcor_val) && !is.na(cor_val) && !is.na(dcor_p) &&
                    (dcor_val - abs(cor_val)) > 0.10 && dcor_p < .05) {
                    linearity_problem <<- TRUE
                }

                add_linearity_predictor(
                    xname,
                    tr("Distance correlation (dCor)", "Correlación de distancia (dCor)"),
                    "dCor",
                    dcor_val,
                    dcor_p
                )

                ce_res <- tryCatch(copentTest(x, y), error = function(e) NULL)

                if (!is.null(ce_res)) {
                    add_linearity_predictor(
                        xname,
                        tr("Copula entropy (copent)", "Entropía copular (copent)"),
                        "CE",
                        ce_res$ce,
                        ce_res$p
                    )
                } else if (!requireNamespace("copent", quietly = TRUE)) {
                    add_linearity_predictor(
                        xname,
                        tr("Copula entropy (copent)", "Entropía copular (copent)"),
                        "CE",
                        NA_real_,
                        NA_real_
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
                tr("Full model", "Modelo completo"),
                tr("Approximate Ramsey RESET", "Ramsey RESET aproximado"),
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
                tr("Full model", "Modelo completo"),
                tr("Approximate Rainbow test", "Rainbow test aproximado"),
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
                    tr(
                        paste0("The other ", coh$total, " test(s) in the table agree with this conclusion."),
                        paste0("Las otras ", coh$total, " pruebas de la tabla coinciden con esta conclusión.")
                    )
                } else {
                    tr(
                        paste0(coh$agree, " of the other ", coh$total, " tests in the table agree with this conclusion; check the table to see which one(s) differ before drawing a final conclusion."),
                        paste0(coh$agree, " de las otras ", coh$total, " pruebas de la tabla coinciden con esta conclusión; revise la tabla para ver cuál(es) difieren antes de sacar una conclusión final.")
                    )
                }
            }

            self$results$linearityGuide$setContent(html_guide(tr("Linearity", "Linealidad"), "regression", "linearityGuide"))

            self$results$linearityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    paste0(
                        tr("Ramsey RESET: p = ", "Ramsey RESET: p = "), fmt_p(reset_p),
                        tr(". Rainbow test: p = ", ". Rainbow test: p = "), fmt_p(rainbow_p),
                        tr(
                            paste0(" (n = ", n_used, "). Both tests check whether the model is missing curvature that a straight line cannot capture."),
                            paste0(" (n = ", n_used, "). Ambas pruebas revisan si el modelo pierde curvatura que una línea recta no puede capturar.")
                        )
                    ),
                    tr(
                        "A significant result (p < .05) does not mean the model is useless: it means the linear form may be missing part of the relationship, which can bias coefficients and predictions in specific ranges of the predictors.",
                        "Un resultado significativo (p < .05) no significa que el modelo sea inútil: significa que la forma lineal puede estar dejando fuera parte de la relación, lo cual puede sesgar coeficientes y predicciones en rangos específicos de los predictores."
                    ),
                    tr(
                        "Common error: treating a non-significant RESET/Rainbow test as proof of perfect linearity. These tests have limited power in small samples (they may miss real curvature) and can flag trivial, practically irrelevant curvature in very large samples. Always read them together with residual-vs-fitted plots.",
                        "Error común: interpretar un RESET/Rainbow no significativo como prueba de linealidad perfecta. Estas pruebas tienen poder limitado en muestras pequeñas (pueden no detectar curvatura real) y pueden marcar como \"significativa\" una curvatura trivial e irrelevante en muestras muy grandes. Léalas siempre junto con los gráficos de residuos vs. valores ajustados."
                    ),
                    tr(
                        "If there is evidence of non-linearity, consider adding polynomial terms, transforming variables, using splines, or reviewing whether a different model family fits the relationship better.",
                        "Si hay evidencia de no linealidad, considere agregar términos polinomiales, transformar variables, usar splines, o revisar si otra familia de modelos se ajusta mejor a la relación."
                    ),
                    if (length(covs) > 0) tr(
                        "Distance correlation (dCor): unlike Pearson's r, which only detects linear association, dCor can detect any form of statistical dependence, including non-monotonic or curved relationships, and equals zero only when the variables are independent. When dCor is notably higher than |Pearson's r| for a predictor, it signals dependence the linear model may be missing.",
                        "Correlación de distancia (dCor): a diferencia de r de Pearson, que solo detecta asociación lineal, dCor puede detectar cualquier forma de dependencia estadística, incluyendo relaciones no monótonas o curvas, y es igual a cero solo cuando las variables son independientes. Cuando dCor es notablemente mayor que |r de Pearson| para un predictor, señala una dependencia que el modelo lineal podría no estar capturando."
                    ) else "",
                    if (length(covs) > 0) tr(
                        "Common error: treating a significant dCor by itself as proof that the relationship is non-linear. dCor is also significant for purely linear relationships; the real diagnostic signal is the size of the gap between dCor and |Pearson's r|, not the significance of dCor alone.",
                        "Error común: tratar un dCor significativo por sí solo como prueba de que la relación es no lineal. dCor también es significativo para relaciones puramente lineales; la señal diagnóstica real es el tamaño de la brecha entre dCor y |r de Pearson|, no la significancia de dCor por sí sola."
                    ) else ""
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # Matrices de correlaciones (Pearson y dCor, formato APA 7)
            # ------------------------------------------------------------

            self$results$correlationMatrixGuide$setContent(html_guide(tr("Correlation Matrix", "Matriz de Correlaciones"), "regression", "correlationMatrixGuide"))

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
                        paste0(sprintf("N = %d.", n_used), " ", tr("* p < .05, ** p < .01, *** p < .001", "* p < .05, ** p < .01, *** p < .001")),
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
                    var1 = pr$v1,
                    var2 = pr$v2,
                    pearson = apaCell(pr$pearsonR, pr$pearsonP),
                    dcor = apaCell(pr$dcor, pr$dcorP),
                    gap = gap,
                    ce = pr$ce,
                    ceP = pr$ceP,
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
                    tr(sprintf("%d pair(s) show a Pearson/dCor gap greater than %.2f: %s. Inspect a scatterplot of each flagged pair; when copula entropy is also significant for a pair, this reinforces the suspicion of an unmodeled non-linear dependency.",
                               length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", ")),
                       sprintf("%d par(es) muestran una brecha Pearson/dCor mayor a %.2f: %s. Revise un diagrama de dispersión de cada par señalado; cuando la entropía copular también es significativa para un par, esto refuerza la sospecha de una dependencia no lineal no modelada.",
                               length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", "))),
                    paragraphs = FALSE
                ))
            }

            self$results$correlationIndividualGuide$setContent(
                plot_guide(
                    "Guía: Relación por par (Pearson / dCor)",
                    "comparar visualmente lo que Pearson r y dCor detectan en la relación entre dos variables.",
                    "valores de la primera variable del par.",
                    "valores de la segunda variable del par.",
                    "puntos alineados en torno a la línea de ajuste lineal cuando r es alto; una curva clara cuando dCor supera notablemente a |r|.",
                    "el ajuste suavizado se separa marcadamente de la línea lineal, sobre todo en pares señalados en la tabla de discordancia.",
                    "revisar si conviene un término no lineal, una transformación, o simplemente confirmar que la relación es lineal."
                )
            )

            self$results$correlationComparativeGuide$setContent(
                plot_guide(
                    "Guía: Comparación de Pearson, dCor y entropía copular entre pares",
                    "comparar de un vistazo los tres coeficientes de asociación (Pearson, dCor, entropía copular) entre todos los pares de variables numéricas del modelo.",
                    "pares de variables numéricas del modelo.",
                    "magnitud del coeficiente (Pearson r, dCor).",
                    "Pearson y dCor con valores similares para la mayoría de los pares.",
                    "una brecha grande entre Pearson y dCor en algún par, especialmente si coincide con entropía copular significativa.",
                    "revisar el gráfico individual de ese par y considerar un término no lineal si la brecha se confirma."
                )
            )

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Residual normality.
            # ES: Normalidad de residuos
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            add_res_norm <- function(test, statistic, value, p_value) {
                add_table_row(
                    self$results$residualNormality,
                    paste0("norm_", safe_key(test)),
                    list(
                        test = test,
                        statistic = statistic,
                        value = clean_num(value),
                        p = clean_num(p_value),
                        pSig = p_sig(p_value)
                    )
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
                add_res_norm("Shapiro-Wilk", "W", NA_real_, NA_real_)

            # Lilliefors / Anderson-Darling / Cramer-von Mises / Shapiro-Francia /
            # Pearson chi-square: identical tryCatch calls in every module,
            # consolidated in shared-helpers.R (.al_nortest_battery).
            # ES: idénticas en todos los módulos, consolidadas en shared-helpers.R.
            .nt_res <- .al_nortest_battery(res)
            li <- .nt_res$li; ad <- .nt_res$ad; cvm <- .nt_res$cvm; sf <- .nt_res$sf

            if (!is.null(li))
                add_res_norm(tr("Lilliefors (corrected K-S)", "Lilliefors (K-S corregido)"), "D", li$statistic[[1]], li$p.value)
            else
                add_res_norm(tr("Lilliefors (corrected K-S)", "Lilliefors (K-S corregido)"), "D", NA_real_, NA_real_)

            if (!is.null(ad))
                add_res_norm("Anderson-Darling", "A²", ad$statistic[[1]], ad$p.value)
            else
                add_res_norm("Anderson-Darling", "A²", NA_real_, NA_real_)

            if (!is.null(cvm))
                add_res_norm("Cramer-von Mises", "W²", cvm$statistic[[1]], cvm$p.value)
            else
                add_res_norm("Cramer-von Mises", "W²", NA_real_, NA_real_)

            if (!is.null(sf))
                add_res_norm("Shapiro-Francia", "W'", sf$statistic[[1]], sf$p.value)
            else
                add_res_norm("Shapiro-Francia", "W'", NA_real_, NA_real_)

            pt <- .nt_res$pt

            if (!is.null(pt))
                add_res_norm(tr("Pearson chi-square", "Pearson chi-cuadrado"), "P", pt$statistic[[1]], pt$p.value)
            else
                add_res_norm(tr("Pearson chi-square", "Pearson chi-cuadrado"), "P", NA_real_, NA_real_)

            jb <- .nc_res$jb

            if (!is.null(jb))
                add_res_norm("Jarque-Bera", "JB", jb$value, jb$p)
            else
                add_res_norm("Jarque-Bera", "JB", NA_real_, NA_real_)

            skew_test <- .nc_res$skew

            if (!is.null(skew_test))
                add_res_norm(tr("Skewness test", "Prueba de asimetría"), "z", skew_test$value, skew_test$p)
            else
                add_res_norm(tr("Skewness test", "Prueba de asimetría"), "z", NA_real_, NA_real_)

            kurt_test <- .nc_res$kurt

            if (!is.null(kurt_test))
                add_res_norm(tr("Kurtosis test", "Prueba de curtosis"), "z", kurt_test$value, kurt_test$p)
            else
                add_res_norm(tr("Kurtosis test", "Prueba de curtosis"), "z", NA_real_, NA_real_)

            self$results$residualNormalityGuide$setContent(html_guide(tr("Residual normality", "Normalidad de residuos"), "regression", "residualNormalityGuide"))

            self$results$residualNormalityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    paste0(
                        tr("Shapiro-Wilk (primary test): ", "Shapiro-Wilk (prueba principal): "),
                        if (!is.null(sw)) paste0("p = ", fmt_p(sw$p.value)) else tr("not computed", "no calculado"),
                        tr(paste0(" (n = ", n_used, ")."), paste0(" (n = ", n_used, ").")),
                        tr(
                            " This test looks at the residuals of the model, not the raw dependent variable.",
                            " Esta prueba evalúa los residuos del modelo, no la variable dependiente en bruto."
                        )
                    ),
                    tr(
                        "Shapiro-Wilk is used as the primary test because it retains the highest statistical power among common normality tests across nearly the full range of sample sizes (Razali & Wah, 2011). A widespread recommendation to switch to Kolmogorov-Smirnov once n exceeds ~50 is a leftover from old software limitations, not a statistical reason: in practice, Kolmogorov-Smirnov is generally one of the weakest of these tests when the mean and standard deviation are estimated from the same data, as is always the case here.",
                        "Se usa Shapiro-Wilk como prueba principal por ser la de mayor poder estadístico entre las pruebas de normalidad más comunes, para prácticamente todo rango de tamaño muestral (Razali & Wah, 2011). La recomendación extendida de cambiar a Kolmogorov-Smirnov cuando n supera ~50 es un remanente de limitaciones de software antiguo, no una razón estadística: en la práctica, Kolmogorov-Smirnov suele ser una de las pruebas más débiles de este grupo cuando la media y la desviación estándar se estiman de los mismos datos, como ocurre siempre aquí."
                    ),
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
                    tr(
                        "In practice, residual normality mainly affects the precision of classical inference (confidence intervals and p-values) in small samples. Point estimates of the coefficients remain valid even with moderate non-normality, and by the Central Limit Theorem, inference becomes more robust as sample size grows.",
                        "En la práctica, la normalidad de residuos afecta principalmente la precisión de la inferencia clásica (intervalos de confianza y valores p) en muestras pequeñas. Las estimaciones puntuales de los coeficientes siguen siendo válidas incluso con no normalidad moderada, y por el Teorema Central del Límite, la inferencia se vuelve más robusta a medida que crece el tamaño muestral."
                    ),
                    tr(
                        "Common error: assuming that a significant normality test invalidates the whole regression. It does not: it is a signal to inspect the shape of the residuals (skewness, heavy tails, outliers) before deciding whether classical inference is still trustworthy.",
                        "Error común: asumir que una prueba de normalidad significativa invalida toda la regresión. No es así: es una señal para revisar la forma de los residuos (asimetría, colas pesadas, atípicos) antes de decidir si la inferencia clásica sigue siendo confiable."
                    ),
                    tr(
                        "Sample-size caveat: Shapiro-Wilk becomes very sensitive with large samples (roughly n > 300), often flagging small, practically irrelevant deviations as statistically significant. With small samples (n < 30) it may lack power to detect real departures from normality.",
                        "Matiz de tamaño de muestra: Shapiro-Wilk se vuelve muy sensible con muestras grandes (aproximadamente n > 300), marcando a menudo desviaciones pequeñas y poco relevantes en la práctica como estadísticamente significativas. Con muestras pequeñas (n < 30) puede carecer de poder para detectar desviaciones reales de la normalidad."
                    )
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Homoscedasticity and heteroscedasticity.
            # ES: Homoscedasticidad y heterocedasticidad
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            add_homo <- function(family, test, statistic, value, df, p_value) {
                add_table_row(
                    self$results$homoscedasticity,
                    paste0("homo_", safe_key(test)),
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
            }

            bp <- .al_bptest(fit)

            if (!is.null(bp)) {
                if (!is.na(clean_num(bp$p.value)) && clean_num(bp$p.value) < .05)
                    homo_problem <- TRUE
                add_homo(tr("Heteroscedasticity", "Heterocedasticidad"), "Breusch-Pagan (lmtest)",
                         "LM", unname(bp$statistic), unname(bp$parameter), bp$p.value)
            } else
                add_homo(tr("Heteroscedasticity", "Heterocedasticidad"), "Breusch-Pagan (lmtest)",
                         "LM", NA_real_, NA_integer_, NA_real_)

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
                add_homo(tr("Heteroscedasticity", "Heterocedasticidad"), tr("White (general)", "White (general)"),
                         "LM", white$value, white$df, white$p)
            else
                add_homo(tr("Heteroscedasticity", "Heterocedasticidad"), tr("White (general)", "White (general)"),
                         "LM", NA_real_, NA_integer_, NA_real_)

            gq <- tryCatch(lmtest::gqtest(fit, order.by = fitted_values), error = function(e) NULL)

            if (!is.null(gq))
                add_homo(tr("Heteroscedasticity", "Heterocedasticidad"), "Goldfeld-Quandt (lmtest)",
                         "F", unname(gq$statistic), min(unname(gq$parameter)), gq$p.value)
            else
                add_homo(tr("Heteroscedasticity", "Heterocedasticidad"), "Goldfeld-Quandt (lmtest)",
                         "F", NA_real_, NA_integer_, NA_real_)

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
                    tr("Variance equality", "Igualdad de varianzas"),
                    tr("Levene by fitted values", "Levene por valores ajustados"),
                    "F",
                    lev_mean$value,
                    lev_mean$df,
                    lev_mean$p
                )
            } else {
                add_homo(
                    tr("Variance equality", "Igualdad de varianzas"),
                    tr("Levene by fitted values", "Levene por valores ajustados"),
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
                    tr("Variance equality", "Igualdad de varianzas"),
                    tr("Brown-Forsythe by fitted values", "Brown-Forsythe por valores ajustados"),
                    "F",
                    lev_median$value,
                    lev_median$df,
                    lev_median$p
                )
            } else {
                add_homo(
                    tr("Variance equality", "Igualdad de varianzas"),
                    tr("Brown-Forsythe by fitted values", "Brown-Forsythe por valores ajustados"),
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
                    tr("Variance equality", "Igualdad de varianzas"),
                    tr("Fligner-Killeen by fitted values", "Fligner-Killeen por valores ajustados"),
                    "χ²",
                    fligner$statistic[[1]],
                    fligner$parameter[[1]],
                    fligner$p.value
                )
            } else {
                add_homo(
                    tr("Variance equality", "Igualdad de varianzas"),
                    tr("Fligner-Killeen by fitted values", "Fligner-Killeen por valores ajustados"),
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
                    tr("Heteroscedasticity", "Heterocedasticidad"),
                    tr("Spearman |residuals| vs fitted", "Spearman |residuos| vs ajustados"),
                    "ρ",
                    spearman_abs$estimate[[1]],
                    NA_integer_,
                    spearman_abs$p.value
                )
            } else {
                add_homo(
                    tr("Heteroscedasticity", "Heterocedasticidad"),
                    tr("Spearman |residuals| vs fitted", "Spearman |residuos| vs ajustados"),
                    "ρ",
                    NA_real_,
                    NA_integer_,
                    NA_real_
                )
            }

            self$results$homoscedasticityGuide$setContent(html_guide(tr("Homoscedasticity and heteroscedasticity", "Homoscedasticidad y heterocedasticidad"), "regression", "homoscedasticityGuide"))

            self$results$homoscedasticityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    paste0(
                        tr("Breusch-Pagan (primary test): ", "Breusch-Pagan (prueba principal): "),
                        if (!is.null(bp)) paste0("p = ", fmt_p(bp$p.value)) else tr("not computed", "no calculado"),
                        tr(paste0(" (n = ", n_used, ")."), paste0(" (n = ", n_used, ").")),
                        tr(" ", " ")
                    ),
                    tr(
                        "Breusch-Pagan is used as the primary test because it was designed specifically to detect heteroscedasticity in regression residuals (Breusch & Pagan, 1979), unlike Levene, Brown-Forsythe, and Fligner-Killeen, which are group-variance tests borrowed from ANOVA and applied here only approximately, by artificially splitting fitted values into bins. The White test (reported alongside) generalizes Breusch-Pagan to detect more complex, non-linear forms of heteroscedasticity (White, 1980).",
                        "Se usa Breusch-Pagan como prueba principal porque fue diseñada específicamente para detectar heterocedasticidad en los residuos de una regresión (Breusch & Pagan, 1979), a diferencia de Levene, Brown-Forsythe y Fligner-Killeen, que son pruebas de igualdad de varianzas tomadas del ANOVA y que aquí se aplican solo de forma aproximada, dividiendo artificialmente los valores ajustados en grupos. El White test (reportado junto a esta) generaliza a Breusch-Pagan para detectar formas más complejas y no lineales de heterocedasticidad (White, 1980)."
                    ),
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
                    tr(
                        "Heteroscedasticity does not bias the regression coefficients themselves, but it does make the standard errors, confidence intervals, and p-values unreliable, because the classical formulas assume constant error variance across all predicted values.",
                        "La heterocedasticidad no sesga los coeficientes de la regresión en sí mismos, pero sí vuelve poco confiables los errores estándar, intervalos de confianza y valores p, porque las fórmulas clásicas suponen varianza constante del error a lo largo de todos los valores predichos."
                    ),
                    tr(
                        "Common error: concluding that a significant heteroscedasticity test means the model must be discarded. In most cases, the practical remedy is not a different model but heteroscedasticity-robust standard errors (also called White or Huber-White standard errors).",
                        "Error común: concluir que una prueba de heterocedasticidad significativa obliga a descartar el modelo. En la mayoría de los casos, el remedio práctico no es otro modelo sino errores estándar robustos a heterocedasticidad (también llamados errores de White o Huber-White)."
                    ),
                    tr(
                        "Sample-size note: Breusch-Pagan and White rely on a large-sample (asymptotic) approximation and can be unreliable with very small samples; in that situation, cross-check with Levene, Fligner-Killeen, or the Spearman correlation between |residuals| and fitted values shown here.",
                        "Nota sobre tamaño de muestra: Breusch-Pagan y White dependen de una aproximación de muestra grande (asintótica) y pueden ser poco confiables con muestras muy pequeñas; en ese caso, contraste con Levene, Fligner-Killeen, o la correlación de Spearman entre |residuos| y valores ajustados que se muestra aquí."
                    )
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
                add_independence(paste0("Breusch-Godfrey, ", tr("lags", "rezagos"), " = ", bg$df),
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
                add_independence(paste0("Ljung-Box, ", tr("lags", "rezagos"), " = ", lb_lag),
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

            self$results$independenceGuide$setContent(html_guide(tr("Error independence", "Independencia de errores"), "regression", "independenceGuide"))

            self$results$independenceInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    paste0(
                        tr("Breusch-Godfrey (primary test): ", "Breusch-Godfrey (prueba principal): "),
                        if (!is.null(bg)) paste0("p = ", fmt_p(bg$p)) else tr("not computed", "no calculado"),
                        tr(paste0(" (n = ", n_used, "). Durbin-Watson (reported for reference): DW = "), paste0(" (n = ", n_used, "). Durbin-Watson (reportado como referencia): DW = ")),
                        fmt_num(dw, 3),
                        tr(". Values close to 2 are compatible with independent errors.", ". Valores cercanos a 2 son compatibles con errores independientes.")
                    ),
                    tr(
                        "Breusch-Godfrey is used as the primary test because it generalizes Durbin-Watson to any lag order and does not have Durbin-Watson's \"inconclusive zone\", a range of values where the classical DW test cannot reject or accept the null hypothesis (Breusch, 1978; Godfrey, 1978). Durbin-Watson is still reported because it remains the most widely recognized statistic in applied practice (Durbin & Watson, 1951).",
                        "Se usa Breusch-Godfrey como prueba principal porque generaliza a Durbin-Watson a cualquier orden de rezago y no tiene la \"zona de inconclusión\" de Durbin-Watson, un rango de valores donde la prueba DW clásica no puede rechazar ni aceptar la hipótesis nula (Breusch, 1978; Godfrey, 1978). Durbin-Watson igual se reporta porque sigue siendo el estadístico más reconocido en la práctica aplicada (Durbin & Watson, 1951)."
                    ),
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
                    tr(
                        "Common error: running and interpreting these tests on data that has no meaningful order (for example, a cross-sectional survey where row order is arbitrary). They are only informative when the order of observations reflects a real sequence: time, spatial position, or measurement order.",
                        "Error común: ejecutar e interpretar estas pruebas sobre datos que no tienen un orden con sentido (por ejemplo, una encuesta transversal donde el orden de las filas es arbitrario). Solo son informativas cuando el orden de las observaciones refleja una secuencia real: tiempo, posición espacial, u orden de medición."
                    ),
                    tr(
                        "If your data does have a meaningful order (repeated measurements over time, data collected sequentially, or clustered/nested observations), significant autocorrelation means the standard errors of the model are likely underestimated, making results look more precise than they really are.",
                        "Si sus datos sí tienen un orden con sentido (mediciones repetidas en el tiempo, datos recolectados secuencialmente, u observaciones agrupadas o anidadas), una autocorrelación significativa implica que los errores estándar del modelo probablemente están subestimados, haciendo que los resultados parezcan más precisos de lo que realmente son."
                    ),
                    tr(
                        "If there is a real dependency structure (repeated measures, nested groups, time series), consider mixed-effects models, generalized least squares, or explicitly time-series-aware methods instead of relying only on this diagnostic.",
                        "Si existe una estructura de dependencia real (medidas repetidas, grupos anidados, series de tiempo), considere modelos de efectos mixtos, mínimos cuadrados generalizados, o métodos específicos para series de tiempo, en vez de depender solo de este diagnóstico."
                    )
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

            add_multi <- function(diagnostic, item, statistic, value) {
                add_table_row(
                    self$results$multicollinearity,
                    paste0("multi_", multi_i),
                    list(
                        diagnostic = diagnostic,
                        item = item,
                        statistic = statistic,
                        value = clean_num(value)
                    )
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

                    add_multi("VIF", colnames(X_no_intercept)[j], "VIF", vif)
                    add_multi(tr("Tolerance", "Tolerancia"), colnames(X_no_intercept)[j], "1/VIF", tol)
                }

                eig <- tryCatch({
                    X_scaled <- scale(X_no_intercept)
                    R <- stats::cor(X_scaled, use = "pairwise.complete.obs")
                    eigen(R, symmetric = TRUE)$values
                }, error = function(e) NULL)

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

                    add_multi(tr("Minimum eigenvalue", "Eigenvalue mínimo"), tr("Design matrix", "Matriz de diseño"), tr("minimum λ", "λ mínimo"), min_eig)
                    add_multi(tr("Condition index", "Índice de condición"), tr("Design matrix", "Matriz de diseño"), "CI", ci)
                    add_multi(tr("Determinant", "Determinante"), tr("Correlation matrix", "Matriz de correlación"), "det(R)", det_r)
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
                    add_multi(tr("Maximum correlation", "Correlación máxima"), tr("Numeric predictors", "Predictores numéricos"), tr("maximum |r|", "|r| máximo"), max_cor)
                }
            }

            model_rank <- tryCatch(qr(stats::model.matrix(fit))$rank, error = function(e) NA_real_)
            model_cols <- tryCatch(ncol(stats::model.matrix(fit)), error = function(e) NA_real_)
            non_estimable <- clean_num(model_cols - model_rank)

            add_multi(tr("Model rank", "Rango del modelo"), tr("Design matrix", "Matriz de diseño"), tr("rank", "rango"), model_rank)
            add_multi(tr("Non-estimable parameters", "Parámetros no estimables"), tr("Design matrix", "Matriz de diseño"),
                      tr("columns - rank", "columnas - rango"), non_estimable)

            if (!is.na(clean_num(max_vif)) && max_vif >= 5)
                col_problem <- TRUE

            if (!is.na(clean_num(max_vif)) && max_vif >= 10)
                severe_col_problem <- TRUE

            if (!is.na(clean_num(max_ci)) && max_ci > 15)
                col_problem <- TRUE

            if (!is.na(clean_num(non_estimable)) && non_estimable > 0)
                col_problem <- TRUE

            self$results$multicollinearityGuide$setContent(html_guide(tr("Multicollinearity", "Multicolinealidad"), "regression", "multicollinearityGuide"))

            self$results$multicollinearityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    paste0(
                        tr("Maximum VIF: ", "VIF máximo: "), fmt_num(max_vif, 2),
                        if (!is.na(max_vif_name)) paste0(tr(" (predictor: ", " (predictor: "), max_vif_name, ")") else "",
                        tr(
                            ". By convention, values between 5 and 10 raise a moderate concern, and values above 10 are considered a severe problem (a threshold traceable to Marquardt, 1970, and widely adopted in reference texts such as Kutner et al.'s Applied Linear Statistical Models and Hair et al.'s Multivariate Data Analysis).",
                            ". Por convención, valores entre 5 y 10 encienden una alerta moderada, y valores por encima de 10 se consideran un problema severo (un umbral que se remonta a Marquardt, 1970, y ampliamente adoptado en textos de referencia como Applied Linear Statistical Models de Kutner et al., y Multivariate Data Analysis de Hair et al.)."
                        )
                    ),
                    paste0(
                        tr(
                            "In practical terms, a VIF of ", "En términos prácticos, un VIF de "
                        ),
                        fmt_num(max_vif, 2),
                        tr(
                            paste0(" means the standard error of that coefficient is about ", fmt_num(sqrt(max_vif), 2), " times larger than it would be without collinearity. This does not invalidate the model as a whole, but it does make that specific coefficient's confidence interval much less precise."),
                            paste0(" significa que el error estándar de ese coeficiente es aproximadamente ", fmt_num(sqrt(max_vif), 2), " veces más grande de lo que sería sin colinealidad. Esto no invalida el modelo en conjunto, pero sí hace que el intervalo de confianza de ese coeficiente en particular sea mucho menos preciso.")
                        )
                    ),
                    tr(
                        "Common error: assuming a high VIF means the predictor should be removed or that the model \"does not work\". The model can still predict well overall; the issue is isolating that specific predictor's individual effect from the others. Removing a substantively relevant variable only because of a high VIF can bias the model more than the collinearity itself did.",
                        "Error común: pensar que un VIF alto significa que hay que eliminar el predictor o que el modelo \"no sirve\". El modelo puede seguir prediciendo bien en conjunto; el problema es aislar el efecto individual de ese predictor frente a los demás. Eliminar una variable sustantivamente relevante solo porque tiene VIF alto puede sesgar el modelo más de lo que la propia colinealidad lo hacía."
                    ),
                    paste0(
                        tr(
                            "Sample-size caveat: this threshold is a convention, not a statistical law. With small samples (n < 100; here n = ",
                            "Matiz de tamaño de muestra: este umbral es una convención, no una ley estadística. Con muestras pequeñas (n < 100; aquí n = "
                        ),
                        n_used,
                        tr(
                            "), even moderate collinearity can make coefficients quite unstable; with larger samples, a similar VIF is usually more tolerable.",
                            "), incluso una colinealidad moderada puede volver los coeficientes bastante inestables; con muestras grandes, un VIF similar suele ser más tolerable."
                        )
                    ),
                    tr(
                        "If collinearity is a real concern, consider combining the most redundant predictors, centering variables involved in interactions, or reporting the finding and discussing its effect on that coefficient's precision rather than discarding the full model.",
                        "Si la colinealidad es una preocupación real, considere combinar los predictores más redundantes, centrar las variables involucradas en interacciones, o reportar el hallazgo y discutir su efecto sobre la precisión de ese coeficiente, en vez de descartar el modelo completo."
                    )
                ),
                paragraphs = TRUE
            ))

            # ------------------------------------------------------------
            # Casos atípicos e influyentes
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

            self$results$influenceGuide$setContent(html_guide(tr("Outlying and influential cases", "Casos atípicos e influyentes"), "regression", "influenceGuide"))

            n_flagged_influence <- sum(nzchar(criteria), na.rm = TRUE)

            self$results$influenceInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(
                    paste0(
                        tr(
                            paste0(n_flagged_influence, " of ", n, " cases were flagged by at least one influence criterion (Cook's D > ", fmt_num(cook_cut, 4), ", leverage > ", fmt_num(lev_cut, 4), ", |DFFITS| > ", fmt_num(dffits_cut, 4), ", or |studentized residual| > ", stud_cut, ")."),
                            paste0(n_flagged_influence, " de ", n, " casos fueron marcados por al menos un criterio de influencia (Cook's D > ", fmt_num(cook_cut, 4), ", leverage > ", fmt_num(lev_cut, 4), ", |DFFITS| > ", fmt_num(dffits_cut, 4), ", o |residuo studentizado| > ", stud_cut, ").")
                        )
                    ),
                    tr(
                        "These criteria measure different aspects of the same question: how unusual a case is (leverage), how large its residual is (studentized residual), and how much it would change the model's coefficients if removed (Cook's D, DFFITS). A flagged case is not automatically an error.",
                        "Estos criterios miden aspectos distintos de la misma pregunta: qué tan inusual es un caso (leverage), qué tan grande es su residuo (residuo studentizado), y cuánto cambiaría los coeficientes del modelo si se eliminara (Cook's D, DFFITS). Un caso marcado no es automáticamente un error."
                    ),
                    tr(
                        "Common error: deleting flagged cases automatically to \"improve\" the model. This can hide real, substantively important observations and produce a model that only fits the well-behaved majority of the data, misrepresenting the population it is meant to describe.",
                        "Error común: eliminar automáticamente los casos marcados para \"mejorar\" el modelo. Esto puede ocultar observaciones reales y sustantivamente importantes, y producir un modelo que solo se ajusta a la mayoría bien comportada de los datos, sin representar fielmente a la población que se pretende describir."
                    ),
                    paste0(
                        tr(
                            "Sample-size caveat: the Cook's D cutoff of 4/n used here (n = ",
                            "Matiz de tamaño de muestra: el umbral de Cook's D de 4/n usado aquí (n = "
                        ),
                        n,
                        tr(
                            paste0(") becomes stricter as the sample shrinks, so in small samples it is common to flag several cases that are not truly problematic. Treat the number of flagged cases as a starting point for inspection, not as a fixed quota to remove."),
                            paste0(") se vuelve más estricto a medida que la muestra es más pequeña, así que en muestras pequeñas es común marcar varios casos que no son realmente problemáticos. Trate el número de casos marcados como un punto de partida para inspección, no como una cuota fija a eliminar.")
                        )
                    ),
                    tr(
                        "For each flagged case, check whether it is a data-entry error, a valid but extreme case, or a signal that the model does not represent all subgroups equally well; compare the model's conclusions with and without the case before deciding.",
                        "Para cada caso marcado, revise si se trata de un error de registro, un caso válido pero extremo, o una señal de que el modelo no representa igual de bien a todos los subgrupos; compare las conclusiones del modelo con y sin ese caso antes de decidir."
                    )
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
            # Visibilidad de gráficos según el conjunto seleccionado
            # ------------------------------------------------------------

            # ------------------------------------------------------------
            # Visibilidad de gráficos por área
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
                        tr("Regression diagnostic plots", "Gráficos diagnósticos de regresión"),
                        c(
                            tr(
                                "Diagnostic plots complement statistical tests and help evaluate patterns that a p-value may hide.",
                                "Los gráficos diagnósticos complementan las pruebas estadísticas y ayudan a evaluar patrones que un p-valor puede ocultar."
                            ),
                            tr(
                                "Use these plots to review linearity, residual normality, homoscedasticity, influence, and predictive quality.",
                                "Use estos gráficos para revisar linealidad, normalidad de residuos, homocedasticidad, influencia y calidad predictiva del modelo."
                            )
                        )
                    )
                )

                self$results$residualsFittedGuide$setContent(
                    plot_guide(
                        "Guía: Residuos vs valores ajustados",
                        "evaluar linealidad y varianza residual constante.",
                        "valores ajustados o predichos por el modelo para cada caso.",
                        "residuos, es decir, diferencia entre valor observado y valor predicho.",
                        "nube aleatoria alrededor de cero, sin forma curva ni embudo.",
                        "curvatura, bandas, embudo o grupos separados.",
                        "revisar términos no lineales, transformación o errores estándar robustos."
                    )
                )

                self$results$predictorEffectsGuide$setContent(
                    plot_guide(
                        "Guía: Efectos visuales de predictores numéricos",
                        "comparar la dirección e intensidad relativa de los predictores numéricos.",
                        "valor estandarizado del predictor evaluado, de bajo a alto.",
                        "valor predicho de la variable dependiente manteniendo constantes los demás predictores.",
                        "líneas aproximadamente rectas y pendientes coherentes con el modelo.",
                        "pendientes muy distintas, líneas planas o efectos difíciles de interpretar.",
                        "revisar tamaño y dirección de efectos, linealidad, interacciones o términos no lineales."
                    )
                )

                self$results$qqResidualsGuide$setContent(
                    plot_guide(
                        "Guía: Q-Q plot de residuos",
                        "evaluar si los residuos estandarizados se aproximan a una distribución normal.",
                        "cuantiles teóricos esperados bajo normalidad.",
                        "cuantiles observados de los residuos estandarizados.",
                        "puntos cercanos a la línea diagonal.",
                        "desviaciones fuertes en extremos, forma de S o colas pesadas.",
                        "interpretar inferencias clásicas con cautela y revisar atípicos o transformaciones."
                    )
                )

                self$results$residualHistogramGuide$setContent(
                    plot_guide(
                        "Guía: Histograma de residuos",
                        "examinar forma, simetría y concentración de los residuos estandarizados.",
                        "residuos estandarizados.",
                        "frecuencia o número de casos.",
                        "distribución aproximadamente simétrica y centrada en cero.",
                        "asimetría marcada, múltiples picos o colas muy largas.",
                        "complementar pruebas de normalidad y revisar casos extremos."
                    )
                )

                self$results$residualNormalCurveGuide$setContent(
                    plot_guide(
                        "Guía: Distribución observada vs normal teórica",
                        "comparar la distribución observada de los residuos con una normal teórica.",
                        "residuos estandarizados del modelo.",
                        "densidad, es decir, concentración relativa de casos.",
                        "curva empírica centrada cerca de cero y similar a la campana normal.",
                        "asimetría, colas pesadas, varios picos o exceso de casos extremos.",
                        "leer pruebas clásicas con cautela y revisar atípicos, transformación o especificación."
                    )
                )

                self$results$scaleLocationGuide$setContent(
                    plot_guide(
                        "Guía: Scale-location plot",
                        "evaluar si la dispersión de los residuos se mantiene estable.",
                        tr("fitted or predicted values from the model.", "valores ajustados o predichos por el modelo."),
                        "raíz cuadrada del valor absoluto de los residuos estandarizados.",
                        "banda horizontal con dispersión similar a lo largo del eje X.",
                        "tendencia ascendente, descendente o forma de embudo.",
                        "considerar errores robustos, transformación o modelo alternativo."
                    )
                )

                self$results$numericBoxplotsGuide$setContent(
                    plot_guide(
                        "Guía: Boxplots de variables numéricas",
                        "detectar valores atípicos univariados en la variable dependiente y predictores numéricos.",
                        "variables numéricas incluidas en el diagnóstico.",
                        "valores observados de cada variable en su escala original.",
                        "cajas compactas, bigotes razonables y pocos puntos extremos.",
                        "muchos puntos fuera de bigotes, asimetría fuerte o valores muy alejados.",
                        "revisar datos extremos antes de interpretar influencia multivariada o residuos."
                    )
                )

                self$results$residualsLeverageGuide$setContent(
                    plot_guide(
                        "Guía: Residuos studentizados vs leverage",
                        "identificar casos con combinación de residuo extremo y alto leverage.",
                        "leverage, o distancia del caso respecto al centro del espacio de predictores.",
                        "residuo studentizado, que expresa qué tan extremo es el error del caso.",
                        "la mayoría de casos cerca de cero y con leverage bajo.",
                        "casos con residuo studentizado grande, leverage alto o Cook's D elevado.",
                        "revisar datos influyentes y comparar análisis de sensibilidad."
                    )
                )

                self$results$cooksDGuide$setContent(
                    plot_guide(
                        "Guía: Cook's D por caso",
                        "detectar observaciones que pueden cambiar el ajuste del modelo.",
                        "número o posición del caso en la base de datos.",
                        "distancia de Cook, indicador de influencia global.",
                        "barras pequeñas y por debajo de la línea de referencia.",
                        "casos con Cook's D por encima del umbral visual.",
                        "verificar plausibilidad del caso y documentar análisis con/sin casos influyentes."
                    )
                )

                self$results$observedPredictedGuide$setContent(
                    plot_guide(
                        "Guía: Valores observados vs predichos",
                        "evaluar la correspondencia entre valores reales y predicciones del modelo.",
                        "valores predichos por el modelo.",
                        "valores observados de la variable dependiente.",
                        "puntos cercanos a la línea diagonal de predicción perfecta.",
                        "dispersión amplia, patrones curvos o sesgo sistemático.",
                        "revisar capacidad predictiva, especificación del modelo y posibles términos adicionales."
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

            self$results$transformationsGuide$setContent(html_guide(tr("Exploratory transformations", "Transformaciones exploratorias"), "regression", "transformationsGuide"))

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
                        tr("Not computed", "No calculado"), tr("Not computed", "No calculado")
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
                        tr("Not computed", "No calculado"), tr("Not computed", "No calculado")
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
                    tr("Reference", "Referencia")
                } else if (normality_total > 0 &&
                           normality_ok >= ceiling(normality_total / 2)) {
                    tr("Possible improvement", "Mejora posible")
                } else {
                    tr("No clear improvement", "Sin mejora clara")
                }

                fit_eval <- if (label == "Original") {
                    tr("Reference", "Referencia")
                } else if (variance_ok) {
                    tr("Compatible with constant variance", "Compatible con varianza constante")
                } else {
                    tr("Review residual variance", "Revisar varianza residual")
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
                tr("Always applicable", "Siempre aplicable"),
                tr("Reference model", "Modelo de referencia")
            )

            if (all(y_original > 0, na.rm = TRUE)) {
                eval_transformed_model(
                    log(y_original),
                    paste0("log(", dep, ")"),
                    paste0(tr("Applicable: ", "Aplicable: "), dep, " > 0"),
                    tr("Marked/severe positive skewness", "Asimetría positiva marcada/severa")
                )
            } else {
                add_transformation(
                    paste0("log(", dep, ")"),
                    paste0("No aplicable: ", dep, " debe ser > 0"),
                    tr("Marked/severe positive skewness", "Asimetría positiva marcada/severa"),
                    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                    NA_real_, NA_real_,
                    tr("Not computed", "No calculado"), tr("Not computed", "No calculado")
                )
            }

            if (all(y_original >= 0, na.rm = TRUE)) {
                eval_transformed_model(
                    sqrt(y_original),
                    paste0("sqrt(", dep, ")"),
                    paste0(tr("Applicable: ", "Aplicable: "), dep, " >= 0"),
                    tr("Mild/moderate positive skewness", "Asimetría positiva leve/moderada")
                )
            } else {
                add_transformation(
                    paste0("sqrt(", dep, ")"),
                    paste0("No aplicable: ", dep, " debe ser >= 0"),
                    tr("Mild/moderate positive skewness", "Asimetría positiva leve/moderada"),
                    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                    NA_real_, NA_real_,
                    tr("Not computed", "No calculado"), tr("Not computed", "No calculado")
                )
            }

            if (all(y_original != 0, na.rm = TRUE)) {
                eval_transformed_model(
                    1 / y_original,
                    paste0("1/", dep),
                    paste0(tr("Applicable: ", "Aplicable: "), dep, " != 0"),
                    tr("Very severe positive skewness", "Asimetría positiva muy severa")
                )
            } else {
                add_transformation(
                    paste0("1/", dep),
                    paste0("No aplicable: ", dep, " no debe contener ceros"),
                    tr("Very severe positive skewness", "Asimetría positiva muy severa"),
                    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                    NA_real_, NA_real_,
                    tr("Not computed", "No calculado"), tr("Not computed", "No calculado")
                )
            }

            transformation_value <- function(index, field, digits = 3) {
                if (length(trans_results) < index)
                    return(tr("Not computed", "No calculado"))

                value <- trans_results[[index]][[field]]

                if (is.null(value))
                    return(tr("Not computed", "No calculado"))

                if (is.numeric(value)) {
                    value <- clean_num(value)

                    if (is.na(value))
                        return(tr("Not computed", "No calculado"))

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
                tr("Applicability", "Aplicabilidad"),
                "applicability"
            )

            add_transformation_row(
                self$results$transformationNormality,
                "norm_criterion",
                tr("Suggested criterion", "Criterio sugerido"),
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
                tr("Evaluation", "Evaluación"),
                "normEval"
            )

            add_transformation_row(
                self$results$transformationFit,
                "fit_applicability",
                tr("Applicability", "Aplicabilidad"),
                "applicability"
            )

            add_transformation_row(
                self$results$transformationFit,
                "fit_criterion",
                tr("Suggested criterion", "Criterio sugerido"),
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
                tr("Adjusted R²", "R² ajustado"),
                "adjR2"
            )

            add_transformation_row(
                self$results$transformationFit,
                "fit_eval",
                tr("Evaluation", "Evaluación"),
                "fitEval"
            )

            # ------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Robust and resampling options.
            # ES: Opciones robustas y de remuestreo
            # -----------------------------------------------------------------------------
            # ------------------------------------------------------------

            self$results$robustOptionsGuide$setContent(html_guide(tr("Robust and resampling options", "Opciones robustas y de remuestreo"), "regression", "robustOptionsGuide"))

            robust_text <- character()

            if (exists("homo_problem") && isTRUE(homo_problem)) {
                robust_text <- c(
                    robust_text,
                    if (lang == "es") {
                        paste(
                            "Varianza residual no constante.",
                            "Una o más pruebas sugieren heterocedasticidad o cambios en la varianza de los residuos.",
                            "Esto no invalida automáticamente el modelo, pero puede afectar errores estándar, intervalos",
                            "de confianza y valores p.",
                            "Considere errores estándar robustos HC3, transformación de la dependiente o un modelo",
                            "alternativo si la conclusión cambia."
                        )
                    } else {
                        paste(
                            "Nonconstant residual variance.",
                            "One or more tests suggest heteroscedasticity or changes in residual variance.",
                            "This does not automatically invalidate the model, but it may affect standard errors,",
                            "confidence intervals and p-values.",
                            "Consider HC3 robust standard errors, transforming the dependent variable or an alternative",
                            "model if the conclusion changes."
                        )
                    }
                )
            }

            if (exists("linearity_problem") && isTRUE(linearity_problem)) {
                robust_text <- c(
                    robust_text,
                    if (lang == "es") {
                        paste(
                            "Posible no linealidad.",
                            "Algún diagnóstico sugiere que la relación entre predictores y respuesta podría no ser",
                            "completamente lineal.",
                            "Revise gráficos de residuos y considere términos polinómicos, transformaciones, splines",
                            "o un modelo no lineal si la curvatura tiene sentido sustantivo."
                        )
                    } else {
                        paste(
                            "Possible nonlinearity.",
                            "At least one diagnostic suggests that the relationship between predictors and the outcome",
                            "may not be fully linear.",
                            "Review residual plots and consider polynomial terms, transformations, splines or a nonlinear",
                            "model if the curvature is substantively meaningful."
                        )
                    }
                )
            }

            if (exists("influence_problem") && isTRUE(influence_problem)) {
                robust_text <- c(
                    robust_text,
                    if (lang == "es") {
                        paste(
                            "Casos atípicos o influyentes.",
                            "Algún caso activó criterios numéricos de influencia o atipicidad.",
                            "La recomendación no es eliminarlo automáticamente, sino revisar el dato original, documentar",
                            "su plausibilidad y comparar análisis de sensibilidad con y sin esos casos."
                        )
                    } else {
                        paste(
                            "Outlying or influential cases.",
                            "At least one case activated numerical influence or outlier criteria.",
                            "The recommendation is not automatic deletion, but reviewing the original value, documenting",
                            "its plausibility and comparing sensitivity analyses with and without those cases."
                        )
                    }
                )
            }

            if (length(robust_text) == 0) {
                robust_text <- c(
                    robust_text,
                    if (lang == "es") {
                        paste(
                            "No se detectaron señales fuertes que obliguen a abandonar el modelo lineal clásico.",
                            "Aun así, la decisión debe considerar gráficos, diseño del estudio, tamaño muestral y sentido",
                            "sustantivo de los coeficientes."
                        )
                    } else {
                        paste(
                            "No strong signals were detected that force abandoning the classical linear model.",
                            "Even so, the decision should consider plots, study design, sample size and the substantive",
                            "meaning of the coefficients."
                        )
                    }
                )
            }

            self$results$robustOptionsText$setContent(
                html_block(
                    tr("Methodological interpretation of robust options",
                       "Interpretación metodológica de opciones robustas"),
                    robust_text
                )
            )

            recommendation <- paste(
                tr("The linear regression model can be interpreted cautiously,",
                   "El modelo de regresión lineal puede interpretarse con cautela,"),
                tr("while reviewing the reported diagnostics.",
                   "revisando los diagnósticos reportados.")
            )

            if (homo_problem) {
                recommendation <- paste(
                    tr("There is evidence of nonconstant residual variance; consider robust standard errors,",
                       "Hay evidencia de varianza residual no constante; considerar errores estándar"),
                    tr("transforming the dependent variable, or using an alternative model.",
                       "robustos, transformación de la dependiente o modelo alternativo.")
                )
            }

            if (auto_problem) {
                recommendation <- paste(
                    tr("There are signs of autocorrelation or residual patterning; consider",
                       "Hay señales de autocorrelación o patrón en residuos; considerar"),
                    tr("temporal structure, HAC/Newey-West errors, or GLS models.",
                       "estructura temporal, errores HAC/Newey-West o modelos GLS.")
                )
            }

            if (severe_col_problem) {
                recommendation <- paste(
                    tr("There is severe multicollinearity; review redundant predictors",
                       "Hay multicolinealidad severa; revisar predictores redundantes"),
                    tr("before interpreting coefficients.",
                       "antes de interpretar coeficientes.")
                )
            }

            if (influence_problem && !homo_problem && !auto_problem &&
                !severe_col_problem) {
                recommendation <- paste(
                    tr("There are outlying or influential cases; inspect them before",
                       "Existen casos atípicos o influyentes; inspeccionarlos antes de"),
                    tr("making decisions about the model.",
                       "tomar decisiones sobre el modelo.")
                )
            }

            self$results$notes$setContent(
                html_block(
                    tr("Notes and recommendation", "Notas y recomendación"),
                    c(
                        paste0(if (lang == "es") "Decisión sugerida: " else "Suggested decision: ", recommendation),
                        tr("Significance codes: * p < .05, ** p < .01, *** p < .001.",
                           "Códigos de significancia: * p < .05, ** p < .01, *** p < .001."),
                        tr("Statistical symbols are kept in international format: n, R², p, df, VIF.",
                           "Los símbolos estadísticos se mantienen en formato internacional: n, R², p, df, VIF.")
                    )
                )

            )

        },

        .requirePlotData = function(image) {
            if (!isTRUE(self$options$showPlots))
                return(FALSE)

            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                image$setError("The ggplot2 package is required to draw diagnostic plots.")
                return(FALSE)
            }

            if (is.null(private$.plotData) || nrow(private$.plotData) == 0) {
                image$setError("No diagnostic plot data are available.")
                return(FALSE)
            }

            TRUE
        },

        .requireCorrPlotData = function(image) {
            if (!isTRUE(self$options$showPlots))
                return(FALSE)

            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                image$setError("The ggplot2 package is required to draw diagnostic plots.")
                return(FALSE)
            }

            if (is.null(private$.corrPairResults) || length(private$.corrPairResults) == 0) {
                image$setError("No hay suficientes variables numéricas para este gráfico.")
                return(FALSE)
            }

            TRUE
        },

        .plotTr = function(en, es) {
            lang <- .al_normalize_lang(self$options$reportLang)

            if (identical(lang, "es"))
                es
            else
                en
        },

        .plotPalette = function() {
            # Base palette + series palette: identical shape and logic in
            # regCheck, logCheck, and timeCheck, consolidated in
            # shared-helpers.R (.al_plot_palette_base /
            # .al_plot_series_palette). fullColor uses Variant A per
            # Archie's decision, Aug 2026 (see doc comment on
            # .al_plot_palette_base).
            # ES: paleta base + paleta de series idénticas en regCheck,
            # logCheck y timeCheck, consolidadas en shared-helpers.R.
            # fullColor usa la Variante A por decisión de Archie, agosto
            # 2026.
            style <- tryCatch(
                self$options$plotStyle,
                error = function(e) "clean"
            )

            if (is.null(style) || length(style) == 0 || !nzchar(style))
                style <- "clean"

            base <- .al_plot_palette_base(style)

            palette_choice <- tryCatch(self$options$plotPalette, error = function(e) "blueOrange")
            if (is.null(palette_choice) || length(palette_choice) == 0 || !nzchar(palette_choice))
                palette_choice <- "blueOrange"

            base$series <- .al_plot_series_palette(palette_choice)

            base
        },

        .plotTheme = function() {
            style <- tryCatch(
                self$options$plotStyle,
                error = function(e) "clean"
            )

            if (is.null(style) || length(style) == 0 || !nzchar(style))
                style <- "clean"

            base <- if (identical(style, "bw")) {
                ggplot2::theme_bw(base_size = 10.5)
            } else if (identical(style, "contrast")) {
                ggplot2::theme_classic(base_size = 10.5)
            } else {
                ggplot2::theme_minimal(base_size = 10.5)
            }

            base +
                ggplot2::theme(
                    plot.title = ggplot2::element_blank(),
                    plot.subtitle = ggplot2::element_text(size = 9.5),
                    axis.title = ggplot2::element_text(size = 9.5),
                    axis.text = ggplot2::element_text(size = 8.5),
                    legend.title = ggplot2::element_text(size = 9),
                    legend.text = ggplot2::element_text(size = 8.5),
                    legend.position = "bottom",
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_line(linewidth = 0.25),
                    plot.margin = ggplot2::margin(4, 6, 4, 6)
                )
        },

        .addSmoother = function(plot, x, y, smoother = "loess", showBand = FALSE) {
            if (identical(smoother, "loess")) {
                plot + ggplot2::geom_smooth(
                    ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                    method = "loess",
                    se = isTRUE(showBand),
                    linewidth = 0.6,
                    color = private$.plotPalette()$smooth,
                    fill = private$.plotPalette()$smooth
                )
            } else if (identical(smoother, "linear")) {
                plot + ggplot2::geom_smooth(
                    ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                    method = "lm",
                    se = isTRUE(showBand),
                    linewidth = 0.6,
                    color = private$.plotPalette()$smooth,
                    fill = private$.plotPalette()$smooth
                )
            } else {
                plot
            }
        },

        .labelPlotCases = function(d) {
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
            p_count <- private$.pModel
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

        .plotResidualsFitted = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.plotData
            d <- d[is.finite(d$fitted) & is.finite(d$residual), , drop = FALSE]

            show_ref <- tryCatch(isTRUE(self$options$linRefLine), error = function(e) TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = residual))

            if (show_ref) {
                plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                      color = private$.plotPalette()$ref)
            }

            plot <- plot +
                ggplot2::geom_point(alpha = 0.75, size = 1.6,
                                    color = private$.plotPalette()$point) +
                ggplot2::labs(
                    x = private$.plotTr("Fitted values", "Valores ajustados"),
                    y = private$.plotTr("Residuals", "Residuos")
                ) +
                private$.plotTheme()

            lin_smoother <- tryCatch(self$options$linSmoother, error = function(e) "loess")
            lin_band <- tryCatch(isTRUE(self$options$linShowBand), error = function(e) FALSE)
            plot <- private$.addSmoother(plot, "fitted", "residual", lin_smoother, lin_band)
            print(plot)
        },

        .plotPredictorEffects = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.predictorEffectsData

            if (is.null(d) || nrow(d) == 0) {
                image$setError("No numeric predictor effects are available.")
                return()
            }

            d <- d[is.finite(d$predictorZ) & is.finite(d$predicted), , drop = FALSE]

            if (nrow(d) == 0) {
                image$setError("No finite predictor effects are available.")
                return()
            }

            plot <- ggplot2::ggplot(
                    d,
                    ggplot2::aes(x = predictorZ, y = predicted, color = predictor)
                ) +
                ggplot2::geom_line(linewidth = 0.8) +
                ggplot2::geom_vline(
                    xintercept = 0,
                    linetype = "dashed",
                    color = private$.plotPalette()$ref
                ) +
                ggplot2::labs(
                    x = private$.plotTr(
                        "Predictor value in standard deviations",
                        "Valor del predictor en desviaciones estándar"
                    ),
                    y = private$.plotTr(
                        "Predicted dependent variable",
                        "Variable dependiente predicha"
                    ),
                    color = private$.plotTr("Predictor", "Predictor")
                ) +
                ggplot2::scale_color_manual(
                    values = {
                        predictors <- unique(as.character(d$predictor))
                        colors <- private$.plotPalette()$series

                        if (is.null(colors) || length(colors) == 0)
                            colors <- grDevices::rainbow(max(1, length(predictors)))

                        colors <- rep(colors, length.out = max(1, length(predictors)))
                        stats::setNames(colors, predictors)
                    }
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotQQResiduals = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.plotData
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
                    fill = private$.plotPalette()$line
                )
            }

            plot <- plot +
                ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.6,
                                      color = private$.plotPalette()$line) +
                ggplot2::geom_point(
                    ggplot2::aes(color = isExtreme),
                    alpha = 0.75, size = 1.6, show.legend = FALSE
                ) +
                ggplot2::scale_color_manual(values = c(
                    "FALSE" = private$.plotPalette()$point,
                    "TRUE" = private$.plotPalette()$alert
                )) +
                ggplot2::labs(
                    x = private$.plotTr("Theoretical quantiles", "Cuantiles teóricos"),
                    y = private$.plotTr("Standardized residuals", "Residuos estandarizados")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotResidualHistogram = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.plotData
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
                                          fill = private$.plotPalette()$fill,
                                          color = private$.plotPalette()$line) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                                    color = private$.plotPalette()$ref)

            if (show_curve && nrow(d) >= 3) {
                bin_width <- diff(range(d$stdResidual)) / n_bins
                mu <- mean(d$stdResidual)
                sigma <- stats::sd(d$stdResidual)
                plot <- plot + ggplot2::stat_function(
                    fun = function(x) stats::dnorm(x, mean = mu, sd = sigma) * nrow(d) * bin_width,
                    color = private$.plotPalette()$smooth,
                    linewidth = 0.7
                )
            }

            plot <- plot +
                ggplot2::labs(
                    x = private$.plotTr("Standardized residuals", "Residuos estandarizados"),
                    y = private$.plotTr("Count", "Frecuencia")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotResidualNormalCurve = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.plotData
            d <- d[is.finite(d$stdResidual), , drop = FALSE]

            if (nrow(d) < 3) {
                image$setError("At least three residuals are required for this plot.")
                return()
            }

            x_min <- min(-4, min(d$stdResidual, na.rm = TRUE))
            x_max <- max(4, max(d$stdResidual, na.rm = TRUE))
            x_grid <- seq(x_min, x_max, length.out = 300)

            normal_df <- data.frame(
                stdResidual = x_grid,
                density = stats::dnorm(x_grid, mean = 0, sd = 1),
                curve = private$.plotTr("Theoretical normal N(0, 1)", "Normal teórica N(0, 1)"),
                stringsAsFactors = FALSE
            )

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = stdResidual)) +
                ggplot2::geom_density(
                    linewidth = 0.8,
                    color = private$.plotPalette()$smooth
                ) +
                ggplot2::geom_line(
                    data = normal_df,
                    ggplot2::aes(x = stdResidual, y = density, linetype = curve),
                    linewidth = 0.6,
                    color = private$.plotPalette()$alert
                ) +
                ggplot2::geom_vline(
                    xintercept = 0,
                    linetype = "dashed",
                    color = private$.plotPalette()$ref
                ) +
                ggplot2::scale_linetype_manual(values = "dashed", name = NULL) +
                ggplot2::labs(
                    x = private$.plotTr("Standardized residuals", "Residuos estandarizados"),
                    y = private$.plotTr("Density", "Densidad")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotScaleLocation = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.plotData
            d <- d[is.finite(d$fitted) & is.finite(d$absStdResidual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = absStdResidual)) +
                ggplot2::geom_point(alpha = 0.75, size = 1.6,
                                    color = private$.plotPalette()$point) +
                ggplot2::labs(
                    x = private$.plotTr("Fitted values", "Valores ajustados"),
                    y = private$.plotTr("sqrt(|standardized residuals|)", "sqrt(|residuos estandarizados|)")
                ) +
                private$.plotTheme()

            plot <- private$.addSmoother(
                plot, "fitted", "absStdResidual",
                tryCatch(self$options$homoSmoother, error = function(e) "loess"),
                tryCatch(isTRUE(self$options$homoShowBand), error = function(e) FALSE)
            )
            print(plot)
        },

        .plotNumericBoxplots = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.numericBoxplotData

            if (is.null(d) || nrow(d) == 0) {
                image$setError("No numeric variables are available for boxplots.")
                return()
            }

            d <- d[is.finite(d$value), , drop = FALSE]

            if (nrow(d) == 0) {
                image$setError("No finite numeric values are available for boxplots.")
                return()
            }

            lang <- .al_normalize_lang(self$options$reportLang)
            lab <- function(en, es) {
                if (lang == "es")
                    es
                else
                    en
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
                    outlier.colour = private$.plotPalette()$alert,
                    outlier.alpha = 0.85,
                    outlier.size = 1.6,
                    fill = private$.plotPalette()$fill,
                    color = private$.plotPalette()$line
                ) +
                private$.plotTheme()

            if (similar_scale) {
                plot <- base_plot +
                    ggplot2::labs(
                        title = lab(
                            "Boxplots of dependent variable and numeric predictors",
                            "Boxplots de la variable dependiente y predictores numéricos"
                        ),
                        subtitle = lab(
                            "Common Y scale for direct comparison across variables.",
                            "Escala Y común para comparación directa entre variables."
                        ),
                        x = lab("Variable", "Variable"),
                        y = lab("Observed values", "Valores observados")
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
                        title = lab(
                            "Boxplots of dependent variable and numeric predictors",
                            "Boxplots de la variable dependiente y predictores numéricos"
                        ),
                        subtitle = lab(
                            "Free Y scales are used because the variables have notably different magnitudes.",
                            "Se usan escalas Y libres porque las variables tienen magnitudes claramente diferentes."
                        ),
                        x = NULL,
                        y = lab(
                            "Observed values in original scale",
                            "Valores observados en escala original"
                        )
                    ) +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        strip.text = ggplot2::element_text(face = "bold")
                    )
            }

            print(plot)
        },

        .plotResidualsLeverage = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.plotData
            d <- d[is.finite(d$leverage) & is.finite(d$studResidual), , drop = FALSE]

            p_count <- private$.pModel
            if (is.null(p_count) || !is.finite(p_count) || p_count < 1)
                p_count <- max(1, length(self$options$covs) + length(self$options$factors) + 1)
            lev_cut <- 2 * p_count / max(1, nrow(d))

            show_threshold <- tryCatch(isTRUE(self$options$influenceShowThreshold), error = function(e) TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = leverage, y = studResidual))

            if (show_threshold) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = c(-3, 0, 3), linetype = "dashed",
                                          color = private$.plotPalette()$ref) +
                    ggplot2::geom_vline(xintercept = lev_cut, linetype = "dashed",
                                        color = private$.plotPalette()$alert)
            } else {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                          color = private$.plotPalette()$ref)
            }

            plot <- plot +
                ggplot2::geom_point(ggplot2::aes(size = cooksD), alpha = 0.75,
                                    color = private$.plotPalette()$point) +
                ggplot2::scale_size_continuous(name = "Cook's D", range = c(1.2, 4)) +
                ggplot2::labs(
                    x = private$.plotTr("Leverage", "Leverage"),
                    y = private$.plotTr("Studentized residuals", "Residuos studentizados")
                ) +
                private$.plotTheme()

            lab <- private$.labelPlotCases(d)

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
                        color = private$.plotPalette()$alert
                    )
            }

            print(plot)
        },

        .plotCooksD = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.plotData
            d <- d[is.finite(d$case) & is.finite(d$cooksD), , drop = FALSE]
            cut <- 4 / max(1, nrow(d))
            show_threshold <- tryCatch(isTRUE(self$options$influenceShowThreshold), error = function(e) TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = case, y = cooksD)) +
                ggplot2::geom_col(alpha = 0.85,
                                    fill = private$.plotPalette()$fill,
                                    color = private$.plotPalette()$line)

            if (show_threshold) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = cut, linetype = "dashed",
                                        color = private$.plotPalette()$alert)
            }

            plot <- plot +
                ggplot2::labs(
                    x = private$.plotTr("Case", "Caso"),
                    y = private$.plotTr("Cook's D", "Cook's D")
                ) +
                private$.plotTheme()

            lab <- private$.labelPlotCases(d)

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
                        color = private$.plotPalette()$alert
                    )
            }

            print(plot)
        },

        .plotCorrelationIndividual = function(image, ...) {
            if (!private$.requireCorrPlotData(image))
                return()

            pr <- private$.corrPairResults
            dat <- private$.corrData
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
                image$setError(private$.plotTr(
                    "Not enough data to draw this plot.",
                    "No hay suficientes datos para este gráfico."
                ))
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
                ) +
                ggplot2::scale_color_manual(
                    values = c(
                        "FALSE" = private$.plotPalette()$point,
                        "TRUE" = private$.plotPalette()$alert
                    ),
                    labels = c(
                        "FALSE" = private$.plotTr("Concordant", "Concordante"),
                        "TRUE" = private$.plotTr("Notable gap", "Brecha notable")
                    ),
                    name = private$.plotTr("Pearson/dCor gap", "Brecha Pearson/dCor")
                )

            if (show_fit) {
                plot <- plot +
                    ggplot2::geom_smooth(
                        method = "lm", se = FALSE, linewidth = 0.55, linetype = "dashed",
                        color = private$.plotPalette()$line
                    ) +
                    ggplot2::geom_smooth(
                        method = "loess", se = FALSE, linewidth = 0.6,
                        color = private$.plotPalette()$smooth
                    )
            }

            plot <- plot +
                ggplot2::facet_wrap(~pairLabel, scales = "free") +
                ggplot2::labs(
                    x = private$.plotTr("Standardized value (variable 1)", "Valor estandarizado (variable 1)"),
                    y = private$.plotTr("Standardized value (variable 2)", "Valor estandarizado (variable 2)")
                ) +
                private$.plotTheme() +
                ggplot2::theme(strip.text = ggplot2::element_text(size = 8))

            print(plot)
        },

        .plotCorrelationComparative = function(image, ...) {
            if (!private$.requireCorrPlotData(image))
                return()

            pr <- private$.corrPairResults
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
                image$setError(private$.plotTr(
                    "Not enough data to draw this plot.",
                    "No hay suficientes datos para este gráfico."
                ))
                return()
            }

            d <- d[order(d$dcor), , drop = FALSE]
            d$pairLabel <- factor(d$pairLabel, levels = d$pairLabel)
            if (!highlight) d$discordant <- FALSE

            alert_col <- private$.plotPalette()$alert
            ref_col <- private$.plotPalette()$ref
            series <- private$.plotPalette()$series
            pearson_col <- series[1]
            dcor_col <- if (length(series) >= 2) series[2] else private$.plotPalette()$smooth

            if (identical(style, "bar")) {
                long <- rbind(
                    data.frame(pairLabel = d$pairLabel, metric = "Pearson |r|", value = d$pearson),
                    data.frame(pairLabel = d$pairLabel, metric = "dCor", value = d$dcor)
                )
                plot <- ggplot2::ggplot(long, ggplot2::aes(x = pairLabel, y = value, fill = metric)) +
                    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6) +
                    ggplot2::scale_fill_manual(
                        values = c("Pearson |r|" = pearson_col, "dCor" = dcor_col),
                        name = NULL
                    ) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        x = NULL,
                        y = private$.plotTr("Coefficient magnitude", "Magnitud del coeficiente")
                    ) +
                    private$.plotTheme()
            } else {
                plot <- ggplot2::ggplot(d, ggplot2::aes(y = pairLabel)) +
                    ggplot2::geom_segment(
                        ggplot2::aes(x = pearson, xend = dcor, yend = pairLabel),
                        color = ifelse(d$discordant, alert_col, ref_col),
                        linewidth = ifelse(d$discordant, 1, 0.6)
                    ) +
                    ggplot2::geom_point(ggplot2::aes(x = pearson, color = "Pearson |r|"), size = 2.4) +
                    ggplot2::geom_point(ggplot2::aes(x = dcor, color = "dCor"), size = 2.4) +
                    ggplot2::scale_color_manual(
                        values = c("Pearson |r|" = pearson_col, "dCor" = dcor_col),
                        name = NULL
                    ) +
                    ggplot2::labs(
                        x = private$.plotTr("Coefficient magnitude", "Magnitud del coeficiente"),
                        y = NULL
                    ) +
                    private$.plotTheme()
            }

            print(plot)
        },

        .plotObservedPredicted = function(image, ...) {
            if (!private$.requirePlotData(image))
                return()

            d <- private$.plotData
            d <- d[is.finite(d$observed) & is.finite(d$fitted), , drop = FALSE]
            lims <- range(c(d$observed, d$fitted), na.rm = TRUE)

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = observed)) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                                      color = private$.plotPalette()$ref) +
                ggplot2::geom_point(alpha = 0.75, size = 1.6,
                                    color = private$.plotPalette()$point) +
                ggplot2::coord_equal(xlim = lims, ylim = lims) +
                ggplot2::labs(
                    x = private$.plotTr("Predicted values", "Valores predichos"),
                    y = private$.plotTr("Observed values", "Valores observados")
                ) +
                private$.plotTheme()

            print(plot)
        }
    )
)
