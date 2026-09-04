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
# Path Analysis & Structural Validation.
# ES: Análisis de Ruta y Validación Estructural.
#
# This file implements pathCheck: a recursive path-analysis module that
# estimates one OLS regression per endogenous variable and reports the
# methodological assumption diagnostics that decide whether the resulting
# path coefficients are trustworthy (multivariate outliers, residual
# normality and homoscedasticity per equation, multicollinearity, Mardia
# multivariate normality, Pearson/dCor/copula-entropy dependence structure,
# and sample-size adequacy).
#
# ES: Este archivo implementa pathCheck: un módulo de análisis de rutas
# recursivo que estima una regresión OLS por variable endógena y reporta
# los diagnósticos de supuestos metodológicos que deciden si los
# coeficientes de ruta resultantes son confiables (atípicos multivariados,
# normalidad y homoscedasticidad de residuos por ecuación, multicolinealidad,
# normalidad multivariada de Mardia, estructura de dependencia
# Pearson/dCor/entropía copular, y adecuación del tamaño de muestra).
#
# Responsibilities
# 1. Let the user specify the path model live (variables + directed
#    relations) and validate its structure (no duplicate/self/cyclic
#    relations, every dependent has a predictor, the model is connected)
#    before allowing it to be finalized.
# 2. Once finalized, fit one lm() per endogenous variable and assemble the
#    path coefficients, R², and indirect/total effects.
# 3. Compute and report the full assumption-diagnostic battery for the
#    fitted equations.
# 4. Render the path diagram (SVG) and the per-equation residual
#    diagnostic plots.
# 5. Assemble a final methodological verdict that summarizes every
#    diagnostic area at a glance.
#
# ES: Responsabilidades
# 1. Permitir que el usuario especifique el modelo de ruta en vivo
#    (variables + relaciones dirigidas) y validar su estructura (sin
#    relaciones duplicadas/autolazos/ciclos, cada dependiente con
#    predictor, modelo conectado) antes de permitir que se finalice.
# 2. Una vez finalizado, ajustar un lm() por variable endógena y ensamblar
#    los coeficientes de ruta, R² y efectos indirectos/totales.
# 3. Calcular y reportar la batería completa de diagnósticos de supuestos
#    para las ecuaciones ajustadas.
# 4. Renderizar el diagrama de ruta (SVG) y los gráficos de diagnóstico de
#    residuos por ecuación.
# 5. Ensamblar un veredicto metodológico final que resuma cada área
#    diagnóstica de un vistazo.
#
# Workflow
# 1. Specify: the user builds the model (vars + relations); the model
#    structure is validated live, on every change.
# 2. Finalize: once isFinalized is checked and the model is valid, the
#    model locks and estimation runs.
# 3. Estimate: fit one lm() per endogenous variable (recursive system).
# 4. Diagnose: compute outliers, normality, homoscedasticity,
#    multicollinearity, Mardia, correlation structure, and sample-size
#    adequacy from the fitted equations.
# 5. Interpret: build the applied-interpretation text for every diagnostic
#    area, in the user's selected report language.
# 6. Summarize: assemble the final model-diagnostic verdict table.
#
# ES: Flujo de trabajo
# 1. Especificar: el usuario construye el modelo (variables + relaciones);
#    la estructura del modelo se valida en vivo, en cada cambio.
# 2. Finalizar: una vez marcado isFinalized y con el modelo válido, el
#    modelo se bloquea y corre la estimación.
# 3. Estimar: ajustar un lm() por variable endógena (sistema recursivo).
# 4. Diagnosticar: calcular atípicos, normalidad, homoscedasticidad,
#    multicolinealidad, Mardia, estructura de correlación y adecuación del
#    tamaño de muestra a partir de las ecuaciones ajustadas.
# 5. Interpretar: construir el texto de interpretación aplicada para cada
#    área diagnóstica, en el idioma de informe seleccionado por el
#    usuario.
# 6. Resumir: ensamblar la tabla de veredicto final de diagnóstico del
#    modelo.
#
# Bilingual support (reportLang) uses the same convention as the rest of
# the suite (.al_normalize_lang() / tr(), defined in texts.R).
# ES: El soporte bilingüe (reportLang) usa la misma convención que el resto
# de la suite (.al_normalize_lang() / tr(), definidas en texts.R).
# -----------------------------------------------------------------------------

pathCheckClass <- R6::R6Class(
    "pathCheckClass",
    inherit = pathCheckBase,
    private = list(

        .pathFits = NULL,
        .edgeStats = NULL,
        .pathEdges = NULL,
        .pathVars = NULL,

        # -----------------------------------------------------------------------------
        # Internal utilities.
        # ES: Utilidades internas.
        # -----------------------------------------------------------------------------

        .plotTr = function(en, es) {
            lang <- .al_normalize_lang(self$options$reportLang)
            if (identical(lang, "es")) es else en
        },

        .plotPalette = function() {
            style <- tryCatch(self$options$plotStyle, error = function(e) "clean")
            if (is.null(style) || length(style) == 0 || !nzchar(style)) style <- "clean"

            if (identical(style, "bw")) {
                return(list(nodeFill = "gray90", nodeLine = "gray20", text = "gray10",
                            edgeSig = "gray10", edgeNs = "gray65", fill = "gray70",
                            line = "gray10"))
            }
            if (identical(style, "contrast")) {
                return(list(nodeFill = "#FFFFFF", nodeLine = "#000000", text = "#000000",
                            edgeSig = "#000000", edgeNs = "#999999", fill = "#BDBDBD",
                            line = "#000000"))
            }
            if (identical(style, "fullColor")) {
                return(list(nodeFill = "#EDE7F6", nodeLine = "#5E35B1", text = "#5E35B1",
                            edgeSig = "#253494", edgeNs = "#7FC8E8", fill = "#A6CEE3",
                            line = "#253494"))
            }
            list(nodeFill = "#F1E9FB", nodeLine = "#7B4FA6", text = "#7B4FA6",
                 edgeSig = "#312B81", edgeNs = "#8FC7E8", fill = "#BDBDBD", line = "#2B2B2B")
        },

        .plotCategoricalPalette = function(n) {
            n <- max(1, as.integer(n))
            pal_name <- tryCatch(self$options$plotPalette, error = function(e) "blueOrange")
            if (is.null(pal_name) || length(pal_name) == 0 || !nzchar(pal_name))
                pal_name <- "blueOrange"

            if (identical(pal_name, "viridis")) {
                cols <- grDevices::hcl.colors(n, palette = "Viridis")
            } else if (identical(pal_name, "greyscale")) {
                cols <- grDevices::grey.colors(n, start = 0.15, end = 0.75)
            } else if (identical(pal_name, "colorblind")) {
                base <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442", "#56B4E9", "#E69F00", "#000000")
                cols <- rep(base, length.out = n)
            } else {
                base <- c("#2C7FB8", "#D95F0E", "#41AB5D", "#8856A7", "#DD3497", "#636363", "#238B45", "#B15928")
                cols <- rep(base, length.out = n)
            }
            cols
        },

        .plotTheme = function() {
            style <- tryCatch(self$options$plotStyle, error = function(e) "clean")
            if (is.null(style) || length(style) == 0 || !nzchar(style)) style <- "clean"
            base <- if (identical(style, "bw")) {
                ggplot2::theme_bw(base_size = 10.5)
            } else if (identical(style, "contrast")) {
                ggplot2::theme_classic(base_size = 10.5)
            } else {
                ggplot2::theme_minimal(base_size = 10.5)
            }
            base + ggplot2::theme(
                plot.title = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 9.5),
                axis.text = ggplot2::element_text(size = 8.5),
                strip.text = ggplot2::element_text(size = 9.5, face = "bold")
            )
        },

        .buildEdges = function(relations) {
            # Cada 'pair' trae i1 (predictor) e i2 (dependiente).
            edges <- list()
            for (pair in relations) {
                pred <- pair$i1
                dep  <- pair$i2
                if (is.null(pred) || is.null(dep) || is.na(pred) || is.na(dep))
                    next()
                edges[[length(edges) + 1]] <- list(pred = pred, dep = dep)
            }
            edges
        },

        # Detección de ciclos en el grafo dirigido pred -> dep (DFS clásico)
        .hasCycle = function(edges) {
            if (length(edges) == 0) return(FALSE)
            nodes <- unique(unlist(lapply(edges, function(e) c(e$pred, e$dep))))
            adj <- stats::setNames(vector("list", length(nodes)), nodes)
            for (n in nodes) adj[[n]] <- character(0)
            for (e in edges) adj[[e$pred]] <- c(adj[[e$pred]], e$dep)

            WHITE <- 0L; GRAY <- 1L; BLACK <- 2L
            color <- stats::setNames(rep(WHITE, length(nodes)), nodes)

            visit <- function(u) {
                color[u] <<- GRAY
                for (v in adj[[u]]) {
                    if (color[v] == GRAY) return(TRUE)
                    if (color[v] == WHITE && visit(v)) return(TRUE)
                }
                color[u] <<- BLACK
                FALSE
            }

            for (n in nodes) {
                if (color[n] == WHITE) {
                    if (visit(n)) return(TRUE)
                }
            }
            FALSE
        },

        # Componentes conexas del grafo no dirigido (para "modelo conectado")
        .connectedComponents = function(vars, edges) {
            parent <- stats::setNames(vars, vars)
            find <- function(x) { while (parent[x] != x) x <- parent[x]; x }
            union <- function(a, b) {
                ra <- find(a); rb <- find(b)
                if (ra != rb) parent[ra] <<- rb
            }
            for (e in edges) {
                if (e$pred %in% vars && e$dep %in% vars) union(e$pred, e$dep)
            }
            roots <- vapply(vars, find, character(1))
            length(unique(roots))
        },

        # Correlación de distancia (dCor, Székely, Rizzo & Bakirov, 2007), base R puro.
        .dCor = function(x, y) {
            x <- as.numeric(x); y <- as.numeric(y)
            n <- length(x)
            distMatrixX <- as.matrix(stats::dist(x))
            distMatrixY <- as.matrix(stats::dist(y))
            ra <- rowMeans(distMatrixX); rb <- rowMeans(distMatrixY)
            A <- distMatrixX - matrix(ra, n, n) - matrix(ra, n, n, byrow = TRUE) + mean(distMatrixX)
            B <- distMatrixY - matrix(rb, n, n) - matrix(rb, n, n, byrow = TRUE) + mean(distMatrixY)
            dcov2  <- mean(A * B)
            dvarX2 <- mean(A * A)
            dvarY2 <- mean(B * B)
            denom  <- dvarX2 * dvarY2
            # NA (not 0): an invalid/zero distance-variance denominator
            # means dCor is undefined for this pair (e.g. a constant
            # variable), not that dependence was measured as exactly zero.
            # Unified suite-wide per Archie's decision, Aug 2026 - matches
            # what logCheck/relatedCheck already did.
            # ES: NA (no 0): un denominador de varianza de distancia
            # inválido/cero significa que dCor no está definido para ese
            # par (p. ej. una variable constante), no que la dependencia se
            # midió como exactamente cero. Unificado en toda la suite -
            # coincide con lo que logCheck/relatedCheck ya hacían.
            if (!is.finite(denom) || denom <= 0) return(NA_real_)
            sqrt(max(dcov2, 0) / sqrt(denom))
        },

        # Prueba de independencia basada en dCor, con valor p por permutación
        # (Székely & Rizzo, 2013; el estimador de dCor tampoco tiene una
        # distribución nula analítica simple para muestras finitas).
        .dCorTest = function(x, y, B = 999) {
            obs <- private$.dCor(x, y)
            null_vals <- vapply(seq_len(B), function(i) private$.dCor(x, sample(y)), numeric(1))
            null_vals <- null_vals[is.finite(null_vals)]
            if (length(null_vals) == 0) return(list(dcor = obs, p = NA_real_))
            pval <- (sum(null_vals >= obs) + 1) / (length(null_vals) + 1)
            list(dcor = obs, p = pval)
        },

        # Delegates to the shared .al_copent_test() (shared-helpers.R)
        # instead of reimplementing the same permutation-based copula
        # entropy test locally — the body was byte-identical.
        # ES: Delega en la función compartida .al_copent_test()
        # (shared-helpers.R) en vez de reimplementar localmente la misma
        # prueba de entropía copular basada en permutaciones — el cuerpo
        # era idéntico byte a byte.
        .copentTest = function(x, y, B = 999) {
            .al_copent_test(x, y, B = B)
        },

        # -----------------------------------------------------------------------------
        # jamovi lifecycle.
        # ES: ciclo de vida de jamovi.
        # -----------------------------------------------------------------------------

        .init = function() {
            private$.initCorrelationMatrix()
        },

        # Las matrices de Pearson y dCor tienen una columna numerada por
        # variable del modelo (formato APA 7: encabezados "1", "2", ... y las
        # etiquetas de fila llevan el nombre completo), agregadas dinámicamente
        # aquí (además de la columna 'var' ya fija en pathcheck.r.yaml).
        .initCorrelationMatrix = function() {
            vars <- self$options$vars
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
            tr <- function(en, es) .al_tr(lang, en, es)

            html_escape <- .al_html_escape

            html_block <- function(title = NULL, text, paragraphs = TRUE, raw = FALSE) {
                .al_html_block(title, text, paragraphs = paragraphs, raw = raw)
            }

            html_guide <- function(title, section, key) {
                html_block(title, .al_html_list(.al_text(lang, section, key)), raw = TRUE)
            }

            set_html_safe <- function(name, html) {
                element <- tryCatch(self$results[[name]], error = function(e) NULL)
                if (is.null(element)) return(invisible(FALSE))
                tryCatch(element$setContent(html), error = function(e) invisible(FALSE))
                invisible(TRUE)
            }

            # -----------------------------------------------------------------------------
            # Report introduction.
            # ES: Introducción del informe.
            # -----------------------------------------------------------------------------
            set_html_safe("intro", paste0(
                "<div style=\"max-width: 7.25in; width: 100%; box-sizing: border-box;\">",
                "<p style=\"font-weight: 700; margin: 0 0 0.10em 0; line-height: 1.25;\">",
                "AssumptionsLab",
                "</p>",
                "<p style=\"margin: 0 0 0.35em 0; line-height: 1.25;\">",
                tr("Assumption check for path analysis", "Revisión de supuestos para análisis de rutas"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                "<p style=\"margin: 0; line-height: 1.35;\">",
                tr(
                    "Use this analysis when you want to review whether a path model has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.",
                    "Use este análisis cuando quiera revisar si un modelo de rutas tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."
                ),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                "</div>"
            ))

            set_result_titles <- function() {
                set_title_safe <- function(name, en, es) {
                    element <- tryCatch(self$results[[name]], error = function(e) NULL)
                    if (is.null(element)) return(invisible(FALSE))
                    tryCatch(element$setTitle(tr(en, es)), error = function(e) invisible(FALSE))
                    invisible(TRUE)
                }
                titles <- list(
                    c("modelSummary", "Current model", "Modelo actual"),
                    c("specifiedModel", "Specified model", "Modelo especificado"),
                    c("modelStructureGuide", "Model Structure", "Estructura del Modelo"),
                    c("exogenousTable", "Exogenous Variables of the Model", "Variables Exógenas del Modelo"),
                    c("endogenousTable", "Endogenous Variables of the Model", "Variables Endógenas del Modelo"),
                    c("validationSummary", "Methodological Model Validation", "Validación Metodológica del Modelo"),
                    c("pathDiagram", "Path Diagram (SVG)", "Diagrama de Ruta (Path Diagram SVG)"),
                    c("outlierGuide", "Multivariate Outliers", "Valores Atípicos Multivariados"),
                    c("outlierTable", "Outlier Case Diagnostics", "Diagnóstico de Casos Atípicos"),
                    c("equationsGuide", "Model Design", "Diseño del Modelo"),
                    c("pathCoefficients", "Path Coefficients", "Coeficientes de Ruta (Path Coefficients)"),
                    c("coefficientsInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("rSquaredTable", "Explained Variance (R²)", "Varianza Explicada (R²)"),
                    c("residualCovarianceGuide", "Residual Covariances", "Covarianzas Residuales"),
                    c("residualCovarianceTable", "Residual Covariances Between Endogenous Variables",
                      "Covarianzas Residuales entre Variables Endógenas"),
                    c("residualPlots", "Residual Diagnostics by Equation", "Diagnóstico de Residuos por Ecuación"),
                    c("normalityGuide", "Residual Normality", "Normalidad de Residuos"),
                    c("residualNormality", "Residual Normality by Equation", "Normalidad de Residuos por Ecuación"),
                    c("normalityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("homoscedasticityGuide", "Homoscedasticity", "Homoscedasticidad"),
                    c("homoscedasticity", "Homoscedasticity by Equation", "Homoscedasticidad por Ecuación"),
                    c("homoscedasticityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("multicollinearityGuide", "Multicollinearity", "Multicolinealidad"),
                    c("multicollinearity", "Multicollinearity by Equation", "Multicolinealidad por Ecuación"),
                    c("multicollinearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("mardiaGuide", "Multivariate Normality (Mardia)", "Normalidad Multivariada (Mardia)"),
                    c("mardiaTable", "Mardia's Multivariate Normality Test", "Prueba de Normalidad Multivariada de Mardia"),
                    c("mardiaInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("correlationMatrixGuide", "Correlation Matrix", "Matriz de Correlaciones"),
                    c("pearsonMatrixTable", "Pearson Correlation Matrix (APA 7 format)",
                      "Matriz de Correlaciones de Pearson (formato APA 7)"),
                    c("dcorMatrixTable", "Distance Correlation Matrix (dCor, APA 7 format)",
                      "Matriz de Correlación de Distancia (dCor, formato APA 7)"),
                    c("correlationComparisonGuide", "Pearson / dCor / Copula Entropy Discordance Analysis",
                      "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                    c("correlationComparisonTable", "Pairs with a Notable Pearson / dCor Gap",
                      "Pares con Diferencia Notable entre Pearson y dCor"),
                    c("correlationComparisonInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("crossEntropyGuide", "Copula Entropy Independence Test", "Independencia por Entropía Copular"),
                    c("crossEntropyTable", "Copula Entropy Independence Test", "Prueba de Independencia por Entropía Copular"),
                    c("crossEntropyInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("indirectEffectsGuide", "Direct, Indirect and Total Effects", "Efectos Directos, Indirectos y Totales"),
                    c("indirectEffectsTable", "Direct, Indirect and Total Effects (Simple Mediation)",
                      "Efectos Directos, Indirectos y Totales (Mediación Simple)"),
                    c("indirectEffectsInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("sampleSizeGuide", "Sample Size Adequacy", "Adecuación del Tamaño de Muestra"),
                    c("sampleSizeTable", "Sample Size Assessment Given Model Complexity",
                      "Valoración del Tamaño de Muestra según la Complejidad del Modelo"),
                    c("sampleSizeInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("modelDiagnosticConclusionGuide", "Diagnostic Conclusion", "Conclusión Diagnóstica del Modelo"),
                    c("modelDiagnosticConclusionTable", "Model Diagnostics Summary", "Resumen de Diagnósticos del Modelo"),
                    c("modelDiagnosticConclusionInterpretation", "Overall Verdict", "Veredicto General")
                )
                for (t in titles) set_title_safe(t[1], t[2], t[3])
            }
            set_result_titles()

            set_table_column_titles <- function() {
                set_col_title_safe <- function(table_name, col_name, en, es) {
                    table <- tryCatch(self$results[[table_name]], error = function(e) NULL)
                    if (is.null(table))
                        return(invisible(FALSE))
                    column <- tryCatch(table$getColumn(col_name), error = function(e) NULL)
                    if (is.null(column))
                        return(invisible(FALSE))
                    tryCatch(column$setTitle(tr(en, es)), error = function(e) invisible(FALSE))
                    invisible(TRUE)
                }

                cols <- list(
                    c("modelSummary", "nVars", "Variables", "Variables"),
                    c("modelSummary", "nRelations", "Relations", "Relaciones"),
                    c("modelSummary", "nEndog", "Endogenous variables", "Variables endógenas"),
                    c("modelSummary", "nExog", "Exogenous variables", "Variables exógenas"),
                    c("modelSummary", "nCases", "Complete cases", "Casos completos"),
                    c("modelSummary", "casesPerRelation", "Cases per relation", "Casos por relación"),

                    c("specifiedModel", "dependent", "Dependent variable", "Variable dependiente"),
                    c("specifiedModel", "predictors", "Predictors", "Predictores"),

                    c("exogenousTable", "variable", "Variable", "Variable"),
                    c("exogenousTable", "outgoingPaths", "Outgoing paths", "Rutas salientes"),
                    c("endogenousTable", "variable", "Variable", "Variable"),
                    c("endogenousTable", "incomingPaths", "Direct predictors", "Predictores directos"),

                    c("residualCovarianceTable", "var1", "Variable 1", "Variable 1"),
                    c("residualCovarianceTable", "var2", "Variable 2", "Variable 2"),
                    c("residualCovarianceTable", "covariance", "Covariance", "Covarianza"),
                    c("residualCovarianceTable", "correlation", "Correlation", "Correlación"),

                    c("validationSummary", "check", "Validation", "Validación"),
                    c("validationSummary", "status", "Status", "Estado"),
                    c("validationSummary", "details", "Details", "Detalles"),

                    c("outlierTable", "case", "Case", "Caso"),
                    c("outlierTable", "d2", "Mahalanobis D²", "D² Mahalanobis"),
                    c("outlierTable", "pD2", "p", "p"),
                    c("outlierTable", "leverage", "Leverage", "Leverage"),
                    c("outlierTable", "cooksD", "Cook's Distance", "Distancia de Cook"),
                    c("outlierTable", "criteria", "Active criteria", "Criterios activados"),

                    c("pathCoefficients", "dep", "Endogenous variable", "Variable Endógena"),
                    c("pathCoefficients", "pred", "Predictor", "Predictor"),
                    c("pathCoefficients", "est", "Est.", "Est."),
                    c("pathCoefficients", "se", "SE", "E.E."),
                    c("pathCoefficients", "t", "t", "t"),
                    c("pathCoefficients", "p", "p", "p"),
                    c("pathCoefficients", "beta", "Std. Beta", "Beta Est."),

                    c("rSquaredTable", "var", "Endogenous variable", "Variable Endógena"),
                    c("rSquaredTable", "r2", "R²", "R²"),
                    c("rSquaredTable", "adjR2", "Adjusted R²", "R² Ajustado"),

                    c("residualNormality", "dep", "Dependent variable", "Variable Dependiente"),
                    c("residualNormality", "test", "Test", "Prueba"),
                    c("residualNormality", "statistic", "Statistic", "Estadístico"),
                    c("residualNormality", "p", "p", "p"),
                    c("residualNormality", "pSig", "Sig.", "Sig."),

                    c("homoscedasticity", "dep", "Dependent variable", "Variable Dependiente"),
                    c("homoscedasticity", "test", "Test", "Prueba"),
                    c("homoscedasticity", "statistic", "Statistic", "Estadístico"),
                    c("homoscedasticity", "value", "Value", "Valor"),
                    c("homoscedasticity", "df", "df", "gl"),
                    c("homoscedasticity", "p", "p", "p"),
                    c("homoscedasticity", "pSig", "Sig.", "Sig."),

                    c("multicollinearity", "dep", "Dependent variable", "Variable Dependiente"),
                    c("multicollinearity", "diagnostic", "Diagnostic", "Diagnóstico"),
                    c("multicollinearity", "item", "Item", "Elemento"),
                    c("multicollinearity", "statistic", "Statistic", "Estadístico"),
                    c("multicollinearity", "value", "Value", "Valor"),

                    c("pearsonMatrixTable", "var", "Variable", "Variable"),
                    c("dcorMatrixTable", "var", "Variable", "Variable"),

                    c("mardiaTable", "measure", "Measure", "Medida"),
                    c("mardiaTable", "value", "Coefficient", "Coeficiente"),
                    c("mardiaTable", "statistic", "Statistic", "Estadístico"),
                    c("mardiaTable", "df", "df", "gl"),
                    c("mardiaTable", "p", "p", "p"),
                    c("mardiaTable", "pSig", "Sig.", "Sig."),

                    c("crossEntropyTable", "var1", "Variable 1", "Variable 1"),
                    c("crossEntropyTable", "var2", "Variable 2", "Variable 2"),
                    c("crossEntropyTable", "connected", "Connected in the model", "Conectadas en el modelo"),
                    c("crossEntropyTable", "ce", "Copula Entropy", "Entropía Copular"),
                    c("crossEntropyTable", "p", "p", "p"),
                    c("crossEntropyTable", "pSig", "Sig.", "Sig."),

                    c("correlationComparisonTable", "var1", "Variable 1", "Variable 1"),
                    c("correlationComparisonTable", "var2", "Variable 2", "Variable 2"),
                    c("correlationComparisonTable", "pearson", "Pearson r", "Pearson r"),
                    c("correlationComparisonTable", "dcor", "dCor", "dCor"),
                    c("correlationComparisonTable", "gap", "Gap (dCor \u2212 |r|)", "Brecha (dCor \u2212 |r|)"),
                    c("correlationComparisonTable", "ce", "Copula Entropy", "Entropía Copular"),
                    c("correlationComparisonTable", "ceP", "p (CE)", "p (CE)"),
                    c("correlationComparisonTable", "ceSig", "Sig. (CE)", "Sig. (CE)"),
                    c("correlationComparisonTable", "flag", "Notable gap", "Diferencia notable"),

                    c("indirectEffectsTable", "x", "Predictor (X)", "Predictor (X)"),
                    c("indirectEffectsTable", "m", "Mediator (M)", "Mediador (M)"),
                    c("indirectEffectsTable", "y", "Dependent Variable (Y)", "Variable Dependiente (Y)"),
                    c("indirectEffectsTable", "directBeta", "Direct Effect (\u03b2)", "Efecto Directo (\u03b2)"),
                    c("indirectEffectsTable", "indirectBeta", "Indirect Effect (\u03b2)", "Efecto Indirecto (\u03b2)"),
                    c("indirectEffectsTable", "totalBeta", "Total Effect (\u03b2)", "Efecto Total (\u03b2)"),
                    c("indirectEffectsTable", "sobelZ", "Sobel z", "z de Sobel"),
                    c("indirectEffectsTable", "sobelP", "p", "p"),
                    c("indirectEffectsTable", "sobelPSig", "Sig.", "Sig."),

                    c("sampleSizeTable", "criterion", "Criterion (author)", "Criterio (autor)"),
                    c("sampleSizeTable", "rule", "Rule", "Regla"),
                    c("sampleSizeTable", "required", "Required N", "N requerido"),
                    c("sampleSizeTable", "actual", "Actual N", "N actual"),
                    c("sampleSizeTable", "status", "Status", "Estado"),

                    c("modelDiagnosticConclusionTable", "area", "Diagnostic Area", "Área Diagnóstica"),
                    c("modelDiagnosticConclusionTable", "status", "Status", "Estado"),
                    c("modelDiagnosticConclusionTable", "detail", "Detail", "Detalle")
                )
                for (col in cols) set_col_title_safe(col[1], col[2], col[3], col[4])
            }
            set_table_column_titles()

            vars      <- self$options$vars
            relations <- self$options$relations
            isFinalized <- self$options$isFinalized
            edges <- private$.buildEdges(relations)
            private$.pathVars  <- vars
            private$.pathEdges <- edges

            deps       <- unique(vapply(edges, function(e) e$dep,  character(1)))
            predsAll   <- unique(vapply(edges, function(e) e$pred, character(1)))
            involved   <- unique(c(deps, predsAll))
            exogenous  <- setdiff(vars, deps)
            endogenous <- deps
            isolated   <- setdiff(vars, involved)

            nCases <- if (length(vars) > 0)
                sum(stats::complete.cases(self$data[, vars, drop = FALSE]))
            else
                NA_integer_
            casesPerRelation <- if (length(edges) > 0 && !is.na(nCases))
                nCases / length(edges)
            else
                NA_real_

            # -----------------------------------------------------------------------------
            # "Current model" (live summary).
            # ES: "Modelo actual" (resumen en vivo).
            # -----------------------------------------------------------------------------
            self$results$modelSummary$setRow(rowNo = 1, values = list(
                nVars      = length(vars),
                nRelations = length(edges),
                nEndog     = length(endogenous),
                nExog      = length(exogenous),
                nCases     = nCases,
                casesPerRelation = casesPerRelation
            ))

            # -----------------------------------------------------------------------------
            # Model structure: exogenous / endogenous (showExogenous/showEndogenous).
            # ES: Estructura del modelo: exógenas / endógenas (showExogenous/showEndogenous).
            # -----------------------------------------------------------------------------
            set_html_safe("modelStructureGuide", html_block(
                tr("Model Structure", "Estructura del Modelo"),
                tr(
                    "Exogenous variables have no incoming paths in the specified model (no other variable predicts them); endogenous variables are predicted by at least one other variable. Use this list as a sanity check that the model reflects the structure you intended, before reading the rest of the report.",
                    "Las variables exógenas no tienen rutas entrantes en el modelo especificado (ninguna otra variable las predice); las variables endógenas son predichas por al menos otra variable. Use este listado como chequeo de cordura de que el modelo refleja la estructura que usted pretendía, antes de leer el resto del informe."
                ),
                paragraphs = FALSE
            ))

            exogTable <- self$results$exogenousTable
            exogTable$deleteRows()
            for (v in exogenous) {
                nOut <- sum(vapply(edges, function(e) identical(e$pred, v), logical(1)))
                exogTable$addRow(rowKey = v, values = list(variable = v, outgoingPaths = nOut))
            }

            endogTable <- self$results$endogenousTable
            endogTable$deleteRows()
            for (v in endogenous) {
                nIn <- sum(vapply(edges, function(e) identical(e$dep, v), logical(1)))
                endogTable$addRow(rowKey = v, values = list(variable = v, incomingPaths = nIn))
            }

            # -----------------------------------------------------------------------------
            # "Specified model" (grouped by dependent).
            # ES: "Modelo especificado" (agrupado por dependiente).
            # -----------------------------------------------------------------------------
            self$results$specifiedModel$deleteRows()
            if (length(edges) == 0) {
                self$results$specifiedModel$addRow(rowKey = "empty", values = list(
                    dependent  = "—",
                    predictors = tr("No relations have been defined yet.",
                                    "No existen relaciones definidas.")
                ))
            } else {
                for (d in deps) {
                    preds_d <- vapply(
                        Filter(function(e) identical(e$dep, d), edges),
                        function(e) e$pred, character(1))
                    self$results$specifiedModel$addRow(rowKey = d, values = list(
                        dependent  = d,
                        predictors = paste(unique(preds_d), collapse = " + ")
                    ))
                }
            }

            # -----------------------------------------------------------------------------
            # Methodological validation (always live).
            # ES: Validación metodológica (siempre en vivo).
            # -----------------------------------------------------------------------------
            self$results$validationSummary$deleteRows()
            addCheck <- function(key, check, ok, detailsOk, detailsFail) {
                self$results$validationSummary$addRow(rowKey = key, values = list(
                    check   = check,
                    status  = if (ok) "\u2714" else "\u26A0",
                    details = if (ok) detailsOk else detailsFail
                ))
            }

            if (length(vars) < 2 || length(edges) == 0) {
                addCheck("spec", tr("Specification", "Especificación"), FALSE,
                    "", tr("Add at least two variables and one relation between them.",
                           "Agregue al menos dos variables y una relación entre ellas."))
            } else {
                edgeKeys <- vapply(edges, function(e) paste(e$pred, e$dep, sep = " -> "), character(1))
                hasDup   <- any(duplicated(edgeKeys))
                addCheck("dup", tr("No duplicate relations", "Sin relaciones duplicadas"), !hasDup,
                    tr("No duplicate relations.", "Sin relaciones duplicadas."),
                    tr("There are repeated relations; remove the duplicates.",
                       "Existen relaciones repetidas; elimine los duplicados."))

                selfLoop <- any(vapply(edges, function(e) identical(e$pred, e$dep), logical(1)))
                addCheck("self", tr("No self-loops", "Sin autolazos"), !selfLoop,
                    tr("No variable predicts itself.", "Ninguna variable depende de sí misma."),
                    tr("A variable cannot predict itself; fix that relation.",
                       "Una variable no puede predecirse a sí misma; corrija la relación."))

                hasCycle <- private$.hasCycle(edges)
                addCheck("cycle", tr("No cycles", "Sin ciclos"), !hasCycle,
                    tr("The model is recursive (no feedback loops).",
                       "El modelo es recursivo (sin retroalimentación)."),
                    tr("A cycle was detected; a recursive (OLS) model does not allow feedback loops.",
                       "Se detectó un ciclo; un modelo recursivo (OLS) no admite retroalimentación."))

                addCheck("hasPred", tr("All dependent variables have predictors",
                                       "Todas las dependientes poseen predictores"), TRUE,
                    tr("Every dependent variable has at least one predictor.",
                       "Cada variable dependiente tiene al menos un predictor."), "")

                nComp <- private$.connectedComponents(vars, edges)
                addCheck("connected", tr("Connected model", "Modelo conectado"), nComp == 1,
                    tr("All variables form a single connected model.",
                       "Todas las variables forman un único modelo conectado."),
                    tr("The model has subgroups of variables with no relation between them.",
                       "El modelo tiene subgrupos de variables sin relación entre sí."))

                if (length(isolated) > 0) {
                    addCheck("isolated",
                        paste0(paste(isolated, collapse = ", "),
                               if (length(isolated) == 1)
                                   tr(" does not take part in any relation", " no participa en ninguna relación")
                               else
                                   tr(" do not take part in any relation", " no participan en ninguna relación")),
                        FALSE, "",
                        tr("Add a relation for this variable or remove it from the model.",
                           "Agregue una relación para esta variable o retírela del modelo."))
                }

                ready <- !hasDup && !selfLoop && !hasCycle && nComp == 1 && length(isolated) == 0
                addCheck("ready", tr("Model ready for analysis", "Modelo listo para análisis"), ready,
                    tr("The model satisfies all validations and can be finalized.",
                       "El modelo cumple todas las validaciones y puede finalizarse."),
                    tr("Resolve the alerts above before finalizing the model.",
                       "Resuelva las alertas anteriores antes de finalizar el modelo."))
            }

            # El diagrama se renderiza siempre que haya variables, sin esperar a
            # 'Finalizar modelo', para que el investigador vea el modelo mientras lo construye.

            # -----------------------------------------------------------------------------
            # Estimation (only after finalizing, with a valid model).
            # ES: Estimación (solo tras finalizar y con modelo válido).
            # -----------------------------------------------------------------------------
            if (!isFinalized || length(deps) == 0)
                return()

            edgeKeys <- vapply(edges, function(e) paste(e$pred, e$dep, sep = " -> "), character(1))
            invalid <- any(duplicated(edgeKeys)) ||
                any(vapply(edges, function(e) identical(e$pred, e$dep), logical(1))) ||
                private$.hasCycle(edges)
            if (invalid) return()

            data <- self$data[, vars, drop = FALSE]
            data <- jmvcore::naOmit(data)

            set_html_safe("equationsGuide", html_guide(tr("Model Design", "Diseño del Modelo"), "path", "designGuide"))

            coefTable <- self$results$pathCoefficients
            r2Table   <- self$results$rSquaredTable

            fits <- list()
            edgeStats <- list()

            for (d in deps) {
                preds_d <- unique(vapply(
                    Filter(function(e) identical(e$dep, d), edges),
                    function(e) e$pred, character(1)))

                yx <- data[, c(d, preds_d), drop = FALSE]
                for (col in names(yx)) yx[[col]] <- jmvcore::toNumeric(yx[[col]])

                form <- as.formula(paste0("`", d, "` ~ ", paste0("`", preds_d, "`", collapse = " + ")))
                fit  <- try(stats::lm(form, data = yx), silent = TRUE)
                if (inherits(fit, "try-error")) next()

                fits[[d]] <- list(fit = fit, preds = preds_d, data = yx)

                s <- summary(fit)
                co <- s$coefficients
                sdY <- stats::sd(yx[[d]])

                for (p in preds_d) {
                    rn <- p
                    if (!(rn %in% rownames(co))) next()
                    sdX <- stats::sd(yx[[p]])
                    beta <- co[rn, "Estimate"] * (sdX / sdY)
                    edgeStats[[paste(p, d, sep = "->")]] <- list(
                        est = co[rn, "Estimate"], se = co[rn, "Std. Error"],
                        t = co[rn, "t value"], p = co[rn, "Pr(>|t|)"], beta = beta
                    )
                    coefTable$addRow(rowKey = paste(d, p, sep = "|"), values = list(
                        dep   = d,
                        pred  = p,
                        est   = co[rn, "Estimate"],
                        se    = co[rn, "Std. Error"],
                        t     = co[rn, "t value"],
                        p     = co[rn, "Pr(>|t|)"],
                        beta  = beta
                    ))
                }

                r2Table$addRow(rowKey = d, values = list(
                    var   = d,
                    r2    = s$r.squared,
                    adjR2 = s$adj.r.squared
                ))
            }

            private$.pathFits <- fits
            private$.edgeStats <- edgeStats

            if (length(fits) == 0) return()

            # -----------------------------------------------------------------------------
            # Residual covariances between endogenous variables (showCovs).
            # ES: Covarianzas residuales entre endógenas (showCovs).
            # -----------------------------------------------------------------------------
            # Covarianzas/correlaciones de error entre ecuaciones: información estándar
            # en reporting de path analysis cuando hay errores correlacionados por
            # diseño (ninguna otra parte del módulo la muestra). Los residuos de todas
            # las ecuaciones están alineados por fila porque `data` ya fue reducida a
            # casos completos sobre TODAS las variables del modelo antes de ajustar cada
            # ecuación (arriba), así que ninguna ecuación pierde filas adicionales.
            depsWithFit <- names(fits)
            if (length(depsWithFit) >= 2) {
                set_html_safe("residualCovarianceGuide", html_block(
                    tr("Residual Covariances", "Covarianzas Residuales"),
                    tr(
                        "When two endogenous variables share unmodeled causes, the residuals of their equations correlate even though no direct path connects them. Reporting these residual (error) covariances is standard practice in path analysis whenever correlated errors are plausible by design.",
                        "Cuando dos variables endógenas comparten causas no modeladas, los residuos de sus ecuaciones se correlacionan aunque ninguna ruta directa las conecte. Reportar estas covarianzas residuales (de error) es una práctica estándar en análisis de rutas cuando es plausible que existan errores correlacionados por diseño."
                    ),
                    paragraphs = FALSE
                ))

                covTable <- self$results$residualCovarianceTable
                covTable$deleteRows()
                resids <- lapply(fits[depsWithFit], function(f) stats::residuals(f$fit))
                pairs <- utils::combn(depsWithFit, 2, simplify = FALSE)
                for (pr in pairs) {
                    r1 <- resids[[pr[1]]]; r2 <- resids[[pr[2]]]
                    n <- min(length(r1), length(r2))
                    if (n < 3) next()
                    covTable$addRow(rowKey = paste(pr, collapse = "|"), values = list(
                        var1 = pr[1],
                        var2 = pr[2],
                        covariance  = stats::cov(r1, r2),
                        correlation = stats::cor(r1, r2)
                    ))
                }
            }

            # Delegates to the shared .al_p_sig() (shared-helpers.R) instead
            # of reimplementing the same threshold logic locally. The local
            # version also lacked an NA guard before the comparisons.
            # ES: Delega en la función compartida .al_p_sig()
            # (shared-helpers.R) en vez de reimplementar localmente la
            # misma lógica de umbrales. La versión local tampoco protegía
            # contra NA antes de las comparaciones.
            p_sig_stars <- .al_p_sig

            # -----------------------------------------------------------------------------
            # Applied interpretation of coefficients (per-equation narrative).
            # ES: Interpretación aplicada de los coeficientes (narrativa por ecuación).
            # -----------------------------------------------------------------------------
            format_p <- function(p) {
                if (is.na(p)) return("NA")
                if (p < .001) return("< .001")
                sprintf("= %.3f", p)
            }
            nSig <- sum(vapply(fits, function(f) {
                co <- summary(f$fit)$coefficients
                rns <- f$preds
                sum(co[rns[rns %in% rownames(co)], "Pr(>|t|)"] < .05)
            }, numeric(1)))
            nCoef <- sum(vapply(fits, function(f) length(f$preds), numeric(1)))

            depLines <- character(0)
            for (d in names(fits)) {
                f <- fits[[d]]
                preds_d <- f$preds
                s <- summary(f$fit)
                rowsInfo <- lapply(preds_d, function(pr) {
                    es <- edgeStats[[paste(pr, d, sep = "->")]]
                    list(pred = pr, beta = es$beta, p = es$p)
                })
                absBetas <- vapply(rowsInfo, function(r) abs(r$beta), numeric(1))
                strongestIdx <- which.max(absBetas)
                predSentences <- vapply(seq_along(rowsInfo), function(i) {
                    r <- rowsInfo[[i]]
                    dirWord <- tr(if (r$beta >= 0) "positively" else "negatively",
                                  if (r$beta >= 0) "positivamente" else "negativamente")
                    sigWord <- if (!is.na(r$p) && r$p < .05)
                        tr("significantly", "de forma significativa")
                    else
                        tr("without statistical significance", "sin significancia estadística")
                    strongTag <- if (i == strongestIdx && length(rowsInfo) > 1)
                        tr(" (strongest direct predictor in this equation)", " (predictor directo más fuerte de esta ecuación)")
                    else ""
                    tr(sprintf("%s predicts %s %s and %s (\u03b2 = %.3f, p %s)%s",
                               r$pred, d, dirWord, sigWord, r$beta, format_p(r$p), strongTag),
                       sprintf("%s predice a %s %s y %s (\u03b2 = %.3f, p %s)%s",
                               r$pred, d, dirWord, sigWord, r$beta, format_p(r$p), strongTag))
                }, character(1))
                eqLine <- tr(
                    sprintf("Equation for %s (R\u00b2 = %.3f, adjusted R\u00b2 = %.3f): %s.",
                            d, s$r.squared, s$adj.r.squared, paste(predSentences, collapse = "; ")),
                    sprintf("Ecuación de %s (R\u00b2 = %.3f, R\u00b2 ajustado = %.3f): %s.",
                            d, s$r.squared, s$adj.r.squared, paste(predSentences, collapse = "; "))
                )
                depLines <- c(depLines, eqLine)
            }

            set_html_safe("coefficientsInterpretation", html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(tr(sprintf("%d of %d path coefficient(s) are statistically significant (p < .05).",
                             nSig, nCoef),
                     sprintf("%d de %d coeficiente(s) de ruta son estadísticamente significativos (p < .05).",
                             nSig, nCoef)),
                  depLines,
                  tr("Common error: interpreting a path coefficient as if it were the total effect; it only reflects the direct effect within its own equation. If a predictor also reaches the outcome through a mediator, see the \"Direct, Indirect and Total Effects\" section for the full decomposition.",
                     "Error común: interpretar un coeficiente de ruta como si fuera el efecto total; solo refleja el efecto directo dentro de su propia ecuación. Si un predictor también llega a la variable dependiente a través de un mediador, consulte la sección \"Efectos Directos, Indirectos y Totales\" para la descomposición completa."))
            ))

            # -----------------------------------------------------------------------------
            # Multivariate outliers.
            # ES: Valores atípicos multivariados.
            # -----------------------------------------------------------------------------
            set_html_safe("outlierGuide", html_guide(tr("Multivariate Outliers", "Valores Atípicos Multivariados"),
                                                       "path", "outlierAnalysisGuide"))

            outlierTable <- self$results$outlierTable
            outlierTable$deleteRows()

            covData <- data
            for (col in names(covData)) covData[[col]] <- jmvcore::toNumeric(covData[[col]])
            covData <- stats::na.omit(covData)

            mahaResult <- tryCatch({
                center <- colMeans(covData)
                S <- stats::cov(covData)
                d2 <- stats::mahalanobis(covData, center, S)
                dfM <- ncol(covData)
                pD2 <- stats::pchisq(d2, df = dfM, lower.tail = FALSE)
                thresholdD2 <- stats::qchisq(0.975, df = dfM)
                list(d2 = d2, p = pD2, threshold = thresholdD2)
            }, error = function(e) NULL)

            # Leverage/Cook's D: se usan de la ecuación con más predictores (la más
            # completa), como referencia más exigente dentro del sistema de ecuaciones.
            mainFit <- fits[[names(which.max(vapply(fits, function(f) length(f$preds), integer(1))))]]
            leverage <- tryCatch(stats::hatvalues(mainFit$fit), error = function(e) NULL)
            cooksD   <- tryCatch(stats::cooks.distance(mainFit$fit), error = function(e) NULL)
            pMain    <- length(stats::coef(mainFit$fit))
            nMain    <- nrow(mainFit$data)
            levThreshold  <- 2 * pMain / nMain
            cookThreshold <- 4 / nMain

            if (!is.null(mahaResult) && !is.null(leverage) && !is.null(cooksD)) {
                n <- length(mahaResult$d2)
                ord <- order(mahaResult$d2, decreasing = TRUE)
                shown <- ord[seq_len(min(20, n))]

                for (i in shown) {
                    crit <- character(0)
                    if (mahaResult$d2[i] > mahaResult$threshold)
                        crit <- c(crit, sprintf("D\u00B2 > %.2f", mahaResult$threshold))
                    if (i <= length(leverage) && leverage[i] > levThreshold)
                        crit <- c(crit, sprintf("Leverage > %.4f", levThreshold))
                    if (i <= length(cooksD) && cooksD[i] > cookThreshold)
                        crit <- c(crit, sprintf("Cook > %.4f", cookThreshold))
                    if (length(crit) == 0) next()

                    outlierTable$addRow(rowKey = i, values = list(
                        case     = i,
                        d2       = mahaResult$d2[i],
                        pD2      = mahaResult$p[i],
                        leverage = if (i <= length(leverage)) leverage[i] else NA_real_,
                        cooksD   = if (i <= length(cooksD)) cooksD[i] else NA_real_,
                        criteria = paste(crit, collapse = "; ")
                    ))
                }

                nFlaggedD2 <- sum(mahaResult$d2 > mahaResult$threshold)
                set_html_safe("outlierGuide", paste0(
                    html_guide(tr("Multivariate Outliers", "Valores Atípicos Multivariados"), "path", "outlierAnalysisGuide"),
                    html_block(NULL, tr(
                        sprintf("%d of %d cases (%.1f%%) exceed the Mahalanobis D² threshold (p < .025).",
                                nFlaggedD2, n, 100 * nFlaggedD2 / n),
                        sprintf("%d de %d casos (%.1f%%) superan el umbral de D² de Mahalanobis (p < .025).",
                                nFlaggedD2, n, 100 * nFlaggedD2 / n)), paragraphs = FALSE)
                ))
            }

            # -----------------------------------------------------------------------------
            # Residual normality per equation.
            # ES: Normalidad de residuos por ecuación.
            # -----------------------------------------------------------------------------
            set_html_safe("normalityGuide", html_guide(tr("Residual Normality", "Normalidad de Residuos"), "path", "normalityGuide"))
            normTable <- self$results$residualNormality
            normTable$deleteRows()

            nNormalityFail <- 0
            nNormalityTotal <- 0
            normFailByDep <- list()
            normTotalByDep <- list()
            normFailedTestsByDep <- list()

            for (d in names(fits)) {
                res <- stats::residuals(fits[[d]]$fit)
                normFailByDep[[d]] <- 0
                normTotalByDep[[d]] <- 0
                normFailedTestsByDep[[d]] <- character(0)

                add_norm <- function(testName, statistic, p) {
                    normTable$addRow(rowKey = paste(d, testName, sep = "|"), values = list(
                        dep = d, test = testName, statistic = statistic, p = p, pSig = p_sig_stars(p)
                    ))
                    nNormalityTotal <<- nNormalityTotal + 1
                    normTotalByDep[[d]] <<- normTotalByDep[[d]] + 1
                    if (!is.na(p) && p < .05) {
                        nNormalityFail <<- nNormalityFail + 1
                        normFailByDep[[d]] <<- normFailByDep[[d]] + 1
                        normFailedTestsByDep[[d]] <<- c(normFailedTestsByDep[[d]], testName)
                    }
                }

                # Shapiro-Wilk / Jarque-Bera / skewness / kurtosis: guard
                # unified suite-wide (n>=3,<=5000 & sd>0 for SW; n>=8 & sd>0
                # for the rest) per Archie's decision, Aug 2026 - see
                # .al_norm_core_battery().
                # ES: guarda unificada para toda la suite - ver
                # .al_norm_core_battery() en shared-helpers.R.
                .nc_res <- .al_norm_core_battery(res)
                sw <- .nc_res$sw
                if (!is.null(sw)) add_norm("Shapiro-Wilk", unname(sw$statistic), sw$p.value)

                # Lilliefors / Anderson-Darling / Cramer-von Mises / Shapiro-Francia /
                # Pearson chi-square: identical tryCatch calls in every module,
                # consolidated in shared-helpers.R (.al_nortest_battery).
                # ES: idénticas en todos los módulos, consolidadas en shared-helpers.R.
                .nt_res <- .al_nortest_battery(res)
                ad <- .nt_res$ad; li <- .nt_res$li; cvm <- .nt_res$cvm
                sf <- .nt_res$sf; pt <- .nt_res$pt
                if (!is.null(ad)) add_norm("Anderson-Darling", unname(ad$statistic), ad$p.value)
                if (!is.null(li)) add_norm(tr("Lilliefors (corrected K-S)", "Lilliefors (K-S corregido)"), unname(li$statistic), li$p.value)
                if (!is.null(cvm)) add_norm("Cramer-von Mises", unname(cvm$statistic), cvm$p.value)
                if (!is.null(sf)) add_norm("Shapiro-Francia", unname(sf$statistic), sf$p.value)
                if (!is.null(pt)) add_norm(tr("Pearson chi-square", "Pearson chi-cuadrado"), unname(pt$statistic), pt$p.value)

                jb <- .nc_res$jb
                if (!is.null(jb)) add_norm("Jarque-Bera", jb$value, jb$p)

                skew_test <- .nc_res$skew
                if (!is.null(skew_test)) add_norm(tr("Skewness test", "Prueba de asimetría"), skew_test$value, skew_test$p)

                kurt_test <- .nc_res$kurt
                if (!is.null(kurt_test)) add_norm(tr("Kurtosis test", "Prueba de curtosis"), kurt_test$value, kurt_test$p)
            }

            nCases <- nrow(data)
            depNormLines <- vapply(names(fits), function(d) {
                fail <- normFailByDep[[d]]; total <- normTotalByDep[[d]]
                if (fail == 0) {
                    tr(sprintf("%s: residuals are compatible with normality (0 of %d tests significant).", d, total),
                       sprintf("%s: los residuos son compatibles con normalidad (0 de %d pruebas significativas).", d, total))
                } else {
                    testsTxt <- paste(normFailedTestsByDep[[d]], collapse = ", ")
                    tr(sprintf("%s: residuals depart from normality in %d of %d tests (%s). Because this equation has %d complete cases, %s",
                               d, fail, total, testsTxt, nCases,
                               if (nCases >= 100)
                                   "the sample is large enough that the significance tests on this equation's path coefficients should still be reasonably robust to this departure, but it is worth inspecting the \"Residual Diagnostics by Equation\" chart for the specific shape of the deviation (skew, heavy tails, a few extreme cases)."
                               else
                                   "with a moderate-to-small sample this departure is more likely to distort the significance tests on this equation's path coefficients; consider bootstrap standard errors or inspecting the \"Residual Diagnostics by Equation\" chart for a specific cause (e.g. remaining outliers, a skewed dependent variable)."),
                       sprintf("%s: los residuos se desvían de la normalidad en %d de %d pruebas (%s). Dado que esta ecuación tiene %d casos completos, %s",
                               d, fail, total, testsTxt, nCases,
                               if (nCases >= 100)
                                   "la muestra es lo bastante grande como para que las pruebas de significancia de los coeficientes de ruta de esta ecuación sigan siendo razonablemente robustas a esta desviación, aunque conviene revisar el gráfico \"Diagnóstico de Residuos por Ecuación\" para ver la forma específica de la desviación (asimetría, colas pesadas, algunos casos extremos)."
                               else
                                   "con una muestra moderada a pequeña esta desviación es más probable que distorsione las pruebas de significancia de los coeficientes de ruta de esta ecuación; considere errores estándar bootstrap o revise el gráfico \"Diagnóstico de Residuos por Ecuación\" en busca de una causa puntual (p. ej. atípicos remanentes, una variable dependiente asimétrica)."))
                }
            }, character(1))

            set_html_safe("normalityInterpretation", html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(tr(sprintf("%d of %d test-equation combination(s) show non-normal residuals (p < .05).",
                             nNormalityFail, nNormalityTotal),
                     sprintf("%d de %d combinación(es) prueba-ecuación muestran residuos no normales (p < .05).",
                             nNormalityFail, nNormalityTotal)),
                  depNormLines)
            ))

            # -----------------------------------------------------------------------------
            # Homoscedasticity per equation.
            # ES: Homoscedasticidad por ecuación.
            # -----------------------------------------------------------------------------
            set_html_safe("homoscedasticityGuide", html_guide(tr("Homoscedasticity", "Homoscedasticidad"), "path", "homoscedasticityGuide"))
            homoTable <- self$results$homoscedasticity
            homoTable$deleteRows()

            nHomoFail <- 0
            nHomoTotal <- 0
            homoFailByDep <- list()
            homoFailedTestsByDep <- list()

            for (d in names(fits)) {
                fit <- fits[[d]]$fit
                preds_d <- fits[[d]]$preds
                res <- stats::residuals(fit)
                fitted_vals <- stats::fitted(fit)
                homoFailByDep[[d]] <- 0
                homoFailedTestsByDep[[d]] <- character(0)

                add_homo <- function(testName, statistic, value, df, p) {
                    homoTable$addRow(rowKey = paste(d, testName, sep = "|"), values = list(
                        dep = d, test = testName, statistic = statistic,
                        value = value, df = df, p = p, pSig = p_sig_stars(p)
                    ))
                    nHomoTotal <<- nHomoTotal + 1
                    if (!is.na(p) && p < .05) {
                        nHomoFail <<- nHomoFail + 1
                        homoFailByDep[[d]] <<- homoFailByDep[[d]] + 1
                        homoFailedTestsByDep[[d]] <<- c(homoFailedTestsByDep[[d]], testName)
                    }
                }

                bp <- .al_bptest(fit)
                if (!is.null(bp))
                    add_homo("Breusch-Pagan (lmtest)", "LM", unname(bp$statistic), as.character(unname(bp$parameter)), bp$p.value)

                gq <- tryCatch(lmtest::gqtest(fit), error = function(e) NULL)
                if (!is.null(gq))
                    add_homo("Goldfeld-Quandt (lmtest)", "F", unname(gq$statistic),
                             paste(unname(gq$parameter), collapse = ", "), gq$p.value)

                white <- tryCatch({
                    e2 <- res^2
                    k <- length(preds_d)
                    if (k >= 1) {
                        terms <- paste0("`", preds_d, "`")
                        sq_terms <- paste0("I(`", preds_d, "`^2)")
                        int_terms <- character(0)
                        if (k >= 2) {
                            combos <- utils::combn(preds_d, 2, simplify = FALSE)
                            int_terms <- vapply(combos, function(pr) paste0("`", pr[1], "`:`", pr[2], "`"), character(1))
                        }
                        rhs <- paste(c(terms, sq_terms, int_terms), collapse = " + ")
                        auxForm <- stats::as.formula(paste("e2 ~", rhs))
                        auxData <- fits[[d]]$data
                        auxData$e2 <- e2
                        auxFit <- stats::lm(auxForm, data = auxData)
                        lm_val <- length(e2) * summary(auxFit)$r.squared
                        dfW <- length(stats::coef(auxFit)) - 1
                        p <- stats::pchisq(lm_val, df = dfW, lower.tail = FALSE)
                        list(value = lm_val, df = dfW, p = p)
                    } else NULL
                }, error = function(e) NULL)
                if (!is.null(white))
                    add_homo(tr("White (general)", "White (general)"), "LM", white$value, as.character(white$df), white$p)

                sp <- tryCatch(stats::cor.test(abs(res), fitted_vals, method = "spearman"), error = function(e) NULL)
                if (!is.null(sp))
                    add_homo(tr("Spearman |residuals| vs fitted", "Spearman |residuos| vs ajustados"),
                             "\u03C1", unname(sp$estimate), NA_character_, sp$p.value)
            }

            depHomoLines <- vapply(names(fits), function(d) {
                fail <- homoFailByDep[[d]]
                if (fail == 0) {
                    tr(sprintf("%s: no test detects heteroscedasticity in this equation's residuals.", d),
                       sprintf("%s: ninguna prueba detecta heterocedasticidad en los residuos de esta ecuación.", d))
                } else {
                    testsTxt <- paste(homoFailedTestsByDep[[d]], collapse = ", ")
                    tr(sprintf("%s: %s detect(s) heteroscedasticity. This does not bias the path coefficients themselves, but it can bias their standard errors and therefore the p-values in the \"Path Coefficients\" table; consider re-checking the affected coefficients with heteroscedasticity-consistent (HC/robust) standard errors before drawing firm conclusions about their significance.",
                               d, testsTxt),
                       sprintf("%s: %s detecta(n) heterocedasticidad. Esto no sesga los coeficientes de ruta en sí, pero sí puede sesgar sus errores estándar y por tanto los valores p de la tabla \"Coeficientes de Ruta\"; conviene revisar los coeficientes afectados con errores estándar robustos/consistentes a heterocedasticidad (HC) antes de sacar conclusiones firmes sobre su significancia.",
                               d, testsTxt))
                }
            }, character(1))

            set_html_safe("homoscedasticityInterpretation", html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                c(tr(sprintf("%d of %d test-equation combination(s) show evidence of heteroscedasticity (p < .05).",
                             nHomoFail, nHomoTotal),
                     sprintf("%d de %d combinación(es) prueba-ecuación muestran evidencia de heterocedasticidad (p < .05).",
                             nHomoFail, nHomoTotal)),
                  depHomoLines)
            ))

            # -----------------------------------------------------------------------------
            # Multicollinearity (VIF, tolerance, eigenvalue, condition index, det(R)).
            # ES: Multicolinealidad (VIF, tolerancia, eigenvalue, índice de condición, det(R)).
            # -----------------------------------------------------------------------------
            set_html_safe("multicollinearityGuide", html_guide(tr("Multicollinearity", "Multicolinealidad"), "path", "multicollinearityGuide"))
            vifTable <- self$results$multicollinearity
            vifTable$deleteRows()

            maxVif <- NA_real_
            maxVifLabel <- ""
            maxCi <- NA_real_
            maxCiLabel <- ""
            vifByDep <- list()
            multiRowKey <- 0

            add_multi_row <- function(d, diagnostic, item, statistic, value) {
                multiRowKey <<- multiRowKey + 1
                vifTable$addRow(rowKey = paste0("multi_", multiRowKey), values = list(
                    dep = d, diagnostic = diagnostic, item = item, statistic = statistic,
                    value = if (is.na(value)) NA_real_ else value
                ))
            }

            for (d in names(fits)) {
                preds_d <- fits[[d]]$preds
                if (length(preds_d) < 2) next()

                X <- tryCatch(stats::model.matrix(fits[[d]]$fit), error = function(e) NULL)
                X_no_intercept <- if (!is.null(X)) X[, colnames(X) != "(Intercept)", drop = FALSE] else NULL

                if (is.null(X_no_intercept) || ncol(X_no_intercept) < 2) next()

                depVifs <- numeric(0)

                for (j in seq_len(ncol(X_no_intercept))) {
                    target <- X_no_intercept[, j]
                    others <- X_no_intercept[, -j, drop = FALSE]

                    vif <- tryCatch({
                        auxiliaryVifModel <- stats::lm(target ~ others)
                        1 / (1 - summary(auxiliaryVifModel)$r.squared)
                    }, error = function(e) NA_real_)

                    tol <- if (is.na(vif)) NA_real_ else 1 / vif
                    pClean <- gsub("^`|`$", "", colnames(X_no_intercept)[j])

                    add_multi_row(d, "VIF", pClean, "VIF", vif)
                    add_multi_row(d, tr("Tolerance", "Tolerancia"), pClean, "1/VIF", tol)

                    if (!is.na(vif)) {
                        depVifs[pClean] <- vif
                        if (is.na(maxVif) || vif > maxVif) {
                            maxVif <- vif
                            maxVifLabel <- paste0(pClean, " -> ", d)
                        }
                    }
                }

                vifByDep[[d]] <- depVifs

                eig <- tryCatch({
                    R <- stats::cor(scale(X_no_intercept), use = "pairwise.complete.obs")
                    eigen(R, symmetric = TRUE)$values
                }, error = function(e) NULL)

                if (!is.null(eig)) {
                    min_eig <- max(min(eig, na.rm = TRUE), .Machine$double.eps)
                    max_eig <- max(eig, na.rm = TRUE)
                    ci <- sqrt(max_eig / min_eig)

                    det_r <- tryCatch(
                        det(stats::cor(scale(X_no_intercept), use = "pairwise.complete.obs")),
                        error = function(e) NA_real_
                    )

                    add_multi_row(d, tr("Minimum eigenvalue", "Eigenvalue mínimo"), tr("Design matrix", "Matriz de diseño"), tr("minimum λ", "λ mínimo"), min_eig)
                    add_multi_row(d, tr("Condition index", "Índice de condición"), tr("Design matrix", "Matriz de diseño"), "CI", ci)
                    add_multi_row(d, tr("Determinant", "Determinante"), tr("Correlation matrix", "Matriz de correlación"), "det(R)", det_r)

                    if (is.na(maxCi) || ci > maxCi) {
                        maxCi <- ci
                        maxCiLabel <- d
                    }
                }
            }

            depVifLines <- if (length(vifByDep) == 0) character(0) else vapply(names(vifByDep), function(d) {
                depVifs <- vifByDep[[d]]
                if (length(depVifs) == 0) return("")
                worst <- names(depVifs)[which.max(depVifs)]
                worstVal <- max(depVifs)
                sevWord <- if (worstVal > 10)
                    tr("a severe multicollinearity problem", "un problema severo de multicolinealidad")
                else if (worstVal > 5)
                    tr("a moderate multicollinearity concern", "una alerta moderada de multicolinealidad")
                else
                    tr("no meaningful multicollinearity", "ninguna multicolinealidad relevante")
                tr(sprintf("%s: highest VIF is %.2f (%s), indicating %s among its predictors.",
                           d, worstVal, worst, sevWord),
                   sprintf("%s: el VIF más alto es %.2f (%s), lo que indica %s entre sus predictores.",
                           d, worstVal, worst, sevWord))
            }, character(1))
            depVifLines <- depVifLines[nzchar(depVifLines)]

            set_html_safe("multicollinearityInterpretation", html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (is.na(maxVif))
                    tr("No equation has two or more predictors, so multicollinearity is not applicable.",
                       "Ninguna ecuación tiene dos o más predictores, por lo que la multicolinealidad no aplica.")
                else
                    c(tr(sprintf("Maximum VIF across the whole model: %.2f (%s). Values between 5 and 10 raise a moderate concern, and above 10 are considered a severe problem (Marquardt, 1970).",
                                 maxVif, maxVifLabel),
                         sprintf("VIF máximo de todo el modelo: %.2f (%s). Valores entre 5 y 10 encienden una alerta moderada, y valores por encima de 10 se consideran un problema severo (Marquardt, 1970).",
                                 maxVif, maxVifLabel)),
                      if (!is.na(maxCi))
                          tr(sprintf("Maximum condition index across equations: %.2f (%s). Values above 15 raise a moderate concern and above 30 a severe one, complementing VIF by diagnosing collinearity across the whole design matrix at once.",
                                     maxCi, maxCiLabel),
                             sprintf("Índice de condición máximo entre ecuaciones: %.2f (%s). Valores por encima de 15 encienden una alerta moderada y por encima de 30 una severa, complementando al VIF al diagnosticar la colinealidad de toda la matriz de diseño a la vez.",
                                     maxCi, maxCiLabel))
                      else "",
                      depVifLines)
            ))

            # -----------------------------------------------------------------------------
            # Mardia multivariate normality.
            # ES: Normalidad multivariada de Mardia.
            # -----------------------------------------------------------------------------
            set_html_safe("mardiaGuide", html_guide(tr("Multivariate Normality (Mardia)", "Normalidad Multivariada (Mardia)"),
                                                       "path", "multivariateNormalityGuide"))
            mardiaTable <- self$results$mardiaTable
            mardiaTable$deleteRows()

            mardiaRes <- tryCatch({
                X <- as.matrix(covData)
                n <- nrow(X); p <- ncol(X)
                Xc <- scale(X, center = TRUE, scale = FALSE)
                Sigma <- (t(Xc) %*% Xc) / n
                Sinv  <- solve(Sigma)
                D <- Xc %*% Sinv %*% t(Xc)

                b1p <- sum(D^3) / n^2
                b2p <- sum(diag(D)^2) / n

                skewDf   <- p * (p + 1) * (p + 2) / 6
                skewStat <- n * b1p / 6
                skewP    <- stats::pchisq(skewStat, df = skewDf, lower.tail = FALSE)

                kurtMean <- p * (p + 2)
                kurtVar  <- 8 * p * (p + 2) / n
                kurtZ    <- (b2p - kurtMean) / sqrt(kurtVar)
                kurtP    <- 2 * stats::pnorm(-abs(kurtZ))

                list(b1p = b1p, skewStat = skewStat, skewDf = skewDf, skewP = skewP,
                     b2p = b2p, kurtZ = kurtZ, kurtP = kurtP)
            }, error = function(e) NULL)

            if (!is.null(mardiaRes)) {
                mardiaTable$addRow(rowKey = "skew", values = list(
                    measure   = tr("Mardia's skewness", "Asimetría de Mardia"),
                    value     = mardiaRes$b1p,
                    statistic = mardiaRes$skewStat,
                    df        = sprintf("%.1f", mardiaRes$skewDf),
                    p         = mardiaRes$skewP,
                    pSig      = p_sig_stars(mardiaRes$skewP)
                ))
                mardiaTable$addRow(rowKey = "kurt", values = list(
                    measure   = tr("Mardia's kurtosis", "Curtosis de Mardia"),
                    value     = mardiaRes$b2p,
                    statistic = mardiaRes$kurtZ,
                    df        = "—",
                    p         = mardiaRes$kurtP,
                    pSig      = p_sig_stars(mardiaRes$kurtP)
                ))
                skewSig <- !is.na(mardiaRes$skewP) && mardiaRes$skewP < .05
                kurtSig <- !is.na(mardiaRes$kurtP) && mardiaRes$kurtP < .05
                mardiaFail <- skewSig || kurtSig
                set_html_safe("mardiaInterpretation", html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    c(
                      tr(sprintf("Multivariate skewness: %s (b1p = %.3f, \u03c7\u00b2(%.1f) = %.2f, p %s).",
                                 if (skewSig) "statistically significant departure from multivariate normality" else "compatible with multivariate normality",
                                 mardiaRes$b1p, mardiaRes$skewDf, mardiaRes$skewStat, format_p(mardiaRes$skewP)),
                         sprintf("Asimetría multivariada: %s (b1p = %.3f, \u03c7\u00b2(%.1f) = %.2f, p %s).",
                                 if (skewSig) "desviación estadísticamente significativa de la normalidad multivariada" else "compatible con normalidad multivariada",
                                 mardiaRes$b1p, mardiaRes$skewDf, mardiaRes$skewStat, format_p(mardiaRes$skewP))),
                      tr(sprintf("Multivariate kurtosis: %s (b2p = %.3f, z = %.2f, p %s).",
                                 if (kurtSig) "statistically significant departure from multivariate normality" else "compatible with multivariate normality",
                                 mardiaRes$b2p, mardiaRes$kurtZ, format_p(mardiaRes$kurtP)),
                         sprintf("Curtosis multivariada: %s (b2p = %.3f, z = %.2f, p %s).",
                                 if (kurtSig) "desviación estadísticamente significativa de la normalidad multivariada" else "compatible con normalidad multivariada",
                                 mardiaRes$b2p, mardiaRes$kurtZ, format_p(mardiaRes$kurtP))),
                      tr(if (mardiaFail)
                             "Because this OLS-based path analysis estimates each equation separately, multivariate normality is not required for the coefficients or their significance tests to be valid; this diagnostic mainly matters if the same model is later re-specified as an SEM estimated by maximum likelihood, where it can affect standard errors and fit indices."
                         else
                             "The set of model variables is compatible with multivariate normality; this is a favorable (though not required) condition if the same model is later re-specified as an SEM estimated by maximum likelihood.",
                         if (mardiaFail)
                             "Dado que este análisis de ruta por OLS estima cada ecuación por separado, la normalidad multivariada no es un requisito para que los coeficientes o sus pruebas de significancia sean válidos; este diagnóstico importa sobre todo si el mismo modelo se replantea más adelante como un SEM estimado por máxima verosimilitud, donde sí puede afectar los errores estándar y los índices de ajuste."
                         else
                             "El conjunto de variables del modelo es compatible con normalidad multivariada; esta es una condición favorable (aunque no obligatoria) si el mismo modelo se replantea más adelante como un SEM estimado por máxima verosimilitud.")
                    )
                ))
            } else {
                set_html_safe("mardiaInterpretation", html_block(NULL,
                    tr("Mardia's test could not be computed (singular covariance matrix or insufficient cases).",
                       "La prueba de Mardia no pudo calcularse (matriz de covarianzas singular o casos insuficientes)."),
                    paragraphs = FALSE))
            }

            # -----------------------------------------------------------------------------
            # Correlation matrices (Pearson and dCor, separate, APA 7 format).
            # ES: Matrices de correlaciones (Pearson y dCor, separadas, formato APA 7).
            # -----------------------------------------------------------------------------
            set_html_safe("correlationMatrixGuide", html_guide(tr("Correlation Matrix", "Matriz de Correlaciones"),
                                                                  "path", "correlationMatrixGuide"))
            pearsonTable <- self$results$pearsonMatrixTable
            pearsonTable$deleteRows()
            dcorTable <- self$results$dcorMatrixTable
            dcorTable$deleteRows()

            # Delegates to the shared .al_fmt_r() / .al_apa_cell()
            # (shared-helpers.R) instead of reimplementing them locally —
            # both were byte-identical to the shared versions.
            # ES: Delega en las funciones compartidas .al_fmt_r() /
            # .al_apa_cell() (shared-helpers.R) en vez de reimplementarlas
            # localmente — ambas eran idénticas, byte a byte, a las
            # versiones compartidas.
            .fmtR <- .al_fmt_r
            .apaCell <- .al_apa_cell

            # Cómputo único por par (Pearson, dCor, entropía copular) para evitar
            # repetir la prueba de permutación de dCor/CE varias veces; el
            # resultado se reutiliza en las matrices APA, en el análisis de
            # discordancia y en la tabla de entropía copular.
            k <- length(vars)
            hasCopent <- requireNamespace("copent", quietly = TRUE)

            B_perm <- tryCatch(as.integer(self$options$permutations), error = function(e) NA_integer_)
            if (is.na(B_perm) || B_perm < 199) B_perm <- 999
            seed_perm <- tryCatch(as.integer(self$options$permutationSeed), error = function(e) NA_integer_)
            if (is.na(seed_perm)) seed_perm <- 2026
            floor_p <- 1 / (B_perm + 1)
            floor_p_str <- sub("^0", "", sprintf("%.3f", floor_p))
            set.seed(seed_perm)

            pairResults <- list()
            for (i in seq_len(k)) {
                for (j in seq_len(k)) {
                    if (j >= i) next()
                    v1 <- vars[i]; v2 <- vars[j]
                    x <- covData[[v1]]; y <- covData[[v2]]
                    ct <- tryCatch(stats::cor.test(x, y, method = "pearson"), error = function(e) NULL)
                    dRes <- tryCatch(private$.dCorTest(x, y, B = B_perm), error = function(e) NULL)
                    ceRes <- if (hasCopent) tryCatch(private$.copentTest(x, y, B = B_perm), error = function(e) NULL) else NULL
                    isConnected <- any(vapply(edges, function(e)
                        (identical(e$pred, v1) && identical(e$dep, v2)) ||
                        (identical(e$pred, v2) && identical(e$dep, v1)), logical(1)))
                    pairResults[[paste(v1, v2, sep = "|")]] <- list(
                        v1 = v1, v2 = v2,
                        pearsonR = if (!is.null(ct)) unname(ct$estimate) else NA_real_,
                        pearsonP = if (!is.null(ct)) ct$p.value else NA_real_,
                        dcor     = if (!is.null(dRes)) dRes$dcor else NA_real_,
                        dcorP    = if (!is.null(dRes)) dRes$p else NA_real_,
                        ce       = if (!is.null(ceRes)) ceRes$ce else NA_real_,
                        ceP      = if (!is.null(ceRes)) ceRes$p else NA_real_,
                        connected = isConnected
                    )
                }
            }

            for (i in seq_len(k)) {
                rowVar <- vars[i]
                pearsonVals <- list(var = sprintf("%d. %s", i, rowVar))
                dcorVals    <- list(var = sprintf("%d. %s", i, rowVar))
                for (j in seq_len(k)) {
                    colName <- paste0("c", j)
                    if (j > i) {
                        pearsonVals[[colName]] <- ""
                        dcorVals[[colName]]    <- ""
                    } else if (j == i) {
                        pearsonVals[[colName]] <- "\u2014"
                        dcorVals[[colName]]    <- "\u2014"
                    } else {
                        pr <- pairResults[[paste(rowVar, vars[j], sep = "|")]]
                        pearsonVals[[colName]] <- if (!is.null(pr)) .apaCell(pr$pearsonR, pr$pearsonP) else ""
                        dcorVals[[colName]]    <- if (!is.null(pr)) .apaCell(pr$dcor, pr$dcorP) else ""
                    }
                }
                pearsonTable$addRow(rowKey = rowVar, values = pearsonVals)
                dcorTable$addRow(rowKey = rowVar, values = dcorVals)
            }

            set_html_safe("correlationMatrixNote", html_block(NULL,
                c(
                    paste0(sprintf("N = %d.", nrow(covData)), " ", .al_text(lang, "common", "sigCodes")),
                    .al_dcor_na_note(lang)
                ),
                paragraphs = FALSE
            ))

            # -----------------------------------------------------------------------------
            # Pearson / dCor / Copula Entropy discordance analysis.
            # ES: Análisis de discordancia Pearson / dCor / Entropía Copular.
            # -----------------------------------------------------------------------------
            set_html_safe("correlationComparisonGuide", html_block(
                tr("Pearson / dCor / Copula Entropy Discordance Analysis", "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                .al_html_list(tr(c(
                    "Because Pearson's r only captures linear association while dCor captures both linear and non-linear association, a pair whose dCor is notably larger than its Pearson |r| is a signal (not proof) of a non-linear relationship that a linear path model may be missing.",
                    "Pairs are flagged in the \"Pairs with a Notable Difference between Pearson and dCor\" table when the gap (dCor minus |Pearson r|) is greater than .10.",
                    "The copula entropy (CE, copent()) result for the same pair is shown alongside as a second, distribution-free line of evidence: when CE is also significant for a flagged pair, this reinforces the suspicion of an unmodeled non-linear dependency; when CE is not significant, the gap should be treated with more caution, since it may simply reflect sampling noise in the dCor permutation test.",
                    "This threshold is a heuristic, not a formal test; always inspect a scatterplot of any flagged pair before concluding the relationship is non-linear.",
                    sprintf("Method: dCor and CE p-values are obtained by permutation (B = %d permutations, seed = %d). The minimum achievable p at this resolution is %s; a p equal to this value should be read as p \u2264 %s (Monte Carlo), not as an exact value. Increase the number of permutations for higher resolution in a final report.",
                            B_perm, seed_perm, floor_p_str, floor_p_str)
                ), c(
                    "Dado que la r de Pearson solo capta asociación lineal mientras que dCor capta asociación lineal y no lineal por igual, un par cuyo dCor sea notablemente mayor que su |r| de Pearson es una señal (no una prueba) de una relación no lineal que un modelo de ruta lineal podría estar pasando por alto.",
                    "Se señalan en la tabla \"Pares con Diferencia Notable entre Pearson y dCor\" los pares con una brecha (dCor menos |r| de Pearson) mayor a .10.",
                    "El resultado de la prueba de entropía copular (CE, copent()) para el mismo par se muestra al lado como una segunda línea de evidencia libre de supuestos distribucionales: cuando CE también es significativa para un par señalado, esto refuerza la sospecha de una dependencia no lineal no modelada; cuando CE no es significativa, la brecha debe tratarse con más cautela, ya que podría reflejar simplemente ruido muestral de la prueba de permutación de dCor.",
                    "Este umbral es una heurística, no una prueba formal; siempre revise un diagrama de dispersión de cualquier par señalado antes de concluir que la relación es no lineal.",
                    sprintf("Método: los valores p de dCor y CE se obtienen por permutación (B = %d permutaciones, semilla = %d). El p mínimo alcanzable con esta resolución es %s; un p igual a este valor debe leerse como p \u2264 %s (Monte Carlo), no como un valor exacto. Aumente el número de permutaciones para mayor resolución en un informe final.",
                            B_perm, seed_perm, floor_p_str, floor_p_str)
                ))),
                raw = TRUE
            ))

            compTable <- self$results$correlationComparisonTable
            compTable$deleteRows()
            gapThreshold <- 0.10
            flaggedPairs <- character(0)
            flaggedCeSig <- 0

            for (key in names(pairResults)) {
                pr <- pairResults[[key]]
                if (is.na(pr$pearsonR) || is.na(pr$dcor)) next()
                gap <- pr$dcor - abs(pr$pearsonR)
                if (gap <= gapThreshold) next()
                flaggedPairs <- c(flaggedPairs, sprintf("%s-%s", pr$v1, pr$v2))
                if (!is.na(pr$ceP) && pr$ceP < .05) flaggedCeSig <- flaggedCeSig + 1
                compTable$addRow(rowKey = key, values = list(
                    var1  = pr$v1,
                    var2  = pr$v2,
                    pearson = .apaCell(pr$pearsonR, pr$pearsonP),
                    dcor    = .apaCell(pr$dcor, pr$dcorP),
                    gap   = gap,
                    ce    = pr$ce,
                    ceP   = pr$ceP,
                    ceSig = p_sig_stars(pr$ceP),
                    flag  = tr("Yes", "Sí")
                ))
            }

            if (length(flaggedPairs) == 0) {
                set_html_safe("correlationComparisonInterpretation", html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    tr(sprintf("No pair shows a Pearson/dCor gap greater than %.2f; there is no indication of unmodeled non-linear association among the model's variables.", gapThreshold),
                       sprintf("Ningún par muestra una brecha Pearson/dCor mayor a %.2f; no hay indicios de asociación no lineal no modelada entre las variables del modelo.", gapThreshold)),
                    paragraphs = FALSE
                ))
            } else {
                set_html_safe("correlationComparisonInterpretation", html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    tr(sprintf("%d pair(s) show a Pearson/dCor gap greater than %.2f: %s. Of these, %d also show significant dependence by copula entropy (p < .05), which reinforces the suspicion of a non-linear relationship for that pair; for the rest, the gap should be treated with more caution and checked against a scatterplot.",
                               length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", "), flaggedCeSig),
                       sprintf("%d par(es) muestran una brecha Pearson/dCor mayor a %.2f: %s. De estos, %d también muestran dependencia significativa por entropía copular (p < .05), lo que refuerza la sospecha de una relación no lineal para ese par; para el resto, la brecha debe tratarse con más cautela y contrastarse con un diagrama de dispersión.",
                               length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", "), flaggedCeSig)),
                    paragraphs = FALSE
                ))
            }

            # -----------------------------------------------------------------------------
            # Independence by copula entropy.
            # ES: Independencia por entropía copular.
            # -----------------------------------------------------------------------------
            set_html_safe("crossEntropyGuide", html_guide(tr("Copula Entropy Independence Test", "Independencia por Entropía Copular"),
                                                             "path", "crossEntropyGuide"))
            ceTable <- self$results$crossEntropyTable
            ceTable$deleteRows()

            if (!hasCopent) {
                set_html_safe("crossEntropyInterpretation", html_block(NULL,
                    tr("The 'copent' package is not installed; this section could not be computed. Install it with install.packages('copent').",
                       "El paquete 'copent' no está instalado; esta sección no pudo calcularse. Instálelo con install.packages('copent')."),
                    paragraphs = FALSE))
            } else {
                nCeSig <- 0
                nCeTotal <- 0
                connectedSigPairs <- character(0)
                for (key in names(pairResults)) {
                    pr <- pairResults[[key]]
                    if (is.na(pr$ce)) next()
                    ceTable$addRow(rowKey = key, values = list(
                        var1      = pr$v1,
                        var2      = pr$v2,
                        connected = if (pr$connected) tr("Yes", "Sí") else tr("No", "No"),
                        ce        = pr$ce,
                        p         = pr$ceP,
                        pSig      = p_sig_stars(pr$ceP)
                    ))
                    nCeTotal <- nCeTotal + 1
                    if (!is.na(pr$ceP) && pr$ceP < .05) {
                        nCeSig <- nCeSig + 1
                        if (!pr$connected) connectedSigPairs <- c(connectedSigPairs, sprintf("%s-%s", pr$v1, pr$v2))
                    }
                }
                unconnNote <- if (length(connectedSigPairs) > 0)
                    tr(sprintf(" Notably, %s show significant dependence despite not being directly connected in the model, which may indicate an omitted path or a common cause not currently included.",
                               paste(connectedSigPairs, collapse = ", ")),
                       sprintf(" En particular, %s muestran dependencia significativa a pesar de no estar conectadas directamente en el modelo, lo que podría indicar una ruta omitida o una causa común no incluida actualmente.",
                               paste(connectedSigPairs, collapse = ", ")))
                else ""
                methodNote <- tr(
                    sprintf(" Method: p-values by permutation (B = %d, seed = %d); minimum achievable p = %s (read as p \u2264 %s, Monte Carlo).",
                            B_perm, seed_perm, floor_p_str, floor_p_str),
                    sprintf(" Método: valores p por permutación (B = %d, semilla = %d); p mínimo alcanzable = %s (léase como p \u2264 %s, Monte Carlo).",
                            B_perm, seed_perm, floor_p_str, floor_p_str)
                )
                set_html_safe("crossEntropyInterpretation", html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    paste0(tr(sprintf("%d of %d variable pair(s) show statistically significant dependence by copula entropy (p < .05).",
                               nCeSig, nCeTotal),
                       sprintf("%d de %d par(es) de variables muestran dependencia estadísticamente significativa por entropía copular (p < .05).",
                               nCeSig, nCeTotal)), unconnNote, methodNote),
                    paragraphs = FALSE
                ))
            }

            # -----------------------------------------------------------------------------
            # Direct, indirect, and total effects (simple mediation).
            # ES: Efectos directos, indirectos y totales (mediación simple).
            # -----------------------------------------------------------------------------
            set_html_safe("indirectEffectsGuide", html_guide(tr("Direct, Indirect and Total Effects", "Efectos Directos, Indirectos y Totales"),
                                                                "path", "indirectEffectsGuide"))
            medTable <- self$results$indirectEffectsTable
            medTable$deleteRows()

            mediationRows <- list()
            for (edge in edges) {
                X <- edge$pred; Y <- edge$dep
                candidateMediators <- unique(vapply(
                    Filter(function(e) identical(e$pred, X), edges), function(e) e$dep, character(1)))
                for (M in candidateMediators) {
                    if (identical(M, Y)) next()
                    hasMY <- any(vapply(edges, function(e) identical(e$pred, M) && identical(e$dep, Y), logical(1)))
                    if (!hasMY) next()
                    aStats <- edgeStats[[paste(X, M, sep = "->")]]
                    bStats <- edgeStats[[paste(M, Y, sep = "->")]]
                    if (is.null(aStats) || is.null(bStats)) next()
                    dStats <- edgeStats[[paste(X, Y, sep = "->")]]

                    indirectBeta <- aStats$beta * bStats$beta
                    indirectEst  <- aStats$est * bStats$est
                    sobelSE <- sqrt(bStats$est^2 * aStats$se^2 + aStats$est^2 * bStats$se^2)
                    sobelZ  <- if (sobelSE > 0) indirectEst / sobelSE else NA_real_
                    sobelP  <- if (!is.na(sobelZ)) 2 * stats::pnorm(-abs(sobelZ)) else NA_real_

                    directBeta <- if (!is.null(dStats)) dStats$beta else NA_real_
                    totalBeta  <- if (!is.na(directBeta)) directBeta + indirectBeta else indirectBeta

                    mediationRows[[length(mediationRows) + 1]] <- list(
                        x = X, m = M, y = Y,
                        directBeta = directBeta, indirectBeta = indirectBeta, totalBeta = totalBeta,
                        sobelZ = sobelZ, sobelP = sobelP
                    )
                }
            }

            for (mr in mediationRows) {
                medTable$addRow(rowKey = paste(mr$x, mr$m, mr$y, sep = "|"), values = list(
                    x = mr$x, m = mr$m, y = mr$y,
                    directBeta = mr$directBeta, indirectBeta = mr$indirectBeta, totalBeta = mr$totalBeta,
                    sobelZ = mr$sobelZ, sobelP = mr$sobelP, sobelPSig = p_sig_stars(mr$sobelP)
                ))
            }

            if (length(mediationRows) == 0) {
                set_html_safe("indirectEffectsInterpretation", html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    tr("No variable in this model both receives a path from a predictor and sends a path to that same predictor's other outcome, so there is no simple single-mediator chain to decompose.",
                       "Ninguna variable de este modelo recibe una ruta de un predictor y a la vez envía una ruta hacia otra variable dependiente de ese mismo predictor, por lo que no hay una cadena de mediación simple de un solo mediador que descomponer."),
                    paragraphs = FALSE
                ))
            } else {
                medLines <- vapply(mediationRows, function(mr) {
                    sigWord <- if (!is.na(mr$sobelP) && mr$sobelP < .05)
                        tr("statistically significant", "estadísticamente significativo")
                    else
                        tr("not statistically significant", "sin significancia estadística")
                    pattern <- if (is.na(mr$directBeta))
                        tr("there is no direct path from %s to %s, so the entire effect is indirect through %s",
                           "no hay ruta directa de %s a %s, por lo que todo el efecto es indirecto a través de %s")
                    else if (sign(mr$directBeta) == sign(mr$indirectBeta))
                        tr("the direct and indirect effects point in the same direction, so %3$s partially explains the effect of %1$s on %2$s alongside the direct path",
                           "los efectos directo e indirecto apuntan en la misma dirección, por lo que %3$s explica parcialmente el efecto de %1$s sobre %2$s junto con la ruta directa")
                    else
                        tr("the direct and indirect effects point in opposite directions (a suppression / inconsistent-mediation pattern), so the total effect of %1$s on %2$s is smaller than either path alone would suggest",
                           "los efectos directo e indirecto apuntan en direcciones opuestas (un patrón de supresión / mediación inconsistente), por lo que el efecto total de %1$s sobre %2$s es menor de lo que cualquiera de las dos rutas por separado sugeriría")
                    patternTxt <- tryCatch(sprintf(pattern, mr$x, mr$y, mr$m), error = function(e)
                        sprintf(pattern, mr$x, mr$y, mr$m))
                    tr(sprintf("%s -> %s -> %s: indirect effect \u03b2 = %.3f (Sobel z = %.2f, p %s, %s); total effect \u03b2 = %.3f. In this chain, %s.",
                               mr$x, mr$m, mr$y, mr$indirectBeta, mr$sobelZ, format_p(mr$sobelP), sigWord, mr$totalBeta, patternTxt),
                       sprintf("%s -> %s -> %s: efecto indirecto \u03b2 = %.3f (z de Sobel = %.2f, p %s, %s); efecto total \u03b2 = %.3f. En esta cadena, %s.",
                               mr$x, mr$m, mr$y, mr$indirectBeta, mr$sobelZ, format_p(mr$sobelP), sigWord, mr$totalBeta, patternTxt))
                }, character(1))

                set_html_safe("indirectEffectsInterpretation", html_block(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    c(medLines,
                      tr("This decomposition only covers single-mediator chains (X -> M -> Y); longer mediating chains are not yet computed. The Sobel test assumes the indirect effect's sampling distribution is approximately normal, which can be inaccurate in small samples; a bootstrap confidence interval for the indirect effect is preferable when in doubt.",
                         "Esta descomposición solo cubre cadenas de un mediador (X -> M -> Y); las cadenas de mediación más largas todavía no se calculan. La prueba de Sobel asume que la distribución muestral del efecto indirecto es aproximadamente normal, lo cual puede ser impreciso en muestras pequeñas; ante la duda, es preferible un intervalo de confianza bootstrap para el efecto indirecto."))
                ))
            }

            # -----------------------------------------------------------------------------
            # Sample-size assessment by model complexity.
            # ES: Valoración del tamaño de muestra según la complejidad del modelo.
            # -----------------------------------------------------------------------------
            set_html_safe("sampleSizeGuide", html_block(
                tr("Sample Size Adequacy", "Adecuación del Tamaño de Muestra"),
                tr(c(
                    "There is no single agreed-upon rule for how large a sample a path model needs; the literature offers several complementary heuristics that weigh model complexity differently.",
                    "Because this module estimates each equation separately by OLS rather than jointly by maximum likelihood, the regression-based rules (Green, 1991) are the most directly applicable, one per equation; the ratio-based rules developed for the broader structural-equation-model family (Bentler & Chou, 1987; Jackson, 2003; Kline, 2023) are shown as additional, more conservative benchmarks, because this path model has the same structural complexity (the same number of estimated paths) as an equivalent SEM.",
                    "Wolf et al. (2013) show via simulation that the sample size actually needed depends heavily on effect size, indicator reliability, and the specific model, and can range from under 30 to several hundred cases for models of similar size; treat every rule in the \"Sample-Size Assessment by Model Complexity\" table as a heuristic floor, not a guarantee of adequate statistical power."
                ), c(
                    "No existe una única regla consensuada sobre qué tan grande debe ser la muestra de un modelo de ruta; la literatura ofrece varias heurísticas complementarias que ponderan la complejidad del modelo de forma distinta.",
                    "Dado que este módulo estima cada ecuación por separado mediante OLS en vez de conjuntamente por máxima verosimilitud, las reglas basadas en regresión (Green, 1991) son las más directamente aplicables, una por ecuación; las reglas de razón desarrolladas para la familia más amplia de modelos de ecuaciones estructurales (Bentler & Chou, 1987; Jackson, 2003; Kline, 2023) se muestran como referencias adicionales más conservadoras, porque este modelo de ruta tiene la misma complejidad estructural (el mismo número de rutas estimadas) que un SEM equivalente.",
                    "Wolf et al. (2013) muestran mediante simulación que el tamaño de muestra realmente necesario depende fuertemente del tamaño del efecto, la confiabilidad de los indicadores y el modelo específico, y puede ir de menos de 30 a varios cientos de casos para modelos de tamaño similar; trate cada regla de la tabla \"Valoración del Tamaño de Muestra según la Complejidad del Modelo\" como un piso heurístico, no como garantía de potencia estadística adecuada."
                ))
            ))

            N <- nrow(covData)
            nRel <- length(edges)
            maxPreds <- if (length(fits) > 0)
                max(vapply(fits, function(f) length(f$preds), integer(1)))
            else 0L

            sizeStatus <- function(required) {
                if (is.na(required)) return("\u2014")
                if (N >= required) tr("Meets", "Cumple")
                else if (N >= 0.9 * required) tr("Marginal", "Marginal")
                else tr("Does not meet", "No cumple")
            }

            sizeCriteria <- list(
                list(criterion = tr("Green (1991) \u2014 overall R\u00b2 test", "Green (1991) \u2014 prueba de R\u00b2 global"),
                     rule = tr("N \u2265 50 + 8m (m = predictors in the largest equation)", "N \u2265 50 + 8m (m = predictores de la ecuación más grande)"),
                     required = 50 + 8 * maxPreds),
                list(criterion = tr("Green (1991) \u2014 individual coefficients test", "Green (1991) \u2014 prueba de coeficientes individuales"),
                     rule = tr("N \u2265 104 + m", "N \u2265 104 + m"),
                     required = 104 + maxPreds),
                list(criterion = tr("Bentler & Chou (1987) \u2014 minimum cases:parameters ratio", "Bentler & Chou (1987) \u2014 razón mínima casos:parámetros"),
                     rule = tr("N \u2265 5 \u00d7 (number of estimated paths)", "N \u2265 5 \u00d7 (número de rutas estimadas)"),
                     required = 5 * nRel),
                list(criterion = tr("Jackson (2003) \u2014 recommended N:q ratio", "Jackson (2003) \u2014 razón N:q recomendada"),
                     rule = tr("N \u2265 10 \u00d7 (number of estimated paths); ideally closer to 20\u00d7", "N \u2265 10 \u00d7 (número de rutas estimadas); idealmente cercano a 20\u00d7"),
                     required = 10 * nRel),
                list(criterion = tr("Kline (2023) \u2014 general absolute minimum", "Kline (2023) \u2014 mínimo absoluto general"),
                     rule = tr("N \u2265 200 as a general floor for models of this structural complexity", "N \u2265 200 como piso general para modelos de esta complejidad estructural"),
                     required = 200)
            )

            szTable <- self$results$sampleSizeTable
            szTable$deleteRows()
            nMet <- 0
            for (i in seq_along(sizeCriteria)) {
                sc <- sizeCriteria[[i]]
                st <- sizeStatus(sc$required)
                if (identical(st, tr("Meets", "Cumple"))) nMet <- nMet + 1
                szTable$addRow(rowKey = i, values = list(
                    criterion = sc$criterion, rule = sc$rule,
                    required = sc$required, actual = N, status = st
                ))
            }

            set_html_safe("sampleSizeInterpretation", html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                tr(sprintf("With N = %d complete cases, %d estimated path(s) across %d equation(s) (largest equation has %d predictor(s)), the model meets %d of %d sample-size criteria in the \"Sample-Size Assessment by Model Complexity\" table.",
                           N, nRel, length(fits), maxPreds, nMet, length(sizeCriteria)),
                   sprintf("Con N = %d casos completos, %d ruta(s) estimada(s) en %d ecuación(es) (la ecuación más grande tiene %d predictor(es)), el modelo cumple %d de %d criterios de la tabla \"Valoración del Tamaño de Muestra según la Complejidad del Modelo\".",
                           N, nRel, length(fits), maxPreds, nMet, length(sizeCriteria))),
                paragraphs = FALSE
            ))

            # -----------------------------------------------------------------------------
            # Model diagnostic conclusion.
            # ES: Conclusión diagnóstica del modelo.
            # -----------------------------------------------------------------------------
            set_html_safe("modelDiagnosticConclusionGuide", html_block(
                tr("Diagnostic Conclusion", "Conclusión Diagnóstica del Modelo"),
                tr("This final section brings together every check run above into a single scorecard, so the overall methodological soundness of the estimated model can be judged at a glance instead of having to hold every individual result in mind.",
                   "Esta sección final reúne todas las verificaciones anteriores en un solo marcador, de modo que la solidez metodológica general del modelo estimado pueda juzgarse de un vistazo en vez de tener que retener cada resultado individual en la mente."),
                paragraphs = FALSE
            ))

            get0safe <- function(name, default) {
                val <- tryCatch(get(name, inherits = TRUE), error = function(e) NULL)
                if (is.null(val)) default else val
            }

            outlierN     <- if (!is.null(mahaResult)) length(mahaResult$d2) else NA_integer_
            outlierFlag  <- get0safe("nFlaggedD2", NA_integer_)
            mardiaOk     <- !isTRUE(get0safe("mardiaFail", FALSE))
            ceSigN       <- get0safe("nCeSig", NA_integer_)
            ceTotalN     <- get0safe("nCeTotal", NA_integer_)
            unconnCe     <- get0safe("connectedSigPairs", character(0))
            gapFlaggedN  <- length(get0safe("flaggedPairs", character(0)))
            gapCeReinf   <- get0safe("flaggedCeSig", 0)
            medSigChains <- if (length(mediationRows) > 0)
                sum(vapply(mediationRows, function(mr) !is.na(mr$sobelP) && mr$sobelP < .05, logical(1)))
            else 0

            concTable <- self$results$modelDiagnosticConclusionTable
            concTable$deleteRows()

            addConc <- function(area, status, detail) {
                concTable$addRow(rowKey = area, values = list(area = area, status = status, detail = detail))
            }

            addConc(tr("Sample size", "Tamaño de muestra"),
                    if (nMet == length(sizeCriteria)) tr("No issues", "Sin problemas")
                    else if (nMet >= length(sizeCriteria) / 2) tr("Attention", "Atención")
                    else tr("Problem", "Problema"),
                    tr(sprintf("Meets %d of %d criteria (see section above).", nMet, length(sizeCriteria)),
                       sprintf("Cumple %d de %d criterios (ver sección anterior).", nMet, length(sizeCriteria))))

            addConc(tr("Multivariate outliers", "Atípicos multivariados"),
                    if (is.na(outlierFlag)) tr("Could not be computed", "No pudo calcularse")
                    else if (outlierFlag == 0) tr("No issues", "Sin problemas")
                    else tr("Attention", "Atención"),
                    if (is.na(outlierFlag) || is.na(outlierN)) tr("Could not be computed.", "No pudo calcularse.")
                    else tr(sprintf("%d of %d cases exceed the Mahalanobis D\u00b2 threshold.", outlierFlag, outlierN),
                            sprintf("%d de %d casos superan el umbral de D\u00b2 de Mahalanobis.", outlierFlag, outlierN)))

            addConc(tr("Residual normality", "Normalidad de residuos"),
                    if (nNormalityFail == 0) tr("No issues", "Sin problemas") else tr("Attention", "Atención"),
                    tr(sprintf("%d of %d test-equation combinations show non-normal residuals.", nNormalityFail, nNormalityTotal),
                       sprintf("%d de %d combinaciones prueba-ecuación muestran residuos no normales.", nNormalityFail, nNormalityTotal)))

            addConc(tr("Homoscedasticity", "Homoscedasticidad"),
                    if (nHomoFail == 0) tr("No issues", "Sin problemas") else tr("Attention", "Atención"),
                    tr(sprintf("%d of %d test-equation combinations show evidence of heteroscedasticity.", nHomoFail, nHomoTotal),
                       sprintf("%d de %d combinaciones prueba-ecuación muestran evidencia de heterocedasticidad.", nHomoFail, nHomoTotal)))

            addConc(tr("Multicollinearity", "Multicolinealidad"),
                    if (is.na(maxVif)) tr("Not applicable", "No aplica")
                    else if (maxVif > 10) tr("Problem", "Problema")
                    else if (maxVif > 5) tr("Attention", "Atención")
                    else tr("No issues", "Sin problemas"),
                    if (is.na(maxVif)) tr("No equation has two or more predictors.", "Ninguna ecuación tiene dos o más predictores.")
                    else tr(sprintf("Maximum VIF = %.2f (%s).", maxVif, maxVifLabel),
                            sprintf("VIF máximo = %.2f (%s).", maxVif, maxVifLabel)))

            addConc(tr("Multivariate normality (Mardia)", "Normalidad multivariada (Mardia)"),
                    if (mardiaOk) tr("No issues", "Sin problemas") else tr("Attention", "Atención"),
                    tr("Only relevant if this model is later re-estimated as an ML-based SEM.",
                       "Solo relevante si este modelo se reestima más adelante como un SEM basado en máxima verosimilitud."))

            addConc(tr("Pearson/dCor discordance", "Discordancia Pearson/dCor"),
                    if (gapFlaggedN == 0) tr("No issues", "Sin problemas") else tr("Attention", "Atención"),
                    if (gapFlaggedN == 0) tr("No pair shows a notable linear/non-linear gap.", "Ningún par muestra una brecha lineal/no lineal notable.")
                    else tr(sprintf("%d pair(s) flagged, %d reinforced by a significant copula entropy test.", gapFlaggedN, gapCeReinf),
                            sprintf("%d par(es) señalado(s), %d reforzado(s) por una prueba de entropía copular significativa.", gapFlaggedN, gapCeReinf)))

            addConc(tr("Copula entropy independence", "Independencia por entropía copular"),
                    if (is.na(ceSigN)) tr("Not computed (copent unavailable)", "No calculado (copent no disponible)")
                    else if (length(unconnCe) > 0) tr("Attention", "Atención")
                    else tr("No issues", "Sin problemas"),
                    if (is.na(ceSigN)) tr("Install the 'copent' package to enable this check.", "Instale el paquete 'copent' para habilitar esta verificación.")
                    else tr(sprintf("%d of %d pairs significant; %d involve variables not directly connected in the model.", ceSigN, ceTotalN, length(unconnCe)),
                            sprintf("%d de %d pares significativos; %d involucran variables no conectadas directamente en el modelo.", ceSigN, ceTotalN, length(unconnCe))))

            addConc(tr("Mediation (indirect effects)", "Mediación (efectos indirectos)"),
                    if (length(mediationRows) == 0) tr("Not applicable", "No aplica")
                    else if (medSigChains > 0) tr("Notable", "Destacable")
                    else tr("No issues", "Sin problemas"),
                    if (length(mediationRows) == 0) tr("No single-mediator chain exists in this model.", "No existe ninguna cadena de un solo mediador en este modelo.")
                    else tr(sprintf("%d of %d mediation chain(s) show a statistically significant indirect effect (Sobel).", medSigChains, length(mediationRows)),
                            sprintf("%d de %d cadena(s) de mediación muestran un efecto indirecto estadísticamente significativo (Sobel).", medSigChains, length(mediationRows))))

            areaLabels <- c(
                sampleSize        = tr("sample size", "tamaño de muestra"),
                outliers          = tr("multivariate outliers", "atípicos multivariados"),
                normality         = tr("residual normality", "normalidad de residuos"),
                homoscedasticity  = tr("homoscedasticity", "homoscedasticidad"),
                multicollinearity = tr("multicollinearity", "multicolinealidad"),
                mardia            = tr("multivariate normality (Mardia)", "normalidad multivariada (Mardia)"),
                pearsonDcorGap    = tr("Pearson/dCor discordance", "discordancia Pearson/dCor"),
                crossEntropy      = tr("copula entropy independence", "independencia por entropía copular")
                # "mediation" queda fuera adrede: un efecto indirecto significativo es un
                # hallazgo sustantivo, no un problema metodológico, y no debe contarse
                # como "área que requiere atención" junto a multicolinealidad/heterocedasticidad.
            )
            areaFlags <- c(
                sampleSize        = nMet < length(sizeCriteria),
                outliers          = !is.na(outlierFlag) && outlierFlag > 0,
                normality         = nNormalityFail > 0,
                homoscedasticity  = nHomoFail > 0,
                multicollinearity = !is.na(maxVif) && maxVif > 5,
                mardia            = !mardiaOk,
                pearsonDcorGap    = gapFlaggedN > 0,
                crossEntropy      = !is.na(ceSigN) && length(unconnCe) > 0
            )
            nFlaggedAreas <- sum(areaFlags, na.rm = TRUE)
            totalAreas <- length(areaFlags)
            flaggedAreaLabels <- unname(areaLabels[areaFlags])

            overallVerdict <- if (nFlaggedAreas == 0)
                tr("Every diagnostic check above is clean: the model shows no notable outliers, its residuals are compatible with normality and homoscedasticity, there is no relevant multicollinearity, the sample size meets the benchmarks reviewed, and no unexplained non-linear or omitted-path signal was detected. This is a well-behaved OLS-based path model given the checks implemented so far.",
                   "Todas las verificaciones anteriores están limpias: el modelo no muestra atípicos notables, sus residuos son compatibles con normalidad y homoscedasticidad, no hay multicolinealidad relevante, el tamaño de muestra cumple los referentes revisados, y no se detectó ninguna señal no explicada de no linealidad o de ruta omitida. Este es un modelo de ruta basado en OLS bien comportado, dadas las verificaciones implementadas hasta ahora.")
            else
                tr(sprintf("%d of %d diagnostic areas need attention: %s. None of these individually invalidates the model, but taken together they indicate where interpretation should be most cautious; address these flagged areas (particularly any severe multicollinearity or heteroscedasticity, since those directly affect the reliability of the path coefficients' significance tests) before treating the model's conclusions as final.",
                           nFlaggedAreas, totalAreas, paste(flaggedAreaLabels, collapse = ", ")),
                   sprintf("%d de %d áreas diagnósticas requieren atención: %s. Ninguna invalida el modelo por sí sola, pero en conjunto indican dónde la interpretación debe ser más cautelosa; atienda estas áreas señaladas (en particular cualquier multicolinealidad o heterocedasticidad severa, ya que afectan directamente la confiabilidad de las pruebas de significancia de los coeficientes de ruta) antes de tratar las conclusiones del modelo como definitivas.",
                           nFlaggedAreas, totalAreas, paste(flaggedAreaLabels, collapse = ", ")))

            set_html_safe("modelDiagnosticConclusionInterpretation", html_block(
                tr("Overall Verdict", "Veredicto General"),
                overallVerdict,
                paragraphs = FALSE
            ))
        },

        # Calcula posiciones (x,y) en [0,1] para cada variable según el diseño elegido.
        .pathLabelSeq = function(n) {
            letters26 <- letters
            out <- character(n)
            for (i in seq_len(n)) {
                cyc <- (i - 1) %/% 26
                idx <- (i - 1) %% 26 + 1
                out[i] <- if (cyc == 0) letters26[idx] else paste0(letters26[idx], cyc + 1)
            }
            out
        },

        .layoutNodes = function(vars, edges, layout, rotate = "exogLeft") {
            n <- length(vars)
            if (n == 0) return(list(x = stats::setNames(numeric(0), character(0)),
                                     y = stats::setNames(numeric(0), character(0))))

            if (identical(layout, "circular")) {
                theta <- seq(0, 2 * pi, length.out = n + 1)[seq_len(n)]
                x <- stats::setNames(0.5 + 0.36 * cos(theta), vars)
                y <- stats::setNames(0.5 + 0.36 * sin(theta), vars)
                return(list(x = x, y = y))
            }

            if (identical(layout, "spring")) {
                if (n == 1) return(list(x = stats::setNames(0.5, vars), y = stats::setNames(0.5, vars)))
                set.seed(42)
                pos <- matrix(stats::runif(n * 2, 0.25, 0.75), ncol = 2)
                rownames(pos) <- vars
                edgeIdx <- Filter(function(ix) !any(is.na(ix)),
                                   lapply(edges, function(e) c(match(e$pred, vars), match(e$dep, vars))))
                k <- 1 / sqrt(n)
                for (iter in 1:150) {
                    disp <- matrix(0, n, 2)
                    for (i in seq_len(n)) {
                        deltas <- pos[i, , drop = FALSE][rep(1, n), ] - pos
                        dist <- pmax(sqrt(rowSums(deltas^2)), 1e-3)
                        force <- (k^2) / dist
                        contrib <- (deltas / dist) * force
                        contrib[i, ] <- 0
                        disp[i, ] <- colSums(contrib)
                    }
                    for (ix in edgeIdx) {
                        delta <- pos[ix[1], ] - pos[ix[2], ]
                        dist <- max(sqrt(sum(delta^2)), 1e-3)
                        force <- (dist^2) / k
                        disp[ix[1], ] <- disp[ix[1], ] - (delta / dist) * force * 0.5
                        disp[ix[2], ] <- disp[ix[2], ] + (delta / dist) * force * 0.5
                    }
                    temp <- 0.08 * (1 - iter / 150)
                    for (i in seq_len(n)) {
                        dlen <- max(sqrt(sum(disp[i, ]^2)), 1e-4)
                        pos[i, ] <- pos[i, ] + (disp[i, ] / dlen) * min(dlen, temp)
                    }
                }
                normalize <- function(v) {
                    r <- range(v)
                    if (diff(r) < 1e-6) return(rep(0.5, length(v)))
                    0.15 + 0.7 * (v - r[1]) / diff(r)
                }
                x <- stats::setNames(normalize(pos[, 1]), vars)
                y <- stats::setNames(normalize(pos[, 2]), vars)
                return(list(x = x, y = y))
            }

            # Jerárquico (por defecto): exógenas y endógenas en extremos opuestos,
            # con la orientación controlada por `rotate` (Izquierda/Arriba/Derecha/Abajo)
            deps <- unique(vapply(edges, function(e) e$dep, character(1)))
            exo  <- setdiff(vars, deps)
            endo <- deps
            firstVars  <- if (length(exo)  > 0) exo  else vars
            secondVars <- if (length(endo) > 0) endo else character(0)
            firstVars  <- union(firstVars, setdiff(vars, union(exo, endo)))

            posAlong <- function(items) {
                m <- length(items)
                if (m == 0) return(numeric(0))
                stats::setNames(seq(0.85, 0.15, length.out = max(m, 1))[seq_len(m)], items)
            }

            if (identical(rotate, "exogTop")) {
                y <- c(stats::setNames(rep(0.85, length(firstVars)), firstVars),
                       stats::setNames(rep(0.15, length(secondVars)), secondVars))
                x <- c(posAlong(firstVars), posAlong(secondVars))
            } else if (identical(rotate, "exogRight")) {
                x <- c(stats::setNames(rep(0.85, length(firstVars)), firstVars),
                       stats::setNames(rep(0.15, length(secondVars)), secondVars))
                y <- c(posAlong(firstVars), posAlong(secondVars))
            } else if (identical(rotate, "exogBottom")) {
                y <- c(stats::setNames(rep(0.15, length(firstVars)), firstVars),
                       stats::setNames(rep(0.85, length(secondVars)), secondVars))
                x <- c(posAlong(firstVars), posAlong(secondVars))
            } else {
                # exogLeft (por defecto)
                x <- c(stats::setNames(rep(0.15, length(firstVars)), firstVars),
                       stats::setNames(rep(0.85, length(secondVars)), secondVars))
                y <- c(posAlong(firstVars), posAlong(secondVars))
            }
            list(x = x, y = y)
        },

        # Contorno poligonal de un nodo (elipse, círculo o rectángulo) para dibujarlo con geom_polygon.
        .nodeShapeDf = function(cx, cy, label, shape, rx = 0.095, ry = 0.05) {
            if (identical(shape, "rectangle")) {
                return(data.frame(
                    x = c(cx - rx, cx + rx, cx + rx, cx - rx),
                    y = c(cy - ry, cy - ry, cy + ry, cy + ry),
                    id = label, stringsAsFactors = FALSE
                ))
            }
            if (identical(shape, "circle")) ry <- rx
            t <- seq(0, 2 * pi, length.out = 41)
            data.frame(x = cx + rx * cos(t), y = cy + ry * sin(t), id = label, stringsAsFactors = FALSE)
        },

        # -----------------------------------------------------------------------------
        # Path diagram (ggplot2, with style/layout/shape parametrization).
        # ES: Diagrama de ruta (ggplot2, con parametrización de estilo, diseño y forma).
        # -----------------------------------------------------------------------------

        .plotPathDiagram = function(image, ...) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                image$setError("The ggplot2 package is required to draw the path diagram.")
                return(FALSE)
            }

            vars <- self$options$vars
            if (length(vars) == 0) return(FALSE)

            result <- tryCatch({
            pal <- private$.plotPalette()
            edges <- private$.buildEdges(self$options$relations)
            shape <- self$options$diagramShape
            edgeLabelMode <- self$options$diagramEdgeLabel
            showErrors <- isTRUE(self$options$diagramShowErrors)
            offsetLabels <- isTRUE(self$options$diagramOffsetLabels)
            rotate <- self$options$diagramRotate
            if (is.null(rotate) || !nzchar(rotate)) rotate <- "exogLeft"

            nodeSizeOpt <- self$options$diagramNodeSize
            if (is.null(nodeSizeOpt) || !nzchar(nodeSizeOpt)) nodeSizeOpt <- "medium"
            sizeMult <- switch(nodeSizeOpt, small = 0.72, large = 1.32, 1)
            fontSize <- 10 * sizeMult
            ggSize <- fontSize / 2.845276

            abbrevOpt <- self$options$diagramAbbreviate
            if (is.null(abbrevOpt) || !nzchar(abbrevOpt)) abbrevOpt <- "none"
            abbrevLabel <- function(v) {
                if (identical(abbrevOpt, "abbrev4")) return(substr(v, 1, 4))
                if (identical(abbrevOpt, "abbrev8")) return(substr(v, 1, 8))
                v
            }

            layout <- private$.layoutNodes(vars, edges, self$options$diagramLayout, rotate)
            posX <- layout$x
            posY <- layout$y
            allItems <- vars
            rx <- 0.095 * sizeMult; ry <- 0.05 * sizeMult
            if (identical(shape, "circle")) ry <- rx

            fits <- private$.pathFits
            deps <- unique(vapply(edges, function(e) e$dep, character(1)))

            # Etiquetas de ruta secuenciales (a, b, c, ...) para el modo "labels"
            pathLetters <- if (length(edges) > 0) {
                stats::setNames(private$.pathLabelSeq(length(edges)), vapply(edges, function(e) paste(e$pred, e$dep, sep = "->"), character(1)))
            } else {
                character(0)
            }

            edgeRows <- NULL
            if (length(edges) > 0) {
                edgeRows <- do.call(rbind, lapply(edges, function(e) {
                    if (!(e$pred %in% names(posX)) || !(e$dep %in% names(posX))) return(NULL)
                    label <- NA_character_
                    sig <- FALSE
                    if (identical(edgeLabelMode, "labels")) {
                        label <- pathLetters[[paste(e$pred, e$dep, sep = "->")]]
                    } else if (!identical(edgeLabelMode, "none") && !is.null(fits) && !is.null(fits[[e$dep]])) {
                        fit <- fits[[e$dep]]$fit
                        co <- summary(fit)$coefficients
                        rn <- e$pred
                        if (rn %in% rownames(co)) {
                            pval <- co[rn, "Pr(>|t|)"]
                            stars <- .al_p_sig(pval)
                            if (identical(edgeLabelMode, "coef")) {
                                label <- sprintf("%.3f%s", co[rn, "Estimate"], stars)
                            } else {
                                d <- fits[[e$dep]]$data
                                sdY <- stats::sd(d[[e$dep]])
                                sdX <- stats::sd(d[[e$pred]])
                                beta <- co[rn, "Estimate"] * (sdX / sdY)
                                label <- sprintf("%.3f%s", beta, stars)
                            }
                            sig <- !is.na(pval) && pval < .05
                        }
                    }
                    x0 <- posX[e$pred]; y0 <- posY[e$pred]
                    depNodeX <- posX[e$dep]
                    depNodeY <- posY[e$dep]
                    dx <- depNodeX - x0; dy <- depNodeY - y0
                    dist <- max(sqrt(dx^2 + dy^2), 1e-4)
                    ux <- dx / dist; uy <- dy / dist
                    margin <- mean(c(rx, ry)) * 1.05
                    data.frame(
                        x = x0 + ux * margin, y = y0 + uy * margin,
                        xend = depNodeX - ux * margin, yend = depNodeY - uy * margin,
                        label = label, sig = sig,
                        perpX = -uy, perpY = ux,
                        stringsAsFactors = FALSE
                    )
                }))
            }

            nodePolys <- do.call(rbind, lapply(allItems, function(v) {
                private$.nodeShapeDf(posX[v], posY[v], v, shape, rx, ry)
            }))
            nodesDf <- data.frame(x = posX[allItems], y = posY[allItems],
                                   label = vapply(allItems, abbrevLabel, character(1)),
                                   stringsAsFactors = FALSE)

            plot <- ggplot2::ggplot() + ggplot2::xlim(-0.05, 1.05) + ggplot2::ylim(-0.05, 1.05)

            if (!is.null(edgeRows) && nrow(edgeRows) > 0) {
                edgeRows$edgeColor <- ifelse(is.na(edgeRows$label), pal$line,
                                             ifelse(edgeRows$sig, pal$edgeSig, pal$edgeNs))
                plot <- plot +
                    ggplot2::geom_segment(
                        data = edgeRows,
                        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                        color = edgeRows$edgeColor, linewidth = 0.9,
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed", angle = 22)
                    )
                if (!identical(edgeLabelMode, "none")) {
                    labeled <- edgeRows[!is.na(edgeRows$label), , drop = FALSE]
                    if (nrow(labeled) > 0) {
                        labeled$mx <- (labeled$x + labeled$xend) / 2
                        labeled$my <- (labeled$y + labeled$yend) / 2
                        if (offsetLabels) {
                            labeled$mx <- labeled$mx + labeled$perpX * 0.045
                            labeled$my <- labeled$my + labeled$perpY * 0.045
                        }
                        plot <- plot +
                            ggplot2::geom_label(
                                data = labeled,
                                ggplot2::aes(x = mx, y = my, label = label),
                                size = ggSize * 0.85, label.size = 0.2, color = pal$text,
                                fill = "white", label.padding = ggplot2::unit(0.12, "lines")
                            )
                    }
                }
            }

            if (showErrors && length(deps) > 0 && !is.null(fits)) {
                errRows <- do.call(rbind, lapply(deps, function(d) {
                    if (!(d %in% names(posX)) || is.null(fits[[d]])) return(NULL)
                    r2 <- summary(fits[[d]]$fit)$r.squared
                    data.frame(
                        x = posX[d] + rx * 1.9, y = posY[d] + ry * 2.1,
                        xend = posX[d], yend = posY[d],
                        label = sprintf("e (%.2f)", sqrt(max(1 - r2, 0))),
                        stringsAsFactors = FALSE
                    )
                }))
                if (!is.null(errRows) && nrow(errRows) > 0) {
                    plot <- plot +
                        ggplot2::geom_segment(
                            data = errRows,
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            color = pal$line, linewidth = 0.5, linetype = "dashed",
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed")
                        ) +
                        ggplot2::geom_text(
                            data = errRows,
                            ggplot2::aes(x = x, y = y, label = label),
                            color = pal$text, size = ggSize * 0.7
                        )
                }
            }

            plot <- plot +
                ggplot2::geom_polygon(data = nodePolys,
                                       ggplot2::aes(x = x, y = y, group = id),
                                       fill = pal$nodeFill, color = pal$nodeLine, linewidth = 1) +
                ggplot2::geom_text(data = nodesDf, ggplot2::aes(x = x, y = y, label = label),
                                    color = pal$text, size = ggSize, fontface = "bold") +
                ggplot2::coord_fixed() +
                ggplot2::theme_void() +
                ggplot2::theme(legend.position = "none")

            print(plot)
            TRUE
            }, error = function(e) {
                image$setError(paste0(
                    "No se pudo dibujar el diagrama de ruta: ", conditionMessage(e)
                ))
                FALSE
            })

            result
        },

        .plotResidualHistograms = function(image, ...) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                image$setError("The ggplot2 package is required to draw diagnostic plots.")
                return(FALSE)
            }

            fits <- private$.pathFits
            if (is.null(fits) || length(fits) == 0) return(FALSE)

            pal <- private$.plotPalette()
            deps <- names(fits)
            cat_cols <- private$.plotCategoricalPalette(length(deps))
            names(cat_cols) <- deps

            d <- do.call(rbind, lapply(deps, function(dep) {
                res <- stats::residuals(fits[[dep]]$fit)
                data.frame(dep = dep, stdResidual = scale(res)[, 1], stringsAsFactors = FALSE)
            }))
            d <- d[is.finite(d$stdResidual), , drop = FALSE]
            d$isExtreme <- abs(d$stdResidual) > 2.5

            bin_method <- tryCatch(self$options$residBinMethod, error = function(e) "sturges")
            if (is.null(bin_method) || !nzchar(bin_method)) bin_method <- "sturges"
            n_bins <- tryCatch({
                if (identical(bin_method, "scott")) grDevices::nclass.scott(d$stdResidual)
                else if (identical(bin_method, "fd")) grDevices::nclass.FD(d$stdResidual)
                else grDevices::nclass.Sturges(d$stdResidual)
            }, error = function(e) 20)
            if (!is.finite(n_bins) || n_bins < 4) n_bins <- 20

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = stdResidual, fill = dep, color = dep)) +
                ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                          bins = n_bins, alpha = 0.75) +
                ggplot2::geom_density(linewidth = 0.8, fill = NA) +
                ggplot2::scale_fill_manual(values = cat_cols, guide = "none") +
                ggplot2::scale_color_manual(values = cat_cols, guide = "none") +
                ggplot2::facet_wrap(~dep, nrow = 1) +
                ggplot2::labs(
                    x = private$.plotTr("Standardized residuals", "Residuos estandarizados"),
                    y = private$.plotTr("Density", "Densidad")
                ) +
                private$.plotTheme()

            if (isTRUE(self$options$residShowNormalCurve)) {
                plot <- plot + ggplot2::stat_function(
                    fun = stats::dnorm, linetype = "dashed", linewidth = 0.7,
                    color = pal$edgeSig, inherit.aes = FALSE)
            }

            if (isTRUE(self$options$residFlagOutliers) && any(d$isExtreme)) {
                plot <- plot + ggplot2::geom_rug(
                    data = d[d$isExtreme, , drop = FALSE],
                    ggplot2::aes(x = stdResidual),
                    inherit.aes = FALSE, color = pal$edgeSig, linewidth = 0.8, sides = "b")
            }

            print(plot)
            TRUE
        }
    )
)
