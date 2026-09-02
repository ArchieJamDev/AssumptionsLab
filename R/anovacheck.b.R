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
#    in the user's selected report language, and recommend a specific
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
#    diagnóstica, en el idioma de informe seleccionado por el usuario, y
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

            set_result_titles <- function() {
                set_title_safe <- function(name, en, es) {
                    element <- tryCatch(self$results[[name]], error = function(e) NULL)
                    if (is.null(element)) return(invisible(FALSE))
                    tryCatch(element$setTitle(tr(en, es)), error = function(e) invisible(FALSE))
                    invisible(TRUE)
                }

                titles <- list(
                    c("intro", "ANOVA/ANCOVA", "ANOVA/ANCOVA"),
                    c("designGuide", "Model design", "Diseño del modelo"),
                    c("designSummary", "Design summary", "Resumen del diseño"),
                    c("cellSummary", "Cell size and balance", "Tamaño de celdas y balance"),
                    c("designInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("diagnosticPlotsGuide", "ANOVA/ANCOVA diagnostic plots", "Gráficos diagnósticos de ANOVA/ANCOVA"),

                    c("groupBoxplotsPlot", "Dependent variable by group", "Variable dependiente por grupo"),
                    c("groupBoxplotsGuide", "Guide: Dependent variable by group", "Guía: Variable dependiente por grupo"),

                    c("residualNormalityGuide", "Residual normality", "Normalidad de residuos"),
                    c("residualNormality", "Residual normality", "Normalidad de residuos"),
                    c("residualNormalityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("residualsFittedPlot", "Residuals vs fitted values", "Residuos vs valores ajustados"),
                    c("residualsFittedGuide", "Guide: Residuals vs fitted values", "Guía: Residuos vs valores ajustados"),
                    c("qqResidualsPlot", "Residual Q-Q plot", "Q-Q plot de residuos"),
                    c("qqResidualsGuide", "Guide: Residual Q-Q plot", "Guía: Q-Q plot de residuos"),
                    c("residualHistogramPlot", "Residual histogram", "Histograma de residuos"),
                    c("residualHistogramGuide", "Guide: Residual histogram", "Guía: Histograma de residuos"),
                    c("residualNormalCurvePlot",
                      "Observed residual distribution vs theoretical normal curve",
                      "Distribución observada de residuos vs curva normal teórica"),
                    c("residualNormalCurveGuide",
                      "Guide: Observed distribution vs theoretical normal curve",
                      "Guía: Distribución observada vs normal teórica"),

                    c("varianceGuide", "Homogeneity of variances", "Homogeneidad de varianzas"),
                    c("varianceTests", "Homogeneity of variances across groups", "Homogeneidad de varianzas entre grupos"),
                    c("varianceInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("slopesGuide", "Homogeneity of slopes", "Homogeneidad de pendientes"),
                    c("slopesTests", "Homogeneity of slopes in ANCOVA", "Homogeneidad de pendientes en ANCOVA"),
                    c("slopesInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("covariateLinearityGuide", "Covariate linearity", "Linealidad de covariables"),
                    c("covariateLinearity", "Covariate linearity", "Linealidad de covariables"),
                    c("covariateLinearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("multicollinearityGuide", "Multicollinearity", "Multicolinealidad"),
                    c("multicollinearity", "Multicollinearity", "Multicolinealidad"),
                    c("multicollinearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("correlationMatrixGuide", "Correlation matrix", "Matriz de correlaciones"),
                    c("pearsonMatrixTable", "Pearson correlation matrix (APA 7 format)", "Matriz de correlaciones de Pearson (formato APA 7)"),
                    c("dcorMatrixTable", "Distance correlation matrix (dCor, APA 7 format)", "Matriz de correlación de distancia (dCor, formato APA 7)"),
                    c("correlationComparisonGuide", "Pearson / dCor / Copula Entropy Discordance Analysis", "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                    c("correlationComparisonTable", "Pairs with a notable difference between Pearson and dCor", "Pares con diferencia notable entre Pearson y dCor"),
                    c("correlationComparisonInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("influenceGuide", "Outlying and influential cases", "Casos atípicos e influyentes"),
                    c("influence", "Outlying and influential cases", "Casos atípicos e influyentes"),
                    c("influenceInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("residualsLeveragePlot", "Studentized residuals vs leverage", "Residuos studentizados vs leverage"),
                    c("residualsLeverageGuide", "Guide: Studentized residuals vs leverage", "Guía: Residuos studentizados vs leverage"),
                    c("cooksDPlot", "Cook's D by case", "Cook's D por caso"),
                    c("cooksDGuide", "Guide: Cook's D by case", "Guía: Cook's D por caso"),

                    c("independenceGuide", "Design independence", "Independencia del diseño"),

                    c("alternativesGuide", "Methodological alternatives", "Alternativas metodológicas"),
                    c("alternatives", "Methodological alternatives", "Alternativas metodológicas"),
                    c("alternativesWhen", "", ""),

                    c("notes", "Notes and recommendation", "Notas y recomendación")
                )

                for (row in titles)
                    set_title_safe(row[1], row[2], row[3])
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

                    tryCatch(
                        column$setTitle(tr(en, es)),
                        error = function(e) invisible(FALSE)
                    )

                    invisible(TRUE)
                }

                cols <- list(
                    c("designSummary", "item", "Item", "Elemento"),
                    c("designSummary", "value", "Value", "Valor"),

                    c("cellSummary", "cell", "Group / cell", "Celda / grupo"),
                    c("cellSummary", "n", "n", "n"),
                    c("cellSummary", "mean", "M", "M"),
                    c("cellSummary", "sd", "SD", "SD"),
                    c("cellSummary", "variance", "Variance", "Varianza"),

                    c("residualNormality", "test", "Test", "Prueba"),
                    c("residualNormality", "statistic", "Statistic", "Estadístico"),
                    c("residualNormality", "value", "Value", "Valor"),
                    c("residualNormality", "pSig", "Sig.", "Sig."),

                    c("varianceTests", "family", "Family", "Familia"),
                    c("varianceTests", "test", "Test", "Prueba"),
                    c("varianceTests", "statistic", "Statistic", "Estadístico"),
                    c("varianceTests", "value", "Value", "Valor"),
                    c("varianceTests", "pSig", "Sig.", "Sig."),

                    c("slopesTests", "factor", "Factor", "Factor"),
                    c("slopesTests", "covariate", "Covariate", "Covariable"),
                    c("slopesTests", "interaction", "Interaction", "Interacción"),
                    c("slopesTests", "statistic", "Statistic", "Estadístico"),
                    c("slopesTests", "value", "Value", "Valor"),
                    c("slopesTests", "pSig", "Sig.", "Sig."),

                    c("covariateLinearity", "covariate", "Covariate", "Covariable"),
                    c("covariateLinearity", "dependent", "Dependent variable", "Variable dependiente"),
                    c("covariateLinearity", "test", "Test / criterion", "Prueba / criterio"),
                    c("covariateLinearity", "statistic", "Statistic", "Estadístico"),
                    c("covariateLinearity", "value", "Value", "Valor"),
                    c("covariateLinearity", "pSig", "Sig.", "Sig."),

                    c("multicollinearity", "diagnostic", "Diagnostic", "Diagnóstico"),
                    c("multicollinearity", "item", "Item", "Elemento"),
                    c("multicollinearity", "statistic", "Statistic", "Estadístico"),
                    c("multicollinearity", "value", "Value", "Valor"),

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

                    c("alternatives", "problem", "Detected problem", "Problema detectado"),
                    c("alternatives", "evidence", "Evidence", "Evidencia"),
                    c("alternatives", "suggestion", "Suggested option", "Opción sugerida")
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

            fmt_num <- function(x, digits = 4) {
                x <- clean_num(x)
                if (is.na(x))
                    return("No calculado")
                format(round(x, digits), nsmall = digits)
            }

            qname <- function(x) {
                paste0("`", gsub("`", "", x), "`")
            }

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

                heading_labels <- unique(c(
                    tr("Quick guide", "Guía breve"), "Quick guide", "Guía breve",
                    tr("Applied Interpretation", "Interpretación Aplicada"), "Applied Interpretation", "Interpretación Aplicada"
                ))

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
                    tr("Quick guide", "Guía breve"),
                    paste0(tr("Objective: ", "Objetivo: "), objective),
                    paste0(tr("X axis: ", "Eje X: "), x_axis),
                    paste0(tr("Y axis: ", "Eje Y: "), y_axis),
                    paste0(tr("Expected pattern: ", "Patrón esperado: "), expected),
                    paste0(tr("Warning sign: ", "Señal de alerta: "), warning_txt),
                    paste0(tr("Possible decision: ", "Decisión posible: "), decision)
                )
            }

            format_p <- function(p) {
                p <- clean_num(p)
                if (is.na(p))
                    return("no calculado")
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

            sample_note <- function(n) {
                if (is.na(n))
                    return("El tamaño muestral no pudo determinarse para esta prueba.")
                if (n < 10)
                    return(paste0(
                        "Con n = ", n, ", el tamaño muestral es muy pequeño; interprete ",
                        "este diagnóstico con mucha cautela."
                    ))
                if (n < 30)
                    return(paste0(
                        "Con n = ", n, ", la muestra es pequeña; la prueba puede tener ",
                        "baja potencia para detectar desviaciones relevantes."
                    ))
                if (n <= 200)
                    return(paste0(
                        "Con n = ", n, ", la muestra es moderada; este resultado puede ",
                        "usarse como diagnóstico inicial."
                    ))
                paste0(
                    "Con n = ", n, ", la muestra es grande; desviaciones pequeñas pueden ",
                    "producir resultados estadísticamente significativos."
                )
            }

            interpret_normality <- function(test, p_value, n, scope) {
                p_value <- clean_num(p_value)

                if (is.na(p_value))
                    return(paste0(test, tr(": not computed.", ": no calculado.")))

                if (p_value < .05)
                    return(paste0(
                        test, ": p = ", format_p(p_value), ", n = ", n,
                        tr(". Significant deviation from normality.", ". Desviación significativa de normalidad.")
                    ))

                paste0(
                    test, ": p = ", format_p(p_value), ", n = ", n,
                    tr(". Compatible with approximate normality.", ". Compatible con normalidad aproximada.")
                )
            }

            interpret_variance <- function(test, p_value, n, groups, family) {
                p_value <- clean_num(p_value)

                if (test == "Hartley Fmax")
                    return(tr(
                        "Hartley Fmax: descriptive variance ratio. High values suggest checking heterogeneity.",
                        "Hartley Fmax: razón descriptiva de varianzas. Valores altos sugieren revisar heterogeneidad."
                    ))

                if (is.na(p_value))
                    return(paste0(test, tr(": not computed.", ": no calculado.")))

                if (p_value < .05)
                    return(paste0(
                        test, ": p = ", format_p(p_value), ", n = ", n,
                        tr(". Significant differences between variances.", ". Diferencias significativas entre varianzas.")
                    ))

                paste0(
                    test, ": p = ", format_p(p_value), ", n = ", n,
                    tr(". Compatible with homogeneous variances.", ". Compatible con varianzas homogéneas.")
                )
            }

            interpret_slope <- function(fac, cov, p_value, n) {
                p_value <- clean_num(p_value)

                if (fac == tr("Not applicable", "No aplicable"))
                    return(tr("Not applicable: requires covariates.", "No aplica: requiere covariables."))

                if (is.na(p_value))
                    return(paste0(fac, " × ", cov, tr(": not computed.", ": no calculado.")))

                if (p_value < .05)
                    return(paste0(
                        fac, " × ", cov, ": p = ", format_p(p_value),
                        tr(". Different slopes between groups.", ". Pendientes diferentes entre grupos.")
                    ))

                paste0(
                    fac, " × ", cov, ": p = ", format_p(p_value),
                    tr(". Compatible with homogeneous slopes.", ". Compatible con pendientes homogéneas.")
                )
            }

            interpret_covariate_linearity <- function(cov, test, p_value, n, extra_r = NA_real_, stat_value = NA_real_) {
                p_value <- clean_num(p_value)

                if (cov == tr("Not applicable", "No aplicable"))
                    return(tr("Not applicable: no covariates were selected.", "No aplica: no se seleccionaron covariables."))

                if (test == tr("Distance correlation (dCor)", "Correlación de distancia (dCor)")) {
                    dcor_val <- clean_num(stat_value)
                    pearson_r <- clean_num(extra_r)

                    if (is.na(dcor_val))
                        return(paste0(test, tr(" on ", " en "), cov, tr(": not computed.", ": no calculado.")))

                    gap <- if (is.na(pearson_r)) NA_real_ else dcor_val - abs(pearson_r)

                    if (!is.na(gap) && gap > 0.10 && !is.na(p_value) && p_value < .05) {
                        return(paste0(
                            test, tr(" on ", " en "), cov,
                            ": dCor = ", format(round(dcor_val, 3), nsmall = 3),
                            tr(", Pearson r = ", ", r de Pearson = "),
                            format(round(pearson_r, 3), nsmall = 3),
                            tr(
                                ". dCor detects dependence that Pearson's r does not fully capture; inspect a scatterplot and consider a non-linear term or transformation for this covariate.",
                                ". dCor detecta una dependencia que r de Pearson no captura por completo; revise un diagrama de dispersión y considere un término no lineal o una transformación para esta covariable."
                            )
                        ))
                    }

                    return(paste0(
                        test, tr(" on ", " en "), cov,
                        ": dCor = ", format(round(dcor_val, 3), nsmall = 3),
                        tr(
                            ". Consistent with the linear association already shown by Pearson's r; no additional non-linear structure is evident.",
                            ". Consistente con la asociación lineal ya mostrada por r de Pearson; no se evidencia estructura no lineal adicional."
                        )
                    ))
                }

                if (is.na(p_value))
                    return(paste0(test, tr(" on ", " en "), cov, tr(": not computed.", ": no calculado.")))

                if (test == tr("Bivariate correlation", "Correlación bivariada"))
                    return(paste0(
                        test, tr(" on ", " en "), cov, ": p = ", format_p(p_value),
                        tr(". Simple linear association.", ". Asociación lineal simple.")
                    ))

                if (p_value < .05)
                    return(paste0(
                        test, tr(" on ", " en "), cov, ": p = ", format_p(p_value),
                        tr(". Possible deviation from linearity.", ". Posible desviación de linealidad.")
                    ))

                paste0(
                    test, tr(" on ", " en "), cov, ": p = ", format_p(p_value),
                    tr(". Compatible with approximate linearity.", ". Compatible con linealidad aproximada.")
                )
            }

            add_table_row <- function(table, key, values) {
                table$addRow(rowKey = key, values = values)
            }

            dep <- self$options$dep
            factors <- self$options$factors
            covs <- self$options$covs

            if (is.null(dep) || dep == "") {
                self$results$intro$setContent(
                    tr("Select a numeric dependent variable.", "Seleccione una variable dependiente numérica.")
                )
                return()
            }

            if (length(factors) == 0) {
                self$results$intro$setContent(
                    tr("Select at least one categorical factor for ANOVA/ANCOVA.", "Seleccione al menos un factor categórico para ANOVA/ANCOVA.")
                )
                return()
            }

            vars <- c(dep, factors, covs)
            dat <- self$data[, vars, drop = FALSE]
            complete <- stats::complete.cases(dat)
            dat2 <- dat[complete, , drop = FALSE]

            n_total <- nrow(dat)
            n_used <- nrow(dat2)
            n_excluded <- n_total - n_used

            if (n_used < 5) {
                self$results$intro$setContent(
                    tr("There are not enough complete cases to fit the model.", "No hay suficientes casos completos para ajustar el modelo.")
                )
                return()
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

            fit <- tryCatch(stats::lm(model_formula, data = dat2), error = function(e) NULL)

            if (is.null(fit)) {
                self$results$intro$setContent(
                    tr("It was not possible to fit the ANOVA/ANCOVA model.", "No fue posible ajustar el modelo ANOVA/ANCOVA.")
                )
                return()
            }

            residuals_raw <- tryCatch(stats::residuals(fit), error = function(e) NA_real_)
            fitted_values <- tryCatch(stats::fitted(fit), error = function(e) NA_real_)
            residuals_stud <- tryCatch(stats::rstudent(fit), error = function(e) NA_real_)
            leverage <- tryCatch(stats::hatvalues(fit), error = function(e) NA_real_)
            cooks_d <- tryCatch(stats::cooks.distance(fit), error = function(e) NA_real_)
            dffits <- tryCatch(stats::dffits(fit), error = function(e) NA_real_)

            p_model <- length(stats::coef(fit))

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

            self$results$residualsFittedGuide$setContent(plot_guide(
                tr("check for residual patterns not captured by group or cell means.",
                   "revisar patrones en los residuos no capturados por las medias de grupo o celda."),
                tr("fitted values (group/cell means, adjusted for covariates) for each case.",
                   "valores ajustados (medias de grupo/celda, ajustadas por covariables) para cada caso."),
                tr("residuals, the difference between the observed and fitted value.",
                   "residuos, la diferencia entre el valor observado y el ajustado."),
                tr("a random cloud around zero, without curvature or funnel shape.",
                   "una nube aleatoria alrededor de cero, sin curvatura ni forma de embudo."),
                tr("curvature, bands, funnel shape, or separated groups.",
                   "curvatura, bandas, forma de embudo o grupos separados."),
                tr("review whether a covariate, interaction, or non-linear term is missing.",
                   "revisar si falta una covariable, interacción o término no lineal.")
            ))

            self$results$qqResidualsGuide$setContent(plot_guide(
                tr("evaluate whether standardized residuals approximate a normal distribution.",
                   "evaluar si los residuos estandarizados se aproximan a una distribución normal."),
                tr("theoretical quantiles expected under normality.",
                   "cuantiles teóricos esperados bajo normalidad."),
                tr("observed quantiles of the standardized residuals.",
                   "cuantiles observados de los residuos estandarizados."),
                tr("points close to the diagonal reference line.",
                   "puntos cercanos a la línea diagonal de referencia."),
                tr("strong deviations at the extremes, S-shape, or heavy tails.",
                   "desviaciones fuertes en los extremos, forma de S o colas pesadas."),
                tr("interpret classical inference with caution and review outliers or transformations.",
                   "interpretar inferencias clásicas con cautela y revisar atípicos o transformaciones.")
            ))

            self$results$residualHistogramGuide$setContent(plot_guide(
                tr("examine the shape, symmetry, and concentration of standardized residuals.",
                   "examinar forma, simetría y concentración de los residuos estandarizados."),
                tr("standardized residuals.", "residuos estandarizados."),
                tr("count of cases.", "frecuencia o número de casos."),
                tr("an approximately symmetric distribution centered near zero.",
                   "una distribución aproximadamente simétrica y centrada en cero."),
                tr("marked skew, multiple peaks, or very long tails.",
                   "asimetría marcada, múltiples picos o colas muy largas."),
                tr("complement normality tests and review extreme cases.",
                   "complementar las pruebas de normalidad y revisar casos extremos.")
            ))

            self$results$residualNormalCurveGuide$setContent(plot_guide(
                tr("compare the observed residual distribution to a theoretical normal curve.",
                   "comparar la distribución observada de los residuos con una curva normal teórica."),
                tr("standardized residuals.", "residuos estandarizados."),
                tr("density, i.e. the relative concentration of cases.",
                   "densidad, es decir, la concentración relativa de casos."),
                tr("an empirical curve centered near zero and similar to the normal bell shape.",
                   "una curva empírica centrada cerca de cero y similar a la campana normal."),
                tr("skew, heavy tails, several peaks, or an excess of extreme cases.",
                   "asimetría, colas pesadas, varios picos o exceso de casos extremos."),
                tr("read classical tests with caution and review outliers, transformation, or specification.",
                   "leer las pruebas clásicas con cautela y revisar atípicos, transformación o especificación.")
            ))

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
                    tr("It was not possible to build the design cells.", "No fue posible construir las celdas del diseño.")
                )
                return()
            }

            cell_n <- table(cell_factor)
            min_cell <- min(cell_n)
            max_cell <- max(cell_n)
            balance_ratio <- max_cell / max(1, min_cell)

            if (min_cell < 5 || balance_ratio > 2)
                design_problem <- TRUE

            self$results$intro$setContent(paste0(
                "<div style=\"max-width: 7.25in; width: 100%; box-sizing: border-box;\">",
                "<p style=\"font-weight: 700; margin: 0 0 0.10em 0; line-height: 1.25;\">",
                "AssumptionsLab",
                "</p>",
                "<p style=\"margin: 0 0 0.35em 0; line-height: 1.25;\">",
                tr("Assumption check for ANOVA/ANCOVA", "Revisión de supuestos para ANOVA/ANCOVA"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr(
                    "Use this analysis when you want to review whether an ANOVA/ANCOVA has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.",
                    "Use este análisis cuando quiera revisar si un ANOVA/ANCOVA tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."
                ),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr("<b>Dependent variable:</b> ", "<b>Variable dependiente:</b> "), html_escape(dep),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr("<b>Factors:</b> ", "<b>Factores:</b> "), html_escape(paste(factors, collapse = ", ")),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr("<b>Covariates:</b> ", "<b>Covariables:</b> "),
                html_escape(ifelse(length(covs) == 0, tr("None", "Ninguna"), paste(covs, collapse = ", "))),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0; line-height: 1.35;\">",
                tr("<b>Analysis type:</b> ", "<b>Tipo de análisis:</b> "),
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

            add_ds(tr("Model", "Modelo"), gsub("`", "", formula_text))
            add_ds(tr("Total cases", "Casos totales"), as.character(n_total))
            add_ds(tr("Complete cases used", "Casos completos usados"), as.character(n_used))
            add_ds(tr("Cases excluded for missing data", "Casos excluidos por datos faltantes"), as.character(n_excluded))
            add_ds(tr("Number of factors", "Número de factores"), as.character(length(factors)))
            add_ds(tr("Number of covariates", "Número de covariables"), as.character(length(covs)))
            add_ds(tr("Number of observed cells", "Número de celdas observadas"), as.character(length(cell_n)))
            add_ds(tr("Minimum cell size", "Tamaño mínimo de celda"), as.character(min_cell))
            add_ds(tr("Maximum cell size", "Tamaño máximo de celda"), as.character(max_cell))
            add_ds(tr("Max/min ratio", "Razón max/min"), fmt_num(balance_ratio))

            cs_i <- 1
            y <- dat2[[dep]]

            private$.groupPlotData <- data.frame(
                cell = as.character(cell_factor),
                value = as.numeric(y),
                stringsAsFactors = FALSE
            )

            self$results$groupBoxplotsGuide$setContent(plot_guide(
                tr("compare central tendency, spread, and univariate outliers across groups or cells.",
                   "comparar tendencia central, dispersión y valores atípicos univariados entre grupos o celdas."),
                tr("groups / cells defined by the selected factor(s).",
                   "grupos / celdas definidos por el o los factores seleccionados."),
                tr("observed values of the dependent variable.",
                   "valores observados de la variable dependiente."),
                tr("boxes of similar size and reasonable whiskers across groups.",
                   "cajas de tamaño similar y bigotes razonables entre grupos."),
                tr("very different spreads, many outliers, or strong asymmetry in one group.",
                   "dispersiones muy distintas, muchos atípicos o asimetría fuerte en un grupo."),
                tr("review those groups before trusting homogeneity of variance results.",
                   "revisar esos grupos antes de confiar en los resultados de homogeneidad de varianza.")
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
                tr("Quick guide", "Guía breve"),
                tr(
                    "ANOVA compares means across groups; ANCOVA adds numeric covariates.",
                    "ANOVA compara medias entre grupos; ANCOVA agrega covariables numéricas."
                ),
                tr(
                    "Check cell sizes, balance, missing data, and design coherence.",
                    "Revise tamaño de celdas, balance, datos faltantes y coherencia del diseño."
                ),
                tr(
                    "The full explanation of these criteria is in Assumption Library.",
                    "La explicación completa de estos criterios está en Assumption Library."
                )
            ))

            small_cells <- names(cell_n)[cell_n < 5]

            design_finding <- if (!design_problem) {
                tr(
                    sprintf(
                        "Cell sizes range from %d to %d (max/min ratio %.2f), which is compatible with a reasonably balanced design.",
                        min_cell, max_cell, balance_ratio
                    ),
                    sprintf(
                        "Los tamaños de celda van de %d a %d (razón max/min %.2f), compatible con un diseño razonablemente balanceado.",
                        min_cell, max_cell, balance_ratio
                    )
                )
            } else if (length(small_cells) > 0) {
                tr(
                    sprintf(
                        "%d of %d cell(s) have fewer than 5 cases (%s); the max/min ratio is %.2f.",
                        length(small_cells), length(cell_n), paste(small_cells, collapse = ", "), balance_ratio
                    ),
                    sprintf(
                        "%d de %d celda(s) tienen menos de 5 casos (%s); la razón max/min es %.2f.",
                        length(small_cells), length(cell_n), paste(small_cells, collapse = ", "), balance_ratio
                    )
                )
            } else {
                tr(
                    sprintf(
                        "No cell has fewer than 5 cases, but the max/min ratio is %.2f, above the balance threshold.",
                        balance_ratio
                    ),
                    sprintf(
                        "Ninguna celda tiene menos de 5 casos, pero la razón max/min es %.2f, por encima del umbral de balance.",
                        balance_ratio
                    )
                )
            }

            self$results$designInterpretation$setContent(
                block96(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    design_finding,
                    tr(
                        "Why it matters: unequal cell sizes interact with variance heterogeneity. When the cell with the smallest n also has the largest variance, the classical F-test becomes anti-conservative (actual Type I error above the nominal level); when the smallest cell has the smallest variance, the test becomes conservative instead.",
                        "Por qué importa: los tamaños de celda desiguales interactúan con la heterogeneidad de varianzas. Cuando la celda con menor n también tiene la mayor varianza, la prueba F clásica se vuelve anticonservadora (el error Tipo I real queda por encima del nominal); cuando la celda más pequeña tiene la menor varianza, la prueba se vuelve conservadora en cambio."
                    ),
                    tr(
                        "Common error: treating any degree of design imbalance as automatically invalidating the ANOVA. With homogeneous variances, moderate imbalance has limited practical impact; the real risk appears specifically when imbalance and variance heterogeneity occur together, so this result should be read alongside the \"Homogeneity of Variances\" section (Maxwell & Delaney, 2004).",
                        "Error común: tratar cualquier grado de desbalance del diseño como algo que invalida automáticamente el ANOVA. Con varianzas homogéneas, un desbalance moderado tiene impacto práctico limitado; el riesgo real aparece específicamente cuando el desbalance y la heterogeneidad de varianzas ocurren juntos, por lo que este resultado debe leerse junto con la sección \"Homogeneidad de varianzas\" (Maxwell & Delaney, 2004)."
                    )
                )
            )


            # -----------------------------------------------------------------------------
            # Residual normality.
            # ES: Normalidad de residuos.
            # -----------------------------------------------------------------------------

            rn_i <- 1
            add_norm <- function(test, statistic, value, p_value) {
                if (!is.na(clean_num(p_value))) {
                    norm_total_count <<- norm_total_count + 1L
                    if (clean_num(p_value) < .05) {
                        normality_problem <<- TRUE
                        norm_sig_count <<- norm_sig_count + 1L
                    }
                }

                add_table_row(
                    self$results$residualNormality,
                    paste0("norm_", rn_i),
                    list(
                        test = test,
                        statistic = statistic,
                        value = clean_num(value),
                        p = clean_num(p_value),
                        pSig = p_sig(p_value)
                    )
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
                add_norm("Shapiro-Wilk", "W", NA_real_, NA_real_)

            # Lilliefors / Anderson-Darling / Cramer-von Mises / Shapiro-Francia /
            # Pearson chi-square: identical tryCatch calls in every module,
            # consolidated in shared-helpers.R (.al_nortest_battery).
            # ES: idénticas en todos los módulos, consolidadas en shared-helpers.R.
            .nt_res <- .al_nortest_battery(res)
            li <- .nt_res$li; ad <- .nt_res$ad; cvm <- .nt_res$cvm; sf <- .nt_res$sf

            if (!is.null(li))
                add_norm(tr("Lilliefors (corrected K-S)", "Lilliefors (K-S corregido)"), "D", li$statistic[[1]], li$p.value)
            else
                add_norm(tr("Lilliefors (corrected K-S)", "Lilliefors (K-S corregido)"), "D", NA_real_, NA_real_)

            if (!is.null(ad))
                add_norm("Anderson-Darling", "A²", ad$statistic[[1]], ad$p.value)
            else
                add_norm("Anderson-Darling", "A²", NA_real_, NA_real_)

            if (!is.null(cvm))
                add_norm("Cramer-von Mises", "W²", cvm$statistic[[1]], cvm$p.value)
            else
                add_norm("Cramer-von Mises", "W²", NA_real_, NA_real_)

            if (!is.null(sf))
                add_norm("Shapiro-Francia", "W'", sf$statistic[[1]], sf$p.value)
            else
                add_norm("Shapiro-Francia", "W'", NA_real_, NA_real_)

            pt <- .nt_res$pt

            if (!is.null(pt))
                add_norm(tr("Pearson chi-square", "Pearson chi-cuadrado"), "P", pt$statistic[[1]], pt$p.value)
            else
                add_norm(tr("Pearson chi-square", "Pearson chi-cuadrado"), "P", NA_real_, NA_real_)

            jb <- .nc_res$jb

            if (!is.null(jb))
                add_norm("Jarque-Bera", "JB", jb$value, jb$p)
            else
                add_norm("Jarque-Bera", "JB", NA_real_, NA_real_)

            skew_test <- .nc_res$skew

            if (!is.null(skew_test))
                add_norm(tr("Skewness test", "Prueba de asimetría"), "z", skew_test$value, skew_test$p)
            else
                add_norm(tr("Skewness test", "Prueba de asimetría"), "z", NA_real_, NA_real_)

            kurt_test <- .nc_res$kurt

            if (!is.null(kurt_test))
                add_norm(tr("Kurtosis test", "Prueba de curtosis"), "z", kurt_test$value, kurt_test$p)
            else
                add_norm(tr("Kurtosis test", "Prueba de curtosis"), "z", NA_real_, NA_real_)

            self$results$residualNormalityGuide$setContent(html_list_guide(
                tr("Quick guide", "Guía breve"),
                tr(
                    "Normality is evaluated on the model's residuals, not on the raw variable.",
                    "La normalidad se evalúa sobre residuos del modelo, no sobre la variable bruta."
                ),
                tr(
                    "p >= .05 is compatible with approximate normality.",
                    "p >= .05 es compatible con normalidad aproximada."
                ),
                tr(
                    "p < .05 suggests a significant deviation from normality.",
                    "p < .05 sugiere desviación significativa de normalidad."
                ),
                tr(
                    "See methodological details in Assumption Library.",
                    "Revise detalles metodológicos en Assumption Library."
                )
            ))


            normality_integration <- if (norm_total_count == 0) {
                ""
            } else if (norm_sig_count == 0) {
                tr(
                    sprintf("None of the %d normality tests computed reached significance: consistent evidence of approximate normality.", norm_total_count),
                    sprintf("Ninguna de las %d pruebas de normalidad calculadas resultó significativa: evidencia consistente de normalidad aproximada.", norm_total_count)
                )
            } else if (norm_sig_count == norm_total_count) {
                tr(
                    sprintf("All %d normality tests computed reached significance: consistent evidence of a real deviation from normality.", norm_total_count),
                    sprintf("Las %d pruebas de normalidad calculadas resultaron significativas: evidencia consistente de una desviación real de normalidad.", norm_total_count)
                )
            } else if (norm_sig_count == 1) {
                tr(
                    sprintf("Only 1 of %d normality tests reached significance. An isolated signal like this, uncorroborated by the rest of the battery, deserves more caution than a result confirmed across multiple tests.", norm_total_count),
                    sprintf("Solo 1 de %d pruebas de normalidad resultó significativa. Una señal aislada como esta, no corroborada por el resto de la batería, merece más cautela que un resultado confirmado por varias pruebas.", norm_total_count)
                )
            } else {
                tr(
                    sprintf("%d of %d normality tests reached significance. This level of agreement across multiple tests strengthens the case for a real deviation from normality, beyond what a single isolated test would indicate.", norm_sig_count, norm_total_count),
                    sprintf("%d de %d pruebas de normalidad resultaron significativas. Este nivel de acuerdo entre varias pruebas refuerza la sospecha de una desviación real de normalidad, más allá de lo que indicaría una sola prueba aislada.", norm_sig_count, norm_total_count)
                )
            }

            self$results$residualNormalityInterpretation$setContent(
                block96(paste(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    paste(normality_texts, collapse = "\n"),
                    normality_integration,
                    tr(
                        "Shapiro-Wilk is used because it retains the highest statistical power among common normality tests across nearly the full range of sample sizes (Razali & Wah, 2011). A widespread recommendation to prefer Kolmogorov-Smirnov once n grows is a leftover from old software limitations, not a statistical reason.",
                        "Se usa Shapiro-Wilk porque mantiene el mayor poder estadístico entre las pruebas de normalidad más comunes para prácticamente todo rango de tamaño muestral (Razali & Wah, 2011). La recomendación extendida de preferir Kolmogorov-Smirnov cuando n crece es un remanente de limitaciones de software antiguo, no una razón estadística."
                    ),
                    tr(
                        "Why it matters: residual normality mainly affects the precision of classical inference (confidence intervals, p-values) in small samples; point estimates of group means remain valid even with moderate non-normality.",
                        "Por qué importa: la normalidad de residuos afecta principalmente la precisión de la inferencia clásica (intervalos de confianza, valores p) en muestras pequeñas; las estimaciones puntuales de las medias de grupo siguen siendo válidas incluso con no normalidad moderada."
                    ),
                    tr(
                        "Common error: assuming a significant normality test invalidates the whole ANOVA. It is a signal to inspect the shape of residuals (skewness, heavy tails, outliers) before deciding whether classical inference is still trustworthy.",
                        "Error común: asumir que una prueba de normalidad significativa invalida todo el ANOVA. Es una señal para revisar la forma de los residuos (asimetría, colas pesadas, atípicos) antes de decidir si la inferencia clásica sigue siendo confiable."
                    ),
                    sep = "\n\n"
                ))
            )

            # -----------------------------------------------------------------------------
            # Homogeneity of variances.
            # ES: Homogeneidad de varianzas.
            # -----------------------------------------------------------------------------

            vt_i <- 1
            add_var <- function(family, test, statistic, value, df, p_value) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05)
                    variance_problem <<- TRUE

                add_table_row(
                    self$results$varianceTests,
                    paste0("var_", vt_i),
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
            lev <- tryCatch(.al_levene_manual(y, cell_factor, "mean"), error = function(e) NULL)

            if (!is.null(lev))
                add_var(tr("Equality of variances", "Igualdad de varianzas"), "Levene", "F", lev$value, lev$df, lev$p)
            else
                add_var(tr("Equality of variances", "Igualdad de varianzas"), "Levene", "F", NA_real_, NA_integer_, NA_real_)

            bf <- tryCatch(.al_levene_manual(y, cell_factor, "median"), error = function(e) NULL)

            if (!is.null(bf))
                add_var(tr("Equality of variances", "Igualdad de varianzas"), "Brown-Forsythe", "F", bf$value, bf$df, bf$p)
            else
                add_var(tr("Equality of variances", "Igualdad de varianzas"), "Brown-Forsythe", "F", NA_real_, NA_integer_, NA_real_)

            bart <- tryCatch(stats::bartlett.test(y ~ cell_factor), error = function(e) NULL)

            if (!is.null(bart))
                add_var(tr("Equality of variances", "Igualdad de varianzas"), "Bartlett", "K²",
                        bart$statistic[[1]], bart$parameter[[1]], bart$p.value)
            else
                add_var(tr("Equality of variances", "Igualdad de varianzas"), "Bartlett", "K²",
                        NA_real_, NA_integer_, NA_real_)

            flig <- tryCatch(stats::fligner.test(y ~ cell_factor), error = function(e) NULL)

            if (!is.null(flig))
                add_var(tr("Equality of variances", "Igualdad de varianzas"), "Fligner-Killeen", "χ²",
                        flig$statistic[[1]], flig$parameter[[1]], flig$p.value)
            else
                add_var(tr("Equality of variances", "Igualdad de varianzas"), "Fligner-Killeen", "χ²",
                        NA_real_, NA_integer_, NA_real_)

            hartley <- tryCatch({
                vars <- tapply(y, cell_factor, stats::var, na.rm = TRUE)
                vars <- vars[!is.na(vars) & vars > 0]
                max(vars) / min(vars)
            }, error = function(e) NA_real_)

            add_var(tr("Variance ratio", "Razón de varianzas"), "Hartley Fmax", "Fmax",
                    hartley, NA_integer_, NA_real_)

            self$results$varianceGuide$setContent(html_list_guide(
                tr("Quick guide", "Guía breve"),
                tr(
                    "These tests check whether variances are similar across groups or cells.",
                    "Estas pruebas revisan si las varianzas son similares entre grupos o celdas."
                ),
                tr(
                    "p >= .05 is compatible with approximately homogeneous variances.",
                    "p >= .05 es compatible con varianzas aproximadamente homogéneas."
                ),
                tr(
                    "p < .05 suggests significant differences between variances.",
                    "p < .05 sugiere diferencias significativas entre varianzas."
                ),
                tr(
                    "See methodological details in Assumption Library.",
                    "Revise detalles metodológicos en Assumption Library."
                )
            ))


            self$results$varianceInterpretation$setContent(
                block96(paste(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    paste(variance_texts, collapse = "\n"),
                    tr(
                        "Levene's test is treated as the primary reference because it is the classical, most widely recognized test for equality of variances across groups (Levene, 1960); Brown-Forsythe and Bartlett are reported alongside for comparison.",
                        "La prueba de Levene se trata como referencia principal por ser la más clásica y ampliamente reconocida para igualdad de varianzas entre grupos (Levene, 1960); Brown-Forsythe y Bartlett se reportan junto a esta para comparar."
                    ),
                    tr(
                        "Why it matters: unequal variances mainly threaten the classical F-test when group sizes are also unequal; with equal group sizes, ANOVA is fairly robust to moderate variance differences.",
                        "Por qué importa: las varianzas desiguales amenazan principalmente a la prueba F clásica cuando además los tamaños de grupo son desiguales; con tamaños de grupo iguales, el ANOVA es bastante robusto a diferencias moderadas de varianza."
                    ),
                    tr(
                        "Common error: treating a significant variance test as reason to abandon ANOVA entirely. The direct remedy is usually switching to Welch's ANOVA, which does not assume equal variances, rather than discarding the analysis.",
                        "Error común: tratar una prueba de varianza significativa como motivo para abandonar por completo el ANOVA. El remedio directo suele ser cambiar al ANOVA de Welch, que no asume varianzas iguales, en vez de descartar el análisis."
                    ),
                    sep = "\n\n"
                ))
            )

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
                add_slope(tr("Not applicable", "No aplicable"), tr("No covariates", "Sin covariables"), "", "", NA_real_, NA_real_)
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
                tr("Quick guide", "Guía breve"),
                tr(
                    "This assumption applies to ANCOVA when covariates are present.",
                    "Este supuesto aplica a ANCOVA cuando hay covariables."
                ),
                tr(
                    "p >= .05 is compatible with approximately homogeneous slopes.",
                    "p >= .05 es compatible con pendientes aproximadamente homogéneas."
                ),
                tr(
                    "p < .05 suggests the slope changes across groups.",
                    "p < .05 sugiere que la pendiente cambia entre grupos."
                ),
                tr(
                    "See methodological details in Assumption Library.",
                    "Revise detalles metodológicos en Assumption Library."
                )
            ))


            self$results$slopesInterpretation$setContent(
                block96(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    paste(slope_texts, collapse = "\n"),
                    if (length(covs) > 0) tr(
                        "Why it matters: if slopes are not homogeneous, a single averaged covariate adjustment misrepresents the group differences, because the covariate-response relationship itself differs by group (Huitema, 2011).",
                        "Por qué importa: si las pendientes no son homogéneas, un ajuste promedio por la covariable representa mal las diferencias entre grupos, porque la relación covariable-respuesta cambia según el grupo (Huitema, 2011)."
                    ) else "",
                    if (length(covs) > 0) tr(
                        "Common error: running standard ANCOVA anyway and reporting adjusted means as if the covariate effect were the same for every group, when the interaction test says otherwise.",
                        "Error común: correr ANCOVA estándar de todos modos y reportar medias ajustadas como si el efecto de la covariable fuera igual en todos los grupos, cuando la prueba de interacción indica lo contrario."
                    ) else "",
                    if (length(covs) > 0) tr(
                        "Sample-size nuance: this interaction test has limited power in small or unbalanced cells, so a non-significant result should not be read as strong proof of homogeneous slopes; with very large samples, even a trivial slope difference can become significant.",
                        "Matiz de tamaño de muestra: esta prueba de interacción tiene poder limitado en celdas pequeñas o desbalanceadas, así que un resultado no significativo no debe leerse como prueba fuerte de pendientes homogéneas; con muestras muy grandes, incluso una diferencia trivial en las pendientes puede volverse significativa."
                    ) else ""
                )
            )

            # -----------------------------------------------------------------------------
            # Covariate linearity.
            # ES: Linealidad de covariables.
            # -----------------------------------------------------------------------------

            cl_i <- 1
            add_covlin <- function(cv, test, statistic, value, p_value, extra_r = NA_real_) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05 &&
                    test %in% c(tr("Exploratory quadratic term", "Término cuadrático exploratorio"),
                                tr("Exploratory Box-Tidwell", "Box-Tidwell exploratorio"))) {
                    linearity_problem <<- TRUE
                }

                add_table_row(
                    self$results$covariateLinearity,
                    paste0("covlin_", cl_i),
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
                add_covlin(tr("Not applicable", "No aplicable"), tr("No covariates", "Sin covariables"), "", NA_real_, NA_real_)
            } else {
                for (cv in covs) {
                    x <- dat2[[cv]]

                    cor_val <- tryCatch(stats::cor(x, y, use = "complete.obs"),
                                        error = function(e) NA_real_)
                    cor_p <- tryCatch(stats::cor.test(x, y)$p.value,
                                      error = function(e) NA_real_)

                    add_covlin(cv, tr("Bivariate correlation", "Correlación bivariada"), "r", cor_val, cor_p)

                    quad_p <- tryCatch({
                        quadDat <- dat2
                        quadDat[[".x_sq_tmp"]] <- x^2
                        f_quad <- stats::as.formula(paste(
                            qname(dep), "~", rhs, "+ .x_sq_tmp"
                        ))
                        fit_quad <- stats::lm(f_quad, data = quadDat)
                        coef(summary(fit_quad))[".x_sq_tmp", "Pr(>|t|)"]
                    }, error = function(e) NA_real_)

                    add_covlin(cv, tr("Exploratory quadratic term", "Término cuadrático exploratorio"), "p", quad_p, quad_p)

                    bt_p <- tryCatch({
                        if (any(x <= 0, na.rm = TRUE))
                            stop("Box-Tidwell requiere valores positivos.")

                        boxTidwellDat <- dat2
                        boxTidwellDat[[".bt_tmp"]] <- x * log(x)
                        f_bt <- stats::as.formula(paste(
                            qname(dep), "~", rhs, "+ .bt_tmp"
                        ))
                        fit_bt <- stats::lm(f_bt, data = boxTidwellDat)
                        coef(summary(fit_bt))[".bt_tmp", "Pr(>|t|)"]
                    }, error = function(e) NA_real_)

                    add_covlin(cv, tr("Exploratory Box-Tidwell", "Box-Tidwell exploratorio"), "p", bt_p, bt_p)

                    dcor_val <- tryCatch(dcor_stat(x, y), error = function(e) NA_real_)
                    dcor_p <- tryCatch(dcor_pvalue(x, y), error = function(e) NA_real_)

                    if (!is.na(dcor_val) && !is.na(cor_val) && !is.na(dcor_p) &&
                        (dcor_val - abs(cor_val)) > 0.10 && dcor_p < .05) {
                        dcor_problem <<- TRUE
                    }

                    add_covlin(cv, tr("Distance correlation (dCor)", "Correlación de distancia (dCor)"), "dCor", dcor_val, dcor_p, extra_r = cor_val)

                    ce_res <- tryCatch(copentTest(x, y), error = function(e) NULL)

                    if (!is.null(ce_res)) {
                        add_covlin(cv, tr("Copula entropy (copent)", "Entropía copular (copent)"), "CE", ce_res$ce, ce_res$p)
                    } else if (!requireNamespace("copent", quietly = TRUE)) {
                        add_covlin(cv, tr("Copula entropy (copent)", "Entropía copular (copent)"), "CE", NA_real_, NA_real_)
                    }
                }
            }

            self$results$covariateLinearityGuide$setContent(html_list_guide(
                tr("Quick guide", "Guía breve"),
                tr(
                    "Linearity applies to numeric covariates, not to categorical factors.",
                    "La linealidad aplica a covariables numéricas, no a factores categóricos."
                ),
                tr(
                    "p >= .05 is compatible with approximate linearity.",
                    "p >= .05 es compatible con linealidad aproximada."
                ),
                tr(
                    "p < .05 suggests possible curvature or misspecification.",
                    "p < .05 sugiere posible curvatura o mala especificación."
                ),
                tr(
                    "See methodological details in Assumption Library.",
                    "Revise detalles metodológicos en Assumption Library."
                )
            ))


            self$results$covariateLinearityInterpretation$setContent(
                block96(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    paste(covlin_texts, collapse = "\n"),
                    if (length(covs) > 0) tr(
                        "Distance correlation (dCor): unlike Pearson's r, which only detects linear association, dCor can detect any form of statistical dependence, including non-monotonic or curved relationships, and equals zero only when the variables are independent.",
                        "Correlación de distancia (dCor): a diferencia de r de Pearson, que solo detecta asociación lineal, dCor puede detectar cualquier forma de dependencia estadística, incluyendo relaciones no monótonas o curvas, y es igual a cero solo cuando las variables son independientes."
                    ) else "",
                    if (length(covs) > 0) tr(
                        "Common error: treating a significant dCor by itself as proof that the relationship is non-linear. dCor is also significant for purely linear relationships; the real diagnostic signal here is the size of the gap between dCor and |Pearson's r|, not the significance of dCor alone.",
                        "Error común: tratar un dCor significativo por sí solo como prueba de que la relación es no lineal. dCor también es significativo para relaciones puramente lineales; la señal diagnóstica real aquí es el tamaño de la brecha entre dCor y |r de Pearson|, no la significancia de dCor por sí sola."
                    ) else "",
                    if (length(covs) > 0) tr(
                        "Sample-size nuance: the permutation test used for dCor has limited resolution in small samples, so a moderate gap should be treated as exploratory rather than conclusive.",
                        "Matiz de tamaño de muestra: la prueba de permutación usada para dCor tiene resolución limitada en muestras pequeñas, así que una brecha moderada debe tratarse como exploratoria y no como concluyente."
                    ) else ""
                )
            )

            # -----------------------------------------------------------------------------
            # Multicollinearity.
            # ES: Multicolinealidad.
            # -----------------------------------------------------------------------------

            multi_i <- 1
            max_vif <- NA_real_
            max_vif_name <- NA_character_

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

            if (length(covs) >= 2) {
                cor_mat <- tryCatch(
                    stats::cor(dat2[, covs, drop = FALSE], use = "pairwise.complete.obs"),
                    error = function(e) NULL
                )

                if (!is.null(cor_mat)) {
                    cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA_real_
                    max_cor <- suppressWarnings(max(abs(cor_mat), na.rm = TRUE))
                    add_multi(tr("Maximum correlation", "Correlación máxima"), tr("Numeric covariates", "Covariables numéricas"), tr("maximum |r|", "|r| máximo"), max_cor)
                }
            }

            self$results$multicollinearityGuide$setContent(html_list_guide(
                tr("Quick guide", "Guía breve"),
                tr(
                    "This check applies when covariates are present (ANCOVA); VIF among a single factor's own dummy-coded levels is not an actionable diagnostic and is not shown for a covariate-free ANOVA.",
                    "Esta comprobación aplica cuando hay covariables (ANCOVA); el VIF entre las columnas dummy de un solo factor no es un diagnóstico accionable y no se muestra en un ANOVA sin covariables."
                ),
                tr(
                    "VIF and tolerance describe how much each model term overlaps with the others; the eigenvalue/condition index diagnose collinearity across the whole design matrix at once.",
                    "VIF y tolerancia describen cuánto se solapa cada término del modelo con los demás; el eigenvalue/índice de condición diagnostican la colinealidad en toda la matriz de diseño a la vez."
                ),
                tr(
                    "VIF >= 5 is a moderate concern; VIF >= 10 is a severe problem.",
                    "VIF >= 5 es una alerta moderada; VIF >= 10 es un problema severo."
                ),
                tr(
                    "See methodological details in Assumption Library.",
                    "Revise detalles metodológicos en Assumption Library."
                )
            ))

            self$results$multicollinearityInterpretation$setContent(block96(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (length(covs) > 0) paste0(
                    tr("Maximum VIF: ", "VIF máximo: "), format(round(clean_num(max_vif), 2), nsmall = 2),
                    if (!is.na(max_vif_name)) paste0(tr(" (term: ", " (término: "), max_vif_name, ")") else "",
                    tr(
                        ". By convention, values between 5 and 10 raise a moderate concern, and values above 10 are considered a severe problem (Marquardt, 1970).",
                        ". Por convención, valores entre 5 y 10 encienden una alerta moderada, y valores por encima de 10 se consideran un problema severo (Marquardt, 1970)."
                    )
                ) else tr("Not applicable: requires covariates.", "No aplica: requiere covariables."),
                if (length(covs) > 0) tr(
                    "Common error: assuming a high VIF means a covariate or factor should be removed. The model can still predict well overall; the issue is isolating that specific term's individual effect from the others.",
                    "Error común: pensar que un VIF alto significa que hay que eliminar la covariable o el factor. El modelo puede seguir prediciendo bien en conjunto; el problema es aislar el efecto individual de ese término frente a los demás."
                ) else "",
                if (length(covs) > 0) tr(
                    "If collinearity is a real concern with multiple covariates, consider combining the most redundant ones or centering variables involved in interactions, rather than discarding the full model.",
                    "Si la colinealidad es una preocupación real con varias covariables, considere combinar las más redundantes o centrar las variables involucradas en interacciones, en vez de descartar el modelo completo."
                ) else ""
            ))

            # -----------------------------------------------------------------------------
            # Correlation matrices (Pearson and dCor, APA 7 format) + copent discordance.
            # ES: Matrices de correlación (Pearson y dCor, formato APA 7) + discordancia con copent.
            # -----------------------------------------------------------------------------

            self$results$correlationMatrixGuide$setContent(html_list_guide(
                tr("Quick guide", "Guía breve"),
                tr(
                    "These two tables complement the multicollinearity checks with an overview of the association between the dependent variable and all numeric covariates, each in APA 7 format (lower triangle, numbered variables).",
                    "Estas dos tablas complementan las pruebas de multicolinealidad con una vista general de la asociación entre la variable dependiente y todas las covariables numéricas, cada una en formato APA 7 (triángulo inferior, variables numeradas)."
                ),
                tr(
                    "The first reports conventional Pearson correlation (linear association only); the second reports distance correlation (dCor, Szekely et al., 2007), which detects linear and non-linear association alike.",
                    "La primera reporta la correlación de Pearson convencional (solo detecta asociación lineal); la segunda reporta la correlación de distancia (dCor, Székely et al., 2007), que detecta asociación lineal y no lineal por igual."
                )
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

                    self$results$correlationMatrixNote$setContent(block96(
                        paste0(sprintf("N = %d.", n_used), " ", tr("* p < .05, ** p < .01, *** p < .001", "* p < .05, ** p < .01, *** p < .001")),
                        .al_dcor_na_note(lang)
                    ))
                } else {
                    self$results$correlationMatrixNote$setContent(block96(
                        tr("Not applicable: fewer than two numeric model variables.", "No aplica: menos de dos variables numéricas del modelo.")
                    ))
                }

                self$results$correlationComparisonGuide$setContent(block96(
                    tr("Pearson / dCor / Copula Entropy Discordance Analysis", "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                    tr(
                        "Because Pearson's r only captures linear association while dCor captures both linear and non-linear association, a pair whose dCor is notably larger than its Pearson |r| is a signal (not proof) of a non-linear relationship.",
                        "Dado que la r de Pearson solo capta asociación lineal mientras que dCor capta asociación lineal y no lineal por igual, un par cuyo dCor sea notablemente mayor que su |r| de Pearson es una señal (no una prueba) de una relación no lineal."
                    ),
                    tr(
                        "Pairs are flagged in the \"Pairs with a Notable Difference between Pearson and dCor\" table when the gap (dCor minus |Pearson r|) is greater than .10. The copula entropy (CE, copent()) result for the same pair is shown alongside as a second, distribution-free line of evidence.",
                        "Se señalan en la tabla \"Pares con diferencia notable entre Pearson y dCor\" los pares con una brecha (dCor menos |r| de Pearson) mayor a .10. El resultado de la prueba de entropía copular (CE, copent()) para el mismo par se muestra al lado como una segunda línea de evidencia libre de supuestos distribucionales."
                    ),
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
                        pearson = apaCell(pr$pearsonR, pr$pearsonP),
                        dcor = apaCell(pr$dcor, pr$dcorP),
                        gap = gap, ce = pr$ce, ceP = pr$ceP,
                        ceSig = p_sig(pr$ceP),
                        flag = tr("Yes", "Sí")
                    ))
                }

                if (length(flaggedPairs) == 0) {
                    self$results$correlationComparisonInterpretation$setContent(block96(
                        tr("Applied Interpretation", "Interpretación Aplicada"),
                        if (k < 2)
                            tr("Not applicable: fewer than two numeric model variables.", "No aplica: menos de dos variables numéricas del modelo.")
                        else
                            tr(sprintf("No pair shows a Pearson/dCor gap greater than %.2f; there is no indication of unmodeled non-linear association among the model's numeric variables.", gapThreshold),
                               sprintf("Ningún par muestra una brecha Pearson/dCor mayor a %.2f; no hay indicios de asociación no lineal no modelada entre las variables numéricas del modelo.", gapThreshold))
                    ))
                } else {
                    self$results$correlationComparisonInterpretation$setContent(block96(
                        tr("Applied Interpretation", "Interpretación Aplicada"),
                        tr(sprintf("%d pair(s) show a Pearson/dCor gap greater than %.2f: %s. Inspect a scatterplot of each flagged pair; when copula entropy is also significant for a pair, this reinforces the suspicion of an unmodeled non-linear dependency.",
                                   length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", ")),
                           sprintf("%d par(es) muestran una brecha Pearson/dCor mayor a %.2f: %s. Revise un diagrama de dispersión de cada par señalado; cuando la entropía copular también es significativa para un par, esto refuerza la sospecha de una dependencia no lineal no modelada.",
                                   length(flaggedPairs), gapThreshold, paste(flaggedPairs, collapse = ", ")))
                    ))
                }
            }

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
                tr(
                    paste0(
                        "No influential cases were detected under the main numeric criteria. ",
                        "This result is compatible with the absence of clearly influential cases ",
                        "according to the thresholds used. Even so, it is worth reviewing the ",
                        "table and plots if any cases show relatively high values."
                    ),
                    paste0(
                        "No se detectaron casos que activen los criterios numéricos principales ",
                        "de influencia o atipicidad. Este resultado es compatible con ausencia de ",
                        "casos claramente influyentes según los umbrales usados. Aun así, conviene ",
                        "revisar la tabla y los gráficos si existen casos con valores relativamente ",
                        "altos."
                    )
                )
            } else {
                tr(
                    paste0(
                        influence_count, " case(s) were detected with at least one criterion ",
                        "activated. This suggests reviewing those cases before interpreting the ",
                        "model. A flagged case should not be removed automatically; first check ",
                        "whether it reflects a data error, a valid extreme case, or substantive ",
                        "information."
                    ),
                    paste0(
                        "Se detectaron ", influence_count, " caso(s) con al menos un criterio ",
                        "activado. Esto sugiere revisar esos casos antes de interpretar el modelo. ",
                        "Un caso marcado no debe eliminarse automáticamente; primero debe verificarse ",
                        "si representa error de datos, caso extremo válido o información sustantiva."
                    )
                )
            }

            self$results$influenceInterpretation$setContent(
                block96(
                    tr("Applied Interpretation", "Interpretación Aplicada"),
                    influence_text,
                    tr(
                        "Why it matters: in ANOVA/ANCOVA, an influential case can shift group means, cell variances, or the covariate slope enough to change which post hoc comparisons look significant.",
                        "Por qué importa: en ANOVA/ANCOVA, un caso influyente puede desplazar las medias de grupo, las varianzas de celda o la pendiente de la covariable lo suficiente como para cambiar qué comparaciones post hoc resultan significativas."
                    ),
                    tr(
                        "Common error: deleting every flagged case to \"clean up\" the model. This can hide genuine subgroup differences and produce a model that only fits the well-behaved majority of the data.",
                        "Error común: eliminar todos los casos marcados para \"limpiar\" el modelo. Esto puede ocultar diferencias reales entre subgrupos y producir un modelo que solo se ajusta a la mayoría bien comportada de los datos."
                    ),
                    tr(
                        "Sample-size nuance: the 4/n threshold used for Cook's D becomes stricter as the sample shrinks, so in small designs it is common to flag several cases that are not truly problematic.",
                        "Matiz de tamaño de muestra: el umbral 4/n usado para Cook's D se vuelve más estricto a medida que la muestra es más pequeña, así que en diseños pequeños es común marcar varios casos que no son realmente problemáticos."
                    )
                )
            )

            self$results$influenceGuide$setContent(html_list_guide(
                tr("Quick guide", "Guía breve"),
                tr(
                    "These diagnostics identify outlying or influential cases in the model.",
                    "Estos diagnósticos identifican casos atípicos o influyentes en el modelo."
                ),
                tr(
                    "A flagged case should not be removed automatically.",
                    "Un caso marcado no debe eliminarse automáticamente."
                ),
                tr(
                    "Check the data, compare a sensitivity analysis, and justify decisions.",
                    "Revise el dato, compare análisis de sensibilidad y justifique decisiones."
                ),
                tr(
                    "See methodological details in Assumption Library.",
                    "Revise detalles metodológicos en Assumption Library."
                )
            ))

            self$results$residualsLeverageGuide$setContent(plot_guide(
                tr("identify cases with a combination of extreme residual and high leverage.",
                   "identificar casos con combinación de residuo extremo y alto leverage."),
                tr("leverage, i.e. how far a case sits from the center of the predictor space.",
                   "leverage, o distancia del caso respecto al centro del espacio de predictores."),
                tr("studentized residual, how extreme the case's error is.",
                   "residuo studentizado, qué tan extremo es el error del caso."),
                tr("most cases near zero and with low leverage.",
                   "la mayoría de los casos cerca de cero y con leverage bajo."),
                tr("cases with a large studentized residual, high leverage, or a large Cook's D.",
                   "casos con residuo studentizado grande, leverage alto o Cook's D elevado."),
                tr("review influential cases and compare a sensitivity analysis.",
                   "revisar los casos influyentes y comparar un análisis de sensibilidad.")
            ))

            self$results$cooksDGuide$setContent(plot_guide(
                tr("detect observations that can change the model's fit.",
                   "detectar observaciones que pueden cambiar el ajuste del modelo."),
                tr("case number or position in the dataset.",
                   "número o posición del caso en la base de datos."),
                tr("Cook's distance, an indicator of overall influence.",
                   "distancia de Cook, indicador de influencia global."),
                tr("small bars, below the reference line.",
                   "barras pequeñas y por debajo de la línea de referencia."),
                tr("cases with Cook's D above the visual threshold.",
                   "casos con Cook's D por encima del umbral visual."),
                tr("verify the case's plausibility and document analyses with/without influential cases.",
                   "verificar la plausibilidad del caso y documentar análisis con/sin casos influyentes.")
            ))


            self$results$independenceGuide$setContent(html_list_guide(
                tr("Quick guide", "Guía breve"),
                tr(
                    "Independence depends mainly on the study design.",
                    "La independencia depende principalmente del diseño del estudio."
                ),
                tr(
                    "Repeated measures, paired data, or nested data require a different approach.",
                    "Medidas repetidas, datos pareados o datos anidados requieren otro enfoque."
                ),
                tr(
                    "Use Related Groups or mixed models if independence is not defensible.",
                    "Use Related Groups o modelos mixtos si la independencia no es defendible."
                ),
                tr(
                    "Why it matters: dependent observations underestimate standard errors, making p-values look more significant than the data actually support.",
                    "Por qué importa: las observaciones dependientes subestiman los errores estándar, haciendo que los valores p parezcan más significativos de lo que los datos realmente respaldan."
                ),
                tr(
                    "Common error: assuming independence is satisfied simply because no diagnostic flagged it. Independence is a property of how the data were collected, not something a single test can confirm after the fact.",
                    "Error común: asumir que la independencia se cumple solo porque ningún diagnóstico la marcó. La independencia es una propiedad de cómo se recolectaron los datos, no algo que una sola prueba pueda confirmar después."
                )
            ))


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
                    tr("Unbalanced design or small cells", "Diseño desbalanceado o celdas pequeñas"),
                    tr("There are small cells or an elevated max/min ratio.", "Hay celdas pequeñas o razón max/min elevada."),
                    tr("Review design, cell sizes, and robustness.", "Revisar diseño, tamaños de celda y robustez."),
                    tr(
                        "When some cells have very few cases or the design is unbalanced.",
                        "Cuando algunas celdas tienen muy pocos casos o el diseño está desbalanceado."
                    )
                )
            }

            if (variance_problem) {
                add_alt(
                    tr("Different variances between groups", "Varianzas diferentes entre grupos"),
                    tr("One or more tests suggest unequal variances.", "Una o más pruebas sugieren desigualdad de varianzas."),
                    tr("Welch ANOVA, Brown-Forsythe ANOVA, or robust methods.", "Welch ANOVA, Brown-Forsythe ANOVA o métodos robustos."),
                    tr(
                        "When homogeneity of variances is not defensible.",
                        "Cuando la homogeneidad de varianzas no es defendible."
                    )
                )
            }

            if (normality_problem) {
                add_alt(
                    tr("Non-normality of residuals", "No normalidad de residuos"),
                    tr("One or more tests suggest a deviation from normality.", "Una o más pruebas sugieren desviación de normalidad."),
                    tr(
                        "Transformation, bootstrap, robust method, or non-parametric alternative.",
                        "Transformación, bootstrap, método robusto o alternativa no paramétrica."
                    ),
                    tr(
                        "When non-normality is strong or combined with outliers.",
                        "Cuando la no normalidad es fuerte o se combina con outliers."
                    )
                )
            }

            if (slopes_problem) {
                add_alt(
                    tr("Non-homogeneous slopes", "Pendientes no homogéneas"),
                    tr("Some factor × covariate interaction was relevant.", "Alguna interacción factor × covariable fue relevante."),
                    tr(
                        "Model the interaction or use a regression approach with interaction.",
                        "Modelar la interacción o usar un enfoque de regresión con interacción."
                    ),
                    tr(
                        "When the covariate-response relationship changes across groups.",
                        "Cuando la relación covariable-respuesta cambia entre grupos."
                    )
                )
            }

            if (linearity_problem) {
                add_alt(
                    tr("Non-linearity of covariates", "No linealidad de covariables"),
                    tr("A covariate shows possible curvature.", "Una covariable muestra posible curvatura."),
                    tr(
                        "Polynomial terms, splines, or covariate transformation.",
                        "Términos polinómicos, splines o transformación de covariable."
                    ),
                    tr(
                        "When the covariate-response relationship does not appear linear.",
                        "Cuando la relación covariable-respuesta no parece lineal."
                    )
                )
            }

            if (influence_problem) {
                add_alt(
                    tr("Outlying or influential cases", "Casos atípicos o influyentes"),
                    tr("Some case activated numeric criteria.", "Algún caso activó criterios numéricos."),
                    tr("Sensitivity analysis or robust method.", "Análisis de sensibilidad o método robusto."),
                    tr(
                        "When some cases may dominate the results.",
                        "Cuando algunos casos pueden dominar los resultados."
                    )
                )
            }

            if (dcor_problem) {
                add_alt(
                    tr("Non-linear covariate dependence (dCor)", "Dependencia no lineal en covariable (dCor)"),
                    tr(
                        "Distance correlation is notably higher than Pearson's r for at least one covariate.",
                        "La correlación de distancia es notablemente mayor que r de Pearson para al menos una covariable."
                    ),
                    tr(
                        "Inspect a scatterplot; consider a non-linear term or transformation for that covariate.",
                        "Revise un diagrama de dispersión; considere un término no lineal o una transformación para esa covariable."
                    ),
                    tr(
                        "When dCor detects dependence that a linear covariate adjustment would miss.",
                        "Cuando dCor detecta una dependencia que un ajuste lineal por la covariable no captaría."
                    )
                )
            }

            if (!design_problem && !variance_problem && !normality_problem &&
                !slopes_problem && !linearity_problem && !influence_problem && !dcor_problem) {
                add_alt(
                    tr("No primary alert", "Sin alerta principal"),
                    tr(
                        "The diagnostics show no strong signals in the assumptions evaluated.",
                        "Los diagnósticos no muestran señales fuertes en los supuestos evaluados."
                    ),
                    tr("Standard ANOVA/ANCOVA.", "ANOVA/ANCOVA estándar."),
                    tr(
                        "When the design and assumptions are reasonably defensible.",
                        "Cuando el diseño y los supuestos son razonablemente defendibles."
                    )
                )
            }

            if (length(alt_when_texts) > 0) {
                self$results$alternativesWhen$setContent(.al_html_list(alt_when_texts))
            } else {
                self$results$alternativesWhen$setContent("")
            }

            self$results$alternativesGuide$setContent(html_list_guide(
                tr("Quick guide", "Guía breve"),
                tr(
                    "This table translates the diagnostics into possible methodological options.",
                    "Esta tabla traduce los diagnósticos en opciones metodológicas posibles."
                ),
                tr(
                    "Do not switch methods because of a single isolated test.",
                    "No cambie de método por una sola prueba aislada."
                ),
                tr(
                    "The decision should combine evidence, design, and research question.",
                    "La decisión debe combinar evidencia, diseño y pregunta de investigación."
                )
            ))

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
                    tr("ANOVA/ANCOVA diagnostic plots", "Gráficos diagnósticos de ANOVA/ANCOVA"),
                    tr(
                        "Diagnostic plots complement statistical tests and help evaluate patterns that a p-value may hide.",
                        "Los gráficos diagnósticos complementan las pruebas estadísticas y ayudan a evaluar patrones que un p-valor puede ocultar."
                    ),
                    tr(
                        "Use these plots to review group balance, residual normality, and influential cases.",
                        "Use estos gráficos para revisar el balance entre grupos, la normalidad de residuos y los casos influyentes."
                    )
                ))
            }

            problem_count <- sum(
                design_problem, variance_problem, normality_problem,
                slopes_problem, linearity_problem, influence_problem, dcor_problem
            )

            recommendation <- if (problem_count == 0) {
                tr(
                    "No diagnostic raised a relevant concern; the ANOVA/ANCOVA model can be interpreted under its standard assumptions.",
                    "Ningún diagnóstico mostró una alerta relevante; el modelo ANOVA/ANCOVA puede interpretarse bajo sus supuestos estándar."
                )
            } else {
                flagged <- c(
                    if (design_problem) tr("design balance", "balance del diseño"),
                    if (variance_problem) tr("homogeneity of variances", "homogeneidad de varianzas"),
                    if (normality_problem) tr("residual normality", "normalidad de residuos"),
                    if (slopes_problem) tr("homogeneity of slopes", "homogeneidad de pendientes"),
                    if (linearity_problem) tr("covariate linearity", "linealidad de covariables"),
                    if (influence_problem) tr("influential cases", "casos influyentes"),
                    if (dcor_problem) tr("non-linear covariate dependence", "dependencia no lineal en covariables")
                )
                tr(
                    sprintf(
                        "%d assumption(s) raised a concern (%s); review the suggested methodological options in this section before relying on the standard ANOVA/ANCOVA F-test.",
                        problem_count, paste(flagged, collapse = ", ")
                    ),
                    sprintf(
                        "%d supuesto(s) mostraron alerta (%s); revise las opciones metodológicas sugeridas en esta sección antes de confiar en la prueba F estándar de ANOVA/ANCOVA.",
                        problem_count, paste(flagged, collapse = ", ")
                    )
                )
            }

            self$results$notes$setContent(block96(
                paste0(tr("Suggested decision: ", "Decisión sugerida: "), recommendation),
                tr(
                    "Significance codes: * p < .05, ** p < .01, *** p < .001.",
                    "Códigos de significancia: * p < .05, ** p < .01, *** p < .001."
                )
            ))
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

        .requireGroupPlotData = function(image) {
            if (!isTRUE(self$options$showPlots))
                return(FALSE)

            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                image$setError("The ggplot2 package is required to draw diagnostic plots.")
                return(FALSE)
            }
            if (is.null(private$.groupPlotData) || nrow(private$.groupPlotData) == 0) {
                image$setError("No group plot data are available.")
                return(FALSE)
            }
            TRUE
        },

        .plotTr = function(en, es) {
            lang <- .al_normalize_lang(self$options$reportLang)
            if (identical(lang, "es")) es else en
        },

        .plotPalette = function() {
            # Base palette (bw/contrast/fullColor/clean): identical shape in
            # every module, consolidated in shared-helpers.R
            # (.al_plot_palette_base). fullColor uses Variant A per Archie's
            # decision, Aug 2026 (see the doc comment on that function).
            # ES: paleta base idéntica en todos los módulos, consolidada en
            # shared-helpers.R. fullColor usa la Variante A por decisión de
            # Archie, agosto 2026.
            style <- tryCatch(self$options$plotStyle, error = function(e) "clean")
            if (is.null(style) || length(style) == 0 || !nzchar(style))
                style <- "clean"

            .al_plot_palette_base(style)
        },

        # Categorical palette used to color the group boxplots by
        # group/cell (plotPalette option). Delegates to the shared
        # .al_plot_series_palette() (shared-helpers.R), the same function
        # logCheck/regCheck/timeCheck use for their plotPalette option, so
        # "Blue-Orange"/"Viridis"/"Grayscale"/"High Contrast" produce the
        # same colors here as everywhere else in the suite. That function
        # returns a fixed-length qualitative color set (it doesn't take a
        # group count); rep_len() cycles it to the number of groups/cells
        # actually being plotted.
        # ES: Paleta categórica usada para colorear los boxplots por
        # grupo/celda (opción plotPalette). Delega en la función
        # compartida .al_plot_series_palette() (shared-helpers.R), la
        # misma que usan logCheck/regCheck/timeCheck para su opción
        # plotPalette, para que "Blue-Orange"/"Viridis"/"Grayscale"/"High
        # Contrast" den los mismos colores acá que en el resto de la
        # suite. Esa función devuelve un set de colores cualitativo de
        # largo fijo (no recibe cantidad de grupos); rep_len() lo cicla a
        # la cantidad real de grupos/celdas que se están graficando.
        .categoricalPalette = function(n) {
            key <- tryCatch(self$options$plotPalette, error = function(e) "blueOrange")
            if (is.null(key) || length(key) == 0 || !nzchar(key))
                key <- "blueOrange"

            rep_len(.al_plot_series_palette(key), n)
        },

        .plotTheme = function() {
            style <- tryCatch(self$options$plotStyle, error = function(e) "clean")
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

        .addSmoother = function(plot, x, y) {
            smoother <- tryCatch(self$options$residSmoother, error = function(e) "none")
            if (is.null(smoother) || length(smoother) == 0 || !nzchar(smoother))
                smoother <- "none"

            show_band <- isTRUE(self$options$residShowBand)

            if (identical(smoother, "loess")) {
                plot + ggplot2::geom_smooth(
                    ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                    method = "loess", se = show_band, linewidth = 0.6,
                    color = private$.plotPalette()$smooth,
                    fill = private$.plotPalette()$smooth
                )
            } else if (identical(smoother, "linear")) {
                plot + ggplot2::geom_smooth(
                    ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                    method = "lm", se = show_band, linewidth = 0.6,
                    color = private$.plotPalette()$smooth,
                    fill = private$.plotPalette()$smooth
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

        .plotResidualsFitted = function(image, ...) {
            if (!private$.requirePlotData(image)) return()
            d <- private$.plotData
            d <- d[is.finite(d$fitted) & is.finite(d$residual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = residual))

            if (isTRUE(self$options$residRefLine)) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = private$.plotPalette()$ref)
            }

            plot <- plot +
                ggplot2::geom_point(alpha = 0.75, size = 1.6, color = private$.plotPalette()$point)

            if (length(unique(d$fitted)) >= 5) {
                plot <- private$.addSmoother(plot, "fitted", "residual")
            }

            plot <- plot +
                ggplot2::labs(
                    x = private$.plotTr("Fitted values", "Valores ajustados"),
                    y = private$.plotTr("Residuals", "Residuos")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotQQResiduals = function(image, ...) {
            if (!private$.requirePlotData(image)) return()
            d <- private$.plotData
            d <- d[is.finite(d$stdResidual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(sample = stdResidual)) +
                ggplot2::stat_qq(alpha = 0.75, size = 1.6, color = private$.plotPalette()$point) +
                ggplot2::stat_qq_line(linewidth = 0.6, color = private$.plotPalette()$line) +
                ggplot2::labs(
                    x = private$.plotTr("Theoretical quantiles", "Cuantiles teóricos"),
                    y = private$.plotTr("Standardized residuals", "Residuos estandarizados")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotResidualHistogram = function(image, ...) {
            if (!private$.requirePlotData(image)) return()
            d <- private$.plotData
            d <- d[is.finite(d$stdResidual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = stdResidual)) +
                ggplot2::geom_histogram(bins = 30, alpha = 0.85,
                                         fill = private$.plotPalette()$fill,
                                         color = private$.plotPalette()$line) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = private$.plotPalette()$ref) +
                ggplot2::labs(
                    x = private$.plotTr("Standardized residuals", "Residuos estandarizados"),
                    y = private$.plotTr("Count", "Frecuencia")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotResidualNormalCurve = function(image, ...) {
            if (!private$.requirePlotData(image)) return()
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
                ggplot2::geom_density(linewidth = 0.8, color = private$.plotPalette()$smooth) +
                ggplot2::geom_line(
                    data = normal_df,
                    ggplot2::aes(x = stdResidual, y = density, linetype = curve),
                    linewidth = 0.6,
                    color = private$.plotPalette()$alert
                ) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = private$.plotPalette()$ref) +
                ggplot2::scale_linetype_manual(values = "dashed", name = NULL) +
                ggplot2::labs(
                    x = private$.plotTr("Standardized residuals", "Residuos estandarizados"),
                    y = private$.plotTr("Density", "Densidad")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotGroupBoxplots = function(image, ...) {
            if (!private$.requireGroupPlotData(image)) return()
            d <- private$.groupPlotData
            d <- d[is.finite(d$value), , drop = FALSE]
            d$cell <- factor(d$cell, levels = unique(d$cell))
            pal <- private$.categoricalPalette(nlevels(d$cell))

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = cell, y = value, fill = cell)) +
                ggplot2::geom_boxplot(
                    width = 0.65,
                    outlier.colour = private$.plotPalette()$alert,
                    outlier.alpha = 0.85,
                    outlier.size = 1.6,
                    color = private$.plotPalette()$line,
                    show.legend = FALSE
                ) +
                ggplot2::scale_fill_manual(values = pal) +
                ggplot2::labs(
                    x = private$.plotTr("Group / cell", "Grupo / celda"),
                    y = private$.plotTr("Observed values", "Valores observados")
                ) +
                private$.plotTheme() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))

            print(plot)
        },

        .plotResidualsLeverage = function(image, ...) {
            if (!private$.requirePlotData(image)) return()
            d <- private$.plotData
            d <- d[is.finite(d$leverage) & is.finite(d$studResidual), , drop = FALSE]

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = leverage, y = studResidual))

            if (isTRUE(self$options$influenceShowThreshold)) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = c(-3, 0, 3), linetype = "dashed", color = private$.plotPalette()$ref)
            }

            plot <- plot +
                ggplot2::geom_point(ggplot2::aes(size = cooksD), alpha = 0.75, color = private$.plotPalette()$point) +
                ggplot2::scale_size_continuous(name = "Cook's D", range = c(1.2, 4)) +
                ggplot2::labs(
                    x = private$.plotTr("Leverage", "Leverage"),
                    y = private$.plotTr("Studentized residuals", "Residuos studentizados")
                ) +
                private$.plotTheme()

            lab <- private$.labelPlotCases(d)
            if (!is.null(lab) && nrow(lab) > 0) {
                plot <- plot + ggplot2::geom_text(
                    data = lab,
                    ggplot2::aes(x = leverage, y = studResidual, label = caseLabel),
                    inherit.aes = FALSE, size = 3, hjust = -0.1, vjust = -0.5,
                    check_overlap = TRUE, color = private$.plotPalette()$alert
                )
            }

            print(plot)
        },

        .plotCooksD = function(image, ...) {
            if (!private$.requirePlotData(image)) return()
            d <- private$.plotData
            d <- d[is.finite(d$case) & is.finite(d$cooksD), , drop = FALSE]
            cut <- 4 / max(1, nrow(d))

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = case, y = cooksD)) +
                ggplot2::geom_col(alpha = 0.85, fill = private$.plotPalette()$fill, color = private$.plotPalette()$line)

            if (isTRUE(self$options$influenceShowThreshold)) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = cut, linetype = "dashed", color = private$.plotPalette()$alert)
            }

            plot <- plot +
                ggplot2::labs(
                    x = private$.plotTr("Case", "Caso"),
                    y = private$.plotTr("Cook's D", "Cook's D")
                ) +
                private$.plotTheme()

            lab <- private$.labelPlotCases(d)
            if (!is.null(lab) && nrow(lab) > 0) {
                plot <- plot + ggplot2::geom_text(
                    data = lab,
                    ggplot2::aes(x = case, y = cooksD, label = caseLabel),
                    inherit.aes = FALSE, size = 3, vjust = -0.6,
                    check_overlap = TRUE, color = private$.plotPalette()$alert
                )
            }

            print(plot)
        }
    )
)
