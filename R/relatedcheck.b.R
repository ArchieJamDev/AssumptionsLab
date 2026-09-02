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
# Related Groups.
# ES: Grupos Relacionados.
#
# This file implements relatedCheck: an assumption-diagnostic module for
# comparing two or more related (repeated/paired) measurements of the same
# subjects. It reports the assumption diagnostics that decide whether a
# paired t-test/Wilcoxon (two measurements) or repeated-measures ANOVA/
# Friedman (more than two) comparison is trustworthy (normality and
# symmetry of the paired differences, sphericity for more than two
# measurements, and outlier screening on the differences), and recommends
# a specific test given the observed pattern.
#
# ES: Este archivo implementa relatedCheck: un módulo de diagnóstico de
# supuestos para comparar dos o más mediciones relacionadas (repetidas/
# pareadas) de los mismos sujetos. Reporta los diagnósticos de supuestos
# que deciden si una comparación por t pareada/Wilcoxon (dos mediciones) o
# ANOVA de medidas repetidas/Friedman (más de dos) es confiable
# (normalidad y simetría de las diferencias pareadas, esfericidad para más
# de dos mediciones, y cribado de atípicos en las diferencias), y
# recomienda una prueba específica según el patrón observado.
#
# Responsibilities
# 1. Compute descriptive statistics per measurement and per pairwise
#    difference.
# 2. Compute and report the normality and symmetry battery for the paired
#    differences, and the sphericity battery when there are more than two
#    measurements.
# 3. Screen for outliers among the paired differences.
# 4. Render the diagnostic plots (profile, paired-difference distribution,
#    Q-Q, observed-vs-normal density).
# 5. Assemble the applied-interpretation text for every diagnostic area
#    and a dynamic test recommendation, in the user's selected report
#    language.
#
# ES: Responsabilidades
# 1. Calcular estadísticos descriptivos por medición y por diferencia
#    pareada.
# 2. Calcular y reportar la batería de normalidad y simetría de las
#    diferencias pareadas, y la batería de esfericidad cuando hay más de
#    dos mediciones.
# 3. Cribar atípicos entre las diferencias pareadas.
# 4. Renderizar los gráficos diagnósticos (perfil, distribución de
#    diferencias pareadas, Q-Q, densidad observada vs normal).
# 5. Ensamblar el texto de interpretación aplicada para cada área
#    diagnóstica y una recomendación de prueba dinámica, en el idioma de
#    informe seleccionado por el usuario.
#
# Workflow
# 1. Describe: compute per-measurement and per-pairwise-difference
#    descriptives.
# 2. Test assumptions: run the normality/symmetry battery on the paired
#    differences, and the sphericity battery when applicable.
# 3. Screen: flag outlying differences.
# 4. Plot: render the profile, difference-distribution, and normality
#    plots.
# 5. Interpret: build the applied-interpretation text for every
#    diagnostic area.
# 6. Recommend: assemble a dynamic recommendation (which test fits the
#    observed assumption pattern) into the notes section.
#
# ES: Flujo de trabajo
# 1. Describir: calcular descriptivos por medición y por diferencia
#    pareada.
# 2. Probar supuestos: correr la batería de normalidad/simetría sobre las
#    diferencias pareadas, y la batería de esfericidad cuando aplique.
# 3. Cribar: marcar diferencias atípicas.
# 4. Graficar: renderizar los gráficos de perfil, distribución de
#    diferencias y normalidad.
# 5. Interpretar: construir el texto de interpretación aplicada para cada
#    área diagnóstica.
# 6. Recomendar: ensamblar una recomendación dinámica (qué prueba se
#    ajusta al patrón de supuestos observado) en la sección de notas.
# -----------------------------------------------------------------------------

relatedCheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "relatedCheckClass",
    inherit = relatedCheckBase,
    private = list(
        .profilePlotData = NULL,

        .init = function() {
            private$.initCorrelationMatrix()
        },

        .initCorrelationMatrix = function() {
            measures <- self$options$measures
            for (tableName in c("pearsonMatrixTable", "dcorMatrixTable")) {
                table <- tryCatch(self$results[[tableName]], error = function(e) NULL)
                if (is.null(table)) next()
                for (i in seq_along(measures)) {
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

        # -----------------------------------------------------------------------------
        # Main analysis entry point.
        # ES: Punto de entrada principal del análisis.
        #
        # Computes descriptives, paired differences, normality/symmetry/sphericity
        # diagnostics, outlier screening, and the dynamic test recommendation for
        # every pair of related measurements; then renders every guide,
        # interpretation, and table title in the user's selected report language.
        #
        # ES: Calcula descriptivos, diferencias pareadas, diagnósticos de
        # normalidad/simetría/esfericidad, cribado de atípicos y la recomendación
        # de prueba dinámica para cada par de mediciones relacionadas; luego
        # renderiza cada guía, interpretación y título de tabla en el idioma de
        # informe seleccionado por el usuario.
        # -----------------------------------------------------------------------------
        .run = function() {

        lang <- .al_normalize_lang(self$options$reportLang)
        tr <- function(en, es = NULL) .al_tr(lang, en, es)
        plot_tr <- function(en, es = NULL) private$.plotTr(en, es, if (exists("image", inherits = FALSE)) image else NULL)

        .al_set_title <- function(name, en, es = NULL) {
            title <- tr(en, es)
            tryCatch({
                self$results[[name]]$setTitle(title)
            }, error = function(e) {
                invisible(NULL)
            })
            invisible(NULL)
        }

        # -----------------------------------------------------------------------------
        # Set every result panel's title and table/column titles in the selected
        # report language.
        # ES: Fijar el título de cada panel de resultados y los títulos de tabla/
        # columna en el idioma de informe seleccionado.
        # -----------------------------------------------------------------------------
        set_result_titles <- function() {
            set_title_safe <- function(name, en, es) {
                element <- tryCatch(self$results[[name]], error = function(e) NULL)
                if (is.null(element))
                    return(invisible(FALSE))

                tryCatch(
                    element$setTitle(tr(en, es)),
                    error = function(e) invisible(FALSE)
                )

                invisible(TRUE)
            }

            titles <- list(
                c("intro", "Related Groups", "Related Groups"),

                c("designGuide",
                  "Related-groups design",
                  "Diseño de grupos relacionados"),
                c("designSummary",
                  "Design summary",
                  "Resumen del diseño"),

                c("descriptiveSummary",
                  "Descriptive statistics by measurement",
                  "Descriptivos por medición"),

                c("profilePlot",
                  "Profile of related measurements",
                  "Perfil de mediciones relacionadas"),
                c("differencePlot",
                  "Distribution of paired differences",
                  "Distribución de diferencias pareadas"),
                c("normalityPlot",
                  "Q-Q plots of paired differences",
                  "Q-Q plots de diferencias pareadas"),
                c("normalCurvePlot",
                  "Observed distribution vs theoretical normal curve",
                  "Distribución observada vs curva normal teórica"),

                c("differenceGuide",
                  "Paired differences",
                  "Diferencias pareadas"),
                c("differenceSummary",
                  "Paired differences",
                  "Diferencias pareadas"),

                c("normalityGuide",
                  "Normality of paired differences",
                  "Normalidad de las diferencias pareadas"),
                c("normality",
                  "Normality of paired differences",
                  "Normalidad de las diferencias pareadas"),
                c("normalitySummary",
                  "Normality summary",
                  "Resumen de normalidad"),
                c("normalityInterpretation",
                  "Applied Interpretation",
                  "Interpretación Aplicada"),

                c("normalityRecommendation", "Recommendation", "Recomendación"),
                c("symmetryGuide",
                  "Symmetry of paired differences",
                  "Simetría de las diferencias pareadas"),
                c("symmetry",
                  "Symmetry of paired differences",
                  "Simetría de las diferencias pareadas"),
                c("symmetryInterpretation",
                  "Applied Interpretation",
                  "Interpretación Aplicada"),

                c("sphericityGuide",
                  "Sphericity",
                  "Esfericidad"),
                c("sphericity",
                  "Sphericity",
                  "Esfericidad"),
                c("sphericityInterpretation",
                  "Applied Interpretation",
                  "Interpretación Aplicada"),

                c("correlationComparisonInterpretation",
                  "Applied Interpretation",
                  "Interpretación Aplicada"),

                c("outliersGuide",
                  "Outliers in paired differences",
                  "Casos atípicos en diferencias"),
                c("outliers",
                  "Outlier diagnostics",
                  "Diagnóstico de valores atípicos"),
                c("outliersInterpretation",
                  "Applied Interpretation",
                  "Interpretación Aplicada"),

                c("methodGuide",
                  "Method selection",
                  "Selección de prueba"),
                c("methodOptions",
                  "Methodological options",
                  "Opciones metodológicas"),

                c("methodWhen", "", ""),
                c("notes",
                  "Notes and recommendation",
                  "Notas y recomendación"),

                c("pearsonMatrixTable",
                  "Pearson correlation matrix (APA 7 format)",
                  "Matriz de correlaciones de Pearson (formato APA 7)"),
                c("dcorMatrixTable",
                  "Distance correlation matrix (dCor, APA 7 format)",
                  "Matriz de correlación de distancia (dCor, formato APA 7)"),

                c("correlationMatrixGuide", "Correlation matrix", "Matriz de correlaciones"),
                c("correlationMatrixNote", " ", " "),
                c("correlationComparisonGuide",
                  "Pearson / dCor / Copula Entropy Discordance Analysis",
                  "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                c("correlationComparisonTable",
                  "Pairs with a Notable Difference between Pearson and dCor",
                  "Pares con diferencia notable entre Pearson y dCor")
            )

            for (item in titles)
                set_title_safe(item[[1]], item[[2]], item[[3]])

            invisible(NULL)
        }

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

            columns <- list(
                c("designSummary", "item", "Item", "Elemento"),
                c("designSummary", "value", "Value", "Valor"),

                c("descriptiveSummary", "measure", "Measurement", "Medición"),
                c("descriptiveSummary", "missing", "Missing", "Faltantes"),

                c("differenceSummary", "comparison", "Comparison", "Comparación"),
                c("differenceSummary", "meanDiff", "M difference", "M diferencia"),
                c("differenceSummary", "sdDiff", "SD difference", "SD diferencia"),
                c("differenceSummary", "medianDiff", "Mdn difference", "Mdn diferencia"),

                c("normality", "scope", "Scope", "Alcance"),
                c("normality", "test", "Test", "Prueba"),
                c("normality", "statistic", "Statistic", "Estadístico"),
                c("normality", "value", "Value", "Valor"),

                c("normalitySummary", "comparison", "Comparison", "Comparación"),
                c("normalitySummary", "tests", "Valid tests", "Pruebas válidas"),
                c("normalitySummary", "significant", "Significant tests", "Pruebas significativas"),
                c("normalitySummary", "interpretation", "Interpretation", "Interpretación"),

                c("symmetry", "comparison", "Comparison", "Comparación"),
                c("symmetry", "statistic", "Statistic", "Estadístico"),
                c("symmetry", "value", "Value", "Valor"),

                c("sphericity", "diagnostic", "Diagnostic", "Diagnóstico"),
                c("sphericity", "statistic", "Statistic", "Estadístico"),
                c("sphericity", "value", "Value", "Valor"),

                c("outliers", "case", "Case", "Caso"),
                c("outliers", "comparison", "Comparison", "Comparación"),
                c("outliers", "difference", "Difference", "Diferencia"),
                c("outliers", "criterion", "Triggered criterion", "Criterio activado"),

                c("methodOptions", "design", "Design", "Diseño"),
                c("methodOptions", "evidence", "Evidence", "Evidencia"),
                c("methodOptions", "suggestion", "Suggested test", "Prueba sugerida"),

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
                c("correlationComparisonTable", "flag", "Notable difference", "Diferencia notable")
            )

            for (item in columns)
                set_col_title_safe(item[[1]], item[[2]], item[[3]], item[[4]])

            invisible(NULL)
        }

        clean_num <- function(x) {
                if (length(x) == 0)
                    return(NA_real_)
                x <- suppressWarnings(as.numeric(x[1]))
                if (is.na(x) || is.nan(x) || is.infinite(x))
                    return(NA_real_)
                x
            }

            sig_code <- function(p_value) {
                if (!is.finite(p_value))
                    return("")
                if (p_value < .001)
                    return("***")
                if (p_value < .01)
                    return("**")
                if (p_value < .05)
                    return("*")
                ""
            }

            # p_sig(): identical logic (via clean_num) in every module,
            # consolidated in shared-helpers.R (.al_p_sig).
            # ES: idéntica (vía clean_num) en todos los módulos,
            # consolidada en shared-helpers.R.
            p_sig <- .al_p_sig

            format_p <- function(p) {
                p <- clean_num(p)
                if (is.na(p))
                    return(tr("not calculated", "no calculado"))
                if (p < .001)
                    return("< .001")
                sub("^0", "", sprintf("%.3f", p))
            }

            sample_note <- function(n) {
                if (is.na(n))
                    return("El tamaño muestral no pudo determinarse para esta prueba.")
                if (n < 10)
                    return(paste0(
                        "Con n = ", n, ", el tamaño muestral es muy pequeño; ",
                        "interprete este diagnóstico con mucha cautela."
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
                    return(paste0(test, tr(": not calculated.", ": no calculado.")))

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

            interpret_symmetry <- function(comparison, p_value, n) {
                p_value <- clean_num(p_value)

                if (is.na(p_value))
                    return(paste0(comparison, tr(": symmetry not calculated.", ": simetría no calculada.")))

                if (p_value < .05)
                    return(paste0(
                        comparison, ": p = ", format_p(p_value),
                        tr(". Suggests significant asymmetry in the differences.", ". Sugiere asimetría significativa de las diferencias.")
                    ))

                paste0(
                    comparison, ": p = ", format_p(p_value),
                    tr(". Compatible with approximate symmetry.", ". Compatible con simetría aproximada.")
                )
            }

            interpret_sphericity <- function(p_value, n, k) {
                p_value <- clean_num(p_value)

                if (k < 3)
                    return(tr("Not applicable: sphericity requires three or more measurements.", "No aplica: la esfericidad requiere tres o más mediciones."))

                if (is.na(p_value))
                    return("Mauchly: no calculado. Revise muestra o matriz de covarianzas.")

                if (p_value < .05)
                    return(paste0(
                        "Mauchly: p = ", format_p(p_value), ", n = ", n,
                        tr(". Significant deviation from sphericity.", ". Desviación significativa de esfericidad.")
                    ))

                paste0(
                    "Mauchly: p = ", format_p(p_value), ", n = ", n,
                    tr(". Compatible with approximate sphericity.", ". Compatible con esfericidad aproximada.")
                )
            }

            add_table_row <- function(table, key, values) {
                table$addRow(rowKey = key, values = values)
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

            block96 <- function(...) {
            txt <- paste(..., sep = "\n")
            txt <- gsub("\\n", "\n", txt)
            txt <- gsub("\\t", " ", txt)
            txt <- gsub("[ \t]+\n", "\n", txt)
            txt <- gsub("\n[ \t]+", "\n", txt)

            lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
            lines <- trimws(lines)
            lines <- lines[nzchar(lines)]

            lines <- vapply(lines, function(z) {
                z <- gsub("[ \t\r]+", " ", z)
                trimws(z)
            }, character(1))

            paste(lines, collapse = "\n")
        }

        html_escape <- .al_html_escape

        html_block <- function(title = NULL, text, paragraphs = TRUE, raw = FALSE) {
            .al_html_block(title, text, paragraphs = paragraphs, raw = raw)
        }

        wrap_paragraphs <- function(x, width = 96) {
            x <- paste(x, collapse = "\n")
            x <- gsub("\\n", "\n", x)
            x <- gsub("\\t", " ", x)

            lines <- unlist(strsplit(x, "\n", fixed = TRUE))
            lines <- trimws(lines)
            lines <- lines[nzchar(lines)]

            lines <- vapply(lines, function(z) {
                z <- gsub("[ \t\r]+", " ", z)
                trimws(z)
            }, character(1))

            paste(lines, collapse = "\n")
        }

        measures <- self$options$measures
            k <- length(measures)
            set_result_titles()
        set_table_column_titles()

            self$results$sphericityGuide$setVisible(k >= 3)
            self$results$sphericity$setVisible(k >= 3)
            self$results$sphericityInterpretation$setVisible(k >= 3)

            self$results$correlationMatrixGuide$setVisible(k >= 2)
            self$results$pearsonMatrixTable$setVisible(k >= 2)
            self$results$dcorMatrixTable$setVisible(k >= 2)
            self$results$correlationMatrixNote$setVisible(k >= 2)
            self$results$correlationComparisonGuide$setVisible(k >= 2)
            self$results$correlationComparisonTable$setVisible(k >= 2)
            self$results$correlationComparisonInterpretation$setVisible(k >= 2)

            if (length(measures) < 2) {
                self$results$intro$setContent(paste0(
                "<div style=\"line-height:1.25; margin:0.05em 0 0.35em 0;\">",
                "<p style=\"font-weight:700; margin:0 0 0.10em 0;\">AssumptionsLab</p>",
                paste0("<p style=\"margin:0 0 0.20em 0;\">", tr("Assumption check for related groups", "Revisión de supuestos para grupos relacionados"), "</p>"),
                "<p style=\"margin:0 0 0.20em 0;\">&nbsp;</p>",
                paste0("<p style=\"margin:0 0 0.20em 0;\">", tr("Use this analysis when you want to review whether a comparison between related groups has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.", "Use este análisis cuando quiera revisar si una comparación entre grupos relacionados tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."), "</p>"),
                "<p style=\"margin:0 0 0.20em 0;\">&nbsp;</p>",
                paste0("<p style=\"margin:0;\">", tr("Select at least two related numeric variables.", "Seleccione al menos dos variables numéricas relacionadas."), "</p>"),
                "</div>"
            ))
                return()
            }

            dat <- self$data[, measures, drop = FALSE]
            complete <- stats::complete.cases(dat)
            dat2 <- dat[complete, , drop = FALSE]

            n_total <- nrow(dat)
            n_used <- nrow(dat2)
            n_excluded <- n_total - n_used

            if (n_used < 3) {
                self$results$intro$setContent(paste0(
                "<div style=\"line-height:1.25; margin:0.05em 0 0.35em 0;\">",
                "<p style=\"font-weight:700; margin:0 0 0.10em 0;\">AssumptionsLab</p>",
                paste0("<p style=\"margin:0 0 0.20em 0;\">", tr("Assumption check for related groups", "Revisión de supuestos para grupos relacionados"), "</p>"),
                "<p style=\"margin:0 0 0.20em 0;\">&nbsp;</p>",
                paste0("<p style=\"margin:0 0 0.20em 0;\">", tr("Use this analysis when you want to review whether a comparison between related groups has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.", "Use este análisis cuando quiera revisar si una comparación entre grupos relacionados tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."), "</p>"),
                "<p style=\"margin:0 0 0.20em 0;\">&nbsp;</p>",
                paste0("<p style=\"margin:0;\">", tr("There are not enough complete cases to evaluate related groups.", "No hay suficientes casos completos para evaluar grupos relacionados."), "</p>"),
                "</div>"
            ))
                return()
            }

            normality_problem <- FALSE
            symmetry_problem <- FALSE
            sphericity_problem <- FALSE
            outlier_problem <- FALSE

            recommendation <- tr("Interpret the results considering design, assumptions, and sample size.", "Interprete los resultados considerando diseño, supuestos y tamaño muestral.")

            self$results$profilePlot$setVisible(isTRUE(self$options$showProfilePlot))
            self$results$differencePlot$setVisible(isTRUE(self$options$showDifferencePlots))
            self$results$normalityPlot$setVisible(isTRUE(self$options$showNormalityPlots))
        self$results$normalCurvePlot$setVisible(isTRUE(self$options$showNormalityPlots))

            symmetry_texts <- character()
            sphericity_text <- ""
            all_diffs <- list()

            self$results$intro$setContent(paste0(
            "<div style=\"line-height:1.25; margin:0.05em 0 0.35em 0;\">",
            "<p style=\"font-weight:700; margin:0 0 0.10em 0;\">AssumptionsLab</p>",
            paste0("<p style=\"margin:0 0 0.20em 0;\">", tr("Assumption check for related groups", "Revisión de supuestos para grupos relacionados"), "</p>"),
            "<p style=\"margin:0 0 0.20em 0;\">&nbsp;</p>",
            paste0("<p style=\"margin:0 0 0.20em 0;\">", tr("Use this analysis when you want to review whether a comparison between related groups has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.", "Use este análisis cuando quiera revisar si una comparación entre grupos relacionados tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."), "</p>"),
            "<p style=\"margin:0 0 0.20em 0;\">&nbsp;</p>",
            paste0("<p style=\"margin:0 0 0.08em 0;\">", tr("<b>Number of related measurements:</b> ", "<b>Número de mediciones relacionadas:</b> "), k, "</p>"),
            paste0("<p style=\"margin:0 0 0.08em 0;\">", tr("<b>Measurements:</b> ", "<b>Mediciones:</b> "), html_escape(paste(measures, collapse = ", ")), "</p>"),
            paste0("<p style=\"margin:0 0 0.08em 0;\">", tr("<b>Design type:</b> ", "<b>Tipo de diseño:</b> ")),
                html_escape(ifelse(k == 2, tr("two paired measurements", "dos mediciones pareadas"), tr("Three or more repeated measurements", "tres o más mediciones repetidas"))),
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

            add_ds(tr("Selected measurements", "Mediciones seleccionadas"), paste(measures, collapse = ", "))
            add_ds(tr("Number of measurements", "Número de mediciones"), as.character(k))
            add_ds(tr("Total cases", "Casos totales"), as.character(n_total))
            add_ds(tr("Complete cases used", "Casos completos usados"), as.character(n_used))
            add_ds(tr("Cases excluded because of missing data", "Casos excluidos por datos faltantes"), as.character(n_excluded))
            add_ds(
                tr("Suggested analysis type", "Tipo de análisis sugerido"),
                ifelse(k == 2, tr("Paired comparison", "Comparación pareada"), tr("Repeated measures", "Medidas repetidas"))
            )

            self$results$designGuide$setContent(html_block(
            tr("Brief guide", "Guía breve"),
            .al_html_list(c(
                tr("Use this option when the same units are measured two or more times.", "Use esta opción cuando las mismas unidades se miden dos o más veces."),
                tr("Each row should represent one person, unit, or related pair.", "Cada fila debe representar una persona, unidad o par relacionado."),
                tr("With two measurements, paired differences are analyzed.", "Con dos mediciones se analizan diferencias pareadas."),
                tr("With three or more measurements, repeated-measures assumptions are reviewed.", "Con tres o más mediciones se revisan supuestos de medidas repetidas.")
            )),
            raw = TRUE
        ))

            # Fill descriptive summary table.
            # ES: Llenar la tabla de resumen descriptivo.
            for (v in measures) {
                x_all <- suppressWarnings(as.numeric(dat[[v]]))
                x_valid <- x_all[is.finite(x_all)]

                add_table_row(
                    self$results$descriptiveSummary,
                    paste0("desc_", make.names(v)),
                    list(
                        measure = v,
                        n = as.integer(length(x_valid)),
                        mean = clean_num(if (length(x_valid) > 0) mean(x_valid) else NA_real_),
                        sd = clean_num(if (length(x_valid) > 1) stats::sd(x_valid) else NA_real_),
                        median = clean_num(if (length(x_valid) > 0) stats::median(x_valid) else NA_real_),
                        min = clean_num(if (length(x_valid) > 0) min(x_valid) else NA_real_),
                        max = clean_num(if (length(x_valid) > 0) max(x_valid) else NA_real_),
                        missing = as.integer(sum(!is.finite(x_all)))
                    )
                )
            }

            # Fill paired difference summary table.
            # ES: Llenar la tabla de resumen de diferencias pareadas.
            if (length(measures) >= 2) {
                diff_id <- 1

                for (i in seq_len(length(measures) - 1)) {
                    for (j in seq.int(i + 1, length(measures))) {
                        v1 <- measures[[i]]
                        v2 <- measures[[j]]

                        measure1Values <- suppressWarnings(as.numeric(dat[[v1]]))
                        measure2Values <- suppressWarnings(as.numeric(dat[[v2]]))
                        ok_pair <- is.finite(measure1Values) & is.finite(measure2Values)
                        d_pair <- measure2Values[ok_pair] - measure1Values[ok_pair]
                        d_pair <- d_pair[is.finite(d_pair)]

                        comp <- paste0(v2, " - ", v1)
                        all_diffs[[comp]] <- d_pair

                        add_table_row(
                            self$results$differenceSummary,
                            paste0("diff_", diff_id),
                            list(
                                comparison = comp,
                                n = as.integer(length(d_pair)),
                                meanDiff = clean_num(if (length(d_pair) > 0) mean(d_pair) else NA_real_),
                                sdDiff = clean_num(if (length(d_pair) > 1) stats::sd(d_pair) else NA_real_),
                                medianDiff = clean_num(if (length(d_pair) > 0) stats::median(d_pair) else NA_real_),
                                minDiff = clean_num(if (length(d_pair) > 0) min(d_pair) else NA_real_),
                                maxDiff = clean_num(if (length(d_pair) > 0) max(d_pair) else NA_real_)
                            )
                        )

                        diff_id <- diff_id + 1
                    }
                }
            }

            if (isTRUE(self$options$showDifferencePlots)) {
                diff_state_rows <- list()
                diff_state_id <- 1L

                for (nm in names(all_diffs)) {
                    d <- all_diffs[[nm]]
                    d <- d[is.finite(d)]

                    if (length(d) > 0) {
                        n_d <- length(d)
                        m_d <- mean(d)
                        sd_d <- if (n_d > 1) stats::sd(d) else NA_real_
                        mdn_d <- stats::median(d)
                        q1_d <- stats::quantile(d, .25, na.rm = TRUE, names = FALSE)
                        q3_d <- stats::quantile(d, .75, na.rm = TRUE, names = FALSE)
                        min_d <- min(d)
                        max_d <- max(d)

                        fmt <- function(z) {
                            if (is.finite(z))
                                sprintf("%.2f", z)
                            else
                                "NA"
                        }

                        summary_label <- paste0(
                            "M = ", fmt(m_d), "\n",
                            "SD = ", fmt(sd_d), "\n",
                            "Mdn = ", fmt(mdn_d), "\n",
                            "n = ", n_d
                        )

                        obs_rows <- data.frame(
                            rowType = "observation",
                            comparison = nm,
                            case = seq_len(n_d),
                            difference = as.numeric(d),
                            n = NA_integer_,
                            mean = NA_real_,
                            sd = NA_real_,
                            median = NA_real_,
                            q1 = NA_real_,
                            q3 = NA_real_,
                            min = NA_real_,
                            max = NA_real_,
                            label = NA_character_,
                            stringsAsFactors = FALSE
                        )

                        summary_row <- data.frame(
                            rowType = "summary",
                            comparison = nm,
                            case = NA_integer_,
                            difference = NA_real_,
                            n = n_d,
                            mean = m_d,
                            sd = sd_d,
                            median = mdn_d,
                            q1 = q1_d,
                            q3 = q3_d,
                            min = min_d,
                            max = max_d,
                            label = summary_label,
                            stringsAsFactors = FALSE
                        )

                        diff_state_rows[[diff_state_id]] <- obs_rows
                        diff_state_id <- diff_state_id + 1L
                        diff_state_rows[[diff_state_id]] <- summary_row
                        diff_state_id <- diff_state_id + 1L
                    }
                }

                if (length(diff_state_rows) > 0)
                    self$results$differencePlot$setState(do.call(rbind, diff_state_rows))
                else
                    self$results$differencePlot$setState(data.frame())
            }

            if (isTRUE(self$options$showProfilePlot)) {
                dat_profile <- dat[stats::complete.cases(dat), , drop = FALSE]

                if (nrow(dat_profile) == 0) {
                    self$results$profilePlot$setState(data.frame(reportLang = character()))
                } else {
                    case_id <- seq_len(nrow(dat_profile))

                    obs_rows <- lapply(seq_along(measures), function(i) {
                        data.frame(
                            rowType = "observation",
                            case = case_id,
                            measure = measures[i],
                            value = suppressWarnings(as.numeric(dat_profile[[i]])),
                            n = NA_integer_,
                            mean = NA_real_,
                            sd = NA_real_,
                            median = NA_real_,
                            ci_low = NA_real_,
                            ci_high = NA_real_,
                            label = NA_character_,
                            stringsAsFactors = FALSE
                        )
                    })

                    summary_rows <- lapply(seq_along(measures), function(i) {
                        nm <- measures[i]
                        x <- suppressWarnings(as.numeric(dat_profile[[i]]))
                        x <- x[is.finite(x)]
                        n_v <- length(x)
                        m_v <- if (n_v > 0) mean(x) else NA_real_
                        sd_v <- if (n_v > 1) stats::sd(x) else NA_real_
                        mdn_v <- if (n_v > 0) stats::median(x) else NA_real_
                        se_v <- if (n_v > 1 && is.finite(sd_v)) sd_v / sqrt(n_v) else NA_real_
                        tcrit <- if (n_v > 1) stats::qt(.975, df = n_v - 1) else NA_real_
                        ci_v <- if (is.finite(se_v) && is.finite(tcrit)) tcrit * se_v else NA_real_

                        fmt <- function(z) {
                            if (is.finite(z))
                                sprintf("%.2f", z)
                            else
                                "NA"
                        }

                        data.frame(
                            rowType = "summary",
                            case = NA_integer_,
                            measure = nm,
                            value = NA_real_,
                            n = n_v,
                            mean = m_v,
                            sd = sd_v,
                            median = mdn_v,
                            ci_low = if (is.finite(ci_v)) m_v - ci_v else NA_real_,
                            ci_high = if (is.finite(ci_v)) m_v + ci_v else NA_real_,
                            label = paste0(
                                "M = ", fmt(m_v), "\n",
                                "SD = ", fmt(sd_v), "\n",
                                "Mdn = ", fmt(mdn_v), "\n",
                                "n = ", n_v
                            ),
                            stringsAsFactors = FALSE
                        )
                    })

                    profile_state <- rbind(
                        do.call(rbind, obs_rows),
                        do.call(rbind, summary_rows)
                    )

                    self$results$profilePlot$setState(profile_state)
                }
            }

            self$results$differenceGuide$setContent(html_block(
            tr("Brief guide", "Guía breve"),
            .al_html_list(c(
                tr("In related groups, changes within the same unit are analyzed.", "En grupos relacionados se analizan cambios dentro de la misma unidad."),
                tr("With two measurements, the key variable is the paired difference.", "Con dos mediciones, la variable clave es la diferencia pareada."),
                tr("A positive mean indicates an increase; a negative mean indicates a decrease.", "Una media positiva indica aumento; una media negativa indica disminución."),
                tr("Review normality, symmetry, and outliers in the differences.", "Revise normalidad, simetría y casos atípicos de las diferencias.")
            )),
            raw = TRUE
        ))

            # Fill normality table for paired differences.
            # ES: Llenar la tabla de normalidad de diferencias pareadas.
            if (isTRUE(self$options$showNormalityPlots)) {
            normality_plot_state_rows <- list()
            normality_plot_state_id <- 1L

            for (nm in names(all_diffs)) {
                d_plot <- all_diffs[[nm]]
                d_plot <- d_plot[is.finite(d_plot)]
                n_plot <- length(d_plot)
                sd_plot <- if (n_plot > 1) stats::sd(d_plot) else NA_real_

                if (n_plot >= 3 && is.finite(sd_plot) && sd_plot > 0) {
                    normality_plot_state_rows[[normality_plot_state_id]] <- data.frame(
                        comparison = nm,
                        value = as.numeric(d_plot),
                        n = as.integer(n_plot),
                        mean = as.numeric(mean(d_plot, na.rm = TRUE)),
                        sd = as.numeric(sd_plot),
                        stringsAsFactors = FALSE
                    )
                    normality_plot_state_id <- normality_plot_state_id + 1L
                }
            }

            if (length(normality_plot_state_rows) > 0) {
                normality_plot_state <- do.call(rbind, normality_plot_state_rows)
            } else {
                normality_plot_state <- data.frame(
                    comparison = character(),
                    value = numeric(),
                    n = integer(),
                    mean = numeric(),
                    sd = numeric(),
                    stringsAsFactors = FALSE
                )
            }

            normality_plot_state$reportLang <- lang
            self$results$normalityPlot$setState(normality_plot_state)
            self$results$normalCurvePlot$setState(normality_plot_state)
        }

        normality_texts <- character()
        normality_records <- data.frame(scope = character(), significant = logical(), n = integer(), stringsAsFactors = FALSE)

            normality_result_text <- function(scope, test, p_value, n) {
                p_value <- clean_num(p_value)

                if (is.na(p_value)) {
                    return(paste0(
                        scope, " | ", test, ": no calculado."
                    ))
                }

                if (p_value < 0.05) {
                    return(paste0(
                        scope, " | ", test, ": p = ", format_p(p_value),
                        ", n = ", n,
                        tr(". Significant deviation from normality.", ". Desviación significativa de normalidad.")
                    ))
                }

                paste0(
                    scope, " | ", test, ": p = ", format_p(p_value),
                    ", n = ", n,
                    tr(". Compatible with approximate normality.", ". Compatible con normalidad aproximada.")
                )
            }


        add_norm_row <- function(scope, test, statistic, value, p_value, n) {
            if (is.finite(p_value)) {
                normality_records <<- rbind(
                    normality_records,
                    data.frame(
                        scope = scope,
                        significant = p_value < 0.05,
                        n = as.integer(n),
                        stringsAsFactors = FALSE
                    )
                )

                if (p_value < 0.05)
                    normality_problem <<- TRUE
            }

            add_table_row(
                self$results$normality,
                paste0("norm_", norm_id),
                list(
                    scope = scope,
                    test = test,
                    statistic = statistic,
                    value = clean_num(value),
                    p = clean_num(p_value),
                    pSig = sig_code(p_value)
                )
            )

            norm_id <<- norm_id + 1
        }

            if (length(all_diffs) > 0) {
                norm_id <- 1

                for (nm in names(all_diffs)) {
                    d_norm <- all_diffs[[nm]]
                    d_norm <- d_norm[is.finite(d_norm)]
                    n_norm <- length(d_norm)
                    sd_norm <- if (n_norm > 1) stats::sd(d_norm) else NA_real_

                    if (n_norm < 3 || !is.finite(sd_norm) || sd_norm <= 0) {
                        add_norm_row(
                            nm,
                            tr("Normality", "Normalidad"),
                            tr("Not calculated", "No calculado"),
                            NA_real_,
                            NA_real_,
                            n_norm
                        )

                        normality_texts <- c(
                            normality_texts,
                            paste0(
                                nm,
                                ": pruebas no calculadas; se requieren al menos 3 diferencias válidas ",
                                "y variabilidad mayor que cero."
                            )
                        )

                        next
                    }

                    # Shapiro-Wilk / Jarque-Bera / skewness / kurtosis: guard
                    # unified suite-wide (n>=3,<=5000 & sd>0 for SW; n>=8 &
                    # sd>0 for JB/skewness; n>=20 & sd>0 for kurtosis
                    # specifically) per Archie's decision, Aug 2026 - see
                    # .al_norm_core_battery(). relatedCheck's own
                    # jb_test_approx()/skew_test_approx()/kurt_test_approx()
                    # already used exactly this guard split; this just
                    # routes the same math through the shared function.
                    # ES: guarda unificada - relatedCheck ya usaba este
                    # mismo corte de umbrales; esto solo enruta la misma
                    # matemática por la función compartida.
                    .nc_d <- .al_norm_core_battery(d_norm)
                    sw <- .nc_d$sw

                    if (!is.null(sw)) {
                        add_norm_row(
                            nm,
                            "Shapiro-Wilk",
                            "W",
                            unname(sw$statistic),
                            sw$p.value,
                            n_norm
                        )
                    }

                    # Lilliefors / Anderson-Darling / Cramer-von Mises / Shapiro-Francia /
                    # Pearson chi-square: identical tryCatch calls in every module,
                    # consolidated in shared-helpers.R (.al_nortest_battery).
                    # ES: idénticas en todos los módulos, consolidadas en shared-helpers.R.
                    .nt_d <- .al_nortest_battery(d_norm)
                    li <- .nt_d$li; ad <- .nt_d$ad; cvm <- .nt_d$cvm; sf <- .nt_d$sf; pt <- .nt_d$pt

                    if (!is.null(li)) {
                        add_norm_row(
                            nm,
                            tr("Lilliefors (corrected K-S)", "Lilliefors (K-S corregido)"),
                            "D",
                            unname(li$statistic),
                            li$p.value,
                            n_norm
                        )
                    }

                    if (!is.null(ad)) {
                        add_norm_row(
                            nm,
                            "Anderson-Darling",
                            "A²",
                            unname(ad$statistic),
                            ad$p.value,
                            n_norm
                        )
                    }

                    if (!is.null(cvm)) {
                        add_norm_row(
                            nm,
                            "Cramer-von Mises",
                            "W²",
                            unname(cvm$statistic),
                            cvm$p.value,
                            n_norm
                        )
                    }

                    if (!is.null(sf)) {
                        add_norm_row(
                            nm,
                            "Shapiro-Francia",
                            "W'",
                            unname(sf$statistic),
                            sf$p.value,
                            n_norm
                        )
                    }

                    if (!is.null(pt)) {
                        add_norm_row(
                            nm,
                            tr("Pearson chi-square", "Pearson chi-cuadrado"),
                            "P",
                            unname(pt$statistic),
                            pt$p.value,
                            n_norm
                        )
                    }

                    jb <- .nc_d$jb

                    if (!is.null(jb)) {
                        add_norm_row(
                            nm,
                            "Jarque-Bera",
                            "JB",
                            jb$value,
                            jb$p,
                            n_norm
                        )
                    }

                    sk <- .nc_d$skew

                    if (!is.null(sk)) {
                        add_norm_row(
                            nm,
                            tr("Skewness test", "Prueba de asimetría"),
                            "z",
                            sk$value,
                            sk$p,
                            n_norm
                        )
                    }

                    ku <- .nc_d$kurt

                    if (!is.null(ku)) {
                        add_norm_row(
                            nm,
                            tr("Kurtosis test", "Prueba de curtosis"),
                            "z",
                            ku$value,
                            ku$p,
                            n_norm
                        )
                    }
                }
            }

            self$results$normalityGuide$setContent(html_block(
                tr("Brief guide", "Guía breve"),
                .al_html_list(c(
                    tr("With two measurements, normality is evaluated in the paired differences.", "Con dos mediciones, la normalidad se evalúa en las diferencias pareadas."),
                    tr("With three or more measurements, paired differences and within-subject residuals are reviewed.", "Con tres o más mediciones, se revisan diferencias pareadas y residuos intra-sujeto."),
                    tr("Shapiro-Wilk is sensitive in small and moderate samples.", "Shapiro-Wilk es sensible en muestras pequeñas y moderadas."),
                    tr("KS and Anderson-Darling compare the empirical shape with a theoretical normal distribution.", "KS y Anderson-Darling comparan la forma empírica con una normal teórica."),
                    tr("Jarque-Bera, skewness, and kurtosis help identify the type of deviation.", "Jarque-Bera, asimetría y curtosis ayudan a identificar el tipo de desviación."),
                    tr("p >= .05 is compatible with approximate normality.", "p >= .05 es compatible con normalidad aproximada."),
                    tr("p < .05 suggests a significant deviation from normality.", "p < .05 sugiere desviación significativa de normalidad.")
                )),
                raw = TRUE
            ))

            normality_summary_texts <- character()

            if (NROW(normality_records) > 0) {
                norm_sum_i <- 1L

                for (nm in unique(normality_records$scope)) {
                    rr <- normality_records[normality_records$scope == nm, , drop = FALSE]
                    total <- NROW(rr)
                    significant <- sum(rr$significant, na.rm = TRUE)

                    if (total == 0) {
                        interpretation <- tr("Not calculated", "No calculado")
                        norm_recommendation <- tr("Review valid data and variability before deciding.", "Revise datos válidos y variabilidad antes de decidir.")
                    } else if (significant == 0) {
                        interpretation <- tr("Compatible with approximate normality", "Compatible con normalidad aproximada")
                        norm_recommendation <- tr("The overall evidence does not suggest significant deviation.", "La evidencia global no sugiere desviación significativa.")
                    } else {
                        interpretation <- paste0(
                            significant,
                            " de ",
                            total,
                            tr(" tests suggest significant deviation", " pruebas sugieren desviación significativa")
                        )
                        norm_recommendation <- tr("Review the Q-Q plot, outliers, and robustness of the decision.", "Revise gráfico Q-Q, casos atípicos y robustez de la decisión.")
                    }

                    add_table_row(
                        self$results$normalitySummary,
                        paste0("normsum_", norm_sum_i),
                        list(
                            comparison = nm,
                            tests = as.integer(total),
                            significant = as.integer(significant),
                            interpretation = interpretation
                        )
                    )

                    normality_summary_texts <- c(
                        normality_summary_texts,
                        paste0(nm, ": ", interpretation, ". ", norm_recommendation)
                    )

                    norm_sum_i <- norm_sum_i + 1L
                }
            }

            self$results$normalityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                block96(
                    tr("The previous table summarizes the normality battery by comparison.", "La tabla anterior sintetiza la batería de normalidad por comparación."),
                    tr("Use the detailed table to identify which specific test activated the signal.", "Use la tabla detallada para identificar qué prueba específica activó la señal."),
                    tr("Use Q-Q plots and outlier review before changing the methodological decision.", "Use gráficos Q-Q y revisión de casos atípicos antes de cambiar la decisión metodológica."),
                    tr(
                        "This battery treats Shapiro-Wilk as the most trustworthy test in the group: it retains the highest statistical power among common normality tests across nearly the full range of sample sizes (Razali & Wah, 2011). A widespread recommendation to prefer Kolmogorov-Smirnov once n grows is a leftover from old software limitations, not a statistical reason.",
                        "Esta batería trata a Shapiro-Wilk como la prueba más confiable del grupo: mantiene el mayor poder estadístico entre las pruebas de normalidad más comunes para prácticamente todo rango de tamaño muestral (Razali & Wah, 2011). La recomendación extendida de preferir Kolmogorov-Smirnov cuando n crece es un remanente de limitaciones de software antiguo, no una razón estadística."
                    ),
                    tr(
                        "Common error: treating a single significant test out of nine as proof that the differences are not usable. A better reading is: how many of the nine tests agree, and does the Q-Q plot show a mild or severe departure.",
                        "Error común: interpretar una sola prueba significativa de las nueve como prueba de que las diferencias no son utilizables. Una mejor lectura es: cuántas de las nueve pruebas coinciden, y si el gráfico Q-Q muestra una desviación leve o severa."
                    )
                )
            ))

        if (length(normality_summary_texts) > 0) {
            self$results$normalityRecommendation$setContent(html_block(
                tr("Recommendation", "Recomendación"),
                paste(normality_summary_texts, collapse = "
")
            ))
        } else {
            self$results$normalityRecommendation$setContent("")
        }

        sym_i <- 1

            add_sym <- function(comparison, statistic, value, p_value) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05)
                    symmetry_problem <<- TRUE

                add_table_row(
                    self$results$symmetry,
                    paste0("sym_", sym_i),
                    list(
                        comparison = comparison,
                        statistic = statistic,
                        value = clean_num(value),
                        p = clean_num(p_value),
                        pSig = p_sig(p_value)
                    )
                )

                symmetry_texts <<- c(
                    symmetry_texts,
                    interpret_symmetry(comparison, p_value, length(d))
                )

                sym_i <<- sym_i + 1
            }

            for (nm in names(all_diffs)) {
                d <- all_diffs[[nm]]
                d <- d[!is.na(d)]

                sk <- tryCatch({
                    m <- mean(d)
                    ss <- stats::sd(d)
                    skew <- mean((d - m)^3) / ss^3
                    z <- skew / sqrt(6 / length(d))
                    p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
                    list(value = z, p = p)
                }, error = function(e) NULL)

                if (!is.null(sk))
                    add_sym(nm, tr("Skewness z", "Asimetría z"), sk$value, sk$p)
                else
                    add_sym(nm, tr("Skewness z", "Asimetría z"), NA_real_, NA_real_)
            }

            self$results$symmetryGuide$setContent(html_block(
            tr("Brief guide", "Guía breve"),
            .al_html_list(c(
                tr("Symmetry of differences is important for the Wilcoxon signed-rank test.", "La simetría de diferencias es importante para Wilcoxon signed-rank."),
                tr("p >= .05 is compatible with approximate symmetry.", "p >= .05 es compatible con simetría aproximada."),
                tr("p < .05 suggests significant asymmetry in the differences.", "p < .05 sugiere asimetría significativa de las diferencias."),
                tr("If there is strong asymmetry, consider the sign test.", "Si hay asimetría fuerte, considere prueba de signos.")
            )),
            raw = TRUE
        ))

            self$results$symmetryInterpretation$setContent(html_block(
            tr("Applied Interpretation", "Interpretación Aplicada"),
            block96(paste(
                paste(symmetry_texts, collapse = "\n"),
                tr(
                    "Why it matters: symmetry of the differences is the key requirement behind the Wilcoxon signed-rank test, which assumes the distribution of differences is symmetric around the (unknown) median. If that assumption fails, the test can still run, but what it is really testing shifts from \"are medians different\" to a vaguer statement about distributions, making the result harder to interpret in plain terms.",
                    "Por qué importa: la simetría de las diferencias es el requisito clave detrás de la prueba de Wilcoxon signed-rank, que asume que la distribución de las diferencias es simétrica alrededor de la mediana (desconocida). Si ese supuesto falla, la prueba igual puede correr, pero lo que realmente evalúa cambia de \"¿son distintas las medianas?\" a una afirmación más vaga sobre las distribuciones, lo cual hace el resultado más difícil de interpretar en términos simples."
                ),
                tr(
                    "Common error: treating symmetry and normality as the same thing. This test only checks whether skewness is close to zero; a distribution can be symmetric and still not normal (e.g., too flat or too peaked), or slightly asymmetric yet close enough to normal for practical purposes.",
                    "Error común: tratar simetría y normalidad como lo mismo. Esta prueba solo revisa si la asimetría es cercana a cero; una distribución puede ser simétrica y aun así no ser normal (por ejemplo, demasiado plana o demasiado apuntada), o levemente asimétrica y aun así cercana a lo normal para fines prácticos."
                ),
                tr(
                    "Sample-size caveat: skewness-based tests have limited power with small samples (they may miss real asymmetry) and can flag trivial asymmetry as significant with very large samples.",
                    "Matiz de tamaño de muestra: las pruebas basadas en asimetría tienen poder limitado en muestras pequeñas (pueden no detectar asimetría real) y pueden marcar como significativa una asimetría trivial con muestras muy grandes."
                ),
                tr(
                    "If asymmetry is strong, prefer the sign test over Wilcoxon signed-rank, since the sign test does not require symmetry of the differences.",
                    "Si la asimetría es fuerte, prefiera la prueba de signos sobre Wilcoxon signed-rank, ya que la prueba de signos no requiere simetría de las diferencias."
                ),
                sep = "\n\n"
            ))
        ))

            sp_i <- 1
            mauchly_p_value <- NA_real_

            add_spher <- function(diagnostic, statistic, value, df, p_value) {
                if (!is.na(clean_num(p_value)) && clean_num(p_value) < .05)
                    sphericity_problem <<- TRUE

                add_table_row(
                    self$results$sphericity,
                    paste0("sph_", sp_i),
                    list(
                        diagnostic = diagnostic,
                        statistic = statistic,
                        value = clean_num(value),
                        df = ifelse(is.na(clean_num(df)), NA_integer_, as.integer(df)),
                        p = ifelse(is.na(clean_num(p_value)), tr("Not applicable", "No aplica"), format_p(p_value)),
                        pSig = p_sig(p_value)
                    )
                )

                sp_i <<- sp_i + 1
            }

            if (k < 3) {
                add_spher(tr("Not applicable", "No aplicable"), tr("Two measurements", "Dos mediciones"), NA_real_, NA_integer_, NA_real_)
            } else {
                sph <- tryCatch({
                    Y <- as.matrix(dat2[, measures, drop = FALSE])
                    S <- stats::cov(Y, use = "complete.obs")

                    H <- stats::contr.helmert(k)
                    C <- t(H)
                    S_c <- C %*% S %*% t(C)

                    p_dim <- k - 1
                    eig <- eigen(S_c, symmetric = TRUE)$values
                    eig <- pmax(eig, .Machine$double.eps)

                    W <- prod(eig) / ((mean(eig))^p_dim)

                    df <- (p_dim * (p_dim + 1) / 2) - 1
                    chi <- -(n_used - 1) * log(W)
                    p <- stats::pchisq(chi, df = df, lower.tail = FALSE)

                    gg <- (sum(eig)^2) / (p_dim * sum(eig^2))
                    hf <- ((n_used * p_dim * gg) - 2) /
                        (p_dim * (n_used - 1 - p_dim * gg))
                    hf <- min(1, max(gg, hf))
                    lb <- 1 / p_dim

                    list(W = W, chi = chi, df = df, p = p,
                         gg = gg, hf = hf, lb = lb)
                }, error = function(e) NULL)

                if (!is.null(sph)) {
                    mauchly_p_value <- sph$p
                    add_spher(tr("Approximate Mauchly", "Mauchly aproximado"), "W", sph$W, sph$df, sph$p)
                    add_spher("Greenhouse-Geisser epsilon", "epsilon",
                              sph$gg, NA_integer_, NA_real_)
                    add_spher("Huynh-Feldt epsilon", "epsilon",
                              sph$hf, NA_integer_, NA_real_)
                    add_spher("Lower-bound epsilon", "epsilon",
                              sph$lb, NA_integer_, NA_real_)
                } else {
                    add_spher(tr("Approximate Mauchly", "Mauchly aproximado"), "W", NA_real_, NA_integer_, NA_real_)
                    add_spher("Greenhouse-Geisser epsilon", "epsilon",
                              NA_real_, NA_integer_, NA_real_)
                    add_spher("Huynh-Feldt epsilon", "epsilon",
                              NA_real_, NA_integer_, NA_real_)
                    add_spher("Lower-bound epsilon", "epsilon",
                              NA_real_, NA_integer_, NA_real_)
                }
            }

            sphericity_text <- interpret_sphericity(mauchly_p_value, n_used, k)

            self$results$sphericityGuide$setContent(html_block(
            tr("Brief guide", "Guía breve"),
            .al_html_list(c(
                tr("Sphericity applies only with three or more related measurements.", "La esfericidad aplica solo con tres o más mediciones relacionadas."),
                tr("p >= .05 is compatible with approximate sphericity.", "p >= .05 es compatible con esfericidad aproximada."),
                tr("p < .05 suggests a significant deviation from sphericity.", "p < .05 sugiere desviación significativa de esfericidad."),
                tr("If it fails, consider GG/HF corrections, Friedman, or mixed models.", "Si falla, considere correcciones GG/HF, Friedman o modelos mixtos.")
            )),
            raw = TRUE
        ))

            self$results$sphericityInterpretation$setContent(html_block(
            tr("Applied Interpretation", "Interpretación Aplicada"),
            block96(paste(
                sphericity_text,
                tr(
                    "Why it matters: repeated-measures ANOVA assumes the variances of the differences between every pair of measurements are equal (sphericity). When this fails, the F-test's degrees of freedom are effectively smaller than assumed, so the uncorrected test rejects the null hypothesis more often than it should, even when there is no real effect.",
                    "Por qué importa: el ANOVA de medidas repetidas asume que las varianzas de las diferencias entre cada par de mediciones son iguales (esfericidad). Cuando esto falla, los grados de libertad efectivos de la prueba F son en realidad menores de lo que se asume, así que la prueba sin corrección rechaza la hipótesis nula con más frecuencia de la debida, incluso cuando no hay un efecto real."
                ),
                tr(
                    "Common error: assuming that a significant Mauchly test means repeated-measures ANOVA cannot be used at all. In practice, it means the uncorrected F-test is likely too liberal; Greenhouse-Geisser or Huynh-Feldt corrections adjust the degrees of freedom precisely to compensate for this.",
                    "Error común: asumir que un Mauchly significativo impide usar por completo el ANOVA de medidas repetidas. En la práctica, significa que la prueba F sin corrección probablemente es demasiado liberal; las correcciones Greenhouse-Geisser o Huynh-Feldt ajustan los grados de libertad precisamente para compensar esto."
                ),
                tr(
                    "Sample-size caveat: Mauchly's test is sensitive to non-normality and can be unreliable with small samples; with very large samples it may flag small, practically unimportant violations of sphericity as significant.",
                    "Matiz de tamaño de muestra: la prueba de Mauchly es sensible a la no normalidad y puede ser poco confiable con muestras pequeñas; con muestras muy grandes puede marcar como significativas violaciones de esfericidad pequeñas y poco relevantes en la práctica."
                ),
                tr(
                    "If sphericity is doubtful, use the Greenhouse-Geisser or Huynh-Feldt corrected p-values in the \"Sphericity and Corrections\" table, switch to Friedman's non-parametric test, or use a mixed model that does not require this assumption.",
                    "Si la esfericidad es dudosa, use los valores p corregidos por Greenhouse-Geisser o Huynh-Feldt de la tabla \"Esfericidad y correcciones\", cambie a la prueba no paramétrica de Friedman, o use un modelo mixto que no requiera este supuesto."
                ),
                sep = "\n\n"
            ))
        ))

            # Correlation matrices (Pearson and dCor, APA 7 format) + copent discordance.
            # ES: Matrices de correlación (Pearson y dCor, formato APA 7) + discordancia con copent.

            self$results$correlationMatrixGuide$setContent(html_block(
                tr("Quick guide", "Guía breve"),
                block96(paste(
                    tr(
                        "These two tables show the intercorrelation structure of the measured variables, each in APA 7 format (lower triangle, numbered variables). This is directly relevant to a related-samples design: strong or weak correlations among the measures affect the power and interpretation of within-subject comparisons.",
                        "Estas dos tablas muestran la estructura de intercorrelación de las variables medidas, cada una en formato APA 7 (triángulo inferior, variables numeradas). Esto es directamente relevante para un diseño de muestras relacionadas: correlaciones fuertes o débiles entre las mediciones afectan la potencia y la interpretación de las comparaciones intrasujeto."
                    ),
                    tr(
                        "The first reports conventional Pearson correlation (linear association only); the second reports distance correlation (dCor, Szekely et al., 2007), which detects linear and non-linear association alike.",
                        "La primera reporta la correlación de Pearson convencional (solo detecta asociación lineal); la segunda reporta la correlación de distancia (dCor, Székely et al., 2007), que detecta asociación lineal y no lineal por igual."
                    ),
                    sep = "\n\n"
                ))
            ))

            {
                # dcor_stat()/dcor_pvalue(): equivalent to every other
                # module's version (regCheck/anovaCheck/logCheck),
                # consolidated in shared-helpers.R (.al_dcor_stat/
                # .al_dcor_test), same pattern already used for copentTest
                # below. Fixed B=199/seed=20260704 preserved unchanged. The
                # shared version's denom guard (!is.finite(denom)) is
                # marginally more defensive than this module's previous
                # is.na(denom) check (also catches an infinite
                # denominator, not just NaN) - no change for any realistic
                # finite input.
                # ES: equivalentes a la versión de los demás módulos
                # (regCheck/anovaCheck/logCheck), consolidadas en
                # shared-helpers.R (.al_dcor_stat/.al_dcor_test), mismo
                # patrón ya usado para copentTest más abajo. B=199/
                # semilla=20260704 fijos, preservados sin cambio. La
                # guarda de denominador compartida (!is.finite(denom)) es
                # marginalmente más defensiva que la anterior is.na(denom)
                # de este módulo (también detecta un denominador
                # infinito, no solo NaN) - sin cambio para ninguna entrada
                # finita realista.
                dcor_stat <- .al_dcor_stat
                dcor_pvalue <- function(x, y) .al_dcor_test(x, y)$p

                # copentTest(): byte-identical in every module that has it,
                # consolidated in shared-helpers.R (.al_copent_test).
                # ES: idéntica en todos los módulos que la usan, consolidada
                # en shared-helpers.R.
                copentTest <- .al_copent_test

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

                matVars <- measures
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

                    self$results$correlationMatrixNote$setContent(html_block(NULL,
                        c(
                            paste0(sprintf("N = %d.", n_used), " ", tr("* p < .05, ** p < .01, *** p < .001", "* p < .05, ** p < .01, *** p < .001")),
                            .al_dcor_na_note(lang)
                        ),
                        paragraphs = FALSE
                    ))
                } else {
                    self$results$correlationMatrixNote$setContent(html_block(NULL,
                        tr("Not applicable: fewer than two measured variables.", "No aplica: menos de dos variables medidas."),
                        paragraphs = FALSE
                    ))
                }

                self$results$correlationComparisonGuide$setContent(html_block(
                    tr("Pearson / dCor / Copula Entropy Discordance Analysis", "Análisis de Discordancia Pearson / dCor / Entropía Copular"),
                    block96(paste(
                        tr(
                            "Because Pearson's r only captures linear association while dCor captures both linear and non-linear association, a pair whose dCor is notably larger than its Pearson |r| is a signal (not proof) of a non-linear relationship.",
                            "Dado que la r de Pearson solo capta asociación lineal mientras que dCor capta asociación lineal y no lineal por igual, un par cuyo dCor sea notablemente mayor que su |r| de Pearson es una señal (no una prueba) de una relación no lineal."
                        ),
                        tr(
                            "Pairs are flagged in the \"Pairs with a Notable Difference between Pearson and dCor\" table when the gap (dCor minus |Pearson r|) is greater than .10. The copula entropy (CE, copent()) result for the same pair is shown alongside as a second, distribution-free line of evidence.",
                            "Se señalan en la tabla \"Pares con diferencia notable entre Pearson y dCor\" los pares con una brecha (dCor menos |r| de Pearson) mayor a .10. El resultado de la prueba de entropía copular (CE, copent()) para el mismo par se muestra al lado como una segunda línea de evidencia libre de supuestos distribucionales."
                        ),
                        .al_permutation_note(lang, 199, 20260704),
                        sep = "\n\n"
                    ))
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
                            tr("Not applicable: fewer than two measured variables.", "No aplica: menos de dos variables medidas.")
                        else
                            tr(sprintf("No pair shows a Pearson/dCor gap greater than %.2f; there is no indication of unmodeled non-linear association among the measured variables.", gapThreshold),
                               sprintf("Ningún par muestra una brecha Pearson/dCor mayor a %.2f; no hay indicios de asociación no lineal no modelada entre las variables medidas.", gapThreshold)),
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
            }

            out_i <- 1
            outlier_counts_by_comparison <- c()

            for (nm in names(all_diffs)) {
                d <- all_diffs[[nm]]
                q1 <- stats::quantile(d, .25, na.rm = TRUE)
                q3 <- stats::quantile(d, .75, na.rm = TRUE)
                iqr <- q3 - q1
                low <- q1 - 1.5 * iqr
                high <- q3 + 1.5 * iqr

                idx <- which(!is.na(d) & (d < low | d > high))
                outlier_counts_by_comparison[nm] <- length(idx)

                if (length(idx) > 0)
                    outlier_problem <- TRUE

                for (ii in idx) {
                    add_table_row(
                        self$results$outliers,
                        paste0("out_", out_i),
                        list(
                            case = as.integer(which(complete)[ii]),
                            comparison = nm,
                            difference = clean_num(d[ii]),
                            criterion = paste0(tr("Outside [", "Fuera de ["), round(low, 4),
                                               ", ", round(high, 4), "]")
                        )
                    )

                    out_i <- out_i + 1
                }
            }

            if (out_i == 1) {
                add_table_row(
                    self$results$outliers,
                    "out_none",
                    list(
                        case = NA_integer_,
                        comparison = tr("No flagged cases", "Sin casos marcados"),
                        difference = NA_real_,
                        criterion = tr("No differences outside 1.5*IQR were detected", "No se detectaron diferencias fuera de 1.5*IQR")
                    )
                )
            }

            self$results$outliersGuide$setContent(html_block(
            tr("Brief guide", "Guía breve"),
            .al_html_list(c(
                tr("Outliers are reviewed on paired differences.", "Los casos atípicos se revisan sobre diferencias pareadas."),
                tr("The IQR rule is used: outside Q1 - 1.5*IQR or Q3 + 1.5*IQR.", "Se usa la regla IQR: fuera de Q1 - 1.5*IQR o Q3 + 1.5*IQR."),
                tr("A flagged case should not be removed automatically.", "Un caso marcado no debe eliminarse automáticamente."),
                tr("Review the value and justify any decision.", "Revise el dato y justifique cualquier decisión.")
            )),
            raw = TRUE
        ))

            n_comparisons_total <- length(all_diffs)
            n_outliers_total <- sum(outlier_counts_by_comparison, na.rm = TRUE)
            comparisons_with_outliers <- names(outlier_counts_by_comparison)[outlier_counts_by_comparison > 0]
            n_comparisons_with_outliers <- length(comparisons_with_outliers)

            outliers_finding_text <- if (n_outliers_total == 0) {
                tr(
                    paste0("Across ", n_comparisons_total, " paired comparison(s), the IQR rule flagged no outlier differences."),
                    paste0("En ", n_comparisons_total, " comparación(es) pareada(s), la regla IQR no marcó ninguna diferencia atípica.")
                )
            } else {
                comparisons_desc <- paste0(
                    comparisons_with_outliers, " (", outlier_counts_by_comparison[comparisons_with_outliers], ")",
                    collapse = ", "
                )
                tr(
                    paste0(
                        "Across ", n_comparisons_total, " paired comparison(s), the IQR rule flagged ", n_outliers_total,
                        " difference value(s) as outliers, concentrated in ", n_comparisons_with_outliers,
                        " comparison(s): ", comparisons_desc, "."
                    ),
                    paste0(
                        "En ", n_comparisons_total, " comparación(es) pareada(s), la regla IQR marcó ", n_outliers_total,
                        " valor(es) de diferencia como atípico(s), concentrados en ", n_comparisons_with_outliers,
                        " comparación(es): ", comparisons_desc, "."
                    )
                )
            }

            self$results$outliersInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                block96(paste(
                    outliers_finding_text,
                    tr(
                        "Why it matters: in a paired design, a case with an unusually large or small difference pulls the mean difference and can inflate its standard deviation, weakening a paired t-test. It also contributes disproportionately to the rank sum in Wilcoxon signed-rank, so its influence does not disappear simply by switching to a non-parametric test.",
                        "Por qué importa: en un diseño pareado, un caso con una diferencia inusualmente grande o pequeña desplaza la media de la diferencia y puede inflar su desviación estándar, debilitando una t pareada. También contribuye de forma desproporcionada a la suma de rangos en Wilcoxon signed-rank, así que su influencia no desaparece por el simple hecho de cambiar a una prueba no paramétrica."
                    ),
                    tr(
                        "Common error: automatically removing every flagged pair. First check whether the value reflects a data entry error, a genuine and substantively meaningful change for that person, or measurement noise, and compare the conclusion with and without the flagged case(s) before deciding.",
                        "Error común: eliminar automáticamente todos los pares marcados. Primero verifique si el valor refleja un error de registro, un cambio genuino y sustantivamente relevante para esa persona, o ruido de medición, y compare la conclusión con y sin el o los casos marcados antes de decidir."
                    ),
                    tr(
                        "Sample-size nuance: with a small number of pairs, the IQR rule's Q1/Q3 estimates are themselves imprecise, so a case can be flagged mainly because the quartiles are unstable rather than because the difference is truly extreme.",
                        "Matiz de tamaño de muestra: con un número pequeño de pares, las estimaciones de Q1/Q3 de la regla IQR son en sí mismas imprecisas, por lo que un caso puede quedar marcado principalmente porque los cuartiles son inestables, no porque la diferencia sea verdaderamente extrema."
                    ),
                    sep = "\n\n"
                ))
            ))

            self$results$methodGuide$setContent(html_block(
            tr("Brief guide", "Guía breve"),
            .al_html_list(c(
                tr("The suggested test depends on the number of measurements and assumptions.", "La prueba sugerida depende del número de mediciones y los supuestos."),
                tr("Two measurements: paired t-test, Wilcoxon signed-rank, or sign test.", "Dos mediciones: t pareada, Wilcoxon signed-rank o prueba de signos."),
                tr("Three or more measurements: RM-ANOVA, corrections, Friedman, or mixed model.", "Tres o más mediciones: RM-ANOVA, correcciones, Friedman o modelo mixto."),
                tr("The decision should combine evidence, design, and research question.", "La decisión debe combinar evidencia, diseño y pregunta de investigación.")
            )),
            raw = TRUE
        ))

            method_i <- 1
        method_when_texts <- character()

            add_method <- function(design, evidence, suggestion, when) {
                add_table_row(
                    self$results$methodOptions,
                    paste0("meth_", method_i),
                    list(
                        design = design,
                        evidence = evidence,
                        suggestion = suggestion
                    )
                )
                            method_when_texts <<- c(method_when_texts, paste0(suggestion, ": ", when))
method_i <<- method_i + 1
            }

            recommendation_parts <- character()

            if (k == 2) {

                if (!normality_problem && !outlier_problem) {
                    add_method(
                        tr("Two paired measurements", "Dos mediciones pareadas"),
                        tr("Approximately normal differences and no severe outliers.", "Diferencias aproximadamente normales y sin atípicos graves."),
                        tr("Paired t-test", "t pareada"),
                        tr("Use when the mean of the differences is the central parameter.", "Use cuando la media de las diferencias sea el parámetro central.")
                    )

                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("For two related measurements, the available evidence is compatible with a parametric paired comparison if the question focuses on the mean of the differences.", "Para dos mediciones relacionadas, la evidencia disponible es compatible con una comparación pareada paramétrica si la pregunta se centra en la media de las diferencias.")
                    )
                } else {
                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("For two related measurements, review the robustness of the comparison because there are signs of doubtful normality or outliers in the differences.", "Para dos mediciones relacionadas, revise la robustez de la comparación porque hay señales de normalidad dudosa o casos atípicos en las diferencias.")
                    )
                }

                add_method(
                    tr("Two paired measurements", "Dos mediciones pareadas"),
                    tr("Doubtful normality, small sample, or ordinal differences.", "Normalidad dudosa, muestra pequeña o diferencias ordinales."),
                    "Wilcoxon signed-rank",
                    tr("Use if the distribution of differences is approximately symmetric.", "Use si la distribución de diferencias es aproximadamente simétrica.")
                )

                add_method(
                    tr("Two paired measurements", "Dos mediciones pareadas"),
                    tr("Marked asymmetry or presence of relevant outliers.", "Asimetría marcada o presencia de atípicos relevantes."),
                    tr("Sign test", "Prueba de signos"),
                    tr("Use if you only want to evaluate the direction of change.", "Use si solo desea evaluar dirección del cambio.")
                )

                if (normality_problem) {
                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("The normality of the differences showed at least one significant signal; review the Q-Q plot and observed distribution before deciding.", "La normalidad de las diferencias mostró al menos una señal significativa; revise Q-Q plot y distribución observada antes de decidir.")
                    )
                }

                if (outlier_problem) {
                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("Outliers were detected in the differences; they should not be removed automatically, but they should be reviewed and documented.", "Se detectaron casos atípicos en diferencias; no deben eliminarse automáticamente, pero sí revisarse y documentarse.")
                    )
                }

            } else {

                if (!normality_problem && !sphericity_problem && !outlier_problem) {
                    add_method(
                        tr("Three or more measurements", "Tres o más mediciones"),
                        tr("Acceptable normality/residuals, compatible sphericity, and no relevant outliers.", "Normalidad/residuos aceptables, esfericidad compatible y sin atípicos relevantes."),
                        "ANOVA de medidas repetidas",
                        tr("Use when the design is complete and the measurements are comparable.", "Use cuando el diseño es completo y las mediciones son comparables.")
                    )

                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("For three or more related measurements, the overall evidence is compatible with repeated-measures ANOVA without special correction.", "Para tres o más mediciones relacionadas, la evidencia global es compatible con ANOVA de medidas repetidas sin corrección especial.")
                    )
                }

                if (sphericity_problem) {
                    add_method(
                        tr("Three or more measurements", "Tres o más mediciones"),
                        tr("Mauchly significant or doubtful sphericity.", "Mauchly significativo o esfericidad dudosa."),
                        tr("RM-ANOVA with GG/HF correction", "RM-ANOVA con corrección GG/HF"),
                        tr("Use Greenhouse-Geisser or Huynh-Feldt corrections if the parametric approach is retained.", "Use correcciones Greenhouse-Geisser o Huynh-Feldt si se mantiene el enfoque paramétrico.")
                    )

                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("The sphericity test was significant; interpreting repeated-measures ANOVA without correction is not recommended.", "La prueba de esfericidad fue significativa; no se recomienda interpretar un ANOVA de medidas repetidas sin corrección.")
                    )
                } else {
                    add_method(
                        tr("Three or more measurements", "Tres o más mediciones"),
                        tr("Compatible sphericity according to the available diagnostic evidence.", "Esfericidad compatible según el diagnóstico disponible."),
                        "ANOVA de medidas repetidas",
                        tr("Use if normality, outliers, and the substantive design are also acceptable.", "Use si también son aceptables normalidad, atípicos y el diseño sustantivo.")
                    )
                }

                if (normality_problem || outlier_problem) {
                    add_method(
                        tr("Three or more measurements", "Tres o más mediciones"),
                        tr("Doubtful normality, ordinal scale, outliers, or concern about robustness.", "Normalidad dudosa, escala ordinal, atípicos o preocupación por robustez."),
                        "Friedman",
                        tr("Use as a nonparametric alternative if robustness of assumptions is a central concern.", "Use como alternativa no paramétrica si la robustez de los supuestos es una preocupación central.")
                    )
                } else {
                    add_method(
                        tr("Three or more measurements", "Tres o más mediciones"),
                        tr("Main assumptions are reasonable, but a robust nonparametric alternative is desired.", "Supuestos principales razonables, pero se desea una alternativa robusta no paramétrica."),
                        "Friedman",
                        tr("Use if the scale, distribution, or substantive question favors ranks over means.", "Use si la escala, distribución o pregunta sustantiva favorece rangos sobre medias.")
                    )
                }

                add_method(
                    tr("Three or more measurements", "Tres o más mediciones"),
                    tr("Missing data, complex structure, covariates, or interest in random effects.", "Datos faltantes, estructura compleja, covariables o interés en efectos aleatorios."),
                    tr("Mixed model", "Modelo mixto"),
                    tr("Use when you need flexibility for incomplete cases, covariates, or complex longitudinal structure.", "Use cuando necesita flexibilidad para casos incompletos, covariables o estructura longitudinal compleja.")
                )

                if (normality_problem) {
                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("Normality showed at least one significant signal; use Q-Q plots, the observed curve, and outliers to evaluate robustness.", "La normalidad mostró al menos una señal significativa; use los Q-Q plots, la curva observada y los casos atípicos para valorar robustez.")
                    )
                }

                if (outlier_problem) {
                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("Outliers were detected in the differences; review those cases before supporting a definitive decision.", "Se detectaron casos atípicos en diferencias; revise esos casos antes de sostener una decisión definitiva.")
                    )
                }

                if (!normality_problem && !sphericity_problem && !outlier_problem) {
                    recommendation_parts <- c(
                        recommendation_parts,
                        tr("No relevant signals for normality, sphericity, or outliers were detected according to the applied criteria.", "No se detectaron señales relevantes de normalidad, esfericidad o atípicos según los criterios aplicados.")
                    )
                }
            }

            if (length(recommendation_parts) == 0) {
                recommendation <- tr("Interpret the results considering design, assumptions, sample size, and research question.", "Interprete los resultados considerando diseño, supuestos, tamaño muestral y pregunta de investigación.")
            } else {
                recommendation <- paste(recommendation_parts, collapse = " ")
            }

        if (length(method_when_texts) > 0) {
            self$results$methodWhen$setContent(html_block(
                NULL,
                .al_html_list(method_when_texts),
                raw = TRUE
            ))
        } else {
            self$results$methodWhen$setContent("")
        }

self$results$notes$setContent(html_block(
            tr("Notes and recommendation", "Notas y recomendación"),
            block96(
                paste0(tr("Suggested decision: ", "Decisión sugerida: "), recommendation),
                tr("Significance codes: * p < .05, ** p < .01, *** p < .001.", "Códigos de significancia: * p < .05, ** p < .01, *** p < .001.")
            )
        ))
        },

        .tr = function(en, es = NULL) {
        lang <- tryCatch(.al_normalize_lang(self$options$reportLang), error = function(e) "es")
        if (is.null(es)) es <- en
        if (identical(lang, "es")) es else en
    }, .emptyRelatedPlot = function(message) {
            graphics::plot.new()
            graphics::par(mar = c(1, 1, 1, 1))
            graphics::text(
                x = 0.5,
                y = 0.55,
                labels = message,
                cex = 0.95
            )
            graphics::text(
                x = 0.5,
                y = 0.40,
                labels = "AssumptionsLab",
                cex = 0.80
            )
        },

        .plotTr = function(en, es = NULL, image = NULL) {
            if (is.null(es))
                es <- en

            plot_lang <- tryCatch({
                st <- if (!is.null(image)) image$state else NULL
                if (!is.null(st) &&
                    is.data.frame(st) &&
                    "reportLang" %in% names(st) &&
                    NROW(st) > 0 &&
                    nzchar(as.character(st$reportLang[1]))) {
                    as.character(st$reportLang[1])
                } else {
                    as.character(self$options$reportLang)
                }
            }, error = function(e) "es")

            plot_lang <- tryCatch(.al_normalize_lang(plot_lang), error = function(e) "es")

            if (identical(plot_lang, "es"))
                es
            else
                en
        },

        # Delegates entirely to the shared .al_plot_palette_base()
        # (shared-helpers.R) — the same base style palette used by every
        # other module in the suite — so a given Plot Style ("Clean
        # Academic", "Black and White", etc.) looks identical regardless
        # of which module renders it. Previously this was a local
        # reimplementation with slightly different color values than the
        # shared one; consolidating removes that inconsistency and the
        # duplicated maintenance burden.
        # ES: Delega por completo en la función base compartida
        # .al_plot_palette_base() (shared-helpers.R) — la misma paleta de
        # estilo que usa el resto de la suite — para que un mismo Plot
        # Style ("Académico limpio", "Blanco y negro", etc.) se vea igual
        # sin importar qué módulo lo renderiza. Antes esto era una
        # reimplementación local con valores de color ligeramente
        # distintos a la compartida; consolidar elimina esa inconsistencia
        # y la carga de mantenimiento duplicada.
        .relatedPlotPalette = function() {
            style <- tryCatch(self$options$plotStyle, error = function(e) "clean")
            if (is.null(style) || length(style) == 0 || !nzchar(style))
                style <- "clean"

            .al_plot_palette_base(style)
        },

        # Categorical palette used to color measurement occasions (profile
        # plot summary points) and pairwise comparisons (paired-differences
        # plot panels). Delegates to the shared .al_plot_series_palette()
        # (shared-helpers.R), the same function logCheck/regCheck/
        # timeCheck/anovaCheck use for their plotPalette option, so
        # "Blue-Orange"/"Viridis"/"Grayscale"/"High Contrast" produce the
        # same colors here as everywhere else in the suite. That function
        # returns a fixed-length qualitative color set (it doesn't take a
        # category count); rep_len() cycles it to the number of
        # measurements/comparisons actually being plotted.
        # ES: Paleta categórica usada para colorear ocasiones de medición
        # (puntos de resumen del gráfico de perfil) y comparaciones
        # pareadas (paneles del gráfico de diferencias). Delega en la
        # función compartida .al_plot_series_palette() (shared-helpers.R),
        # la misma que usan logCheck/regCheck/timeCheck/anovaCheck para su
        # opción plotPalette, para que "Blue-Orange"/"Viridis"/"Grayscale"/
        # "High Contrast" den los mismos colores acá que en el resto de la
        # suite. Esa función devuelve un set de colores cualitativo de
        # largo fijo (no recibe cantidad de categorías); rep_len() lo cicla
        # a la cantidad real de mediciones/comparaciones que se grafican.
        .categoricalPalette = function(n) {
            key <- tryCatch(self$options$plotPalette, error = function(e) "blueOrange")
            if (is.null(key) || length(key) == 0 || !nzchar(key))
                key <- "blueOrange"

            rep_len(.al_plot_series_palette(key), n)
        },

        .plotProfile = function(image, ...) {
            if (!isTRUE(self$options$showProfilePlot))
                return()

            self$results$profilePlot$setVisible(TRUE)

            st <- image$state

        plot_tr <- function(en, es = NULL) private$.plotTr(en, es, if (exists("image", inherits = FALSE)) image else NULL)

            if (is.null(st) || NROW(st) == 0) {
                private$.emptyRelatedPlot(
                    paste(
                        plot_tr("No complete cases are available for the selected measurements.", "No hay casos completos para las mediciones seleccionadas."),
                        plot_tr("The profile plot cannot be built.", "No es posible construir el gráfico de perfil."),
                        sep = "\n"
                    )
                )
                return()
            }

            needed <- c(
                "rowType", "case", "measure", "value", "n", "mean",
                "sd", "median", "ci_low", "ci_high", "label"
            )

            if (!all(needed %in% names(st))) {
                private$.emptyRelatedPlot(
                    paste(
                        plot_tr("The plot internal state does not contain the expected columns.", "El estado interno del gráfico no contiene las columnas esperadas."),
                        plot_tr("Run the analysis again or check the Image result definition.", "Vuelva a ejecutar el análisis o revise la definición del resultado Image."),
                        sep = "\n"
                    )
                )
                return()
            }

            st$rowType <- as.character(st$rowType)
            st$measure <- as.character(st$measure)

            measures <- unique(st$measure[st$rowType == "summary"])

            if (length(measures) < 2)
                measures <- unique(st$measure)

            if (length(measures) < 2) {
                private$.emptyRelatedPlot(
                    paste(
                        plot_tr("Select at least two related measurements.", "Seleccione al menos dos mediciones relacionadas."),
                        plot_tr("The plot summarizes central tendency, dispersion, and change.", "El gráfico resume tendencia central, dispersión y cambio."),
                        sep = "\n"
                    )
                )
                return()
            }

            long_df <- st[st$rowType == "observation", , drop = FALSE]
            stats_df <- st[st$rowType == "summary", , drop = FALSE]

            num_cols <- c("case", "value", "n", "mean", "sd", "median", "ci_low", "ci_high")
            for (cc in intersect(num_cols, names(long_df)))
                long_df[[cc]] <- suppressWarnings(as.numeric(long_df[[cc]]))
            for (cc in intersect(num_cols, names(stats_df)))
                stats_df[[cc]] <- suppressWarnings(as.numeric(stats_df[[cc]]))

            long_df <- long_df[is.finite(long_df$value), , drop = FALSE]
            stats_df <- stats_df[is.finite(stats_df$mean), , drop = FALSE]

            long_df$measure <- factor(long_df$measure, levels = measures)
            stats_df$measure <- factor(stats_df$measure, levels = measures)

            if (NROW(long_df) == 0 || NROW(stats_df) == 0) {
                private$.emptyRelatedPlot(
                    paste(
                        plot_tr("No finite values were obtained to build the plot.", "No se obtuvieron valores finitos para construir el gráfico."),
                        plot_tr("Review the selected measurements.", "Revise las mediciones seleccionadas."),
                        sep = "\n"
                    )
                )
                return()
            }

            pal <- private$.relatedPlotPalette()
            cat_pal <- private$.categoricalPalette(length(measures))
            names(cat_pal) <- as.character(measures)

            case_id <- unique(long_df$case[is.finite(long_df$case)])
            n_cases <- length(case_id)

            max_cases_to_draw <- 120
            if (n_cases > max_cases_to_draw) {
                set.seed(1234)
                keep_cases <- sort(sample(case_id, max_cases_to_draw))
                traj_note <- paste0(
                    plot_tr("Showing ", "Se muestran "), max_cases_to_draw, plot_tr(" individual trajectories from ", " trayectorias individuales de "),
                    n_cases, plot_tr(" cases to preserve readability.", " casos para mantener legibilidad.")
                )
            } else {
                keep_cases <- case_id
                traj_note <- paste0(
                    plot_tr("Individual trajectories are shown for the ", "Se muestran trayectorias individuales de los "), n_cases, plot_tr(" complete cases.", " casos completos.")
                )
            }

            long_draw <- long_df[long_df$case %in% keep_cases, , drop = FALSE]

            ranges <- vapply(measures, function(mm) {
                x <- long_df$value[as.character(long_df$measure) == mm]
                x <- x[is.finite(x)]
                if (length(x) == 0)
                    return(NA_real_)
                diff(range(x))
            }, numeric(1))

            finite_ranges <- ranges[is.finite(ranges) & ranges > 0]
            scale_note <- ""

            if (length(finite_ranges) >= 2) {
                ratio_ranges <- max(finite_ranges) / min(finite_ranges)
                if (is.finite(ratio_ranges) && ratio_ranges >= 4) {
                    scale_note <- paste(
                        plot_tr("Warning: the measurements appear to be on very different scales or ranges;", "Advertencia: las mediciones parecen estar en escalas o rangos muy distintos;"),
                        plot_tr("compare the profile on the original scale with caution.", "compare el perfil en escala original con cautela.")
                    )
                }
            }

            caption_parts <- c(
                plot_tr("Large points = mean; bars = 95% CI; diamond = median.", "Puntos grandes = media; barras = IC 95%; rombo = mediana."),
                traj_note
            )

            if (nzchar(scale_note))
                caption_parts <- c(caption_parts, scale_note)

            caption_text <- paste(caption_parts, collapse = "\n")

            y_all <- c(
                long_df$value,
                stats_df$ci_low,
                stats_df$ci_high,
                stats_df$mean,
                stats_df$median
            )
            y_all <- y_all[is.finite(y_all)]

            if (length(y_all) == 0) {
                private$.emptyRelatedPlot(
                    paste(
                        plot_tr("No finite values were obtained to build the plot.", "No se obtuvieron valores finitos para construir el gráfico."),
                        plot_tr("Review the selected measurements.", "Revise las mediciones seleccionadas."),
                        sep = "\n"
                    )
                )
                return()
            }

            y_span <- diff(range(y_all))
            if (!is.finite(y_span) || y_span <= 0)
                y_span <- max(abs(y_all), na.rm = TRUE) * 0.15 + 1

            stats_df$label_y <- ifelse(
                is.finite(stats_df$ci_high),
                stats_df$ci_high + 0.06 * y_span,
                stats_df$mean + 0.06 * y_span
            )

            p <- ggplot2::ggplot() +
                ggplot2::geom_line(
                    data = long_draw,
                    mapping = ggplot2::aes(x = measure, y = value, group = case),
                    color = pal$ref,
                    alpha = 0.12,
                    linewidth = 0.35
                ) +
                ggplot2::geom_point(
                    data = long_draw,
                    mapping = ggplot2::aes(x = measure, y = value),
                    color = pal$ref,
                    alpha = 0.12,
                    size = 1.1
                ) +
                ggplot2::geom_errorbar(
                    data = stats_df,
                    mapping = ggplot2::aes(x = measure, ymin = ci_low, ymax = ci_high),
                    width = 0.08,
                    linewidth = 0.75,
                    color = pal$line,
                    na.rm = TRUE
                ) +
                ggplot2::geom_line(
                    data = stats_df,
                    mapping = ggplot2::aes(x = measure, y = mean, group = 1),
                    color = pal$line,
                    linewidth = 1.05
                ) +
                ggplot2::geom_point(
                    data = stats_df,
                    mapping = ggplot2::aes(x = measure, y = mean, fill = measure),
                    shape = 21,
                    size = 3.8,
                    stroke = 1.0,
                    color = pal$line,
                    show.legend = FALSE
                ) +
                ggplot2::scale_fill_manual(values = cat_pal) +
                ggplot2::geom_point(
                    data = stats_df,
                    mapping = ggplot2::aes(x = measure, y = median),
                    shape = 23,
                    size = 2.8,
                    stroke = 0.9,
                    fill = "#FFFFFF",
                    color = pal$point
                ) +
                ggplot2::geom_label(
                    data = stats_df,
                    mapping = ggplot2::aes(x = measure, y = label_y, label = label),
                    vjust = 0,
                    size = 2.9,
                    lineheight = 1.00,
                    label.size = 0.20,
                    fill = "#FFFFFF",
                    color = pal$point
                ) +
                ggplot2::labs(
                    title = plot_tr("Profile of related measurements", "Perfil de mediciones relacionadas"),
                    subtitle = plot_tr("Pedagogical summary of central tendency, dispersion, and change across measurements", "Resumen pedagógico de tendencia central, dispersión y cambio entre mediciones"),
                    x = plot_tr("Measurement", "Medición"),
                    y = plot_tr("Observed value", "Valor observado"),
                    caption = caption_text
                ) +
                ggplot2::scale_y_continuous(
                    expand = ggplot2::expansion(mult = c(0.06, 0.40))
                ) +
                ggplot2::coord_cartesian(clip = "off") +
                ggplot2::theme_minimal(base_size = 11) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0),
                    plot.subtitle = ggplot2::element_text(size = 10),
                    axis.title.x = ggplot2::element_text(face = "bold"),
                    axis.title.y = ggplot2::element_text(face = "bold"),
                    axis.text.x = ggplot2::element_text(face = "bold"),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_blank(),
                    panel.grid.major.y = ggplot2::element_line(color = pal$grid),
                    plot.caption = ggplot2::element_text(hjust = 0, size = 9),
                    plot.margin = ggplot2::margin(12, 30, 16, 12)
                )

            print(p)
        },

        .plotDifferences = function(image, ...) {
            if (!isTRUE(self$options$showDifferencePlots))
                return()

            self$results$differencePlot$setVisible(TRUE)

            st <- image$state

        plot_tr <- function(en, es = NULL) private$.plotTr(en, es, if (exists("image", inherits = FALSE)) image else NULL)

            if (is.null(st) || NROW(st) == 0) {
                private$.emptyRelatedPlot(
                    paste(
                        plot_tr("No valid paired differences are available to plot.", "No hay diferencias pareadas válidas para graficar."),
                        plot_tr("Check that at least two numeric measurements with valid data exist.", "Revise que existan al menos dos mediciones numéricas con datos válidos."),
                        sep = "\n"
                    )
                )
                return()
            }

            needed <- c(
                "rowType", "comparison", "case", "difference", "n",
                "mean", "sd", "median", "q1", "q3", "min", "max", "label"
            )

            if (!all(needed %in% names(st))) {
                private$.emptyRelatedPlot(
                    paste(
                        plot_tr("The internal state of the difference plot does not contain the expected columns.", "El estado interno del gráfico de diferencias no contiene las columnas esperadas."),
                        plot_tr("Run the analysis again or check the Image result definition.", "Vuelva a ejecutar el análisis o revise la definición del resultado Image."),
                        sep = "\n"
                    )
                )
                return()
            }

            st$rowType <- as.character(st$rowType)
            st$comparison <- as.character(st$comparison)

            obs_df <- st[st$rowType == "observation", , drop = FALSE]
            stats_df <- st[st$rowType == "summary", , drop = FALSE]

            num_cols <- c("case", "difference", "n", "mean", "sd", "median", "q1", "q3", "min", "max")
            for (cc in intersect(num_cols, names(obs_df)))
                obs_df[[cc]] <- suppressWarnings(as.numeric(obs_df[[cc]]))
            for (cc in intersect(num_cols, names(stats_df)))
                stats_df[[cc]] <- suppressWarnings(as.numeric(stats_df[[cc]]))

            obs_df <- obs_df[is.finite(obs_df$difference), , drop = FALSE]
            stats_df <- stats_df[is.finite(stats_df$mean), , drop = FALSE]

            comparisons <- unique(stats_df$comparison)
            if (length(comparisons) == 0)
                comparisons <- unique(obs_df$comparison)

            if (length(comparisons) == 0 || NROW(obs_df) == 0) {
                private$.emptyRelatedPlot(
                    paste(
                        plot_tr("No finite differences were obtained to build the plot.", "No se obtuvieron diferencias finitas para construir el gráfico."),
                        plot_tr("Review the selected measurements.", "Revise las mediciones seleccionadas."),
                        sep = "\n"
                    )
                )
                return()
            }

            obs_df$comparison <- factor(obs_df$comparison, levels = comparisons)
            stats_df$comparison <- factor(stats_df$comparison, levels = comparisons)

            pal <- private$.relatedPlotPalette()
            cat_pal <- private$.categoricalPalette(length(comparisons))
            names(cat_pal) <- as.character(comparisons)

            y_all <- c(obs_df$difference, stats_df$min, stats_df$max, stats_df$mean, stats_df$median)
            y_all <- y_all[is.finite(y_all)]
            y_span <- diff(range(y_all))
            if (!is.finite(y_span) || y_span <= 0)
                y_span <- max(abs(y_all), na.rm = TRUE) * 0.15 + 1

            stats_df$label_y <- pmax(stats_df$mean, stats_df$median, stats_df$max, na.rm = TRUE) +
                0.05 * y_span

            caption_text <- paste(
                plot_tr("Each point represents one individual paired difference.", "Cada punto representa una diferencia pareada individual."),
                plot_tr("The horizontal line at 0 indicates no change.", "La línea horizontal en 0 indica ausencia de cambio."),
                plot_tr("Box = IQR; central line = median; large point = mean.", "Caja = IQR; línea central = mediana; punto grande = media."),
                sep = "\n"
            )

            p <- ggplot2::ggplot(obs_df, ggplot2::aes(x = comparison, y = difference)) +
                ggplot2::geom_hline(
                    yintercept = 0,
                    linetype = "dashed",
                    linewidth = 0.55,
                    color = pal$ref
                ) +
                ggplot2::geom_boxplot(
                    mapping = ggplot2::aes(fill = comparison),
                    width = 0.38,
                    outlier.shape = NA,
                    color = pal$line,
                    linewidth = 0.65,
                    alpha = 0.75,
                    show.legend = FALSE
                ) +
                ggplot2::scale_fill_manual(values = cat_pal) +
                ggplot2::geom_jitter(
                    width = 0.10,
                    height = 0,
                    alpha = 0.28,
                    size = 1.3,
                    color = pal$ref
                ) +
                ggplot2::geom_point(
                    data = stats_df,
                    mapping = ggplot2::aes(x = comparison, y = mean),
                    inherit.aes = FALSE,
                    shape = 21,
                    size = 3.7,
                    stroke = 1.0,
                    fill = pal$fill,
                    color = pal$line
                ) +
                ggplot2::geom_point(
                    data = stats_df,
                    mapping = ggplot2::aes(x = comparison, y = median),
                    inherit.aes = FALSE,
                    shape = 23,
                    size = 2.8,
                    stroke = 0.9,
                    fill = "#FFFFFF",
                    color = pal$point
                ) +
                ggplot2::geom_label(
                    data = stats_df,
                    mapping = ggplot2::aes(x = comparison, y = label_y, label = label),
                    inherit.aes = FALSE,
                    vjust = 0,
                    size = 2.9,
                    lineheight = 1.00,
                    label.size = 0.20,
                    fill = "#FFFFFF",
                    color = pal$point
                ) +
                ggplot2::labs(
                    title = plot_tr("Distribution of paired differences", "Distribución de diferencias pareadas"),
                    subtitle = plot_tr("Visual diagnostic of change within the same units", "Diagnóstico visual del cambio dentro de las mismas unidades"),
                    x = plot_tr("Comparison", "Comparación"),
                    y = plot_tr("Paired difference", "Diferencia pareada"),
                    caption = caption_text
                ) +
                ggplot2::scale_y_continuous(
                    expand = ggplot2::expansion(mult = c(0.08, 0.34))
                ) +
                ggplot2::coord_cartesian(clip = "off") +
                ggplot2::theme_minimal(base_size = 11) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0),
                    plot.subtitle = ggplot2::element_text(size = 10),
                    axis.title.x = ggplot2::element_text(face = "bold"),
                    axis.title.y = ggplot2::element_text(face = "bold"),
                    axis.text.x = ggplot2::element_text(face = "bold", angle = 20, hjust = 1),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_blank(),
                    panel.grid.major.y = ggplot2::element_line(color = pal$grid),
                    plot.caption = ggplot2::element_text(hjust = 0, size = 9),
                    plot.margin = ggplot2::margin(12, 30, 18, 12)
                )

            print(p)
        },

        .plotNormality = function(image, ...) {
            if (!isTRUE(self$options$showNormalityPlots))
                return()

            self$results$normalityPlot$setVisible(TRUE)

            st <- image$state

        plot_tr <- function(en, es = NULL) private$.plotTr(en, es, if (exists("image", inherits = FALSE)) image else NULL)

            if (is.null(st) || NROW(st) == 0) {
                private$.emptyRelatedPlot(paste(
                    plot_tr("No valid paired differences are available for the Q-Q plot.", "No hay diferencias pareadas válidas para Q-Q plot."),
                    plot_tr("At least 3 valid differences and variability greater than zero are required.", "Se requieren al menos 3 diferencias válidas y variabilidad mayor que cero."),
                    sep = "\n"
                ))
                return()
            }

            needed <- c("comparison", "value", "n", "mean", "sd")

            if (!all(needed %in% names(st))) {
                private$.emptyRelatedPlot(paste(
                    plot_tr("The Q-Q plot internal state does not contain the expected columns.", "El estado interno del Q-Q plot no contiene las columnas esperadas."),
                    plot_tr("Run the analysis again or check the Image result definition.", "Vuelva a ejecutar el análisis o revise la definición del resultado Image."),
                    sep = "\n"
                ))
                return()
            }

            st$comparison <- as.character(st$comparison)
            st$value <- suppressWarnings(as.numeric(st$value))
            st <- st[is.finite(st$value) & nzchar(st$comparison), , drop = FALSE]

            if (NROW(st) == 0) {
                private$.emptyRelatedPlot(plot_tr("No finite values are available to plot the Q-Q plot.", "No hay valores finitos para graficar Q-Q plot."))
                return()
            }

            ok <- vapply(
                split(st$value, st$comparison),
                function(x) length(x[is.finite(x)]) >= 3 && stats::sd(x[is.finite(x)]) > 0,
                logical(1)
            )

            keep <- names(ok)[ok]

            if (length(keep) == 0) {
                private$.emptyRelatedPlot(paste(
                    plot_tr("No plottable comparisons are available.", "No hay comparaciones graficables."),
                    plot_tr("Each comparison requires at least 3 valid differences and variance greater than zero.", "Cada comparación requiere al menos 3 diferencias válidas y varianza mayor que cero."),
                    sep = "\n"
                ))
                return()
            }

            st <- st[st$comparison %in% keep, , drop = FALSE]
            st$comparison <- factor(st$comparison, levels = keep)

            pal <- private$.relatedPlotPalette()

            caption_text <- paste(
                plot_tr("Points close to the line suggest approximate normality.", "Puntos cercanos a la línea sugieren normalidad aproximada."),
                plot_tr("Interpret the plot together with numerical tests, sample size, and outliers.", "Interprete el gráfico junto con las pruebas numéricas, tamaño muestral y casos atípicos."),
                sep = "\n"
            )

            p <- ggplot2::ggplot(st, ggplot2::aes(sample = value)) +
                ggplot2::stat_qq(
                    color = pal$point,
                    alpha = 0.65,
                    size = 1.15
                ) +
                ggplot2::stat_qq_line(
                    color = pal$line,
                    linewidth = 0.75,
                    linetype = "dashed"
                ) +
                ggplot2::facet_wrap(
                    ggplot2::vars(comparison),
                    scales = "free",
                    ncol = if (length(keep) <= 2) 1 else 2
                ) +
                ggplot2::labs(
                    title = plot_tr("Q-Q plots of paired differences", "Q-Q plots de diferencias pareadas"),
                    subtitle = plot_tr("Visual diagnostic of normality by comparison", "Diagnóstico visual de normalidad por comparación"),
                    x = plot_tr("Theoretical normal quantiles", "Cuantiles teóricos normales"),
                    y = plot_tr("Observed quantiles", "Cuantiles observados"),
                    caption = caption_text
                ) +
                ggplot2::theme_minimal(base_size = 10.5) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0),
                    plot.subtitle = ggplot2::element_text(size = 9.5),
                    strip.text = ggplot2::element_text(face = "bold", size = 8.6),
                    axis.title.x = ggplot2::element_text(face = "bold"),
                    axis.title.y = ggplot2::element_text(face = "bold"),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_line(color = pal$grid),
                    plot.caption = ggplot2::element_text(hjust = 0, size = 8.5),
                    plot.margin = ggplot2::margin(10, 18, 14, 10)
                )

            print(p)
        },

        .plotNormalCurve = function(image, ...) {
            if (!isTRUE(self$options$showNormalityPlots))
                return()

            self$results$normalCurvePlot$setVisible(TRUE)

            st <- image$state

        plot_tr <- function(en, es = NULL) private$.plotTr(en, es, if (exists("image", inherits = FALSE)) image else NULL)

            if (is.null(st) || NROW(st) == 0) {
                private$.emptyRelatedPlot(paste(
                    plot_tr("No valid paired differences are available to compare with the theoretical normal curve.", "No hay diferencias pareadas válidas para comparar con la normal teórica."),
                    plot_tr("At least 3 valid differences and variability greater than zero are required.", "Se requieren al menos 3 diferencias válidas y variabilidad mayor que cero."),
                    sep = "\n"
                ))
                return()
            }

            needed <- c("comparison", "value", "n", "mean", "sd")

            if (!all(needed %in% names(st))) {
                private$.emptyRelatedPlot(paste(
                    plot_tr("The normal-curve plot internal state does not contain the expected columns.", "El estado interno del gráfico de curva normal no contiene las columnas esperadas."),
                    plot_tr("Run the analysis again or check the Image result definition.", "Vuelva a ejecutar el análisis o revise la definición del resultado Image."),
                    sep = "\n"
                ))
                return()
            }

            st$comparison <- as.character(st$comparison)
            st$value <- suppressWarnings(as.numeric(st$value))
            st <- st[is.finite(st$value) & nzchar(st$comparison), , drop = FALSE]

            if (NROW(st) == 0) {
                private$.emptyRelatedPlot(plot_tr("No finite values are available to plot the distribution.", "No hay valores finitos para graficar distribución."))
                return()
            }

            split_values <- split(st$value, st$comparison)

            keep <- names(split_values)[vapply(
                split_values,
                function(x) length(x[is.finite(x)]) >= 3 && stats::sd(x[is.finite(x)]) > 0,
                logical(1)
            )]

            if (length(keep) == 0) {
                private$.emptyRelatedPlot(paste(
                    plot_tr("No plottable comparisons are available.", "No hay comparaciones graficables."),
                    plot_tr("Each comparison requires at least 3 valid differences and variance greater than zero.", "Cada comparación requiere al menos 3 diferencias válidas y varianza mayor que cero."),
                    sep = "\n"
                ))
                return()
            }

            curve_rows <- list()
            row_id <- 1L

            for (nm in keep) {
                x <- split_values[[nm]]
                x <- x[is.finite(x)]

                den <- tryCatch(stats::density(x, na.rm = TRUE), error = function(e) NULL)

                if (!is.null(den)) {
                    curve_rows[[row_id]] <- data.frame(
                        comparison = nm,
                        x = as.numeric(den$x),
                        y = as.numeric(den$y),
                        curve = plot_tr("Observed density", "Densidad observada"),
                        stringsAsFactors = FALSE
                    )
                    row_id <- row_id + 1L
                }

                x_grid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 200)

                curve_rows[[row_id]] <- data.frame(
                    comparison = nm,
                    x = as.numeric(x_grid),
                    y = as.numeric(stats::dnorm(
                        x_grid,
                        mean = mean(x, na.rm = TRUE),
                        sd = stats::sd(x, na.rm = TRUE)
                    )),
                    curve = plot_tr("Theoretical normal", "Normal teórica"),
                    stringsAsFactors = FALSE
                )
                row_id <- row_id + 1L
            }

            if (length(curve_rows) == 0) {
                private$.emptyRelatedPlot(plot_tr("It was not possible to compute densities for plotting.", "No fue posible calcular densidades para graficar."))
                return()
            }

            curve_df <- do.call(rbind, curve_rows)
            curve_df$comparison <- factor(curve_df$comparison, levels = keep)
            curve_df$curve <- factor(curve_df$curve, levels = c(plot_tr("Observed density", "Densidad observada"), plot_tr("Theoretical normal", "Normal teórica")))

            pal <- private$.relatedPlotPalette()

            caption_text <- paste(
                plot_tr("The solid line shows the observed density.", "La línea continua muestra la densidad observada."),
                plot_tr("The dashed line shows a theoretical normal curve with the same mean and SD.", "La línea discontinua muestra una normal teórica con la misma media y SD."),
                plot_tr("Use this plot together with the Q-Q plot, numerical tests, and outliers.", "Use este gráfico junto con Q-Q plot, pruebas numéricas y casos atípicos."),
                sep = "\n"
            )

            p <- ggplot2::ggplot(
                curve_df,
                ggplot2::aes(x = x, y = y, linetype = curve)
            ) +
                ggplot2::geom_line(
                    color = pal$line,
                    linewidth = 0.85
                ) +
                ggplot2::facet_wrap(
                    ggplot2::vars(comparison),
                    scales = "free",
                    ncol = if (length(keep) <= 2) 1 else 2
                ) +
                ggplot2::scale_linetype_manual(
                    values = c("solid", "dashed")
                ) +
                ggplot2::labs(
                    title = plot_tr("Observed distribution vs theoretical normal curve", "Distribución observada vs normal teórica"),
                    subtitle = plot_tr("Distribution-shape comparison by paired difference", "Comparación de forma distributiva por diferencia pareada"),
                    x = plot_tr("Paired difference", "Diferencia pareada"),
                    y = plot_tr("Density", "Densidad"),
                    linetype = "",
                    caption = caption_text
                ) +
                ggplot2::theme_minimal(base_size = 10.5) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0),
                    plot.subtitle = ggplot2::element_text(size = 9.5),
                    strip.text = ggplot2::element_text(face = "bold", size = 8.6),
                    axis.title.x = ggplot2::element_text(face = "bold"),
                    axis.title.y = ggplot2::element_text(face = "bold"),
                    legend.position = "bottom",
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_line(color = pal$grid),
                    plot.caption = ggplot2::element_text(hjust = 0, size = 8.5),
                    plot.margin = ggplot2::margin(10, 18, 14, 10)
                )

            print(p)
        }
    )
)
