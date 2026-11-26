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
# Independent Groups.
# ES: Grupos Independientes.
#
# This file implements groupCheck: an assumption-diagnostic module for
# comparing two or more independent groups on a single dependent variable.
# It reports the assumption diagnostics that decide whether a classical
# t-test/ANOVA comparison of group means is trustworthy (normality by
# group, homogeneity of variances, outlier screening, and case-level
# influence via Cook's distance and Mahalanobis distance), and recommends
# a specific test given the pattern of results.
#
# ES: Este archivo implementa groupCheck: un módulo de diagnóstico de
# supuestos para comparar dos o más grupos independientes en una única
# variable dependiente. Reporta los diagnósticos de supuestos que deciden
# si una comparación clásica de medias de grupo por t/ANOVA es confiable
# (normalidad por grupo, homogeneidad de varianzas, cribado de atípicos e
# influencia a nivel de caso mediante la distancia de Cook y la distancia
# de Mahalanobis), y recomienda una prueba específica según el patrón de
# resultados.
#
# Responsibilities
# 1. Validate the selected dependent and grouping variables and prepare
#    the analysis sample (valid group values, complete cases for the
#    case-level model).
# 2. Compute descriptive statistics and IQR-based outlier screening for
#    every group.
# 3. Fit the group-comparison model and compute case-level influence
#    diagnostics (Cook's distance, Mahalanobis distance).
# 4. Compute and report the normality battery per group and the
#    homogeneity-of-variance battery across groups.
# 5. Render the diagnostic plots (distribution by group, Q-Q, observed-vs-
#    normal density), grouped by methodological purpose.
# 6. Assemble the applied-interpretation text for every diagnostic area
#    and a dynamic test recommendation, in the user's selected report
#    language.
#
# ES: Responsabilidades
# 1. Validar las variables dependiente y de agrupación seleccionadas y
#    preparar la muestra de análisis (valores de grupo válidos, casos
#    completos para el modelo a nivel de caso).
# 2. Calcular estadísticos descriptivos y el cribado de atípicos basado en
#    IQR para cada grupo.
# 3. Ajustar el modelo de comparación de grupos y calcular los
#    diagnósticos de influencia a nivel de caso (distancia de Cook,
#    distancia de Mahalanobis).
# 4. Calcular y reportar la batería de normalidad por grupo y la batería
#    de homogeneidad de varianzas entre grupos.
# 5. Renderizar los gráficos diagnósticos (distribución por grupo, Q-Q,
#    densidad observada vs normal), agrupados por propósito metodológico.
# 6. Ensamblar el texto de interpretación aplicada para cada área
#    diagnóstica y una recomendación de prueba dinámica, en el idioma de
#    informe seleccionado por el usuario.
#
# Workflow
# 1. Validate: check that a dependent and a grouping variable were
#    selected and that the dependent variable is numeric.
# 2. Prepare: build the analysis sample (valid group rows, complete
#    cases for the case-level model) and translate result titles.
# 3. Describe: compute descriptive statistics and render the distribution
#    plot per group.
# 4. Screen: flag IQR outliers and case-level influential points (Cook's
#    distance, Mahalanobis distance).
# 5. Test assumptions: run the normality battery per group and the
#    homogeneity-of-variance battery across groups.
# 6. Plot: render the Q-Q and observed-vs-normal diagnostic plots.
# 7. Interpret: build the applied-interpretation text for every
#    diagnostic area.
# 8. Recommend: assemble a dynamic recommendation (which test fits the
#    observed assumption pattern) into the executive summary and notes.
#
# ES: Flujo de trabajo
# 1. Validar: comprobar que se seleccionaron una variable dependiente y
#    una variable de agrupación, y que la dependiente es numérica.
# 2. Preparar: construir la muestra de análisis (filas con grupo válido,
#    casos completos para el modelo a nivel de caso).
# 3. Describir: calcular estadísticos descriptivos y renderizar el
#    gráfico de distribución por grupo.
# 4. Cribar: marcar atípicos por IQR y puntos influyentes a nivel de caso
#    (distancia de Cook, distancia de Mahalanobis).
# 5. Probar supuestos: correr la batería de normalidad por grupo y la
#    batería de homogeneidad de varianzas entre grupos.
# 6. Graficar: renderizar los gráficos diagnósticos Q-Q y de densidad
#    observada vs normal.
# 7. Interpretar: construir el texto de interpretación aplicada para cada
#    área diagnóstica.
# 8. Recomendar: ensamblar una recomendación dinámica (qué prueba se
#    ajusta al patrón de supuestos observado) en el resumen ejecutivo y
#    las notas.
# -----------------------------------------------------------------------------

groupCheckClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "groupCheckClass",
    inherit = groupCheckBase,
    private = list(
        .qqPlotData = NULL,
        .qqPlotSelected = NULL,

        # -----------------------------------------------------------------------------
        # Lay out the rows whose keys are known from the options alone, with their
        # labels; .run() fills them with setRow() (via .al_add_row()). The rows per
        # group level, per case and the two-group F test depend on the data and
        # are still added in .run().
        # ES: Crea las filas cuyas claves se conocen solo con las opciones, con sus
        # etiquetas; .run() las rellena con setRow() (vía .al_add_row()). Las filas
        # por nivel del grupo, por caso y la prueba F de dos grupos dependen de los
        # datos y se siguen añadiendo en .run().
        # -----------------------------------------------------------------------------
        .init = function() {
            . <- .al_lang(self$options)$tr

            .al_init_rows(self$results$design,
                c("design", "total", "validGroup", "missingGroup", "groups", "missingDep"),
                list(list(item = .("Detected design")),
                     list(item = .("Total rows")),
                     list(item = .("Rows with valid group value")),
                     list(item = .("Rows without group value")),
                     list(item = .("Detected groups")),
                     list(item = .("Missing dependent values"))))

            .al_init_rows(self$results$homogeneity, paste0("hom_", 1:5),
                list(list(test = .("Levene (median-centered)")),
                     list(test = "Brown-Forsythe"),
                     list(test = "Levene"),
                     list(test = "Bartlett"),
                     list(test = "Fligner-Killeen")))
        },

        # -----------------------------------------------------------------------------
        # Main analysis entry point.
        # ES: Punto de entrada principal del análisis.
        #
        # Validates the dependent/grouping variables, computes descriptives, outlier
        # and case-level influence screening, the normality and homogeneity-of-
        # variance batteries, and the dynamic test recommendation for the selected
        # independent-groups comparison; then renders every guide, interpretation,
        # and table title in jamovi's language.
        #
        # ES: Valida las variables dependiente/de agrupación, calcula descriptivos,
        # cribado de atípicos e influencia a nivel de caso, las baterías de
        # normalidad y homogeneidad de varianzas, y la recomendación de prueba
        # dinámica para la comparación de grupos independientes seleccionada; luego
        # renderiza cada guía, interpretación y título de tabla en el idioma de
        # informe seleccionado por el usuario.
        # -----------------------------------------------------------------------------
        .run = function() {
            # Hide the fixed guides when "Explanations" is off (see
            # .al_apply_guides()). ES: oculta las guías fijas si está desactivado.
            on.exit(.al_apply_guides(self), add = TRUE)
            data <- self$data
            dep <- self$options$dep
            group <- self$options$group

            # Report-language helpers: `.` translates through jamovi's own
            # catalog (jamovi/i18n/es.po) in the language jamovi sends with
            # this run; txt() looks up an entry of the shared text table by
            # section/key (texts.R).
            # ES: ayudantes de idioma del informe: `.` traduce con el
            # catálogo propio de jamovi (jamovi/i18n/es.po) en el idioma que
            # jamovi envía con esta ejecución; txt() busca una entrada de la
            # tabla de textos compartida por sección/clave (texts.R).
            lang <- .al_lang(self$options)
            . <- lang$tr

            txt <- function(section, key) {
                .al_text(lang, section, key)
            }

            # Coerces a value to NA_real_ whenever it is missing, empty, NaN, or
            # infinite, so downstream formatting/comparison functions never have to
            # special-case those states themselves.
            # ES: convierte un valor en NA_real_ cuando está ausente, vacío, NaN o
            # infinito, para que las funciones de formato/comparación posteriores no
            # tengan que tratar esos casos por separado.
            clean_num <- function(x) {
                if (length(x) == 0)
                    return(NA_real_)
                if (is.nan(x) || is.infinite(x))
                    return(NA_real_)
                x
            }

            # p_sig(): identical logic (via clean_num) in every module,
            # consolidated in shared-helpers.R (.al_p_sig). groupCheck's own
            # version skipped clean_num() (only checked is.na/is.nan
            # directly) - a defensive no-op difference for any real
            # p-value; see .al_p_sig()'s doc comment for detail.
            # ES: idéntica (vía clean_num) en todos los módulos,
            # consolidada en shared-helpers.R. La versión propia de
            # groupCheck se saltaba clean_num() - diferencia defensiva sin
            # efecto real; ver el comentario de .al_p_sig().
            p_sig <- .al_p_sig

            # -----------------------------------------------------------------------------
            # Verbal decision labels for normality and variance tests.
            # ES: Etiquetas verbales de decisión para las pruebas de normalidad y
            # varianza.
            #
            # Translate a raw p-value into the plain-language decision shown in the
            # report tables, using the same .05/.01/.001 significance thresholds as
            # the rest of the module, so every table reads consistently regardless of
            # which specific test produced the p-value.
            #
            # ES: Traducen un valor p crudo a la decisión en lenguaje llano que se
            # muestra en las tablas del informe, usando los mismos umbrales de
            # significancia .05/.01/.001 que el resto del módulo, de modo que todas
            # las tablas se lean de forma consistente sin importar qué prueba produjo
            # el valor p.
            # -----------------------------------------------------------------------------
            p_decision <- function(p) {
                if (is.na(p) || is.nan(p))
                    return(.("Not calc."))

                if (p < .001)
                    return(.("Sig. dev. 0.1%"))

                if (p < .01)
                    return(.("Sig. dev. 1%"))

                if (p < .05)
                    return(.("Sig. dev. 5%"))

                .("Approx. normal")
            }

            variance_decision <- function(p) {
                if (is.na(p) || is.nan(p))
                    return(.("Not calc."))

                if (p < .05)
                    return(.("Significant variance difference"))

                .("Homogeneous")
            }

            # -----------------------------------------------------------------------------
            # Interpretation-text wrapping utilities.
            # ES: Utilidades de ajuste de texto para las interpretaciones.
            #
            # wrap_text() collapses a piece of applied-interpretation prose to single
            # spaces and re-wraps it at a fixed width; block96() applies the same
            # wrapping to every paragraph of a multi-paragraph block independently, so
            # that HTML text built from string concatenation always renders with
            # consistent line lengths regardless of how the source strings were split.
            #
            # ES: wrap_text() colapsa un fragmento de prosa de interpretación aplicada
            # a espacios simples y lo reajusta a un ancho fijo; block96() aplica el
            # mismo ajuste a cada párrafo de un bloque de varios párrafos por
            # separado, de modo que el texto HTML construido por concatenación de
            # cadenas siempre se renderice con longitudes de línea consistentes sin
            # importar cómo se dividieron las cadenas de origen.
            # -----------------------------------------------------------------------------
            wrap_text <- function(..., width = 96) {
                txt <- paste(..., collapse = "")
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
                txt <- gsub("\\\\n", "\n", txt)
                txt <- gsub("\\\\t", " ", txt)
                txt <- gsub("[ \t]+\n", "\n", txt)
                txt <- gsub("\n[ \t]+", "\n", txt)

                lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
                lines <- trimws(lines)
                lines <- lines[nzchar(lines)]

                lines <- vapply(
                    lines,
                    function(z) wrap_text(z, width = 96),
                    character(1)
                )

                paste(lines, collapse = "\n")
            }

            html_escape <- .al_html_escape

            html_block <- .al_html_block

            # -----------------------------------------------------------------------------
            # Introductory panel renderer.
            # ES: Renderizador del panel introductorio.
            #
            # Builds the fixed HTML shown at the top of the report (module name, short
            # description of when to use the analysis, and the selected variable
            # names). Kept as its own function because it is called twice: once with
            # variable names when the analysis runs, and once without them for the
            # placeholder shown before the user has selected variables.
            #
            # ES: Construye el HTML fijo que se muestra en la parte superior del
            # informe (nombre del módulo, descripción breve de cuándo usar el
            # análisis y los nombres de las variables seleccionadas). Se mantiene como
            # función propia porque se invoca dos veces: una con los nombres de
            # variable cuando el análisis corre, y otra sin ellos para el mensaje de
            # marcador de posición que se muestra antes de que el usuario seleccione
            # variables.
            # -----------------------------------------------------------------------------
            render_groupcheck_intro <- function(show_vars = TRUE) {

                title_block <- .("<p style=\"font-weight:700; margin:0 0 0.10em 0;\">AssumptionsLab</p><p style=\"margin:0 0 0.35em 0;\">Assumption check for independent groups</p>")

                use_paragraph <- .("Use this analysis when you want to review whether a comparison between independent groups has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.")

                var_lines <- character(0)
                if (isTRUE(show_vars)) {
                    var_lines <- c(
                        sprintf(.("<b>Dependent variable:</b> %s"), html_escape(dep)),
                        sprintf(.("<b>Grouping variable:</b> %s"), html_escape(group))
                    )
                }

                paste0(
                    "<div style=\"max-width:7.25in; width:100%; box-sizing:border-box;  text-align: justify; margin:0.20em 0 1.05em 0;\">",
                    title_block,
                    "<p style=\"margin:0 0 0.55em 0;\">\u00A0</p>",
                    "<p style=\"margin:0 0 0.55em 0; line-height:1.32;\">", html_escape(use_paragraph), "</p>",
                    "<p style=\"margin:0 0 0.55em 0;\">\u00A0</p>",
                    if (length(var_lines) > 0)
                        paste0(
                            "<p style=\"margin:0 0 0.25em 0; line-height:1.32;\">",
                            var_lines,
                            "</p>",
                            collapse = ""
                        )
                    else
                        "",
                    "</div>"
                )
            }

            # -----------------------------------------------------------------------------
            # Numeric formatting for report tables.
            # ES: Formato numérico para las tablas del informe.
            #
            # fmt_num() trims a numeric value to a fixed number of decimals and strips
            # trailing zeros; fmt_p() formats a p-value in the conventional "< .001"
            # / leading-zero-dropped style used throughout the report tables. Both
            # route non-finite/missing input through clean_num() and fall back to the
            # "Not calc." label so a failed statistic never renders as a raw NA.
            #
            # ES: fmt_num() recorta un valor numérico a un número fijo de decimales y
            # elimina los ceros finales; fmt_p() formatea un valor p en el estilo
            # convencional "< .001" / sin cero inicial usado en todas las tablas del
            # informe. Ambas enrutan la entrada no finita/faltante a través de
            # clean_num() y usan la etiqueta "No calc." como respaldo, de modo que un
            # estadístico fallido nunca se muestre como un NA crudo.
            # -----------------------------------------------------------------------------
            fmt_num <- function(x, digits = 4) {
                x <- clean_num(x)

                if (is.na(x))
                    return(.("Not calc."))

                out <- formatC(x, format = "f", digits = digits)
                out <- sub("0+$", "", out)
                out <- sub("\\.$", "", out)
                out
            }

            fmt_p <- function(p) {
                p <- clean_num(p)

                if (is.na(p))
                    return(.("Not calc."))

                if (p < .001)
                    return("< .001")

                out <- sprintf("%.3f", p)
                sub("^0", "", out)
            }

            # -----------------------------------------------------------------------------
            # Input validation.
            # ES: Validación de entrada.
            #
            # Every check below returns early with an explanatory placeholder message
            # instead of the report, so the user is never shown a partially computed
            # analysis or a raw R error while the variable selection is incomplete or
            # unsuitable.
            #
            # ES: Cada verificación siguiente retorna anticipadamente con un mensaje
            # explicativo en lugar del informe, de modo que el usuario nunca vea un
            # análisis parcialmente calculado ni un error crudo de R mientras la
            # selección de variables esté incompleta o no sea adecuada.
            # -----------------------------------------------------------------------------
            # Not configured yet (nothing selected): return quietly and leave the
            # empty results skeleton in place; reject() is kept for real
            # problems the user can fix.
            # ES: Aún sin configurar (nada seleccionado): volver en silencio y
            # dejar el esqueleto vacío; reject() queda para errores reales.
            if (is.null(dep) || is.null(group))
                return()

            if (! dep %in% names(data) || ! group %in% names(data)) {
                jmvcore::reject(.("Selected variables were not found in the data."))
            }

            y <- data[[dep]]
            g <- data[[group]]

            if (! is.numeric(y)) {
                jmvcore::reject(.("The dependent variable must be numeric."))
            }


            # -----------------------------------------------------------------------------
            # Data preparation: valid rows and group factor.
            # ES: Preparación de datos: filas válidas y factor de grupo.
            #
            # Rows with a missing group value are excluded up front (valid_group) so
            # every downstream computation shares the same group factor and level set;
            # rows with a missing dependent value are kept at this stage (only
            # excluded per-group inside each computation) so the design-summary table
            # can still report how many were missing.
            #
            # ES: las filas con valor de grupo faltante se excluyen desde el inicio
            # (valid_group) para que todo cálculo posterior comparta el mismo factor
            # de grupo y el mismo conjunto de niveles; las filas con valor faltante en
            # la variable dependiente se conservan en esta etapa (solo se excluyen por
            # grupo dentro de cada cálculo) para que la tabla de resumen del diseño
            # pueda seguir reportando cuántas faltaron.
            # -----------------------------------------------------------------------------
            valid_group <- ! is.na(g)
            y2 <- y[valid_group]
            g2 <- as.factor(g[valid_group])
            group_levels <- levels(g2)

            n_total <- nrow(data)
            n_used <- sum(valid_group)
            n_no_group <- sum(! valid_group)
            n_groups <- length(group_levels)
            n_missing_dep <- sum(is.na(y))
            missing_dep_pct <- round(100 * n_missing_dep / n_total, 2)

            self$results$intro$setContent(render_groupcheck_intro(show_vars = TRUE))

            private$.fillDesign(., n_total, n_used, n_no_group, n_groups, n_missing_dep, missing_dep_pct)

            private$.fillDescriptives(y2, g2, group_levels)

            private$.setDistributionPlot(dep, group, y2, g2, group_levels)

            outlier_table <- private$.screenOutliers(y2, g2, group_levels)

            step <- private$.checkCaseDiagnostics(., p_sig, block96, html_block, y, g, valid_group, n_total, n_groups, outlier_table)
            n_model <- step$n_model
            n_any_flagged <- step$n_any_flagged

            step <- private$.checkNormality(., clean_num, p_sig, p_decision, y2, g2, group_levels)
            shapiro_p_by_group <- step$shapiro_p_by_group
            normality_table <- step$normality_table
            group_sig_counts <- step$group_sig_counts
            group_valid_counts <- step$group_valid_counts

            homogeneity_rows <- private$.checkHomogeneity(., clean_num, p_sig, variance_decision, block96, html_block, fmt_p, y2, g2, n_total, n_groups, shapiro_p_by_group, group_sig_counts, group_valid_counts)

            # -----------------------------------------------------------------------------
            # Diagnostic guide texts: distribution plot and case diagnostics.
            # ES: Textos de guía diagnóstica: gráfico de distribución y diagnóstico de
            # casos.
            # -----------------------------------------------------------------------------
            case_diagnostics_guide <- c(.("These diagnostics help identify cases that may have an unusual effect on the group comparison model."), .("Cook's D evaluates potential influence: a flagged case may affect model estimates or group differences."), .("Mahalanobis D² is a multivariate distance measure. In this module, with a single dependent variable, it reduces to an equivalent measure based on the standardized distance from the group mean (its degenerate one-dimensional case)."), .("A flagged case is not automatically an error and should not be removed only because it is statistically unusual."), .("Recommended practice: check the original data value, verify whether the case is plausible, compare results with and without the case, and report any exclusion decision transparently."))

            self$results$distributionPlotGuide$setContent(html_block(
                txt("common", "briefGuide"),
                .al_html_list(c(
                    .("Use this plot to compare the median, IQR, approximate shape, spread, sample size and extreme values across groups."),
                    .("The boxplot keeps the median visible. Optional layers can add violin density, individual observations and the mean marker."),
                    .("Interpret visual differences together with descriptive statistics, outlier screening and assumption tests.")
                )),
                raw = TRUE))

            self$results$caseDiagnosticsGuide$setContent(html_block(
                txt("common", "briefGuide"),
                .al_html_list(case_diagnostics_guide),
                raw = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Normality guide text.
            # ES: Texto de guía de normalidad.
            # -----------------------------------------------------------------------------
            normality_guide_text <- c(.("Normality is evaluated within each independent group because parametric group comparisons assume approximately normal distributions at each level of the grouping factor."), .("A p value greater than or equal to .05 is compatible with approximate normality, but it does not prove that the distribution is perfectly normal."), .("A p value below .05 suggests a statistically significant deviation from normality. The practical importance of that deviation should be judged together with sample size, outliers, and graphical evidence."), .("With large samples, normality tests can detect small deviations that may not seriously affect the analysis. With small samples, tests may have low power, so visual inspection and substantive judgment are especially important."), .("When normality evidence is mixed, a parametric test can still be defensible if deviations are limited and sample sizes are adequate; robust or non-parametric alternatives can be reported as sensitivity analyses."))

            self$results$normalityGuide$setContent(html_block(
                txt("common", "briefGuide"),
                .al_html_list(normality_guide_text),
                raw = TRUE
            ))

            private$.qqPlotData <- NULL
            private$.qqPlotSelected <- NULL

            private$.setQQPlots(data, dep, group, ., txt, html_block)

            homogeneity_table <- private$.fillHomogeneityTable(homogeneity_rows)

            # -----------------------------------------------------------------------------
            # Variance guide text.
            # ES: Texto de guía de varianza.
            # -----------------------------------------------------------------------------
            variance_guide_text <- c(.("These tests evaluate whether the group variances are reasonably similar."), .("A p value greater than or equal to .05 is compatible with homogeneous variances, although it does not prove that variances are identical."), .("A p value below .05 suggests statistically significant variance heterogeneity. This matters because unequal variances can affect the standard error, confidence intervals, and p values of classic parametric comparisons."), .("When group sizes are unequal, heterogeneity of variances is more consequential. Welch procedures are usually preferable in that situation because they do not require equal variances."), .("Bartlett's test is sensitive to non-normality. Levene, Brown-Forsythe, and Fligner-Killeen are often more robust choices for applied data."))

            self$results$homogeneityGuide$setContent(html_block(
                txt("common", "briefGuide"),
                .al_html_list(variance_guide_text),
                raw = TRUE
            ))

            private$.fillSummary(., txt, html_escape, html_block, group_levels, n_total, n_groups, outlier_table, n_model, n_any_flagged, normality_table, homogeneity_table)


        },

        .fillDesign = function(., n_total, n_used, n_no_group, n_groups, n_missing_dep, missing_dep_pct) {
            # -----------------------------------------------------------------------------
            # Design summary table.
            # ES: Tabla de resumen del diseño.
            #
            # Reports the detected design (two groups vs. three-or-more vs. only one)
            # together with basic row/group accounting, so the reader can immediately
            # confirm groupCheck parsed the intended comparison before looking at any
            # diagnostic result.
            #
            # ES: Reporta el diseño detectado (dos grupos vs. tres o más vs. solo uno)
            # junto con un recuento básico de filas/grupos, para que el lector pueda
            # confirmar de inmediato que groupCheck interpretó la comparación prevista
            # antes de revisar cualquier resultado diagnóstico.
            # -----------------------------------------------------------------------------
            design_text <- if (n_groups == 2) {
                .("Comparison of two independent groups")
            } else if (n_groups > 2) {
                .("Comparison of three or more independent groups")
            } else {
                .("Only one group detected")
            }

            .al_add_row(self$results$design, "design", values = list(
                item = .("Detected design"),
                value = design_text
            ))

            .al_add_row(self$results$design, "total", values = list(
                item = .("Total rows"),
                value = as.character(n_total)
            ))

            .al_add_row(self$results$design, "validGroup", values = list(
                item = .("Rows with valid group value"),
                value = as.character(n_used)
            ))

            .al_add_row(self$results$design, "missingGroup", values = list(
                item = .("Rows without group value"),
                value = as.character(n_no_group)
            ))

            .al_add_row(self$results$design, "groups", values = list(
                item = .("Detected groups"),
                value = as.character(n_groups)
            ))

            .al_add_row(self$results$design, "missingDep", values = list(
                item = .("Missing dependent values"),
                value = paste0(n_missing_dep, " (", missing_dep_pct, "%)")
            ))

            invisible(NULL)
        },

        .fillDescriptives = function(y2, g2, group_levels) {
            # -----------------------------------------------------------------------------
            # Descriptive statistics by group.
            # ES: Estadísticos descriptivos por grupo.
            # -----------------------------------------------------------------------------
            summary_rows <- lapply(group_levels, function(level) {
                values <- y2[g2 == level]
                values_valid <- values[! is.na(values)]
                n_valid <- length(values_valid)

                data.frame(
                    group = as.character(level),
                    n = n_valid,
                    missing = sum(is.na(values)),
                    mean = if (n_valid > 0) mean(values_valid) else NA_real_,
                    sd = if (n_valid > 1) stats::sd(values_valid) else NA_real_,
                    median = if (n_valid > 0) stats::median(values_valid) else NA_real_,
                    min = if (n_valid > 0) min(values_valid) else NA_real_,
                    max = if (n_valid > 0) max(values_valid) else NA_real_,
                    stringsAsFactors = FALSE
                )
            })

            summary_table <- do.call(rbind, summary_rows)

            for (i in seq_len(nrow(summary_table))) {
                .al_add_row(self$results$descriptives, paste0("desc_", i),
                    values = list(
                        group = summary_table$group[i],
                        n = summary_table$n[i],
                        missing = summary_table$missing[i],
                        mean = summary_table$mean[i],
                        sd = summary_table$sd[i],
                        median = summary_table$median[i],
                        min = summary_table$min[i],
                        max = summary_table$max[i]
                    )
                )
            }

            invisible(NULL)
        },

        .setDistributionPlot = function(dep, group, y2, g2, group_levels) {
            # -----------------------------------------------------------------------------
            # Distribution plot data (boxplot/violin state).
            # ES: Datos del gráfico de distribución (estado de boxplot/violin).
            #
            # Builds the long-format data frame consumed by .plotGroupDistribution();
            # kept as a plot state object (rather than recomputed inside the plotting
            # callback) because jamovi re-invokes the plotting function on export/
            # resize without re-running .run().
            #
            # ES: construye el data frame en formato largo que consume
            # .plotGroupDistribution(); se mantiene como objeto de estado del gráfico
            # (en lugar de recalcularse dentro del callback de graficado) porque
            # jamovi vuelve a invocar la función de graficado al exportar/redimensionar
            # sin volver a ejecutar .run().
            # -----------------------------------------------------------------------------
            distribution_plot_state <- data.frame(
                value = numeric(0),
                group = character(0),
                depLabel = character(0),
                stringsAsFactors = FALSE)

            show_distribution_plot <- isTRUE(self$options$showDistributionPlot)

            if (show_distribution_plot) {
                dist_y <- suppressWarnings(as.numeric(y2))
                dist_g <- as.factor(g2)
                dist_ok <- is.finite(dist_y) & !is.na(dist_g)

                if (any(dist_ok)) {
                    distribution_plot_state <- data.frame(
                        value = dist_y[dist_ok],
                        group = as.character(dist_g[dist_ok]),
                        depLabel = dep,
                        stringsAsFactors = FALSE)

                    distribution_plot_state$group <- factor(
                        distribution_plot_state$group,
                        levels = group_levels)

                    distribution_plot_state <- distribution_plot_state[
                        order(distribution_plot_state$group), , drop = FALSE]
                }
            }

            self$results$distributionPlot$setState(distribution_plot_state)

            invisible(NULL)
        },

        .screenOutliers = function(y2, g2, group_levels) {
            # -----------------------------------------------------------------------------
            # Outlier screening via the IQR rule.
            # ES: Cribado de valores atípicos mediante la regla IQR.
            #
            # Applies the classic Tukey fences (1.5 x IQR for outliers, 3 x IQR for
            # extreme values) independently within each group, since IQR-based
            # screening is only meaningful relative to a group's own spread. Requires
            # at least 4 valid values per group to compute a stable quartile estimate;
            # smaller groups are reported with NA rather than an unreliable fence.
            #
            # ES: aplica las cercas clásicas de Tukey (1.5 x IQR para atípicos, 3 x IQR
            # para valores extremos) de forma independiente dentro de cada grupo, ya
            # que el cribado basado en IQR solo tiene sentido en relación con la
            # dispersión propia de cada grupo. Requiere al menos 4 valores válidos por
            # grupo para calcular una estimación estable de los cuartiles; los grupos
            # más pequeños se reportan con NA en lugar de una cerca poco confiable.
            # -----------------------------------------------------------------------------
            outlier_rows <- lapply(group_levels, function(level) {
                values <- y2[g2 == level]
                values_valid <- values[! is.na(values)]
                n_valid <- length(values_valid)

                if (n_valid >= 4) {
                    q1 <- as.numeric(stats::quantile(values_valid, probs = .25, names = FALSE))
                    q3 <- as.numeric(stats::quantile(values_valid, probs = .75, names = FALSE))
                    iqr <- q3 - q1
                    lower <- q1 - 1.5 * iqr
                    upper <- q3 + 1.5 * iqr
                    lower_extreme <- q1 - 3 * iqr
                    upper_extreme <- q3 + 3 * iqr
                    outliers <- sum(values_valid < lower | values_valid > upper)
                    extreme <- sum(values_valid < lower_extreme | values_valid > upper_extreme)
                } else {
                    q1 <- NA_real_
                    q3 <- NA_real_
                    iqr <- NA_real_
                    lower <- NA_real_
                    upper <- NA_real_
                    outliers <- NA_integer_
                    extreme <- NA_integer_
                }

                data.frame(
                    group = as.character(level),
                    n = n_valid,
                    q1 = q1,
                    q3 = q3,
                    iqr = iqr,
                    lower = lower,
                    upper = upper,
                    outliers = outliers,
                    extreme = extreme,
                    stringsAsFactors = FALSE
                )
            })

            outlier_table <- do.call(rbind, outlier_rows)

            for (i in seq_len(nrow(outlier_table))) {
                .al_add_row(self$results$outliers, paste0("out_", i),
                    values = list(
                        group = outlier_table$group[i],
                        n = outlier_table$n[i],
                        q1 = outlier_table$q1[i],
                        q3 = outlier_table$q3[i],
                        iqr = outlier_table$iqr[i],
                        lower = outlier_table$lower[i],
                        upper = outlier_table$upper[i],
                        outliers = outlier_table$outliers[i],
                        extreme = outlier_table$extreme[i]
                    )
                )
            }

            outlier_table
        },

        .checkCaseDiagnostics = function(., p_sig, block96, html_block, y, g, valid_group, n_total, n_groups, outlier_table) {
            # -----------------------------------------------------------------------------
            # Case-level diagnostics: Cook's distance and Mahalanobis distance.
            # ES: Diagnósticos a nivel de caso: distancia de Cook y distancia de
            # Mahalanobis.
            #
            # Fits y_model ~ g_model (equivalent to a one-way ANOVA) on complete cases
            # only, then derives two complementary influence measures from it: Cook's
            # distance (impact of removing the case on the fitted group means, cutoff
            # 4/n as a screening threshold, not a hard rule) and a Mahalanobis-style
            # squared distance from the case's own group mean, expressed in standard-
            # deviation units. With a single dependent variable, Mahalanobis distance
            # reduces to the univariate standardized-distance case.
            #
            # ES: ajusta y_model ~ g_model (equivalente a un ANOVA de un factor) solo
            # sobre los casos completos, y de ahí deriva dos medidas de influencia
            # complementarias: la distancia de Cook (impacto de eliminar el caso sobre
            # las medias de grupo ajustadas, punto de corte 4/n como umbral de
            # cribado, no una regla estricta) y una distancia al cuadrado tipo
            # Mahalanobis respecto a la media del propio grupo del caso, expresada en
            # unidades de desviación estándar. Con una única variable dependiente, la
            # distancia de Mahalanobis se reduce al caso univariado de distancia
            # estandarizada.
            # -----------------------------------------------------------------------------
            complete_cases <- which(valid_group & ! is.na(y))
            y_model <- y[complete_cases]
            g_model <- droplevels(as.factor(g[complete_cases]))
            n_model <- length(y_model)

            cook_values <- rep(NA_real_, n_model)

            if (n_model >= 3 && length(unique(g_model)) >= 2) {
                model <- tryCatch(stats::lm(y_model ~ g_model), error = function(e) NULL)
                if (! is.null(model)) {
                    cook_values <- tryCatch(stats::cooks.distance(model), error = function(e) rep(NA_real_, n_model))
                }
            }

            cook_cutoff <- if (n_model > 0) 4 / n_model else NA_real_

            mahal_d2 <- rep(NA_real_, n_model)
            mahal_p <- rep(NA_real_, n_model)

            if (n_model >= 3 && stats::sd(y_model, na.rm = TRUE) > 0) {
                mahal_d2 <- ((y_model - mean(y_model, na.rm = TRUE)) / stats::sd(y_model, na.rm = TRUE))^2
                mahal_p <- stats::pchisq(mahal_d2, df = 1, lower.tail = FALSE)
            }

            case_diag <- data.frame(
                case = complete_cases,
                group = as.character(g_model),
                value = y_model,
                cookD = cook_values,
                cookFlag = ifelse(
                    ! is.na(cook_values) & ! is.na(cook_cutoff) & cook_values > cook_cutoff,
                    .("Potential influence"),
                    ""
                ),
                mahalD2 = mahal_d2,
                mahalP = mahal_p,
                mahalSig = vapply(mahal_p, p_sig, character(1)),
                mahalFlag = ifelse(
                    # Antes: mahal_p < .001, mucho más estricto que el criterio de
                    # significancia (p < .05) que la propia columna mahalSig ya
                    # muestra con asteriscos. Esto producía el bug reportado: casos
                    # significativos en la tabla (p = .005, .019, .035) no se
                    # contaban en el resumen ("0 caso(s)"). Alineado a p < .05.
                    ! is.na(mahal_p) & mahal_p < .05,
                    .("High distance"),
                    ""
                ),
                stringsAsFactors = FALSE
            )

            case_diag <- case_diag[order(
                -ifelse(is.na(case_diag$cookD), -Inf, case_diag$cookD),
                -ifelse(is.na(case_diag$mahalD2), -Inf, case_diag$mahalD2)
            ), , drop = FALSE]

            max_cases_to_show <- min(20, nrow(case_diag))

            if (max_cases_to_show > 0) {
                for (i in seq_len(max_cases_to_show)) {
                    .al_add_row(self$results$caseDiagnostics, paste0("case_", i),
                        values = list(
                            case = case_diag$case[i],
                            group = case_diag$group[i],
                            value = case_diag$value[i],
                            cookD = case_diag$cookD[i],
                            cookFlag = case_diag$cookFlag[i],
                            mahalD2 = case_diag$mahalD2[i],
                            mahalP = case_diag$mahalP[i],
                            mahalSig = case_diag$mahalSig[i],
                            mahalFlag = case_diag$mahalFlag[i]
                        )
                    )
                }
            }

            # -----------------------------------------------------------------------------
            # Case diagnostics applied-interpretation text.
            # ES: Texto de interpretación aplicada de los diagnósticos de casos.
            #
            # Tallies the IQR-rule and case-level (Cook's D / Mahalanobis) flags and
            # explains, in applied terms, that the two diagnostics answer different
            # questions and are not expected to always agree.
            #
            # ES: cuenta las marcas de la regla IQR y las de nivel de caso (Cook's D /
            # Mahalanobis) y explica, en términos aplicados, que ambos diagnósticos
            # responden preguntas distintas y no se espera que siempre coincidan.
            # -----------------------------------------------------------------------------
            n_groups_with_outliers <- sum(outlier_table$outliers > 0, na.rm = TRUE)
            n_iqr_outliers <- sum(outlier_table$outliers, na.rm = TRUE)
            n_iqr_extreme <- sum(outlier_table$extreme, na.rm = TRUE)

            n_cook_flagged <- sum(nzchar(case_diag$cookFlag), na.rm = TRUE)
            n_mahal_flagged <- sum(nzchar(case_diag$mahalFlag), na.rm = TRUE)
            n_any_flagged <- sum(nzchar(case_diag$cookFlag) | nzchar(case_diag$mahalFlag), na.rm = TRUE)

            self$results$caseDiagnosticsInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                block96(paste(
                    sprintf(.("Across %s cases in %s group(s), the IQR rule flagged %s outlier value(s) (%s of them extreme) spread across %s group(s). Separately, the case-level model (n = %s) flagged %s case(s) by Cook's D (cutoff = %s) and %s case(s) by Mahalanobis distance, for %s unique case(s) flagged by at least one of these two criteria."), n_total, n_groups, n_iqr_outliers, n_iqr_extreme, n_groups_with_outliers, n_model, n_cook_flagged, round(cook_cutoff, 4), n_mahal_flagged, n_any_flagged),
                    .("These two diagnostics answer related but distinct questions: the IQR rule flags values that are unusual relative to their own group's spread, while Cook's D and Mahalanobis distance flag cases that are unusual relative to the fitted group-comparison model as a whole. A case can be flagged by one and not the other."),
                    .("Why it matters: in group comparisons, a flagged case can shift a group's mean, its variance, or the overall homogeneity-of-variance conclusion enough to change which comparisons look defensible, especially in small or unbalanced groups."),
                    sprintf(.("A flagged case should not be removed mechanically to \"clean up\" the data — first check whether it reflects a data entry mistake, a valid but extreme case, or substantive information about that group, comparing results with and without it before deciding. Worth keeping in mind while doing so: the 4/n threshold used for Cook's D (here 4/%s = %s) is numerically larger — more lenient — as the model sample shrinks, and smaller — stricter — as it grows, yet small or unbalanced groups often still flag several cases because individual Cook's D values tend to be inflated when n is small. Treat 4/n as a screening threshold, not confirmed influence."), n_model, round(cook_cutoff, 4)),
                    sep = "\n\n"
                ))
            ))

            list(n_model = n_model, n_any_flagged = n_any_flagged)
        },

        .checkNormality = function(., clean_num, p_sig, p_decision, y2, g2, group_levels) {
            # -----------------------------------------------------------------------------
            # Normality diagnostic battery per group.
            # ES: Batería diagnóstica de normalidad por grupo.
            #
            # Runs the full normality battery (Shapiro-Wilk, Lilliefors, Anderson-
            # Darling, Cramér-von Mises, Shapiro-Francia, Pearson chi-square, Jarque-
            # Bera, skewness and kurtosis tests) independently within every group,
            # because a parametric group comparison assumes approximate normality at
            # each level of the grouping factor, not only in the pooled sample.
            #
            # ES: ejecuta la batería completa de normalidad (Shapiro-Wilk, Lilliefors,
            # Anderson-Darling, Cramér-von Mises, Shapiro-Francia, chi-cuadrado de
            # Pearson, Jarque-Bera, y pruebas de asimetría y curtosis) de forma
            # independiente dentro de cada grupo, porque una comparación paramétrica
            # de grupos supone normalidad aproximada en cada nivel del factor de
            # agrupación, no solo en la muestra combinada.
            # -----------------------------------------------------------------------------
            normality_rows <- list()

            normality_use <- function(test) {
                if (test == "Shapiro-Wilk")
                    return(.("Preferred for small to moderate samples; widely used for normality screening."))

                if (test == .("Lilliefors (corrected K-S)"))
                    return(.("General empirical distribution check, corrected for parameters estimated from the sample."))

                if (test == "Anderson-Darling")
                    return(.("Useful when deviations in the distribution tails are important."))

                if (test == "Cram\u00e9r-von Mises")
                    return(.("Alternative empirical-distribution test, sensitive across the whole range of the distribution."))

                if (test == "Shapiro-Francia")
                    return(.("Simplified variant of Shapiro-Wilk, competitive in large samples."))

                if (test == .("Pearson chi-square"))
                    return(.("Compares observed vs expected frequencies by class; less powerful with small samples."))

                if (test == "Jarque-Bera")
                    return(.("Evaluates skewness and kurtosis jointly; more informative in moderate to large samples."))

                if (test == .("Skewness test"))
                    return(.("Identifies asymmetric departures from normality."))

                if (test == .("Kurtosis test"))
                    return(.("Identifies departures related to peakedness or tail weight."))

                ""
            }

            add_normality <- function(level, test, n, statistic, value, p, reason = NA_character_) {
                value <- clean_num(value)
                p <- clean_num(p)

                normality_rows[[length(normality_rows) + 1]] <<- data.frame(
                    group = as.character(level),
                    test = test,
                    n = n,
                    statistic = statistic,
                    value = value,
                    p = p,
                    pSig = p_sig(p),
                    decision = p_decision(p),
                    use = normality_use(test),
                    reason = if (is.null(reason)) NA_character_ else reason,
                    stringsAsFactors = FALSE
                )
            }

            shapiro_p_by_group <- c()

            for (level in group_levels) {

                values <- y2[g2 == level]
                x <- values[! is.na(values)]
                n_valid <- length(x)

                # Shapiro-Wilk / Jarque-Bera / skewness / kurtosis: guard
                # unified suite-wide (n>=3,<=5000 & sd>0 for SW; n>=8 & sd>0
                # for the rest) per Archie's decision, Aug 2026 - see
                # .al_norm_core_battery(). groupCheck already used this exact
                # guard manually; this just routes the same math through the
                # shared function instead of duplicating it.
                # ES: guarda unificada para toda la suite - groupCheck ya
                # usaba esta misma guarda manualmente; esto solo enruta la
                # misma matemática por la función compartida.
                .nc_x <- .al_norm_core_battery(x)
                sw <- .nc_x$sw

                if (!is.null(sw)) {
                    add_normality(level, "Shapiro-Wilk", n_valid, "W", unname(sw$statistic), sw$p.value)
                    shapiro_p_by_group <- c(shapiro_p_by_group, sw$p.value)
                } else {
                    add_normality(level, "Shapiro-Wilk", n_valid, "W", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_x$reasons$sw, .))
                }

                # Lilliefors / Anderson-Darling / Cramér-von Mises / Shapiro-Francia /
                # Pearson chi-square: identical tryCatch calls in every module,
                # consolidated in shared-helpers.R (.al_nortest_battery).
                # ES: idénticas en todos los módulos, consolidadas en shared-helpers.R.
                .nt_x <- .al_nortest_battery(x)
                li <- .nt_x$li; ad <- .nt_x$ad; cvm <- .nt_x$cvm; sf <- .nt_x$sf

                if (!is.null(li)) {
                    add_normality(level, .("Lilliefors (corrected K-S)"), n_valid, "D", unname(li$statistic), li$p.value)
                } else {
                    add_normality(level, .("Lilliefors (corrected K-S)"), n_valid, "D", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_x$reasons$li, .))
                }

                if (!is.null(ad)) {
                    add_normality(level, "Anderson-Darling", n_valid, "A²", unname(ad$statistic), ad$p.value)
                } else {
                    add_normality(level, "Anderson-Darling", n_valid, "A²", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_x$reasons$ad, .))
                }

                if (!is.null(cvm)) {
                    add_normality(level, "Cram\u00e9r-von Mises", n_valid, "W²", unname(cvm$statistic), cvm$p.value)
                } else {
                    add_normality(level, "Cram\u00e9r-von Mises", n_valid, "W²", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_x$reasons$cvm, .))
                }

                if (!is.null(sf)) {
                    add_normality(level, "Shapiro-Francia", n_valid, "W'", unname(sf$statistic), sf$p.value)
                } else {
                    add_normality(level, "Shapiro-Francia", n_valid, "W'", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_x$reasons$sf, .))
                }

                pt <- .nt_x$pt
                if (!is.null(pt)) {
                    add_normality(level, .("Pearson chi-square"), n_valid, "P", unname(pt$statistic), pt$p.value)
                } else {
                    add_normality(level, .("Pearson chi-square"), n_valid, "P", NA_real_, NA_real_, reason = .al_norm_reason_text(.nt_x$reasons$pt, .))
                }

                if (!is.null(.nc_x$jb)) {
                    add_normality(level, "Jarque-Bera", n_valid, "JB", .nc_x$jb$value, .nc_x$jb$p)
                } else {
                    add_normality(level, "Jarque-Bera", n_valid, "JB", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_x$reasons$jb, .))
                }

                if (!is.null(.nc_x$skew)) {
                    add_normality(level, .("Skewness test"), n_valid, "z", .nc_x$skew$value, .nc_x$skew$p)
                } else {
                    add_normality(level, .("Skewness test"), n_valid, "z", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_x$reasons$skew, .))
                }

                if (!is.null(.nc_x$kurt)) {
                    add_normality(level, .("Kurtosis test"), n_valid, "z", .nc_x$kurt$value, .nc_x$kurt$p)
                } else {
                    add_normality(level, .("Kurtosis test"), n_valid, "z", NA_real_, NA_real_, reason = .al_norm_reason_text(.nc_x$reasons$kurt, .))
                }
            }

            normality_table <- do.call(rbind, normality_rows)

            row_counter <- 1

            for (level in group_levels) {

                sub_norm <- normality_table[normality_table$group == level, , drop = FALSE]

                for (j in seq_len(nrow(sub_norm))) {

                    group_label <- if (j == 1) as.character(level) else ""

                    row_key <- paste0("norm_", row_counter)
                    .al_add_row(self$results$normality, row_key,
                        values = list(
                            group = group_label,
                            test = sub_norm$test[j],
                            n = sub_norm$n[j],
                            statistic = sub_norm$statistic[j],
                            value = if (is.na(sub_norm$value[j])) "" else sub_norm$value[j],
                            p = if (is.na(sub_norm$p[j])) "" else sub_norm$p[j],
                            pSig = sub_norm$pSig[j],
                            decision = sub_norm$decision[j]
                        )
                    )
                    if (!is.na(sub_norm$reason[j]))
                        tryCatch(
                            self$results$normality$addFootnote(col = "p", note = sub_norm$reason[j], rowKey = row_key),
                            error = function(e) invisible(NULL)
                        )

                    row_counter <- row_counter + 1
                }
            }

            group_sig_counts <- c()
            group_valid_counts <- c()

            for (i in seq_along(group_levels)) {
                level <- group_levels[i]
                sub <- normality_table[normality_table$group == level, , drop = FALSE]
                significant <- sum(nzchar(as.character(sub$pSig)), na.rm = TRUE)
                valid_tests <- sum(as.character(sub$decision) != .("Not calc."), na.rm = TRUE)

                group_sig_counts[as.character(level)] <- significant
                group_valid_counts[as.character(level)] <- valid_tests

                conclusion <- if (significant == 0) {
                    .("Approx. normal")
                } else {
                    .("At least one sig. test")
                }

                .al_add_row(self$results$normalitySummary, paste0("normsum_", i),
                    values = list(
                        group = as.character(level),
                        significant = significant,
                        conclusion = conclusion
                    )
                )
            }

            list(shapiro_p_by_group = shapiro_p_by_group, normality_table = normality_table, group_sig_counts = group_sig_counts, group_valid_counts = group_valid_counts)
        },

        .checkHomogeneity = function(., clean_num, p_sig, variance_decision, block96, html_block, fmt_p, y2, g2, n_total, n_groups, shapiro_p_by_group, group_sig_counts, group_valid_counts) {
            # -----------------------------------------------------------------------------
            # Homogeneity-of-variance diagnostic battery.
            # ES: Batería diagnóstica de homogeneidad de varianzas.
            #
            # Runs Levene (mean- and median-centered), Brown-Forsythe, Bartlett, and
            # Fligner-Killeen across the complete-case sample. Levene (mean-centered)
            # is treated as the primary/anchor test in the applied interpretation
            # below because it is the classical, most widely recognized test for
            # equality of variances (Levene, 1960); the others are reported alongside
            # as robustness checks, since some are more sensitive to non-normality
            # (Bartlett) and others less so (Brown-Forsythe, Fligner-Killeen).
            #
            # ES: ejecuta Levene (centrado en media y en mediana), Brown-Forsythe,
            # Bartlett y Fligner-Killeen sobre la muestra de casos completos. Levene
            # (centrado en media) se trata como la prueba principal/ancla en la
            # interpretación aplicada de más abajo por ser la prueba clásica más
            # ampliamente reconocida para igualdad de varianzas (Levene, 1960); las
            # demás se reportan junto a esta como verificaciones de robustez, ya que
            # algunas son más sensibles a la no normalidad (Bartlett) y otras menos
            # (Brown-Forsythe, Fligner-Killeen).
            # -----------------------------------------------------------------------------
            complete_idx <- ! is.na(y2)
            y_complete <- y2[complete_idx]
            g_complete <- droplevels(g2[complete_idx])

            variance_by_group <- tapply(y_complete, g_complete, stats::var, na.rm = TRUE)
            variance_by_group <- variance_by_group[! is.na(variance_by_group)]

            variance_ratio <- if (length(variance_by_group) >= 2 && min(variance_by_group) > 0) {
                max(variance_by_group) / min(variance_by_group)
            } else {
                NA_real_
            }

            homogeneity_rows <- list()

            add_homogeneity <- function(test, statistic, value, df1, df2, p, use = NULL) {
                value <- clean_num(value)
                p <- clean_num(p)

                # `use` was accepted but never stored anywhere below - every
                # failure-reason string passed at the call sites (below) was
                # silently discarded, so a failed test showed a bare NA with
                # no explanation at all. Only kept as a row-level reason when
                # the test actually failed (p is NA) - on a successful row
                # `use` carries a general description of the test, not a
                # failure reason, and general descriptions belong in the
                # guide text, not a per-row footnote.
                # ES: `use` se aceptaba pero nunca se guardaba en ningún
                # lado - cada cadena de razón de fallo pasada en los sitios
                # de llamada (abajo) se descartaba en silencio, así que una
                # prueba fallida mostraba un NA desnudo sin explicación
                # alguna. Solo se conserva como razón a nivel de fila cuando
                # la prueba realmente falló (p es NA) - en una fila exitosa
                # `use` lleva una descripción general de la prueba, no una
                # razón de fallo, y las descripciones generales van en el
                # texto de guía, no en una nota al pie por fila.
                homogeneity_rows[[length(homogeneity_rows) + 1]] <<- data.frame(
                    test = test,
                    statistic = statistic,
                    value = value,
                    df1 = df1,
                    df2 = df2,
                    p = p,
                    pSig = p_sig(p),
                    decision = variance_decision(p),
                    reason = if (is.na(p) && !is.null(use)) use else NA_character_,
                    stringsAsFactors = FALSE
                )
            }

            if (length(unique(g_complete)) >= 2) {

                lev_median <- tryCatch(
                    car::leveneTest(y_complete, g_complete, center = median),
                    error = function(e) NULL
                )

                if (! is.null(lev_median)) {
                    add_homogeneity(
                        .("Levene (median-centered)"),
                        "F",
                        as.numeric(lev_median[1, "F value"]),
                        as.integer(lev_median[1, "Df"]),
                        as.integer(lev_median[2, "Df"]),
                        as.numeric(lev_median[1, "Pr(>F)"]),
                        .("General robust option; recommended for routine variance screening.")
                    )
                } else {
                    add_homogeneity(.("Levene (median-centered)"), "F", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Could not be computed - each group needs at least 2 observations for this test."))
                }

                bf <- tryCatch(
                    car::leveneTest(y_complete, g_complete, center = median),
                    error = function(e) NULL
                )

                if (! is.null(bf)) {
                    add_homogeneity(
                        "Brown-Forsythe",
                        "F",
                        as.numeric(bf[1, "F value"]),
                        as.integer(bf[1, "Df"]),
                        as.integer(bf[2, "Df"]),
                        as.numeric(bf[1, "Pr(>F)"]),
                        .("Median-centered Levene variant; useful when normality is doubtful.")
                    )
                } else {
                    add_homogeneity("Brown-Forsythe", "F", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Could not be computed - each group needs at least 2 observations for this test."))
                }

                lev_mean <- tryCatch(
                    car::leveneTest(y_complete, g_complete, center = mean),
                    error = function(e) NULL
                )

                if (! is.null(lev_mean)) {
                    add_homogeneity(
                        "Levene",
                        "F",
                        as.numeric(lev_mean[1, "F value"]),
                        as.integer(lev_mean[1, "Df"]),
                        as.integer(lev_mean[2, "Df"]),
                        as.numeric(lev_mean[1, "Pr(>F)"]),
                        .("Classical Levene form; more sensitive to non-normality than median-centered versions.")
                    )
                } else {
                    add_homogeneity("Levene", "F", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Could not be computed - each group needs at least 2 observations for this test."))
                }

            } else {
                add_homogeneity(.("Levene (median-centered)"), "F", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Requires at least two groups"))
                add_homogeneity("Brown-Forsythe", "F", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Requires at least two groups"))
                add_homogeneity("Levene", "F", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Requires at least two groups"))
            }

            if (length(unique(g_complete)) >= 2) {

                bart <- tryCatch(stats::bartlett.test(y_complete, g_complete), error = function(e) NULL)

                if (! is.null(bart)) {
                    add_homogeneity(
                        "Bartlett",
                        "K²",
                        unname(bart$statistic),
                        as.integer(unname(bart$parameter)),
                        NA_integer_,
                        bart$p.value,
                        .("Powerful when normality is plausible; sensitive to non-normality.")
                    )
                } else {
                    add_homogeneity("Bartlett", "K²", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Could not be computed - each group needs at least 2 observations for this test."))
                }

                flig <- tryCatch(stats::fligner.test(y_complete, g_complete), error = function(e) NULL)

                if (! is.null(flig)) {
                    add_homogeneity(
                        "Fligner-Killeen",
                        "χ²",
                        unname(flig$statistic),
                        as.integer(unname(flig$parameter)),
                        NA_integer_,
                        flig$p.value,
                        .("Non-parametric and robust; useful when normality is doubtful.")
                    )
                } else {
                    add_homogeneity("Fligner-Killeen", "χ²", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Could not be computed - each group needs at least 2 observations for this test."))
                }

                # -----------------------------------------------------------------------------
                # Applied-interpretation text: normality.
                # ES: Texto de interpretación aplicada: normalidad.
                #
                # Builds the narrative that anchors on Shapiro-Wilk (chosen for its
                # power across sample sizes, Razali & Wah, 2011) but also looks across
                # the full battery per group, distinguishing an isolated significant
                # test from a consistent multi-test signal of non-normality before
                # concluding anything.
                #
                # ES: construye la narrativa que se ancla en Shapiro-Wilk (elegida por
                # su potencia en todo el rango de tamaños muestrales, Razali & Wah,
                # 2011) pero que también revisa la batería completa por grupo,
                # distinguiendo una prueba significativa aislada de una señal
                # consistente entre varias pruebas antes de concluir cualquier cosa.
                # -----------------------------------------------------------------------------
                n_sig_shapiro <- sum(shapiro_p_by_group < .05, na.rm = TRUE)
                n_valid_shapiro <- sum(!is.na(shapiro_p_by_group))

                flagged_levels <- names(group_sig_counts)[group_sig_counts > 0]

                evidence_integration_text <- if (length(flagged_levels) == 0) {
                    .("Across the full battery of tests, no group accumulates more than an isolated significant result, which further supports treating the groups as approximately normal.")
                } else {
                    strong_levels <- flagged_levels[group_sig_counts[flagged_levels] >= 3]
                    weak_levels <- flagged_levels[group_sig_counts[flagged_levels] == 1]
                    parts <- c()

                    if (length(strong_levels) > 0) {
                        strong_desc <- paste0(strong_levels, " (", group_sig_counts[strong_levels], "/", group_valid_counts[strong_levels], ")", collapse = ", ")
                        parts <- c(parts, sprintf(
                            if (length(strong_levels) == 1)
                                .("Looking beyond Shapiro-Wilk alone at the full battery, %s accumulates agreement across several tests, a more consistent signal of non-normality.")
                            else
                                .("Looking beyond Shapiro-Wilk alone at the full battery, %s accumulate agreement across several tests, a more consistent signal of non-normality."),
                            strong_desc
                        ))
                    }

                    if (length(weak_levels) > 0) {
                        weak_desc <- paste(weak_levels, collapse = ", ")
                        parts <- c(parts, sprintf(.("For %s, only a single test out of the battery was significant, which is more consistent with an isolated, possibly spurious result than with a real departure from normality."), weak_desc))
                    }

                    if (length(parts) == 0) {
                        .("The flagged groups show a moderate number of significant tests, not a single isolated result nor a near-unanimous pattern; treat this as evidence worth noting but not decisive on its own.")
                    } else {
                        paste(parts, collapse = " ")
                    }
                }

                self$results$normalitySummaryInterpretation$setContent(html_block(
                    .("Applied Interpretation"),
                    block96(paste(
                        sprintf(.("%s of %s groups showed a significant Shapiro-Wilk deviation from normality (n = %s total, %s groups)."), n_sig_shapiro, n_valid_shapiro, n_total, n_groups),
                        if (n_sig_shapiro > 0 && n_total >= 200) {
                            .("Key message: the tests detect deviations, but with this sample size that does not automatically mean parametric tests are inadequate — large samples make Shapiro-Wilk detect even small, practically irrelevant departures from normality.")
                        } else {
                            ""
                        },
                        .("Shapiro-Wilk is used because it retains the highest statistical power among common normality tests across nearly the full range of sample sizes (Razali & Wah, 2011; see references). A widespread recommendation to prefer Kolmogorov-Smirnov once n grows is a leftover from old software limitations, not a statistical reason."),
                        evidence_integration_text,
                        .("Why it matters: normality by group mainly affects the precision of classical inference (confidence intervals, p-values) in small groups. With unequal or non-normal groups, Welch's t-test/ANOVA or a non-parametric alternative (Mann-Whitney, Kruskal-Wallis) is usually a safer default than the classical test."),
                        .("Nor should every single group be required to pass normality before trusting any comparison: with reasonably balanced group sizes and a moderate total n, group comparisons are often robust to mild non-normality, and the bigger concern is a large, asymmetric departure in a small group. This connects directly to sample size: Shapiro-Wilk becomes very sensitive with large groups (roughly n > 300 per group) and may lack power in small groups (n < 15) to detect real departures, so significance alone says little without also looking at group size."),
                        sep = "\n\n"
                    ))
                ))

                # -----------------------------------------------------------------------------
                # Applied-interpretation text: homogeneity of variance.
                # ES: Texto de interpretación aplicada: homogeneidad de varianzas.
                #
                # Anchors the narrative on Levene (mean-centered) and reports whether
                # the other tests in the table agree with its conclusion, then
                # explains the practical remedy (Welch's t-test/ANOVA) rather than
                # treating a significant result as a reason to discard the comparison.
                #
                # ES: ancla la narrativa en Levene (centrado en media) y reporta si las
                # demás pruebas de la tabla coinciden con su conclusión, y luego
                # explica el remedio práctico (t de Welch/ANOVA de Welch) en lugar de
                # tratar un resultado significativo como motivo para descartar la
                # comparación.
                # -----------------------------------------------------------------------------
                lev_mean_p <- tryCatch(as.numeric(lev_mean[1, "Pr(>F)"]), error = function(e) NA_real_)
                lev_median_p <- tryCatch(as.numeric(lev_median[1, "Pr(>F)"]), error = function(e) NA_real_)
                bart_p <- if (!is.null(bart)) bart$p.value else NA_real_
                flig_p <- if (!is.null(flig)) flig$p.value else NA_real_

                homog_coh_agree <- sum(c(lev_median_p, bart_p, flig_p) < .05, na.rm = TRUE)
                homog_coh_total <- sum(!is.na(c(lev_median_p, bart_p, flig_p)))
                homog_anchor_sig <- !is.na(lev_mean_p) && lev_mean_p < .05
                homog_coh_agree_dir <- sum((c(lev_median_p, bart_p, flig_p) < .05) == homog_anchor_sig, na.rm = TRUE)

                self$results$homogeneityInterpretation$setContent(html_block(
                    .("Applied Interpretation"),
                    block96(paste(
                        sprintf(.("Levene (primary test): p = %s (n = %s, %s groups)."), fmt_p(lev_mean_p), n_total, n_groups),
                        .("Levene's test is used as the primary test because it is the classical, most widely recognized test for equality of variances across groups (Levene, 1960; see references). Brown-Forsythe (median-centered) and Fligner-Killeen are more robust when normality is doubtful, and are reported alongside for comparison."),
                        if (homog_coh_total > 0) {
                            if (homog_coh_agree_dir == homog_coh_total) {
                                sprintf(.("The other %s test(s) in the table agree with this conclusion."), homog_coh_total)
                            } else {
                                sprintf(.("%s of the other %s tests in the table agree with this conclusion; check the table to see which one(s) differ."), homog_coh_agree_dir, homog_coh_total)
                            }
                        } else "",
                        .("Unequal variances (heteroscedasticity) mainly threaten the classical Student's t-test and standard ANOVA when group sizes are also unequal — with equal group sizes, these tests are fairly robust to moderate variance differences — which is why a significant Levene test is not, by itself, a reason to abandon the comparison entirely: the direct remedy is switching to Welch's t-test or Welch's ANOVA, which do not assume equal variances, rather than discarding the analysis."),
                        .("Sample-size caveat: with very unequal group sizes, even a moderate variance ratio can distort the classical test's Type I error rate; with equal group sizes, the classical test tolerates larger variance differences reasonably well."),
                        sep = "\n\n"
                    ))
                ))

            } else {
                add_homogeneity("Bartlett", "K²", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Requires at least two groups"))
                add_homogeneity("Fligner-Killeen", "χ²", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Requires at least two groups"))
            }

            # -----------------------------------------------------------------------------
            # Two-group variance F-test.
            # ES: Prueba F de varianza para dos grupos.
            #
            # Adds the classic two-sample F variance-ratio test only when there are
            # exactly two groups, since var.test() is defined for two samples; with
            # three or more groups the battery above (Levene/Brown-Forsythe/Bartlett/
            # Fligner-Killeen) already covers homogeneity of variance.
            #
            # ES: agrega la prueba F clásica de razón de varianzas para dos muestras
            # solo cuando hay exactamente dos grupos, ya que var.test() está definida
            # para dos muestras; con tres o más grupos, la batería anterior (Levene/
            # Brown-Forsythe/Bartlett/Fligner-Killeen) ya cubre la homogeneidad de
            # varianzas.
            # -----------------------------------------------------------------------------
            if (length(unique(g_complete)) == 2) {

                group_names <- levels(g_complete)
                group1Values <- y_complete[g_complete == group_names[1]]
                x2 <- y_complete[g_complete == group_names[2]]

                ftest <- tryCatch(stats::var.test(group1Values, x2), error = function(e) NULL)

                if (! is.null(ftest)) {
                    add_homogeneity(
                        "F test",
                        "F",
                        unname(ftest$statistic),
                        as.integer(ftest$parameter[1]),
                        as.integer(ftest$parameter[2]),
                        ftest$p.value,
                        .("Classic two-group variance test; sensitive to non-normality.")
                    )
                } else {
                    add_homogeneity("F test", "F", NA_real_, NA_integer_, NA_integer_, NA_real_, .("Could not be computed - each group needs at least 2 observations to estimate a variance."))
                }

            }

            homogeneity_rows
        },

        .setQQPlots = function(data, dep, group, ., txt, html_block) {
            # -----------------------------------------------------------------------------
            # Q-Q plot state preparation.
            # ES: Preparación del estado del gráfico Q-Q.
            #
            # Builds the long-format data used by .plotGroupQQNormality(): the global
            # (all valid cases) panel plus one panel per group, each included only
            # when it is methodologically viable (>= 10 valid cases and non-zero
            # variability) so an unstable Q-Q plot is never rendered.
            #
            # ES: construye los datos en formato largo que usa
            # .plotGroupQQNormality(): el panel global (todos los casos válidos) más
            # un panel por grupo, cada uno incluido solo cuando es metodológicamente
            # viable (>= 10 casos válidos y variabilidad mayor que cero), de modo que
            # nunca se renderice un Q-Q plot inestable.
            # -----------------------------------------------------------------------------
            # BEGIN AL_QQ_STATE
            qq_plot_state <- data.frame(
                value = numeric(0),
                group = character(0),
                stringsAsFactors = FALSE
            )

            show_qq_plots <- isTRUE(self$options$showQQPlots)


            if (show_qq_plots) {

                qq_y <- suppressWarnings(as.numeric(data[[dep]]))
                qq_g <- as.factor(data[[group]])

                qq_ok <- is.finite(qq_y) & ! is.na(qq_g)
                qq_y <- qq_y[qq_ok]
                qq_g <- droplevels(qq_g[qq_ok])

                qq_parts <- list()

                if (length(qq_y) >= 10 && stats::sd(qq_y, na.rm = TRUE) > 0) {
                    qq_parts[[length(qq_parts) + 1]] <- data.frame(
                        value = qq_y,
                        group = .("All valid cases"),
                        stringsAsFactors = FALSE
                    )
                }

                if (length(qq_y) >= 1 && length(levels(qq_g)) >= 1) {

                    for (level in levels(qq_g)) {

                        values <- qq_y[qq_g == level]
                        values <- values[is.finite(values)]

                        if (length(values) >= 10 && stats::sd(values, na.rm = TRUE) > 0) {
                            qq_parts[[length(qq_parts) + 1]] <- data.frame(
                                value = values,
                                group = as.character(level),
                                stringsAsFactors = FALSE
                            )
                        }
                    }
                }

                if (length(qq_parts) > 0) {
                    qq_plot_state <- do.call(rbind, qq_parts)
                    qq_plot_state$group <- factor(
                        qq_plot_state$group,
                        levels = unique(qq_plot_state$group)
                    )
                }
            }

            self$results$qqNormalityPlot$setState(qq_plot_state)
            # END AL_QQ_STATE

            # -----------------------------------------------------------------------------
            # Observed-vs-normal-curve state preparation.
            # ES: Preparación del estado de la curva observada vs normal.
            #
            # Mirrors the Q-Q state block above: a global panel plus one per group,
            # each standardized (z-scored) and included only when it is
            # methodologically viable (>= 20 valid cases and non-zero variability),
            # for the density-vs-theoretical-normal overlay plot.
            #
            # ES: refleja el bloque de estado Q-Q anterior: un panel global más uno
            # por grupo, cada uno estandarizado (puntuación z) e incluido solo cuando
            # es metodológicamente viable (>= 20 casos válidos y variabilidad mayor
            # que cero), para el gráfico de superposición de densidad observada vs
            # normal teórica.
            # -----------------------------------------------------------------------------
            # BEGIN AL_NORMAL_CURVE_STATE
            normal_curve_state <- data.frame(
                z = numeric(0),
                group = character(0),
                stringsAsFactors = FALSE
            )

            show_normal_curve_plots <- isTRUE(self$options$showNormalCurvePlots)


            if (show_normal_curve_plots) {

                nc_y <- suppressWarnings(as.numeric(data[[dep]]))
                nc_g <- as.factor(data[[group]])

                nc_ok <- is.finite(nc_y) & ! is.na(nc_g)
                nc_y <- nc_y[nc_ok]
                nc_g <- droplevels(nc_g[nc_ok])

                nc_parts <- list()

                if (length(nc_y) >= 20 && stats::sd(nc_y, na.rm = TRUE) > 0) {
                    nc_parts[[length(nc_parts) + 1]] <- data.frame(
                        z = as.numeric(scale(nc_y)),
                        group = .("All valid cases"),
                        stringsAsFactors = FALSE
                    )
                }

                if (length(nc_y) >= 1 && length(levels(nc_g)) >= 1) {

                    for (level in levels(nc_g)) {

                        values <- nc_y[nc_g == level]
                        values <- values[is.finite(values)]

                        if (length(values) >= 20 && stats::sd(values, na.rm = TRUE) > 0) {
                            nc_parts[[length(nc_parts) + 1]] <- data.frame(
                                z = as.numeric(scale(values)),
                                group = as.character(level),
                                stringsAsFactors = FALSE
                            )
                        }
                    }
                }

                if (length(nc_parts) > 0) {
                    normal_curve_state <- do.call(rbind, nc_parts)
                    normal_curve_state$group <- factor(
                        normal_curve_state$group,
                        levels = unique(normal_curve_state$group)
                    )
                }
            }

            self$results$normalCurvePlot$setState(normal_curve_state)
            # END AL_NORMAL_CURVE_STATE

            # -----------------------------------------------------------------------------
            # Visual guide texts: Q-Q and observed-vs-normal-curve plots.
            # ES: Textos de guía visual: gráficos Q-Q y de curva observada vs normal.
            # -----------------------------------------------------------------------------
            qq_plot_guide_text <- c(.("Q-Q plots compare the observed quantiles of each selected group with the quantiles expected under a normal distribution."), .("Points close to the reference line are compatible with approximate normality."), .("Systematic curvature, strong tail departures, or isolated points far from the line suggest that the normality assumption should be reviewed together with the numerical tests."), .("AssumptionsLab draws the global plot and every group-level plot that is methodologically viable. To avoid unstable visual interpretations, Q-Q plots require at least 10 valid cases and non-zero variability."))

            self$results$qqNormalityGuide$setContent(html_block(
                txt("common", "briefGuide"),
                .al_html_list(qq_plot_guide_text),
                raw = TRUE
            ))

            normal_curve_guide_text <- c(.("This plot compares the observed standardized distribution with the theoretical normal curve."), .("AssumptionsLab draws the global plot and every group-level plot that is methodologically viable."), .("To avoid unstable visual interpretations, observed-vs-normal curves require at least 20 valid cases and non-zero variability."), .("Clear asymmetry, heavy tails, multiple peaks, or strong separation from the theoretical curve suggest reviewing the normality assumption together with the Q-Q plots and numerical tests."))

            self$results$normalCurveGuide$setContent(html_block(
                txt("common", "briefGuide"),
                .al_html_list(normal_curve_guide_text),
                raw = TRUE
            ))

            invisible(NULL)
        },

        .fillHomogeneityTable = function(homogeneity_rows) {
            # -----------------------------------------------------------------------------
            # Homogeneity results table population.
            # ES: Llenado de la tabla de resultados de homogeneidad.
            # -----------------------------------------------------------------------------
            homogeneity_table <- do.call(rbind, homogeneity_rows)

            for (i in seq_len(nrow(homogeneity_table))) {
                row_key <- paste0("hom_", i)
                .al_add_row(self$results$homogeneity, row_key,
                    values = list(
                        test = homogeneity_table$test[i],
                        statistic = homogeneity_table$statistic[i],
                        value = if (is.na(homogeneity_table$value[i])) "" else homogeneity_table$value[i],
                        df1 = homogeneity_table$df1[i],
                        df2 = homogeneity_table$df2[i],
                        p = if (is.na(homogeneity_table$p[i])) "" else homogeneity_table$p[i],
                        pSig = homogeneity_table$pSig[i],
                        decision = homogeneity_table$decision[i]
                    )
                )
                if (!is.na(homogeneity_table$reason[i]))
                    tryCatch(
                        self$results$homogeneity$addFootnote(col = "p", note = homogeneity_table$reason[i], rowKey = row_key),
                        error = function(e) invisible(NULL)
                    )
            }

            homogeneity_table
        },

        .fillSummary = function(., txt, html_escape, html_block, group_levels, n_total, n_groups, outlier_table, n_model, n_any_flagged, normality_table, homogeneity_table) {
            # -----------------------------------------------------------------------------
            # Executive-summary evidence tally.
            # ES: Recuento de evidencia para el resumen ejecutivo.
            #
            # Consolidates outlier, normality, and homogeneity counts into the small
            # set of numbers the test recommendation and the checklist further below
            # are built from. normality_significant/homogeneity_significant are read
            # from the pSig column rather than a raw p < .05 comparison, since pSig is
            # itself derived from p and is what the printed tables actually show.
            #
            # ES: consolida los conteos de atípicos, normalidad y homogeneidad en el
            # pequeño conjunto de números sobre el que se construyen la recomendación
            # de prueba y la lista de verificación más adelante.
            # normality_significant/homogeneity_significant se leen de la columna
            # pSig en vez de comparar p < .05 directamente, ya que pSig se deriva del
            # propio p y es lo que las tablas impresas realmente muestran.
            # -----------------------------------------------------------------------------
            total_outliers <- sum(outlier_table$outliers, na.rm = TRUE)
            total_extreme <- sum(outlier_table$extreme, na.rm = TRUE)

            normality_group_note <- if (length(group_levels) >= 2) {
                sprintf(.("Normality table groups: Grupo 1 = %s; Grupo 2 = %s."), group_levels[1], group_levels[2])
            } else {
                ""
            }

            normality_significant <- sum(nzchar(as.character(normality_table$pSig)), na.rm = TRUE)
            homogeneity_significant <- sum(nzchar(as.character(homogeneity_table$pSig)), na.rm = TRUE)

            caution_sources <- character(0)

            if (normality_significant > 0)
                caution_sources <- c(caution_sources, .("normality diagnostics"))

            if (homogeneity_significant > 0)
                caution_sources <- c(caution_sources, .("variance homogeneity diagnostics"))

            if (total_extreme > 0)
                caution_sources <- c(caution_sources, .("extreme values"))

            caution_text <- if (length(caution_sources) == 0) {
                .("No major assumption warning was detected by the selected screening rules.")
            } else {
                sprintf(.("Caution is suggested because the following source(s) require review: %s."), paste(caution_sources, collapse = ", "))
            }

            # -----------------------------------------------------------------------------
            # Test-recommendation logic.
            # ES: Lógica de recomendación de prueba.
            #
            # Chooses a short recommended-test label from the two-group/more-than-two-
            # group design and the accumulated assumption evidence: no relevant
            # concerns favors the classical test, heterogeneous variances favors
            # Welch's variant, and otherwise the sample size (n >= 30, a conventional
            # rule-of-thumb for the central limit theorem to reasonably apply to the
            # sampling distribution of group means) decides whether the classical test
            # is still defensible or a mix of options should be weighed.
            #
            # ES: elige una etiqueta breve de prueba recomendada a partir del diseño
            # (dos grupos/más de dos grupos) y la evidencia de supuestos acumulada: sin
            # preocupaciones relevantes favorece la prueba clásica, varianzas
            # heterogéneas favorece la variante de Welch, y en otro caso el tamaño
            # muestral (n >= 30, una regla convencional para que el teorema central
            # del límite aplique razonablemente a la distribución muestral de las
            # medias de grupo) decide si la prueba clásica sigue siendo defendible o
            # si conviene ponderar varias opciones.
            # -----------------------------------------------------------------------------
            decision_label <- if (n_groups == 2) {
                if (homogeneity_significant == 0 && normality_significant == 0 && total_extreme == 0) {
                    .("Independent-samples t-test")
                } else if (homogeneity_significant > 0) {
                    .("Welch's t-test or a robust alternative")
                } else if (n_model >= 30) {
                    .("Independent-samples t-test (Welch's t-test and Mann-Whitney U as sensitivity checks)")
                } else {
                    .("Independent-samples t-test, Welch's t-test, or Mann-Whitney U depending on the analytic priority")
                }
            } else if (n_groups > 2) {
                if (homogeneity_significant == 0 && normality_significant == 0 && total_extreme == 0) {
                    .("One-way ANOVA")
                } else if (homogeneity_significant > 0) {
                    .("Welch ANOVA or a robust ANOVA alternative")
                } else if (n_model >= 30) {
                    .("One-way ANOVA (Welch ANOVA and Kruskal-Wallis as sensitivity checks)")
                } else {
                    .("One-way ANOVA, Welch ANOVA, or Kruskal-Wallis depending on the analytic priority")
                }
            } else {
                .("Not applicable")
            }

            evidence_title <- .("Evidence considered:")

            evidence_items <- c(sprintf(.("Detected groups: %s."), n_groups), sprintf(.("Normality tests with significant results: %s."), normality_significant), sprintf(.("Variance tests with significant results: %s."), homogeneity_significant), sprintf(.("Extreme values detected by the IQR rule: %s."), total_extreme))

            # -----------------------------------------------------------------------------
            # Narrative guidance text by design and evidence pattern.
            # ES: Texto narrativo de orientación según el diseño y el patrón de
            # evidencia.
            #
            # Expands decision_label into full applied-interpretation paragraphs,
            # mirroring the same four-way branching (design x evidence pattern) so the
            # notes panel below can present the recommendation with its full
            # methodological justification rather than only the short label.
            #
            # ES: expande decision_label en párrafos completos de interpretación
            # aplicada, siguiendo la misma ramificación en cuatro vías (diseño x patrón
            # de evidencia), de modo que el panel de notas más abajo pueda presentar
            # la recomendación con su justificación metodológica completa y no solo
            # con la etiqueta breve.
            # -----------------------------------------------------------------------------
            guidance <- if (n_groups == 2) {

                if (homogeneity_significant == 0 && normality_significant == 0 && total_extreme == 0) {
                    c(.("The available evidence is compatible with a parametric comparison between two independent groups."), .("Because the variance tests do not show significant heterogeneity and the normality diagnostics do not show relevant deviations, the independent-samples t-test is a defensible primary option."), .("Welch's t-test may still be reported as a robustness check, especially when group sizes are unequal."), .("The final decision should also consider the study design, measurement scale, sample size, and the consequences of using a parametric or robust strategy."))
                } else if (homogeneity_significant > 0) {
                    c(.("The evidence suggests that the equal-variance assumption may not be fully defensible."), .("In this situation, Welch's t-test is usually preferable to the classic equal-variance t-test because it adjusts the inference for unequal variances."), .("A non-parametric alternative such as Mann-Whitney U may be considered if distributional concerns are also important or if the interpretation focuses on ranks rather than means."), .("Do not choose the test mechanically: report the evidence, explain the assumption concern, and justify the selected strategy."))
                } else {
                    if (n_model >= 30) {
                        c(sprintf(.("The independent-samples t-test is defensible for this comparison given the sample size (n = %s) and the homogeneous variances observed; by the central limit theorem, the sampling distribution of the group means is approximately normal at this size even when individual normality tests flag deviations."), n_model), caution_text, .("As a sensitivity check, Welch's t-test and Mann-Whitney U can additionally be reported; a comparable conclusion across all three would reinforce the result."))
                    } else {
                    c(.("The variance assumption appears acceptable, but at least one diagnostic suggests that the distributional evidence should be reviewed carefully."), caution_text, .("A parametric comparison may still be defensible when deviations are limited, the sample size is adequate, and the substantive interpretation focuses on group means."), .("Welch's t-test is a useful robust parametric option. Mann-Whitney U is appropriate when the analyst prioritizes a non-parametric interpretation or when distributional concerns are considered substantively important."))
                    }
                }

            } else if (n_groups > 2) {

                if (homogeneity_significant == 0 && normality_significant == 0 && total_extreme == 0) {
                    c(.("The available evidence is compatible with a parametric comparison among independent groups."), .("Because the normality and variance diagnostics are acceptable, one-way ANOVA is a defensible primary option."), .("Robust or non-parametric alternatives may still be reported as sensitivity analyses when the research context requires additional caution."), .("The final decision should connect the statistical evidence with the research design and the intended interpretation of group differences."))
                } else if (homogeneity_significant > 0) {
                    c(.("The evidence suggests that the equal-variance assumption may not be fully defensible."), .("Welch ANOVA is usually preferable to classic one-way ANOVA when variances are heterogeneous, especially with unequal group sizes."), .("A robust ANOVA procedure may also be considered, depending on the research question and available methods."), .("The report should explain that the choice is based on the observed variance evidence rather than on a purely automatic rule."))
                } else {
                    if (n_model >= 30) {
                        c(sprintf(.("One-way ANOVA is defensible for this comparison given the sample size (n = %s) and the homogeneous variances observed; by the central limit theorem, the sampling distribution of the group means is approximately normal at this size even when individual normality tests flag deviations."), n_model), caution_text, .("As a sensitivity check, Welch ANOVA and Kruskal-Wallis can additionally be reported; a comparable conclusion across all three would reinforce the result."))
                    } else {
                    c(.("The variance assumption appears acceptable, but at least one diagnostic suggests that the distributional evidence should be reviewed carefully."), caution_text, .("One-way ANOVA may still be defensible when deviations are limited and sample sizes are adequate."), .("Welch ANOVA provides a robust parametric alternative. Kruskal-Wallis is appropriate when the analyst prioritizes a non-parametric comparison."))
                    }
                }

            } else {
                .("A methodological decision cannot be suggested because fewer than two groups were detected.")
            }

            # -----------------------------------------------------------------------------
            # Executive summary assembly.
            # ES: Ensamblaje del resumen ejecutivo.
            #
            # Builds the short checklist (outliers, influence, normality,
            # homogeneity) and final verdict line shown at the top-level
            # "Methodological Conclusion" panel, giving the reader a one-glance
            # summary before the full report detail.
            #
            # ES: construye la lista de verificaci\u00f3n breve (at\u00edpicos, influencia,
            # normalidad, homogeneidad) y la l\u00ednea de veredicto final que se muestra
            # en el panel de nivel superior "Conclusi\u00f3n Metodol\u00f3gica", dando al
            # lector un resumen de un vistazo antes del detalle completo del informe.
            # -----------------------------------------------------------------------------
            check_mark <- "\u2713"

            outlier_line <- if (total_outliers == 0 && total_extreme == 0) {
                sprintf(.("%s No outliers were detected by the IQR rule."), check_mark)
            } else {
                sprintf(.("%s %s outlier(s) and %s extreme value(s) were detected by the IQR rule — worth a quick look, not automatically a problem."), check_mark, total_outliers, total_extreme)
            }

            influence_line <- if (n_any_flagged == 0) {
                sprintf(.("%s No potentially influential cases were flagged (Cook's D / Mahalanobis)."), check_mark)
            } else {
                sprintf(.("%s %s potentially influential case(s) were flagged and are worth reviewing."), check_mark, n_any_flagged)
            }

            normality_line <- if (normality_significant == 0) {
                sprintf(.("%s Normality shows no relevant deviations."), check_mark)
            } else if (n_total >= 200) {
                sprintf(.("%s Normality shows deviations expected at this sample size, not necessarily a practical problem."), check_mark)
            } else {
                sprintf(.("%s Normality shows deviations that should be weighed in the decision."), check_mark)
            }

            homogeneity_line <- if (homogeneity_significant == 0) {
                sprintf(.("%s Variance homogeneity is satisfied."), check_mark)
            } else {
                sprintf(.("%s Variance homogeneity is not fully satisfied."), check_mark)
            }

            verdict_line <- sprintf(.("The comparison using %s is methodologically defensible."), decision_label)

            self$results$executiveSummary$setContent(html_block(
                .("Methodological Conclusion"),
                paste(
                    outlier_line,
                    influence_line,
                    normality_line,
                    homogeneity_line,
                    "",
                    verdict_line,
                    sep = "\n"
                ),
                paragraphs = FALSE
            ))

            # -----------------------------------------------------------------------------
            # Notes and recommendation panel.
            # ES: Panel de notas y recomendación.
            #
            # Assembles the final report section: the suggested decision label, the
            # evidence it is based on, and the full narrative guidance text, followed
            # by the shared significance-code and statistical-symbol legends so the
            # report is self-contained.
            #
            # ES: ensambla la sección final del informe: la decisión sugerida, la
            # evidencia en que se basa y el texto narrativo completo de orientación,
            # seguidos de las leyendas compartidas de códigos de significancia y
            # símbolos estadísticos, para que el informe sea autocontenido.
            # -----------------------------------------------------------------------------
            self$results$notes$setContent(html_block(
                .("Notes and recommendation"),
                paste0(
                    paste0(
                        "<p style=\"margin: 0.15em 0 0.65em 0; line-height: 1.40;\">",
                        html_escape(.al_clean_text(c(
                            sprintf(.("Suggested decision: %s."), decision_label),
                            evidence_title
                        ))),
                        "</p>",
                        collapse = "\n"
                    ),
                    "\n",
                    .al_html_list(evidence_items),
                    "\n",
                    paste0(
                        "<p style=\"margin: 0.15em 0 0.65em 0; line-height: 1.40;\">",
                        html_escape(.al_clean_text(c(guidance, txt("common", "sigCodes"), txt("common", "statsSymbols")))),
                        "</p>",
                        collapse = "\n"
                    )
                ),
                raw = TRUE
            ))

            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # Plot theme/palette and language helpers shared by every plotting callback.
        # Migrated to jamovi's native ggtheme/theme mechanism - see
        # logcheck.b.R's .plotColors()/.plotSeriesColors() (the pilot for
        # this change, 2026-09-17) for the full rationale. groupCheck's own
        # plots are drawn with base R graphics (graphics::boxplot/plot/
        # points), not ggplot2, so only jamovi's `theme` (a plain
        # $color/$fill/$palette/$bw list) is usable here - `ggtheme` (a list
        # of ggplot2 scale/theme objects) has no base-R equivalent and is
        # left unused in every render function's signature. The N-group
        # coloring jamovi normally gives ggplot2 plots "for free" via
        # ggtheme's embedded discrete scale is replicated here by calling
        # the same underlying jmvcore::colorPalette() jamovi itself uses.
        #
        # ES: Tema/paleta de gráficos e idioma compartidos por cada función
        # de graficado. Migrado al mecanismo nativo ggtheme/theme de jamovi
        # - ver .plotColors()/.plotSeriesColors() en logcheck.b.R (el
        # piloto de este cambio, 2026-09-17) para el razonamiento completo.
        # Los propios gráficos de groupCheck se dibujan con gráficos base
        # de R (graphics::boxplot/plot/points), no ggplot2, así que solo es
        # utilizable el `theme` de jamovi (una lista simple
        # $color/$fill/$palette/$bw) - `ggtheme` (una lista de objetos de
        # escala/tema de ggplot2) no tiene equivalente en gráficos base de
        # R y queda sin usar en la firma de cada función de graficado. El
        # coloreado por N grupos que jamovi normalmente da "gratis" a los
        # gráficos ggplot2 vía la escala discreta embebida en ggtheme se
        # replica aquí llamando al mismo jmvcore::colorPalette() que usa
        # jamovi internamente.
        # -----------------------------------------------------------------------------

        .plotColors = function(theme) {
            base_color <- if (!is.null(theme$color) && length(theme$color) >= 1) theme$color[1] else "#333333"
            accent_color <- if (!is.null(theme$color) && length(theme$color) >= 2) theme$color[2] else "#2C7FB8"
            accent_fill <- if (!is.null(theme$fill) && length(theme$fill) >= 2) theme$fill[2] else "#A6CEE3"

            list(
                point = base_color, line = base_color, ref = "gray50",
                alert = accent_color, smooth = accent_color, fill = accent_fill,
                box_border = base_color, box_fill = "#F2F2F2", violin_fill = accent_fill,
                mean = accent_color, median = base_color, axis = base_color, grid = "#E6E6E6"
            )
        },

        .plotSeriesColors = function(n) {
            n <- max(1, as.integer(n))
            # Always replicate jamovi's own N-color scheme (the same
            # function ggtheme's embedded discrete scale calls
            # internally for ggplot2 plots) instead of a fixed hard-
            # coded blue/orange set.
            # ES: replica siempre el esquema de N colores propio
            # de jamovi (la misma función que llama internamente la
            # escala discreta embebida en ggtheme para gráficos
            # ggplot2) en vez de un conjunto azul/naranja fijo.
            jmv_pal <- tryCatch(self$options$palette, error = function(e) "jmv")
            if (is.null(jmv_pal) || length(jmv_pal) == 0 || !nzchar(jmv_pal)) jmv_pal <- "jmv"
            return(tryCatch(jmvcore::colorPalette(n, jmv_pal, "fill"), error = function(e) NULL))
        },

        # -----------------------------------------------------------------------------
        # Distribution (boxplot/violin) plot renderer.
        # ES: Renderizador del gráfico de distribución (boxplot/violin).
        #
        # Draws the by-group distribution plot from the state built earlier in
        # .run() (distribution_plot_state): a boxplot per group with optional violin-
        # density, jittered points, and mean-marker layers, colored per the user's
        # plot-appearance options. A group needs at least 3 valid cases to receive a
        # boxplot, since fewer points cannot support a meaningful quartile display.
        #
        # ES: dibuja el gráfico de distribución por grupo a partir del estado
        # construido antes en .run() (distribution_plot_state): un boxplot por grupo
        # con capas opcionales de densidad tipo violin, puntos con jitter y marca de
        # media, coloreado según las opciones de apariencia del usuario. Un grupo
        # necesita al menos 3 casos válidos para recibir un boxplot, ya que con menos
        # puntos no se puede sostener una visualización de cuartiles significativa.
        # -----------------------------------------------------------------------------
        .plotGroupDistribution = function(image, theme, ...) {
            . <- .al_lang(self$options)$tr

            if (!isTRUE(self$options$showDistributionPlot))
                return(invisible(TRUE))

            pal <- private$.plotColors(theme)

            draw_message <- function(msg) {
                graphics::plot.new()
                graphics::text(
                    x = 0.5,
                    y = 0.5,
                    labels = msg,
                    cex = 0.90,
                    col = pal$axis)
                invisible(TRUE)
            }

            d <- image$state

            if (is.null(d) || !is.data.frame(d) || nrow(d) == 0)
                return(draw_message(.("No valid cases were available for the distribution plot.")))

            d$value <- suppressWarnings(as.numeric(d$value))
            d$group <- as.character(d$group)
            d <- d[is.finite(d$value) & !is.na(d$group), , drop = FALSE]

            if (nrow(d) == 0)
                return(draw_message(.("No valid cases were available for the distribution plot.")))

            dep <- if ("depLabel" %in% names(d) && length(unique(d$depLabel)) > 0) {
                as.character(unique(d$depLabel)[1])
            } else {
                .("Analyzed variable")
            }

            groups <- unique(d$group)
            values_by_group <- lapply(groups, function(level) {
                values <- d$value[d$group == level]
                values[is.finite(values)]
            })
            names(values_by_group) <- groups

            n_by_group <- vapply(values_by_group, length, integer(1))
            box_viable <- n_by_group >= 3

            if (!any(box_viable))
                return(draw_message(.("No boxplot was generated. At least one group with 3 valid cases is required.")))

            values_by_group <- values_by_group[box_viable]
            groups <- names(values_by_group)
            n_by_group <- n_by_group[box_viable]
            n_groups <- length(values_by_group)

            all_values <- unlist(values_by_group, use.names = FALSE)
            y_range <- range(all_values, finite = TRUE)
            if (!all(is.finite(y_range)) || diff(y_range) == 0) {
                y_range <- y_range + c(-0.5, 0.5)
            } else {
                pad <- diff(y_range) * 0.08
                y_range <- y_range + c(-pad, pad)
            }

            old_par <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(old_par), add = TRUE)

            bottom_margin <- if (n_groups > 5) 6.0 else 4.8
            graphics::par(
                mar = c(bottom_margin, 4.8, 3.3, 1.4),
                xpd = FALSE)

            graphics::plot(
                x = seq_len(n_groups),
                y = rep(NA_real_, n_groups),
                type = "n",
                xlim = c(0.5, n_groups + 0.5),
                ylim = y_range,
                xaxt = "n",
                xlab = "",
                ylab = dep,
                main = .("Distribution of the analyzed variable by group"),
                col.axis = pal$axis,
                col.lab = pal$axis,
                col.main = pal$axis)

            graphics::grid(nx = NA, ny = NULL, col = pal$grid, lty = 1)
            graphics::axis(
                side = 1,
                at = seq_len(n_groups),
                labels = groups,
                las = if (n_groups > 4) 2 else 1,
                col.axis = pal$axis,
                col = pal$axis)

            group_cols <- if (isTRUE(self$options$distColorByGroup)) {
                cols <- private$.plotSeriesColors(n_groups)
                if (is.null(cols)) rep(pal$box_fill, n_groups) else cols
            } else {
                rep(pal$box_fill, n_groups)
            }

            if (isTRUE(self$options$addViolinPlot)) {
                for (i in seq_along(values_by_group)) {
                    values <- values_by_group[[i]]
                    sdv <- stats::sd(values, na.rm = TRUE)
                    if (length(values) >= 20 && is.finite(sdv) && sdv > 0) {
                        den <- tryCatch(stats::density(values, na.rm = TRUE), error = function(e) NULL)
                        if (!is.null(den) && length(den$x) > 1 && max(den$y) > 0) {
                            width <- 0.34 * den$y / max(den$y)
                            graphics::polygon(
                                x = c(i - width, rev(i + width)),
                                y = c(den$x, rev(den$x)),
                                border = pal$box_border,
                                col = grDevices::adjustcolor(group_cols[i], alpha.f = 0.45))
                        }
                    }
                }
            }

            graphics::boxplot(
                values_by_group,
                at = seq_len(n_groups),
                add = TRUE,
                axes = FALSE,
                names = FALSE,
                outline = TRUE,
                boxwex = 0.32,
                border = pal$box_border,
                col = grDevices::adjustcolor(group_cols, alpha.f = 0.90),
                medcol = pal$median,
                whiskcol = pal$box_border,
                staplecol = pal$box_border,
                outcol = pal$point,
                outpch = 16,
                outcex = 0.65)

            if (isTRUE(self$options$showJitterPoints)) {
                .al_local_seed(2026)
                for (i in seq_along(values_by_group)) {
                    values <- values_by_group[[i]]
                    xj <- stats::runif(length(values), min = i - 0.16, max = i + 0.16)
                    graphics::points(
                        xj,
                        values,
                        pch = 16,
                        cex = 0.48,
                        col = grDevices::adjustcolor(group_cols[i], alpha.f = 0.55))
                }
            }

            if (isTRUE(self$options$showMeanMarker)) {
                for (i in seq_along(values_by_group)) {
                    values <- values_by_group[[i]]
                    if (length(values) >= 2) {
                        m <- mean(values, na.rm = TRUE)
                        if (is.finite(m)) {
                            graphics::points(
                                i,
                                m,
                                pch = 23,
                                bg = grDevices::adjustcolor(pal$mean, alpha.f = 0.80),
                                col = pal$mean,
                                cex = 1.05)
                        }
                    }
                }
            }

            n_labels <- paste0("n=", n_by_group)
            y_label_pos <- y_range[1] + diff(y_range) * 0.03
            graphics::text(
                x = seq_len(n_groups),
                y = y_label_pos,
                labels = n_labels,
                cex = 0.72,
                col = pal$axis)

            legend_items <- c(
                .("Boxplot: median and IQR"))

            legend_pch <- c(15)
            legend_col <- c(pal$box_border)
            legend_pt_bg <- c(grDevices::adjustcolor(pal$box_fill, alpha.f = 0.90))

            if (isTRUE(self$options$addViolinPlot)) {
                legend_items <- c(legend_items, .("Violin: approximate shape"))
                legend_pch <- c(legend_pch, 15)
                legend_col <- c(legend_col, pal$box_border)
                legend_pt_bg <- c(legend_pt_bg, grDevices::adjustcolor(pal$violin_fill, alpha.f = 0.55))
            }

            if (isTRUE(self$options$showJitterPoints)) {
                legend_items <- c(legend_items, .("Points: observed cases"))
                legend_pch <- c(legend_pch, 16)
                legend_col <- c(legend_col, grDevices::adjustcolor(pal$point, alpha.f = 0.65))
                legend_pt_bg <- c(legend_pt_bg, NA)
            }

            if (isTRUE(self$options$showMeanMarker)) {
                legend_items <- c(legend_items, .("Diamond: mean"))
                legend_pch <- c(legend_pch, 23)
                legend_col <- c(legend_col, pal$mean)
                legend_pt_bg <- c(legend_pt_bg, grDevices::adjustcolor(pal$mean, alpha.f = 0.80))
            }

            graphics::legend(
                "topright",
                legend = legend_items,
                pch = legend_pch,
                col = legend_col,
                pt.bg = legend_pt_bg,
                bty = "n",
                cex = 0.72)

            invisible(TRUE)
        },

        # -----------------------------------------------------------------------------
        # Q-Q normality plot renderer.
        # ES: Renderizador del gráfico Q-Q de normalidad.
        #
        # Draws one Q-Q panel per entry in the state built by the "Q-Q plot state
        # preparation" block in .run() (global panel plus one per viable group),
        # laid out in a grid. Optionally overlays a 95% pointwise confidence band
        # around the reference line and flags |z| > 2.5 points as potential
        # outliers, purely as a visual aid alongside the numerical diagnostics.
        #
        # ES: dibuja un panel Q-Q por cada entrada del estado construido en el
        # bloque "Preparación del estado del gráfico Q-Q" de .run() (panel global
        # más uno por cada grupo viable), dispuestos en una cuadrícula. Opcionalmente
        # superpone una banda de confianza puntual del 95% alrededor de la línea de
        # referencia y marca los puntos con |z| > 2.5 como atípicos potenciales,
        # puramente como apoyo visual junto a los diagnósticos numéricos.
        # -----------------------------------------------------------------------------
        .plotGroupQQNormality = function(image, theme, ...) {
            . <- .al_lang(self$options)$tr

            d <- image$state

            pal <- private$.plotColors(theme)

            draw_message <- function(msg) {
                graphics::plot.new()
                graphics::text(
                    x = 0.5,
                    y = 0.5,
                    labels = msg,
                    cex = 0.9,
                    col = pal$line
                )
                TRUE
            }

            if (is.null(d) || ! is.data.frame(d) || nrow(d) == 0) {
                return(draw_message(.("No Q-Q plot was generated. At least 10 valid cases and non-zero variability are required.")))
            }

            d$value <- suppressWarnings(as.numeric(d$value))
            d$group <- as.character(d$group)

            d <- d[is.finite(d$value) & ! is.na(d$group), , drop = FALSE]

            if (nrow(d) < 10) {
                return(draw_message(.("No Q-Q plot was generated. At least 10 valid cases are required.")))
            }

            groups <- unique(d$group)
            n_panels <- length(groups)

            if (n_panels < 1) {
                return(draw_message(.("No valid groups were available for Q-Q plots.")))
            }

            n_col <- if (n_panels <= 2) n_panels else 2
            n_row <- ceiling(n_panels / n_col)

            old_par <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(old_par), add = TRUE)

            graphics::par(
                mfrow = c(n_row, n_col),
                mar = c(4.1, 4.1, 2.6, 1.2),
                oma = c(0, 0, 0, 0)
            )

            for (g in groups) {

                values <- d$value[d$group == g]
                values <- values[is.finite(values)]

                if (length(values) < 10 || stats::sd(values, na.rm = TRUE) <= 0) {
                    graphics::plot.new()
                    graphics::title(main = g, col.main = pal$line)
                    graphics::text(
                        0.5,
                        0.5,
                        labels = .("Not enough valid cases"),
                        cex = 0.85,
                        col = pal$line
                    )
                } else {
                    qq_res <- stats::qqnorm(values, plot.it = FALSE)
                    m <- mean(values, na.rm = TRUE)
                    sdv <- stats::sd(values, na.rm = TRUE)

                    graphics::plot(
                        qq_res$x,
                        qq_res$y,
                        type = "n",
                        main = g,
                        xlab = .("Theoretical quantiles"),
                        ylab = .("Observed quantiles"),
                        col.main = pal$line,
                        col.lab = pal$line,
                        col.axis = pal$line
                    )

                    if (isTRUE(self$options$qqShowBand)) {
                        n_i <- length(values)
                        z_ord <- sort(qq_res$x)
                        fitted_line <- m + sdv * z_ord
                        se <- (sdv / stats::dnorm(z_ord)) *
                            sqrt(stats::pnorm(z_ord) * (1 - stats::pnorm(z_ord)) / n_i)
                        se[!is.finite(se)] <- 0
                        upper <- fitted_line + 1.96 * se
                        lower <- fitted_line - 1.96 * se
                        graphics::polygon(
                            c(z_ord, rev(z_ord)),
                            c(upper, rev(lower)),
                            col = grDevices::adjustcolor(pal$ref, alpha.f = 0.18),
                            border = NA
                        )
                    }

                    z_scores <- if (sdv > 0) (values - m) / sdv else rep(0, length(values))
                    is_extreme <- isTRUE(self$options$qqFlagOutliers) & abs(z_scores) > 2.5

                    graphics::points(
                        qq_res$x[!is_extreme],
                        qq_res$y[!is_extreme],
                        pch = 19,
                        cex = 0.65,
                        col = pal$point
                    )
                    if (any(is_extreme)) {
                        graphics::points(
                            qq_res$x[is_extreme],
                            qq_res$y[is_extreme],
                            pch = 17,
                            cex = 0.85,
                            col = pal$alert
                        )
                    }

                    stats::qqline(values, lwd = 1.2, col = pal$line)
                }
            }

            TRUE
        },

        # -----------------------------------------------------------------------------
        # Observed-vs-normal-curve plot renderer.
        # ES: Renderizador del gráfico de curva observada vs normal.
        #
        # Draws, per panel in the state built by the "Observed-vs-normal-curve state
        # preparation" block in .run(), the standardized observed density (kernel
        # density estimate, user-adjustable bandwidth) overlaid on the theoretical
        # standard-normal curve, so shape departures (skew, heavy tails, multiple
        # modes) are visible directly rather than only through summary statistics.
        #
        # ES: dibuja, por cada panel del estado construido en el bloque
        # "Preparación del estado de la curva observada vs normal" de .run(), la
        # densidad observada estandarizada (estimación de densidad kernel, ancho de
        # banda ajustable por el usuario) superpuesta a la curva normal estándar
        # teórica, de modo que las desviaciones de forma (asimetría, colas pesadas,
        # múltiples modas) sean visibles directamente y no solo a través de
        # estadísticos resumen.
        # -----------------------------------------------------------------------------
        .plotGroupNormalCurve = function(image, theme, ...) {
            . <- .al_lang(self$options)$tr

            d <- image$state

            pal <- private$.plotColors(theme)

            draw_message <- function(msg) {
                graphics::plot.new()
                graphics::text(
                    x = 0.5,
                    y = 0.5,
                    labels = msg,
                    cex = 0.9,
                    col = pal$line
                )
                TRUE
            }

            if (is.null(d) || ! is.data.frame(d) || nrow(d) == 0) {
                return(draw_message(.("No observed-vs-normal curve was generated. At least 20 valid cases and non-zero variability are required.")))
            }

            d$z <- suppressWarnings(as.numeric(d$z))
            d$group <- as.character(d$group)

            d <- d[is.finite(d$z) & ! is.na(d$group), , drop = FALSE]

            if (nrow(d) < 20) {
                return(draw_message(.("No observed-vs-normal curve was generated. At least 20 valid cases are required.")))
            }

            groups <- unique(d$group)
            n_panels <- length(groups)

            if (n_panels < 1) {
                return(draw_message(.("No valid groups were available for observed-vs-normal curves.")))
            }

            n_col <- if (n_panels <= 2) n_panels else 2
            n_row <- ceiling(n_panels / n_col)

            old_par <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(old_par), add = TRUE)

            graphics::par(
                mfrow = c(n_row, n_col),
                mar = c(4.1, 4.1, 2.6, 1.2),
                oma = c(0, 0, 0, 0)
            )

            for (g in groups) {

                z <- d$z[d$group == g]
                z <- z[is.finite(z)]

                if (length(z) < 20 || stats::sd(z, na.rm = TRUE) <= 0) {
                    graphics::plot.new()
                    graphics::title(main = g, col.main = pal$line)
                    graphics::text(
                        0.5,
                        0.5,
                        labels = .("Not enough valid cases"),
                        cex = 0.85,
                        col = pal$line
                    )
                } else {
                    bw_adjust <- tryCatch(as.numeric(self$options$curveBandwidth), error = function(e) 1)
                    if (is.null(bw_adjust) || length(bw_adjust) == 0 || !is.finite(bw_adjust) || bw_adjust <= 0)
                        bw_adjust <- 1

                    den <- stats::density(z, adjust = bw_adjust, na.rm = TRUE)

                    graphics::plot(
                        den,
                        main = g,
                        xlab = .("Standardized values"),
                        ylab = .("Density"),
                        xlim = c(-4, 4),
                        lwd = 1.5,
                        col = pal$smooth,
                        col.main = pal$line,
                        col.lab = pal$line,
                        col.axis = pal$line
                    )

                    graphics::curve(
                        stats::dnorm(x),
                        from = -4,
                        to = 4,
                        add = TRUE,
                        lty = 2,
                        lwd = 1.2,
                        col = pal$alert
                    )

                    graphics::abline(v = 0, lty = 3, lwd = 1, col = pal$ref)

                    if (isTRUE(self$options$curveShowRug)) {
                        graphics::rug(z, col = grDevices::adjustcolor(pal$line, alpha.f = 0.55))
                    }

                    graphics::legend(
                        "topright",
                        legend = c(
                            .("Observed density"),
                            .("Theoretical normal")
                        ),
                        col = c(pal$smooth, pal$alert),
                        lty = c(1, 2),
                        lwd = c(1.5, 1.2),
                        bty = "n",
                        cex = 0.75
                    )
                }
            }

            TRUE
        }
)

)
