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
# palette helpers, and the general .run()/.plotPalette()/
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
# .run()/.plotPalette()/.plotTheme().
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
                            table$addColumn(name = colName, title = as.character(i), type = "number"),
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
            lang <- .al_lang(self$options)

            . <- lang$tr

            html_escape <- .al_html_escape

            html_block <- .al_html_block

            html_guide <- function(title, items) {
                html_block(title, .al_html_list(items), raw = TRUE)
            }

            clean_num <- .al_clean_num

            fmt_num <- function(x, digits = 3) {
                x <- clean_num(x)
                if (is.na(x))
                    return(.("Not computed"))
                format(round(x, digits), nsmall = digits)
            }

            p_sig <- .al_p_sig

            # add_row()/qname(): identical logic in every module that had
            # them, consolidated in shared-helpers.R (.al_add_row/
            # .al_qname) - see that file for the full rationale (qname() is
            # the fix for the formula-breaking-on-spaces crash jamovi's own
            # module review reported, Sep 2026).
            # ES: lógica idéntica en cada módulo que las tenía, consolidada
            # en shared-helpers.R (.al_add_row/.al_qname) - ver ese archivo
            # para el razonamiento completo (qname() es el arreglo para el
            # choque de fórmula-rota-con-espacios que reportó la revisión
            # oficial del módulo de jamovi, sep. 2026).
            add_row <- .al_add_row
            qname <- .al_qname

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





            # -----------------------------------------------------------------------------
            # Data / dependent-variable checks.
            # ES: Datos / verificaciones de la variable dependiente.
            # -----------------------------------------------------------------------------
            dep_var <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors

            data <- self$data
            if (is.null(data) || nrow(data) == 0) {
                jmvcore::reject(.("No data are available."))
            }

            if (is.null(dep_var) || !nzchar(dep_var) || !(dep_var %in% names(data))) {
                jmvcore::reject(.("Dependent variable not selected."))
            }

            dep_raw <- data[[dep_var]]
            if (!is.factor(dep_raw)) {
                jmvcore::reject(.("The dependent variable must be an ordinal (ordered factor) variable."))
            }

            dep_levels <- levels(droplevels(dep_raw))
            n_levels <- length(dep_levels)

            if (n_levels < 3) {
                jmvcore::reject(.("The dependent variable has fewer than 3 categories. Use logCheck for a binary (2-category) dependent variable instead - it gives richer binary-specific diagnostics (ROC/AUC, Hosmer-Lemeshow, odds ratios)."))
            }

            predictors <- c(covs, factors)
            if (length(predictors) == 0) {
                jmvcore::reject(.("Select at least one predictor."))
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

            model_error <- NULL
            model <- tryCatch({
                MASS::polr(formula, data = model_data_complete, Hess = TRUE, method = "logistic")
            }, error = function(e) {
                model_error <<- conditionMessage(e)
                NULL
            })

            if (is.null(model)) {
                jmvcore::reject(sprintf(.("Error fitting the model (check for near-perfect separation or a predictor with an unused level in a subset of the data): %s"), model_error))
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
                .("Assumption check for ordinal logistic regression (proportional odds)"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">\u00A0</p>",
                "<p style=\"margin: 0 0 0.55em 0; line-height: 1.35;\">",
                .("Use this analysis when you want to review whether a proportional-odds (ordinal logistic) model has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data."),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">\u00A0</p>",
                html_block(NULL, c(
                    paste0(
                        .("<b>Categories (low to high):</b> "),
                        html_escape(paste(dep_levels, collapse = " < "))
                    ),
                    sprintf(.("<b>Complete cases:</b> %s | <b>Smallest category:</b> %s (n = %s)"), n_complete, html_escape(min_cat_name), min_cat_n),
                    sprintf(.("<b>Estimated parameters:</b> %s (predictor coefficients + cutpoints)"), n_model_params)
                ), paragraphs = TRUE, escape = FALSE),
                "</div>"
            ))

            # -----------------------------------------------------------------------------
            # Model design.
            # ES: Diseño del modelo.
            # -----------------------------------------------------------------------------
            self$results$designGuide$setContent(html_guide(.("Model design"), c(.("The proportional-odds model treats the dependent variable's categories as ordered but not equally spaced, and models the cumulative logit - log-odds of being at or below each cutpoint - as a linear function of the predictors."), .("Before interpreting the model, review that every category has a reasonable number of cases and that the number of estimated parameters (predictor coefficients plus k-1 cutpoints) is small relative to the smallest category."), .("A model can be computed even when some categories are sparse, but coefficient estimates and standard errors become unstable as the smallest category shrinks relative to the number of parameters."))))

            add_row(self$results$design, "design_1", list(element = .("Total cases"), value = as.character(n_total)))
            add_row(self$results$design, "design_2", list(element = .("Complete cases"), value = as.character(n_complete)))
            add_row(self$results$design, "design_3", list(element = .("Number of categories"), value = as.character(n_levels)))
            # design is a native jamovi Table (plain-text cells), unlike the
            # intro block above - html_escape() would leak literal "&amp;"-
            # style text into the cell instead of rendering as HTML, so the
            # raw category label goes here unescaped.
            # ES: design es una Table nativa de jamovi (celdas de texto
            # plano), a diferencia del bloque de introducción de arriba -
            # html_escape() dejaría texto literal "&amp;" en la celda en vez
            # de interpretarse como HTML, así que aquí va la etiqueta de
            # categoría cruda, sin escapar.
            add_row(self$results$design, "design_4", list(element = .("Smallest category"), value = paste0(min_cat_name, " (n = ", min_cat_n, ")")))
            add_row(self$results$design, "design_5", list(element = .("Estimated parameters"), value = as.character(n_model_params)))

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
            self$results$separationGuide$setContent(html_guide(.("Complete separation"), c(.("Complete or quasi-complete separation occurs when a predictor (or combination of predictors) perfectly or nearly perfectly predicts which side of a cutpoint a case falls on."), .("Very large coefficients together with very large standard errors are the typical signal of this problem."), .("This check only screens for the extreme coefficient/standard-error signature in the predictor (slope) rows; it does not guarantee adequate cell counts across every category combination."))))

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
                            type = .("Complete separation"),
                            coefficient = coef_val
                        ))
                    }
                }
            }
            if (n_separations == 0) {
                add_row(self$results$separation, "sep_none", list(
                    predictor = .("Not detected"), type = "-", coefficient = NA
                ))
            }

            self$results$separationInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                if (n_separations > 0) {
                    sprintf(.("%s of %s predictor(s) show coefficients and standard errors consistent with complete or quasi-complete separation. Estimates for these predictors are not reliable and should not be interpreted at face value."), n_separations, n_predictors)
                } else {
                    .("No coefficient/standard-error pattern consistent with complete separation was detected in the predictor (slope) rows.")
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
            self$results$linearityGuide$setContent(html_guide(.("Linearity in the cumulative logit"), c(.("The proportional-odds model does not assume the outcome relates linearly to the predictors, but that the cumulative logit does."), .("This assumption is checked with a Box-Tidwell-style procedure, adding an interaction term between each numeric predictor and its own logarithm to the ordinal model."), .("A significant term suggests the real relationship is not linear in the cumulative logit and the predictor may need a transformation or a nonlinear term."), .("This check only applies to numeric predictors; categorical predictors do not have this assumption."))))

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
                .("Applied Interpretation"),
                if (bt_tested == 0) {
                    .("No numeric predictors were available to test for linearity in the cumulative logit.")
                } else {
                    c(
                        sprintf(.("%s of %s numeric predictor(s) showed a significant Box-Tidwell-style interaction, suggesting the relationship with the cumulative logit may not be linear for those predictors."), bt_significant, bt_tested),
                        .("If non-linearity is confirmed, consider adding a quadratic or spline term for that predictor, or categorizing it into meaningful groups.")
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
                .("Proportional odds (parallel lines)"),
                c(.("The proportional-odds model assumes each predictor has the SAME effect on every cutpoint of the outcome - the 'parallel lines' or 'proportional odds' assumption, and the one assumption unique to this model family."), .("Brant's (1990; see references) test compares the coefficients from separate binary logits fit at each cutpoint against the single, pooled coefficient the proportional-odds model reports; a significant term means that predictor's effect is not constant across cutpoints."), .("If the omnibus test is significant but only one or two predictors drive it, consider a partial-proportional-odds model that lets just those predictors vary by cutpoint, rather than abandoning the ordinal approach entirely."), .("This test has limited power in small samples or with sparse categories; a non-significant result there is weaker evidence of proportionality than the same result with a larger, better-balanced sample."))
            ))

            brant_available <- requireNamespace("brant", quietly = TRUE)
            brant_res <- if (brant_available) tryCatch(brant::brant(model), error = function(e) NULL) else NULL

            if (!brant_available) {
                add_row(self$results$proportionalOdds, "pom_missing", list(
                    term = .("Package 'brant' not installed"),
                    chiSq = NA, df = NA, p = NA, pSig = ""
                ))
            } else if (is.null(brant_res)) {
                add_row(self$results$proportionalOdds, "pom_error", list(
                    term = .("Test could not be computed"),
                    chiSq = NA, df = NA, p = NA, pSig = ""
                ))
            } else {
                brant_terms <- rownames(brant_res)
                for (i in seq_along(brant_terms)) {
                    term_label <- if (identical(brant_terms[i], "Omnibus")) .("Omnibus") else brant_terms[i]
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
                .("Applied Interpretation"),
                if (!brant_available) {
                    .("The 'brant' package is not installed, so the proportional-odds assumption could not be tested. Install it (install.packages(\"brant\")) to enable this check - it is the central assumption of this model family and should not be skipped in a final analysis.")
                } else if (is.null(brant_res)) {
                    .("Brant's test could not be computed for this model, most often because a category or predictor combination is too sparse.")
                } else if (!is.na(pom_omnibus_p) && pom_omnibus_p < 0.05) {
                    c(
                        sprintf(.("The omnibus test is significant (p = %s), indicating at least one predictor's effect is not constant across cutpoints - the proportional-odds assumption is not well supported for this model as specified."), fmt_num(pom_omnibus_p, 3)),
                        .("Check the per-term rows above to see which predictor(s) drive the violation before deciding between a partial-proportional-odds model, a multinomial model, or reporting cutpoint-specific effects.")
                    )
                } else {
                    .("The omnibus test is not significant, compatible with the proportional-odds assumption holding across the predictors in this model.")
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Goodness of fit - soft dependency on 'generalhoslem'.
            # ES: Bondad de ajuste - dependencia opcional del paquete
            # 'generalhoslem'.
            # -----------------------------------------------------------------------------
            self$results$goodnessOfFitGuide$setContent(html_guide(.("Goodness of fit"), c(.("Goodness of fit evaluates whether the model, as a whole, reasonably reproduces the observed category frequencies."), .("The Hosmer-Lemeshow test does not generalize directly to an ordinal response; the Lipsitz (1996) and Pulkstenis-Robinson (2004; see references) tests are the ordinal-specific analogues used here instead."), .("The Pulkstenis-Robinson test needs at least one categorical predictor to partition cases into covariate patterns; it is not shown when only numeric predictors are selected. It also needs at least as many distinct covariate patterns as groups to be valid (Fagerland & Hosmer, 2013; see references) - with very few categorical predictors or very few levels, treat a result from this test with extra caution."), .("As with any chi-square-based fit test, these lose power with very small samples and can flag trivially small lack of fit with very large ones - read them together with the proportional-odds result and substantive knowledge of the data."))))

            gof_available <- requireNamespace("generalhoslem", quietly = TRUE)

            if (!gof_available) {
                add_row(self$results$goodnessOfFit, "gof_missing", list(
                    test = .("Package 'generalhoslem' not installed"),
                    statistic = NA, df = NA, p = NA, pSig = ""
                ))
            } else {
                add_gof_row <- function(key, test, res, reason) {
                    # generalhoslem's tests do not always throw on a
                    # degenerate model - like hoslem.test() (found in
                    # logCheck), they can warn and return a result with
                    # p.value = NA instead of erroring - so a result is only
                    # accepted when its p-value is actually finite, not
                    # merely non-NULL.
                    # ES: las pruebas de generalhoslem no siempre lanzan
                    # error ante un modelo degenerado - igual que
                    # hoslem.test() (encontrado en logCheck), pueden avisar y
                    # devolver un resultado con p.value = NA en vez de
                    # fallar - así que un resultado solo se acepta cuando su
                    # p-valor es realmente finito, no solo por no ser NULL.
                    if (!is.null(res) && !is.finite(res$p.value))
                        res <- NULL

                    add_row(self$results$goodnessOfFit, key, list(
                        test = test,
                        statistic = if (!is.null(res)) unname(res$statistic) else NA_real_,
                        df = if (!is.null(res)) unname(res$parameter) else NA_integer_,
                        p = if (!is.null(res)) res$p.value else NA_real_,
                        pSig = if (!is.null(res)) p_sig(res$p.value) else ""
                    ))
                    if (is.null(res) && !is.null(reason))
                        tryCatch(
                            self$results$goodnessOfFit$addFootnote(col = "p", note = reason, rowKey = key),
                            error = function(e) invisible(NULL)
                        )
                }

                lipsitz_res <- tryCatch(generalhoslem::lipsitz.test(model), error = function(e) NULL)
                add_gof_row("gof_lipsitz", "Lipsitz", lipsitz_res, .("Could not be computed - the predicted probabilities are not spread out enough to form the test's groups (too few observations or too little variation)."))

                if (length(factors) > 0) {
                    pr_chisq <- tryCatch(generalhoslem::pulkrob.chisq(model, catvars = factors), error = function(e) NULL)
                    add_gof_row("gof_pr_chisq", .("Pulkstenis-Robinson (chi-squared)"), pr_chisq, .("Could not be computed - needs at least as many distinct covariate patterns as outcome categories."))
                    pr_dev <- tryCatch(generalhoslem::pulkrob.deviance(model, catvars = factors), error = function(e) NULL)
                    add_gof_row("gof_pr_dev", .("Pulkstenis-Robinson (deviance)"), pr_dev, .("Could not be computed - needs at least as many distinct covariate patterns as outcome categories."))
                }
            }

            gof_any_sig <- FALSE
            if (gof_available) {
                gof_rows_df <- tryCatch(self$results$goodnessOfFit$asDF, error = function(e) NULL)
                if (!is.null(gof_rows_df) && "p" %in% names(gof_rows_df))
                    gof_any_sig <- any(!is.na(gof_rows_df$p) & gof_rows_df$p < 0.05)
            }

            self$results$goodnessOfFitInterpretation$setContent(html_block(
                .("Applied Interpretation"),
                if (!gof_available) {
                    .("The 'generalhoslem' package is not installed, so goodness of fit could not be tested. Install it (install.packages(\"generalhoslem\")) to enable this check.")
                } else if (gof_any_sig) {
                    .("At least one goodness-of-fit test is significant, suggesting the model does not reproduce the observed category frequencies well in some region of the predictor space. Consider additional predictors, interactions, or reviewing category coding before trusting model-based predictions.")
                } else {
                    .("The available goodness-of-fit tests are compatible with adequate fit.")
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
            self$results$discriminationGuide$setContent(html_guide(.("Pseudo-R²"), c(.("There is no single ROC curve or AUC once the outcome has more than two ordered categories, so overall model quality is summarized here with pseudo-R² measures instead."), .("McFadden's pseudo-R² compares the fitted model's log-likelihood to an intercept-only model; values of .20-.40 are often described as a good fit, but this rule of thumb is much less established than R² conventions for linear regression."), .("Nagelkerke's pseudo-R² rescales a related measure so it can reach 1.0, making it more directly comparable across models, but it is not on the same numeric scale as a linear-regression R² and should not be interpreted as \"percent of variance explained\" in that sense."))))

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
                .("Applied Interpretation"),
                if (is.na(mcfadden_r2)) {
                    .("Pseudo-R² could not be computed (the null model failed to converge).")
                } else {
                    sprintf(.("McFadden R² = %s; Nagelkerke R² = %s."), fmt_num(mcfadden_r2, 3), fmt_num(nagelkerke_r2, 3))
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
            self$results$multicollinearityGuide$setContent(html_guide(.("Multicollinearity"), c(.("Multicollinearity evaluates whether predictors carry very redundant information."), .("VIF values close to 1 suggest low collinearity; high values indicate a predictor is too well explained by other predictors."), .("If meaningful collinearity is present, consider combining variables, removing redundant predictors, centering variables, or changing the analytic question."))))

            mc_visible <- length(predictors) >= 2

            max_vif <- NA_real_
            max_vif_name <- NA_character_

            if (mc_visible) {
                multi_i <- 0
                add_multi <- function(diagnostic, item, statistic, value, reason = NULL) {
                    multi_i <<- multi_i + 1
                    key <- paste0("multi_", multi_i)
                    add_row(self$results$multicollinearity, key, list(
                        diagnostic = diagnostic, item = item, statistic = statistic, value = value
                    ))
                    if (!is.null(reason) && is.na(value))
                        tryCatch(
                            self$results$multicollinearity$addFootnote(col = "value", note = reason, rowKey = key),
                            error = function(e) invisible(NULL)
                        )
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

                        vif_reason <- if (is.na(vif)) .("Could not be computed - regressing this predictor on the others is likely singular (more predictors than complete cases, or an exact linear dependency among predictors).") else NULL
                        add_multi("VIF", pClean, "VIF", vif, reason = vif_reason)
                        add_multi(.("Tolerance"), pClean, "1/VIF", tol, reason = vif_reason)

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

                    add_multi(.("Minimum eigenvalue"), .("Design matrix"), "\u03bb min", min_eig)
                    add_multi(.("Condition index"), .("Design matrix"), "CI", cond_index)
                    add_multi(.("Determinant"), .("Correlation matrix"), "det(R)", det_r)
                } else {
                    add_multi(.("VIF / tolerance"), .("Not applicable"), "", NA_real_)
                }

                self$results$multicollinearityInterpretation$setContent(html_block(
                    .("Applied Interpretation"),
                    if (!is.na(max_vif)) {
                        sprintf(.("Maximum VIF: %s (predictor: %s). Values between 5 and 10 raise a moderate concern, and values above 10 are considered a severe problem (Marquardt, 1970; see references)."), fmt_num(max_vif, 2), max_vif_name)
                    } else {
                        .("Multicollinearity could not be assessed for this predictor set.")
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

            self$results$correlationMatrixGuide$setContent(html_guide(.("Correlation Matrix"), c(.("These two tables complement the multicollinearity checks with an overview of the association between the dependent variable and all numeric predictors, each in APA 7 format (lower triangle, numbered variables)."), .("The first reports conventional Pearson correlation (linear association only); the second reports distance correlation (Székely et al., 2007), which detects linear and non-linear association alike."), .("The dependent variable enters this matrix as its integer category code (1, 2, 3, ...), a rough interval approximation - not a formal rank or polyserial correlation - so read its row/column as an approximate signal, not a precise effect size."))))


            if (k >= 2) {
                pairResults <- list()
                for (i in seq_len(k)) {
                    for (j in seq_len(k)) {
                        if (j >= i) next()
                        # Let jamovi stop the permutation tests when an option changes.
                        # ES: Permite que jamovi detenga las permutaciones si cambia una opción.
                        private$.checkpoint(flush = FALSE)
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
                    add_row(self$results$pearsonMatrixTable, paste0("pm_", i), pearsonVals)
                    for (cn in names(pearsonSyms)) if (nzchar(pearsonSyms[[cn]])) self$results$pearsonMatrixTable$addSymbol(rowNo = self$results$pearsonMatrixTable$rowCount, cn, pearsonSyms[[cn]])
                    add_row(self$results$dcorMatrixTable, paste0("dm_", i), dcorVals)
                    for (cn in names(dcorSyms)) if (nzchar(dcorSyms[[cn]])) self$results$dcorMatrixTable$addSymbol(rowNo = self$results$dcorMatrixTable$rowCount, cn, dcorSyms[[cn]])
                }

                self$results$correlationMatrixNote$setContent(html_block(NULL, sprintf(.("N = %s. * p < .05, ** p < .01, *** p < .001. %s"), n_complete, .al_dcor_na_note(lang)), paragraphs = FALSE))

                notable_pairs <- 0
                for (key in names(pairResults)) {
                    pr <- pairResults[[key]]
                    if (is.na(pr$dcor) || is.na(pr$pearsonR)) next()
                    gap <- pr$dcor - abs(pr$pearsonR)
                    if (!is.na(gap) && gap > 0.10) {
                        notable_pairs <- notable_pairs + 1
                        add_row(self$results$correlationComparisonTable, paste0("cc_", notable_pairs), list(
                            var1 = pr$v1, var2 = pr$v2,
                            pearson = pr$pearsonR, pearsonSig = .al_p_sig(pr$pearsonP), dcor = pr$dcor, dcorSig = .al_p_sig(pr$dcorP),
                            gap = gap, ce = pr$ce, ceP = pr$ceP, ceSig = p_sig(pr$ceP),
                            flag = .("Yes")
                        ))
                    }
                }

                self$results$correlationComparisonGuide$setContent(html_block(
                    .("Pearson / dCor / Copula Entropy Discordance Analysis"),
                    .al_html_list(c(.("Because Pearson's r only captures linear association while dCor captures both linear and non-linear association, a pair whose dCor is notably larger than its Pearson |r| is a signal (not proof) of a non-linear relationship."), .("Pairs are flagged in the table below when the gap (dCor minus |Pearson r|) is greater than .10. The copula entropy (CE, copent()) result for the same pair is shown alongside as a second, distribution-free line of evidence."), .al_permutation_note(lang, 199, 20260704))),
                    raw = TRUE
                ))

                self$results$correlationComparisonInterpretation$setContent(html_block(
                    .("Applied Interpretation"),
                    if (notable_pairs == 0) {
                        .("No pair shows a Pearson/dCor gap greater than 0.10; there is no indication of unmodeled non-linear association among the model's numeric variables.")
                    } else {
                        sprintf(.("%s pair(s) show a notable Pearson/dCor gap; inspect a scatterplot before concluding the relationship is non-linear."), notable_pairs)
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
            self$results$influenceGuide$setContent(html_guide(.("Influential cases"), c(.("polr() does not expose Cook's D or leverage the way a linear or binary logistic model does, so influence here is screened with a simpler, case-level Pearson-type residual instead: how far the model's fitted probability for the case's OWN observed category falls from a perfect fit."), .("Cases are flagged at |residual| > 2.5, the same convention used elsewhere in this suite for standardized-residual screening."), .("This is a reasonable approximation, not a single canonical published statistic for this model family - for a fuller graphical treatment, see Liu et al. (2009; see references)."), .("A flagged case should not be removed automatically; check whether it is a recording error, a valid but extreme case, or a sign the model does not represent all subgroups well."))))

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
                .("Applied Interpretation"),
                if (is.null(fitted_probs)) {
                    .("Fitted probabilities could not be computed for this model.")
                } else if (n_influential == 0) {
                    .("No cases exceed the |residual| > 2.5 screening threshold.")
                } else {
                    sprintf(.("%s of %s cases (%s%%) exceed the |residual| > 2.5 screening threshold."), n_influential, n_complete, fmt_num(pct_influential, 1))
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
                .("Coefficients and odds ratios (proportional odds)"),
                c(.("Under proportional odds, each predictor has a single odds ratio that applies to every cutpoint: it is the multiplicative change in the odds of being at or above any given category, for a one-unit increase in the predictor (or for belonging to a category versus the reference category)."), .("Read the confidence interval, not only the point estimate: if it includes 1, the effect is not statistically distinguishable from no association."), .("This single-OR interpretation is only valid to the extent the proportional-odds assumption holds - if the Brant test above flags a predictor, its odds ratio here should not be read as constant across cutpoints."))
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
                .("Applied Interpretation"),
                if (length(ors) == 0) {
                    .("No predictor coefficients were estimated.")
                } else {
                    strongest_idx <- which.max(abs(log(ors)))
                    sprintf(.("Strongest association: %s, OR = %s."), names(ors)[strongest_idx], fmt_num(ors[strongest_idx], 2))
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Notes and recommendation.
            # ES: Notas y recomendación.
            # -----------------------------------------------------------------------------
            recommendations <- c()
            if (!is.na(pom_omnibus_p) && pom_omnibus_p < 0.05) {
                recommendations <- c(recommendations, .("The proportional-odds assumption is not well supported; consider a partial-proportional-odds or multinomial model."))
            }
            if (bt_significant > 0) {
                recommendations <- c(recommendations, .("At least one numeric predictor shows a non-linear relationship with the cumulative logit; consider a transformation or categorization."))
            }
            if (n_influential > 0) {
                recommendations <- c(recommendations, .("There are cases with a large Pearson-type residual; inspect them before relying on the model's predictions."))
            }

            self$results$notes$setContent(html_block(NULL, c(
                paste0(
                    .("Suggested decision: "),
                    if (length(recommendations) > 0) paste(recommendations, collapse = " ") else .("No major assumption concerns were flagged; the model's coefficients appear reasonably defensible.")
                ),
                sprintf(.("Model fitted with %s cases."), n_complete)
            ), paragraphs = TRUE))

            # -----------------------------------------------------------------------------
            # State for the plots. Each image gets only the fields its own render
            # function reads - not one shared list for both plots - since jamovi's review
            # flagged that repeating the same (potentially large) state across images
            # multiplies what gets written into every exported/saved results file.
            # model_data (the full complete-case data frame) is only needed for the
            # linearity plot's per-predictor empirical-logit binning; the influence plot
            # only draws Pearson residuals against case number.
            # ES: Estado para los gráficos. Cada imagen recibe solo los campos que su
            # propia función de render lee - no una lista compartida para ambos gráficos -
            # ya que la revisión de jamovi señaló que repetir el mismo estado
            # (potencialmente grande) en varias imágenes multiplica lo que se escribe en
            # cada archivo de resultados exportado o guardado. model_data (el data frame
            # completo de casos completos) solo hace falta para el binning de logit
            # empírico por predictor del gráfico de linealidad; el gráfico de influencia
            # solo dibuja residuos de Pearson contra el número de caso.
            # -----------------------------------------------------------------------------
            mid_cut <- ceiling(n_levels / 2)
            dep_binary_mid <- as.integer(as.integer(model_data_complete$dep_ord) > mid_cut)

            self$results$linearityPlot$setState(list(
                covs = covs,
                model_data = model_data_complete,
                dep_binary_mid = dep_binary_mid
            ))
            self$results$influencePlot$setState(list(
                pearson_resid = if (!is.null(fitted_probs)) pearson_resid else rep(NA_real_, n_complete),
                n = n_complete
            ))

            lin_show <- tryCatch(isTRUE(self$options$linShowPlots), error = function(e) TRUE)
            infl_show <- tryCatch(isTRUE(self$options$influenceShowPlots), error = function(e) TRUE)
            show_plots_master <- tryCatch(isTRUE(self$options$showPlots), error = function(e) TRUE)

            self$results$linearityPlot$setVisible(show_plots_master && lin_show && length(covs) > 0)
            self$results$influencePlot$setVisible(show_plots_master && infl_show)
        },

        # -----------------------------------------------------------------------------
        # Plot helpers (identical pattern to logCheck: .emptyLogPlot,
        # .plotStyle, .plotPalette, .plotTheme).
        # ES: Helpers de gráficos (patrón idéntico a logCheck).
        # -----------------------------------------------------------------------------
        .emptyLogPlot = function(message) {
            graphics::plot.new()
            graphics::par(mar = c(1, 1, 1, 1))
            graphics::text(x = 0.5, y = 0.55, labels = message, cex = 0.95)
            graphics::text(x = 0.5, y = 0.40, labels = "AssumptionsLab", cex = 0.80)
            TRUE
        },


        # Plot theme/palette migrated to jamovi's native ggtheme/theme
        # mechanism - see the detailed rationale in logcheck.b.R's
        # .plotColors()/.plotSeriesColors() (the pilot for this change,
        # 2026-09-17). Identical design here.
        # ES: Tema/paleta de gráficos migrado al mecanismo nativo
        # ggtheme/theme de jamovi - ver el razonamiento detallado en
        # .plotColors()/.plotSeriesColors() de logcheck.b.R (el piloto de
        # este cambio, 2026-09-17). Mismo diseño aquí.
        .plotColors = function(theme) {
            base_color <- if (!is.null(theme$color) && length(theme$color) >= 1) theme$color[1] else "#333333"
            accent_color <- if (!is.null(theme$color) && length(theme$color) >= 2) theme$color[2] else "#2C7FB8"
            accent_fill <- if (!is.null(theme$fill) && length(theme$fill) >= 2) theme$fill[2] else "#A6CEE3"

            list(
                point = base_color, line = base_color, ref = "gray50",
                alert = accent_color, smooth = accent_color, fill = accent_fill
            )
        },

        # Plot colors always follow the palette chosen in jamovi's settings:
        # NULL lets ggtheme's own discrete scale color each series. The
        # module's own palette option (Okabe-Ito, viridis) was removed, as
        # jamovi asked in the MokkenLab and EGAnetLab audits.
        # ES: Los colores siguen siempre la paleta elegida en jamovi: NULL
        # deja que la escala discreta de ggtheme coloree cada serie. Se quitó
        # la opción de paleta propia del módulo (Okabe-Ito, viridis), como
        # pidió jamovi en las auditorías de MokkenLab y EGAnetLab.
        .plotSeriesColors = function() {
            NULL
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
        .plotLinearity = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            st <- image$state
            if (is.null(st)) return(FALSE)

            covs <- st$covs

            if (length(covs) == 0) {
                return(private$.emptyLogPlot(.("No numeric predictors to plot.")))
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
                return(private$.emptyLogPlot(.("Not enough variation to plot linearity in the cumulative logit.")))
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
                ggplot2::facet_wrap(~predictor, scales = "free_x") +
                ggplot2::labs(
                    x = .("Predictor (binned mean)"),
                    y = .("Empirical cumulative logit (median cutpoint)"),
                    size = .("Group n")
                ) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::margin(4, 6, 4, 6))

            if (!is.null(series_colors))
                p <- p + ggplot2::scale_color_manual(values = series_colors)

            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Pearson-type residual by case.
        # ES: Residuo tipo Pearson por caso.
        # -----------------------------------------------------------------------------
        .plotInfluence = function(image, ggtheme, theme, ...) {
            . <- .al_lang(self$options)$tr
            st <- image$state
            if (is.null(st)) return(FALSE)

            pal <- private$.plotColors(theme)

            resid_vals <- st$pearson_resid
            n <- st$n
            threshold <- 2.5

            if (all(is.na(resid_vals))) {
                return(private$.emptyLogPlot(.("Residuals could not be computed for this model.")))
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
                ggplot2::geom_point(ggplot2::aes(color = flag), size = 1.6, show.legend = FALSE)

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
                ggplot2::labs(x = .("Case"), y = .("Pearson-type residual")) +
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
