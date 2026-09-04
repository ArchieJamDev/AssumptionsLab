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
# Multinomial Logistic Regression.
# ES: Regresión Logística Multinomial.
#
# This file implements multCheck: an assumption-diagnostic module for the
# multinomial-logit model (one unordered/nominal dependent variable with 3+
# categories, numeric and/or categorical predictors, fit with
# nnet::multinom()). It completes AssumptionsLab's "Regression" family
# (regCheck -> linear, logCheck -> binary logistic, ordCheck -> ordinal
# logistic, multCheck -> nominal/multinomial logistic), built Sep 2026 as
# the ordinal sibling's natural counterpart for an outcome whose categories
# have no meaningful order at all.
#
# NEW relative to ordCheck's diagnostic battery: Independence of Irrelevant
# Alternatives (IIA) - the assumption unique to this model family, tested
# with a manually-implemented Hausman-McFadden (1984) test - replacing
# ordCheck's proportional-odds/Brant-test slot, which has no multinomial
# analogue (there are no cutpoints to keep parallel). Linearity in the logit
# is redesigned as a single joint likelihood-ratio test per predictor
# (df = K-1) rather than a per-category Wald p, because nnet::multinom()
# fits every category's equation simultaneously and adding an x*log(x) term
# adds one parameter to EACH non-reference-category equation at once. A new
# K x K classification (confusion) table is added - a genuinely useful,
# ordinal-scale-free diagnostic that neither logCheck's ROC/AUC (binary
# only) nor ordCheck's pseudo-R2-only approach can provide once the outcome
# has 3+ unordered categories.
#
# REUSED from ordCheck/shared-helpers.R unchanged: the dependent-variable
# validation shape, the model-design summary and category-distribution
# table, the complete/quasi-complete separation screen (adapted to loop over
# the coefficient MATRIX - categories x predictors - instead of a single
# coefficient vector), the generalhoslem soft-dependency goodness-of-fit
# guard pattern, McFadden/Nagelkerke pseudo-R2 (logLik() works identically
# for a "multinom" fit), the manual VIF/tolerance/eigenvalue/condition-index
# multicollinearity block (a property of the predictors' own design matrix,
# independent of the response family), the custom per-case Pearson-type
# influence residual on the observed category's fitted probability, the
# .run()/.plotTr()/.plotPalette()/.plotTheme() plot scaffolding, and inline
# tr() bilingual text (not a new texts.R section).
#
# ES: Este archivo implementa multCheck: un módulo de diagnóstico de
# supuestos para el modelo logit multinomial (una variable dependiente
# nominal/no ordenada con 3+ categorías, predictores numéricos y/o
# categóricos, ajustado con nnet::multinom()). Completa la familia
# "Regresión" de AssumptionsLab (regCheck -> lineal, logCheck -> logística
# binaria, ordCheck -> logística ordinal, multCheck -> logística
# nominal/multinomial), construido en sep 2026 como la contraparte natural
# del hermano ordinal para un desenlace cuyas categorías no tienen ningún
# orden con sentido.
#
# NUEVO respecto a la batería de ordCheck: Independencia de Alternativas
# Irrelevantes (IIA) - el supuesto propio de esta familia de modelos,
# probado con una prueba de Hausman-McFadden (1984) implementada
# manualmente - reemplazando el slot de momios proporcionales/prueba de
# Brant de ordCheck, que no tiene análogo multinomial (no hay puntos de
# corte que mantener paralelos). La linealidad en el logit se rediseña como
# una única prueba de razón de verosimilitud conjunta por predictor
# (gl = K-1) en vez de un p de Wald por categoría, porque nnet::multinom()
# ajusta la ecuación de cada categoría simultáneamente y agregar un término
# x*log(x) agrega un parámetro a CADA ecuación de categoría no-referencia a
# la vez. Se agrega una nueva tabla de clasificación (matriz de confusión)
# K x K - un diagnóstico genuinamente útil y libre de supuestos de escala
# ordinal que ni el ROC/AUC de logCheck (solo binario) ni el enfoque de solo
# pseudo-R2 de ordCheck pueden dar una vez que el desenlace tiene 3+
# categorías no ordenadas.
#
# REUTILIZADO de ordCheck/shared-helpers.R sin cambios: la forma de
# validación de la variable dependiente, el resumen de diseño del modelo y
# la tabla de distribución por categoría, el cribado de separación
# completa/cuasi-completa (adaptado para recorrer la MATRIZ de coeficientes
# - categorías x predictores - en vez de un solo vector de coeficientes), el
# patrón de guarda de dependencia opcional de generalhoslem para bondad de
# ajuste, el pseudo-R2 de McFadden/Nagelkerke (logLik() funciona igual para
# un ajuste "multinom"), el bloque manual de VIF/tolerancia/
# eigenvalue/índice de condición para multicolinealidad (una propiedad de la
# matriz de diseño de los predictores, independiente de la familia del
# desenlace), el residuo tipo Pearson por caso sobre la probabilidad
# ajustada de la categoría observada, el andamiaje de gráficos
# .run()/.plotTr()/.plotPalette()/.plotTheme(), y el texto bilingüe con
# tr() en línea (no una nueva sección de texts.R).
#
# Soft dependency (Sep 2026, same pattern as ordCheck's brant/
# generalhoslem): 'generalhoslem' for goodness-of-fit (lipsitz.test,
# pulkrob.chisq, pulkrob.deviance), which documents support for both "polr"
# and "multinom" fitted objects. A missing package produces an explicit
# "package not installed" row, never a silently blank/NA result; a test
# that errors on this specific fit produces its own explicit
# "not computable for this model" row (CODE_STYLE.md SS19/SS21). 'nnet' itself
# is a HARD dependency (part of R's "recommended" package set, exactly like
# MASS is for ordCheck) - no requireNamespace guard around it anywhere in
# this file.
#
# ES: Dependencia opcional (sep 2026, mismo patrón que brant/generalhoslem
# de ordCheck): 'generalhoslem' para bondad de ajuste (lipsitz.test,
# pulkrob.chisq, pulkrob.deviance), que documenta soporte tanto para
# objetos ajustados "polr" como "multinom". Un paquete faltante produce una
# fila explícita de "paquete no instalado", nunca un resultado en silencio
# vacío/NA; una prueba que falla para este ajuste específico produce su
# propia fila explícita de "no se pudo calcular para este modelo"
# (CODE_STYLE.md SS19/SS21). 'nnet' en sí es una dependencia OBLIGATORIA
# (parte del conjunto de paquetes "recomendados" de R, igual que MASS lo es
# para ordCheck) - no hay ninguna guarda requireNamespace a su alrededor en
# ningún lugar de este archivo.
#
# Independence of Irrelevant Alternatives - Hausman-McFadden test (Sep
# 2026, per Archie's decision): implemented manually with nnet::multinom()
# alone, no new package dependency. For each category in turn, the rows
# whose observed outcome IS that category are removed and the model is
# refit on the remaining rows (the standard IIA test design: if the
# remaining alternatives' relative odds are genuinely independent of the
# excluded alternative, removing it should not shift their coefficients).
# The coefficients and covariance matrix that are common to the full and
# restricted fits (category x predictor combinations estimated in both -
# this automatically self-adjusts when the omitted category happens to be
# the reference level, since nnet::multinom() then silently re-picks a new
# reference from the remaining levels) are compared via
# H = (b_r - b_f)' [V_r - V_f]^-1 (b_r - b_f), asymptotically chi-square
# with df = length(b_r). V_r - V_f is not guaranteed to be positive
# definite in finite samples (a well-documented degeneracy of the
# Hausman-McFadden test, not a bug) - when solve() fails, the row explains
# this rather than showing a blank or crashing, exactly as CODE_STYLE.md
# SS19/SS21 requires.
#
# ES: Independencia de Alternativas Irrelevantes - prueba de
# Hausman-McFadden (sep 2026, por decisión de Archie): implementada
# manualmente solo con nnet::multinom(), sin ninguna dependencia de paquete
# nueva. Para cada categoría por turno, se eliminan las filas cuyo
# desenlace observado ES esa categoría y se reajusta el modelo con las
# filas restantes (el diseño estándar de la prueba IIA: si los momios
# relativos de las alternativas restantes son genuinamente independientes
# de la alternativa excluida, eliminarla no debería desplazar sus
# coeficientes). Los coeficientes y la matriz de covarianza comunes entre
# el ajuste completo y el restringido (combinaciones categoría x predictor
# estimadas en ambos - esto se autoajusta cuando la categoría omitida
# resulta ser el nivel de referencia, ya que nnet::multinom() entonces
# vuelve a elegir en silencio una nueva referencia entre los niveles
# restantes) se comparan con H = (b_r - b_f)' [V_r - V_f]^-1 (b_r - b_f),
# asintóticamente ji-cuadrado con gl = length(b_r). V_r - V_f no está
# garantizada como definida positiva en muestras finitas (una degeneración
# bien documentada de la prueba de Hausman-McFadden, no un error) - cuando
# solve() falla, la fila explica esto en vez de mostrarse en blanco o
# fallar, exactamente como exige CODE_STYLE.md SS19/SS21.
#
# Coefficients / odds-ratios table (Sep 2026, per Archie's decision):
# unlike ordCheck (single OR per predictor, proportional odds) or logCheck
# (single OR per predictor, binary), nnet::multinom() fits K-1 separate
# log-odds equations, one per non-reference category versus the reference.
# The table therefore has one row per predictor TERM x non-reference
# CATEGORY, including the "(Intercept)" term (exp(intercept) is the
# baseline odds of that category versus the reference when every predictor
# equals zero - a legitimate, interpretable quantity, unlike a
# proportional-odds cutpoint, so no separate intercepts/cutpoints table is
# used here). p-values are computed manually the same way ordCheck does
# (z = Estimate/SE, two-sided normal p), since summary.multinom(), like
# summary.polr(), does not report them.
#
# ES: Tabla de coeficientes / odds ratios (sep 2026, por decisión de
# Archie): a diferencia de ordCheck (un solo OR por predictor, momios
# proporcionales) o logCheck (un solo OR por predictor, binario),
# nnet::multinom() ajusta K-1 ecuaciones de log-momios separadas, una por
# categoría no-referencia frente a la referencia. La tabla por lo tanto
# tiene una fila por TÉRMINO predictor x CATEGORÍA no-referencia, incluido
# el término "(Intercept)" (exp(intercepto) es el momio base de esa
# categoría frente a la referencia cuando todos los predictores valen cero
# - una cantidad legítima e interpretable, a diferencia de un punto de
# corte de momios proporcionales, así que aquí no se usa una tabla separada
# de intercepts/cutpoints). Los valores p se calculan manualmente igual que
# en ordCheck (z = Estimación/EE, p normal bilateral), ya que
# summary.multinom(), igual que summary.polr(), no los reporta.
#
# Classification table (Sep 2026, new addition per Archie's decision): a
# K x K table of observed vs. predicted category (predicted = which.max of
# the fitted probabilities, via stats::predict(model)), with a per-category
# recall column and overall accuracy reported in the interpretation text.
# Unlike ROC/AUC (which both logCheck and ordCheck correctly avoid once the
# outcome has more than two unordered classes - there is no single ROC
# curve without an ordering), this table has no ordinal-scale assumption
# baked in, and directly answers "does this multinomial model actually
# discriminate the categories" in a way pseudo-R2 alone does not. Its
# columns are built dynamically in .init()/.initClassificationTable(),
# following the same defensive getColumn()-then-addColumn() pattern already
# used for the correlation-matrix tables in this suite, because the number
# of categories is only known once the dependent variable's data is read.
#
# ES: Tabla de clasificación (sep 2026, adición nueva por decisión de
# Archie): una tabla K x K de categoría observada vs. predicha (predicha =
# el máximo de las probabilidades ajustadas, vía stats::predict(model)),
# con una columna de sensibilidad (recall) por categoría y la exactitud
# global reportada en el texto de interpretación. A diferencia del ROC/AUC
# (que tanto logCheck como ordCheck evitan correctamente una vez que el
# desenlace tiene más de dos clases no ordenadas - no hay una única curva
# ROC sin un orden), esta tabla no tiene ningún supuesto de escala ordinal
# incorporado, y responde directamente "¿este modelo multinomial realmente
# discrimina las categorías?" de una forma que el pseudo-R2 por sí solo no
# puede. Sus columnas se construyen dinámicamente en
# .init()/.initClassificationTable(), siguiendo el mismo patrón defensivo
# getColumn()-luego-addColumn() ya usado para las tablas de matriz de
# correlación en esta suite, porque el número de categorías solo se conoce
# una vez que se leen los datos de la variable dependiente.
#
# Correlation matrix scope (Sep 2026, per Archie's decision): ordCheck
# includes its (ordinal, hence integer-codable) dependent variable in its
# Pearson/dCor matrix. Here the outcome is unordered, so integer-coding it
# for a correlation matrix has no defensible interpretation - the matrix
# therefore covers ONLY the numeric predictors (still useful context
# alongside the multicollinearity block), and this scope choice is stated
# explicitly in the guide text below.
#
# ES: Alcance de la matriz de correlaciones (sep 2026, por decisión de
# Archie): ordCheck incluye su variable dependiente (ordinal, por lo tanto
# codificable como entero) en su matriz de Pearson/dCor. Aquí el desenlace
# no está ordenado, así que codificarlo como entero para una matriz de
# correlaciones no tiene una interpretación defendible - la matriz por lo
# tanto cubre SOLO los predictores numéricos (contexto igual de útil junto
# al bloque de multicolinealidad), y esta decisión de alcance se declara
# explícitamente en el texto de la guía más abajo.
#
# Workflow
# 1. Validate the dependent variable and predictors, then fit the
#    multinomial-logit model.
# 2. Summarize the model design.
# 3. Screen for complete or quasi-complete separation.
# 4. Test linearity in the multinomial logit with a joint likelihood-ratio
#    test per predictor.
# 5. Test Independence of Irrelevant Alternatives (Hausman-McFadden).
# 6. Evaluate goodness of fit (Lipsitz and Pulkstenis-Robinson tests).
# 7. Report pseudo-R2 discrimination.
# 8. Build the observed-vs-predicted classification table.
# 9. Assess multicollinearity among the predictors.
# 10. Build the correlation matrices among numeric predictors (Pearson /
#     dCor / copula entropy).
# 11. Detect influential cases.
# 12. Report coefficients and odds ratios by category.
# 13. Assemble the recommendation and notes.
#
# ES: Flujo de trabajo
# 1. Validar la variable dependiente y los predictores, y ajustar el
#    modelo logit multinomial.
# 2. Resumir el diseño del modelo.
# 3. Detectar separación completa o casi completa.
# 4. Evaluar la linealidad en el logit multinomial con una prueba de razón
#    de verosimilitud conjunta por predictor.
# 5. Probar la Independencia de Alternativas Irrelevantes
#    (Hausman-McFadden).
# 6. Evaluar la bondad de ajuste (pruebas de Lipsitz y
#    Pulkstenis-Robinson).
# 7. Reportar la discriminación mediante pseudo-R2.
# 8. Construir la tabla de clasificación observado vs. predicho.
# 9. Evaluar la multicolinealidad entre los predictores.
# 10. Construir las matrices de correlación entre predictores numéricos
#     (Pearson / dCor / entropía copular).
# 11. Detectar casos influyentes.
# 12. Reportar coeficientes y odds ratios por categoría.
# 13. Elaborar la recomendación y las notas.
# -----------------------------------------------------------------------------

multCheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "multCheckClass",
    inherit = multCheckBase,
    private = list(
        .init = function() {
            private$.initCorrelationMatrix()
            private$.initClassificationTable()
        },

        # Numeric-predictors-only column set (see file header: the outcome
        # is unordered, so it is excluded from this matrix, unlike
        # ordCheck's dep+covs matrix).
        # ES: Conjunto de columnas de solo predictores numéricos (ver
        # encabezado del archivo: el desenlace no está ordenado, así que se
        # excluye de esta matriz, a diferencia de la matriz dep+covs de
        # ordCheck).
        .initCorrelationMatrix = function() {
            covs <- self$options$covs
            for (tableName in c("pearsonMatrixTable", "dcorMatrixTable")) {
                table <- tryCatch(self$results[[tableName]], error = function(e) NULL)
                if (is.null(table)) next()
                for (i in seq_along(covs)) {
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

        # The classification table's predicted-category columns depend on
        # the dependent variable's actual factor levels, which are only
        # known once the data is read - unlike every other dynamic-column
        # table in this suite (whose columns depend only on which
        # variables the user selected, known from options alone). Called
        # from both .init() (best-effort, so columns exist as early as
        # possible) and again defensively from .run() itself (see
        # .ensureClassificationColumns()) once the validated dep_levels are
        # in hand, so a mismatch between the two moments can never leave a
        # column missing when addRow() is called.
        # ES: Las columnas de categoría predicha de la tabla de
        # clasificación dependen de los niveles reales del factor de la
        # variable dependiente, que solo se conocen una vez leídos los
        # datos - a diferencia de cualquier otra tabla de columnas
        # dinámicas de esta suite (cuyas columnas dependen solo de qué
        # variables eligió el usuario, algo que ya se sabe desde las
        # opciones). Se llama tanto desde .init() (best-effort, para que
        # las columnas existan lo antes posible) como de nuevo, de forma
        # defensiva, desde .run() (ver .ensureClassificationColumns()) una
        # vez que se tienen los dep_levels validados, para que un
        # desajuste entre ambos momentos nunca pueda dejar una columna
        # faltante cuando se llama a addRow().
        .initClassificationTable = function() {
            data <- tryCatch(self$data, error = function(e) NULL)
            dep <- self$options$dep
            if (is.null(data) || is.null(dep) || !nzchar(dep) || !(dep %in% names(data)))
                return(invisible(FALSE))
            dep_raw <- data[[dep]]
            if (!is.factor(dep_raw))
                return(invisible(FALSE))
            dep_levels <- levels(droplevels(dep_raw))
            private$.ensureClassificationColumns(dep_levels)
        },

        .ensureClassificationColumns = function(dep_levels) {
            table <- tryCatch(self$results$classification, error = function(e) NULL)
            if (is.null(table) || length(dep_levels) == 0)
                return(invisible(FALSE))

            for (i in seq_along(dep_levels)) {
                colName <- paste0("pred", i)
                existing <- tryCatch(table$getColumn(colName), error = function(e) NULL)
                if (is.null(existing)) {
                    tryCatch(
                        table$addColumn(name = colName, title = dep_levels[i], type = "integer"),
                        error = function(e) NULL
                    )
                }
            }

            existing_total <- tryCatch(table$getColumn("total"), error = function(e) NULL)
            if (is.null(existing_total)) {
                tryCatch(table$addColumn(name = "total", title = "n", type = "integer"), error = function(e) NULL)
            }

            existing_recall <- tryCatch(table$getColumn("recall"), error = function(e) NULL)
            if (is.null(existing_recall)) {
                tryCatch(
                    table$addColumn(name = "recall", title = "Recall", type = "number", format = "pc"),
                    error = function(e) NULL
                )
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
            # (.al_dcor_stat/.al_dcor_test), same pattern as the other
            # modules that have them. Fixed B=199/seed=20260704.
            # ES: consolidadas en shared-helpers.R (.al_dcor_stat/
            # .al_dcor_test), mismo patrón que los otros módulos que las
            # tienen. B=199/semilla=20260704 fijos.
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

                    c("linearityGuide", "Linearity in the multinomial logit", "Linealidad en el logit multinomial"),
                    c("linearity",
                      "Linearity in the multinomial logit (LR test)",
                      "Linealidad en el logit multinomial (prueba LR)"),
                    c("linearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),
                    c("linearityPlot", "Empirical logit vs predictor", "Logit empírico vs predictor"),

                    c("iiaGuide",
                      "Independence of Irrelevant Alternatives (IIA)",
                      "Independencia de alternativas irrelevantes (IIA)"),
                    c("iia", "Hausman-McFadden test (IIA)", "Prueba de Hausman-McFadden (IIA)"),
                    c("iiaInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("goodnessOfFitGuide", "Goodness of fit", "Bondad de ajuste"),
                    c("goodnessOfFit", "Goodness of fit", "Bondad de ajuste"),
                    c("goodnessOfFitInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("discriminationGuide", "Pseudo-R²", "Pseudo-R²"),
                    c("discrimination", "Pseudo-R²", "Pseudo-R²"),
                    c("discriminationInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("classificationGuide", "Classification table", "Tabla de clasificación"),
                    c("classification", "Observed vs. predicted", "Observado vs. predicho"),
                    c("classificationInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("multicollinearityGuide", "Multicollinearity", "Multicolinealidad"),
                    c("multicollinearity", "Multicollinearity", "Multicolinealidad"),
                    c("multicollinearityInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

                    c("correlationMatrixGuide",
                      "Correlation Matrix (numeric predictors)",
                      "Matriz de Correlaciones (predictores numéricos)"),
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
                      "Coefficients and odds ratios (by category)",
                      "Coeficientes y odds ratios (por categoría)"),
                    c("coefficients", "Coefficients and odds ratios", "Coeficientes y odds ratios"),
                    c("coefficientsInterpretation", "Applied Interpretation", "Interpretación Aplicada"),

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
                    c("separation", "category", "Category", "Categoría"),
                    c("separation", "type", "Type", "Tipo"),
                    c("separation", "coefficient", "Coefficient", "Coeficiente"),

                    c("linearity", "predictor", "Predictor", "Predictor"),
                    c("linearity", "chiSq", "X²", "X²"),
                    c("linearity", "df", "df", "gl"),
                    c("linearity", "pSig", "Sig.", "Sig."),

                    c("iia", "category", "Omitted category", "Categoría omitida"),
                    c("iia", "chiSq", "X²", "X²"),
                    c("iia", "df", "df", "gl"),
                    c("iia", "pSig", "Sig.", "Sig."),
                    c("iia", "note", "Note", "Nota"),

                    c("goodnessOfFit", "test", "Test", "Prueba"),
                    c("goodnessOfFit", "statistic", "Statistic", "Estadístico"),
                    c("goodnessOfFit", "df", "df", "gl"),
                    c("goodnessOfFit", "pSig", "Sig.", "Sig."),

                    c("discrimination", "metric", "Metric", "Métrica"),
                    c("discrimination", "value", "Value", "Valor"),

                    c("classification", "observed", "Observed", "Observado"),
                    c("classification", "total", "n", "n"),
                    c("classification", "recall", "Recall", "Recall"),

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
                    c("influence", "observed", "Observed category", "Categoría observada"),
                    c("influence", "pearsonStat", "Pearson statistic", "Estadístico de Pearson"),
                    c("influence", "p", "p", "p"),

                    c("coefficients", "predictor", "Predictor", "Predictor"),
                    c("coefficients", "category", "Category", "Categoría"),
                    c("coefficients", "or", "OR", "OR"),
                    c("coefficients", "ciLower", "95% CI lower", "IC 95% inferior"),
                    c("coefficients", "ciUpper", "95% CI upper", "IC 95% superior"),
                    c("coefficients", "pSig", "Sig.", "Sig.")
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
                    "The dependent variable must be a nominal (factor) variable.",
                    "La variable dependiente debe ser una variable nominal (factor)."
                ), paragraphs = FALSE))
                return()
            }

            dep_levels <- levels(droplevels(dep_raw))
            n_levels <- length(dep_levels)

            if (n_levels < 3) {
                self$results$intro$setContent(html_block(NULL, tr(
                    "The dependent variable has fewer than 3 categories. Use logCheck for a binary (2-category) dependent variable instead - it gives richer binary-specific diagnostics (ROC/AUC, Hosmer-Lemeshow, a single odds-ratio table).",
                    "La variable dependiente tiene menos de 3 categorías. Use logCheck en cambio para una variable dependiente binaria (2 categorías) - da diagnósticos binarios más ricos (ROC/AUC, Hosmer-Lemeshow, una tabla de un solo odds ratio)."
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

            # The reference category is always the factor's FIRST level (no
            # separate yaml selector - see multcheck.a.yaml's dep option
            # comment); every non-reference log-odds equation below is read
            # "versus reference_label".
            # ES: La categoría de referencia es siempre el PRIMER nivel del
            # factor (sin selector yaml separado - ver el comentario de la
            # opción dep en multcheck.a.yaml); toda ecuación de log-momios
            # no-referencia de abajo se lee "frente a reference_label".
            reference_label <- dep_levels[1]
            non_ref_labels <- dep_levels[-1]

            model_data <- data
            model_data$dep_nom <- factor(dep_raw, levels = dep_levels)
            complete_cases <- stats::complete.cases(model_data[, c("dep_nom", predictors), drop = FALSE])
            model_data_complete <- model_data[complete_cases, ]

            n_total <- nrow(data)
            n_complete <- nrow(model_data_complete)
            n_predictors <- length(predictors)

            cat_counts <- table(model_data_complete$dep_nom)
            min_cat_n <- min(cat_counts)
            min_cat_name <- names(cat_counts)[which.min(cat_counts)]

            formula_str <- paste("dep_nom ~", paste(qname(predictors), collapse = " + "))
            formula <- stats::as.formula(formula_str)
            null_formula <- stats::as.formula("dep_nom ~ 1")

            model <- tryCatch({
                nnet::multinom(formula, data = model_data_complete, trace = FALSE)
            }, error = function(e) NULL)

            if (is.null(model)) {
                self$results$notes$setContent(html_block(NULL, tr(
                    "Error fitting the model. Check for near-perfect separation or a predictor with an unused level in a subset of the data.",
                    "Error al ajustar el modelo. Revise si hay cuasi-separación o un predictor con un nivel no usado en un subconjunto de los datos."
                ), paragraphs = FALSE))
                return()
            }

            null_model <- tryCatch({
                nnet::multinom(null_formula, data = model_data_complete, trace = FALSE)
            }, error = function(e) NULL)

            # summary.multinom(), like summary.polr(), does not report
            # p-values (only coefficients and standard errors) - z-based
            # two-sided p-values are computed manually here, same technique
            # as ordCheck.
            # ES: summary.multinom(), igual que summary.polr(), no reporta
            # valores p (solo coeficientes y errores estándar) - se
            # calculan manualmente aquí valores p bilaterales basados en z,
            # misma técnica que ordCheck.
            model_summary <- summary(model)
            coef_matrix <- model_summary$coefficients
            se_matrix <- model_summary$standard.errors
            if (is.null(dim(coef_matrix))) {
                coef_matrix <- matrix(coef_matrix, nrow = 1, dimnames = list(non_ref_labels[1], names(coef_matrix)))
                se_matrix <- matrix(se_matrix, nrow = 1, dimnames = list(non_ref_labels[1], names(se_matrix)))
            }
            z_matrix <- coef_matrix / se_matrix
            p_matrix <- 2 * stats::pnorm(abs(z_matrix), lower.tail = FALSE)

            n_model_params <- length(coef_matrix)

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
                tr("Assumption check for multinomial logistic regression", "Revisión de supuestos para regresión logística multinomial"),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                "<p style=\"margin: 0 0 0.55em 0; line-height: 1.35;\">",
                tr(
                    "Use this analysis when you want to review whether a multinomial-logit model, for an unordered outcome with 3 or more categories, has defensible methodological assumptions. The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.",
                    "Use este análisis cuando quiera revisar si un modelo logit multinomial, para un desenlace no ordenado con 3 o más categorías, tiene supuestos metodológicos defendibles. El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística con evidencia obtenida de sus propios datos."
                ),
                "</p>",
                "<p style=\"margin: 0 0 0.25em 0;\">&nbsp;</p>",
                html_block(NULL, c(
                    paste0(
                        tr("<b>Categories:</b> ", "<b>Categorías:</b> "),
                        html_escape(paste(dep_levels, collapse = ", "))
                    ),
                    paste0(
                        tr("<b>Reference category:</b> ", "<b>Categoría de referencia:</b> "),
                        html_escape(reference_label),
                        tr(" (the factor's first level - reorder levels in jamovi's variable editor to change it).",
                           " (el primer nivel del factor - reordene los niveles en el editor de variables de jamovi para cambiarlo).")
                    ),
                    paste0(
                        tr("<b>Complete cases:</b> ", "<b>Casos completos:</b> "), n_complete,
                        tr(" | <b>Smallest category:</b> ", " | <b>Categoría más pequeña:</b> "),
                        html_escape(min_cat_name), " (n = ", min_cat_n, ")"
                    ),
                    paste0(
                        tr("<b>Estimated parameters:</b> ", "<b>Parámetros estimados:</b> "),
                        n_model_params,
                        tr(" (across ", " (entre "), n_levels - 1,
                        tr(" non-reference category equation(s))", " ecuación(es) de categoría no-referencia)")
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
                    "The multinomial-logit model treats the dependent variable's categories as unordered, and fits one log-odds equation per non-reference category versus the reference category, all estimated jointly.",
                    "Before interpreting the model, review that every category has a reasonable number of cases and that the number of estimated parameters is small relative to the smallest category.",
                    "A model can be computed even when some categories are sparse, but coefficient estimates and standard errors become unstable as the smallest category shrinks relative to the number of parameters."
                ),
                c(
                    "El modelo logit multinomial trata las categorías de la variable dependiente como no ordenadas, y ajusta una ecuación de log-momios por categoría no-referencia frente a la categoría de referencia, todas estimadas conjuntamente.",
                    "Antes de interpretar el modelo, revise que cada categoría tenga un número razonable de casos y que el número de parámetros estimados sea pequeño frente a la categoría más pequeña.",
                    "Un modelo puede calcularse incluso cuando alguna categoría es escasa, pero las estimaciones de coeficientes y sus errores estándar se vuelven inestables a medida que la categoría más pequeña se achica frente al número de parámetros."
                )
            )))

            add_row(self$results$design, "design_1", list(element = tr("Total cases", "Casos totales"), value = as.character(n_total)))
            add_row(self$results$design, "design_2", list(element = tr("Complete cases", "Casos completos"), value = as.character(n_complete)))
            add_row(self$results$design, "design_3", list(element = tr("Number of categories", "Número de categorías"), value = as.character(n_levels)))
            add_row(self$results$design, "design_4", list(element = tr("Reference category", "Categoría de referencia"), value = html_escape(reference_label)))
            add_row(self$results$design, "design_5", list(element = tr("Smallest category", "Categoría más pequeña"), value = paste0(html_escape(min_cat_name), " (n = ", min_cat_n, ")")))
            add_row(self$results$design, "design_6", list(element = tr("Estimated parameters", "Parámetros estimados"), value = as.character(n_model_params)))

            for (i in seq_along(cat_counts)) {
                add_row(self$results$categoryDistribution, paste0("cat_", i), list(
                    category = names(cat_counts)[i],
                    n = as.integer(cat_counts[i]),
                    proportion = as.numeric(cat_counts[i]) / n_complete
                ))
            }

            # -----------------------------------------------------------------------------
            # Complete / quasi-complete separation: same coefficient/SE
            # screen as ordCheck, adapted to loop over the coefficient
            # MATRIX (non-reference categories x predictor terms) instead
            # of a single coefficient vector. The "(Intercept)" column is
            # excluded from the screen, mirroring ordCheck's exclusion of
            # its cutpoint rows: a large intercept is common on its own and
            # is not itself a separation signal the way an extreme SLOPE
            # is.
            # ES: Separación completa/cuasi-completa: mismo cribado de
            # coeficiente/EE que ordCheck, adaptado para recorrer la MATRIZ
            # de coeficientes (categorías no-referencia x términos
            # predictores) en vez de un solo vector. La columna
            # "(Intercept)" se excluye del cribado, reflejando la exclusión
            # de las filas de punto de corte de ordCheck: un intercepto
            # grande es común por sí solo y no es en sí una señal de
            # separación de la forma en que sí lo es una PENDIENTE extrema.
            # -----------------------------------------------------------------------------
            self$results$separationGuide$setContent(html_guide(tr("Complete separation", "Separación completa"), tr(
                c(
                    "Complete or quasi-complete separation occurs when a predictor (or combination of predictors) perfectly or nearly perfectly predicts which category a case falls into.",
                    "Very large coefficients together with very large standard errors are the typical signal of this problem.",
                    "This check only screens for the extreme coefficient/standard-error signature in the predictor (slope) terms of each non-reference-category equation; it does not guarantee adequate cell counts across every category combination."
                ),
                c(
                    "La separación completa o cuasi-completa ocurre cuando un predictor (o combinación de predictores) predice perfecta o casi perfectamente en qué categoría cae un caso.",
                    "Coeficientes muy grandes junto con errores estándar muy grandes son la señal típica de este problema.",
                    "Esta revisión solo detecta la firma extrema de coeficiente/error estándar en los términos de predictores (pendientes) de cada ecuación de categoría no-referencia; no garantiza conteos de celda adecuados en cada combinación de categorías."
                )
            )))

            n_separations <- 0
            slope_terms <- setdiff(colnames(coef_matrix), "(Intercept)")
            if (length(slope_terms) > 0) {
                for (category in rownames(coef_matrix)) {
                    for (term in slope_terms) {
                        coef_val <- coef_matrix[category, term]
                        se_val <- se_matrix[category, term]
                        if (!is.na(coef_val) && !is.na(se_val) && abs(coef_val) > 5 && se_val > 2) {
                            n_separations <- n_separations + 1
                            add_row(self$results$separation, paste0("sep_", n_separations), list(
                                predictor = term,
                                category = category,
                                type = tr("Complete separation", "Separación completa"),
                                coefficient = coef_val
                            ))
                        }
                    }
                }
            }
            if (n_separations == 0) {
                add_row(self$results$separation, "sep_none", list(
                    predictor = tr("Not detected", "No detectada"), category = "-", type = "-", coefficient = NA
                ))
            }

            self$results$separationInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (n_separations > 0) {
                    tr(
                        paste0(n_separations, " predictor-category combination(s) show coefficients and standard errors consistent with complete or quasi-complete separation. Estimates for these terms are not reliable and should not be interpreted at face value."),
                        paste0(n_separations, " combinación(es) predictor-categoría muestran coeficientes y errores estándar consistentes con separación completa o cuasi-completa. Las estimaciones de esos términos no son confiables y no deben interpretarse literalmente.")
                    )
                } else {
                    tr(
                        "No coefficient/standard-error pattern consistent with complete separation was detected in the predictor (slope) terms of any category equation.",
                        "No se detectó ningún patrón de coeficiente/error estándar consistente con separación completa en los términos de predictores (pendientes) de ninguna ecuación de categoría."
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Linearity in the multinomial logit: for each numeric
            # predictor, an x*log(x) interaction term is added to EVERY
            # non-reference-category equation at once (nnet::multinom()
            # fits them jointly, so a single added term in the formula
            # expands to K-1 new coefficients), and the resulting model is
            # compared to the original one with a likelihood-ratio test
            # (df = K-1) - a single joint test per predictor, cleaner than
            # a per-category Wald table, mirroring how ordCheck already
            # frames this as one test per predictor.
            # ES: Linealidad en el logit multinomial: para cada predictor
            # numérico, se agrega un término de interacción x*log(x) a
            # TODAS las ecuaciones de categoría no-referencia a la vez
            # (nnet::multinom() las ajusta conjuntamente, así que un solo
            # término agregado en la fórmula se expande a K-1 coeficientes
            # nuevos), y el modelo resultante se compara con el original
            # mediante una prueba de razón de verosimilitud (gl = K-1) -
            # una sola prueba conjunta por predictor, más limpia que una
            # tabla de Wald por categoría, reflejando cómo ordCheck ya
            # enmarca esto como una prueba por predictor.
            # -----------------------------------------------------------------------------
            self$results$linearityGuide$setContent(html_guide(tr("Linearity in the multinomial logit", "Linealidad en el logit multinomial"), tr(
                c(
                    "The multinomial-logit model does not assume the outcome relates linearly to the predictors, but that each category's log-odds (versus the reference) does.",
                    "This assumption is checked with a Box-Tidwell-style procedure, adding an interaction term between each numeric predictor and its own logarithm to every non-reference-category equation at once, then comparing the two models with a likelihood-ratio test.",
                    "A significant test suggests the real relationship is not linear in the logit for at least one category and the predictor may need a transformation or a nonlinear term.",
                    "This check only applies to numeric predictors; categorical predictors do not have this assumption."
                ),
                c(
                    "El modelo logit multinomial no asume que el desenlace se relacione linealmente con los predictores, sino que el log-momio de cada categoría (frente a la referencia) lo haga.",
                    "Este supuesto se revisa con un procedimiento estilo Box-Tidwell, agregando un término de interacción entre cada predictor numérico y su propio logaritmo a todas las ecuaciones de categoría no-referencia a la vez, y comparando ambos modelos con una prueba de razón de verosimilitud.",
                    "Una prueba significativa sugiere que la relación real no es lineal en el logit para al menos una categoría y el predictor podría necesitar una transformación o un término no lineal.",
                    "Esta revisión solo aplica a predictores numéricos; los predictores categóricos no tienen este supuesto."
                )
            )))

            bt_tested <- 0
            bt_significant <- 0
            lr_df <- n_levels - 1L
            ll_full_model <- as.numeric(stats::logLik(model))
            if (length(covs) > 0) {
                for (predictor in covs) {
                    x <- model_data_complete[[predictor]]
                    if (!is.numeric(x) || any(x <= 0, na.rm = TRUE)) next()

                    bt_data <- model_data_complete
                    bt_data$.bt_log_int <- x * log(x)
                    bt_formula <- stats::as.formula(paste(
                        "dep_nom ~", paste(qname(predictors), collapse = " + "), "+", qname(".bt_log_int")
                    ))

                    bt_model <- tryCatch(
                        nnet::multinom(bt_formula, data = bt_data, trace = FALSE),
                        error = function(e) NULL
                    )
                    if (is.null(bt_model)) next()

                    ll_bt_model <- as.numeric(stats::logLik(bt_model))
                    lr_stat <- 2 * (ll_bt_model - ll_full_model)
                    if (!is.finite(lr_stat) || lr_stat < 0) next()

                    lr_p <- stats::pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

                    bt_tested <- bt_tested + 1
                    if (!is.na(lr_p) && lr_p < 0.05) bt_significant <- bt_significant + 1

                    add_row(self$results$linearity, paste0("lin_", predictor), list(
                        predictor = predictor, chiSq = lr_stat, df = lr_df, p = lr_p, pSig = p_sig(lr_p)
                    ))
                }
            }

            self$results$linearityInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (bt_tested == 0) {
                    tr("No numeric predictors were available to test for linearity in the multinomial logit.",
                       "No había predictores numéricos disponibles para revisar la linealidad en el logit multinomial.")
                } else {
                    c(
                        tr(
                            paste0(bt_significant, " of ", bt_tested, " numeric predictor(s) showed a significant likelihood-ratio test, suggesting the relationship with the logit may not be linear for those predictors in at least one category."),
                            paste0(bt_significant, " de ", bt_tested, " predictor(es) numérico(s) mostraron una prueba de razón de verosimilitud significativa, lo que sugiere que la relación con el logit podría no ser lineal para esos predictores en al menos una categoría.")
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
            # Independence of Irrelevant Alternatives (IIA) -
            # Hausman-McFadden test, implemented manually (see file header
            # for the full rationale and the numerical-degeneracy guard).
            # ES: Independencia de Alternativas Irrelevantes (IIA) - prueba
            # de Hausman-McFadden, implementada manualmente (ver encabezado
            # del archivo para el razonamiento completo y la guarda de
            # degeneración numérica).
            # -----------------------------------------------------------------------------
            self$results$iiaGuide$setContent(html_guide(
                tr("Independence of Irrelevant Alternatives (IIA)", "Independencia de alternativas irrelevantes (IIA)"),
                tr(
                    c(
                        "The multinomial-logit model assumes the relative odds between any two categories do not depend on which OTHER categories are also available - Independence of Irrelevant Alternatives (IIA), the one assumption unique to this model family.",
                        "The Hausman-McFadden (1984) test checks this by refitting the model after removing every case belonging to one category at a time; if IIA holds, the coefficients shared with the full model should not change beyond sampling error.",
                        "One row is shown per omitted category. A significant test for an omitted category is evidence against IIA; a non-significant test is compatible with it.",
                        "In finite samples, the test statistic's denominator (a covariance-matrix difference) is not always invertible - a well-documented degeneracy of this specific test, not a coding error - and is reported here as 'not computable' rather than a misleading number."
                    ),
                    c(
                        "El modelo logit multinomial asume que los momios relativos entre dos categorías cualesquiera no dependen de qué OTRAS categorías también estén disponibles - Independencia de Alternativas Irrelevantes (IIA), el único supuesto propio de esta familia de modelos.",
                        "La prueba de Hausman-McFadden (1984) revisa esto reajustando el modelo tras eliminar, de a una, todos los casos que pertenecen a una categoría; si la IIA se sostiene, los coeficientes compartidos con el modelo completo no deberían cambiar más allá del error de muestreo.",
                        "Se muestra una fila por categoría omitida. Una prueba significativa para una categoría omitida es evidencia en contra de la IIA; una prueba no significativa es compatible con ella.",
                        "En muestras finitas, el denominador del estadístico de prueba (una diferencia de matrices de covarianza) no siempre es invertible - una degeneración bien documentada de esta prueba específica, no un error de programación - y se reporta aquí como 'no calculable' en vez de un número engañoso."
                    )
                )
            ))

            iia_predictors_rhs <- paste(qname(predictors), collapse = " + ")
            vcov_full <- tryCatch(stats::vcov(model), error = function(e) NULL)

            iia_any_significant <- FALSE
            iia_any_computable <- FALSE

            for (omitted_category in dep_levels) {
                restricted_data <- model_data_complete[model_data_complete$dep_nom != omitted_category, ]
                restricted_levels <- setdiff(dep_levels, omitted_category)
                restricted_data$dep_nom <- factor(as.character(restricted_data$dep_nom), levels = restricted_levels)

                restricted_model <- tryCatch(
                    nnet::multinom(
                        stats::as.formula(paste("dep_nom ~", iia_predictors_rhs)),
                        data = restricted_data, trace = FALSE
                    ),
                    error = function(e) NULL
                )

                if (is.null(restricted_model) || is.null(vcov_full)) {
                    add_row(self$results$iia, paste0("iia_", omitted_category), list(
                        category = omitted_category, chiSq = NA, df = NA, p = NA, pSig = "",
                        note = tr("Restricted model could not be fit", "No se pudo ajustar el modelo restringido")
                    ))
                    next()
                }

                coef_restricted <- coef(restricted_model)
                vcov_restricted <- tryCatch(stats::vcov(restricted_model), error = function(e) NULL)

                # When only one non-reference category remains in the
                # restricted fit, coef()/vcov() drop the category prefix
                # from their names (there is no ambiguity left to resolve)
                # - both are re-labelled here with the surviving category's
                # name so they can be matched against the full model's
                # "category:term" naming below.
                # ES: Cuando en el ajuste restringido queda solo una
                # categoría no-referencia, coef()/vcov() eliminan el
                # prefijo de categoría de sus nombres (ya no hay ninguna
                # ambigüedad que resolver) - ambos se vuelven a etiquetar
                # aquí con el nombre de la categoría sobreviviente para que
                # puedan compararse con el nombrado "categoría:término" del
                # modelo completo, más abajo.
                if (is.null(dim(coef_restricted))) {
                    surviving_category <- restricted_levels[2]
                    coef_restricted <- matrix(coef_restricted, nrow = 1, dimnames = list(surviving_category, names(coef_restricted)))
                    if (!is.null(vcov_restricted)) {
                        dimnames(vcov_restricted) <- list(
                            paste(surviving_category, dimnames(vcov_restricted)[[1]], sep = ":"),
                            paste(surviving_category, dimnames(vcov_restricted)[[2]], sep = ":")
                        )
                    }
                }

                if (is.null(vcov_restricted)) {
                    add_row(self$results$iia, paste0("iia_", omitted_category), list(
                        category = omitted_category, chiSq = NA, df = NA, p = NA, pSig = "",
                        note = tr("Covariance matrix not available for the restricted model", "Matriz de covarianza no disponible para el modelo restringido")
                    ))
                    next()
                }

                common_categories <- intersect(rownames(coef_matrix), rownames(coef_restricted))
                common_terms <- intersect(colnames(coef_matrix), colnames(coef_restricted))

                if (length(common_categories) == 0 || length(common_terms) == 0) {
                    add_row(self$results$iia, paste0("iia_", omitted_category), list(
                        category = omitted_category, chiSq = NA, df = NA, p = NA, pSig = "",
                        note = tr("No coefficients are common to both models", "Ningún coeficiente es común a ambos modelos")
                    ))
                    next()
                }

                param_names <- as.vector(t(outer(common_categories, common_terms, paste, sep = ":")))
                b_full <- as.vector(t(coef_matrix[common_categories, common_terms, drop = FALSE]))
                b_restricted <- as.vector(t(coef_restricted[common_categories, common_terms, drop = FALSE]))
                names(b_full) <- param_names
                names(b_restricted) <- param_names

                v_full <- vcov_full[param_names, param_names, drop = FALSE]
                v_restricted <- vcov_restricted[param_names, param_names, drop = FALSE]

                coef_diff <- b_restricted - b_full
                vcov_diff <- v_restricted - v_full
                df_iia <- length(coef_diff)

                hausman_stat <- tryCatch({
                    vcov_diff_inv <- solve(vcov_diff)
                    stat <- as.numeric(t(coef_diff) %*% vcov_diff_inv %*% coef_diff)
                    if (!is.finite(stat) || stat < 0) NA_real_ else stat
                }, error = function(e) NA_real_)

                if (is.na(hausman_stat)) {
                    add_row(self$results$iia, paste0("iia_", omitted_category), list(
                        category = omitted_category, chiSq = NA, df = df_iia, p = NA, pSig = "",
                        note = tr(
                            "Not computable (the covariance-matrix difference is not invertible - often itself evidence against a genuine IIA violation)",
                            "No calculable (la diferencia de matrices de covarianza no es invertible - a menudo, en sí misma, evidencia en contra de una violación genuina de la IIA)"
                        )
                    ))
                    next()
                }

                iia_any_computable <- TRUE
                hausman_p <- stats::pchisq(hausman_stat, df = df_iia, lower.tail = FALSE)
                if (!is.na(hausman_p) && hausman_p < 0.05) iia_any_significant <- TRUE

                add_row(self$results$iia, paste0("iia_", omitted_category), list(
                    category = omitted_category, chiSq = hausman_stat, df = df_iia,
                    p = hausman_p, pSig = p_sig(hausman_p), note = ""
                ))
            }

            self$results$iiaInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (!iia_any_computable) {
                    tr(
                        "The Hausman-McFadden statistic could not be computed for any omitted category in this model - most often because the covariance-matrix difference was not positive definite, itself a known and fairly common outcome of this test, not necessarily evidence of a problem with the model.",
                        "El estadístico de Hausman-McFadden no pudo calcularse para ninguna categoría omitida en este modelo - con mayor frecuencia porque la diferencia de matrices de covarianza no fue definida positiva, en sí misma un resultado conocido y bastante común de esta prueba, no necesariamente evidencia de un problema con el modelo."
                    )
                } else if (iia_any_significant) {
                    tr(
                        "At least one omitted category produced a significant Hausman-McFadden statistic, suggesting the Independence of Irrelevant Alternatives assumption may not hold for this model - the relative odds between the remaining categories shift when that category is removed. Consider a nested-logit or multinomial-probit model, which do not require IIA, before relying heavily on this model's odds ratios.",
                        "Al menos una categoría omitida produjo un estadístico de Hausman-McFadden significativo, lo que sugiere que el supuesto de Independencia de Alternativas Irrelevantes podría no sostenerse para este modelo - los momios relativos entre las categorías restantes cambian al eliminar esa categoría. Considere un modelo logit anidado o probit multinomial, que no requieren IIA, antes de confiar demasiado en los odds ratios de este modelo."
                    )
                } else {
                    tr(
                        "No omitted category produced a significant Hausman-McFadden statistic, compatible with the Independence of Irrelevant Alternatives assumption holding for this model.",
                        "Ninguna categoría omitida produjo un estadístico de Hausman-McFadden significativo, compatible con que el supuesto de Independencia de Alternativas Irrelevantes se sostenga para este modelo."
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Goodness of fit - soft dependency on 'generalhoslem', same
            # guard pattern as ordCheck.
            # ES: Bondad de ajuste - dependencia opcional del paquete
            # 'generalhoslem', mismo patrón de guarda que ordCheck.
            # -----------------------------------------------------------------------------
            self$results$goodnessOfFitGuide$setContent(html_guide(tr("Goodness of fit", "Bondad de ajuste"), tr(
                c(
                    "Goodness of fit evaluates whether the model, as a whole, reasonably reproduces the observed category frequencies.",
                    "The Lipsitz (1996) and Pulkstenis-Robinson (2004) tests were developed for the proportional-odds model but generalhoslem documents support for 'multinom' fitted objects as well; they are used here for consistency with ordCheck's battery, since no goodness-of-fit test specific to the unordered multinomial case is as widely implemented.",
                    "The Pulkstenis-Robinson test needs at least one categorical predictor to partition cases into covariate patterns; it is not shown when only numeric predictors are selected. It also needs at least as many distinct covariate patterns as groups to be valid (Fagerland & Hosmer, 2013) - with very few categorical predictors or very few levels, treat a result from this test with extra caution.",
                    "As with any chi-square-based fit test, these lose power with very small samples and can flag trivially small lack of fit with very large ones - read them together with the IIA result and substantive knowledge of the data."
                ),
                c(
                    "La bondad de ajuste evalúa si el modelo, en conjunto, reproduce razonablemente las frecuencias de categoría observadas.",
                    "Las pruebas de Lipsitz (1996) y Pulkstenis-Robinson (2004) se desarrollaron para el modelo de momios proporcionales, pero generalhoslem documenta soporte también para objetos ajustados 'multinom'; se usan aquí por consistencia con la batería de ordCheck, ya que no hay una prueba de bondad de ajuste específica para el caso multinomial no ordenado tan ampliamente implementada.",
                    "La prueba de Pulkstenis-Robinson necesita al menos un predictor categórico para particionar los casos en patrones de covariables; no se muestra cuando solo hay predictores numéricos seleccionados. También necesita al menos tantos patrones de covariables distintos como grupos para ser válida (Fagerland & Hosmer, 2013) - con muy pocos predictores categóricos o muy pocos niveles, trate un resultado de esta prueba con cautela adicional.",
                    "Como cualquier prueba de ajuste basada en ji-cuadrado, estas pierden poder con muestras muy pequeñas y pueden marcar una falta de ajuste trivial con muestras muy grandes - léalas junto con el resultado de IIA y el conocimiento sustantivo de los datos."
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
                } else {
                    add_row(self$results$goodnessOfFit, "gof_lipsitz_err", list(
                        test = tr("Lipsitz (not computable for this model)", "Lipsitz (no calculable para este modelo)"),
                        statistic = NA, df = NA, p = NA, pSig = ""
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
                    } else {
                        add_row(self$results$goodnessOfFit, "gof_pr_chisq_err", list(
                            test = tr("Pulkstenis-Robinson chi-squared (not computable for this model)", "Pulkstenis-Robinson ji-cuadrado (no calculable para este modelo)"),
                            statistic = NA, df = NA, p = NA, pSig = ""
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
                    } else {
                        add_row(self$results$goodnessOfFit, "gof_pr_dev_err", list(
                            test = tr("Pulkstenis-Robinson deviance (not computable for this model)", "Pulkstenis-Robinson devianza (no calculable para este modelo)"),
                            statistic = NA, df = NA, p = NA, pSig = ""
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
            # Pseudo-R² (discrimination replacement, same rationale as
            # ordCheck: no single ROC/AUC once the outcome has 3+ unordered
            # categories).
            # ES: Pseudo-R² (reemplazo de discriminación, mismo
            # razonamiento que ordCheck: no hay un único ROC/AUC una vez
            # que el desenlace tiene 3+ categorías no ordenadas).
            # -----------------------------------------------------------------------------
            self$results$discriminationGuide$setContent(html_guide(tr("Pseudo-R²", "Pseudo-R²"), tr(
                c(
                    "There is no single ROC curve or AUC once the outcome has more than two unordered categories, so overall model quality is summarized here with pseudo-R² measures instead, complemented by the classification table below.",
                    "McFadden's pseudo-R² compares the fitted model's log-likelihood to an intercept-only model; values of .20-.40 are often described as a good fit, but this rule of thumb is much less established than R² conventions for linear regression.",
                    "Nagelkerke's pseudo-R² rescales a related measure so it can reach 1.0, making it more directly comparable across models, but it is not on the same numeric scale as a linear-regression R² and should not be interpreted as \"percent of variance explained\" in that sense."
                ),
                c(
                    "No hay una única curva ROC ni AUC una vez que el desenlace tiene más de dos categorías no ordenadas, así que aquí la calidad general del modelo se resume con medidas de pseudo-R² en cambio, complementadas con la tabla de clasificación de abajo.",
                    "El pseudo-R² de McFadden compara la log-verosimilitud del modelo ajustado contra un modelo solo con intercepto; valores de .20-.40 suelen describirse como un buen ajuste, pero esta regla es mucho menos establecida que las convenciones de R² en regresión lineal.",
                    "El pseudo-R² de Nagelkerke reescala una medida relacionada para que pueda llegar a 1.0, haciéndolo más directamente comparable entre modelos, pero no está en la misma escala numérica que un R² de regresión lineal y no debería interpretarse como \"porcentaje de varianza explicada\" en ese sentido."
                )
            )))

            ll_model <- ll_full_model
            ll_null <- if (!is.null(null_model)) as.numeric(stats::logLik(null_model)) else NA_real_

            mcfadden_r2 <- if (!is.na(ll_null)) 1 - (ll_model / ll_null) else NA_real_
            nagelkerke_r2 <- if (!is.na(ll_null)) {
                num <- 1 - exp((2 / n_complete) * (ll_null - ll_model))
                den <- 1 - exp((2 / n_complete) * ll_null)
                num / den
            } else NA_real_

            add_row(self$results$discrimination, "disc_1", list(metric = "McFadden R²", value = mcfadden_r2))
            add_row(self$results$discrimination, "disc_2", list(metric = "Nagelkerke R²", value = nagelkerke_r2))

            self$results$discriminationInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (is.na(mcfadden_r2)) {
                    tr("Pseudo-R² could not be computed (the null model failed to converge).",
                       "El pseudo-R² no pudo calcularse (el modelo nulo no convergió).")
                } else {
                    tr(
                        paste0("McFadden R² = ", fmt_num(mcfadden_r2, 3), "; Nagelkerke R² = ", fmt_num(nagelkerke_r2, 3), "."),
                        paste0("McFadden R² = ", fmt_num(mcfadden_r2, 3), "; Nagelkerke R² = ", fmt_num(nagelkerke_r2, 3), ".")
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Classification (confusion) table: observed vs. predicted
            # category (predicted = arg-max of the fitted probabilities,
            # via stats::predict(model)). See file header for why this is
            # a genuinely new diagnostic for the unordered/nominal case.
            # ES: Tabla de clasificación (matriz de confusión): categoría
            # observada vs. predicha (predicha = el máximo de las
            # probabilidades ajustadas, vía stats::predict(model)). Ver el
            # encabezado del archivo para por qué este es un diagnóstico
            # genuinamente nuevo para el caso no ordenado/nominal.
            # -----------------------------------------------------------------------------
            self$results$classificationGuide$setContent(html_guide(tr("Classification table", "Tabla de clasificación"), tr(
                c(
                    "Each case is assigned to the category with the highest fitted probability under the model, then compared against its actually observed category.",
                    "Recall (the proportion of each observed category correctly predicted) is shown by row; overall accuracy is reported below the table, alongside the chance baseline for this many categories (1 / number of categories, if the categories were equally likely).",
                    "A model can have a statistically significant pseudo-R² while still classifying poorly in practical terms - this table is the more concrete, applied complement to the pseudo-R² figures above."
                ),
                c(
                    "Cada caso se asigna a la categoría con la probabilidad ajustada más alta bajo el modelo, y luego se compara contra su categoría realmente observada.",
                    "El recall (la proporción de cada categoría observada predicha correctamente) se muestra por fila; la exactitud global se reporta debajo de la tabla, junto con la línea base de azar para este número de categorías (1 / número de categorías, si las categorías fueran igual de probables).",
                    "Un modelo puede tener un pseudo-R² estadísticamente significativo y aun así clasificar mal en términos prácticos - esta tabla es el complemento más concreto y aplicado de las cifras de pseudo-R² de arriba."
                )
            )))

            private$.ensureClassificationColumns(dep_levels)

            predicted_class <- tryCatch(stats::predict(model), error = function(e) NULL)
            overall_accuracy <- NA_real_

            if (!is.null(predicted_class)) {
                confusion <- table(
                    observed = model_data_complete$dep_nom,
                    predicted = factor(predicted_class, levels = dep_levels)
                )

                n_correct <- 0
                for (i in seq_along(dep_levels)) {
                    row_label <- dep_levels[i]
                    row_counts <- as.integer(confusion[row_label, ])
                    row_total <- sum(row_counts)
                    row_recall <- if (row_total > 0) row_counts[i] / row_total else NA_real_
                    n_correct <- n_correct + row_counts[i]

                    row_values <- list(observed = row_label, total = row_total, recall = row_recall)
                    for (j in seq_along(dep_levels)) row_values[[paste0("pred", j)]] <- row_counts[j]

                    add_row(self$results$classification, paste0("class_", i), row_values)
                }

                overall_accuracy <- n_correct / n_complete
            } else {
                add_row(self$results$classification, "class_none", list(
                    observed = tr("Predictions could not be computed", "No se pudieron calcular las predicciones")
                ))
            }

            chance_baseline <- 1 / n_levels

            self$results$classificationInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (is.na(overall_accuracy)) {
                    tr("The classification table could not be built for this model.",
                       "No se pudo construir la tabla de clasificación para este modelo.")
                } else {
                    tr(
                        paste0("Overall accuracy = ", fmt_num(100 * overall_accuracy, 1), "%, against a ", fmt_num(100 * chance_baseline, 1), "% chance baseline for ", n_levels, " equally likely categories. Compare accuracy to this baseline, not to 100%: even a modest gain over chance can reflect a genuinely useful model when the categories are hard to tell apart."),
                        paste0("Exactitud global = ", fmt_num(100 * overall_accuracy, 1), "%, contra una línea base de azar de ", fmt_num(100 * chance_baseline, 1), "% para ", n_levels, " categorías igual de probables. Compare la exactitud contra esta línea base, no contra el 100%: incluso una ganancia modesta sobre el azar puede reflejar un modelo genuinamente útil cuando las categorías son difíciles de distinguir.")
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Multicollinearity: manual VIF/tolerance from the predictors'
            # own design matrix - copied verbatim from ordCheck/logCheck/
            # regCheck's block (a property of the predictors alone,
            # entirely independent of the response family).
            # ES: Multicolinealidad: VIF/tolerancia manual desde la propia
            # matriz de diseño de los predictores - copiado sin cambios del
            # bloque de ordCheck/logCheck/regCheck (una propiedad solo de
            # los predictores, totalmente independiente de la familia del
            # desenlace).
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

                    add_multi(tr("Minimum eigenvalue", "Eigenvalue mínimo"), tr("Design matrix", "Matriz de diseño"), "λ min", min_eig)
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
            # Correlation matrices (Pearson / dCor / copula entropy) among
            # the NUMERIC PREDICTORS ONLY - see file header for why the
            # unordered dependent variable is excluded here (unlike
            # ordCheck, whose ordinal outcome is defensibly integer-coded).
            # ES: Matrices de correlación (Pearson / dCor / entropía
            # copular) SOLO entre los predictores numéricos - ver el
            # encabezado del archivo para por qué la variable dependiente
            # no ordenada se excluye aquí (a diferencia de ordCheck, cuyo
            # desenlace ordinal se codifica como entero de forma
            # defendible).
            # -----------------------------------------------------------------------------
            matVars <- covs
            k <- length(matVars)

            corrData <- model_data_complete

            self$results$correlationMatrixGuide$setContent(html_guide(tr("Correlation Matrix (numeric predictors)", "Matriz de Correlaciones (predictores numéricos)"), tr(
                c(
                    "These two tables complement the multicollinearity checks with an overview of the association among the numeric predictors, each in APA 7 format (lower triangle, numbered variables).",
                    "The first reports conventional Pearson correlation (linear association only); the second reports distance correlation (Székely et al., 2007), which detects linear and non-linear association alike.",
                    "The dependent variable is deliberately excluded from this matrix: unlike ordCheck's ordinal outcome, this variable's categories are unordered, so there is no defensible way to integer-code it for a correlation coefficient - its relationship to the predictors is instead reported through the coefficients/odds-ratios table."
                ),
                c(
                    "Estas dos tablas complementan las pruebas de multicolinealidad con una vista general de la asociación entre los predictores numéricos, cada una en formato APA 7 (triángulo inferior, variables numeradas).",
                    "La primera reporta la correlación de Pearson convencional (solo detecta asociación lineal); la segunda reporta la correlación de distancia (Székely et al., 2007), que detecta asociación lineal y no lineal por igual.",
                    "La variable dependiente se excluye deliberadamente de esta matriz: a diferencia del desenlace ordinal de ordCheck, las categorías de esta variable no están ordenadas, así que no hay una forma defendible de codificarla como entero para un coeficiente de correlación - su relación con los predictores se reporta en cambio mediante la tabla de coeficientes/odds ratios."
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
                            pearsonVals[[colName]] <- "—"
                            dcorVals[[colName]] <- "—"
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
                        tr("No pair shows a Pearson/dCor gap greater than 0.10; there is no indication of unmodeled non-linear association among the model's numeric predictors.",
                           "Ningún par muestra una brecha Pearson/dCor mayor a 0.10; no hay indicios de asociación no lineal no modelada entre los predictores numéricos del modelo.")
                    } else {
                        tr(paste0(notable_pairs, " pair(s) show a notable Pearson/dCor gap; inspect a scatterplot before concluding the relationship is non-linear."),
                           paste0(notable_pairs, " par(es) muestran una brecha Pearson/dCor notable; revise un diagrama de dispersión antes de concluir que la relación es no lineal."))
                    },
                    paragraphs = TRUE
                ))
            }

            # -----------------------------------------------------------------------------
            # Influential cases: per-case Pearson-type residual on the
            # observed category's fitted probability - copied verbatim
            # from ordCheck (see that file's header for the rationale and
            # its limits; predict(..., type = "probs") returns an n x K
            # matrix for "multinom" objects too, so the formula is
            # identical).
            # ES: Casos influyentes: residuo tipo Pearson por caso sobre la
            # probabilidad ajustada de la categoría observada - copiado sin
            # cambios de ordCheck (ver el encabezado de ese archivo para el
            # razonamiento y sus límites; predict(..., type = "probs")
            # devuelve una matriz n x K también para objetos "multinom",
            # así que la fórmula es idéntica).
            # -----------------------------------------------------------------------------
            self$results$influenceGuide$setContent(html_guide(tr("Influential cases", "Casos influyentes"), tr(
                c(
                    "nnet::multinom() does not expose Cook's D or leverage the way a linear or binary logistic model does, so influence here is screened with a simpler, case-level Pearson-type residual instead: how far the model's fitted probability for the case's OWN observed category falls from a perfect fit.",
                    "Cases are flagged at |residual| > 2.5, the same convention used elsewhere in this suite for standardized-residual screening.",
                    "This is a reasonable approximation, not a single canonical published statistic for this model family - for a fuller graphical treatment of an analogous ordinal case, see Liu et al. (2009).",
                    "A flagged case should not be removed automatically; check whether it is a recording error, a valid but extreme case, or a sign the model does not represent all subgroups well."
                ),
                c(
                    "nnet::multinom() no expone Cook's D ni leverage como sí lo hace un modelo lineal o logístico binario, así que aquí la influencia se criba con un residuo tipo Pearson por caso más simple en cambio: qué tan lejos está la probabilidad ajustada del modelo para la categoría REALMENTE OBSERVADA del caso de un ajuste perfecto.",
                    "Los casos se marcan en |residuo| > 2.5, la misma convención usada en otra parte de esta suite para el cribado de residuos estandarizados.",
                    "Esta es una aproximación razonable, no un único estadístico canónico publicado para esta familia de modelos - para un tratamiento gráfico más completo de un caso ordinal análogo, ver Liu et al. (2009).",
                    "Un caso marcado no debe eliminarse automáticamente; revise si es un error de registro, un caso válido pero extremo, o una señal de que el modelo no representa bien a todos los subgrupos."
                )
            )))

            fitted_probs <- tryCatch(stats::predict(model, type = "probs"), error = function(e) NULL)

            n_influential <- 0
            if (!is.null(fitted_probs)) {
                obs_idx <- as.integer(model_data_complete$dep_nom)
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
                        observed = as.character(model_data_complete$dep_nom[i]),
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
            # Coefficients / odds ratios, one row per predictor TERM x
            # non-reference CATEGORY (including "(Intercept)" - see file
            # header for why no separate intercepts/cutpoints table is
            # used here).
            # ES: Coeficientes / odds ratios, una fila por TÉRMINO
            # predictor x CATEGORÍA no-referencia (incluido "(Intercept)" -
            # ver el encabezado del archivo para por qué aquí no se usa
            # una tabla separada de intercepts/cutpoints).
            # -----------------------------------------------------------------------------
            self$results$coefficientsGuide$setContent(html_guide(
                tr("Coefficients and odds ratios (by category)", "Coeficientes y odds ratios (por categoría)"),
                tr(
                    c(
                        "Each non-reference category has its own set of coefficients: exp(coefficient) is the odds ratio of belonging to THAT category rather than the reference category, for a one-unit increase in the predictor (or for belonging to a predictor's category versus its own reference level).",
                        "The \"(Intercept)\" row's odds ratio is the baseline odds of that category versus the reference when every predictor equals zero (or is at its reference level) - it is not itself a predictor effect and is shown for completeness, not for substantive interpretation.",
                        "Read the confidence interval, not only the point estimate: if it includes 1, the effect is not statistically distinguishable from no association.",
                        "These odds ratios are only meaningfully interpretable to the extent Independence of Irrelevant Alternatives holds - if the Hausman-McFadden test above flags a category, treat that category's odds ratios with additional caution."
                    ),
                    c(
                        "Cada categoría no-referencia tiene su propio conjunto de coeficientes: exp(coeficiente) es el odds ratio de pertenecer a ESA categoría en vez de a la categoría de referencia, por un aumento de una unidad en el predictor (o por pertenecer a una categoría de un predictor frente a su propio nivel de referencia).",
                        "El odds ratio de la fila \"(Intercept)\" es el momio base de esa categoría frente a la referencia cuando todos los predictores valen cero (o están en su nivel de referencia) - no es en sí un efecto de predictor y se muestra por completitud, no para interpretación sustantiva.",
                        "Lea el intervalo de confianza, no solo el estimador puntual: si incluye 1, el efecto no es estadísticamente distinguible de la ausencia de asociación.",
                        "Estos odds ratios solo son interpretables con sentido en la medida en que se sostenga la Independencia de Alternativas Irrelevantes - si la prueba de Hausman-McFadden de arriba marca una categoría, trate los odds ratios de esa categoría con cautela adicional."
                    )
                )
            ))

            ci_array <- tryCatch(stats::confint(model), error = function(e) NULL)

            coef_row_i <- 0
            strongest_abs_log_or <- -Inf
            strongest_label <- NA_character_

            for (category in rownames(coef_matrix)) {
                for (term in colnames(coef_matrix)) {
                    coef_row_i <- coef_row_i + 1

                    estimate <- coef_matrix[category, term]
                    or_val <- exp(estimate)
                    p_val <- p_matrix[category, term]

                    ci_lower <- NA_real_
                    ci_upper <- NA_real_
                    if (!is.null(ci_array)) {
                        ci_slice <- tryCatch({
                            if (length(dim(ci_array)) == 3) ci_array[term, , category] else ci_array[term, ]
                        }, error = function(e) NULL)
                        if (!is.null(ci_slice) && length(ci_slice) == 2) {
                            ci_lower <- exp(ci_slice[1])
                            ci_upper <- exp(ci_slice[2])
                        }
                    }

                    add_row(self$results$coefficients, paste0("coef_", coef_row_i), list(
                        predictor = term,
                        category = category,
                        or = or_val,
                        ciLower = ci_lower,
                        ciUpper = ci_upper,
                        p = p_val,
                        pSig = p_sig(p_val)
                    ))

                    if (!identical(term, "(Intercept)") && is.finite(estimate) && abs(estimate) > strongest_abs_log_or) {
                        strongest_abs_log_or <- abs(estimate)
                        strongest_label <- paste0(term, " (", category, ")")
                    }
                }
            }

            self$results$coefficientsInterpretation$setContent(html_block(
                tr("Applied Interpretation", "Interpretación Aplicada"),
                if (is.na(strongest_label)) {
                    tr("No predictor coefficients were estimated.", "No se estimaron coeficientes de predictores.")
                } else {
                    tr(
                        paste0("Strongest association: ", strongest_label, ", |log(OR)| = ", fmt_num(strongest_abs_log_or, 2), "."),
                        paste0("Asociación más fuerte: ", strongest_label, ", |log(OR)| = ", fmt_num(strongest_abs_log_or, 2), ".")
                    )
                },
                paragraphs = TRUE
            ))

            # -----------------------------------------------------------------------------
            # Notes and recommendation.
            # ES: Notas y recomendación.
            # -----------------------------------------------------------------------------
            recommendations <- c()
            if (iia_any_significant) {
                recommendations <- c(recommendations, tr(
                    "The Independence of Irrelevant Alternatives assumption is not well supported for at least one omitted category; consider a nested-logit or multinomial-probit model.",
                    "El supuesto de Independencia de Alternativas Irrelevantes no está bien respaldado para al menos una categoría omitida; considere un modelo logit anidado o probit multinomial."
                ))
            }
            if (bt_significant > 0) {
                recommendations <- c(recommendations, tr(
                    "At least one numeric predictor shows a non-linear relationship with the multinomial logit; consider a transformation or categorization.",
                    "Al menos un predictor numérico muestra una relación no lineal con el logit multinomial; considere una transformación o categorización."
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
            plot_state <- list(
                reportLang = lang,
                model_data = model_data_complete,
                obs_category = as.character(model_data_complete$dep_nom),
                reference_label = reference_label,
                non_ref_labels = non_ref_labels,
                covs = covs,
                n = n_complete,
                pearson_resid = if (!is.null(fitted_probs)) pearson_resid else rep(NA_real_, n_complete)
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
        # Plot helpers (identical pattern to ordCheck/logCheck:
        # .emptyLogPlot, .plotTr, .plotStyle, .plotPalette, .plotTheme).
        # ES: Helpers de gráficos (patrón idéntico a ordCheck/logCheck).
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
        # Empirical logit vs predictor, one facet per predictor x
        # non-reference-category combination: each category's binary
        # empirical logit is built from only the rows observed as EITHER
        # that category OR the reference category (the same two-category
        # comparison that category's own log-odds equation represents),
        # adapted from ordCheck's single-cutpoint version by faceting on
        # category as well as predictor.
        # ES: Logit empírico vs predictor, un panel por combinación de
        # predictor x categoría no-referencia: el logit empírico binario de
        # cada categoría se construye solo con las filas observadas como
        # ESA categoría O la categoría de referencia (la misma comparación
        # de dos categorías que representa la propia ecuación de log-momios
        # de esa categoría), adaptado de la versión de un solo punto de
        # corte de ordCheck agregando un panel también por categoría.
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

            obs_category <- st$obs_category
            reference_label <- st$reference_label
            non_ref_labels <- st$non_ref_labels

            plot_rows <- list()
            for (predictor in covs) {
                x_full <- st$model_data[[predictor]]
                for (category in non_ref_labels) {
                    mask <- obs_category %in% c(reference_label, category)
                    x_sub <- x_full[mask]
                    y_sub <- as.integer(obs_category[mask] == category)

                    df <- emp_logit_df(x_sub, y_sub, bins = bins)
                    if (!is.null(df)) {
                        df$predictor <- predictor
                        df$category <- category
                        plot_rows[[paste(predictor, category, sep = "|")]] <- df
                    }
                }
            }

            if (length(plot_rows) == 0) {
                return(private$.emptyLogPlot(tr_p(
                    "Not enough variation to plot linearity in the multinomial logit.",
                    "No hay suficiente variación para graficar la linealidad en el logit multinomial."
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
                ggplot2::facet_grid(category ~ predictor, scales = "free_x") +
                ggplot2::labs(
                    x = tr_p("Predictor (binned mean)", "Predictor (media por grupo)"),
                    y = tr_p("Empirical logit (category vs. reference)", "Logit empírico (categoría vs. referencia)"),
                    size = tr_p("Group n", "n del grupo")
                ) +
                private$.plotTheme()

            print(p)
            TRUE
        },

        # -----------------------------------------------------------------------------
        # Pearson-type residual by case - copied verbatim from ordCheck.
        # ES: Residuo tipo Pearson por caso - copiado sin cambios de
        # ordCheck.
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
                highresid = plot_df[abs(plot_df$resid) > threshold, , drop = FALSE],
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
