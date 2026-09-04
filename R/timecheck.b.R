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
# Series Temporales.
# ES: Series Temporales.
#
# This file implements timeCheck: a methodological guide and diagnostic
# battery for time-series modeling. For a selected temporal model (ARIMA,
# SARIMA, ETS, VAR, VECM, GARCH) it shows the static matrix of assumptions,
# diagnostics, methodological role, and associated decision that a
# methodologically defensible analysis with that model should review, and it
# runs the minimum recommended battery of diagnostic tests on the selected
# series. Every empirical result is reported as a graded evidence tier (no
# evidence against / moderate evidence / clear evidence of deviation) with a
# concrete recommended methodological action, never as an absolute
# "assumption met/not met" label — see the Documento Maestro's guidance on
# not over-interpreting assumption tests.
#
# ES: Este archivo implementa timeCheck: una guía metodológica y una batería
# de diagnóstico para el modelado de series temporales. Para un modelo
# temporal seleccionado (ARIMA, SARIMA, ETS, VAR, VECM, GARCH) muestra la
# matriz estática de supuestos, diagnósticos, función metodológica y decisión
# asociada que un análisis metodológicamente defendible con ese modelo debería
# revisar, y ejecuta la batería mínima recomendada de pruebas diagnósticas
# sobre la(s) serie(s) seleccionada(s). Cada resultado empírico se reporta
# como un nivel de evidencia gradual (sin evidencia de incumplimiento /
# evidencia moderada / evidencia clara de incumplimiento) acompañado de una
# acción metodológica concreta, nunca como una etiqueta absoluta de "supuesto
# cumplido/no cumplido" — ver la guía del Documento Maestro sobre no
# sobre-interpretar las pruebas de supuestos.
#
# Responsibilities
# 1. Read the selected series, model, and seasonal frequency.
# 2. Resolve every section's text via the shared .al_text() repository
#    (texts.R, section "time") and wire bilingual result/column titles.
# 3. Populate the static model profile and assumption/condition tables.
# 4. Run the minimum recommended diagnostic battery for the selected model,
#    classify each result into a graded evidence tier, and attach a concrete
#    recommended action, wrapping every external computation defensively so
#    a missing package or a numerically unstable fit degrades gracefully
#    instead of failing the whole analysis.
#
# ES: Responsabilidades
# 1. Leer la serie seleccionada, el modelo y la frecuencia estacional.
# 2. Resolver el texto de cada sección vía el repositorio compartido
#    .al_text() (texts.R, sección "time") y cablear los títulos de resultado
#    y de columna bilingües.
# 3. Poblar las tablas estáticas de perfil del modelo y de supuestos/
#    condiciones.
# 4. Ejecutar la batería mínima de diagnóstico recomendada para el modelo
#    seleccionado, clasificar cada resultado en un nivel de evidencia gradual
#    y adjuntar una acción recomendada concreta, envolviendo cada cómputo
#    externo de forma defensiva para que un paquete faltante o un ajuste
#    numéricamente inestable degrade con gracia en vez de fallar todo el
#    análisis.
#
# Workflow
# 1. Read the selected series, model, exogenous variables, and seasonal
#    frequency.
# 2. Wire the bilingual text and the shared statistical/diagnostic
#    helpers.
# 3. Populate the model profile and the static assumption/condition
#    matrix for the selected model.
# 4. Guard: verify enough series are selected and the required packages
#    are installed before running anything.
# 5. Assemble the numeric series data, dropping incomplete rows.
# 6. Fit the selected model (ARIMA, SARIMA, ETS, VAR, VECM, or GARCH) and
#    run its diagnostic battery: stationarity, residual independence/
#    heteroscedasticity/normality, and model-specific checks, populating
#    the forecast/residual plot data along the way.
# 7. Populate plot visibility from the diagnostics' plot data.
# 8. Assemble the cited methodological foundations for the selected
#    model.
# 9. Build the guide, interpretation, and scope panels.
#
# ES: Flujo de trabajo
# 1. Leer la serie o series seleccionadas, el modelo, las variables
#    exógenas y la frecuencia estacional.
# 2. Cablear el texto bilingüe y los auxiliares estadísticos/diagnósticos
#    compartidos.
# 3. Poblar el perfil del modelo y la matriz estática de supuestos/
#    condiciones para el modelo seleccionado.
# 4. Guarda: verificar que haya series suficientes seleccionadas y que
#    los paquetes requeridos estén instalados antes de ejecutar nada.
# 5. Ensamblar los datos numéricos de las series, descartando las filas
#    incompletas.
# 6. Ajustar el modelo seleccionado (ARIMA, SARIMA, ETS, VAR, VECM o
#    GARCH) y ejecutar su batería de diagnóstico: estacionariedad,
#    independencia/heteroscedasticidad/normalidad de los residuos y
#    verificaciones propias del modelo, poblando de paso los datos de
#    los gráficos de pronóstico/residuos.
# 7. Poblar la visibilidad de los gráficos a partir de los datos de
#    diagnóstico.
# 8. Ensamblar los fundamentos metodológicos citados para el modelo
#    seleccionado.
# 9. Construir los paneles de guía, interpretación y alcance.
# -----------------------------------------------------------------------------

timeCheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "timeCheckClass",
    inherit = timeCheckBase,
    private = list(
        .plotModel = NULL,
        .plotSeriesData = NULL,
        .plotIsDate = FALSE,
        .plotAcfData = NULL,
        .plotPacfData = NULL,
        .plotResidualsData = NULL,
        .plotVolatilityData = NULL,
        .plotN = NULL,

        .run = function() {

            series <- self$options$series
            date_var <- self$options$dateVar
            exogenous <- self$options$exogenous
            model <- self$options$model
            freq <- self$options$frequency
            if (is.null(freq) || is.na(freq) || freq < 1) freq <- 1

            private$.plotModel <- model
            private$.plotSeriesData <- NULL
            private$.plotIsDate <- FALSE
            private$.plotAcfData <- NULL
            private$.plotPacfData <- NULL
            private$.plotResidualsData <- NULL
            private$.plotVolatilityData <- NULL
            private$.plotN <- NULL

            # -----------------------------------------------------------------------------
            # Bilingual wiring (AssumptionsLab standard — see regCheck/pathCheck).
            # ES: Cableado bilingüe (estándar AssumptionsLab — ver regCheck/pathCheck).
            # -----------------------------------------------------------------------------
            lang <- .al_normalize_lang(self$options$reportLang)

            tr <- function(en, es) .al_tr(lang, en, es)

            txt <- function(section, key) {
                .al_text(lang, section, key)
            }

            html_escape <- .al_html_escape

            html_block <- function(title = NULL, text, paragraphs = TRUE, raw = FALSE) {
                .al_html_block(title, text, paragraphs = paragraphs, raw = raw)
            }

            set_html_safe <- function(name, html) {
                element <- tryCatch(self$results[[name]], error = function(e) NULL)
                if (is.null(element)) return(invisible(FALSE))
                tryCatch(element$setContent(html), error = function(e) invisible(FALSE))
                invisible(TRUE)
            }

            set_visible_safe <- function(name, visible) {
                element <- tryCatch(self$results[[name]], error = function(e) NULL)
                if (is.null(element)) return(invisible(FALSE))
                tryCatch(element$setVisible(visible), error = function(e) invisible(FALSE))
                invisible(TRUE)
            }

            # -----------------------------------------------------------------------------
            # Statistical helpers shared by every model.
            # ES: Auxiliares estadísticos compartidos por todos los modelos.
            # -----------------------------------------------------------------------------
            alpha_moderate <- 0.05
            alpha_clear <- 0.01

            fmt_p <- function(p) {
                if (is.null(p) || length(p) == 0 || is.na(p)) return("\u2014")
                p <- as.numeric(p)
                if (p < .001) return("< .001")
                formatC(p, digits = 3, format = "f")
            }

            fmt_stat <- function(x, digits = 3) {
                if (is.null(x) || length(x) == 0 || is.na(x)) return("\u2014")
                formatC(as.numeric(x), digits = digits, format = "f")
            }

            classify <- function(p, direction = "low_bad") {
                if (is.null(p) || length(p) == 0 || is.na(p))
                    return(list(tier = "na", label = tr("Not computable", "No calculable")))
                p <- as.numeric(p)
                if (identical(direction, "low_good")) {
                    # H0 is the deviation itself (e.g. "unit root present"). Rejecting
                    # H0 (p <= alpha_moderate) is evidence FOR the assumption, at any
                    # strength below that threshold — a p of .015 and a p of .001 both
                    # mean "reject the unit root", so both read as "no evidence of
                    # deviation". The moderate/clear tiers apply only once we fail to
                    # reject, i.e. once p climbs past alpha_moderate.
                    # ES: H0 es la desviación misma (p. ej. "hay raíz unitaria").
                    # Rechazar H0 (p <= alpha_moderate) es evidencia A FAVOR del
                    # supuesto, con cualquier fuerza por debajo de ese umbral — un p de
                    # .015 y uno de .001 significan ambos "se rechaza la raíz unitaria",
                    # así que ambos se leen como "sin evidencia de incumplimiento". Los
                    # niveles moderado/claro aplican solo cuando no se rechaza, es
                    # decir, cuando p supera a alpha_moderate.
                    if (p <= alpha_moderate) tier <- "none"
                    else if (p <= alpha_moderate * 2) tier <- "moderate"
                    else tier <- "clear"
                } else {
                    if (p >= alpha_moderate) tier <- "none"
                    else if (p >= alpha_clear) tier <- "moderate"
                    else tier <- "clear"
                }
                label <- switch(tier,
                    none = tr("No evidence against", "Sin evidencia de incumplimiento"),
                    moderate = tr("Moderate evidence", "Evidencia moderada"),
                    clear = tr("Clear evidence of deviation", "Evidencia clara de incumplimiento")
                )
                list(tier = tier, label = label)
            }

            # Manual ARCH-LM (Engle, 1982): regresses squared residuals on their own
            # lags; n*R^2 ~ chi-square(lags) under H0 of no ARCH effects. Implemented
            # locally with base stats so the module does not need an extra package
            # just for this one test.
            # ES: ARCH-LM manual (Engle, 1982): regresa los residuos al cuadrado sobre
            # sus propios rezagos; n*R^2 ~ chi-cuadrado(rezagos) bajo H0 de ausencia de
            # efectos ARCH. Implementado localmente con stats base para no requerir un
            # paquete adicional solo para esta prueba.
            arch_lm_test <- function(resid, lags = 12) {
                resid <- as.numeric(resid)
                resid <- resid[!is.na(resid)]
                n <- length(resid)
                if (n < (lags + 10)) return(list(statistic = NA, p.value = NA))
                e2 <- resid^2
                y <- e2[(lags + 1):n]
                x <- sapply(1:lags, function(k) e2[(lags + 1 - k):(n - k)])
                colnames(x) <- paste0("lag", 1:lags)
                df_reg <- as.data.frame(x)
                df_reg$y <- y
                fit <- tryCatch(stats::lm(y ~ ., data = df_reg), error = function(e) NULL)
                if (is.null(fit)) return(list(statistic = NA, p.value = NA))
                r2 <- summary(fit)$r.squared
                stat <- length(y) * r2
                p <- 1 - stats::pchisq(stat, df = lags)
                list(statistic = stat, p.value = p)
            }

            require_pkgs <- function(pkgs) {
                all(vapply(pkgs, function(p) requireNamespace(p, quietly = TRUE), logical(1)))
            }

            missing_pkgs <- function(pkgs) {
                pkgs[!vapply(pkgs, function(p) requireNamespace(p, quietly = TRUE), logical(1))]
            }

            # -----------------------------------------------------------------------------
            # Diagnostic row accumulator. Each call classifies one test result into a
            # graded evidence tier and, when the tier is "moderate" or "clear" (or the
            # row is purely informational), attaches the model-specific recommended
            # action fetched from texts.R ("time" section, "actions" key).
            # ES: Acumulador de filas de diagnóstico. Cada llamada clasifica un
            # resultado de prueba en un nivel de evidencia gradual y, cuando el nivel es
            # "moderada" o "clara" (o la fila es puramente informativa), adjunta la
            # acción recomendada específica del modelo, obtenida de texts.R (sección
            # "time", clave "actions").
            # -----------------------------------------------------------------------------
            results_rows <- list()
            actions_rows <- list()
            model_actions <- NULL

            # -----------------------------------------------------------------------------
            # Synthesis accumulator for the "Lectura metodológica" panel. Parallel to
            # results_rows but internal-only (never rendered as a jamovi table): each
            # entry keeps the raw evidence tier and test label under its action_key
            # ("domain"), so build_methodological_reading() can group results by
            # diagnostic domain and generate real prose from the actual run instead of
            # static text. See texts.R "time.reading" for the templates.
            # ES: Acumulador de síntesis para el panel "Lectura metodológica". Paralelo
            # a results_rows pero solo interno (nunca se renderiza como tabla jamovi):
            # cada entrada guarda el nivel de evidencia crudo y la etiqueta de la
            # prueba bajo su action_key ("dominio"), para que
            # build_methodological_reading() pueda agrupar resultados por dominio de
            # diagnóstico y generar prosa real a partir de la corrida en vez de texto
            # estático. Ver texts.R "time.reading" para las plantillas.
            # -----------------------------------------------------------------------------
            synthesis_rows <- list()

            # Long explanatory sentences (e.g. push_note() notices, or the GARCH
            # pre-fit ARCH-LM elaboration) no longer go directly into the narrow
            # "Evidencia" cell -- that's what was forcing horizontal scroll in the
            # live results panel. Instead the cell holds a short label and the full
            # text is attached as a table footnote (renders wrapped, below the
            # table) via note_footnotes, applied after the table rows are added.
            # ES: Las oraciones largas (p. ej. los avisos de push_note(), o la
            # aclaración del ARCH-LM previo al ajuste en GARCH) ya no van directo a
            # la celda estrecha de "Evidencia" -- eso era lo que forzaba el scroll
            # horizontal en el panel de resultados en vivo. En su lugar, la celda
            # lleva una etiqueta corta y el texto completo se adjunta como nota al
            # pie de la tabla (se renderiza envuelto, debajo de la tabla) vía
            # note_footnotes, aplicado después de agregar las filas.
            note_footnotes <- list()

            domain_groups <- list(
                stationarity       = c("stationarity"),
                residualAutocorr   = c("ljungBox", "ljungBoxSeasonal", "serial"),
                heteroscedasticity = c("archLM"),
                normality          = c("jarqueBera"),
                stability          = c("roots", "stability", "cusum", "nyblom"),
                cointegration      = c("johansen"),
                garchPersistence   = c("persistence"),
                garchAsymmetry     = c("signBias"),
                garchGof           = c("gof")
            )

            model_domains <- list(
                arima  = c("stationarity", "residualAutocorr", "heteroscedasticity", "normality", "stability"),
                sarima = c("stationarity", "residualAutocorr", "heteroscedasticity", "normality", "stability"),
                ets    = c("residualAutocorr", "heteroscedasticity", "normality"),
                var    = c("stationarity", "stability", "residualAutocorr", "heteroscedasticity", "normality"),
                vecm   = c("stationarity", "cointegration", "stability", "residualAutocorr", "heteroscedasticity", "normality"),
                garch  = c("stationarity", "residualAutocorr", "heteroscedasticity", "garchPersistence", "garchAsymmetry", "normality", "garchGof")
            )

            # Groups results by diagnostic domain and turns them into real prose via
            # texts.R templates. Returns NULL (not an empty string) when the domain has
            # no rows or no authored template yet, so the caller can drop it cleanly
            # instead of leaving a blank paragraph.
            # ES: Agrupa resultados por dominio de diagnóstico y los convierte en prosa
            # real vía las plantillas de texts.R. Devuelve NULL (no cadena vacía)
            # cuando el dominio no tiene filas o aún no tiene plantilla autorada, para
            # que quien lo llama lo descarte limpiamente en vez de dejar un párrafo
            # vacío.
            synthesize_domain <- function(domain_key, rows) {
                if (length(rows) == 0) return(NULL)
                templ <- txt("time", "reading")[[domain_key]]
                if (is.null(templ)) return(NULL)

                tiers <- vapply(rows, function(r) r$tier, character(1))
                tests <- vapply(rows, function(r) r$test, character(1))
                n <- length(rows)

                if (n == 1) {
                    frag <- templ$single[[tiers[1]]]
                    if (is.null(frag)) return(NULL)
                    return(sprintf(frag, tests[1]))
                }

                # Some domains (currently: cointegration/johansen) never produce a
                # real none/moderate/clear classification — both the trace and the
                # max-eigenvalue tests are always reported as "info" (a suggested
                # rank, not evidence against an assumption). Without this branch
                # they would fall into all_deviant below, wrongly implying the tests
                # "agree in signaling" something.
                # ES: Algunos dominios (por ahora: cointegration/johansen) nunca
                # producen una clasificación real de none/moderate/clear — tanto la
                # prueba de traza como la de máximo autovalor siempre se reportan
                # como "info" (un rango sugerido, no evidencia en contra de un
                # supuesto). Sin esta rama caerían en all_deviant más abajo,
                # implicando incorrectamente que las pruebas "coinciden en señalar"
                # algo.
                if (all(tiers == "info")) {
                    if (is.null(templ$informational)) return(NULL)
                    return(templ$informational)
                }

                if (all(tiers == "none")) {
                    if (is.null(templ$all_none)) return(NULL)
                    sprintf(templ$all_none, n)
                } else if (!any(tiers == "none")) {
                    if (is.null(templ$all_deviant)) return(NULL)
                    sprintf(templ$all_deviant, n)
                } else {
                    if (is.null(templ$mixed)) return(NULL)
                    dissenting <- tests[tiers != "none"]
                    n_agree <- sum(tiers == "none")
                    verb <- if (length(dissenting) == 1) tr("does", "señala") else tr("do", "señalan")
                    sprintf(templ$mixed, n_agree, n, paste(dissenting, collapse = ", "), verb)
                }
            }

            # Assembles the full "Lectura metodológica" panel for the selected model by
            # walking its applicable domains in order and concatenating each domain's
            # synthesized paragraph, closing with an overall convergence read. Falls
            # back to the static generic text (texts.R "time.interpretation") whenever
            # nothing could be synthesized — e.g. the fit failed, or no domain for this
            # model has an authored template yet — so the panel is never left empty.
            # ES: Ensambla el panel "Lectura metodológica" completo para el modelo
            # seleccionado recorriendo sus dominios aplicables en orden y concatenando
            # el párrafo sintetizado de cada uno, cerrando con una lectura de
            # convergencia general. Recae en el texto genérico estático (texts.R
            # "time.interpretation") cuando no se pudo sintetizar nada — p. ej. el
            # ajuste falló, o ningún dominio de este modelo tiene plantilla autorada
            # todavía — para que el panel nunca quede vacío.
            build_methodological_reading <- function(model) {
                domains <- model_domains[[model]]
                if (is.null(domains) || length(synthesis_rows) == 0)
                    return(txt("time", "interpretation"))

                paragraphs <- character(0)
                for (dk in domains) {
                    keys <- domain_groups[[dk]]
                    rows <- Filter(function(r) r$domain %in% keys, synthesis_rows)
                    frag <- synthesize_domain(dk, rows)
                    if (!is.null(frag) && nzchar(frag)) paragraphs <- c(paragraphs, frag)
                }
                if (length(paragraphs) == 0) return(txt("time", "interpretation"))

                all_tiers <- vapply(synthesis_rows, function(r) r$tier, character(1))
                closing <- if (any(all_tiers == "clear"))
                    tr("Taken together, at least one diagnostic area shows clear evidence of deviation and should be addressed before relying on this fit.",
                       "En conjunto, al menos un área de diagnóstico muestra evidencia clara de incumplimiento y debería atenderse antes de confiar en este ajuste.")
                else if (any(all_tiers == "moderate"))
                    tr("Taken together, the evidence is mixed: some diagnostics raise moderate concerns worth reviewing.",
                       "En conjunto, la evidencia es mixta: algunos diagnósticos plantean dudas moderadas que conviene revisar.")
                else
                    tr("Taken together, the diagnostic battery shows no clear evidence against the assumptions checked for this model.",
                       "En conjunto, la batería de diagnóstico no muestra evidencia clara en contra de los supuestos revisados para este modelo.")

                paste(c(paragraphs, closing), collapse = " ")
            }

            push_diag <- function(test, statistic, p, direction = "low_bad",
                                   action_key = NULL, force_action = FALSE,
                                   evidence_override = NULL, tier_override = NULL,
                                   footnote = NULL) {
                if (!is.null(tier_override)) {
                    default_label <- switch(tier_override,
                        none = tr("No evidence against", "Sin evidencia de incumplimiento"),
                        moderate = tr("Moderate evidence", "Evidencia moderada"),
                        clear = tr("Clear evidence of deviation", "Evidencia clara de incumplimiento"),
                        tr("Informational", "Informativo")
                    )
                    cl <- list(tier = tier_override,
                               label = if (!is.null(evidence_override)) evidence_override else default_label)
                } else if (!is.null(evidence_override)) {
                    cl <- list(tier = "info", label = evidence_override)
                } else {
                    cl <- classify(p, direction)
                }
                statistic_txt <- if (is.character(statistic)) statistic else fmt_stat(statistic)
                row_idx <- as.integer(length(results_rows) + 1)
                results_rows[[row_idx]] <<- list(
                    test = test,
                    statistic = statistic_txt,
                    pvalue = fmt_p(p),
                    evidence = cl$label
                )
                if (!is.null(footnote))
                    note_footnotes[[length(note_footnotes) + 1]] <<- list(row = row_idx, text = footnote)
                show_action <- isTRUE(force_action) || cl$tier %in% c("moderate", "clear")
                if (show_action && !is.null(action_key)) {
                    action_entry <- if (!is.null(model_actions)) model_actions[[action_key]] else NULL
                    connector <- switch(cl$tier,
                        clear = tr("It is recommended to ", "Se recomienda "),
                        moderate = tr("It may be worth considering ", "Podría valorarse "),
                        tr("It may be advisable to consider ", "Conviene considerar ")
                    )
                    if (is.null(action_entry)) {
                        action_text <- tr("No specific recommendation is registered for this diagnostic.",
                                          "No hay una recomendación específica registrada para este diagnóstico.")
                    } else if (is.list(action_entry)) {
                        just <- action_entry$justification
                        sugg <- action_entry$suggestion
                        action_text <- paste0(
                            if (!is.null(just) && nzchar(just)) paste0(just, " ") else "",
                            connector, sugg
                        )
                    } else {
                        # Backward-compatible fallback for any action entry still stored
                        # as a plain string rather than list(justification=, suggestion=).
                        action_text <- action_entry
                    }
                    actions_rows[[length(actions_rows) + 1]] <<- list(test = test, action = action_text)
                }
                if (!is.null(action_key)) {
                    synthesis_rows[[length(synthesis_rows) + 1]] <<- list(
                        domain = action_key,
                        tier = cl$tier,
                        test = test
                    )
                }
                invisible(NULL)
            }

            push_note <- function(message) {
                row_idx <- as.integer(length(results_rows) + 1)
                results_rows[[row_idx]] <<- list(
                    test = tr("Note", "Nota"),
                    statistic = "\u2014",
                    pvalue = "\u2014",
                    evidence = tr("See note below", "Ver nota abajo")
                )
                note_footnotes[[length(note_footnotes) + 1]] <<- list(row = row_idx, text = message)
                invisible(NULL)
            }

            set_result_titles <- function() {

                set_title_safe <- function(name, en, es) {
                    element <- tryCatch(self$results[[name]], error = function(e) NULL)
                    if (is.null(element)) return(invisible(FALSE))
                    tryCatch(element$setTitle(tr(en, es)), error = function(e) invisible(FALSE))
                    invisible(TRUE)
                }

                titles <- list(
                    c("intro", "Time Series", "Series Temporales"),
                    c("modelProfile",
                      "Model methodological profile",
                      "Perfil metodológico del modelo"),
                    c("modelDescription", "About this model", "Sobre este modelo"),
                    c("conditionsMatrix",
                      "Assumptions and conditions to evaluate",
                      "Supuestos y condiciones a evaluar"),
                    c("decisionsMatrix",
                      "Methodological role and associated decision",
                      "Función metodológica y decisión asociada"),
                    c("runNotice", "Notice", "Aviso"),
                    c("seriesPlot", "Time series", "Serie(s) temporal(es)"),
                    c("diagnosticsResults", "Test results", "Resultados de las pruebas"),
                    c("diagnosticsActions",
                      "Recommended methodological action",
                      "Acción metodológica recomendada"),
                    c("acfPlot", "Autocorrelation (ACF)", "Autocorrelación (ACF)"),
                    c("pacfPlot", "Partial autocorrelation (PACF)", "Autocorrelación parcial (PACF)"),
                    c("residualsPlot", "Residuals over time", "Residuos en el tiempo"),
                    c("volatilityPlot", "Estimated conditional volatility", "Volatilidad condicional estimada"),
                    c("foundations",
                      "Methodological foundations and references",
                      "Fundamentos metodológicos y referencias"),
                    c("interpretation", "Methodological reading", "Lectura metodológica"),
                    c("implementationNotes", "Scope of this version", "Alcance de esta versión")
                )

                for (t in titles)
                    set_title_safe(t[1], t[2], t[3])

                invisible(TRUE)
            }

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
                    c("modelProfile", "element", "Item", "Elemento"),
                    c("modelProfile", "value", "Value", "Valor"),

                    c("conditionsMatrix", "area", "Area", "Área"),
                    c("conditionsMatrix", "condition", "Assumption / condition", "Supuesto / condición"),
                    c("conditionsMatrix", "diagnostic", "Diagnostic", "Diagnóstico"),

                    c("decisionsMatrix", "area", "Area", "Área"),
                    c("decisionsMatrix", "condition", "Assumption / condition", "Supuesto / condición"),
                    c("decisionsMatrix", "role", "Methodological role", "Función metodológica"),
                    c("decisionsMatrix", "decision", "Associated decision", "Decisión asociada"),

                    c("diagnosticsResults", "test", "Test", "Prueba"),
                    c("diagnosticsResults", "statistic", "Statistic", "Estadístico"),
                    c("diagnosticsResults", "pvalue", "p", "p"),
                    c("diagnosticsResults", "evidence", "Evidence", "Evidencia"),

                    c("diagnosticsActions", "test", "Test", "Prueba"),
                    c("diagnosticsActions", "action", "Recommended action", "Acción recomendada")
                )

                for (col in cols)
                    set_col_title_safe(col[1], col[2], col[3], col[4])

                invisible(TRUE)
            }

            set_result_titles()
            set_table_column_titles()

            # -----------------------------------------------------------------------------
            # Model profile: which series (if any) and which model are active.
            # ES: Perfil del modelo: qué serie (si la hay) y qué modelo están activos.
            # -----------------------------------------------------------------------------
            model_labels <- txt("time", "modelLabels")
            model_label <- model_labels[[model]]
            if (is.null(model_label))
                model_label <- model

            series_value <- if (is.null(series) || length(series) == 0) {
                txt("time", "noSeriesSelected")
            } else {
                paste(series, collapse = ", ")
            }

            profile_table <- self$results$modelProfile
            profile_table$deleteRows()
            profile_table$addRow(rowKey = "series", values = list(
                element = txt("time", "seriesRowLabel"),
                value = series_value
            ))
            profile_table$addRow(rowKey = "model", values = list(
                element = txt("time", "modelRowLabel"),
                value = model_label
            ))

            if (model %in% c("arima", "sarima") && !is.null(exogenous) && length(exogenous) > 0) {
                profile_table$addRow(rowKey = "exogenous", values = list(
                    element = tr("External regressors (ARIMAX/SARIMAX)", "Regresores externos (ARIMAX/SARIMAX)"),
                    value = paste(exogenous, collapse = ", ")
                ))
            }

            # -----------------------------------------------------------------------------
            # Model description: what the selected model is, when to use it, what it
            # offers and its main limitations, so this decision is not left implicit.
            # ES: Descripción del modelo: qué es el modelo seleccionado, cuándo usarlo,
            # qué ofrece y sus principales limitaciones, para que esta decisión no
            # quede implícita.
            # -----------------------------------------------------------------------------
            model_descriptions <- txt("time", "modelDescriptions")
            model_description <- model_descriptions[[model]]
            if (!is.null(model_description))
                set_html_safe("modelDescription", html_block(NULL, model_description, paragraphs = FALSE))

            # -----------------------------------------------------------------------------
            # Static assumption/condition matrix for the selected model, split into two
            # narrower tables so no row is cut off in the results panel.
            # ES: Matriz estática de supuestos/condiciones para el modelo seleccionado,
            # dividida en dos tablas más angostas para que ninguna fila quede cortada
            # en el panel de resultados.
            # -----------------------------------------------------------------------------
            matrix_data <- txt("time", "matrix")
            model_rows <- matrix_data[[model]]

            conditions_table <- self$results$conditionsMatrix
            decisions_table <- self$results$decisionsMatrix
            conditions_table$deleteRows()
            decisions_table$deleteRows()

            if (!is.null(model_rows)) {
                for (i in seq_along(model_rows)) {
                    row <- model_rows[[i]]
                    conditions_table$addRow(rowKey = i, values = list(
                        area = row[1],
                        condition = row[2],
                        diagnostic = row[3]
                    ))
                    decisions_table$addRow(rowKey = i, values = list(
                        area = row[1],
                        condition = row[2],
                        role = row[4],
                        decision = row[5]
                    ))
                }
            }

            # -----------------------------------------------------------------------------
            # Guard: decide whether the diagnostic battery can run at all (enough
            # series selected, required packages available), before touching any data.
            # ES: Guarda: decide si la batería de diagnóstico puede ejecutarse (series
            # suficientes seleccionadas, paquetes requeridos disponibles), antes de
            # tocar cualquier dato.
            # -----------------------------------------------------------------------------
            n_series <- if (is.null(series)) 0 else length(series)
            needs_multivariate <- model %in% c("var", "vecm")
            min_series_needed <- if (needs_multivariate) 2 else 1

            required_pkgs <- switch(model,
                arima = c("forecast", "tseries"),
                sarima = c("forecast", "tseries"),
                ets = c("forecast", "tseries"),
                var = c("vars", "tseries", "strucchange"),
                vecm = c("vars", "urca", "tseries"),
                garch = c("rugarch", "tseries"),
                c("forecast", "tseries")
            )

            notice <- NULL

            if (n_series == 0) {
                notice <- tr(
                    "Select at least one time series to run the diagnostics below.",
                    "Seleccione al menos una serie temporal para ejecutar los diagnósticos siguientes."
                )
            } else if (n_series < min_series_needed) {
                notice <- tr(
                    "VAR and VECM require at least two series. Add another series or choose a univariate model.",
                    "VAR y VECM requieren al menos dos series. Agregue otra serie o elija un modelo univariado."
                )
            } else {
                missing <- missing_pkgs(required_pkgs)
                if (length(missing) > 0) {
                    notice <- paste0(
                        tr("This model's diagnostics require the following R package(s), which are not installed: ",
                           "Los diagnósticos de este modelo requieren el o los siguientes paquetes de R, que no están instalados: "),
                        paste(missing, collapse = ", "),
                        tr(". Please install them from R before running this diagnostic battery.",
                           ". Instálelos desde R antes de ejecutar esta batería de diagnóstico.")
                    )
                }
            }

            if (!is.null(notice)) {
                set_html_safe("runNotice", html_block(NULL, notice, paragraphs = FALSE))
                set_visible_safe("runNotice", TRUE)
                set_visible_safe("diagnosticsResults", FALSE)
                set_visible_safe("diagnosticsActions", FALSE)
                self$results$diagnosticsResults$deleteRows()
                self$results$diagnosticsActions$deleteRows()
            } else {
                set_visible_safe("runNotice", FALSE)
                set_visible_safe("diagnosticsResults", TRUE)
                set_visible_safe("diagnosticsActions", TRUE)

                model_actions <- txt("time", "actions")[[model]]

                # -----------------------------------------------------------------------------
                # Assemble the numeric data for the selected series. Rows with any
                # missing value across the selected series are dropped (complete cases);
                # this can shift temporal spacing if missingness is not at the edges,
                # which is noted to the user when it happens.
                # ES: Ensamblar los datos numéricos de las series seleccionadas. Se
                # descartan las filas con algún valor faltante en las series
                # seleccionadas (casos completos); esto puede alterar el espaciado
                # temporal si la falta de datos no está en los bordes, lo cual se
                # advierte al usuario cuando ocurre.
                # -----------------------------------------------------------------------------
                raw_data <- self$data
                series_mat <- sapply(series, function(v) jmvcore::toNumeric(raw_data[[v]]))
                if (is.null(dim(series_mat))) series_mat <- matrix(series_mat, ncol = length(series))
                colnames(series_mat) <- series

                has_xreg <- model %in% c("arima", "sarima") && !is.null(exogenous) && length(exogenous) > 0
                if (has_xreg) {
                    xreg_mat <- sapply(exogenous, function(v) jmvcore::toNumeric(raw_data[[v]]))
                    if (is.null(dim(xreg_mat))) xreg_mat <- matrix(xreg_mat, ncol = length(exogenous))
                    colnames(xreg_mat) <- exogenous
                    combined_mat <- cbind(series_mat, xreg_mat)
                } else {
                    xreg_mat <- NULL
                    combined_mat <- series_mat
                }

                complete_rows <- stats::complete.cases(combined_mat)
                n_total <- nrow(combined_mat)
                n_complete <- sum(complete_rows)
                series_mat <- series_mat[complete_rows, , drop = FALSE]
                if (has_xreg) xreg_mat <- xreg_mat[complete_rows, , drop = FALSE]

                x_labels <- if (!is.null(date_var)) {
                    tryCatch({
                        raw_dates <- raw_data[[date_var]][complete_rows]
                        parsed <- as.Date(as.character(raw_dates))
                        if (all(is.na(parsed))) as.character(raw_dates) else parsed
                    }, error = function(e) NULL)
                } else {
                    NULL
                }
                if (is.null(x_labels) || length(x_labels) != nrow(series_mat))
                    x_labels <- seq_len(nrow(series_mat))

                if (nrow(series_mat) > 0) {
                    private$.plotSeriesData <- do.call(rbind, lapply(series, function(v) {
                        data.frame(
                            x = x_labels,
                            y = as.numeric(series_mat[, v]),
                            series = v,
                            stringsAsFactors = FALSE
                        )
                    }))
                    private$.plotIsDate <- inherits(x_labels, "Date")
                }

                if (n_complete < n_total) {
                    push_note(tr(
                        paste0(n_total - n_complete, " row(s) with missing data were excluded (", n_complete, " of ", n_total, " used)."),
                        paste0("Se excluyeron ", n_total - n_complete, " fila(s) con datos faltantes (se usaron ", n_complete, " de ", n_total, ").")
                    ))
                }

                min_obs <- if (needs_multivariate) 30 else 20

                if (n_complete < min_obs) {

                    push_note(tr(
                        paste0("Too few complete observations (", n_complete, ") to run this model's diagnostics reliably; at least ", min_obs, " are recommended."),
                        paste0("Muy pocas observaciones completas (", n_complete, ") para ejecutar con fiabilidad los diagnósticos de este modelo; se recomiendan al menos ", min_obs, ".")
                    ))

                } else {

                    diag_result <- tryCatch({
                        switch(model,
                            arima = private$.computeArima(series_mat[, 1], freq, tr, push_diag, push_note, arch_lm_test, fmt_stat, xreg_mat),
                            sarima = private$.computeSarima(series_mat[, 1], freq, tr, push_diag, push_note, arch_lm_test, fmt_stat, xreg_mat),
                            ets = private$.computeEts(series_mat[, 1], freq, tr, push_diag, push_note, arch_lm_test, fmt_stat),
                            var = private$.computeVar(series_mat, tr, push_diag, push_note, arch_lm_test, fmt_stat),
                            vecm = private$.computeVecm(series_mat, tr, push_diag, push_note, arch_lm_test, fmt_stat),
                            garch = private$.computeGarch(series_mat[, 1], tr, push_diag, push_note, arch_lm_test, fmt_stat),
                            push_note(tr("Unknown model.", "Modelo desconocido."))
                        )
                        NULL
                    }, error = function(e) {
                        conditionMessage(e)
                    })

                    if (!is.null(diag_result)) {
                        push_note(paste0(
                            tr("The diagnostic battery could not complete: ", "La batería de diagnóstico no pudo completarse: "),
                            diag_result
                        ))
                    }
                }

                for (i in seq_along(results_rows))
                    self$results$diagnosticsResults$addRow(rowKey = i, values = results_rows[[i]])

                for (fn in note_footnotes)
                    self$results$diagnosticsResults$addFootnote(col = "evidence", note = fn$text, rowNo = fn$row)

                for (i in seq_along(actions_rows))
                    self$results$diagnosticsActions$addRow(rowKey = i, values = actions_rows[[i]])
            }

            # -----------------------------------------------------------------------------
            # Plot visibility: each plot's private data field is populated only when
            # that plot is applicable and computable, so visibility is just whether
            # showPlots is on and the corresponding data field ended up non-empty.
            # ES: Visibilidad de gráficos: el campo privado de datos de cada gráfico
            # solo se llena cuando ese gráfico aplica y es calculable, así que la
            # visibilidad es simplemente si showPlots está activo y el campo de datos
            # correspondiente terminó con contenido.
            # -----------------------------------------------------------------------------
            plots_on <- isTRUE(self$options$showPlots)
            set_visible_safe("seriesPlot", plots_on && !is.null(private$.plotSeriesData) && nrow(private$.plotSeriesData) > 0)
            set_visible_safe("acfPlot", plots_on && !is.null(private$.plotAcfData))
            set_visible_safe("pacfPlot", plots_on && !is.null(private$.plotPacfData))
            set_visible_safe("residualsPlot", plots_on && !is.null(private$.plotResidualsData))
            set_visible_safe("volatilityPlot", plots_on && !is.null(private$.plotVolatilityData))

            # -----------------------------------------------------------------------------
            # Methodological foundations: for the selected model, concatenate the
            # relevant cited foundation blocks (texts.R "time.foundations") so every
            # empirical test shown above is backed by its primary methodological
            # source, following the Documento Maestro's bibliographic-integration
            # principle.
            # ES: Fundamentos metodológicos: para el modelo seleccionado, concatena
            # los bloques de fundamento citados relevantes (texts.R
            # "time.foundations") para que cada prueba empírica mostrada arriba esté
            # respaldada por su fuente metodológica primaria, siguiendo el principio
            # de integración bibliográfica del Documento Maestro.
            # -----------------------------------------------------------------------------
            foundation_keys <- switch(model,
                arima = c("unitRoot", "ljungBox", "archGarch", "jarqueBera", "autoArima"),
                sarima = c("unitRoot", "seasonalUnitRoot", "ljungBox", "archGarch", "jarqueBera", "autoArima"),
                ets = c("ljungBox", "archGarch", "jarqueBera"),
                var = c("unitRoot", "varFoundations", "parameterStability"),
                vecm = c("unitRoot", "cointegration", "varFoundations", "parameterStability"),
                garch = c("unitRoot", "archGarch", "parameterStability", "asymmetricGarch"),
                character(0)
            )

            foundations_data <- txt("time", "foundations")
            foundations_parts <- vapply(foundation_keys, function(k) {
                block <- foundations_data[[k]]
                if (is.null(block) || length(block) < 2) return("")
                html_block(block[1], block[-1], paragraphs = FALSE)
            }, character(1))
            foundations_parts <- foundations_parts[nzchar(foundations_parts)]
            set_html_safe("foundations", paste(foundations_parts, collapse = "\n"))

            # -----------------------------------------------------------------------------
            # Guide / interpretation / scope panels.
            # ES: Paneles de guía / interpretación / alcance.
            # -----------------------------------------------------------------------------
            set_html_safe("intro", html_block(NULL, txt("time", "intro"), paragraphs = FALSE))
            set_html_safe("interpretation", html_block(NULL, build_methodological_reading(model), paragraphs = FALSE))
            set_html_safe("implementationNotes", html_block(NULL, txt("time", "scopeNote"), paragraphs = FALSE))
        },

        # -----------------------------------------------------------------------------
        # ARIMA diagnostics: stationarity (ADF/PP/KPSS + ndiffs), a fitted ARIMA
        # model's residual independence (Ljung-Box), residual conditional
        # heteroscedasticity (ARCH-LM), residual normality (Jarque-Bera), AR/MA
        # root stability, and information criteria.
        # ES: Diagnósticos ARIMA: estacionariedad (ADF/PP/KPSS + ndiffs), la
        # independencia de los residuos de un modelo ARIMA ajustado (Ljung-Box), la
        # heterocedasticidad condicional residual (ARCH-LM), la normalidad residual
        # (Jarque-Bera), la estabilidad de las raíces AR/MA y los criterios de
        # información.
        # -----------------------------------------------------------------------------
        # -----------------------------------------------------------------------------
        # Shared ACF/PACF computation for the plots, used by every univariate model
        # (ARIMA, SARIMA, ETS, GARCH). Computed on the raw series independently of
        # whether the model itself was successfully fitted, since ACF/PACF are
        # useful even when a fit fails.
        # ES: Cómputo compartido de ACF/PACF para los gráficos, usado por todos los
        # modelos univariados (ARIMA, SARIMA, ETS, GARCH). Se calcula sobre la serie
        # cruda independientemente de si el modelo en sí se ajustó con éxito, ya que
        # ACF/PACF son útiles incluso cuando un ajuste falla.
        # -----------------------------------------------------------------------------
        .computeAcfPacf = function(x) {
            x <- as.numeric(stats::na.omit(x))
            n <- length(x)
            if (n < 8) return(invisible(NULL))

            max_lag <- min(n - 1, max(10, floor(10 * log10(n))))

            acf_res <- tryCatch(stats::acf(x, lag.max = max_lag, plot = FALSE), error = function(e) NULL)
            if (!is.null(acf_res) && length(acf_res$lag) > 1) {
                private$.plotAcfData <- data.frame(
                    lag = as.numeric(acf_res$lag)[-1],
                    value = as.numeric(acf_res$acf)[-1]
                )
            }

            pacf_res <- tryCatch(stats::pacf(x, lag.max = max_lag, plot = FALSE), error = function(e) NULL)
            if (!is.null(pacf_res) && length(pacf_res$lag) > 0) {
                private$.plotPacfData <- data.frame(
                    lag = as.numeric(pacf_res$lag),
                    value = as.numeric(pacf_res$acf)
                )
            }

            private$.plotN <- n
            invisible(NULL)
        },

        .computeArima = function(x, freq, tr, push_diag, push_note, arch_lm_test, fmt_stat, xreg = NULL) {

            x <- as.numeric(stats::na.omit(x))
            n <- length(x)
            private$.computeAcfPacf(x)

            adf <- tryCatch(tseries::adf.test(x), error = function(e) NULL)
            if (!is.null(adf))
                push_diag(tr("ADF (unit root)", "ADF (raíz unitaria)"),
                          adf$statistic, adf$p.value, "low_good", "stationarity")

            pp <- tryCatch(tseries::pp.test(x), error = function(e) NULL)
            if (!is.null(pp))
                push_diag(tr("Phillips-Perron (unit root)", "Phillips-Perron (raíz unitaria)"),
                          pp$statistic, pp$p.value, "low_good", "stationarity")

            kpss <- tryCatch(tseries::kpss.test(x, null = "Level"), error = function(e) NULL)
            if (!is.null(kpss))
                push_diag("KPSS", kpss$statistic, kpss$p.value, "low_bad", "stationarity")

            nd <- tryCatch(forecast::ndiffs(x), error = function(e) NA)
            push_diag(tr("Suggested differencing (ndiffs)", "Diferenciación sugerida (ndiffs)"),
                      as.character(nd), NA, evidence_override = tr("Informational", "Informativo"),
                      action_key = "ndiffs", force_action = (!is.na(nd) && nd > 0))

            has_xreg <- !is.null(xreg) && nrow(xreg) == n
            if (has_xreg)
                push_note(tr(
                    "External regressors were included: this is an ARIMAX fit, not a plain ARIMA.",
                    "Se incluyeron regresores externos: este ajuste es ARIMAX, no un ARIMA simple."
                ))

            fit <- tryCatch(
                if (has_xreg) forecast::auto.arima(x, xreg = xreg, seasonal = FALSE)
                else forecast::auto.arima(x, seasonal = FALSE),
                error = function(e) NULL
            )
            if (is.null(fit)) {
                push_note(tr("The ARIMA model could not be fitted (auto.arima).",
                              "No fue posible ajustar el modelo ARIMA (auto.arima)."))
                return(invisible(NULL))
            }

            resid <- as.numeric(stats::residuals(fit))
            private$.plotResidualsData <- data.frame(x = seq_along(resid), residual = resid)
            n_params <- length(fit$coef[names(fit$coef) != "intercept"])
            lb_lag <- min(20, max(8, floor(n / 5)))

            lb <- tryCatch(stats::Box.test(resid, lag = lb_lag, type = "Ljung-Box",
                                            fitdf = min(n_params, lb_lag - 1)),
                            error = function(e) NULL)
            if (!is.null(lb))
                push_diag(tr("Ljung-Box (residuals)", "Ljung-Box (residuos)"),
                          lb$statistic, lb$p.value, "low_bad", "ljungBox")

            arch <- arch_lm_test(resid, lags = min(12, max(4, floor(n / 10))))
            push_diag(tr("ARCH-LM (residuals)", "ARCH-LM (residuos)"),
                      arch$statistic, arch$p.value, "low_bad", "archLM")

            jb <- tryCatch(tseries::jarque.bera.test(resid), error = function(e) NULL)
            if (!is.null(jb))
                push_diag(tr("Jarque-Bera (residuals)", "Jarque-Bera (residuos)"),
                          jb$statistic, jb$p.value, "low_bad", "jarqueBera")

            roots_ok <- tryCatch({
                ar_coef <- fit$model$phi
                ma_coef <- fit$model$theta
                min_mod <- Inf
                if (!is.null(ar_coef) && length(ar_coef) > 0 && any(ar_coef != 0)) {
                    r <- polyroot(c(1, -ar_coef))
                    min_mod <- min(min_mod, Mod(r))
                }
                if (!is.null(ma_coef) && length(ma_coef) > 0 && any(ma_coef != 0)) {
                    r <- polyroot(c(1, ma_coef))
                    min_mod <- min(min_mod, Mod(r))
                }
                min_mod
            }, error = function(e) NA)

            if (!is.na(roots_ok) && is.finite(roots_ok)) {
                ok <- roots_ok > 1
                push_diag(tr("AR/MA root stability", "Estabilidad de raíces AR/MA"),
                          roots_ok, NA,
                          evidence_override = if (ok) tr("No evidence against", "Sin evidencia de incumplimiento")
                                              else tr("Clear evidence of deviation", "Evidencia clara de incumplimiento"),
                          tier_override = if (ok) "none" else "clear",
                          action_key = "roots", force_action = !ok)
            }

            push_diag(tr("AICc / BIC", "AICc / BIC"),
                      paste0("AICc=", fmt_stat(fit$aicc), ", BIC=", fmt_stat(fit$bic)),
                      NA, evidence_override = tr("Informational", "Informativo"))

            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # SARIMA diagnostics: everything ARIMA checks, plus seasonal unit-root
        # evidence (OCSB, Canova-Hansen via nsdiffs) and a seasonal-lag Ljung-Box on
        # the fitted seasonal model's residuals.
        # ES: Diagnósticos SARIMA: todo lo que revisa ARIMA, más evidencia de raíz
        # unitaria estacional (OCSB, Canova-Hansen vía nsdiffs) y un Ljung-Box a
        # rezago estacional sobre los residuos del modelo estacional ajustado.
        # -----------------------------------------------------------------------------
        .computeSarima = function(x, freq, tr, push_diag, push_note, arch_lm_test, fmt_stat, xreg = NULL) {

            x <- as.numeric(stats::na.omit(x))
            n <- length(x)
            private$.computeAcfPacf(x)

            adf <- tryCatch(tseries::adf.test(x), error = function(e) NULL)
            if (!is.null(adf))
                push_diag(tr("ADF (unit root)", "ADF (raíz unitaria)"),
                          adf$statistic, adf$p.value, "low_good", "stationarity")

            pp <- tryCatch(tseries::pp.test(x), error = function(e) NULL)
            if (!is.null(pp))
                push_diag(tr("Phillips-Perron (unit root)", "Phillips-Perron (raíz unitaria)"),
                          pp$statistic, pp$p.value, "low_good", "stationarity")

            kpss <- tryCatch(tseries::kpss.test(x, null = "Level"), error = function(e) NULL)
            if (!is.null(kpss))
                push_diag("KPSS", kpss$statistic, kpss$p.value, "low_bad", "stationarity")

            nd <- tryCatch(forecast::ndiffs(x), error = function(e) NA)
            push_diag(tr("Suggested non-seasonal differencing (ndiffs)", "Diferenciación no estacional sugerida (ndiffs)"),
                      as.character(nd), NA, evidence_override = tr("Informational", "Informativo"),
                      action_key = "ndiffs", force_action = (!is.na(nd) && nd > 0))

            xt <- stats::ts(x, frequency = freq)

            if (freq > 1) {
                nsd <- tryCatch(forecast::nsdiffs(xt, test = "ch"), error = function(e) NA)
                push_diag(tr("Suggested seasonal differencing (Canova-Hansen)", "Diferenciación estacional sugerida (Canova-Hansen)"),
                          as.character(nsd), NA, evidence_override = tr("Informational", "Informativo"),
                          action_key = "nsdiffs", force_action = (!is.na(nsd) && nsd > 0))

                nso <- tryCatch(forecast::nsdiffs(xt, test = "ocsb"), error = function(e) NA)
                push_diag(tr("Suggested seasonal differencing (OCSB)", "Diferenciación estacional sugerida (OCSB)"),
                          as.character(nso), NA, evidence_override = tr("Informational", "Informativo"),
                          action_key = "nsdiffs", force_action = (!is.na(nso) && nso > 0))
            } else {
                push_note(tr("Seasonal frequency is 1 (no seasonal cycle); seasonal diagnostics were skipped.",
                              "La frecuencia estacional es 1 (sin ciclo estacional); se omitieron los diagnósticos estacionales."))
            }

            has_xreg <- !is.null(xreg) && nrow(xreg) == n
            if (has_xreg)
                push_note(tr(
                    "External regressors were included: this is a SARIMAX fit, not a plain SARIMA.",
                    "Se incluyeron regresores externos: este ajuste es SARIMAX, no un SARIMA simple."
                ))

            fit <- tryCatch(
                if (has_xreg) forecast::auto.arima(xt, xreg = xreg, seasonal = (freq > 1))
                else forecast::auto.arima(xt, seasonal = (freq > 1)),
                error = function(e) NULL
            )
            if (is.null(fit)) {
                push_note(tr("The SARIMA model could not be fitted (auto.arima).",
                              "No fue posible ajustar el modelo SARIMA (auto.arima)."))
                return(invisible(NULL))
            }

            resid <- as.numeric(stats::residuals(fit))
            private$.plotResidualsData <- data.frame(x = seq_along(resid), residual = resid)
            n_params <- length(fit$coef)

            lb_lag <- min(20, max(8, floor(n / 5)))
            lb <- tryCatch(stats::Box.test(resid, lag = lb_lag, type = "Ljung-Box",
                                            fitdf = min(n_params, lb_lag - 1)),
                            error = function(e) NULL)
            if (!is.null(lb))
                push_diag(tr("Ljung-Box (residuals)", "Ljung-Box (residuos)"),
                          lb$statistic, lb$p.value, "low_bad", "ljungBox")

            if (freq > 1 && n > 2 * freq) {
                lb_lag_s <- min(3 * freq, n - 1)
                lb_s <- tryCatch(stats::Box.test(resid, lag = lb_lag_s, type = "Ljung-Box",
                                                  fitdf = min(n_params, lb_lag_s - 1)),
                                  error = function(e) NULL)
                if (!is.null(lb_s))
                    push_diag(tr("Ljung-Box at seasonal lags", "Ljung-Box en rezagos estacionales"),
                              lb_s$statistic, lb_s$p.value, "low_bad", "ljungBoxSeasonal")
            }

            arch <- arch_lm_test(resid, lags = min(12, max(4, floor(n / 10))))
            push_diag(tr("ARCH-LM (residuals)", "ARCH-LM (residuos)"),
                      arch$statistic, arch$p.value, "low_bad", "archLM")

            jb <- tryCatch(tseries::jarque.bera.test(resid), error = function(e) NULL)
            if (!is.null(jb))
                push_diag(tr("Jarque-Bera (residuals)", "Jarque-Bera (residuos)"),
                          jb$statistic, jb$p.value, "low_bad", "jarqueBera")

            roots_ok <- tryCatch({
                ar_coef <- fit$model$phi
                ma_coef <- fit$model$theta
                min_mod <- Inf
                if (!is.null(ar_coef) && length(ar_coef) > 0 && any(ar_coef != 0)) {
                    r <- polyroot(c(1, -ar_coef))
                    min_mod <- min(min_mod, Mod(r))
                }
                if (!is.null(ma_coef) && length(ma_coef) > 0 && any(ma_coef != 0)) {
                    r <- polyroot(c(1, ma_coef))
                    min_mod <- min(min_mod, Mod(r))
                }
                min_mod
            }, error = function(e) NA)

            if (!is.na(roots_ok) && is.finite(roots_ok)) {
                ok <- roots_ok > 1
                push_diag(tr("AR/MA root stability", "Estabilidad de raíces AR/MA"),
                          roots_ok, NA,
                          evidence_override = if (ok) tr("No evidence against", "Sin evidencia de incumplimiento")
                                              else tr("Clear evidence of deviation", "Evidencia clara de incumplimiento"),
                          tier_override = if (ok) "none" else "clear",
                          action_key = "roots", force_action = !ok)
            }

            push_diag(tr("AICc / BIC", "AICc / BIC"),
                      paste0("AICc=", fmt_stat(fit$aicc), ", BIC=", fmt_stat(fit$bic)),
                      NA, evidence_override = tr("Informational", "Informativo"))

            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # ETS diagnostics: residual independence (Ljung-Box), residual conditional
        # heteroscedasticity (ARCH-LM), residual normality (Jarque-Bera), and
        # information criteria. No unit-root testing: ETS models level, trend, and
        # seasonality directly rather than assuming a fixed differencing order.
        # ES: Diagnósticos ETS: independencia de los residuos (Ljung-Box),
        # heterocedasticidad condicional residual (ARCH-LM), normalidad residual
        # (Jarque-Bera) y criterios de información. Sin prueba de raíz unitaria: ETS
        # modela nivel, tendencia y estacionalidad directamente en vez de asumir un
        # orden de diferenciación fijo.
        # -----------------------------------------------------------------------------
        .computeEts = function(x, freq, tr, push_diag, push_note, arch_lm_test, fmt_stat) {

            x <- as.numeric(stats::na.omit(x))
            n <- length(x)
            private$.computeAcfPacf(x)
            xt <- stats::ts(x, frequency = freq)

            fit <- tryCatch(forecast::ets(xt), error = function(e) NULL)
            if (is.null(fit)) {
                push_note(tr("The ETS model could not be fitted.", "No fue posible ajustar el modelo ETS."))
                return(invisible(NULL))
            }

            resid <- as.numeric(stats::residuals(fit))
            private$.plotResidualsData <- data.frame(x = seq_along(resid), residual = resid)
            lb_lag <- min(20, max(8, floor(n / 5)))

            lb <- tryCatch(stats::Box.test(resid, lag = lb_lag, type = "Ljung-Box"),
                            error = function(e) NULL)
            if (!is.null(lb))
                push_diag(tr("Ljung-Box (residuals)", "Ljung-Box (residuos)"),
                          lb$statistic, lb$p.value, "low_bad", "ljungBox")

            arch <- arch_lm_test(resid, lags = min(12, max(4, floor(n / 10))))
            push_diag(tr("ARCH-LM (residuals)", "ARCH-LM (residuos)"),
                      arch$statistic, arch$p.value, "low_bad", "archLM")

            jb <- tryCatch(tseries::jarque.bera.test(resid), error = function(e) NULL)
            if (!is.null(jb))
                push_diag(tr("Jarque-Bera (residuals)", "Jarque-Bera (residuos)"),
                          jb$statistic, jb$p.value, "low_bad", "jarqueBera")

            push_diag(tr("AICc / BIC", "AICc / BIC"),
                      paste0("AICc=", fmt_stat(fit$aicc), ", BIC=", fmt_stat(fit$bic)),
                      NA, evidence_override = tr("Informational", "Informativo"))

            push_diag(tr("Selected ETS specification", "Especificación ETS seleccionada"),
                      fit$method, NA, evidence_override = tr("Informational", "Informativo"))

            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # Shared per-variable stationarity loop (ADF/PP/KPSS) used by VAR and VECM.
        # ES: Bucle compartido de estacionariedad por variable (ADF/PP/KPSS) usado
        # por VAR y VECM.
        # -----------------------------------------------------------------------------
        .stationarityPerVariable = function(mat, tr, push_diag) {
            for (v in colnames(mat)) {
                xv <- as.numeric(mat[, v])
                adf <- tryCatch(tseries::adf.test(xv), error = function(e) NULL)
                if (!is.null(adf))
                    push_diag(paste0("ADF (", v, ")"), adf$statistic, adf$p.value, "low_good", "stationarity")
                pp <- tryCatch(tseries::pp.test(xv), error = function(e) NULL)
                if (!is.null(pp))
                    push_diag(paste0("PP (", v, ")"), pp$statistic, pp$p.value, "low_good", "stationarity")
                kpss <- tryCatch(tseries::kpss.test(xv, null = "Level"), error = function(e) NULL)
                if (!is.null(kpss))
                    push_diag(paste0("KPSS (", v, ")"), kpss$statistic, kpss$p.value, "low_bad", "stationarity")
            }
            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # VAR diagnostics: per-variable stationarity, lag-order suggestion
        # (VARselect), system stability (companion-matrix roots), residual serial
        # correlation (multivariate Portmanteau), residual conditional
        # heteroscedasticity (multivariate ARCH-LM), residual multivariate
        # normality, and per-equation parameter stability (OLS-CUSUM).
        # ES: Diagnósticos VAR: estacionariedad por variable, sugerencia de orden de
        # rezago (VARselect), estabilidad del sistema (raíces de la matriz
        # compañera), correlación serial residual (Portmanteau multivariante),
        # heterocedasticidad condicional residual (ARCH-LM multivariante),
        # normalidad multivariante residual, y estabilidad paramétrica por ecuación
        # (OLS-CUSUM).
        # -----------------------------------------------------------------------------
        .computeVar = function(mat, tr, push_diag, push_note, arch_lm_test, fmt_stat) {

            if (!requireNamespace("vars", quietly = TRUE)) {
                push_note(tr("The 'vars' package is not installed; VAR diagnostics cannot be computed.",
                              "El paquete 'vars' no está instalado; no se pueden calcular los diagnósticos VAR."))
                return(invisible(NULL))
            }

            private$.stationarityPerVariable(mat, tr, push_diag)

            n <- nrow(mat)
            k <- ncol(mat)
            lag_max <- max(1, min(10, floor(n / (k * 3)) - 1))

            sel <- tryCatch(vars::VARselect(mat, lag.max = max(1, lag_max), type = "const"),
                             error = function(e) NULL)
            p_lag <- 1
            if (!is.null(sel)) {
                p_lag <- unname(sel$selection["AIC(n)"])
                if (is.na(p_lag) || p_lag < 1) p_lag <- 1
                push_diag(tr("Suggested lag order (VARselect, AIC)", "Orden de rezago sugerido (VARselect, AIC)"),
                          as.character(p_lag), NA, evidence_override = tr("Informational", "Informativo"))
            }

            fit <- tryCatch(vars::VAR(mat, p = p_lag, type = "const"), error = function(e) NULL)
            if (is.null(fit)) {
                push_note(tr("The VAR model could not be fitted.", "No fue posible ajustar el modelo VAR."))
                return(invisible(NULL))
            }

            rts <- tryCatch(vars::roots(fit, modulus = TRUE), error = function(e) NULL)
            if (!is.null(rts) && length(rts) > 0 && !all(is.na(rts))) {
                max_mod <- max(rts, na.rm = TRUE)
                ok <- isTRUE(max_mod < 1)
                push_diag(tr("VAR stability (companion-matrix roots)", "Estabilidad VAR (raíces de la matriz compañera)"),
                          max_mod, NA,
                          evidence_override = if (ok) tr("No evidence against", "Sin evidencia de incumplimiento")
                                              else tr("Clear evidence of deviation", "Evidencia clara de incumplimiento"),
                          tier_override = if (ok) "none" else "clear",
                          action_key = "stability", force_action = !ok)
            }

            pt_lags <- max(p_lag + 4, min(16, floor(n / 5)))
            serial <- tryCatch(vars::serial.test(fit, lags.pt = pt_lags, type = "PT.asymptotic"),
                                error = function(e) NULL)
            if (!is.null(serial) && !is.null(serial$serial))
                push_diag(tr("Portmanteau (multivariate residual autocorrelation)",
                              "Portmanteau (autocorrelación residual multivariante)"),
                          serial$serial$statistic, serial$serial$p.value, "low_bad", "serial")

            arch_mv <- tryCatch(vars::arch.test(fit, lags.multi = min(5, p_lag + 2), multivariate.only = TRUE),
                                 error = function(e) NULL)
            if (!is.null(arch_mv) && !is.null(arch_mv$arch.mul))
                push_diag(tr("ARCH-LM (multivariate)", "ARCH-LM (multivariante)"),
                          arch_mv$arch.mul$statistic, arch_mv$arch.mul$p.value, "low_bad", "archLM")

            norm_mv <- tryCatch(vars::normality.test(fit, multivariate.only = TRUE), error = function(e) NULL)
            if (!is.null(norm_mv) && !is.null(norm_mv$jb.mul) && !is.null(norm_mv$jb.mul$JB))
                push_diag(tr("Jarque-Bera (multivariate normality)", "Jarque-Bera (normalidad multivariante)"),
                          norm_mv$jb.mul$JB$statistic, norm_mv$jb.mul$JB$p.value, "low_bad", "jarqueBera")

            if (requireNamespace("strucchange", quietly = TRUE)) {
                cusum_p <- tryCatch({
                    ps <- vapply(fit$varresult, function(eq) {
                        efp_fit <- strucchange::efp(formula(eq), data = eq$model, type = "OLS-CUSUM")
                        sc <- strucchange::sctest(efp_fit)
                        sc$p.value
                    }, numeric(1))
                    ps
                }, error = function(e) NULL)
                if (!is.null(cusum_p) && length(cusum_p) > 0 && !all(is.na(cusum_p))) {
                    worst_p <- min(cusum_p, na.rm = TRUE)
                    push_diag(tr("OLS-CUSUM (parameter stability, worst equation)",
                                  "OLS-CUSUM (estabilidad paramétrica, peor ecuación)"),
                              NA, worst_p, "low_bad", "cusum")
                }
            } else {
                push_note(tr("The 'strucchange' package is not installed; the OLS-CUSUM stability test was skipped.",
                              "El paquete 'strucchange' no está instalado; se omitió la prueba de estabilidad OLS-CUSUM."))
            }

            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # VECM diagnostics: per-variable stationarity, lag-order suggestion, Johansen
        # trace and maximum-eigenvalue cointegration tests (rank suggested against
        # the 5% critical values), and — once converted to its equivalent VAR
        # representation via vec2var — the same residual serial-correlation,
        # ARCH-LM, and normality diagnostics used for VAR.
        # ES: Diagnósticos VECM: estacionariedad por variable, sugerencia de orden
        # de rezago, pruebas de cointegración de Johansen (traza y máximo
        # autovalor; rango sugerido contra los valores críticos al 5%) y, una vez
        # convertido a su representación VAR equivalente vía vec2var, los mismos
        # diagnósticos de correlación serial residual, ARCH-LM y normalidad usados
        # para VAR.
        # -----------------------------------------------------------------------------
        .computeVecm = function(mat, tr, push_diag, push_note, arch_lm_test, fmt_stat) {

            if (!requireNamespace("vars", quietly = TRUE) || !requireNamespace("urca", quietly = TRUE)) {
                push_note(tr("The 'vars' and/or 'urca' packages are not installed; VECM diagnostics cannot be computed.",
                              "Los paquetes 'vars' y/o 'urca' no están instalados; no se pueden calcular los diagnósticos VECM."))
                return(invisible(NULL))
            }

            private$.stationarityPerVariable(mat, tr, push_diag)

            n <- nrow(mat)
            k <- ncol(mat)
            lag_max <- max(2, min(10, floor(n / (k * 3)) - 1))

            sel <- tryCatch(vars::VARselect(mat, lag.max = max(2, lag_max), type = "const"),
                             error = function(e) NULL)
            k_lag <- 2
            if (!is.null(sel)) {
                k_lag <- unname(sel$selection["AIC(n)"])
                if (is.na(k_lag) || k_lag < 2) k_lag <- 2
                push_diag(tr("Suggested lag order (VARselect, AIC)", "Orden de rezago sugerido (VARselect, AIC)"),
                          as.character(k_lag), NA, evidence_override = tr("Informational", "Informativo"))
            }

            jo_trace <- tryCatch(urca::ca.jo(mat, type = "trace", ecdet = "const", K = k_lag),
                                  error = function(e) NULL)
            jo_eigen <- tryCatch(urca::ca.jo(mat, type = "eigen", ecdet = "const", K = k_lag),
                                  error = function(e) NULL)

            determine_rank <- function(jo) {
                teststat <- jo@teststat
                cval <- jo@cval[, "5pct"]
                m <- length(teststat)
                rank <- 0
                for (i in m:1) {
                    if (teststat[i] > cval[i]) rank <- rank + 1
                    else break
                }
                rank
            }

            rank <- 1
            if (!is.null(jo_trace)) {
                rank_trace <- tryCatch(determine_rank(jo_trace), error = function(e) NA)
                if (!is.na(rank_trace)) {
                    push_diag(tr("Johansen trace test (suggested rank)", "Prueba de traza de Johansen (rango sugerido)"),
                              as.character(rank_trace), NA,
                              evidence_override = tr("Informational", "Informativo"),
                              action_key = "johansen", force_action = TRUE)
                    rank <- rank_trace
                }
            }
            if (!is.null(jo_eigen)) {
                rank_eigen <- tryCatch(determine_rank(jo_eigen), error = function(e) NA)
                if (!is.na(rank_eigen))
                    push_diag(tr("Johansen max-eigenvalue test (suggested rank)", "Prueba de máximo autovalor de Johansen (rango sugerido)"),
                              as.character(rank_eigen), NA,
                              evidence_override = tr("Informational", "Informativo"),
                              action_key = "johansen", force_action = TRUE)
            }

            if (is.null(jo_trace)) {
                push_note(tr("The Johansen test could not be computed.", "No fue posible calcular la prueba de Johansen."))
                return(invisible(NULL))
            }

            rank <- max(1, min(rank, k - 1))

            v2v <- tryCatch(vars::vec2var(jo_trace, r = rank), error = function(e) NULL)
            if (is.null(v2v)) {
                push_note(tr("The cointegrating VECM could not be converted to its VAR representation (vec2var).",
                              "No fue posible convertir el VECM cointegrado a su representación VAR (vec2var)."))
                return(invisible(NULL))
            }

            rts <- tryCatch(vars::roots(v2v, modulus = TRUE), error = function(e) NULL)
            if (!is.null(rts) && length(rts) > 0 && !all(is.na(rts))) {
                max_mod <- max(rts, na.rm = TRUE)
                ok <- isTRUE(max_mod < 1)
                push_diag(tr("System stability (companion-matrix roots)", "Estabilidad del sistema (raíces de la matriz compañera)"),
                          max_mod, NA,
                          evidence_override = if (ok) tr("No evidence against", "Sin evidencia de incumplimiento")
                                              else tr("Clear evidence of deviation", "Evidencia clara de incumplimiento"),
                          tier_override = if (ok) "none" else "clear",
                          action_key = "stability", force_action = !ok)
            }

            pt_lags <- max(k_lag + 4, min(16, floor(n / 5)))
            serial <- tryCatch(vars::serial.test(v2v, lags.pt = pt_lags, type = "PT.asymptotic"),
                                error = function(e) NULL)
            if (!is.null(serial) && !is.null(serial$serial))
                push_diag(tr("Portmanteau (multivariate residual autocorrelation)",
                              "Portmanteau (autocorrelación residual multivariante)"),
                          serial$serial$statistic, serial$serial$p.value, "low_bad", "serial")

            arch_mv <- tryCatch(vars::arch.test(v2v, lags.multi = min(5, k_lag + 2), multivariate.only = TRUE),
                                 error = function(e) NULL)
            if (!is.null(arch_mv) && !is.null(arch_mv$arch.mul))
                push_diag(tr("ARCH-LM (multivariate)", "ARCH-LM (multivariante)"),
                          arch_mv$arch.mul$statistic, arch_mv$arch.mul$p.value, "low_bad", "archLM")

            norm_mv <- tryCatch(vars::normality.test(v2v, multivariate.only = TRUE), error = function(e) NULL)
            if (!is.null(norm_mv) && !is.null(norm_mv$jb.mul) && !is.null(norm_mv$jb.mul$JB))
                push_diag(tr("Jarque-Bera (multivariate normality)", "Jarque-Bera (normalidad multivariante)"),
                          norm_mv$jb.mul$JB$statistic, norm_mv$jb.mul$JB$p.value, "low_bad", "jarqueBera")

            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # GARCH diagnostics: stationarity of the provided series (assumed to already
        # be a returns-type series, not raw price levels), a pre-fit ARCH-LM check
        # that justifies modeling conditional heteroscedasticity, then — once a
        # GARCH(1,1) is fitted — standardized-residual serial correlation, residual
        # ARCH-LM, variance persistence, sign-bias (asymmetry) testing, and Nyblom
        # parameter-stability testing.
        # ES: Diagnósticos GARCH: estacionariedad de la serie provista (se asume que
        # ya es una serie de retornos, no niveles de precio), una prueba ARCH-LM
        # previa al ajuste que justifica modelar heterocedasticidad condicional, y —
        # una vez ajustado un GARCH(1,1) — correlación serial de los residuos
        # estandarizados, ARCH-LM residual, persistencia de la varianza, prueba de
        # sesgo de signo (asimetría) y prueba de estabilidad de Nyblom.
        # -----------------------------------------------------------------------------
        .computeGarch = function(x, tr, push_diag, push_note, arch_lm_test, fmt_stat) {

            x <- as.numeric(stats::na.omit(x))
            n <- length(x)
            private$.computeAcfPacf(x)

            push_note(tr(
                "GARCH assumes the selected series already represents returns (or another approximately mean-stationary series), not raw price levels.",
                "GARCH asume que la serie seleccionada ya representa retornos (u otra serie aproximadamente estacionaria en media), no niveles de precio en bruto."
            ))

            adf <- tryCatch(tseries::adf.test(x), error = function(e) NULL)
            if (!is.null(adf))
                push_diag(tr("ADF (unit root, on the provided series)", "ADF (raíz unitaria, sobre la serie provista)"),
                          adf$statistic, adf$p.value, "low_good", "stationarity")

            pp <- tryCatch(tseries::pp.test(x), error = function(e) NULL)
            if (!is.null(pp))
                push_diag(tr("Phillips-Perron (unit root)", "Phillips-Perron (raíz unitaria)"),
                          pp$statistic, pp$p.value, "low_good", "stationarity")

            kpss <- tryCatch(tseries::kpss.test(x, null = "Level"), error = function(e) NULL)
            if (!is.null(kpss))
                push_diag("KPSS", kpss$statistic, kpss$p.value, "low_bad", "stationarity")

            pre <- arch_lm_test(x - mean(x), lags = min(12, max(4, floor(n / 10))))
            pre_cl_ok <- !is.na(pre$p.value) && pre$p.value < 0.05
            push_diag(tr("ARCH-LM (pre-fit, on the series)", "ARCH-LM (previo al ajuste, sobre la serie)"),
                      pre$statistic, pre$p.value,
                      evidence_override = if (pre_cl_ok)
                          tr("ARCH effects detected", "Efectos ARCH detectados")
                          else
                          tr("No clear ARCH effects", "Sin efectos ARCH claros"),
                      footnote = if (pre_cl_ok)
                          tr("ARCH effects detected \u2014 modeling conditional heteroscedasticity is justified.",
                             "Se detectan efectos ARCH \u2014 modelar heterocedasticidad condicional está justificado.")
                          else
                          tr("No clear ARCH effects detected in the raw series.",
                             "No se detectan efectos ARCH claros en la serie en bruto."))

            if (!requireNamespace("rugarch", quietly = TRUE)) {
                push_note(tr("The 'rugarch' package is not installed; the GARCH fit and its post-fit diagnostics were skipped.",
                              "El paquete 'rugarch' no está instalado; se omitieron el ajuste GARCH y sus diagnósticos posteriores."))
                return(invisible(NULL))
            }

            spec <- tryCatch(rugarch::ugarchspec(
                variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
            ), error = function(e) NULL)

            fit <- if (!is.null(spec))
                tryCatch(rugarch::ugarchfit(spec = spec, data = x, solver = "hybrid"), error = function(e) NULL)
            else NULL

            converged <- !is.null(fit) && tryCatch(isTRUE(fit@fit$convergence == 0), error = function(e) FALSE)
            if (!converged) {
                push_note(tr("The GARCH(1,1) model did not converge; post-fit diagnostics were skipped.",
                              "El modelo GARCH(1,1) no convergió; se omitieron los diagnósticos posteriores al ajuste."))
                return(invisible(NULL))
            }

            std_resid <- tryCatch(as.numeric(rugarch::residuals(fit, standardize = TRUE)),
                                   error = function(e) NULL)

            if (!is.null(std_resid)) {
                private$.plotResidualsData <- data.frame(x = seq_along(std_resid), residual = std_resid)

                lb_lag <- min(20, max(8, floor(n / 5)))
                lb <- tryCatch(stats::Box.test(std_resid, lag = lb_lag, type = "Ljung-Box"),
                                error = function(e) NULL)
                if (!is.null(lb))
                    push_diag(tr("Ljung-Box (standardized residuals)", "Ljung-Box (residuos estandarizados)"),
                              lb$statistic, lb$p.value, "low_bad", "ljungBox")

                arch_post <- arch_lm_test(std_resid, lags = min(12, max(4, floor(n / 10))))
                push_diag(tr("ARCH-LM (standardized residuals)", "ARCH-LM (residuos estandarizados)"),
                          arch_post$statistic, arch_post$p.value, "low_bad", "archLM")
            }

            sigma_vals <- tryCatch(as.numeric(rugarch::sigma(fit)), error = function(e) NULL)
            if (!is.null(sigma_vals) && length(sigma_vals) > 0)
                private$.plotVolatilityData <- data.frame(x = seq_along(sigma_vals), sigma = sigma_vals)

            tryCatch({
                coefs <- rugarch::coef(fit)
                a1 <- unname(coefs["alpha1"])
                b1 <- unname(coefs["beta1"])
                if (!is.na(a1) && !is.na(b1)) {
                    persistence <- a1 + b1
                    near_unit <- isTRUE(persistence >= 0.98)
                    push_diag(tr("Variance persistence (alpha1 + beta1)", "Persistencia de la varianza (alpha1 + beta1)"),
                              persistence, NA,
                              evidence_override = if (near_unit)
                                  tr("Very high (near-integrated variance)", "Muy alta (varianza casi integrada)")
                                  else tr("Within a typical range", "Dentro de un rango típico"),
                              tier_override = if (near_unit) "clear" else "none",
                              action_key = "persistence", force_action = near_unit)
                }
            }, error = function(e) NULL)

            tryCatch({
                sb <- rugarch::signbias(fit)
                if (!is.null(sb) && isTRUE(nrow(sb) >= 1)) {
                    joint_row <- as.data.frame(sb)[nrow(sb), , drop = FALSE]
                    p_col <- which(grepl("prob", colnames(joint_row), ignore.case = TRUE))[1]
                    stat_col <- which(grepl("t.value|t-value|statistic", colnames(joint_row), ignore.case = TRUE))[1]
                    if (!is.na(p_col)) {
                        p_val <- as.numeric(joint_row[1, p_col])
                        stat_val <- if (!is.na(stat_col)) as.numeric(joint_row[1, stat_col]) else NA
                        if (!is.na(p_val))
                            push_diag(tr("Sign bias (joint effect)", "Sesgo de signo (efecto conjunto)"),
                                      stat_val, p_val, "low_bad", "signBias")
                    }
                }
            }, error = function(e) NULL)

            tryCatch({
                nyb <- rugarch::nyblom(fit)
                if (!is.null(nyb) && !is.null(nyb$JointCritical) && !is.null(nyb$Joint)) {
                    jc <- nyb$JointCritical
                    crit5 <- suppressWarnings(jc["5%"])
                    if (is.na(crit5)) crit5 <- suppressWarnings(jc["5pct"])
                    if (is.na(crit5) && length(jc) >= 2) crit5 <- as.numeric(jc[2])
                    if (is.na(crit5) && length(jc) >= 1) crit5 <- as.numeric(jc[1])
                    crit5 <- as.numeric(crit5)
                    if (!is.na(crit5)) {
                        ok <- isTRUE(as.numeric(nyb$Joint) < crit5)
                        push_diag(tr("Nyblom (joint parameter stability)", "Nyblom (estabilidad paramétrica conjunta)"),
                                  as.numeric(nyb$Joint), NA,
                                  evidence_override = if (ok) tr("No evidence against", "Sin evidencia de incumplimiento")
                                                      else tr("Clear evidence of deviation", "Evidencia clara de incumplimiento"),
                                  tier_override = if (ok) "none" else "clear",
                                  action_key = "nyblom", force_action = !ok)
                    }
                }
            }, error = function(e) NULL)

            tryCatch({
                gof_res <- rugarch::gof(fit, groups = c(20, 30, 40, 50))
                gof_mat <- gof_res@gof
                if (!is.null(gof_mat) && isTRUE(nrow(gof_mat) >= 1)) {
                    p_col <- which(grepl("p-value|p.value", colnames(gof_mat), ignore.case = TRUE))[1]
                    if (!is.na(p_col)) {
                        p_val <- as.numeric(gof_mat[1, p_col])
                        if (!is.na(p_val))
                            push_diag(tr("Goodness of fit (20 groups)", "Bondad de ajuste (20 grupos)"),
                                      NA, p_val, "low_bad", "gof")
                    }
                }
            }, error = function(e) NULL)

            invisible(NULL)
        },

        # -----------------------------------------------------------------------------
        # Shared plotting helpers (style, palette, guard, bilingual labels), matching
        # the exact pattern used across the rest of AssumptionsLab (see regCheck).
        # ES: Auxiliares de graficación compartidos (estilo, paleta, guarda,
        # etiquetas bilingües), calcados del patrón usado en el resto de
        # AssumptionsLab (ver regCheck).
        # -----------------------------------------------------------------------------
        .plotTr = function(en, es) {
            lang <- .al_normalize_lang(self$options$reportLang)
            if (identical(lang, "es")) es else en
        },

        .plotPalette = function() {
            # Base palette + series palette: identical shape and logic in
            # regCheck, logCheck, and timeCheck, consolidated in
            # shared-helpers.R (.al_plot_palette_base /
            # .al_plot_series_palette). fullColor already matched Variant A
            # here (Archie's suite-wide standard, Aug 2026) - no visible
            # change for timeCheck. The shared base always includes a
            # "smooth" key (regCheck's original shape); timeCheck's own
            # base never had one and never reads it, so this is an inert
            # addition.
            # ES: paleta base + paleta de series idénticas en regCheck,
            # logCheck y timeCheck, consolidadas en shared-helpers.R.
            # fullColor ya coincidía con la Variante A acá (estándar de
            # Archie, agosto 2026) - sin cambio visible para timeCheck. La
            # base compartida siempre incluye una clave "smooth" (forma
            # original de regCheck); la base propia de timeCheck nunca
            # tuvo una y nunca la lee, así que es un agregado inerte.
            style <- tryCatch(self$options$plotStyle, error = function(e) "clean")
            if (is.null(style) || length(style) == 0 || !nzchar(style)) style <- "clean"

            base <- .al_plot_palette_base(style)

            palette_choice <- tryCatch(self$options$plotPalette, error = function(e) "blueOrange")
            if (is.null(palette_choice) || length(palette_choice) == 0 || !nzchar(palette_choice))
                palette_choice <- "blueOrange"

            base$series <- .al_plot_series_palette(palette_choice)

            base
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

        .requirePlot = function(image, data_field) {
            if (!isTRUE(self$options$showPlots))
                return(FALSE)

            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                image$setError("The ggplot2 package is required to draw diagnostic plots.")
                return(FALSE)
            }

            d <- private[[data_field]]
            if (is.null(d) || (is.data.frame(d) && nrow(d) == 0)) {
                image$setError(private$.plotTr(
                    "No plot data are available.",
                    "No hay datos disponibles para este gráfico."
                ))
                return(FALSE)
            }

            TRUE
        },

        .plotSeries = function(image, ...) {
            if (!private$.requirePlot(image, ".plotSeriesData")) return()

            d <- private$.plotSeriesData
            n_series <- length(unique(d$series))
            pal <- private$.plotPalette()

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))

            if (n_series > 1) {
                plot <- plot +
                    ggplot2::geom_line(ggplot2::aes(color = series), linewidth = 0.7) +
                    ggplot2::scale_color_manual(values = pal$series, name = NULL)
            } else {
                plot <- plot +
                    ggplot2::geom_line(linewidth = 0.7, color = pal$line)
            }

            plot <- plot +
                ggplot2::labs(
                    x = if (isTRUE(private$.plotIsDate))
                        private$.plotTr("Date", "Fecha")
                    else
                        private$.plotTr("Observation", "Observación"),
                    y = private$.plotTr("Value", "Valor")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotAcf = function(image, ...) {
            if (!private$.requirePlot(image, ".plotAcfData")) return()

            d <- private$.plotAcfData
            n <- private$.plotN
            bound <- if (!is.null(n) && n > 0) 1.96 / sqrt(n) else NA
            pal <- private$.plotPalette()

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = lag, y = value)) +
                ggplot2::geom_hline(yintercept = 0, color = pal$ref)

            if (!is.na(bound)) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = "dashed",
                                         color = pal$alert)
            }

            plot <- plot +
                ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0),
                                       color = pal$line, linewidth = 0.6) +
                ggplot2::labs(
                    x = private$.plotTr("Lag", "Rezago"),
                    y = "ACF"
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotPacf = function(image, ...) {
            if (!private$.requirePlot(image, ".plotPacfData")) return()

            d <- private$.plotPacfData
            n <- private$.plotN
            bound <- if (!is.null(n) && n > 0) 1.96 / sqrt(n) else NA
            pal <- private$.plotPalette()

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = lag, y = value)) +
                ggplot2::geom_hline(yintercept = 0, color = pal$ref)

            if (!is.na(bound)) {
                plot <- plot +
                    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = "dashed",
                                         color = pal$alert)
            }

            plot <- plot +
                ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0),
                                       color = pal$line, linewidth = 0.6) +
                ggplot2::labs(
                    x = private$.plotTr("Lag", "Rezago"),
                    y = "PACF"
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotResiduals = function(image, ...) {
            if (!private$.requirePlot(image, ".plotResidualsData")) return()

            d <- private$.plotResidualsData
            pal <- private$.plotPalette()

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = residual)) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = pal$ref) +
                ggplot2::geom_line(color = pal$line, linewidth = 0.5, alpha = 0.8) +
                ggplot2::geom_point(color = pal$point, size = 1.1, alpha = 0.7) +
                ggplot2::labs(
                    x = private$.plotTr("Observation", "Observación"),
                    y = private$.plotTr("Residual", "Residuo")
                ) +
                private$.plotTheme()

            print(plot)
        },

        .plotVolatility = function(image, ...) {
            if (!private$.requirePlot(image, ".plotVolatilityData")) return()

            d <- private$.plotVolatilityData
            pal <- private$.plotPalette()

            plot <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = sigma)) +
                ggplot2::geom_line(color = pal$line, linewidth = 0.7) +
                ggplot2::labs(
                    x = private$.plotTr("Observation", "Observación"),
                    y = private$.plotTr("Conditional standard deviation", "Desviación estándar condicional")
                ) +
                private$.plotTheme()

            print(plot)
        }
    )
)
