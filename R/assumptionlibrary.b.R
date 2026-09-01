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
# Assumption Library.
# ES: Biblioteca de Supuestos.
#
# This file implements the Assumption Library: a reference glossary explaining
# what each statistical assumption and test means and when it is used across
# AssumptionsLab. It does not analyze the user's data; each analysis module
# interprets its own results separately, using the user's actual data.
#
# ES: Este archivo implementa la Biblioteca de Supuestos: un glosario de
# referencia que explica qué significa cada supuesto y prueba estadística, y
# cuándo se usa, a lo largo de AssumptionsLab. No analiza los datos del
# usuario; cada módulo de análisis interpreta sus propios resultados por
# separado, con los datos reales del usuario.
#
# Responsibilities
# 1. Hold the conceptual explanation of every assumption/test category
#    covered by the suite (normality, homoscedasticity, independence, etc.),
#    always at full guide depth, in every active report language.
# 2. Filter which category is shown, based on the user's selection.
#
# ES: Responsabilidades
# 1. Mantener la explicación conceptual de cada categoría de supuesto/prueba
#    cubierta por la suite (normalidad, homoscedasticidad, independencia,
#    etc.), siempre con la profundidad de la guía completa, en cada idioma
#    de informe activo.
# 2. Filtrar qué categoría se muestra, según la selección del usuario.
#
# Workflow
# 1. Read the selected category and report language.
# 2. Resolve every section's text via the shared .al_text() repository
#    (texts.R, section "library") and wire bilingual result titles.
# 3. Assemble the explanatory content for each assumption/test category.
# 4. Hide any section not matching the selected category.
#
# ES: Flujo de trabajo
# 1. Leer la categoría seleccionada y el idioma del informe.
# 2. Resolver el texto de cada sección vía el repositorio compartido
#    .al_text() (texts.R, sección "library") y cablear los títulos de
#    resultado bilingües.
# 3. Ensamblar el contenido explicativo de cada categoría de supuesto/prueba.
# 4. Ocultar cualquier sección que no coincida con la categoría
#    seleccionada.
# -----------------------------------------------------------------------------

assumptionLibraryClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "assumptionLibraryClass",
    inherit = assumptionLibraryBase,
    private = list(
        .run = function() {

            category <- self$options$category

            # -----------------------------------------------------------------------------
            # Bilingual wiring (AssumptionsLab standard — see regCheck/logCheck).
            # ES: Cableado bilingüe (estándar AssumptionsLab — ver regCheck/logCheck).
            # -----------------------------------------------------------------------------
            lang <- .al_normalize_lang(self$options$reportLang)

            # Delegates to the shared .al_tr() (shared-helpers.R) instead of
            # reimplementing the same en/es selection logic locally.
            # ES: Delega en la función compartida .al_tr() (shared-
            # helpers.R) en vez de reimplementar localmente la misma
            # lógica de selección en/es.
            tr <- function(en, es = NULL) .al_tr(lang, en, es)

            txt <- function(key) {
                .al_text(lang, "library", key)
            }

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
                    c("intro", "Assumption Library", "Biblioteca de Supuestos"),
                    c("normality", "Normality", "Normalidad"),
                    c("homoscedasticity",
                      "Homoscedasticity / Homogeneity of Variance",
                      "Homocedasticidad / Homogeneidad de varianzas"),
                    c("linearity", "Linearity", "Linealidad"),
                    c("independence", "Independence", "Independencia"),
                    c("multicollinearity", "Multicollinearity", "Multicolinealidad"),
                    c("influence",
                      "Outliers and Influential Cases",
                      "Casos atípicos e influencia"),
                    c("sphericity", "Sphericity", "Esfericidad"),
                    c("robust",
                      "Transformations and Robust Alternatives",
                      "Transformaciones y alternativas robustas"),
                    c("notes", "Recommended Use of Assumption Library", "Uso de esta biblioteca")
                )

                for (t in titles)
                    set_title_safe(t[1], t[2], t[3])

                invisible(TRUE)
            }

            set_result_titles()

            show_section <- function(name) {
                category == "all" || category == name
            }

            hide_if_needed <- function(result, name) {
                if (!show_section(name))
                    result$setVisible(FALSE)
            }

            # -----------------------------------------------------------------------------
            # Escape HTML-unsafe characters.
            # ES: Escapar caracteres inseguros para HTML.
            #
            # Required before inserting any raw text into an HTML result, since much of
            # this library's text contains literal "<"/">" (e.g. "p < .05", "VIF < 5",
            # "|DFFITS| > 2*sqrt(p/n)") that would otherwise be mistaken for HTML tags by
            # the renderer and silently break the whole panel.
            #
            # ES: Necesario antes de insertar cualquier texto crudo en un resultado HTML,
            # ya que buena parte del texto de esta biblioteca contiene "<"/">" literales
            # (p. ej. "p < .05", "VIF < 5", "|DFFITS| > 2*sqrt(p/n)") que de otro modo el
            # renderizador confundiría con etiquetas HTML y rompería el panel entero en
            # silencio.
            # -----------------------------------------------------------------------------
            .al_esc <- function(x) {
                x <- gsub("&", "&amp;", x, fixed = TRUE)
                x <- gsub("<", "&lt;", x, fixed = TRUE)
                x <- gsub(">", "&gt;", x, fixed = TRUE)
                x
            }

            # -----------------------------------------------------------------------------
            # Render a section's content vector as real HTML.
            # ES: Renderizar el vector de contenido de una sección como HTML real.
            #
            # Every section below is written as a plain character vector: the first
            # element is the section title, "" elements mark paragraph/block breaks, and
            # within a block a short line ending in ":" acts as a bold sub-header for the
            # lines that follow. This function is what turns that plain-text structure
            # into actual markup — bold title, bold sub-headers on their own line, each
            # blank-separated block as a real <p> paragraph (its wrapped lines joined with
            # a space, since they are one flowing sentence split only for source
            # readability), and a block whose lines mostly look like "Label: text" items
            # (e.g. per-module usage lists, sample-size criteria) rendered instead as
            # bolded, indented list lines. Without this, setContent() would receive a
            # single plain-text string with literal "\n" characters, which HTML collapses
            # into one dense undifferentiated paragraph — the bug this function fixes.
            #
            # ES: Cada sección de abajo se escribe como un vector de caracteres plano: el
            # primer elemento es el título de la sección, los elementos "" marcan cortes de
            # párrafo/bloque, y dentro de un bloque una línea corta terminada en ":" actúa
            # como sub-encabezado en negrita para las líneas que siguen. Esta función es la
            # que convierte esa estructura de texto plano en markup real — título en
            # negrita, sub-encabezados en negrita en su propia línea, cada bloque separado
            # por línea en blanco como un párrafo <p> real (sus líneas envueltas unidas con
            # un espacio, ya que son una sola oración fluida partida solo por legibilidad
            # del código fuente), y un bloque cuyas líneas en su mayoría parecen items
            # "Etiqueta: texto" (p. ej. listas de uso por módulo, criterios de tamaño
            # muestral) renderizado en cambio como líneas de lista en negrita e
            # indentadas. Sin esto, setContent() recibía un único string de texto plano
            # con caracteres "\n" literales, que HTML colapsa en un solo párrafo denso sin
            # diferenciar — el bug que esta función corrige.
            #
            # Input: lines - character vector; lines[1] is the title, the rest is body.
            # Output: a single HTML string ready for result$setContent().
            #
            # ES: Entrada: lines - vector de caracteres; lines[1] es el título, el resto es
            # el cuerpo.
            # ES: Salida: un único string HTML listo para result$setContent().
            # -----------------------------------------------------------------------------
            # -----------------------------------------------------------------------------
            # Render a section title as HTML.
            # ES: Renderizar un título de sección como HTML.
            # -----------------------------------------------------------------------------
            .al_title_html <- function(title) {
                paste0(
                    '<p style="font-weight: 700; font-size: 1.1em; margin: 0 0 0.5em 0;">',
                    .al_esc(title), '</p>'
                )
            }

            # -----------------------------------------------------------------------------
            # Render a section's body lines (everything after the title) as HTML.
            # ES: Renderizar las líneas de cuerpo de una sección (todo tras el título)
            # como HTML.
            #
            # Factored out of .al_render() so a section can splice a real HTML table (see
            # .al_table() below) between two body-text blocks, something a single
            # line-vector cannot express on its own.
            #
            # ES: Separado de .al_render() para que una sección pueda intercalar una tabla
            # HTML real (ver .al_table() abajo) entre dos bloques de texto, algo que un
            # solo vector de líneas no puede expresar por sí solo.
            # -----------------------------------------------------------------------------
            .al_body <- function(rest) {
                rest <- as.character(rest)
                if (length(rest) > 0 && identical(rest[1], ""))
                    rest <- rest[-1]

                blocks <- list()
                current <- character(0)
                for (ln in rest) {
                    if (identical(ln, "")) {
                        if (length(current) > 0) {
                            blocks[[length(blocks) + 1]] <- current
                            current <- character(0)
                        }
                    } else {
                        current <- c(current, ln)
                    }
                }
                if (length(current) > 0)
                    blocks[[length(blocks) + 1]] <- current

                parts <- character(0)
                for (block in blocks) {
                    header <- NULL
                    body_lines <- block
                    if (nchar(block[1]) <= 70 && grepl(":$", block[1]) && length(block) > 1) {
                        header <- block[1]
                        body_lines <- block[-1]
                    }

                    if (!is.null(header)) {
                        parts <- c(parts, paste0(
                            '<p style="margin: 0.7em 0 0.15em 0; font-weight: 700;">',
                            .al_esc(header), '</p>'
                        ))
                    }

                    is_item <- vapply(body_lines, function(x) grepl("^.{1,45}?: ", x), logical(1))
                    looks_like_list <- length(body_lines) > 1 && mean(is_item) >= 0.6

                    if (looks_like_list) {
                        for (bl in body_lines) {
                            m <- regexpr(": ", bl, fixed = TRUE)
                            if (m[1] > 0) {
                                label <- substr(bl, 1, m[1])
                                remainder <- substr(bl, m[1] + 2, nchar(bl))
                                parts <- c(parts, paste0(
                                    '<p style="margin: 0 0 0.2em 1.2em;"><b>', .al_esc(label),
                                    '</b> ', .al_esc(remainder), '</p>'
                                ))
                            } else {
                                parts <- c(parts, paste0(
                                    '<p style="margin: 0 0 0.2em 1.2em;">', .al_esc(bl), '</p>'
                                ))
                            }
                        }
                    } else if (length(body_lines) > 0) {
                        paragraph <- paste(body_lines, collapse = " ")
                        parts <- c(parts, paste0(
                            '<p style="margin: 0 0 0.6em 0;">', .al_esc(paragraph), '</p>'
                        ))
                    }
                }

                paste(parts, collapse = "")
            }

            # -----------------------------------------------------------------------------
            # Render a full section (title + body) as HTML, wrapped in one container.
            # ES: Renderizar una sección completa (título + cuerpo) como HTML, envuelta en
            # un único contenedor.
            #
            # The standard entry point for a section with no table: lines[1] is the
            # title, the rest is passed to .al_body(). Sections that need a table build
            # their HTML by hand instead, combining .al_title_html(), .al_body(), and
            # .al_table() directly (see the independence section for an example).
            #
            # ES: El punto de entrada estándar para una sección sin tabla: lines[1] es el
            # título, el resto se pasa a .al_body(). Las secciones que necesitan una tabla
            # construyen su HTML a mano en cambio, combinando .al_title_html(),
            # .al_body() y .al_table() directamente (ver la sección de independencia como
            # ejemplo).
            # -----------------------------------------------------------------------------
            .al_render <- function(lines) {
                lines <- as.character(lines)
                title <- lines[1]
                rest <- lines[-1]
                paste0(
                    '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                    .al_title_html(title),
                    .al_body(rest),
                    '</div>'
                )
            }

            # -----------------------------------------------------------------------------
            # Render a comparison table as HTML.
            # ES: Renderizar una tabla comparativa como HTML.
            #
            # For content that is genuinely tabular (e.g. comparing several tests across
            # the same criteria) — a real <table> scans far better than trying to force
            # rows into the paragraph/list format .al_body() produces.
            #
            # ES: Para contenido genuinamente tabular (p. ej. comparar varias pruebas
            # según los mismos criterios) — una <table> real se lee mucho mejor que forzar
            # filas al formato de párrafo/lista que produce .al_body().
            #
            # Inputs: headers - character vector of column titles; rows - a list, each
            # element a character vector of the same length as headers (one row's cells).
            # Output: an HTML <table> string.
            #
            # ES: Entradas: headers - vector de caracteres con los títulos de columna;
            # rows - una lista, cada elemento un vector de caracteres de la misma longitud
            # que headers (las celdas de una fila).
            # ES: Salida: un string de <table> HTML.
            # -----------------------------------------------------------------------------
            .al_table <- function(headers, rows) {
                th <- paste0(
                    '<th style="text-align: left; padding: 4px 8px; border-bottom: 2px solid #999; font-weight: 700;">',
                    .al_esc(headers), '</th>', collapse = ""
                )
                trs <- vapply(rows, function(r) {
                    tds <- paste0(
                        '<td style="text-align: left; padding: 4px 8px; border-bottom: 1px solid #ddd; vertical-align: top;">',
                        .al_esc(r), '</td>', collapse = ""
                    )
                    paste0("<tr>", tds, "</tr>")
                }, character(1))
                paste0(
                    '<table style="border-collapse: collapse; width: 100%; margin: 0.5em 0 0.8em 0; font-size: 0.95em;">',
                    "<thead><tr>", th, "</tr></thead>",
                    "<tbody>", paste(trs, collapse = ""), "</tbody>",
                    "</table>"
                )
            }

            self$results$intro$setContent(.al_render(txt("intro")))

            self$results$normality$setContent(paste0(
                '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                paste0(
                    .al_title_html(txt("normalityPart1")[1]),
                    .al_body(txt("normalityPart1")[-1]),
                    .al_table(txt("normalityTableHeaders"), txt("normalityTableRows")),
                    .al_body(txt("normalityPart2")[-1])
                ),
                '</div>'
            ))

            self$results$homoscedasticity$setContent(paste0(
                '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                paste0(
                    .al_title_html(txt("homoscedasticityPart1")[1]),
                    .al_body(txt("homoscedasticityPart1")[-1]),
                    .al_table(txt("homoscedasticityGroupTableHeaders"), txt("homoscedasticityGroupTableRows")),
                    .al_body(txt("homoscedasticityPart2")[-1]),
                    .al_table(txt("homoscedasticityRegTableHeaders"), txt("homoscedasticityRegTableRows")),
                    .al_body(txt("homoscedasticityPart3")[-1])
                ),
                '</div>'
            ))

            self$results$linearity$setContent(paste0(
                '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                paste0(
                    .al_title_html(txt("linearityPart1")[1]),
                    .al_body(txt("linearityPart1")[-1]),
                    .al_table(txt("linearityTableHeaders"), txt("linearityTableRows")),
                    .al_body(txt("linearityPart2")[-1])
                ),
                '</div>'
            ))

            self$results$independence$setContent(paste0(
                '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                paste0(
                    .al_title_html(txt("independencePart1")[1]),
                    .al_body(txt("independencePart1")[-1]),
                    .al_table(txt("independenceTableHeaders"), txt("independenceTableRows")),
                    .al_body(txt("independencePart2")[-1])
                ),
                '</div>'
            ))

            self$results$multicollinearity$setContent(paste0(
                '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                paste0(
                    .al_title_html(txt("multicollinearityPart1")[1]),
                    .al_body(txt("multicollinearityPart1")[-1]),
                    .al_table(txt("multicollinearityTableHeaders"), txt("multicollinearityTableRows")),
                    .al_body(txt("multicollinearityPart2")[-1])
                ),
                '</div>'
            ))

            self$results$influence$setContent(paste0(
                '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                paste0(
                    .al_title_html(txt("influencePart1")[1]),
                    .al_body(txt("influencePart1")[-1]),
                    .al_table(txt("influenceTableHeaders"), txt("influenceTableRows")),
                    .al_body(txt("influencePart2")[-1])
                ),
                '</div>'
            ))

            self$results$sphericity$setContent(paste0(
                '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                paste0(
                    .al_title_html(txt("sphericityPart1")[1]),
                    .al_body(txt("sphericityPart1")[-1]),
                    .al_table(txt("sphericityTableHeaders"), txt("sphericityTableRows")),
                    .al_body(txt("sphericityPart2")[-1])
                ),
                '</div>'
            ))

            self$results$robust$setContent(paste0(
                '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;">',
                paste0(
                    .al_title_html(txt("robustPart1")[1]),
                    .al_body(txt("robustPart1")[-1]),
                    .al_table(txt("robustTableHeaders"), txt("robustTableRows")),
                    .al_body(txt("robustPart2")[-1])
                ),
                '</div>'
            ))

            hide_if_needed(self$results$normality, "normality")
            hide_if_needed(self$results$homoscedasticity, "homoscedasticity")
            hide_if_needed(self$results$linearity, "linearity")
            hide_if_needed(self$results$independence, "independence")
            hide_if_needed(self$results$multicollinearity, "multicollinearity")
            hide_if_needed(self$results$influence, "influence")
            hide_if_needed(self$results$sphericity, "sphericity")
            hide_if_needed(self$results$robust, "robust")

            self$results$notes$setContent(.al_render(txt("notes")))
        }
    )
)
