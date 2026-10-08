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
# 1. Render the comparison table for each assumption/test category covered
#    by the suite (normality, homoscedasticity, independence, etc.), in
#    every active report language.
# 2. Filter which category's items are shown, based on the user's selection.
#
# ES: Responsabilidades
# 1. Renderizar la tabla comparativa de cada categoría de supuesto/prueba
#    cubierta por la suite (normalidad, homoscedasticidad, independencia,
#    etc.), en cada idioma de informe activo.
# 2. Filtrar qué ítems de categoría se muestran, según la selección del
#    usuario.
#
# Narrative guide text per category (every "<category>" and
# "<category>Guide2"/"Guide3" Html item) is static content defined directly
# in jamovi/assumptionlibrary.r.yaml and translated through jamovi's own
# native i18n catalog (jamovi/i18n/es.po), not through this module's
# reportLang option: that text never changes at runtime, so there is nothing
# for .run() to set. Only each category's comparison table
# ("<category>Table"/"...GroupTable"/"...RegTable") is still built here,
# because jamovi has no YAML-level way to express a table with several
# statically-different rows (a column's own "content:" field can only hold
# ONE fixed value repeated down that column, never distinct per-row text) -
# see CODE_STYLE.md §21 and ARCHITECTURE.md §11 for why this split exists
# and why going further would reintroduce the exact jmvcore translator-
# caching bug this project hit in FiabilityLab (`.()` called from R is
# literally `self$options$translate()`, the same cached call).
#
# ES: El texto narrativo de guía por categoría (cada ítem Html
# "<categoría>" y "<categoría>Guide2"/"Guide3") es contenido estático
# definido directamente en jamovi/assumptionlibrary.r.yaml y traducido por
# el propio catálogo i18n nativo de jamovi (jamovi/i18n/es.po), no por la
# opción reportLang de este módulo: ese texto nunca cambia en tiempo de
# ejecución, así que .run() no tiene nada que fijarle. Solo la tabla
# comparativa de cada categoría ("<categoría>Table"/"...GroupTable"/
# "...RegTable") sigue construyéndose aquí, porque jamovi no tiene forma a
# nivel YAML de expresar una tabla con varias filas estáticas distintas (el
# campo "content:" de una columna solo puede llevar UN valor fijo repetido
# en toda la columna, nunca texto distinto por fila) - ver CODE_STYLE.md
# §21 y ARCHITECTURE.md §11 para por qué existe esta división y por qué ir
# más allá reintroduciría exactamente el bug de caché del traductor de
# jmvcore que este proyecto encontró en FiabilityLab (`.()` llamado desde R
# es literalmente `self$options$translate()`, la misma llamada cacheada).
#
# Workflow
# 1. Read the selected category and report language.
# 2. Resolve each category's table text via the shared .al_text()
#    repository (texts.R, section "library") and render it as HTML.
# 3. Hide any item not matching the selected category.
#
# ES: Flujo de trabajo
# 1. Leer la categoría seleccionada y el idioma del informe.
# 2. Resolver el texto de la tabla de cada categoría vía el repositorio
#    compartido .al_text() (texts.R, sección "library") y renderizarlo
#    como HTML.
# 3. Ocultar cualquier ítem que no coincida con la categoría
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
            # Only the comparison tables below use this; the static guide text
            # is translated natively instead (see the file header).
            # ES: Cableado bilingüe (estándar AssumptionsLab — ver regCheck/
            # logCheck). Solo las tablas comparativas de abajo lo usan; el
            # texto estático de guía se traduce de forma nativa en cambio
            # (ver el encabezado del archivo).
            # -----------------------------------------------------------------------------
            lang <- .al_normalize_lang(self$options$reportLang)

            txt <- function(key) {
                .al_text(lang, "library", key)
            }

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
            # Required before inserting any raw text into an HTML result, since some
            # table cells contain literal "<"/">" (e.g. "p < .05", "VIF < 5") that
            # would otherwise be mistaken for HTML tags by the renderer.
            #
            # ES: Necesario antes de insertar cualquier texto crudo en un resultado HTML,
            # ya que algunas celdas de tabla contienen "<"/">" literales (p. ej.
            # "p < .05", "VIF < 5") que de otro modo el renderizador confundiría con
            # etiquetas HTML.
            # -----------------------------------------------------------------------------
            .al_esc <- function(x) {
                x <- gsub("&", "&amp;", x, fixed = TRUE)
                x <- gsub("<", "&lt;", x, fixed = TRUE)
                x <- gsub(">", "&gt;", x, fixed = TRUE)
                x
            }

            # -----------------------------------------------------------------------------
            # Render a comparison table as HTML.
            # ES: Renderizar una tabla comparativa como HTML.
            #
            # Inputs: headers - character vector of column titles; rows - a list, each
            # element a character vector of the same length as headers (one row's cells).
            # Output: an HTML <table> string, wrapped in the same container used
            # throughout the library's other items.
            #
            # ES: Entradas: headers - vector de caracteres con los títulos de columna;
            # rows - una lista, cada elemento un vector de caracteres de la misma longitud
            # que headers (las celdas de una fila).
            # ES: Salida: un string de <table> HTML, envuelto en el mismo contenedor
            # usado en el resto de los ítems de la biblioteca.
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
                    '<div style="max-width: 7.25in; width: 100%; box-sizing: border-box;">',
                    '<table style="border-collapse: collapse; width: 100%; margin: 0.5em 0 0.8em 0; font-size: 0.95em;">',
                    "<thead><tr>", th, "</tr></thead>",
                    "<tbody>", paste(trs, collapse = ""), "</tbody>",
                    "</table>",
                    '</div>'
                )
            }

            self$results$normalityTable$setContent(
                .al_table(txt("normalityTableHeaders"), txt("normalityTableRows"))
            )
            self$results$homoscedasticityGroupTable$setContent(
                .al_table(txt("homoscedasticityGroupTableHeaders"), txt("homoscedasticityGroupTableRows"))
            )
            self$results$homoscedasticityRegTable$setContent(
                .al_table(txt("homoscedasticityRegTableHeaders"), txt("homoscedasticityRegTableRows"))
            )
            self$results$linearityTable$setContent(
                .al_table(txt("linearityTableHeaders"), txt("linearityTableRows"))
            )
            self$results$independenceTable$setContent(
                .al_table(txt("independenceTableHeaders"), txt("independenceTableRows"))
            )
            self$results$multicollinearityTable$setContent(
                .al_table(txt("multicollinearityTableHeaders"), txt("multicollinearityTableRows"))
            )
            self$results$influenceTable$setContent(
                .al_table(txt("influenceTableHeaders"), txt("influenceTableRows"))
            )
            self$results$sphericityTable$setContent(
                .al_table(txt("sphericityTableHeaders"), txt("sphericityTableRows"))
            )
            self$results$proportionalOddsTable$setContent(
                .al_table(txt("proportionalOddsTableHeaders"), txt("proportionalOddsTableRows"))
            )
            # "iia" is the short, unambiguous prefix already used for this
            # assumption's txt() keys and multCheck's own result names.
            # ES: "iia" es el prefijo corto e inequívoco ya usado para las
            # claves txt() de este supuesto y los nombres de resultado
            # propios de multCheck.
            self$results$independenceIrrelevantAlternativesTable$setContent(
                .al_table(txt("iiaTableHeaders"), txt("iiaTableRows"))
            )
            self$results$robustTable$setContent(
                .al_table(txt("robustTableHeaders"), txt("robustTableRows"))
            )

            hide_if_needed(self$results$normality, "normality")
            hide_if_needed(self$results$normalityTable, "normality")
            hide_if_needed(self$results$normalityGuide2, "normality")
            hide_if_needed(self$results$homoscedasticity, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticityGroupTable, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticityGuide2, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticityRegTable, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticityGuide3, "homoscedasticity")
            hide_if_needed(self$results$linearity, "linearity")
            hide_if_needed(self$results$linearityTable, "linearity")
            hide_if_needed(self$results$linearityGuide2, "linearity")
            hide_if_needed(self$results$independence, "independence")
            hide_if_needed(self$results$independenceTable, "independence")
            hide_if_needed(self$results$independenceGuide2, "independence")
            hide_if_needed(self$results$multicollinearity, "multicollinearity")
            hide_if_needed(self$results$multicollinearityTable, "multicollinearity")
            hide_if_needed(self$results$multicollinearityGuide2, "multicollinearity")
            hide_if_needed(self$results$influence, "influence")
            hide_if_needed(self$results$influenceTable, "influence")
            hide_if_needed(self$results$influenceGuide2, "influence")
            hide_if_needed(self$results$sphericity, "sphericity")
            hide_if_needed(self$results$sphericityTable, "sphericity")
            hide_if_needed(self$results$sphericityGuide2, "sphericity")
            hide_if_needed(self$results$proportionalOdds, "proportionalOdds")
            hide_if_needed(self$results$proportionalOddsTable, "proportionalOdds")
            hide_if_needed(self$results$proportionalOddsGuide2, "proportionalOdds")
            hide_if_needed(self$results$independenceIrrelevantAlternatives, "independenceIrrelevantAlternatives")
            hide_if_needed(self$results$independenceIrrelevantAlternativesTable, "independenceIrrelevantAlternatives")
            hide_if_needed(self$results$independenceIrrelevantAlternativesGuide2, "independenceIrrelevantAlternatives")
            hide_if_needed(self$results$robust, "robust")
            hide_if_needed(self$results$robustTable, "robust")
            hide_if_needed(self$results$robustGuide2, "robust")
        }
    )
)
