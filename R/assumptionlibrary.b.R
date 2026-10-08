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
# Every category's content - guide text AND its comparison table - is static:
# it never depends on the user's data, so there is nothing for .run() to
# build. Each category's full HTML (guide paragraphs + the HTML <table> that
# used to be built by .al_table() at runtime) is now a static `content:`
# field defined directly in jamovi/assumptionlibrary.r.yaml, generated once
# from the EN/ES text already in texts.R (section "library") and extracted/
# translated through jamovi's own native i18n catalog (jamovi/i18n/es.po) -
# the same mechanism as `title`/`description`, confirmed in jamovi-compiler's
# own i18n.js. This follows jamovi's UI language automatically and needed no
# `reportLang` option at all, so that option (and its separate "Report
# Language" control) was removed from this analysis entirely - keeping it
# around after nothing used it would have been actively misleading, not
# neutral. See CODE_STYLE.md §20/§21 and ARCHITECTURE.md §11 for why this
# is safe here (static content) but not for the 9 analysis modules' guide/
# interpretation text (computed from the user's data on every run, so it
# still needs `tr()`/`reportLang`, never `.()` called from R - that call is
# literally the cached `Options$translate()` behind the FiabilityLab
# translator regression).
#
# Each category is still several result items (a guide paragraph, its
# table, a closing guide paragraph - five items for homoscedasticity,
# which has two tables) rather than one combined item, purely because R's
# own parser refuses a single string literal needing Unicode escapes once
# it crosses roughly 10,000 characters ("string constant is too long" at
# compile time) - several of these categories' combined guide+table text
# comfortably exceeds that. Splitting keeps every individual item's
# content well under the limit while every piece stays equally static and
# natively translated; it is not a language-handling split like the
# earlier, reverted attempt (every item here uses exactly the same
# mechanism, just addressed by a different result name).
#
# ES: El contenido de cada categoría - texto de guía Y su tabla comparativa -
# es estático: nunca depende de los datos del usuario, así que no hay nada
# que .run() deba construir. El HTML completo de cada categoría (párrafos de
# guía + la tabla HTML que antes construía .al_table() en tiempo de
# ejecución) es ahora un campo `content:` estático definido directamente en
# jamovi/assumptionlibrary.r.yaml, generado una sola vez desde el texto EN/ES
# que ya existía en texts.R (sección "library") y extraído/traducido vía el
# propio catálogo i18n nativo de jamovi (jamovi/i18n/es.po) - el mismo
# mecanismo que `title`/`description`, confirmado en el propio i18n.js de
# jamovi-compiler. Esto sigue el idioma de la interfaz de jamovi
# automáticamente y no necesitó ninguna opción `reportLang`, así que esa
# opción (y su control "Report Language" aparte) se quitó por completo de
# este análisis - dejarla ahí sin que nada la usara habría sido activamente
# engañoso, no neutral. Ver CODE_STYLE.md §20/§21 y ARCHITECTURE.md §11 para
# por qué esto es seguro aquí (contenido estático) pero no para el texto de
# guía/interpretación de los 9 módulos de análisis (calculado a partir de
# los datos del usuario en cada corrida, así que sigue necesitando
# `tr()`/`reportLang`, nunca `.()` llamado desde R - esa llamada es
# literalmente el `Options$translate()` cacheado detrás de la regresión del
# traductor de FiabilityLab).
#
# Cada categoría sigue siendo varios ítems de resultado (un párrafo de
# guía, su tabla, un párrafo de guía de cierre - cinco ítems para
# homoscedasticity, que tiene dos tablas) en vez de uno combinado, solo
# porque el propio parser de R rechaza un literal de cadena que necesite
# escapes Unicode una vez que cruza aproximadamente 10,000 caracteres
# ("string constant is too long" en tiempo de compilación) - el texto
# combinado de guía+tabla de varias de estas categorías supera
# cómodamente ese límite. Dividirlo mantiene el contenido de cada ítem
# individual bien por debajo del límite mientras cada pieza sigue siendo
# igual de estática y traducida de forma nativa; no es una división de
# manejo de idioma como el intento anterior, revertido (cada ítem aquí
# usa exactamente el mismo mecanismo, solo que con un nombre de resultado
# distinto).
#
# Responsibility
# 1. Show only the items matching the user's selected category.
#
# ES: Responsabilidad
# 1. Mostrar solo los ítems que coinciden con la categoría seleccionada por
#    el usuario.
# -----------------------------------------------------------------------------

assumptionLibraryClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "assumptionLibraryClass",
    inherit = assumptionLibraryBase,
    private = list(
        .run = function() {

            category <- self$options$category

            show_section <- function(name) {
                category == "all" || category == name
            }

            hide_if_needed <- function(result, name) {
                if (!show_section(name))
                    result$setVisible(FALSE)
            }

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
