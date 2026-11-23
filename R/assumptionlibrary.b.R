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
# neutral. The 9 analysis modules now use the same native catalog for
# their data-dependent text: English strings inside `.()`, translated at run
# time through `.al_lang()` (see shared-helpers.R and ARCHITECTURE.md §11).
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
# engañoso, no neutral. Los 9 módulos de análisis usan ahora el mismo
# catálogo nativo para su texto que depende de los datos: cadenas en inglés
# dentro de `.()`, traducidas en tiempo de ejecución con `.al_lang()` (ver
# shared-helpers.R y ARCHITECTURE.md §11).
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
# 1. Fill each category's Sources table, the native jamovi Table that
#    carries the category's `refs:` (see .al_library_sources()).
# 2. Show only the items matching the user's selected category.
#
# ES: Responsabilidad
# 1. Llenar la tabla Fuentes de cada categoría, la Table nativa de jamovi
#    que lleva los `refs:` de la categoría (ver .al_library_sources()).
# 2. Mostrar solo los ítems que coinciden con la categoría seleccionada por
#    el usuario.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .al_library_sources()
#
# Input: . - the translator of the run (`lang$tr`, see .al_lang()).
# Output: a named list, one entry per Sources table of the library; each
#   entry is a list of c(method, source) pairs, one per table row.
#
# Each category of the library ends with a native jamovi Table ("Sources")
# whose `refs:` (jamovi/assumptionlibrary.r.yaml) point to jamovi/00refs.yaml.
# jamovi's client draws the reference numbers and the References list only
# for Table results, never for Html ones (ARCHITECTURE.md §11), so these
# small tables are what lets the library cite the way every other analysis
# of the module does. The rows name each method with its author and year,
# so the reader can match a row to its entry in jamovi's References list.
# Method names that are proper names are not translated; the rest go
# through `.()` and jamovi/i18n/es.po.
#
# ES: Cada categoría de la biblioteca termina con una Table nativa de jamovi
# ("Fuentes") cuyos `refs:` apuntan a jamovi/00refs.yaml. El cliente de
# jamovi solo dibuja los números de referencia y la lista de Referencias
# para resultados Table, nunca Html (ARCHITECTURE.md §11), así que estas
# tablas pequeñas son las que permiten que la biblioteca cite igual que los
# demás análisis del módulo. Cada fila nombra el método con su autor y año,
# para que el lector la relacione con su entrada en la lista de
# Referencias de jamovi. Los nombres propios no se traducen; el resto pasa
# por `.()` y jamovi/i18n/es.po.
# -----------------------------------------------------------------------------
.al_library_sources <- function(.) {
    list(
        normalitySources = list(
            c("Shapiro-Wilk", "Shapiro & Wilk (1965)"),
            c(.("Power of Shapiro-Wilk against other normality tests"), "Razali & Wah (2011)"),
            c(.("Lilliefors (corrected Kolmogorov-Smirnov)"), "Lilliefors (1967)"),
            c("Anderson-Darling and Cram\u00e9r-von Mises", "Anderson & Darling (1952)"),
            c("Shapiro-Francia", "Shapiro & Francia (1972)"),
            c("Jarque-Bera", "Jarque & Bera (1987)")),
        homoscedasticitySources = list(
            c("Levene", "Levene (1960)"),
            c("Brown-Forsythe", "Brown & Forsythe (1974)"),
            c("Bartlett", "Bartlett (1937)"),
            c("Fligner-Killeen", "Fligner & Killeen (1976)"),
            c(.("Hartley's Fmax"), "Hartley (1950)"),
            c("Breusch-Pagan", "Breusch & Pagan (1979)"),
            c("White", "White (1980)"),
            c("Goldfeld-Quandt", "Goldfeld & Quandt (1965)"),
            c(.("Welch correction"), "Welch (1947)"),
            c(.("HC3 robust standard errors"), "MacKinnon & White (1985)")),
        linearitySources = list(
            c(.("Distance correlation (dCor)"), "Sz\u00e9kely, Rizzo & Bakirov (2007)"),
            c(.("Copula entropy"), "Ma & Sun (2011)"),
            c("Box-Tidwell", "Box & Tidwell (1962)"),
            c("Ramsey RESET", "Ramsey (1969)"),
            c("Rainbow test", "Utts (1982)")),
        independenceSources = list(
            c("Durbin-Watson", "Durbin & Watson (1951)"),
            c("Breusch-Godfrey", "Breusch (1978); Godfrey (1978)"),
            c("Ljung-Box", "Ljung & Box (1978)"),
            c(.("Runs test"), "Wald & Wolfowitz (1940)")),
        multicollinearitySources = list(
            c(.("VIF and tolerance"), "Marquardt (1970)"),
            c(.("Rules of thumb for the VIF"), "O'Brien (2007)"),
            c(.("Condition index and variance proportions"), "Belsley, Kuh & Welsch (1980)")),
        influenceSources = list(
            c(.("IQR rule"), "Tukey (1977)"),
            c(.("Cook's D"), "Cook (1977)"),
            c(.("Leverage, studentized residuals and DFFITS"), "Belsley, Kuh & Welsch (1980)"),
            c("Mahalanobis D\u00b2", "Mahalanobis (1936)")),
        sphericitySources = list(
            c("Mauchly (W)", "Mauchly (1940)"),
            c(.("Greenhouse-Geisser and lower-bound"), "Greenhouse & Geisser (1959)"),
            c("Huynh-Feldt", "Huynh & Feldt (1976)"),
            c(.("Friedman test"), "Friedman (1937)")),
        proportionalOddsSources = list(
            c(.("Proportional-odds model"), "McCullagh (1980)"),
            c(.("Brant test"), "Brant (1990)")),
        independenceIrrelevantAlternativesSources = list(
            c(.("Hausman-McFadden test"), "Hausman & McFadden (1984)"),
            c(.("Behaviour of Hausman-type IIA tests"), "Cheng & Long (2007)")),
        countModelsSources = list(
            c(.("Overdispersion and count regression"), "Cameron & Trivedi (2013)"),
            c(.("Dean-Lawless score test"), "Dean & Lawless (1989)"),
            c(.("Cameron-Trivedi auxiliary regression"), "Cameron & Trivedi (1990)"),
            c(.("Likelihood ratio test on the boundary"), "Self & Liang (1987)"),
            c(.("Score test for zero inflation"), "van den Broek (1995)")),
        robustSources = list(
            c(.("Welch's t and F tests"), "Welch (1947)"),
            c("Brown-Forsythe", "Brown & Forsythe (1974)"),
            c(.("Wilcoxon signed-rank test"), "Wilcoxon (1945)"),
            c(.("Friedman test"), "Friedman (1937)"),
            c(.("HC3 robust standard errors"), "MacKinnon & White (1985)")))
}

assumptionLibraryClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "assumptionLibraryClass",
    inherit = assumptionLibraryBase,
    private = list(
        .init = function() {

            . <- .al_lang(self$options)$tr
            sources <- .al_library_sources(.)

            for (name in names(sources)) {
                table <- self$results[[name]]
                rows <- sources[[name]]
                for (i in seq_along(rows))
                    table$addRow(rowKey = i, values = list(
                        method = rows[[i]][[1]],
                        source = rows[[i]][[2]]))
            }
        },

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
            hide_if_needed(self$results$normalitySources, "normality")
            hide_if_needed(self$results$homoscedasticity, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticityGroupTable, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticityGuide2, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticityRegTable, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticityGuide3, "homoscedasticity")
            hide_if_needed(self$results$homoscedasticitySources, "homoscedasticity")
            hide_if_needed(self$results$linearity, "linearity")
            hide_if_needed(self$results$linearityTable, "linearity")
            hide_if_needed(self$results$linearityGuide2, "linearity")
            hide_if_needed(self$results$linearitySources, "linearity")
            hide_if_needed(self$results$independence, "independence")
            hide_if_needed(self$results$independenceTable, "independence")
            hide_if_needed(self$results$independenceGuide2, "independence")
            hide_if_needed(self$results$independenceSources, "independence")
            hide_if_needed(self$results$multicollinearity, "multicollinearity")
            hide_if_needed(self$results$multicollinearityTable, "multicollinearity")
            hide_if_needed(self$results$multicollinearityGuide2, "multicollinearity")
            hide_if_needed(self$results$multicollinearitySources, "multicollinearity")
            hide_if_needed(self$results$influence, "influence")
            hide_if_needed(self$results$influenceTable, "influence")
            hide_if_needed(self$results$influenceGuide2, "influence")
            hide_if_needed(self$results$influenceSources, "influence")
            hide_if_needed(self$results$sphericity, "sphericity")
            hide_if_needed(self$results$sphericityTable, "sphericity")
            hide_if_needed(self$results$sphericityGuide2, "sphericity")
            hide_if_needed(self$results$sphericitySources, "sphericity")
            hide_if_needed(self$results$proportionalOdds, "proportionalOdds")
            hide_if_needed(self$results$proportionalOddsTable, "proportionalOdds")
            hide_if_needed(self$results$proportionalOddsGuide2, "proportionalOdds")
            hide_if_needed(self$results$proportionalOddsSources, "proportionalOdds")
            hide_if_needed(self$results$independenceIrrelevantAlternatives, "independenceIrrelevantAlternatives")
            hide_if_needed(self$results$independenceIrrelevantAlternativesTable, "independenceIrrelevantAlternatives")
            hide_if_needed(self$results$independenceIrrelevantAlternativesGuide2, "independenceIrrelevantAlternatives")
            hide_if_needed(self$results$independenceIrrelevantAlternativesSources, "independenceIrrelevantAlternatives")
            hide_if_needed(self$results$countModels, "countModels")
            hide_if_needed(self$results$countModelsTable, "countModels")
            hide_if_needed(self$results$countModelsGuide2, "countModels")
            hide_if_needed(self$results$countModelsSources, "countModels")
            hide_if_needed(self$results$robust, "robust")
            hide_if_needed(self$results$robustTable, "robust")
            hide_if_needed(self$results$robustGuide2, "robust")
            hide_if_needed(self$results$robustSources, "robust")
        }
    )
)
