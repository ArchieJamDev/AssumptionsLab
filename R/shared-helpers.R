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
# Shared, purely structural helpers used across module .b.R files.
#
# This file holds ONLY code that was already byte-for-byte or behaviorally
# identical across two or more modules (verified before merging, per the
# AssumptionsLab de-duplication mapping). It does not change any threshold,
# color, axis, edge-case handling, or narrative text that existed before the
# merge. Narrative/interpretation text stays in texts.R and in each module's
# own .b.R file; this file is compute/translation plumbing only.
#
# ES: Helpers compartidos y puramente estructurales, usados por varios
# archivos .b.R de módulos.
#
# ES: Este archivo contiene SOLO código que ya era idéntico, en bytes o en
# comportamiento, entre dos o más módulos (verificado antes de fusionar,
# según el mapeo de duplicación de AssumptionsLab). No cambia ningún umbral,
# color, eje, manejo de casos borde, ni texto narrativo que existiera antes
# de la fusión. El texto narrativo/interpretativo se queda en texts.R y en
# el propio archivo .b.R de cada módulo; este archivo es solo plomería de
# cómputo/traducción.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .al_tr()
#
# Input: lang - already-normalized language code ("es" or "en", typically the
#   result of .al_normalize_lang()); en - English text; es - Spanish text
#   (optional; defaults to `en` when the text is identical in both languages).
# Output: `es` when lang is "es", otherwise `en`.
#
# This is the canonical two-language translator. Every module previously
# defined its own local `tr()` closure with this exact behavior (some under
# slightly different signatures). One module (regCheck) additionally accepted
# `fr`/`pt`/`it` parameters via a 5-language switch(), but no call site in the
# suite ever passed those three arguments - it was dead code left over from a
# multi-language plan that is no longer in scope (suite is English/Spanish
# only). That dead branch is retired here, not migrated.
#
# ES: Traductor canónico de dos idiomas. Cada módulo definía antes su propio
# closure local `tr()` con este mismo comportamiento (algunos con una firma
# ligeramente distinta). Un módulo (regCheck) además aceptaba parámetros
# `fr`/`pt`/`it` vía un switch() de 5 idiomas, pero ningún punto de llamada
# en la suite pasaba jamás esos tres argumentos - era código muerto de un
# plan multi-idioma que ya no está en alcance (la suite es solo
# inglés/español). Esa rama muerta se retira acá, no se migra.
# -----------------------------------------------------------------------------
.al_tr <- function(lang, en, es = NULL) {
    if (is.null(es))
        es <- en
    if (identical(lang, "es")) es else en
}

# -----------------------------------------------------------------------------
# .al_bptest()
#
# Input: fit - a fitted model object accepted by lmtest::bptest() (e.g. an lm
#   object).
# Output: the htest object returned by lmtest::bptest(fit), unchanged; NULL if
#   the call errors (e.g. package unavailable, model not applicable).
#
# Thin, behavior-preserving wrapper. Both pathCheck and regCheck called
# `tryCatch(lmtest::bptest(fit), error = function(e) NULL)` independently and
# then read `$statistic`, `$parameter`, `$p.value` off the result in their own
# (differently formatted) ways downstream. Only the error-handled call itself
# is centralized here; each module keeps extracting fields and building its
# own result-table row exactly as before.
#
# ES: Envoltorio delgado que preserva el comportamiento. Tanto pathCheck como
# regCheck llamaban por separado
# `tryCatch(lmtest::bptest(fit), error = function(e) NULL)` y luego leían
# `$statistic`, `$parameter`, `$p.value` del resultado cada uno a su manera
# (con formato distinto) más abajo. Acá se centraliza solo la llamada con
# manejo de error; cada módulo sigue extrayendo los campos y armando su
# propia fila de tabla de resultados exactamente como antes.
# -----------------------------------------------------------------------------
.al_bptest <- function(fit) {
    tryCatch(lmtest::bptest(fit), error = function(e) NULL)
}

# -----------------------------------------------------------------------------
# .al_html_escape()
#
# Input: x - a character vector (or coercible to one).
# Output: x with &, <, >, " replaced by their HTML entities.
#
# Byte-identical across all 7 modules that had their own local copy
# (anovaCheck, groupCheck, logCheck, pathCheck, regCheck, relatedCheck,
# timeCheck) before this merge - a direct, zero-risk consolidation.
#
# ES: Idéntica byte a byte en los 7 módulos que tenían su propia copia local
# antes de esta fusión - una consolidación directa y de riesgo cero.
# -----------------------------------------------------------------------------
.al_html_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
}

# -----------------------------------------------------------------------------
# .al_html_block()
#
# Input: title - optional heading text (already translated), or NULL/""
#   to omit it; text - a character vector of already-translated fragments
#   (each element becomes its own paragraph when paragraphs = TRUE, or all
#   elements are joined with a space into one paragraph when
#   paragraphs = FALSE); paragraphs - see above; escape - if TRUE (default),
#   each fragment is HTML-escaped before insertion; pass FALSE only when the
#   caller has already built trusted inline markup (e.g. "<b>...</b>"
#   labels) with any dynamic values pre-escaped itself; raw - if TRUE, text
#   is treated as ALREADY-COMPLETE HTML (e.g. the output of
#   .al_html_list()) and is inserted as-is, with no <p> wrapping and no
#   escaping, regardless of the paragraphs/escape arguments.
# Output: an HTML string: a <div> (justified, page-break-safe) containing an
#   optional <h3> title and the assembled body.
#
# This consolidates what were 3 independently-evolved implementations
# across 6 modules (pathCheck, regCheck, logCheck, timeCheck used one
# variant without justification; groupCheck had the same variant but WITH
# `text-align: justify` already correctly added; relatedCheck used a third,
# differently-structured variant, also without justification). This merged
# version keeps groupCheck's justification (the only one that already had
# it - verified correct against its rendered output) and logCheck's escape
# parameter (the only module that needed it). No threshold, color, or
# narrative text changes - purely the HTML-assembly plumbing.
#
# ES: Esto consolida lo que eran 3 implementaciones evolucionadas por
# separado en 6 módulos (pathCheck, regCheck, logCheck, timeCheck usaban
# una variante sin justificar; groupCheck tenía la misma variante pero YA
# con `text-align: justify` agregado correctamente; relatedCheck usaba una
# tercera variante, estructurada distinto, también sin justificar). Esta
# versión fusionada conserva la justificación de groupCheck (la única que
# ya la tenía - verificada contra su salida renderizada) y el parámetro
# escape de logCheck (el único módulo que lo necesitaba). Sin cambios de
# umbral, color, ni texto narrativo - solo plomería de ensamblado HTML.
# -----------------------------------------------------------------------------
.al_html_block <- function(title = NULL, text, paragraphs = TRUE, escape = TRUE, raw = FALSE) {

    if (isTRUE(raw)) {
        body <- paste(text, collapse = "\n")
    } else {
        parts <- .al_clean_text(text)

        if (length(parts) == 0)
            return("")

        p_style <- "margin: 0.15em 0 0.65em 0; line-height: 1.40;"
        maybe_escape <- if (isTRUE(escape)) .al_html_escape else identity

        if (isTRUE(paragraphs)) {
            body <- paste0(
                "<p style=\"", p_style, "\">",
                maybe_escape(parts),
                "</p>",
                collapse = "\n"
            )
        } else {
            body <- paste0(
                "<p style=\"", p_style, "\">",
                maybe_escape(paste(parts, collapse = " ")),
                "</p>"
            )
        }
    }

    if (nzchar(body) == FALSE)
        return("")

    h_style <- "margin: 0.55em 0 0.50em 0; line-height: 1.25;"
    div_style <- paste(
        "max-width: 7.25in;",
        "width: 100%;",
        "box-sizing: border-box; text-align: justify;"
    )

    if (!is.null(title) && nzchar(title)) {
        body <- paste0(
            "<h3 style=\"", h_style, "\">",
            .al_html_escape(title),
            "</h3>\n",
            body
        )
    }

    paste0("<div style=\"", div_style, "\">\n", body, "\n</div>")
}

# -----------------------------------------------------------------------------
# .al_html_list()
#
# Input: items - a character vector of already-translated list items, WITHOUT
#   any leading "- " marker (the marker is drawn by the <li> itself);
#   ordered - FALSE (default) for a bulleted <ul>, TRUE for a numbered <ol>;
#   escape - if TRUE (default), each item is HTML-escaped.
# Output: a complete, justified <ul>/<ol> HTML string, or "" if items is
#   empty after cleaning. Pass the result to .al_html_block(..., raw = TRUE)
#   to add a title/wrapper around it.
#
# Before this function existed, every "Brief guide" bullet list in the
# suite was built by prefixing each translated sentence with a literal
# "- " and feeding the joined text through block96()/html_block() as plain
# prose. Those two functions independently trimmed and rejoined the text,
# which discarded the line breaks between items - so what was meant to be
# a list rendered as a single undifferentiated paragraph with hyphens
# embedded mid-sentence. This function replaces that pattern with a real
# HTML list, with each item as its own justified <li>.
#
# ES: Antes de que existiera esta función, cada lista con viñetas de "Brief
# guide" en la suite se armaba prefijando cada oración traducida con un
# "- " literal y pasando el texto unido por block96()/html_block() como
# prosa plana. Esas dos funciones recortaban y volvían a unir el texto por
# separado, lo cual descartaba los saltos de línea entre ítems - así que lo
# que debía ser una lista se renderizaba como un único párrafo
# indiferenciado con guiones incrustados a mitad de oración. Esta función
# reemplaza ese patrón por una lista HTML real, con cada ítem como su
# propio <li> justificado.
# -----------------------------------------------------------------------------
.al_html_list <- function(items, ordered = FALSE, escape = TRUE) {
    parts <- .al_clean_text(items)

    if (length(parts) == 0)
        return("")

    maybe_escape <- if (isTRUE(escape)) .al_html_escape else identity

    tag <- if (isTRUE(ordered)) "ol" else "ul"
    li_style <- "margin: 0 0 0.30em 0; line-height: 1.40; text-align: justify;"
    list_style <- "margin: 0.15em 0 0.65em 0; padding-left: 1.35em;"

    items_html <- paste0(
        "<li style=\"", li_style, "\">",
        maybe_escape(parts),
        "</li>",
        collapse = "\n"
    )

    paste0("<", tag, " style=\"", list_style, "\">\n", items_html, "\n</", tag, ">")
}

# -----------------------------------------------------------------------------
# .al_nortest_battery()
#
# Input: x - a numeric vector already filtered to finite/non-NA values (the
#   caller's own residual or difference vector, exactly as it was passed to
#   each individual nortest:: call before this merge).
# Output: a named list with 5 elements - li, ad, cvm, sf, pt - each either the
#   htest object returned by the corresponding nortest:: function, or NULL if
#   that call errored. Field names match the local variable names every
#   calling module already used (li/ad/cvm/sf/pt), so callers can destructure
#   with `li <- b$li; ad <- b$ad; ...` and keep every downstream line (row
#   building, NA fallback, narrative text) completely unchanged.
#
# These 5 calls - Lilliefors, Anderson-Darling, Cramer-von Mises,
# Shapiro-Francia, Pearson chi-square - were verified byte-identical (same
# tryCatch/error-handling shape, no per-module guard condition) across
# anovaCheck, groupCheck, pathCheck, regCheck, and relatedCheck. Unlike
# Shapiro-Wilk and the manual Jarque-Bera/skewness/kurtosis formulas (which
# have a real guard-condition split between modules - see the mapping notes
# - and are intentionally NOT included here), these 5 have no divergence to
# reconcile, so consolidating them changes nothing about what gets computed.
#
# ES: Estas 5 llamadas - Lilliefors, Anderson-Darling, Cramer-von Mises,
# Shapiro-Francia, Pearson chi-cuadrado - se verificaron idénticas byte a
# byte (mismo manejo de tryCatch/error, sin ninguna guarda propia por
# módulo) en anovaCheck, groupCheck, pathCheck, regCheck y relatedCheck. A
# diferencia de Shapiro-Wilk y las fórmulas manuales de Jarque-Bera/
# asimetría/curtosis (que sí tienen una división real de guardas entre
# módulos - ver notas de mapeo - y quedan deliberadamente FUERA de acá),
# estas 5 no tienen ninguna divergencia que reconciliar, así que
# consolidarlas no cambia nada de lo que se calcula.
# -----------------------------------------------------------------------------
.al_nortest_battery <- function(x) {
    list(
        li  = tryCatch(nortest::lillie.test(x), error = function(e) NULL),
        ad  = tryCatch(nortest::ad.test(x), error = function(e) NULL),
        cvm = tryCatch(nortest::cvm.test(x), error = function(e) NULL),
        sf  = tryCatch(nortest::sf.test(x), error = function(e) NULL),
        pt  = tryCatch(nortest::pearson.test(x), error = function(e) NULL)
    )
}

# -----------------------------------------------------------------------------
# .al_norm_core_battery()
#
# Input: x - a numeric vector already filtered to finite/non-NA values.
# Output: a named list with 4 elements:
#   sw   - the htest object from stats::shapiro.test(x), or NULL if the guard
#          below fails or the call errors;
#   jb   - list(value=, p=) for the manual Jarque-Bera statistic, or NULL;
#   skew - list(value=, p=) for the manual skewness z-test, or NULL;
#   kurt - list(value=, p=) for the manual kurtosis z-test, or NULL.
#
# Guard adopted as the suite-wide standard (Archie's decision, Aug 2026,
# refined after checking the technical basis for each threshold):
#   - Shapiro-Wilk requires 3 <= n <= 5000 (the documented valid range of
#     Royston's algorithm as implemented by stats::shapiro.test - not a
#     choice, a hard constraint of the function itself) AND sd(x) > 0.
#   - Jarque-Bera and the skewness z-test require n >= 8 AND sd(x) > 0.
#   - The kurtosis z-test requires n >= 20 AND sd(x) > 0 - a STRICTER,
#     SEPARATE threshold from skewness/JB. This is not arbitrary: the
#     skewness and kurtosis z-approximations (z = skew/sqrt(6/n),
#     z = (kurt-3)/sqrt(24/n)) converge to standard normal at different
#     rates, and the kurtosis estimator's convergence is documented as
#     markedly slower (Urzua, 1996, discussing why the original
#     Jarque-Bera statistic performs poorly in small/medium samples;
#     the n>8 / n>20 split specifically for the skewness vs. kurtosis
#     z-tests is documented in D'Agostino-style treatments, e.g. Real
#     Statistics Using Excel's exposition of the D'Agostino skewness and
#     kurtosis tests). relatedCheck's original jb_test_approx()/
#     skew_test_approx() (n>=8) and kurt_test_approx() (n>=20) already
#     had this split correct; groupCheck's uniform n>=8 for all three
#     (correct for JB/skewness, too lenient for kurtosis specifically)
#     and anovaCheck/regCheck/pathCheck's complete absence of any sd>0
#     guard are the two things this consolidation corrects, matching
#     everything to relatedCheck's originally-correct design.
#
# ES: Guarda adoptada como estándar de la suite (decisión de Archie,
# agosto 2026, refinada tras verificar la base técnica de cada umbral):
# Shapiro-Wilk exige 3 <= n <= 5000 (rango de validez documentado del
# algoritmo de Royston tal como lo implementa stats::shapiro.test - no es
# una elección, es una restricción dura de la función) Y sd(x) > 0;
# Jarque-Bera y la prueba z de asimetría exigen n >= 8 Y sd(x) > 0; la
# prueba z de curtosis exige n >= 20 Y sd(x) > 0 - un umbral MÁS
# ESTRICTO y SEPARADO del de asimetría, porque la aproximación normal de
# la curtosis converge mucho más lento (Urzúa, 1996; el corte n>8/n>20
# específico para asimetría vs. curtosis está documentado en
# tratamientos estilo D'Agostino, p. ej. Real Statistics Using Excel).
# relatedCheck ya tenía este corte correcto en sus propios closures;
# groupCheck (n>=8 parejo para las tres, correcto para JB/asimetría pero
# laxo para curtosis) y anovaCheck/regCheck/pathCheck (sin ninguna
# guarda de sd>0) son lo que esta consolidación corrige, alineando todo
# al diseño que relatedCheck ya tenía bien.
# -----------------------------------------------------------------------------
.al_norm_core_battery <- function(x) {
    n <- length(x)
    sx <- stats::sd(x)
    valid_sd <- is.finite(sx) && sx > 0

    sw <- NULL
    if (n >= 3 && n <= 5000 && valid_sd)
        sw <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)

    jb <- NULL
    skew <- NULL
    kurt <- NULL

    m <- if (valid_sd) mean(x) else NA_real_

    if (n >= 8 && valid_sd) {
        sk <- mean((x - m) ^ 3) / sx ^ 3
        ku <- mean((x - m) ^ 4) / sx ^ 4

        # Jarque-Bera still uses n>=8 (it inherits skewness's threshold,
        # matching relatedCheck's original jb_test_approx()); only the
        # standalone kurtosis z-test below gets the stricter n>=20 guard.
        # ES: Jarque-Bera sigue con n>=8 (hereda el umbral de asimetría,
        # igual que el jb_test_approx() original de relatedCheck); solo
        # la prueba z de curtosis independiente, abajo, lleva la guarda
        # más estricta de n>=20.
        jb_val <- n / 6 * (sk ^ 2 + ((ku - 3) ^ 2 / 4))
        jb <- list(value = jb_val, p = stats::pchisq(jb_val, df = 2, lower.tail = FALSE))

        z_sk <- sk / sqrt(6 / n)
        skew <- list(value = z_sk, p = 2 * stats::pnorm(abs(z_sk), lower.tail = FALSE))

        if (n >= 20) {
            z_ku <- (ku - 3) / sqrt(24 / n)
            kurt <- list(value = z_ku, p = 2 * stats::pnorm(abs(z_ku), lower.tail = FALSE))
        }
    }

    list(sw = sw, jb = jb, skew = skew, kurt = kurt)
}

# -----------------------------------------------------------------------------
# .al_levene_manual()
#
# Input: value - numeric vector (the raw variable in anovaCheck, or
#   regression residuals in regCheck); group - a factor/vector defining the
#   groups to compare (a real grouping variable in anovaCheck, or a binned
#   "low/medium/high" factor built from fitted values in regCheck) - may be
#   NULL; center - "mean" for classic Levene, "median" for Brown-Forsythe
#   (median-centered Levene).
# Output: list(value=, df=, p=) with the F statistic, numerator df, and
#   p-value from anova(lm(absolute_deviation ~ group)), or NULL if group is
#   NULL, has fewer than 2 levels after removing incomplete cases, or the
#   computation errors.
#
# anovaCheck's lev_test() and regCheck's levene_group_test() ran the exact
# same manual Levene algorithm (deviations from a per-group center, F-test
# via lm+anova, centre="mean" for Levene / "median" for Brown-Forsythe -
# genuinely DIFFERENT computations per call, unlike groupCheck's
# car::leveneTest-based rows, which are a separate, unaudited implementation
# left untouched per the de-duplication mapping). regCheck additionally
# guarded for a NULL or single-level `group` (its groups are bins of fitted
# values, which can legitimately collapse to one bin); anovaCheck's group is
# always a real, pre-validated factor, so this guard is inert there in
# practice - adding it is a defensive no-op for anovaCheck, not a behavior
# change in any realistic scenario, but is flagged here for transparency.
#
# ES: lev_test() de anovaCheck y levene_group_test() de regCheck corrían el
# mismo algoritmo manual de Levene (desviaciones respecto a un centro por
# grupo, prueba F vía lm+anova, centro="mean" para Levene / "median" para
# Brown-Forsythe - cómputos genuinamente DISTINTOS por llamada, a diferencia
# de las filas de groupCheck basadas en car::leveneTest, que son una
# implementación separada y no auditada, dejada intacta según el mapeo de
# duplicación). regCheck además se protegía contra un `group` NULL o de un
# solo nivel (sus grupos son bins de valores ajustados, que pueden colapsar
# a un solo bin legítimamente); el grupo de anovaCheck siempre es un factor
# real ya validado, así que esta guarda es inerte ahí en la práctica -
# agregarla es una protección defensiva sin cambio de comportamiento en
# ningún escenario realista, pero se deja constancia acá por transparencia.
# -----------------------------------------------------------------------------
.al_levene_manual <- function(value, group, center = c("mean", "median")) {
    center <- match.arg(center)

    if (is.null(group))
        return(NULL)

    d <- data.frame(value = value, group = group)
    d <- d[stats::complete.cases(d), , drop = FALSE]

    if (length(unique(d$group)) < 2)
        return(NULL)

    centers <- tapply(
        d$value,
        d$group,
        if (center == "mean") mean else stats::median,
        na.rm = TRUE
    )

    d$absdev <- abs(d$value - centers[d$group])
    fit_lev <- stats::lm(absdev ~ group, data = d)
    tab <- stats::anova(fit_lev)

    list(value = tab$`F value`[1], df = tab$Df[1], p = tab$`Pr(>F)`[1])
}

# -----------------------------------------------------------------------------
# .al_copent_test()
#
# Input: x, y - numeric vectors of equal length; B - number of permutations
#   used to build the null distribution (default 199, matching every
#   calling module's hardcoded default).
# Output: list(ce=, p=) with the observed copula entropy (copent::copent())
#   and its permutation p-value, or NULL if the copent package is
#   unavailable or the observed value cannot be computed.
#
# Verified byte-identical (after whitespace normalization) across
# anovaCheck, logCheck, regCheck, relatedCheck, and pathCheck - a fully
# self-contained function with no dependency on any other module-local
# closure, so this consolidation carries zero behavior-change risk.
# pathCheck's B/seed values come from its own user-facing options
# (self$options$permutations / permutationSeed), set at the call site
# before invoking this function — that call-site configurability was the
# reason pathCheck was first thought to need its own separate
# implementation, but the function body itself was identical all along,
# so pathCheck's local .copentTest() is now a thin wrapper delegating
# here (Aug 2026), preserving its own default B and every call site
# unchanged.
#
# ES: Verificada idéntica byte a byte (tras normalizar espacios) en
# anovaCheck, logCheck, regCheck, relatedCheck y pathCheck - una función
# totalmente autocontenida, sin depender de ningún otro closure local del
# módulo, así que esta consolidación no tiene ningún riesgo de cambiar
# comportamiento. Los valores de B/semilla de pathCheck vienen de sus
# propias opciones visibles al usuario (self$options$permutations /
# permutationSeed), fijadas en el sitio de la llamada antes de invocar
# esta función — esa configurabilidad en el sitio de la llamada fue la
# razón por la que inicialmente se pensó que pathCheck necesitaba su
# propia implementación separada, pero el cuerpo de la función era
# idéntico desde siempre, así que el .copentTest() local de pathCheck
# ahora es un envoltorio delgado que delega acá (agosto 2026),
# preservando su propio valor de B por defecto y sin tocar ningún sitio
# de llamada.
# -----------------------------------------------------------------------------
.al_copent_test <- function(x, y, B = 199) {
    if (!requireNamespace("copent", quietly = TRUE))
        return(NULL)

    x <- as.numeric(x); y <- as.numeric(y)
    obs <- tryCatch(copent::copent(cbind(x, y)), error = function(e) NA_real_)

    if (is.na(obs))
        return(NULL)

    null_vals <- vapply(seq_len(B), function(i) {
        tryCatch(copent::copent(cbind(x, sample(y))), error = function(e) NA_real_)
    }, numeric(1))

    null_vals <- null_vals[is.finite(null_vals)]

    if (length(null_vals) == 0)
        return(list(ce = obs, p = NA_real_))

    pval <- (sum(null_vals >= obs) + 1) / (length(null_vals) + 1)
    list(ce = obs, p = pval)
}

# -----------------------------------------------------------------------------
# .al_dcor_stat(x, y)
#
# Distance correlation (Szekely et al., 2007) between two numeric vectors,
# via the standard double-centered distance-matrix formula. Returns NA_real_
# (not 0) when the statistic is undefined for the pair (e.g. a constant
# variable, zero distance-variance denominator) or when n < 4.
#
# Verified byte-identical (after whitespace/name normalization) across
# anovaCheck, logCheck, regCheck, and relatedCheck - consolidated here per
# Archie's "prioritize shared functions" instruction (Sep 2026). pathCheck
# has its own private$.dCorTest() with a different interface (its own
# B/seed options wired at the call site) and is left untouched.
#
# ES: Correlacion de distancia (Szekely et al., 2007) entre dos vectores
# numericos, con la formula estandar de matrices de distancia doblemente
# centradas. Devuelve NA_real_ (no 0) cuando el estadistico queda
# indefinido para el par (p. ej. una variable constante, denominador de
# varianza de distancia cero) o cuando n < 4.
#
# Verificada identica byte a byte (tras normalizar espacios/nombres) en
# anovaCheck, logCheck, regCheck y relatedCheck - consolidada aqui por la
# instruccion de Archie de "priorizar funciones compartidas" (sep 2026).
# pathCheck tiene su propio private$.dCorTest() con una interfaz distinta
# (sus propias opciones de B/semilla fijadas en el sitio de la llamada) y
# se deja intacto.
# -----------------------------------------------------------------------------
.al_dcor_stat <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    n <- length(x)
    if (n < 4) return(NA_real_)

    ax <- as.matrix(stats::dist(x))
    by <- as.matrix(stats::dist(y))

    a_row <- rowMeans(ax); a_col <- colMeans(ax); a_grand <- mean(ax)
    b_row <- rowMeans(by); b_col <- colMeans(by); b_grand <- mean(by)

    A <- ax - outer(a_row, a_col, "+") + a_grand
    Bm <- by - outer(b_row, b_col, "+") + b_grand

    dcov2 <- mean(A * Bm)
    dvarx2 <- mean(A * A)
    dvary2 <- mean(Bm * Bm)

    denom <- sqrt(dvarx2 * dvary2)
    if (!is.finite(denom) || denom <= 0) return(NA_real_)

    sqrt(max(0, dcov2) / denom)
}

# -----------------------------------------------------------------------------
# .al_dcor_test(x, y, B = 199, seed = 20260704)
#
# Permutation p-value for .al_dcor_stat(), consolidating each module's local
# dcor_pvalue(). Same fixed default B/seed as the 4 modules being
# consolidated (regCheck, logCheck, anovaCheck, relatedCheck all hardcoded
# reps=199/seed=20260704 identically) - preserved here rather than changed,
# per the no-behavior-change rule. pathCheck is untouched (own B/seed from
# user-facing options, wired at its own call sites).
#
# ES: Valor p por permutacion para .al_dcor_stat(), consolidando el
# dcor_pvalue() local de cada modulo. Mismo B/semilla fijos por defecto que
# los 4 modulos consolidados (regCheck, logCheck, anovaCheck, relatedCheck
# tenian reps=199/semilla=20260704 hardcodeados de forma identica) -
# preservados aqui en vez de cambiados, por la regla de no cambiar
# comportamiento. pathCheck queda intacto (su propio B/semilla desde
# opciones de usuario, fijadas en sus propios sitios de llamada).
# -----------------------------------------------------------------------------
.al_dcor_test <- function(x, y, B = 199, seed = 20260704) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 4) return(list(dcor = NA_real_, p = NA_real_))

    obs <- .al_dcor_stat(x, y)
    if (is.na(obs)) return(list(dcor = NA_real_, p = NA_real_))

    set.seed(seed)
    perm <- vapply(seq_len(B), function(k) .al_dcor_stat(x, sample(y)), numeric(1))

    p <- (1 + sum(perm >= obs, na.rm = TRUE)) / (1 + B)
    list(dcor = obs, p = p)
}

# -----------------------------------------------------------------------------
# .al_permutation_note(lang, B, seed)
#
# Bilingual method-transparency sentence for the dCor/CE permutation tests:
# states B, seed, and the minimum achievable p at that resolution (floor =
# 1/(B+1)), so a p printed at the floor is read as "p <= floor" (Monte
# Carlo), not as an exact value. Added per Archie's request (Sep 2026) to
# extend to relatedCheck/logCheck/anovaCheck/regCheck the transparency
# pathCheck already had. pathCheck keeps its own inline version untouched,
# since its B/seed are user-configurable and its sentence additionally
# invites raising B for a final report - not applicable to these 4
# modules, where B/seed are fixed and not exposed as options.
#
# ES: Oracion bilingue de transparencia metodologica para las pruebas de
# permutacion de dCor/CE: indica B, semilla y el p minimo alcanzable a esa
# resolucion (piso = 1/(B+1)), para que un p impreso en el piso se lea
# como "p <= piso" (Monte Carlo), no como un valor exacto. Agregada por
# pedido de Archie (sep 2026) para extender a relatedCheck/logCheck/
# anovaCheck/regCheck la transparencia que pathCheck ya tenia. pathCheck
# conserva su propia version inline sin tocar, ya que su B/semilla son
# configurables por el usuario y su oracion invita ademas a subir B para
# un informe final - no aplica a estos 4 modulos, donde B/semilla son
# fijos y no estan expuestos como opciones.
# -----------------------------------------------------------------------------
.al_permutation_note <- function(lang, B, seed) {
    floor_p <- 1 / (B + 1)
    floor_p_str <- sub("^0", "", sprintf("%.3f", floor_p))
    .al_tr(
        lang,
        sprintf("Method: dCor and CE p-values are obtained by permutation (B = %d permutations, seed = %d). The minimum achievable p at this resolution is %s; a p equal to this value should be read as p \u2264 %s (Monte Carlo), not as an exact value.",
                B, seed, floor_p_str, floor_p_str),
        sprintf("M\u00e9todo: los valores p de dCor y CE se obtienen por permutaci\u00f3n (B = %d permutaciones, semilla = %d). El p m\u00ednimo alcanzable con esta resoluci\u00f3n es %s; un p igual a este valor debe leerse como p \u2264 %s (Monte Carlo), no como un valor exacto.",
                B, seed, floor_p_str, floor_p_str)
    )
}

# -----------------------------------------------------------------------------
# .al_dcor_na_note(lang)
#
# Bilingual clarifying sentence for correlation-matrix notes: explains that a
# blank/NA dCor cell means the statistic was undefined for that pair (zero
# variance in one variable, or too few complete cases), not that dependence
# was measured as zero. Added per Archie's decision, Aug 2026, alongside
# unifying dcor_stat()'s invalid-denominator return value to NA_real_
# (previously 0 in anovaCheck/regCheck/pathCheck) to make that convention
# explicit to the reader wherever the correlation matrices appear.
#
# ES: Oración bilingüe aclaratoria para las notas de la matriz de
# correlación: explica que una celda vacía/NA en dCor significa que el
# estadístico quedó indefinido para ese par (varianza cero en alguna
# variable, o muy pocos casos completos), no que la dependencia se midió
# como cero. Agregada por decisión de Archie, agosto 2026, junto con
# unificar el valor de retorno de dcor_stat() ante un denominador inválido
# a NA_real_ (antes 0 en anovaCheck/regCheck/pathCheck), para dejarle
# explícita esa convención al lector donde sea que aparezcan las matrices.
# -----------------------------------------------------------------------------
.al_dcor_na_note <- function(lang) {
    .al_tr(
        lang,
        "A blank dCor cell means the statistic could not be computed for that pair (e.g. one of the variables has zero variance, or there are too few complete cases) - it does not imply zero dependence.",
        "Una celda vac\u00eda en dCor significa que el estad\u00edstico no pudo calcularse para ese par (p. ej. una de las variables tiene varianza cero, o hay muy pocos casos completos) - no implica dependencia cero."
    )
}

# -----------------------------------------------------------------------------
# .al_p_sig()
#
# Input: p - a p-value (any length/type accepted by clean_num: coerced to
#   numeric, first element taken, NA/NaN/Inf all normalized to NA_real_).
# Output: "***" / "**" / "*" / "" per the standard .001/.01/.05 thresholds.
#
# Verified identical logic in anovaCheck, logCheck, regCheck, and
# relatedCheck (all wrap p through clean_num() first). groupCheck had its
# own version that skipped clean_num() and checked only is.na()/is.nan()
# directly - functionally equivalent for any real p-value (a p-value is
# never Inf or a multi-element vector in practice), so adopting the
# clean_num()-wrapped version everywhere, including groupCheck, is a
# defensive no-op there and a real consolidation for the other four.
#
# ES: Lógica idéntica verificada en anovaCheck, logCheck, regCheck y
# relatedCheck (los cuatro envuelven p con clean_num() primero). groupCheck
# tenía su propia versión que se saltaba clean_num() y solo chequeaba
# is.na()/is.nan() directamente - funcionalmente equivalente para cualquier
# p real (un p-value nunca es Inf ni un vector de varios elementos en la
# práctica), así que adoptar la versión con clean_num() en todos lados,
# incluido groupCheck, es una protección defensiva sin efecto ahí y una
# consolidación real para los otros cuatro.
# -----------------------------------------------------------------------------
.al_p_sig <- function(p) {
    p <- .al_clean_num(p)

    if (is.na(p))
        return("")
    if (p < .001)
        return("***")
    if (p < .01)
        return("**")
    if (p < .05)
        return("*")
    ""
}

# -----------------------------------------------------------------------------
# .al_clean_num()
#
# Shared implementation of the clean_num() helper every module already
# defined identically (length-0 or non-finite -> NA_real_, otherwise the
# first element coerced to numeric). Extracted here only so .al_p_sig() has
# something to call; each module's own local clean_num() is untouched and
# keeps being used everywhere else it already was.
#
# ES: Implementación compartida del ayudante clean_num() que cada módulo ya
# definía idéntico (longitud 0 o no finito -> NA_real_, si no, el primer
# elemento convertido a numérico). Se extrae acá solo para que .al_p_sig()
# tenga qué llamar; el clean_num() local de cada módulo queda intacto y
# sigue usándose en todo lo demás donde ya se usaba.
# -----------------------------------------------------------------------------
.al_clean_num <- function(x) {
    if (length(x) == 0)
        return(NA_real_)

    x <- suppressWarnings(as.numeric(x[1]))

    if (is.na(x) || is.nan(x) || is.infinite(x))
        return(NA_real_)

    x
}

# -----------------------------------------------------------------------------
# .al_fmt_r() / .al_apa_cell()
#
# .al_fmt_r(r, digits): formats a correlation coefficient APA-style (fixed
#   decimals, leading zero dropped - ".394" not "0.394"), or "" if r is NA.
# .al_apa_cell(r, p, digits): .al_fmt_r(r) with a significance-stars suffix
#   from .al_p_sig(p) appended, or "" if r is NA.
#
# Verified byte-identical across anovaCheck, logCheck, regCheck, and
# relatedCheck's correlation-matrix builders. pathCheck has its own
# .fmtR()/.apaCell() using p_sig_stars() (no clean_num wrapping) - left
# untouched, matching the decision to keep pathCheck's whole
# correlation-matrix engine (configurable permutations/seed) separate.
#
# ES: Verificadas idénticas byte a byte en los constructores de matriz de
# correlación de anovaCheck, logCheck, regCheck y relatedCheck. pathCheck
# tiene su propio .fmtR()/.apaCell() con p_sig_stars() (sin pasar por
# clean_num) - se deja intacto, siguiendo la decisión de mantener aparte
# todo el motor de matriz de correlación de pathCheck (permutaciones/
# semilla configurables).
# -----------------------------------------------------------------------------
.al_fmt_r <- function(r, digits = 3) {
    if (is.na(r))
        return("")

    s <- sprintf(paste0("%.", digits, "f"), r)
    sub("^(-?)0\\.", "\\1.", s)
}

.al_apa_cell <- function(r, p, digits = 3) {
    if (is.na(r))
        return("")

    paste0(.al_fmt_r(r, digits), .al_p_sig(p))
}

# -----------------------------------------------------------------------------
# .al_plot_palette_base() / .al_plot_series_palette()
#
# .al_plot_palette_base(style): returns list(point=, line=, ref=, alert=,
#   fill=, smooth=) for style "bw" / "contrast" / "fullColor" / anything else
#   ("clean", the default). Every calling module already had this exact
#   4-branch structure; the "bw", "contrast", and default branches were
#   byte-identical everywhere. The "fullColor" branch had two competing hex
#   sets in production for ref/alert/smooth:
#     Variant A - ref #6B7280, alert #F28E2B, smooth #1D91C0 (anovaCheck,
#       regCheck, timeCheck)
#     Variant B - ref #7A7A7A, alert #D95F0E, smooth #2C7FB8 (groupCheck,
#       logCheck)
#   Archie chose Variant A as the suite-wide standard (Aug 2026): it's the
#   one already used by 2 of the 3 modules that also have the "series"
#   multi-color palette below (regCheck, timeCheck), vs. only 1 (logCheck)
#   for Variant B. This changes the fullColor preset's look in groupCheck
#   and logCheck; bw/contrast/clean are unaffected everywhere.
#
# .al_plot_series_palette(choice): returns a multi-color vector for
#   "viridis" / "greyscale" / "colorblind" / anything else (the default
#   blue-orange set). Verified byte-identical in logCheck, regCheck, and
#   timeCheck - the 3 modules that originally needed a categorical
#   multi-series palette (multi-class ROC curves, multiple time series).
#   anovaCheck was added Aug 2026 when its group boxplots gained a
#   color-by-group option (plotPalette); groupCheck's plots remain
#   single-series diagnostics (Q-Q, residuals) with no categorical
#   dimension to color, so it still doesn't call this. pathCheck's
#   palette (node/edge colors for path diagrams) is a structurally
#   different thing entirely and was never part of this consolidation.
#
# ES: .al_plot_palette_base(style): mismos 4 casos que ya tenía cada
# módulo; "bw"/"contrast"/por defecto eran idénticos en todos lados.
# "fullColor" tenía dos combinaciones de color compitiendo en producción
# para ref/alert/smooth (Variante A y B, ver arriba). Archie eligió la
# Variante A como estándar de toda la suite (agosto 2026): es la que ya
# usaban 2 de los 3 módulos que también tienen la paleta multicolor
# "series" de abajo (regCheck, timeCheck), contra solo 1 (logCheck) para
# la Variante B. Esto cambia el aspecto del preset fullColor en groupCheck
# y logCheck; bw/contrast/clean quedan iguales en todos lados.
#
# .al_plot_series_palette(choice): paleta multicolor para gráficos con
# varias categorías (curvas ROC por clase, varias series temporales).
# Verificada idéntica en logCheck, regCheck y timeCheck - los 3 módulos
# que originalmente la necesitaban. anovaCheck se sumó en agosto 2026
# cuando sus boxplots por grupo ganaron la opción de colorear por grupo
# (plotPalette); los gráficos de groupCheck siguen siendo diagnósticos de
# una sola serie (Q-Q, residuos) sin dimensión categórica que colorear,
# así que sigue sin llamarla. La paleta de pathCheck (colores de
# nodos/aristas para diagramas de ruta) es una cosa estructuralmente
# distinta y nunca formó parte de esta consolidación.
# -----------------------------------------------------------------------------
.al_plot_palette_base <- function(style) {
    if (identical(style, "bw")) {
        return(list(
            point = "gray25", line = "gray10", ref = "gray35",
            alert = "gray10", fill = "gray70", smooth = "gray10",
            grid = "#D9D9D9"
        ))
    }

    if (identical(style, "contrast")) {
        return(list(
            point = "#222222", line = "#000000", ref = "#444444",
            alert = "#000000", fill = "#BDBDBD", smooth = "#000000",
            grid = "#BDBDBD"
        ))
    }

    if (identical(style, "fullColor")) {
        return(list(
            point = "#2C7FB8", line = "#253494", ref = "#6B7280",
            alert = "#F28E2B", fill = "#A6CEE3", smooth = "#1D91C0",
            grid = "#D9EAF7"
        ))
    }

    list(
        point = "#4D4D4D", line = "#2B2B2B", ref = "#7A7A7A",
        alert = "#555555", fill = "#BDBDBD", smooth = "#2C7FB8",
        grid = "#E0E0E0"
    )
}

.al_plot_series_palette <- function(choice) {
    if (identical(choice, "viridis")) {
        # Real stops sampled from the viridis colormap (no package
        # dependency needed). A requireNamespace("viridisLite") branch
        # used to sit here with this same 6-stop vector as its fallback;
        # that made the "viridis" option render differently depending on
        # whether viridisLite happened to be installed on the machine
        # running jamovi. Dropping the conditional and always using this
        # vector keeps the result deterministic everywhere, at no visual
        # cost (it already was the fallback).
        # ES: Puntos reales muestreados del colormap viridis (sin
        # dependencia de paquete). Antes había una rama
        # requireNamespace("viridisLite") con este mismo vector de 6
        # puntos como respaldo; eso hacía que la opción "viridis" se
        # viera distinto según si viridisLite estaba instalado en la
        # máquina donde corre jamovi. Quitar la condición y usar siempre
        # este vector mantiene el resultado determinista en cualquier
        # entorno, sin costo visual (ya era el valor de respaldo).
        return(c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"))
    }

    if (identical(choice, "greyscale"))
        return(c("gray10", "gray30", "gray50", "gray65", "gray80"))

    if (identical(choice, "colorblind"))
        return(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

    c("#2C7FB8", "#F28E2B", "#1B9E77", "#9467BD", "#E15759")
}

# -----------------------------------------------------------------------------
# .al_influence_diagnostics()
#
# Input: n - number of complete cases; p_model - true number of estimated
#   model parameters (length(coef(fit)), NOT the raw count of selected
#   predictor variables - see the leverage-threshold bug family noted
#   throughout this project's de-duplication mapping); residuals_stud,
#   leverage, cooks_d, dffits - numeric vectors of length n from the fitted
#   model; top_n - how many top-ranked cases to keep (default 20).
# Output: list(cook_cut=, lev_cut=, dffits_cut=, stud_cut=, criteria=
#   [character vector, one "triggered criteria" string per case, "" if
#   none], problem= [TRUE if any case triggered any criterion], ord=
#   [indices of the top_n cases by composite score, for the table loop]).
#
# Verified byte-identical (after normalizing brace style) between anovaCheck
# and regCheck: same 4 thresholds (|studentized residual|>3, leverage>2p/n,
# Cook's D>4/n, |DFFITS|>2*sqrt(p/n)), same composite ranking (cooks_d +
# leverage + |stud.resid|/10), same top-20 cap. logCheck (simpler 2-criteria
# OR-flag, no cap), pathCheck (combines Mahalanobis D² + leverage + Cook's
# D from the largest-predictor equation, ordered by Mahalanobis), and
# groupCheck (Cook's D + univariate Mahalanobis z², no leverage/DFFITS at
# all) are each a deliberately different design for their own model type
# and are intentionally NOT included here - see the de-duplication mapping
# notes for why groupCheck's and pathCheck's "Mahalanobis" must never be
# merged despite the shared name (different statistics entirely).
#
# ES: Verificada idéntica byte a byte (normalizando el estilo de llaves)
# entre anovaCheck y regCheck: mismos 4 umbrales, mismo ranking compuesto,
# mismo tope de 20. logCheck (2 criterios OR, sin tope), pathCheck (combina
# Mahalanobis D² + leverage + Cook's D de la ecuación con más predictores,
# ordenado por Mahalanobis) y groupCheck (Cook's D + Mahalanobis
# univariado z², sin leverage/DFFITS) son cada uno un diseño
# deliberadamente distinto para su propio tipo de modelo y quedan
# intencionalmente FUERA de acá - ver las notas del mapeo de duplicación
# sobre por qué el "Mahalanobis" de groupCheck y pathCheck nunca deben
# fusionarse pese al nombre compartido (son estadísticos distintos).
# -----------------------------------------------------------------------------
.al_influence_diagnostics <- function(n, p_model, residuals_stud, leverage, cooks_d, dffits, top_n = 20) {
    cook_cut <- 4 / n
    lev_cut <- 2 * p_model / n
    dffits_cut <- 2 * sqrt(p_model / n)
    stud_cut <- 3

    criteria <- character(n)

    for (i in seq_len(n)) {
        f <- c()

        if (!is.na(.al_clean_num(residuals_stud[i])) &&
            abs(residuals_stud[i]) > stud_cut)
            f <- c(f, paste0("|res. stud.| > ", round(stud_cut, 3)))

        if (!is.na(.al_clean_num(leverage[i])) &&
            leverage[i] > lev_cut)
            f <- c(f, paste0("leverage > ", round(lev_cut, 4)))

        if (!is.na(.al_clean_num(cooks_d[i])) &&
            cooks_d[i] > cook_cut)
            f <- c(f, paste0("Cook's D > ", round(cook_cut, 4)))

        if (!is.na(.al_clean_num(dffits[i])) &&
            abs(dffits[i]) > dffits_cut)
            f <- c(f, paste0("|DFFITS| > ", round(dffits_cut, 4)))

        criteria[i] <- paste(f, collapse = "; ")
    }

    problem <- any(nzchar(criteria), na.rm = TRUE)

    ord_score <- rep(0, n)
    ord_score <- ord_score + ifelse(is.na(cooks_d), 0, cooks_d)
    ord_score <- ord_score + ifelse(is.na(leverage), 0, leverage)
    ord_score <- ord_score + ifelse(is.na(residuals_stud), 0, abs(residuals_stud) / 10)

    ord <- order(ord_score, decreasing = TRUE, na.last = TRUE)
    ord <- ord[seq_len(min(top_n, length(ord)))]

    list(
        cook_cut = cook_cut,
        lev_cut = lev_cut,
        dffits_cut = dffits_cut,
        stud_cut = stud_cut,
        criteria = criteria,
        problem = problem,
        ord = ord
    )
}
