# -----------------------------------------------------------------------------
# Report language follows jamovi's language, with no cache between runs.
# ES: El idioma del informe sigue al de jamovi, sin caché entre ejecuciones.
#
# Each analysis is run in Spanish, then English, then Spanish again. Every
# text key used by the report must resolve (no "[missing text" marker), the
# English and Spanish reports must differ, and the second Spanish run must
# match the first: a translator cached from an earlier run would break that.
# ES: Cada análisis se ejecuta en español, luego en inglés y otra vez en
# español. Toda clave de texto debe resolverse, los informes en inglés y en
# español deben diferir y la segunda ejecución en español debe coincidir con
# la primera; un traductor guardado de una ejecución anterior lo rompería.
# -----------------------------------------------------------------------------

set.seed(20261118)
lang_data <- data.frame(
    y = rnorm(60),
    x1 = rnorm(60),
    x2 = rnorm(60),
    g = factor(rep(c("a", "b", "c"), each = 20))
)

check_language_cycle <- function(fun, ...) {
    es1 <- runAnalysisIn("es", fun, ...)
    en <- runAnalysisIn("en", fun, ...)
    es2 <- runAnalysisIn("es", fun, ...)
    for (txt in c(es1, en, es2))
        expect_false(grepl("[missing text", txt, fixed = TRUE))
    expect_false(identical(es1, en))
    expect_identical(es1, es2)
    invisible(list(es = es1, en = en))
}

test_that("regCheck report follows the language, with no cache", {
    out <- check_language_cycle(AssumptionsLab::regCheck,
        data = lang_data, dep = "y", covs = c("x1", "x2"))
    expect_false(grepl("Report Language", out$en, fixed = TRUE))
})

test_that("anovaCheck report follows the language, with no cache", {
    check_language_cycle(AssumptionsLab::anovaCheck,
        data = lang_data, dep = "y", factors = "g")
})

test_that("groupCheck report follows the language, with no cache", {
    sub <- droplevels(lang_data[lang_data$g != "c", ])
    check_language_cycle(AssumptionsLab::groupCheck,
        data = sub, dep = "y", group = "g")
})

test_that("countCheck report follows the language, with no cache", {
    set.seed(20261123)
    count_data <- data.frame(y = rpois(60, 2), x1 = rnorm(60),
        g = factor(rep(c("a", "b", "c"), each = 20)))
    out <- check_language_cycle(AssumptionsLab::countCheck,
        data = count_data, dep = "y", covs = "x1", factors = "g")
    expect_true(grepl("Sobredispersión", out$es, fixed = TRUE))
})
