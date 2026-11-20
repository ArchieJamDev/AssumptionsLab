# -----------------------------------------------------------------------------
# Assumption Library: native jamovi citations.
# ES: Biblioteca de Supuestos: citas nativas de jamovi.
#
# Each category ends with a Sources table that carries the category's
# `refs:`. These tests check that every Sources table is filled, follows
# the category filter, is translated, and that every `refs:` key of the
# library exists in jamovi/00refs.yaml.
# ES: Cada categoría termina con una tabla Fuentes que lleva los `refs:` de
# la categoría. Estas pruebas revisan que cada tabla Fuentes tenga filas,
# siga el filtro de categoría y se traduzca, y que cada clave `refs:` de la
# biblioteca exista en jamovi/00refs.yaml.
# -----------------------------------------------------------------------------

# The library uses no variables, so jmvcore::select() keeps no columns; a
# data frame with rows would then fail on its row names. An empty data
# frame is what the library actually needs.
# ES: La biblioteca no usa variables; un data frame vacío basta.
library_data <- data.frame()

library_sources <- c(
    "normalitySources", "homoscedasticitySources", "linearitySources",
    "independenceSources", "multicollinearitySources", "influenceSources",
    "sphericitySources", "proportionalOddsSources",
    "independenceIrrelevantAlternativesSources", "robustSources")

test_that("every library category has a filled Sources table", {
    res <- AssumptionsLab::assumptionLibrary(data = library_data)
    for (name in library_sources) {
        table <- res[[name]]
        expect_true(table$rowCount > 0, info = name)
        expect_true(table$visible, info = name)
    }
})

test_that("Sources tables follow the category filter", {
    res <- AssumptionsLab::assumptionLibrary(data = library_data,
        category = "normality")
    expect_true(res$normalitySources$visible)
    expect_false(res$robustSources$visible)
    expect_false(res$linearitySources$visible)
})

test_that("Sources tables follow jamovi's language", {
    es <- runAnalysisIn("es", AssumptionsLab::assumptionLibrary,
        data = library_data, category = "normality")
    en <- runAnalysisIn("en", AssumptionsLab::assumptionLibrary,
        data = library_data, category = "normality")
    expect_true(grepl("Power of Shapiro-Wilk", en, fixed = TRUE))
    expect_true(grepl("Potencia de Shapiro-Wilk", es, fixed = TRUE))
    expect_true(grepl("Shapiro & Wilk (1965)", es, fixed = TRUE))
})

test_that("every refs: key of the library exists in 00refs.yaml", {
    ryaml <- testthat::test_path("..", "..", "jamovi", "assumptionlibrary.r.yaml")
    refs <- testthat::test_path("..", "..", "jamovi", "00refs.yaml")
    skip_if_not(file.exists(ryaml) && file.exists(refs))

    lines <- readLines(ryaml, encoding = "UTF-8")
    keys <- character(0)
    inRefs <- FALSE
    for (line in lines) {
        if (grepl("^    refs:\\s*$", line)) {
            inRefs <- TRUE
        } else if (inRefs && grepl("^      - ", line)) {
            keys <- c(keys, trimws(sub("^      - ", "", line)))
        } else {
            inRefs <- FALSE
        }
    }
    expect_true(length(keys) >= length(library_sources))

    defined <- sub(":\\s*$", "",
        trimws(grep("^    [A-Za-z0-9_]+:\\s*$", readLines(refs, encoding = "UTF-8"),
            value = TRUE)))
    expect_length(setdiff(keys, defined), 0)
})
