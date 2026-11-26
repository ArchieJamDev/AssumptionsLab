# -----------------------------------------------------------------------------
# Assumption Library: native Text and Table elements.
# ES: Biblioteca de Supuestos: elementos nativos Text y Table.
#
# The prose of each category is a Text element that carries the category's
# `refs:`, and each comparison table is a native Table filled in .init().
# These tests check that every element is filled, follows the category
# filter and jamovi's language, that no HTML markup is left, and that every
# `refs:` key of the library exists in jamovi/00refs.yaml.
# ES: La prosa de cada categoría es un elemento Text con los `refs:` de la
# categoría y cada tabla comparativa es una Table nativa llenada en .init().
# Estas pruebas revisan que todo esté lleno, siga el filtro de categoría y
# el idioma de jamovi, que no quede marcado HTML y que cada clave `refs:`
# exista en jamovi/00refs.yaml.
# -----------------------------------------------------------------------------

# The library uses no variables, so jmvcore::select() keeps no columns; a
# data frame with rows would then fail on its row names. An empty data
# frame is what the library actually needs.
# ES: La biblioteca no usa variables; un data frame vacío basta.
library_data <- data.frame()

library_tables <- c(
    "normalityTable", "homoscedasticityGroupTable", "homoscedasticityRegTable",
    "linearityTable", "independenceTable", "multicollinearityTable",
    "influenceTable", "sphericityTable", "proportionalOddsTable",
    "independenceIrrelevantAlternativesTable", "countModelsTable",
    "robustTable")

library_texts <- c(
    "intro", "normality", "normalityGuide2", "homoscedasticity",
    "homoscedasticityGuide2", "homoscedasticityGuide3", "linearity",
    "linearityGuide2", "independence", "independenceGuide2",
    "multicollinearity", "multicollinearityGuide2", "influence",
    "influenceGuide2", "sphericity", "sphericityGuide2", "proportionalOdds",
    "proportionalOddsGuide2", "independenceIrrelevantAlternatives",
    "independenceIrrelevantAlternativesGuide2", "countModels",
    "countModelsGuide2", "robust", "robustGuide2", "notes")

test_that("every comparison table is a filled native Table", {
    res <- AssumptionsLab::assumptionLibrary(data = library_data)
    for (name in library_tables) {
        table <- res[[name]]
        expect_true(inherits(table, "Table"), info = name)
        expect_true(table$rowCount > 0, info = name)
        expect_true(table$visible, info = name)
        df <- as.data.frame(table)
        expect_false(any(grepl("<[a-z]", unlist(df))), info = name)
    }
})

test_that("every prose element is filled, with no HTML markup left", {
    res <- AssumptionsLab::assumptionLibrary(data = library_data)
    for (name in library_texts) {
        content <- res[[name]]$content
        expect_true(nzchar(content), info = name)
        expect_false(grepl("<(div|p|b|table|td|span)[ >]", content), info = name)
        expect_false(grepl("&(amp|lt|gt);", content), info = name)
    }
    expect_true(grepl("**Core principle:**", res$intro$content, fixed = TRUE))
    expect_true(grepl("\n- **Regression:**", res$normality$content, fixed = TRUE))
})

test_that("the library follows the category filter", {
    res <- AssumptionsLab::assumptionLibrary(data = library_data,
        category = "normality")
    expect_true(res$normalityTable$visible)
    expect_true(res$normalityGuide2$visible)
    expect_true(res$intro$visible)
    expect_false(res$robustTable$visible)
    expect_false(res$countModelsGuide2$visible)
    expect_false(res$linearity$visible)
})

test_that("the library follows jamovi's language", {
    es <- runAnalysisIn("es", AssumptionsLab::assumptionLibrary,
        data = library_data, category = "normality")
    en <- runAnalysisIn("en", AssumptionsLab::assumptionLibrary,
        data = library_data, category = "normality")
    expect_true(grepl("General normality, the battery's primary test", en, fixed = TRUE))
    expect_true(grepl("Normalidad general", es, fixed = TRUE))
    expect_true(grepl("Shapiro & Wilk, 1965", es, fixed = TRUE))
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
    expect_true(length(keys) >= 11)
    expect_false(any(grepl("Sources", lines, fixed = TRUE)))

    defined <- sub(":\\s*$", "",
        trimws(grep("^    [A-Za-z0-9_]+:\\s*$", readLines(refs, encoding = "UTF-8"),
            value = TRUE)))
    expect_length(setdiff(keys, defined), 0)
})
