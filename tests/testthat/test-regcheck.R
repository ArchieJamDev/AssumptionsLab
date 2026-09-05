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
# regCheck edge-case tests.
# ES: Pruebas de casos límite de regCheck.
#
# regCheck fits an OLS stats::lm() for a numeric dependent variable against
# one or more numeric/categorical predictors, and already guards on a
# missing dependent variable, zero selected predictors (covs and factors
# both empty), and fewer than five complete cases before the model is ever
# fit. These tests confirm those guards still fire, and that a constant
# predictor (a classic multicollinearity/rank-deficiency trigger) does not
# break model fitting or the diagnostics computed from it.
#
# ES: regCheck ajusta un stats::lm() de mínimos cuadrados ordinarios para
# una variable dependiente numérica contra uno o más predictores
# numéricos/categóricos, y ya protege contra la ausencia de variable
# dependiente, cero predictores seleccionados (covs y factors vacíos) y
# menos de cinco casos completos antes de ajustar el modelo. Estas pruebas
# confirman que esas protecciones siguen activándose, y que un predictor
# constante (un disparador clásico de multicolinealidad/deficiencia de
# rango) no rompe el ajuste del modelo ni los diagnósticos calculados a
# partir de él.
# -----------------------------------------------------------------------------

test_that("regCheck tolerates accented and symbol variable names", {

    data <- edgeSpecialNameData()

    expect_no_error(
        AssumptionsLab::regCheck(
            data = data,
            dep  = "niño_años",
            covs = "café ± más"
        )
    )
})

test_that("regCheck shows its own guidance for a single-row data set", {

    data <- edgeSingleRowData()

    expect_no_error(
        AssumptionsLab::regCheck(
            data = data,
            dep  = "dep",
            covs = "cov1"
        )
    )
})

test_that("regCheck shows its own guidance when the dependent variable is entirely NA", {

    data <- edgeAllNaData("dep", n = 30)

    expect_no_error(
        AssumptionsLab::regCheck(
            data = data,
            dep  = "dep",
            covs = "cov1"
        )
    )
})

test_that("regCheck tolerates a zero-variance numeric predictor", {

    data <- edgeConstantData("cov1", n = 30)

    expect_no_error(
        AssumptionsLab::regCheck(
            data = data,
            dep  = "dep",
            covs = c("cov1", "cov2")
        )
    )
})

test_that("regCheck tolerates a categorical predictor with a single level", {

    data <- edgeBaseData(n = 30)
    data$group3 <- edgeLevelFactor(nrow(data), 1)

    expect_no_error(
        AssumptionsLab::regCheck(
            data    = data,
            dep     = "dep",
            factors = "group3"
        )
    )
})

test_that("regCheck shows its own guidance when no predictor is selected", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::regCheck(
            data = data,
            dep  = "dep"
        )
    )
})

test_that("regCheck's suggested decision reacts to small-sample non-normal residuals", {

    # normality_problem is computed from the residual Shapiro-Wilk test but,
    # before this fix, was never consulted when building the "Suggested
    # decision" recommendation - a small-n, skewed-residual fit like this one
    # used to fall through to the generic "no strong signals" text even
    # though the residuals were clearly non-normal. Seed chosen so that only
    # normality is flagged (no heteroscedasticity, autocorrelation, severe
    # collinearity, or influence).
    set.seed(1)
    n <- 20
    x <- rnorm(n)
    e <- rchisq(n, df = 2) - 2
    data <- data.frame(y = 2 * x + e, x = x)

    res <- AssumptionsLab::regCheck(data = data, dep = "y", covs = "x")
    notes_html <- res$notes$content

    expect_match(notes_html, "bootstrap", ignore.case = TRUE)
    expect_match(notes_html, "Central Limit Theorem", fixed = TRUE)
})
