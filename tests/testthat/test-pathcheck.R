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
# pathCheck edge-case tests.
# ES: Pruebas de casos límite de pathCheck.
#
# pathCheck's option shape differs from the regression-family modules: `vars`
# is a plain Variables list, but `relations` is a Pairs option - a list of
# list(i1 = <predictor>, i2 = <dependent>) entries, built into an internal
# edge list by private$.buildEdges(). Full estimation (stats::lm() per
# endogenous variable) only runs once isFinalized = TRUE AND at least one
# dependent variable is implied by relations; an incomplete/invalid
# specification (fewer than two variables, a self-loop, a cycle) is instead
# reported through the always-live validationSummary table rather than by
# short-circuiting with an intro message the way the regression-family
# modules do. These tests confirm a malformed or degenerate specification
# never reaches an uncaught R error, and that a constant endogenous
# (dependent) variable - whose zero standard deviation feeds directly into a
# beta = estimate * (sdX / sdY) standardization - does not produce anything
# worse than a non-finite (Inf/NaN) coefficient.
#
# ES: La forma de las opciones de pathCheck difiere de los módulos de la
# familia de regresión: `vars` es una lista Variables simple, pero
# `relations` es una opción Pairs - una lista de entradas list(i1 =
# <predictor>, i2 = <dependiente>), convertida en una lista interna de
# aristas por private$.buildEdges(). La estimación completa (stats::lm() por
# cada variable endógena) solo corre una vez que isFinalized = TRUE Y
# relations implica al menos una variable dependiente; una especificación
# incompleta/inválida (menos de dos variables, un autolazo, un ciclo) se
# reporta en cambio mediante la tabla validationSummary, siempre activa, en
# lugar de salir anticipadamente con un mensaje de introducción como hacen
# los módulos de la familia de regresión. Estas pruebas confirman que una
# especificación mal formada o degenerada nunca llega a un error no
# controlado de R, y que una variable endógena (dependiente) constante -
# cuya desviación estándar de cero alimenta directamente una estandarización
# beta = estimación * (sdX / sdY) - no produce nada peor que un coeficiente
# no finito (Inf/NaN).
# -----------------------------------------------------------------------------

test_that("pathCheck tolerates accented and symbol variable names", {

    data <- edgeSpecialNameData()

    expect_no_error(
        AssumptionsLab::pathCheck(
            data        = data,
            vars        = c("niño_años", "café ± más"),
            relations   = list(list(i1 = "café ± más", i2 = "niño_años")),
            isFinalized = TRUE
        )
    )
})

test_that("pathCheck tolerates a single-row data set", {

    data <- edgeSingleRowData()

    expect_no_error(
        AssumptionsLab::pathCheck(
            data        = data,
            vars        = c("cov1", "dep"),
            relations   = list(list(i1 = "cov1", i2 = "dep")),
            isFinalized = TRUE
        )
    )
})

test_that("pathCheck tolerates a dependent (endogenous) variable that is entirely NA", {

    data <- edgeAllNaData("dep", n = 30)

    expect_no_error(
        AssumptionsLab::pathCheck(
            data        = data,
            vars        = c("cov1", "dep"),
            relations   = list(list(i1 = "cov1", i2 = "dep")),
            isFinalized = TRUE
        )
    )
})

test_that("pathCheck tolerates a zero-variance exogenous predictor", {

    data <- edgeConstantData("cov1", n = 30)

    expect_no_error(
        AssumptionsLab::pathCheck(
            data        = data,
            vars        = c("cov1", "dep"),
            relations   = list(list(i1 = "cov1", i2 = "dep")),
            isFinalized = TRUE
        )
    )
})

test_that("pathCheck tolerates a zero-variance endogenous (dependent) variable", {

    data <- edgeConstantData("dep", n = 30)

    expect_no_error(
        AssumptionsLab::pathCheck(
            data        = data,
            vars        = c("cov1", "dep"),
            relations   = list(list(i1 = "cov1", i2 = "dep")),
            isFinalized = TRUE
        )
    )
})

test_that("pathCheck tolerates an under-specified model (fewer than two variables, no relations)", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::pathCheck(
            data        = data,
            vars        = "dep",
            relations   = list(),
            isFinalized = TRUE
        )
    )
})

test_that("pathCheck tolerates a self-loop relation", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::pathCheck(
            data        = data,
            vars        = c("dep", "cov1"),
            relations   = list(list(i1 = "dep", i2 = "dep")),
            isFinalized = TRUE
        )
    )
})

test_that("pathCheck tolerates a cyclic relation", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::pathCheck(
            data        = data,
            vars        = c("dep", "cov1"),
            relations   = list(
                list(i1 = "dep", i2 = "cov1"),
                list(i1 = "cov1", i2 = "dep")
            ),
            isFinalized = TRUE
        )
    )
})
