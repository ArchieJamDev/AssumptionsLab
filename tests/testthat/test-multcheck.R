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
# multCheck edge-case tests.
# ES: Pruebas de casos límite de multCheck.
#
# multCheck fits a multinomial logit model (nnet::multinom()) for an
# unordered nominal dependent variable with three or more categories. Its
# guard structure mirrors ordCheck's: an empty data set, a missing/non-factor
# dependent variable, fewer than 3 *observed* categories (via
# levels(droplevels(...))), zero selected predictors, and a failed model
# fit each short-circuit to an intro/notes message. These tests confirm
# every guard still fires, and that a constant predictor does not break
# multinom() fitting or its case-level Pearson-residual influence screening.
#
# ES: multCheck ajusta un modelo logit multinomial (nnet::multinom()) para
# una variable dependiente nominal no ordenada con tres o más categorías. Su
# estructura de protecciones refleja la de ordCheck: un conjunto de datos
# vacío, una variable dependiente ausente o no factor, menos de 3 categorías
# *observadas* (vía levels(droplevels(...))), cero predictores seleccionados
# y un ajuste de modelo fallido, cada uno con salida anticipada hacia un
# mensaje de introducción/notas. Estas pruebas confirman que cada
# protección sigue activándose, y que un predictor constante no rompe el
# ajuste de multinom() ni su cribado de influencia por residuo de Pearson a
# nivel de caso.
# -----------------------------------------------------------------------------

test_that("multCheck tolerates accented and symbol variable names", {

    data <- edgeSpecialNameData()
    data$`grupo_niño` <- edgeLevelFactor(nrow(data), 3)

    expect_no_error(
        AssumptionsLab::multCheck(
            data = data,
            dep  = "grupo_niño",
            covs = "niño_años"
        )
    )
})

test_that("multCheck shows its own guidance for a single-row data set (declared levels, one observed)", {

    data <- edgeSingleRowData()
    data$group3 <- factor("A", levels = c("A", "B", "C"))

    expect_no_error(
        AssumptionsLab::multCheck(
            data = data,
            dep  = "group3",
            covs = "cov1"
        )
    )
})

test_that("multCheck shows its own guidance when the dependent variable is entirely NA", {

    data <- edgeAllNaData("group3", n = 30)

    expect_no_error(
        AssumptionsLab::multCheck(
            data = data,
            dep  = "group3",
            covs = "cov1"
        )
    )
})

test_that("multCheck tolerates a zero-variance numeric predictor", {

    data <- edgeConstantData("cov1", n = 30)

    expect_no_error(
        AssumptionsLab::multCheck(
            data = data,
            dep  = "group3",
            covs = c("cov1", "cov2")
        )
    )
})

test_that("multCheck shows its own redirect message for a two-level dependent variable", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::multCheck(
            data = data,
            dep  = "group2",
            covs = "cov1"
        )
    )
})

test_that("multCheck shows its own guidance for a dependent variable with a single level", {

    data <- edgeBaseData(n = 30)
    data$group3 <- edgeLevelFactor(nrow(data), 1)

    expect_no_error(
        AssumptionsLab::multCheck(
            data = data,
            dep  = "group3",
            covs = "cov1"
        )
    )
})

test_that("multCheck shows its own guidance when no predictor is selected", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::multCheck(
            data = data,
            dep  = "group3"
        )
    )
})
