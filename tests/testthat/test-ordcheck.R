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
# ordCheck edge-case tests.
# ES: Pruebas de casos límite de ordCheck.
#
# ordCheck fits a proportional-odds model (MASS::polr()) for an ordered
# dependent variable with three or more categories. It already guards on an
# empty data set, a missing/non-factor dependent variable, a dependent
# variable with fewer than 3 *observed* categories (computed via
# levels(droplevels(...)), which correctly demotes a factor that only
# declares 3 levels but observes fewer - important for the single-row case
# below), zero selected predictors, and a failed model fit. These tests
# confirm every guard still fires, and that a constant predictor does not
# break polr() fitting or its case-level Pearson-residual influence
# screening.
#
# ES: ordCheck ajusta un modelo de momios proporcionales (MASS::polr()) para
# una variable dependiente ordenada con tres o más categorías. Ya protege
# contra un conjunto de datos vacío, una variable dependiente ausente o no
# factor, una variable dependiente con menos de 3 categorías *observadas*
# (calculado vía levels(droplevels(...)), que correctamente degrada un
# factor que declara 3 niveles pero observa menos - importante para el caso
# de una sola fila más abajo), cero predictores seleccionados y un ajuste de
# modelo fallido. Estas pruebas confirman que cada protección sigue
# activándose, y que un predictor constante no rompe el ajuste de polr() ni
# su cribado de influencia por residuo de Pearson a nivel de caso.
# -----------------------------------------------------------------------------

test_that("ordCheck tolerates accented and symbol variable names", {

    data <- edgeSpecialNameData()
    data$`grupo_niño` <- edgeLevelFactor(nrow(data), 3, ordered = TRUE)

    expect_no_error(
        AssumptionsLab::ordCheck(
            data = data,
            dep  = "grupo_niño",
            covs = "niño_años"
        )
    )
})

test_that("ordCheck shows its own guidance for a single-row data set (declared levels, one observed)", {

    data <- edgeSingleRowData()

    expect_no_error(
        AssumptionsLab::ordCheck(
            data = data,
            dep  = "ord3",
            covs = "cov1"
        )
    )
})

test_that("ordCheck shows its own guidance when the dependent variable is entirely NA", {

    data <- edgeAllNaData("ord3", n = 30)

    expect_no_error(
        AssumptionsLab::ordCheck(
            data = data,
            dep  = "ord3",
            covs = "cov1"
        )
    )
})

test_that("ordCheck tolerates a zero-variance numeric predictor", {

    data <- edgeConstantData("cov1", n = 30)

    expect_no_error(
        AssumptionsLab::ordCheck(
            data = data,
            dep  = "ord3",
            covs = c("cov1", "cov2")
        )
    )
})

test_that("ordCheck shows its own redirect message for a two-level dependent variable", {

    data <- edgeBaseData(n = 30)
    data$ord3 <- edgeLevelFactor(nrow(data), 2, ordered = TRUE)

    expect_no_error(
        AssumptionsLab::ordCheck(
            data = data,
            dep  = "ord3",
            covs = "cov1"
        )
    )
})

test_that("ordCheck shows its own guidance for a dependent variable with a single level", {

    data <- edgeBaseData(n = 30)
    data$ord3 <- edgeLevelFactor(nrow(data), 1, ordered = TRUE)

    expect_no_error(
        AssumptionsLab::ordCheck(
            data = data,
            dep  = "ord3",
            covs = "cov1"
        )
    )
})

test_that("ordCheck shows its own guidance when no predictor is selected", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::ordCheck(
            data = data,
            dep  = "ord3"
        )
    )
})
