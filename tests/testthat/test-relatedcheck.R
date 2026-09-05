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
# relatedCheck edge-case tests.
# ES: Pruebas de casos límite de relatedCheck.
#
# relatedCheck evaluates paired/repeated-measures comparisons across two or
# more related numeric variables (measures). The module already guards on
# "fewer than two measures selected" and "fewer than three complete cases"
# with an explanatory intro message; these tests confirm those guards still
# fire under pathological input, and that everything downstream of them
# (normality, symmetry, sphericity for k >= 3, correlation comparisons)
# tolerates degenerate columns without an uncaught R error.
#
# ES: relatedCheck evalúa comparaciones pareadas/de medidas repetidas entre
# dos o más variables numéricas relacionadas (measures). El módulo ya
# protege contra "menos de dos medidas seleccionadas" y "menos de tres casos
# completos" con un mensaje explicativo en la introducción; estas pruebas
# confirman que esas protecciones siguen activándose ante entradas
# patológicas, y que todo lo que ocurre después de ellas (normalidad,
# simetría, esfericidad para k >= 3, comparaciones de correlación) tolera
# columnas degeneradas sin un error no controlado de R.
# -----------------------------------------------------------------------------

test_that("relatedCheck tolerates accented and symbol variable names", {

    data <- edgeSpecialNameData()

    expect_no_error(
        AssumptionsLab::relatedCheck(
            data     = data,
            measures = c("niño_años", "café ± más")
        )
    )
})

test_that("relatedCheck tolerates a single-row data set", {

    data <- edgeSingleRowData()

    expect_no_error(
        AssumptionsLab::relatedCheck(
            data     = data,
            measures = c("dep", "outcome")
        )
    )
})

test_that("relatedCheck tolerates a measure that is entirely NA", {

    data <- edgeAllNaData("outcome")

    expect_no_error(
        AssumptionsLab::relatedCheck(
            data     = data,
            measures = c("dep", "outcome")
        )
    )
})

test_that("relatedCheck tolerates a zero-variance measure", {

    data <- edgeConstantData("outcome")

    expect_no_error(
        AssumptionsLab::relatedCheck(
            data     = data,
            measures = c("dep", "outcome")
        )
    )
})

test_that("relatedCheck tolerates three or more related measures (sphericity path)", {

    data <- edgeBaseData()

    expect_no_error(
        AssumptionsLab::relatedCheck(
            data     = data,
            measures = c("dep", "outcome", "cov1")
        )
    )
})

test_that("relatedCheck shows its own guidance when only one measure is selected", {

    data <- edgeBaseData()

    expect_no_error(
        AssumptionsLab::relatedCheck(
            data     = data,
            measures = "dep"
        )
    )
})
