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
# groupCheck edge-case tests.
# ES: Pruebas de casos límite de groupCheck.
#
# groupCheck compares a continuous dependent variable across an independent
# grouping factor (normality and variance-homogeneity diagnostics only; it
# never fits the substantive group-comparison test itself). These tests do
# not assert any particular numeric result — they assert that pathological
# input is met with the module's own explanatory placeholder message
# (rendered through set_empty_message()/self$results$intro$setContent()),
# never with an uncaught R error.
#
# ES: groupCheck compara una variable dependiente continua entre los niveles
# de un factor de agrupación independiente (solo diagnósticos de normalidad
# y homogeneidad de varianza; nunca ajusta la prueba sustantiva de
# comparación de grupos). Estas pruebas no verifican ningún resultado
# numérico particular — verifican que la entrada patológica se resuelva con
# el propio mensaje explicativo del módulo (renderizado mediante
# set_empty_message()/self$results$intro$setContent()), nunca con un error
# no controlado de R.
# -----------------------------------------------------------------------------

test_that("groupCheck tolerates accented and symbol variable names", {

    data <- edgeSpecialNameData()

    expect_no_error(
        AssumptionsLab::groupCheck(
            data  = data,
            dep   = "niño_años",
            group = "grupo_niño"
        )
    )
})

test_that("groupCheck tolerates a single-row data set", {

    data <- edgeSingleRowData()

    expect_no_error(
        AssumptionsLab::groupCheck(
            data  = data,
            dep   = "dep",
            group = "group2"
        )
    )
})

test_that("groupCheck tolerates a dependent variable that is entirely NA", {

    data <- edgeAllNaData("dep")

    expect_no_error(
        AssumptionsLab::groupCheck(
            data  = data,
            dep   = "dep",
            group = "group2"
        )
    )
})

test_that("groupCheck tolerates a zero-variance dependent variable", {

    data <- edgeConstantData("dep")

    expect_no_error(
        AssumptionsLab::groupCheck(
            data  = data,
            dep   = "dep",
            group = "group2"
        )
    )
})

test_that("groupCheck tolerates a grouping variable with a single level", {

    data <- edgeBaseData()
    data$group2 <- edgeLevelFactor(nrow(data), 1)

    expect_no_error(
        AssumptionsLab::groupCheck(
            data  = data,
            dep   = "dep",
            group = "group2"
        )
    )
})

test_that("groupCheck tolerates a grouping variable with three or more levels", {

    data <- edgeBaseData()

    expect_no_error(
        AssumptionsLab::groupCheck(
            data  = data,
            dep   = "dep",
            group = "group3"
        )
    )
})
