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
# anovaCheck edge-case tests.
# ES: Pruebas de casos límite de anovaCheck.
#
# anovaCheck fits an lm() across the cells of one or more factors (optionally
# with covariates, turning it into an ANCOVA) and diagnoses that fit. It
# already guards on a missing dependent variable, zero selected factors, and
# fewer than five complete cases, all short-circuiting to an intro message
# before stats::lm() is ever called. These tests confirm those guards still
# fire, and that a constant covariate or a single-level factor do not break
# the model-fitting or diagnostic code that runs after them.
#
# ES: anovaCheck ajusta un lm() sobre las celdas de uno o más factores
# (opcionalmente con covariables, lo que lo convierte en un ANCOVA) y
# diagnostica ese ajuste. Ya protege contra la ausencia de variable
# dependiente, cero factores seleccionados y menos de cinco casos completos,
# todos con salida anticipada hacia un mensaje introductorio antes de
# invocar siquiera stats::lm(). Estas pruebas confirman que esas
# protecciones siguen activándose, y que una covariable constante o un
# factor de un solo nivel no rompen el ajuste del modelo ni el código de
# diagnóstico que corre después.
# -----------------------------------------------------------------------------

test_that("anovaCheck tolerates accented and symbol variable names", {

    data <- edgeSpecialNameData()

    expect_no_error(
        AssumptionsLab::anovaCheck(
            data    = data,
            dep     = "niño_años",
            factors = "grupo_niño"
        )
    )
})

test_that("anovaCheck shows its own guidance for a single-row data set", {

    data <- edgeSingleRowData()

    expect_no_error(
        AssumptionsLab::anovaCheck(
            data    = data,
            dep     = "dep",
            factors = "group3"
        )
    )
})

test_that("anovaCheck shows its own guidance when the dependent variable is entirely NA", {

    data <- edgeAllNaData("dep", n = 30)

    expect_no_error(
        AssumptionsLab::anovaCheck(
            data    = data,
            dep     = "dep",
            factors = "group3"
        )
    )
})

test_that("anovaCheck tolerates a zero-variance covariate", {

    data <- edgeConstantData("cov1", n = 30)

    expect_no_error(
        AssumptionsLab::anovaCheck(
            data    = data,
            dep     = "dep",
            factors = "group3",
            covs    = "cov1"
        )
    )
})

test_that("anovaCheck tolerates a single-level factor", {

    data <- edgeBaseData(n = 30)
    data$group3 <- edgeLevelFactor(nrow(data), 1)

    expect_no_error(
        AssumptionsLab::anovaCheck(
            data    = data,
            dep     = "dep",
            factors = "group3"
        )
    )
})

test_that("anovaCheck shows its own guidance when no factor is selected", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::anovaCheck(
            data = data,
            dep  = "dep"
        )
    )
})
