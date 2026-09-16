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
# logCheck edge-case tests.
# ES: Pruebas de casos límite de logCheck.
#
# logCheck fits a binary glm() (logit link) and already guards, in order, on
# an empty data set, a missing dependent variable, a dependent variable that
# is not exactly 2-level/2-valued, zero selected predictors, and a failed
# model fit, each short-circuiting to an intro/notes message. These tests
# confirm every guard still fires under pathological input, including the
# case where a dependent factor carries two *declared* levels but the data
# supplies only one *observed* level (which the level-count guard alone does
# not catch), and that a constant covariate does not break glm() fitting.
#
# ES: logCheck ajusta un glm() binario (enlace logit) y ya protege, en
# orden, contra un conjunto de datos vacío, una variable dependiente no
# seleccionada, una variable dependiente que no tiene exactamente 2 niveles
# o valores, cero predictores seleccionados y un ajuste de modelo fallido,
# cada uno con salida anticipada hacia un mensaje de introducción/notas.
# Estas pruebas confirman que cada protección sigue activándose ante entrada
# patológica, incluido el caso en que un factor dependiente declara dos
# niveles pero los datos solo aportan un nivel *observado* (que la
# protección de conteo de niveles por sí sola no detecta), y que un
# covariable constante no rompe el ajuste de glm().
# -----------------------------------------------------------------------------

test_that("logCheck tolerates accented and symbol variable names", {

    data <- edgeSpecialNameData()

    expect_no_error(
        AssumptionsLab::logCheck(
            data = data,
            dep  = "nivel_hogar",
            covs = "niño_años"
        )
    )
})

test_that("logCheck tolerates a predictor name containing a space", {

    # jamovi's Sep 2026 module review reported a crash when a predictor name
    # contains a space (a common case: spreadsheet-derived column headers).
    # The root cause was unquoted formula construction
    # (`as.formula(paste("y ~", paste(predictors, collapse = "+")))`); fixed
    # via the qname()/strip_qname() helper pair, mirroring ordCheck/
    # pathCheck. Covers both the main glm() formula and the Box-Tidwell
    # linearity-in-the-logit interaction formula, and confirms the
    # odds-ratios/linearity tables display the name without stray backticks.
    # ES: la revisión del módulo de jamovi de sep. 2026 reportó un fallo
    # cuando un nombre de predictor contiene un espacio (caso común:
    # encabezados de columna provenientes de una hoja de cálculo). La causa
    # raíz era la construcción de fórmula sin comillas
    # (`as.formula(paste("y ~", paste(predictors, collapse = "+")))`);
    # corregido con el par de funciones qname()/strip_qname(), siguiendo el
    # mismo patrón de ordCheck/pathCheck. Cubre tanto la fórmula glm()
    # principal como la fórmula de interacción de linealidad-en-el-logit de
    # Box-Tidwell, y confirma que las tablas de razones de momios/linealidad
    # muestran el nombre sin comillas invertidas sobrantes.

    withr::local_seed(20260916)
    n <- 60
    data <- data.frame(
        `Test Score` = stats::rnorm(n, 70, 10),
        anxiety      = stats::rnorm(n, 50, 10),
        outcome      = edgeLevelFactor(n, 2),
        check.names  = FALSE
    )

    res <- AssumptionsLab::logCheck(
        data = data,
        dep  = "outcome",
        covs = c("Test Score", "anxiety")
    )

    expect_true(res$oddsRatios$isFilled())
    expect_true(res$linearity$isFilled())

    predictorNames <- res$oddsRatios$asDF$predictor
    expect_false(any(grepl("`", predictorNames, fixed = TRUE)))
    expect_true("Test Score" %in% predictorNames)
})

test_that("logCheck tolerates a single-row data set (declared levels, one observed)", {

    data <- edgeSingleRowData()

    expect_no_error(
        AssumptionsLab::logCheck(
            data = data,
            dep  = "bin",
            covs = "cov1"
        )
    )
})

test_that("logCheck shows its own guidance when the dependent variable is entirely NA", {

    data <- edgeAllNaData("bin", n = 30)

    expect_no_error(
        AssumptionsLab::logCheck(
            data = data,
            dep  = "bin",
            covs = "cov1"
        )
    )
})

test_that("logCheck tolerates a zero-variance numeric predictor", {

    data <- edgeConstantData("cov1", n = 30)

    expect_no_error(
        AssumptionsLab::logCheck(
            data = data,
            dep  = "bin",
            covs = c("cov1", "cov2")
        )
    )
})

test_that("logCheck shows its own guidance for a dependent variable with a single level", {

    data <- edgeBaseData(n = 30)
    data$bin <- edgeLevelFactor(nrow(data), 1)

    expect_no_error(
        AssumptionsLab::logCheck(
            data = data,
            dep  = "bin",
            covs = "cov1"
        )
    )
})

test_that("logCheck shows its own guidance for a dependent variable with three levels", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::logCheck(
            data = data,
            dep  = "group3",
            covs = "cov1"
        )
    )
})

test_that("logCheck shows its own guidance when no predictor is selected", {

    data <- edgeBaseData(n = 30)

    expect_no_error(
        AssumptionsLab::logCheck(
            data = data,
            dep  = "bin"
        )
    )
})
