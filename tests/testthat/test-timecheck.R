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
# timeCheck edge-case tests.
# ES: Pruebas de casos límite de timeCheck.
#
# timeCheck's option shape differs from every other module in this suite: it
# takes `series` (one or more numeric series), an optional `dateVar` and
# `exogenous`, and a `model` choice among arima/sarima/ets/var/vecm/garch,
# each with its own minimum-observation threshold (20 complete cases for a
# univariate model, 30 for VAR/VECM) enforced BEFORE any model-fitting
# function runs. Critically, the entire model-fitting dispatch
# (private$.computeArima/.computeSarima/.computeEts/.computeVar/.computeVecm/
# .computeGarch) is itself wrapped in one top-level tryCatch() that turns any
# uncaught error into a "the diagnostic battery could not complete" note -
# these tests confirm that safety net actually holds for genuinely
# pathological series (constant, all-NA, too short), not merely that the
# length/package guards fire before it.
#
# ES: La forma de las opciones de timeCheck difiere de la de todos los demás
# módulos de esta suite: toma `series` (una o más series numéricas), un
# `dateVar` y `exogenous` opcionales, y una elección de `model` entre
# arima/sarima/ets/var/vecm/garch, cada uno con su propio umbral mínimo de
# observaciones (20 casos completos para un modelo univariado, 30 para
# VAR/VECM) exigido ANTES de que corra cualquier función de ajuste de
# modelo. De forma crucial, todo el despacho de ajuste de modelo
# (private$.computeArima/.computeSarima/.computeEts/.computeVar/.computeVecm/
# .computeGarch) está a su vez envuelto en un único tryCatch() de nivel
# superior que convierte cualquier error no controlado en una nota "la
# batería de diagnóstico no pudo completarse" - estas pruebas confirman que
# esa red de seguridad realmente sostiene series genuinamente patológicas
# (constantes, enteramente NA, demasiado cortas), y no solo que las
# protecciones de longitud/paquetes se activan antes de llegar a ella.
# -----------------------------------------------------------------------------

# A univariate model needs >= 20 complete cases before diagnostics run at
# all (see the min_obs guard in timecheck.b.R); n = 40 comfortably clears
# that so the tryCatch-guarded model-fitting path is genuinely exercised,
# not merely the "too few observations" short-circuit.
# ES: un modelo univariado necesita >= 20 casos completos antes de que
# corran los diagnósticos (ver la protección min_obs en timecheck.b.R);
# n = 40 supera eso con margen para que el camino de ajuste de modelo,
# protegido con tryCatch, se ejercite de verdad, no solo el atajo de
# "muy pocas observaciones".
edgeTimeSeriesData <- function(n = 40, seed = 20260904, constant = FALSE, allNa = FALSE) {

    withr::local_seed(seed)

    values <- if (allNa) {
        rep(NA_real_, n)
    } else if (constant) {
        rep(3, n)
    } else {
        stats::rnorm(n) + seq_len(n) * 0.05
    }

    data.frame(
        `serie_niño` = values,
        series2      = stats::rnorm(n),
        check.names  = FALSE,
        stringsAsFactors = FALSE
    )
}

test_that("timeCheck tolerates an accented series name", {

    data <- edgeTimeSeriesData(n = 40)

    expect_no_error(
        AssumptionsLab::timeCheck(
            data   = data,
            series = "serie_niño",
            model  = "sarima"
        )
    )
})

test_that("timeCheck shows its own guidance for a series far too short to diagnose", {

    data <- edgeTimeSeriesData(n = 5)

    expect_no_error(
        AssumptionsLab::timeCheck(
            data   = data,
            series = "serie_niño",
            model  = "arima"
        )
    )
})

test_that("timeCheck tolerates a series that is entirely NA", {

    data <- edgeTimeSeriesData(n = 40, allNa = TRUE)

    expect_no_error(
        AssumptionsLab::timeCheck(
            data   = data,
            series = "serie_niño",
            model  = "arima"
        )
    )
})

test_that("timeCheck's tryCatch-wrapped model fitting tolerates a constant series", {

    data <- edgeTimeSeriesData(n = 40, constant = TRUE)

    expect_no_error(
        AssumptionsLab::timeCheck(
            data   = data,
            series = "serie_niño",
            model  = "sarima"
        )
    )
})

test_that("timeCheck's tryCatch-wrapped GARCH fitting tolerates a constant series", {

    data <- edgeTimeSeriesData(n = 40, constant = TRUE)

    expect_no_error(
        AssumptionsLab::timeCheck(
            data   = data,
            series = "serie_niño",
            model  = "garch"
        )
    )
})

test_that("timeCheck shows its own guidance when VAR is chosen with only one series", {

    data <- edgeTimeSeriesData(n = 40)

    expect_no_error(
        AssumptionsLab::timeCheck(
            data   = data,
            series = "serie_niño",
            model  = "var"
        )
    )
})

test_that("timeCheck's tryCatch-wrapped VECM fitting tolerates two short series", {

    data <- edgeTimeSeriesData(n = 15)

    expect_no_error(
        AssumptionsLab::timeCheck(
            data   = data,
            series = c("serie_niño", "series2"),
            model  = "vecm"
        )
    )
})

# No "no series selected" case: `series` has no default in the generated
# wrapper (jamovi's UI itself requires at least one series before Run is
# enabled), so omitting it entirely raises R's own missing-argument error
# before the analysis logic ever runs - not a state the module needs to
# guard against, the same reason no other test file here tries omitting
# its own mandatory dep/group argument either.
