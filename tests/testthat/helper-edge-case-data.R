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
# Shared edge-case data fixtures.
# ES: Datos compartidos para casos límite.
#
# Every AssumptionsLab analysis is exercised, in tests/testthat/test-*.R, on
# pathological input drawn from these builders: non-ASCII/symbol column
# names, a single-row data set, an entirely missing column, a zero-variance
# column, and factors with the minimum number of levels a given module's
# design requires. Centralizing them here keeps each module's test file
# focused on which edge case is meaningful for that module, rather than on
# re-deriving the fixture itself.
#
# ES: Cada análisis de AssumptionsLab se pone a prueba, en
# tests/testthat/test-*.R, con entradas patológicas construidas a partir de
# estos generadores: nombres de columna con caracteres no ASCII o símbolos,
# un conjunto de datos de una sola fila, una columna completamente faltante,
# una columna de varianza cero, y factores con el número mínimo de niveles
# que el diseño de cada módulo exige. Centralizarlos aquí permite que el
# archivo de pruebas de cada módulo se concentre en qué caso límite es
# relevante para ese módulo, en lugar de reconstruir la misma base de datos.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Build a factor with an exact number of levels.
# ES: Construir un factor con un número exacto de niveles.
#
# Used to probe each module's documented level-count guards (e.g. groupCheck
# needs a 2-level group, ordCheck/multCheck need a 3+ level dependent
# variable) by generating factors with exactly one, two, or three levels on
# demand.
#
# ES: Se usa para poner a prueba las validaciones de número de niveles
# documentadas en cada módulo (p. ej. groupCheck necesita un grupo de 2
# niveles, ordCheck/multCheck necesitan una variable dependiente de 3 o más
# niveles), generando factores con exactamente uno, dos o tres niveles bajo
# demanda.
# -----------------------------------------------------------------------------
edgeLevelFactor <- function(n, levelCount, ordered = FALSE) {

    labels <- LETTERS[seq_len(max(levelCount, 1))]
    values <- rep(labels, length.out = n)

    factor(values, levels = labels, ordered = ordered)
}

# -----------------------------------------------------------------------------
# Baseline valid data set.
# ES: Conjunto de datos base válido.
#
# A small but well-formed data set (continuous predictors/dependents, 2-,
# 3-level nominal factors and 3-level ordinal factors) used as the starting
# point that individual tests then corrupt with one pathological column at a
# time, so a failure can be attributed to the specific corruption rather than
# to an unrelated data quirk.
#
# ES: Un conjunto de datos pequeño pero bien formado (predictores/variables
# dependientes continuas, factores nominales de 2 y 3 niveles y factores
# ordinales de 3 niveles) usado como punto de partida que cada prueba
# corrompe luego con una sola columna patológica a la vez, de modo que una
# falla pueda atribuirse a la corrupción específica y no a un defecto de
# datos ajeno.
# -----------------------------------------------------------------------------
edgeBaseData <- function(n = 30, seed = 20260904) {

    withr::local_seed(seed)

    data.frame(
        dep       = stats::rnorm(n, mean = 50, sd = 10),
        outcome   = stats::rnorm(n, mean = 50, sd = 10),
        cov1      = stats::rnorm(n),
        cov2      = stats::rnorm(n),
        group2    = edgeLevelFactor(n, 2),
        group3    = edgeLevelFactor(n, 3),
        ord3      = edgeLevelFactor(n, 3, ordered = TRUE),
        bin       = edgeLevelFactor(n, 2),
        stringsAsFactors = FALSE
    )
}

# -----------------------------------------------------------------------------
# Data set with accented and symbol column names.
# ES: Conjunto de datos con nombres de columna acentuados y con símbolos.
#
# jamovi's own submission guidance calls for testing "special characters" in
# the data; the most consequential place they can appear is in the variable
# names themselves, since those names flow through non-standard evaluation
# (jmvcore::resolveQuo / marshalData) before reaching each module's .b.R
# file. Column names here mix non-ASCII letters, accents and a mathematical
# symbol, all of which are syntactically valid R names when backtick-quoted.
#
# ES: La propia guía de envío de jamovi pide probar "caracteres especiales"
# en los datos; el lugar donde más impacto tienen es en los propios nombres
# de variable, ya que esos nombres atraviesan evaluación no estándar
# (jmvcore::resolveQuo / marshalData) antes de llegar al archivo .b.R de
# cada módulo. Los nombres de columna aquí combinan letras no ASCII, acentos
# y un símbolo matemático, todos válidos sintácticamente en R si se citan
# con comillas invertidas.
# -----------------------------------------------------------------------------
edgeSpecialNameData <- function(n = 30, seed = 20260904) {

    withr::local_seed(seed)

    data.frame(
        `niño_años`     = stats::rnorm(n, mean = 50, sd = 10),
        `café ± más`    = stats::rnorm(n),
        `nivel_hogar`   = edgeLevelFactor(n, 2),
        `grupo_niño`    = edgeLevelFactor(n, 3),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
}

# -----------------------------------------------------------------------------
# Single-observation data set.
# ES: Conjunto de datos con una sola observación.
#
# n = 1 is the smallest data set a jamovi user can still submit to an
# analysis (an empty data set is refused by jamovi itself before reaching R).
# Every downstream statistic that requires variance, degrees of freedom, or
# more than one group is expected to degrade to a documented message rather
# than error out on subscript/length mismatches.
#
# ES: n = 1 es el conjunto de datos más pequeño que un usuario de jamovi aún
# puede enviar a un análisis (jamovi rechaza un conjunto de datos vacío
# antes de que llegue a R). Se espera que todo estadístico posterior que
# requiera varianza, grados de libertad o más de un grupo degrade a un
# mensaje documentado en lugar de fallar por desajustes de subíndices o
# longitudes.
# -----------------------------------------------------------------------------
edgeSingleRowData <- function() {

    data.frame(
        dep     = 42,
        outcome = 42,
        cov1    = 1,
        cov2    = 1,
        group2  = factor("A", levels = c("A", "B")),
        group3  = factor("A", levels = c("A", "B", "C")),
        ord3    = factor("A", levels = c("A", "B", "C"), ordered = TRUE),
        bin     = factor("A", levels = c("A", "B")),
        stringsAsFactors = FALSE
    )
}

# -----------------------------------------------------------------------------
# Baseline data set with one column forced entirely to NA.
# ES: Conjunto de datos base con una columna forzada por completo a NA.
#
# Simulates a variable that was selected in the interface but never actually
# recorded (e.g. an entire survey wave skipped). columnName identifies which
# column of edgeBaseData() is overwritten, so a test can force the dependent
# variable, a covariate, or a factor to be entirely missing.
#
# ES: Simula una variable que fue seleccionada en la interfaz pero nunca se
# registró (p. ej. una ola completa de encuesta omitida). columnName indica
# qué columna de edgeBaseData() se sobrescribe, de modo que una prueba pueda
# forzar que la variable dependiente, un covariable o un factor estén
# enteramente faltantes.
# -----------------------------------------------------------------------------
edgeAllNaData <- function(columnName, n = 30, seed = 20260904) {

    data <- edgeBaseData(n = n, seed = seed)

    # Assigning bare NA (logical) would silently coerce a numeric column to
    # logical, which jmvcore's own option-type validation then rejects
    # before the analysis ever runs. In-place assignment to every element
    # preserves the column's original class (numeric or factor) while still
    # emptying it out.
    # ES: Asignar NA a secas (lógico) convertiría en silencio una columna
    # numérica a lógica, lo que la propia validación de tipos de opciones de
    # jmvcore rechazaría antes de que el análisis llegue a ejecutarse. La
    # asignación en el lugar a cada elemento conserva la clase original de
    # la columna (numérica o factor) y aun así la vacía por completo.
    data[[columnName]][] <- NA

    data
}

# -----------------------------------------------------------------------------
# Baseline data set with one numeric column forced to zero variance.
# ES: Conjunto de datos base con una columna numérica forzada a varianza cero.
#
# A constant predictor is a classic trigger for rank-deficient design
# matrices, division-by-zero in standardization, and singular
# variance-covariance matrices in multicollinearity diagnostics. columnName
# identifies which numeric column of edgeBaseData() is replaced by a
# repeated constant.
#
# ES: Un predictor constante es un disparador clásico de matrices de diseño
# con rango deficiente, división por cero al estandarizar y matrices de
# varianza-covarianza singulares en los diagnósticos de multicolinealidad.
# columnName indica qué columna numérica de edgeBaseData() se reemplaza por
# una constante repetida.
# -----------------------------------------------------------------------------
edgeConstantData <- function(columnName, constantValue = 7, n = 30, seed = 20260904) {

    data <- edgeBaseData(n = n, seed = seed)
    data[[columnName]] <- rep(constantValue, n)

    data
}
