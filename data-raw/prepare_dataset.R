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
# Test dataset preparation.
# ES: Preparación del conjunto de datos de prueba.
#
# Translates the original Spanish-language survey export
# (assumptionslab_raw_source.csv, n = 450, untouched) into the English
# column names and category labels the module's own interface uses, then
# injects reproducible MCAR missingness and a handful of deliberate
# outliers so the file exercises every currently implemented module's
# validation and missing-data/outlier handling, not just its happy path
# -- mirroring the philosophy PeopleLab's data-raw/generate_dataset.R
# established for the sibling project. Unlike that script, this one does
# not simulate values from scratch: every substantive value (scores,
# categories, dates, time-series levels) is copied verbatim from the
# original export. Only column names, category labels, and the specific
# cells selected for injected missingness/outliers are touched.
#
# ES: Traduce la exportación original de la encuesta en español
# (assumptionslab_raw_source.csv, n = 450, intacta) a los nombres de
# columna y etiquetas de categoría en inglés que usa la interfaz del
# propio módulo, y luego inyecta valores faltantes MCAR reproducibles y
# un puñado de atípicos deliberados para que el archivo ejercite la
# validación y el manejo de datos faltantes/atípicos de cada módulo
# actualmente implementado, no solo su camino feliz -- replicando la
# filosofía que data-raw/generate_dataset.R estableció para el proyecto
# hermano PeopleLab. A diferencia de aquel script, este no simula valores
# desde cero: cada valor sustantivo (puntajes, categorías, fechas,
# niveles de series de tiempo) se copia textualmente de la exportación
# original. Solo se tocan los nombres de columna, las etiquetas de
# categoría y las celdas específicas elegidas para la inyección de datos
# faltantes/atípicos.
#
# Responsibilities
# 1. Read the untouched raw export and rename every column to English.
# 2. Recode Spanish category labels (Sexo, Nivel_estudios,
#    Experiencia_inversion, Intencion_ahorro) to English, preserving the
#    ordinal order of the ordered scales.
# 3. Inject ~3% MCAR missingness into six numeric columns spanning
#    groupCheck, relatedCheck, anovaCheck, regCheck, and pathCheck's
#    dependent/predictor roles.
# 4. Inject a handful of deliberate outliers/edge cases (extreme
#    anxiety, extreme purchase intention, a paired-scores "backslide")
#    so influence/outlier diagnostics have genuine cases to flag.
# 5. Simulate investment_preference, an unordered 3-level nominal outcome
#    (Fixed Income / Real Estate / Stocks) driven by a known multinomial-
#    logit model, so multCheck has a genuine dependent variable with
#    recoverable, category-specific signal to detect -- the bundled
#    dataset previously had no unordered 3+-level nominal column at all.
# 6. Never touch the grouping/identifier columns or the eight time-series
#    columns -- those stay complete and in original row order so
#    timeCheck exercises the clean-signal path deterministically.
# 7. Write the result to ../data/ and print diagnostics so a run of this
#    script doubles as a sanity check on what was injected.
#
# ES: Responsabilidades
# 1. Leer la exportación cruda intacta y renombrar cada columna al inglés.
# 2. Recodificar las etiquetas de categoría en español (Sexo,
#    Nivel_estudios, Experiencia_inversion, Intencion_ahorro) al inglés,
#    preservando el orden ordinal de las escalas ordenadas.
# 3. Inyectar ~3% de valores faltantes MCAR en seis columnas numéricas
#    que abarcan los roles de variable dependiente/predictora de
#    groupCheck, relatedCheck, anovaCheck, regCheck y pathCheck.
# 4. Inyectar un puñado de atípicos/casos borde deliberados (ansiedad
#    extrema, intención de compra extrema, un "retroceso" en las
#    medidas pareadas) para que los diagnósticos de influencia/atípicos
#    tengan casos reales que señalar.
# 5. Simular investment_preference, un desenlace nominal no ordenado de 3
#    niveles (Fixed Income / Real Estate / Stocks) generado por un modelo
#    logit multinomial conocido, para que multCheck tenga una variable
#    dependiente genuina con señal recuperable y específica por categoría
#    -- el dataset no tenía antes ninguna columna nominal no ordenada de
#    3+ niveles.
# 6. Nunca tocar las columnas de agrupación/identificador ni las ocho
#    columnas de series de tiempo -- esas permanecen completas y en el
#    orden de fila original para que timeCheck ejercite el camino de
#    señal limpia de forma determinística.
# 6. Escribir el resultado en ../data/ e imprimir diagnósticos para que
#    correr este script sirva también como verificación de lo inyectado.
# -----------------------------------------------------------------------------

set.seed(20260904)

# -----------------------------------------------------------------------------
# 1. Read the raw export and translate column names.
# ES: 1. Leer la exportación cruda y traducir los nombres de columna.
# -----------------------------------------------------------------------------
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
    "data-raw"
}

raw_path <- file.path(script_dir, "assumptionslab_raw_source.csv")
raw <- read.csv(raw_path, stringsAsFactors = FALSE, encoding = "UTF-8")

name_map <- c(
    ID = "id",
    Fecha = "date",
    Sexo = "gender",
    Edad = "age_range",
    Nivel_estudios = "education_level",
    Experiencia_inversion = "investment_experience",
    Intencion_ahorro = "savings_intention",
    Pretest = "pretest_score",
    Postest_1 = "posttest_score_1",
    Postest_2 = "posttest_score_2",
    Ansiedad = "anxiety",
    Bienestar = "wellbeing",
    Intencion_compra = "purchase_intention",
    Necesidad_cognicion = "need_for_cognition",
    Resiliencia = "resilience",
    Planificacion = "planning",
    Confortar = "comfort",
    Neuroticismo = "neuroticism",
    Nomofobia = "nomophobia",
    SARIMA_serie = "sarima_series",
    ARIMA_serie = "arima_series",
    ETS_serie = "ets_series",
    GARCH_serie = "garch_series",
    VAR_serie_1 = "var_series_1",
    VAR_serie_2 = "var_series_2",
    VECM_serie_1 = "vecm_series_1",
    VECM_serie_2 = "vecm_series_2"
)
names(raw) <- name_map[names(raw)]

# -----------------------------------------------------------------------------
# 2. Recode category labels to English.
# ES: 2. Recodificar las etiquetas de categoría al inglés.
#
# Nivel_estudios/education_level and Intencion_ahorro/savings_intention
# keep their Low < Medium < High order intact -- both feed ordCheck's
# 3+-level ordinal dependent-variable slot, and Jamovi's CSV import
# respects the order categories first appear in unless a user later
# reassigns it, so this is a deliberate, documented choice rather than
# an accident of alphabetical sorting.
# ES: education_level e Intencion_ahorro/savings_intention conservan su
# orden Low < Medium < High intacto -- ambas alimentan la variable
# dependiente ordinal de 3+ niveles de ordCheck, y la importación CSV de
# Jamovi respeta el orden en que las categorías aparecen por primera vez
# salvo que un usuario lo reasigne después, así que esta es una decisión
# deliberada y documentada, no un accidente del orden alfabético.
# -----------------------------------------------------------------------------
raw$gender <- c(Mujer = "Female", Hombre = "Male")[raw$gender]
raw$education_level <- c(Bajo = "Low", Medio = "Medium", Alto = "High")[raw$education_level]
raw$investment_experience <- c(No = "No", SI = "Yes")[raw$investment_experience]
raw$savings_intention <- c(Bajo = "Low", Medio = "Medium", Alto = "High")[raw$savings_intention]

dat <- raw[, unname(name_map)]
dat$date <- as.Date(dat$date)

n <- nrow(dat)

# -----------------------------------------------------------------------------
# 3. Inject ~3% MCAR missingness.
# ES: 3. Inyectar ~3% de valores faltantes MCAR.
#
# posttest_score_2 is included deliberately: dropping a member of a
# pretest/posttest_1/posttest_2 measurement set is the realistic way a
# repeated-measures dataset goes incomplete, and relatedCheck should be
# exercised on genuinely incomplete pairs, not just complete ones.
# ES: posttest_score_2 se incluye deliberadamente: perder un miembro del
# conjunto de medidas pretest/posttest_1/posttest_2 es la forma realista
# en que un dataset de medidas repetidas queda incompleto, y relatedCheck
# debe ejercitarse sobre pares genuinamente incompletos, no solo sobre
# pares completos.
# -----------------------------------------------------------------------------
inject_na <- function(x, rate = 0.03) {
    idx <- sample(seq_along(x), size = round(length(x) * rate))
    x[idx] <- NA
    x
}

mcar_cols <- c(
    "anxiety", "wellbeing", "purchase_intention",
    "need_for_cognition", "resilience", "posttest_score_2"
)
for (col in mcar_cols) dat[[col]] <- inject_na(dat[[col]], rate = 0.03)

# -----------------------------------------------------------------------------
# 4. Inject a handful of deliberate outliers / edge cases.
# ES: 4. Inyectar un puñado de atípicos/casos borde deliberados.
#
# Three pairs, each with a distinct methodological purpose: (1) extreme
# anxiety, for groupCheck/regCheck's univariate outlier and influence
# diagnostics; (2) an implausibly low purchase intention, for
# anovaCheck/pathCheck's multivariate (Mahalanobis-type) outlier
# detection; (3) a paired-scores "backslide" (posttest below pretest by
# a wide margin), the realistic edge case relatedCheck's paired-
# difference influence diagnostics should be able to flag. Row indices
# are drawn with the fixed seed above rather than hardcoded, and are
# disjoint from each other and from the MCAR-missing rows for the same
# columns by construction (sampled from the still-complete-at-injection-
# time indices of each target column).
# ES: Tres pares, cada uno con un propósito metodológico distinto: (1)
# ansiedad extrema, para los diagnósticos univariados de atípicos e
# influencia de groupCheck/regCheck; (2) una intención de compra
# implausiblemente baja, para la detección de atípicos multivariados
# (tipo Mahalanobis) de anovaCheck/pathCheck; (3) un "retroceso" en las
# medidas pareadas (postest muy por debajo del pretest), el caso borde
# realista que los diagnósticos de influencia de diferencias pareadas de
# relatedCheck deben poder señalar. Los índices de fila se extraen con
# la semilla fija de arriba en vez de fijarse a mano, y son disjuntos
# entre sí y de las filas con NA inyectado en las mismas columnas por
# construcción (muestreados de los índices aún completos de cada
# columna objetivo al momento de la inyección).
# -----------------------------------------------------------------------------
anxiety_idx <- sample(which(!is.na(dat$anxiety)), 2)
dat$anxiety[anxiety_idx] <- c(95.4, 91.8)

purchase_idx <- sample(setdiff(which(!is.na(dat$purchase_intention)), anxiety_idx), 2)
dat$purchase_intention[purchase_idx] <- c(6.5, 9.2)

backslide_idx <- sample(setdiff(which(!is.na(dat$posttest_score_2)), c(anxiety_idx, purchase_idx)), 2)
dat$posttest_score_1[backslide_idx] <- c(1, 2)
dat$posttest_score_2[backslide_idx] <- c(1, 1)

# -----------------------------------------------------------------------------
# 5. Simulate investment_preference: an unordered, 3-level nominal outcome
# for multCheck's dependent-variable slot.
# ES: 5. Simular investment_preference: un desenlace nominal no ordenado
# de 3 niveles para la variable dependiente de multCheck.
#
# A multinomial-logit model with Fixed Income as the reference category
# (first factor level), driven by neuroticism, investment_experience, and
# planning -- three predictors that are themselves complete (no injected
# missingness above), so this column has no missingness of its own by
# construction. Real Estate's log-odds are driven mainly by planning
# (long-horizon, tangible-asset thinking); Stocks' log-odds are driven
# mainly by neuroticism (risk aversion, negative) and investment_experience
# (positive) -- deliberately different predictors matter for different
# categories, so multCheck's category-specific coefficient table has a
# genuine story to tell, not a uniform effect copy-pasted across
# categories. Calibrated by simulation (see data/README.md) to McFadden
# pseudo-R^2 ~ .10 and ~50% classification accuracy against a 33% chance
# baseline -- a real, moderate, recoverable signal, not an inflated toy
# example. Category is drawn stochastically from the fitted probabilities
# (not the arg-max), so the signal is genuine but noisy, as in any real
# survey outcome.
#
# ES: Un modelo logit multinomial con Fixed Income como categoría de
# referencia (primer nivel del factor), impulsado por neuroticism,
# investment_experience y planning -- tres predictores que ya son
# completos (sin valores faltantes inyectados arriba), por lo que esta
# columna no tiene valores faltantes propios por construcción. El
# logit de Real Estate está impulsado principalmente por planning
# (pensamiento de horizonte largo, activo tangible); el logit de Stocks
# está impulsado principalmente por neuroticism (aversión al riesgo,
# negativo) e investment_experience (positivo) -- deliberadamente,
# predictores distintos importan para categorías distintas, para que la
# tabla de coeficientes por categoría de multCheck tenga una historia
# genuina que contar, no un efecto uniforme copiado entre categorías.
# Calibrado por simulación (ver data/README.md) a un pseudo-R^2 de
# McFadden ~.10 y ~50% de precisión de clasificación contra una base de
# azar del 33% -- una señal real y moderada, no un ejemplo de juguete
# inflado. La categoría se extrae de forma estocástica a partir de las
# probabilidades ajustadas (no del máximo), para que la señal sea
# genuina pero ruidosa, como en cualquier desenlace real de encuesta.
# -----------------------------------------------------------------------------
neuroticism_centered <- dat$neuroticism - mean(dat$neuroticism, na.rm = TRUE)
experience_yes <- as.integer(dat$investment_experience == "Yes")
planning_centered <- dat$planning - mean(dat$planning, na.rm = TRUE)

eta_real_estate <- 0.05 - 0.03 * neuroticism_centered + 0.6 * experience_yes + 0.18 * planning_centered
eta_stocks <- -0.10 - 0.07 * neuroticism_centered + 1.3 * experience_yes - 0.04 * planning_centered

denom <- 1 + exp(eta_real_estate) + exp(eta_stocks)
prob_fixed_income <- 1 / denom
prob_real_estate <- exp(eta_real_estate) / denom
prob_stocks <- exp(eta_stocks) / denom

draw <- runif(n)
investment_preference <- ifelse(
    draw < prob_fixed_income, "Fixed Income",
    ifelse(draw < prob_fixed_income + prob_real_estate, "Real Estate", "Stocks")
)
dat$investment_preference <- factor(
    investment_preference,
    levels = c("Fixed Income", "Real Estate", "Stocks")
)

# -----------------------------------------------------------------------------
# 6. Write the result next to this script, in ../data/, jamovi's
# standard convention for a module's bundled example data (see
# jamovi/0000.yaml's "datasets:" entry and data/README.md).
# ES: 5. Escribir el resultado junto a este script, en ../data/, la
# convención estándar de jamovi para los datos de ejemplo incluidos con
# un módulo (ver la entrada "datasets:" de jamovi/0000.yaml y
# data/README.md).
# -----------------------------------------------------------------------------
out_path <- file.path(script_dir, "..", "data", "assumptionslab_test_data.csv")
write.csv(dat, out_path, row.names = FALSE, na = "")

# -----------------------------------------------------------------------------
# Diagnostics: printed so a run of this script doubles as a sanity check
# that the injected missingness/outliers landed as intended.
# ES: Diagnósticos: impresos para que correr este script sirva también
# como verificación de que los datos faltantes/atípicos inyectados
# aterrizaron como se pretendía.
# -----------------------------------------------------------------------------
cat("Wrote:", out_path, "\n")
cat("n =", n, "\n\n")

cat("Missing values by column (only columns with any):\n")
na_counts <- sapply(dat, function(x) sum(is.na(x)))
print(na_counts[na_counts > 0])

cat("\nInjected outlier rows:\n")
cat("  anxiety     :", anxiety_idx, "->", dat$anxiety[anxiety_idx], "\n")
cat("  purchase    :", purchase_idx, "->", dat$purchase_intention[purchase_idx], "\n")
cat("  backslide   :", backslide_idx, "-> pretest",
    dat$pretest_score[backslide_idx], "/ posttest_1",
    dat$posttest_score_1[backslide_idx], "/ posttest_2",
    dat$posttest_score_2[backslide_idx], "\n")

cat("\nCategory distributions:\n")
for (col in c("gender", "age_range", "education_level", "investment_experience",
               "savings_intention", "investment_preference")) {
    cat("  ", col, ":\n", sep = "")
    print(table(dat[[col]]))
}

if (requireNamespace("nnet", quietly = TRUE)) {
    cat("\ninvestment_preference sanity fit (neuroticism + investment_experience + planning):\n")
    fit <- nnet::multinom(
        investment_preference ~ neuroticism + investment_experience + planning,
        data = dat, trace = FALSE
    )
    fit0 <- nnet::multinom(investment_preference ~ 1, data = dat, trace = FALSE)
    cat("  McFadden pseudo-R^2:", round(1 - as.numeric(logLik(fit)) / as.numeric(logLik(fit0)), 4), "\n")
    cat("  Classification accuracy:", round(mean(predict(fit) == dat$investment_preference), 3), "\n")
}

cat("\nDate range:", format(min(dat$date)), "to", format(max(dat$date)), "\n")
