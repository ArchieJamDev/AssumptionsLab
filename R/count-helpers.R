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
# Count-model statistics.
# ES: Estadísticos para modelos de recuento.
#
# The numerical core of countCheck (Poisson and negative binomial
# regression), kept apart from the report code as plain functions with no
# jmvcore dependency. That separation lets the test suite and
# validation/countcheck_verification.R check every statistic against an
# independent computation without running a jamovi analysis, and keeps
# countcheck.b.R focused on assembling the report.
#
# ES: El núcleo numérico de countCheck (regresión de Poisson y binomial
# negativa), separado del código del informe como funciones simples sin
# dependencia de jmvcore. Esa separación permite que la suite de pruebas y
# validation/countcheck_verification.R comprueben cada estadístico contra un
# cálculo independiente sin ejecutar un análisis de jamovi, y deja
# countcheck.b.R centrado en armar el informe.
#
# Responsibilities
# 1. Validate that the outcome is a non-negative integer count.
# 2. Fit the Poisson model and the negative binomial model (MASS::glm.nb)
#    with an optional log-exposure offset.
# 3. Compute overdispersion statistics, the Poisson-versus-negative-binomial
#    likelihood ratio test, excess-zero checks, randomized quantile
#    residuals, spline-based linearity tests and partial residuals.
#
# ES: Responsabilidades
# 1. Validar que el desenlace sea un recuento entero no negativo.
# 2. Ajustar el modelo de Poisson y el binomial negativo (MASS::glm.nb) con
#    un offset opcional del logaritmo de la exposición.
# 3. Calcular los estadísticos de sobredispersión, la prueba de razón de
#    verosimilitudes Poisson frente a binomial negativa, las comprobaciones
#    de exceso de ceros, los residuos cuantílicos aleatorizados, las pruebas
#    de linealidad con splines y los residuos parciales.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Name of the offset column added to the model data.
# ES: Nombre de la columna de offset agregada a los datos del modelo.
#
# A fixed, syntactic name that cannot collide with a jamovi variable name
# in practice; it goes into the formula as offset(.al_count_offset_col).
# ES: Un nombre fijo y sintáctico que en la práctica no choca con un nombre
# de variable de jamovi; entra en la fórmula como offset(...).
# -----------------------------------------------------------------------------
.al_count_offset_col <- "al_count_log_exposure_"

# -----------------------------------------------------------------------------
# Check that a variable holds non-negative integer counts.
# ES: Comprobar que una variable contiene recuentos enteros no negativos.
#
# Input: y - the outcome values of the complete cases.
# Output: NULL when y is a valid count outcome, otherwise a short code naming
#   the problem ("nonnumeric", "negative", "noninteger", "allzero",
#   "constant") so the caller can write the matching sentence.
#
# ES: Entrada: y - los valores del desenlace en los casos completos.
# ES: Salida: NULL si y es un desenlace de recuento válido; si no, un código
#   corto que nombra el problema, para que quien llama escriba la oración
#   correspondiente.
# -----------------------------------------------------------------------------
.al_count_outcome_problem <- function(y) {
    if (!is.numeric(y))
        return("nonnumeric")
    if (any(y < 0))
        return("negative")
    if (any(abs(y - round(y)) > 1e-8))
        return("noninteger")
    if (all(y == 0))
        return("allzero")
    if (length(unique(y)) < 2)
        return("constant")
    NULL
}

# -----------------------------------------------------------------------------
# Fit the Poisson and negative binomial models.
# ES: Ajustar los modelos de Poisson y binomial negativo.
#
# Both models are always fitted, whichever one the user diagnoses: the
# overdispersion tests are statements about the Poisson model, and the
# likelihood ratio test and the expected zeros compare the two.
# glm.nb() often warns rather than fails when the data show no
# overdispersion (theta grows without bound and the alternation limit is
# reached); those warnings are kept and reported, never hidden.
#
# ES: Siempre se ajustan ambos modelos, sea cual sea el que se diagnostica:
# las pruebas de sobredispersión son afirmaciones sobre el modelo de
# Poisson, y la prueba de razón de verosimilitudes y los ceros esperados
# comparan los dos. glm.nb() suele advertir en vez de fallar cuando los
# datos no muestran sobredispersión (theta crece sin límite y se alcanza el
# límite de alternancias); esas advertencias se guardan y se informan,
# nunca se ocultan.
#
# Inputs: formula - the model formula, already including
#   offset(al_count_log_exposure_) when there is an exposure; data - the
#   complete cases.
# Output: list(pois, pois_error, nb, nb_error, nb_warnings).
# ES: Entradas: formula - la fórmula del modelo, que ya incluye el offset si
#   hay exposición; data - los casos completos.
# ES: Salida: list(pois, pois_error, nb, nb_error, nb_warnings).
# -----------------------------------------------------------------------------
.al_count_fit <- function(formula, data) {
    pois_error <- NULL
    pois <- tryCatch(
        stats::glm(formula, data = data, family = stats::poisson(link = "log")),
        error = function(e) {
            pois_error <<- conditionMessage(e)
            NULL
        }
    )

    nb_error <- NULL
    nb_warnings <- character(0)
    nb <- tryCatch(
        withCallingHandlers(
            MASS::glm.nb(formula, data = data),
            warning = function(w) {
                nb_warnings <<- c(nb_warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        ),
        error = function(e) {
            nb_error <<- conditionMessage(e)
            NULL
        }
    )
    if (!is.null(nb) && !is.finite(nb$theta)) {
        nb_error <- "theta is not finite"
        nb <- NULL
    }

    list(
        pois = pois, pois_error = pois_error,
        nb = nb, nb_error = nb_error, nb_warnings = unique(nb_warnings)
    )
}

# -----------------------------------------------------------------------------
# Overdispersion statistics for the Poisson fit.
# ES: Estadísticos de sobredispersión para el ajuste de Poisson.
#
# - Pearson chi-square / df: the dispersion ratio; about 1 under the
#   Poisson model. The p-value is the upper tail of chi-square(df).
# - Deviance / df: the same idea with the deviance; reported for reference,
#   since it is less reliable than Pearson's version with small means.
# - Dean-Lawless score test (Dean & Lawless, 1989): z = sum((y - mu)^2 - y)
#   / sqrt(2 * sum(mu^2)), one-sided against negative binomial (NB2)
#   overdispersion, var = mu + alpha * mu^2.
# - Cameron-Trivedi auxiliary regression (Cameron & Trivedi, 1990):
#   ((y - mu)^2 - y) / mu regressed on mu without an intercept; the slope
#   estimates alpha of the same NB2 form and its z is tested one-sided.
#
# ES: - Chi-cuadrado de Pearson / gl: el cociente de dispersión; cercano a 1
#   bajo el modelo de Poisson. El valor p es la cola superior de
#   chi-cuadrado(gl).
# - Devianza / gl: la misma idea con la devianza; se informa como
#   referencia, porque es menos fiable que la versión de Pearson con medias
#   pequeñas.
# - Prueba de puntuación de Dean-Lawless: z unilateral frente a la
#   sobredispersión binomial negativa (NB2), var = mu + alfa * mu^2.
# - Regresión auxiliar de Cameron-Trivedi: ((y - mu)^2 - y) / mu sobre mu
#   sin intercepto; la pendiente estima alfa de la misma forma NB2 y su z
#   se contrasta de forma unilateral.
#
# Input: fit - a fitted Poisson glm.
# Output: a named list of statistics, each NA when not computable.
# ES: Entrada: fit - un glm de Poisson ajustado.
# ES: Salida: una lista con nombre de estadísticos, cada uno NA si no se
#   puede calcular.
# -----------------------------------------------------------------------------
.al_count_dispersion <- function(fit) {
    y <- fit$y
    mu <- stats::fitted(fit)
    df_res <- fit$df.residual

    pearson <- sum(stats::residuals(fit, type = "pearson")^2)
    deviance <- fit$deviance

    out <- list(
        pearson = pearson, deviance = deviance, df = df_res,
        pearson_ratio = if (df_res > 0) pearson / df_res else NA_real_,
        deviance_ratio = if (df_res > 0) deviance / df_res else NA_real_,
        pearson_p = if (df_res > 0) stats::pchisq(pearson, df_res, lower.tail = FALSE) else NA_real_,
        deviance_p = if (df_res > 0) stats::pchisq(deviance, df_res, lower.tail = FALSE) else NA_real_,
        dl_z = NA_real_, dl_p = NA_real_,
        ct_alpha = NA_real_, ct_z = NA_real_, ct_p = NA_real_
    )

    denom <- sqrt(2 * sum(mu^2))
    if (is.finite(denom) && denom > 0) {
        out$dl_z <- sum((y - mu)^2 - y) / denom
        out$dl_p <- stats::pnorm(out$dl_z, lower.tail = FALSE)
    }

    aux <- ((y - mu)^2 - y) / mu
    ct <- tryCatch(summary(stats::lm(aux ~ 0 + mu))$coefficients, error = function(e) NULL)
    if (!is.null(ct) && nrow(ct) == 1 && is.finite(ct[1, 2]) && ct[1, 2] > 0) {
        out$ct_alpha <- ct[1, 1]
        out$ct_z <- ct[1, 1] / ct[1, 2]
        out$ct_p <- stats::pnorm(out$ct_z, lower.tail = FALSE)
    }

    out
}

# -----------------------------------------------------------------------------
# Likelihood ratio test of Poisson against negative binomial.
# ES: Prueba de razón de verosimilitudes de Poisson frente a binomial
# negativa.
#
# The Poisson model is the negative binomial with alpha = 1/theta = 0, a
# value on the boundary of the parameter space, so the LR statistic follows
# a 50:50 mixture of a point mass at 0 and chi-square(1) rather than
# chi-square(1) itself (Self & Liang, 1987). The p-value is therefore half
# the usual chi-square(1) tail.
#
# ES: El modelo de Poisson es la binomial negativa con alfa = 1/theta = 0,
# un valor en la frontera del espacio de parámetros, así que el estadístico
# de razón de verosimilitudes sigue una mezcla 50:50 de una masa en 0 y una
# chi-cuadrado(1), no la chi-cuadrado(1) misma (Self & Liang, 1987). Por eso
# el valor p es la mitad de la cola habitual de chi-cuadrado(1).
#
# Input: pois, nb - the two fitted models (nb may be NULL).
# Output: list(lr, p), NA when the negative binomial fit is not available.
# ES: Entrada: pois, nb - los dos modelos ajustados (nb puede ser NULL).
# ES: Salida: list(lr, p), NA si el ajuste binomial negativo no está.
# -----------------------------------------------------------------------------
.al_count_lr_test <- function(pois, nb) {
    if (is.null(pois) || is.null(nb))
        return(list(lr = NA_real_, p = NA_real_))
    lr <- 2 * (as.numeric(stats::logLik(nb)) - as.numeric(stats::logLik(pois)))
    if (!is.finite(lr))
        return(list(lr = NA_real_, p = NA_real_))
    lr <- max(lr, 0)
    list(lr = lr, p = 0.5 * stats::pchisq(lr, df = 1, lower.tail = FALSE))
}

# -----------------------------------------------------------------------------
# Expected number of zeros, and van den Broek's score test.
# ES: Número esperado de ceros y prueba de puntuación de van den Broek.
#
# The expected zeros are the sum over cases of each model's fitted
# probability of a zero: exp(-mu) for Poisson and (theta / (theta +
# mu))^theta for the negative binomial. More observed zeros than expected
# is the first sign of zero inflation.
#
# van den Broek's (1995) score test compares the Poisson model with a
# zero-inflated Poisson model:
#   S = [sum((1{y = 0} - p0) / p0)]^2 /
#       [sum((1 - p0) / p0) - mu' X (X' W X)^-1 X' mu],  W = diag(mu),
# with p0 = exp(-mu); S follows chi-square(1) under the Poisson model. The
# test is only defined against the Poisson model: overdispersion alone also
# produces extra zeros, so a significant result must be read together with
# the negative binomial's expected zeros.
#
# ES: Los ceros esperados son la suma, sobre los casos, de la probabilidad
# ajustada de un cero en cada modelo: exp(-mu) para Poisson y
# (theta / (theta + mu))^theta para la binomial negativa. Observar más ceros
# de los esperados es la primera señal de inflación de ceros.
#
# La prueba de puntuación de van den Broek (1995) compara el modelo de
# Poisson con un modelo de Poisson inflado en ceros; S sigue una
# chi-cuadrado(1) bajo el modelo de Poisson. La prueba solo está definida
# frente al modelo de Poisson: la sobredispersión por sí sola también
# produce ceros de más, así que un resultado significativo debe leerse junto
# con los ceros esperados por la binomial negativa.
#
# Input: pois, nb - the fitted models (nb may be NULL).
# Output: list(observed, expected_pois, expected_nb, score, p).
# ES: Entrada: pois, nb - los modelos ajustados (nb puede ser NULL).
# ES: Salida: list(observed, expected_pois, expected_nb, score, p).
# -----------------------------------------------------------------------------
.al_count_zeros <- function(pois, nb = NULL) {
    y <- pois$y
    mu <- stats::fitted(pois)
    p0 <- exp(-mu)

    out <- list(
        observed = sum(y == 0),
        expected_pois = sum(p0),
        expected_nb = NA_real_,
        score = NA_real_,
        p = NA_real_
    )

    if (!is.null(nb)) {
        mu_nb <- stats::fitted(nb)
        out$expected_nb <- sum((nb$theta / (nb$theta + mu_nb))^nb$theta)
    }

    X <- tryCatch(stats::model.matrix(pois), error = function(e) NULL)
    if (!is.null(X) && all(p0 > 0)) {
        info <- tryCatch(solve(crossprod(X, X * mu)), error = function(e) NULL)
        if (!is.null(info)) {
            Xmu <- crossprod(X, mu)
            denom <- sum((1 - p0) / p0) - as.numeric(t(Xmu) %*% info %*% Xmu)
            num <- sum(((y == 0) - p0) / p0)
            if (is.finite(denom) && denom > 0) {
                out$score <- num^2 / denom
                out$p <- stats::pchisq(out$score, df = 1, lower.tail = FALSE)
            }
        }
    }

    out
}

# -----------------------------------------------------------------------------
# Observed and expected frequency of each count value.
# ES: Frecuencia observada y esperada de cada valor del recuento.
#
# Feeds the frequency plot: for each count k = 0..K, the observed number of
# cases with y = k and the number each model expects (the sum of the fitted
# probabilities of k). K stops at the 99th percentile of y (at most 40
# values) so a few extreme counts do not flatten the plot.
#
# ES: Alimenta el gráfico de frecuencias: para cada recuento k = 0..K, el
# número observado de casos con y = k y el número que espera cada modelo
# (la suma de las probabilidades ajustadas de k). K se detiene en el
# percentil 99 de y (como mucho 40 valores) para que unos pocos recuentos
# extremos no aplasten el gráfico.
# -----------------------------------------------------------------------------
.al_count_frequencies <- function(y, mu_pois, mu_nb = NULL, theta = NULL) {
    k_max <- min(max(y), max(1, ceiling(stats::quantile(y, 0.99, names = FALSE))), 39)
    k <- 0:k_max
    out <- data.frame(
        count = k,
        observed = vapply(k, function(v) sum(y == v), numeric(1)),
        poisson = vapply(k, function(v) sum(stats::dpois(v, mu_pois)), numeric(1))
    )
    out$negbin <- if (!is.null(mu_nb) && !is.null(theta))
        vapply(k, function(v) sum(stats::dnbinom(v, size = theta, mu = mu_nb)), numeric(1))
    else
        NA_real_
    out
}

# -----------------------------------------------------------------------------
# Randomized quantile residuals (Dunn & Smyth, 1996).
# ES: Residuos cuantílicos aleatorizados (Dunn & Smyth, 1996).
#
# For a discrete outcome, u is drawn uniformly between F(y - 1) and F(y)
# under the fitted model and transformed with qnorm(); when the model is
# right the residuals are exactly standard normal, which makes their Q-Q
# plot readable for counts, unlike Pearson or deviance residuals. The seed
# is fixed so the same data always give the same plot.
#
# ES: Para un desenlace discreto, u se extrae de forma uniforme entre
# F(y - 1) y F(y) bajo el modelo ajustado y se transforma con qnorm();
# cuando el modelo es correcto, los residuos son exactamente normales
# estándar, lo que hace legible su gráfico Q-Q con recuentos, a diferencia
# de los residuos de Pearson o de devianza. La semilla es fija para que los
# mismos datos den siempre el mismo gráfico.
# -----------------------------------------------------------------------------
.al_count_quantile_residuals <- function(y, mu, theta = NULL, seed = 20260704) {
    if (is.null(theta)) {
        lower <- stats::ppois(y - 1, mu)
        upper <- stats::ppois(y, mu)
    } else {
        lower <- stats::pnbinom(y - 1, size = theta, mu = mu)
        upper <- stats::pnbinom(y, size = theta, mu = mu)
    }
    .al_local_seed(seed)
    u <- stats::runif(length(y), min = lower, max = upper)
    u <- pmin(pmax(u, 1e-12), 1 - 1e-12)
    stats::qnorm(u)
}

# -----------------------------------------------------------------------------
# Fit a count model of the chosen family.
# ES: Ajustar un modelo de recuento de la familia elegida.
#
# Used by the linearity check to refit the model with a spline term; quiet
# about glm.nb()'s convergence warnings, which the main fit already reports.
# ES: Lo usa la comprobación de linealidad para reajustar el modelo con un
# término spline; no repite las advertencias de convergencia de glm.nb(),
# que ya informa el ajuste principal.
# -----------------------------------------------------------------------------
.al_count_refit <- function(formula, data, family) {
    tryCatch(
        suppressWarnings(
            if (identical(family, "negbin"))
                MASS::glm.nb(formula, data = data)
            else
                stats::glm(formula, data = data, family = stats::poisson(link = "log"))
        ),
        error = function(e) NULL
    )
}

# -----------------------------------------------------------------------------
# Linearity on the log scale for one numeric predictor.
# ES: Linealidad en la escala logarítmica para un predictor numérico.
#
# A count model assumes each numeric predictor acts linearly on log(mu).
# The check replaces the predictor's linear term with a natural cubic
# spline of 3 degrees of freedom (splines::ns, part of base R) and compares
# the two fits with a likelihood ratio test on 2 df (Harrell, 2015). A
# predictor with fewer than 5 distinct values is not tested, since a
# 3-df spline cannot be fitted meaningfully.
#
# ES: Un modelo de recuento supone que cada predictor numérico actúa de
# forma lineal sobre log(mu). La comprobación reemplaza el término lineal
# del predictor por un spline cúbico natural de 3 grados de libertad
# (splines::ns, parte de R base) y compara ambos ajustes con una prueba de
# razón de verosimilitudes de 2 gl (Harrell, 2015). Un predictor con menos
# de 5 valores distintos no se prueba, porque no se puede ajustar con
# sentido un spline de 3 gl.
#
# Inputs: fit - the fitted model being diagnosed; rhs_terms - the
#   right-hand side terms (already backtick-quoted); predictor - the
#   quoted name of the term to replace; data, family, offset_term - as in
#   the main fit.
# Output: list(lr, df, p, reason); reason is a code when not computable.
# ES: Entradas: fit - el modelo ajustado que se diagnostica; rhs_terms - los
#   términos del lado derecho (ya entre comillas invertidas); predictor - el
#   nombre citado del término a reemplazar; data, family, offset_term - como
#   en el ajuste principal.
# ES: Salida: list(lr, df, p, reason); reason es un código si no se puede
#   calcular.
# -----------------------------------------------------------------------------
.al_count_spline_test <- function(fit, rhs_terms, predictor, raw_x, data, family, offset_term = NULL) {
    if (length(unique(raw_x)) < 5)
        return(list(lr = NA_real_, df = 2, p = NA_real_, reason = "fewvalues"))

    spline_terms <- rhs_terms
    spline_terms[spline_terms == predictor] <- paste0("splines::ns(", predictor, ", df = 3)")
    rhs <- paste(c(spline_terms, offset_term), collapse = " + ")
    alt <- .al_count_refit(stats::as.formula(paste("al_count_y_ ~", rhs)), data, family)
    if (is.null(alt))
        return(list(lr = NA_real_, df = 2, p = NA_real_, reason = "fit"))

    lr <- 2 * (as.numeric(stats::logLik(alt)) - as.numeric(stats::logLik(fit)))
    if (!is.finite(lr))
        return(list(lr = NA_real_, df = 2, p = NA_real_, reason = "fit"))
    lr <- max(lr, 0)
    list(lr = lr, df = 2, p = stats::pchisq(lr, df = 2, lower.tail = FALSE), reason = NULL)
}
