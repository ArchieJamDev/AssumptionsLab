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
# countCheck tests.
# ES: Pruebas de countCheck.
#
# Three groups: (1) the numerical core in count-helpers.R against
# independent computations and against the published negative binomial
# fit of MASS::quine (Venables & Ripley, 2002, theta about 1.27); (2) the
# analysis on simulated data where the right answer is known (Poisson data
# show no overdispersion, negative binomial data do, a quadratic predictor
# fails the linearity check); (3) Category A rejections and edge cases.
#
# ES: Tres grupos: (1) el núcleo numérico de count-helpers.R frente a
# cálculos independientes y frente al ajuste binomial negativo publicado de
# MASS::quine (Venables & Ripley, 2002, theta cercano a 1.27); (2) el
# análisis con datos simulados cuya respuesta se conoce (datos de Poisson
# sin sobredispersión, datos binomiales negativos con ella, un predictor
# cuadrático que no supera la comprobación de linealidad); (3) rechazos de
# Categoría A y casos límite.
# -----------------------------------------------------------------------------

count_sim <- function(n = 300, seed = 20261123, size = NULL, quad = 0) {
    set.seed(seed)
    d <- data.frame(
        x = stats::rnorm(n),
        g = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
        time = stats::runif(n, 0.5, 3)
    )
    mu <- d$time * exp(0.3 + 0.4 * d$x + quad * d$x^2 + 0.3 * (d$g == "b"))
    d$y <- if (is.null(size)) stats::rpois(n, mu) else stats::rnbinom(n, size = size, mu = mu)
    d
}

row_value <- function(table, key, col) {
    table$getCell(rowKey = key, col)$value
}

test_that("count helpers reproduce independent computations", {
    d <- count_sim(size = 2)
    d$al_count_y_ <- d$y
    fits <- AssumptionsLab:::.al_count_fit(al_count_y_ ~ x + g, d)
    mu <- fitted(fits$pois)

    disp <- AssumptionsLab:::.al_count_dispersion(fits$pois)
    expect_equal(disp$pearson, sum((d$y - mu)^2 / mu))
    expect_equal(disp$dl_z, sum((d$y - mu)^2 - d$y) / sqrt(2 * sum(mu^2)))
    aux <- ((d$y - mu)^2 - d$y) / mu
    ct <- summary(lm(aux ~ 0 + mu))$coefficients
    expect_equal(disp$ct_z, unname(ct[1, 1] / ct[1, 2]))

    lr <- AssumptionsLab:::.al_count_lr_test(fits$pois, fits$nb)
    expect_equal(lr$lr, 2 * (as.numeric(logLik(fits$nb)) - as.numeric(logLik(fits$pois))))
    expect_equal(lr$p, 0.5 * pchisq(lr$lr, 1, lower.tail = FALSE))

    z <- AssumptionsLab:::.al_count_zeros(fits$pois, fits$nb)
    expect_equal(z$observed, sum(d$y == 0))
    expect_equal(z$expected_pois, sum(dpois(0, mu)))
    expect_equal(z$expected_nb, sum(dnbinom(0, size = fits$nb$theta, mu = fitted(fits$nb))))
})

test_that("van den Broek's score test reduces to its intercept-only form", {
    set.seed(7)
    y <- rpois(150, 1.2)
    fit <- glm(y ~ 1, family = poisson)
    lambda <- mean(y)
    p0 <- exp(-lambda)
    n <- length(y)
    expected <- (sum(y == 0) / p0 - n)^2 / (n * (1 - p0) / p0 - n * lambda)
    expect_equal(AssumptionsLab:::.al_count_zeros(fit)$score, expected)
})

test_that("randomized quantile residuals are reproducible and normal under the true model", {
    set.seed(3)
    mu <- exp(rnorm(2000, 0.5, 0.5))
    y <- rpois(2000, mu)
    r1 <- AssumptionsLab:::.al_count_quantile_residuals(y, mu)
    r2 <- AssumptionsLab:::.al_count_quantile_residuals(y, mu)
    expect_identical(r1, r2)
    expect_lt(abs(mean(r1)), 0.1)
    expect_lt(abs(sd(r1) - 1), 0.1)
})

test_that("countCheck reproduces the negative binomial fit of MASS::quine", {
    res <- AssumptionsLab::countCheck(data = MASS::quine, dep = "Days",
        factors = c("Eth", "Sex", "Age", "Lrn"), family = "negbin")
    theta <- row_value(res$modelComparison, "nb", "theta")
    expect_equal(theta, 1.2749, tolerance = 1e-3)
    ref <- MASS::glm.nb(Days ~ Eth + Sex + Age + Lrn, data = MASS::quine)
    expect_equal(row_value(res$rateRatios, "irr_2", "irr"), unname(exp(coef(ref)[2])), tolerance = 1e-6)
    expect_lt(row_value(res$dispersion, "pearson", "p"), 0.001)
    expect_gt(row_value(res$dispersion, "pearsonNb", "p"), 0.05)
})

test_that("Poisson data show no overdispersion and negative binomial data do", {
    pois <- AssumptionsLab::countCheck(data = count_sim(), dep = "y",
        covs = "x", factors = "g", exposure = "time")
    expect_gt(row_value(pois$dispersion, "dl", "p"), 0.05)
    expect_lt(abs(row_value(pois$dispersion, "pearson", "ratio") - 1), 0.25)

    nb <- AssumptionsLab::countCheck(data = count_sim(size = 1.5), dep = "y",
        covs = "x", factors = "g", exposure = "time")
    expect_lt(row_value(nb$dispersion, "dl", "p"), 0.001)
    expect_lt(row_value(nb$dispersion, "lr", "p"), 0.001)
    expect_true(grepl("overdispersion", nb$notes$content, fixed = TRUE))
})

test_that("the exposure offset is used, on either scale", {
    d <- count_sim()
    raw <- AssumptionsLab::countCheck(data = d, dep = "y", covs = "x", exposure = "time")
    d$logtime <- log(d$time)
    logged <- AssumptionsLab::countCheck(data = d, dep = "y", covs = "x",
        exposure = "logtime", exposureScale = "log")
    ref <- glm(y ~ x + offset(log(time)), data = d, family = poisson)
    expect_equal(row_value(raw$rateRatios, "irr_2", "irr"), unname(exp(coef(ref)[2])))
    expect_equal(row_value(logged$rateRatios, "irr_2", "irr"), unname(exp(coef(ref)[2])))
})

test_that("a quadratic predictor fails the spline linearity check", {
    res <- AssumptionsLab::countCheck(data = count_sim(quad = 0.3), dep = "y",
        covs = "x", exposure = "time")
    expect_lt(row_value(res$linearity, "lin_x", "p"), 0.01)
    lin <- AssumptionsLab::countCheck(data = count_sim(), dep = "y",
        covs = "x", exposure = "time")
    expect_gt(row_value(lin$linearity, "lin_x", "p"), 0.01)
})

test_that("countCheck rejects outcomes that are not counts", {
    d <- count_sim()
    d$neg <- d$y - 1
    d$frac <- d$y + 0.5
    d$zero <- 0
    expect_error(AssumptionsLab::countCheck(data = d, dep = "neg", covs = "x"), "negative")
    expect_error(AssumptionsLab::countCheck(data = d, dep = "frac", covs = "x"), "non-integer")
    expect_error(AssumptionsLab::countCheck(data = d, dep = "zero", covs = "x"), "nothing to model")
    d$badtime <- d$time - 1
    expect_error(AssumptionsLab::countCheck(data = d, dep = "y", covs = "x", exposure = "badtime"), "logarithm")
})

test_that("countCheck runs without predictors and with awkward names", {
    d <- count_sim()
    expect_no_error(AssumptionsLab::countCheck(data = d, dep = "y"))
    names(d)[names(d) == "x"] <- "Income (2024)"
    res <- AssumptionsLab::countCheck(data = d, dep = "y", covs = "Income (2024)", factors = "g")
    expect_true(is.finite(row_value(res$linearity, "lin_Income (2024)", "p")))
    expect_equal(res$rateRatios$getCell(rowKey = "irr_2", "predictor")$value, "Income (2024)")
})

test_that("countCheck tolerates missing values and a constant covariate", {
    d <- count_sim()
    d$x[c(2, 5, 9)] <- NA
    d$const <- 1
    expect_no_error(AssumptionsLab::countCheck(data = d, dep = "y", covs = c("x", "const")))
})
