# -----------------------------------------------------------------------------
# AssumptionsLab
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
# GNU General Public License v3.0 or later; see LICENSE.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# countCheck verification.
# ES: Verificación de countCheck.
#
# Part 1 checks the numbers of R/count-helpers.R against published results
# and independent formulas: the negative binomial fit of MASS::quine
# (Venables & Ripley, 2002, theta = 1.275 for Days ~ Eth + Sex + Age + Lrn)
# and the closed forms of each statistic.
#
# Part 2 is a small Monte Carlo study of the size and power of the
# decisions countCheck reports: the Dean-Lawless and Cameron-Trivedi
# overdispersion tests, the boundary-corrected likelihood ratio test, van
# den Broek's zero-inflation score test and the 3-df spline linearity test.
# Each cell has `n_rep` replications (default 1000, seed 20261123); the
# Monte Carlo margin of a rejection rate near .05 is about +/-1.4 points.
#
# ES: La parte 1 comprueba los números de R/count-helpers.R frente a
# resultados publicados y fórmulas independientes. La parte 2 es un estudio
# Monte Carlo breve del tamaño y la potencia de las decisiones que informa
# countCheck.
#
# Run from the repository root:  Rscript validation/countcheck_verification.R
# Writes validation/countcheck_verification_results.csv.
# -----------------------------------------------------------------------------

source("R/count-helpers.R")
suppressPackageStartupMessages(library(MASS))

n_rep <- as.integer(Sys.getenv("AL_COUNT_REPS", "1000"))

# -----------------------------------------------------------------------------
# Part 1. Published values and closed forms.
# ES: Parte 1. Valores publicados y formas cerradas.
# -----------------------------------------------------------------------------
d <- quine
d$al_count_y_ <- d$Days
fits <- .al_count_fit(al_count_y_ ~ Eth + Sex + Age + Lrn, d)
stopifnot(abs(fits$nb$theta - 1.275) < 0.001)

mu <- fitted(fits$pois)
disp <- .al_count_dispersion(fits$pois)
stopifnot(
    all.equal(disp$pearson, sum((d$Days - mu)^2 / mu)),
    all.equal(disp$dl_z, sum((d$Days - mu)^2 - d$Days) / sqrt(2 * sum(mu^2)))
)
lr <- .al_count_lr_test(fits$pois, fits$nb)
stopifnot(all.equal(lr$p, 0.5 * pchisq(lr$lr, 1, lower.tail = FALSE)))
z <- .al_count_zeros(fits$pois, fits$nb)
stopifnot(all.equal(z$expected_nb, sum(dnbinom(0, size = fits$nb$theta, mu = fitted(fits$nb)))))
cat(sprintf("Part 1 passed: theta = %.4f, Pearson ratio = %.2f, LR = %.1f\n",
    fits$nb$theta, disp$pearson_ratio, lr$lr))

# -----------------------------------------------------------------------------
# Part 2. Size and power.
# ES: Parte 2. Tamaño y potencia.
# -----------------------------------------------------------------------------
one_rep <- function(n, scenario) {
    x <- rnorm(n)
    eta <- 0.5 + 0.4 * x + if (scenario == "nonlinear") 0.25 * x^2 else 0
    mu <- exp(eta)
    y <- switch(scenario,
        poisson = , nonlinear = rpois(n, mu),
        negbin = rnbinom(n, size = 2, mu = mu),
        zip = rpois(n, mu) * rbinom(n, 1, 0.8))
    dd <- data.frame(al_count_y_ = y, x = x)
    f <- .al_count_fit(al_count_y_ ~ x, dd)
    if (is.null(f$pois)) return(rep(NA, 5))
    ds <- .al_count_dispersion(f$pois)
    lr <- .al_count_lr_test(f$pois, f$nb)
    zs <- .al_count_zeros(f$pois)
    sp <- .al_count_spline_test(f$pois, "x", "x", x, dd, "poisson")
    c(dl = ds$dl_p < .05, ct = ds$ct_p < .05, lr = isTRUE(lr$p < .05),
      zip = isTRUE(zs$p < .05), spline = isTRUE(sp$p < .05))
}

set.seed(20261123)
cells <- expand.grid(scenario = c("poisson", "negbin", "zip", "nonlinear"), n = c(100, 300), stringsAsFactors = FALSE)
results <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    reps <- t(replicate(n_rep, one_rep(cells$n[i], cells$scenario[i])))
    data.frame(scenario = cells$scenario[i], n = cells$n[i], t(colMeans(reps, na.rm = TRUE)))
}))
results[, 3:7] <- round(results[, 3:7], 3)
print(results, row.names = FALSE)
write.csv(results, "validation/countcheck_verification_results.csv", row.names = FALSE)
