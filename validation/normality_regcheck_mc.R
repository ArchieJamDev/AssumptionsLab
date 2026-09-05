# -----------------------------------------------------------------------------
# AssumptionsLab
# Monte Carlo validation of the normality battery: residual case (regCheck).
#
# Companion to normality_groupcheck_mc.R (raw-variable case). This script
# evaluates the residual case: regCheck fits an OLS model Y = Xb + e and
# checks normality of the residuals e-hat, not of Y or X directly. Residuals
# are a linear transformation of the true errors (e-hat = M*e, M = I-H), so
# even when the true errors are drawn non-normal, the fitted residuals do
# not simply inherit that shape unmodified -- this is the structural
# difference from the raw-variable case that motivated a separate script.
#
# Five decision strategies, mirroring the raw-variable script's structure,
# for whether to trust classical OLS inference (coefficient p-values/CIs)
# on a target predictor, or fall back to a bootstrap-based inference:
#   1. Shapiro-Wilk (primary test) on residuals, alone.
#   2. Majority vote across all 9 tests in the residual battery.
#   3. regCheck's actual rule (fixed in R/regcheck.b.R for this validation):
#      non-normal residuals AND n<30 AND no other flagged problem -> bootstrap;
#      otherwise -> classical.
#   4. Graphical proxy (|skewness|>1 or |excess kurtosis|>1 on residuals).
#   5. Always bootstrap, without diagnosis.
#
# Downstream test: classical OLS t-test on one target coefficient vs. a
# nonparametric bootstrap percentile CI/test for the same coefficient.
#
# Scope: residual normality only, for a simple multiple-regression model.
# Says nothing about the raw-variable case (see normality_groupcheck_mc.R),
# about regCheck's other diagnostics (homoscedasticity, linearity,
# multicollinearity, influence), or about the other seven submodules.
#
# Requires: nortest, boot. Reproducible with the fixed seed below.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(nortest)
  library(boot)
})
set.seed(20260906)

alpha <- 0.05
# n_rep and n_boot are both much smaller than the raw-variable script's:
# each replication here includes a full bootstrap CI, which dominates
# runtime (benchmarked at ~0.2s/replication at n_boot=99, vs. ~0.005s for
# the raw-variable script's normality-test-only replications). At these
# settings the Monte Carlo margin is roughly +/-2.2 percentage points
# (SE for a proportion at n_rep=500), wider than the raw-variable script's
# +/-0.3-0.7pp -- report differences smaller than that with corresponding
# caution.
n_rep <- 500
n_boot <- 99
families <- c("normal", "skew", "heavytail", "contam")
n_sizes  <- c(15, 30, 150)       # small / CLT threshold / large
deltas   <- c(0, 0.5)            # true coefficient on the target predictor

# --- error-generating families (unit-ish variance) ---
rerr <- function(n, family) {
  switch(family,
    normal    = rnorm(n, 0, 1),
    skew      = { x <- rchisq(n, df = 3); (x - 3) / sqrt(6) },
    heavytail = { x <- rt(n, df = 3); x / sqrt(3) },
    contam    = { m <- rbinom(n, 1, 0.10); ifelse(m == 1, rnorm(n, 0, 3), rnorm(n, 0, 1)) }
  )
}

# --- nine-test normality battery (mirrors shared-helpers.R) ---
normality_battery <- function(x) {
  n <- length(x); sx <- sqrt(mean((x - mean(x))^2)); m <- mean(x)
  sw_p  <- tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
  li_p  <- tryCatch(nortest::lillie.test(x)$p.value, error = function(e) NA)
  ad_p  <- tryCatch(nortest::ad.test(x)$p.value, error = function(e) NA)
  cvm_p <- tryCatch(nortest::cvm.test(x)$p.value, error = function(e) NA)
  sf_p  <- tryCatch(nortest::sf.test(x)$p.value, error = function(e) NA)
  sk <- mean((x - m)^3) / sx^3
  ku <- mean((x - m)^4) / sx^4
  jb_val <- n/6 * (sk^2 + ((ku - 3)^2 / 4)); jb_p <- pchisq(jb_val, df = 2, lower.tail = FALSE)
  z_sk <- sk / sqrt(6 / n); skew_p <- 2 * pnorm(abs(z_sk), lower.tail = FALSE)
  kurt_p <- if (n >= 20) { z_ku <- (ku - 3) / sqrt(24 / n); 2 * pnorm(abs(z_ku), lower.tail = FALSE) } else NA
  pear_p <- tryCatch(nortest::pearson.test(x)$p.value, error = function(e) NA)
  ps <- c(sw_p, li_p, ad_p, cvm_p, sf_p, pear_p, jb_p, skew_p, kurt_p)
  list(sw_p = sw_p, sig9 = sum(ps < alpha, na.rm = TRUE), skew = sk, kurt_excess = ku - 3)
}

# --- one replication: fit OLS, diagnose residuals, decide, test target coef ---
one_rep <- function(n, family, delta) {
  x1 <- rnorm(n); x2 <- rnorm(n)          # two numeric predictors
  e  <- rerr(n, family)
  y  <- 1.0 * x1 + delta * x2 + e         # delta is the TRUE coefficient on x2
  d  <- data.frame(y = y, x1 = x1, x2 = x2)

  fit <- lm(y ~ x1 + x2, data = d)
  res <- residuals(fit)
  bat <- normality_battery(res)
  n_used <- n

  primary_flag   <- bat$sw_p < alpha
  majority_flag  <- bat$sig9 > 4                       # majority of 9
  graphical_flag <- abs(bat$skew) > 1 || abs(bat$kurt_excess) > 1
  al_rule_flag   <- primary_flag && (n_used < 30)      # regCheck's actual (fixed) rule

  classical_p <- summary(fit)$coefficients["x2", "Pr(>|t|)"]

  boot_p <- {
    stat_fun <- function(data, idx) coef(lm(y ~ x1 + x2, data = data[idx, ]))["x2"]
    b <- boot::boot(d, stat_fun, R = n_boot)
    ci <- tryCatch(boot::boot.ci(b, type = "perc")$percent[4:5], error = function(e) c(NA, NA))
    # two-sided percentile-bootstrap "significance": does the 95% CI exclude 0?
    as.integer(!(ci[1] <= 0 && ci[2] >= 0))
  }

  decide <- function(flag) if (flag) boot_p else as.integer(classical_p < alpha)

  c(reject1 = decide(primary_flag),
    reject2 = decide(majority_flag),
    reject3 = decide(al_rule_flag),
    reject4 = decide(graphical_flag),
    reject5 = boot_p,
    flag3   = as.integer(al_rule_flag))
}

results <- list(); idx <- 1
for (fam in families) {
  for (n in n_sizes) {
    for (delta in deltas) {
      cat(sprintf("[%s | n=%d | delta=%.1f] running %d reps...\n", fam, n, delta, n_rep))
      out <- vector("list", n_rep)
      for (r in seq_len(n_rep)) out[[r]] <- one_rep(n, fam, delta)
      m <- do.call(rbind, out)
      results[[idx]] <- data.frame(
        family = fam, n = n, delta = delta,
        rate1 = mean(m[, "reject1"]), rate2 = mean(m[, "reject2"]),
        rate3 = mean(m[, "reject3"]), rate4 = mean(m[, "reject4"]),
        rate5 = mean(m[, "reject5"]), pct_flag3 = mean(m[, "flag3"])
      )
      idx <- idx + 1
    }
  }
}

final <- do.call(rbind, results)
write.csv(final, "normality_regcheck_mc_results.csv", row.names = FALSE)
print(final, digits = 3)
