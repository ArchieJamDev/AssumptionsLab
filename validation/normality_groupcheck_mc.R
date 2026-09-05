# -----------------------------------------------------------------------------
# AssumptionsLab
# Monte Carlo validation of the normality battery: raw-variable case.
#
# The nine-test normality battery is shared infrastructure, implemented once
# and reused by five of AssumptionsLab's nine submodules, applied to two
# structurally different targets: a raw observed variable (groupCheck,
# anovaCheck, relatedCheck) or a fitted model's residuals (regCheck,
# pathCheck). This script evaluates the raw-variable case, using the
# concrete decision it feeds in groupCheck: the classical-vs-Welch
# two-sample-test recommendation. Five candidate strategies are compared:
#   1. Shapiro-Wilk (primary test) alone.
#   2. Majority vote across all 18 test-group cells (9 tests x 2 groups).
#   3. groupCheck's actual four-branch decision rule, as coded.
#   4. A graphical-inspection proxy (|skewness|>1 or |excess kurtosis|>1).
#   5. Always Welch, without diagnosis.
#
# Scope: this validates ONLY the normality battery's raw-variable case. It
# says nothing about the battery's residual-based use (regCheck/pathCheck)
# or about the homoscedasticity/linearity/independence batteries -- those
# are not evaluated here and no claim is made about them. A companion
# script for the residual case is planned.
#
# Used in: "AssumptionsLab: Pedagogical Accompaniment and Graduated Evidence
# for Statistical Assumption Checking in Jamovi" (AJS), Section "Monte Carlo
# Validation of the Normality-Battery Rule".
#
# Requires: nortest. Reproducible with the fixed seed below.
# Runtime: ~35-40 minutes on a single core (32 cells x 5,000 replications).
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(nortest))
set.seed(20260905)

alpha <- 0.05
n_rep <- 5000
families <- c("normal", "skew", "heavytail", "contam")
n_pairs  <- list(c(10, 10), c(15, 15), c(35, 84), c(150, 150))  # (35,84) is the
                                                                 # paper's own
                                                                 # groupCheck example
deltas   <- c(0, 0.5)

# --- data-generating families (unit-ish variance, mean 0 before shifting) ---
rgen <- function(n, family) {
  switch(family,
    normal    = rnorm(n, 0, 1),
    skew      = { x <- rchisq(n, df = 3); (x - 3) / sqrt(6) },
    heavytail = { x <- rt(n, df = 3); x / sqrt(3) },
    contam    = { m <- rbinom(n, 1, 0.10); ifelse(m == 1, rnorm(n, 0, 3), rnorm(n, 0, 1)) }
  )
}

# --- nine-test normality battery on one vector; mirrors shared-helpers.R ---
normality_battery <- function(x) {
  n <- length(x); sx <- sd(x); m <- mean(x)
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
  pear_p <- tryCatch({
    k <- max(5, min(10, floor(n / 5)))
    breaks <- qnorm(seq(0, 1, length.out = k + 1), mean = m, sd = sx)
    breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
    obs <- table(cut(x, breaks)); exp <- rep(n / k, k)
    pchisq(sum((obs - exp)^2 / exp), df = k - 1, lower.tail = FALSE)
  }, error = function(e) NA)
  ps <- c(sw_p, li_p, ad_p, cvm_p, sf_p, pear_p, jb_p, skew_p, kurt_p)
  list(sw_p = sw_p, sig9 = sum(ps < alpha, na.rm = TRUE), skew = sk, kurt_excess = ku - 3)
}

levene_p <- function(x, y) {
  g <- factor(c(rep("a", length(x)), rep("b", length(y)))); v <- c(x, y)
  z <- abs(v - ave(v, g, FUN = median))
  summary(aov(z ~ g))[[1]][["Pr(>F)"]][1]
}
iqr_extreme_count <- function(x) {
  q <- quantile(x, c(.25, .75)); iqr <- diff(q)
  sum(x < q[1] - 3 * iqr | x > q[2] + 3 * iqr)
}

one_rep <- function(n1, n2, family, delta) {
  x <- rgen(n1, family); y <- rgen(n2, family) + delta
  bx <- normality_battery(x); by <- normality_battery(y)
  lev_p <- levene_p(x, y)
  total_extreme <- iqr_extreme_count(x) + iqr_extreme_count(y)
  n_model <- n1 + n2
  normality_significant   <- bx$sig9 + by$sig9          # out of 18
  homogeneity_significant <- as.integer(lev_p < alpha)
  primary_flag   <- (bx$sw_p < alpha) || (by$sw_p < alpha)
  majority_flag  <- normality_significant > 9
  graphical_flag <- (abs(bx$skew) > 1 || abs(bx$kurt_excess) > 1 ||
                      abs(by$skew) > 1 || abs(by$kurt_excess) > 1)

  # groupCheck's actual 2-group decision rule (groupcheck.b.R)
  al_branch <- if (homogeneity_significant == 0 && normality_significant == 0 && total_extreme == 0) {
    "classical"
  } else if (homogeneity_significant > 0) {
    "welch"
  } else if (n_model >= 30) {
    "classical"        # "classical, with Welch/Mann-Whitney as sensitivity checks"
  } else {
    "ambiguous"         # module names both options "depending on analytic priority"
  }

  run_test <- function(test) {
    if (test == "classical") t.test(x, y, var.equal = TRUE)$p.value
    else t.test(x, y, var.equal = FALSE)$p.value
  }

  strat1 <- if (primary_flag) "welch" else "classical"
  strat2 <- if (majority_flag) "welch" else "classical"
  strat3 <- al_branch
  strat4 <- if (graphical_flag) "welch" else "classical"
  # "ambiguous" is scored against the classical test (named first by the module),
  # while its incidence is tracked separately.
  p3 <- run_test(if (strat3 == "ambiguous") "classical" else strat3)

  c(reject1 = as.integer(run_test(strat1) < alpha),
    reject2 = as.integer(run_test(strat2) < alpha),
    branch3 = strat3,
    reject3 = as.integer(p3 < alpha),
    reject4 = as.integer(run_test(strat4) < alpha),
    reject5 = as.integer(run_test("welch") < alpha))
}

results <- list(); idx <- 1
for (fam in families) {
  for (np in n_pairs) {
    n1 <- np[1]; n2 <- np[2]
    for (delta in deltas) {
      cat(sprintf("[%s | n=(%d,%d) | delta=%.1f] running %d reps...\n", fam, n1, n2, delta, n_rep))
      out <- vector("list", n_rep)
      for (r in seq_len(n_rep)) out[[r]] <- one_rep(n1, n2, fam, delta)
      df <- do.call(rbind.data.frame, lapply(out, function(z) {
        data.frame(reject1 = as.integer(z["reject1"]), reject2 = as.integer(z["reject2"]),
                   branch3 = z["branch3"], reject3 = as.integer(z["reject3"]),
                   reject4 = as.integer(z["reject4"]), reject5 = as.integer(z["reject5"]))
      }))
      results[[idx]] <- data.frame(
        family = fam, n1 = n1, n2 = n2, delta = delta,
        rate1 = mean(df$reject1), rate2 = mean(df$reject2), rate3 = mean(df$reject3),
        pct_ambiguous3 = mean(df$branch3 == "ambiguous"),
        pct_classical3 = mean(df$branch3 == "classical"),
        pct_welch3     = mean(df$branch3 == "welch"),
        rate4 = mean(df$reject4), rate5 = mean(df$reject5))
      idx <- idx + 1
    }
  }
}

final <- do.call(rbind, results)
write.csv(final, "normality_groupcheck_mc_results.csv", row.names = FALSE)
print(final, digits = 3)
