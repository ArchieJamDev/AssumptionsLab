# Pilot re-run of the residual-normality Monte Carlo at a properly powered
# bootstrap resample count (R=999 instead of R=99), restricted to the cell
# the reviewer named (n=15, skewed errors), both delta=0 and delta=0.5, to
# check whether the Type-I-error pattern reported at R=99 reverses at a
# larger, conventional resample count. Same generative code as
# normality_regcheck_mc.R; only n_rep/n_boot/scope differ.
suppressPackageStartupMessages({ library(nortest); library(boot) })
set.seed(20260907)

alpha <- 0.05
n_rep <- 500
n_boot <- 999
n <- 15
family <- "skew"
deltas <- c(0, 0.5)

rerr <- function(n, family) {
  x <- rchisq(n, df = 3); (x - 3) / sqrt(6)
}

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

one_rep <- function(n, delta) {
  x1 <- rnorm(n); x2 <- rnorm(n)
  e <- rerr(n, family)
  y <- 1.0 * x1 + delta * x2 + e
  d <- data.frame(y = y, x1 = x1, x2 = x2)
  fit <- lm(y ~ x1 + x2, data = d)
  res <- residuals(fit)
  bat <- normality_battery(res)

  primary_flag   <- bat$sw_p < alpha
  majority_flag  <- bat$sig9 > 4
  graphical_flag <- abs(bat$skew) > 1 || abs(bat$kurt_excess) > 1
  al_rule_flag   <- primary_flag && (n < 30)

  classical_p <- summary(fit)$coefficients["x2", "Pr(>|t|)"]

  stat_fun <- function(data, idx) coef(lm(y ~ x1 + x2, data = data[idx, ]))["x2"]
  b <- boot::boot(d, stat_fun, R = n_boot)
  ci <- tryCatch(boot::boot.ci(b, type = "perc")$percent[4:5], error = function(e) c(NA, NA))
  boot_p <- as.integer(!(ci[1] <= 0 && ci[2] >= 0))

  decide <- function(flag) if (flag) boot_p else as.integer(classical_p < alpha)

  c(reject1 = decide(primary_flag),
    reject2 = decide(majority_flag),
    reject3 = decide(al_rule_flag),
    reject4 = decide(graphical_flag),
    reject5 = boot_p)
}

results <- list()
for (delta in deltas) {
  cat(sprintf("[skew | n=15 | delta=%.1f | R=999] running %d reps...\n", delta, n_rep))
  out <- vector("list", n_rep)
  for (r in seq_len(n_rep)) out[[r]] <- one_rep(n, delta)
  m <- do.call(rbind, out)
  results[[length(results) + 1]] <- data.frame(
    family = "skew", n = n, delta = delta, R_boot = n_boot,
    rate1 = mean(m[, "reject1"]), rate2 = mean(m[, "reject2"]),
    rate3 = mean(m[, "reject3"]), rate4 = mean(m[, "reject4"]),
    rate5 = mean(m[, "reject5"])
  )
}
final <- do.call(rbind, results)
write.csv(final, "normality_regcheck_mc_pilot_r999_results.csv", row.names = FALSE)
print(final, digits = 3)
