# Confirmatory Monte Carlo check of the dCor-Pearson gap heuristic used by
# AssumptionsLab to flag a pair as possibly non-linear (Section 4 of the
# article): a pair is flagged when dCor(X,Y) - |r(X,Y)| exceeds 0.10. This is
# a single, narrowly scoped confirmatory demonstration -- one linear scenario
# and one canonical non-linear (quadratic) scenario, at one sample size,
# with no permutation testing -- not a general evaluation of dCor/copula
# entropy across the wider space of possible non-linear forms.
suppressPackageStartupMessages(library(energy))
set.seed(20260908)

n <- 119        # ties to the article's own applied-case sample size (Section 6)
n_rep <- 5000
gap_threshold <- 0.10

one_rep <- function(case) {
  x <- rnorm(n)
  if (case == "linear") {
    y <- x + rnorm(n)
  } else if (case == "quadratic") {
    y <- x^2 + rnorm(n)
  }
  r <- cor(x, y)
  d <- energy::dcor(x, y)
  c(r = r, dcor = d, gap = d - abs(r))
}

run_case <- function(case) {
  out <- t(sapply(seq_len(n_rep), function(i) one_rep(case)))
  data.frame(
    case = case,
    n = n,
    mean_abs_r = mean(abs(out[, "r"])),
    mean_dcor = mean(out[, "dcor"]),
    mean_gap = mean(out[, "gap"]),
    pct_flagged = mean(out[, "gap"] > gap_threshold) * 100
  )
}

results <- rbind(run_case("linear"), run_case("quadratic"))
write.csv(results, "linearity_dcor_mc_results.csv", row.names = FALSE)
print(results, digits = 4)
