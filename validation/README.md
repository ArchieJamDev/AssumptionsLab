# Validation

Standalone scripts that evaluate the statistical properties of AssumptionsLab's
own decision logic, as opposed to unit tests (`tests/testthat/`), which check
that the module's code runs correctly and produces the expected numbers.

## `normality_groupcheck_mc.R`

The nine-test normality battery is shared infrastructure, implemented once
and reused by five of AssumptionsLab's nine submodules, applied to two
structurally different targets: a raw observed variable (`groupCheck`,
`anovaCheck`, `relatedCheck`) or a fitted model's residuals (`regCheck`,
`pathCheck`). This script evaluates the raw-variable case, using the
concrete decision it feeds in `groupCheck`: the classical-vs-Welch
two-sample-test recommendation. It compares the module's actual
primary-plus-battery rule against a single primary test, a majority vote
across the battery, a graphical-inspection proxy, and always using a robust
test without diagnosis. 32 cells (4 distribution families x 4 sample-size
pairs x 2 true effect sizes), 5,000 replications each, seed `20260905`.

**Scope**: this evaluates only the normality battery's raw-variable case. It
does not evaluate the residual case (`regCheck`/`pathCheck`) or the
homoscedasticity/linearity/independence batteries. See
`normality_regcheck_mc.R` for the residual case.

## `normality_regcheck_mc.R`

Companion script for the residual case: `regCheck` checks normality of a
fitted model's residuals, not of an observed variable, and residuals are a
linear transformation of the true errors (`e_hat = M*e`), not a direct draw
from the same distribution, so the raw-variable results do not
automatically transfer. This evaluation required a real decision rule to
compare against, which `regCheck` did not yet have: `normality_problem` was
computed from the residual Shapiro-Wilk test but never consulted when
building the "Suggested decision" text, unlike the analogous flags for
heteroscedasticity, autocorrelation, severe multicollinearity, and
influence. The missing branch was added to `R/regcheck.b.R` (same `n<30`
CLT convention as `groupCheck`) before running this evaluation; a
regression test locks it in.

Compares five strategies for whether to trust the classical OLS t-test on a
target coefficient or fall back to a percentile bootstrap CI (R=99
resamples): (1) Shapiro-Wilk on residuals alone; (2) majority vote across
the 9-test residual battery; (3) `regCheck`'s actual (now fixed) rule; (4) a
graphical proxy; (5) always bootstrap. 24 cells (4 distribution families x
3 sample sizes x 2 true effect sizes), 500 replications each, seed
`20260906`. Both `n_rep` and the bootstrap resample count are much smaller
than the raw-variable script's, since a full bootstrap dominates
per-replication cost (~0.2s at R=99, vs. ~0.005s for a normality-test-only
replication) -- the resulting Monte Carlo margin is wider (~+/-2.2
percentage points) and the bootstrap itself coarser than a properly powered
one (R>=999) would be; both are stated limitations of this run, not of the
underlying question.

Run with `Rscript normality_regcheck_mc.R` (requires `nortest` and `boot`;
~40-45 minutes on a single core). Reproduces
`normality_regcheck_mc_results.csv`, already included here.

Run with `Rscript normality_groupcheck_mc.R` (requires the `nortest` package;
~35-40 minutes on a single core). Reproduces `normality_groupcheck_mc_results.csv`,
already included here.

Reported in: Chacón, A. (2026). "AssumptionsLab: Pedagogical Accompaniment and
Graduated Evidence for Statistical Assumption Checking in Jamovi", Austrian
Journal of Statistics, Section "Monte Carlo Validation of the
Normality-Battery Rule".
