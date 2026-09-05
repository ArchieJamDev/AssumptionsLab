# Validation

Standalone scripts that evaluate the statistical properties of AssumptionsLab's
own decision logic, as opposed to unit tests (`tests/testthat/`), which check
that the module's code runs correctly and produces the expected numbers.

## `normality_groupcheck_mc.R`

Monte Carlo comparison of five candidate decision strategies for `groupCheck`'s
classical-vs-Welch two-sample-test recommendation, evaluating the module's
actual primary-plus-battery rule against a single primary test, a majority
vote across the battery, a graphical-inspection proxy, and always using a
robust test without diagnosis. 32 cells (4 distribution families x 4
sample-size pairs x 2 true effect sizes), 5,000 replications each, seed
`20260905`.

**Scope**: this evaluates only the normality battery as `groupCheck` applies
it to a raw (not residual) variable in a two-group comparison. It does not
evaluate the other eight submodules, `regCheck`'s/`pathCheck`'s residual-based
normality checks, or the homoscedasticity/linearity/independence batteries.

Run with `Rscript normality_groupcheck_mc.R` (requires the `nortest` package;
~35-40 minutes on a single core). Reproduces `normality_groupcheck_mc_results.csv`,
already included here.

Reported in: Chacón, A. (2026). "AssumptionsLab: Pedagogical Accompaniment and
Graduated Evidence for Statistical Assumption Checking in Jamovi", Austrian
Journal of Statistics, Section "Monte Carlo Validation of the
Normality-Battery Rule".
