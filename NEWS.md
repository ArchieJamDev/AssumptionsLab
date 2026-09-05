# Validation scripts (unreleased, 2026-09-05)

## Bug fixes

- `validation/normality_groupcheck_mc.R` and `validation/normality_regcheck_mc.R`
  computed the shared nine-test normality battery's skewness, kurtosis, and
  Jarque-Bera statistics using `stats::sd(x)` (Bessel-corrected, 1/(n-1)),
  the same convention `shared-helpers.R` used *before* the 1.5.1 fix
  documented below — not the population standard deviation
  (`sqrt(mean((x-mean(x))^2))`) the module has actually used since that
  fix. The two validation scripts were therefore evaluating a stale,
  pre-1.5.1 version of the module's own statistics rather than its current
  behavior, an inconsistency an external AI-assisted review of this
  article's supporting code surfaced. `validation/normality_regcheck_mc_pilot_r999.R`
  already used the correct population-SD convention and needed no change.
  Both main scripts were corrected and rerun in full (5,000 replications
  x 32 cells for the raw-variable case; 500 replications x 24 cells for
  the residual case); the resulting `*_results.csv` files, and the
  article's Tables reporting them, have been regenerated and updated
  accordingly. The qualitative conclusions of both evaluations are
  unchanged; only the specific reported rates shifted, mostly by less
  than a percentage point.
- `validation/normality_groupcheck_mc_results.csv` also carried an
  `npair` column the current script does not produce, evidence the
  shipped CSV predated some structural edit to the script (e.g. the
  later addition of the (10,10) supplemental cell). Regenerating the CSV
  from the current script resolves this; script and results file now
  match column-for-column.

# AssumptionsLab 1.5.2 (2026-09-05)

## Bug fixes

- `regCheck`: `normality_problem` was computed from the residual
  Shapiro-Wilk test but never consulted when building the "Suggested
  decision" recommendation text, unlike the analogous flags for
  heteroscedasticity, autocorrelation, severe multicollinearity, and
  influence, each of which already gated its own branch. Added the
  missing branch: when residuals are non-normal, n < 30, and no other
  diagnostic is flagged, the module now recommends a bootstrap
  confidence interval, a nonparametric alternative, or explicit
  caution about classical inference — the same n < 30
  central-limit-theorem convention `groupCheck` already uses for its
  own normality-driven branch. A regression test locks in the new
  branch.

## Validation

- Added `validation/`, a standing location (separate from
  `tests/testthat/`, which checks code correctness, not statistical
  performance) for Monte Carlo scripts that evaluate the module's
  decision rules rather than implement them:
  - `normality_groupcheck_mc.R`: compares `groupCheck`'s
    classical-vs-Welch decision rule for a two-group comparison
    against four alternative strategies, for the normality battery's
    raw-variable case (32 cells, 5,000 replications each).
  - `normality_regcheck_mc.R`: the residual-case companion, using the
    `regCheck` fix above as its "actual rule" strategy (24 cells, 500
    replications, R=99 bootstrap resamples).
  - `normality_regcheck_mc_pilot_r999.R`: a pilot rerun of the single
    cell most sensitive to the residual script's bootstrap resample
    count (n=15, skewed errors), at R=999, confirming the qualitative
    finding is not an artifact of the smaller R=99 budget used
    elsewhere for computational tractability.

Developed to support the module's AJS article, "AssumptionsLab:
Pedagogical Accompaniment and Graduated Evidence for Statistical
Assumption Checking in Jamovi."

# AssumptionsLab 1.5.1 (2026-09-05)

## Bug fixes

- `logCheck`: fixed a crash in the influence-diagnostics loop when
  `cooks.distance()`/`hatvalues()` return `NaN` for a saturated or
  near-saturated fit, and a p-value/odds-ratio misalignment that
  occurred whenever a predictor was dropped as aliased from
  `summary()$coefficients`.
- `pathCheck`: fixed coefficient lookups never matching predictor names
  that require backtick-quoting in a formula (accents, spaces, symbols),
  which silently treated every such predictor as aliased and could crash
  the applied-interpretation narrative; also fixed several comparisons
  that crashed with too few complete cases to estimate a covariance
  matrix (Mahalanobis distance, leverage, Cook's D all return `NA` in
  that case, and `NA` inside an `if()` is a fatal R error, not a
  skipped case).
- `timeCheck`: `dateVar` and `exogenous` were missing `default: null` in
  `jamovi/timecheck.a.yaml`, making them required arguments with no
  default in the generated wrapper function instead of the optional
  variables the interface already documented them as.
- `texts.R`: corrected 10 mentions of a non-existent "mlogCheck" module
  (in both the pre-existing Proportional Odds Library entry and the new
  IIA entry below) to the module's real name, `multCheck`.
- `shared-helpers.R`'s manual Jarque-Bera/skewness/kurtosis formulas
  (used by `groupCheck`, `relatedCheck`, `anovaCheck`, `regCheck`, and
  `pathCheck`) divided the `mean()`-based (1/n) central moment by
  `stats::sd(x)`'s Bessel-corrected (1/(n-1)) standard deviation instead
  of the matching 1/n population value — a mixed-moment convention that
  understated both statistics and did not reproduce
  `tseries::jarque.bera.test()` (the reference implementation `timeCheck`
  itself calls) on identical data, confirmed independently while
  cross-validating the module's output against reference R
  implementations. Now uses the population (1/n) standard deviation
  throughout, matching `tseries` to at least 6 decimal places.

## New content

- Added an "Independence of Irrelevant Alternatives (IIA)" entry to the
  `assumptionLibrary` module, alongside 3 verified bibliographic
  references (Hausman & McFadden 1984; Small & Hsiao 1985; Cheng & Long
  2007) — `multCheck` had implemented the Hausman-McFadden IIA test since
  1.5.0 but never got the accompanying Library/Bibliography entries
  `ordCheck`'s Proportional Odds test received.

## Testing

- Added an automated edge-case robustness suite
  (`tests/testthat/`, one file per statistical module) covering
  non-ASCII/symbol variable names, single-row data sets, entirely
  missing columns, zero-variance predictors, and minimum-cardinality
  factors — 60 checks, run automatically in CI via a new, explicit
  "install as a plain R package + run testthat" step in
  `.github/workflows/jamovi-check.yml` (separate from `jmvtools::check()`,
  which never touches `tests/`).
- `DESCRIPTION`'s `Suggests:` gained `testthat (>= 3.3.0)` and `withr`.

## Documentation

- Corrected `ARCHITECTURE.md`/`DEVELOPER_GUIDE.md`'s directory-structure
  diagrams (a stale top-level `assets/` that never existed; missing
  `data/`, `data-raw/`, `inst/`) and removed "Path Analysis" from
  `ARCHITECTURE.md`'s list of future modules (already implemented as
  `pathCheck`).
- Updated `docs/AssumptionsLab_Documento_Maestro_v1_3.md`'s module list
  and status section, stale since before the module-expansion work in
  1.5.0.
- Fixed several internal contradictions in `data/README.md` (a
  "reserved for the planned multCheck" section for a module that's now
  built; a claim that no column is ever derived from a statistical model,
  written directly below the section describing `investment_preference`
  being generated by exactly that).

# AssumptionsLab 1.5.0 (2026-09-04)

## New analyses

- **Ordinal Logistic Regression (`ordCheck`)** — proportional-odds model
  diagnostics (`MASS::polr`): separation screening, linearity in the
  cumulative logit, the proportional-odds assumption (Brant test),
  goodness of fit (Lipsitz, Pulkstenis-Robinson), pseudo-R²,
  multicollinearity, correlation matrices, per-case influence
  diagnostics, and odds ratios with cutpoints.
- **Multinomial Logistic Regression (`multCheck`)** — `nnet::multinom`
  diagnostics for a 3+-level unordered nominal outcome: a category-by-
  predictor coefficients/odds-ratios table, the Independence of
  Irrelevant Alternatives assumption (Hausman-McFadden test), linearity
  in the logit as a joint likelihood-ratio test, goodness of fit,
  pseudo-R², a K×K classification table, multicollinearity, a
  predictors-only correlation matrix, and per-case influence
  diagnostics. Completes the Regression family alongside `regCheck`
  (linear), `logCheck` (binary logistic), and `ordCheck` (ordinal
  logistic).

With this release, `ordCheck` is also registered in `jamovi/0000.yaml`
for the first time — its files existed since a prior development pass
but were never wired into the module's analysis menu.

- Added a "Proportional Odds" entry to the `assumptionLibrary` module,
  accompanying `ordCheck`'s Brant-test diagnostic.

## Test dataset

- Added a bundled example dataset,
  `data/assumptionslab_test_data.csv` (n = 450), translated from the
  project's original Spanish-language survey export and augmented with
  reproducible (seeded) MCAR missingness and deliberate outliers so it
  exercises every implemented module's validation and missing-data/
  outlier handling, not just the happy path. Includes a new
  `investment_preference` column (simulated, unordered 3-level nominal)
  specifically for `multCheck`'s dependent-variable slot. Documented in
  full, module by module, in `data/README.md`; generated reproducibly by
  `data-raw/prepare_dataset.R`.

## Fixes and internal cleanup

- `DESCRIPTION`'s `Imports`/`Suggests` now match what the code actually
  uses: added `R6` (used by every module's class definition but
  previously undeclared), `MASS`, `nnet`; moved the genuinely optional,
  `requireNamespace`-guarded `brant` and `generalhoslem` to `Suggests`;
  removed `sandwich` and `reshape2`, which were declared but unused.
- Removed three stray, byte-identical duplicate files left over from
  editing (`R/regcheck_b.R`, `jamovi/ordcheck_a.yaml`,
  `jamovi/assumptionlibrarya.yaml`).
- Brought several modules into compliance with `CODE_STYLE.md`:
  `groupcheck.b.R` gained its file description, a Workflow section,
  bilingual section headers throughout, and had dead/duplicate code
  removed; `bibliography.b.R`'s file header no longer claims a
  Vancouver/IEEE citation-style selector that never existed (the module
  has always been APA-7th-only); `pathcheck.b.R` had non-descriptive
  variable names (`a`, `b`, `aux`, `x1`/`y1`) renamed; `logcheck.b.R`/
  `regcheck.b.R`/`bibliography.b.R` had Spanish-only section headers
  translated; `ordcheck.b.R`/`timecheck.b.R` gained their required
  Workflow sections.

# AssumptionsLab 1.0.0 (2026-08-31)

Initial development release. `assumptionLibrary`, `bibliography`,
`groupCheck`, `relatedCheck`, `anovaCheck`, `regCheck`, `logCheck`,
`pathCheck`, and `timeCheck`.
