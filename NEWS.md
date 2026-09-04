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
