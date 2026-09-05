# AssumptionsLab Test Dataset

`assumptionslab_test_data.csv` — a survey/financial-behavior dataset
(n = 450) translated and augmented by
[`prepare_dataset.R`](../data-raw/prepare_dataset.R) from an original
Spanish-language export, built to exercise every currently implemented
AssumptionsLab module with real relationships in the data rather than
pure noise.

This lives in `data/` and is registered under `datasets:` in
`jamovi/0000.yaml`, jamovi's standard convention for a module's bundled
example data — inside jamovi Desktop it shows up under **File → Open →
Data Library → AssumptionsLab** once the module is installed, no manual
file hunting required.

**Unlike PeopleLab's `peoplelab_test_data.csv`, this file is mostly not
simulated from scratch.** Every substantive value in the 26 columns
translated from the original export — scores, categories, dates,
time-series levels — is copied verbatim from that export (kept untouched
at
[`../data-raw/assumptionslab_raw_source.csv`](../data-raw/assumptionslab_raw_source.csv)).
Four things were changed on top of that: (1) column names and category
labels were translated from Spanish to English, (2) ~3% MCAR missingness
was injected into six columns, (3) a handful of deliberate outliers were
injected, and (4) one entirely new column, `investment_preference`, was
simulated from a multinomial-logit model to give `multCheck` a dependent
variable to test (see its section below) — all four reproducible under a
fixed seed (`set.seed(20260904)`). Because the 26 translated columns'
relationships were not engineered to a target effect size, this file
documents what a run **currently finds** on them, not a guaranteed
recoverable signal — re-running the analyses below after any edit to the
raw source may change those numbers. `investment_preference` is the
exception: its relationships *were* deliberately calibrated (see its
section below for the target pseudo-R²/accuracy) precisely because
nothing to calibrate against existed in the original export.

To regenerate it (identical output, same seed):

```bash
Rscript data-raw/prepare_dataset.R
```

## Data-quality features baked in on purpose

- **~3% missing values** (missing completely at random) in `anxiety`,
  `wellbeing`, `purchase_intention`, `need_for_cognition`, `resilience`,
  and `posttest_score_2`. The last one deliberately breaks some
  `pretest_score`/`posttest_score_1`/`posttest_score_2` triplets, so
  `relatedCheck` is exercised on genuinely incomplete repeated measures,
  not just complete ones.
- **A handful of deliberate outliers/edge cases**: two participants with
  an implausibly high `anxiety` (95.4, 91.8, against a natural range of
  roughly 10–58), two with an implausibly low `purchase_intention` (6.5,
  9.2, against a natural range of roughly 44–90), and two with a
  paired-scores "backslide" (`posttest_score_1`/`posttest_score_2` far
  below `pretest_score`, e.g. pretest 8 → posttest 1/1) — a realistic
  edge case for a repeated-measures influence diagnostic to flag.
- **Grouping/identifier columns and all eight time-series columns are
  left completely untouched** (no injected missingness, no outliers, and
  time-series row order is preserved), so `timeCheck` always exercises
  the clean-signal path deterministically.

## Variables

### Identifiers & demographics

| Column | Type | Description |
|---|---|---|
| `id` | integer | Row identifier. Not an analysis variable. |
| `date` | date | Survey/observation date, 2024-01-08 to 2025-12-30. Not a time index for the `*_series` columns below (those are separate simulated series in row order) — only relevant if a module needs a date variable of its own. |
| `gender` | nominal (2 levels) | Female / Male, balanced 225/225. `groupCheck`'s `group` candidate. |
| `age_range` | nominal, 5 brackets | 18-22 / 23-27 / 28-32 / 33-37 / 38-42. `anovaCheck`'s `factors` candidate (unbalanced group sizes, 48–147). |
| `education_level` | **ordinal**, 3 levels | Low / Medium / High (in that order — see `prepare_dataset.R`'s note on why the CSV import order matters). `ordCheck`'s `dep` candidate, or `anovaCheck`/`regCheck` factor. |
| `investment_experience` | nominal (2 levels) | No / Yes (302 / 148). `logCheck`'s binary `dep` candidate. |
| `savings_intention` | **ordinal**, 3 levels | Low / Medium / High (206 / 171 / 73). `ordCheck`'s primary `dep` candidate. |

### Module — Independent Groups (`groupCheck`)

| Column | Role |
|---|---|
| `anxiety` | `dep` — continuous, ~3% missing, 2 injected high-outlier cases. |
| `gender` | `group`. |

**Currently finds:** a significant Welch two-sample difference (Female
mean 27.9 vs. Male mean 25.0; t = 2.99, df = 432.6, p = .003) — a real
effect to detect, plus the two injected outliers (95.4, 91.8) to test
whether the outlier-flagging plot catches them regardless of which
group they land in.

### Module — Related Groups (`relatedCheck`)

| Column | Role |
|---|---|
| `pretest_score`, `posttest_score_1`, `posttest_score_2` | `measures` — three related integer measurements (course/intervention pre-post design). `posttest_score_2` has ~3% MCAR missingness. |

**Currently finds:** a significant, monotonic improvement across the
three measurements (means 8.02 → 9.25 → 10.22; paired t pretest vs.
posttest_1: t = 24.99, p < 2e-16), plus two deliberately implausible
"backslide" cases (pretest 8/7 → posttest 1/1) that should surface as
influential in the paired-difference diagnostics.

### Module — ANOVA/ANCOVA (`anovaCheck`)

| Column | Role |
|---|---|
| `purchase_intention` | `dep` — continuous, ~3% missing, 2 injected low-outlier cases. |
| `education_level`, `age_range` | `factors` candidates. |
| `anxiety`, `neuroticism` | `covs` candidates (for ANCOVA). |

**Currently finds:** a significant one-way effect of `education_level`
on `purchase_intention` (F(2, 433) = 3.32, p = .037).

### Module — Simple and Multiple Regression (`regCheck`)

| Column | Role |
|---|---|
| `wellbeing` | `dep` — continuous, ~3% missing. Note: naturally ceiling-clustered near 70 (a bounded composite scale), which is itself a useful stress test for the normality-of-residuals diagnostics. |
| `anxiety`, `need_for_cognition`, `resilience`, `planning`, `comfort`, `neuroticism`, `nomophobia` | `covs` candidates. |
| `gender`, `education_level` | `factors` candidates. |

**Currently finds:** R² = .247 with `anxiety` (p = .033) and
`resilience` (p < .001) as the significant predictors among the seven —
a realistic mix where not every covariate contributes, useful for
exercising the multicollinearity and coefficient-significance tables
rather than a uniformly "everything is significant" toy example.

### Module — Logistic Regression (`logCheck`)

| Column | Role |
|---|---|
| `investment_experience` | binary `dep` (`Yes` as event). |
| `anxiety`, `neuroticism`, `purchase_intention`, `resilience` | `covs` candidates. |
| `education_level`, `savings_intention` | `factors` candidates. |

**Currently finds:** the anxiety/neuroticism/purchase_intention model
has no individually significant predictor at the conventional .05 level
— a deliberately realistic "weak model" case alongside the stronger
signals in the other modules, good for exercising the module's
goodness-of-fit and calibration diagnostics without an inflated,
unrealistic pseudo-R².

### Module — Ordinal Logistic Regression (`ordCheck`)

| Column | Role |
|---|---|
| `savings_intention` | `dep` — ordinal, 3 levels, 206/171/73. |
| `anxiety`, `neuroticism`, `resilience` | `covs` candidates. |
| `gender`, `education_level` | `factors` candidates. |

**Currently finds:** `anxiety` closest to conventional significance
(t = -1.72) among the three predictors tried — again a realistic,
not-everything-significant case, and with 3 ordered levels it exercises
the proportional-odds (Brant) and goodness-of-fit diagnostics properly
(unlike a 2-level variable, which `ordCheck` itself warns is better
suited to `logCheck`).

### Module — Multinomial Logistic Regression (`multCheck`)

| Column | Type | Role |
|---|---|---|
| `investment_preference` | **nominal (unordered)**, 3 levels | `dep` — Fixed Income (reference level, first factor level) / Real Estate / Stocks. Simulated (not copied from the original export) from a multinomial-logit model driven by `neuroticism`, `investment_experience`, and `planning` — all three complete, so this column has no missingness of its own. |
| `neuroticism`, `planning` | `covs` candidates. |
| `investment_experience` | `factors` candidate. |

Every other categorical column in this file is either binary or already
ordinal (`gender`, `age_range`, `education_level`, `investment_experience`,
`savings_intention`) — before `investment_preference` was added, the
dataset had no unordered nominal variable with 3+ levels, so `multCheck`'s
dependent-variable slot had nothing to point at. Deliberately, *different*
predictors drive different categories (`planning` mainly drives Real
Estate vs. Fixed Income; `neuroticism` and `investment_experience` mainly
drive Stocks vs. Fixed Income) rather than one uniform effect
copy-pasted across categories, so the category-specific coefficient
table has a genuine story to tell.

**Currently finds** (`investment_preference ~ neuroticism +
investment_experience + planning`, McFadden pseudo-R² = .061,
classification accuracy 50.4% against a 33% chance baseline for 3
balanced-ish classes: 122/144/184): the Real Estate equation has
`neuroticism` (p = .002), `investment_experience` (p = .044), and
`planning` (p = .004) all significant; the Stocks equation has
`neuroticism` and `investment_experience` significant (p < .001 both) but
not `planning` (p = .27) — a realistic, non-uniform pattern across the two
non-reference categories, not an inflated toy example. The
Hausman-McFadden IIA test comes back "not computable" for every omitted
category on this data (a non-invertible covariance difference) — expected
and honestly reported, not itself evidence of a violation, since this
column was never simulated to break IIA.

### Module — Path Analysis & Structural Validation (`pathCheck`)

| Column | Role |
|---|---|
| `anxiety`, `wellbeing`, `purchase_intention`, `need_for_cognition`, `resilience`, `planning`, `comfort`, `neuroticism`, `nomophobia` | `vars` — nine continuous psychological/behavioral scales with genuine correlational structure to model as `relations`. |

**Currently finds** (pairwise correlations among the nine): the
strongest relationships are `anxiety`↔`resilience` (r = -.51),
`anxiety`↔`neuroticism` (r = .53), `anxiety`↔`comfort` (r = .42), and
`need_for_cognition`↔`planning` (r = .63) — a plausible path structure
such as `neuroticism`/`anxiety` → `resilience` → `wellbeing` alongside a
mostly-independent `need_for_cognition` → `planning` chain, rather than
either an unrealistic all-variables-correlated block or pure noise.

### Module — Time Series (`timeCheck`)

| Column | Role | Notes |
|---|---|---|
| `sarima_series` | `series`, `model = sarima` | Range ~42–81, trending/seasonal shape. |
| `arima_series` | `series`, `model = arima` | Range ~99–146, trending. |
| `ets_series` | `series`, `model = ets` | Range ~24–73. |
| `garch_series` | `series`, `model = garch` | Range ~-5.3–6.6, centered near 0 — return/volatility-shaped, unlike the other level series. |
| `var_series_1`, `var_series_2` | `series` (both together), `model = var` | Ranges ~46–54 and ~37–43 — a bivariate system. |
| `vecm_series_1`, `vecm_series_2` | `series` (both together), `model = vecm` | Ranges ~16–41 and ~2–28 — a second bivariate system, for the cointegration-oriented model. |
| `date` | optional `dateVar` | Not required — all eight series are already in the correct row order; supplying `date` only changes axis labeling. |

All eight series are complete (no injected missingness) and in original
row order, by design (see "Data-quality features" above) — `timeCheck`
should always run cleanly on these regardless of what missingness/
outlier work is being tested elsewhere in the file.

## Regenerating

Every column copied verbatim from `assumptionslab_raw_source.csv` (all
demographics, psychological/behavioral scales, paired scores, and time
series) is untouched by any statistical model — `prepare_dataset.R` only
translates their names/labels and injects missingness/outliers at fixed,
seeded row indices; to change *which* columns get missingness or
outliers, edit the `mcar_cols` vector or the three outlier-injection
blocks near the bottom of the script and re-run it. The one exception is
`investment_preference`, which the script *does* simulate from a
multinomial-logit model (see the `multCheck` section above) — to change
its behavior, edit the `eta_real_estate`/`eta_stocks` coefficients in the
script's "Simulate investment_preference" section instead.
