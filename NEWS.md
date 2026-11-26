# AssumptionsLab (development version)

## New analysis: Count and Rate Regression (Poisson / Negative Binomial)

- `countCheck` (menu Regression > Poisson / Negative Binomial) checks the
  assumptions of Poisson and negative binomial regression (`MASS::glm.nb`)
  for a count outcome, with an optional exposure entered as an offset
  (raw exposure, whose log is taken, or a variable already on the log
  scale), so the model describes rates.
- Overdispersion: Pearson and deviance dispersion ratios, the Dean-Lawless
  score test, the Cameron-Trivedi auxiliary regression and the
  Poisson-versus-negative-binomial likelihood ratio test with its boundary
  correction (half the chi-square tail), plus a table comparing both models
  (log-likelihood, AIC, BIC, theta and alpha) and a mean-variance plot.
- Excess zeros: observed against expected zeros under both models, van den
  Broek's score test against the Poisson model and a plot of observed
  against expected frequencies of each count.
- Residuals: Pearson, deviance and randomized quantile residuals (Dunn &
  Smyth, 1996), a residuals-versus-linear-predictor plot and a Q-Q plot of
  the quantile residuals.
- Linearity of each numeric predictor on the log scale (likelihood ratio
  test against a 3-df natural spline) with partial residual plots;
  generalized VIF; Cook's distance and leverage; incidence rate ratios with
  Wald intervals.
- The numerical core lives in `R/count-helpers.R`, free of jmvcore, and is
  checked against independent computations in
  `validation/countcheck_verification.R`.
- Eleven new sources in `jamovi/00refs.yaml` and `docs/Bibliography.md`.
- The Assumption Library gains a category, "Equidispersion and Excess
  Zeros".

## Second jamovi review (1.7.1)

- The Assumption Library is built only from native jamovi elements. Each
  comparison table is a native `Table` (columns declared in the r.yaml,
  rows added in `.init()`), so it takes the results theme, Copy Latex and
  a real grid when pasted. The prose is a `Text` element that carries each
  category's `refs:` directly; the Sources tables are gone. Every
  paragraph, list item and cell is its own entry in the translation
  catalogue, with no HTML or CSS around the words.
- `minApp` is now 28.3.0 (needed for `Text`), and `jmvcore (>= 28.3)`.
- Regression: the correlation plots follow the "Diagnostic plots" switch,
  so turning it off no longer leaves two empty frames; their controls are
  greyed out with the switch.
- Opening an analysis with nothing selected no longer shows an error: the
  analysis returns quietly and keeps the empty results skeleton.
  `reject()` stays for real problems (wrong variable type, too few cases,
  model that cannot be fitted).
- New "Explanations" checkbox (on by default) in every data analysis. Off,
  it hides the introduction and every fixed `*Guide` block for a compact
  report; the applied interpretations of the user's own results stay. It
  sits under a new "Output" heading at the top of the panel, which also
  takes the "Diagnostic plots" switch where that switch had a box of its
  own.
- Fixed seeds (permutation p-values, quantile residuals, plot jitter,
  trajectory sampling, spring layout) now restore the session's
  random-number state when they finish, through `.al_local_seed()`.
- References the text cites are now attached: Firth (1993), Peduzzi et
  al. (1996) and Zhang & Yu (1998) in Logistic Regression; Canova &
  Hansen, Osborn et al., Brown et al., Nyblom, Hyndman & Khandakar,
  Lütkepohl, Engle & Granger, Nelson and Glosten et al. in Time Series;
  Wright, Duncan, Baron & Kenny and Kozachenko & Leonenko in Path
  Analysis; Liu et al. (2009) in Ordinal and Multinomial; Maxwell &
  Delaney in ANOVA. The unused `small_1985` entry was removed, and five
  entries gained a DOI or publisher URL (Levene, 1960, has none to give).
- Polish: `plotStyle` removed from every `clearWith`; "Cramér-von Mises"
  with its accent; results titles "Independent Groups" and "Related
  Groups"; sentence case for control labels; Independent Groups'
  variable boxes restrict their types; counts and seeds are `Integer`
  options; Time Series declares `compilerMode: tame`; the Levene footnote
  reads "Requires at least two groups".

# AssumptionsLab 1.7.1 (2026-11-20)

## The Assumption Library cites like the other analyses

- Each category of the Assumption Library now ends with a native jamovi
  table, "Sources", that names each method with its author and year and
  carries the category's `refs:`. jamovi numbers these sources and lists
  them in its References section, as it does for the other analyses, and
  the category filter hides them with the rest of the category. jamovi
  draws references only for tables, never for the library's HTML text,
  which is why the library needed these tables.
- The library's introduction no longer links to `docs/Bibliography.md`;
  it explains where the references are shown instead.
- Two sources the library and Regression discuss were missing from
  `jamovi/00refs.yaml` and `docs/Bibliography.md`: Utts (1982), for the
  Rainbow test, and Wald & Wolfowitz (1940), for the runs test.
- New tests check that every Sources table is filled, follows the
  category filter and the language, and that every `refs:` key of the
  library exists in `jamovi/00refs.yaml`.

# AssumptionsLab 1.7.0 (2026-11-20)

## Report language follows jamovi

- Every analysis now writes its report in the language set in jamovi
  (English or Spanish), like the option panels already did. The
  per-analysis "Report Language" option (`reportLang`) is gone, so a saved
  `.omv` file opens in the reader's own language.
- Report text built in R is English inside `.()` and is translated through
  jamovi's catalog (`jamovi/i18n/es.po`, compiled to `inst/i18n/es.json`),
  using `.al_lang()` in `R/shared-helpers.R`. jmvcore creates a new
  translator for each run, so a language change reaches the next run; the
  earlier note that `.()` stayed fixed at the language of the R process
  was wrong (checked against jmvcore 2.7.38, as in MokkenLab).
- `R/texts.R` is now one English table, `.al_text_table(.)`, instead of
  separate English and Spanish copies (about 1,000 lines instead of 4,800).
- Result titles come only from the `.r.yaml` files; the code that
  overwrote them at run time was removed.
- Sentences are kept whole for translators (`sprintf(.("... %s ..."))`)
  instead of being joined from fragments, which also fixes several places
  where Spanish text appeared in English reports.
- New tests run analyses in Spanish, English and Spanish again and check
  that every text resolves and nothing is cached between runs.

## Lessons from the jamovi audits of MokkenLab and EGAnetLab

- **Permutation tests can be interrupted.** The dCor and copula entropy
  permutation tests call jamovi's checkpoint before each variable pair (in
  every analysis that runs them), so changing an option stops a long run
  instead of waiting for every remaining pair.
- **Whole paragraphs for translators.** Ten Time Series paragraphs (the
  introduction, the scope note, four model descriptions and four
  methodological foundations) were split into line-length fragments in
  `R/texts.R`; each is now a single `.()` entry. The report reads the same
  ("Assumptions/Conditions" loses a stray space). `catalog.pot`, `es.po`
  and `inst/i18n/es.json` updated (72 fragments replaced by 10 entries).
- The Time Series options box is titled "Time Series Model" (title case,
  like every other box).
- **Plot colours follow jamovi.** The module's own palette option
  (`plotPalette`: Okabe-Ito, viridis) is removed from all nine analyses;
  every plot uses the palette chosen in jamovi's settings, as jamovi asked
  of MokkenLab and EGAnetLab. Saved analyses that still carry the option
  open normally. `.al_plot_series_palette()` is gone with it.
- **Fix:** in ANOVA/ANCOVA, the residuals-vs-fitted plot failed when a
  LOESS or linear smoother was chosen (it called a palette helper that no
  longer existed); the smoother now takes the theme's accent colour.
- **Numbers in numeric columns.** p values, test statistics and
  correlations that were written into text columns are now numbers that
  follow jamovi's decimal and p-value settings and paste into spreadsheets
  as numbers. This covers:
  - the normality and homogeneity tables of Independent Groups;
  - the independence table of Regression;
  - the sphericity table of Related Groups;
  - the Mardia df of Path Analysis;
  - the p column of the Time Series diagnostics.
  In the Pearson/dCor gap tables, Pearson r and dCor are numbers with their
  significance stars in new *Sig. (r)* and *Sig. (dCor)* columns, like
  *Sig. (CE)*. A value that cannot be computed leaves an empty cell instead
  of "Not calc.", and its footnote explains why.
- **Results keep their place.** The following sections no longer appear
  or disappear when the analysis runs. Their visibility is now declared in
  the `.r.yaml` files, so jamovi lays them out before the results arrive:
  - the correlation sections (shown whenever there are numeric
    predictors);
  - multicollinearity;
  - sphericity in Related Groups;
  - the distribution, Q-Q and normal-curve plots of Independent Groups.

  When a section does not apply, it stays in place with its "Not
  applicable" row or note, for example sphericity with two measurements, a
  single predictor column, or a single numeric predictor in Multinomial
  Logistic Regression.
- **Greyed-out options.** Plot options are disabled until the plot they
  belong to is ticked, in ANOVA/ANCOVA, Logistic, Multinomial, Ordinal and
  Linear Regression, and Path Analysis, as Independent Groups already did.
- **Fix:** two cells of Linear Regression ("No aplicable" in the linearity
  table and "VIF / tolerancia" in multicollinearity) were hard-coded in
  Spanish and now go through the translation catalog.
- **Correlation matrices are numeric.** The Pearson and dCor matrices keep
  their APA layout (numbered variables, lower triangle), but each cell is
  now a number. Its significance stars are attached with jamovi's own table
  symbols, as in jamovi's Correlation Matrix, so the matrices follow
  jamovi's decimal settings and paste into spreadsheets as numbers.
- **Related Groups lays out its descriptives first.** The *Descriptives by
  measurement* table has one row per selected measurement from the start
  (`rows: (measures)`), so it no longer grows while the analysis runs.
- **Native notice in Time Series.** When the diagnostics cannot run (no
  series, a VAR/VECM with a single series, or missing R packages), a jamovi
  notice explains why above the diagnostics. The diagnostics tables stay in
  place, and the notice no longer appears or disappears as an Html block.
  The missing-packages message is now one whole sentence for translators.
- The description of Assumption Library says that entries follow jamovi's
  language, instead of "available in English or Spanish".
- **Shorter `.run()` methods.** Each analysis's `.run()` (700 to 2,400
  lines) now prepares the data and the model, then calls one private
  method per report section (for example `.checkNormality()`,
  `.checkMulticollinearity()`, `.fillCorrelations()`). Each step receives
  what it reads as arguments and returns what later steps need; the code
  inside each step did not change, so the reports are the same. The
  longest `.run()` is now under 600 lines (Time Series keeps its
  diagnostic battery in `.run()` because the row accumulators write to
  variables that `.run()` reads afterwards).

---

# Validation scripts (unreleased, 2026-09-05)

## Additions

- `validation/case_study_public_data.R`: runs `groupCheck`, `regCheck` and
  `pathCheck` on public datasets (`lavaan::HolzingerSwineford1939` and
  `psychTools::epi.bfi`) and checks 20 of the module's outputs against
  independent reference implementations (`stats`, `car`, `lmtest`, `energy`,
  `tseries`); all match on AssumptionsLab 1.6.0. It replaces the private
  survey data that the manuscript's applied cases and verification table
  previously relied on, so every number in them can now be reproduced from
  public data.

## Bug fixes

- `validation/normality_groupcheck_mc.R` and `validation/normality_regcheck_mc.R`
  reimplemented the battery's Pearson chi-square goodness-of-fit test with a
  manual equal-probability binning (a fixed 5-10 class cap, no degrees-of-
  freedom adjustment for the two estimated parameters) instead of calling
  `nortest::pearson.test()`, the actual function `shared-helpers.R` calls.
  A quick diagnostic (2,000 null replications per sample size) found the
  manual version badly miscalibrated under the null --- rejecting at
  0.6-1.9% instead of the nominal 5%, versus 4.7-5.6% for the real
  function --- because it never subtracted 2 degrees of freedom for the
  estimated mean and standard deviation. Both scripts now call
  `nortest::pearson.test(x)$p.value` directly, matching the module exactly.
  Rerunning both simulations left Type~I error and power (`rate1`-`rate5`)
  essentially unchanged in the raw-variable case, since a single better-
  calibrated test rarely flips a nine-test majority vote by itself; but the
  module's own two-group decision rule (Strategy 3), which flags *any*
  significant test rather than a majority, saw its ambiguous-branch rate at
  the smallest sample size $(10,10)$ shift from roughly 19-58% to 23-62%
  across families --- a previously under-reporting test now correctly
  contributing evidence. The residual case was materially unaffected (its
  decision rule keys off Shapiro-Wilk alone, not the full battery). Found
  via the same external AI-assisted code review as the fix below.
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

# AssumptionsLab 1.6.0 (2026-09-17)

## Interface changes

- Every module's `plotStyle` option (Clean Academic / Black and White /
  High Contrast / Full color) is removed, and `plotPalette` is narrowed
  from 4 style-specific choices to 3: Follow jamovi theme (default),
  Colorblind-safe (Okabe-Ito), Viridis. All 9 analysis modules
  (`logCheck`, `ordCheck`, `multCheck`, `anovaCheck`, `regCheck`,
  `timeCheck`, `groupCheck`, `relatedCheck`, `pathCheck`) migrated their
  plots from a hand-rolled theme/palette system to jamovi's own native
  `ggtheme`/`theme` mechanism, so every plot now follows the user's
  jamovi-wide Theme and Palette settings by default instead of
  duplicating them with a separate module-level control. Colorblind-safe
  and viridis remain genuinely additive options jamovi does not
  otherwise offer. Fixed one real bug uncovered along the way: a
  `scale_color_manual()`/`scale_fill_manual()` call added *before*
  `+ ggtheme` in the plot-building chain is silently overridden by
  ggtheme's own embedded discrete scale — this had been silently
  discoloring the outlier/influence-flag highlight in `logCheck`,
  `ordCheck`, `multCheck`, and `regCheck`'s plots since those modules
  were first written; all now build the chain in the correct order.
- The standalone `Bibliography` analysis is removed from the menu.
  Per-analysis method attribution moved to jamovi's own native
  `00refs.yaml`/`refs:` mechanism, attached directly to the table/plot
  each source backs, across all 9 analysis modules — this is also why
  in-app citations now render in jamovi's own numbered,
  order-of-appearance format rather than APA, a constraint of jamovi's
  reference renderer rather than a module choice (see `CODE_STYLE.md`
  §21). The complete APA 7th edition reference list and each source's
  bibliometric profile (citation counts, journal indexing) moved to
  `docs/Bibliography.md`. `Assumption Library` keeps its own
  filterable category browser, with a plain-text pointer to that same
  documentation file — jamovi's own results renderer only shows a
  `refs:` reference list for `Table`-type results, never for `Html`,
  so Library's narrative category sections cannot carry their own
  native citation list the way an analysis module's tables do.
- `Assumption Library`'s entire content (the narrative explanation of
  each assumption/test, and its comparison table) moved from its own
  `reportLang` option to jamovi's native i18n catalog: every category's
  guide paragraphs and the HTML `<table>` that used to be built at
  runtime are now static `content:` fields defined directly in
  `jamovi/assumptionlibrary.r.yaml` and translated through
  `jamovi/i18n/es.po`, following jamovi's own UI language automatically.
  This is possible because none of Library's content depends on the
  user's data, unlike the 9 analysis modules - so nothing there ever
  needs `.()` called from R, the exact cached call behind the
  FiabilityLab translator regression. `reportLang` and its "Report
  Language" control were removed from this analysis entirely: an
  interim version left the comparison tables on `reportLang` while the
  guide text followed jamovi's UI language, which meant the two could
  show different languages at once depending on which control was
  changed - exactly the confusing behavior removing `reportLang`
  everywhere avoids. Each category is still several result items rather
  than one, because a single result's combined guide+table text can
  exceed the ~10,000-character limit R's own parser imposes on a string
  literal needing Unicode escapes - a length constraint only, since
  every item uses the same static, natively-translated mechanism (see
  `CODE_STYLE.md` §20/§21).

## New features

- Added Spanish (`es`) as a jamovi interface-language translation via
  jamovi's own native i18n mechanism (`jamovi/i18n/catalog.pot` +
  `es.po`, 530 translated strings): option-panel titles, descriptions,
  and result/table headers now follow jamovi's own UI-language setting,
  independent of the module's existing `reportLang` option (which still
  separately controls the language of the report's analytical content).
  Normalized roughly half of every module's `.r.yaml` result titles from
  hardcoded Spanish to English first, so the yaml source is one
  consistent language for jamovi's translation tooling to build from.

## Bug fixes

- `groupCheck`, `relatedCheck`, `anovaCheck`, `logCheck`, `ordCheck`,
  `multCheck`, `regCheck`, `timeCheck`, `pathCheck`: a diagnostic test
  failing on its own (too few observations, a constant series, a
  near-collinear sub-model, a package-level edge case) could silently
  drop its entire results row instead of showing it as "Not computable"
  with a footnote naming the specific reason — the row-adding helper had
  no `else` branch for the failure case. Every such helper across all 9
  modules now always renders the row, with a methodologically specific
  footnote rather than a generic message. A related, broader issue was
  fixed alongside it: several R statistical functions (`tseries::
  adf.test`/`Box.test`/`jarque.bera.test`, `nortest::sf.test`,
  `ResourceSelection::hoslem.test`, and a hand-rolled skewness test) do
  not throw on degenerate input — they warn and return a result with a
  non-finite statistic/p-value instead — so a bare `tryCatch` was not
  enough to catch a bad result; every affected call site now also checks
  the result is finite before treating it as real.
- `anovaCheck`: `fmt_num()`'s "Not computed" fallback text ignored the
  `reportLang` option and was always shown in English regardless of the
  user's selected report language.
- Blocking conditions (missing variables, wrong factor-level count, no
  predictors selected, a model that fails to fit entirely) across all 9
  modules now call `jmvcore::reject()` with the actual failure reason,
  so jamovi shows its own error presentation instead of a
  completed-looking report with the explanation buried in the intro
  text.

## Internal

- Consolidated the `qname()`/`strip_qname()`/`add_row()`/`html_block()`
  helpers — previously re-declared as identical local closures in most
  analysis files — into `shared-helpers.R` as `.al_qname()`/
  `.al_strip_qname()`/`.al_add_row()`/reused `.al_html_block()`
  directly. This is the same kind of duplication that caused the
  formula-breaking-on-spaces bug fixed in 1.5.3: a helper missing from
  one file's copy goes unnoticed until it matters. `pathCheck`'s inline
  backtick-quoting at its two `as.formula()` call sites is now routed
  through the shared `.al_qname()` too.

# AssumptionsLab 1.5.3 (2026-09-16)

## Bug fixes

- `regCheck`, `anovaCheck`, `timeCheck`, `pathCheck`: every diagnostic plot
  exported blank to PDF/image while rendering correctly in jamovi's live
  results panel. Root cause: each `Image` result's render function read its
  plotting data from an R6 `private$` field, populated only within the live
  `.run()` execution instance — jamovi re-renders `Image`s for export from a
  separate, fresh instance whose `private$` fields are empty, so there was
  nothing to draw from. Fixed by moving every plot's data into
  `self$results$<image>$setState(x)` at computation time and reading
  `image$state` (which does survive into the export context) in each render
  function, the pattern `groupCheck`, `relatedCheck`, `logCheck`, `ordCheck`,
  and `multCheck` already used correctly. Found by jamovi's official module
  review team (HIGH severity) during the library-submission review of 1.5.2.
- `logCheck`: crashed when any predictor name contained a space (or another
  character invalid in an unquoted R formula — common in spreadsheet-derived
  column headers), in both the main model formula and the Box-Tidwell
  linearity-in-the-logit interaction formula. Root cause: model formulas
  were built via `as.formula(paste("y ~", paste(predictors, collapse =
  "+")))` without backtick-quoting each name first. Fixed with a
  `qname()`/`strip_qname()` helper pair — quoting names for formula
  construction and for any name-based coefficient lookup built
  independently of the fitted model's own coefficient names, and stripping
  the backticks only at user-facing display sites — mirroring the pattern
  `ordCheck` and `pathCheck` already used. Also corrected the Box-Tidwell
  interaction-coefficient lookup regex, which would otherwise have silently
  failed to match a backtick-preserved coefficient name for any predictor
  requiring quoting. A regression test (a predictor named `Test Score`)
  locks in the fix. Found by jamovi's official module review team (HIGH
  severity) during the library-submission review of 1.5.2.

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
