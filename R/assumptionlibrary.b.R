# -----------------------------------------------------------------------------
# AssumptionsLab
# A Jamovi module for statistical assumptions assessment and methodological
# decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of AssumptionsLab.
#
# AssumptionsLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# AssumptionsLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with AssumptionsLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Assumption Library.
# ES: Biblioteca de Supuestos.
#
# A reference glossary explaining what each statistical assumption and test
# means and when it is used across AssumptionsLab. It does not analyze the
# user's data; each analysis interprets its own results separately.
#
# Since version 1.2.0 of this analysis (module minApp 28.3.0) the library is
# built only from native jamovi results elements:
# - the prose of each category is a `Text` element (Markdown: bold, lists),
#   carrying the category's `refs:` so jamovi numbers the sources and lists
#   them under References (refs on Text/Html work since jamovi 2.7.7);
# - each comparison table is a native `Table` whose columns are declared in
#   jamovi/assumptionlibrary.r.yaml and whose rows are added in .init().
# Every paragraph, list item and table cell is its own `.()` entry, so each
# one is a clean entry of jamovi/i18n/es.po with no markup or CSS around it.
# Proper names whose Spanish form is identical (e.g. "Shapiro-Wilk") are
# not wrapped in `.()`. Which category is shown is declared in the r.yaml
# with `visible: (category:all || category:<name>)`, so .run() has nothing
# to do.
#
# ES: Desde la versión 1.2.0 de este análisis (minApp 28.3.0) la biblioteca
# usa solo elementos nativos de jamovi: la prosa de cada categoría es un
# elemento `Text` (Markdown: negritas, listas) con los `refs:` de la
# categoría, y cada tabla comparativa es una `Table` nativa con columnas
# declaradas en el r.yaml y filas añadidas en .init(). Cada párrafo, ítem
# de lista y celda es su propia entrada `.()` de jamovi/i18n/es.po, sin
# marcado ni CSS. La visibilidad por categoría se declara en el r.yaml.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .al_library_text(.) / .al_library_tables(.)
#
# Input: . - the translator of the run (`lang$tr`, see .al_lang()).
# Output: .al_library_text(): a named list, one entry per Text element; each
#   entry is a list of blocks (kind "h" = bold heading line, "p" = paragraph,
#   "li" = list item). .al_library_tables(): a named list, one entry per
#   Table element; each entry is a list of rows (character vectors in the
#   column order declared in the r.yaml).
#
# ES: Entrada: . - el traductor de la ejecución. Salida: lista con los
# bloques de cada elemento Text (h = encabezado en negrita, p = párrafo,
# li = ítem de lista) y lista con las filas de cada Table.
# -----------------------------------------------------------------------------
.al_library_text <- function(.) {
    h  <- function(x) list(kind = "h", text = x)
    p  <- function(x) list(kind = "p", text = x)
    li <- function(x) list(kind = "li", text = x)
    list(
        intro = list(
            p(.("This library summarizes the assumptions and tests used across AssumptionsLab. Its purpose is not to analyze data directly, but to serve as a methodological guide for interpreting the tests within each analysis.")),
            h(.("Core principle:")),
            p(.("The library explains what each assumption means and when it is used. Each analysis then interprets its own results using the user's actual data.")),
            p(.("References: jamovi numbers the sources that each section cites and lists them in full in the References section at the end of these results.")),
            h(.("General rule for p-values:")),
            p(.("p < .05 indicates a statistically significant deviation from the assumption being evaluated. p >= .05 indicates that the result is compatible with the assumption being approximately met.")),
            h(.("Important:")),
            p(.("Compatible does not mean proven. It means that, with this data and this test, no statistically significant deviation is observed.")),
            p(.("Avoid interpreting assumption tests mechanically. The decision should combine p-values, sample size, plots, design, and substantive judgment."))
        ),
        normality = list(
            h(.("What it assesses:")),
            p(.("Normality evaluates whether a distribution reasonably approximates a normal distribution. In applied statistical analysis, it is not always the original variable that is evaluated.")),
            h(.("Where it's used in AssumptionsLab:")),
            li(.("**Independent Groups:** normality by group or of residuals, depending on context.")),
            li(.("**Related Groups:** normality of paired differences or within-subject residuals.")),
            li(.("**ANOVA/ANCOVA:** normality of model residuals.")),
            li(.("**Regression:** normality of model residuals.")),
            h(.("How the battery is interpreted:")),
            p(.("Shapiro-Wilk is used as the primary test because it is the most powerful in small to moderate samples; the others are reported as secondary evidence. The interpretation integrates both: how many of the secondary tests agree with Shapiro-Wilk's conclusion, not just whether Shapiro-Wilk alone is significant."))
        ),
        normalityGuide2 = list(
            h("Shapiro-Wilk:"),
            p(.("A classic and widely used normality test (Shapiro & Wilk, 1965). Requires at least 3 valid cases. In R it is computed for up to 5000 observations. It is especially powerful in small to moderate samples relative to the other tests in this battery (Razali & Wah, 2011).")),
            h(.("Lilliefors (corrected Kolmogorov-Smirnov):")),
            p(.("Compares the observed distribution against a theoretical one, but corrects the critical values because the mean and standard deviation are estimated from the same data (Lilliefors, 1967). The classic Kolmogorov-Smirnov test (without this correction) assumes parameters known in advance and is not valid in this context; this is why this module always uses the corrected Lilliefors version, not the classic K-S test.")),
            h("Anderson-Darling:"),
            p(.("Gives more weight to the tails (Anderson & Darling, 1952). It is useful when heavy tails, extreme values, or deviations at the ends of the distribution are a concern.")),
            h("Cramér-von Mises:"),
            p(.("Compares the entire observed cumulative distribution against the theoretical one (Anderson & Darling, 1952). It is more sensitive than Kolmogorov-Smirnov to deviations spread across the central body of the distribution, not just at one point.")),
            h("Shapiro-Francia:"),
            p(.("A variant of Shapiro-Wilk based on the same principles, with a different calculation that can perform better in some moderate or large samples (Shapiro & Francia, 1972).")),
            h(.("Pearson chi-square:")),
            p(.("Compares observed and expected frequencies by interval. It depends on how the data are grouped into intervals, so it tends to be less powerful than tests based on the full distribution.")),
            h("Jarque-Bera:"),
            p(.("Jointly evaluates skewness and kurtosis (Jarque & Bera, 1987). It is more informative in moderate or large samples, and less stable in very small samples.")),
            h("Skewness test:"),
            p(.("Evaluates skewness. Positive skewness suggests a long right tail; negative skewness suggests a long left tail.")),
            h("Kurtosis test:"),
            p(.("Evaluates peakedness and tail weight. It can indicate heavy tails, light tails, or a concentration different from what is expected under normality.")),
            h(.("Sample-size criteria:")),
            li(.("**n < 10:** interpretation is very unstable.")),
            li(.("**10 <= n < 30:** small sample; low power to detect deviations.")),
            li(.("**30 <= n <= 200:** moderate sample; the tests are useful diagnostics.")),
            li(.("**n > 200:** small deviations can produce significant p-values.")),
            li(.("**n > 5000:** Shapiro-Wilk is not computed in R.")),
            h(.("Interpretation:")),
            p(.("p < .05 suggests a statistically significant deviation from a normal distribution. p >= .05 indicates that the result is compatible with an approximately normal distribution.")),
            h(.("Methodological decision:")),
            p(.("Combine tests, plots, skewness, kurtosis, sample size, and outliers. If several tests suggest non-normality, consider transformation, bootstrap, robust methods, or nonparametric alternatives."))
        ),
        homoscedasticity = list(
            h(.("What it assesses:")),
            p(.("Evaluates whether variability is approximately constant. In group comparisons this is called homogeneity of variance (Levene, 1960); in regression it is called homoscedasticity of residuals.")),
            h(.("Where it's used in AssumptionsLab:")),
            li(.("**Independent Groups:** equality of variances across groups.")),
            li(.("**ANOVA/ANCOVA:** equality of variances across cells or groups.")),
            li(.("**Regression:** constant residual variance across fitted values.")),
            h(.("Why it matters:")),
            p(.("When group variances are very different, the standard error of the test statistic no longer correctly reflects the real variability, and confidence intervals and p-values can become unreliable — especially with unequal group sizes (Welch, 1947). In regression, heteroscedasticity does not bias OLS coefficients, but it does bias their standard errors, affecting t and F tests on those coefficients.")),
            h(.("Two families of tests:")),
            p(.("Group tests (Levene, Brown-Forsythe, Bartlett, Fligner-Killeen, Hartley's Fmax) compare dispersion across k predefined groups. Regression tests (Breusch-Pagan, White, Goldfeld-Quandt, Spearman between residuals and fitted values) evaluate whether residual variance changes systematically with predicted values or predictors, rather than comparing discrete groups."))
        ),
        homoscedasticityGuide2 = list(
            h(.("Levene — what it assesses and when to use it:")),
            p(.("Compares dispersion across groups using each observation's absolute deviation from its group mean, then applies a one-way ANOVA to those deviations (Levene, 1960). H0: the k population variances are equal. H1: at least one differs. It is a reasonable first check, especially with large samples and approximately symmetric data.")),
            h(.("Brown-Forsythe — what it assesses and when to use it:")),
            p(.("Follows the same logic as Levene but centers each group on its median instead of its mean (Brown & Forsythe, 1974). Using the median makes it notably more robust to skewness, heavy tails, and outliers. In practice, when there is doubt about the shape of the distribution within each group, Brown-Forsythe should be preferred over Levene by default.")),
            h(.("Bartlett — what it assesses and when to use it:")),
            p(.("Tests equality of variances using a likelihood-ratio approach that assumes normality within each group (Bartlett, 1937). It is the most powerful test in this family when that normality is reasonable, but it loses validity quickly if it is not: a deviation from normality can make Bartlett reject equal variances even when the true variances are equal. It should not be used as the sole basis for a decision if the data are not normal — check the Normality section of this library first.")),
            h(.("Fligner-Killeen — what it assesses and when to use it:")),
            p(.("A nonparametric, rank-based test on the absolute deviations from the median (Fligner & Killeen, 1976). It makes no distributional assumption, which makes it the most robust option in this family against non-normality and outliers, at the cost of some power when the data are in fact reasonably normal.")),
            h(.("Hartley's Fmax — what it assesses and limitations:")),
            p(.("Simply the ratio between the largest and smallest sample variance among the k groups (Hartley, 1950). It is easy to compute and interpret, but its tabulated critical values are only strictly valid with balanced designs (equal n per group) and normality; it is therefore used more as a complementary descriptive indicator than as a primary formal test.")),
            h(.("Interpretation (group tests):")),
            p(.("p < .05 suggests statistically significant differences between group variances. p >= .05 indicates that the result is compatible with approximately homogeneous variances."))
        ),
        homoscedasticityGuide3 = list(
            h(.("Breusch-Pagan — what it assesses and when to use it:")),
            p(.("Regresses the squared residuals on the original model's predictors and tests whether that auxiliary regression explains a significantly larger share of variance than zero (Breusch & Pagan, 1979). H0: residual variance is constant (homoscedasticity). H1: residual variance depends on the predictors. It is the default check for linear regression when an approximately linear relationship between variance and predictors is suspected.")),
            h(.("White — what it assesses and when to use it:")),
            p(.("Generalizes Breusch-Pagan by adding squares and cross-products of the predictors to the auxiliary regression, without assuming a specific functional form for the heteroscedasticity (White, 1980). It is more general and more suitable when residual variance is suspected to depend on the predictors non-linearly or through interactions, but it loses some power relative to Breusch-Pagan when the real heteroscedasticity is simply linear.")),
            h(.("Goldfeld-Quandt — what it assesses and when to use it:")),
            p(.("Orders observations by a variable (typically a predictor or the fitted values), omits a central band, and compares the residual sum of squares between the low-value and high-value subgroups using an F test (Goldfeld & Quandt, 1965). It is especially useful when variance is suspected to change in a stepped or marked way around an identifiable point of the ordering variable, rather than smoothly and continuously.")),
            h(.("Spearman |residuals| vs fitted — what it assesses and when to use it:")),
            p(.("Computes the Spearman rank correlation between the absolute value of the residuals and the fitted values. It is exploratory and nonparametric: it does not assume a specific functional form, only detecting whether error magnitude grows or shrinks monotonically with the predicted value. It is a good visual/numeric complement to Breusch-Pagan and White, not a formal substitute.")),
            h(.("Interpretation (regression tests):")),
            p(.("p < .05 suggests that residual variance is not constant (heteroscedasticity). p >= .05 indicates that the result is compatible with approximately constant residual variance.")),
            h(.("Methodological decision:")),
            p(.("In group comparisons, if there is evidence of heterogeneity of variance, consider the Welch correction (Welch, 1947) for the t or F statistic, the Brown-Forsythe test itself as an alternative to classic ANOVA, or robust methods. In regression, consider heteroscedasticity-robust HC3 standard errors (MacKinnon & White, 1985), a transformation of the dependent variable, or a model that explicitly models the variance."))
        ),
        linearity = list(
            h(.("What it assesses:")),
            p(.("Evaluates whether the relationship between numeric predictors or covariates and the dependent variable can reasonably be represented by a straight line.")),
            h(.("Where it's used in AssumptionsLab:")),
            li(.("**Regression:** numeric predictors and overall model fit.")),
            li(.("**ANOVA/ANCOVA:** numeric covariates.")),
            li(.("**Path/Related/Logistic:** dCor and copula entropy as a non-linear dependence diagnostic complementary to Pearson.")),
            p(.("It does not apply the same way to categorical factors, because these represent groups or levels, not continuous linear relationships."))
        ),
        linearityGuide2 = list(
            h(.("Bivariate correlation:")),
            p(.("Describes simple linear association between two variables. It is exploratory and does not replace diagnosing the full model.")),
            h(.("Distance correlation (dCor) and copula entropy:")),
            p(.("Pearson's r only detects linear association. dCor (Székely et al., 2007) detects linear and non-linear association equally, without assuming a functional form. Copula entropy (Ma & Sun, 2011) is an additional dependence measure, free of distributional assumptions, based on the copula of the variables. A pair with dCor notably higher than its Pearson |r| (gap > .10) is a signal, not proof, of a non-linear relationship that a linear model might be missing; it is worth confirming with a scatterplot before concluding non-linearity. The p-value for dCor and for copula entropy is computed by permutation, so its resolution depends on the number of permutations used.")),
            h(.("Exploratory quadratic term:")),
            p(.("Adds x² to the model as an additional predictor and compares it against the original model. p < .05 on the x² coefficient suggests possible curvature not captured by the linear relationship. It is a quick, one-predictor-at-a-time check; it does not replace RESET, which evaluates the full model.")),
            h(.("Box-Tidwell — what it assesses:")),
            p(.("Evaluates whether a specific numeric predictor would need a power transformation (a form of type Xᵏ) to relate linearly to the dependent variable or, in logistic regression, to the logit. The mechanism: an x × ln(x) interaction term is added to the model alongside x itself, and the significance of that term's coefficient is tested. H0: x does not need to be transformed (the relationship is already approximately linear on the current scale). H1: the relationship would improve with a power transformation of x. Requires x > 0, because the term uses ln(x); it cannot be applied to predictors with negative or zero values without shifting them first.")),
            h(.("Box-Tidwell — when to use it:")),
            p(.("It is the reference test for evaluating logit-linearity of a continuous numeric predictor in logistic regression — unlike the quadratic term or a scatterplot, it directly evaluates the correct scale of the predictor (log, root, linear) instead of only detecting curvature. It is also useful in linear regression when a specific predictor, not the full model, is suspected to be the source of the non-linearity.")),
            h(.("Box-Tidwell — limitations:")),
            p(.("It evaluates one predictor at a time, so it says nothing about joint curvature across several predictors or about general model misspecification (that is what Ramsey RESET is for). With 0 of several predictors significant, the correct conclusion is \"no statistical evidence of a deviation from linearity was detected\", not \"linearity is confirmed\" — the test may simply lack power in the available sample.")),
            h(.("Ramsey RESET — what it assesses:")),
            p(.("Evaluates whether the model, as a whole, is functionally misspecified — that is, whether it fails to capture some systematic form (curvature, interactions, omitted terms) that a model strictly linear in the current predictors cannot represent. The mechanism: powers of the fitted values (typically the square and the cube) are added to the model, and an F-test contrasts whether those powers significantly improve the fit relative to the original model. H0: the original model is correctly specified (the powers add no additional explanation). H1: the model omits some relevant functional form.")),
            h(.("Ramsey RESET — when to use it:")),
            p(.("As a global check after reviewing the individual predictors: if Box-Tidwell or the quadratic term do not flag any particular predictor but the overall fit looks poor, RESET can detect non-linearity that only appears in combination across predictors, or in a form (for example, an interaction) that no single per-predictor check captures on its own. It is the most general of the three tests.")),
            h(.("Ramsey RESET — limitations:")),
            p(.("A significant RESET indicates something about the functional form is wrong, but not what: it does not distinguish between genuine curvature, an omitted interaction, an omitted variable correlated with the predictors, or an influential outlier. It is worth reviewing the residual plots and the other linearity tests before deciding which correction to apply.")),
            h(.("Rainbow test — what it assesses:")),
            p(.("Evaluates whether the linear fit is stable across the range of fitted values, rather than concentrating the evidence on one specific form of non-linearity. The mechanism: cases are ordered by their fitted value, the central band is taken (by default, around 50% of the cases with intermediate fitted values), and the model is refit using only that central subset. An F-test compares the residual sum of squares of the full model against that of the model fit only to the central band. H0: the linear fit is equally adequate in the central band as in the full set. H1: the fit deteriorates when the extremes are included, suggesting the relationship is not uniformly linear across the full range of the predictors.")),
            h(.("Rainbow test — when to use it:")),
            p(.("It is useful as a complement to RESET when the non-linearity is suspected not to be smooth, global curvature, but rather a relationship that behaves differently at the extremes of the predictor range versus the center — for example, effects that flatten out or accelerate only at extreme values. It requires a reasonable sample (in this module, at least 20 cases) so the central sub-model has enough degrees of freedom.")),
            h(.("Common to RESET and Rainbow — a frequent error:")),
            p(.("Interpreting a non-significant RESET or Rainbow result as proof of perfect linearity. Both tests have limited power in small samples (they may miss real curvature) and can flag trivial, practically irrelevant curvature as significant in very large samples. Always read them together with the residuals-vs-fitted plot, not in isolation.")),
            h(.("Sample-size criteria:")),
            p(.("With small samples it can be difficult to detect curvature. With large samples, small curvatures can become significant.")),
            h(.("Interpretation:")),
            p(.("p < .05 on curvature or specification tests suggests a possible deviation from linearity. p >= .05 indicates that the result is compatible with an approximately linear relationship.")),
            h(.("Methodological decision:")),
            p(.("Consider polynomial terms, splines, transformations, or non-linear models if there is evidence of substantive curvature."))
        ),
        independence = list(
            h(.("What it assesses:")),
            p(.("Evaluates whether observations or errors can be considered independent. This assumption depends mainly on the study design.")),
            h(.("Where it's used in AssumptionsLab:")),
            li(.("**ANOVA/ANCOVA:** independence between units or subjects.")),
            li(.("**Regression:** independence of errors, especially in ordered data.")),
            li(.("**Related Groups:** dependence within a unit is expected, independence between units.")),
            h(.("Core idea:")),
            p(.("The four tests in this section relate to the temporal dependence of residuals, but they do not answer exactly the same question or apply to the same object. In a regression or time series, one usually wants the residuals to be approximately white noise, i.e., with no systematic temporal pattern. If there is residual autocorrelation, the OLS coefficients can remain unbiased under certain conditions, but the standard errors, t-tests, confidence intervals, and F-tests can become unreliable.")),
            h(.("The choice among the four depends mainly on:")),
            p(.("Whether you are analyzing residuals from a regression or residuals from an ARIMA/SARIMA-type time-series model. Whether autocorrelation is suspected only at lag 1 or at several lags at once. Whether the model contains the lagged dependent variable as a regressor. Whether the goal is to detect linear autocorrelation specifically, or a more general lack of randomness in the sequence."))
        ),
        independenceGuide2 = list(
            h(.("Durbin-Watson — what it assesses:")),
            p(.("Evaluates whether the residuals of a regression show first-order autocorrelation. H0: no first-order autocorrelation. H1: there is first-order autocorrelation, positive or negative. Its statistic ranges roughly from 0 to 4: near 2 indicates no first-order autocorrelation; below 2 suggests positive autocorrelation; above 2 suggests negative autocorrelation. There is an approximate relationship DW ≈ 2 × (1 − r1), where r1 is the estimated residual autocorrelation at lag 1.")),
            h(.("Durbin-Watson — when to use it:")),
            p(.("As a simple diagnostic when there is a linear least-squares regression, temporally ordered data, specific interest in the dependence between a residual and the immediately preceding one, and a model without the lagged dependent variable as a regressor.")),
            h(.("Durbin-Watson — when not to choose it:")),
            p(.("When the model includes the lagged dependent variable (a dynamic model), when autocorrelation of order 2, 3, 12, etc. is suspected, when there is a more complex temporal structure such as seasonality, or when a formal test with an easily generalizable p-value is needed. In modern applied practice, Durbin-Watson is usually reported as a quick descriptive indicator, while Breusch-Godfrey is generally preferred for the formal test.")),
            h(.("Breusch-Godfrey — what it assesses:")),
            p(.("Also called the LM test for serial correlation, it tests whether the residuals of a regression show autocorrelation up to an order p chosen by the analyst. H0: no residual autocorrelation up to order p. H1: autocorrelation exists in at least one of the lags from 1 to p. The test is based on an auxiliary regression that incorporates the lagged residuals; under H0, the LM statistic approximately follows a chi-square distribution with p degrees of freedom.")),
            h(.("Breusch-Godfrey — when to use it:")),
            p(.("It is the most general option for regressions. It is worth using with econometric regressions, when more than one lag needs to be tested, when the model includes lagged explanatory variables or the lagged dependent variable itself, and when a formal test with a p-value is needed to decide between modeling AR errors, using GLS, applying HAC/Newey-West errors, or rethinking the model's dynamic specification.")),
            h(.("Breusch-Godfrey — how to choose the order p:")),
            p(.("It should not be chosen mechanically or overstated. With annual data it usually makes sense to start with 1 or 2 lags; with quarterly data, try 1 to 4; with monthly data, testing up to 12 can be reasonable if there is a possible annual pattern; with higher frequencies it is worth relying on process knowledge and reviewing the residuals' autocorrelation function (ACF) and partial autocorrelation function (PACF). Practical rule: when in doubt between Durbin-Watson and Breusch-Godfrey for a regression, Breusch-Godfrey is preferred.")),
            h(.("Ljung-Box — what it assesses:")),
            p(.("This is a joint (portmanteau) test that assesses whether the autocorrelations up to a horizon h are jointly zero. H0: no autocorrelation up to lag h. H1: autocorrelation exists in at least one of those lags. Its Q statistic combines, weighted by sample size and each lag, the squares of the estimated residual autocorrelations up to lag h; so it does not ask whether there is autocorrelation only at lag 1, but whether the residuals, taken together, look like white noise up to that horizon.")),
            h(.("Ljung-Box — when to use it and which horizon h to choose:")),
            p(.("It is especially useful for diagnosing the residuals of an ARIMA, SARIMA, ETS, or other time-series forecasting model, and for checking whether temporal structure remains unexplained after fitting the model. It is typically applied to the residuals of an already-fitted model, not to the raw series. The horizon h depends on frequency and objective: with quarterly data h=4 or 8 is usually reviewed; with monthly data, h=12 is a natural starting point; with daily data, a horizon consistent with weekly, monthly, or business cycles is worth using. When applying it to the residuals of an ARIMA, the degrees of freedom must also be adjusted for the AR and MA parameters already estimated.")),
            h(.("Ljung-Box — difference from Breusch-Godfrey:")),
            p(.("Both can review several lags at once, but their natural context differs: Breusch-Godfrey is used in a regression with covariates and asks whether that regression's errors are serially correlated; Ljung-Box is a global diagnostic of the residuals of a time-series model and asks whether those residuals look like white noise.")),
            h(.("Runs test — what it actually assesses:")),
            p(.("Unlike the three previous tests, it is not primarily a test of linear autocorrelation. It evaluates whether the order of a sequence looks random. A run is a consecutive sequence of observations of the same class — for example, consecutive positive residuals versus negative ones, or values above versus below the median. H0: the sequence was generated randomly. H1: the sequence does not look random. The test compares the observed number of runs with the number expected under randomness: too few runs suggest clustering or persistence; too many can suggest excessive alternation.")),
            h(.("Runs test — when to use it:")),
            p(.("When a nonparametric diagnostic is needed, when the residuals are not normal or contain outliers, when the interest is in detecting sign clustering, regime changes, or a general lack of randomness, when working with a binary sequence (signs, pass/fail, defective/non-defective), or as a nonparametric complement to the autocorrelation tests above.")),
            h(.("Runs test — what it does not conclude:")),
            p(.("A significant result only supports that the order of the series does not look random; it does not necessarily imply AR(1)-type linear autocorrelation. It can detect non-randomness arising from trends, structural changes, persistence of signs, or non-linear patterns. Conversely, a series may not show strong linear autocorrelation and still not be completely random.")),
            h(.("Practical decision tree:")),
            p(.("Linear regression with temporal data, no lagged dependent variable: start with a plot of residuals over time, the residuals' ACF/PACF, and Breusch-Godfrey; use Durbin-Watson only as a quick lag-1 check. Regression with the lagged dependent variable or other dynamic components: use Breusch-Godfrey and the residuals' ACF/PACF; avoid relying on Durbin-Watson. Already-fitted ARIMA or SARIMA model: use the residuals' ACF and Ljung-Box, to verify that the model has absorbed the temporal dependence. General randomness without assuming a specific linear relationship: use the Runs test, a sequence plot, and the ACF as a complement.")),
            h(.("Final recommendation if a default test is needed:")),
            p(.("General temporal or econometric regression: Breusch-Godfrey. ARIMA/SARIMA or forecasting model: Ljung-Box. Quick, basic AR(1) autocorrelation check in a static regression: Durbin-Watson. Nonparametric check of randomness or sign pattern: Runs test. In almost every case, it is worth accompanying the test with a residuals ACF: the p-value indicates whether there is overall evidence against the absence of dependence, and the ACF helps identify at which lags the problem lies.")),
            h(.("Preconditions:")),
            p(.("These tests make more sense when there is a temporal, spatial, or sequential order. In cross-sectional data with no natural order, they should be interpreted with caution.")),
            h(.("Interpretation:")),
            p(.("p < .05 suggests a statistically significant deviation from independence or randomness of residuals. p >= .05 indicates that the result is compatible with approximate independence or randomness of residuals.")),
            h(.("Methodological decision:")),
            p(.("If dependence is present, consider mixed models, GLS, time-series models, or HAC/Newey-West robust errors, depending on the design."))
        ),
        multicollinearity = list(
            h(.("What it assesses:")),
            p(.("Evaluates whether the model's predictors are too closely related to one another. Multicollinearity does not necessarily reduce the model's overall predictive power, but it inflates the variance of individual coefficients and makes interpreting them separately harder (Marquardt, 1970).")),
            h(.("Where it's used in AssumptionsLab:")),
            p(.("Regression and linear models with several predictors.")),
            h(.("Why it matters:")),
            p(.("When two or more predictors share a lot of information, the model cannot precisely separate each one's own effect; individual coefficients become unstable (they change substantially when cases or predictors are added or removed) and their standard errors inflate, even though the model's overall fit (R²) and its predictions can remain reasonable (O'Brien, 2007)."))
        ),
        multicollinearityGuide2 = list(
            h(.("VIF and tolerance — what they assess:")),
            p(.("A predictor's VIF is obtained by regressing that predictor on all the other predictors in the model and using the resulting R²: VIF = 1/(1-R²). A high VIF means much of that predictor's variation can already be explained by the others, leaving little \"own\" information to estimate its coefficient precisely. Tolerance is simply 1/VIF, on the opposite scale.")),
            h(.("VIF and tolerance — when to use them and a caution about thresholds:")),
            p(.("They are the most common and easiest-to-interpret per-variable diagnostic. The conventional thresholds (VIF < 5 or < 10) are widely used rules, not universal limits derived mathematically: the tolerable level of VIF depends on sample size, how much variance the model explains, and whether the interest is in individual coefficients or only in overall prediction (O'Brien, 2007). With large samples, moderate VIFs can be harmless; with small samples, even VIFs of 3-4 can already produce unstable coefficients.")),
            h(.("Maximum correlation between predictors:")),
            p(.("The simplest bivariate check: it identifies pairs of predictors that are highly correlated with each other. It is useful as a first glance, but it does not detect \"distributed\" collinearity, where no single pair is highly correlated but a linear combination of three or more predictors is — that kind of collinearity only shows up in VIF, eigenvalues, or the condition index.")),
            h(.("Condition index and eigenvalues:")),
            p(.("These come from the decomposition of the standardized predictor matrix. An eigenvalue near zero indicates a direction in predictor space with almost no independent variation — that is, a nearly redundant linear combination. The condition index summarizes this as the ratio between the largest and smallest eigenvalue; high values flag collinearity, but it is worth also reviewing the variance proportions by dimension to identify which specific predictors are involved (Belsley et al., 1980).")),
            h(.("Determinant and model rank:")),
            p(.("A near-zero determinant of the predictor correlation matrix is another aggregate signal of redundancy. Model rank is a different and stricter check: if the rank is less than the number of columns in the design matrix, an exact linear dependency exists (not just high correlation), and some model parameters are not estimable at all — R typically flags them as NA in the model output.")),
            h(.("Methodological decision:")),
            p(.("If collinearity is high but the main goal is prediction, it may not require action. If the goal is interpreting individual coefficients, consider removing or combining redundant predictors, centering polynomial terms and interactions (which reduces structural collinearity without changing the substantive model), or using penalized methods (ridge, lasso) that stabilize estimates in exchange for controlled bias (Marquardt, 1970)."))
        ),
        influence = list(
            h(.("What it assesses:")),
            p(.("Evaluates whether results may be dominated by a few extreme, unusual, or highly influential cases rather than reflecting the general pattern of the data.")),
            h(.("Where it's used in AssumptionsLab:")),
            p(.("Independent Groups, Related Groups, ANOVA/ANCOVA, and Regression.")),
            h(.("An important distinction:")),
            p(.("An outlier is a value unusual relative to the rest of the data; an influential case is one whose presence or absence substantially changes the analysis's result. A case can be an outlier without being influential, and influential without being an obvious outlier — this is why several diagnostics are needed, not just one (Belsley et al., 1980; Cook, 1977)."))
        ),
        influenceGuide2 = list(
            h(.("IQR — what it assesses and when to use it:")),
            p(.("Flags values below Q1 - 1.5 × IQR or above Q3 + 1.5 × IQR of the variable being analyzed. In related groups this is applied to the paired differences, not the original measurements. It is the simplest check and does not depend on any fitted model — which makes it a natural starting point before fitting any analysis.")),
            h(.("Studentized residual — what it assesses and when to use it:")),
            p(.("Identifies observations with a large discrepancy from what the model predicts, expressed in standard-error units. It is useful for detecting cases the model fits poorly, but a large residual does not automatically mean that case is moving the estimates — for that, Cook's D or DFFITS are needed.")),
            h(.("Leverage — what it assesses and a caution about the threshold:")),
            p(.("Identifies unusual combinations of predictor values, without yet looking at the dependent variable. Rule of thumb: leverage > 2p/n, where p is the model's true number of estimated parameters (length(coef(fit)) in R) — not the number of selected variables. When a predictor is a factor with more than 2 levels, each additional level adds a parameter through dummy coding; using the variable count instead of the true parameter count understates the threshold and flags too many cases.")),
            h(.("Cook's D — what it assesses and when to use it:")),
            p(.("Combines leverage and residual magnitude into a single measure of influence on the full set of estimated coefficients (Cook, 1977). Rule of thumb: Cook's D > 4/n. Treat it as a screening threshold, not as confirmed influence: 4/n is numerically larger (more lenient) the smaller the sample, and smaller (stricter) as it grows; even so, small samples tend to keep flagging several cases because individual Cook's D values tend to inflate when n is small.")),
            h(.("DFFITS — what it assesses and when to use it:")),
            p(.("Evaluates how much a case's own fitted value changes when it is removed and the model is refit without it (Belsley et al., 1980). Rule of thumb: |DFFITS| > 2 × sqrt(p/n). It is conceptually very close to Cook's D — both capture influence on the fit — so it is best read as corroborating evidence, not as an independent second finding.")),
            h(.("Mahalanobis D² — what it assesses and when to use it:")),
            p(.("Evaluates a case's multivariate distance from the predictor centroid, accounting for correlations among predictors (Mahalanobis, 1936). It is the multivariate analogue of leverage: it detects unusual predictor combinations even when no single variable is an extreme value on its own. Like leverage, it says nothing by itself about whether that unusual combination actually affects the results.")),
            h(.("Interpretation:")),
            p(.("A flagged case should not be removed automatically. It may be a data-entry error, a valid extreme case, or important substantive evidence that the model is not capturing well.")),
            h(.("Methodological decision:")),
            p(.("Review the original data point, run a sensitivity analysis (compare results with and without the case), and justify any exclusion with transparent criteria declared in advance — never chosen after seeing that the exclusion \"improves\" the result."))
        ),
        sphericity = list(
            h(.("What it assesses:")),
            p(.("Sphericity evaluates whether the variances of the differences between all possible pairs of repeated measurements are approximately equal (Mauchly, 1940).")),
            h(.("Where it's used in AssumptionsLab:")),
            p(.("Related Groups with three or more related measurements.")),
            p(.("It does not apply with only two measurements, because with two levels there is only one possible pair of differences, and no variances to compare against each other.")),
            h(.("Why it matters:")),
            p(.("The classic repeated-measures ANOVA assumes sphericity so that its F statistic follows the nominal F distribution. When it is violated, the observed F tends to inflate and the real Type I error exceeds the nominal one; the degrees-of-freedom corrections exist precisely to compensate for that inflation without changing the F statistic itself."))
        ),
        sphericityGuide2 = list(
            h(.("Mauchly — what it assesses and limitations:")),
            p(.("Tests H0: the variances of the differences between all pairs of levels are equal (sphericity), against H1: at least one pair differs (Mauchly, 1940). It is the historical reference test, but its behavior is not uniform: with small samples it can lack power to detect real violations, and with large samples it can flag deviations from sphericity too small to have practical relevance for the F statistic.")),
            h(.("Greenhouse-Geisser — what it does and when to use it:")),
            p(.("Rather than testing sphericity directly, it estimates how far the observed covariance pattern departs from it (epsilon, between 1/(k-1) and 1) and reduces the F test's degrees of freedom by multiplying them by that epsilon (Greenhouse & Geisser, 1959). The F statistic itself does not change; only the degrees of freedom used to obtain the p-value change, making the test more conservative. It is the recommended default correction when the estimated epsilon is low (< .75).")),
            h(.("Huynh-Feldt — what it does and when to use it:")),
            p(.("An alternative correction that adjusts the Greenhouse-Geisser epsilon to reduce its tendency to be overly conservative, especially when the sphericity violation is mild or moderate (Huynh & Feldt, 1976). The usual convention is to prefer Huynh-Feldt when the Greenhouse-Geisser-estimated epsilon exceeds .75, and Greenhouse-Geisser when it is below that.")),
            h(.("Lower-bound — what it does and when to use it:")),
            p(.("Applies the most extreme mathematically possible correction, assuming the worst-case violation of sphericity. It is useful as a safety bound — if the effect remains significant even under this severe correction, the conclusion is robust to any degree of sphericity violation — but it sacrifices considerable power and is rarely the final correction reported.")),
            h(.("Interpretation:")),
            p(.("p < .05 on Mauchly's test suggests a statistically significant deviation from sphericity. p >= .05 indicates that the result is compatible with approximate sphericity.")),
            h(.("Methodological decision:")),
            p(.("If there is evidence against sphericity, use the Greenhouse-Geisser or Huynh-Feldt correction depending on the estimated epsilon, consider the nonparametric Friedman test (Friedman, 1937) as an alternative, or fit a mixed model that does not assume sphericity, depending on the study design."))
        ),
        proportionalOdds = list(
            h(.("What it evaluates:")),
            p(.("The proportional-odds model assumes each predictor's effect on the ordinal outcome has the same magnitude at every cutpoint between categories (the \"parallel lines\" assumption). Brant's (1990) test is the standard check.")),
            h(.("Where it is used in AssumptionsLab:")),
            p(.("ordCheck (ordinal logistic regression).")),
            p(.("It does not apply to binary logistic regression (logCheck) or multinomial regression (multCheck), since neither has multiple cutpoints whose slopes need to be compared against each other.")),
            h(.("Why it matters:")),
            p(.("When violated, a single reported odds ratio per predictor misrepresents its real effect - understating it at some cutpoints and overstating it at others - and can lead to mistaken substantive conclusions about which categories a predictor actually discriminates between."))
        ),
        proportionalOddsGuide2 = list(
            h(.("Brant — what it evaluates and its limitations:")),
            p(.("Tests H0: each predictor's coefficient is equal across every cutpoint (proportional odds), against H1: at least one cutpoint differs (Brant, 1990). It compares the coefficients from separate binary logits fit at each cutpoint against the single coefficient the proportional-odds model reports. It has limited power in small samples or with sparse categories.")),
            h(.("Partial proportional-odds model — what it does and when to use it:")),
            p(.("Lets a specific predictor's coefficient vary across cutpoints while every other predictor keeps a single constant coefficient. It is the most surgical option when Brant flags one or two specific predictors rather than a widespread violation.")),
            h(.("Multinomial model — what it does and when to use it:")),
            p(.("Treats every category of the outcome as nominal, with no assumed order or proportionality relationship between coefficients. It is the safer alternative when Brant's omnibus test is significant and affects several predictors at once, at the cost of not exploiting the categories' order.")),
            h(.("Reporting cutpoint-specific effects — what it does and when to use it:")),
            p(.("Instead of forcing a single model, independent binary logits are fit at each cutpoint and a different odds ratio is reported for each. It is a transparent alternative when the goal is describing the pattern of the relationship rather than producing a single parsimonious model.")),
            h(.("Interpretation:")),
            p(.("p < .05 on Brant's omnibus test suggests a statistically significant deviation from proportional odds. p >= .05 indicates the result is compatible with approximate proportional odds.")),
            h(.("Methodological decision:")),
            p(.("If there is evidence against proportional odds, first check which individual predictor(s) Brant's test flags (not just the omnibus). With one or two specific predictors, consider a partial-proportional-odds model; with a widespread violation, consider a multinomial model (multCheck) or report the cutpoint-specific effects separately."))
        ),
        independenceIrrelevantAlternatives = list(
            h(.("What it evaluates:")),
            p(.("The multinomial-logit model assumes the relative odds between any two categories of the outcome do not depend on which other categories are also available in the choice set - Independence of Irrelevant Alternatives (IIA). It is the one assumption unique to this model family, with no equivalent in binary or ordinal logistic regression.")),
            h(.("Where it is used in AssumptionsLab:")),
            p(.("multCheck (multinomial logistic regression).")),
            p(.("It does not apply to binary logistic regression (logCheck), since with only two categories there are no \"irrelevant alternatives\" that could be dropped from the choice set. It also does not apply to ordinal logistic regression (ordCheck), which instead depends on the proportional-odds (parallel-lines) assumption across ordered cutpoints.")),
            h(.("Why it matters:")),
            p(.("When violated, the reported odds ratios between categories are not stable - they shift depending on which other categories are present in the choice set - which undermines interpreting a single set of coefficients as describing a consistent preference among alternatives."))
        ),
        independenceIrrelevantAlternativesGuide2 = list(
            h(.("Hausman-McFadden — what it evaluates and its limitations:")),
            p(.("Tests H0: the coefficients shared between the full model and a model refit without one category do not change beyond sampling error (IIA holds), against H1: they do change (Hausman & McFadden, 1984). It is computed once per omitted category. In finite samples, the statistic depends on the difference between two covariance matrices, which is not always invertible — a well-documented numerical degeneracy of this specific test, not evidence of a calculation error. When this happens, the result is reported as \"not computable\": this is not itself proof that IIA holds (Cheng & Long, 2007, document that Hausman-type IIA tests behave erratically in practice).")),
            h(.("Nested logit model — what it does and when to use it:")),
            p(.("Groups the outcome's categories into a hierarchical nest structure, allowing correlated unobserved factors within a nest while keeping independence across nests. It is the more principled alternative when the categories flagged by Hausman-McFadden share an identifiable substantive grouping.")),
            h(.("Multinomial probit model — what it does and when to use it:")),
            p(.("Drops the logit's independent-errors assumption altogether in favor of correlated, multivariate-normal errors across alternatives. It is the more general - but computationally heavier - alternative when no natural nest structure exists.")),
            h(.("Reporting coefficients with caution — what it does and when to use it:")),
            p(.("Rather than switching models, the categories flagged by the Hausman-McFadden test are documented explicitly and their odds ratios interpreted with added reservation. It is a transparent option when only one or two categories are flagged, not a widespread pattern.")),
            h(.("Interpretation:")),
            p(.("p < .05 on the Hausman-McFadden test for an omitted category suggests a statistically significant deviation from IIA for that category. p >= .05 indicates the result is compatible with IIA. A \"not computable\" result should not be read as evidence either for or against IIA: it is a known limitation of the test itself, not a substantive finding.")),
            h(.("Methodological decision:")),
            p(.("If at least one omitted category produces a significant statistic, consider a nested-logit or multinomial-probit model (neither requires IIA) before relying on this model's odds ratios, or document the flagged categories with explicit caution. If no category is significant - or the result is mostly \"not computable\" - this is compatible with (but not definitive proof of) IIA holding for this model."))
        ),
        countModels = list(
            h(.("What it evaluates:")),
            p(.("The Poisson model assumes the variance of the count equals its mean (equidispersion), and both the Poisson and the negative binomial models assume their own distribution produces the zeros in the data. Overdispersion (variance above the mean) and excess zeros (more zeros than the model expects) are the two most common departures in applied count data (Cameron & Trivedi, 2013).")),
            h(.("Where it is used in AssumptionsLab:")),
            p(.("countCheck (Poisson and negative binomial regression, with an optional exposure offset).")),
            p(.("It does not apply to linear, logistic, ordinal or multinomial regression, whose outcomes are not counts.")),
            h(.("Why it matters:")),
            p(.("Under overdispersion, a Poisson model keeps roughly correct rate ratios but understates their standard errors, so confidence intervals are too narrow and p-values too small. Excess zeros that neither model reproduces usually signal a separate process, such as a group of cases that cannot experience the event at all."))
        ),
        countModelsGuide2 = list(
            h(.("Overdispersion tests - what they evaluate and their limitations:")),
            p(.("The Dean-Lawless score test (Dean & Lawless, 1989) and the Cameron-Trivedi auxiliary regression (Cameron & Trivedi, 1990) both test H0: equidispersion against H1: overdispersion of the negative binomial form. They only need the Poisson fit. Like any test, they become very sensitive in large samples, so the dispersion ratio and the mean-variance plot should be read alongside them.")),
            h(.("Likelihood ratio test - what it does and when to use it:")),
            p(.("Compares the Poisson and negative binomial log-likelihoods. Because the Poisson model is the negative binomial with alpha = 0, a value on the boundary of the parameter space, the statistic follows a 50:50 mixture of 0 and chi-square(1), so the usual p-value is halved (Self & Liang, 1987).")),
            h(.("Excess zeros - what it evaluates and its limitations:")),
            p(.("The observed number of zeros is compared with the sum of each case's fitted probability of a zero under each model. van den Broek's (1995) score test compares a zero-inflated Poisson model with the Poisson model; overdispersion alone also produces extra zeros, so a significant result does not by itself justify a zero-inflated model.")),
            h(.("Interpretation:")),
            p(.("A Pearson ratio close to 1 and non-significant tests are compatible with equidispersion. A ratio clearly above 1 with significant tests indicates overdispersion; a ratio clearly below 1 indicates underdispersion.")),
            h(.("Methodological decision:")),
            p(.("With overdispersion, prefer the negative binomial model (or at least robust standard errors). If the negative binomial still under-predicts the zeros, consider a zero-inflated or hurdle model and justify the extra process substantively. With underdispersion, consider a generalized Poisson or Conway-Maxwell-Poisson model."))
        ),
        robust = list(
            h(.("What they assess:")),
            p(.("These are not assumptions in themselves, but methodological responses when a specific assumption is not defensible or when results are sensitive to important deviations from it.")),
            h(.("Where they're used in AssumptionsLab:")),
            p(.("Independent Groups, Related Groups, ANOVA/ANCOVA, and Regression.")),
            h(.("How to choose among them:")),
            p(.("The right alternative depends on which specific assumption fails, not on a general preference for \"robust\" methods. Switching methods without first identifying which assumption was violated tends to complicate interpretation without solving the real problem."))
        ),
        robustGuide2 = list(
            h(.("Transformations of the dependent variable:")),
            p(.("log(Y): useful with marked positive skewness and values strictly greater than 0. sqrt(Y): useful with count data or mild-to-moderate positive skewness, and values >= 0. 1/Y: useful only with caution for severe positive skewness and values not close to zero (the inverse transformation is highly sensitive near zero). Every transformation changes the scale on which results are interpreted, so this should be reported explicitly.")),
            h(.("Welch — what it does and when to use it:")),
            p(.("Adjusts the degrees of freedom of the t or F statistic so that equal variances across groups are not required (Welch, 1947). It is especially recommended when group sizes are unequal, because in that scenario the classic t-test or ANOVA is more sensitive to unequal variances than when groups are similarly sized.")),
            h(.("Brown-Forsythe (as a robust ANOVA alternative):")),
            p(.("Applies the same logic as Welch but using location estimators more robust than the mean, which makes it preferable when, in addition to unequal variances, there is skewness or outliers in the groups (Brown & Forsythe, 1974).")),
            h(.("Wilcoxon signed-rank — what it does and when to use it:")),
            p(.("A nonparametric alternative for two related measurements: instead of comparing the paired differences directly, it converts them to ranks and tests whether the sum of positively signed ranks differs from what is expected under H0 of symmetry around zero (Wilcoxon, 1945). It requires the differences to be approximately symmetric, though not normal; with strong asymmetry in the differences, this test also loses validity.")),
            h(.("Friedman — what it does and when to use it:")),
            p(.("Extends the same rank-based logic to three or more related measurements, converting each row (subject) into ranks across conditions and testing whether those ranks differ systematically across conditions (Friedman, 1937). It is the natural nonparametric alternative to repeated-measures ANOVA when normality of the differences is not defensible.")),
            h(.("HC3 robust errors — what they do and when to use them:")),
            p(.("Recompute a regression's coefficient standard errors so they remain valid even under heteroscedasticity, without changing the coefficients' point estimates (MacKinnon & White, 1985). HC3 is the recommended variant in small or moderate samples, because it corrects the bias in variance estimates better than other HC variants in that regime.")),
            h(.("Mixed models:")),
            p(.("Appropriate for nested, longitudinal, repeated, or otherwise dependence-structured data that a simple fixed-effects model cannot represent. Rather than correcting a violated assumption after the fact, they directly model the source of dependence (for example, variation across subjects or across groups) as part of the model structure.")),
            h(.("Methodological decision:")),
            p(.("Do not switch methods based on a single isolated test. The alternative chosen should respond to the overall pattern of evidence, the study design, and the research question — not just the p-value of one diagnostic."))
        ),
        notes = list(
            p(.("Use this library as a general methodological reference. Use the specific analyses to interpret results with your own data."))
        )
    )
}

.al_library_tables <- function(.) {
    list(
        normalityTable = list(
            c("Shapiro-Wilk", .("General normality, the battery's primary test"), .("Small or moderate sample; the default reference test"), .("In R it is only computed for up to 5000 observations (Shapiro & Wilk, 1965)")),
            c("Lilliefors", .("Normality via K-S with corrected critical values"), .("When an alternative to Shapiro-Wilk based on the cumulative distribution is needed"), .("The classic (uncorrected) K-S test is not valid here; the corrected version is always used (Lilliefors, 1967)")),
            c("Anderson-Darling", .("Normality with more weight on the tails"), .("When heavy tails or extreme values are a concern"), .("Can be overly sensitive to trivial tail deviations with very large samples")),
            c("Cramér-von Mises", .("Normality comparing the entire cumulative distribution"), .("When deviations are suspected to be spread across the central body, not at one point"), .("Less used and less familiar than Shapiro-Wilk or Anderson-Darling in applied practice")),
            c("Shapiro-Francia", .("A Shapiro-Wilk variant with a different calculation"), .("Moderate or large samples, as complementary evidence to Shapiro-Wilk"), .("Its practical advantage over Shapiro-Wilk is marginal in many scenarios (Shapiro & Francia, 1972)")),
            c(.("Pearson chi-square"), .("Normality by comparing observed and expected frequencies by interval"), .("As additional secondary evidence, not as the primary test"), .("Depends on how the data are grouped into intervals; tends to be less powerful than full-distribution tests")),
            c("Jarque-Bera", .("Normality by jointly evaluating skewness and kurtosis"), .("Moderate or large samples"), .("Less stable and informative in very small samples (Jarque & Bera, 1987)")),
            c("Skewness test", .("Skewness of the distribution only"), .("Point diagnostic on the direction of skewness (long left or right tail)"), .("Evaluates only one aspect of shape; does not replace a general normality test")),
            c("Kurtosis test", .("Peakedness and tail weight only"), .("Point diagnostic on heavy tails, light tails, or a concentration different from normal"), .("Evaluates only one aspect of shape; does not replace a general normality test"))
        ),
        homoscedasticityGroupTable = list(
            c("Levene", .("Equality of variances using absolute deviations from each group's mean"), .("General, widely used first check"), .("Based on the mean; less robust than Brown-Forsythe to skewness or heavy tails")),
            c("Brown-Forsythe", .("Equality of variances using absolute deviations from each group's median"), .("Preferred alternative to Levene when there is skewness, heavy tails, or outliers"), .("Slight power loss versus Levene when data are genuinely normal")),
            c("Bartlett", .("Equality of variances assuming normality within each group"), .("When per-group normality is reasonable and the most powerful test is wanted"), .("Very sensitive to non-normality; may flag heterogeneity that is actually non-normality")),
            c("Fligner-Killeen", .("Equality of variances via a rank-based test on the deviations"), .("Non-normal data, outliers, or when the most robust alternative is wanted"), .("Lower power than Levene or Bartlett when the data are in fact approximately normal")),
            c(.("Hartley's Fmax"), .("Ratio between the largest and smallest group variance"), .("Quick descriptive indicator, especially in balanced designs"), .("Not a formal test with a well-defined distribution outside balanced designs"))
        ),
        homoscedasticityRegTable = list(
            c("Breusch-Pagan", .("Whether residual variance depends linearly on predictors or fitted values"), .("Standard heteroscedasticity check in linear regression"), .("Assumes the shape of any heteroscedasticity is approximately linear in the predictors")),
            c("White", .("General heteroscedasticity, including non-linear and interaction patterns"), .("When a more complex form of heteroscedasticity than linear is suspected"), .("Less powerful than Breusch-Pagan when the real heteroscedasticity is in fact simple and linear")),
            c("Goldfeld-Quandt", .("Difference in residual variance between two ordered zones of the model"), .("When a clear break point in variance is suspected along an ordering variable"), .("Requires choosing the ordering variable and omitting a central band of data")),
            c(.("Spearman |residuals| vs fitted"), .("Whether residual magnitude grows or shrinks monotonically with fitted values"), .("Exploratory, nonparametric, easy-to-read diagnostic"), .("Only captures monotonic relationships; not a formal test in the sense of the previous three"))
        ),
        linearityTable = list(
            c(.("Bivariate correlation"), .("Simple linear association between two variables"), .("Initial exploratory glance"), .("Only detects linear association; does not replace diagnosing the full model")),
            c(.("dCor / copula entropy"), .("Linear and non-linear dependence, without assuming a functional form"), .("Exploratory complement to Pearson when a non-linear relationship is suspected"), .("A dCor-Pearson gap is a signal, not proof; needs confirming with a scatterplot")),
            c(.("Quadratic term"), .("Curvature of a specific predictor against the original model"), .("Quick check, one predictor at a time"), .("Does not replace RESET, which evaluates the full model")),
            c("Box-Tidwell", .("Whether a specific predictor needs a power transformation"), .("Reference test for logit-linearity (logistic regression)"), .("Evaluates one predictor at a time; requires x > 0; does not assess joint curvature (Box & Tidwell, 1962)")),
            c("Ramsey RESET", .("Overall functional misspecification of the model"), .("Global check after reviewing individual predictors"), .("Indicates something is wrong, but not what (curvature, omitted interaction, outlier) (Ramsey, 1969)")),
            c("Rainbow test", .("Whether the linear fit is stable across the full range of fitted values"), .("Complement to RESET when non-linearity may be concentrated at the extremes"), .("Requires a reasonable sample (at least 20 cases in this module) for the central sub-model"))
        ),
        independenceTable = list(
            c("Durbin-Watson", .("First-order residual autocorrelation"), .("Simple static linear regression"), .("Not appropriate if the model includes the lagged dependent variable as a regressor; does not handle higher-order lags well")),
            c("Breusch-Godfrey", .("Residual autocorrelation up to a chosen order p"), .("Econometric, dynamic, or multi-lag regressions"), .("The order p must be chosen")),
            c("Ljung-Box", .("Joint autocorrelation across several lags"), .("Diagnosing residuals of ARIMA/SARIMA and time-series models"), .("Indicates joint dependence, but does not by itself identify which lag or its shape")),
            c("Runs test", .("Randomness of the sequence based on sign changes or position relative to a reference"), .("Nonparametric, exploratory diagnostic"), .("Does not directly measure lag-based linear autocorrelation"))
        ),
        multicollinearityTable = list(
            c("VIF", .("How much a coefficient's variance is inflated by its association with the other predictors"), .("VIF < 5 acceptable; 5-10 concern; ≥ 10 severe problem"), .("The thresholds are conventions, not universal rules (O'Brien, 2007); does not identify which variables are redundant with which")),
            c(.("Tolerance"), .("The inverse of VIF (1/VIF)"), .("> .20 acceptable; < .20 concern; < .10 severe problem"), .("Same information as VIF on a different scale; adds nothing new by itself")),
            c(.("Maximum correlation"), .("The largest bivariate correlation between pairs of predictors"), .("|r| ≥ .80 suggests possible bivariate redundancy"), .("Only detects collinearity between pairs; does not see redundancy spread across three or more predictors")),
            c(.("Condition index"), .("The ratio between the largest and smallest eigenvalue of the standardized predictor matrix"), .("> 15 concern; > 30 severe collinearity"), .("Requires also reviewing variance proportions by dimension to know which predictors are involved")),
            c(.("Eigenvalues / determinant"), .("How close the predictor matrix is to being singular"), .("Values near zero suggest redundant dimensions"), .("An aggregate signal; does not directly identify which combination of predictors is redundant")),
            c(.("Model rank"), .("Whether an exact linear dependency exists among predictors"), .("Rank < number of columns indicates non-estimable parameters"), .("An exact-identification problem, distinct from the approximate collinearity the measures above capture"))
        ),
        influenceTable = list(
            c("IQR", .("Distance of a value from the interquartile range"), .("Outside Q1 − 1.5·IQR or Q3 + 1.5·IQR"), .("Looks only at the variable univariately; does not consider the model or other variables")),
            c(.("Studentized residual"), .("The case's discrepancy from the fitted model, in standardized error units"), .("|studentized residual| > 3"), .("A large residual flags poor local fit, not necessarily that the case moves the estimates")),
            c("Leverage", .("How unusual the case's combination of predictors is"), .("Leverage > 2p/n (p = true number of estimated parameters)"), .("High leverage alone does not imply the case is influential if its residual is small")),
            c("Cook's D", .("The case's joint influence on all of the model's estimates"), .("Cook's D > 4/n, as a screening threshold (Cook, 1977)"), .("4/n is more lenient with small samples and stricter with large ones; treat as screening, not confirmation")),
            c("DFFITS", .("How much the case's own fitted value changes when it is excluded from the model"), "|DFFITS| > 2·√(p/n)", .("Sensitive to the same kind of cases as Cook's D; best read together, not separately")),
            c("Mahalanobis D²", .("The case's multivariate distance from the predictor centroid"), .("Large values relative to a chi-square with p degrees of freedom"), .("Detects unusual predictor combinations (multivariate leverage), not whether those combinations affect the result (Mahalanobis, 1936)"))
        ),
        sphericityTable = list(
            c("Mauchly (W)", .("Tests the hypothesis of exact sphericity"), .("As an initial check before deciding whether to correct degrees of freedom"), .("Unstable with small samples and overly sensitive (flags trivial violations) with large samples")),
            c("Greenhouse-Geisser", .("Corrects degrees of freedom by multiplying them by an estimated epsilon"), .("Conservative default correction, especially if epsilon < .75"), .("Can be excessively conservative (loses power) when the real violation is mild")),
            c("Huynh-Feldt", .("Corrects degrees of freedom with an adjusted epsilon, less conservative than GG"), .("Preferred over Greenhouse-Geisser when the estimated epsilon > .75"), .("Can overestimate epsilon (be too liberal) in small samples")),
            c("Lower-bound", .("Applies the most severe possible correction (epsilon = 1/(k-1))"), .("As an extreme conservative bound when avoiding false positives is the top priority"), .("Very conservative: sacrifices considerable statistical power relative to GG or HF"))
        ),
        proportionalOddsTable = list(
            c(.("Brant test"), .("Tests whether each predictor's coefficient is equal across every cutpoint"), .("As an initial check right after fitting the model, before interpreting a single OR per predictor"), .("Limited power in small samples or sparse categories; a non-significant result is weak evidence of proportionality")),
            c(.("Partial proportional-odds model"), .("Lets only the predictors flagged by Brant vary by cutpoint, keeping the rest at a single coefficient"), .("When Brant flags one or two specific predictors, not the full omnibus"), .("Less standardized across statistical software; interpretation gets more complex when mixing constant and varying effects")),
            c(.("Multinomial model (multCheck)"), .("Abandons the proportionality assumption by treating each category as nominal"), .("When the violation is widespread (significant omnibus, several predictors affected)"), .("Loses the efficiency of exploiting the categories' order; more parameters to estimate")),
            c(.("Report cutpoint-specific effects"), .("Fits separate binary logits at each cutpoint and reports a different OR for each"), .("As a transparent alternative when the goal is documenting the pattern rather than forcing a single model"), .("No single pooled model summarizes the effect; comparisons across cutpoints lose some statistical efficiency"))
        ),
        independenceIrrelevantAlternativesTable = list(
            c(.("Hausman-McFadden test"), .("Refits the model omitting each outcome category in turn and compares the coefficients shared with the full model"), .("As an initial check right after fitting the multinomial model, before relying on the reported odds ratios"), .("The statistic is not always computable (the covariance-matrix difference can fail to be invertible in finite samples); a non-computable result is not proof that IIA holds")),
            c(.("Nested logit model"), .("Groups outcome categories into a hierarchical nest structure, allowing correlation between alternatives within the same nest"), .("When the IIA violation follows a substantively meaningful grouping pattern (e.g., categories sharing unobserved attributes)"), .("Requires specifying a reasonable nest structure a priori; results are sensitive to that choice")),
            c(.("Multinomial probit model"), .("Replaces the logit's independent-errors assumption with multivariate-normal errors, allowing correlation between alternatives"), .("When enough computing time is available and no natural nest structure exists"), .("Computationally more expensive; has no closed form and requires numerical integration or simulation")),
            c(.("Report coefficients with caution"), .("Instead of discarding the model, the categories flagged by Hausman-McFadden are documented and their odds ratios interpreted with added reservation"), .("As a transparent alternative when the violation affects only one or two specific categories rather than the general pattern"), .("Does not fix the underlying problem, only documents it, leaving interpretive caution to the reader"))
        ),
        countModelsTable = list(
            c(.("Pearson dispersion ratio"), .("Divides the Pearson chi-square by its degrees of freedom; about 1 under the Poisson model"), .("As the first, descriptive check of any Poisson fit"), .("A ratio, not a test; with small means it can be unreliable")),
            c(.("Dean-Lawless and Cameron-Trivedi tests"), .("Test the Poisson model against negative binomial overdispersion, var = mu + alpha mu^2"), .("To confirm formally what the ratio suggests"), .("Very sensitive in large samples, where a trivial excess of variance becomes significant")),
            c(.("Likelihood ratio test, Poisson vs negative binomial"), .("Compares the two fits; the p-value is halved because alpha = 0 lies on the boundary"), .("To decide between the two models"), .("Needs the negative binomial fit to converge; glm.nb() warns when there is no overdispersion")),
            c(.("Observed vs expected zeros and van den Broek score test"), .("Compares the observed zeros with the zeros each model predicts; the score test checks a zero-inflated Poisson against the Poisson model"), .("When the outcome has many zeros"), .("The score test is confounded by overdispersion; read it with the negative binomial expected zeros")),
            c(.("Negative binomial, zero-inflated and hurdle models"), .("Model the extra variance, or a separate process for the zeros"), .("When overdispersion or excess zeros are confirmed"), .("Zero-inflated and hurdle models need a substantive justification for the extra process"))
        ),
        robustTable = list(
            c(.("Unequal variances across groups"), .("Welch correction"), "Independent Groups, ANOVA", .("Adjusts the degrees of freedom of the t or F statistic without assuming equal variances (Welch, 1947)")),
            c(.("Unequal variances, with skewness or outliers"), .("Brown-Forsythe (as robust ANOVA)"), "ANOVA", .("Extends Welch's logic using location estimators more robust than the mean")),
            c(.("Non-normal but symmetric paired differences"), "Wilcoxon signed-rank", .("Related Groups (two measurements)"), .("Uses the ranks of the differences instead of their values, without assuming normality (Wilcoxon, 1945)")),
            c(.("Three or more related measurements, non-normal"), "Friedman", .("Related Groups (three or more measurements)"), .("Extends the rank-based logic to more than two related measurements (Friedman, 1937)")),
            c(.("Heteroscedastic residuals"), .("HC3 robust standard errors"), "Regression", .("Corrects the standard errors of coefficients without changing their point estimates (MacKinnon & White, 1985)")),
            c(.("Marked skewness of the dependent variable"), .("Transformation (log, root, inverse)"), .("Any analysis with a continuous variable"), .("Changes the variable's scale so its distribution more closely approaches normality")),
            c(.("Complex dependence structure (nested, longitudinal)"), .("Mixed models"), .("Repeated, nested, or hierarchical data"), .("Explicitly models the correlation between related observations instead of assuming independence"))
        )
    )
}

# -----------------------------------------------------------------------------
# .al_library_markdown(blocks)
#
# Turns a list of blocks into the Markdown a jamovi Text element expects:
# paragraphs separated by a blank line, a heading as a bold line of its own,
# and consecutive list items as one Markdown list.
#
# ES: Convierte los bloques en el Markdown que espera un elemento Text de
# jamovi: párrafos separados por una línea en blanco, encabezados en
# negrita y los ítems consecutivos como una sola lista.
# -----------------------------------------------------------------------------
.al_library_markdown <- function(blocks) {
    paras <- character(0)
    in_list <- FALSE
    for (b in blocks) {
        if (identical(b$kind, "li")) {
            item <- paste0("- ", b$text)
            if (in_list)
                paras[length(paras)] <- paste0(paras[length(paras)], "\n", item)
            else
                paras <- c(paras, item)
            in_list <- TRUE
        } else {
            paras <- c(paras, if (identical(b$kind, "h")) paste0("**", b$text, "**") else b$text)
            in_list <- FALSE
        }
    }
    paste(paras, collapse = "\n\n")
}

assumptionLibraryClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "assumptionLibraryClass",
    inherit = assumptionLibraryBase,
    private = list(
        .init = function() {

            . <- .al_lang(self$options)$tr

            texts <- .al_library_text(.)
            for (name in names(texts))
                self$results[[name]]$setContent(.al_library_markdown(texts[[name]]))

            tables <- .al_library_tables(.)
            for (name in names(tables)) {
                table <- self$results[[name]]
                keys <- vapply(table$columns, function(column) column$name, character(1))
                rows <- tables[[name]]
                for (i in seq_along(rows))
                    table$addRow(rowKey = i, values = stats::setNames(as.list(rows[[i]]), keys))
            }
        },

        .run = function() {
            # Everything is static and filled in .init(); which category is
            # shown is declared in jamovi/assumptionlibrary.r.yaml.
            # ES: Todo es estático y se llena en .init(); la categoría visible
            # se declara en jamovi/assumptionlibrary.r.yaml.
            invisible(NULL)
        }
    )
)
