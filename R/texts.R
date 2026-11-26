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
# Text repository.
# ES: Repositorio de textos.
#
# Every user-facing report string that is assembled at runtime from a
# (section, key) lookup lives here, written in English inside jamovi's `.()`
# so the jamovi compiler extracts it into the same catalog as the interface
# text (jamovi/i18n/catalog.pot and es.po; `jmvtools::i18nUpdate("es")` adds
# new strings, and `jmvtools::prepare()` compiles es.po into
# inst/i18n/es.json). The report therefore follows jamovi's own language
# setting, like the menus and result titles. Adding a language needs only
# one more .po file; no R code changes.
#
# .al_text_table() takes the translator of the current run (`.`, see
# .al_lang() in shared-helpers.R) and returns the translated table; it is
# built once per run, on first use, and kept in `lang`.
#
# Each entry is a whole sentence or paragraph, never a line fragment, so a
# translator always sees complete text. Character vectors are lists of
# items (bullets, paragraphs or table cells, depending on the caller).
#
# ES: Todo texto del informe que se arma en tiempo de ejecución a partir de
# una búsqueda (sección, clave) vive aquí, escrito en inglés dentro de `.()`
# de jamovi para que el compilador de jamovi lo extraiga al mismo catálogo
# que el texto de la interfaz (jamovi/i18n/catalog.pot y es.po;
# `jmvtools::i18nUpdate("es")` agrega los textos nuevos y
# `jmvtools::prepare()` compila es.po en inst/i18n/es.json). Así el informe
# sigue el ajuste de idioma de jamovi, igual que los menús y los títulos de
# los resultados. Agregar un idioma solo requiere otro archivo .po, sin
# cambiar código R.
#
# .al_text_table() recibe el traductor de la ejecución actual (`.`, ver
# .al_lang() en shared-helpers.R) y devuelve la tabla traducida; se arma una
# vez por ejecución, al primer uso, y se guarda en `lang`.
#
# Cada entrada es una oración o un párrafo completo, nunca un fragmento de
# línea, para que quien traduce vea siempre el texto entero. Los vectores
# de caracteres son listas de elementos (viñetas, párrafos o celdas de
# tabla, según quien los usa).
# -----------------------------------------------------------------------------
.al_text_table <- function(.) list(
    common = list(
        moduleTitle = .("AssumptionsLab"),
        briefGuide = .("Brief guide"),
        appliedInterpretation = .("Applied interpretation"),
        methodologicalDecision = .("Methodological decision"),
        notes = .("Notes"),
        references = .("References"),
        seeLibrary = .("See Assumption Library for methodological details."),
        doNotDeleteAutomatically = .("A flagged case should not be removed automatically."),
        reviewAndJustify = .("Review the original value and justify any decision."),
        notComputed = .("Not computed"),
        compatible = .("Compatible"),
        significantDeviation = .("Significant deviation"),
        sigCodes = .("Significance codes: * p < .05, ** p < .01, *** p < .001."),
        statsSymbols = .("Statistical symbols are kept in international format."),
        colVariable = .("Variable"),
        colGroup = .("Group"),
        colTest = .("Test"),
        colN = .("n"),
        colMissing = .("Missing"),
        colMean = .("M"),
        colSD = .("SD"),
        colMedian = .("Mdn"),
        colMin = .("Min"),
        colMax = .("Max"),
        colQ1 = .("Q1"),
        colQ3 = .("Q3"),
        colIQR = .("IQR"),
        colLowerLimit = .("Lower limit"),
        colUpperLimit = .("Upper limit"),
        colOutliers = .("Outliers"),
        colExtreme = .("Extreme"),
        colStatistic = .("Statistic"),
        colValue = .("Value"),
        colP = .("p"),
        colSig = .("Sig."),
        colDf = .("df"),
        colDf1 = .("df1"),
        colDf2 = .("df2"),
        colInterpretation = .("Interpretation"),
        colConclusion = .("Conclusion")
    ),
    independentGroups = list(
        title = .("Independent Groups"),
        intro = c(
        .("Use this analysis when you want to review whether a comparison between independent groups has defensible methodological assumptions."),
        .("The goal is not only to compute tests, but to help justify the statistical decision with evidence obtained from your own data.")),
        tableDesign = .("Design summary"),
        tableDescriptives = .("Descriptive statistics by group"),
        tableOutliers = .("Outlier evaluation by group"),
        tableCaseDiagnostics = .("Case diagnostics"),
        tableNormality = .("Normality tests by group"),
        tableNormalitySummary = .("Normality decision summary"),
        tableVariance = .("Variance homogeneity"),
        designGuide = c(
        .("Use this review when rows represent independent units and each unit belongs to one group only."),
        .("Before choosing Student's t, ANOVA, Welch or Kruskal-Wallis, review group sizes, missing data, normality, variances and potentially influential cases."),
        .("The analysis is more defensible when the decision is based on the whole body of evidence rather than on a single isolated test.")),
        descriptivesGuide = c(
        .("Descriptive statistics allow you to observe initial group differences before applying an inferential test."),
        .("Compare means, medians and dispersion. Large differences between mean and median may suggest skewness or the influence of extreme values."),
        .("Descriptives do not test hypotheses by themselves, but they guide the reading of assumptions.")),
        outliersGuide = c(
        .("The IQR rule identifies unusual values within each group."),
        .("An outlier is not necessarily an error: it may be a real, rare or influential case."),
        .("In small groups, a few extreme values can modify means, variances and normality tests."),
        .("Review the original data before deciding whether to run sensitivity analyses.")),
        caseDiagnosticsGuide = c(
        .("These diagnostics identify potentially influential or unusual cases in the group comparison model."),
        .("Cook's D flags cases that may change model estimates."),
        .("Mahalanobis D² helps detect values that are far from the expected pattern."),
        .("A flagged case should not be removed automatically: review the value, compare analyses with and without the case, and justify the decision.")),
        normalityGuide = c(
        .("Normality is reviewed within each independent group because parametric procedures assume approximately normal distributions at each level of the factor. p >= .05 is compatible with approximate normality; this does not prove perfect normality. p < .05 suggests a significant deviation from normality."),
        .("In small samples, complement tests with plots and outlier review.")),
        varianceGuide = c(
        .("These tests review whether group variances are reasonably similar. p >= .05 is compatible with homogeneous variances. p < .05 suggests significant variance differences."),
        .("If group sizes are unequal, variance heterogeneity can affect interpretation more strongly and may make Welch or robust methods preferable.")),
        normalityCompatible = .("Compatible with approximate normality"),
        normalityDeviation5 = .("Significant deviation at 5%"),
        normalityDeviation1 = .("Significant deviation at 1%"),
        normalityDeviation001 = .("Significant deviation at 0.1%"),
        normalitySummaryCompatible = .("Compatible with approximate normality"),
        normalitySummaryDeviation = .("Significant deviation in at least one test"),
        varianceCompatible = .("Compatible with homogeneous variances"),
        varianceDeviation = .("Significant variance differences"),
        recommendationParametricCaution = c(
        .("The parametric analysis may be defensible with caution if the design is truly independent, variances are compatible with homogeneity and normality does not show severe deviations."),
        .("If there are small groups, relevant imbalance or influential cases, review sensitivity analyses before making the final decision.")),
        recommendationWelchOrRobust = c(
        .("Consider Welch, Brown-Forsythe, Kruskal-Wallis or a robust alternative if there is variance heterogeneity, clear normality deviation or influential cases that change the conclusions."),
        .("The choice should combine statistical evidence, study design and the research question."))
    ),
    regression = list(
        title = .("Simple & Multiple Regression"),
        intro = c(
        .("Use this analysis to review whether a simple or multiple linear regression model has defensible methodological assumptions."),
        .("The goal is not only to detect problems, but to teach how each diagnostic affects the interpretation of coefficients, standard errors, confidence intervals and p-values."),
        .("The final decision should combine statistical evidence, study design, data quality and the substantive meaning of the model.")),
        designGuide = c(
        .("Linear regression evaluates the relationship between a numeric dependent variable and one or more predictors."),
        .("Before interpreting coefficients, review whether the dependent variable is quantitative, whether predictors are correctly defined and whether the sample size is reasonable for the number of estimated parameters."),
        .("A model can be computed even when assumptions are weak, but its conclusions may be less defensible if those problems are not documented.")),
        linearityGuide = c(
        .("Linearity evaluates whether the relationship between numeric predictors and the dependent variable can be reasonably described by a straight line."),
        .("This assumption matters because a linear model may produce misleading coefficients when the true relationship is curved, stepped or changes direction."),
        .("Tests and exploratory terms help detect curvature, but they should be read together with residual plots and substantive knowledge."),
        .("If there is evidence of nonlinearity, consider transformations, polynomial terms, splines or another type of model.")),
        residualNormalityGuide = c(
        .("Normality in regression is evaluated on residuals, not on the raw dependent variable."),
        .("This assumption mainly affects classical inferences such as confidence intervals and significance tests, especially in small samples. p >= .05 is compatible with approximate residual normality; it does not prove perfect normality. p < .05 suggests a significant deviation."),
        .("Review whether it is due to skewness, heavy tails, outliers or model misspecification.")),
        homoscedasticityGuide = c(
        .("Homoscedasticity evaluates whether residual variability is approximately constant across fitted values."),
        .("When this assumption is weakened, coefficients may still be useful, but standard errors, confidence intervals and p-values may become unreliable. p >= .05 is compatible with approximate homoscedasticity. p < .05 suggests heteroscedasticity."),
        .("In that case, consider robust standard errors, transformations or alternative models.")),
        independenceGuide = c(
        .("Error independence means residuals should not be correlated by measurement order, time, space, group, classroom, participant or another structure."),
        .("This assumption depends mainly on study design, not only on a statistical test."),
        .("The Durbin-Watson test helps review first-order autocorrelation when data have a natural order."),
        .("If observations are clustered, repeated or nested, consider mixed, longitudinal or other methods that represent that dependence.")),
        multicollinearityGuide = c(
        .("Multicollinearity evaluates whether predictors contain highly redundant information."),
        .("It usually does not bias predictions, but it can make coefficients unstable, increase standard errors and complicate interpretation of individual predictors."),
        .("VIF values close to 1 suggest low collinearity."),
        .("High values indicate that a predictor is too well explained by other predictors."),
        .("If collinearity is important, consider combining variables, removing redundant predictors, centering variables or changing the analytic question.")),
        correlationMatrixGuide = c(
        .("These two tables complement the multicollinearity checks with an overview of the association between the dependent variable and all numeric predictors, each in APA 7 format (lower triangle, numbered variables)."),
        .("The first reports conventional Pearson correlation (linear association only); the second reports distance correlation (Szekely et al., 2007), which detects linear and non-linear association alike."),
        .("The discordance table flags pairs where dCor notably exceeds |Pearson r|, with copula entropy (copent) as a second line of evidence.")),
        influenceGuide = c(
        .("Influence diagnostics identify cases that may substantially change coefficients, fit or model conclusions."),
        .("Studentized residuals, leverage, Cook's D and DFFITS review different aspects of the same problem: how unusual a case is and how much it changes the model."),
        .("An influential case should not be removed automatically."),
        .("Review whether it is a data entry error, a valid but extreme case, or a sign that the model does not represent all subgroups well.")),
        transformationsGuide = c(
        .("Transformations may help when there is strong skewness, nonlinear relationships or nonconstant residual variance."),
        .("They should not be used only to improve an assumption test; they should make statistical and substantive sense."),
        .("When a variable is transformed, the interpretation of the coefficient also changes."),
        .("Document which transformation was used, why it was needed and how the resulting model should be interpreted.")),
        robustOptionsGuide = c(
        .("Robust options are not an automatic correction, but alternatives when classical assumptions are not sufficiently defensible."),
        .("Robust standard errors may help under heteroscedasticity."),
        .("Robust models may reduce the influence of extreme cases."),
        .("Mixed, generalized or nonlinear models may be more appropriate if the problem comes from design, distribution or functional form."))
    ),
    logistic = list(
        title = .("Logistic Regression"),
        intro = c(
        .("Use this analysis to review whether a binary logistic regression model has defensible methodological assumptions before interpreting odds ratios, coefficients or classifications."),
        .("The goal is not only to detect problems, but to teach how each diagnostic affects the validity of conclusions: complete separation, sample size per variable, linearity in the logit, goodness of fit, discrimination, multicollinearity and the influence of individual cases.")),
        designGuide = c(
        .("Logistic regression models the probability of a binary event as a function of one or more predictors."),
        .("Before interpreting the model, check that the dependent variable has exactly two categories, that events and non-events are reasonably represented, and that the ratio of events per predictor variable (EPV) is sufficient."),
        .("Peduzzi et al. (1996) recommend at least 10 events per predictor variable as a practical reference; with low EPV, coefficients can be biased and standard errors can be unreliable.")),
        separationGuide = c(
        .("Complete or quasi-complete separation occurs when a predictor (or combination of predictors) perfectly or nearly perfectly classifies the events."),
        .("When this happens, the maximum-likelihood algorithm does not converge stably: coefficients and standard errors can inflate to extreme values with no substantive meaning."),
        .("Very large coefficients together with very large standard errors are the typical signal of this problem."),
        .("If separation is present, consider checking whether the predictor is a disguised copy of the dependent variable, combining rare categories, or using Firth's (1993) penalized logistic regression, designed specifically to correct maximum-likelihood bias and produce finite estimates even under separation.")),
        linearityGuide = c(
        .("Logistic regression does not assume the event relates linearly to the predictors, but that the logit (the log odds) does."),
        .("This assumption is usually checked with the Box-Tidwell procedure (Box & Tidwell, 1962), which adds interaction terms between each numeric predictor and its own logarithm."),
        .("A significant term suggests the real relationship is not linear in the logit and that the predictor may need a transformation or a nonlinear term (for example, a polynomial or a spline)."),
        .("This check only applies to numeric predictors; categorical predictors do not have this assumption.")),
        goodnessOfFitGuide = c(
        .("Goodness of fit evaluates whether the model, as a whole, reasonably reproduces the observed data."),
        .("Residual deviance compares the fitted model with a saturated model; very high values relative to the degrees of freedom suggest a poor fit."),
        .("The Hosmer-Lemeshow test (1980) groups cases by predicted probability and compares observed and expected frequencies; a low p-value suggests the model does not fit well in some probability range."),
        .("This test loses sensitivity with very large samples and is sensitive to the number of groups chosen, so it should be interpreted together with other diagnostics, not in isolation.")),
        discriminationGuide = c(
        .("Discrimination evaluates how well the model distinguishes between cases with and without the event, regardless of whether the predicted probabilities are well calibrated."),
        .("The area under the ROC curve (AUC) summarizes this ability (Hanley & McNeil, 1982): 0.5 is equivalent to no discrimination (chance level), values between 0.7 and 0.8 are considered acceptable, and values above 0.8 are considered good in most applied settings."),
        .("A high AUC does not guarantee good calibration or good goodness of fit: a model can discriminate well and still over- or under-estimate the real probabilities.")),
        multicollinearityGuide = c(
        .("Multicollinearity evaluates whether predictors carry very redundant information."),
        .("It does not usually bias predictions, but it can make coefficients unstable, widen standard errors and make it harder to interpret each predictor individually."),
        .("VIF values close to 1 suggest low collinearity."),
        .("High values indicate a predictor is too well explained by other predictors."),
        .("If meaningful collinearity is present, consider combining variables, removing redundant predictors, centering variables or changing the analytic question.")),
        correlationMatrixGuide = c(
        .("These two tables complement the multicollinearity checks with an overview of the association between the dependent variable (coded 0/1) and all numeric predictors, each in APA 7 format (lower triangle, numbered variables)."),
        .("The first reports conventional Pearson correlation (equivalent to a point-biserial correlation with a binary variable; linear association only); the second reports distance correlation (Szekely et al., 2007), which detects linear and non-linear association alike."),
        .("The discordance table flags pairs where dCor notably exceeds |Pearson r|, with copula entropy (copent) as a second line of evidence.")),
        influenceGuide = c(
        .("Influence diagnostics identify cases that may substantially change coefficients or model fit."),
        .("In logistic regression, Cook's D and leverage are computed on the model's working scale (weighted Pearson residuals), so they are approximate analogues, not identical counterparts, of their linear-regression versions (Pregibon, 1981, introduced these extensions and the weighted hat matrix for generalized linear models)."),
        .("An influential case should not be removed automatically."),
        .("Check whether it is a recording error, a valid but extreme case, or a sign that the model does not represent all subgroups well.")),
        oddsRatiosGuide = c(
        .("The odds ratio (OR) is the exponentiated coefficient; it represents how much the odds of the event are multiplied for each one-unit increase in the predictor (or for belonging to a category versus the reference category)."),
        .("An OR of 1 indicates no effect, values above 1 indicate higher probability of the event, and values below 1 indicate lower probability."),
        .("The OR's confidence interval is more informative than the p-value alone: if it includes 1, the effect is not statistically distinguishable from no association."),
        .("Common error: interpreting the OR as if it were a relative risk."),
        .("The two coincide only when the event is rare; with common events (above 10%), the OR systematically overstates the real relative effect (Zhang & Yu, 1998)."))
    ),
    count = list(
        title = .("Count and Rate Regression"),
        designGuide = c(
        .("Poisson and negative binomial regression model a count (number of events, visits, admissions, errors) as a function of one or more predictors, on the log scale: each coefficient is a log rate ratio."),
        .("When cases were observed for different lengths of time or represent populations of different sizes, the exposure (person-time, population at risk) enters as an offset, log(exposure), so the model describes rates rather than raw counts."),
        .("The Poisson model assumes the variance of the count equals its mean (equidispersion); the negative binomial model relaxes this by adding a dispersion parameter, var = mu + mu^2/theta (Hilbe, 2011)."),
        .("Before interpreting rate ratios, check that the outcome is a non-negative whole number, how many zeros it has, and how its variance compares with its mean.")),
        dispersionGuide = c(
        .("Overdispersion means the counts vary more than the Poisson model allows. It is the most common problem with Poisson regression in applied data, and it is caused by unmeasured heterogeneity between cases, clustering or contagion between events."),
        .("Its consequence is serious: coefficients stay roughly unbiased, but standard errors are too small, so confidence intervals are too narrow and p-values too optimistic."),
        .("The Pearson dispersion ratio (Pearson chi-square divided by its degrees of freedom) should be close to 1 under the Poisson model; values clearly above 1 indicate overdispersion and values clearly below 1 indicate underdispersion (Cameron & Trivedi, 2013)."),
        .("Two formal tests are reported against the negative binomial alternative: the Dean-Lawless score test (Dean & Lawless, 1989) and the Cameron-Trivedi auxiliary regression (Cameron & Trivedi, 1990)."),
        .("The likelihood ratio test compares the Poisson and negative binomial fits directly; because the Poisson model sits on the boundary of the negative binomial (alpha = 0), its p-value is half the usual chi-square tail (Self & Liang, 1987).")),
        modelComparisonGuide = c(
        .("This table compares the two candidate models fitted to the same cases. A lower AIC or BIC indicates a better balance between fit and complexity."),
        .("The negative binomial dispersion parameter is shown as theta and as alpha = 1/theta: alpha close to 0 (theta very large) means the negative binomial reduces to the Poisson model.")),
        zerosGuide = c(
        .("Count data often contain more zeros than a Poisson or negative binomial model expects, for example when part of the population cannot experience the event at all (structural zeros)."),
        .("The check compares the observed number of zeros with the number each model expects, the sum of each case's fitted probability of a zero."),
        .("van den Broek's (1995) score test compares the Poisson model with a zero-inflated Poisson model. Overdispersion alone also produces extra zeros, so a significant result must be read together with the zeros expected by the negative binomial model."),
        .("If the negative binomial model reproduces the zeros well, a zero-inflated model is usually unnecessary; if it does not, consider a zero-inflated or hurdle model and justify it substantively.")),
        residualsGuide = c(
        .("Pearson residuals divide the raw residual by the standard deviation the model predicts; deviance residuals measure each case's contribution to the deviance. Both should scatter around 0 with no pattern against the fitted values."),
        .("With counts, and above all with small means, neither residual is normally distributed even when the model is right, so their Q-Q plot is hard to read. The randomized quantile residuals of Dunn and Smyth (1996) solve this: when the model is correct they are exactly standard normal."),
        .("A funnel shape in the residuals suggests the variance function is wrong (for example, Poisson when the data are overdispersed); a curve suggests a misspecified predictor or a missing term.")),
        linearityGuide = c(
        .("A count model assumes each numeric predictor acts linearly on the log of the expected count, not on the count itself."),
        .("The check replaces each numeric predictor's linear term with a natural cubic spline of 3 degrees of freedom and compares the two fits with a likelihood ratio test on 2 degrees of freedom (Harrell, 2015). A significant result suggests the relationship on the log scale is curved."),
        .("The partial residual plot shows the same thing graphically: the working residuals plus the predictor's linear contribution, against the predictor. A smooth curve that departs from the straight line points to non-linearity."),
        .("This check only applies to numeric predictors; categorical predictors do not have this assumption.")),
        multicollinearityGuide = c(
        .("Multicollinearity evaluates whether predictors carry very redundant information. It does not bias the predicted counts, but it makes coefficients unstable and widens their standard errors."),
        .("The generalized variance inflation factor (GVIF) is reported, adjusted as GVIF^(1/(2 df)) so that categorical predictors with several levels are comparable with numeric ones; its square is on the usual VIF scale.")),
        influenceGuide = c(
        .("Influence diagnostics identify cases that may substantially change the coefficients. In generalized linear models, Cook's distance and leverage come from the weighted hat matrix of the fitted model (Pregibon, 1981; McCullagh & Nelder, 1989)."),
        .("With count data, a single case with a very large count can dominate the fit, especially under the Poisson model, whose variance is small for large counts."),
        .("An influential case should not be removed automatically. Check whether it is a recording error, a valid but extreme case, or a sign that the model does not represent all subgroups well.")),
        rateRatiosGuide = c(
        .("The incidence rate ratio (IRR) is the exponentiated coefficient: it is the factor by which the expected count (or rate, when there is an offset) is multiplied for each one-unit increase in the predictor, or for a category compared with the reference category."),
        .("An IRR of 1 indicates no effect; above 1, a higher rate; below 1, a lower rate."),
        .("The confidence intervals shown are Wald intervals from the model being diagnosed. If the data are overdispersed and the Poisson model is used, these intervals are too narrow."))
    ),
    path = list(
        title = .("Path Analysis"),
        outlierAnalysisGuide = c(
        .("Before interpreting any coefficient, it is worth checking whether some cases deviate unusually from the rest of the sample, since path analysis relies on ordinary least squares, which are sensitive to extreme values."),
        .("Mahalanobis distance (D²) evaluates how atypical each case is considering all model variables jointly (multivariate); a case is flagged as an outlier when D² exceeds the 97.5th percentile of a chi-square distribution with degrees of freedom equal to the number of model variables (Mahalanobis, 1936)."),
        .("Leverage and Cook's distance are computed on the equation with the most predictors in the model (the most complete one), as a practical, more demanding reference for influence within the equation system."),
        .("A case is flagged on leverage when it exceeds 2p/n (p = estimated parameters, including the intercept; n = cases), and on Cook's distance when it exceeds 4/n."),
        .("No single criterion is definitive on its own: a case can have high leverage without being influential (if its observed value matches what was expected), or the reverse."),
        .("Common error: automatically removing any flagged case."),
        .("First check whether it is a data entry error, a valid but extreme case, or an observation that is substantively important for the theory before deciding to exclude it.")),
        designGuide = c(
        .("Classical path analysis (Wright, 1934; Duncan, 1966) decomposes a system of hypothesized causal relationships among observed variables into a series of regression equations, one for each endogenous variable (with at least one incoming arrow)."),
        .("Before interpreting the coefficients, check that the ratio of cases per path relationship is sufficient: a common practical reference is at least 10 cases per estimated parameter (Bentler & Chou, 1987)."),
        .("This approach assumes the model is recursive: there are no feedback loops (for example, A causing B and B causing A at the same time).")),
        equationsGuide = c(
        .("Each endogenous variable is predicted from the variables pointing to it in the path diagram, fitted as an independent linear regression."),
        .("Because all variables are standardized before fitting, each coefficient is directly a standardized path coefficient in Wright's (1934) classical sense: it represents the expected change in the dependent variable, in standard deviations, per one standard deviation increase in the predictor."),
        .("Standardized path coefficients allow comparing the relative importance of different predictors within the same equation, which unstandardized coefficients do not allow when variables are on different scales.")),
        normalityGuide = c(
        .("Ordinary least squares estimation of each equation does not require the observed variables themselves to be normal, but it does require the residuals of each equation to be reasonably so, mainly so that the significance tests on the path coefficients are reliable."),
        .("Two complementary tests are reported per equation: Shapiro-Wilk, generally the most powerful in small-to-moderate samples, and Anderson-Darling (Anderson & Darling, 1952), which weights the tails of the distribution more heavily (Razali & Wah, 2011)."),
        .("With very large samples, even small, practically irrelevant deviations can become statistically significant; it is worth also reviewing the Q-Q plot or histogram of residuals before concluding.")),
        multivariateNormalityGuide = c(
        .("This complementary diagnostic evaluates multivariate normality of the full set of model variables using Mardia's (1970) skewness and kurtosis tests."),
        .("Separate-equation OLS path analysis does not require multivariate normality to be valid; this check matters more if you later fit the same model with a joint estimator (for example, maximum-likelihood-based SEM).")),
        correlationMatrixGuide = c(
        .("These two tables complement the within-equation multicollinearity checks with an overall view of association among all model variables, each in APA 7 format (lower triangle, numbered variables)."),
        .("The first reports the usual Pearson correlation (linear association only); the second reports the distance correlation (Székely et al., 2007), which detects both linear and non-linear association equally."),
        .("A large gap between dCor and the absolute Pearson value for the same pair suggests the relationship between those two variables may not be linear, which matters because path analysis assumes linear relationships among variables.")),
        homoscedasticityGuide = c(
        .("Homoscedasticity assumes the variance of each equation's residuals is roughly constant across fitted values."),
        .("Two tests are reported per equation: Breusch & Pagan (1979), which assumes the error variance is a linear function of the predictors, and White (1980), more general, which also captures curvature and interaction effects on the variance at the cost of fewer degrees of freedom."),
        .("Heteroscedasticity does not bias the path coefficients, but it does bias their standard errors, which can lead to wrong conclusions about which paths are statistically significant.")),
        multicollinearityGuide = c(
        .("Multicollinearity evaluates whether the predictors within the same equation carry very redundant information."),
        .("The variance inflation factor (VIF) summarizes this redundancy; values above 5 raise a moderate concern and above 10 are considered a severe problem (Marquardt, 1970)."),
        .("It only applies to equations with two or more predictors; it does not make sense for an endogenous variable with a single direct predictor.")),
        indirectEffectsGuide = c(
        .("When a variable influences another both directly and through one or more mediating variables, the total effect decomposes into a direct effect plus an indirect effect, following Wright's (1934) path-tracing rule: the indirect effect of each mediating chain is the product of the path coefficients along that chain."),
        .("Baron and Kenny (1986) popularized the classical framework for distinguishing full from partial mediation in the social sciences and psychology."),
        .("The Sobel (1982) z-test offers an analytic way to evaluate the significance of a simple indirect effect (a single mediator), though bootstrap confidence intervals are now preferred because they are less sensitive to the non-normality of the product of two coefficients.")),
        crossEntropyGuide = c(
        .("Copula entropy (CE) is a dependence measure based on the copula of the variables (Ma & Sun, 2011), which captures the dependence structure between variables independently of their marginal distributions, without assuming any particular functional form (linear, monotonic, etc.)."),
        .("Unlike Pearson's r (which only detects linear association) or even dCor (which detects any dependence but is based on distances), CE is mathematically linked to the mutual information between the two variables, measured in bits of information (Shannon entropy)."),
        .("A significant p-value (p < .05) means the observed copula entropy is higher than expected under independence, suggesting the variables share information beyond what would occur by chance."),
        .("A non-significant p-value means the data are compatible with the variables being statistically independent."),
        .("Common error: interpreting a non-significant CE test as proof that two variables are completely unrelated."),
        .("Like all statistical tests, failure to reject independence does not prove independence - it only means the data do not provide strong evidence against it."),
        .("With small samples, the test may lack power to detect real but weak dependencies."),
        .("Sample-size caveat: the copula entropy estimator used by copent() (based on Kozachenko- Leonenko k-nearest neighbors) requires sufficient sample size to produce stable estimates."),
        .("With very small samples (n < 20), the p-values may be unreliable."),
        .("With very large samples, even trivial dependencies can become statistically significant."),
        .("Practical use in path analysis: if two variables that are not directly connected in your model show significant copula entropy dependence, this may suggest an omitted path or a common cause not included in the model."),
        .("However, do not add paths based solely on this diagnostic - theoretical justification should always come first."),
        .("Reference: copula entropy was formally defined by Ma and Sun (2011); its estimation uses the Kozachenko and Leonenko (1987) k-nearest-neighbor method."),
        .("The copent R package implements this estimator for independence testing."),
        .("Full references are available in the Bibliography tool.")),
        crossEntropyInterpretation = c(
        .("Copula entropy complements the Pearson and dCor matrices with a third perspective on dependence: information theory. While Pearson measures linear association and dCor measures distance-based dependence, copula entropy measures how much information two variables share regardless of the shape of their relationship."),
        .("When all three diagnostics agree (low Pearson, low dCor, non-significant CE), the evidence for independence between that pair of variables is strong. When they disagree - for example, low Pearson but significant CE - this suggests a non-linear dependence that Pearson does not capture but that information theory does detect."),
        .("Common error: using this table to reverse-engineer the model by adding paths based on the highest CE values. Path relationships should come from theory, not be discovered post-hoc from the data - otherwise the model capitalizes on chance and will not replicate."))
    ),
    time = list(
        title = .("Time Series"),
        intro = c(
        .("Time Series maps the statistical assumptions and methodological conditions that should be reviewed before and after fitting a time-series model. Unlike a single assumption, temporal modeling requires checking several conditions — stationarity, lag structure, specification, and residual behavior — whose exact relevance depends on the model chosen. Select a model to see its methodological matrix: Model → Assumptions/Conditions → Diagnostic → Methodological Role → Decision.")),
        interpretation = .("The matrix shows only the conditions relevant to the selected model. Not all of them are strict mathematical requirements: applicability conditions (which must hold for the model to make sense), specification criteria (which guide the choice of parameters such as p, d, q, or the cointegration rank), and post-fit diagnostics (which evaluate whether the already-estimated model adequately captured the structure of the data) are distinguished. Conflating these categories is a common methodological error: for example, residual normality in an ARIMA model is a useful diagnostic, not a strict mathematical requirement for estimation."),
        scopeNote = c(
        .("This version implements the methodological matrix (Model → Assumptions/Conditions → Diagnostic → Methodological Role → Decision), the minimum recommended battery of statistical diagnostics per model (unit-root tests, ACF/PACF, Ljung-Box, ARCH-LM, Jarque-Bera, and for VAR/VECM/GARCH also Johansen, Portmanteau, CUSUM/Nyblom, and sign bias, depending on the selected model), diagnostic plots (series, ACF/PACF, residuals, and, for GARCH, conditional volatility), and optional external regressors for ARIMA/SARIMA (ARIMAX/SARIMAX). Every result is classified as a graded evidence tier, never as an absolute \"assumption met/not met\" verdict.")),
        seriesRowLabel = .("Time series"),
        modelRowLabel = .("Selected model"),
        noSeriesSelected = .("No time series variable has been selected."),
        modelLabels = list(
            arima = .("ARIMA"),
            sarima = .("SARIMA"),
            ets = .("ETS / Exponential Smoothing"),
            var = .("VAR"),
            vecm = .("VECM"),
            garch = .("GARCH")
        ),
        modelDescriptions = list(
            arima = c(
        .("ARIMA (AutoRegressive Integrated Moving Average) models a single time series from its own past: its lagged values (autoregressive component, p), the number of differences needed to make it stationary (integrated component, d), and lagged errors (moving-average component, q). Use it when you have a single series without marked seasonality, or when you want to model an already deseasonalized series. It offers short- to medium-term forecasts with confidence intervals, and is the conceptual foundation for SARIMA, ARIMAX, and GARCH. On its own it does not capture relationships with other series (that is what VAR/VECM are for) or changes in variance over time (that is what GARCH is for).")),
            sarima = .("SARIMA extends ARIMA by adding a seasonal component (P, D, Q, s) that explicitly models the repetition of patterns every s periods (for example, every 12 months in monthly data). Use it when the series shows a clear, recognizable seasonal pattern — sales with holiday peaks, electricity consumption with daily or weekly cycles, tourism with a high season — that a plain ARIMA could not capture. It is this module's default model precisely because it covers the most frequent case in applied practice. In exchange for that added power, it requires more historical data (ideally several complete cycles) and a correctly specified periodicity (frequency)."),
            ets = c(
        .("ETS (Error, Trend, Seasonal) is a family of exponential-smoothing models that decomposes the series into level, trend, and seasonality, each with its own form (none, additive, or multiplicative, with or without damping). Unlike ARIMA/SARIMA, it does not rely on unit-root tests or differencing: it explicitly weights recent observations more heavily than older ones. It is a strong alternative when the series has a relatively simple and stable trend and/or seasonality, and tends to be easier to interpret and communicate than an equivalent SARIMA. It is less suited when the dynamics depend on other variables (use VAR/VECM/ARIMAX) or when volatility itself is the object of study (use GARCH).")),
            var = c(
        .("VAR (Vector AutoRegressive) models several series simultaneously, where each one is explained by its own lags and by the lags of every other series in the system, without imposing in advance which variable causes which. Use it when you are interested in the joint dynamics between two or more stationary series — for example, interest rate and inflation, or sales of complementary products — and want to study how a shock in one series propagates to the others (impulse-response functions) without specifying rigid causal relationships. It requires all series to be stationary (or treated to become so); if the series share a long-run equilibrium relationship while each is individually non-stationary, VECM is usually the more appropriate choice instead.")),
            vecm = .("VECM (Vector Error Correction Model) is the version of VAR appropriate when the series are not individually stationary but are cointegrated: there is a linear combination of them that is stationary, reflecting a long-run equilibrium relationship (for example, between the exchange rate and relative prices, or between consumption and income). Use it when theory or evidence suggest the series move together in the long run even though they may drift apart temporarily in the short run. Besides the short-run dynamics typical of a VAR, it offers an explicit error-correction mechanism that pushes the system back toward equilibrium. Fitting a VECM without first checking for cointegration (with the Johansen test) is a common methodological error: without genuine cointegration, a VAR in differences is usually the correct choice."),
            garch = c(
        .("GARCH (Generalized AutoRegressive Conditional Heteroskedasticity) does not model the level of the series but its conditional variance: it allows volatility to change over time and cluster into calm and turbulent periods, instead of assuming it is constant. Use it with high-frequency financial or economic series (asset returns, exchange rates, interest rates) where the magnitude of fluctuations varies systematically over time. It offers risk/volatility estimates that feed more realistic prediction intervals than an ARIMA with constant variance. It assumes the input series is already approximately mean-stationary (typically returns, not price levels); if there is asymmetry between positive and negative shocks, it is worth considering GJR-GARCH or EGARCH as alternatives."))
        ),
        matrix = list(
            arima = list(
                c(
        .("Stationarity"),
        .("Stationarity / unit-root behavior"),
        .("ADF / KPSS / PP"),
        .("Condition for determining whether the series requires differencing."),
        .("Guides non-seasonal differencing (d).")),
                c(
        .("Temporal dependence"),
        .("Autocorrelation and partial autocorrelation"),
        .("ACF / PACF"),
        .("Supports identification of the p and q orders."),
        .("Guides ARIMA specification.")),
                c(
        .("Residuals"),
        .("Residual independence"),
        .("Ljung-Box"),
        .("Checks whether temporal structure remains after fitting."),
        .("Revise the specification if residual autocorrelation remains.")),
                c(
        .("Residuals"),
        .("Residual distribution (diagnostic)"),
        .("Q-Q / Shapiro-Wilk"),
        .("Evaluates the adequacy of the error distribution."),
        .("Use as a complementary diagnostic; do not treat it as a universal mathematical requirement."))
            ),
            sarima = list(
                c(
        .("Stationarity"),
        .("Non-seasonal stationarity / unit root"),
        .("ADF / KPSS / PP"),
        .("Determines the need for non-seasonal differencing."),
        .("Guides d and the transformation of the series.")),
                c(
        .("Seasonality"),
        .("Seasonal structure"),
        .("ACF / PACF / seasonal diagnostic"),
        .("Determines whether systematic dependence exists at seasonal lags."),
        .("Guides D and the seasonal orders P and Q.")),
                c(
        .("Temporal dependence"),
        .("Autocorrelation and partial autocorrelation"),
        .("ACF / PACF"),
        .("Supports identification of the non-seasonal orders p and q."),
        .("Guides SARIMA specification.")),
                c(
        .("Residuals"),
        .("Residual independence"),
        .("Ljung-Box"),
        .("Checks whether temporal structure remains after fitting."),
        .("Revise the specification if residual autocorrelation remains.")),
                c(
        .("Residuals"),
        .("Residual distribution (diagnostic)"),
        .("Q-Q / Shapiro-Wilk"),
        .("Evaluates the adequacy of the error distribution."),
        .("Use as a complementary diagnostic; do not treat it as a universal mathematical requirement."))
            ),
            ets = list(
                c(
        .("Temporal structure"),
        .("Temporal dependence and systematic pattern"),
        .("ACF / PACF / time plots"),
        .("Identifies structure that should be explained by level, trend, and seasonality."),
        .("Guides the specification of the ETS components.")),
                c(
        .("Seasonality"),
        .("Presence and stability of the seasonal pattern"),
        .("Seasonal ACF / graphical diagnostic"),
        .("Determines whether a seasonal component is needed."),
        .("Guides ETS specification.")),
                c(
        .("Residuals"),
        .("Absence of residual autocorrelation"),
        .("Ljung-Box / residual ACF"),
        .("Checks that the model has captured the relevant temporal structure."),
        .("Revise the specification if dependence remains.")),
                c(
        .("Residuals"),
        .("Error distribution and variance (diagnostic)"),
        .("Q-Q / ACF / residual variability"),
        .("Evaluates the adequacy of the errors for inference and forecasting."),
        .("Consider transformation or an alternative specification when appropriate."))
            ),
            var = list(
                c(
        .("Stationarity"),
        .("Stationarity of the system's series"),
        .("ADF / KPSS / PP"),
        .("Determines whether the series are suitable for a VAR in levels or require treatment."),
        .("Avoids specifications incompatible with the order of integration.")),
                c(
        .("Specification"),
        .("Lag structure"),
        .("AIC / BIC / HQ criteria + lag diagnostics"),
        .("Determines an appropriate lag length."),
        .("Selects the VAR's temporal specification.")),
                c(
        .("Residuals"),
        .("Absence of residual autocorrelation"),
        .("LM / Portmanteau / multivariate Ljung-Box"),
        .("Checks that no systematic temporal dependence remains."),
        .("Revise the number of lags or the specification.")),
                c(
        .("Residuals"),
        .("Residual heteroscedasticity (diagnostic)"),
        .("Heteroscedasticity tests"),
        .("Evaluates the stability of the error variance."),
        .("Consider robust inference or an alternative specification.")),
                c(
        .("Residuals"),
        .("Multivariate normality (diagnostic)"),
        .("Mardia / Q-Q diagnostic"),
        .("Evaluates a condition useful for certain inferential procedures."),
        .("Treat as a diagnostic, not as a universal VAR requirement."))
            ),
            vecm = list(
                c(
        .("Integration"),
        .("Series with a compatible order of integration"),
        .("ADF / KPSS / PP"),
        .("Determines the order of integration of the variables."),
        .("Verifies the precondition for considering cointegration.")),
                c(
        .("Cointegration"),
        .("Existence and rank of cointegration"),
        .("Johansen"),
        .("Determines how many long-run equilibrium relationships exist."),
        .("Guides the VECM's cointegration rank.")),
                c(
        .("Specification"),
        .("Lag structure"),
        .("AIC / BIC / HQ"),
        .("Determines a lag length compatible with the system."),
        .("Guides the dynamic specification.")),
                c(
        .("Residuals"),
        .("Absence of residual autocorrelation"),
        .("LM / Portmanteau"),
        .("Checks whether unexplained temporal dependence remains."),
        .("Revise lags or specification if autocorrelation is present.")),
                c(
        .("Residuals"),
        .("Heteroscedasticity and normality (diagnostic)"),
        .("Heteroscedasticity tests / Mardia"),
        .("Evaluates the properties of the errors for inference."),
        .("Use robust diagnostics when distributional assumptions do not hold."))
            ),
            garch = list(
                c(
        .("Conditional variance"),
        .("ARCH effects / conditional heteroscedasticity"),
        .("ARCH-LM"),
        .("Determines whether the variance depends on past information."),
        .("Justifies considering a volatility model.")),
                c(
        .("Stationarity"),
        .("Stability condition of the variance process"),
        .("Parameter diagnostic / persistence"),
        .("Evaluates whether the variance dynamics are compatible with a stable process."),
        .("Revise the specification if persistence implies instability.")),
                c(
        .("Standardized residuals"),
        .("Absence of autocorrelation in the mean"),
        .("ACF / Ljung-Box"),
        .("Checks that the mean dynamics have been adequately captured."),
        .("Revise the mean equation if dependence remains.")),
                c(
        .("Standardized residuals"),
        .("Absence of residual ARCH effects"),
        .("ARCH-LM / ACF of squared residuals"),
        .("Checks that the volatility dynamics have been captured."),
        .("Revise the GARCH specification if ARCH effects remain.")),
                c(
        .("Distribution"),
        .("Distribution of the innovations (diagnostic)"),
        .("Q-Q / Jarque-Bera / chosen distribution"),
        .("Evaluates the adequacy of the assumed distribution for the innovations."),
        .("Consider an alternative distribution when appropriate."))
            )
        ),
        actions = list(
            arima = list(
                stationarity = list(
                    justification = "",
                    suggestion = .("apply the suggested number of regular differences (d) before fitting the model, or let auto.arima determine it automatically; alternatively, use a variance-stabilizing transformation")
                ),
                ndiffs = list(
                    justification = "",
                    suggestion = .("apply the suggested number of regular differences (d) before fitting the model")
                ),
                ljungBox = list(
                    justification = "",
                    suggestion = .("increase the AR/MA order (p, q) or check for a missing structural component, considering an ARIMA with more lags")
                ),
                archLM = list(
                    justification = .("Residuals show conditional heteroscedasticity."),
                    suggestion = .("model the variance (ARIMA-GARCH) or use robust errors for inference")
                ),
                jarqueBera = list(
                    justification = "",
                    suggestion = .("use Student-t innovations or bootstrap for intervals and tests, rather than assuming normal errors")
                ),
                roots = list(
                    justification = .("The fitted model is not stationary/invertible."),
                    suggestion = .("review the ARIMA order or the differencing applied")
                )
            ),
            sarima = list(
                stationarity = list(
                    justification = "",
                    suggestion = .("apply the suggested number of regular differences (d) before fitting the model, or consider a variance-stabilizing transformation")
                ),
                ndiffs = list(
                    justification = "",
                    suggestion = .("apply the suggested number of regular differences (d) before fitting the model")
                ),
                nsdiffs = list(
                    justification = "",
                    suggestion = .("apply a seasonal difference (D) as suggested before proceeding")
                ),
                ljungBox = list(
                    justification = "",
                    suggestion = .("increase the non-seasonal AR/MA order (p, q) or check for a missing structural component")
                ),
                ljungBoxSeasonal = list(
                    justification = .("Seasonal structure may still be uncaptured."),
                    suggestion = .("review the seasonal component (P, D, Q)")
                ),
                archLM = list(
                    justification = .("Residuals show conditional heteroscedasticity."),
                    suggestion = .("model the variance (SARIMA-GARCH) or use robust errors for inference")
                ),
                jarqueBera = list(
                    justification = "",
                    suggestion = .("use Student-t innovations or bootstrap for intervals and tests, rather than assuming normal errors")
                ),
                roots = list(
                    justification = .("The fitted model is not stationary/invertible."),
                    suggestion = .("review the SARIMA order or the differencing applied")
                )
            ),
            ets = list(
                ljungBox = list(
                    justification = "",
                    suggestion = .("add an additional error, trend, or seasonal component to the ETS specification")
                ),
                archLM = list(
                    justification = .("Residuals show conditional heteroscedasticity."),
                    suggestion = .("transform the series or consider a model with non-constant variance")
                ),
                jarqueBera = list(
                    justification = "",
                    suggestion = .("use bootstrap or non-normal innovations for intervals and forecasts, rather than assuming normal errors")
                )
            ),
            var = list(
                stationarity = list(
                    justification = "",
                    suggestion = .("evaluate a VAR in differences or cointegration (VECM) instead of a VAR in levels, if several series are non-stationary")
                ),
                stability = list(
                    justification = .("The system is unstable."),
                    suggestion = .("reduce the lag order, review the specification, or difference the series")
                ),
                serial = list(
                    justification = .("Residual serial correlation suggests the current order is insufficient."),
                    suggestion = .("increase the number of VAR lags")
                ),
                archLM = list(
                    justification = "",
                    suggestion = .("consider a VAR with heteroscedasticity-robust errors or a multivariate volatility model (e.g., BEKK-GARCH)")
                ),
                jarqueBera = list(
                    justification = "",
                    suggestion = .("use robust inference (bootstrap) rather than relying on multivariate normality for hypothesis tests")
                ),
                cusum = list(
                    justification = .("At least one equation shows parameter instability."),
                    suggestion = .("consider a VAR with structural change or estimate by sub-periods")
                )
            ),
            vecm = list(
                stationarity = list(
                    justification = "",
                    suggestion = .("confirm the series are integrated of order 1 before proceeding with VECM; if any turns out stationary in levels, reconsider its inclusion")
                ),
                johansen = list(
                    justification = "",
                    suggestion = .("use the suggested cointegration rank as a starting point, confirming it against theoretical judgment and the full trace matrix before fixing it")
                ),
                stability = list(
                    justification = .("The system converted to VAR is unstable."),
                    suggestion = .("review the cointegration rank or the lag order")
                ),
                serial = list(
                    justification = .("Residual serial correlation suggests uncaptured structure."),
                    suggestion = .("increase the VECM's lag order K")
                ),
                archLM = list(
                    justification = "",
                    suggestion = .("consider a specification with heteroscedasticity-robust errors")
                ),
                jarqueBera = list(
                    justification = "",
                    suggestion = .("use robust inference (bootstrap) rather than relying on multivariate normality")
                )
            ),
            garch = list(
                stationarity = list(
                    justification = "",
                    suggestion = .("transform the series to returns (log-differences) before fitting the GARCH model, if it is not stationary")
                ),
                ljungBox = list(
                    justification = .("The conditional mean dynamics may not be well captured."),
                    suggestion = .("add a mean structure (ARMA) before the GARCH component")
                ),
                archLM = list(
                    justification = .("The GARCH(1,1) did not capture all the conditional heteroscedasticity."),
                    suggestion = .("consider a higher GARCH(p,q) order")
                ),
                persistence = list(
                    justification = "",
                    suggestion = .("consider an IGARCH model or review the sample length, given that persistence is very high (near 1)")
                ),
                signBias = list(
                    justification = .("There is evidence of asymmetry in how variance responds to positive/negative shocks."),
                    suggestion = .("evaluate GJR-GARCH or EGARCH instead of a symmetric GARCH")
                ),
                nyblom = list(
                    justification = .("Parameters are not stable over time."),
                    suggestion = .("re-estimate by sub-periods or consider a model with time-varying parameters")
                ),
                gof = list(
                    justification = .("The distribution fit is inadequate."),
                    suggestion = .("consider Student-t or generalized error (GED) innovations instead of the normal distribution")
                )
            )
        ),
        reading = list(
            stationarity = list(
                single = list(
                    none = .("The %s test finds no evidence of a unit root/non-stationarity in the series, supporting treating it as stationary."),
                    moderate = .("The %s test offers moderate evidence of non-stationarity in the series."),
                    clear = .("The %s test offers clear evidence of non-stationarity in the series."),
                    info = .("The %s test is reported for informational purposes."),
                    na = .("The %s test could not be computed for this run.")
                ),
                all_none = .("The %d unit-root/stationarity tests run agree in showing no evidence of non-stationarity, supporting treating the series as stationary."),
                all_deviant = .("The %d unit-root/stationarity tests run agree in signaling evidence of non-stationarity."),
                mixed = .("Of the %2$d unit-root/stationarity tests run, %1$d show no evidence of non-stationarity, while %3$s %4$s signal it; the evidence on the series' stationarity is mixed.")
            ),
            residualAutocorr = list(
                single = list(
                    none = .("The %s test finds no evidence of residual autocorrelation, supporting that the model captured the relevant temporal structure."),
                    moderate = .("The %s test offers moderate evidence of residual autocorrelation not captured by the model."),
                    clear = .("The %s test offers clear evidence of residual autocorrelation not captured by the model."),
                    info = .("The %s test is reported for informational purposes."),
                    na = .("The %s test could not be computed for this run.")
                ),
                all_none = .("The %d residual-autocorrelation tests run agree in showing no evidence of remaining temporal structure, supporting the current model specification."),
                all_deviant = .("The %d residual-autocorrelation tests run agree in signaling temporal structure not captured by the model."),
                mixed = .("Of the %2$d residual-autocorrelation tests run, %1$d show no evidence of remaining structure, while %3$s %4$s signal it; the evidence on the adequacy of the specification is mixed.")
            ),
            heteroscedasticity = list(
                single = list(
                    none = .("The %s test finds no evidence of conditional heteroscedasticity in the residuals."),
                    moderate = .("The %s test offers moderate evidence of conditional heteroscedasticity not captured in the residuals."),
                    clear = .("The %s test offers clear evidence of conditional heteroscedasticity not captured in the residuals."),
                    info = .("The %s test is reported for informational purposes."),
                    na = .("The %s test could not be computed for this run.")
                ),
                all_none = .("The %d conditional-heteroscedasticity tests run agree in showing no evidence of uncaptured residual variance."),
                all_deviant = .("The %d conditional-heteroscedasticity tests run agree in signaling uncaptured residual variance."),
                mixed = .("Of the %2$d conditional-heteroscedasticity tests run, %1$d show no evidence of uncaptured residual variance, while %3$s %4$s signal it; the evidence is mixed.")
            ),
            normality = list(
                single = list(
                    none = .("The %s test finds no evidence of deviation from normality in the residuals."),
                    moderate = .("The %s test offers moderate evidence of non-normality in the residuals."),
                    clear = .("The %s test offers clear evidence of non-normality in the residuals; keep in mind this is a complementary diagnostic, not a strict mathematical requirement for most models covered here."),
                    info = .("The %s test is reported for informational purposes."),
                    na = .("The %s test could not be computed for this run.")
                ),
                all_none = .("The %d normality tests run agree in showing no evidence of deviation from normality in the residuals."),
                all_deviant = .("The %d normality tests run agree in signaling deviation from normality in the residuals."),
                mixed = .("Of the %2$d normality tests run, %1$d show no evidence of deviation from normality, while %3$s %4$s signal it; the evidence is mixed.")
            ),
            stability = list(
                single = list(
                    none = .("The %s diagnostic shows no evidence of instability in the fitted model."),
                    moderate = .("The %s diagnostic offers moderate evidence of instability in the fitted model."),
                    clear = .("The %s diagnostic offers clear evidence of instability in the fitted model."),
                    info = .("The %s diagnostic is reported for informational purposes."),
                    na = .("The %s diagnostic could not be computed for this run.")
                ),
                all_none = .("The %d stability diagnostics run agree in showing no evidence of instability in the fitted model."),
                all_deviant = .("The %d stability diagnostics run agree in signaling instability in the fitted model."),
                mixed = .("Of the %2$d stability diagnostics run, %1$d show no evidence of instability, while %3$s %4$s signal it; the evidence on the model's stability is mixed.")
            ),
            cointegration = list(
                single = list(
                    none = .("%s finds no evidence of cointegration between the series."),
                    moderate = .("%s offers moderate evidence of cointegration between the series."),
                    clear = .("%s offers clear evidence of cointegration between the series."),
                    info = .("%s suggests, for informational purposes, a cointegration rank."),
                    na = .("%s could not be computed for this run.")
                ),
                informational = .("The Johansen tests run (trace and max-eigenvalue) suggest, for informational purposes, a cointegration rank; it should be weighed against theoretical judgment and the full trace matrix before being fixed."),
                all_none = .("The %d cointegration tests run agree in showing no evidence of a cointegrating relationship."),
                all_deviant = .("The %d cointegration tests run agree in signaling a cointegrating relationship between the series."),
                mixed = .("Of the %2$d cointegration tests run, %1$d show no evidence of cointegration, while %3$s %4$s signal it; the evidence is mixed.")
            ),
            garchPersistence = list(
                single = list(
                    none = .("%s falls within a typical range, with no sign of a near-integrated variance process."),
                    moderate = .("%s is reported at a moderate level of evidence."),
                    clear = .("%s is very high, close to integration, suggesting volatility shocks decay very slowly."),
                    info = .("%s is reported for informational purposes."),
                    na = .("Conditional variance persistence could not be computed for this run.")
                )
            ),
            garchAsymmetry = list(
                single = list(
                    none = .("The %s test finds no evidence of asymmetry in the variance's response to positive/negative shocks."),
                    moderate = .("The %s test offers moderate evidence of asymmetry in the variance's response."),
                    clear = .("The %s test offers clear evidence of asymmetry in the variance's response to positive/negative shocks."),
                    info = .("The %s test is reported for informational purposes."),
                    na = .("The sign-bias test could not be computed for this run.")
                )
            ),
            garchGof = list(
                single = list(
                    none = .("The %s test finds no evidence that the distribution assumed for the innovations is inadequate."),
                    moderate = .("The %s test offers moderate evidence that the assumed distribution does not fit well."),
                    clear = .("The %s test offers clear evidence that the assumed distribution does not fit the innovations well."),
                    info = .("The %s test is reported for informational purposes."),
                    na = .("The goodness-of-fit test could not be computed for this run.")
                )
            )
        ),
        foundations = list(
            unitRoot = c(
        .("Unit roots (ADF, Phillips-Perron, KPSS)"),
        .("ADF (Dickey & Fuller, 1979) and Phillips-Perron (Phillips & Perron, 1988) share the same null hypothesis — the series has a unit root, i.e. it is non-stationary — but correct for possible residual autocorrelation and heteroscedasticity differently: ADF adds lagged differences as regressors; Phillips-Perron adjusts the statistic with a nonparametric, Newey-West-type correction. KPSS (Kwiatkowski et al., 1992) reverses the logic: its null hypothesis is that the series IS stationary. This is why the three tests are reported together: if ADF and Phillips-Perron reject the unit root (small p) and KPSS fails to reject stationarity (large p), the evidence converges clearly. When the two families of tests disagree, it usually points to a series with weak long-run components or an insufficient sample to distinguish between the two hypotheses, not a computational error.")),
            seasonalUnitRoot = c(
        .("Seasonal unit root (Canova-Hansen, OCSB)"),
        .("Canova and Hansen (1995) test whether the seasonal pattern is stable over time or changes enough to require seasonal differencing; forecast::nsdiffs() uses this test by default. OCSB (Osborn et al., 1988) evaluates a related hypothesis through an auxiliary regression with simultaneous seasonal and non-seasonal lags. Both tests usually agree on series with clear seasonality; when they differ, it is reasonable to prefer Canova-Hansen's suggestion for the D order in a SARIMA, since it is the one implemented by default in the automatic forecasting literature (Hyndman & Khandakar, 2008), and use OCSB as a cross-check.")),
            ljungBox = c(
        .("Residual independence (Ljung-Box)"),
        .("Ljung and Box (1978) improved on the original Box-Pierce test by correcting its asymptotic approximation, especially relevant in small or moderate samples. The test jointly evaluates several lags of residual autocorrelation at once, not just one; this is why the chosen number of lags matters: too few may miss real dependence, and too many dilute the test's power by mixing lags with no genuine autocorrelation alongside those that do have it.")),
            archGarch = c(
        .("Conditional heteroscedasticity (ARCH-LM, GARCH)"),
        .("Engle (1982) introduced ARCH models from a central empirical observation in financial and macroeconomic series: periods of high volatility tend to cluster in time, rather than being randomly distributed. The Lagrange multiplier test (ARCH-LM) tests exactly that: whether the variance of the residuals at a given moment depends on its own magnitude at previous moments. Bollerslev (1986) generalized the model to GARCH, allowing the conditional variance to also depend on its own lagged values, not only on past shocks, which drastically reduces the number of parameters needed compared to a high-order ARCH to capture the same persistence.")),
            jarqueBera = c(
        .("Residual normality (Jarque-Bera)"),
        .("Jarque and Bera (1987) built a joint test of skewness and kurtosis: under normality, both should be close to zero (kurtosis in its excess form). The statistic combines the two deviations into a single chi-square test with 2 degrees of freedom. It is, above all, a useful diagnostic about the shape of the residuals, not a strict mathematical requirement for estimating most of the temporal models covered here; its practical relevance depends on whether the downstream inference procedure (intervals, forecasts) explicitly assumes normal errors.")),
            cointegration = c(
        .("Cointegration (Johansen, Engle-Granger)"),
        .("Engle & Granger (1987) formalized the idea of cointegration: even though each series may be individually non-stationary (integrated of order 1), a specific linear combination of several of them can be stationary, reflecting a long-run equilibrium relationship. Johansen (1991) extended this idea into a maximum likelihood framework within a VAR, allowing a formal test of how many independent cointegrating relationships exist (the rank) via the trace and maximum-eigenvalue tests, instead of estimating a single relationship as in the original Engle-Granger two-step procedure.")),
            varFoundations = c(
        .("VAR/VECM specification"),
        .("The reference treatment for lag-order selection, multivariate residual diagnostics, and the conversion between VAR and VECM representations follows the framework laid out by Lütkepohl (2005), the standard text on multivariate time-series models. The information criteria (AIC, BIC, HQ) used by VARselect() compete in a consistent direction: AIC tends to favor more heavily parameterized models (more lags), while BIC penalizes complexity more severely and tends to suggest more parsimonious specifications; when both agree, the chosen order is more trustworthy.")),
            parameterStability = c(
        .("Parameter stability (CUSUM, Nyblom)"),
        .("Brown et al., (1975) proposed the CUSUM of recursive residuals to detect whether a regression's coefficients remain constant across the sample or whether a structural change occurs at some point not specified in advance. Nyblom (1989) generalized the idea to a framework where parameters can vary gradually as a martingale process, rather than only at a single discrete break point, which makes it especially suited for evaluating the joint stability of an already-fitted GARCH model's parameters.")),
            asymmetricGarch = c(
        .("Volatility asymmetry (sign bias, GJR, EGARCH)"),
        .("A symmetric GARCH assumes that a positive and a negative shock of the same magnitude affect future variance equally. Engle and Ng (1993) designed the sign bias test precisely to detect when that assumption fails — a very common pattern in equity returns, where declines tend to raise volatility more than equally sized gains (the leverage effect). When the test is significant, two families of models respond to that asymmetry differently: GJR-GARCH (Glosten et al., 1993) adds a term that only activates on negative shocks, while EGARCH (Nelson, 1991) models the logarithm of the variance, which also avoids having to constrain the parameters to positive values to guarantee a valid variance.")),
            autoArima = c(
        .("Automatic order selection (Hyndman-Khandakar)"),
        .("The algorithm behind forecast::auto.arima() (Hyndman & Khandakar, 2008) does not exhaustively test every combination of p, d, q: it starts from the differencing orders suggested by the unit-root tests and then runs a stepwise search over the space of neighboring models, comparing by AICc. This makes it much faster than an exhaustive search, but it means the result is a reasonable local optimum, not guaranteed to be the best possible model; for difficult series it is worth cross-checking the result against visual inspection of the ACF/PACF."))
        )
    )
)
# -----------------------------------------------------------------------------
# Text rendering helpers.
# ES: Auxiliares de renderizado de texto.
#
# Jamovi already prints result titles from the .r.yaml file. These helpers
# only render body text. Avoid aggressive wrapping because it looks narrow
# beside native jamovi tables and can produce poor PDF output.
#
# ES: Jamovi ya imprime los títulos de resultado desde el archivo .r.yaml.
# Estos auxiliares solo renderizan el texto del cuerpo. Se evita el ajuste
# de línea agresivo porque se ve angosto junto a las tablas nativas de
# jamovi y puede producir un PDF de mala calidad.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .al_text()
#
# Input: lang - the environment made by .al_lang(self$options) for this run;
#   section, key - the entry to look up.
# Output: the translated entry (a character vector, or a list for nested
#   entries); "[missing text: section.key]" when the entry does not exist,
#   so a missing key is visible in the report and caught by the tests.
#
# ES: Entrada: lang - el entorno creado por .al_lang(self$options) para esta
#   ejecución; section, key - la entrada que se busca.
# ES: Salida: la entrada traducida (un vector de caracteres, o una lista
#   para entradas anidadas); "[missing text: section.key]" cuando la entrada
#   no existe, para que una clave faltante se vea en el informe y la
#   detecten las pruebas.
# -----------------------------------------------------------------------------
.al_text <- function(lang, section, key) {
    if (is.null(lang$texts))
        lang$texts <- .al_text_table(lang$tr)
    value <- lang$texts[[section]][[key]]
    if (is.null(value))
        value <- paste0("[missing text: ", section, ".", key, "]")
    value
}

# -----------------------------------------------------------------------------
# Normalize whitespace in one or more text fragments.
# ES: Normalizar espacios en blanco en uno o más fragmentos de texto.
#
# Before text items are rendered, embedded newlines/tabs and repeated
# spaces are collapsed, and any empty item is dropped.
#
# ES: Antes de renderizar los elementos de texto se colapsan los saltos de
# línea/tabulaciones incrustados y los espacios repetidos, y se descarta
# cualquier elemento vacío.
#
# Input: x - a character vector, or a (possibly nested) list of character
# vectors.
# Output: a flat character vector with whitespace normalized and empty
# entries removed.
#
# ES: Entrada: x - un vector de caracteres, o una lista (posiblemente
# anidada) de vectores de caracteres.
# ES: Salida: un vector de caracteres plano con los espacios en blanco
# normalizados y las entradas vacías eliminadas.
# -----------------------------------------------------------------------------
.al_clean_text <- function(x) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- as.character(x)
    x <- gsub("\\\\n", " ", x)
    x <- gsub("[\t\r\n]+", " ", x)
    x <- gsub(" +", " ", x)
    x <- trimws(x)
    x[nzchar(x)]
}

# -----------------------------------------------------------------------------
# Assemble cleaned text fragments into one paragraph block.
# ES: Ensamblar fragmentos de texto limpios en un bloque de párrafo.
#
# The shared building block behind every *_block() wrapper below: clean the
# input fragments, then join them with paragraph_sep so module code gets
# back a single string ready to hand to html_block()/setContent().
#
# ES: El bloque de construcción compartido detrás de cada envoltorio
# *_block() de abajo: limpia los fragmentos de entrada y luego los une con
# paragraph_sep, de modo que el código del módulo recibe una única cadena
# lista para pasar a html_block()/setContent().
#
# Inputs: ... - one or more text fragments (character vectors or lists);
# width - currently unused, reserved for a future explicit wrap width;
# paragraph_sep - the separator inserted between fragments (default: a
# space then a newline, so paragraphs render as separate lines without an
# extra blank line between them).
# Output: a single assembled character string, or "" if every fragment was
# empty after cleaning.
#
# ES: Entradas: ... - uno o más fragmentos de texto (vectores de caracteres
# o listas); width - actualmente sin uso, reservado para un futuro ancho de
# ajuste explícito; paragraph_sep - el separador insertado entre
# fragmentos (por defecto: un espacio y un salto de línea, para que los
# párrafos se rendericen como líneas separadas sin una línea en blanco
# adicional entre ellos).
# ES: Salida: una única cadena ensamblada, o "" si todos los fragmentos
# quedaron vacíos tras la limpieza.
# -----------------------------------------------------------------------------
.al_block <- function(..., width = NULL, paragraph_sep = " \n") {
    parts <- .al_clean_text(list(...))
    if (length(parts) == 0)
        return("")
    paste(parts, collapse = paragraph_sep)
}

# -----------------------------------------------------------------------------
# Thin .al_block() wrappers for each report-section kind.
# ES: Envoltorios delgados de .al_block() para cada tipo de sección de
# informe.
#
# These four wrappers exist so a module's .b.R code reads as "give me the
# text block for this guide/interpretation/pedagogical note", rather than
# repeating the .al_text()+.al_block() pair at every call site. They are
# functionally thin today (three of them just call .al_block() directly),
# but keep each call site's *intent* explicit and give each report-section
# kind its own place to grow independently later (e.g. a different
# paragraph_sep for pedagogical notes) without touching call sites.
#
# ES: Estos cuatro envoltorios existen para que el código .b.R de un módulo
# se lea como "dame el bloque de texto para esta guía/interpretación/nota
# pedagógica", en vez de repetir el par .al_text()+.al_block() en cada
# punto de llamado. Hoy son funcionalmente delgados (tres de ellos solo
# llaman a .al_block() directamente), pero mantienen explícita la
# *intención* de cada punto de llamado y le dan a cada tipo de sección de
# informe su propio lugar para crecer de forma independiente más adelante
# (p. ej. un paragraph_sep distinto para notas pedagógicas) sin tocar los
# puntos de llamado.
# -----------------------------------------------------------------------------
.al_text_block <- function(lang, section, key, width = NULL) {
    .al_block(.al_text(lang, section, key), width = width)
}

.al_guide_block <- function(lang, section, key, width = NULL) {
    .al_block(.al_text(lang, section, key), width = width)
}

.al_interpretation_block <- function(lang, text, width = NULL) {
    .al_block(text, width = width)
}

.al_pedagogical_block <- function(title = NULL, text, width = NULL) {
    .al_block(text, width = width)
}
