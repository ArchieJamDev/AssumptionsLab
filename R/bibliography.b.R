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
# Bibliography.
# ES: Bibliografía.
#
# This file implements the Bibliography tool: a curated, verified database of
# the methodological references cited throughout AssumptionsLab, filterable
# by topic and rendered in APA 7th edition. AssumptionsLab always cites in
# APA 7th edition; there is no citation-style selector (see CODE_STYLE.md
# §21 / DEVELOPER_GUIDE.md).
#
# ES: Este archivo implementa la herramienta de Bibliografía: una base de
# datos curada y verificada de las referencias metodológicas citadas en todo
# AssumptionsLab, filtrable por tema y renderizada en formato APA 7.ª
# edición. AssumptionsLab siempre cita en APA 7.ª edición; no existe un
# selector de estilo de citación (ver CODE_STYLE.md §21 / DEVELOPER_GUIDE.md).
#
# Responsibilities
# 1. Hold the curated reference database (refs_db), with every entry
#    individually verified against its primary source.
# 2. Filter references by the topic selected in the module's options.
# 3. Format each reference in APA 7th edition style, the module's fixed
#    citation format.
# 4. Assemble the introductory text, the formatted reference list, and a
#    methodological note into the report.
#
# ES: Responsabilidades
# 1. Mantener la base de datos curada de referencias (refs_db), con cada
#    entrada verificada individualmente contra su fuente primaria.
# 2. Filtrar las referencias según el tema seleccionado en las opciones del
#    módulo.
# 3. Formatear cada referencia en formato APA 7.ª edición, el estilo de
#    citación fijo del módulo.
# 4. Ensamblar el texto introductorio, la lista de referencias formateada y
#    una nota metodológica en el informe.
#
# Workflow
# 1. Read the selected topic and report language.
# 2. Filter the curated database down to the entries matching the topic.
# 3. Format each filtered entry in APA 7th edition style.
# 4. Render the introduction, the reference list, and the methodological
#    note.
#
# ES: Flujo de trabajo
# 1. Leer el tema seleccionado y el idioma del informe.
# 2. Filtrar la base de datos curada a las entradas que coinciden con el
#    tema.
# 3. Formatear cada entrada filtrada en formato APA 7.ª edición.
# 4. Renderizar la introducción, la lista de referencias y la nota
#    metodológica.
# -----------------------------------------------------------------------------

bibliographyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "bibliographyClass",
    inherit = bibliographyBase,
    private = list(
        .run = function() {

            topic <- self$options$topic
            lang <- .al_normalize_lang(self$options$reportLang)

            # AssumptionsLab always cites in APA 7th edition; there is no
            # style selector (see CODE_STYLE.md / DEVELOPER_GUIDE.md).
            # ES: AssumptionsLab siempre cita en APA 7.ª edición; no hay
            # selector de estilo (ver CODE_STYLE.md / DEVELOPER_GUIDE.md).
            style_label <- "APA 7th"

            esc <- function(x) {
                x <- gsub("&", "&amp;", x, fixed = TRUE)
                x <- gsub("<", "&lt;", x, fixed = TRUE)
                x <- gsub(">", "&gt;", x, fixed = TRUE)
                x
            }

            # -----------------------------------------------------------------------------
            # Curated reference database.
            # ES: Base de datos de referencias curadas.
            #
            # Every entry was individually verified by direct lookup (authors, journal
            # or publisher, volume, issue, pages, and DOI/link confirmed against the
            # primary source: Oxford Academic, JSTOR, Taylor & Francis, Econometric
            # Society, Wiley, Springer, etc.) before being included here. No reference
            # is added unverified, and no DOI is invented: when no verifiable DOI
            # exists, a real, direct link to the source is used instead.
            #
            # ES: Cada entrada fue verificada individualmente por búsqueda directa
            # (autores, revista/editorial, volumen, número, páginas y DOI/enlace
            # confirmados contra la fuente primaria: Oxford Academic, JSTOR, Taylor &
            # Francis, Econometric Society, Wiley, Springer, etc.) antes de incluirse
            # aquí. No se agregan referencias sin verificar, y no se inventan DOIs:
            # cuando no existe DOI verificable se usa un enlace directo y real a la
            # fuente.
            # -----------------------------------------------------------------------------
            refs_db <- list(
                list(
                    authors = list(c("Shapiro", "S. S."), c("Wilk", "M. B.")),
                    year = 1965,
                    title = "An analysis of variance test for normality (complete samples)",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrika",
                    volume = "52", issue = "3-4", pages = "591-611",
                    doi = "10.1093/biomet/52.3-4.591",
                    topics = c("normality", "assumptions")
                ),
                list(
                    authors = list(c("Razali", "N. M."), c("Wah", "Y. B.")),
                    year = 2011,
                    title = "Power comparisons of Shapiro-Wilk, Kolmogorov-Smirnov, Lilliefors and Anderson-Darling tests",
                    ref_type = "review",
                    is_book = FALSE,
                    journal = "Journal of Statistical Modeling and Analytics",
                    volume = "2", issue = "1", pages = "21-33",
                    doi = NULL, url = "https://www.nrc.gov/docs/ml1714/ml17143a100.pdf",
                    topics = c("normality", "assumptions")
                ),
                list(
                    authors = list(c("Breusch", "T. S."), c("Pagan", "A. R.")),
                    year = 1979,
                    title = "A simple test for heteroscedasticity and random coefficient variation",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Econometrica",
                    volume = "47", issue = "5", pages = "1287-1294",
                    doi = "10.2307/1911963",
                    topics = c("homoscedasticity", "assumptions")
                ),
                list(
                    authors = list(c("White", "H.")),
                    year = 1980,
                    title = "A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Econometrica",
                    volume = "48", issue = "4", pages = "817-838",
                    doi = "10.2307/1912934",
                    topics = c("homoscedasticity", "robust")
                ),
                list(
                    authors = list(c("Lilliefors", "H. W.")),
                    year = 1967,
                    title = "On the Kolmogorov-Smirnov test for normality with mean and variance unknown",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the American Statistical Association",
                    volume = "62", issue = "318", pages = "399-402",
                    doi = "10.1080/01621459.1967.10482916",
                    topics = c("normality", "assumptions")
                ),
                list(
                    authors = list(c("Shapiro", "S. S."), c("Francia", "R. S.")),
                    year = 1972,
                    title = "An approximate analysis of variance test for normality",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Journal of the American Statistical Association",
                    volume = "67", issue = "337", pages = "215-216",
                    doi = "10.1080/01621459.1972.10481232",
                    topics = c("normality", "assumptions")
                ),
                list(
                    authors = list(c("Bartlett", "M. S.")),
                    year = 1937,
                    title = "Properties of sufficiency and statistical tests",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Proceedings of the Royal Society of London, Series A",
                    volume = "160", issue = "901", pages = "268-282",
                    doi = "10.1098/rspa.1937.0109",
                    topics = c("homoscedasticity", "assumptions")
                ),
                list(
                    authors = list(c("Brown", "M. B."), c("Forsythe", "A. B.")),
                    year = 1974,
                    title = "Robust tests for the equality of variances",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the American Statistical Association",
                    volume = "69", issue = "346", pages = "364-367",
                    doi = "10.1080/01621459.1974.10482955",
                    topics = c("homoscedasticity", "robust")
                ),
                list(
                    authors = list(c("Fligner", "M. A."), c("Killeen", "T. J.")),
                    year = 1976,
                    title = "Distribution-free two-sample tests for scale",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the American Statistical Association",
                    volume = "71", issue = "353", pages = "210-213",
                    doi = "10.1080/01621459.1976.10480351",
                    topics = c("homoscedasticity", "robust")
                ),
                list(
                    authors = list(c("Hartley", "H. O.")),
                    year = 1950,
                    title = "The maximum F-ratio as a short-cut test for heterogeneity of variance",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrika",
                    volume = "37", issue = "3-4", pages = "308-312",
                    doi = "10.1093/biomet/37.3-4.308",
                    topics = c("homoscedasticity", "assumptions")
                ),
                list(
                    authors = list(c("Goldfeld", "S. M."), c("Quandt", "R. E.")),
                    year = 1965,
                    title = "Some tests for homoscedasticity",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the American Statistical Association",
                    volume = "60", issue = "310", pages = "539-547",
                    doi = "10.2307/2282689",
                    topics = c("homoscedasticity", "assumptions")
                ),
                list(
                    authors = list(c("MacKinnon", "J. G."), c("White", "H.")),
                    year = 1985,
                    title = "Some heteroskedasticity-consistent covariance matrix estimators with improved finite sample properties",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Journal of Econometrics",
                    volume = "29", issue = "3", pages = "305-325",
                    doi = "10.1016/0304-4076(85)90158-7",
                    topics = c("homoscedasticity", "robust")
                ),
                list(
                    authors = list(c("Welch", "B. L.")),
                    year = 1947,
                    title = "The generalization of 'Student's' problem when several different population variances are involved",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrika",
                    volume = "34", issue = "1-2", pages = "28-35",
                    doi = "10.1093/biomet/34.1-2.28",
                    topics = c("homoscedasticity", "robust", "assumptions")
                ),
                list(
                    authors = list(c("Mauchly", "J. W.")),
                    year = 1940,
                    title = "Significance test for sphericity of a normal n-variate distribution",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "The Annals of Mathematical Statistics",
                    volume = "11", issue = "2", pages = "204-209",
                    doi = "10.1214/aoms/1177731915",
                    topics = c("assumptions")
                ),
                list(
                    authors = list(c("Greenhouse", "S. W."), c("Geisser", "S.")),
                    year = 1959,
                    title = "On methods in the analysis of profile data",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Psychometrika",
                    volume = "24", issue = "2", pages = "95-112",
                    doi = "10.1007/BF02289823",
                    topics = c("assumptions")
                ),
                list(
                    authors = list(c("Huynh", "H."), c("Feldt", "L. S.")),
                    year = 1976,
                    title = "Estimation of the Box correction for degrees of freedom from sample data in randomized block and split-plot designs",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Journal of Educational Statistics",
                    volume = "1", issue = "1", pages = "69-82",
                    doi = "10.2307/1164736",
                    topics = c("assumptions")
                ),
                list(
                    authors = list(c("Mahalanobis", "P. C.")),
                    year = 1936,
                    title = "On the generalized distance in statistics",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Proceedings of the National Institute of Sciences of India",
                    volume = "2", issue = "1", pages = "49-55",
                    doi = NULL, url = NULL,
                    topics = c("assumptions")
                ),
                list(
                    authors = list(c("Wilcoxon", "F.")),
                    year = 1945,
                    title = "Individual comparisons by ranking methods",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrics Bulletin",
                    volume = "1", issue = "6", pages = "80-83",
                    doi = "10.2307/3001968",
                    topics = c("robust", "assumptions")
                ),
                list(
                    authors = list(c("Friedman", "M.")),
                    year = 1937,
                    title = "The use of ranks to avoid the assumption of normality implicit in the analysis of variance",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the American Statistical Association",
                    volume = "32", issue = "200", pages = "675-701",
                    doi = "10.2307/2279372",
                    topics = c("robust", "assumptions")
                ),
                list(
                    authors = list(c("Ramsey", "J. B.")),
                    year = 1969,
                    title = "Tests for specification errors in classical linear least-squares regression analysis",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the Royal Statistical Society, Series B",
                    volume = "31", issue = "2", pages = "350-371",
                    doi = "10.1111/j.2517-6161.1969.tb00796.x",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Marquardt", "D. W.")),
                    year = 1970,
                    title = "Generalized inverses, ridge regression, biased linear estimation, and nonlinear estimation",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Technometrics",
                    volume = "12", issue = "3", pages = "591-612",
                    doi = "10.1080/00401706.1970.10488699",
                    topics = c("regression")
                ),
                list(
                    authors = list(c("O'Brien", "R. M.")),
                    year = 2007,
                    title = "A caution regarding rules of thumb for variance inflation factors",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Quality & Quantity",
                    volume = "41", issue = "5", pages = "673-690",
                    doi = "10.1007/s11135-006-9018-6",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Durbin", "J."), c("Watson", "G. S.")),
                    year = 1951,
                    title = "Testing for serial correlation in least squares regression II",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrika",
                    volume = "38", issue = "1-2", pages = "159-178",
                    doi = "10.1093/biomet/38.1-2.159",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Breusch", "T. S.")),
                    year = 1978,
                    title = "Testing for autocorrelation in dynamic linear models",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Australian Economic Papers",
                    volume = "17", issue = "31", pages = "334-355",
                    doi = "10.1111/j.1467-8454.1978.tb00635.x",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Godfrey", "L. G.")),
                    year = 1978,
                    title = "Testing against general autoregressive and moving average error models when the regressors include lagged dependent variables",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Econometrica",
                    volume = "46", issue = "6", pages = "1293-1302",
                    doi = NULL, url = "https://www.jstor.org/stable/1913829",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Cook", "R. D.")),
                    year = 1977,
                    title = "Detection of influential observation in linear regression",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Technometrics",
                    volume = "19", issue = "1", pages = "15-18",
                    doi = "10.1080/00401706.1977.10489493",
                    topics = c("path", "regression")
                ),
                list(
                    authors = list(c("Levene", "H.")),
                    year = 1960,
                    title = "Robust tests for equality of variances. In I. Olkin (Ed.), Contributions to Probability and Statistics (pp. 278-292)",
                    ref_type = "seminal",
                    is_book = TRUE,
                    publisher = "Stanford University Press",
                    doi = NULL, url = NULL,
                    topics = c("homoscedasticity", "assumptions")
                ),
                list(
                    authors = list(c("Huitema", "B. E.")),
                    year = 2011,
                    title = "The Analysis of Covariance and Alternatives (2nd ed.)",
                    ref_type = "book",
                    is_book = TRUE,
                    publisher = "Wiley",
                    doi = NULL, url = NULL,
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Maxwell", "S. E."), c("Delaney", "H. D.")),
                    year = 2004,
                    title = "Designing Experiments and Analyzing Data: A Model Comparison Perspective (2nd ed.)",
                    ref_type = "book",
                    is_book = TRUE,
                    publisher = "Lawrence Erlbaum Associates",
                    doi = NULL, url = NULL,
                    topics = c("assumptions")
                ),
                list(
                    authors = list(c("Tukey", "J. W.")),
                    year = 1977,
                    title = "Exploratory Data Analysis",
                    ref_type = "book",
                    is_book = TRUE,
                    publisher = "Addison-Wesley",
                    doi = NULL, url = NULL,
                    topics = c("assumptions", "related")
                ),
                list(
                    authors = list(c("Belsley", "D. A."), c("Kuh", "E."), c("Welsch", "R. E.")),
                    year = 1980,
                    title = "Regression Diagnostics: Identifying Influential Data and Sources of Collinearity",
                    ref_type = "book",
                    is_book = TRUE,
                    publisher = "Wiley",
                    doi = "10.1002/0471725153",
                    topics = c("path", "regression")
                ),
                list(
                    authors = list(c("Hosmer", "D. W."), c("Lemeshow", "S.")),
                    year = 1980,
                    title = "Goodness of fit tests for the multiple logistic regression model",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Communications in Statistics - Theory and Methods",
                    volume = "9", issue = "10", pages = "1043-1069",
                    doi = "10.1080/03610928008827941",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Peduzzi", "P."), c("Concato", "J."), c("Kemper", "E."), c("Holford", "T. R."), c("Feinstein", "A. R.")),
                    year = 1996,
                    title = "A simulation study of the number of events per variable in logistic regression analysis",
                    ref_type = "review",
                    is_book = FALSE,
                    journal = "Journal of Clinical Epidemiology",
                    volume = "49", issue = "12", pages = "1373-1379",
                    doi = "10.1016/S0895-4356(96)00236-3",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Firth", "D.")),
                    year = 1993,
                    title = "Bias reduction of maximum likelihood estimates",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrika",
                    volume = "80", issue = "1", pages = "27-38",
                    doi = "10.1093/biomet/80.1.27",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Box", "G. E. P."), c("Tidwell", "P. W.")),
                    year = 1962,
                    title = "Transformation of the independent variables",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Technometrics",
                    volume = "4", issue = "4", pages = "531-550",
                    doi = "10.1080/00401706.1962.10490038",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Pregibon", "D.")),
                    year = 1981,
                    title = "Logistic regression diagnostics",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "The Annals of Statistics",
                    volume = "9", issue = "4", pages = "705-724",
                    doi = "10.1214/aos/1176345513",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Hanley", "J. A."), c("McNeil", "B. J.")),
                    year = 1982,
                    title = "The meaning and use of the area under a receiver operating characteristic (ROC) curve",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Radiology",
                    volume = "143", issue = "1", pages = "29-36",
                    doi = "10.1148/radiology.143.1.7063747",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Zhang", "J."), c("Yu", "K. F.")),
                    year = 1998,
                    title = "What's the relative risk? A method of correcting the odds ratio in cohort studies of common outcomes",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "JAMA",
                    volume = "280", issue = "19", pages = "1690-1691",
                    doi = "10.1001/jama.280.19.1690",
                    topics = c("regression", "assumptions")
                ),
                # ---------------------------------------------------------
                # Ordinal logistic regression (ordCheck, Sep 2026). Tagged
                # c("regression", "assumptions") like the rest of the
                # logistic-regression cluster above - there is no separate
                # "logistic"/"ordinal" topic in this database, so these
                # follow the existing convention rather than introduce a
                # new tag.
                # ES: Regresión logística ordinal (ordCheck, sep 2026).
                # Etiquetadas c("regression", "assumptions") como el resto
                # del grupo de regresión logística de arriba - no existe un
                # tema "logistic"/"ordinal" separado en esta base, así que
                # siguen la convención existente en vez de introducir una
                # etiqueta nueva.
                # ---------------------------------------------------------
                list(
                    authors = list(c("Brant", "R.")),
                    year = 1990,
                    title = "Assessing proportionality in the proportional odds model for ordinal logistic regression",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrics",
                    volume = "46", issue = "4", pages = "1171-1178",
                    doi = "10.2307/2532457",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("McCullagh", "P.")),
                    year = 1980,
                    title = "Regression models for ordinal data",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the Royal Statistical Society: Series B (Methodological)",
                    volume = "42", issue = "2", pages = "109-127",
                    doi = "10.1111/j.2517-6161.1980.tb01109.x",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Pulkstenis", "E."), c("Robinson", "T. J.")),
                    year = 2004,
                    title = "Goodness-of-fit tests for ordinal response regression models",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Statistics in Medicine",
                    volume = "23", issue = "6", pages = "999-1014",
                    doi = "10.1002/sim.1659",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Fagerland", "M. W."), c("Hosmer", "D. W.")),
                    year = 2013,
                    title = "A goodness-of-fit test for the proportional odds regression model",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Statistics in Medicine",
                    volume = "32", issue = "13", pages = "2235-2249",
                    doi = "10.1002/sim.5645",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Liu", "I."), c("Mukherjee", "B."), c("Suesse", "T."), c("Sparrow", "D."), c("Park", "S. K.")),
                    year = 2009,
                    title = "Graphical diagnostics to check model misspecification for the proportional odds regression model",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Statistics in Medicine",
                    # Issue number left unverified (every secondary source
                    # found cites this as "28:412-429" without an issue
                    # number) - using "" rather than NULL because the
                    # formatter below does paste0(..., "(", r$issue, ")",
                    # ...) unconditionally, and a NULL argument makes
                    # paste0() return character(0) for the whole citation,
                    # not just drop the parentheses. Archie: please confirm
                    # the issue number from the Wiley/DOI page and fill it
                    # in here.
                    # ES: Número de issue sin verificar (cada fuente
                    # secundaria encontrada cita esto como "28:412-429" sin
                    # número de issue) - se usa "" en vez de NULL porque el
                    # formateador de abajo hace paste0(..., "(", r$issue,
                    # ")", ...) sin condicional, y un argumento NULL hace
                    # que paste0() devuelva character(0) para toda la cita,
                    # no solo que se omitan los paréntesis. Archie: por
                    # favor confirma el número de issue desde la página de
                    # Wiley/DOI y complétalo aquí.
                    volume = "28", issue = "", pages = "412-429",
                    doi = "10.1002/sim.3386",
                    topics = c("regression", "assumptions")
                ),
                # ---------------------------------------------------------
                # Independence of Irrelevant Alternatives / Hausman-McFadden
                # test (multCheck, Sep 2026). Tagged c("regression",
                # "assumptions") for the same reason as the ordCheck cluster
                # above - there is no separate "logistic"/"multinomial"
                # topic in this database (see jamovi/bibliography.a.yaml's
                # "Linearity and Regression" option), so these follow the
                # existing convention rather than introduce a new tag.
                # ES: Independencia de Alternativas Irrelevantes / prueba de
                # Hausman-McFadden (multCheck, sep 2026). Etiquetadas
                # c("regression", "assumptions") por la misma razon que el
                # grupo de ordCheck de arriba - no existe un tema
                # "logistic"/"multinomial" separado en esta base (ver la
                # opcion "Linearity and Regression" de
                # jamovi/bibliography.a.yaml), asi que siguen la convencion
                # existente en vez de introducir una etiqueta nueva.
                # ---------------------------------------------------------
                list(
                    authors = list(c("Hausman", "J."), c("McFadden", "D.")),
                    year = 1984,
                    title = "Specification tests for the multinomial logit model",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Econometrica",
                    volume = "52", issue = "5", pages = "1219-1240",
                    doi = "10.2307/1910997",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Small", "K. A."), c("Hsiao", "C.")),
                    year = 1985,
                    title = "Multinomial logit specification tests",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "International Economic Review",
                    volume = "26", issue = "3", pages = "619-627",
                    doi = "10.2307/2526707",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Cheng", "S."), c("Long", "J. S.")),
                    year = 2007,
                    title = "Testing for IIA in the multinomial logit model",
                    ref_type = "review",
                    is_book = FALSE,
                    journal = "Sociological Methods & Research",
                    volume = "35", issue = "4", pages = "583-600",
                    doi = "10.1177/0049124106292361",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Mardia", "K. V.")),
                    year = 1970,
                    title = "Measures of multivariate skewness and kurtosis with applications",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrika",
                    volume = "57", issue = "3", pages = "519-530",
                    doi = "10.1093/biomet/57.3.519",
                    topics = c("path", "normality", "assumptions")
                ),
                list(
                    authors = list(c("Wright", "S.")),
                    year = 1934,
                    title = "The method of path coefficients",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Annals of Mathematical Statistics",
                    volume = "5", issue = "3", pages = "161-215",
                    doi = "10.1214/aoms/1177732676",
                    topics = c("path", "regression", "assumptions")
                ),
                list(
                    authors = list(c("Duncan", "O. D.")),
                    year = 1966,
                    title = "Path analysis: Sociological examples",
                    ref_type = "application",
                    is_book = FALSE,
                    journal = "American Journal of Sociology",
                    volume = "72", issue = "1", pages = "1-16",
                    doi = "10.1086/224256",
                    topics = c("path", "regression", "assumptions")
                ),
                list(
                    authors = list(c("Baron", "R. M."), c("Kenny", "D. A.")),
                    year = 1986,
                    title = "The moderator-mediator variable distinction in social psychological research: conceptual, strategic, and statistical considerations",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of Personality and Social Psychology",
                    volume = "51", issue = "6", pages = "1173-1182",
                    doi = "10.1037/0022-3514.51.6.1173",
                    topics = c("path", "regression", "assumptions")
                ),
                list(
                    authors = list(c("Sobel", "M. E.")),
                    year = 1982,
                    title = "Asymptotic confidence intervals for indirect effects in structural equation models",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Sociological Methodology",
                    volume = "13", pages = "290-312",
                    doi = "10.2307/270723",
                    topics = c("path", "regression", "assumptions")
                ),
                list(
                    authors = list(c("Green", "S. B.")),
                    year = 1991,
                    title = "How many subjects does it take to do a regression analysis?",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Multivariate Behavioral Research",
                    volume = "26", issue = "3", pages = "499-510",
                    doi = "10.1207/s15327906mbr2603_7",
                    topics = c("path", "regression")
                ),
                list(
                    authors = list(c("Bentler", "P. M."), c("Chou", "C.-P.")),
                    year = 1987,
                    title = "Practical issues in structural equation modeling",
                    ref_type = "review",
                    is_book = FALSE,
                    journal = "Sociological Methods & Research",
                    volume = "16", issue = "1", pages = "78-117",
                    doi = "10.1177/0049124187016001004",
                    topics = c("path")
                ),
                list(
                    authors = list(c("Jackson", "D. L.")),
                    year = 2003,
                    title = "Revisiting sample size and number of parameter estimates: Some support for the N:q hypothesis",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Structural Equation Modeling",
                    volume = "10", issue = "1", pages = "128-141",
                    doi = "10.1207/S15328007SEM1001_6",
                    topics = c("path")
                ),
                list(
                    authors = list(c("Kline", "R. B.")),
                    year = 2023,
                    title = "Principles and Practice of Structural Equation Modeling (5th ed.)",
                    ref_type = "book",
                    is_book = TRUE,
                    publisher = "The Guilford Press",
                    topics = c("path")
                ),
                list(
                    authors = list(c("Székely", "G. J."), c("Rizzo", "M. L."), c("Bakirov", "N. K.")),
                    year = 2007,
                    title = "Measuring and testing dependence by correlation of distances",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "The Annals of Statistics",
                    volume = "35", issue = "6", pages = "2769-2794",
                    doi = "10.1214/009053607000000505",
                    topics = c("regression", "assumptions")
                ),
                list(
                    authors = list(c("Anderson", "T. W."), c("Darling", "D. A.")),
                    year = 1952,
                    title = "Asymptotic theory of certain 'goodness of fit' criteria based on stochastic processes",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Annals of Mathematical Statistics",
                    volume = "23", issue = "2", pages = "193-212",
                    doi = "10.1214/aoms/1177729437",
                    topics = c("normality", "assumptions")
                ),
                list(
                    authors = list(c("Ma", "J."), c("Sun", "Z.")),
                    year = 2011,
                    title = "Mutual information is copula entropy",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Tsinghua Science and Technology",
                    volume = "16", issue = "1", pages = "51-54",
                    doi = "10.1016/S1007-0214(11)70008-6",
                    topics = c("path", "assumptions")
                ),
                list(
                    authors = list(c("Kozachenko", "L. F."), c("Leonenko", "N. N.")),
                    year = 1987,
                    title = "Sample estimate of the entropy of a random vector",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Problems of Information Transmission",
                    volume = "23", issue = "2", pages = "95-101",
                    doi = NULL, url = "http://mi.mathnet.ru/eng/ppi/v23/i2/p9",
                    topics = c("path", "assumptions")
                ),
                list(
                    authors = list(c("Dickey", "D. A."), c("Fuller", "W. A.")),
                    year = 1979,
                    title = "Distribution of the estimators for autoregressive time series with a unit root",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the American Statistical Association",
                    volume = "74", issue = "366a", pages = "427-431",
                    doi = "10.1080/01621459.1979.10482531",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Phillips", "P. C. B."), c("Perron", "P.")),
                    year = 1988,
                    title = "Testing for a unit root in time series regression",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrika",
                    volume = "75", issue = "2", pages = "335-346",
                    doi = "10.1093/biomet/75.2.335",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Kwiatkowski", "D."), c("Phillips", "P. C. B."), c("Schmidt", "P."), c("Shin", "Y.")),
                    year = 1992,
                    title = "Testing the null hypothesis of stationarity against the alternative of a unit root",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of Econometrics",
                    volume = "54", issue = "1-3", pages = "159-178",
                    doi = "10.1016/0304-4076(92)90104-Y",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Canova", "F."), c("Hansen", "B. E.")),
                    year = 1995,
                    title = "Are seasonal patterns constant over time? A test for seasonal stability",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of Business & Economic Statistics",
                    volume = "13", issue = "3", pages = "237-252",
                    doi = "10.1080/07350015.1995.10524598",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Osborn", "D. R."), c("Chui", "A. P. L."), c("Smith", "J. P."), c("Birchenhall", "C. R.")),
                    year = 1988,
                    title = "Seasonality and the order of integration for consumption",
                    ref_type = "methodological",
                    is_book = FALSE,
                    journal = "Oxford Bulletin of Economics and Statistics",
                    volume = "50", issue = "4", pages = "361-377",
                    doi = "10.1111/j.1468-0084.1988.mp50004002.x",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Ljung", "G. M."), c("Box", "G. E. P.")),
                    year = 1978,
                    title = "On a measure of lack of fit in time series models",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Biometrika",
                    volume = "65", issue = "2", pages = "297-303",
                    doi = "10.1093/biomet/65.2.297",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Engle", "R. F.")),
                    year = 1982,
                    title = "Autoregressive conditional heteroscedasticity with estimates of the variance of United Kingdom inflation",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Econometrica",
                    volume = "50", issue = "4", pages = "987-1007",
                    doi = "10.2307/1912773",
                    topics = c("time_series", "homoscedasticity", "assumptions")
                ),
                list(
                    authors = list(c("Bollerslev", "T.")),
                    year = 1986,
                    title = "Generalized autoregressive conditional heteroskedasticity",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of Econometrics",
                    volume = "31", issue = "3", pages = "307-327",
                    doi = "10.1016/0304-4076(86)90063-1",
                    topics = c("time_series", "homoscedasticity", "assumptions")
                ),
                list(
                    authors = list(c("Jarque", "C. M."), c("Bera", "A. K.")),
                    year = 1987,
                    title = "A test for normality of observations and regression residuals",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "International Statistical Review",
                    volume = "55", issue = "2", pages = "163-172",
                    doi = "10.2307/1403192",
                    topics = c("time_series", "normality", "assumptions")
                ),
                list(
                    authors = list(c("Johansen", "S.")),
                    year = 1991,
                    title = "Estimation and hypothesis testing of cointegration vectors in Gaussian vector autoregressive models",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Econometrica",
                    volume = "59", issue = "6", pages = "1551-1580",
                    doi = "10.2307/2938278",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Engle", "R. F."), c("Granger", "C. W. J.")),
                    year = 1987,
                    title = "Co-integration and error correction: Representation, estimation, and testing",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Econometrica",
                    volume = "55", issue = "2", pages = "251-276",
                    doi = "10.2307/1913236",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Nyblom", "J.")),
                    year = 1989,
                    title = "Testing for the constancy of parameters over time",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the American Statistical Association",
                    volume = "84", issue = "405", pages = "223-230",
                    doi = "10.1080/01621459.1989.10478759",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Brown", "R. L."), c("Durbin", "J."), c("Evans", "J. M.")),
                    year = 1975,
                    title = "Techniques for testing the constancy of regression relationships over time",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of the Royal Statistical Society, Series B",
                    volume = "37", issue = "2", pages = "149-192",
                    doi = "10.1111/j.2517-6161.1975.tb01532.x",
                    topics = c("time_series", "regression", "assumptions")
                ),
                list(
                    authors = list(c("Engle", "R. F."), c("Ng", "V. K.")),
                    year = 1993,
                    title = "Measuring and testing the impact of news on volatility",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of Finance",
                    volume = "48", issue = "5", pages = "1749-1778",
                    doi = "10.1111/j.1540-6261.1993.tb05127.x",
                    topics = c("time_series", "assumptions")
                ),
                list(
                    authors = list(c("Glosten", "L. R."), c("Jagannathan", "R."), c("Runkle", "D. E.")),
                    year = 1993,
                    title = "On the relation between the expected value and the volatility of the nominal excess return on stocks",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Journal of Finance",
                    volume = "48", issue = "5", pages = "1779-1801",
                    doi = "10.1111/j.1540-6261.1993.tb05128.x",
                    topics = c("time_series", "robust")
                ),
                list(
                    authors = list(c("Nelson", "D. B.")),
                    year = 1991,
                    title = "Conditional heteroskedasticity in asset returns: A new approach",
                    ref_type = "seminal",
                    is_book = FALSE,
                    journal = "Econometrica",
                    volume = "59", issue = "2", pages = "347-370",
                    doi = "10.2307/2938260",
                    topics = c("time_series", "robust")
                ),
                list(
                    authors = list(c("Hyndman", "R. J."), c("Khandakar", "Y.")),
                    year = 2008,
                    title = "Automatic time series forecasting: The forecast package for R",
                    ref_type = "application",
                    is_book = FALSE,
                    journal = "Journal of Statistical Software",
                    volume = "27", issue = "3", pages = "1-22",
                    doi = "10.18637/jss.v027.i03",
                    topics = c("time_series")
                ),
                list(
                    authors = list(c("Lütkepohl", "H.")),
                    year = 2005,
                    title = "New Introduction to Multiple Time Series Analysis",
                    ref_type = "book",
                    is_book = TRUE,
                    publisher = "Springer",
                    doi = "10.1007/978-3-540-27752-1",
                    topics = c("time_series")
                )
            )

            # ------------------------------------------------------------
            # Journal-level bibliometrics (Scopus / Web of Science / other
            # indexes / quartile). Indexed by exact journal name rather
            # than by individual reference, because this data belongs to
            # the journal, not the article. Verified per journal, not
            # separately for each of the 69 references.
            # ES: Bibliometria por revista (Scopus / Web of Science /
            # otras indexaciones / cuartil). Se indexa por nombre exacto
            # de revista, no por referencia individual, porque estos
            # datos son propiedad de la revista, no del articulo.
            # Verificado por revista, no por cada uno de los 69 articulos.
            journal_biblio <- list(
                "American Journal of Sociology" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Annals of Mathematical Statistics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1 (historica; continuada por Annals of Statistics / Annals of Probability)"),
                "Australian Economic Papers" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q2/Q3"),
                "Biometrics Bulletin" = list(scopus = TRUE, wos = TRUE, other = "indexada como parte del historico de Biometrics (fusionada en 1947)", quartile = "Q1 (historica)"),
                "Biometrika" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "British Journal of Management" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Communications in Statistics - Theory and Methods" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q2/Q3"),
                "Econometrica" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "International Statistical Review" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1/Q2"),
                "JAMA" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Journal of Business & Economic Statistics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Journal of Clinical Epidemiology" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Journal of Econometrics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Journal of Educational Statistics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q2"),
                "Journal of Finance" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Journal of Personality and Social Psychology" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Journal of Statistical Modeling and Analytics" = list(scopus = FALSE, wos = FALSE, other = "Google Scholar, MyJurnal", quartile = "N/A"),
                "Journal of Statistical Software" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Journal of the American Statistical Association" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Journal of the Royal Statistical Society, Series B" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Multivariate Behavioral Research" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1/Q2"),
                "Oxford Bulletin of Economics and Statistics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1/Q2"),
                "Problems of Information Transmission" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q2/Q3"),
                "Proceedings of the National Institute of Sciences of India" = list(scopus = FALSE, wos = FALSE, other = "predecesora historica del Proceedings of the Indian National Science Academy", quartile = "N/A"),
                "Proceedings of the Royal Society of London, Series A" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Psychometrika" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Quality & Quantity" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q2"),
                "Radiology" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Sociological Methodology" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Sociological Methods & Research" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Structural Equation Modeling" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Technometrics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "The Annals of Mathematical Statistics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1 (historica; continuada por Annals of Statistics / Annals of Probability)"),
                "The Annals of Statistics" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1"),
                "Tsinghua Science and Technology" = list(scopus = TRUE, wos = TRUE, other = "", quartile = "Q1 (SJR) / Q2 (JCR)"),
                "Addison-Wesley" = list(scopus = FALSE, wos = FALSE, other = "libro; muy citado en Google Scholar", quartile = "N/A"),
                "Springer" = list(scopus = FALSE, wos = FALSE, other = "libro; muy citado en Google Scholar", quartile = "N/A"),
                "Stanford University Press" = list(scopus = FALSE, wos = FALSE, other = "libro; muy citado en Google Scholar", quartile = "N/A"),
                "The Guilford Press" = list(scopus = FALSE, wos = FALSE, other = "libro; muy citado en Google Scholar", quartile = "N/A"),
                "Wiley" = list(scopus = FALSE, wos = FALSE, other = "libro; muy citado en Google Scholar", quartile = "N/A")
            )
            # Note: the list above covers the most frequent journals in the
            # reference database. Any article's journal not appearing here
            # is marked "pending verification" in the table instead of
            # showing a fabricated value.
            #
            # ES: la lista anterior cubre las revistas mas frecuentes en la
            # base de referencias. Cualquier revista de un articulo que no
            # aparezca aqui se marca "pendiente de verificar" en la tabla
            # en vez de mostrar un dato inventado.
            get_journal_biblio <- function(journal_name) {
                if (is.null(journal_name) || !nzchar(journal_name)) {
                    return(list(scopus = NA, wos = NA, other = "", quartile = "pendiente de verificar"))
                }
                hit <- journal_biblio[[journal_name]]
                if (is.null(hit)) {
                    return(list(scopus = NA, wos = NA, other = "", quartile = "pendiente de verificar"))
                }
                hit
            }

            # ------------------------------------------------------------
            # Bibliometric summary (table at the end of Bibliography).
            # ES: Resumen bibliometrico (tabla al final de Bibliography).
            # ------------------------------------------------------------
            ref_type_label <- function(rt, lang) {
                .al_tr(lang,
                    switch(rt,
                        seminal = "Original/seminal",
                        methodological = "Methodological",
                        review = "Review/comparison",
                        application = "Application",
                        book = "Reference book",
                        rt
                    ),
                    switch(rt,
                        seminal = "Original/seminal",
                        methodological = "Metodol\u00f3gica",
                        review = "Revisi\u00f3n/comparaci\u00f3n",
                        application = "Aplicaci\u00f3n",
                        book = "Libro de referencia",
                        rt
                    )
                )
            }

            yesno <- function(x, lang) {
                if (is.na(x)) return(.al_tr(lang, "pending", "pendiente"))
                if (isTRUE(x)) .al_tr(lang, "Yes", "S\u00ed") else "No"
            }

            # ------------------------------------------------------------
            # Citation database (Google Scholar / CiNii / RePEc / Crossref /
            # other sources depending on the entry). Snapshot from August
            # 2026. The highest value reported across sources is shown when
            # more than one is available, never an average nor the lowest.
            # Source: AssumptionsLab_Citation_Database.csv (Archie, Aug.
            # 2026).
            #
            # ES: Base de citas (Google Scholar / CiNii / RePEc / Crossref /
            # otras fuentes segun el caso). Instantanea de agosto de 2026.
            # Se muestra el valor mas alto reportado entre fuentes cuando
            # hay mas de una disponible, no un promedio ni el mas bajo.
            # Fuente: AssumptionsLab_Citation_Database.csv (Archie, ago.
            # 2026).
            citation_db <- list(
                "Anderson|1952"   = list(citations = "26",    source = "CiNii Research",           other = ""),
                "Baron|1986"      = list(citations = "73000", source = "Google Scholar (reportado)", other = "Web of Science (>32.000)"),
                "Bartlett|1937"   = list(citations = "45",    source = "CiNii Research",           other = "scite.ai (337)"),
                "Bollerslev|1986" = list(citations = "23221", source = "Semantic Scholar",           other = "EconPapers/RePEc (9119)"),
                "Breusch|1979"    = list(citations = "4812",  source = "Semantic Scholar",           other = "EconPapers/RePEc (1320)"),
                "Breusch|1978"    = list(citations = "943",   source = "Wiley/Crossref",            other = "RePEc (512)"),
                "Cook|1977"       = list(citations = "4661",  source = "BishtRef",                  other = "CiNii (11)"),
                "Shapiro|1965" = list(citations = "19700", source = "OpenAlex", other = "Crossref API ( 14478"),
                "Razali|2011" = list(citations = "9510", source = "Google Scholar", other = ""),
                "White|1980" = list(citations = "26316", source = "OpenAlex", other = "Crossref API (18417)"),
                "Lilliefors|1967" = list(citations = "3444", source = "Crossref API", other = "OpenAlex (3080)"),
                "Shapiro|1972" = list(citations = "1079", source = "OpenAlex", other = "Crossref API ( 906)"),
                "Brown|1974" = list(citations = "4639", source = "Google Scholar", other = "Crossref API (2317); OpenAlex ( 2910)"),
                "Fligner|1976" = list(citations = "13", source = "OpenAlex", other = "Crossref API (12)"),
                "Hartley|1950" = list(citations = "444", source = "OpenAlex", other = "Crossref API (69)"),
                "Goldfeld|1965" = list(citations = "492", source = "Crossref API", other = "OpenAlex (128)"),
                "MacKinnon|1985" = list(citations = "1697", source = "OpenAlex", other = "Crossref API (1283)"),
                "Welch|1947" = list(citations = "4140", source = "OpenAlex", other = "Crossref API (1317)"),
                "Mauchly|1940" = list(citations = "1144", source = "OpenAlex", other = "Crossref API (847)"),
                "Greenhouse|1959" = list(citations = "5000", source = "OpenAlex", other = "Crossref API (3931)"),
                "Huynh|1976" = list(citations = "614", source = "OpenAlex", other = "Crossref API (498)"),
                "Mahalanobis|1936" = list(citations = "14812", source = "Google Scholar", other = ""),
                "Wilcoxon|1945" = list(citations = "14807", source = "OpenAlex", other = "Crossref API (12552)"),
                "Friedman|1937" = list(citations = "6494", source = "Crossref API", other = "OpenAlex (3886)"),
                "Ramsey|1969" = list(citations = "3345", source = "OpenAlex", other = "Crossref API (2288)"),
                "Marquardt|1970" = list(citations = "5773", source = "Crossref API", other = "OpenAlex (869)"),
                "O'Brien|2007" = list(citations = "10819", source = "OpenAlex", other = "Crossref API (8978)"),
                "Durbin|1951" = list(citations = "3647", source = "OpenAlex", other = "Crossref API (1491)"),
                "Godfrey|1978" = list(citations = "1452", source = "OpenAlex", other = "Crossref API (943)"),
                "Huitema|2011" = list(citations = "1951", source = "Google Scholar", other = ""),
                "Tukey|1977" = list(citations = "32182", source = "Google Scholar", other = ""),
                "Belsley|1980" = list(citations = "7307", source = "OpenAlex", other = "Crossref API (6525)"),
                "Hosmer|1980" = list(citations = "1861", source = "OpenAlex", other = "Crossref API (1585)"),
                "Peduzzi|1996" = list(citations = "9333", source = "OpenAlex", other = "Crossref API (7759)"),
                "Firth|1993" = list(citations = "4623", source = "OpenAlex", other = "Crossref API (4051"),
                "Box|1962" = list(citations = "909", source = "OpenAlex", other = "Crossref API (789"),
                "Pregibon|1981" = list(citations = "1306", source = "OpenAlex", other = "Crossref API (895)"),
                "Hanley|1982" = list(citations = "22039", source = "OpenAlex", other = "Crossref API (17505)"),
                "Zhang|1998" = list(citations = "4162", source = "OpenAlex", other = "Crossref API (3386)"),
                "Mardia|1970" = list(citations = "4455", source = "OpenAlex", other = "Crossref API (3748)"),
                "Wright|1934" = list(citations = "2378", source = "OpenAlex", other = "Crossref API (1668)"),
                "Duncan|1966" = list(citations = "1286", source = "OpenAlex", other = "Crossref API (792)"),
                "Sobel|1982" = list(citations = "12734", source = "OpenAlex", other = "Crossref API (8145)"),
                "Green|1991" = list(citations = "3838", source = "OpenAlex", other = "Crossref API (2678)"),
                "Bentler|1987" = list(citations = "5741", source = "OpenAlex", other = "Crossref API (4265)"),
                "Jackson|2003" = list(citations = "1224", source = "OpenAlex", other = "Crossref API (915)"),
                "Székely|2007" = list(citations = "2332", source = "OpenAlex", other = "Crossref API (2084)"),
                "Ma|2011" = list(citations = "176", source = "OpenAlex", other = "Crossref API (151)"),
                "Kozachenko|1987" = list(citations = "1344", source = "Google Scholar", other = ""),
                "Dickey|1979" = list(citations = "23128", source = "OpenAlex", other = "Crossref API (6688)"),
                "Phillips|1988" = list(citations = "18122", source = "OpenAlex", other = "Crossref API (12692)"),
                "Kwiatkowski|1992" = list(citations = "12655", source = "OpenAlex", other = "Crossref API (8643)"),
                "Canova|1995" = list(citations = "754", source = "Google Scholar", other = "Crossref API (227); OpenAlex (193)"),
                "Osborn|1988" = list(citations = "304", source = "OpenAlex", other = "Crossref API (186)"),
                "Ljung|1978" = list(citations = "6096", source = "OpenAlex", other = "Crossref API (4754)"),
                "Engle|1982" = list(citations = "20792", source = "OpenAlex", other = "Crossref API (14663)"),
                "Jarque|1987" = list(citations = "2984", source = "OpenAlex", other = "Crossref API (2282)"),
                "Johansen|1991" = list(citations = "11229", source = "OpenAlex", other = "Crossref API (6742)"),
                "Engle|1987" = list(citations = "32099", source = "OpenAlex", other = "Crossref API (19239)"),
                "Nyblom|1989" = list(citations = "878", source = "OpenAlex", other = "Crossref API (684)"),
                "Brown|1975" = list(citations = "5117", source = "OpenAlex", other = "Crossref API (3522)"),
                "Engle|1993" = list(citations = "3716", source = "OpenAlex", other = "Crossref API (2647)"),
                "Glosten|1993" = list(citations = "11346", source = "Crossref API", other = "OpenAlex (8736)"),
                "Nelson|1991" = list(citations = "10467", source = "OpenAlex", other = "Crossref API (6988)"),
                "Hyndman|2008" = list(citations = "3529", source = "OpenAlex", other = "Crossref API (2498)"),
                "Lütkepohl|2005" = list(citations = "6022", source = "OpenAlex", other = "Crossref API (2917)")
            )

            get_citation <- function(first_author, year) {
                key <- paste0(first_author, "|", year)
                hit <- citation_db[[key]]
                if (is.null(hit)) {
                    return(list(citations = NULL, source = "", other = ""))
                }
                hit
            }

            # In-text citation label from a refs_db entry, following APA 7th
            # edition: 1-2 authors spelled out with "&"; 3 or more authors
            # as "First et al. (Year)" starting from the very first
            # citation.
            # ES: etiqueta de cita en el texto a partir de una entrada de
            # refs_db, siguiendo APA 7.a edicion: 1-2 autores completos con
            # "&"; 3 o mas autores como "Primero et al. (Ano)" desde la
            # primera cita.
            in_text_cite <- function(r) {
                n <- length(r$authors)
                surnames <- vapply(r$authors, function(a) a[1], character(1))
                label <- if (n == 1) {
                    surnames[1]
                } else if (n == 2) {
                    paste0(surnames[1], " & ", surnames[2])
                } else {
                    paste0(surnames[1], " et al.")
                }
                paste0(label, " (", r$year, ")")
            }

            build_citations_table <- function(lang) {
                ref_hdr    <- .al_tr(lang, "Reference", "Referencia")
                cites_hdr  <- .al_tr(lang, "Citations", "Citaciones")
                source_hdr <- .al_tr(lang, "Source", "Fuente")
                other_hdr  <- .al_tr(lang, "Other sources", "Otras fuentes")
                type_hdr   <- .al_tr(lang, "Reference type", "Tipo de referencia")
                pending    <- .al_tr(lang, "Pending", "Pendiente")

                td_left   <- 'style="text-align: left; padding: 6px 10px;"'
                td_center <- 'style="text-align: center; padding: 6px 10px;"'
                th_left   <- 'style="text-align: left; padding: 6px 10px; border-bottom: 1px solid #000;"'
                th_center <- 'style="text-align: center; padding: 6px 10px; border-bottom: 1px solid #000;"'

                # Published articles only: books are excluded from this
                # table (same logic as build_biblio_table).
                #
                # ES: Solo articulos publicados: los libros se excluyen de
                # esta tabla (misma logica que build_biblio_table).
                article_refs <- Filter(function(r) !isTRUE(r$is_book), refs_db)

                # Alphabetical order by the first column (the in-text
                # citation label, "Surname (Year)"), matching what
                # build_biblio_table already does via sort(journal_names).
                #
                # ES: Orden alfabetico por la primera columna (etiqueta de
                # cita en el texto: "Apellido (Anio)"), igual que ya se hace
                # en build_biblio_table via sort(journal_names).
                cite_labels  <- vapply(article_refs, in_text_cite, character(1))
                article_refs <- article_refs[order(cite_labels)]

                rows_html <- vapply(article_refs, function(r) {
                    first_author <- r$authors[[1]][1]
                    cite <- get_citation(first_author, r$year)
                    cites_val  <- if (!is.null(cite$citations)) cite$citations else pending
                    source_val <- if (nzchar(cite$source)) cite$source else "\u2014"
                    other_val  <- if (nzchar(cite$other)) cite$other else "\u2014"
                    paste0(
                        "<tr>",
                        "<td ", td_left, ">", esc(in_text_cite(r)), "</td>",
                        "<td ", td_center, ">", esc(cites_val), "</td>",
                        "<td ", td_center, ">", esc(source_val), "</td>",
                        "<td ", td_center, ">", esc(other_val), "</td>",
                        "<td ", td_center, ">", esc(ref_type_label(r$ref_type, lang)), "</td>",
                        "</tr>"
                    )
                }, character(1))

                table_label <- .al_tr(lang, "Table 1", "Tabla 1")
                title_txt <- .al_tr(lang, "Citations", "Citaciones")
                overall_title_txt <- if (lang == "es") {
                    "Perfil bibliom\u00e9trico e impacto de la literatura utilizada"
                } else {
                    "Bibliometric Profile and Impact of the Literature Used"
                }
                overall_subtitle_txt <- if (lang == "es") {
                    "(s\u00f3lo publicaciones peri\u00f3dicas)"
                } else {
                    "(periodicals only)"
                }
                section_title_txt <- if (lang == "es") {
                    "M\u00e9tricas de citaci\u00f3n a nivel de art\u00edculo"
                } else {
                    "Article-Level Citation Metrics"
                }
                note_label <- .al_tr(lang, "Note.", "Nota.")
                intro_txt <- if (lang == "es") {
                    paste0(
                        "<p>El n\u00famero de citaciones de cada referencia se reporta como el valor ",
                        "m\u00e1s alto encontrado entre las fuentes consultadas (Google Scholar, ",
                        "Crossref, RePEc/EconPapers, CiNii Research u otras bases equivalentes), no ",
                        "un promedio ni el valor m\u00e1s bajo. Distintas fuentes suelen reportar cifras ",
                        "diferentes para la misma referencia, ya que cada base indiza un conjunto ",
                        "distinto de trabajos citantes; la columna \u201cOtras fuentes\u201d indica cu\u00e1ndo se ",
                        "detect\u00f3 esa discrepancia. Los valores corresponden a una instant\u00e1nea tomada ",
                        "en agosto de 2026 y cambian continuamente. Los libros no se incluyen en esta ",
                        "tabla; solo se listan aqu\u00ed art\u00edculos publicados en revistas (ver Tabla 1).</p>"
                    )
                } else {
                    paste0(
                        "<p>The citation count for each reference is reported as the highest value ",
                        "found across the sources consulted (Google Scholar, Crossref, RePEc/",
                        "EconPapers, CiNii Research, or other equivalent databases), not an average ",
                        "or the lowest value. Different sources often report different figures for ",
                        "the same reference, since each database indexes a different set of citing ",
                        "works; the \u201cOther sources\u201d column indicates when that discrepancy was ",
                        "detected. Values correspond to a snapshot taken in August 2026 and change ",
                        "continuously. Books are not included in this table; only journal-published ",
                        "articles are listed here (see Table 1).</p>"
                    )
                }

                paste0(
                    '<div style="max-width: 700px; width: 100%; line-height: 1; margin-top: 24px; ',
                    'text-align: justify;">',
                    '<div style="margin-bottom: 2px;">',
                    '<h3 style="margin: 0;">', overall_title_txt, '</h3>',
                    '<div style="font-size: 0.75em; font-style: italic; line-height: 1; margin-top: 2px;">',
                    overall_subtitle_txt, '</div>',
                    '</div>',
                    '<h4 style="margin-top: 16px;">', section_title_txt, '</h4>',
                    intro_txt,
                    '<div style="page-break-inside: avoid; break-inside: avoid;">',
                    '<table style="border-collapse: collapse; width: 100%; margin-bottom: 4px;">',
                    '<tr><td style="border: none; padding: 0; line-height: 1;"><b>', table_label, '</b></td></tr>',
                    '<tr><td style="border: none; padding: 0; font-style: italic; line-height: 1;">', title_txt, '</td></tr>',
                    '</table>',
                    '<table style="border-collapse: collapse; width: 100%; font-size: 0.85em; ',
                    'border-top: 2px solid #000; border-bottom: 2px solid #000;">',
                    "<thead><tr>",
                    "<th ", th_left, ">", ref_hdr, "</th>",
                    "<th ", th_center, ">", cites_hdr, "</th>",
                    "<th ", th_center, ">", source_hdr, "</th>",
                    "<th ", th_center, ">", other_hdr, "</th>",
                    "<th ", th_center, ">", type_hdr, "</th>",
                    "</tr></thead>",
                    "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
                    "</table>",
                    "</div>",
                    "</div>"
                )
            }

            build_biblio_table <- function(lang) {
                venue_hdr   <- .al_tr(lang, "Journal", "Revista")
                scopus_hdr  <- "Scopus"
                wos_hdr     <- "Web of Science"
                other_hdr   <- .al_tr(lang, "Other relevant indexes", "Otras indexaciones relevantes")
                quart_hdr   <- .al_tr(lang, "Quartile", "Cuartil")

                # Journals only: books have no bibliographic indexing data
                # (Scopus/WoS/quartile are attributes of periodicals, not
                # books), so they are excluded from this table instead of
                # showing empty cells or "N/A". One row per journal (not per
                # reference): this data belongs to the journal, so listing
                # the same journal multiple times (once per article
                # published in it) would be redundant. Reference type,
                # being a per-article datum, lives in the Citations table
                # (Table 1), not here.
                #
                # ES: Solo revistas: los libros no tienen datos de
                # indexacion bibliografica (Scopus/WoS/cuartil son
                # atributos de revistas periodicas, no de libros), asi que
                # se excluyen de esta tabla en vez de mostrar celdas vacias
                # o "N/A". Una fila por revista (no por referencia): estos
                # datos son propiedad de la revista, asi que listar la
                # misma revista varias veces (una por cada articulo
                # publicado en ella) es redundante. El tipo de referencia,
                # al ser un dato por articulo, vive en la tabla de
                # Citaciones (Tabla 1), no aqui.
                journal_names <- unique(vapply(
                    Filter(function(r) !isTRUE(r$is_book), refs_db),
                    function(r) r$journal, character(1)
                ))
                journal_names <- sort(journal_names)

                td_center <- 'style="text-align: center; padding: 6px 10px;"'
                th_center <- 'style="text-align: center; padding: 6px 10px; border-bottom: 1px solid #000;"'

                rows_html <- vapply(journal_names, function(jname) {
                    bib <- get_journal_biblio(jname)
                    paste0(
                        "<tr>",
                        "<td ", td_center, ">", esc(jname), "</td>",
                        "<td ", td_center, ">", esc(yesno(bib$scopus, lang)), "</td>",
                        "<td ", td_center, ">", esc(yesno(bib$wos, lang)), "</td>",
                        "<td ", td_center, ">", esc(bib$other), "</td>",
                        "<td ", td_center, ">", esc(bib$quartile), "</td>",
                        "</tr>"
                    )
                }, character(1))

                table_label <- .al_tr(lang, "Table 2", "Tabla 2")
                title_txt <- .al_tr(lang, "Bibliometric Summary", "Resumen bibliom\u00e9trico")
                section_title_txt <- if (lang == "es") {
                    "M\u00e9tricas de calidad y divulgaci\u00f3n a nivel de revista"
                } else {
                    "Journal-Level Quality and Dissemination Metrics"
                }
                note_label <- .al_tr(lang, "Note.", "Nota.")
                intro_txt <- if (lang == "es") {
                    paste0(
                        "<p>Esta tabla describe, para cada revista en la que se public\u00f3 al menos ",
                        "una de las referencias de AssumptionsLab, su presencia en las principales ",
                        "bases de indexaci\u00f3n bibliogr\u00e1fica (Scopus, Web of Science) y, cuando aplica, ",
                        "otras indexaciones relevantes y el cuartil de la revista. Estos indicadores ",
                        "reflejan la calidad editorial y el alcance de divulgaci\u00f3n de la publicaci\u00f3n, ",
                        "no la calidad del art\u00edculo individual citado. Cada revista aparece una sola ",
                        "vez, aunque se hayan citado varios art\u00edculos publicados en ella (ver Tabla 2).</p>"
                    )
                } else {
                    paste0(
                        "<p>This table describes, for each journal in which at least one of ",
                        "AssumptionsLab's references was published, its presence in the main ",
                        "bibliographic indexing databases (Scopus, Web of Science) and, where ",
                        "applicable, other relevant indexes and the journal's quartile. These ",
                        "indicators reflect the editorial quality and dissemination reach of the ",
                        "publication, not the quality of the individual cited article. Each journal ",
                        "appears only once, even if several articles published in it were cited ",
                        "(see Table 2).</p>"
                    )
                }
                note_txt <- if (lang == "es") {
                    paste0(
                        note_label, " Los datos de revista (Scopus, Web of Science, cuartil, otras ",
                        "indexaciones) se verificaron por revista, no por art\u00edculo individual, ya ",
                        "que son propiedad de la publicaci\u00f3n, no del art\u00edculo. El tipo de referencia ",
                        "de cada art\u00edculo se muestra en la Tabla 1 (Citaciones), no aqu\u00ed. Los ",
                        "libros no se incluyen porque los indicadores de esta tabla son propios de ",
                        "publicaciones peri\u00f3dicas."
                    )
                } else {
                    paste0(
                        note_label, " Journal-level data (Scopus, Web of Science, quartile, other ",
                        "indexes) were verified per journal, not per individual article, since they ",
                        "belong to the publication, not the article. Each article's reference type ",
                        "is shown in Table 1 (Citations), not here. Books are not included, since ",
                        "the indicators in this table are specific to periodicals."
                    )
                }

                paste0(
                    '<div style="max-width: 700px; width: 100%; line-height: 1; margin-top: 24px; text-align: justify;">',
                    '<h4 style="margin-top: 0;">', section_title_txt, '</h4>',
                    intro_txt,
                    '<div style="page-break-inside: avoid; break-inside: avoid;">',
                    '<table style="border-collapse: collapse; width: 100%; margin-bottom: 4px;">',
                    '<tr><td style="border: none; padding: 0; line-height: 1;"><b>', table_label, '</b></td></tr>',
                    '<tr><td style="border: none; padding: 0; font-style: italic; line-height: 1;">', title_txt, '</td></tr>',
                    '</table>',
                    '<table style="border-collapse: collapse; width: 100%; font-size: 0.85em; ',
                    'border-top: 2px solid #000; border-bottom: 2px solid #000;">',
                    "<thead><tr>",
                    "<th ", th_center, ">", venue_hdr, "</th>",
                    "<th ", th_center, ">", scopus_hdr, "</th>",
                    "<th ", th_center, ">", wos_hdr, "</th>",
                    "<th ", th_center, ">", other_hdr, "</th>",
                    "<th ", th_center, ">", quart_hdr, "</th>",
                    "</tr></thead>",
                    "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
                    "</table>",
                    "</div>",
                    '<p style="margin-top: 4px; font-size: 0.85em;">', note_txt, "</p>",
                    "</div>"
                )
            }


            join_authors_apa <- function(authors) {
                n <- length(authors)
                parts <- vapply(authors, function(a) paste0(esc(a[1]), ", ", esc(a[2])), character(1))
                if (n == 1) return(parts[1])
                if (n == 2) return(paste0(parts[1], ", &amp; ", parts[2]))
                paste0(paste(parts[1:(n - 1)], collapse = ", "), ", &amp; ", parts[n])
            }

            # ------------------------------------------------------------
            # Verified DOI / link (never invented).
            # ES: DOI / enlace verificado (nunca inventado).
            # ------------------------------------------------------------
            link_html <- function(r) {
                if (!is.null(r$doi) && nzchar(r$doi)) {
                    url <- paste0("https://doi.org/", r$doi)
                    paste0('<a href="', url, '">', esc(url), "</a>")
                } else if (!is.null(r$url) && nzchar(r$url)) {
                    paste0('<a href="', r$url, '">', esc(r$url), "</a>")
                } else {
                    ""
                }
            }

            # ------------------------------------------------------------
            # Citation formatter(s), with a hanging indent via CSS.
            # ES: Formateadores de cita por estilo, con sangria francesa
            # via CSS.
            # ------------------------------------------------------------
            hanging <- function(inner_html) {
                paste0(
                    '<p style="margin: 0 0 12px 0; padding-left: 2em; text-indent: -2em; ',
                    'line-height: 1; text-align: left;">', inner_html, "</p>"
                )
            }

            format_apa <- function(r) {
                link <- link_html(r)
                if (isTRUE(r$is_book)) {
                    inner <- paste0(
                        join_authors_apa(r$authors), " (", r$year, "). <i>", esc(r$title), "</i>. ",
                        esc(r$publisher), ".",
                        if (nzchar(link)) paste0(" ", link) else ""
                    )
                } else {
                    inner <- paste0(
                        join_authors_apa(r$authors), " (", r$year, "). ", esc(r$title), ". ",
                        "<i>", esc(r$journal), ", ", r$volume, "</i>(", r$issue, "), ", r$pages, ".",
                        if (nzchar(link)) paste0(" ", link) else ""
                    )
                }
                hanging(inner)
            }

            # AssumptionsLab always formats references in APA 7th edition —
            # there is no style selector, so format_ref is a direct alias.
            # ES: AssumptionsLab siempre formatea las referencias en APA
            # 7.ª edición — no hay selector de estilo, así que format_ref
            # es un alias directo.
            format_ref <- format_apa

            # ------------------------------------------------------------
            # Filter by topic and sort alphabetically by first author's
            # surname.
            # ES: Filtrado por tema y orden alfabetico por primer apellido.
            # ------------------------------------------------------------
            selected <- if (identical(topic, "all")) {
                refs_db
            } else {
                Filter(function(r) topic %in% r$topics, refs_db)
            }

            sort_key <- vapply(selected, function(r) r$authors[[1]][1], character(1))
            selected <- selected[order(sort_key)]

            page_style <- 'style="max-width: 700px; line-height: 1; text-align: justify;"'

            title_txt <- .al_tr(lang, "Methodological bibliography", "Bibliograf\u00eda metodol\u00f3gica")

            intro <- if (lang == "es") {
                paste0(
                    '<div ', page_style, '>',
                    "<h3>", title_txt, "</h3>",
                    "<p>Esta secci\u00f3n re\u00fane las referencias metodol\u00f3gicas que respaldan las decisiones, ",
                    "criterios e interpretaciones de AssumptionsLab. Cada referencia fue verificada individualmente ",
                    "(autores, revista o editorial, volumen, p\u00e1ginas y DOI o enlace directo) antes de incluirse; ",
                    "AssumptionsLab no genera citas autom\u00e1ticamente ni acepta referencias sin verificar.</p>",
                    "<p>Estilo de referencia: <b>", style_label, "</b>.</p>",
                    "</div>"
                )
            } else {
                paste0(
                    '<div ', page_style, '>',
                    "<h3>", title_txt, "</h3>",
                    "<p>This section collects the methodological references that support the decisions, criteria, ",
                    "and interpretations in AssumptionsLab. Each reference was individually verified (authors, ",
                    "journal or publisher, volume, pages, and DOI or direct link) before being included; ",
                    "AssumptionsLab does not auto-generate citations or accept unverified references.</p>",
                    "<p>Reference style: <b>", style_label, "</b>.</p>",
                    "</div>"
                )
            }

            topic_label <- switch(
                topic,
                all = .al_tr(lang, "All references", "Todas las referencias"),
                assumptions = .al_tr(lang, "Statistical assumptions", "Supuestos estad\u00edsticos"),
                normality = .al_tr(lang, "Normality", "Normalidad"),
                homoscedasticity = .al_tr(lang, "Homogeneity / Homoscedasticity", "Homogeneidad / Homocedasticidad"),
                regression = .al_tr(lang, "Linearity and regression", "Linealidad y regresi\u00f3n"),
                related = .al_tr(lang, "Related groups", "Grupos relacionados"),
                robust = .al_tr(lang, "Robust methods and alternatives", "M\u00e9todos robustos y alternativas"),
                topic
            )

            if (length(selected) == 0) {
                refs <- if (lang == "es") {
                    paste0(
                        '<div ', page_style, '>',
                        "<p><b>Tema seleccionado:</b> ", esc(topic_label), "</p>",
                        "<p>Todav\u00eda no hay referencias curadas para este tema. Las referencias se agregan ",
                        "progresivamente a medida que se profundiza la interpretaci\u00f3n de cada componente ",
                        "de AssumptionsLab.</p>",
                        "</div>"
                    )
                } else {
                    paste0(
                        '<div ', page_style, '>',
                        "<p><b>Selected topic:</b> ", esc(topic_label), "</p>",
                        "<p>There are no curated references for this topic yet. References are added ",
                        "progressively as the interpretation of each AssumptionsLab component is deepened.</p>",
                        "</div>"
                    )
                }
            } else {
                ref_lines <- vapply(selected, format_ref, character(1))
                refs <- paste0(
                    '<div ', page_style, '>',
                    "<p><b>", .al_tr(lang, "Selected topic: ", "Tema seleccionado: "), esc(topic_label), "</b></p>",
                    paste(ref_lines, collapse = ""),
                    "</div>"
                )
            }

            notes <- if (lang == "es") {
                paste0(
                    '<div ', page_style, '>',
                    "<p>La bibliograf\u00eda aqu\u00ed presentada re\u00fane art\u00edculos y libros de los autores ",
                    "originales, priorizando las referencias cl\u00e1sicas y aquellas que han recibido mayor ",
                    "reconocimiento y uso en la investigaci\u00f3n actual. No pretende ser exhaustiva, sino ",
                    "ofrecer una selecci\u00f3n precisa de las fuentes m\u00e1s relevantes para los supuestos y ",
                    "estad\u00edsticos considerados en AssumptionsLab. Es recomendable contrastar peri\u00f3dicamente ",
                    "estas referencias con la literatura cient\u00edfica m\u00e1s reciente, ya que pueden publicarse ",
                    "nuevos estudios que respalden, cuestionen o ampl\u00eden los m\u00e9todos aqu\u00ed incluidos.</p>",
                    "</div>"
                )
            } else {
                paste0(
                    '<div ', page_style, '>',
                    "<p>The bibliography presented here brings together articles and books by the original ",
                    "authors, prioritizing classic references and those that have received the greatest ",
                    "recognition and use in current research. It does not aim to be exhaustive, but rather to ",
                    "offer a precise selection of the sources most relevant to the assumptions and statistics ",
                    "considered in AssumptionsLab. It is advisable to periodically compare these references ",
                    "against the most recent scientific literature, since new studies may be published that ",
                    "support, challenge, or expand the methods included here.</p>",
                    "</div>"
                )
            }

            self$results$intro$setContent(intro)
            self$results$references$setContent(refs)
            self$results$notes$setContent(notes)
            self$results$citationsTable$setContent(build_citations_table(lang))
            self$results$biblioSummary$setContent(build_biblio_table(lang))
        }
    )
)
