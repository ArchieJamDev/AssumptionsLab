# AssumptionsLab Architecture

**Version:** 1.1 (2026-09-17: §12 Internationalization rewritten around
the three actual bilingual mechanisms now implemented; §11 extended to
Bibliography and the "why an analysis, not a passive reference" rationale;
§4, §8, §10 cross-referenced to the native plot-theme, shared-helpers, and
Category A/B mechanisms — lessons from jamovi's official module review)  
**Project:** AssumptionsLab  
**License:** GNU General Public License v3.0  
**Author:** Arquímedes De León Chacón Chacón

---

# Table of Contents

1. Introduction
2. Architectural Philosophy
3. High-Level Architecture
4. Directory Structure
5. Module Architecture
6. Data Flow
7. Analysis Lifecycle
8. Report Generation Architecture
9. Interpretation Engine
10. Graphical Architecture
11. Methodological Library
12. Internationalization
13. Documentation Architecture
14. Future Expansion
15. Design Principles

---

# 1. Introduction

AssumptionsLab has been designed as a modular scientific software platform for
evaluating statistical assumptions and supporting methodological decision
making.

The architecture prioritizes

• scientific rigor;

• modularity;

• maintainability;

• educational value;

• reproducibility.

Every component has a clearly defined responsibility.

-------------------------------------------------------------------------------

# Introducción

AssumptionsLab ha sido diseñado como una plataforma modular para la evaluación
de supuestos estadísticos y el apoyo a la toma de decisiones metodológicas.

La arquitectura prioriza

• rigor científico;

• modularidad;

• mantenibilidad;

• valor educativo;

• reproducibilidad.

Cada componente posee una responsabilidad claramente definida.

-------------------------------------------------------------------------------

# 2. Architectural Philosophy

The architecture follows five fundamental principles.

### Separation of responsibilities

Each file performs one well-defined task.

### Progressive workflow

Every analysis follows the same logical sequence.

### Educational software

The software explains statistical decisions rather than merely producing
results.

### Scientific transparency

Every methodological decision can be identified inside the source code.

### Scalability

New analyses should integrate without modifying the existing architecture.

-------------------------------------------------------------------------------

# Filosofía arquitectónica

Cada nuevo módulo debe adaptarse a la arquitectura existente.

La arquitectura nunca debe adaptarse a un módulo específico.

-------------------------------------------------------------------------------

# 3. High-Level Architecture

```
                USER

                  │

                  ▼

        User Interface (.yaml)

                  │

                  ▼

        Analysis Engine (.R)

                  │

                  ▼

    Statistical Computations

                  │

                  ▼

      Diagnostic Evaluation

                  │

                  ▼

 Methodological Interpretation

                  │

                  ▼

        Report Generation

                  │

                  ▼

            Final Output
```

-------------------------------------------------------------------------------

# Arquitectura general

Toda información sigue un flujo unidireccional.

No existen ciclos innecesarios.

-------------------------------------------------------------------------------

# 4. Directory Structure

```
AssumptionsLab/

│

├── R/

├── jamovi/

│   └── assets/

├── inst/

│   └── assets/

├── data/

├── data-raw/

├── docs/

├── tests/

├── .github/

├── README.md

├── LICENSE

├── CODE_STYLE.md

├── DEVELOPER_GUIDE.md

├── ARCHITECTURE.md

├── NEWS.md

├── DESCRIPTION

└── NAMESPACE
```

-------------------------------------------------------------------------------

## Responsibilities

### R/

Implements every statistical algorithm. `R/shared-helpers.R` holds every
small function (`.al_*()`) duplicated identically across two or more
analysis files — formula-quoting, row-adding wrappers, HTML block
builders, plot color derivation, the normality/stationarity batteries.
A new duplicate belongs there, not copy-pasted into a third file. See
DEVELOPER_GUIDE.md §13.

### jamovi/

Defines analysis options and user interfaces. Also holds `jamovi/assets/`,
the module's own icon and the bundled example dataset.

### inst/

Package-installed resources: icons/logos and the plain-text `CITATION` file.

### data/ and data-raw/

`data/` ships the bundled example dataset shown inside jamovi (registered
under `datasets:` in `jamovi/0000.yaml`); `data-raw/` holds the script that
produces it reproducibly from the original source export.

### docs/

Contains project documentation.

### tests/

Stores validation procedures.

-------------------------------------------------------------------------------

# 5. Module Architecture

Every analysis consists of four primary components.

```
analysis.a.yaml

↓

analysis.u.yaml

↓

analysis.r.yaml

↓

analysis.b.R
```

-------------------------------------------------------------------------------

## analysis.a.yaml

Defines

• options

• variables

• controls

• defaults

-------------------------------------------------------------------------------

## analysis.u.yaml

Defines

• layout

• groups

• visibility

• interface organization

-------------------------------------------------------------------------------

## analysis.r.yaml

Defines

• result tables

• images

• HTML outputs

• textual reports

-------------------------------------------------------------------------------

## analysis.b.R

Implements

• validation

• computations

• diagnostics

• graphics

• interpretations

• reporting

-------------------------------------------------------------------------------

# 6. Data Flow

```
User Selection

↓

Input Validation

↓

Dataset Preparation

↓

Missing Data Processing

↓

Descriptive Statistics

↓

Outlier Detection

↓

Assumption Assessment

↓

Statistical Analysis

↓

Diagnostic Graphics

↓

Interpretation Engine

↓

Report Assembly

↓

Output
```

-------------------------------------------------------------------------------

# Flujo de datos

Cada etapa depende únicamente de la anterior.

No deben existir cálculos redundantes.

-------------------------------------------------------------------------------

# 7. Analysis Lifecycle

Every module should follow exactly the same lifecycle.

```
Initialization

↓

Validation

↓

Preparation

↓

Analysis

↓

Diagnostics

↓

Interpretation

↓

Recommendations

↓

Report

↓

Finish
```

This workflow must remain consistent throughout the project.

-------------------------------------------------------------------------------

# Ciclo de vida del análisis

La experiencia del usuario debe ser idéntica en todos los módulos.

-------------------------------------------------------------------------------

# 8. Report Generation Architecture

Reports are generated after every statistical computation has been completed.

The report is composed of

Introduction

↓

Descriptive Statistics

↓

Assumption Diagnostics

↓

Interpretation

↓

Recommendations

↓

References

Each section should be generated independently.

A report is only ever fully blocked (`jmvcore::reject()`) by a condition
that prevents the whole analysis from running. A single failed diagnostic
test never blocks the report — its row always renders, tiered "Not
computable" with a specific footnote. See CODE_STYLE.md §19.1.

-------------------------------------------------------------------------------

# Arquitectura del informe

La generación del informe nunca debe mezclarse con los cálculos estadísticos.

Un informe solo queda completamente bloqueado (`jmvcore::reject()`) por
una condición que impide ejecutar todo el análisis. Una única prueba
diagnóstica fallida nunca bloquea el informe — su fila siempre se
renderiza, como "No computable" con una nota al pie específica. Ver
CODE_STYLE.md §19.1.

-------------------------------------------------------------------------------

# 9. Interpretation Engine

The interpretation engine represents one of the most important components of
AssumptionsLab.

Its objective is to transform statistical outputs into methodological
recommendations.

```
Statistical Results

↓

Methodological Rules

↓

Interpretation Templates

↓

Educational Explanation

↓

Recommendations
```

The engine should never merely reproduce numerical values.

-------------------------------------------------------------------------------

# Motor de interpretación

Toda interpretación debe responder

¿Qué ocurrió?

¿Por qué?

¿Qué significa?

¿Qué debe hacer ahora el investigador?

-------------------------------------------------------------------------------

# 10. Graphical Architecture

Graphs are organized according to methodological purpose.

```
Distribution

↓

Normality

↓

Variance

↓

Outliers

↓

Influence

↓

Residuals

↓

Final Diagnostics
```

Graphs should reinforce interpretation rather than duplicate numerical results.

Every plot's visual identity (theme, colors) is inherited from jamovi's
own Theme/Palette settings via its native `ggtheme`/`theme` mechanism,
never reimplemented as a module-level style system. See DEVELOPER_GUIDE.md
§10 for the render-function contract.

-------------------------------------------------------------------------------

# Arquitectura gráfica

Los gráficos constituyen herramientas metodológicas.

No elementos decorativos.

La identidad visual de cada gráfico se hereda de los propios ajustes de
Tema/Paleta de jamovi vía su mecanismo nativo `ggtheme`/`theme`, nunca
reimplementada como un sistema de estilo a nivel de módulo. Ver
DEVELOPER_GUIDE.md §10 para el contrato de las funciones de render.

-------------------------------------------------------------------------------

# 11. Methodological Library

The Library and Bibliography are independent educational subsystems.

Their objectives are

• explain concepts;

• define statistical indicators;

• describe assumptions;

• support learning;

• complement reports;

• trace a diagnostic back to its source literature (Bibliography).

Both should remain independent from statistical computations — neither
reads nor computes anything from the user's dataset.

Independent from computations does not mean they belong outside jamovi's
*analysis* framework. Both are implemented as jamovi analyses on purpose:
each has its own filterable options (topic/category, language) and
produces results that render dynamically from those options — the same
shape as any other analysis, regardless of whether it touches `self$data`.
jamovi's own passive citation mechanism (`00refs.yaml` + `refs:`) solves a
different problem — a fixed, non-interactive list of the packages/methods
an analysis used — and would drop the topic filter and the language
toggle entirely. Keeping Library and Bibliography as menu analyses is also
what lets their content appear in the user's exported report alongside
the rest of their results, which a native help/about panel does not.

-------------------------------------------------------------------------------

# Biblioteca metodológica

La Library y la Bibliography constituyen subsistemas educativos
independientes.

No realizan cálculos.

Explican resultados.

Independiente de los cálculos no significa que deban quedar fuera del
marco de *analysis* de jamovi. Ambas se implementan como análisis de
jamovi a propósito: cada una tiene sus propias opciones filtrables
(tema/categoría, idioma) y produce resultados que se generan
dinámicamente a partir de esas opciones — la misma forma que cualquier
otro análisis, sin importar si toca `self$data`. El propio mecanismo
pasivo de citación de jamovi (`00refs.yaml` + `refs:`) resuelve un
problema distinto — una lista fija y no interactiva de los
paquetes/métodos que usó un análisis — y perdería por completo el filtro
por tema y el selector de idioma. Mantener Library y Bibliography como
análisis del menú es también lo que permite que su contenido aparezca en
el informe exportado del usuario junto al resto de sus resultados, algo
que un panel nativo de ayuda/acerca de no ofrece.

-------------------------------------------------------------------------------

# 12. Internationalization

AssumptionsLab follows a bilingual philosophy, implemented as three
separate mechanisms that must not be confused with one another.

```
Source code                Report content              Interface
comments                   (reportLang)                 (jamovi i18n)

English, then       Chosen per-analysis by    Follows jamovi's own
a Spanish "# ES:"    the user, independent     UI-language setting,
block below          of jamovi's own UI        independent of
(CODE_STYLE.md)       language (tr()/          reportLang
                       texts.R)
                                                jamovi/i18n/catalog.pot
                                                + es.po, auto-extracted
                                                from every title/
                                                description/label in
                                                .a.yaml/.u.yaml/.r.yaml/
                                                0000.yaml — no markup
                                                needed in the yaml
                                                itself
```

The interface catalog is jamovi's own native mechanism, not a custom
reimplementation: its compiler scans every yaml definition file and
extracts translatable strings automatically. `jmvtools::i18nCreate("es")`
seeds a translation file; `jmvtools::i18nUpdate("es")` re-syncs it as
strings change. Because gettext's model is one source string to one
translation, every yaml source string must be written in one consistent
language (English) — a module with mixed-language yaml text produces a
mixed-language catalog no translation file can fix.

A future language added to any of the three mechanisms should not require
architectural modification — source comments simply gain no new language
(bilingual is fixed), report content gains a new `reportLang` choice plus
`texts.R` entries, and the interface gains one more `.po` file.

-------------------------------------------------------------------------------

# Internacionalización

La arquitectura de AssumptionsLab sigue una filosofía bilingüe,
implementada como tres mecanismos separados que no deben confundirse entre
sí.

```
Comentarios de           Contenido del informe        Interfaz
código fuente             (reportLang)                 (i18n de jamovi)

Inglés, seguido de   Elegido por el usuario     Sigue el propio ajuste
un bloque "# ES:"     para cada análisis,        de idioma de interfaz
en español debajo     independiente del idioma   de jamovi, independiente
(CODE_STYLE.md)        de interfaz de jamovi      de reportLang
                        (tr()/texts.R)
                                                   jamovi/i18n/catalog.pot
                                                   + es.po, extraídos
                                                   automáticamente de
                                                   cada title/
                                                   description/label en
                                                   .a.yaml/.u.yaml/
                                                   .r.yaml/0000.yaml —
                                                   sin necesitar ninguna
                                                   marca en el yaml
```

El catálogo de interfaz es el propio mecanismo nativo de jamovi, no una
reimplementación propia: su compilador escanea cada archivo de definición
yaml y extrae los strings traducibles automáticamente.
`jmvtools::i18nCreate("es")` siembra un archivo de traducción;
`jmvtools::i18nUpdate("es")` lo resincroniza a medida que cambian los
strings. Como el modelo de gettext es un string fuente por cada
traducción, todo string fuente del yaml debe escribirse en un solo idioma
consistente (inglés) — un módulo con texto yaml en idiomas mezclados
produce un catálogo mezclado que ningún archivo de traducción puede
arreglar.

Un idioma futuro agregado a cualquiera de los tres mecanismos no debería
requerir modificación arquitectónica — los comentarios de código
simplemente no ganan un idioma nuevo (el bilingüismo es fijo), el
contenido del informe gana una nueva opción de `reportLang` más entradas
en `texts.R`, y la interfaz gana un archivo `.po` más.

-------------------------------------------------------------------------------

# 13. Documentation Architecture

Documentation exists at four levels.

```
Repository

↓

Module

↓

Source File

↓

Function
```

Each level should answer progressively more detailed questions.

Repository

What is AssumptionsLab?

Module

What analysis is implemented?

Source File

How is the analysis organized?

Function

How is each task performed?

-------------------------------------------------------------------------------

# Arquitectura documental

La documentación constituye un componente de la arquitectura.

No un elemento accesorio.

-------------------------------------------------------------------------------

# 14. Future Expansion

The architecture has been designed to accommodate future developments without
major structural modifications.

Potential future modules include

• Bayesian statistics

• CB-SEM

• PLS-SEM

• Multilevel models

• Longitudinal analysis

• Survival analysis

• Meta-analysis

• Machine learning diagnostics

Every future module should reuse the same architecture.

-------------------------------------------------------------------------------

# Expansión futura

La escalabilidad constituye un principio fundamental del proyecto.

-------------------------------------------------------------------------------

# 15. Design Principles

Every architectural decision should satisfy the following principles.

### Scientific

Algorithms should faithfully implement accepted statistical procedures.

### Educational

Users should understand every methodological decision.

### Modular

Components should remain independent whenever possible.

### Transparent

Every important decision should be documented.

### Maintainable

Future developers should understand the architecture without external guidance.

### Consistent

Every module should behave as part of the same software ecosystem.

### Reproducible

Analyses should produce reproducible results from identical data.

-------------------------------------------------------------------------------

# Principios de diseño

La arquitectura de AssumptionsLab pretende equilibrar

ingeniería del software,

metodología estadística,

experiencia del usuario

y

valor educativo.

Estos principios deberán preservarse durante toda la evolución del proyecto.

-------------------------------------------------------------------------------

# Final Statement

AssumptionsLab has been designed as an extensible scientific platform rather
than a collection of independent statistical procedures.

Its architecture seeks to guarantee scientific quality, educational excellence,
software sustainability and methodological transparency for researchers,
students and developers worldwide.

-------------------------------------------------------------------------------

# Declaración final

AssumptionsLab ha sido concebido como una plataforma científica extensible y no
como una colección de procedimientos estadísticos independientes.

Su arquitectura busca garantizar calidad científica, excelencia educativa,
sostenibilidad del software y transparencia metodológica para investigadores,
estudiantes y desarrolladores de todo el mundo.

-------------------------------------------------------------------------------

**End of document**
