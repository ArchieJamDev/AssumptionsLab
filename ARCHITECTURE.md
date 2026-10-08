# AssumptionsLab Architecture

**Version:** 1.2 (2026-10-08: §11 updated — the standalone `Bibliography`
analysis removed from the menu, `refs:` wired into all 9 analysis modules,
and the attempt to do the same inside Assumption Library reverted once
jamovi's client turned out to render `refs:` only for `Table`-type
results, never `Html`; 2026-09-17: §12 Internationalization rewritten
around the three actual bilingual mechanisms now implemented; §11
extended to Bibliography and the "why an analysis, not a passive
reference" rationale; §4, §8, §10 cross-referenced to the native
plot-theme, shared-helpers, and Category A/B mechanisms — lessons from
jamovi's official module review)  
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

The Library is an independent educational subsystem.

Its objectives are

• explain concepts;

• define statistical indicators;

• describe assumptions;

• support learning;

• complement reports.

It should remain independent from statistical computations — it neither
reads nor computes anything from the user's dataset. Independent from
computations does not mean it belongs outside jamovi's *analysis*
framework: it is implemented as a jamovi analysis on purpose, with its
own filterable options (topic/category, language) and results that
render dynamically from those options — the same shape as any other
analysis, regardless of whether it touches `self$data`.

**Bibliography is no longer a menu analysis.** jamovi's own module review
asked that per-analysis method attribution move to jamovi's native
citation mechanism (`00refs.yaml` + `refs:`), attached directly to the
Table/Image that used each source — exactly how every other jamovi
module cites its own methods, and the only way a citation reaches the
user's exported report alongside the result it supports. Bibliography's
other two roles were split rather than dropped: its full APA 7th
reference list plus bibliometric profile (citation counts, journal
indexing/quartile) now live in `docs/Bibliography.md` — preserving that
research effort outside jamovi's interface rather than losing it. We
also tried folding its curated, topic-filterable reading list into
Library's own category sections via `refs:`, since Library already
discusses each source by name — but jamovi's results renderer only
shows a `refs:` reference list for `Table`-type results (confirmed by
reading its Electron client source directly); an `Html`-type result's
renderer never touches it, even though the `refs:` data itself reaches
the client correctly. Since every one of Library's category sections is
`Html` (narrative guide text, not tabular), the native mechanism has
nowhere to render there, so Library keeps a plain-text pointer to
`docs/Bibliography.md` instead. See `docs/Bibliography.md` itself for
why, and CODE_STYLE.md §21 for the citation-style consequence of this
split (jamovi's own numbered format in-app, APA 7th in
`docs/Bibliography.md`).

-------------------------------------------------------------------------------

# Biblioteca metodológica

La Library constituye un subsistema educativo independiente.

Sus objetivos son

• explicar conceptos;

• definir indicadores estadísticos;

• describir supuestos;

• apoyar el aprendizaje;

• complementar informes.

Debe permanecer independiente de los cálculos estadísticos — no lee ni
calcula nada del dataset del usuario. Independiente de los cálculos no
significa que deba quedar fuera del marco de *analysis* de jamovi: se
implementa como un análisis de jamovi a propósito, con sus propias
opciones filtrables (tema/categoría, idioma) y resultados que se generan
dinámicamente a partir de esas opciones — la misma forma que cualquier
otro análisis, sin importar si toca `self$data`.

**Bibliography ya no es un análisis del menú.** La propia revisión del
módulo de jamovi pidió que la atribución de método por análisis se
mudara al mecanismo nativo de citación de jamovi (`00refs.yaml` +
`refs:`), enganchado directamente a la Table/Image que usó cada fuente —
exactamente como cita sus propios métodos cualquier otro módulo de
jamovi, y la única forma de que una cita llegue al informe exportado del
usuario junto al resultado que respalda. Los otros dos roles de
Bibliography se repartieron en vez de eliminarse: su lista completa de
referencias en APA 7.ª edición más su perfil bibliométrico (conteo de
citas, indexación/cuartil de revista) ahora viven en
`docs/Bibliography.md` — conservando ese esfuerzo de investigación fuera
de la interfaz de jamovi en vez de perderlo. También intentamos fusionar
su lista de lectura curada y filtrable por tema dentro de las propias
secciones de categoría de Library vía `refs:`, ya que Library ya
menciona cada fuente por su nombre — pero el renderizador de resultados
de jamovi solo muestra la lista de referencias de `refs:` para
resultados tipo `Table` (confirmado leyendo directamente el código
fuente de su cliente Electron); el renderizador de un resultado tipo
`Html` nunca la usa, aunque los datos de `refs:` sí llegan
correctamente al cliente. Como cada sección de categoría de Library es
`Html` (texto narrativo de guía, no tabular), el mecanismo nativo no
tiene dónde renderizarse ahí, así que Library conserva en cambio un
simple puntero de texto a `docs/Bibliography.md`. Ver el propio
`docs/Bibliography.md` para el porqué, y CODE_STYLE.md §21 para la
consecuencia de estilo de citación de esta división (el propio formato
numerado de jamovi dentro de la app, APA 7.ª edición en
`docs/Bibliography.md`).

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

## Why report content keeps its own `reportLang`, on purpose

`reportLang`/`tr()`/`texts.R` is not a stopgap waiting to be replaced by
jamovi's native `.()` catalog. `.()` is only safe for text jamovi's
compiler extracts from yaml — `jmvcore::Options$translate()` builds its
translator once per R engine process and never invalidates it, so text
assembled dynamically in R and routed through `.()` freezes at whatever
language was active when that process started; nothing short of
restarting jamovi picks up a change. Confirmed by reading `jmvcore`
directly after the regression first shipped (and was reverted) in a
sibling module, FiabilityLab.

Instant, reliable switching also serves the module's teaching mission
directly, not just its mechanics. AssumptionsLab is built to teach
methodology to novice researchers and students. Seeing the same
analytical paragraph in Spanish and then, with one click, in English is
itself part of that lesson — it shows a student early why precise
methodological vocabulary matters, since that precision is what
eventually gets read and published. A mechanism that cannot guarantee an
instant, reliable switch would undermine that, not just inconvenience it.

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

## Por qué el contenido del informe conserva su propio `reportLang`, a propósito

`reportLang`/`tr()`/`texts.R` no es una solución provisional a la espera
de ser reemplazada por el catálogo nativo `.()` de jamovi. `.()` solo es
seguro para texto que el compilador de jamovi extrae del yaml —
`jmvcore::Options$translate()` construye su traductor una sola vez por
proceso del motor de R y nunca lo invalida, así que el texto ensamblado
dinámicamente en R y enrutado por `.()` queda congelado en el idioma que
estaba activo cuando ese proceso arrancó; nada salvo reiniciar jamovi
recoge un cambio. Confirmado leyendo `jmvcore` directamente después de que
la regresión se publicara (y se revirtiera) en un módulo hermano,
FiabilityLab.

El cambio de idioma instantáneo y confiable también sirve directamente a
la misión pedagógica del módulo, no solo a su mecánica. AssumptionsLab
está construido para enseñar metodología a investigadores noveles y
estudiantes. Ver el mismo párrafo analítico en español y luego, con un
clic, en inglés es en sí mismo parte de esa lección — le muestra al
estudiante desde temprano por qué importa la precisión del vocabulario
metodológico, ya que esa precisión es lo que eventualmente se lee y se
publica. Un mecanismo que no puede garantizar un cambio instantáneo y
confiable socavaría eso, no solo lo haría menos cómodo.

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
