# AssumptionsLab Architecture

**Version:** 1.3 (2026-11-18: §12 rewritten — report text computed in
R now uses jamovi's `.()` catalog and the per-analysis `reportLang`
option is gone; 2026-10-08: §11 updated — the standalone `Bibliography`
analysis removed from the menu, `refs:` wired into all 9 analysis modules,
and the attempt to do the same inside Assumption Library reverted once
jamovi's client turned out to render `refs:` only for `Table`-type
results, never `Html`; Library's entire content — guide text and
comparison tables alike — then moved to static `content:` fields
translated through jamovi's native i18n catalog, since none of it
depends on the user's data; its `reportLang` option was removed
entirely, closing the gap this split briefly left (jamovi's own UI
language controlling guide text while a separate option controlled the
tables); 2026-09-17: §12 Internationalization rewritten
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

`R/count-helpers.R` holds the numerical core of Count and Rate Regression
(`countCheck`): plain functions with no jmvcore dependency, so the tests
and `validation/countcheck_verification.R` can check each statistic
without running a jamovi analysis. A new analysis with a substantial
numerical core can follow the same split.

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
nowhere to render there. Since 1.7.1 each category therefore ends with a
small native `Table`, "Sources" (`<category>Sources`, filled in `.init()`
by `.al_library_sources()`), that names each method with its author and
year and carries the category's `refs:`. jamovi then numbers those
sources and lists them in its References section, exactly as it does for
the other analyses, and the category filter hides a category's sources
together with its guide. The plain-text pointer to `docs/Bibliography.md`
was removed from the library's introduction; that page stays the APA 7th
edition home of the full list (CODE_STYLE.md §21: jamovi's own numbered
format in-app, APA 7th in `docs/Bibliography.md`).

**Library as native Text and Table elements (second jamovi review,
minApp 28.3.0).** jamovi later moved reference rendering into the code
shared by every result type (jamovi 2.7.7), so `refs:` attached to `Html`
and `Text` elements are now numbered and listed like table references.
The library therefore dropped its Sources tables: each category's prose is
a `Text` element (jamovi's Markdown: bold lines and lists, no HTML or
CSS) that carries the category's `refs:` directly, and each comparison
table is a native `Table` whose columns are declared in
`jamovi/assumptionlibrary.r.yaml` and whose rows are added in `.init()`.
Every paragraph, list item and cell is its own `.()` entry
(`.al_library_text()` / `.al_library_tables()` in
`R/assumptionlibrary.b.R`), so the Spanish catalogue holds clean sentences
and the old split around R's string-length limit is gone. The category
filter is declared in the r.yaml as `visible: (category:all ||
category:<name>)`, and `.run()` has nothing left to do.

**Library's entire content moved to jamovi's native i18n catalog,
`reportLang` and all.** A separate part of the same review asked Library
to use jamovi's translation catalog instead of its own `reportLang`
option. None of Library's content depends on the user's data: it is
reference/glossary text, identical on every run regardless of what
dataset is loaded, so it can live in the yaml and needs no R code at all.
(The 9 analysis modules, whose text is computed from the data, reach the
same catalog through `.()` in R; see §12.)
Every category's guide paragraphs AND its comparison table (the HTML
`<table>` that `.al_table()` used to build at runtime) are now static
`content:` fields defined directly in `jamovi/assumptionlibrary.r.yaml`,
extracted and translated through `jamovi/i18n/es.po` exactly like
`title` (see CODE_STYLE.md §20), with `.run()` reduced to only
show/hide items by category. `reportLang` itself, and its separate
"Report Language" control, were removed from this analysis entirely —
an initial version kept guide text on the native catalog while leaving
the tables on `reportLang`, and that split surfaced exactly the
confusing behavior it risked: changing jamovi's UI language moved the
prose but not the tables, and changing the module's own language option
moved the tables but not the prose. One category's combined guide+table
text can run well past 10,000 characters, past which R's own parser
refuses a single string literal needing Unicode escapes ("string
constant is too long"), so each category is still several result items
(a guide paragraph, its table, a closing guide paragraph) rather than
one — a length constraint, not a mechanism difference: every item uses
exactly the same static, natively-translated approach.

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
tiene dónde renderizarse ahí. Desde 1.7.1 cada categoría termina por eso
con una `Table` nativa pequeña, "Fuentes" (`<categoría>Sources`, llenada
en `.init()` por `.al_library_sources()`), que nombra cada método con su
autor y año y lleva los `refs:` de la categoría. jamovi numera esas
fuentes y las muestra en su sección de Referencias, igual que en los
demás análisis, y el filtro de categoría oculta las fuentes de una
categoría junto con su guía. Se quitó de la introducción de la
biblioteca el puntero de texto a `docs/Bibliography.md`; esa página
sigue siendo la sede en APA 7.ª edición de la lista completa
(CODE_STYLE.md §21: formato numerado de jamovi dentro de la app, APA 7.ª
edición en `docs/Bibliography.md`).

**Library como elementos nativos Text y Table (segunda revisión de
jamovi, minApp 28.3.0).** jamovi pasó después el dibujo de referencias al
código común de todos los tipos de resultado (jamovi 2.7.7), así que los
`refs:` de elementos `Html` y `Text` ya se numeran y listan como los de
las tablas. Por eso la biblioteca quitó sus tablas Fuentes: la prosa de
cada categoría es un elemento `Text` (Markdown de jamovi: líneas en
negrita y listas, sin HTML ni CSS) que lleva directamente los `refs:` de
la categoría, y cada tabla comparativa es una `Table` nativa con columnas
declaradas en `jamovi/assumptionlibrary.r.yaml` y filas añadidas en
`.init()`. Cada párrafo, ítem y celda es su propia entrada `.()`
(`.al_library_text()` / `.al_library_tables()`), de modo que el catálogo
en español guarda frases limpias y desaparece la división por el límite
de longitud de cadenas de R. El filtro de categoría se declara en el
r.yaml con `visible: (category:all || category:<nombre>)`.

**Todo el contenido de Library se mudó al catálogo i18n nativo de jamovi,
incluida la opción `reportLang` misma.** Otra parte de la misma revisión
pidió que Library usara el catálogo de traducción de jamovi en vez de su
propia opción `reportLang`. Ningún contenido de Library depende de los
datos del usuario: es texto de referencia/glosario, idéntico en cada
corrida sin importar qué dataset esté cargado, así que puede vivir en el
yaml sin código R. (Los 9 módulos de análisis, cuyo texto se calcula a
partir de los datos, llegan al mismo catálogo con `.()` en R; ver §12.) Los párrafos
de guía de cada categoría Y su tabla comparativa (el `<table>` HTML que
antes construía `.al_table()` en tiempo de ejecución) son ahora campos
`content:` estáticos definidos directamente en
`jamovi/assumptionlibrary.r.yaml`, extraídos y traducidos vía
`jamovi/i18n/es.po` exactamente igual que `title` (ver CODE_STYLE.md
§20), con `.run()` reducido a solo mostrar/ocultar ítems por categoría.
`reportLang` misma, y su control aparte "Report Language", se quitaron
por completo de este análisis — una versión inicial dejaba el texto de
guía en el catálogo nativo mientras las tablas seguían en `reportLang`,
y esa división mostró exactamente el comportamiento confuso que
arriesgaba: cambiar el idioma de la interfaz de jamovi movía la prosa
pero no las tablas, y cambiar la opción de idioma propia del módulo
movía las tablas pero no la prosa. El texto combinado de guía+tabla de
una categoría puede superar los 10,000 caracteres, límite más allá del
cual el propio parser de R rechaza un literal de cadena que necesite
escapes Unicode ("string constant is too long"), así que cada categoría
sigue siendo varios ítems de resultado (un párrafo de guía, su tabla, un
párrafo de guía de cierre) en vez de uno solo — una restricción de
longitud, no una diferencia de mecanismo: cada ítem usa exactamente el
mismo enfoque estático y traducido de forma nativa.

-------------------------------------------------------------------------------

# 12. Internationalization

AssumptionsLab is bilingual (English and Spanish) through two mechanisms.

```
Source code comments            Report and interface text
                                (jamovi i18n)

English, then a Spanish         One catalog: jamovi/i18n/catalog.pot
"# ES:" block below             + es.po, compiled to inst/i18n/es.json.
(CODE_STYLE.md)                 Follows jamovi's own language setting.
```

**Interface and fixed result text.** jamovi's compiler extracts every
title, description and label from `.a.yaml`, `.u.yaml`, `.r.yaml` and
`0000.yaml`. No markup is needed in the yaml.

**Report text computed in R.** Guides, interpretations, notes, table
cells and plot labels are written in English inside `.()`. Each run builds
`lang <- .al_lang(self$options)` and binds `. <- lang$tr`; render methods
bind `. <- .al_lang(self$options)$tr`; helpers take `.` as an argument.
`lang$tr` calls `options$translate()`. The long text table in `R/texts.R`
is a function of the translator, `.al_text_table(.)`, read through
`.al_text(lang, section, key)`. Sentences are kept whole for translators:
values go in with `sprintf(.("... %s ..."), value)`, and the Spanish entry
may reorder them with `%1$s`, `%2$s`.

**No cache.** jmvcore (checked in 2.7.38) creates a new translator for each
analysis instance, so a change of jamovi's language reaches the next run
of every analysis. Earlier notes in this repository said `.()` froze at
the language of the R process; that is not the case, and the
per-analysis `reportLang` option that worked around it was removed in
1.7.0 (as in MokkenLab and EGAnetLab).

**Rules.** Every source string is English, because gettext maps one
source string to one translation. No msgid may end in `[...]`: jmvcore
reads a trailing ` [ctx]` as a translation context. Results titles live in
`.r.yaml` and are not overwritten from R. After changing strings, run
`jmvtools::i18nUpdate("es")`, translate the new entries in `es.po`, and
rebuild so `inst/i18n/es.json` is regenerated.

A future language needs only one more `.po` file.

-------------------------------------------------------------------------------

# Internacionalización

AssumptionsLab es bilingüe (inglés y español) mediante dos mecanismos.

```
Comentarios de código           Texto del informe y de la interfaz
                                (i18n de jamovi)

Inglés, seguido de un           Un solo catálogo: jamovi/i18n/catalog.pot
bloque "# ES:" en español       + es.po, compilado a inst/i18n/es.json.
(CODE_STYLE.md)                 Sigue el idioma configurado en jamovi.
```

**Interfaz y textos fijos de resultados.** El compilador de jamovi extrae
cada title, description y label de `.a.yaml`, `.u.yaml`, `.r.yaml` y
`0000.yaml`. El yaml no necesita ninguna marca.

**Texto del informe calculado en R.** Las guías, interpretaciones, notas,
celdas de tablas y etiquetas de gráficos se escriben en inglés dentro de
`.()`. Cada ejecución crea `lang <- .al_lang(self$options)` y enlaza
`. <- lang$tr`; los métodos de renderizado enlazan
`. <- .al_lang(self$options)$tr`; los helpers reciben `.` como argumento.
`lang$tr` llama a `options$translate()`. La tabla de textos largos de
`R/texts.R` es una función del traductor, `.al_text_table(.)`, que se lee
con `.al_text(lang, section, key)`. Las oraciones se mantienen completas
para quien traduce: los valores entran con `sprintf(.("... %s ..."), valor)`
y la entrada en español puede reordenarlos con `%1$s`, `%2$s`.

**Sin caché.** jmvcore (revisado en 2.7.38) crea un traductor nuevo por
cada instancia del análisis, así que un cambio de idioma en jamovi llega a
la siguiente ejecución de cada análisis. Notas anteriores de este
repositorio afirmaban que `.()` quedaba fijo en el idioma del proceso de R;
no es así, y la opción `reportLang` de cada análisis que lo evitaba se
quitó en la 1.7.0 (como en MokkenLab y EGAnetLab).

**Reglas.** Todo texto fuente va en inglés, porque gettext asigna una
traducción a cada texto fuente. Ningún msgid puede terminar en `[...]`:
jmvcore interpreta un ` [ctx]` final como contexto de traducción. Los
títulos de resultados viven en `.r.yaml` y no se sobrescriben desde R.
Después de cambiar textos, ejecutar `jmvtools::i18nUpdate("es")`, traducir
las entradas nuevas en `es.po` y recompilar para regenerar
`inst/i18n/es.json`.

Un idioma futuro solo necesita un archivo `.po` más.

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
