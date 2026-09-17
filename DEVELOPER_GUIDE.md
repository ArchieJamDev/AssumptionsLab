# AssumptionsLab Developer Guide

**Version:** 1.1 (2026-09-17: native jamovi plot themes, the three
bilingual mechanisms, shared-helpers consolidation, Category A/B pointer,
testing/release checklist items — lessons from jamovi's official module
review)  
**Project:** AssumptionsLab  
**License:** GNU General Public License v3.0  
**Author:** Arquímedes De León Chacón Chacón

---

# Table of Contents

1. Introduction
2. Project Philosophy
3. Project Structure
4. Understanding a Jamovi Module
5. Development Workflow
6. Creating a New Analysis
7. User Interface Standards
8. Statistical Workflow
9. Report Generation
10. Graphical Standards
11. Methodological Interpretations
12. Documentation Standards
13. Coding Standards
14. Testing
15. Version Control
16. Scientific Integrity
17. Release Checklist
18. Future Development

---

# 1. Introduction

Welcome to the AssumptionsLab development team.

This guide explains how every new analysis should be designed, implemented,
documented and maintained.

The objective is to ensure that every module developed for AssumptionsLab
maintains the same scientific quality, user experience and editorial style.

This document complements **CODE_STYLE.md**.

---

# Introducción

Bienvenido al equipo de desarrollo de AssumptionsLab.

Esta guía explica cómo debe diseñarse, implementarse, documentarse y mantenerse
cada nuevo análisis.

Su objetivo es garantizar que todos los módulos mantengan la misma calidad
científica, experiencia de usuario y estilo editorial.

Este documento complementa a **CODE_STYLE.md**.

-------------------------------------------------------------------------------

# 2. Project Philosophy

AssumptionsLab is not simply a statistical software package.

It is an educational platform designed to improve statistical decision making.

Every module must therefore:

• teach;

• justify;

• explain;

• recommend;

• document.

Every programming decision should reflect these principles.

-------------------------------------------------------------------------------

# Filosofía del proyecto

AssumptionsLab no es simplemente un programa estadístico.

Es una plataforma educativa diseñada para mejorar la toma de decisiones
metodológicas.

Cada módulo debe:

• enseñar;

• justificar;

• explicar;

• recomendar;

• documentar.

Toda decisión de programación debe reflejar estos principios.

-------------------------------------------------------------------------------

# 3. Project Structure

The project follows the standard Jamovi architecture.

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

│

├── README.md

├── LICENSE

├── CODE_STYLE.md

├── DEVELOPER_GUIDE.md

├── ARCHITECTURE.md

├── NEWS.md

└── DESCRIPTION
```

-------------------------------------------------------------------------------

# Estructura del proyecto

Cada directorio tiene una responsabilidad específica.

**R/**

Statistical algorithms.

**jamovi/**

User interface definitions, plus the module's own icon and bundled example
dataset under `jamovi/assets/`.

**inst/**

Package-installed icons/logos and the plain-text `CITATION` file.

**data/ and data-raw/**

`data/` ships the example dataset shown inside jamovi; `data-raw/` holds the
script that produces it reproducibly.

**docs/**

Project documentation.

**tests/**

Validation scripts.

-------------------------------------------------------------------------------

# 4. Understanding a Jamovi Module

Every statistical analysis consists of four main files.

```
analysis.a.yaml

analysis.u.yaml

analysis.r.yaml

analysis.b.R
```

-------------------------------------------------------------------------------

## analysis.a.yaml

Defines the analysis options available to the user.

Responsibilities

• Variables

• Factors

• Covariates

• Checkboxes

• Radio buttons

• Option defaults

-------------------------------------------------------------------------------

## analysis.u.yaml

Defines the visual interface.

Responsibilities

• Layout

• Groups

• Controls

• Icons

• User experience

-------------------------------------------------------------------------------

## analysis.r.yaml

Defines the result objects.

Responsibilities

• Tables

• Images

• HTML

• Text

• References

-------------------------------------------------------------------------------

## analysis.b.R

Implements the statistical engine.

Responsibilities

• Validation

• Data preparation

• Statistical analysis

• Diagnostics

• Graphics

• Interpretations

• Report generation

-------------------------------------------------------------------------------

# 5. Development Workflow

Every new module should follow the same workflow.

```
Research

↓

Design

↓

Interface

↓

Implementation

↓

Interpretation

↓

Graphics

↓

Documentation

↓

Testing

↓

Review

↓

Release
```

Never implement a module without first defining its methodological objective.

-------------------------------------------------------------------------------

# Flujo de desarrollo

Todo módulo debe seguir exactamente este proceso.

Nunca comenzar programando.

Siempre comenzar diseñando el análisis.

-------------------------------------------------------------------------------

# 6. Creating a New Analysis

Recommended sequence.

Step 1

Define the methodological objective.

Step 2

Define statistical assumptions.

Step 3

Identify required diagnostics.

Step 4

Design the interface.

Step 5

Implement calculations.

Step 6

Develop graphical diagnostics.

Step 7

Write methodological interpretations.

Step 8

Generate report.

Step 9

Document code.

Step 10

Validate results.

-------------------------------------------------------------------------------

# 7. User Interface Standards

The interface should always be intuitive.

Recommended order.

```
Variables

↓

Grouping variables

↓

Statistical options

↓

Diagnostics

↓

Graphs

↓

Report options
```

Avoid crowded interfaces.

Group related options.

Use consistent terminology.

-------------------------------------------------------------------------------

# Estándares de interfaz

La interfaz debe ser consistente entre todos los módulos.

El usuario nunca debe aprender una nueva organización al cambiar de análisis.

-------------------------------------------------------------------------------

# 8. Statistical Workflow

Every analysis should follow the same internal sequence.

```
Input validation

↓

Data preparation

↓

Missing values

↓

Descriptive statistics

↓

Outlier diagnostics

↓

Assumption assessment

↓

Statistical analysis

↓

Diagnostic graphics

↓

Interpretation

↓

Recommendations

↓

Report
```

-------------------------------------------------------------------------------

# Flujo estadístico

Este flujo constituye la arquitectura lógica oficial de AssumptionsLab.

No debe alterarse sin una justificación metodológica.

-------------------------------------------------------------------------------

# 9. Report Generation

Reports should be educational.

Every report should answer four questions.

What happened?

Why?

How should it be interpreted?

What should the researcher do next?

Never report numbers without interpretation.

A blocking condition (missing variable, wrong factor-level count, a model
that fails to fit entirely) is not the same failure as one diagnostic test
failing while the rest of the analysis succeeds. See CODE_STYLE.md §19.1
for the Category A/Category B distinction — the first calls
`jmvcore::reject()`, the second always renders its row as "Not computable"
with a specific footnote. Never silently drop a row.

-------------------------------------------------------------------------------

# Generación del informe

El informe constituye el principal producto del análisis.

Debe ser útil tanto para estudiantes como para investigadores.

Una condición bloqueante no es la misma falla que una prueba diagnóstica
individual que falla mientras el resto del análisis se completa. Ver
CODE_STYLE.md §19.1 para la distinción entre Categoría A y Categoría B —
la primera llama a `jmvcore::reject()`, la segunda siempre renderiza su
fila como "No computable" con una nota al pie específica. Nunca descartar
una fila en silencio.

-------------------------------------------------------------------------------

# 10. Graphical Standards

Graphics are methodological tools.

They are not decorative elements.

Recommended order.

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

Final diagnostic

All graphics should maintain a consistent visual identity.

Visual identity comes from jamovi itself, not from a module-level
reimplementation. Every render function signature is
`function(image, ggtheme, theme, ...)`; build the plot on `+ ggtheme` so it
follows the user's own jamovi Theme/Palette settings. Derive fixed colors
(point, line, alert, ref) from `theme$color`/`theme$fill` via a small
`.plotColors(theme)` helper — see `logcheck.b.R` for the reference pattern.
Offer a module-level `plotPalette` option only for choices jamovi does not
itself provide (colorblind-safe, viridis) — never a `plotStyle` option,
which only duplicates jamovi's own Theme control. When a manual N-category
scale is needed, apply it via `scale_color_manual()`/`scale_fill_manual()`
placed *after* `+ ggtheme` in the chain — ggtheme's own discrete scale
otherwise silently wins.

-------------------------------------------------------------------------------

# Estándares gráficos

Los gráficos deben apoyar la interpretación metodológica.

Nunca deben incluirse únicamente por motivos estéticos.

La identidad visual proviene del propio jamovi, no de una reimplementación
a nivel de módulo. Toda función de render tiene la firma
`function(image, ggtheme, theme, ...)`; construir el gráfico sobre
`+ ggtheme` para que siga los ajustes de Tema/Paleta que el usuario ya
tiene configurados en jamovi. Derivar los colores fijos (punto, línea,
alerta, referencia) desde `theme$color`/`theme$fill` mediante un pequeño
ayudante `.plotColors(theme)` — ver `logcheck.b.R` como patrón de
referencia. Ofrecer una opción `plotPalette` a nivel de módulo solo para
opciones que jamovi mismo no ofrece (apta para daltonismo, viridis) —
nunca una opción `plotStyle`, que solo duplica el propio control de Tema
de jamovi. Cuando se necesite una escala manual de N categorías, aplicarla
vía `scale_color_manual()`/`scale_fill_manual()` colocada *después* de
`+ ggtheme` en la cadena — de lo contrario, la escala discreta propia de
ggtheme gana en silencio.

-------------------------------------------------------------------------------

# 11. Methodological Interpretations

Every statistical result should include an interpretation.

Interpretations should explain

what,

why,

importance,

limitations,

recommendations.

Never interpret only p-values.

Always explain methodological implications.

-------------------------------------------------------------------------------

# Interpretaciones metodológicas

AssumptionsLab is an educational project.

Interpretations therefore have higher priority than numerical results.

-------------------------------------------------------------------------------

# 12. Documentation Standards

All source files must comply with CODE_STYLE.md.

Documentation must explain

purpose,

design,

methodology,

architecture,

limitations.

Comments should answer questions that code alone cannot answer.

-------------------------------------------------------------------------------

# Estándares de documentación

Toda documentación debe mantenerse bilingüe.

English

↓

Spanish

AssumptionsLab has three separate bilingual mechanisms — do not confuse
them.

1. **Source comments.** Every `.R`/`.yaml` comment: English, then a
   Spanish `# ES:` block immediately below. Governed by CODE_STYLE.md.
2. **Report content.** The `reportLang` option, `tr()`, and `texts.R`.
   Controls the language of the analytical content the user sees inside a
   running analysis's results — chosen per-analysis by the user,
   independent of jamovi's own interface language.
3. **Interface catalog.** jamovi's own native i18n mechanism
   (`jamovi/i18n/catalog.pot` + `es.po`). Controls option-panel titles,
   descriptions, and result/table headers — follows jamovi's own
   UI-language setting, entirely independent of `reportLang`. See
   ARCHITECTURE.md §12.

-------------------------------------------------------------------------------

# ES:

AssumptionsLab tiene tres mecanismos bilingües separados — no
confundirlos.

1. **Comentarios de código fuente.** Cada comentario `.R`/`.yaml`: inglés,
   seguido de un bloque `# ES:` en español inmediatamente debajo. Regido
   por CODE_STYLE.md.
2. **Contenido del informe.** La opción `reportLang`, `tr()` y `texts.R`.
   Controla el idioma del contenido analítico que el usuario ve dentro de
   los resultados de un análisis en ejecución — elegido por el usuario
   para cada análisis, independiente del idioma de interfaz de jamovi.
3. **Catálogo de interfaz.** El propio mecanismo nativo de i18n de jamovi
   (`jamovi/i18n/catalog.pot` + `es.po`). Controla los títulos del panel
   de opciones, descripciones y encabezados de resultado/tabla — sigue el
   propio ajuste de idioma de interfaz de jamovi, totalmente independiente
   de `reportLang`. Ver ARCHITECTURE.md §12.

-------------------------------------------------------------------------------

# 13. Coding Standards

Code must be

Readable.

Modular.

Consistent.

Maintainable.

Avoid duplicated code.

Avoid unnecessary complexity.

Prefer explicit names.

Document methodological decisions.

A small helper (formula-quoting, a `Table$addRow()` wrapper, an HTML
block builder) declared identically as a local closure in two or more
`.b.R` files belongs in `shared-helpers.R` instead, as a top-level `.al_*()`
function. This is not only style: a formula-quoting bug that crashed on
variable names with spaces went unnoticed for months because one file's
local copy of the fix was simply never written — a duplicated helper is a
duplicated place to forget the fix. Alias it locally
(`qname <- .al_qname`) rather than rewriting every call site, so the
shared logic still reads with a short name.

-------------------------------------------------------------------------------

# Estándares de programación

El objetivo no es escribir menos código.

El objetivo es escribir mejor código.

Un ayudante pequeño (entrecomillado de fórmulas, un envoltorio de
`Table$addRow()`, un constructor de bloque HTML) declarado idéntico como
closure local en dos o más archivos `.b.R` pertenece en `shared-helpers.R`
en su lugar, como función `.al_*()` de nivel superior. Esto no es solo
estilo: un bug de entrecomillado de fórmulas que se caía con nombres de
variable con espacio pasó desapercibido por meses porque a un archivo
simplemente le faltaba la copia local del arreglo — un ayudante duplicado
es un lugar duplicado donde olvidar el arreglo. Crear un alias local
(`qname <- .al_qname`) en vez de reescribir cada punto de llamada, para
que la lógica compartida se siga leyendo con un nombre corto.

-------------------------------------------------------------------------------

# 14. Testing

Before releasing any module verify

□ Compiles correctly.

□ No warnings.

□ No runtime errors.

□ Numerical validation completed.

□ Graphics generated correctly.

□ Interpretations consistent.

□ Documentation updated.

□ Translation verified.

□ User interface reviewed.

□ Plots render correctly under jamovi's default theme and at least one
  non-default palette (colorblind-safe or viridis).

□ If any `.a.yaml`/`.u.yaml`/`.r.yaml` text changed, `jmvtools::i18nUpdate("es")`
  was run and every new string has a translation.

-------------------------------------------------------------------------------

# Pruebas

Ningún módulo debe considerarse terminado sin completar esta lista.

-------------------------------------------------------------------------------

# 15. Version Control

Every significant modification should include

Description

Reason

Files modified

Potential impact

Future considerations

Maintain a clear project history.

-------------------------------------------------------------------------------

# Control de versiones

Todo cambio debe poder justificarse.

-------------------------------------------------------------------------------

# 16. Scientific Integrity

Never

modify calculations to obtain expected results.

Never

hide methodological limitations.

Never

remove observations without justification.

Always

prioritize methodological transparency.

Scientific integrity has priority over software convenience.

-------------------------------------------------------------------------------

# Integridad científica

La credibilidad del proyecto depende de la transparencia de sus decisiones.

-------------------------------------------------------------------------------

# 17. Release Checklist

Before publishing a new version verify

□ GPL header.

□ Documentation updated.

□ CODE_STYLE compliance.

□ Bilingual comments.

□ NEWS updated.

□ README updated.

□ Version updated.

□ Tests completed.

□ Examples verified.

□ Repository synchronized.

□ `jamovi/i18n/catalog.pot`/`es.po` regenerated if any interface text
  changed (`jmvtools::i18nUpdate("es")`).

-------------------------------------------------------------------------------

# Lista de publicación

Una versión solo se considera lista cuando todos los elementos anteriores han
sido completados.

-------------------------------------------------------------------------------

# 18. Future Development

Future modules should preserve the same architecture.

Potential future developments include

• Bayesian statistics

• CB-SEM

• PLS-SEM

• Meta-analysis

• Measurement invariance

• Longitudinal analysis

• Survival analysis

• Multilevel models

• Machine learning diagnostics

Every new module should integrate seamlessly with the existing project.

-------------------------------------------------------------------------------

# Final Statement

AssumptionsLab is designed to become an international reference for teaching
statistical assumptions through transparent, reproducible and educational
software.

Every contributor shares the responsibility of preserving these principles.

-------------------------------------------------------------------------------

# Declaración final

AssumptionsLab aspira a convertirse en una referencia internacional para la
enseñanza de los supuestos estadísticos mediante un software transparente,
reproducible y educativo.

Todo desarrollador que contribuya al proyecto comparte la responsabilidad de
preservar estos principios.

-------------------------------------------------------------------------------

End of document.
