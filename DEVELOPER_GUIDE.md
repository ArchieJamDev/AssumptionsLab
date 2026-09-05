# AssumptionsLab Developer Guide

**Version:** 1.0  
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

-------------------------------------------------------------------------------

# Generación del informe

El informe constituye el principal producto del análisis.

Debe ser útil tanto para estudiantes como para investigadores.

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

-------------------------------------------------------------------------------

# Estándares gráficos

Los gráficos deben apoyar la interpretación metodológica.

Nunca deben incluirse únicamente por motivos estéticos.

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

-------------------------------------------------------------------------------

# Estándares de programación

El objetivo no es escribir menos código.

El objetivo es escribir mejor código.

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
