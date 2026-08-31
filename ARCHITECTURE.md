# AssumptionsLab Architecture

**Version:** 1.0  
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

├── docs/

├── assets/

├── tests/

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

Implements every statistical algorithm.

### jamovi/

Defines analysis options and user interfaces.

### docs/

Contains project documentation.

### tests/

Stores validation procedures.

### assets/

Stores graphical resources.

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

-------------------------------------------------------------------------------

# Arquitectura del informe

La generación del informe nunca debe mezclarse con los cálculos estadísticos.

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

-------------------------------------------------------------------------------

# Arquitectura gráfica

Los gráficos constituyen herramientas metodológicas.

No elementos decorativos.

-------------------------------------------------------------------------------

# 11. Methodological Library

The Library is an independent educational subsystem.

Its objectives are

• explain concepts;

• define statistical indicators;

• describe assumptions;

• support learning;

• complement reports.

The Library should remain independent from statistical computations.

-------------------------------------------------------------------------------

# Biblioteca metodológica

La Library constituye un diccionario metodológico integrado.

No realiza cálculos.

Explica resultados.

-------------------------------------------------------------------------------

# 12. Internationalization

AssumptionsLab follows a bilingual philosophy.

Source code

English

↓

Spanish comments

User interface

Language files

↓

Translations

Reports

Localized text

↓

User language

Future translations should not require architectural modifications.

-------------------------------------------------------------------------------

# Internacionalización

La arquitectura está preparada para incorporar nuevos idiomas.

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

• Path Analysis

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
