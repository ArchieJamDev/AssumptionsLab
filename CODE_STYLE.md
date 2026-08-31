# AssumptionsLab Code Style Guide

Version: 1.0
Project: AssumptionsLab
License: GNU GPL v3
Author: Arquímedes De León Chacón Chacón

---

# 1. Philosophy

AssumptionsLab is more than a statistical software project.

It is an educational, methodological and scientific software project.

The source code must therefore be understandable, maintainable and educational.

Every source file should be readable as a technical document rather than merely
as executable code.

Our objective is not only to build software.

Our objective is to teach methodology through software.

---

# Filosofía

AssumptionsLab es mucho más que un proyecto de software estadístico.

Es un proyecto científico, metodológico y educativo.

Por ello, el código fuente debe ser comprensible, mantenible y didáctico.

Cada archivo debe poder leerse como un documento técnico y no únicamente como
código ejecutable.

Nuestro objetivo no es solamente desarrollar software.

Nuestro objetivo es enseñar metodología mediante software.

-------------------------------------------------------------------------------

# 2. General Principles

The following principles govern every source file.

• Readability over cleverness.
• Consistency over personal preference.
• Documentation over assumptions.
• Simplicity over unnecessary complexity.
• Scientific rigor over shortcuts.

-------------------------------------------------------------------------------

# Principios generales

Los siguientes principios gobiernan todos los archivos fuente.

• Legibilidad antes que ingenio.
• Consistencia antes que preferencias personales.
• Documentación antes que suposiciones.
• Simplicidad antes que complejidad innecesaria.
• Rigor científico antes que atajos.

-------------------------------------------------------------------------------

# 3. Source File Structure

Every source file must follow the same structure.

1. GPL copyright
2. File description
3. Responsibilities
4. Workflow
5. Code

-------------------------------------------------------------------------------

Example

GPL Header

↓

File Description

↓

Responsibilities

↓

Workflow

↓

Functions

↓

Implementation

-------------------------------------------------------------------------------

# 4. File Description

Every file must begin with a bilingual description.

Example

# -----------------------------------------------------------------------------
# Independent Groups Analysis
# ES: Análisis de grupos independientes.
#
# This file implements the complete analytical workflow for independent-group
# assumption diagnostics.
#
# ES: Este archivo implementa el flujo completo para el diagnóstico de
# supuestos en grupos independientes.
# -----------------------------------------------------------------------------

-------------------------------------------------------------------------------

# 5. Workflow

Large source files must include a workflow section.

Example

Workflow

1. Validate input
2. Prepare data
3. Compute descriptives
4. Detect outliers
5. Evaluate assumptions
6. Generate graphics
7. Build interpretations
8. Assemble report

Every workflow must also include the Spanish translation.

-------------------------------------------------------------------------------

# 6. Section Headers

All sections must use the following separator.

# -----------------------------------------------------------------------------
# Compute descriptive statistics.
# ES: Calcular estadísticos descriptivos.
# -----------------------------------------------------------------------------

Do not use

########################

========================

******

or similar separators.

-------------------------------------------------------------------------------

# 7. Function Documentation

Every non-trivial function must be documented.

Documentation must explain

• What the function does.
• Why it exists.
• Inputs.
• Outputs.
• Methodological notes when appropriate.

Example

# -----------------------------------------------------------------------------
# Compute Cook's distance.
# ES: Calcular la distancia de Cook.
#
# Identifies potentially influential observations by measuring the impact of
# each case on the fitted regression model.
#
# ES: Identifica observaciones potencialmente influyentes midiendo el impacto
# de cada caso sobre el modelo ajustado.
# -----------------------------------------------------------------------------

-------------------------------------------------------------------------------

# 8. Comment Philosophy

Never describe obvious code.

Bad

x <- mean(values)

# Compute mean.

Good

# The mean is reported because it is required by both the descriptive table
# and the methodological interpretation.

ES:

# La media se informa porque es utilizada tanto por la tabla descriptiva como
# por la interpretación metodológica.

-------------------------------------------------------------------------------

# 9. Bilingual Documentation

English is the primary language.

Spanish is always provided immediately below.

Example

# Generate QQ plot.
# ES: Generar gráfico QQ.

Never mix both languages on the same line.

-------------------------------------------------------------------------------

# 10. Methodological Comments

Whenever an algorithm reflects a methodological decision, explain the reason.

Example

# Welch's correction is preferred because it provides more reliable Type I
# error control when variances are unequal.

# ES:
# La corrección de Welch se prefiere porque proporciona un mejor control del
# error Tipo I cuando las varianzas son desiguales.

-------------------------------------------------------------------------------

# 11. Variable Names

Variable names must be descriptive.

Preferred

groupMeans

pooledVariance

cookDistance

studentizedResiduals

Avoid

a

b

tmp

aux

x1

-------------------------------------------------------------------------------

# 12. Function Names

Functions should describe actions.

Preferred

computeNormality()

buildInterpretation()

createQQPlot()

assembleReport()

Avoid

test()

run()

execute()

process()

-------------------------------------------------------------------------------

# 13. Line Length

Recommended maximum

100 characters.

Long comments should be wrapped.

-------------------------------------------------------------------------------

# 14. Empty Lines

Separate logical blocks with one empty line.

Do not insert multiple consecutive blank lines.

-------------------------------------------------------------------------------

# 15. Indentation

Use four spaces.

Do not mix tabs and spaces.

-------------------------------------------------------------------------------

# 16. Code Blocks

Large functions should be divided into logical blocks.

Example

Validation

↓

Preparation

↓

Statistics

↓

Diagnostics

↓

Interpretation

↓

Output

-------------------------------------------------------------------------------

# 17. Report Generation

Report construction must follow the same logical order as the statistical
workflow.

Never generate report sections in arbitrary order.

-------------------------------------------------------------------------------

# 18. Graphics

Graphics should be grouped according to methodological purpose.

Distribution

Normality

Variance

Influence

Correlation

Residuals

Appearance

Never by plotting library.

-------------------------------------------------------------------------------

# 19. Error Messages

Error messages should

• explain the problem;

• explain why it occurred;

• indicate how to solve it.

-------------------------------------------------------------------------------

# 20. YAML Files

YAML comments should explain

• why an option exists;

• what analysis it affects;

• how it is used.

Avoid comments that merely repeat field names.

-------------------------------------------------------------------------------

# 21. Scientific Integrity

Never modify calculations to obtain expected results.

Never suppress warnings without justification.

Never remove observations automatically.

Always document methodological decisions.

AssumptionsLab always cites in APA 7th edition. This is fixed, not a user
preference — there is no citation-style or reference-style selector anywhere
in the interface (Bibliography, Assumption Library, or any analysis).

---

# Integridad científica

Nunca modificar los cálculos para obtener resultados esperados.

Nunca suprimir advertencias sin justificación.

Nunca eliminar observaciones automáticamente.

Documentar siempre las decisiones metodológicas.

AssumptionsLab siempre cita en APA 7.ª edición. Esto es fijo, no una
preferencia del usuario — no existe ningún selector de estilo de citación o
de referencia en ninguna parte de la interfaz (Bibliography, Assumption
Library, ni ningún análisis).

-------------------------------------------------------------------------------

# 22. Backward Compatibility

Documentation improvements must never modify

• algorithms;

• outputs;

• numerical results;

• statistical decisions.

Documentation is editorial.

It is never functional.

-------------------------------------------------------------------------------

# 23. Golden Rule

Every comment must answer at least one question that the code alone cannot
answer.

If a comment merely repeats the code,

remove it.

If a comment explains

• why,

• when,

• where,

• methodological implications,

keep it.

-------------------------------------------------------------------------------

# 24. AssumptionsLab Identity

The source code must reflect the same philosophy as the software.

Readable.

Scientific.

Educational.

Consistent.

Maintainable.

International.

Accessible to the Spanish-speaking scientific community.

Useful to future developers.

-------------------------------------------------------------------------------

End of document.
