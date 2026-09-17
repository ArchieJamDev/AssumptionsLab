# AssumptionsLab Code Style Guide

Version: 1.1 (2026-09-17: added §19.1 Category A/B error handling, native
jamovi plot themes to §18, native jamovi i18n catalog to §20 — lessons
from jamovi's official module review)
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

Never reimplement jamovi's own Theme/Palette controls. Every render function
receives `ggtheme`/`theme` from jamovi itself — build on `+ ggtheme`, never
a parallel `plotStyle` option. A module-level palette option is only
justified when it is genuinely additive to what jamovi already offers (a
colorblind-safe or viridis set jamovi does not ship) — verify with
`jmvcore::colorPalette()` before adding one, don't assume.

A manual `scale_color_manual()`/`scale_fill_manual()` must be added to the
plot *after* `+ ggtheme`, never before — ggtheme carries its own discrete
scale, and whichever scale is added last to the ggplot object wins.

-------------------------------------------------------------------------------

# ES:

Nunca reimplementar los propios controles de Tema/Paleta de jamovi. Cada
función de render recibe `ggtheme`/`theme` desde el propio jamovi —
construir sobre `+ ggtheme`, nunca una opción `plotStyle` paralela. Una
opción de paleta a nivel de módulo solo se justifica cuando es genuinamente
aditiva respecto a lo que jamovi ya ofrece (un conjunto apto para
daltonismo o viridis que jamovi no trae) — verificar con
`jmvcore::colorPalette()` antes de agregar una, no asumir.

Un `scale_color_manual()`/`scale_fill_manual()` manual debe agregarse al
gráfico *después* de `+ ggtheme`, nunca antes — ggtheme trae su propia
escala discreta, y para una misma estética gana la escala agregada al
final del objeto ggplot.

-------------------------------------------------------------------------------

# 19. Error Messages

Error messages should

• explain the problem;

• explain why it occurred;

• indicate how to solve it.

## 19.1 Category A vs. Category B

Not every failure is the same failure. Distinguish the two before writing a
`tryCatch`.

**Category A** — a condition that blocks the entire analysis (no dependent
variable, wrong factor-level count, no predictors, a model that fails to
fit). Call `jmvcore::reject(message)` with the real reason, including
`conditionMessage(e)` where relevant. Never let the analysis render a
completed-looking report with the explanation buried in the intro text.

**Category B** — one diagnostic test fails while the rest of the analysis
succeeds (too few observations, a constant series, a near-singular
sub-model). The row must always render — never silently vanish — tiered
"Not computable," with a footnote naming the *specific* methodological
reason. A generic "could not be computed" is barely better than a vanished
row.

A sharp trap for Category B: many R statistical functions do not throw on
degenerate input — they warn and return a result with a non-finite
statistic or p-value instead. `!is.null(result)` is not enough; also check
`is.finite(result$p.value)` before treating a result as real.

-------------------------------------------------------------------------------

# ES: 19.1 Categoría A vs. Categoría B

No toda falla es la misma falla. Distinguir las dos antes de escribir un
`tryCatch`.

**Categoría A** — una condición que bloquea todo el análisis (falta la
variable dependiente, número incorrecto de niveles del factor, sin
predictores, un modelo que no logra ajustarse). Llamar a
`jmvcore::reject(mensaje)` con la razón real, incluyendo
`conditionMessage(e)` cuando corresponda. Nunca dejar que el análisis
muestre un informe con apariencia completa mientras la explicación queda
enterrada en el texto introductorio.

**Categoría B** — una prueba diagnóstica falla mientras el resto del
análisis se completa (muy pocas observaciones, una serie constante, un
submodelo casi singular). La fila siempre debe renderizarse — nunca
desaparecer en silencio — como "No computable", con una nota al pie que
nombre la razón metodológica *específica*. Un mensaje genérico de "no se
pudo calcular" apenas es mejor que una fila desaparecida.

Una trampa frecuente para la Categoría B: muchas funciones estadísticas de
R no lanzan error ante datos degenerados — advierten y devuelven un
resultado con estadístico o p-valor no finito. `!is.null(resultado)` no
basta; verificar también `is.finite(resultado$p.value)` antes de tratar un
resultado como real.

-------------------------------------------------------------------------------

# 20. YAML Files

YAML comments should explain

• why an option exists;

• what analysis it affects;

• how it is used.

Avoid comments that merely repeat field names.

Every `title`/`description`/`label`/`menuTitle` value in `.a.yaml`,
`.u.yaml`, `.r.yaml`, and `0000.yaml` is automatically extracted by
jamovi's own compiler into its translation catalog — no `.()` markup
needed. Write these fields in English only. A string in any other
language there is not translated on the fly; it becomes the untranslatable
source text jamovi's catalog serves to every language, including English.
Regenerate the catalog (`jmvtools::i18nUpdate("es")`) whenever such text
changes.

-------------------------------------------------------------------------------

# ES:

Evitar comentarios que solo repiten nombres de campo.

Todo valor `title`/`description`/`label`/`menuTitle` en `.a.yaml`,
`.u.yaml`, `.r.yaml` y `0000.yaml` es extraído automáticamente por el
propio compilador de jamovi hacia su catálogo de traducción — no se
necesita ninguna marca `.()`. Escribir estos campos solo en inglés. Un
texto en otro idioma ahí no se traduce sobre la marcha; se convierte en el
texto fuente no traducible que el catálogo de jamovi sirve a todo idioma,
incluido el inglés. Regenerar el catálogo
(`jmvtools::i18nUpdate("es")`) cada vez que ese texto cambie.

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
