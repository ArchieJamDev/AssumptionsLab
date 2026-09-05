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
# Central repository for pedagogical texts, table titles, column labels,
# notes, and interpretation templates used across every AssumptionsLab
# module, in every active language (currently es/en). Keeping all wording
# here, instead of scattered across each module's .b.R file, is what lets a
# single module render its full report in either language from the same
# analysis code.
#
# ES: Repositorio central de textos pedagógicos, títulos de tabla, etiquetas
# de columna, notas y plantillas de interpretación usados en todos los
# módulos de AssumptionsLab, en cada idioma activo (actualmente es/en).
# Mantener todo el texto aquí, en vez de disperso en el .b.R de cada módulo,
# es lo que permite que un mismo módulo renderice su informe completo en
# cualquiera de los dos idiomas desde el mismo código de análisis.
#
# Responsibilities
# 1. Hold every user-facing string (guides, interpretations, table/column
#    titles) for every module, in every active language.
# 2. Provide lookup helpers that resolve a (language, section, key) triple
#    to its text, falling back to English if a Spanish string is missing.
# 3. Provide text-cleaning and paragraph-assembly helpers shared by every
#    module's report-building code.
#
# ES: Responsabilidades
# 1. Mantener cada cadena de texto orientada al usuario (guías,
#    interpretaciones, títulos de tabla/columna) de cada módulo, en cada
#    idioma activo.
# 2. Proveer funciones de búsqueda que resuelven una tripleta (idioma,
#    sección, clave) a su texto, retrocediendo a inglés si falta la cadena
#    en español.
# 3. Proveer funciones de limpieza de texto y ensamblado de párrafos
#    compartidas por el código de construcción de informes de cada módulo.
#
# Workflow
# 1. A module's .b.R file calls .al_text()/.al_text_block() (or one of its
#    thin wrappers) with the current report language, a section name, and a
#    key.
# 2. .al_normalize_lang() first resolves whatever language value jamovi
#    passed in (an option code, a language name, mixed case, etc.) to a
#    canonical "es" or "en".
# 3. .al_text() looks up that (language, section, key) triple in
#    .al_texts; if the string is missing in the requested language, it
#    falls back to the English entry.
# 4. .al_clean_text()/.al_block() normalize whitespace and assemble the
#    resolved string(s) into the paragraph text the module renders.
#
# ES: Flujo de trabajo
# 1. El .b.R de un módulo llama a .al_text()/.al_text_block() (o alguno de
#    sus envoltorios) con el idioma actual del informe, un nombre de
#    sección y una clave.
# 2. .al_normalize_lang() primero resuelve cualquier valor de idioma que
#    haya pasado jamovi (un código de opción, un nombre de idioma,
#    mayúsculas mixtas, etc.) a un "es" o "en" canónico.
# 3. .al_text() busca esa tripleta (idioma, sección, clave) en .al_texts;
#    si falta la cadena en el idioma solicitado, retrocede a la entrada en
#    inglés.
# 4. .al_clean_text()/.al_block() normalizan espacios en blanco y ensamblan
#    la(s) cadena(s) resuelta(s) en el texto de párrafo que renderiza el
#    módulo.
# -----------------------------------------------------------------------------
.al_texts <- list(
# Active languages: es = Español, en = English. A future language is added
# by creating another top-level list here (e.g. pt, fr, it).
# ES: Idiomas activos: es = Español, en = English. Un idioma futuro se
# agrega creando otra lista de primer nivel aquí (p. ej. pt, fr, it).
es = list(
     # ---- common: shared strings used across every module ----
     # ES: common: cadenas compartidas usadas en todos los módulos
     common = list(
         moduleTitle = "AssumptionsLab",
         briefGuide = "Guía breve",
         appliedInterpretation = "Interpretación aplicada",
         methodologicalDecision = "Decisión metodológica",
         notes = "Notas",
         references = "Referencias",
         seeLibrary = "Revise detalles metodológicos en Assumption Library.",
         doNotDeleteAutomatically = "Un caso marcado no debe eliminarse automáticamente.",
         reviewAndJustify = "Revise el valor original y justifique cualquier decisión.",
         notComputed = "No calculado",
         compatible = "Compatible",
         significantDeviation = "Desviación significativa",
         sigCodes = "Códigos de significancia: * p < .05, ** p < .01, *** p < .001.",
         statsSymbols = "Los símbolos estadísticos se mantienen en formato internacional.",
         colVariable = "Variable",
         colGroup = "Grupo",
         colTest = "Prueba",
         colN = "n",
         colMissing = "Faltantes",
         colMean = "M",
         colSD = "SD",
         colMedian = "Mdn",
         colMin = "Min",
         colMax = "Max",
         colQ1 = "Q1",
         colQ3 = "Q3",
         colIQR = "IQR",
         colLowerLimit = "Límite inferior",
         colUpperLimit = "Límite superior",
         colOutliers = "Atípicos",
         colExtreme = "Extremos",
         colStatistic = "Estadístico",
         colValue = "Valor",
         colP = "p",
         colSig = "Sig.",
         colDf = "df",
         colDf1 = "df1",
         colDf2 = "df2",
         colInterpretation = "Interpretación",
         colConclusion = "Conclusión"
     ),
     # ---- independentGroups: groupCheck module strings ----
     # ES: independentGroups: cadenas del módulo groupCheck
     independentGroups = list(
         title = "Independent Groups",
         intro = c(
             "Use este análisis cuando quiera revisar si una comparación entre grupos independientes",
             "tiene supuestos metodológicos defendibles.",
             "El objetivo no es solo calcular pruebas, sino ayudar a justificar la decisión estadística",
             "con evidencia obtenida de sus propios datos."
         ),
         tableDesign = "Resumen del diseño",
         tableDescriptives = "Estadísticos descriptivos por grupo",
         tableOutliers = "Evaluación de valores atípicos por grupo",
         tableCaseDiagnostics = "Diagnóstico de casos",
         tableNormality = "Pruebas de normalidad por grupo",
         tableNormalitySummary = "Resumen de decisión sobre normalidad",
         tableVariance = "Homogeneidad de varianzas",
         designGuide = c(
             "Use esta revisión cuando las filas representen unidades independientes y cada unidad",
             "pertenezca a un solo grupo.",
             "Antes de elegir t de Student, ANOVA, Welch o Kruskal-Wallis, revise tamaño de grupos,",
             "datos faltantes, normalidad, varianzas y casos potencialmente influyentes.",
             "El análisis es más defendible cuando la decisión se basa en el conjunto de evidencias,",
             "no en una sola prueba aislada."
         ),
         descriptivesGuide = c(
             "Los descriptivos permiten observar diferencias iniciales entre grupos antes de aplicar",
             "una prueba inferencial.",
             "Compare medias, medianas y dispersión. Diferencias grandes entre media y mediana pueden",
             "sugerir asimetría o influencia de valores extremos.",
             "Los descriptivos no prueban hipótesis por sí solos, pero orientan la lectura de los supuestos."
         ),
         outliersGuide = c(
             "La regla IQR identifica valores inusuales dentro de cada grupo.",
             "Un valor atípico no es necesariamente un error: puede ser un caso real, raro o influyente.",
             "En grupos pequeños, pocos valores extremos pueden modificar medias, varianzas y pruebas",
             "de normalidad.",
             "Revise los datos originales antes de decidir si hará análisis de sensibilidad."
         ),
         caseDiagnosticsGuide = c(
             "Estos diagnósticos identifican casos potencialmente influyentes o inusuales en el modelo",
             "de comparación de grupos.",
             "Cook's D señala casos que pueden cambiar estimaciones del modelo.",
             "Mahalanobis D² ayuda a detectar valores alejados respecto al patrón esperado.",
             "Un caso marcado no debe eliminarse automáticamente: revise el dato, compare análisis",
             "con y sin el caso, y justifique la decisión."
         ),
         normalityGuide = c(
             "La normalidad se revisa dentro de cada grupo independiente porque los procedimientos",
             "paramétricos suponen distribuciones aproximadamente normales en cada nivel del factor.",
             "p >= .05 es compatible con normalidad aproximada; esto no demuestra normalidad perfecta.",
             "p < .05 sugiere una desviación significativa respecto a la normalidad.",
             "En muestras pequeñas, complemente las pruebas con gráficos y revisión de atípicos."
         ),
         varianceGuide = c(
             "Estas pruebas revisan si las varianzas de los grupos son razonablemente similares.",
             "p >= .05 es compatible con varianzas homogéneas.",
             "p < .05 sugiere diferencias significativas entre varianzas.",
             "Si los tamaños de grupo son desiguales, la heterogeneidad de varianzas puede afectar",
             "más la interpretación y hacer preferible Welch o métodos robustos."
         ),
         normalityCompatible = "Compatible con normalidad aproximada",
         normalityDeviation5 = "Desviación significativa al 5%",
         normalityDeviation1 = "Desviación significativa al 1%",
         normalityDeviation001 = "Desviación significativa al 0.1%",
         normalitySummaryCompatible = "Compatible con normalidad aproximada",
         normalitySummaryDeviation = "Desviación significativa en al menos una prueba",
         varianceCompatible = "Compatible con varianzas homogéneas",
         varianceDeviation = "Diferencias significativas entre varianzas",
         recommendationParametricCaution = c(
             "El análisis paramétrico puede ser defendible con cautela si el diseño realmente es",
             "independiente, las varianzas son compatibles con homogeneidad y la normalidad no muestra",
             "desviaciones graves.",
             "Si existen grupos pequeños, desbalance importante o casos influyentes, revise análisis",
             "de sensibilidad antes de tomar una decisión final."
         ),
         recommendationWelchOrRobust = c(
             "Considere Welch, Brown-Forsythe, Kruskal-Wallis o una alternativa robusta si hay",
             "heterogeneidad de varianzas, desviaciones claras de normalidad o casos influyentes",
             "que cambian las conclusiones.",
             "La elección debe combinar evidencia estadística, diseño del estudio y pregunta de",
             "investigación."
         )
     ),
     # ---- regression: regCheck module strings ----
     # ES: regression: cadenas del módulo regCheck
     regression = list(
         title = "Simple & Multiple Regression",
         intro = c(
             "Use este análisis para revisar si un modelo de regresión lineal simple o múltiple",
             "tiene supuestos metodológicos defendibles.",
             "El objetivo no es solo detectar problemas, sino enseñar cómo cada diagnóstico afecta",
             "la interpretación de coeficientes, errores estándar, intervalos de confianza y valores p.",
             "La decisión final debe combinar evidencia estadística, diseño del estudio, calidad de",
             "los datos y sentido sustantivo del modelo."
         ),
         designGuide = c(
             "La regresión lineal evalúa la relación entre una variable dependiente numérica y uno o más predictores.",
             "Antes de interpretar coeficientes, revise si la variable dependiente es cuantitativa, si los predictores están correctamente definidos y si el tamaño muestral es razonable para el número de parámetros estimados.",
             "Un modelo puede calcularse aunque sus supuestos sean débiles, pero sus conclusiones pueden ser poco defendibles si no se documentan esos problemas."
         ),
         linearityGuide = c(
             "La linealidad evalúa si la relación entre los predictores numéricos y la variable dependiente puede describirse razonablemente mediante una línea recta.",
             "Este supuesto importa porque un modelo lineal puede producir coeficientes engañosos cuando la relación real es curva, escalonada o cambia de dirección.",
             "Las pruebas y términos exploratorios ayudan a detectar curvatura, pero deben leerse junto con gráficos de residuos y conocimiento sustantivo.",
             "Si hay evidencia de no linealidad, considere transformar variables, agregar términos polinomiales, usar splines o elegir otro tipo de modelo."
         ),
         residualNormalityGuide = c(
             "La normalidad en regresión se evalúa sobre los residuos, no sobre la variable dependiente bruta.",
             "Este supuesto afecta principalmente la precisión de inferencias clásicas como intervalos de confianza y pruebas de significación, sobre todo en muestras pequeñas.",
             "p >= .05 es compatible con normalidad aproximada de residuos; no demuestra normalidad perfecta.",
             "p < .05 sugiere desviación significativa.",
             "Revise si se debe a asimetría, colas pesadas, valores atípicos o mala especificación del modelo."
         ),
         homoscedasticityGuide = c(
             "La homocedasticidad evalúa si la variabilidad de los residuos es aproximadamente constante a lo largo de los valores predichos.",
             "Cuando este supuesto se debilita, los coeficientes pueden seguir siendo útiles, pero los errores estándar, intervalos de confianza y valores p pueden ser poco confiables.",
             "p >= .05 es compatible con homocedasticidad aproximada.",
             "p < .05 sugiere heterocedasticidad.",
             "En ese caso, considere errores estándar robustos, transformaciones o modelos alternativos."
         ),
         independenceGuide = c(
             "La independencia de errores significa que los residuos no deben estar correlacionados por el orden de medición, tiempo, espacio, grupo, aula, participante u otra estructura.",
             "Este supuesto depende principalmente del diseño del estudio, no solo de una prueba estadística.",
             "La prueba Durbin-Watson ayuda a revisar autocorrelación de primer orden cuando los datos tienen un orden natural.",
             "Si las observaciones están agrupadas, repetidas o anidadas, considere modelos mixtos, modelos longitudinales o métodos que representen esa dependencia."
         ),
         multicollinearityGuide = c(
             "La multicolinealidad evalúa si los predictores contienen información muy redundante.",
             "No suele sesgar las predicciones, pero puede volver inestables los coeficientes, ampliar errores estándar y dificultar la interpretación individual de cada predictor.",
             "VIF cercanos a 1 sugieren baja colinealidad.",
             "Valores altos indican que un predictor se explica demasiado bien por otros predictores.",
             "Si hay colinealidad importante, considere combinar variables, eliminar predictores redundantes, centrar variables o cambiar la pregunta analítica."
         ),
         correlationMatrixGuide = c(
             "Estas dos tablas complementan las pruebas de multicolinealidad con una vista general de la asociación entre la variable dependiente y todos los predictores numéricos, cada una en formato APA 7 (triángulo inferior, variables numeradas).",
             "La primera reporta la correlación de Pearson convencional (solo detecta asociación lineal); la segunda reporta la correlación de distancia (Székely et al., 2007), que detecta asociación lineal y no lineal por igual.",
             "La tabla de discordancia señala los pares donde dCor supera notablemente a |r| de Pearson, con la entropía copular (copent) como segunda línea de evidencia."
         ),
         influenceGuide = c(
             "Los diagnósticos de influencia identifican casos que pueden modificar de forma importante los coeficientes, el ajuste o las conclusiones del modelo.",
             "Residuo studentizado, leverage, Cook's D y DFFITS revisan aspectos distintos del mismo problema: qué tan raro es un caso y cuánto cambia el modelo.",
             "Un caso influyente no debe eliminarse automáticamente.",
             "Revise si es un error de registro, un caso válido pero extremo, o una señal de que el modelo no representa bien a todos los subgrupos."
         ),
         transformationsGuide = c(
             "Las transformaciones pueden ayudar cuando hay asimetría fuerte, relaciones no lineales o varianza residual no constante.",
             "No deben usarse solo para mejorar una prueba de supuestos; deben tener sentido estadístico y sustantivo.",
             "Al transformar una variable, cambia también la interpretación del coeficiente.",
             "Documente qué transformación usó, por qué fue necesaria y cómo se interpreta el modelo resultante."
         ),
         robustOptionsGuide = c(
             "Las opciones robustas no son una corrección automática, sino alternativas cuando los supuestos clásicos no son suficientemente defendibles.",
             "Errores estándar robustos pueden ayudar frente a heterocedasticidad.",
             "Modelos robustos pueden reducir la influencia de casos extremos.",
             "Modelos mixtos, generalizados o no lineales pueden ser más adecuados si el problema viene del diseño, la distribución o la forma funcional."
         )
     ),
     # ---- logistic: logCheck module strings ----
     # ES: logistic: cadenas del módulo logCheck
     logistic = list(
         title = "Regresión logística",
         intro = c(
             "Use este análisis para revisar si un modelo de regresión logística binaria tiene",
             "supuestos metodológicos defendibles antes de interpretar odds ratios, coeficientes",
             "o clasificaciones.",
             "El objetivo no es solo detectar problemas, sino enseñar cómo cada diagnóstico afecta",
             "la validez de las conclusiones: separación completa, tamaño de muestra por variable,",
             "linealidad en el logit, bondad de ajuste, discriminación, multicolinealidad e",
             "influencia de casos individuales."
         ),
         designGuide = c(
             "La regresión logística modela la probabilidad de un evento binario en función de uno o más predictores.",
             "Antes de interpretar el modelo, revise que la variable dependiente tenga exactamente dos categorías, que los eventos y no eventos estén razonablemente representados, y que la razón de eventos por variable predictora (EPV) sea suficiente.",
             "Peduzzi et al., (1996) recomiendan al menos 10 eventos por variable predictora como referencia práctica; con EPV bajo, los coeficientes pueden estar sesgados y los errores estándar pueden ser poco confiables."
         ),
         separationGuide = c(
             "La separación completa o cuasi-completa ocurre cuando un predictor (o combinación de predictores) clasifica perfectamente o casi perfectamente los eventos.",
             "Cuando esto sucede, el algoritmo de máxima verosimilitud no converge de forma estable: los coeficientes y errores estándar pueden inflarse a valores extremos sin significado sustantivo.",
             "Coeficientes muy grandes junto con errores estándar muy grandes son la señal típica de este problema.",
             "Si hay separación, considere revisar si el predictor es una copia encubierta de la variable dependiente, combinar categorías poco frecuentes, o usar métodos como la regresión logística penalizada de Firth (1993), diseñada específicamente para corregir el sesgo de máxima verosimilitud y producir estimaciones finitas incluso bajo separación."
         ),
         linearityGuide = c(
             "La regresión logística no asume que el evento se relacione linealmente con los predictores, sino que el logit (el logaritmo de la razón de momios) lo haga.",
             "Este supuesto se revisa habitualmente con el procedimiento de Box-Tidwell (Box & Tidwell, 1962), que agrega términos de interacción entre cada predictor numérico y su propio logaritmo.",
             "Un término significativo sugiere que la relación real no es lineal en el logit y que el predictor podría necesitar una transformación o un término no lineal (por ejemplo, un polinomio o una spline).",
             "Esta revisión solo aplica a predictores numéricos; los predictores categóricos no tienen este supuesto."
         ),
         goodnessOfFitGuide = c(
             "La bondad de ajuste evalúa si el modelo, en conjunto, reproduce razonablemente los datos observados.",
             "La devianza residual compara el modelo ajustado con un modelo saturado; valores muy altos en relación con los grados de libertad sugieren un ajuste pobre.",
             "La prueba de Hosmer-Lemeshow (1980) agrupa los casos por probabilidad predicha y compara frecuencias observadas y esperadas; un valor p bajo sugiere que el modelo no ajusta bien en algún rango de probabilidades.",
             "Esta prueba pierde sensibilidad con muestras muy grandes y es sensible al número de grupos elegido, por lo que debe interpretarse junto con otros diagnósticos, no de forma aislada."
         ),
         discriminationGuide = c(
             "La discriminación evalúa qué tan bien el modelo distingue entre casos con y sin el evento, independientemente de si las probabilidades predichas están bien calibradas.",
             "El área bajo la curva ROC (AUC) resume esta capacidad (Hanley & McNeil, 1982): 0.5 equivale a discriminación nula (azar), valores entre 0.7 y 0.8 se consideran aceptables, y valores por encima de 0.8 se consideran buenos en la mayoría de aplicaciones aplicadas.",
             "Un AUC alto no garantiza buena calibración ni buena bondad de ajuste: un modelo puede discriminar bien y aun así sobreestimar o subestimar las probabilidades reales."
         ),
         multicollinearityGuide = c(
             "La multicolinealidad evalúa si los predictores contienen información muy redundante.",
             "No suele sesgar las predicciones, pero puede volver inestables los coeficientes, ampliar errores estándar y dificultar la interpretación individual de cada predictor.",
             "VIF cercanos a 1 sugieren baja colinealidad.",
             "Valores altos indican que un predictor se explica demasiado bien por otros predictores.",
             "Si hay colinealidad importante, considere combinar variables, eliminar predictores redundantes, centrar variables o cambiar la pregunta analítica."
         ),
         correlationMatrixGuide = c(
             "Estas dos tablas complementan las pruebas de multicolinealidad con una vista general de la asociación entre la variable dependiente (codificada 0/1) y todos los predictores numéricos, cada una en formato APA 7 (triángulo inferior, variables numeradas).",
             "La primera reporta la correlación de Pearson convencional (equivalente a una correlación biserial-puntual con una variable binaria; solo detecta asociación lineal); la segunda reporta la correlación de distancia (Székely et al., 2007), que detecta asociación lineal y no lineal por igual.",
             "La tabla de discordancia señala los pares donde dCor supera notablemente a |r| de Pearson, con la entropía copular (copent) como segunda línea de evidencia."
         ),
         influenceGuide = c(
             "Los diagnósticos de influencia identifican casos que pueden cambiar sustancialmente los coeficientes o el ajuste del modelo.",
             "En regresión logística, la D de Cook y el leverage se calculan en la escala de trabajo del modelo (residuos de Pearson ponderados), por lo que son análogos aproximados, no equivalentes idénticos, de sus versiones en regresión lineal (Pregibon, 1981, introdujo estas extensiones y la matriz hat ponderada para modelos lineales generalizados).",
             "Un caso influyente no debe eliminarse automáticamente.",
             "Revise si se trata de un error de registro, un caso válido pero extremo, o una señal de que el modelo no representa bien a todos los subgrupos."
         ),
         oddsRatiosGuide = c(
             "El odds ratio (OR) es el coeficiente exponenciado; representa cuánto se multiplican los momios del evento por cada unidad de aumento en el predictor (o por pertenecer a una categoría frente a la de referencia).",
             "Un OR de 1 indica ausencia de efecto, valores mayores a 1 indican mayor probabilidad del evento, y valores menores a 1 indican menor probabilidad.",
             "El intervalo de confianza del OR es más informativo que el valor p aislado: si incluye 1, el efecto no es estadísticamente distinguible de la ausencia de asociación.",
             "Error común: interpretar el OR como si fuera un riesgo relativo.",
             "Ambos coinciden solo cuando el evento es poco frecuente; con eventos frecuentes (más del 10%), el OR exagera sistemáticamente el efecto relativo real (Zhang & Yu, 1998)."
         )
     ),
     # ---- path: pathCheck module strings ----
     # ES: path: cadenas del módulo pathCheck
     path = list(
         title = "Análisis de Ruta",
         outlierAnalysisGuide = c(
             "Antes de interpretar cualquier coeficiente conviene revisar si algunos casos se apartan de forma inusual del resto de la muestra, ya que el análisis de ruta se basa en mínimos cuadrados ordinarios, sensibles a valores extremos.",
             "La distancia de Mahalanobis (D²) evalúa qué tan atípico es cada caso considerando todas las variables del modelo en conjunto (multivariado); se marca como atípico cuando D² supera el percentil 97.5 de una distribución ji-cuadrado con grados de libertad igual al número de variables del modelo (Mahalanobis, 1936).",
             "El leverage y la distancia de Cook se calculan sobre la ecuación con más predictores del modelo (la más completa), como referencia práctica más exigente de influencia dentro del sistema de ecuaciones.",
             "El leverage marca un caso cuando supera 2p/n (p = parámetros estimados, incluyendo el intercepto; n = casos), y la distancia de Cook cuando supera 4/n.",
             "Ningún criterio por sí solo es definitivo: un caso puede tener leverage alto sin ser influyente (si su valor observado coincide con lo esperado), o viceversa.",
             "Error común: eliminar automáticamente cualquier caso marcado.",
             "Revise primero si se trata de errores de digitación, casos válidos pero extremos, u observaciones sustantivamente importantes para la teoría antes de decidir excluirlas."
         ),
         designGuide = c(
             "El análisis de ruta clásico (Wright, 1934; Duncan, 1966) descompone un sistema de relaciones causales hipotetizadas entre variables observadas en una serie de ecuaciones de regresión, una por cada variable endógena (con al menos una flecha entrante).",
             "Antes de interpretar los coeficientes, revise que la razón de casos por relación de ruta sea suficiente: como referencia práctica general, al menos 10 casos por parámetro estimado (Bentler & Chou, 1987).",
             "Este enfoque asume que el modelo es recursivo: no hay ciclos de retroalimentación (por ejemplo, A causa B y B causa A al mismo tiempo)."
         ),
         equationsGuide = c(
             "Cada variable endógena se predice a partir de las variables que apuntan hacia ella en el diagrama de ruta, ajustada como una regresión lineal independiente.",
             "Como todas las variables se estandarizan antes de ajustar, cada coeficiente es directamente un coeficiente de ruta estandarizado en el sentido clásico de Wright (1934): representa el cambio esperado en la variable dependiente, en desviaciones estándar, por cada aumento de una desviación estándar en el predictor.",
             "Los coeficientes de ruta estandarizados permiten comparar la importancia relativa de distintos predictores dentro de la misma ecuación, algo que los coeficientes sin estandarizar no permiten cuando las variables tienen escalas distintas."
         ),
         normalityGuide = c(
             "La estimación por mínimos cuadrados ordinarios de cada ecuación no exige que las variables observadas sean normales, sino que los residuos de cada ecuación lo sean razonablemente, sobre todo para que las pruebas de significancia de los coeficientes de ruta sean confiables.",
             "Se reportan dos pruebas complementarias por ecuación: Shapiro-Wilk, generalmente la más potente en muestras pequeñas a moderadas, y Anderson-Darling (Anderson & Darling, 1952), que pondera más las colas de la distribución (Razali & Wah, 2011).",
             "Con muestras muy grandes, incluso desviaciones pequeñas y sin relevancia práctica pueden resultar estadísticamente significativas; conviene revisar también el gráfico Q-Q o el histograma de residuos antes de concluir."
         ),
         multivariateNormalityGuide = c(
             "Este diagnóstico complementario evalúa la normalidad multivariada del conjunto de variables del modelo mediante las pruebas de asimetría y curtosis de Mardia (1970).",
             "El análisis de ruta por ecuaciones OLS separadas no exige normalidad multivariada para ser válido; este chequeo es más relevante si en el futuro se plantea el mismo modelo con un estimador conjunto (por ejemplo, SEM basado en máxima verosimilitud)."
         ),
         correlationMatrixGuide = c(
             "Estas dos tablas complementan las pruebas de multicolinealidad dentro de cada ecuación con una vista general de la asociación entre todas las variables del modelo, cada una en formato APA 7 (triángulo inferior, variables numeradas).",
             "La primera reporta la correlación de Pearson convencional (solo detecta asociación lineal); la segunda reporta la correlación de distancia (dCor, Székely et al., 2007), que detecta asociación lineal y no lineal por igual.",
             "Una brecha grande entre dCor y el valor absoluto de Pearson para un mismo par sugiere que la relación entre esas dos variables podría no ser lineal, lo cual es relevante porque el análisis de ruta asume relaciones lineales entre las variables."
         ),
         homoscedasticityGuide = c(
             "La homoscedasticidad supone que la varianza de los residuos de cada ecuación es aproximadamente constante a lo largo de los valores predichos.",
             "Se reportan dos pruebas por ecuación: Breusch y Pagan (1979), que asume que la varianza del error es una función lineal de los predictores, y White (1980), más general, que también captura curvatura e interacciones en la varianza a costa de menos grados de libertad.",
             "La heteroscedasticidad no sesga los coeficientes de ruta, pero sí sus errores estándar, lo que puede llevar a conclusiones equivocadas sobre qué rutas son estadísticamente significativas."
         ),
         multicollinearityGuide = c(
             "La multicolinealidad evalúa si los predictores de una misma ecuación contienen información muy redundante entre sí.",
             "El factor de inflación de varianza (VIF) resume esta redundancia; valores por encima de 5 encienden una alerta moderada y por encima de 10 se consideran un problema severo (Marquardt, 1970).",
             "Solo aplica a ecuaciones con dos o más predictores; no tiene sentido para una variable endógena con un solo predictor directo."
         ),
         indirectEffectsGuide = c(
             "Cuando una variable influye sobre otra tanto de forma directa como a través de una o más variables mediadoras, el efecto total se descompone en efecto directo más efecto indirecto, siguiendo la regla de trazado de rutas de Wright (1934): el efecto indirecto de cada cadena mediadora es el producto de los coeficientes de ruta a lo largo de esa cadena.",
             "Baron y Kenny (1986) popularizaron el marco clásico para distinguir mediación total de mediación parcial en ciencias sociales y psicología.",
             "La prueba z de Sobel (1982) ofrece una forma analítica de evaluar la significancia de un efecto indirecto simple (un solo mediador), aunque hoy se prefieren los intervalos de confianza bootstrap por ser menos sensibles a la falta de normalidad del producto de dos coeficientes."
         ),
         crossEntropyGuide = c(
             "La entropía copular (CE) es una medida de dependencia basada en la cópula de las variables (Ma & Sun, 2011), que captura la estructura de dependencia entre variables con independencia de sus distribuciones marginales, sin asumir ninguna forma funcional particular (lineal, monótona, etc.).",
             "A diferencia de la r de Pearson (que solo detecta asociación lineal) o incluso dCor (que detecta cualquier dependencia pero se basa en distancias), CE está matemáticamente vinculada a la información mutua entre las dos variables, medida en bits de información (entropía de Shannon).",
             "Un valor p significativo (p < .05) significa que la entropía copular observada es mayor de lo esperado bajo independencia, sugiriendo que las variables comparten información más allá de lo que ocurriría por azar.",
             "Un valor p no significativo significa que los datos son compatibles con que las variables sean estadísticamente independientes.",
             "Error común: interpretar una prueba de CE no significativa como prueba de que dos variables están completamente no relacionadas.",
             "Como todas las pruebas estadísticas, no rechazar independencia no prueba independencia - solo significa que los datos no proporcionan evidencia fuerte en contra.",
             "Con muestras pequeñas, la prueba puede carecer de poder para detectar dependencias reales pero débiles.",
             "Matiz de tamaño de muestra: el estimador de entropía copular usado por copent() (basado en k-vecinos más cercanos de Kozachenko-Leonenko) requiere un tamaño de muestra suficiente para producir estimaciones estables.",
             "Con muestras muy pequeñas (n < 20), los valores p pueden ser poco confiables.",
             "Con muestras muy grandes, incluso dependencias triviales pueden volverse estadísticamente significativas.",
             "Uso práctico en análisis de ruta: si dos variables que no están conectadas directamente en su modelo muestran dependencia significativa por entropía copular, esto puede sugerir una ruta omitida o una causa común no incluida en el modelo.",
             "Sin embargo, no agregue rutas basándose solo en este diagnóstico - la justificación teórica siempre debe venir primero.",
             "Referencia: la entropía copular fue definida formalmente por Ma y Sun (2011); su estimación usa el método de k-vecinos más cercanos de Kozachenko y Leonenko (1987).",
             "El paquete R copent implementa este estimador para pruebas de independencia.",
             "Las referencias completas están disponibles en la herramienta de Bibliografía."
         ),
         crossEntropyInterpretation = c(
             "La entropía copular complementa las matrices de Pearson y dCor con una tercera",
             "perspectiva sobre la dependencia: la teoría de la información. Mientras Pearson mide",
             "asociación lineal y dCor mide dependencia basada en distancias, la entropía copular",
             "mide cuánta información comparten dos variables independientemente de la forma de su",
             "relación.",
             "Cuando los tres diagnósticos coinciden (Pearson bajo, dCor bajo, CE no significativa),",
             "la evidencia de independencia entre ese par de variables es sólida. Cuando discrepan -",
             "por ejemplo, Pearson bajo pero CE significativa - esto sugiere una dependencia no",
             "lineal que Pearson no captura pero que la teoría de la información sí detecta.",
             "Error común: usar esta tabla para diseñar el modelo al revés, agregando rutas basadas",
             "en los valores de CE más altos. Las relaciones de ruta deben venir de la teoría, no",
             "descubrirse a posteriori a partir de los datos - de lo contrario el modelo capitaliza",
             "el azar y no se replicará.",
             "Si el paquete 'copent' no está instalado, esta sección no puede calcularse. Instálelo",
             "con install.packages('copent') para habilitar este diagnóstico avanzado."
         )
     ),
     # ---- library: assumptionLibrary module strings ----
     # ES: library: cadenas del módulo assumptionLibrary
     library = list(
         title = "Assumption Library",
         intro = c(
             "Assumption Library",
             "",
             "Esta biblioteca resume los supuestos y pruebas usados en AssumptionsLab.",
             "Su propósito no es analizar datos directamente, sino servir como guía",
             "metodológica para interpretar las pruebas dentro de cada análisis.",
             "",
             "Principio central:",
             "La biblioteca explica qué significa cada supuesto y cuándo se usa.",
             "Cada análisis interpreta después los resultados con los datos reales del usuario.",
             "",
             "Estilo de citación: APA 7.ª edición.",
             "",
             "Regla general para p-valores:",
             "p < .05 indica una desviación estadísticamente significativa respecto",
             "al supuesto evaluado.",
             "p >= .05 indica que el resultado es compatible con el supuesto evaluado",
             "de forma aproximada.",
             "",
             "Importante:",
             "Compatible no significa demostrado. Significa que, con estos datos y esta",
             "prueba, no se observa una desviación estadísticamente significativa.",
             "",
             "Evite interpretar las pruebas de supuestos de forma mecánica. La decisión",
             "debe combinar p-valores, tamaño muestral, gráficos, diseño y criterio sustantivo."
         ),
         normalityPart1 = c(
             "Normalidad",
             "",
             "Qué evalúa:",
             "La normalidad evalúa si una distribución se aproxima razonablemente a una",
             "distribución normal. En análisis estadístico aplicado, no siempre se evalúa",
             "la variable original.",
             "",
             "Dónde se usa en AssumptionsLab:",
             "Independent Groups: normalidad por grupos o de residuos según el contexto.",
             "Related Groups: normalidad de diferencias pareadas o residuos intra-sujeto.",
             "ANOVA/ANCOVA: normalidad de residuos del modelo.",
             "Regression: normalidad de residuos del modelo.",
             "",
             "Cómo se interpreta la batería:",
             "Shapiro-Wilk se usa como prueba principal por ser la más potente en muestras",
             "pequeñas o moderadas; las demás se reportan como evidencia secundaria. La",
             "interpretación integra ambas: cuántas de las pruebas secundarias coinciden",
             "con la conclusión de Shapiro-Wilk, no solo si Shapiro-Wilk es significativa",
             "por sí sola."
         ),
         normalityTableHeaders = c("Prueba", "Qué contrasta", "Uso típico", "Principal limitación"),
         normalityTableRows = list(
             c("Shapiro-Wilk", "Normalidad general, prueba principal de la batería",
               "Muestra pequeña o moderada; es la prueba de referencia por defecto",
               "En R solo se calcula hasta 5000 observaciones (Shapiro & Wilk, 1965)"),
             c("Lilliefors", "Normalidad mediante K-S con valores críticos corregidos",
               "Cuando se necesita una alternativa a Shapiro-Wilk basada en la distribución acumulada",
               "El K-S clásico (sin corregir) no es válido aquí; siempre se usa la versión corregida (Lilliefors, 1967)"),
             c("Anderson-Darling", "Normalidad con mayor peso en las colas",
               "Cuando preocupan colas pesadas o valores extremos",
               "Puede ser demasiado sensible a desviaciones triviales en las colas con muestras muy grandes"),
             c("Cramér-von Mises", "Normalidad comparando toda la distribución acumulada",
               "Cuando se sospechan desviaciones distribuidas en el cuerpo central, no en un punto",
               "Menos utilizada y menos familiar que Shapiro-Wilk o Anderson-Darling en la práctica aplicada"),
             c("Shapiro-Francia", "Variante de Shapiro-Wilk con cálculo distinto",
               "Muestras moderadas o grandes, como evidencia complementaria a Shapiro-Wilk",
               "Su ventaja práctica sobre Shapiro-Wilk es marginal en muchos escenarios (Shapiro & Francia, 1972)"),
             c("Pearson chi-cuadrado", "Normalidad comparando frecuencias observadas y esperadas por intervalos",
               "Como evidencia secundaria adicional, no como prueba principal",
               "Depende de cómo se agrupan los datos en intervalos; suele ser menos potente que pruebas basadas en la distribución completa"),
             c("Jarque-Bera", "Normalidad evaluando conjuntamente asimetría y curtosis",
               "Muestras moderadas o grandes", "Menos estable e informativa en muestras muy pequeñas (Jarque & Bera, 1987)"),
             c("Skewness test", "Solo asimetría de la distribución",
               "Diagnóstico puntual sobre la dirección de la asimetría (cola larga a la izquierda o a la derecha)",
               "Evalúa un solo aspecto de la forma; no sustituye a una prueba de normalidad general"),
             c("Kurtosis test", "Solo apuntamiento y peso de colas",
               "Diagnóstico puntual sobre colas pesadas, livianas o concentración distinta a la normal",
               "Evalúa un solo aspecto de la forma; no sustituye a una prueba de normalidad general")
         ),
         normalityPart2 = c(
             "",
             "",
             "Shapiro-Wilk:",
             "Prueba clásica y muy usada para normalidad (Shapiro & Wilk, 1965). Requiere",
             "al menos 3 casos válidos. En R se calcula hasta 5000 observaciones. Es",
             "especialmente potente en muestras pequeñas o moderadas frente a otras",
             "pruebas de esta batería (Razali & Wah, 2011).",
             "",
             "Lilliefors (Kolmogorov-Smirnov corregido):",
             "Compara la distribución observada con una distribución teórica, pero corrige",
             "los valores críticos porque la media y la desviación se estiman de los mismos",
             "datos (Lilliefors, 1967). El Kolmogorov-Smirnov clásico (sin esta corrección)",
             "asume parámetros conocidos de antemano y no es válido en este contexto; por",
             "eso este módulo siempre usa la versión corregida de Lilliefors, no el K-S",
             "clásico.",
             "",
             "Anderson-Darling:",
             "Da más peso a las colas de la distribución (Anderson & Darling, 1952). Es",
             "útil cuando preocupan colas pesadas, valores extremos o desviaciones en los",
             "extremos de la distribución.",
             "",
             "Cramér-von Mises:",
             "Compara toda la distribución acumulada observada contra la teórica (Anderson",
             "& Darling, 1952). Es más sensible que Kolmogorov-Smirnov a desviaciones",
             "distribuidas en el cuerpo central de la distribución, no solo en un punto.",
             "",
             "Shapiro-Francia:",
             "Variante de Shapiro-Wilk basada en los mismos principios, con un cálculo",
             "distinto que puede comportarse mejor en algunas muestras moderadas o grandes",
             "(Shapiro & Francia, 1972).",
             "",
             "Pearson chi-cuadrado:",
             "Compara frecuencias observadas y esperadas por intervalos. Depende de cómo",
             "se agrupan los datos en intervalos, por lo que suele ser menos potente que",
             "las pruebas basadas en la distribución completa.",
             "",
             "Jarque-Bera:",
             "Evalúa conjuntamente asimetría y curtosis (Jarque & Bera, 1987). Es más",
             "informativa en muestras moderadas o grandes, y menos estable en muestras muy",
             "pequeñas.",
             "",
             "Skewness test:",
             "Evalúa asimetría. Asimetría positiva sugiere cola larga a la derecha;",
             "asimetría negativa sugiere cola larga a la izquierda.",
             "",
             "Kurtosis test:",
             "Evalúa apuntamiento y peso de colas. Puede indicar colas pesadas, colas",
             "livianas o concentración distinta a la esperada bajo normalidad.",
             "",
             "Criterios muestrales:",
             "n < 10: interpretación muy inestable.",
             "10 <= n < 30: muestra pequeña; baja potencia para detectar desviaciones.",
             "30 <= n <= 200: muestra moderada; las pruebas son diagnósticos útiles.",
             "n > 200: pequeñas desviaciones pueden producir p-valores significativos.",
             "n > 5000: Shapiro-Wilk no se calcula en R.",
             "",
             "Interpretación:",
             "p < .05 sugiere una desviación estadísticamente significativa respecto",
             "a una distribución normal.",
             "p >= .05 indica que el resultado es compatible con una distribución",
             "aproximadamente normal.",
             "",
             "Decisión metodológica:",
             "Combine pruebas, gráficos, asimetría, curtosis, tamaño muestral y casos",
             "atípicos. Si varias pruebas sugieren no normalidad, considere transformación,",
             "bootstrap, métodos robustos o alternativas no paramétricas."
         ),
         homoscedasticityPart1 = c(
             "Homocedasticidad / Homogeneidad de varianzas",
             "",
             "Qué evalúa:",
             "Evalúa si la variabilidad es aproximadamente constante. En comparaciones de",
             "grupos se habla de homogeneidad de varianzas (Levene, 1960); en regresión se",
             "habla de homocedasticidad de residuos.",
             "",
             "Dónde se usa en AssumptionsLab:",
             "Independent Groups: igualdad de varianzas entre grupos.",
             "ANOVA/ANCOVA: igualdad de varianzas entre celdas o grupos.",
             "Regression: varianza residual constante a lo largo de los valores ajustados.",
             "",
             "Por qué importa:",
             "Cuando las varianzas de los grupos son muy distintas, el error estándar del",
             "estadístico de prueba deja de reflejar correctamente la variabilidad real,",
             "y los intervalos de confianza y valores p pueden volverse poco fiables —",
             "sobre todo con tamaños de grupo desiguales (Welch, 1947). En regresión, la",
             "heterocedasticidad no sesga los coeficientes OLS, pero sí sus errores",
             "estándar, afectando los tests t y F sobre esos coeficientes.",
             "",
             "Dos familias de pruebas:",
             "Las pruebas de grupos (Levene, Brown-Forsythe, Bartlett, Fligner-Killeen,",
             "Hartley Fmax) comparan dispersión entre k grupos definidos de antemano. Las",
             "pruebas de regresión (Breusch-Pagan, White, Goldfeld-Quandt, Spearman entre",
             "residuos y ajustados) evalúan si la varianza residual cambia sistemáticamente",
             "con los valores predichos o con los predictores, en vez de comparar grupos",
             "discretos."
         ),
         homoscedasticityGroupTableHeaders = c("Prueba", "Qué contrasta", "Uso típico", "Principal limitación"),
         homoscedasticityGroupTableRows = list(
             c("Levene",
               "Igualdad de varianzas usando desviaciones absolutas respecto a la media de cada grupo",
               "Prueba general y ampliamente usada como primer chequeo",
               "Basada en la media; menos robusta que Brown-Forsythe ante asimetría o colas pesadas"),
             c("Brown-Forsythe",
               "Igualdad de varianzas usando desviaciones absolutas respecto a la mediana de cada grupo",
               "Alternativa preferida a Levene cuando hay asimetría, colas pesadas o casos atípicos",
               "Ligera pérdida de potencia frente a Levene si los datos son realmente normales"),
             c("Bartlett",
               "Igualdad de varianzas asumiendo normalidad dentro de cada grupo",
               "Cuando la normalidad por grupo es razonable y se busca la prueba más potente",
               "Muy sensible a no normalidad; puede marcar heterogeneidad que en realidad es no normalidad"),
             c("Fligner-Killeen",
               "Igualdad de varianzas mediante un test de rangos sobre las desviaciones",
               "Datos no normales, con atípicos, o cuando se busca la alternativa más robusta",
               "Menor potencia que Levene o Bartlett cuando los datos sí son aproximadamente normales"),
             c("Hartley Fmax",
               "Razón entre la varianza más grande y la más pequeña de los grupos",
               "Indicador descriptivo rápido, sobre todo en diseños balanceados",
               "No es un contraste formal con distribución bien definida fuera de diseños balanceados")
         ),
         homoscedasticityPart2 = c(
             "",
             "",
             "Levene — qué evalúa y cuándo usarlo:",
             "Compara la dispersión entre grupos usando las desviaciones absolutas de cada",
             "observación respecto a la media de su grupo, y aplica un ANOVA de un factor a",
             "esas desviaciones (Levene, 1960). H0: las k varianzas poblacionales son",
             "iguales. H1: al menos una difiere. Es razonable como primer chequeo general,",
             "especialmente con muestras grandes y datos aproximadamente simétricos.",
             "",
             "Brown-Forsythe — qué evalúa y cuándo usarlo:",
             "Es la misma lógica de Levene pero centrando cada grupo en su mediana en vez",
             "de su media (Brown & Forsythe, 1974). Al usar la mediana, es notablemente",
             "más robusta ante asimetría, colas pesadas y casos atípicos. En la práctica,",
             "cuando hay dudas sobre la forma de la distribución dentro de cada grupo,",
             "conviene preferir Brown-Forsythe sobre Levene por defecto.",
             "",
             "Bartlett — qué evalúa y cuándo usarlo:",
             "Contrasta igualdad de varianzas mediante una razón de verosimilitud que",
             "asume normalidad dentro de cada grupo (Bartlett, 1937). Es la prueba más potente de esta",
             "familia cuando esa normalidad es razonable, pero pierde validez rápidamente",
             "si no lo es: una desviación de normalidad puede hacer que Bartlett rechace",
             "igualdad de varianzas aunque las varianzas reales sean iguales. No debe",
             "usarse como única base de decisión si los datos no son normales — conviene",
             "revisar primero la sección de Normalidad de esta misma biblioteca.",
             "",
             "Fligner-Killeen — qué evalúa y cuándo usarlo:",
             "Es una prueba de rangos, no paramétrica, sobre las desviaciones absolutas",
             "respecto a la mediana (Fligner & Killeen, 1976). No asume ninguna forma",
             "distribucional particular, lo que la hace la opción más robusta de esta",
             "familia frente a no normalidad y atípicos, a costa de algo de potencia",
             "cuando los datos sí son razonablemente normales.",
             "",
             "Hartley Fmax — qué evalúa y limitaciones:",
             "Es simplemente la razón entre la varianza muestral mayor y la menor entre",
             "los k grupos (Hartley, 1950). Es fácil de calcular e interpretar, pero sus",
             "valores críticos tabulados solo son estrictamente válidos con diseños",
             "balanceados (mismo n por grupo) y normalidad; por eso se usa más como",
             "indicador descriptivo complementario que como prueba formal principal.",
             "",
             "Interpretación (pruebas de grupos):",
             "p < .05 sugiere diferencias estadísticamente significativas entre las",
             "varianzas de los grupos. p >= .05 indica que el resultado es compatible con",
             "varianzas aproximadamente homogéneas."
         ),
         homoscedasticityRegTableHeaders = c("Prueba", "Qué contrasta", "Uso típico", "Principal limitación"),
         homoscedasticityRegTableRows = list(
             c("Breusch-Pagan",
               "Si la varianza residual depende linealmente de los predictores o valores ajustados",
               "Chequeo estándar de heterocedasticidad en regresión lineal",
               "Asume que la forma de la heterocedasticidad, si existe, es aproximadamente lineal en los predictores"),
             c("White",
               "Heterocedasticidad general, incluyendo patrones no lineales y de interacción",
               "Cuando se sospecha una forma de heterocedasticidad más compleja que la lineal",
               "Menos potente que Breusch-Pagan cuando la heterocedasticidad real sí es simple y lineal"),
             c("Goldfeld-Quandt",
               "Diferencia de varianza residual entre dos zonas ordenadas del modelo",
               "Cuando se sospecha un punto de quiebre claro en la varianza a lo largo de una variable ordenadora",
               "Requiere elegir la variable de ordenamiento y omitir una franja central de datos"),
             c("Spearman |residuos| vs ajustados",
               "Si la magnitud de los residuos crece o decrece de forma monótona con los valores ajustados",
               "Diagnóstico exploratorio, no paramétrico y fácil de interpretar",
               "Solo capta relaciones monótonas; no es un contraste formal en el sentido de las tres anteriores")
         ),
         homoscedasticityPart3 = c(
             "",
             "",
             "Breusch-Pagan — qué evalúa y cuándo usarlo:",
             "Regresa los residuos al cuadrado sobre los predictores del modelo original y",
             "contrasta si esa regresión auxiliar explica una proporción de varianza",
             "significativamente mayor que cero (Breusch & Pagan, 1979). H0: la varianza",
             "residual es constante (homocedasticidad). H1: la varianza residual depende de",
             "los predictores. Es el chequeo por defecto para regresión lineal cuando se",
             "sospecha una relación aproximadamente lineal entre varianza y predictores.",
             "",
             "White — qué evalúa y cuándo usarlo:",
             "Generaliza Breusch-Pagan agregando a la regresión auxiliar los cuadrados y",
             "productos cruzados de los predictores, sin asumir una forma funcional",
             "específica para la heterocedasticidad (White, 1980). Es más general y más",
             "conveniente cuando se sospecha que la varianza residual depende de los",
             "predictores de forma no lineal o a través de interacciones, pero pierde algo",
             "de potencia frente a Breusch-Pagan cuando la heterocedasticidad real es",
             "simplemente lineal.",
             "",
             "Goldfeld-Quandt — qué evalúa y cuándo usarlo:",
             "Ordena las observaciones según una variable (típicamente un predictor o los",
               "valores ajustados), omite una franja central, y compara la suma de cuadrados",
             "residual entre el subgrupo con valores bajos y el subgrupo con valores altos",
             "mediante un test F (Goldfeld & Quandt, 1965). Es especialmente útil cuando se",
             "sospecha que la varianza cambia de forma escalonada o marcada en torno a un",
             "punto identificable de la variable ordenadora, más que de forma suave y",
             "continua.",
             "",
             "Spearman |residuos| vs ajustados — qué evalúa y cuándo usarlo:",
             "Calcula la correlación de rangos de Spearman entre el valor absoluto de los",
             "residuos y los valores ajustados. Es exploratorio y no paramétrico: no asume",
             "una forma funcional concreta, solo detecta si la magnitud del error crece o",
             "decrece de forma monótona con el valor predicho. Es un buen complemento",
             "visual/numérico a Breusch-Pagan y White, no un sustituto formal.",
             "",
             "Interpretación (pruebas de regresión):",
             "p < .05 sugiere que la varianza residual no es constante (heterocedasticidad).",
             "p >= .05 indica que el resultado es compatible con varianza residual",
             "aproximadamente constante.",
             "",
             "Decisión metodológica:",
             "En comparaciones de grupos, si hay evidencia de heterogeneidad de varianzas,",
             "considere la corrección de Welch (Welch, 1947) para el estadístico t o F, la",
             "propia prueba de Brown-Forsythe como alternativa a la ANOVA clásica, o",
             "métodos robustos. En regresión, considere errores estándar robustos a",
             "heterocedasticidad tipo HC3 (MacKinnon & White, 1985), una transformación de",
             "la variable dependiente, o un modelo que module explícitamente la varianza."
         ),

         linearityPart1 = c(
             "Linealidad",
             "",
             "Qué evalúa:",
             "Evalúa si la relación entre predictores numéricos o covariables y la variable",
             "dependiente puede representarse razonablemente con una línea recta.",
             "",
             "Dónde se usa en AssumptionsLab:",
             "Regression: predictores numéricos y ajuste global del modelo.",
             "ANOVA/ANCOVA: covariables numéricas.",
             "Path/Related/Logistic: dCor y entropía copular como diagnóstico de dependencia no lineal complementario a Pearson.",
             "",
             "No aplica del mismo modo a factores categóricos, porque estos representan",
             "grupos o niveles, no relaciones lineales continuas."
         ),
         linearityTableHeaders = c("Diagnóstico", "Qué evalúa", "Uso típico", "Principal limitación"),
         linearityTableRows = list(
             c("Correlación bivariada", "Asociación lineal simple entre dos variables",
               "Vistazo exploratorio inicial", "Solo detecta asociación lineal; no sustituye el diagnóstico del modelo completo"),
             c("dCor / entropía copular", "Dependencia lineal y no lineal, sin asumir forma funcional",
               "Complemento exploratorio a Pearson cuando se sospecha relación no lineal",
               "Una brecha dCor-Pearson es señal, no prueba; requiere confirmarse con un diagrama de dispersión"),
             c("Término cuadrático", "Curvatura de un predictor específico frente al modelo original",
               "Chequeo rápido, un predictor a la vez", "No reemplaza a RESET, que evalúa el modelo completo"),
             c("Box-Tidwell", "Si un predictor específico necesita una transformación de potencia",
               "Prueba de referencia para linealidad en el logit (regresión logística)",
               "Evalúa un predictor a la vez; requiere x > 0; no evalúa curvatura conjunta (Box & Tidwell, 1962)"),
             c("Ramsey RESET", "Mala especificación funcional del modelo en su conjunto",
               "Chequeo global tras revisar los predictores individuales",
               "Indica que algo está mal, pero no qué (curvatura, interacción omitida, outlier) (Ramsey, 1969)"),
             c("Rainbow test", "Si el ajuste lineal es estable a lo largo de todo el rango de valores ajustados",
               "Complemento a RESET cuando la no linealidad podría concentrarse en los extremos",
               "Requiere una muestra razonable (al menos 20 casos en este módulo) para el submodelo central")
         ),
         linearityPart2 = c(
             "",
             "",
             "Correlación bivariada:",
             "Describe asociación lineal simple entre dos variables. Es exploratoria y no",
             "sustituye el diagnóstico del modelo completo.",
             "",
             "Correlación de distancia (dCor) y entropía copular:",
             "La r de Pearson solo detecta asociación lineal. dCor (Székely et al.,",
             "2007) detecta asociación lineal y no lineal por igual, sin asumir",
             "una forma funcional. La entropía copular (Ma & Sun, 2011) es una medida de",
             "dependencia adicional, libre de supuestos distribucionales, basada en la",
             "cópula de las variables. Un par con dCor notablemente mayor que su |r| de",
             "Pearson (brecha > .10) es una señal, no una prueba, de una relación no",
             "lineal que un modelo lineal podría estar pasando por alto; conviene",
             "confirmarlo con un diagrama de dispersión antes de concluir no linealidad.",
             "El p-valor de dCor y de la entropía copular se calcula por permutación,",
             "así que su resolución depende del número de permutaciones usado.",
             "",
             "Término cuadrático exploratorio:",
             "Agrega x² al modelo como predictor adicional y compara contra el modelo",
             "original. p < .05 en el coeficiente de x² sugiere posible curvatura no",
             "capturada por la relación lineal. Es un chequeo rápido y de un solo",
             "predictor a la vez; no reemplaza a RESET, que evalúa el modelo completo.",
             "",
             "Box-Tidwell — qué evalúa:",
             "Evalúa si un predictor numérico específico necesitaría una transformación",
             "de potencia (una forma de tipo Xᵏ) para relacionarse linealmente con la",
             "variable dependiente o, en regresión logística, con el logit. El",
             "mecanismo: se agrega al modelo un término interacción x × ln(x) además del",
             "propio x, y se prueba si el coeficiente de ese término es significativo.",
             "H0: no hace falta transformar x (la relación ya es aproximadamente lineal",
             "en la escala actual). H1: la relación mejoraría con una transformación de",
             "potencia de x. Requiere x > 0, porque el término usa ln(x); no puede",
             "aplicarse a predictores con valores negativos o iguales a cero sin",
             "desplazarlos primero.",
             "",
             "Box-Tidwell — cuándo usarlo:",
             "Es la prueba de referencia para evaluar linealidad en el logit de un",
             "predictor numérico continuo en regresión logística — a diferencia del",
             "término cuadrático o de un diagrama de dispersión, evalúa directamente la",
             "escala correcta del predictor (log, raíz, lineal) en vez de solo detectar",
             "curvatura. También es útil en regresión lineal cuando se sospecha que un",
             "predictor específico, no el modelo completo, es el origen de la",
             "no linealidad.",
             "",
             "Box-Tidwell — limitaciones:",
             "Evalúa un predictor a la vez, así que no dice nada sobre curvatura",
             "conjunta entre varios predictores ni sobre mala especificación general del",
             "modelo (para eso está Ramsey RESET). Con 0 de varios predictores",
             "significativos, el resultado correcto es \"no se detectó evidencia",
             "estadística de desviación de linealidad\", no \"se confirma linealidad\" —",
             "la prueba puede simplemente carecer de poder en la muestra disponible.",
             "",
             "Ramsey RESET — qué evalúa:",
             "Evalúa si el modelo, en su conjunto, tiene una mala especificación",
             "funcional — es decir, si le falta capturar alguna forma sistemática",
             "(curvatura, interacciones, términos omitidos) que un modelo estrictamente",
             "lineal en los predictores actuales no puede representar. El mecanismo:",
             "se agregan al modelo potencias de los valores ajustados (típicamente el",
             "cuadrado y el cubo) y se contrasta, mediante un test F, si esas potencias",
             "mejoran significativamente el ajuste frente al modelo original. H0: el",
             "modelo original está correctamente especificado (las potencias no aportan",
             "explicación adicional). H1: el modelo omite alguna forma funcional",
             "relevante.",
             "",
             "Ramsey RESET — cuándo usarlo:",
             "Como chequeo global después de revisar los predictores individuales: si",
             "Box-Tidwell o el término cuadrático no marcan ningún predictor en",
             "particular pero el ajuste global se ve deficiente, RESET puede detectar",
             "una no linealidad que solo aparece en combinación de predictores, o en",
             "una forma (por ejemplo una interacción) que ningún chequeo por-predictor",
             "captura por separado. Es la prueba más general de las tres.",
             "",
             "Ramsey RESET — limitaciones:",
             "Un RESET significativo indica que algo en la forma funcional está mal,",
             "pero no dice qué: no distingue entre curvatura genuina, una interacción",
             "omitida, una variable omitida correlacionada con los predictores, o un",
             "outlier influyente. Conviene revisar los gráficos de residuos y las demás",
             "pruebas de linealidad antes de decidir qué corrección aplicar.",
             "",
             "Rainbow test — qué evalúa:",
             "Evalúa si el ajuste lineal es estable a lo largo del rango de los valores",
             "ajustados, en vez de concentrar la evidencia en una sola forma específica",
             "de no linealidad. El mecanismo: se ordenan los casos por su valor",
             "ajustado, se toma el tramo central (por defecto, alrededor del 50% de los",
             "casos con valores ajustados intermedios) y se reajusta el modelo solo con",
             "ese subconjunto central. Se compara, mediante un test F, la suma de",
             "cuadrados residual del modelo completo contra la del modelo ajustado solo",
             "al tramo central. H0: el ajuste lineal es igual de adecuado en el tramo",
             "central que en el conjunto completo. H1: el ajuste se deteriora al incluir",
             "los extremos, lo que sugiere que la relación no es uniformemente lineal en",
             "todo el rango de los predictores.",
             "",
             "Rainbow test — cuándo usarlo:",
             "Es útil como complemento a RESET cuando se sospecha que la no linealidad",
             "no es una curvatura suave y global, sino que la relación se comporta",
             "distinto en los extremos del rango de los predictores frente al centro —",
             "por ejemplo, efectos que se aplanan o se aceleran solo en valores extremos.",
             "Requiere una muestra razonable (en este módulo, al menos 20 casos) para",
             "que el submodelo central tenga suficientes grados de libertad.",
             "",
             "Común a RESET y Rainbow — error frecuente:",
             "Interpretar un RESET o Rainbow no significativo como prueba de linealidad",
             "perfecta. Ambas pruebas tienen poder limitado en muestras pequeñas (pueden",
             "no detectar curvatura real) y pueden marcar como significativa una",
             "curvatura trivial y sin relevancia práctica en muestras muy grandes.",
             "Léalas siempre junto con los gráficos de residuos vs. valores ajustados,",
             "no de forma aislada.",
             "",
             "Criterios muestrales:",
             "Con muestras pequeñas puede ser difícil detectar curvatura.",
             "Con muestras grandes, curvaturas pequeñas pueden resultar significativas.",
             "",
             "Interpretación:",
             "p < .05 en pruebas de curvatura o especificación sugiere una posible",
             "desviación de la linealidad.",
             "p >= .05 indica que el resultado es compatible con una relación",
             "aproximadamente lineal.",
             "",
             "Decisión metodológica:",
             "Considerar términos polinómicos, splines, transformaciones o modelos no",
             "lineales si hay evidencia de curvatura sustantiva."
         ),
         independencePart1 = c(
             "Independencia",
             "",
             "Qué evalúa:",
             "Evalúa si las observaciones o errores pueden considerarse independientes.",
             "Este supuesto depende principalmente del diseño del estudio.",
             "",
             "Dónde se usa en AssumptionsLab:",
             "ANOVA/ANCOVA: independencia entre unidades o sujetos.",
             "Regression: independencia de errores, especialmente en datos ordenados.",
             "Related Groups: dependencia dentro de unidad esperada, independencia entre unidades.",
             "",
             "Idea central:",
             "Las cuatro pruebas de esta sección se relacionan con la dependencia temporal",
             "de los residuos, pero no responden exactamente la misma pregunta ni se aplican",
             "al mismo objeto. En una regresión o serie temporal, normalmente interesa que",
             "los residuos sean aproximadamente ruido blanco, es decir, sin patrón temporal",
             "sistemático. Si hay autocorrelación residual, los coeficientes OLS pueden",
             "seguir siendo insesgados bajo ciertas condiciones, pero los errores estándar,",
             "los tests t, los intervalos de confianza y los tests F pueden dejar de ser",
             "confiables.",
             "",
             "La elección entre las cuatro depende principalmente de:",
             "Si se analizan residuos de una regresión o residuos de un modelo de series",
             "temporales tipo ARIMA/SARIMA.",
             "Si se sospecha autocorrelación solo en el rezago 1 o en varios rezagos a la vez.",
             "Si el modelo contiene la variable dependiente rezagada como regresor.",
             "Si se busca detectar autocorrelación lineal específicamente, o falta de",
             "aleatoriedad más general en la secuencia."
         ),
         independenceTableHeaders = c("Prueba", "Qué contrasta", "Uso típico", "Principal limitación"),
         independenceTableRows = list(
             c("Durbin-Watson",
               "Autocorrelación residual de orden 1",
               "Regresión lineal estática simple",
               "No es adecuada si el modelo incluye la dependiente rezagada como regresor; no evalúa bien rezagos superiores"),
             c("Breusch-Godfrey",
               "Autocorrelación residual hasta un orden p elegido",
               "Regresiones econométricas, dinámicas o con varios rezagos",
               "Hay que decidir el orden p"),
             c("Ljung-Box",
               "Autocorrelación conjunta en varios rezagos",
               "Diagnóstico de residuos de modelos ARIMA/SARIMA y series temporales",
               "Indica dependencia conjunta, pero no identifica por sí sola en qué rezago está ni su forma"),
             c("Runs test",
               "Aleatoriedad de la secuencia según cambios de signo o posición respecto a una referencia",
               "Diagnóstico no paramétrico y exploratorio",
               "No mide directamente autocorrelación lineal por rezagos")
         ),
         independencePart2 = c(
             "",
             "",
             "Durbin-Watson — qué evalúa:",
             "Evalúa si los residuos de una regresión presentan autocorrelación de primer",
             "orden. H0: no hay autocorrelación de orden 1. H1: hay autocorrelación de orden",
             "1, positiva o negativa. Su estadístico va aproximadamente entre 0 y 4: cerca",
             "de 2 indica ausencia de autocorrelación de primer orden; por debajo de 2",
             "sugiere autocorrelación positiva; por encima de 2 sugiere autocorrelación",
             "negativa. Existe una relación aproximada DW ≈ 2 × (1 − r1), donde r1 es la",
             "autocorrelación residual estimada en el rezago 1.",
             "",
             "Durbin-Watson — cuándo usarlo:",
             "Como diagnóstico simple cuando hay una regresión lineal por mínimos cuadrados,",
             "datos ordenados temporalmente, interés específico en la dependencia entre un",
             "residuo y el inmediatamente anterior, y un modelo sin la variable dependiente",
             "rezagada como regresor.",
             "",
             "Durbin-Watson — cuándo no elegirlo:",
             "Cuando el modelo incluye la dependiente rezagada (un modelo dinámico), cuando",
             "se sospecha autocorrelación de orden 2, 3, 12, etc., cuando hay una estructura",
             "temporal más compleja como estacionalidad, o cuando se necesita un contraste",
             "formal con p-valor fácil de generalizar. En la práctica aplicada moderna,",
             "Durbin-Watson suele reportarse como indicador descriptivo rápido, mientras que",
             "Breusch-Godfrey suele preferirse para el contraste formal.",
             "",
             "Breusch-Godfrey — qué evalúa:",
             "También llamado LM de autocorrelación serial, contrasta si los residuos de",
             "una regresión tienen autocorrelación hasta un orden p elegido por quien",
             "analiza. H0: no hay autocorrelación residual hasta el orden p. H1: existe",
             "autocorrelación en al menos uno de los rezagos de 1 a p. La prueba se basa en",
             "una regresión auxiliar que incorpora los residuos rezagados; bajo H0, el",
             "estadístico LM sigue aproximadamente una distribución chi-cuadrado con p",
             "grados de libertad.",
             "",
             "Breusch-Godfrey — cuándo usarlo:",
             "Es la opción más general para regresiones. Conviene usarla con regresiones",
             "econométricas, cuando se quiere probar más de un rezago, cuando el modelo",
             "incluye variables explicativas rezagadas o la propia dependiente rezagada, y",
             "cuando se necesita un contraste formal con p-valor para decidir entre modelar",
             "errores AR, usar GLS, aplicar errores HAC/Newey-West, o replantear la",
             "especificación dinámica del modelo.",
             "",
             "Breusch-Godfrey — cómo elegir el orden p:",
             "No conviene elegirlo mecánicamente ni exagerarlo. Con datos anuales suele",
             "tener sentido empezar con 1 o 2 rezagos; con datos trimestrales, probar de 1 a",
             "4; con datos mensuales, puede ser razonable probar hasta 12 si hay un posible",
             "patrón anual; con frecuencias más altas conviene apoyarse en el conocimiento",
             "del proceso y revisar la función de autocorrelación (ACF) y autocorrelación",
             "parcial (PACF) de los residuos. Regla práctica: ante la duda entre",
             "Durbin-Watson y Breusch-Godfrey para una regresión, se prefiere",
             "Breusch-Godfrey.",
             "",
             "Ljung-Box — qué evalúa:",
             "Es una prueba conjunta (portmanteau test) que contrasta si las",
             "autocorrelaciones hasta un horizonte h son conjuntamente cero. H0: no hay",
             "autocorrelación hasta el rezago h. H1: existe autocorrelación en al menos",
             "alguno de esos rezagos. Su estadístico Q combina, ponderado por el tamaño de",
             "muestra y cada rezago, los cuadrados de las autocorrelaciones residuales",
             "estimadas hasta el rezago h; por eso no pregunta si hay autocorrelación solo",
             "en el rezago 1, sino si los residuos, en conjunto, parecen ruido blanco hasta",
             "ese horizonte.",
             "",
             "Ljung-Box — cuándo usarlo y qué horizonte h elegir:",
             "Es especialmente útil para diagnosticar residuos de un modelo ARIMA, SARIMA,",
             "ETS u otro modelo de predicción temporal, y para comprobar si tras ajustar el",
             "modelo queda estructura temporal sin explicar. Su uso habitual es sobre los",
             "residuos de un modelo ya ajustado, no sobre la serie bruta. El horizonte h",
             "depende de la frecuencia y del objetivo: con datos trimestrales suele",
             "revisarse h=4 u 8; con datos mensuales, h=12 es un punto de partida natural;",
             "con datos diarios conviene un horizonte coherente con ciclos semanales,",
             "mensuales o de negocio. Al aplicarla sobre residuos de un ARIMA, además hay",
             "que ajustar los grados de libertad por los parámetros AR y MA ya estimados.",
             "",
             "Ljung-Box — diferencia frente a Breusch-Godfrey:",
             "Ambas pueden revisar varios rezagos a la vez, pero su contexto natural es",
             "distinto: Breusch-Godfrey se usa en una regresión con covariables y pregunta",
             "si los errores de esa regresión están serialmente correlacionados; Ljung-Box",
             "es un diagnóstico global de los residuos de un modelo de serie temporal y",
             "pregunta si esos residuos parecen ruido blanco.",
             "",
             "Runs test — qué evalúa realmente:",
             "A diferencia de las tres pruebas anteriores, no es principalmente una prueba",
             "de autocorrelación lineal. Evalúa si el orden de una secuencia parece",
             "aleatorio. Una racha es una secuencia consecutiva de observaciones de la",
             "misma clase — por ejemplo, residuos positivos consecutivos frente a",
             "negativos, o valores por encima frente a valores por debajo de la mediana.",
             "H0: la secuencia se generó aleatoriamente. H1: la secuencia no parece",
             "aleatoria. El test compara el número observado de rachas con el esperado bajo",
             "aleatoriedad: muy pocas rachas sugieren agrupamiento o persistencia;",
             "demasiadas pueden sugerir alternancia excesiva.",
             "",
             "Runs test — cuándo usarlo:",
             "Cuando se busca un diagnóstico no paramétrico, cuando los residuos no son",
             "normales o tienen atípicos, cuando interesa detectar agrupamiento de signos,",
             "cambios de régimen o falta de aleatoriedad en general, cuando se trabaja con",
             "una secuencia binaria (signos, aprobado/reprobado, defectuoso/no defectuoso),",
             "o como complemento no paramétrico a las pruebas de autocorrelación anteriores.",
             "",
             "Runs test — qué no concluye:",
             "Un resultado significativo solo respalda que el orden de la serie no parece",
             "aleatorio; no implica necesariamente que exista autocorrelación lineal de tipo",
             "AR(1). Puede detectar no aleatoriedad originada por tendencias, cambios",
             "estructurales, persistencia de signos o patrones no lineales. A la inversa,",
             "una serie puede no mostrar autocorrelación lineal fuerte y aun así no ser",
             "completamente aleatoria.",
             "",
             "Árbol de decisión práctico:",
             "Regresión lineal con datos temporales, sin dependiente rezagada: empiece con",
             "un gráfico de residuos en el tiempo, la ACF/PACF de residuos, y",
             "Breusch-Godfrey; use Durbin-Watson solo como comprobación rápida del rezago 1.",
             "Regresión con la dependiente rezagada u otros componentes dinámicos: use",
             "Breusch-Godfrey y la ACF/PACF de residuos; evite basarse en Durbin-Watson.",
             "Modelo ARIMA o SARIMA ya ajustado: use la ACF de residuos y Ljung-Box, para",
             "verificar que el modelo haya absorbido la dependencia temporal.",
             "Aleatoriedad general sin asumir una relación lineal concreta: use Runs test,",
             "un gráfico de secuencia, y la ACF como complemento.",
             "",
             "Recomendación final si se busca una prueba por defecto:",
             "Regresión temporal o econométrica general: Breusch-Godfrey.",
             "Modelo ARIMA/SARIMA o forecasting: Ljung-Box.",
             "Chequeo rápido y básico de autocorrelación AR(1) en una regresión estática:",
             "Durbin-Watson.",
             "Comprobación no paramétrica de aleatoriedad o patrón de signos: Runs test.",
             "En casi todos los casos, conviene acompañar la prueba con una ACF de",
             "residuos: el p-valor indica si hay evidencia global contra la ausencia de",
             "dependencia, y la ACF ayuda a ver en qué rezagos está el problema.",
             "",
             "Criterios previos:",
             "Estas pruebas tienen más sentido cuando hay un orden temporal, espacial o",
             "secuencial. En datos transversales sin orden natural, deben interpretarse",
             "con cautela.",
             "",
             "Interpretación:",
             "p < .05 sugiere una desviación estadísticamente significativa respecto",
             "a independencia o aleatoriedad de residuos.",
             "p >= .05 indica que el resultado es compatible con independencia o",
             "aleatoriedad aproximada de residuos.",
             "",
             "Decisión metodológica:",
             "Si hay dependencia, considerar modelos mixtos, GLS, modelos temporales o",
             "errores robustos HAC/Newey-West según el diseño."
         ),
         multicollinearityPart1 = c(
             "Multicolinealidad",
             "",
             "Qué evalúa:",
             "Evalúa si los predictores del modelo están demasiado relacionados entre sí.",
             "La multicolinealidad no necesariamente reduce la capacidad predictiva global",
             "del modelo, pero infla la varianza de los coeficientes individuales y",
             "dificulta interpretarlos por separado (Marquardt, 1970).",
             "",
             "Dónde se usa en AssumptionsLab:",
             "Regression y modelos lineales con varios predictores.",
             "",
             "Por qué importa:",
             "Cuando dos o más predictores comparten mucha información, el modelo no puede",
             "distinguir con precisión el efecto propio de cada uno; los coeficientes",
             "individuales se vuelven inestables (cambian mucho al agregar o quitar casos",
             "o predictores) y sus errores estándar se inflan, aunque el ajuste global",
             "(R²) y las predicciones del modelo completo puedan seguir siendo",
             "razonables (O'Brien, 2007)."
         ),
         multicollinearityTableHeaders = c("Diagnóstico", "Qué mide", "Regla práctica habitual", "Principal limitación"),
         multicollinearityTableRows = list(
             c("VIF", "Cuánto se infla la varianza de un coeficiente por su asociación con los demás predictores",
               "VIF < 5 aceptable; 5-10 alerta; ≥ 10 problema severo",
               "Los umbrales son convenciones, no reglas universales (O'Brien, 2007); no distingue cuáles variables son redundantes entre sí"),
             c("Tolerancia", "El inverso del VIF (1/VIF)",
               "> .20 aceptable; < .20 alerta; < .10 problema severo",
               "Misma información que VIF en otra escala; no aporta nada adicional por sí sola"),
             c("Correlación máxima", "La mayor correlación bivariada entre pares de predictores",
               "|r| ≥ .80 sugiere posible redundancia bivariada",
               "Solo detecta colinealidad entre pares; no ve redundancia distribuida entre tres o más predictores"),
             c("Índice de condición", "La razón entre el mayor y menor eigenvalue de la matriz de predictores estandarizada",
               "> 15 alerta; > 30 colinealidad severa",
               "Requiere revisar también las proporciones de varianza por dimensión para saber qué predictores están involucrados"),
             c("Eigenvalues / determinante", "Cuán cerca de ser singular está la matriz de predictores",
               "Valores cercanos a cero sugieren dimensiones redundantes",
               "Es una señal agregada; no identifica directamente qué combinación de predictores es redundante"),
             c("Rango del modelo", "Si existe dependencia lineal exacta entre predictores",
               "Rango < número de columnas indica parámetros no estimables",
               "Es un problema de identificación exacta, distinto de la colinealidad aproximada que capturan las medidas anteriores")
         ),
         multicollinearityPart2 = c(
             "",
             "",
             "VIF y tolerancia — qué evalúan:",
             "El VIF de un predictor se obtiene regresando ese predictor sobre todos los",
             "demás predictores del modelo y usando el R² resultante: VIF = 1/(1-R²). Un",
             "VIF alto significa que gran parte de la variación de ese predictor ya puede",
             "explicarse con los demás, dejando poca información \"propia\" para estimar su",
             "coeficiente con precisión. La tolerancia es simplemente 1/VIF, expresada en",
             "la escala opuesta.",
             "",
             "VIF y tolerancia — cuándo usarlos y advertencia sobre los umbrales:",
             "Son el diagnóstico más habitual y fácil de interpretar por variable. Los",
             "umbrales convencionales (VIF < 5 o < 10) son reglas de uso extendido, no",
             "límites universales derivados matemáticamente: el nivel de VIF tolerable",
             "depende del tamaño muestral, de cuánta varianza explica el modelo y de si el",
             "interés está en los coeficientes individuales o solo en la predicción global",
             "(O'Brien, 2007). Con muestras grandes, VIF moderados pueden ser inofensivos;",
             "con muestras pequeñas, incluso VIF de 3-4 pueden ya generar coeficientes",
             "inestables.",
             "",
             "Correlación máxima entre predictores:",
             "Es el chequeo bivariado más simple: identifica pares de predictores muy",
             "correlacionados entre sí. Es útil como primer vistazo, pero no detecta la",
             "colinealidad \"distribuida\", en la que ningún par individual está muy",
             "correlacionado pero una combinación lineal de tres o más predictores sí lo",
             "está — ese tipo de colinealidad solo aparece en VIF, eigenvalues o índice de",
             "condición.",
             "",
             "Índice de condición y eigenvalues:",
             "Provienen de la descomposición de la matriz de predictores estandarizada.",
             "Un eigenvalue cercano a cero indica una dirección en el espacio de",
             "predictores con casi ninguna variación independiente — es decir, una",
             "combinación lineal casi redundante. El índice de condición resume esto como",
             "la razón entre el eigenvalue mayor y el menor; valores altos señalan",
             "colinealidad, pero conviene revisar también las proporciones de varianza por",
             "dimensión para identificar qué predictores concretos están implicados",
             "(Belsley et al., 1980).",
             "",
             "Determinante y rango del modelo:",
             "El determinante de la matriz de correlaciones entre predictores cercano a",
             "cero es otra señal agregada de redundancia. El rango del modelo es un",
             "chequeo distinto y más estricto: si el rango es menor que el número de",
             "columnas de la matriz de diseño, existe una dependencia lineal exacta (no",
             "solo alta correlación) y algunos parámetros del modelo no son estimables en",
             "absoluto — R típicamente los marca como NA en la salida del modelo.",
             "",
             "Decisión metodológica:",
             "Si la colinealidad es alta pero el objetivo principal es predicción, puede",
             "no requerir acción. Si el objetivo es interpretar coeficientes individuales,",
             "considere retirar o combinar predictores redundantes, centrar términos",
             "polinómicos e interacciones (lo que reduce colinealidad estructural sin",
             "cambiar el modelo sustantivo), o usar métodos penalizados (ridge, lasso) que",
             "estabilizan las estimaciones a cambio de un sesgo controlado (Marquardt,",
             "1970)."
         ),

         influencePart1 = c(
             "Casos atípicos e influencia",
             "",
             "Qué evalúa:",
             "Evalúa si los resultados pueden estar dominados por pocos casos extremos,",
             "inusuales o muy influyentes, en vez de reflejar el patrón general de los",
             "datos.",
             "",
             "Dónde se usa en AssumptionsLab:",
             "Independent Groups, Related Groups, ANOVA/ANCOVA y Regression.",
             "",
             "Distinción importante:",
             "Un caso atípico (outlier) es un valor inusual respecto al resto de los",
             "datos; un caso influyente es uno cuya presencia o ausencia cambia",
             "sustancialmente el resultado del análisis. Un caso puede ser atípico sin ser",
             "influyente, e influyente sin ser un atípico evidente — por eso conviene",
             "varios diagnósticos, no solo uno (Belsley et al., 1980; Cook, 1977)."
         ),
         influenceTableHeaders = c("Diagnóstico", "Qué mide", "Regla práctica habitual", "Principal limitación"),
         influenceTableRows = list(
             c("IQR", "Distancia de un valor respecto al rango intercuartílico",
               "Fuera de Q1 − 1.5·IQR o Q3 + 1.5·IQR",
               "Solo mira la variable de forma univariante; no considera el modelo ni otras variables"),
             c("Residuo studentizado", "Discrepancia del caso respecto al modelo ajustado, en unidades de error estandarizadas",
               "|residuo studentizado| > 3",
               "Un residuo grande señala mal ajuste puntual, no necesariamente que el caso mueva las estimaciones"),
             c("Leverage", "Cuán inusual es la combinación de predictores del caso",
               "Leverage > 2p/n (p = número real de parámetros estimados)",
               "Un leverage alto no implica por sí solo que el caso sea influyente si su residuo es pequeño"),
             c("Cook's D", "Influencia conjunta del caso sobre todas las estimaciones del modelo",
               "Cook's D > 4/n, como umbral de cribado (Cook, 1977)",
               "4/n es más laxo con muestras pequeñas y más estricto con muestras grandes; tratar como cribado, no como confirmación"),
             c("DFFITS", "Cuánto cambia el valor ajustado del propio caso al excluirlo del modelo",
               "|DFFITS| > 2·√(p/n)",
               "Sensible al mismo tipo de casos que Cook's D; conviene interpretarlos juntos, no por separado"),
             c("Mahalanobis D²", "Distancia multivariante del caso respecto al centroide de los predictores",
               "Valores altos según chi-cuadrado con p grados de libertad",
               "Detecta combinaciones inusuales de predictores (leverage multivariante), no si esas combinaciones afectan el resultado (Mahalanobis, 1936)")
         ),
         influencePart2 = c(
             "",
             "",
             "IQR — qué evalúa y cuándo usarlo:",
             "Marca valores por debajo de Q1 - 1.5*IQR o por encima de Q3 + 1.5*IQR de la",
             "variable analizada. En grupos relacionados se aplica sobre las diferencias",
             "pareadas, no sobre las mediciones originales. Es el chequeo más simple y no",
             "depende de ningún modelo ajustado — por eso es el punto de partida natural",
             "antes de ajustar cualquier análisis.",
             "",
             "Residuo studentizado — qué evalúa y cuándo usarlo:",
             "Identifica observaciones con una discrepancia grande respecto a lo que el",
             "modelo predice, expresada en unidades de desviación estándar del error. Es",
             "útil para detectar casos que el modelo ajusta mal, pero un residuo grande no",
             "implica automáticamente que ese caso esté moviendo las estimaciones — para",
             "eso hacen falta Cook's D o DFFITS.",
             "",
             "Leverage — qué evalúa y advertencia sobre el umbral:",
             "Identifica combinaciones inusuales de valores de predictores, sin mirar",
             "todavía la variable dependiente. Regla práctica: leverage > 2p/n, donde p es",
             "el número real de parámetros estimados del modelo (length(coef(fit)) en R)",
             "— no la cantidad de variables seleccionadas. Cuando un predictor es un",
             "factor con más de 2 niveles, cada nivel adicional agrega un parámetro por",
             "codificación dummy; usar el conteo de variables en vez del conteo real de",
             "parámetros subestima el umbral y marca casos de más.",
             "",
             "Cook's D — qué evalúa y cuándo usarlo:",
             "Combina leverage y magnitud del residuo en una sola medida de influencia",
             "sobre el conjunto de coeficientes estimados (Cook, 1977). Regla práctica:",
             "Cook's D > 4/n. Trátelo como un umbral de cribado, no como influencia",
             "confirmada: 4/n es numéricamente mayor (más laxo) cuanto más pequeña es la",
             "muestra, y menor (más estricto) cuanto más crece; aun así, las muestras",
             "pequeñas suelen seguir marcando varios casos porque los valores individuales",
             "de Cook's D tienden a inflarse cuando n es pequeño.",
             "",
             "DFFITS — qué evalúa y cuándo usarlo:",
             "Evalúa cuánto cambia el valor ajustado del propio caso al retirarlo y",
             "reajustar el modelo sin él (Belsley et al., 1980). Regla práctica:",
             "|DFFITS| > 2*sqrt(p/n). Es conceptualmente muy cercano a Cook's D — ambos",
             "capturan influencia sobre el ajuste — así que conviene leerlos como",
             "corroboración mutua, no como dos evidencias independientes.",
             "",
             "Mahalanobis D² — qué evalúa y cuándo usarlo:",
             "Evalúa la distancia multivariante de un caso respecto al centroide de los",
             "predictores, considerando las correlaciones entre ellos (Mahalanobis, 1936).",
             "Es el análogo multivariante del leverage: detecta combinaciones inusuales de",
             "predictores incluso cuando ninguna variable por separado es un valor",
             "extremo. Al igual que el leverage, no dice nada por sí solo sobre si esa",
             "combinación inusual afecta realmente los resultados.",
             "",
             "Interpretación:",
             "Un caso marcado no debe eliminarse automáticamente. Puede ser un error de",
             "captura de datos, un caso extremo válido, o evidencia sustantiva importante",
             "que el modelo no está capturando bien.",
             "",
             "Decisión metodológica:",
             "Revise el dato original, realice un análisis de sensibilidad (comparar",
             "resultados con y sin el caso) y justifique cualquier exclusión con criterios",
             "transparentes y declarados de antemano, nunca elegidos después de ver que la",
             "exclusión \"mejora\" el resultado."
         ),

         sphericityPart1 = c(
             "Esfericidad",
             "",
             "Qué evalúa:",
             "La esfericidad evalúa si las varianzas de las diferencias entre todos los",
             "pares posibles de mediciones repetidas son aproximadamente iguales (Mauchly,",
             "1940).",
             "",
             "Dónde se usa en AssumptionsLab:",
             "Related Groups con tres o más mediciones relacionadas.",
             "",
             "No aplica con solo dos mediciones, porque con dos niveles solo hay un par de",
             "diferencias posible y no hay varianzas que comparar entre sí.",
             "",
             "Por qué importa:",
             "El ANOVA de medidas repetidas clásico asume esfericidad para que su",
             "estadístico F siga la distribución F nominal. Cuando se viola, el F",
             "observado tiende a inflarse y el error Tipo I real supera al nominal; las",
             "correcciones de grados de libertad existen precisamente para compensar esa",
             "inflación sin cambiar el estadístico F en sí."
         ),
         sphericityTableHeaders = c("Diagnóstico / corrección", "Qué hace", "Cuándo usarlo", "Principal limitación"),
         sphericityTableRows = list(
             c("Mauchly (W)", "Contrasta la hipótesis de esfericidad exacta",
               "Como chequeo inicial antes de decidir si corregir los grados de libertad",
               "Inestable con muestras pequeñas y demasiado sensible (marca violaciones triviales) con muestras grandes"),
             c("Greenhouse-Geisser", "Corrige los grados de libertad multiplicándolos por epsilon estimado",
               "Corrección conservadora por defecto, especialmente si epsilon < .75",
               "Puede ser excesivamente conservadora (pierde potencia) cuando la violación real es leve"),
             c("Huynh-Feldt", "Corrige los grados de libertad con un epsilon ajustado, menos conservador que GG",
               "Preferible sobre Greenhouse-Geisser cuando epsilon estimado > .75",
               "Puede sobrestimar epsilon (ser demasiado liberal) en muestras pequeñas"),
             c("Lower-bound", "Aplica la corrección más severa posible (epsilon = 1/(k-1))",
               "Como cota conservadora extrema cuando se prioriza evitar falsos positivos a toda costa",
               "Muy conservadora: sacrifica bastante potencia estadística frente a GG o HF")
         ),
         sphericityPart2 = c(
             "",
             "",
             "Mauchly — qué evalúa y limitaciones:",
             "Contrasta H0: las varianzas de todas las diferencias entre pares de niveles",
             "son iguales (esfericidad), frente a H1: al menos un par difiere (Mauchly,",
             "1940). Es la prueba de referencia histórica, pero su comportamiento no es",
             "uniforme: con muestras pequeñas puede carecer de poder para detectar",
             "violaciones reales, y con muestras grandes puede marcar como significativas",
             "desviaciones de esfericidad demasiado pequeñas para tener relevancia",
             "práctica sobre el estadístico F.",
             "",
             "Greenhouse-Geisser — qué hace y cuándo usarla:",
             "En lugar de contrastar esfericidad directamente, estima cuánto se aleja de",
             "ella el patrón de covarianzas observado (epsilon, entre 1/(k-1) y 1) y",
             "reduce los grados de libertad del test F multiplicándolos por ese epsilon",
             "(Greenhouse & Geisser, 1959). El estadístico F no cambia; solo cambian los",
             "grados de libertad usados para obtener el valor p, haciendo el contraste más",
             "conservador. Es la corrección por defecto recomendada cuando epsilon",
             "estimado es bajo (< .75).",
             "",
             "Huynh-Feldt — qué hace y cuándo usarla:",
             "Es una corrección alternativa que ajusta el epsilon de Greenhouse-Geisser",
             "para reducir su tendencia a ser demasiado conservadora, especialmente",
             "cuando la violación de esfericidad es leve o moderada (Huynh & Feldt, 1976).",
             "La convención habitual es preferir Huynh-Feldt cuando el epsilon estimado por",
             "Greenhouse-Geisser es superior a .75, y Greenhouse-Geisser cuando es menor.",
             "",
             "Lower-bound — qué hace y cuándo usarla:",
             "Aplica la corrección más extrema matemáticamente posible, asumiendo el peor",
             "caso de violación de esfericidad. Es útil como cota de seguridad —si el",
             "efecto sigue siendo significativo incluso con esta corrección tan severa, la",
             "conclusión es robusta a cualquier grado de violación de esfericidad— pero",
             "sacrifica bastante potencia y rara vez es la corrección final que se reporta.",
             "",
             "Interpretación:",
             "p < .05 en Mauchly sugiere una desviación estadísticamente significativa",
             "respecto a la esfericidad. p >= .05 indica que el resultado es compatible",
             "con esfericidad aproximada.",
             "",
             "Decisión metodológica:",
             "Si hay evidencia contra esfericidad, use la corrección Greenhouse-Geisser o",
             "Huynh-Feldt según el epsilon estimado, considere la prueba no paramétrica de",
             "Friedman (Friedman, 1937) como alternativa, o ajuste un modelo mixto que no",
             "asuma esfericidad, según el diseño del estudio."
         ),

         # ---- proportionalOdds: ordCheck's parallel-lines assumption, added
         # Sep 2026 alongside ordCheck. New library category, not a
         # subsection of an existing one - modeled directly on the
         # "sphericity" precedent above (a narrow assumption tied to one
         # specific model family, not a cross-cutting concept). See
         # /areas/assumptionslab-ordinal-multinomial.md for the decision
         # record.
         # ES: proportionalOdds: el supuesto de líneas paralelas de
         # ordCheck, agregado en sep 2026 junto con ordCheck. Categoría
         # nueva de la biblioteca, no una subsección de una existente -
         # modelada directamente sobre el precedente de "sphericity" de
         # arriba (un supuesto angosto ligado a una familia de modelo
         # específica, no un concepto transversal).
         proportionalOddsPart1 = c(
             "Momios proporcionales",
             "",
             "Qué evalúa:",
             "El modelo de momios proporcionales asume que el efecto de cada predictor",
             "sobre el desenlace ordinal tiene la misma magnitud en todos los puntos de",
             "corte entre categorías (el supuesto de \"líneas paralelas\"). La prueba de",
             "Brant (1990) es el chequeo estándar.",
             "",
             "Dónde se usa en AssumptionsLab:",
             "ordCheck (regresión logística ordinal).",
             "",
             "No aplica a regresión logística binaria (logCheck) ni a regresión",
             "multinomial (multCheck), porque ninguna de las dos tiene múltiples puntos",
             "de corte cuyas pendientes deban compararse entre sí.",
             "",
             "Por qué importa:",
             "Si se viola, un único odds ratio reportado por predictor representa mal su",
             "efecto real: lo subestima en algunos puntos de corte y lo sobrestima en",
             "otros, y puede llevar a conclusiones sustantivas equivocadas sobre entre",
             "qué categorías realmente discrimina un predictor."
         ),
         proportionalOddsTableHeaders = c("Diagnóstico / alternativa", "Qué hace", "Cuándo usarlo", "Principal limitación"),
         proportionalOddsTableRows = list(
             c("Prueba de Brant", "Contrasta si el coeficiente de cada predictor es igual en todos los puntos de corte",
               "Como chequeo inicial tras ajustar el modelo, antes de interpretar un único OR por predictor",
               "Poder limitado en muestras pequeñas o categorías escasas; un resultado no significativo no es prueba fuerte de proporcionalidad"),
             c("Modelo de momios parcialmente proporcionales", "Deja variar por punto de corte solo los predictores señalados por Brant, y fija el resto con un único coeficiente",
               "Cuando Brant señala uno o dos predictores puntuales, no el ómnibus completo",
               "Menos estandarizado en software estadístico; la interpretación se complica al mezclar efectos constantes y variables"),
             c("Modelo multinomial (multCheck)", "Abandona el supuesto de proporcionalidad tratando cada categoría como nominal",
               "Cuando la violación es generalizada (ómnibus significativo, varios predictores afectados)",
               "Pierde la eficiencia de aprovechar el orden de las categorías; hay que estimar más parámetros"),
             c("Reportar efectos por punto de corte", "Ajusta logits binarios separados en cada punto de corte y reporta un OR distinto para cada uno",
               "Como alternativa transparente cuando solo interesa documentar el patrón, no forzar un modelo único",
               "No hay un único modelo conjunto que resuma el efecto; las comparaciones entre puntos de corte pierden algo de eficiencia estadística")
         ),
         proportionalOddsPart2 = c(
             "",
             "",
             "Brant — qué evalúa y limitaciones:",
             "Contrasta H0: el coeficiente de cada predictor es igual en todos los puntos",
             "de corte (momios proporcionales), frente a H1: al menos un punto de corte",
             "difiere (Brant, 1990). Compara los coeficientes de logits binarios",
             "separados ajustados en cada punto de corte contra el coeficiente único que",
             "reporta el modelo de momios proporcionales. Tiene poder limitado en",
             "muestras pequeñas o con categorías escasas.",
             "",
             "Modelo de momios parcialmente proporcionales — qué hace y cuándo usarlo:",
             "Permite que el coeficiente de un predictor específico varíe entre puntos de",
             "corte, mientras el resto de los predictores mantiene un único coeficiente",
             "constante. Es la opción más quirúrgica cuando Brant señala uno o dos",
             "predictores puntuales en vez de una violación generalizada.",
             "",
             "Modelo multinomial — qué hace y cuándo usarlo:",
             "Trata cada categoría del desenlace como nominal, sin asumir ningún orden ni",
             "ninguna relación de proporcionalidad entre coeficientes. Es la alternativa",
             "más segura cuando la prueba ómnibus de Brant es significativa y afecta a",
             "varios predictores a la vez, al costo de no aprovechar la información de",
             "orden entre categorías.",
             "",
             "Reportar efectos por punto de corte — qué hace y cuándo usarlo:",
             "En vez de forzar un modelo único, se ajustan logits binarios independientes",
             "en cada punto de corte y se reporta un odds ratio distinto para cada uno.",
             "Es una alternativa transparente cuando el objetivo es describir el patrón",
             "de la relación más que producir un modelo parsimonioso único.",
             "",
             "Interpretación:",
             "p < .05 en la prueba ómnibus de Brant sugiere una desviación",
             "estadísticamente significativa respecto a los momios proporcionales.",
             "p >= .05 indica que el resultado es compatible con momios proporcionales",
             "aproximados.",
             "",
             "Decisión metodológica:",
             "Si hay evidencia contra los momios proporcionales, revise primero qué",
             "predictor(es) individuales la prueba de Brant señala (no solo el ómnibus).",
             "Con uno o dos predictores puntuales, considere un modelo de momios",
             "parcialmente proporcionales; con una violación generalizada, considere un",
             "modelo multinomial (multCheck) o reportar los efectos por punto de corte",
             "por separado."
         ),

         # ---- iia: multCheck's own assumption (Independence of Irrelevant
         # Alternatives), added Sep 2026 alongside proportionalOdds above.
         # New library category, not a subsection of an existing one -
         # same rationale as proportionalOdds: a narrow assumption tied to
         # one specific model family (multinomial logit), not a
         # cross-cutting concept. Placed right after proportionalOdds
         # because both are regression-family, model-specific "sibling"
         # assumptions (ordCheck / multCheck). Content is grounded in
         # multcheck.b.R's own iiaGuide text and "not computable" fallback
         # wording - see that file's header for the full rationale.
         # ES: iia: el supuesto propio de multCheck (Independencia de
         # Alternativas Irrelevantes), agregado en sep 2026 junto con
         # proportionalOdds de arriba. Categoría nueva de la biblioteca, no
         # una subsección de una existente - mismo razonamiento que
         # proportionalOdds: un supuesto angosto ligado a una familia de
         # modelo específica (logit multinomial), no un concepto
         # transversal. Ubicado justo después de proportionalOdds porque
         # ambos son supuestos "hermanos" propios de un modelo de la
         # familia Regression (ordCheck / multCheck). El contenido se basa
         # en el texto propio de iiaGuide de multcheck.b.R y en su mensaje
         # de reserva "no calculable" - ver el encabezado de ese archivo
         # para el razonamiento completo.
         iiaPart1 = c(
             "Independencia de alternativas irrelevantes (IIA)",
             "",
             "Qué evalúa:",
             "El modelo logit multinomial asume que los momios relativos entre dos",
             "categorías cualesquiera del desenlace no dependen de qué otras categorías",
             "también estén disponibles en el conjunto de elección: la independencia de",
             "alternativas irrelevantes (IIA). Es el único supuesto propio de esta familia",
             "de modelos, sin equivalente en regresión logística binaria ni ordinal.",
             "",
             "Dónde se usa en AssumptionsLab:",
             "multCheck (regresión logística multinomial).",
             "",
             "No aplica a regresión logística binaria (logCheck), porque con solo dos",
             "categorías no existen \"alternativas irrelevantes\" que puedan retirarse del",
             "conjunto de elección. Tampoco aplica a regresión logística ordinal",
             "(ordCheck), que en su lugar depende del supuesto de momios proporcionales",
             "(líneas paralelas) entre puntos de corte ordenados.",
             "",
             "Por qué importa:",
             "Si se viola, los odds ratios reportados entre categorías no son estables:",
             "cambian según qué otras categorías estén presentes en el conjunto de",
             "elección, lo que socava interpretar un único conjunto de coeficientes como si",
             "describiera una preferencia consistente entre alternativas."
         ),
         iiaTableHeaders = c("Diagnóstico / alternativa", "Qué hace", "Cuándo usarlo", "Principal limitación"),
         iiaTableRows = list(
             c("Prueba de Hausman-McFadden", "Reajusta el modelo omitiendo, de a una, cada categoría del desenlace y compara los coeficientes compartidos con el modelo completo",
               "Como chequeo inicial tras ajustar el modelo multinomial, antes de confiar en los odds ratios reportados",
               "El estadístico no siempre es calculable (la diferencia de matrices de covarianza puede no ser invertible en muestras finitas); un resultado no calculable no es prueba de que la IIA se sostenga"),
             c("Modelo logit anidado", "Agrupa las categorías del desenlace en una estructura jerárquica de nidos, permitiendo correlación entre alternativas dentro de un mismo nido",
               "Cuando la violación de IIA sigue un patrón de agrupamiento sustantivo (p. ej., categorías que comparten atributos no observados)",
               "Requiere especificar a priori una estructura de nidos razonable; los resultados son sensibles a esa elección"),
             c("Modelo probit multinomial", "Reemplaza el supuesto logit de errores independientes por errores con distribución normal multivariante, permitiendo correlación entre alternativas",
               "Cuando se dispone de tiempo de cómputo suficiente y no hay una estructura de nidos natural",
               "Computacionalmente más costoso; no tiene forma cerrada y requiere integración numérica o simulación"),
             c("Reportar coeficientes con cautela", "En vez de descartar el modelo, se documentan las categorías señaladas por Hausman-McFadden y se interpretan sus odds ratios con reserva adicional",
               "Como alternativa transparente cuando la violación afecta solo una o dos categorías puntuales, no un patrón generalizado",
               "No corrige el problema subyacente, solo lo documenta, dejando la cautela interpretativa a cargo del lector")
         ),
         iiaPart2 = c(
             "",
             "",
             "Hausman-McFadden — qué evalúa y limitaciones:",
             "Contrasta H0: los coeficientes compartidos entre el modelo completo y un",
             "modelo reajustado sin una categoría no cambian más allá del error de",
             "muestreo (la IIA se sostiene), frente a H1: sí cambian (Hausman & McFadden,",
             "1984). Se calcula una vez por cada categoría omitida. En muestras finitas,",
             "el estadístico depende de la diferencia entre dos matrices de covarianza,",
             "que no siempre es invertible — una degeneración numérica bien documentada",
             "de esta prueba específica, no evidencia de un error de cálculo. Cuando esto",
             "ocurre, el resultado se reporta como \"no calculable\": esto no es en sí mismo",
             "prueba de que la IIA se sostenga (Cheng & Long, 2007, documentan que las",
             "pruebas de tipo Hausman para IIA se comportan de forma errática en la",
             "práctica).",
             "",
             "Modelo logit anidado — qué hace y cuándo usarlo:",
             "Agrupa las categorías del desenlace en una estructura jerárquica de nidos,",
             "permitiendo factores no observados correlacionados dentro de un mismo nido",
             "mientras mantiene independencia entre nidos distintos. Es la alternativa más",
             "fundamentada cuando las categorías señaladas por Hausman-McFadden comparten",
             "un agrupamiento sustantivo identificable.",
             "",
             "Modelo probit multinomial — qué hace y cuándo usarlo:",
             "Abandona por completo el supuesto logit de errores independientes en favor",
             "de errores correlacionados con distribución normal multivariante entre",
             "alternativas. Es la alternativa más general —pero computacionalmente más",
             "pesada— cuando no existe una estructura de nidos natural.",
             "",
             "Reportar coeficientes con cautela — qué hace y cuándo usarlo:",
             "En vez de cambiar de modelo, se documentan explícitamente las categorías",
             "señaladas por la prueba de Hausman-McFadden y se interpretan sus odds ratios",
             "con reserva adicional. Es una opción transparente cuando solo una o dos",
             "categorías quedan señaladas, no un patrón generalizado.",
             "",
             "Interpretación:",
             "p < .05 en la prueba de Hausman-McFadden para una categoría omitida sugiere",
             "una desviación estadísticamente significativa respecto a la IIA para esa",
             "categoría. p >= .05 indica que el resultado es compatible con la IIA. Un",
             "resultado \"no calculable\" no debe interpretarse como evidencia a favor ni en",
             "contra: es una limitación conocida de la prueba en sí, no un hallazgo",
             "sustantivo.",
             "",
             "Decisión metodológica:",
             "Si al menos una categoría omitida produce un estadístico significativo,",
             "considere un modelo logit anidado o probit multinomial (ninguno de los dos",
             "requiere IIA) antes de confiar en los odds ratios de este modelo, o",
             "documente con cautela las categorías señaladas. Si ninguna categoría resulta",
             "significativa —o si el resultado es mayormente \"no calculable\"—, esto es",
             "compatible con (aunque no prueba definitiva de) que la IIA se sostiene para",
             "este modelo."
         ),

         robustPart1 = c(
             "Transformaciones y alternativas robustas",
             "",
             "Qué evalúan:",
             "No son supuestos en sí mismos, sino respuestas metodológicas cuando un",
             "supuesto no es defendible o cuando los resultados son sensibles a",
             "desviaciones importantes de un supuesto.",
             "",
             "Dónde se usan en AssumptionsLab:",
             "Independent Groups, Related Groups, ANOVA/ANCOVA y Regression.",
             "",
             "Cómo elegir entre ellas:",
             "La alternativa correcta depende de qué supuesto específico falla, no de una",
             "preferencia general por métodos \"robustos\". Cambiar de método sin",
             "identificar primero qué supuesto se violó suele complicar la interpretación",
             "sin resolver el problema real."
         ),
         robustTableHeaders = c("Si el problema es...", "Alternativa habitual", "Aplica en", "Idea central"),
         robustTableRows = list(
             c("Varianzas desiguales entre grupos", "Corrección de Welch",
               "Independent Groups, ANOVA",
               "Ajusta los grados de libertad del test t o F sin asumir varianzas iguales (Welch, 1947)"),
             c("Varianzas desiguales, con asimetría o atípicos", "Brown-Forsythe (como ANOVA robusta)",
               "ANOVA",
               "Extiende la lógica de Welch usando estimadores de posición más robustos que la media"),
             c("Diferencias pareadas no normales pero simétricas", "Wilcoxon signed-rank",
               "Related Groups (dos mediciones)",
               "Usa los rangos de las diferencias en vez de sus valores, sin asumir normalidad (Wilcoxon, 1945)"),
             c("Tres o más mediciones relacionadas, no normales", "Friedman",
               "Related Groups (tres o más mediciones)",
               "Extiende la lógica de rangos a más de dos mediciones relacionadas (Friedman, 1937)"),
             c("Heterocedasticidad de residuos", "Errores estándar robustos HC3",
               "Regression",
               "Corrige los errores estándar de los coeficientes sin cambiar sus estimaciones puntuales (MacKinnon & White, 1985)"),
             c("Asimetría marcada de la variable dependiente", "Transformación (log, raíz, inversa)",
               "Cualquier análisis con variable continua",
               "Cambia la escala de la variable para que la distribución se acerque más a la normalidad"),
             c("Estructura de dependencia compleja (anidada, longitudinal)", "Modelos mixtos",
               "Datos repetidos, anidados o jerárquicos",
               "Modela explícitamente la correlación entre observaciones relacionadas en vez de asumir independencia")
         ),
         robustPart2 = c(
             "",
             "",
             "Transformaciones de la variable dependiente:",
             "log(Y): útil con asimetría positiva marcada y valores estrictamente mayores",
             "que 0. sqrt(Y): útil con datos de conteo o asimetría positiva leve o",
             "moderada, y valores >= 0. 1/Y: útil solo con cautela ante asimetría positiva",
             "severa y valores que no se acerquen a cero (la transformación inversa es muy",
             "sensible cerca de cero). Toda transformación cambia la escala de",
             "interpretación de los resultados, así que conviene reportarlo explícitamente.",
             "",
             "Welch — qué hace y cuándo usarla:",
             "Ajusta los grados de libertad del estadístico t o F para no requerir",
             "varianzas iguales entre grupos (Welch, 1947). Es especialmente recomendable",
             "cuando los tamaños de grupo son desiguales, porque en ese escenario la",
             "prueba t o el ANOVA clásicos son más sensibles a la heterogeneidad de",
             "varianzas que cuando los grupos tienen tamaños similares.",
             "",
             "Brown-Forsythe (como alternativa robusta al ANOVA):",
             "Aplica la misma lógica de Welch pero usando estimadores de tendencia",
             "central más robustos que la media, lo que la hace preferible cuando además",
             "de varianzas desiguales hay asimetría o casos atípicos en los grupos (Brown",
             "& Forsythe, 1974).",
             "",
             "Wilcoxon signed-rank — qué hace y cuándo usarla:",
             "Alternativa no paramétrica para dos mediciones relacionadas: en vez de",
             "comparar las diferencias pareadas directamente, las convierte en rangos y",
             "contrasta si la suma de rangos con signo positivo difiere de la esperada",
             "bajo H0 de simetría alrededor de cero (Wilcoxon, 1945). Requiere que las",
             "diferencias sean aproximadamente simétricas, aunque no normales; con",
             "asimetría fuerte en las diferencias, esta prueba también pierde validez.",
             "",
             "Friedman — qué hace y cuándo usarla:",
             "Extiende la misma lógica de rangos a tres o más mediciones relacionadas,",
             "convirtiendo cada fila (sujeto) en rangos entre las condiciones y",
             "contrastando si esos rangos difieren sistemáticamente entre condiciones",
             "(Friedman, 1937). Es la alternativa no paramétrica natural al ANOVA de",
             "medidas repetidas cuando la normalidad de las diferencias no es defendible.",
             "",
             "Errores robustos HC3 — qué hacen y cuándo usarlos:",
             "Recalculan los errores estándar de los coeficientes de una regresión de",
             "forma que sigan siendo válidos incluso si hay heterocedasticidad, sin",
             "cambiar las estimaciones puntuales de los coeficientes (MacKinnon & White,",
             "1985). HC3 es la variante recomendada en muestras pequeñas o moderadas,",
             "porque corrige mejor el sesgo de las estimaciones de varianza que otras",
             "variantes HC en ese régimen.",
             "",
             "Modelos mixtos:",
             "Adecuados para datos anidados, longitudinales, repetidos o con cualquier",
             "estructura de dependencia que un modelo de efectos fijos simple no pueda",
             "representar. En vez de corregir un supuesto violado después del hecho,",
             "modelan directamente la fuente de dependencia (por ejemplo, variación entre",
             "sujetos o entre grupos) como parte de la estructura del modelo.",
             "",
             "Decisión metodológica:",
             "No cambie de método por el resultado de una sola prueba aislada. La",
             "alternativa elegida debe responder al patrón conjunto de evidencia, al",
             "diseño del estudio y a la pregunta de investigación — no solo al p-valor de",
             "un diagnóstico puntual."
         ),

         notes = c(
             "Uso recomendado de Assumption Library",
             "",
             "Use esta biblioteca como referencia metodológica general.",
             "Use los análisis específicos para interpretar resultados con sus propios datos."
         )
     ),
     # ---- time: timeCheck module strings ----
     # ES: time: cadenas del módulo timeCheck
     time = list(
         title = "Series Temporales",
         intro = c(
             "Series Temporales relaciona los supuestos estadísticos y las condiciones",
             "metodológicas que deben evaluarse antes y después de ajustar un modelo de",
             "series temporales. A diferencia de un supuesto único, el modelado temporal",
             "exige revisar varias condiciones — de estacionariedad, de estructura de",
             "rezagos, de especificación y de residuos — cuya relevancia exacta depende del",
             "modelo elegido. Seleccione un modelo para ver su matriz metodológica: Modelo →",
             "Supuestos/condiciones → Diagnóstico → Función metodológica → Decisión."
         ),
         interpretation = c(
             "La matriz muestra únicamente las condiciones relevantes para el modelo",
             "seleccionado. No todas son requisitos matemáticos estrictos: se distinguen",
             "condiciones de aplicabilidad (deben cumplirse para que el modelo tenga sentido),",
             "criterios de especificación (orientan la elección de parámetros como p, d, q o",
             "el rango de cointegración) y diagnósticos posteriores al ajuste (evalúan si el",
             "modelo ya estimado capturó adecuadamente la estructura de los datos). Confundir",
             "estas categorías es un error metodológico frecuente: por ejemplo, la normalidad",
             "de los residuos en un ARIMA es un diagnóstico útil, no un requisito matemático",
             "estricto para la estimación."
         ),
         scopeNote = c(
             "Esta versión implementa la matriz metodológica (Modelo → Supuestos/",
             "condiciones → Diagnóstico → Función metodológica → Decisión), la batería",
             "mínima de diagnósticos estadísticos por modelo (pruebas de raíz unitaria,",
             "ACF/PACF, Ljung-Box, ARCH-LM, Jarque-Bera, y para VAR/VECM/GARCH también",
             "Johansen, Portmanteau, CUSUM/Nyblom y sesgo de signo, según el modelo",
             "seleccionado), gráficos diagnósticos (serie, ACF/PACF, residuos y, en GARCH,",
             "volatilidad condicional), y regresores externos opcionales en ARIMA/SARIMA",
             "(ARIMAX/SARIMAX). Cada resultado se clasifica como un nivel de evidencia",
             "gradual, nunca como un veredicto absoluto de \"supuesto cumplido/no cumplido\"."
         ),
         seriesRowLabel = "Serie temporal",
         modelRowLabel = "Modelo seleccionado",
         noSeriesSelected = "No se ha seleccionado una serie temporal.",
         modelLabels = list(
             arima = "ARIMA",
             sarima = "SARIMA",
             ets = "ETS / Suavizamiento exponencial",
             var = "VAR",
             vecm = "VECM",
             garch = "GARCH"
         ),
         modelDescriptions = list(
             arima = c(
                 "ARIMA (AutoRegressive Integrated Moving Average) modela una única serie",
                 "temporal a partir de su propio pasado: sus valores rezagados (componente",
                 "autorregresivo, p), el número de diferencias necesarias para volverla",
                 "estacionaria (componente integrado, d) y los errores rezagados (componente de",
                 "media móvil, q). Úselo cuando tenga una sola serie sin estacionalidad marcada,",
                 "o cuando quiera modelar una serie ya desestacionalizada. Ofrece pronósticos de",
                 "corto y mediano plazo con intervalos de confianza, y es la base conceptual de",
                 "SARIMA, ARIMAX y GARCH. No captura por sí solo relaciones con otras series",
                 "(para eso están VAR/VECM) ni cambios en la varianza a lo largo del tiempo",
                 "(para eso está GARCH)."
             ),
             sarima = c(
                 "SARIMA extiende ARIMA agregando un componente estacional (P, D, Q, s) que",
                 "modela explícitamente la repetición de patrones cada s períodos (por ejemplo,",
                 "cada 12 meses en datos mensuales). Úselo cuando la serie muestre un patrón",
                 "estacional claro y reconocible \u2014 ventas con picos navideños, consumo eléctrico",
                 "con ciclos diarios o semanales, turismo con temporada alta \u2014 que un ARIMA simple",
                 "no podría capturar. Es el modelo por defecto de este módulo precisamente porque",
                 "cubre el caso más frecuente en la práctica aplicada. A cambio de esa potencia",
                 "adicional, requiere más datos históricos (idealmente varios ciclos completos) y",
                 "una periodicidad (frecuencia) correctamente especificada."
             ),
             ets = c(
                 "ETS (Error, Trend, Seasonal) es una familia de modelos de suavizamiento",
                 "exponencial que descompone la serie en nivel, tendencia y estacionalidad,",
                 "cada uno con su propia forma (nula, aditiva o multiplicativa, con o sin",
                 "amortiguamiento). A diferencia de ARIMA/SARIMA, no se apoya en pruebas de raíz",
                 "unitaria ni en diferenciación: pondera las observaciones recientes más que las",
                 "antiguas de forma explícita. Es una alternativa fuerte cuando la serie tiene",
                 "tendencia y/o estacionalidad relativamente simples y estables, y suele ser más",
                 "fácil de interpretar y comunicar que un SARIMA equivalente. Es menos adecuado",
                 "cuando la dinámica depende de otras variables (use VAR/VECM/ARIMAX) o cuando",
                 "la volatilidad misma es el objeto de estudio (use GARCH)."
             ),
             var = c(
                 "VAR (Vector AutoRegressive) modela varias series simultáneamente, donde cada",
                 "una se explica por sus propios rezagos y por los rezagos de todas las demás",
                 "series del sistema, sin imponer de antemano cuál variable causa cuál. Úselo",
                 "cuando le interese la dinámica conjunta entre dos o más series estacionarias",
                 "\u2014por ejemplo, tasa de interés e inflación, o ventas de productos",
                 "complementarios\u2014 y quiera estudiar cómo un choque en una serie se propaga a",
                 "las demás (funciones de impulso-respuesta) sin especificar relaciones",
                 "causales rígidas. Requiere que todas las series sean estacionarias (o",
                 "tratadas para serlo); si las series comparten una relación de equilibrio de",
                 "largo plazo mientras cada una es no estacionaria individualmente, VECM suele",
                 "ser la opción más apropiada en su lugar."
             ),
             vecm = c(
                 "VECM (Vector Error Correction Model) es la versión de VAR apropiada cuando",
                 "las series no son estacionarias individualmente pero están cointegradas: existe",
                 "una combinación lineal de ellas que sí es estacionaria, reflejando una relación",
                 "de equilibrio de largo plazo (por ejemplo, entre el tipo de cambio y los",
                 "precios relativos, o entre consumo e ingreso). Úselo cuando la teoría o la",
                 "evidencia sugieran que las series se mueven juntas en el largo plazo aunque se",
                 "aparten de forma temporal en el corto plazo. Ofrece, además de la dinámica de",
                 "corto plazo típica de un VAR, un mecanismo explícito de corrección de error que",
                 "empuja al sistema de vuelta al equilibrio. Ajustar un VECM sin verificar antes",
                 "cointegración (con la prueba de Johansen) es un error metodológico frecuente:",
                 "sin cointegración real, un VAR en diferencias suele ser la opción correcta."
             ),
             garch = c(
                 "GARCH (Generalized AutoRegressive Conditional Heteroskedasticity) no modela",
                 "el nivel de la serie sino su varianza condicional: permite que la volatilidad",
                 "cambie en el tiempo y se agrupe en períodos de calma y de turbulencia, en vez de",
                 "asumirla constante. Úselo con series financieras o económicas de alta",
                 "frecuencia (retornos de activos, tipos de cambio, tasas) donde la magnitud de",
                 "las fluctuaciones varía sistemáticamente en el tiempo. Ofrece estimaciones de",
                 "riesgo/volatilidad que alimentan intervalos de predicción más realistas que un",
                 "ARIMA con varianza constante. Asume que la serie de entrada ya es",
                 "aproximadamente estacionaria en media (típicamente retornos, no niveles de",
                 "precio); si hay asimetría entre choques positivos y negativos, valga revisar",
                 "GJR-GARCH o EGARCH como alternativas."
             )
         ),
         matrix = list(
                arima = list(
                  c(
                      "Estacionariedad",
                      "Estacionariedad / comportamiento de raíz unitaria",
                      "ADF / KPSS / PP",
                      "Condición para determinar si la serie requiere diferenciación.",
                      "Orientar la diferenciación no estacional (d)."
                  ),
                  c(
                      "Dependencia temporal",
                      "Autocorrelación y autocorrelación parcial",
                      "ACF / PACF",
                      "Apoyar la identificación de los órdenes p y q.",
                      "Orientar la especificación ARIMA."
                  ),
                  c(
                      "Residuos",
                      "Independencia de los residuos",
                      "Ljung-Box",
                      "Comprobar si permanece estructura temporal tras el ajuste.",
                      "Revisar la especificación si existe autocorrelación residual."
                  ),
                  c(
                      "Residuos",
                      "Distribución de los residuos (diagnóstico)",
                      "Q-Q / Shapiro-Wilk",
                      "Evaluar la adecuación de la distribución de los errores.",
                      "Usar como diagnóstico complementario; no tratarlo como requisito matemático universal."
                  )
                ),
                sarima = list(
                  c(
                      "Estacionariedad",
                      "Estacionariedad no estacional / raíz unitaria",
                      "ADF / KPSS / PP",
                      "Determinar la necesidad de diferenciación no estacional.",
                      "Orientar d y la transformación de la serie."
                  ),
                  c(
                      "Estacionalidad",
                      "Estructura estacional",
                      "ACF / PACF / diagnóstico estacional",
                      "Determinar si existe dependencia sistemática en rezagos estacionales.",
                      "Orientar D y los órdenes estacionales P y Q."
                  ),
                  c(
                      "Dependencia temporal",
                      "Autocorrelación y autocorrelación parcial",
                      "ACF / PACF",
                      "Apoyar la identificación de los órdenes no estacionales p y q.",
                      "Orientar la especificación SARIMA."
                  ),
                  c(
                      "Residuos",
                      "Independencia de los residuos",
                      "Ljung-Box",
                      "Comprobar si permanece estructura temporal después del ajuste.",
                      "Revisar la especificación si existe autocorrelación residual."
                  ),
                  c(
                      "Residuos",
                      "Distribución de los residuos (diagnóstico)",
                      "Q-Q / Shapiro-Wilk",
                      "Evaluar la adecuación de la distribución de los errores.",
                      "Usar como diagnóstico complementario; no tratarlo como requisito matemático universal."
                  )
                ),
                ets = list(
                  c(
                      "Estructura temporal",
                      "Dependencia temporal y patrón sistemático",
                      "ACF / PACF / gráficos temporales",
                      "Identificar estructura que debe quedar explicada por nivel, tendencia y estacionalidad.",
                      "Orientar la especificación de los componentes ETS."
                  ),
                  c(
                      "Estacionalidad",
                      "Presencia y estabilidad del patrón estacional",
                      "ACF estacional / diagnóstico gráfico",
                      "Determinar si un componente estacional es necesario.",
                      "Orientar la especificación ETS."
                  ),
                  c(
                      "Residuos",
                      "Ausencia de autocorrelación residual",
                      "Ljung-Box / ACF residual",
                      "Comprobar que el modelo ha capturado la estructura temporal relevante.",
                      "Revisar la especificación si queda dependencia."
                  ),
                  c(
                      "Residuos",
                      "Distribución y varianza de los errores (diagnóstico)",
                      "Q-Q / ACF / variabilidad residual",
                      "Evaluar la adecuación de los errores para inferencia y pronóstico.",
                      "Considerar transformación o especificación alternativa cuando proceda."
                  )
                ),
                var = list(
                  c(
                      "Estacionariedad",
                      "Estacionariedad de las series del sistema",
                      "ADF / KPSS / PP",
                      "Determinar si las series son adecuadas para un VAR en niveles o requieren tratamiento.",
                      "Evitar especificaciones incompatibles con el orden de integración."
                  ),
                  c(
                      "Especificación",
                      "Estructura de rezagos",
                      "Criterios AIC / BIC / HQ + diagnóstico de rezagos",
                      "Determinar una longitud de rezago adecuada.",
                      "Seleccionar la especificación temporal del VAR."
                  ),
                  c(
                      "Residuos",
                      "Ausencia de autocorrelación residual",
                      "LM / Portmanteau / Ljung-Box multivariado",
                      "Comprobar que no queda dependencia temporal sistemática.",
                      "Revisar el número de rezagos o la especificación."
                  ),
                  c(
                      "Residuos",
                      "Heterocedasticidad residual (diagnóstico)",
                      "Pruebas de heterocedasticidad",
                      "Evaluar estabilidad de la varianza de los errores.",
                      "Considerar inferencia robusta o una especificación alternativa."
                  ),
                  c(
                      "Residuos",
                      "Normalidad multivariada (diagnóstico)",
                      "Mardia / diagnóstico Q-Q",
                      "Evaluar una condición útil para determinados procedimientos inferenciales.",
                      "Tratar como diagnóstico, no como requisito universal del VAR."
                  )
                ),
                vecm = list(
                  c(
                      "Integración",
                      "Series con orden de integración compatible",
                      "ADF / KPSS / PP",
                      "Determinar el orden de integración de las variables.",
                      "Verificar la condición previa para plantear cointegración."
                  ),
                  c(
                      "Cointegración",
                      "Existencia y rango de cointegración",
                      "Johansen",
                      "Determinar cuántas relaciones de equilibrio de largo plazo existen.",
                      "Orientar el rango de cointegración del VECM."
                  ),
                  c(
                      "Especificación",
                      "Estructura de rezagos",
                      "AIC / BIC / HQ",
                      "Determinar una longitud de rezago compatible con el sistema.",
                      "Orientar la especificación dinámica."
                  ),
                  c(
                      "Residuos",
                      "Ausencia de autocorrelación residual",
                      "LM / Portmanteau",
                      "Comprobar si queda dependencia temporal no explicada.",
                      "Revisar rezagos o especificación si existe autocorrelación."
                  ),
                  c(
                      "Residuos",
                      "Heterocedasticidad y normalidad (diagnóstico)",
                      "Pruebas de heterocedasticidad / Mardia",
                      "Evaluar las propiedades de los errores para inferencia.",
                      "Usar diagnósticos robustos cuando los supuestos distributivos no se sostienen."
                  )
                ),
                garch = list(
                  c(
                      "Varianza condicional",
                      "Efectos ARCH / heterocedasticidad condicional",
                      "ARCH-LM",
                      "Determinar si la varianza depende de información pasada.",
                      "Justificar la consideración de un modelo de volatilidad."
                  ),
                  c(
                      "Estacionariedad",
                      "Condición de estabilidad del proceso de varianza",
                      "Diagnóstico de parámetros / persistencia",
                      "Evaluar si la dinámica de la varianza es compatible con un proceso estable.",
                      "Revisar la especificación si la persistencia implica inestabilidad."
                  ),
                  c(
                      "Residuos estandarizados",
                      "Ausencia de autocorrelación en media",
                      "ACF / Ljung-Box",
                      "Comprobar que la dinámica de la media ha sido adecuadamente capturada.",
                      "Revisar la ecuación de media si queda dependencia."
                  ),
                  c(
                      "Residuos estandarizados",
                      "Ausencia de ARCH residual",
                      "ARCH-LM / ACF de residuos al cuadrado",
                      "Comprobar que la dinámica de volatilidad ha sido capturada.",
                      "Revisar la especificación GARCH si queda efecto ARCH."
                  ),
                  c(
                      "Distribución",
                      "Distribución de las innovaciones (diagnóstico)",
                      "Q-Q / Jarque-Bera / distribución elegida",
                      "Evaluar la adecuación de la distribución asumida para las innovaciones.",
                      "Considerar una distribución alternativa cuando proceda."
                  )
                )
         ),
         actions = list(
             arima = list(
                 stationarity = list(justification = "",
                     suggestion = "aplicar el número de diferencias regulares sugerido (d) antes de ajustar el modelo, o dejar que auto.arima lo determine automáticamente; alternativamente, usar una transformación estabilizadora de varianza"),
                 ndiffs = list(justification = "",
                     suggestion = "aplicar el número de diferencias regulares sugerido (d) antes de ajustar el modelo"),
                 ljungBox = list(justification = "",
                     suggestion = "aumentar el orden AR/MA (p, q) o revisar si falta algún componente estructural, considerando un ARIMA con más rezagos"),
                 archLM = list(justification = "Los residuos muestran heterocedasticidad condicional.",
                     suggestion = "modelar la varianza (ARIMA-GARCH) o usar errores robustos para la inferencia"),
                 jarqueBera = list(justification = "",
                     suggestion = "usar innovaciones t-Student o bootstrap para intervalos y pruebas, en vez de asumir normalidad de los errores"),
                 roots = list(justification = "El modelo ajustado no resulta estacionario/invertible.",
                     suggestion = "revisar el orden ARIMA o la diferenciación aplicada")
             ),
             sarima = list(
                 stationarity = list(justification = "",
                     suggestion = "aplicar el número de diferencias regulares sugerido (d) antes de ajustar el modelo, o considerar una transformación estabilizadora de varianza"),
                 ndiffs = list(justification = "",
                     suggestion = "aplicar el número de diferencias regulares sugerido (d) antes de ajustar el modelo"),
                 nsdiffs = list(justification = "",
                     suggestion = "aplicar una diferencia estacional (D) según lo sugerido antes de proceder"),
                 ljungBox = list(justification = "",
                     suggestion = "aumentar el orden AR/MA no estacional (p, q) o revisar si falta algún componente estructural"),
                 ljungBoxSeasonal = list(justification = "Puede faltar estructura estacional por capturar.",
                     suggestion = "revisar el componente estacional (P, D, Q)"),
                 archLM = list(justification = "Los residuos muestran heterocedasticidad condicional.",
                     suggestion = "modelar la varianza (SARIMA-GARCH) o usar errores robustos para la inferencia"),
                 jarqueBera = list(justification = "",
                     suggestion = "usar innovaciones t-Student o bootstrap para intervalos y pruebas, en vez de asumir normalidad de los errores"),
                 roots = list(justification = "El modelo ajustado no resulta estacionario/invertible.",
                     suggestion = "revisar el orden SARIMA o la diferenciación aplicada")
             ),
             ets = list(
                 ljungBox = list(justification = "",
                     suggestion = "incorporar un componente de error, tendencia o estacionalidad adicional en la especificación ETS"),
                 archLM = list(justification = "Los residuos muestran heterocedasticidad condicional.",
                     suggestion = "transformar la serie o considerar un modelo con varianza no constante"),
                 jarqueBera = list(justification = "",
                     suggestion = "usar bootstrap o innovaciones no normales para intervalos y pronósticos, en vez de asumir normalidad de los errores")
             ),
             var = list(
                 stationarity = list(justification = "",
                     suggestion = "evaluar un VAR en diferencias o cointegración (VECM) en lugar de un VAR en niveles, si varias series no son estacionarias"),
                 stability = list(justification = "El sistema resulta inestable.",
                     suggestion = "reducir el orden de rezago, revisar la especificación, o diferenciar las series"),
                 serial = list(justification = "La correlación serial residual sugiere que el orden actual es insuficiente.",
                     suggestion = "aumentar el número de rezagos del VAR"),
                 archLM = list(justification = "",
                     suggestion = "considerar un VAR con errores robustos a heterocedasticidad o un modelo multivariante de volatilidad (por ejemplo, BEKK-GARCH)"),
                 jarqueBera = list(justification = "",
                     suggestion = "usar inferencia robusta (bootstrap) en vez de depender de la normalidad multivariante para las pruebas de hipótesis"),
                 cusum = list(justification = "Al menos una ecuación muestra inestabilidad paramétrica.",
                     suggestion = "considerar un VAR con cambio estructural o estimar por sub-períodos")
             ),
             vecm = list(
                 stationarity = list(justification = "",
                     suggestion = "confirmar que las series sean integradas de orden 1 antes de proceder con VECM; si alguna resulta estacionaria en niveles, reconsiderar su inclusión"),
                 johansen = list(justification = "",
                     suggestion = "usar el rango de cointegración sugerido como punto de partida, confirmándolo con el juicio teórico y la matriz de traza completa antes de fijarlo"),
                 stability = list(justification = "El sistema convertido a VAR resulta inestable.",
                     suggestion = "revisar el rango de cointegración o el orden de rezago"),
                 serial = list(justification = "La correlación serial residual sugiere estructura no capturada.",
                     suggestion = "aumentar el orden de rezago K del VECM"),
                 archLM = list(justification = "",
                     suggestion = "considerar una especificación con errores robustos a heterocedasticidad"),
                 jarqueBera = list(justification = "",
                     suggestion = "usar inferencia robusta (bootstrap) en vez de depender de la normalidad multivariante")
             ),
             garch = list(
                 stationarity = list(justification = "",
                     suggestion = "transformar la serie a retornos (log-diferencias) antes de ajustar el GARCH, si no resulta estacionaria"),
                 ljungBox = list(justification = "La dinámica de la media condicional puede no estar bien capturada.",
                     suggestion = "incorporar una estructura de media (ARMA) antes del componente GARCH"),
                 archLM = list(justification = "El GARCH(1,1) no capturó toda la heterocedasticidad condicional.",
                     suggestion = "considerar un orden GARCH(p,q) mayor"),
                 persistence = list(justification = "",
                     suggestion = "considerar un modelo IGARCH o revisar la longitud de la muestra, dado que la persistencia es muy alta (cercana a 1)"),
                 signBias = list(justification = "Hay evidencia de asimetría en la respuesta de la varianza a choques positivos/negativos.",
                     suggestion = "evaluar GJR-GARCH o EGARCH en vez de un GARCH simétrico"),
                 nyblom = list(justification = "Los parámetros no resultan estables en el tiempo.",
                     suggestion = "reestimar por sub-períodos o considerar un modelo con parámetros cambiantes"),
                 gof = list(justification = "El ajuste de la distribución no resulta adecuado.",
                     suggestion = "considerar innovaciones t-Student o de error generalizado (GED) en vez de la normal")
             )
         ),
         # ------------------------------------------------------------------
         # Lectura metodológica sintetizada por dominio de diagnóstico. Cada
         # dominio agrupa uno o más action_key de push_diag() (ver
         # domain_groups en timecheck.b.R) y provee plantillas para el caso
         # de una sola prueba en el dominio (single, por nivel de evidencia)
         # y para el caso de varias pruebas coincidiendo (all_none/
         # all_deviant) o discrepando (mixed). Un dominio sin entrada aquí
         # simplemente se omite del panel "Lectura metodológica" — no rompe
         # nada, solo produce un párrafo menos. Piloto: solo "stationarity"
         # poblado por ahora; el resto se agrega de forma incremental tras
         # confirmar este primero en jamovi real.
         # ------------------------------------------------------------------
         reading = list(
             stationarity = list(
                 single = list(
                     none = "La prueba %s no encuentra evidencia de raíz unitaria/no estacionariedad en la serie, lo que respalda tratarla como estacionaria.",
                     moderate = "La prueba %s ofrece evidencia moderada de no estacionariedad en la serie.",
                     clear = "La prueba %s ofrece evidencia clara de no estacionariedad en la serie.",
                     info = "La prueba %s se reporta con fines informativos.",
                     na = "La prueba %s no pudo calcularse para esta corrida."
                 ),
                 all_none = "Las %d pruebas de raíz unitaria/estacionariedad ejecutadas coinciden en no mostrar evidencia de no estacionariedad, lo que respalda tratar la serie como estacionaria.",
                 all_deviant = "Las %d pruebas de raíz unitaria/estacionariedad ejecutadas coinciden en señalar evidencia de no estacionariedad.",
                 mixed = "De las %2$d pruebas de raíz unitaria/estacionariedad ejecutadas, %1$d no muestran evidencia de no estacionariedad, mientras que %3$s sí la %4$s; la evidencia sobre la estacionariedad de la serie es mixta."
             ),
             residualAutocorr = list(
                 single = list(
                     none = "La prueba %s no encuentra evidencia de autocorrelación residual, lo que respalda que el modelo capturó adecuadamente la estructura temporal.",
                     moderate = "La prueba %s ofrece evidencia moderada de autocorrelación residual no capturada por el modelo.",
                     clear = "La prueba %s ofrece evidencia clara de autocorrelación residual no capturada por el modelo.",
                     info = "La prueba %s se reporta con fines informativos.",
                     na = "La prueba %s no pudo calcularse para esta corrida."
                 ),
                 all_none = "Las %d pruebas de autocorrelación residual ejecutadas coinciden en no mostrar evidencia de estructura temporal remanente, lo que respalda la especificación actual del modelo.",
                 all_deviant = "Las %d pruebas de autocorrelación residual ejecutadas coinciden en señalar estructura temporal no capturada por el modelo.",
                 mixed = "De las %2$d pruebas de autocorrelación residual ejecutadas, %1$d no muestran evidencia de estructura remanente, mientras que %3$s sí la %4$s; la evidencia sobre la adecuación de la especificación es mixta."
             ),
             heteroscedasticity = list(
                 single = list(
                     none = "La prueba %s no encuentra evidencia de heterocedasticidad condicional en los residuos.",
                     moderate = "La prueba %s ofrece evidencia moderada de heterocedasticidad condicional no capturada en los residuos.",
                     clear = "La prueba %s ofrece evidencia clara de heterocedasticidad condicional no capturada en los residuos.",
                     info = "La prueba %s se reporta con fines informativos.",
                     na = "La prueba %s no pudo calcularse para esta corrida."
                 ),
                 all_none = "Las %d pruebas de heterocedasticidad condicional ejecutadas coinciden en no mostrar evidencia de varianza residual no capturada.",
                 all_deviant = "Las %d pruebas de heterocedasticidad condicional ejecutadas coinciden en señalar varianza residual no capturada.",
                 mixed = "De las %2$d pruebas de heterocedasticidad condicional ejecutadas, %1$d no muestran evidencia de varianza residual no capturada, mientras que %3$s sí la %4$s; la evidencia es mixta."
             ),
             normality = list(
                 single = list(
                     none = "La prueba %s no encuentra evidencia de desviación de la normalidad en los residuos.",
                     moderate = "La prueba %s ofrece evidencia moderada de no normalidad en los residuos.",
                     clear = "La prueba %s ofrece evidencia clara de no normalidad en los residuos; recuerde que esto es un diagnóstico complementario, no un requisito matemático estricto para la mayoría de los modelos aquí cubiertos.",
                     info = "La prueba %s se reporta con fines informativos.",
                     na = "La prueba %s no pudo calcularse para esta corrida."
                 ),
                 all_none = "Las %d pruebas de normalidad ejecutadas coinciden en no mostrar evidencia de desviación de la normalidad en los residuos.",
                 all_deviant = "Las %d pruebas de normalidad ejecutadas coinciden en señalar desviación de la normalidad en los residuos.",
                 mixed = "De las %2$d pruebas de normalidad ejecutadas, %1$d no muestran evidencia de desviación de la normalidad, mientras que %3$s sí la %4$s; la evidencia es mixta."
             ),
             stability = list(
                 single = list(
                     none = "El diagnóstico %s no muestra evidencia de inestabilidad en el modelo ajustado.",
                     moderate = "El diagnóstico %s ofrece evidencia moderada de inestabilidad en el modelo ajustado.",
                     clear = "El diagnóstico %s ofrece evidencia clara de inestabilidad en el modelo ajustado.",
                     info = "El diagnóstico %s se reporta con fines informativos.",
                     na = "El diagnóstico %s no pudo calcularse para esta corrida."
                 ),
                 all_none = "Los %d diagnósticos de estabilidad ejecutados coinciden en no mostrar evidencia de inestabilidad en el modelo ajustado.",
                 all_deviant = "Los %d diagnósticos de estabilidad ejecutados coinciden en señalar inestabilidad en el modelo ajustado.",
                 mixed = "De los %2$d diagnósticos de estabilidad ejecutados, %1$d no muestran evidencia de inestabilidad, mientras que %3$s sí la %4$s; la evidencia sobre la estabilidad del modelo es mixta."
             ),
             cointegration = list(
                 single = list(
                     none = "%s no encuentra evidencia de cointegración entre las series.",
                     moderate = "%s ofrece evidencia moderada de cointegración entre las series.",
                     clear = "%s ofrece evidencia clara de cointegración entre las series.",
                     info = "%s sugiere, de forma informativa, un rango de cointegración.",
                     na = "%s no pudo calcularse para esta corrida."
                 ),
                 informational = "Las pruebas de Johansen ejecutadas (traza y máximo autovalor) sugieren, de forma informativa, un rango de cointegración; conviene contrastarlo con el juicio teórico y la matriz de traza completa antes de fijarlo.",
                 all_none = "Las %d pruebas de cointegración ejecutadas coinciden en no mostrar evidencia de una relación de cointegración.",
                 all_deviant = "Las %d pruebas de cointegración ejecutadas coinciden en señalar una relación de cointegración entre las series.",
                 mixed = "De las %2$d pruebas de cointegración ejecutadas, %1$d no muestran evidencia de cointegración, mientras que %3$s sí la %4$s; la evidencia es mixta."
             ),
             garchPersistence = list(
                 single = list(
                     none = "%s se encuentra dentro de un rango típico, sin indicios de una varianza casi integrada.",
                     moderate = "%s se reporta con un nivel de evidencia moderado.",
                     clear = "%s resulta muy alta, cercana a la integración, lo que sugiere que los choques de volatilidad se disipan muy lentamente.",
                     info = "%s se reporta con fines informativos.",
                     na = "La persistencia de la varianza condicional no pudo calcularse para esta corrida."
                 )
             ),
             garchAsymmetry = list(
                 single = list(
                     none = "La prueba %s no encuentra evidencia de asimetría en la respuesta de la varianza a choques positivos/negativos.",
                     moderate = "La prueba %s ofrece evidencia moderada de asimetría en la respuesta de la varianza.",
                     clear = "La prueba %s ofrece evidencia clara de asimetría en la respuesta de la varianza a choques positivos/negativos.",
                     info = "La prueba %s se reporta con fines informativos.",
                     na = "La prueba de sesgo de signo no pudo calcularse para esta corrida."
                 )
             ),
             garchGof = list(
                 single = list(
                     none = "La prueba %s no encuentra evidencia de que la distribución asumida para las innovaciones sea inadecuada.",
                     moderate = "La prueba %s ofrece evidencia moderada de que la distribución asumida no se ajusta bien.",
                     clear = "La prueba %s ofrece evidencia clara de que la distribución asumida no se ajusta bien a las innovaciones.",
                     info = "La prueba %s se reporta con fines informativos.",
                     na = "La prueba de bondad de ajuste no pudo calcularse para esta corrida."
                 )
             )
         ),
         foundations = list(
             unitRoot = c(
                 "Raíces unitarias (ADF, Phillips-Perron, KPSS)",
                 "ADF (Dickey y Fuller, 1979) y Phillips-Perron (Phillips y Perron, 1988) comparten la",
                 "misma hipótesis nula \u2014 la serie tiene raíz unitaria, es decir, es no estacionaria \u2014,",
                 "pero corrigen de forma distinta la posible autocorrelación y heterocedasticidad de los",
                 "errores: ADF agrega rezagos de la diferencia como regresores; Phillips-Perron ajusta",
                 "el estadístico con una corrección no paramétrica tipo Newey-West. KPSS (Kwiatkowski",
                 "et al., 1992) invierte la lógica: su hipótesis nula es que la serie",
                 "SÍ es estacionaria. Por eso las tres pruebas se reportan juntas: si ADF y",
                 "Phillips-Perron rechazan la raíz unitaria (p pequeño) y KPSS no rechaza la",
                 "estacionariedad (p grande), la evidencia converge con claridad. Cuando las",
                 "conclusiones de ambos tipos de prueba difieren, suele indicar una serie con",
                 "componentes de largo plazo débiles o una muestra insuficiente para distinguir entre",
                 "ambas hipótesis, no un error de cómputo."
             ),
             seasonalUnitRoot = c(
                 "Raíz unitaria estacional (Canova-Hansen, OCSB)",
                 "Canova y Hansen (1995) prueban si el patrón estacional es estable en el tiempo o si",
                 "cambia lo suficiente como para requerir diferenciación estacional; forecast::nsdiffs()",
                 "usa esta prueba por defecto. OCSB (Osborn et al., 1988) evalúa",
                 "una hipótesis relacionada mediante una regresión auxiliar con rezagos estacionales y",
                 "no estacionales simultáneos. Ambas pruebas suelen coincidir en series con",
                 "estacionalidad clara; cuando difieren, es razonable preferir la sugerencia de",
                 "Canova-Hansen para el orden D en un SARIMA, dado que es la implementada por",
                 "defecto en la literatura de pronóstico automático (Hyndman y Khandakar, 2008), y",
                 "usar OCSB como verificación cruzada."
             ),
             ljungBox = c(
                 "Independencia residual (Ljung-Box)",
                 "Ljung y Box (1978) mejoraron la prueba de Box-Pierce original corrigiendo su",
                 "aproximación asintótica, especialmente relevante en muestras pequeñas o moderadas.",
                 "La prueba evalúa conjuntamente varios rezagos de autocorrelación residual a la vez,",
                 "no uno solo; por eso el número de rezagos elegido importa: muy pocos pueden no",
                 "detectar dependencia real, y demasiados diluyen el poder de la prueba al incluir",
                 "rezagos sin autocorrelación genuina junto a los que sí la tienen."
             ),
             archGarch = c(
                 "Heterocedasticidad condicional (ARCH-LM, GARCH)",
                 "Engle (1982) introdujo los modelos ARCH a partir de una observación empírica",
                 "central en series financieras y macroeconómicas: los períodos de alta volatilidad",
                 "tienden a agruparse en el tiempo, en vez de distribuirse al azar. El multiplicador",
                 "de Lagrange (ARCH-LM) prueba precisamente eso: si la varianza de los residuos en un",
                 "momento dado depende de su propia magnitud en momentos anteriores. Bollerslev (1986)",
                 "generalizó el modelo a GARCH, permitiendo que la varianza condicional dependa",
                 "también de sus propios valores rezagados y no solo de los choques pasados, lo que",
                 "reduce drásticamente el número de parámetros necesarios frente a un ARCH de orden",
                 "alto para capturar la misma persistencia."
             ),
             jarqueBera = c(
                 "Normalidad de los residuos (Jarque-Bera)",
                 "Jarque y Bera (1987) construyeron una prueba conjunta de asimetría y curtosis: bajo",
                 "normalidad, ambas deberían ser cercanas a cero (curtosis en su forma de exceso).",
                 "El estadístico combina las dos desviaciones en una sola prueba chi-cuadrado con 2",
                 "grados de libertad. Es, ante todo, un diagnóstico útil sobre la forma de los",
                 "residuos, no un requisito matemático estricto para estimar la mayoría de los",
                 "modelos temporales cubiertos aquí; su relevancia práctica depende de si el",
                 "procedimiento de inferencia posterior (intervalos, pronósticos) asume explícitamente",
                 "normalidad de los errores."
             ),
             cointegration = c(
                 "Cointegración (Johansen, Engle-Granger)",
                 "Engle y Granger (1987) formalizaron la idea de cointegración: aunque cada serie sea",
                 "individualmente no estacionaria (integrada de orden 1), una combinación lineal",
                 "específica de varias de ellas puede ser estacionaria, reflejando una relación de",
                 "equilibrio de largo plazo. Johansen (1991) extendió esta idea a un marco de máxima",
                 "verosimilitud dentro de un VAR, permitiendo probar formalmente cuántas relaciones",
                 "de cointegración independientes existen (el rango) mediante las pruebas de traza y",
                 "de máximo autovalor, en vez de estimar una sola relación como en el procedimiento",
                 "de dos pasos original de Engle-Granger."
             ),
             varFoundations = c(
                 "Especificación VAR/VECM",
                 "El tratamiento de referencia para la selección de rezagos, el diagnóstico residual",
                 "multivariante y la conversión entre representaciones VAR y VECM sigue el marco",
                 "expuesto por Lütkepohl (2005), el texto estándar sobre modelos de series",
                 "temporales multivariadas. Los criterios de información (AIC, BIC, HQ) usados por",
                 "VARselect() compiten entre sí en un mismo sentido: AIC tiende a favorecer modelos",
                 "más parametrizados (más rezagos), mientras que BIC penaliza la complejidad con más",
                 "severidad y suele sugerir especificaciones más parsimoniosas; cuando ambos",
                 "coinciden, la elección del orden es más confiable."
             ),
             parameterStability = c(
                 "Estabilidad paramétrica (CUSUM, Nyblom)",
                 "Brown et al., (1975) propusieron el CUSUM de residuos recursivos para",
                 "detectar si los coeficientes de una regresión permanecen constantes a lo largo de",
                 "la muestra o si existe un cambio estructural en algún punto no especificado de",
                 "antemano. Nyblom (1989) generalizó la idea a un marco donde los parámetros pueden",
                 "variar gradualmente como un proceso martingala, en vez de solo en un punto de",
                 "quiebre discreto, lo que lo hace especialmente adecuado para evaluar la estabilidad",
                 "conjunta de los parámetros de un modelo GARCH ya ajustado."
             ),
             asymmetricGarch = c(
                 "Asimetría en la volatilidad (sesgo de signo, GJR, EGARCH)",
                 "Un GARCH simétrico asume que un choque positivo y uno negativo de la misma magnitud",
                 "afectan la varianza futura por igual. Engle y Ng (1993) diseñaron la prueba de",
                 "sesgo de signo precisamente para detectar cuándo esa suposición falla \u2014 un patrón",
                 "muy común en retornos accionarios, donde las caídas suelen elevar la volatilidad más",
                 "que las subidas de igual tamaño (efecto apalancamiento). Cuando la prueba es",
                 "significativa, dos familias de modelos responden a esa asimetría de formas",
                 "distintas: GJR-GARCH (Glosten et al., 1993) agrega un término que",
                 "solo se activa ante choques negativos, mientras que EGARCH (Nelson, 1991) modela",
                 "el logaritmo de la varianza, lo que además evita tener que restringir los",
                 "parámetros a valores positivos para garantizar una varianza válida."
             ),
             autoArima = c(
                 "Selección automática de orden (Hyndman-Khandakar)",
                 "El algoritmo detrás de forecast::auto.arima() (Hyndman y Khandakar, 2008) no prueba",
                 "exhaustivamente todas las combinaciones de p, d, q: parte de los órdenes de",
                 "diferenciación sugeridos por las pruebas de raíz unitaria y luego realiza una",
                 "búsqueda escalonada (stepwise) sobre el espacio de modelos vecinos, comparando por",
                 "AICc. Esto lo hace mucho más rápido que una búsqueda exhaustiva, pero significa que",
                 "el resultado es un óptimo local razonable, no garantizado como el mejor modelo",
                 "posible; con series difíciles vale la pena contrastar el resultado contra la",
                 "inspección visual de la ACF/PACF."
             )
         )
     )
 ),
 en = list(
     # ---- common: shared strings used across every module ----
     # ES: common: cadenas compartidas usadas en todos los módulos
     common = list(
         moduleTitle = "AssumptionsLab",
         briefGuide = "Brief guide",
         appliedInterpretation = "Applied interpretation",
         methodologicalDecision = "Methodological decision",
         notes = "Notes",
         references = "References",
         seeLibrary = "See Assumption Library for methodological details.",
         doNotDeleteAutomatically = "A flagged case should not be removed automatically.",
         reviewAndJustify = "Review the original value and justify any decision.",
         notComputed = "Not computed",
         compatible = "Compatible",
         significantDeviation = "Significant deviation",
         sigCodes = "Significance codes: * p < .05, ** p < .01, *** p < .001.",
         statsSymbols = "Statistical symbols are kept in international format.",
         colVariable = "Variable",
         colGroup = "Group",
         colTest = "Test",
         colN = "n",
         colMissing = "Missing",
         colMean = "M",
         colSD = "SD",
         colMedian = "Mdn",
         colMin = "Min",
         colMax = "Max",
         colQ1 = "Q1",
         colQ3 = "Q3",
         colIQR = "IQR",
         colLowerLimit = "Lower limit",
         colUpperLimit = "Upper limit",
         colOutliers = "Outliers",
         colExtreme = "Extreme",
         colStatistic = "Statistic",
         colValue = "Value",
         colP = "p",
         colSig = "Sig.",
         colDf = "df",
         colDf1 = "df1",
         colDf2 = "df2",
         colInterpretation = "Interpretation",
         colConclusion = "Conclusion"
     ),
     # ---- independentGroups: groupCheck module strings ----
     # ES: independentGroups: cadenas del módulo groupCheck
     independentGroups = list(
         title = "Independent Groups",
         intro = c(
             "Use this analysis when you want to review whether a comparison between independent",
             "groups has defensible methodological assumptions.",
             "The goal is not only to compute tests, but to help justify the statistical decision",
             "with evidence obtained from your own data."
         ),
         tableDesign = "Design summary",
         tableDescriptives = "Descriptive statistics by group",
         tableOutliers = "Outlier evaluation by group",
         tableCaseDiagnostics = "Case diagnostics",
         tableNormality = "Normality tests by group",
         tableNormalitySummary = "Normality decision summary",
         tableVariance = "Variance homogeneity",
         designGuide = c(
             "Use this review when rows represent independent units and each unit belongs to one",
             "group only.",
             "Before choosing Student's t, ANOVA, Welch or Kruskal-Wallis, review group sizes,",
             "missing data, normality, variances and potentially influential cases.",
             "The analysis is more defensible when the decision is based on the whole body of",
             "evidence rather than on a single isolated test."
         ),
         descriptivesGuide = c(
             "Descriptive statistics allow you to observe initial group differences before applying",
             "an inferential test.",
             "Compare means, medians and dispersion. Large differences between mean and median may",
             "suggest skewness or the influence of extreme values.",
             "Descriptives do not test hypotheses by themselves, but they guide the reading of",
             "assumptions."
         ),
         outliersGuide = c(
             "The IQR rule identifies unusual values within each group.",
             "An outlier is not necessarily an error: it may be a real, rare or influential case.",
             "In small groups, a few extreme values can modify means, variances and normality tests.",
             "Review the original data before deciding whether to run sensitivity analyses."
         ),
         caseDiagnosticsGuide = c(
             "These diagnostics identify potentially influential or unusual cases in the group",
             "comparison model.",
             "Cook's D flags cases that may change model estimates.",
             "Mahalanobis D² helps detect values that are far from the expected pattern.",
             "A flagged case should not be removed automatically: review the value, compare analyses",
             "with and without the case, and justify the decision."
         ),
         normalityGuide = c(
             "Normality is reviewed within each independent group because parametric procedures",
             "assume approximately normal distributions at each level of the factor.",
             "p >= .05 is compatible with approximate normality; this does not prove perfect",
             "normality.",
             "p < .05 suggests a significant deviation from normality.",
             "In small samples, complement tests with plots and outlier review."
         ),
         varianceGuide = c(
             "These tests review whether group variances are reasonably similar.",
             "p >= .05 is compatible with homogeneous variances.",
             "p < .05 suggests significant variance differences.",
             "If group sizes are unequal, variance heterogeneity can affect interpretation more",
             "strongly and may make Welch or robust methods preferable."
         ),
         normalityCompatible = "Compatible with approximate normality",
         normalityDeviation5 = "Significant deviation at 5%",
         normalityDeviation1 = "Significant deviation at 1%",
         normalityDeviation001 = "Significant deviation at 0.1%",
         normalitySummaryCompatible = "Compatible with approximate normality",
         normalitySummaryDeviation = "Significant deviation in at least one test",
         varianceCompatible = "Compatible with homogeneous variances",
         varianceDeviation = "Significant variance differences",
         recommendationParametricCaution = c(
             "The parametric analysis may be defensible with caution if the design is truly",
             "independent, variances are compatible with homogeneity and normality does not show",
             "severe deviations.",
             "If there are small groups, relevant imbalance or influential cases, review sensitivity",
             "analyses before making the final decision."
         ),
         recommendationWelchOrRobust = c(
             "Consider Welch, Brown-Forsythe, Kruskal-Wallis or a robust alternative if there is",
             "variance heterogeneity, clear normality deviation or influential cases that change",
             "the conclusions.",
             "The choice should combine statistical evidence, study design and the research",
             "question."
         )
     ),
     # ---- regression: regCheck module strings ----
     # ES: regression: cadenas del módulo regCheck
     regression = list(
         title = "Simple & Multiple Regression",
         intro = c(
             "Use this analysis to review whether a simple or multiple linear regression model",
             "has defensible methodological assumptions.",
             "The goal is not only to detect problems, but to teach how each diagnostic affects",
             "the interpretation of coefficients, standard errors, confidence intervals and p-values.",
             "The final decision should combine statistical evidence, study design, data quality",
             "and the substantive meaning of the model."
         ),
         designGuide = c(
             "Linear regression evaluates the relationship between a numeric dependent variable and one or more predictors.",
             "Before interpreting coefficients, review whether the dependent variable is quantitative, whether predictors are correctly defined and whether the sample size is reasonable for the number of estimated parameters.",
             "A model can be computed even when assumptions are weak, but its conclusions may be less defensible if those problems are not documented."
         ),
         linearityGuide = c(
             "Linearity evaluates whether the relationship between numeric predictors and the dependent variable can be reasonably described by a straight line.",
             "This assumption matters because a linear model may produce misleading coefficients when the true relationship is curved, stepped or changes direction.",
             "Tests and exploratory terms help detect curvature, but they should be read together with residual plots and substantive knowledge.",
             "If there is evidence of nonlinearity, consider transformations, polynomial terms, splines or another type of model."
         ),
         residualNormalityGuide = c(
             "Normality in regression is evaluated on residuals, not on the raw dependent variable.",
             "This assumption mainly affects classical inferences such as confidence intervals and significance tests, especially in small samples.",
             "p >= .05 is compatible with approximate residual normality; it does not prove perfect normality.",
             "p < .05 suggests a significant deviation.",
             "Review whether it is due to skewness, heavy tails, outliers or model misspecification."
         ),
         homoscedasticityGuide = c(
             "Homoscedasticity evaluates whether residual variability is approximately constant across fitted values.",
             "When this assumption is weakened, coefficients may still be useful, but standard errors, confidence intervals and p-values may become unreliable.",
             "p >= .05 is compatible with approximate homoscedasticity.",
             "p < .05 suggests heteroscedasticity.",
             "In that case, consider robust standard errors, transformations or alternative models."
         ),
         independenceGuide = c(
             "Error independence means residuals should not be correlated by measurement order, time, space, group, classroom, participant or another structure.",
             "This assumption depends mainly on study design, not only on a statistical test.",
             "The Durbin-Watson test helps review first-order autocorrelation when data have a natural order.",
             "If observations are clustered, repeated or nested, consider mixed, longitudinal or other methods that represent that dependence."
         ),
         multicollinearityGuide = c(
             "Multicollinearity evaluates whether predictors contain highly redundant information.",
             "It usually does not bias predictions, but it can make coefficients unstable, increase standard errors and complicate interpretation of individual predictors.",
             "VIF values close to 1 suggest low collinearity.",
             "High values indicate that a predictor is too well explained by other predictors.",
             "If collinearity is important, consider combining variables, removing redundant predictors, centering variables or changing the analytic question."
         ),
         correlationMatrixGuide = c(
             "These two tables complement the multicollinearity checks with an overview of the association between the dependent variable and all numeric predictors, each in APA 7 format (lower triangle, numbered variables).",
             "The first reports conventional Pearson correlation (linear association only); the second reports distance correlation (Szekely et al., 2007), which detects linear and non-linear association alike.",
             "The discordance table flags pairs where dCor notably exceeds |Pearson r|, with copula entropy (copent) as a second line of evidence."
         ),
         influenceGuide = c(
             "Influence diagnostics identify cases that may substantially change coefficients, fit or model conclusions.",
             "Studentized residuals, leverage, Cook's D and DFFITS review different aspects of the same problem: how unusual a case is and how much it changes the model.",
             "An influential case should not be removed automatically.",
             "Review whether it is a data entry error, a valid but extreme case, or a sign that the model does not represent all subgroups well."
         ),
         transformationsGuide = c(
             "Transformations may help when there is strong skewness, nonlinear relationships or nonconstant residual variance.",
             "They should not be used only to improve an assumption test; they should make statistical and substantive sense.",
             "When a variable is transformed, the interpretation of the coefficient also changes.",
             "Document which transformation was used, why it was needed and how the resulting model should be interpreted."
         ),
         robustOptionsGuide = c(
             "Robust options are not an automatic correction, but alternatives when classical assumptions are not sufficiently defensible.",
             "Robust standard errors may help under heteroscedasticity.",
             "Robust models may reduce the influence of extreme cases.",
             "Mixed, generalized or nonlinear models may be more appropriate if the problem comes from design, distribution or functional form."
         )
     ),
     # ---- logistic: logCheck module strings ----
     # ES: logistic: cadenas del módulo logCheck
     logistic = list(
         title = "Logistic Regression",
         intro = c(
             "Use this analysis to review whether a binary logistic regression model has defensible",
             "methodological assumptions before interpreting odds ratios, coefficients or",
             "classifications.",
             "The goal is not only to detect problems, but to teach how each diagnostic affects the",
             "validity of conclusions: complete separation, sample size per variable, linearity in",
             "the logit, goodness of fit, discrimination, multicollinearity and the influence of",
             "individual cases."
         ),
         designGuide = c(
             "Logistic regression models the probability of a binary event as a function of one or more predictors.",
             "Before interpreting the model, check that the dependent variable has exactly two categories, that events and non-events are reasonably represented, and that the ratio of events per predictor variable (EPV) is sufficient.",
             "Peduzzi et al.",
             "(1996) recommend at least 10 events per predictor variable as a practical reference; with low EPV, coefficients can be biased and standard errors can be unreliable."
         ),
         separationGuide = c(
             "Complete or quasi-complete separation occurs when a predictor (or combination of predictors) perfectly or nearly perfectly classifies the events.",
             "When this happens, the maximum-likelihood algorithm does not converge stably: coefficients and standard errors can inflate to extreme values with no substantive meaning.",
             "Very large coefficients together with very large standard errors are the typical signal of this problem.",
             "If separation is present, consider checking whether the predictor is a disguised copy of the dependent variable, combining rare categories, or using Firth's (1993) penalized logistic regression, designed specifically to correct maximum-likelihood bias and produce finite estimates even under separation."
         ),
         linearityGuide = c(
             "Logistic regression does not assume the event relates linearly to the predictors, but that the logit (the log odds) does.",
             "This assumption is usually checked with the Box-Tidwell procedure (Box & Tidwell, 1962), which adds interaction terms between each numeric predictor and its own logarithm.",
             "A significant term suggests the real relationship is not linear in the logit and that the predictor may need a transformation or a nonlinear term (for example, a polynomial or a spline).",
             "This check only applies to numeric predictors; categorical predictors do not have this assumption."
         ),
         goodnessOfFitGuide = c(
             "Goodness of fit evaluates whether the model, as a whole, reasonably reproduces the observed data.",
             "Residual deviance compares the fitted model with a saturated model; very high values relative to the degrees of freedom suggest a poor fit.",
             "The Hosmer-Lemeshow test (1980) groups cases by predicted probability and compares observed and expected frequencies; a low p-value suggests the model does not fit well in some probability range.",
             "This test loses sensitivity with very large samples and is sensitive to the number of groups chosen, so it should be interpreted together with other diagnostics, not in isolation."
         ),
         discriminationGuide = c(
             "Discrimination evaluates how well the model distinguishes between cases with and without the event, regardless of whether the predicted probabilities are well calibrated.",
             "The area under the ROC curve (AUC) summarizes this ability (Hanley & McNeil, 1982): 0.5 is equivalent to no discrimination (chance level), values between 0.7 and 0.8 are considered acceptable, and values above 0.8 are considered good in most applied settings.",
             "A high AUC does not guarantee good calibration or good goodness of fit: a model can discriminate well and still over- or under-estimate the real probabilities."
         ),
         multicollinearityGuide = c(
             "Multicollinearity evaluates whether predictors carry very redundant information.",
             "It does not usually bias predictions, but it can make coefficients unstable, widen standard errors and make it harder to interpret each predictor individually.",
             "VIF values close to 1 suggest low collinearity.",
             "High values indicate a predictor is too well explained by other predictors.",
             "If meaningful collinearity is present, consider combining variables, removing redundant predictors, centering variables or changing the analytic question."
         ),
         correlationMatrixGuide = c(
             "These two tables complement the multicollinearity checks with an overview of the association between the dependent variable (coded 0/1) and all numeric predictors, each in APA 7 format (lower triangle, numbered variables).",
             "The first reports conventional Pearson correlation (equivalent to a point-biserial correlation with a binary variable; linear association only); the second reports distance correlation (Szekely et al., 2007), which detects linear and non-linear association alike.",
             "The discordance table flags pairs where dCor notably exceeds |Pearson r|, with copula entropy (copent) as a second line of evidence."
         ),
         influenceGuide = c(
             "Influence diagnostics identify cases that may substantially change coefficients or model fit.",
             "In logistic regression, Cook's D and leverage are computed on the model's working scale (weighted Pearson residuals), so they are approximate analogues, not identical counterparts, of their linear-regression versions (Pregibon, 1981, introduced these extensions and the weighted hat matrix for generalized linear models).",
             "An influential case should not be removed automatically.",
             "Check whether it is a recording error, a valid but extreme case, or a sign that the model does not represent all subgroups well."
         ),
         oddsRatiosGuide = c(
             "The odds ratio (OR) is the exponentiated coefficient; it represents how much the odds of the event are multiplied for each one-unit increase in the predictor (or for belonging to a category versus the reference category).",
             "An OR of 1 indicates no effect, values above 1 indicate higher probability of the event, and values below 1 indicate lower probability.",
             "The OR's confidence interval is more informative than the p-value alone: if it includes 1, the effect is not statistically distinguishable from no association.",
             "Common error: interpreting the OR as if it were a relative risk.",
             "The two coincide only when the event is rare; with common events (above 10%), the OR systematically overstates the real relative effect (Zhang & Yu, 1998)."
         )
     ),
     # ---- path: pathCheck module strings ----
     # ES: path: cadenas del módulo pathCheck
     path = list(
         title = "Path Analysis",
         outlierAnalysisGuide = c(
             "Before interpreting any coefficient, it is worth checking whether some cases deviate unusually from the rest of the sample, since path analysis relies on ordinary least squares, which are sensitive to extreme values.",
             "Mahalanobis distance (D²) evaluates how atypical each case is considering all model variables jointly (multivariate); a case is flagged as an outlier when D² exceeds the 97.5th percentile of a chi-square distribution with degrees of freedom equal to the number of model variables (Mahalanobis, 1936).",
             "Leverage and Cook's distance are computed on the equation with the most predictors in the model (the most complete one), as a practical, more demanding reference for influence within the equation system.",
             "A case is flagged on leverage when it exceeds 2p/n (p = estimated parameters, including the intercept; n = cases), and on Cook's distance when it exceeds 4/n.",
             "No single criterion is definitive on its own: a case can have high leverage without being influential (if its observed value matches what was expected), or the reverse.",
             "Common error: automatically removing any flagged case.",
             "First check whether it is a data entry error, a valid but extreme case, or an observation that is substantively important for the theory before deciding to exclude it."
         ),
         designGuide = c(
             "Classical path analysis (Wright, 1934; Duncan, 1966) decomposes a system of hypothesized causal relationships among observed variables into a series of regression equations, one for each endogenous variable (with at least one incoming arrow).",
             "Before interpreting the coefficients, check that the ratio of cases per path relationship is sufficient: a common practical reference is at least 10 cases per estimated parameter (Bentler & Chou, 1987).",
             "This approach assumes the model is recursive: there are no feedback loops (for example, A causing B and B causing A at the same time)."
         ),
         equationsGuide = c(
             "Each endogenous variable is predicted from the variables pointing to it in the path diagram, fitted as an independent linear regression.",
             "Because all variables are standardized before fitting, each coefficient is directly a standardized path coefficient in Wright's (1934) classical sense: it represents the expected change in the dependent variable, in standard deviations, per one standard deviation increase in the predictor.",
             "Standardized path coefficients allow comparing the relative importance of different predictors within the same equation, which unstandardized coefficients do not allow when variables are on different scales."
         ),
         normalityGuide = c(
             "Ordinary least squares estimation of each equation does not require the observed variables themselves to be normal, but it does require the residuals of each equation to be reasonably so, mainly so that the significance tests on the path coefficients are reliable.",
             "Two complementary tests are reported per equation: Shapiro-Wilk, generally the most powerful in small-to-moderate samples, and Anderson-Darling (Anderson & Darling, 1952), which weights the tails of the distribution more heavily (Razali & Wah, 2011).",
             "With very large samples, even small, practically irrelevant deviations can become statistically significant; it is worth also reviewing the Q-Q plot or histogram of residuals before concluding."
         ),
         multivariateNormalityGuide = c(
             "This complementary diagnostic evaluates multivariate normality of the full set of model variables using Mardia's (1970) skewness and kurtosis tests.",
             "Separate-equation OLS path analysis does not require multivariate normality to be valid; this check matters more if you later fit the same model with a joint estimator (for example, maximum-likelihood-based SEM)."
         ),
         correlationMatrixGuide = c(
             "These two tables complement the within-equation multicollinearity checks with an overall view of association among all model variables, each in APA 7 format (lower triangle, numbered variables).",
             "The first reports the usual Pearson correlation (linear association only); the second reports the distance correlation (Székely et al., 2007), which detects both linear and non-linear association equally.",
             "A large gap between dCor and the absolute Pearson value for the same pair suggests the relationship between those two variables may not be linear, which matters because path analysis assumes linear relationships among variables."
         ),
         homoscedasticityGuide = c(
             "Homoscedasticity assumes the variance of each equation's residuals is roughly constant across fitted values.",
             "Two tests are reported per equation: Breusch & Pagan (1979), which assumes the error variance is a linear function of the predictors, and White (1980), more general, which also captures curvature and interaction effects on the variance at the cost of fewer degrees of freedom.",
             "Heteroscedasticity does not bias the path coefficients, but it does bias their standard errors, which can lead to wrong conclusions about which paths are statistically significant."
         ),
         multicollinearityGuide = c(
             "Multicollinearity evaluates whether the predictors within the same equation carry very redundant information.",
             "The variance inflation factor (VIF) summarizes this redundancy; values above 5 raise a moderate concern and above 10 are considered a severe problem (Marquardt, 1970).",
             "It only applies to equations with two or more predictors; it does not make sense for an endogenous variable with a single direct predictor."
         ),
         indirectEffectsGuide = c(
             "When a variable influences another both directly and through one or more mediating variables, the total effect decomposes into a direct effect plus an indirect effect, following Wright's (1934) path-tracing rule: the indirect effect of each mediating chain is the product of the path coefficients along that chain.",
             "Baron and Kenny (1986) popularized the classical framework for distinguishing full from partial mediation in the social sciences and psychology.",
             "The Sobel (1982) z-test offers an analytic way to evaluate the significance of a simple indirect effect (a single mediator), though bootstrap confidence intervals are now preferred because they are less sensitive to the non-normality of the product of two coefficients."
         ),
         crossEntropyGuide = c(
             "Copula entropy (CE) is a dependence measure based on the copula of the variables (Ma & Sun, 2011), which captures the dependence structure between variables independently of their marginal distributions, without assuming any particular functional form (linear, monotonic, etc.).",
             "Unlike Pearson's r (which only detects linear association) or even dCor (which detects any dependence but is based on distances), CE is mathematically linked to the mutual information between the two variables, measured in bits of information (Shannon entropy).",
             "A significant p-value (p < .05) means the observed copula entropy is higher than expected under independence, suggesting the variables share information beyond what would occur by chance.",
             "A non-significant p-value means the data are compatible with the variables being statistically independent.",
             "Common error: interpreting a non-significant CE test as proof that two variables are completely unrelated.",
             "Like all statistical tests, failure to reject independence does not prove independence - it only means the data do not provide strong evidence against it.",
             "With small samples, the test may lack power to detect real but weak dependencies.",
             "Sample-size caveat: the copula entropy estimator used by copent() (based on Kozachenko- Leonenko k-nearest neighbors) requires sufficient sample size to produce stable estimates.",
             "With very small samples (n < 20), the p-values may be unreliable.",
             "With very large samples, even trivial dependencies can become statistically significant.",
             "Practical use in path analysis: if two variables that are not directly connected in your model show significant copula entropy dependence, this may suggest an omitted path or a common cause not included in the model.",
             "However, do not add paths based solely on this diagnostic - theoretical justification should always come first.",
             "Reference: copula entropy was formally defined by Ma and Sun (2011); its estimation uses the Kozachenko and Leonenko (1987) k-nearest-neighbor method.",
             "The copent R package implements this estimator for independence testing.",
             "Full references are available in the Bibliography tool."
         ),
         crossEntropyInterpretation = c(
             "Copula entropy complements the Pearson and dCor matrices with a third perspective on",
             "dependence: information theory. While Pearson measures linear association and dCor",
             "measures distance-based dependence, copula entropy measures how much information two",
             "variables share regardless of the shape of their relationship.",
             "When all three diagnostics agree (low Pearson, low dCor, non-significant CE), the",
             "evidence for independence between that pair of variables is strong. When they disagree",
             "- for example, low Pearson but significant CE - this suggests a non-linear dependence",
             "that Pearson does not capture but that information theory does detect.",
             "Common error: using this table to reverse-engineer the model by adding paths based on",
             "the highest CE values. Path relationships should come from theory, not be discovered",
             "post-hoc from the data - otherwise the model capitalizes on chance and will not",
             "replicate.",
             "If the 'copent' package is not installed, this section cannot be computed. Install it",
             "with install.packages('copent') to enable this advanced diagnostic."
         )
     ),
     # ---- library: assumptionLibrary module strings ----
     # ES: library: cadenas del módulo assumptionLibrary
     library = list(
         title = "Assumption Library",
         intro = c(
             "Assumption Library",
             "",
             "This library summarizes the assumptions and tests used across AssumptionsLab.",
             "Its purpose is not to analyze data directly, but to serve as a methodological",
             "guide for interpreting the tests within each analysis.",
             "",
             "Core principle:",
             "The library explains what each assumption means and when it is used.",
             "Each analysis then interprets its own results using the user's actual data.",
             "",
             "Citation style: APA 7th edition.",
             "",
             "General rule for p-values:",
             "p < .05 indicates a statistically significant deviation from the",
             "assumption being evaluated.",
             "p >= .05 indicates that the result is compatible with the assumption",
             "being approximately met.",
             "",
             "Important:",
             "Compatible does not mean proven. It means that, with this data and this",
             "test, no statistically significant deviation is observed.",
             "",
             "Avoid interpreting assumption tests mechanically. The decision",
             "should combine p-values, sample size, plots, design, and substantive judgment."
         ),
         normalityPart1 = c(
             "Normality",
             "",
             "What it assesses:",
             "Normality evaluates whether a distribution reasonably approximates a normal",
             "distribution. In applied statistical analysis, it is not always the original",
             "variable that is evaluated.",
             "",
             "Where it's used in AssumptionsLab:",
             "Independent Groups: normality by group or of residuals, depending on context.",
             "Related Groups: normality of paired differences or within-subject residuals.",
             "ANOVA/ANCOVA: normality of model residuals.",
             "Regression: normality of model residuals.",
             "",
             "How the battery is interpreted:",
             "Shapiro-Wilk is used as the primary test because it is the most powerful in",
             "small to moderate samples; the others are reported as secondary evidence. The",
             "interpretation integrates both: how many of the secondary tests agree with",
             "Shapiro-Wilk's conclusion, not just whether Shapiro-Wilk alone is significant."
         ),
         normalityTableHeaders = c("Test", "What it tests", "Typical use", "Main limitation"),
         normalityTableRows = list(
             c("Shapiro-Wilk", "General normality, the battery's primary test",
               "Small or moderate sample; the default reference test",
               "In R it is only computed for up to 5000 observations (Shapiro & Wilk, 1965)"),
             c("Lilliefors", "Normality via K-S with corrected critical values",
               "When an alternative to Shapiro-Wilk based on the cumulative distribution is needed",
               "The classic (uncorrected) K-S test is not valid here; the corrected version is always used (Lilliefors, 1967)"),
             c("Anderson-Darling", "Normality with more weight on the tails",
               "When heavy tails or extreme values are a concern",
               "Can be overly sensitive to trivial tail deviations with very large samples"),
             c("Cramér-von Mises", "Normality comparing the entire cumulative distribution",
               "When deviations are suspected to be spread across the central body, not at one point",
               "Less used and less familiar than Shapiro-Wilk or Anderson-Darling in applied practice"),
             c("Shapiro-Francia", "A Shapiro-Wilk variant with a different calculation",
               "Moderate or large samples, as complementary evidence to Shapiro-Wilk",
               "Its practical advantage over Shapiro-Wilk is marginal in many scenarios (Shapiro & Francia, 1972)"),
             c("Pearson chi-square", "Normality by comparing observed and expected frequencies by interval",
               "As additional secondary evidence, not as the primary test",
               "Depends on how the data are grouped into intervals; tends to be less powerful than full-distribution tests"),
             c("Jarque-Bera", "Normality by jointly evaluating skewness and kurtosis",
               "Moderate or large samples", "Less stable and informative in very small samples (Jarque & Bera, 1987)"),
             c("Skewness test", "Skewness of the distribution only",
               "Point diagnostic on the direction of skewness (long left or right tail)",
               "Evaluates only one aspect of shape; does not replace a general normality test"),
             c("Kurtosis test", "Peakedness and tail weight only",
               "Point diagnostic on heavy tails, light tails, or a concentration different from normal",
               "Evaluates only one aspect of shape; does not replace a general normality test")
         ),
         normalityPart2 = c(
             "",
             "",
             "Shapiro-Wilk:",
             "A classic and widely used normality test (Shapiro & Wilk, 1965). Requires at",
             "least 3 valid cases. In R it is computed for up to 5000 observations. It is",
             "especially powerful in small to moderate samples relative to the other tests",
             "in this battery (Razali & Wah, 2011).",
             "",
             "Lilliefors (corrected Kolmogorov-Smirnov):",
             "Compares the observed distribution against a theoretical one, but corrects",
             "the critical values because the mean and standard deviation are estimated",
             "from the same data (Lilliefors, 1967). The classic Kolmogorov-Smirnov test",
             "(without this correction) assumes parameters known in advance and is not",
             "valid in this context; this is why this module always uses the corrected",
             "Lilliefors version, not the classic K-S test.",
             "",
             "Anderson-Darling:",
             "Gives more weight to the tails (Anderson & Darling, 1952). It is useful when",
             "heavy tails, extreme values, or deviations at the ends of the distribution",
             "are a concern.",
             "",
             "Cramér-von Mises:",
             "Compares the entire observed cumulative distribution against the",
             "theoretical one (Anderson & Darling, 1952). It is more sensitive than",
             "Kolmogorov-Smirnov to deviations spread across the central body of the",
             "distribution, not just at one point.",
             "",
             "Shapiro-Francia:",
             "A variant of Shapiro-Wilk based on the same principles, with a different",
             "calculation that can perform better in some moderate or large samples",
             "(Shapiro & Francia, 1972).",
             "",
             "Pearson chi-square:",
             "Compares observed and expected frequencies by interval. It depends on how",
             "the data are grouped into intervals, so it tends to be less powerful than",
             "tests based on the full distribution.",
             "",
             "Jarque-Bera:",
             "Jointly evaluates skewness and kurtosis (Jarque & Bera, 1987). It is more",
             "informative in moderate or large samples, and less stable in very small",
             "samples.",
             "",
             "Skewness test:",
             "Evaluates skewness. Positive skewness suggests a long right tail; negative",
             "skewness suggests a long left tail.",
             "",
             "Kurtosis test:",
             "Evaluates peakedness and tail weight. It can indicate heavy tails, light",
             "tails, or a concentration different from what is expected under normality.",
             "",
             "Sample-size criteria:",
             "n < 10: interpretation is very unstable.",
             "10 <= n < 30: small sample; low power to detect deviations.",
             "30 <= n <= 200: moderate sample; the tests are useful diagnostics.",
             "n > 200: small deviations can produce significant p-values.",
             "n > 5000: Shapiro-Wilk is not computed in R.",
             "",
             "Interpretation:",
             "p < .05 suggests a statistically significant deviation from a normal",
             "distribution.",
             "p >= .05 indicates that the result is compatible with an approximately",
             "normal distribution.",
             "",
             "Methodological decision:",
             "Combine tests, plots, skewness, kurtosis, sample size, and outliers. If",
             "several tests suggest non-normality, consider transformation, bootstrap,",
             "robust methods, or nonparametric alternatives."
         ),
         homoscedasticityPart1 = c(
             "Homoscedasticity / Homogeneity of Variance",
             "",
             "What it assesses:",
             "Evaluates whether variability is approximately constant. In group",
             "comparisons this is called homogeneity of variance (Levene, 1960); in",
             "regression it is called homoscedasticity of residuals.",
             "",
             "Where it's used in AssumptionsLab:",
             "Independent Groups: equality of variances across groups.",
             "ANOVA/ANCOVA: equality of variances across cells or groups.",
             "Regression: constant residual variance across fitted values.",
             "",
             "Why it matters:",
             "When group variances are very different, the standard error of the test",
             "statistic no longer correctly reflects the real variability, and confidence",
             "intervals and p-values can become unreliable — especially with unequal",
             "group sizes (Welch, 1947). In regression, heteroscedasticity does not bias",
             "OLS coefficients, but it does bias their standard errors, affecting t and F",
             "tests on those coefficients.",
             "",
             "Two families of tests:",
             "Group tests (Levene, Brown-Forsythe, Bartlett, Fligner-Killeen, Hartley's",
             "Fmax) compare dispersion across k predefined groups. Regression tests",
             "(Breusch-Pagan, White, Goldfeld-Quandt, Spearman between residuals and",
             "fitted values) evaluate whether residual variance changes systematically",
             "with predicted values or predictors, rather than comparing discrete groups."
         ),
         homoscedasticityGroupTableHeaders = c("Test", "What it tests", "Typical use", "Main limitation"),
         homoscedasticityGroupTableRows = list(
             c("Levene",
               "Equality of variances using absolute deviations from each group's mean",
               "General, widely used first check",
               "Based on the mean; less robust than Brown-Forsythe to skewness or heavy tails"),
             c("Brown-Forsythe",
               "Equality of variances using absolute deviations from each group's median",
               "Preferred alternative to Levene when there is skewness, heavy tails, or outliers",
               "Slight power loss versus Levene when data are genuinely normal"),
             c("Bartlett",
               "Equality of variances assuming normality within each group",
               "When per-group normality is reasonable and the most powerful test is wanted",
               "Very sensitive to non-normality; may flag heterogeneity that is actually non-normality"),
             c("Fligner-Killeen",
               "Equality of variances via a rank-based test on the deviations",
               "Non-normal data, outliers, or when the most robust alternative is wanted",
               "Lower power than Levene or Bartlett when the data are in fact approximately normal"),
             c("Hartley's Fmax",
               "Ratio between the largest and smallest group variance",
               "Quick descriptive indicator, especially in balanced designs",
               "Not a formal test with a well-defined distribution outside balanced designs")
         ),
         homoscedasticityPart2 = c(
             "",
             "",
             "Levene — what it assesses and when to use it:",
             "Compares dispersion across groups using each observation's absolute",
             "deviation from its group mean, then applies a one-way ANOVA to those",
             "deviations (Levene, 1960). H0: the k population variances are equal. H1: at",
             "least one differs. It is a reasonable first check, especially with large",
             "samples and approximately symmetric data.",
             "",
             "Brown-Forsythe — what it assesses and when to use it:",
             "Follows the same logic as Levene but centers each group on its median",
             "instead of its mean (Brown & Forsythe, 1974). Using the median makes it",
             "notably more robust to skewness, heavy tails, and outliers. In practice,",
             "when there is doubt about the shape of the distribution within each group,",
             "Brown-Forsythe should be preferred over Levene by default.",
             "",
             "Bartlett — what it assesses and when to use it:",
             "Tests equality of variances using a likelihood-ratio approach that assumes",
             "normality within each group (Bartlett, 1937). It is the most powerful test in this family",
             "when that normality is reasonable, but it loses validity quickly if it is",
             "not: a deviation from normality can make Bartlett reject equal variances",
             "even when the true variances are equal. It should not be used as the sole",
             "basis for a decision if the data are not normal — check the Normality",
             "section of this library first.",
             "",
             "Fligner-Killeen — what it assesses and when to use it:",
             "A nonparametric, rank-based test on the absolute deviations from the median",
             "(Fligner & Killeen, 1976). It makes no distributional assumption, which",
             "makes it the most robust option in this family against non-normality and",
             "outliers, at the cost of some power when the data are in fact reasonably",
             "normal.",
             "",
             "Hartley's Fmax — what it assesses and limitations:",
             "Simply the ratio between the largest and smallest sample variance among the",
             "k groups (Hartley, 1950). It is easy to compute and interpret, but its",
             "tabulated critical values are only strictly valid with balanced designs",
             "(equal n per group) and normality; it is therefore used more as a",
             "complementary descriptive indicator than as a primary formal test.",
             "",
             "Interpretation (group tests):",
             "p < .05 suggests statistically significant differences between group",
             "variances. p >= .05 indicates that the result is compatible with",
             "approximately homogeneous variances."
         ),
         homoscedasticityRegTableHeaders = c("Test", "What it tests", "Typical use", "Main limitation"),
         homoscedasticityRegTableRows = list(
             c("Breusch-Pagan",
               "Whether residual variance depends linearly on predictors or fitted values",
               "Standard heteroscedasticity check in linear regression",
               "Assumes the shape of any heteroscedasticity is approximately linear in the predictors"),
             c("White",
               "General heteroscedasticity, including non-linear and interaction patterns",
               "When a more complex form of heteroscedasticity than linear is suspected",
               "Less powerful than Breusch-Pagan when the real heteroscedasticity is in fact simple and linear"),
             c("Goldfeld-Quandt",
               "Difference in residual variance between two ordered zones of the model",
               "When a clear break point in variance is suspected along an ordering variable",
               "Requires choosing the ordering variable and omitting a central band of data"),
             c("Spearman |residuals| vs fitted",
               "Whether residual magnitude grows or shrinks monotonically with fitted values",
               "Exploratory, nonparametric, easy-to-read diagnostic",
               "Only captures monotonic relationships; not a formal test in the sense of the previous three")
         ),
         homoscedasticityPart3 = c(
             "",
             "",
             "Breusch-Pagan — what it assesses and when to use it:",
             "Regresses the squared residuals on the original model's predictors and",
             "tests whether that auxiliary regression explains a significantly larger",
             "share of variance than zero (Breusch & Pagan, 1979). H0: residual variance",
             "is constant (homoscedasticity). H1: residual variance depends on the",
             "predictors. It is the default check for linear regression when an",
             "approximately linear relationship between variance and predictors is",
             "suspected.",
             "",
             "White — what it assesses and when to use it:",
             "Generalizes Breusch-Pagan by adding squares and cross-products of the",
             "predictors to the auxiliary regression, without assuming a specific",
             "functional form for the heteroscedasticity (White, 1980). It is more",
             "general and more suitable when residual variance is suspected to depend on",
             "the predictors non-linearly or through interactions, but it loses some",
             "power relative to Breusch-Pagan when the real heteroscedasticity is simply",
             "linear.",
             "",
             "Goldfeld-Quandt — what it assesses and when to use it:",
             "Orders observations by a variable (typically a predictor or the fitted",
             "values), omits a central band, and compares the residual sum of squares",
             "between the low-value and high-value subgroups using an F test (Goldfeld &",
             "Quandt, 1965). It is especially useful when variance is suspected to change",
             "in a stepped or marked way around an identifiable point of the ordering",
             "variable, rather than smoothly and continuously.",
             "",
             "Spearman |residuals| vs fitted — what it assesses and when to use it:",
             "Computes the Spearman rank correlation between the absolute value of the",
             "residuals and the fitted values. It is exploratory and nonparametric: it",
             "does not assume a specific functional form, only detecting whether error",
             "magnitude grows or shrinks monotonically with the predicted value. It is a",
             "good visual/numeric complement to Breusch-Pagan and White, not a formal",
             "substitute.",
             "",
             "Interpretation (regression tests):",
             "p < .05 suggests that residual variance is not constant (heteroscedasticity).",
             "p >= .05 indicates that the result is compatible with approximately",
             "constant residual variance.",
             "",
             "Methodological decision:",
             "In group comparisons, if there is evidence of heterogeneity of variance,",
             "consider the Welch correction (Welch, 1947) for the t or F statistic, the",
             "Brown-Forsythe test itself as an alternative to classic ANOVA, or robust",
             "methods. In regression, consider heteroscedasticity-robust HC3 standard",
             "errors (MacKinnon & White, 1985), a transformation of the dependent",
             "variable, or a model that explicitly models the variance."
         ),

         linearityPart1 = c(
             "Linearity",
             "",
             "What it assesses:",
             "Evaluates whether the relationship between numeric predictors or covariates",
             "and the dependent variable can reasonably be represented by a straight line.",
             "",
             "Where it's used in AssumptionsLab:",
             "Regression: numeric predictors and overall model fit.",
             "ANOVA/ANCOVA: numeric covariates.",
             "Path/Related/Logistic: dCor and copula entropy as a non-linear dependence diagnostic complementary to Pearson.",
             "",
             "It does not apply the same way to categorical factors, because these",
             "represent groups or levels, not continuous linear relationships."
         ),
         linearityTableHeaders = c("Diagnostic", "What it assesses", "Typical use", "Main limitation"),
         linearityTableRows = list(
             c("Bivariate correlation", "Simple linear association between two variables",
               "Initial exploratory glance", "Only detects linear association; does not replace diagnosing the full model"),
             c("dCor / copula entropy", "Linear and non-linear dependence, without assuming a functional form",
               "Exploratory complement to Pearson when a non-linear relationship is suspected",
               "A dCor-Pearson gap is a signal, not proof; needs confirming with a scatterplot"),
             c("Quadratic term", "Curvature of a specific predictor against the original model",
               "Quick check, one predictor at a time", "Does not replace RESET, which evaluates the full model"),
             c("Box-Tidwell", "Whether a specific predictor needs a power transformation",
               "Reference test for logit-linearity (logistic regression)",
               "Evaluates one predictor at a time; requires x > 0; does not assess joint curvature (Box & Tidwell, 1962)"),
             c("Ramsey RESET", "Overall functional misspecification of the model",
               "Global check after reviewing individual predictors",
               "Indicates something is wrong, but not what (curvature, omitted interaction, outlier) (Ramsey, 1969)"),
             c("Rainbow test", "Whether the linear fit is stable across the full range of fitted values",
               "Complement to RESET when non-linearity may be concentrated at the extremes",
               "Requires a reasonable sample (at least 20 cases in this module) for the central sub-model")
         ),
         linearityPart2 = c(
             "",
             "",
             "Bivariate correlation:",
             "Describes simple linear association between two variables. It is",
             "exploratory and does not replace diagnosing the full model.",
             "",
             "Distance correlation (dCor) and copula entropy:",
             "Pearson's r only detects linear association. dCor (Székely et al.,",
             "2007) detects linear and non-linear association equally, without",
             "assuming a functional form. Copula entropy (Ma & Sun, 2011) is an",
             "additional dependence measure, free of distributional assumptions, based on",
             "the copula of the variables. A pair with dCor notably higher than its",
             "Pearson |r| (gap > .10) is a signal, not proof, of a non-linear",
             "relationship that a linear model might be missing; it is worth confirming",
             "with a scatterplot before concluding non-linearity.",
             "The p-value for dCor and for copula entropy is computed by permutation,",
             "so its resolution depends on the number of permutations used.",
             "",
             "Exploratory quadratic term:",
             "Adds x² to the model as an additional predictor and compares it against",
             "the original model. p < .05 on the x² coefficient suggests possible",
             "curvature not captured by the linear relationship. It is a quick,",
             "one-predictor-at-a-time check; it does not replace RESET, which evaluates",
             "the full model.",
             "",
             "Box-Tidwell — what it assesses:",
             "Evaluates whether a specific numeric predictor would need a power",
             "transformation (a form of type Xᵏ) to relate linearly to the dependent",
             "variable or, in logistic regression, to the logit. The mechanism: an",
             "x × ln(x) interaction term is added to the model alongside x itself, and",
             "the significance of that term's coefficient is tested. H0: x does not need",
             "to be transformed (the relationship is already approximately linear on the",
             "current scale). H1: the relationship would improve with a power",
             "transformation of x. Requires x > 0, because the term uses ln(x); it",
             "cannot be applied to predictors with negative or zero values without",
             "shifting them first.",
             "",
             "Box-Tidwell — when to use it:",
             "It is the reference test for evaluating logit-linearity of a continuous",
             "numeric predictor in logistic regression — unlike the quadratic term or a",
             "scatterplot, it directly evaluates the correct scale of the predictor (log,",
             "root, linear) instead of only detecting curvature. It is also useful in",
             "linear regression when a specific predictor, not the full model, is",
             "suspected to be the source of the non-linearity.",
             "",
             "Box-Tidwell — limitations:",
             "It evaluates one predictor at a time, so it says nothing about joint",
             "curvature across several predictors or about general model",
             "misspecification (that is what Ramsey RESET is for). With 0 of several",
             "predictors significant, the correct conclusion is \"no statistical evidence",
             "of a deviation from linearity was detected\", not \"linearity is confirmed\" —",
             "the test may simply lack power in the available sample.",
             "",
             "Ramsey RESET — what it assesses:",
             "Evaluates whether the model, as a whole, is functionally misspecified —",
             "that is, whether it fails to capture some systematic form (curvature,",
             "interactions, omitted terms) that a model strictly linear in the current",
             "predictors cannot represent. The mechanism: powers of the fitted values",
             "(typically the square and the cube) are added to the model, and an F-test",
             "contrasts whether those powers significantly improve the fit relative to",
             "the original model. H0: the original model is correctly specified (the",
             "powers add no additional explanation). H1: the model omits some relevant",
             "functional form.",
             "",
             "Ramsey RESET — when to use it:",
             "As a global check after reviewing the individual predictors: if",
             "Box-Tidwell or the quadratic term do not flag any particular predictor but",
             "the overall fit looks poor, RESET can detect non-linearity that only",
             "appears in combination across predictors, or in a form (for example, an",
             "interaction) that no single per-predictor check captures on its own. It is",
             "the most general of the three tests.",
             "",
             "Ramsey RESET — limitations:",
             "A significant RESET indicates something about the functional form is",
             "wrong, but not what: it does not distinguish between genuine curvature, an",
             "omitted interaction, an omitted variable correlated with the predictors, or",
             "an influential outlier. It is worth reviewing the residual plots and the",
             "other linearity tests before deciding which correction to apply.",
             "",
             "Rainbow test — what it assesses:",
             "Evaluates whether the linear fit is stable across the range of fitted",
             "values, rather than concentrating the evidence on one specific form of",
             "non-linearity. The mechanism: cases are ordered by their fitted value, the",
             "central band is taken (by default, around 50% of the cases with",
             "intermediate fitted values), and the model is refit using only that",
             "central subset. An F-test compares the residual sum of squares of the full",
             "model against that of the model fit only to the central band. H0: the",
             "linear fit is equally adequate in the central band as in the full set.",
             "H1: the fit deteriorates when the extremes are included, suggesting the",
             "relationship is not uniformly linear across the full range of the",
             "predictors.",
             "",
             "Rainbow test — when to use it:",
             "It is useful as a complement to RESET when the non-linearity is suspected",
             "not to be smooth, global curvature, but rather a relationship that behaves",
             "differently at the extremes of the predictor range versus the center — for",
             "example, effects that flatten out or accelerate only at extreme values.",
             "It requires a reasonable sample (in this module, at least 20 cases) so the",
             "central sub-model has enough degrees of freedom.",
             "",
             "Common to RESET and Rainbow — a frequent error:",
             "Interpreting a non-significant RESET or Rainbow result as proof of perfect",
             "linearity. Both tests have limited power in small samples (they may miss",
             "real curvature) and can flag trivial, practically irrelevant curvature as",
             "significant in very large samples. Always read them together with the",
             "residuals-vs-fitted plot, not in isolation.",
             "",
             "Sample-size criteria:",
             "With small samples it can be difficult to detect curvature.",
             "With large samples, small curvatures can become significant.",
             "",
             "Interpretation:",
             "p < .05 on curvature or specification tests suggests a possible",
             "deviation from linearity.",
             "p >= .05 indicates that the result is compatible with an approximately",
             "linear relationship.",
             "",
             "Methodological decision:",
             "Consider polynomial terms, splines, transformations, or non-linear models",
             "if there is evidence of substantive curvature."
         ),
         independencePart1 = c(
             "Independence",
             "",
             "What it assesses:",
             "Evaluates whether observations or errors can be considered independent.",
             "This assumption depends mainly on the study design.",
             "",
             "Where it's used in AssumptionsLab:",
             "ANOVA/ANCOVA: independence between units or subjects.",
             "Regression: independence of errors, especially in ordered data.",
             "Related Groups: dependence within a unit is expected, independence between units.",
             "",
             "Core idea:",
             "The four tests in this section relate to the temporal dependence of",
             "residuals, but they do not answer exactly the same question or apply to",
             "the same object. In a regression or time series, one usually wants the",
             "residuals to be approximately white noise, i.e., with no systematic",
             "temporal pattern. If there is residual autocorrelation, the OLS",
             "coefficients can remain unbiased under certain conditions, but the",
             "standard errors, t-tests, confidence intervals, and F-tests can become",
             "unreliable.",
             "",
             "The choice among the four depends mainly on:",
             "Whether you are analyzing residuals from a regression or residuals from an",
             "ARIMA/SARIMA-type time-series model.",
             "Whether autocorrelation is suspected only at lag 1 or at several lags at",
             "once.",
             "Whether the model contains the lagged dependent variable as a regressor.",
             "Whether the goal is to detect linear autocorrelation specifically, or a",
             "more general lack of randomness in the sequence."
         ),
         independenceTableHeaders = c("Test", "What it tests", "Typical use", "Main limitation"),
         independenceTableRows = list(
                c("Durbin-Watson",
                  "First-order residual autocorrelation",
                  "Simple static linear regression",
                  "Not appropriate if the model includes the lagged dependent variable as a regressor; does not handle higher-order lags well"),
                c("Breusch-Godfrey",
                  "Residual autocorrelation up to a chosen order p",
                  "Econometric, dynamic, or multi-lag regressions",
                  "The order p must be chosen"),
                c("Ljung-Box",
                  "Joint autocorrelation across several lags",
                  "Diagnosing residuals of ARIMA/SARIMA and time-series models",
                  "Indicates joint dependence, but does not by itself identify which lag or its shape"),
                c("Runs test",
                  "Randomness of the sequence based on sign changes or position relative to a reference",
                  "Nonparametric, exploratory diagnostic",
                  "Does not directly measure lag-based linear autocorrelation")
         ),
         independencePart2 = c(
             "",
             "",
             "Durbin-Watson — what it assesses:",
             "Evaluates whether the residuals of a regression show first-order",
             "autocorrelation. H0: no first-order autocorrelation. H1: there is",
             "first-order autocorrelation, positive or negative. Its statistic ranges",
             "roughly from 0 to 4: near 2 indicates no first-order autocorrelation;",
             "below 2 suggests positive autocorrelation; above 2 suggests negative",
             "autocorrelation. There is an approximate relationship DW ≈ 2 × (1 − r1),",
             "where r1 is the estimated residual autocorrelation at lag 1.",
             "",
             "Durbin-Watson — when to use it:",
             "As a simple diagnostic when there is a linear least-squares regression,",
             "temporally ordered data, specific interest in the dependence between a",
             "residual and the immediately preceding one, and a model without the",
             "lagged dependent variable as a regressor.",
             "",
             "Durbin-Watson — when not to choose it:",
             "When the model includes the lagged dependent variable (a dynamic model),",
             "when autocorrelation of order 2, 3, 12, etc. is suspected, when there is a",
             "more complex temporal structure such as seasonality, or when a formal",
             "test with an easily generalizable p-value is needed. In modern applied",
             "practice, Durbin-Watson is usually reported as a quick descriptive",
             "indicator, while Breusch-Godfrey is generally preferred for the formal",
             "test.",
             "",
             "Breusch-Godfrey — what it assesses:",
             "Also called the LM test for serial correlation, it tests whether the",
             "residuals of a regression show autocorrelation up to an order p chosen by",
             "the analyst. H0: no residual autocorrelation up to order p. H1:",
             "autocorrelation exists in at least one of the lags from 1 to p. The test",
             "is based on an auxiliary regression that incorporates the lagged",
             "residuals; under H0, the LM statistic approximately follows a chi-square",
             "distribution with p degrees of freedom.",
             "",
             "Breusch-Godfrey — when to use it:",
             "It is the most general option for regressions. It is worth using with",
             "econometric regressions, when more than one lag needs to be tested, when",
             "the model includes lagged explanatory variables or the lagged dependent",
             "variable itself, and when a formal test with a p-value is needed to",
             "decide between modeling AR errors, using GLS, applying HAC/Newey-West",
             "errors, or rethinking the model's dynamic specification.",
             "",
             "Breusch-Godfrey — how to choose the order p:",
             "It should not be chosen mechanically or overstated. With annual data it",
             "usually makes sense to start with 1 or 2 lags; with quarterly data, try 1",
             "to 4; with monthly data, testing up to 12 can be reasonable if there is a",
             "possible annual pattern; with higher frequencies it is worth relying on",
             "process knowledge and reviewing the residuals' autocorrelation function",
             "(ACF) and partial autocorrelation function (PACF). Practical rule: when in",
             "doubt between Durbin-Watson and Breusch-Godfrey for a regression,",
             "Breusch-Godfrey is preferred.",
             "",
             "Ljung-Box — what it assesses:",
             "This is a joint (portmanteau) test that assesses whether the",
             "autocorrelations up to a horizon h are jointly zero. H0: no",
             "autocorrelation up to lag h. H1: autocorrelation exists in at least one",
             "of those lags. Its Q statistic combines, weighted by sample size and each",
             "lag, the squares of the estimated residual autocorrelations up to lag h;",
             "so it does not ask whether there is autocorrelation only at lag 1, but",
             "whether the residuals, taken together, look like white noise up to that",
             "horizon.",
             "",
             "Ljung-Box — when to use it and which horizon h to choose:",
             "It is especially useful for diagnosing the residuals of an ARIMA, SARIMA,",
             "ETS, or other time-series forecasting model, and for checking whether",
             "temporal structure remains unexplained after fitting the model. It is",
             "typically applied to the residuals of an already-fitted model, not to the",
             "raw series. The horizon h depends on frequency and objective: with",
             "quarterly data h=4 or 8 is usually reviewed; with monthly data, h=12 is a",
             "natural starting point; with daily data, a horizon consistent with",
             "weekly, monthly, or business cycles is worth using. When applying it to",
             "the residuals of an ARIMA, the degrees of freedom must also be adjusted",
             "for the AR and MA parameters already estimated.",
             "",
             "Ljung-Box — difference from Breusch-Godfrey:",
             "Both can review several lags at once, but their natural context differs:",
             "Breusch-Godfrey is used in a regression with covariates and asks whether",
             "that regression's errors are serially correlated; Ljung-Box is a global",
             "diagnostic of the residuals of a time-series model and asks whether those",
             "residuals look like white noise.",
             "",
             "Runs test — what it actually assesses:",
             "Unlike the three previous tests, it is not primarily a test of linear",
             "autocorrelation. It evaluates whether the order of a sequence looks",
             "random. A run is a consecutive sequence of observations of the same",
             "class — for example, consecutive positive residuals versus negative ones,",
             "or values above versus below the median. H0: the sequence was generated",
             "randomly. H1: the sequence does not look random. The test compares the",
             "observed number of runs with the number expected under randomness: too",
             "few runs suggest clustering or persistence; too many can suggest",
             "excessive alternation.",
             "",
             "Runs test — when to use it:",
             "When a nonparametric diagnostic is needed, when the residuals are not",
             "normal or contain outliers, when the interest is in detecting sign",
             "clustering, regime changes, or a general lack of randomness, when working",
             "with a binary sequence (signs, pass/fail, defective/non-defective), or as",
             "a nonparametric complement to the autocorrelation tests above.",
             "",
             "Runs test — what it does not conclude:",
             "A significant result only supports that the order of the series does not",
             "look random; it does not necessarily imply AR(1)-type linear",
             "autocorrelation. It can detect non-randomness arising from trends,",
             "structural changes, persistence of signs, or non-linear patterns.",
             "Conversely, a series may not show strong linear autocorrelation and still",
             "not be completely random.",
             "",
             "Practical decision tree:",
             "Linear regression with temporal data, no lagged dependent variable: start",
             "with a plot of residuals over time, the residuals' ACF/PACF, and",
             "Breusch-Godfrey; use Durbin-Watson only as a quick lag-1 check.",
             "Regression with the lagged dependent variable or other dynamic",
             "components: use Breusch-Godfrey and the residuals' ACF/PACF; avoid",
             "relying on Durbin-Watson.",
             "Already-fitted ARIMA or SARIMA model: use the residuals' ACF and",
             "Ljung-Box, to verify that the model has absorbed the temporal",
             "dependence.",
             "General randomness without assuming a specific linear relationship: use",
             "the Runs test, a sequence plot, and the ACF as a complement.",
             "",
             "Final recommendation if a default test is needed:",
             "General temporal or econometric regression: Breusch-Godfrey.",
             "ARIMA/SARIMA or forecasting model: Ljung-Box.",
             "Quick, basic AR(1) autocorrelation check in a static regression:",
             "Durbin-Watson.",
             "Nonparametric check of randomness or sign pattern: Runs test.",
             "In almost every case, it is worth accompanying the test with a residuals",
             "ACF: the p-value indicates whether there is overall evidence against the",
             "absence of dependence, and the ACF helps identify at which lags the",
             "problem lies.",
             "",
             "Preconditions:",
             "These tests make more sense when there is a temporal, spatial, or",
             "sequential order. In cross-sectional data with no natural order, they",
             "should be interpreted with caution.",
             "",
             "Interpretation:",
             "p < .05 suggests a statistically significant deviation from independence",
             "or randomness of residuals.",
             "p >= .05 indicates that the result is compatible with approximate",
             "independence or randomness of residuals.",
             "",
             "Methodological decision:",
             "If dependence is present, consider mixed models, GLS, time-series models,",
             "or HAC/Newey-West robust errors, depending on the design."
         ),
         multicollinearityPart1 = c(
             "Multicollinearity",
             "",
             "What it assesses:",
             "Evaluates whether the model's predictors are too closely related to one",
             "another. Multicollinearity does not necessarily reduce the model's overall",
             "predictive power, but it inflates the variance of individual coefficients",
             "and makes interpreting them separately harder (Marquardt, 1970).",
             "",
             "Where it's used in AssumptionsLab:",
             "Regression and linear models with several predictors.",
             "",
             "Why it matters:",
             "When two or more predictors share a lot of information, the model cannot",
             "precisely separate each one's own effect; individual coefficients become",
             "unstable (they change substantially when cases or predictors are added or",
             "removed) and their standard errors inflate, even though the model's overall",
             "fit (R²) and its predictions can remain reasonable (O'Brien, 2007)."
         ),
         multicollinearityTableHeaders = c("Diagnostic", "What it measures", "Common rule of thumb", "Main limitation"),
         multicollinearityTableRows = list(
             c("VIF", "How much a coefficient's variance is inflated by its association with the other predictors",
               "VIF < 5 acceptable; 5-10 concern; ≥ 10 severe problem",
               "The thresholds are conventions, not universal rules (O'Brien, 2007); does not identify which variables are redundant with which"),
             c("Tolerance", "The inverse of VIF (1/VIF)",
               "> .20 acceptable; < .20 concern; < .10 severe problem",
               "Same information as VIF on a different scale; adds nothing new by itself"),
             c("Maximum correlation", "The largest bivariate correlation between pairs of predictors",
               "|r| ≥ .80 suggests possible bivariate redundancy",
               "Only detects collinearity between pairs; does not see redundancy spread across three or more predictors"),
             c("Condition index", "The ratio between the largest and smallest eigenvalue of the standardized predictor matrix",
               "> 15 concern; > 30 severe collinearity",
               "Requires also reviewing variance proportions by dimension to know which predictors are involved"),
             c("Eigenvalues / determinant", "How close the predictor matrix is to being singular",
               "Values near zero suggest redundant dimensions",
               "An aggregate signal; does not directly identify which combination of predictors is redundant"),
             c("Model rank", "Whether an exact linear dependency exists among predictors",
               "Rank < number of columns indicates non-estimable parameters",
               "An exact-identification problem, distinct from the approximate collinearity the measures above capture")
         ),
         multicollinearityPart2 = c(
             "",
             "",
             "VIF and tolerance — what they assess:",
             "A predictor's VIF is obtained by regressing that predictor on all the",
             "other predictors in the model and using the resulting R²: VIF = 1/(1-R²).",
             "A high VIF means much of that predictor's variation can already be",
             "explained by the others, leaving little \"own\" information to estimate its",
             "coefficient precisely. Tolerance is simply 1/VIF, on the opposite scale.",
             "",
             "VIF and tolerance — when to use them and a caution about thresholds:",
             "They are the most common and easiest-to-interpret per-variable diagnostic.",
             "The conventional thresholds (VIF < 5 or < 10) are widely used rules, not",
             "universal limits derived mathematically: the tolerable level of VIF depends",
             "on sample size, how much variance the model explains, and whether the",
             "interest is in individual coefficients or only in overall prediction",
             "(O'Brien, 2007). With large samples, moderate VIFs can be harmless; with",
             "small samples, even VIFs of 3-4 can already produce unstable coefficients.",
             "",
             "Maximum correlation between predictors:",
             "The simplest bivariate check: it identifies pairs of predictors that are",
             "highly correlated with each other. It is useful as a first glance, but it",
             "does not detect \"distributed\" collinearity, where no single pair is highly",
             "correlated but a linear combination of three or more predictors is —",
             "that kind of collinearity only shows up in VIF, eigenvalues, or the",
             "condition index.",
             "",
             "Condition index and eigenvalues:",
             "These come from the decomposition of the standardized predictor matrix. An",
             "eigenvalue near zero indicates a direction in predictor space with almost",
             "no independent variation — that is, a nearly redundant linear combination.",
             "The condition index summarizes this as the ratio between the largest and",
             "smallest eigenvalue; high values flag collinearity, but it is worth also",
             "reviewing the variance proportions by dimension to identify which specific",
             "predictors are involved (Belsley et al., 1980).",
             "",
             "Determinant and model rank:",
             "A near-zero determinant of the predictor correlation matrix is another",
             "aggregate signal of redundancy. Model rank is a different and stricter",
             "check: if the rank is less than the number of columns in the design matrix,",
             "an exact linear dependency exists (not just high correlation), and some",
             "model parameters are not estimable at all — R typically flags them as NA in",
             "the model output.",
             "",
             "Methodological decision:",
             "If collinearity is high but the main goal is prediction, it may not",
             "require action. If the goal is interpreting individual coefficients,",
             "consider removing or combining redundant predictors, centering polynomial",
             "terms and interactions (which reduces structural collinearity without",
             "changing the substantive model), or using penalized methods (ridge, lasso)",
             "that stabilize estimates in exchange for controlled bias (Marquardt, 1970)."
         ),
         influencePart1 = c(
             "Outliers and Influential Cases",
             "",
             "What it assesses:",
             "Evaluates whether results may be dominated by a few extreme, unusual, or",
             "highly influential cases rather than reflecting the general pattern of the",
             "data.",
             "",
             "Where it's used in AssumptionsLab:",
             "Independent Groups, Related Groups, ANOVA/ANCOVA, and Regression.",
             "",
             "An important distinction:",
             "An outlier is a value unusual relative to the rest of the data; an",
             "influential case is one whose presence or absence substantially changes the",
             "analysis's result. A case can be an outlier without being influential, and",
             "influential without being an obvious outlier — this is why several",
             "diagnostics are needed, not just one (Belsley et al., 1980; Cook,",
             "1977)."
         ),
         influenceTableHeaders = c("Diagnostic", "What it measures", "Common rule of thumb", "Main limitation"),
         influenceTableRows = list(
             c("IQR", "Distance of a value from the interquartile range",
               "Outside Q1 − 1.5·IQR or Q3 + 1.5·IQR",
               "Looks only at the variable univariately; does not consider the model or other variables"),
             c("Studentized residual", "The case's discrepancy from the fitted model, in standardized error units",
               "|studentized residual| > 3",
               "A large residual flags poor local fit, not necessarily that the case moves the estimates"),
             c("Leverage", "How unusual the case's combination of predictors is",
               "Leverage > 2p/n (p = true number of estimated parameters)",
               "High leverage alone does not imply the case is influential if its residual is small"),
             c("Cook's D", "The case's joint influence on all of the model's estimates",
               "Cook's D > 4/n, as a screening threshold (Cook, 1977)",
               "4/n is more lenient with small samples and stricter with large ones; treat as screening, not confirmation"),
             c("DFFITS", "How much the case's own fitted value changes when it is excluded from the model",
               "|DFFITS| > 2·√(p/n)",
               "Sensitive to the same kind of cases as Cook's D; best read together, not separately"),
             c("Mahalanobis D²", "The case's multivariate distance from the predictor centroid",
               "Large values relative to a chi-square with p degrees of freedom",
               "Detects unusual predictor combinations (multivariate leverage), not whether those combinations affect the result (Mahalanobis, 1936)")
         ),
         influencePart2 = c(
             "",
             "",
             "IQR — what it assesses and when to use it:",
             "Flags values below Q1 - 1.5*IQR or above Q3 + 1.5*IQR of the variable being",
             "analyzed. In related groups this is applied to the paired differences, not",
             "the original measurements. It is the simplest check and does not depend on",
             "any fitted model — which makes it a natural starting point before fitting",
             "any analysis.",
             "",
             "Studentized residual — what it assesses and when to use it:",
             "Identifies observations with a large discrepancy from what the model",
             "predicts, expressed in standard-error units. It is useful for detecting",
             "cases the model fits poorly, but a large residual does not automatically",
             "mean that case is moving the estimates — for that, Cook's D or DFFITS are",
             "needed.",
             "",
             "Leverage — what it assesses and a caution about the threshold:",
             "Identifies unusual combinations of predictor values, without yet looking at",
             "the dependent variable. Rule of thumb: leverage > 2p/n, where p is the",
             "model's true number of estimated parameters (length(coef(fit)) in R) — not",
             "the number of selected variables. When a predictor is a factor with more",
             "than 2 levels, each additional level adds a parameter through dummy coding;",
             "using the variable count instead of the true parameter count understates",
             "the threshold and flags too many cases.",
             "",
             "Cook's D — what it assesses and when to use it:",
             "Combines leverage and residual magnitude into a single measure of",
             "influence on the full set of estimated coefficients (Cook, 1977). Rule of",
             "thumb: Cook's D > 4/n. Treat it as a screening threshold, not as confirmed",
             "influence: 4/n is numerically larger (more lenient) the smaller the sample,",
             "and smaller (stricter) as it grows; even so, small samples tend to keep",
             "flagging several cases because individual Cook's D values tend to inflate",
             "when n is small.",
             "",
             "DFFITS — what it assesses and when to use it:",
             "Evaluates how much a case's own fitted value changes when it is removed and",
             "the model is refit without it (Belsley et al., 1980). Rule of thumb:",
             "|DFFITS| > 2*sqrt(p/n). It is conceptually very close to Cook's D — both",
             "capture influence on the fit — so it is best read as corroborating",
             "evidence, not as an independent second finding.",
             "",
             "Mahalanobis D² — what it assesses and when to use it:",
             "Evaluates a case's multivariate distance from the predictor centroid,",
             "accounting for correlations among predictors (Mahalanobis, 1936). It is the",
             "multivariate analogue of leverage: it detects unusual predictor",
             "combinations even when no single variable is an extreme value on its own.",
             "Like leverage, it says nothing by itself about whether that unusual",
             "combination actually affects the results.",
             "",
             "Interpretation:",
             "A flagged case should not be removed automatically. It may be a data-entry",
             "error, a valid extreme case, or important substantive evidence that the",
             "model is not capturing well.",
             "",
             "Methodological decision:",
             "Review the original data point, run a sensitivity analysis (compare",
             "results with and without the case), and justify any exclusion with",
             "transparent criteria declared in advance — never chosen after seeing that",
             "the exclusion \"improves\" the result."
         ),
         sphericityPart1 = c(
             "Sphericity",
             "",
             "What it assesses:",
             "Sphericity evaluates whether the variances of the differences between all",
             "possible pairs of repeated measurements are approximately equal (Mauchly,",
             "1940).",
             "",
             "Where it's used in AssumptionsLab:",
             "Related Groups with three or more related measurements.",
             "",
             "It does not apply with only two measurements, because with two levels there",
             "is only one possible pair of differences, and no variances to compare",
             "against each other.",
             "",
             "Why it matters:",
             "The classic repeated-measures ANOVA assumes sphericity so that its F",
             "statistic follows the nominal F distribution. When it is violated, the",
             "observed F tends to inflate and the real Type I error exceeds the nominal",
             "one; the degrees-of-freedom corrections exist precisely to compensate for",
             "that inflation without changing the F statistic itself."
         ),
         sphericityTableHeaders = c("Diagnostic / correction", "What it does", "When to use it", "Main limitation"),
         sphericityTableRows = list(
             c("Mauchly (W)", "Tests the hypothesis of exact sphericity",
               "As an initial check before deciding whether to correct degrees of freedom",
               "Unstable with small samples and overly sensitive (flags trivial violations) with large samples"),
             c("Greenhouse-Geisser", "Corrects degrees of freedom by multiplying them by an estimated epsilon",
               "Conservative default correction, especially if epsilon < .75",
               "Can be excessively conservative (loses power) when the real violation is mild"),
             c("Huynh-Feldt", "Corrects degrees of freedom with an adjusted epsilon, less conservative than GG",
               "Preferred over Greenhouse-Geisser when the estimated epsilon > .75",
               "Can overestimate epsilon (be too liberal) in small samples"),
             c("Lower-bound", "Applies the most severe possible correction (epsilon = 1/(k-1))",
               "As an extreme conservative bound when avoiding false positives is the top priority",
               "Very conservative: sacrifices considerable statistical power relative to GG or HF")
         ),
         sphericityPart2 = c(
             "",
             "",
             "Mauchly — what it assesses and limitations:",
             "Tests H0: the variances of the differences between all pairs of levels are",
             "equal (sphericity), against H1: at least one pair differs (Mauchly, 1940).",
             "It is the historical reference test, but its behavior is not uniform: with",
             "small samples it can lack power to detect real violations, and with large",
             "samples it can flag deviations from sphericity too small to have practical",
             "relevance for the F statistic.",
             "",
             "Greenhouse-Geisser — what it does and when to use it:",
             "Rather than testing sphericity directly, it estimates how far the observed",
             "covariance pattern departs from it (epsilon, between 1/(k-1) and 1) and",
             "reduces the F test's degrees of freedom by multiplying them by that epsilon",
             "(Greenhouse & Geisser, 1959). The F statistic itself does not change; only",
             "the degrees of freedom used to obtain the p-value change, making the test",
             "more conservative. It is the recommended default correction when the",
             "estimated epsilon is low (< .75).",
             "",
             "Huynh-Feldt — what it does and when to use it:",
             "An alternative correction that adjusts the Greenhouse-Geisser epsilon to",
             "reduce its tendency to be overly conservative, especially when the",
             "sphericity violation is mild or moderate (Huynh & Feldt, 1976). The usual",
             "convention is to prefer Huynh-Feldt when the Greenhouse-Geisser-estimated",
             "epsilon exceeds .75, and Greenhouse-Geisser when it is below that.",
             "",
             "Lower-bound — what it does and when to use it:",
             "Applies the most extreme mathematically possible correction, assuming the",
             "worst-case violation of sphericity. It is useful as a safety bound — if",
             "the effect remains significant even under this severe correction, the",
             "conclusion is robust to any degree of sphericity violation — but it",
             "sacrifices considerable power and is rarely the final correction reported.",
             "",
             "Interpretation:",
             "p < .05 on Mauchly's test suggests a statistically significant deviation",
             "from sphericity. p >= .05 indicates that the result is compatible with",
             "approximate sphericity.",
             "",
             "Methodological decision:",
             "If there is evidence against sphericity, use the Greenhouse-Geisser or",
             "Huynh-Feldt correction depending on the estimated epsilon, consider the",
             "nonparametric Friedman test (Friedman, 1937) as an alternative, or fit a",
             "mixed model that does not assume sphericity, depending on the study design."
         ),

         # ---- proportionalOdds: see the matching ES block above for the
         # design rationale (new category, modeled on "sphericity").
         # ---------------------------------------------------------------
         proportionalOddsPart1 = c(
             "Proportional Odds",
             "",
             "What it evaluates:",
             "The proportional-odds model assumes each predictor's effect on the",
             "ordinal outcome has the same magnitude at every cutpoint between",
             "categories (the \"parallel lines\" assumption). Brant's (1990) test is the",
             "standard check.",
             "",
             "Where it is used in AssumptionsLab:",
             "ordCheck (ordinal logistic regression).",
             "",
             "It does not apply to binary logistic regression (logCheck) or",
             "multinomial regression (multCheck), since neither has multiple cutpoints",
             "whose slopes need to be compared against each other.",
             "",
             "Why it matters:",
             "When violated, a single reported odds ratio per predictor misrepresents",
             "its real effect - understating it at some cutpoints and overstating it at",
             "others - and can lead to mistaken substantive conclusions about which",
             "categories a predictor actually discriminates between."
         ),
         proportionalOddsTableHeaders = c("Diagnostic / alternative", "What it does", "When to use it", "Main limitation"),
         proportionalOddsTableRows = list(
             c("Brant test", "Tests whether each predictor's coefficient is equal across every cutpoint",
               "As an initial check right after fitting the model, before interpreting a single OR per predictor",
               "Limited power in small samples or sparse categories; a non-significant result is weak evidence of proportionality"),
             c("Partial proportional-odds model", "Lets only the predictors flagged by Brant vary by cutpoint, keeping the rest at a single coefficient",
               "When Brant flags one or two specific predictors, not the full omnibus",
               "Less standardized across statistical software; interpretation gets more complex when mixing constant and varying effects"),
             c("Multinomial model (multCheck)", "Abandons the proportionality assumption by treating each category as nominal",
               "When the violation is widespread (significant omnibus, several predictors affected)",
               "Loses the efficiency of exploiting the categories' order; more parameters to estimate"),
             c("Report cutpoint-specific effects", "Fits separate binary logits at each cutpoint and reports a different OR for each",
               "As a transparent alternative when the goal is documenting the pattern rather than forcing a single model",
               "No single pooled model summarizes the effect; comparisons across cutpoints lose some statistical efficiency")
         ),
         proportionalOddsPart2 = c(
             "",
             "",
             "Brant — what it evaluates and its limitations:",
             "Tests H0: each predictor's coefficient is equal across every cutpoint",
             "(proportional odds), against H1: at least one cutpoint differs (Brant,",
             "1990). It compares the coefficients from separate binary logits fit at",
             "each cutpoint against the single coefficient the proportional-odds model",
             "reports. It has limited power in small samples or with sparse",
             "categories.",
             "",
             "Partial proportional-odds model — what it does and when to use it:",
             "Lets a specific predictor's coefficient vary across cutpoints while",
             "every other predictor keeps a single constant coefficient. It is the",
             "most surgical option when Brant flags one or two specific predictors",
             "rather than a widespread violation.",
             "",
             "Multinomial model — what it does and when to use it:",
             "Treats every category of the outcome as nominal, with no assumed order",
             "or proportionality relationship between coefficients. It is the safer",
             "alternative when Brant's omnibus test is significant and affects",
             "several predictors at once, at the cost of not exploiting the",
             "categories' order.",
             "",
             "Reporting cutpoint-specific effects — what it does and when to use it:",
             "Instead of forcing a single model, independent binary logits are fit at",
             "each cutpoint and a different odds ratio is reported for each. It is a",
             "transparent alternative when the goal is describing the pattern of the",
             "relationship rather than producing a single parsimonious model.",
             "",
             "Interpretation:",
             "p < .05 on Brant's omnibus test suggests a statistically significant",
             "deviation from proportional odds. p >= .05 indicates the result is",
             "compatible with approximate proportional odds.",
             "",
             "Methodological decision:",
             "If there is evidence against proportional odds, first check which",
             "individual predictor(s) Brant's test flags (not just the omnibus). With",
             "one or two specific predictors, consider a partial-proportional-odds",
             "model; with a widespread violation, consider a multinomial model",
             "(multCheck) or report the cutpoint-specific effects separately."
         ),

         # ---- iia: see the matching ES block above for the design
         # rationale (new category, placed right after proportionalOdds as
         # its regression-family sibling assumption).
         # ---------------------------------------------------------------
         iiaPart1 = c(
             "Independence of Irrelevant Alternatives (IIA)",
             "",
             "What it evaluates:",
             "The multinomial-logit model assumes the relative odds between any two",
             "categories of the outcome do not depend on which other categories are",
             "also available in the choice set - Independence of Irrelevant",
             "Alternatives (IIA). It is the one assumption unique to this model family,",
             "with no equivalent in binary or ordinal logistic regression.",
             "",
             "Where it is used in AssumptionsLab:",
             "multCheck (multinomial logistic regression).",
             "",
             "It does not apply to binary logistic regression (logCheck), since with",
             "only two categories there are no \"irrelevant alternatives\" that could be",
             "dropped from the choice set. It also does not apply to ordinal logistic",
             "regression (ordCheck), which instead depends on the proportional-odds",
             "(parallel-lines) assumption across ordered cutpoints.",
             "",
             "Why it matters:",
             "When violated, the reported odds ratios between categories are not",
             "stable - they shift depending on which other categories are present in",
             "the choice set - which undermines interpreting a single set of",
             "coefficients as describing a consistent preference among alternatives."
         ),
         iiaTableHeaders = c("Diagnostic / alternative", "What it does", "When to use it", "Main limitation"),
         iiaTableRows = list(
             c("Hausman-McFadden test", "Refits the model omitting each outcome category in turn and compares the coefficients shared with the full model",
               "As an initial check right after fitting the multinomial model, before relying on the reported odds ratios",
               "The statistic is not always computable (the covariance-matrix difference can fail to be invertible in finite samples); a non-computable result is not proof that IIA holds"),
             c("Nested logit model", "Groups outcome categories into a hierarchical nest structure, allowing correlation between alternatives within the same nest",
               "When the IIA violation follows a substantively meaningful grouping pattern (e.g., categories sharing unobserved attributes)",
               "Requires specifying a reasonable nest structure a priori; results are sensitive to that choice"),
             c("Multinomial probit model", "Replaces the logit's independent-errors assumption with multivariate-normal errors, allowing correlation between alternatives",
               "When enough computing time is available and no natural nest structure exists",
               "Computationally more expensive; has no closed form and requires numerical integration or simulation"),
             c("Report coefficients with caution", "Instead of discarding the model, the categories flagged by Hausman-McFadden are documented and their odds ratios interpreted with added reservation",
               "As a transparent alternative when the violation affects only one or two specific categories rather than the general pattern",
               "Does not fix the underlying problem, only documents it, leaving interpretive caution to the reader")
         ),
         iiaPart2 = c(
             "",
             "",
             "Hausman-McFadden — what it evaluates and its limitations:",
             "Tests H0: the coefficients shared between the full model and a model",
             "refit without one category do not change beyond sampling error (IIA",
             "holds), against H1: they do change (Hausman & McFadden, 1984). It is",
             "computed once per omitted category. In finite samples, the statistic",
             "depends on the difference between two covariance matrices, which is not",
             "always invertible — a well-documented numerical degeneracy of this",
             "specific test, not evidence of a calculation error. When this happens,",
             "the result is reported as \"not computable\": this is not itself proof",
             "that IIA holds (Cheng & Long, 2007, document that Hausman-type IIA",
             "tests behave erratically in practice).",
             "",
             "Nested logit model — what it does and when to use it:",
             "Groups the outcome's categories into a hierarchical nest structure,",
             "allowing correlated unobserved factors within a nest while keeping",
             "independence across nests. It is the more principled alternative when",
             "the categories flagged by Hausman-McFadden share an identifiable",
             "substantive grouping.",
             "",
             "Multinomial probit model — what it does and when to use it:",
             "Drops the logit's independent-errors assumption altogether in favor of",
             "correlated, multivariate-normal errors across alternatives. It is the",
             "more general - but computationally heavier - alternative when no",
             "natural nest structure exists.",
             "",
             "Reporting coefficients with caution — what it does and when to use it:",
             "Rather than switching models, the categories flagged by the",
             "Hausman-McFadden test are documented explicitly and their odds ratios",
             "interpreted with added reservation. It is a transparent option when",
             "only one or two categories are flagged, not a widespread pattern.",
             "",
             "Interpretation:",
             "p < .05 on the Hausman-McFadden test for an omitted category suggests a",
             "statistically significant deviation from IIA for that category. p >=",
             ".05 indicates the result is compatible with IIA. A \"not computable\"",
             "result should not be read as evidence either for or against IIA: it is",
             "a known limitation of the test itself, not a substantive finding.",
             "",
             "Methodological decision:",
             "If at least one omitted category produces a significant statistic,",
             "consider a nested-logit or multinomial-probit model (neither requires",
             "IIA) before relying on this model's odds ratios, or document the",
             "flagged categories with explicit caution. If no category is",
             "significant - or the result is mostly \"not computable\" - this is",
             "compatible with (but not definitive proof of) IIA holding for this",
             "model."
         ),

         robustPart1 = c(
             "Transformations and Robust Alternatives",
             "",
             "What they assess:",
             "These are not assumptions in themselves, but methodological responses when",
             "a specific assumption is not defensible or when results are sensitive to",
             "important deviations from it.",
             "",
             "Where they're used in AssumptionsLab:",
             "Independent Groups, Related Groups, ANOVA/ANCOVA, and Regression.",
             "",
             "How to choose among them:",
             "The right alternative depends on which specific assumption fails, not on a",
             "general preference for \"robust\" methods. Switching methods without first",
             "identifying which assumption was violated tends to complicate",
             "interpretation without solving the real problem."
         ),
         robustTableHeaders = c("If the problem is...", "Common alternative", "Applies in", "Core idea"),
         robustTableRows = list(
             c("Unequal variances across groups", "Welch correction",
               "Independent Groups, ANOVA",
               "Adjusts the degrees of freedom of the t or F statistic without assuming equal variances (Welch, 1947)"),
             c("Unequal variances, with skewness or outliers", "Brown-Forsythe (as robust ANOVA)",
               "ANOVA",
               "Extends Welch's logic using location estimators more robust than the mean"),
             c("Non-normal but symmetric paired differences", "Wilcoxon signed-rank",
               "Related Groups (two measurements)",
               "Uses the ranks of the differences instead of their values, without assuming normality (Wilcoxon, 1945)"),
             c("Three or more related measurements, non-normal", "Friedman",
               "Related Groups (three or more measurements)",
               "Extends the rank-based logic to more than two related measurements (Friedman, 1937)"),
             c("Heteroscedastic residuals", "HC3 robust standard errors",
               "Regression",
               "Corrects the standard errors of coefficients without changing their point estimates (MacKinnon & White, 1985)"),
             c("Marked skewness of the dependent variable", "Transformation (log, root, inverse)",
               "Any analysis with a continuous variable",
               "Changes the variable's scale so its distribution more closely approaches normality"),
             c("Complex dependence structure (nested, longitudinal)", "Mixed models",
               "Repeated, nested, or hierarchical data",
               "Explicitly models the correlation between related observations instead of assuming independence")
         ),
         robustPart2 = c(
             "",
             "",
             "Transformations of the dependent variable:",
             "log(Y): useful with marked positive skewness and values strictly greater",
             "than 0. sqrt(Y): useful with count data or mild-to-moderate positive",
             "skewness, and values >= 0. 1/Y: useful only with caution for severe",
             "positive skewness and values not close to zero (the inverse transformation",
             "is highly sensitive near zero). Every transformation changes the scale on",
             "which results are interpreted, so this should be reported explicitly.",
             "",
             "Welch — what it does and when to use it:",
             "Adjusts the degrees of freedom of the t or F statistic so that equal",
             "variances across groups are not required (Welch, 1947). It is especially",
             "recommended when group sizes are unequal, because in that scenario the",
             "classic t-test or ANOVA is more sensitive to unequal variances than when",
             "groups are similarly sized.",
             "",
             "Brown-Forsythe (as a robust ANOVA alternative):",
             "Applies the same logic as Welch but using location estimators more robust",
             "than the mean, which makes it preferable when, in addition to unequal",
             "variances, there is skewness or outliers in the groups (Brown & Forsythe,",
             "1974).",
             "",
             "Wilcoxon signed-rank — what it does and when to use it:",
             "A nonparametric alternative for two related measurements: instead of",
             "comparing the paired differences directly, it converts them to ranks and",
             "tests whether the sum of positively signed ranks differs from what is",
             "expected under H0 of symmetry around zero (Wilcoxon, 1945). It requires the",
             "differences to be approximately symmetric, though not normal; with strong",
             "asymmetry in the differences, this test also loses validity.",
             "",
             "Friedman — what it does and when to use it:",
             "Extends the same rank-based logic to three or more related measurements,",
             "converting each row (subject) into ranks across conditions and testing",
             "whether those ranks differ systematically across conditions (Friedman,",
             "1937). It is the natural nonparametric alternative to repeated-measures",
             "ANOVA when normality of the differences is not defensible.",
             "",
             "HC3 robust errors — what they do and when to use them:",
             "Recompute a regression's coefficient standard errors so they remain valid",
             "even under heteroscedasticity, without changing the coefficients' point",
             "estimates (MacKinnon & White, 1985). HC3 is the recommended variant in",
             "small or moderate samples, because it corrects the bias in variance",
             "estimates better than other HC variants in that regime.",
             "",
             "Mixed models:",
             "Appropriate for nested, longitudinal, repeated, or otherwise",
             "dependence-structured data that a simple fixed-effects model cannot",
             "represent. Rather than correcting a violated assumption after the fact,",
             "they directly model the source of dependence (for example, variation",
             "across subjects or across groups) as part of the model structure.",
             "",
             "Methodological decision:",
             "Do not switch methods based on a single isolated test. The alternative",
             "chosen should respond to the overall pattern of evidence, the study",
             "design, and the research question — not just the p-value of one",
             "diagnostic."
         ),
         notes = c(
             "Recommended Use of Assumption Library",
             "",
             "Use this library as a general methodological reference.",
             "Use the specific analyses to interpret results with your own data."
         )
     ),
     # ---- time: timeCheck module strings ----
     # ES: time: cadenas del módulo timeCheck
     time = list(
         title = "Time Series",
         intro = c(
             "Time Series maps the statistical assumptions and methodological conditions",
             "that should be reviewed before and after fitting a time-series model. Unlike a",
             "single assumption, temporal modeling requires checking several conditions —",
             "stationarity, lag structure, specification, and residual behavior — whose",
             "exact relevance depends on the model chosen. Select a model to see its",
             "methodological matrix: Model → Assumptions/Conditions → Diagnostic →",
             "Methodological Role → Decision."
         ),
         interpretation = c(
             "The matrix shows only the conditions relevant to the selected model. Not all",
             "of them are strict mathematical requirements: applicability conditions (which",
             "must hold for the model to make sense), specification criteria (which guide",
             "the choice of parameters such as p, d, q, or the cointegration rank), and",
             "post-fit diagnostics (which evaluate whether the already-estimated model",
             "adequately captured the structure of the data) are distinguished. Conflating",
             "these categories is a common methodological error: for example, residual",
             "normality in an ARIMA model is a useful diagnostic, not a strict mathematical",
             "requirement for estimation."
         ),
         scopeNote = c(
             "This version implements the methodological matrix (Model → Assumptions/",
             "Conditions → Diagnostic → Methodological Role → Decision), the minimum",
             "recommended battery of statistical diagnostics per model (unit-root tests,",
             "ACF/PACF, Ljung-Box, ARCH-LM, Jarque-Bera, and for VAR/VECM/GARCH also",
             "Johansen, Portmanteau, CUSUM/Nyblom, and sign bias, depending on the selected",
             "model), diagnostic plots (series, ACF/PACF, residuals, and, for GARCH,",
             "conditional volatility), and optional external regressors for ARIMA/SARIMA",
             "(ARIMAX/SARIMAX). Every result is classified as a graded evidence tier, never",
             "as an absolute \"assumption met/not met\" verdict."
         ),
         seriesRowLabel = "Time series",
         modelRowLabel = "Selected model",
         noSeriesSelected = "No time series variable has been selected.",
         modelLabels = list(
             arima = "ARIMA",
             sarima = "SARIMA",
             ets = "ETS / Exponential Smoothing",
             var = "VAR",
             vecm = "VECM",
             garch = "GARCH"
         ),
         modelDescriptions = list(
             arima = c(
                 "ARIMA (AutoRegressive Integrated Moving Average) models a single time series",
                 "from its own past: its lagged values (autoregressive component, p), the",
                 "number of differences needed to make it stationary (integrated component, d),",
                 "and lagged errors (moving-average component, q). Use it when you have a single",
                 "series without marked seasonality, or when you want to model an already",
                 "deseasonalized series. It offers short- to medium-term forecasts with",
                 "confidence intervals, and is the conceptual foundation for SARIMA, ARIMAX, and",
                 "GARCH. On its own it does not capture relationships with other series (that is",
                 "what VAR/VECM are for) or changes in variance over time (that is what GARCH is",
                 "for)."
             ),
             sarima = c(
                 "SARIMA extends ARIMA by adding a seasonal component (P, D, Q, s) that",
                 "explicitly models the repetition of patterns every s periods (for example,",
                 "every 12 months in monthly data). Use it when the series shows a clear,",
                 "recognizable seasonal pattern \u2014 sales with holiday peaks, electricity",
                 "consumption with daily or weekly cycles, tourism with a high season \u2014 that a",
                 "plain ARIMA could not capture. It is this module's default model precisely",
                 "because it covers the most frequent case in applied practice. In exchange for",
                 "that added power, it requires more historical data (ideally several complete",
                 "cycles) and a correctly specified periodicity (frequency)."
             ),
             ets = c(
                 "ETS (Error, Trend, Seasonal) is a family of exponential-smoothing models that",
                 "decomposes the series into level, trend, and seasonality, each with its own",
                 "form (none, additive, or multiplicative, with or without damping). Unlike",
                 "ARIMA/SARIMA, it does not rely on unit-root tests or differencing: it",
                 "explicitly weights recent observations more heavily than older ones. It is a",
                 "strong alternative when the series has a relatively simple and stable trend",
                 "and/or seasonality, and tends to be easier to interpret and communicate than an",
                 "equivalent SARIMA. It is less suited when the dynamics depend on other",
                 "variables (use VAR/VECM/ARIMAX) or when volatility itself is the object of",
                 "study (use GARCH)."
             ),
             var = c(
                 "VAR (Vector AutoRegressive) models several series simultaneously, where each",
                 "one is explained by its own lags and by the lags of every other series in the",
                 "system, without imposing in advance which variable causes which. Use it when",
                 "you are interested in the joint dynamics between two or more stationary series",
                 "\u2014 for example, interest rate and inflation, or sales of complementary",
                 "products \u2014 and want to study how a shock in one series propagates to the",
                 "others (impulse-response functions) without specifying rigid causal",
                 "relationships. It requires all series to be stationary (or treated to become",
                 "so); if the series share a long-run equilibrium relationship while each is",
                 "individually non-stationary, VECM is usually the more appropriate choice",
                 "instead."
             ),
             vecm = c(
                 "VECM (Vector Error Correction Model) is the version of VAR appropriate when",
                 "the series are not individually stationary but are cointegrated: there is a",
                 "linear combination of them that is stationary, reflecting a long-run",
                 "equilibrium relationship (for example, between the exchange rate and relative",
                 "prices, or between consumption and income). Use it when theory or evidence",
                 "suggest the series move together in the long run even though they may drift",
                 "apart temporarily in the short run. Besides the short-run dynamics typical of a",
                 "VAR, it offers an explicit error-correction mechanism that pushes the system",
                 "back toward equilibrium. Fitting a VECM without first checking for",
                 "cointegration (with the Johansen test) is a common methodological error:",
                 "without genuine cointegration, a VAR in differences is usually the correct",
                 "choice."
             ),
             garch = c(
                 "GARCH (Generalized AutoRegressive Conditional Heteroskedasticity) does not",
                 "model the level of the series but its conditional variance: it allows",
                 "volatility to change over time and cluster into calm and turbulent periods,",
                 "instead of assuming it is constant. Use it with high-frequency financial or",
                 "economic series (asset returns, exchange rates, interest rates) where the",
                 "magnitude of fluctuations varies systematically over time. It offers",
                 "risk/volatility estimates that feed more realistic prediction intervals than",
                 "an ARIMA with constant variance. It assumes the input series is already",
                 "approximately mean-stationary (typically returns, not price levels); if there",
                 "is asymmetry between positive and negative shocks, it is worth considering",
                 "GJR-GARCH or EGARCH as alternatives."
             )
         ),
         matrix = list(
                arima = list(
                  c(
                      "Stationarity",
                      "Stationarity / unit-root behavior",
                      "ADF / KPSS / PP",
                      "Condition for determining whether the series requires differencing.",
                      "Guides non-seasonal differencing (d)."
                  ),
                  c(
                      "Temporal dependence",
                      "Autocorrelation and partial autocorrelation",
                      "ACF / PACF",
                      "Supports identification of the p and q orders.",
                      "Guides ARIMA specification."
                  ),
                  c(
                      "Residuals",
                      "Residual independence",
                      "Ljung-Box",
                      "Checks whether temporal structure remains after fitting.",
                      "Revise the specification if residual autocorrelation remains."
                  ),
                  c(
                      "Residuals",
                      "Residual distribution (diagnostic)",
                      "Q-Q / Shapiro-Wilk",
                      "Evaluates the adequacy of the error distribution.",
                      "Use as a complementary diagnostic; do not treat it as a universal mathematical requirement."
                  )
                ),
                sarima = list(
                  c(
                      "Stationarity",
                      "Non-seasonal stationarity / unit root",
                      "ADF / KPSS / PP",
                      "Determines the need for non-seasonal differencing.",
                      "Guides d and the transformation of the series."
                  ),
                  c(
                      "Seasonality",
                      "Seasonal structure",
                      "ACF / PACF / seasonal diagnostic",
                      "Determines whether systematic dependence exists at seasonal lags.",
                      "Guides D and the seasonal orders P and Q."
                  ),
                  c(
                      "Temporal dependence",
                      "Autocorrelation and partial autocorrelation",
                      "ACF / PACF",
                      "Supports identification of the non-seasonal orders p and q.",
                      "Guides SARIMA specification."
                  ),
                  c(
                      "Residuals",
                      "Residual independence",
                      "Ljung-Box",
                      "Checks whether temporal structure remains after fitting.",
                      "Revise the specification if residual autocorrelation remains."
                  ),
                  c(
                      "Residuals",
                      "Residual distribution (diagnostic)",
                      "Q-Q / Shapiro-Wilk",
                      "Evaluates the adequacy of the error distribution.",
                      "Use as a complementary diagnostic; do not treat it as a universal mathematical requirement."
                  )
                ),
                ets = list(
                  c(
                      "Temporal structure",
                      "Temporal dependence and systematic pattern",
                      "ACF / PACF / time plots",
                      "Identifies structure that should be explained by level, trend, and seasonality.",
                      "Guides the specification of the ETS components."
                  ),
                  c(
                      "Seasonality",
                      "Presence and stability of the seasonal pattern",
                      "Seasonal ACF / graphical diagnostic",
                      "Determines whether a seasonal component is needed.",
                      "Guides ETS specification."
                  ),
                  c(
                      "Residuals",
                      "Absence of residual autocorrelation",
                      "Ljung-Box / residual ACF",
                      "Checks that the model has captured the relevant temporal structure.",
                      "Revise the specification if dependence remains."
                  ),
                  c(
                      "Residuals",
                      "Error distribution and variance (diagnostic)",
                      "Q-Q / ACF / residual variability",
                      "Evaluates the adequacy of the errors for inference and forecasting.",
                      "Consider transformation or an alternative specification when appropriate."
                  )
                ),
                var = list(
                  c(
                      "Stationarity",
                      "Stationarity of the system's series",
                      "ADF / KPSS / PP",
                      "Determines whether the series are suitable for a VAR in levels or require treatment.",
                      "Avoids specifications incompatible with the order of integration."
                  ),
                  c(
                      "Specification",
                      "Lag structure",
                      "AIC / BIC / HQ criteria + lag diagnostics",
                      "Determines an appropriate lag length.",
                      "Selects the VAR's temporal specification."
                  ),
                  c(
                      "Residuals",
                      "Absence of residual autocorrelation",
                      "LM / Portmanteau / multivariate Ljung-Box",
                      "Checks that no systematic temporal dependence remains.",
                      "Revise the number of lags or the specification."
                  ),
                  c(
                      "Residuals",
                      "Residual heteroscedasticity (diagnostic)",
                      "Heteroscedasticity tests",
                      "Evaluates the stability of the error variance.",
                      "Consider robust inference or an alternative specification."
                  ),
                  c(
                      "Residuals",
                      "Multivariate normality (diagnostic)",
                      "Mardia / Q-Q diagnostic",
                      "Evaluates a condition useful for certain inferential procedures.",
                      "Treat as a diagnostic, not as a universal VAR requirement."
                  )
                ),
                vecm = list(
                  c(
                      "Integration",
                      "Series with a compatible order of integration",
                      "ADF / KPSS / PP",
                      "Determines the order of integration of the variables.",
                      "Verifies the precondition for considering cointegration."
                  ),
                  c(
                      "Cointegration",
                      "Existence and rank of cointegration",
                      "Johansen",
                      "Determines how many long-run equilibrium relationships exist.",
                      "Guides the VECM's cointegration rank."
                  ),
                  c(
                      "Specification",
                      "Lag structure",
                      "AIC / BIC / HQ",
                      "Determines a lag length compatible with the system.",
                      "Guides the dynamic specification."
                  ),
                  c(
                      "Residuals",
                      "Absence of residual autocorrelation",
                      "LM / Portmanteau",
                      "Checks whether unexplained temporal dependence remains.",
                      "Revise lags or specification if autocorrelation is present."
                  ),
                  c(
                      "Residuals",
                      "Heteroscedasticity and normality (diagnostic)",
                      "Heteroscedasticity tests / Mardia",
                      "Evaluates the properties of the errors for inference.",
                      "Use robust diagnostics when distributional assumptions do not hold."
                  )
                ),
                garch = list(
                  c(
                      "Conditional variance",
                      "ARCH effects / conditional heteroscedasticity",
                      "ARCH-LM",
                      "Determines whether the variance depends on past information.",
                      "Justifies considering a volatility model."
                  ),
                  c(
                      "Stationarity",
                      "Stability condition of the variance process",
                      "Parameter diagnostic / persistence",
                      "Evaluates whether the variance dynamics are compatible with a stable process.",
                      "Revise the specification if persistence implies instability."
                  ),
                  c(
                      "Standardized residuals",
                      "Absence of autocorrelation in the mean",
                      "ACF / Ljung-Box",
                      "Checks that the mean dynamics have been adequately captured.",
                      "Revise the mean equation if dependence remains."
                  ),
                  c(
                      "Standardized residuals",
                      "Absence of residual ARCH effects",
                      "ARCH-LM / ACF of squared residuals",
                      "Checks that the volatility dynamics have been captured.",
                      "Revise the GARCH specification if ARCH effects remain."
                  ),
                  c(
                      "Distribution",
                      "Distribution of the innovations (diagnostic)",
                      "Q-Q / Jarque-Bera / chosen distribution",
                      "Evaluates the adequacy of the assumed distribution for the innovations.",
                      "Consider an alternative distribution when appropriate."
                  )
                )
         ),
         actions = list(
             arima = list(
                 stationarity = list(justification = "",
                     suggestion = "apply the suggested number of regular differences (d) before fitting the model, or let auto.arima determine it automatically; alternatively, use a variance-stabilizing transformation"),
                 ndiffs = list(justification = "",
                     suggestion = "apply the suggested number of regular differences (d) before fitting the model"),
                 ljungBox = list(justification = "",
                     suggestion = "increase the AR/MA order (p, q) or check for a missing structural component, considering an ARIMA with more lags"),
                 archLM = list(justification = "Residuals show conditional heteroscedasticity.",
                     suggestion = "model the variance (ARIMA-GARCH) or use robust errors for inference"),
                 jarqueBera = list(justification = "",
                     suggestion = "use Student-t innovations or bootstrap for intervals and tests, rather than assuming normal errors"),
                 roots = list(justification = "The fitted model is not stationary/invertible.",
                     suggestion = "review the ARIMA order or the differencing applied")
             ),
             sarima = list(
                 stationarity = list(justification = "",
                     suggestion = "apply the suggested number of regular differences (d) before fitting the model, or consider a variance-stabilizing transformation"),
                 ndiffs = list(justification = "",
                     suggestion = "apply the suggested number of regular differences (d) before fitting the model"),
                 nsdiffs = list(justification = "",
                     suggestion = "apply a seasonal difference (D) as suggested before proceeding"),
                 ljungBox = list(justification = "",
                     suggestion = "increase the non-seasonal AR/MA order (p, q) or check for a missing structural component"),
                 ljungBoxSeasonal = list(justification = "Seasonal structure may still be uncaptured.",
                     suggestion = "review the seasonal component (P, D, Q)"),
                 archLM = list(justification = "Residuals show conditional heteroscedasticity.",
                     suggestion = "model the variance (SARIMA-GARCH) or use robust errors for inference"),
                 jarqueBera = list(justification = "",
                     suggestion = "use Student-t innovations or bootstrap for intervals and tests, rather than assuming normal errors"),
                 roots = list(justification = "The fitted model is not stationary/invertible.",
                     suggestion = "review the SARIMA order or the differencing applied")
             ),
             ets = list(
                 ljungBox = list(justification = "",
                     suggestion = "add an additional error, trend, or seasonal component to the ETS specification"),
                 archLM = list(justification = "Residuals show conditional heteroscedasticity.",
                     suggestion = "transform the series or consider a model with non-constant variance"),
                 jarqueBera = list(justification = "",
                     suggestion = "use bootstrap or non-normal innovations for intervals and forecasts, rather than assuming normal errors")
             ),
             var = list(
                 stationarity = list(justification = "",
                     suggestion = "evaluate a VAR in differences or cointegration (VECM) instead of a VAR in levels, if several series are non-stationary"),
                 stability = list(justification = "The system is unstable.",
                     suggestion = "reduce the lag order, review the specification, or difference the series"),
                 serial = list(justification = "Residual serial correlation suggests the current order is insufficient.",
                     suggestion = "increase the number of VAR lags"),
                 archLM = list(justification = "",
                     suggestion = "consider a VAR with heteroscedasticity-robust errors or a multivariate volatility model (e.g., BEKK-GARCH)"),
                 jarqueBera = list(justification = "",
                     suggestion = "use robust inference (bootstrap) rather than relying on multivariate normality for hypothesis tests"),
                 cusum = list(justification = "At least one equation shows parameter instability.",
                     suggestion = "consider a VAR with structural change or estimate by sub-periods")
             ),
             vecm = list(
                 stationarity = list(justification = "",
                     suggestion = "confirm the series are integrated of order 1 before proceeding with VECM; if any turns out stationary in levels, reconsider its inclusion"),
                 johansen = list(justification = "",
                     suggestion = "use the suggested cointegration rank as a starting point, confirming it against theoretical judgment and the full trace matrix before fixing it"),
                 stability = list(justification = "The system converted to VAR is unstable.",
                     suggestion = "review the cointegration rank or the lag order"),
                 serial = list(justification = "Residual serial correlation suggests uncaptured structure.",
                     suggestion = "increase the VECM's lag order K"),
                 archLM = list(justification = "",
                     suggestion = "consider a specification with heteroscedasticity-robust errors"),
                 jarqueBera = list(justification = "",
                     suggestion = "use robust inference (bootstrap) rather than relying on multivariate normality")
             ),
             garch = list(
                 stationarity = list(justification = "",
                     suggestion = "transform the series to returns (log-differences) before fitting the GARCH model, if it is not stationary"),
                 ljungBox = list(justification = "The conditional mean dynamics may not be well captured.",
                     suggestion = "add a mean structure (ARMA) before the GARCH component"),
                 archLM = list(justification = "The GARCH(1,1) did not capture all the conditional heteroscedasticity.",
                     suggestion = "consider a higher GARCH(p,q) order"),
                 persistence = list(justification = "",
                     suggestion = "consider an IGARCH model or review the sample length, given that persistence is very high (near 1)"),
                 signBias = list(justification = "There is evidence of asymmetry in how variance responds to positive/negative shocks.",
                     suggestion = "evaluate GJR-GARCH or EGARCH instead of a symmetric GARCH"),
                 nyblom = list(justification = "Parameters are not stable over time.",
                     suggestion = "re-estimate by sub-periods or consider a model with time-varying parameters"),
                 gof = list(justification = "The distribution fit is inadequate.",
                     suggestion = "consider Student-t or generalized error (GED) innovations instead of the normal distribution")
             )
         ),
         # ------------------------------------------------------------------
         # Synthesized methodological reading, by diagnostic domain. Mirrors
         # the Spanish "reading" block above — see its comment for the
         # domain/template contract. Pilot: only "stationarity" populated.
         # ------------------------------------------------------------------
         reading = list(
             stationarity = list(
                 single = list(
                     none = "The %s test finds no evidence of a unit root/non-stationarity in the series, supporting treating it as stationary.",
                     moderate = "The %s test offers moderate evidence of non-stationarity in the series.",
                     clear = "The %s test offers clear evidence of non-stationarity in the series.",
                     info = "The %s test is reported for informational purposes.",
                     na = "The %s test could not be computed for this run."
                 ),
                 all_none = "The %d unit-root/stationarity tests run agree in showing no evidence of non-stationarity, supporting treating the series as stationary.",
                 all_deviant = "The %d unit-root/stationarity tests run agree in signaling evidence of non-stationarity.",
                 mixed = "Of the %2$d unit-root/stationarity tests run, %1$d show no evidence of non-stationarity, while %3$s %4$s signal it; the evidence on the series' stationarity is mixed."
             ),
             residualAutocorr = list(
                 single = list(
                     none = "The %s test finds no evidence of residual autocorrelation, supporting that the model captured the relevant temporal structure.",
                     moderate = "The %s test offers moderate evidence of residual autocorrelation not captured by the model.",
                     clear = "The %s test offers clear evidence of residual autocorrelation not captured by the model.",
                     info = "The %s test is reported for informational purposes.",
                     na = "The %s test could not be computed for this run."
                 ),
                 all_none = "The %d residual-autocorrelation tests run agree in showing no evidence of remaining temporal structure, supporting the current model specification.",
                 all_deviant = "The %d residual-autocorrelation tests run agree in signaling temporal structure not captured by the model.",
                 mixed = "Of the %2$d residual-autocorrelation tests run, %1$d show no evidence of remaining structure, while %3$s %4$s signal it; the evidence on the adequacy of the specification is mixed."
             ),
             heteroscedasticity = list(
                 single = list(
                     none = "The %s test finds no evidence of conditional heteroscedasticity in the residuals.",
                     moderate = "The %s test offers moderate evidence of conditional heteroscedasticity not captured in the residuals.",
                     clear = "The %s test offers clear evidence of conditional heteroscedasticity not captured in the residuals.",
                     info = "The %s test is reported for informational purposes.",
                     na = "The %s test could not be computed for this run."
                 ),
                 all_none = "The %d conditional-heteroscedasticity tests run agree in showing no evidence of uncaptured residual variance.",
                 all_deviant = "The %d conditional-heteroscedasticity tests run agree in signaling uncaptured residual variance.",
                 mixed = "Of the %2$d conditional-heteroscedasticity tests run, %1$d show no evidence of uncaptured residual variance, while %3$s %4$s signal it; the evidence is mixed."
             ),
             normality = list(
                 single = list(
                     none = "The %s test finds no evidence of deviation from normality in the residuals.",
                     moderate = "The %s test offers moderate evidence of non-normality in the residuals.",
                     clear = "The %s test offers clear evidence of non-normality in the residuals; keep in mind this is a complementary diagnostic, not a strict mathematical requirement for most models covered here.",
                     info = "The %s test is reported for informational purposes.",
                     na = "The %s test could not be computed for this run."
                 ),
                 all_none = "The %d normality tests run agree in showing no evidence of deviation from normality in the residuals.",
                 all_deviant = "The %d normality tests run agree in signaling deviation from normality in the residuals.",
                 mixed = "Of the %2$d normality tests run, %1$d show no evidence of deviation from normality, while %3$s %4$s signal it; the evidence is mixed."
             ),
             stability = list(
                 single = list(
                     none = "The %s diagnostic shows no evidence of instability in the fitted model.",
                     moderate = "The %s diagnostic offers moderate evidence of instability in the fitted model.",
                     clear = "The %s diagnostic offers clear evidence of instability in the fitted model.",
                     info = "The %s diagnostic is reported for informational purposes.",
                     na = "The %s diagnostic could not be computed for this run."
                 ),
                 all_none = "The %d stability diagnostics run agree in showing no evidence of instability in the fitted model.",
                 all_deviant = "The %d stability diagnostics run agree in signaling instability in the fitted model.",
                 mixed = "Of the %2$d stability diagnostics run, %1$d show no evidence of instability, while %3$s %4$s signal it; the evidence on the model's stability is mixed."
             ),
             cointegration = list(
                 single = list(
                     none = "%s finds no evidence of cointegration between the series.",
                     moderate = "%s offers moderate evidence of cointegration between the series.",
                     clear = "%s offers clear evidence of cointegration between the series.",
                     info = "%s suggests, for informational purposes, a cointegration rank.",
                     na = "%s could not be computed for this run."
                 ),
                 informational = "The Johansen tests run (trace and max-eigenvalue) suggest, for informational purposes, a cointegration rank; it should be weighed against theoretical judgment and the full trace matrix before being fixed.",
                 all_none = "The %d cointegration tests run agree in showing no evidence of a cointegrating relationship.",
                 all_deviant = "The %d cointegration tests run agree in signaling a cointegrating relationship between the series.",
                 mixed = "Of the %2$d cointegration tests run, %1$d show no evidence of cointegration, while %3$s %4$s signal it; the evidence is mixed."
             ),
             garchPersistence = list(
                 single = list(
                     none = "%s falls within a typical range, with no sign of a near-integrated variance process.",
                     moderate = "%s is reported at a moderate level of evidence.",
                     clear = "%s is very high, close to integration, suggesting volatility shocks decay very slowly.",
                     info = "%s is reported for informational purposes.",
                     na = "Conditional variance persistence could not be computed for this run."
                 )
             ),
             garchAsymmetry = list(
                 single = list(
                     none = "The %s test finds no evidence of asymmetry in the variance's response to positive/negative shocks.",
                     moderate = "The %s test offers moderate evidence of asymmetry in the variance's response.",
                     clear = "The %s test offers clear evidence of asymmetry in the variance's response to positive/negative shocks.",
                     info = "The %s test is reported for informational purposes.",
                     na = "The sign-bias test could not be computed for this run."
                 )
             ),
             garchGof = list(
                 single = list(
                     none = "The %s test finds no evidence that the distribution assumed for the innovations is inadequate.",
                     moderate = "The %s test offers moderate evidence that the assumed distribution does not fit well.",
                     clear = "The %s test offers clear evidence that the assumed distribution does not fit the innovations well.",
                     info = "The %s test is reported for informational purposes.",
                     na = "The goodness-of-fit test could not be computed for this run."
                 )
             )
         ),
         foundations = list(
             unitRoot = c(
                 "Unit roots (ADF, Phillips-Perron, KPSS)",
                 "ADF (Dickey & Fuller, 1979) and Phillips-Perron (Phillips & Perron, 1988) share the",
                 "same null hypothesis \u2014 the series has a unit root, i.e. it is non-stationary \u2014 but",
                 "correct for possible residual autocorrelation and heteroscedasticity differently: ADF",
                 "adds lagged differences as regressors; Phillips-Perron adjusts the statistic with a",
                 "nonparametric, Newey-West-type correction. KPSS (Kwiatkowski et al., 1992)",
                 "reverses the logic: its null hypothesis is that the series IS",
                 "stationary. This is why the three tests are reported together: if ADF and",
                 "Phillips-Perron reject the unit root (small p) and KPSS fails to reject",
                 "stationarity (large p), the evidence converges clearly. When the two families of",
                 "tests disagree, it usually points to a series with weak long-run components or an",
                 "insufficient sample to distinguish between the two hypotheses, not a computational",
                 "error."
             ),
             seasonalUnitRoot = c(
                 "Seasonal unit root (Canova-Hansen, OCSB)",
                 "Canova and Hansen (1995) test whether the seasonal pattern is stable over time or",
                 "changes enough to require seasonal differencing; forecast::nsdiffs() uses this test",
                 "by default. OCSB (Osborn et al., 1988) evaluates a related",
                 "hypothesis through an auxiliary regression with simultaneous seasonal and",
                 "non-seasonal lags. Both tests usually agree on series with clear seasonality; when",
                 "they differ, it is reasonable to prefer Canova-Hansen's suggestion for the D order",
                 "in a SARIMA, since it is the one implemented by default in the automatic",
                 "forecasting literature (Hyndman & Khandakar, 2008), and use OCSB as a cross-check."
             ),
             ljungBox = c(
                 "Residual independence (Ljung-Box)",
                 "Ljung and Box (1978) improved on the original Box-Pierce test by correcting its",
                 "asymptotic approximation, especially relevant in small or moderate samples. The",
                 "test jointly evaluates several lags of residual autocorrelation at once, not just",
                 "one; this is why the chosen number of lags matters: too few may miss real",
                 "dependence, and too many dilute the test's power by mixing lags with no genuine",
                 "autocorrelation alongside those that do have it."
             ),
             archGarch = c(
                 "Conditional heteroscedasticity (ARCH-LM, GARCH)",
                 "Engle (1982) introduced ARCH models from a central empirical observation in",
                 "financial and macroeconomic series: periods of high volatility tend to cluster in",
                 "time, rather than being randomly distributed. The Lagrange multiplier test",
                 "(ARCH-LM) tests exactly that: whether the variance of the residuals at a given",
                 "moment depends on its own magnitude at previous moments. Bollerslev (1986)",
                 "generalized the model to GARCH, allowing the conditional variance to also depend on",
                 "its own lagged values, not only on past shocks, which drastically reduces the",
                 "number of parameters needed compared to a high-order ARCH to capture the same",
                 "persistence."
             ),
             jarqueBera = c(
                 "Residual normality (Jarque-Bera)",
                 "Jarque and Bera (1987) built a joint test of skewness and kurtosis: under",
                 "normality, both should be close to zero (kurtosis in its excess form). The",
                 "statistic combines the two deviations into a single chi-square test with 2 degrees",
                 "of freedom. It is, above all, a useful diagnostic about the shape of the residuals,",
                 "not a strict mathematical requirement for estimating most of the temporal models",
                 "covered here; its practical relevance depends on whether the downstream inference",
                 "procedure (intervals, forecasts) explicitly assumes normal errors."
             ),
             cointegration = c(
                 "Cointegration (Johansen, Engle-Granger)",
                 "Engle & Granger (1987) formalized the idea of cointegration: even though each",
                 "series may be individually non-stationary (integrated of order 1), a specific",
                 "linear combination of several of them can be stationary, reflecting a long-run",
                 "equilibrium relationship. Johansen (1991) extended this idea into a maximum",
                 "likelihood framework within a VAR, allowing a formal test of how many independent",
                 "cointegrating relationships exist (the rank) via the trace and maximum-eigenvalue",
                 "tests, instead of estimating a single relationship as in the original Engle-Granger",
                 "two-step procedure."
             ),
             varFoundations = c(
                 "VAR/VECM specification",
                 "The reference treatment for lag-order selection, multivariate residual diagnostics,",
                 "and the conversion between VAR and VECM representations follows the framework laid",
                 "out by Lütkepohl (2005), the standard text on multivariate time-series models. The",
                 "information criteria (AIC, BIC, HQ) used by VARselect() compete in a consistent",
                 "direction: AIC tends to favor more heavily parameterized models (more lags), while",
                 "BIC penalizes complexity more severely and tends to suggest more parsimonious",
                 "specifications; when both agree, the chosen order is more trustworthy."
             ),
             parameterStability = c(
                 "Parameter stability (CUSUM, Nyblom)",
                 "Brown et al., (1975) proposed the CUSUM of recursive residuals to detect",
                 "whether a regression's coefficients remain constant across the sample or whether a",
                 "structural change occurs at some point not specified in advance. Nyblom (1989)",
                 "generalized the idea to a framework where parameters can vary gradually as a",
                 "martingale process, rather than only at a single discrete break point, which makes",
                 "it especially suited for evaluating the joint stability of an already-fitted",
                 "GARCH model's parameters."
             ),
             asymmetricGarch = c(
                 "Volatility asymmetry (sign bias, GJR, EGARCH)",
                 "A symmetric GARCH assumes that a positive and a negative shock of the same",
                 "magnitude affect future variance equally. Engle and Ng (1993) designed the sign",
                 "bias test precisely to detect when that assumption fails \u2014 a very common pattern",
                 "in equity returns, where declines tend to raise volatility more than equally sized",
                 "gains (the leverage effect). When the test is significant, two families of models",
                 "respond to that asymmetry differently: GJR-GARCH (Glosten et al., 1993)",
                 "adds a term that only activates on negative shocks, while EGARCH (Nelson,",
                 "1991) models the logarithm of the variance, which also avoids having to constrain",
                 "the parameters to positive values to guarantee a valid variance."
             ),
             autoArima = c(
                 "Automatic order selection (Hyndman-Khandakar)",
                 "The algorithm behind forecast::auto.arima() (Hyndman & Khandakar, 2008) does not",
                 "exhaustively test every combination of p, d, q: it starts from the differencing",
                 "orders suggested by the unit-root tests and then runs a stepwise search over the",
                 "space of neighboring models, comparing by AICc. This makes it much faster than an",
                 "exhaustive search, but it means the result is a reasonable local optimum, not",
                 "guaranteed to be the best possible model; for difficult series it is worth",
                 "cross-checking the result against visual inspection of the ACF/PACF."
             )
         )
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
# Normalize a report-language value to a canonical "es"/"en" code.
# ES: Normalizar un valor de idioma de informe a un código canónico "es"/"en".
#
# jamovi's reportLang option, or a value typed by a caller, may arrive as an
# option code, a language name, or in mixed case. Every other helper in this
# file assumes a canonical two-letter code, so this normalization must run
# first.
#
# ES: La opción reportLang de jamovi, o un valor escrito por quien llama,
# puede llegar como un código de opción, un nombre de idioma, o en
# mayúsculas mixtas. Todo otro auxiliar de este archivo asume un código
# canónico de dos letras, así que esta normalización debe ejecutarse
# primero.
#
# Input: lang - the raw language value (character, or NULL/NA).
# Output: "es" or "en"; defaults to "es" when the input is missing or
# unrecognized, matching the suite's default report language.
#
# ES: Entrada: lang - el valor de idioma sin procesar (character, o
# NULL/NA).
# ES: Salida: "es" o "en"; por defecto "es" cuando la entrada falta o no se
# reconoce, coincidiendo con el idioma de informe por defecto de la suite.
# -----------------------------------------------------------------------------
.al_normalize_lang <- function(lang) {
    if (is.null(lang) || length(lang) == 0 || is.na(lang))
        return("es")
    lang <- as.character(lang)[1]
    lang_low <- tolower(lang)
    if (lang_low %in% c("es", "esp", "español", "espanol", "spanish"))
        return("es")
    if (lang_low %in% c("en", "eng", "english", "inglés", "ingles"))
        return("en")
    "es"
}

# -----------------------------------------------------------------------------
# Look up one text string by language, section, and key.
# ES: Buscar una cadena de texto por idioma, sección y clave.
#
# Resolves a (language, section, key) triple against .al_texts. If the
# string is missing in the requested language (a translation gap), it falls
# back to the English entry rather than silently rendering nothing, since
# English is this suite's primary/complete language (see CODE_STYLE.md
# section 9).
#
# ES: Resuelve una tripleta (idioma, sección, clave) contra .al_texts. Si la
# cadena falta en el idioma solicitado (un vacío de traducción), retrocede
# a la entrada en inglés en vez de renderizar nada silenciosamente, ya que
# el inglés es el idioma primario/completo de esta suite (ver
# CODE_STYLE.md sección 9).
#
# Inputs: lang - raw language value, normalized internally; section - the
# top-level module key in .al_texts (e.g. "path"); key - the specific text
# key within that section.
# Output: the resolved character string, or a "[missing text: ...]"
# placeholder if the key does not exist in either language, so a missing
# translation is visible in the report instead of silently blank.
#
# ES: Entradas: lang - valor de idioma sin procesar, normalizado
# internamente; section - la clave de módulo de primer nivel en .al_texts
# (p. ej. "path"); key - la clave de texto específica dentro de esa
# sección.
# ES: Salida: la cadena resuelta, o un marcador "[missing text: ...]" si la
# clave no existe en ninguno de los dos idiomas, para que una traducción
# faltante sea visible en el informe en vez de quedar en blanco en
# silencio.
# -----------------------------------------------------------------------------
.al_text <- function(lang, section, key) {
    lang <- .al_normalize_lang(lang)
    value <- NULL
    if (!is.null(.al_texts[[lang]]) &&
        !is.null(.al_texts[[lang]][[section]]) &&
        !is.null(.al_texts[[lang]][[section]][[key]])) {
        value <- .al_texts[[lang]][[section]][[key]]
    }
    if (is.null(value) &&
        !is.null(.al_texts[["en"]]) &&
        !is.null(.al_texts[["en"]][[section]]) &&
        !is.null(.al_texts[["en"]][[section]][[key]])) {
        value <- .al_texts[["en"]][[section]][[key]]
    }
    if (is.null(value))
        value <- paste0("[missing text: ", section, ".", key, "]")
    value
}

# -----------------------------------------------------------------------------
# Normalize whitespace in one or more text fragments.
# ES: Normalizar espacios en blanco en uno o más fragmentos de texto.
#
# .al_texts stores each paragraph as a character vector of short lines (for
# readability in this source file). Before those lines can be joined into a
# single rendered paragraph, embedded newlines/tabs and repeated spaces must
# be collapsed, and any empty fragment dropped.
#
# ES: .al_texts guarda cada párrafo como un vector de caracteres de líneas
# cortas (por legibilidad en este archivo fuente). Antes de que esas líneas
# puedan unirse en un único párrafo renderizado, hay que colapsar los
# saltos de línea/tabulaciones incrustados y los espacios repetidos, y
# descartar cualquier fragmento vacío.
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
