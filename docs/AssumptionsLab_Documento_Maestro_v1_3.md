# AssumptionsLab – Documento Maestro v1.3
## Integración Library + Bibliografía + Estructura del Proyecto

---

## 1. Propósito del documento

Este documento consolida la versión 1.3 del sistema AssumptionsLab aplicado a Jamovi, integrando:

- Estructura conceptual del proyecto
- Sistema de Library (biblioteca de contenidos)
- Sistema de Bibliografía unificada
- Flujo pedagógico orientado a aprendizaje estadístico
- Diseño modular para uso académico

El objetivo es servir como fuente única de referencia para estudiantes y docentes, centralizando contenido teórico, práctico y fuentes externas.

---

## 2. Filosofía del sistema

AssumptionsLab se basa en tres principios fundamentales:

### 2.1 Aprendizaje basado en comprensión
El estudiante no solo ejecuta análisis en Jamovi, sino que comprende las decisiones estadísticas detrás de cada procedimiento.

### 2.2 Transparencia de supuestos
Cada técnica estadística está acompañada de:
- Supuestos
- Condiciones de uso
- Interpretación correcta
- Riesgos de mal uso

### 2.3 Integración teoría-práctica
El aprendizaje ocurre mediante la conexión directa entre:
- Conceptos teóricos
- Ejecución en Jamovi
- Interpretación de resultados

---

## 3. Estructura del sistema

El sistema se organiza en tres capas:

### 3.1 Library (Biblioteca de contenidos)
La Library contiene todos los recursos educativos del sistema.

Incluye:
- Conceptos estadísticos fundamentales
- Explicaciones de supuestos
- Guías de interpretación
- Ejemplos aplicados
- Errores comunes

Cada entrada en la Library debe incluir:
- Título
- Descripción clara
- Nivel de dificultad
- Relación con otras entradas
- Referencias bibliográficas asociadas

---

### 3.2 Módulos de análisis (Jamovi)
Cada módulo incluye:

- Objetivo del análisis
- Condiciones de uso
- Supuestos estadísticos
- Procedimiento en Jamovi
- Interpretación de resultados
- Errores frecuentes

Ejemplos de módulos ya implementados (ver README.md para el listado
completo y siempre actualizado):
- Comparación de grupos independientes y relacionados
- ANOVA/ANCOVA
- Regresión lineal, logística, logística ordinal y logística multinomial
- Path Analysis y validación estructural
- Series de tiempo

---

### 3.3 Bibliografía unificada
La bibliografía se centraliza como sistema transversal.

Características:
- Asociada a cada entrada de la Library
- Referencias académicas verificables
- Fuentes primarias y secundarias
- Enlaces a manuales, papers y libros

Función principal:
Permitir al estudiante profundizar directamente en las fuentes originales.

### 3.4 Por qué Library sigue siendo análisis, y qué pasó con Bibliography
La revisión oficial de jamovi (16 de septiembre de 2026, con seguimiento
en octubre) pidió mover la atribución de método por análisis al
mecanismo nativo `00refs.yaml`/`refs:` de jamovi, enganchado directamente
a la tabla/gráfico que usó cada fuente — igual que cualquier otro módulo
de jamovi, y la única forma de que esa cita llegue al informe exportado
del estudiante junto al resultado que respalda. Se aceptó este cambio:
`Bibliography` dejó de ser un análisis del menú.

Eso no significa que se haya perdido el trabajo de investigación
invertido en ella. Sus tres roles se repartieron:

- **Atribución de método por análisis** → `00refs.yaml`/`refs:`, nativo
  de jamovi, aparece junto a cada resultado.
- **Lista de lectura curada y filtrable por tema**, **perfil
  bibliométrico** (conteo de citas, indexación Scopus/Web of
  Science/cuartil) y la **lista completa en APA 7.ª edición** → viven en
  `docs/Bibliography.md`, fuera de la interfaz de jamovi. Se intentó
  enganchar la lista de lectura dentro de `Assumption Library` vía
  `refs:`, pero el renderizador de jamovi solo muestra esa lista de
  referencias para resultados tipo tabla, nunca para los resultados
  `Html` de los que está hecha Library — así que Library solo tiene un
  puntero de texto a `docs/Bibliography.md`, igual que cualquier otro
  texto del módulo.

Una consecuencia importante: el renderizador nativo de referencias de
jamovi usa su propio formato numerado por orden de aparición (`[1]`,
`[2]`...), no APA — una restricción de la interfaz de jamovi, no una
elección de AssumptionsLab. La cita en APA 7.ª edición, fija y completa,
sigue viviendo en `docs/Bibliography.md`. Ver ARCHITECTURE.md §11 y
CODE_STYLE.md §21 para el detalle técnico completo.

---

## 4. Sistema de Bibliografía

La bibliografía no es un apéndice final, sino un sistema activo.

### 4.1 Estructura
Cada referencia incluye:

- Autor(es)
- Año
- Título
- Fuente
- Tipo (libro, artículo, manual, recurso web)

### 4.2 Integración con Library
Cada concepto de la Library debe incluir:
- Referencias asociadas
- Lecturas recomendadas
- Nivel de profundidad sugerido

### 4.3 Objetivo pedagógico
- Evitar aprendizaje aislado
- Conectar teoría con evidencia científica
- Fomentar pensamiento crítico

---

## 5. Flujo pedagógico del estudiante

El aprendizaje sigue este ciclo:

1. Introducción conceptual (Library)
2. Comprensión de supuestos
3. Ejecución en Jamovi
4. Observación de resultados
5. Interpretación guiada
6. Consulta bibliográfica
7. Reflexión y aplicación

---

## 6. Principios de diseño del contenido

- Claridad conceptual por encima de complejidad técnica
- Progresión gradual de dificultad
- Evitar sobrecarga de información
- Reforzar conexión entre conceptos
- Promover autonomía del estudiante

---

## 7. Versión del sistema

- Versión del documento: 1.3
- Versión de software correspondiente: AssumptionsLab 1.6.0
- Estado: expansión de módulos completada (diez módulos de análisis) más
  la respuesta a la revisión oficial de jamovi (16 de septiembre de
  2026, con seguimiento en octubre): manejo de errores Categoría A/B en
  los 9 módulos de diagnóstico, migración de gráficos al mecanismo
  nativo de tema/paleta de jamovi, catálogo de traducción i18n nativo de
  jamovi (inglés + español), consolidación de ayudantes compartidos, y
  retiro del análisis independiente `Bibliography` del menú (enganchado
  `00refs.yaml`/`refs:` en los 9 módulos de análisis; la bibliografía
  completa en APA 7.ª edición y el perfil bibliométrico ahora viven en
  `docs/Bibliography.md`)
- Alcance: estructura base + integración bibliográfica + Library
  funcional + suite completa de módulos de análisis + alineación con los
  mecanismos nativos de jamovi (tema de gráficos, i18n)
- Próximo paso: ver la Sección 14 "Future Expansion" de
  ARCHITECTURE.md (métodos bayesianos, SEM/PLS-SEM, modelos
  multinivel, análisis de supervivencia, meta-análisis, diagnósticos
  de aprendizaje automático)

---

## 8. Nota final

Este documento actúa como base estructural del sistema AssumptionsLab y debe actualizarse de forma incremental conforme se incorporen nuevos módulos, mejoras pedagógicas y expansión de la Library.

Última revisión de contenido: 2026-10-08, con motivo del retiro del
análisis `Bibliography` del menú y la consolidación de la bibliografía
en `docs/Bibliography.md`, descritos en la Sección 3.4 y la Sección 7.
