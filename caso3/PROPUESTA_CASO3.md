# Propuesta · Caso 3 · Respuestas continuas positivas

> Estructura **base** (borrador). Espejo de los Casos 1 y 2. A refinar contigo antes de desarrollar
> cada unidad.

## Pregunta que vertebra el caso

Caso 1: *«¿ocurre el evento? ¿y cuándo?»*. Caso 2: *«¿cuántas veces y a qué ritmo?»*.
**Caso 3: *«¿cuánto?»*** — la **magnitud** positiva y continua: el **coste** de un siniestro (severidad),
el gasto sanitario, una duración, una concentración. Es la contraparte natural de la **frecuencia** del
Caso 2: frecuencia × severidad = **coste esperado** (prima pura).

## Familia de modelos

Respuestas $Y>0$ continuas y **asimétricas a la derecha**: el modelo lineal gaussiano no vale. La
familia de referencia es el **GLM Gamma** (varianza $\propto\mu^2$, coeficiente de variación constante;
enlace **log** para efectos multiplicativos, o el **inverso** canónico), con alternativas (inversa
gaussiana, lognormal) y el puente con el cero (**Tweedie**, para el coste total con masa en 0).


# Presentación del caso 

## Una visión conjunta del caso

El banco contiene varios mecanismos que se irán revelando progresivamente.

| Componente            | Característica incorporada                                   |
| --------------------- | ------------------------------------------------------------ |
| Coste de intervención | Respuesta Gamma, positiva y asimétrica                       |
| Degradación           | Respuesta Gamma con variabilidad creciente                   |
| Mediciones repetidas  | Correlación dentro de máquina                                |
| Máquinas              | Diferencias en nivel inicial y pendiente                     |
| Plantas               | Heterogeneidad contextual no observada                       |
| Plan preventivo       | Asignación no aleatoria                                      |
| Sensores              | Covariables dependientes del tiempo                          |
| Mantenimiento         | Efecto protector que se debilita                             |
| Tiempo hasta fallo    | Censura administrativa                                       |
| Riesgo basal          | Aumento no lineal con el tiempo                              |
| Datos ausentes        | Probabilidad de ausencia relacionada con el estado observado |

A lo largo del bloque no utilizaremos siempre todas las variables ni todas las tablas. Cada unidad seleccionará la respuesta, la estructura de datos y el nivel de complejidad apropiados.

El objetivo no será únicamente obtener el modelo con mejor ajuste, sino reconstruir con rigor el proceso de modelización:

1. definir la pregunta;
2. identificar la unidad de análisis;
3. reconocer la distribución y la dependencia;
4. formular el predictor;
5. estimar e interpretar los efectos;
6. evaluar la especificación;
7. comparar modelos;
8. obtener predicciones relevantes para la decisión industrial.


## Mapa de unidades (propuesto)

| Unidad | Contenido | Objetivo (espiral) |
|---|---|---|
| **3.1** | **GLM Gamma**: la distribución de referencia para positivas. Contexto · modelización y estimación (enlaces log/inverso, $\phi$) · interpretación · inferencia y selección · bondad de ajuste y diagnóstico. | Continuas positivas |
| **3.2** | **Más allá de la Gamma**: inversa gaussiana, lognormal (log-OLS), y **modelar la dispersión** (GLM dobles / heterocedasticidad). Comparación de alternativas. | Continuas positivas |
| **3.3** | **Ceros y positivos juntos: Tweedie**. Modelo compuesto Poisson–Gamma; masa en 0; **prima pura = frecuencia × severidad** (nexo con el Caso 2). | Continuas positivas |
| **3.4** | **Continuas positivas agrupadas: GLMM Gamma** (interceptos aleatorios, anidamiento). | Mixtos (espiral) |
| **3.5** | **Del importe al tiempo: modelos de vida acelerada (AFT)**. Los tiempos son positivos continuos; nexo con la supervivencia del Caso 1. | Supervivencia (espiral) |
| **3.6** | **Selección, validación cruzada y regularización**. | Continuas positivas |
| **3.7** | **Estudio de caso, exposición y evaluación**. | Comunicar y reproducir |

## Correspondencia con la estructura de los casos anteriores

- Unidades **de un solo modelo** (3.1 Gamma; 3.4 mixtos; 3.5 AFT) → recorrido completo de 2.1
  (contexto → modelización/estimación → interpretación → inferencia/selección → bondad de ajuste).
- Unidades **de familia de alternativas** (3.2 alternativas a la Gamma; 3.3 Tweedie vs Gamma/lognormal)
  → problema/identificación → bloque breve por modelo → **comparación** al final.
- Cierre siempre con **comparación de alternativas** y **validación contra el DGP**.
