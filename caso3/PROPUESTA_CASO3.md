# Propuesta · Caso 3 · Magnitudes positivas y vida útil hasta el fallo

> Mapa **refinado** tras contrastar con la guía docente (cronograma, contenidos del Caso 3 y objetivos
> O4–O6) y con lo ya cubierto en los Casos 1 y 2. Espejo estructural de esos dos casos.

## Pregunta que vertebra el caso

Caso 1: *«¿ocurre el evento? ¿y cuándo?»*. Caso 2: *«¿cuántas veces y a qué ritmo?»*.
**Caso 3: *«¿cuánto y hasta cuándo?»*** — la **magnitud** positiva y continua (el **coste** de una
intervención, la **degradación** acumulada) y la **vida útil** (el **tiempo** hasta el primer fallo).

## Familia de modelos

Respuestas $Y>0$ continuas y **asimétricas a la derecha**: el modelo lineal gaussiano no vale. La
familia de referencia es el **GLM Gamma** (varianza $\propto\mu^2$, coeficiente de variación constante;
enlace **log** para efectos multiplicativos, o el **inverso** canónico), con la **inversa gaussiana** y
la **lognormal / log-OLS** como alternativas. Como los **tiempos** son también positivos continuos, el
mismo marco conduce a los modelos **AFT**, y de ahí a **Cox** como frontera semiparamétrica.

## Anclaje en la guía docente

- **Cronograma.** Semanas **10–13** de contenido y semana **14** para estudio de caso, exposiciones y
  cierre. Foco literal del cronograma: *«Continuas positivas, AFT, Cox, síntesis»*.
- **Contenidos del Caso 3.** GLM para continuas positivas (Gamma, inversa gaussiana); elección de
  enlace; comparación con log-OLS. Selección de variables (la regularización con `glmnet` se introduce
  en el Caso 2 sobre Poisson; **en Gamma `glmnet` no aplica y se aborda como límite conceptual**).
  Supervivencia: **AFT** paramétricos (exponencial, Weibull, log-normal) con **censura en la
  verosimilitud**; **Kaplan–Meier descriptivo**; **Cox de riesgos proporcionales como frontera**
  (semiparamétrico, no GLM) **y su diagnóstico**. **Síntesis: las tres lentes** sobre un mismo problema.
- **Objetivos.** O4 (magnitudes positivas y vida útil: Gamma, lognormal, AFT), O5 (mixtos y
  supervivencia en espiral, con Cox como frontera), O6 (comunicar y reproducir).
- **Informe del Caso 3:** **artículo de investigación** (§7.1 de la guía).

## Mapa de unidades (refinado)

| Unidad | Sem. | Contenido | Objetivo (espiral) |
|---|---|---|---|
| **3.1** | 10 | **GLM Gamma**: contexto (dónde falla el gaussiano) · modelización y estimación (enlaces log/inverso, $\phi$) · interpretación multiplicativa · inferencia y selección · bondad de ajuste, diagnóstico y predicción. | Continuas positivas |
| **3.2** | 11 | **Elegir familia y escala**: inversa gaussiana y lognormal (**log-OLS**) frente a Gamma; relación media-varianza; **modelar la dispersión**; comparación y criterio de elección. | Continuas positivas |
| **3.3** | 11–12 | **Continuas positivas agrupadas: GLMM Gamma**. Interceptos y **pendientes aleatorias** por máquina, anidamiento en plantas; componentes de varianza; predicción condicional vs marginal. | Mixtos (espiral) |
| **3.4** | 12 | **Del importe al tiempo: AFT**. Kaplan–Meier descriptivo; los tiempos como positivos continuos; exponencial, Weibull, log-normal; **censura en la verosimilitud**; factor de aceleración. | Continuas positivas + supervivencia |
| **3.5** | 13 | **Cox como frontera**: riesgos proporcionales, verosimilitud parcial, **por qué no es un GLM**; **diagnóstico de la proporcionalidad** (residuos de Schoenfeld); efectos no proporcionales. | Supervivencia (espiral) |
| **3.6** | 13 | **Síntesis: las tres lentes y el presupuesto**. El mismo fallo por `cloglog` persona-periodo (Caso 1), Poisson a trozos (Caso 2) y Cox extendido (3.5), sobre `seguimiento` y contra el DGP; el mapa $V(\mu)=\phi\mu^p$ que completa la línea GLM y la **Tweedie** (`panel`: coste anual con ceros) como síntesis frecuencia × severidad; validación agrupada por máquina; regularización como límite conceptual; fronteras. | Síntesis |
| **3.7** | 14 | **Estudio de caso, exposición y evaluación** (artículo de investigación). | Comunicar y reproducir |

## Decisiones de diseño (y por qué)

- **AFT y Cox recuperan su peso.** La guía los sitúa como contenido nuclear del caso (O4 nombra AFT;
  O5 sitúa Cox como frontera). Cada uno recibe unidad propia, y Cox incluye **diagnóstico de PH**,
  que es lo que justifica a Therneau & Grambsch (2000) en la bibliografía del caso.
- **Sin reenseñar tiempo discreto ni Poisson a trozos.** Son las unidades **1.5** y **2.5**; el
  cronograma asigna «riesgos a trozos» al Caso 2. Aquí se **reactivan** dentro de la síntesis (3.6),
  no se reexplican: eso libera ~1 semana.
- **Tweedie dentro de 3.6, como síntesis aplicada.** [REVISADO] La objeción que la dejaba fuera —el
  DGP de degradación no tenía masa en cero— desaparece con el banco de **averías**: la tabla `panel`
  se diseñó a propósito con ~52 % de máquina-años a coste cero (`DISENO_BANCO_AVERIAS.md`, decisión
  «Tweedie como cierre»). Argumento didáctico: es la única pieza que **sintetiza materialmente** las
  dos mitades del curso (Poisson–Gamma compuesta = frecuencia del Caso 2 × severidad del Caso 3),
  completa el mapa de varianzas potencia y, a diferencia de Cox, **sigue dentro del marco GLM**.
  Entra como una sección aplicada (no unidad), sin teoría de la densidad (Dunn–Smyth solo en callout
  plegado; $p$ lo estima `glmmTMB`). No figura en contenidos ni librerías del Caso 3 en la guía: se
  propone añadir una línea a los contenidos («coste agregado con ceros: familia Tweedie como síntesis
  frecuencia × severidad») y `glmmTMB` a las librerías del caso.
- **Regularización, como dice la guía.** No es unidad: se trata en 3.6 como **límite conceptual**,
  porque `glmnet` no implementa la familia Gamma. La regularización operativa ya se hizo en el Caso 2.
- **Extensiones avanzadas, en un cierre breve.** PAMM, Royston–Parmar, Aalen, riesgos competitivos y
  **Andersen–Gill** (eventos recurrentes: nuestro banco *es* recurrente y 3.4–3.5 lo trocean en gaps) se
  citan al final de 3.6 como «una mirada más allá del GLM» (así lo formula la guía), sin desarrollo.
- **Mixtos con unidad propia (3.3).** La guía describe el hilo como introducido en el Caso 1 y
  reactivado en el Caso 2, y no lista `lme4`/`glmmTMB` entre las librerías del Caso 3; pero el DGP ya
  incorpora `b0_maquina`, `b1_maquina` y `u_planta`, y el criterio del curso es mantener los dos hilos
  en espiral en los tres casos. Se mantiene como unidad, apoyada en la estructura longitudinal del
  banco (medidas repetidas por máquina, máquinas anidadas en plantas).

## Cómo trabajamos cada unidad

- Unidades **de un solo modelo** (3.1 Gamma; 3.3 GLMM Gamma; 3.4 AFT; 3.5 Cox) → recorrido completo:
  contexto → modelización/estimación → interpretación → inferencia/selección → bondad de ajuste y
  diagnóstico.
- Unidades **de familia de alternativas** (3.2) → problema/identificación → bloque breve por modelo →
  **comparación** al final.
- Cierre siempre con **comparación de alternativas** y **validación contra el DGP** (la «verdad» del
  banco está en `attr(banco, "verdad")`).
- En cada unidad se resuelve el caso guía y se dejan **cuestiones sin resolver**, evaluables, para el
  estudiantado.

## Estructura detallada de la Unidad 3.6 [ACTUALIZADO]

**«¿Cuánto costará el mantenimiento el año que viene?» — Síntesis: las tres lentes y el presupuesto.**
Datos: `seguimiento` y `panel`. Semana 13 (compartida con 3.5): unidad **breve**; un solo modelo nuevo
(Tweedie), que es precisamente la síntesis. Reactivar, no reexplicar.

- **6.1 El mismo fallo, tres lentes.** Sobre `seguimiento` (start-stop mensual), idéntico predictor:
  `cloglog` persona-periodo (reactiva 1.5), Poisson a trozos con offset (reactiva 2.5) y el Cox
  extendido que 3.5 deja ajustado. Tabla de coeficientes lado a lado + verdad del DGP (`beta_haz`).
  Equivalencias: Prentice & Gloeckler (1978) para cloglog agrupado; Holford (1980) y Laird & Olivier
  (1981) para Poisson a trozos = exponencial a trozos [VERIFICAR entradas en `references.bib`; el
  borrador viejo citaba `@aitkin1980` para cloglog — revisar]. Callout «el AFT es la lente distinta»,
  con matiz nuevo: en este banco la Weibull del gap tiene forma ≈ 1 (exponencial), el único caso en
  que AFT y PH coinciden exactamente — el caso puente es real. Nota práctica: 48 meses como `factor()`
  es inviable; agrupar el tiempo base (semestres/años) y justificar por qué no afecta a la comparación.
  Cierra con subsección de validación contra el DGP.
- **6.2 Completar el mapa: la familia de dispersión exponencial.** Contexto: `coste_total` anual con
  ~52 % de ceros — ninguna familia vista sirve (Gamma no admite el 0; Poisson no es continua). El
  continuo $V(\mu)=\phi\mu^p$ reorganiza el curso: gaussiana ($p=0$), Poisson ($p=1$), Gamma ($p=2$),
  inversa gaussiana ($p=3$); el hueco $1<p<2$ es la Tweedie Poisson–Gamma compuesta (Tweedie, 1984;
  Jørgensen, 1987, 1997) [VERIFICAR entradas bib]. Aquí vive «completar los modelos en la línea GLM».
- **6.3 El presupuesto: Tweedie = frecuencia × severidad.**
  `glmmTMB(coste_total ~ … + offset(log(exposicion)), family = tweedie)` sobre `panel`; lectura de
  $\hat p \in (1,2)$ y efectos multiplicativos sobre el coste anual esperado. Coherencia interna:
  $E(\text{coste}) = E(N)\cdot E(Y)$ — la prima pura del Caso 2 en clave industrial (Smyth &
  Jørgensen, 2002; Dunn & Smyth, 2005) [VERIFICAR]. Callout breve contrastando con el modelo en dos
  partes (hurdle, reactiva el exceso de ceros del Caso 2), sin desarrollarlo. Predicción del
  presupuesto por máquina y del parque. Validación contra el DGP (el coste anual esperado es
  calculable desde `verdad`); la fragilidad hace que el panel **no** sea Tweedie exacto marginalmente:
  contarlo como hallazgo, no ocultarlo.
- **6.4 Selección y validación con datos agrupados; regularización como límite.** `group_vfold_cv`
  por **`id_maquina`** (en este banco no hay plantas) sobre el modelo de presupuesto; recordatorio del
  índice C anunciado en 3.5. Regularización como límite conceptual: `glmnet` no implementa Gamma ni
  Tweedie; salidas (a) gaussiana sobre `log` —trampa de retransformación de 3.2— o (b) familia `cox`.
- **6.5 Más allá del GLM: el mapa y las fronteras.** Tabla-mapa «¿qué es —y qué no— un GLM?» (del
  borrador, con la fila Tweedie pasada de "extensión" a contenido de 6.3). Extensiones solo citadas
  (ver decisión más abajo). Mensaje de cierre del curso.
- **Tarea del estudiante (callout final)**, ligada a secciones: (1) tres lentes y por qué coinciden;
  (2) por qué el AFT no entra en la tabla y en qué caso sí — que descubran que su banco es ese caso;
  (3) ajustar la Tweedie, interpretar $\hat p$ y contrastar el presupuesto predicho con frecuencia ×
  severidad calculadas por separado; (4) CV por máquina vs por filas; (5) justificar el límite de
  `glmnet` y ejecutar una alternativa; (6) elegir una extensión de 6.5 y argumentar su uso.

> Nota de reconstrucción: el borrador actual de `_unidad_3_6.qmd` está escrito contra el banco viejo
> (`modelo_intervalos`, `temperatura`, `vibracion`, `id_planta`) y debe reescribirse íntegro sobre
> `seguimiento` y `panel`.

## Datos

[ACTUALIZADO] Banco de **averías** (`R/dgp_averias.R`), que sustituye al banco de «degradación»: fábrica
de mobiliario, **400 máquinas** en 5 procesos, ventana de **4 años**, ~1.200 averías; censura de gaps
~25 %; panel Tweedie con ~52 % de máquina-años a cero (calibración verificada, semilla 20252026).
Cinco tablas de análisis: `maquinas`, `averias` (3.1–3.3), `intervalos` (3.4–3.5), `seguimiento`
(start-stop mensual; 3.5–3.6) y `panel` (máquina-año; 3.6). Diseño completo, mecanismos generadores y
decisiones cerradas en `DISENO_BANCO_AVERIAS.md`. Caché en `caso3/datos`.

### Datos reales para el estudio de caso (Unidad 3.7) — **pendiente de preparar**

Para el estudio de caso se contempla usar un banco **real** como contrapunto al simulado, replicando
lo que el Caso 1 hace con GLOW:

**Backblaze Drive Stats.** Fiabilidad de discos duros en centros de datos, publicado trimestralmente
desde 2013. Encaja conceptualmente con el caso: tiempo-a-fallo real con **censura** (discos retirados
antes de fallar), covariables S.M.A.R.T. que **cambian en el tiempo**, y agrupación natural por
modelo/fabricante —análoga a nuestras plantas—. Instantánea Q1 2026: 341.263 discos, 1.030 fallos,
AFR 1,24 %.

- Fuente y descarga: <https://www.backblaze.com/cloud-storage/resources/hard-drive-test-data>
- **Licencia** (verificada en la web de Backblaze): uso libre **citando a Backblaze como fuente**; el
  usuario es responsable del uso; se pueden vender obras derivadas; **no** se puede vender el dato en
  sí, que es gratuito.

Dos obstáculos prácticos a resolver antes de adoptarlo:

1. **Tamaño.** Cada trimestre son ~1 GB comprimido y ~12 GB en disco (un CSV por día, una fila por
   disco y día). Hay que **preprocesarlo a una tabla disco-nivel** (`id`, `modelo`, `tiempo`, `fallo`
   y covariables basales) y distribuir ese extracto, no el crudo.
2. **Censura extrema.** Con AFR ~1,4 %, más del 98 % de los discos quedan censurados, frente al ~40 %
   del banco simulado. Bien planteado esto es didáctico —muestra un régimen de censura muy distinto—,
   pero obliga a ajustar las expectativas sobre la potencia de los contrastes.

> Nota: **no añadir aún la entrada a `references.bib`**. Los `.qmd` del curso usan `nocite: @*`, así
> que cualquier entrada nueva aparecería en la bibliografía de los tres casos. Se añadirá al
> desarrollar la Unidad 3.7.
