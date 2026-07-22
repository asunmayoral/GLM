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
| **3.6** | 13 | **Síntesis: las tres lentes**. El mismo fallo visto como tiempo discreto/`cloglog` (Caso 1), Poisson a trozos (Caso 2) y AFT/Cox (Caso 3): qué estima cada una y cuándo coinciden. Selección y validación; **regularización como límite conceptual** en Gamma; mirada breve a extensiones. | Síntesis |
| **3.7** | 14 | **Estudio de caso, exposición y evaluación** (artículo de investigación). | Comunicar y reproducir |

## Decisiones de diseño (y por qué)

- **AFT y Cox recuperan su peso.** La guía los sitúa como contenido nuclear del caso (O4 nombra AFT;
  O5 sitúa Cox como frontera). Cada uno recibe unidad propia, y Cox incluye **diagnóstico de PH**,
  que es lo que justifica a Therneau & Grambsch (2000) en la bibliografía del caso.
- **Sin reenseñar tiempo discreto ni Poisson a trozos.** Son las unidades **1.5** y **2.5**; el
  cronograma asigna «riesgos a trozos» al Caso 2. Aquí se **reactivan** dentro de la síntesis (3.6),
  no se reexplican: eso libera ~1 semana.
- **Tweedie fuera del temario.** No figura en los contenidos, ni en O4, ni en las librerías del Caso 3.
  Se **menciona** en 3.6 como extensión natural (coste total = frecuencia × severidad, nexo con el
  Caso 2), sin desarrollarlo. Desarrollarlo exigiría además rediseñar el DGP para tener masa en cero.
- **Regularización, como dice la guía.** No es unidad: se trata en 3.6 como **límite conceptual**,
  porque `glmnet` no implementa la familia Gamma. La regularización operativa ya se hizo en el Caso 2.
- **Extensiones avanzadas, en un cierre breve.** PAMM, Royston–Parmar, Aalen y riesgos competitivos se
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

## Datos

Banco de **mantenimiento industrial simulado** (`R/dgp_continuas.R`): 600 máquinas en 12 plantas,
seguimiento de 720 días, censura administrativa (**~40 %**, verificada por simulación). Cuatro tablas
de análisis: `modelo_base` (coste por intervención), `modelo_longitudinal` (degradación repetida),
`modelo_supervivencia` (tiempo al primer fallo) y `modelo_intervalos` (máquina-intervalo, formato
*start-stop*). Caché en `caso3/datos`.

**Los tiempos de fallo son continuos.** La rejilla de 30 días es solo el mecanismo de simulación: el
instante exacto se extrae de una exponencial truncada dentro de cada intervalo. No hay empates, así
que 3.4 y 3.5 trabajan directamente con `Surv(tiempo, fallo)`. `modelo_intervalos` **no** sirve para
discretizar el tiempo, sino para aportar covariables que cambian dentro del seguimiento (Cox con
covariables dependientes del tiempo, 3.5) y para sostener la comparación de lentes de 3.6.

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
