# Diseño del banco de datos de auto · columna vertebral de los estudios de caso (Casos 2 y 3 + proyecto final)

> **Estado: DISEÑO para validar. Reemplaza y amplía `ESPECIFICACION_DGP_AUTO_2_7.md`.**
> Este documento fija, con detalle, todas las variables, su significado y el proceso generador (DGP),
> *antes* de programar. Un único generador `dgp_auto.R` produce un banco de datos rico que sirve a:
> el **Caso 2** (conteos, tasas, sobredispersión, ceros, mixtos, riesgos a trozos, selección), el
> **Caso 3** (Gamma/IG, AFT, Cox como frontera, regularización) y el **proyecto final** que conjuga
> ambos («las tres lentes» sobre el mismo cliente). Sustituye a la cohorte sanitaria en la Unidad 2.7.

## 0 · Decisiones cerradas (contigo)

- Dominio único: **cartera de auto** (UE/España, euros). La 2.7 pasa a ser auto; el diccionario «salud»
  se conserva en el motor como alternativa opcional a coste casi nulo.
- Alcance: **frecuencia + severidad + supervivencia**, para cubrir Casos 2 y 3 con el mismo banco.
- Calibración **anclada a freMTPL** (magnitudes verificadas) y a la literatura actuarial.
- **Densidad continua** (hab/km²) *y* **zona agrupada** (urbana/mixta/rural), para enseñar el paso de
  continua a categórica.
- **Sexo** con señal real pero marcado **legalmente prohibido** (Test-Achats).
- Supervivencia con **tres objetivos**: tiempo al **primer** siniestro, tiempo **entre** siniestros
  (recurrente) y tiempo a la **baja** (churn). Escala de tiempo base: **antigüedad de la póliza**.
- **Baja** simulada bajo **AFT no-PH** (log-logística); **intensidad de siniestros** bajo **Weibull
  (PH)** — para que «AFT vs Cox» se *observe* en los datos.
- **Fragilidad latente por póliza** que genera el historial: unifica sobredispersión (Caso 2.2),
  efectos mixtos (2.4) y frailty (Caso 3).
- Variantes por equipo: por **semilla + un subconjunto de parámetros** (ver §9).

---

## 1 · Objetivos de análisis ↔ resultados de aprendizaje (O1–O6)

Los resultados de aprendizaje de la guía docente:

- **O1** — Reconocer el marco común de los GLM (familia exponencial, predictor lineal, enlace).
- **O2** — Respuestas binarias y politómicas (logística, OR, calibración, selección).
- **O3** — Conteos y tasas (Poisson/NB, offset, IRR, sobredispersión, ceros, CV y regularización).
- **O4** — Magnitudes positivas y vida útil (Gamma/log-normal, AFT, interpretación multiplicativa).
- **O5** — Extender a mixtos y supervivencia (GLMM, tiempo discreto, riesgos a trozos, AFT, Cox frontera).
- **O6** — Comunicar y reproducir (informe, exposición, reproducibilidad).

Cada objetivo de análisis que el banco debe **poder** sostener, con la variable respuesta y dónde se usa:

| # | Objetivo de análisis | Respuesta / estructura | RA | Caso · Unidad |
|---|---|---|---|---|
| A | Tasa de siniestralidad y su predicción; IRR | `n_siniestros` + `offset=log(exposicion)` | O3 | 2 · 2.1 |
| B | Asociación vs independencia entre factores | tabla de contingencia (`tipo`×`zona`×`uso` × `tuvo_siniestro`) | O3 | 2 · 2.1 |
| C | Movilidad del riesgo entre dos años | `bonus_malus_prev` → `bonus_malus_act` (tabla cuadrada) | O3 | 2 · 2.1 |
| D | Sobredispersión (heterogeneidad individual) | `n_siniestros` con **fragilidad** → quasi-Poisson / NB | O3 | 2 · 2.2 |
| E | Exceso de ceros (masa estructural) | `n_fraude` (zero-inflated) | O3 | 2 · 2.3 |
| F | Conteos agrupados; ICC; predicción marginal | `n_gestiones` con `(1|agencia)` (y región) | O5 | 2 · 2.4 |
| G | Riesgos a trozos ≡ Poisson; hazard por tramo | `tiempo_primer_sin`,`evento` → persona-periodo | O5 | 2 · 2.5 |
| H | Selección: AIC/BIC → validación cruzada → lasso | muchos predictores + ruido | O3 | 2 · 2.6 |
| I | **Severidad**: coste por siniestro (asimétrico) | `coste_siniestro` (Gamma **y** IG, enlace log) | O4 | 3 |
| J | **AFT** de la duración (no-PH) | `t_baja`,`baja` (log-logística/log-normal) | O4·O5 | 3 |
| K | **Cox** como frontera; escala de tiempo; PH test | `t_baja` / `t_primer_sin` + covariables | O5 | 3 |
| L | **Eventos recurrentes**; offset variable; covariables t-dependientes | panel `(tstart,tstop]` con `n_partes` | O5 | 3 (callout) |
| M | **Regularización** en Gamma/Tweedie | `coste` / prima con muchos predictores | O3·O4 | 3 |
| N | **Prima pura** = frecuencia × severidad (Tweedie) | `coste_total` por póliza | O3·O4 | 3 / proyecto |
| O | **Lente binaria**: ¿reclama? ¿se da de baja? | `tuvo_siniestro`, `baja` (logística) | O2 | proyecto (reconecta Caso 1) |
| P | *Fairness*: factor prohibido y proxies | `sexo` (señal real, no usable) | O2·O6 | 2.6 / 3 / proyecto |
| Q | Comunicar y reproducir | todo, con `attr("verdad")` y semilla | O6 | 2.7 · 3 · proyecto |

**Proyecto final (síntesis «tres lentes»).** El mismo cliente mirado como: *binaria* (O2: ¿siniestro?
¿baja?), *conteo* (O3: ¿cuántos partes, a qué tasa?), *continua positiva y vida útil* (O4: ¿cuánto
cuesta cada parte? ¿cuánto dura la póliza?), con los hilos de **mixtos** (O5, la fragilidad) y
**supervivencia** (O5, primer evento / recurrente / baja) recorriéndolo. Cierre natural: **política de
primas** (prima pura = frecuencia × severidad, con recargo por incertidumbre y ajuste bonus-malus).

---

## 2 · Filosofía generativa

Un solo motor, un principio: **generar a la resolución más fina y agregar hacia abajo**. Simulamos, por
póliza, un **proceso puntual marcado con fragilidad**: la secuencia de instantes de siniestro
$t_1<t_2<\dots$ y su **coste** (la marca). De ese objeto se derivan, por reducción, todas las
estructuras: primer evento, recurrentes, conteos anuales, tramos discretos, coste por parte y coste
total. La **fragilidad** $Z_i$ (riesgo latente por póliza) es la pieza que unifica el curso: hace el
conteo **sobredisperso** (2.2), es el **efecto aleatorio** de los mixtos (2.4/O5) y la **frailty** de
supervivencia (Caso 3). Todo parámetro es argumento del simulador y queda en `attr(., "verdad")`.

---

## 3 · Estructura relacional y esquema de síntesis

Tres niveles anidados (región ⊃ agencia ⊃ póliza) y dos tablas de salida (póliza y panel), más una
tabla larga de siniestros:

```
 ┌──────────────┐        ┌──────────────┐         ┌─────────────────────────────────────────────┐
 │  REGIONES    │  1:N   │   AGENCIAS   │   1:N    │                 PÓLIZAS                     │
 │  id_region   │───────▶│  id_agencia  │────────▶│  id_poliza (PK)                             │
 │  u_region ~  │        │  id_region FK│         │  id_agencia FK                              │
 │   N(0,σ_reg²)│        │  canal       │         │  ── covariables fijas (rating) ──           │
 └──────────────┘        │  u_agencia ~ │         │  edad, carnet, potencia, valor, densidad,   │
                         │   N(0,σ_a²)  │         │  zona, uso, tipo, combustible, km, sexo*,   │
                         └──────────────┘         │  + ruido                                    │
                                                  │  ── latente ──  Z_i (fragilidad, Gamma)     │
                                                  │  ── tiempo ──   antiguedad, exposicion,     │
                                                  │                 t_baja, baja, motivo_fin    │
                                                  │  ── conteos ──  n_siniestros, n_asistencia, │
                                                  │                 n_fraude, n_gestiones       │
                                                  │  ── supervivencia ── t_primer_sin, evento   │
                                                  │  ── severidad ── coste_total, coste_medio   │
                                                  │  ── bonus-malus ── bm_prev, bm_act          │
                                                  └───────────────┬─────────────────────────────┘
                                                                  │ 1:N (una fila por parte)
                                                  ┌───────────────▼─────────────────────────────┐
                                                  │             SINIESTROS                       │
                                                  │  id_poliza FK · k · t_k (continuo) ·         │
                                                  │  coste_k (Gamma) · anio                       │
                                                  └───────────────┬─────────────────────────────┘
                                                                  │ agregación por año
                                                  ┌───────────────▼─────────────────────────────┐
                                                  │      PANEL PÓLIZA-AÑO (counting process)     │
                                                  │  id_poliza · anio · tstart · tstop ·         │
                                                  │  expo_anual · n_partes_anual · bm_anual ·    │
                                                  │  edad_t · antig_veh_t · en_riesgo            │
                                                  │  offset = log(tstop − tstart)  (VARIABLE)    │
                                                  └──────────────────────────────────────────────┘
    (*) sexo: señal real pero prohibido como factor de tarificación (Test-Achats).
```

**Tamaños por defecto:** ~6 regiones, ~30 agencias (tamaño desigual), N≈3.000 pólizas; horizonte de
observación `T_max` (p. ej. 10 años).

---

## 4 · Diccionario de variables (completo)

### 4.1 · Estructura y exposición

| Variable | Tipo | Significado | Papel / DGP |
|---|---|---|---|
| `id_poliza` | id | Identificador de la póliza | clave |
| `id_agencia` | factor | Agencia que gestiona la póliza | agrupación (2.4); efecto `u_agencia` |
| `id_region` | factor | Región (agrupa agencias) | nivel superior del anidamiento |
| `antiguedad` | num (años) | Tiempo que el cliente lleva en la cartera (tenure) | escala de tiempo de supervivencia |
| `exposicion` | num (0.30–1.00) | Antigüedad **relativa** al cliente más veterano (`antiguedad/T_max`) | **offset** `log(exposicion)` |

### 4.2 · Factores de riesgo ACTIVOS (frecuencia)

| Variable | Tipo | Significado | Efecto (IRR) · fuente |
|---|---|---|---|
| `edad_conductor` | num → **tramos** | Edad del conductor principal | U: 18–25 alto · 25–35 ~0.60 · 35–65 ~0.45 · >65 ~0.55 (freMTPL) |
| `antiguedad_carnet` | num | Años de carnet (experiencia) | ~0.95 por +1 SD (−) |
| `potencia_cv` | num | Potencia del vehículo (CV) | ~1.13 por +1 SD (+); interacción ↑ con malus |
| `cilindrada` | num | Cilindrada (cc) | correlada con potencia (colinealidad didáctica) |
| `antiguedad_vehiculo` | num | Antigüedad del vehículo (años) | leve hump 5–15 años (~1.10) |
| `combustible` | factor | Gasolina / diésel | diésel ligeramente + (más km); freMTPL VehGas |
| `densidad` | num (hab/km²) | Densidad de población del domicilio | **+ monótono** (urbano ~2× rural); freMTPL |
| `zona_circulacion` | factor 3 | Agrupación de `densidad`: urbana/mixta/rural | versión categórica de `densidad` |
| `km_anuales` | num | Kilómetros declarados al año | **+ cóncavo** (~1.25 al doblar; elast. ~0.35) |
| `uso` | factor 2 | Particular / comercial | comercial ~1.25 |
| `tipo_vehiculo` | factor 3 | Turismo / moto / furgoneta | moto ~1.20, furgoneta ~1.10 |

### 4.3 · Factores de SEVERIDAD (coste por siniestro) — Caso 3

| Variable | Efecto sobre el coste medio (IRR) | Nota |
|---|---|---|
| `valor_vehiculo` | ~1.30 por +1 SD | **fuerte** en severidad, **débil** en frecuencia (contraste) |
| `potencia_cv` | ~1.10 por +1 SD | |
| `edad_conductor` | joven <25 ~1.25 · >65 ~1.15 | |
| `tipo_vehiculo` | moto ~1.35 | daños personales más graves |
| `densidad` / `zona` | urbana ~1.05 | densidad pesa mucho en frecuencia, poco en severidad |
| `sexo` | hombre ~1.10 | señal real, **prohibido** |

Coste $\sim \text{Gamma}(\text{media}=\exp(x'\beta_{sev}),\ \text{forma}=\nu)$; alternativa **Inverse-Gaussian**
para comparar. $\nu\approx 1.5$–$2$ (CV≈0.7–0.8, cola larga). `valor_vehiculo` pasa de ruido (teoría) a
predictor activo de severidad.

### 4.4 · Factor PROHIBIDO

| Variable | Tipo | Significado | Papel |
|---|---|---|---|
| `sexo` | factor | Sexo del conductor principal | señal real (frecuencia ~1.10, severidad ~1.10) **pero no usable** como factor de tarificación (Test-Achats). Para trabajar *fairness*, proxies y selección responsable. |

### 4.5 · Predictores de RUIDO (para selección / lasso, 2.6)

`color_vehiculo`, `estado_civil`, `forma_pago` (anual/mensual), `financiado` (0/1), `n_conductores`,
`nivel_estudios`, `medio_contacto` (presencial/telefónico/telemático), `dia_semana_alta`. Relación
**nula** por diseño.

### 4.6 · Respuestas de CONTEO (una por fenómeno del Caso 2)

| Variable | Fenómeno | Base λ/año | Proceso | Unidad |
|---|---|---|---|---|
| `n_siniestros` | **RC / sobredispersión** | ~0.15 | proceso puntual con **fragilidad** `Z_i` → NB marginal | 2.1 · 2.2 |
| `n_asistencia` | **Limpia** | ~0.45 | Poisson pura (sin fragilidad) | 2.1 |
| `n_fraude` | **Exceso de ceros** | ~0.10 | zero-inflated (logit + Poisson) | 2.3 |
| `n_gestiones` | **Agrupación** | ~0.30 | Poisson(exp(η + u_agencia + u_region)) | 2.4 |

`n_siniestros` es el que alimenta severidad y supervivencia; su fragilidad es la sobredispersión de 2.2.

### 4.7 · SINIESTROS (tabla larga) y SEVERIDAD

| Variable | Tipo | Significado |
|---|---|---|
| `id_poliza` | id | póliza a la que pertenece el parte |
| `k` | int | orden del parte (1, 2, …) |
| `t_k` | num (años) | instante del parte desde el alta (tiempo **continuo**) |
| `coste_k` | num (€) | coste del parte (Gamma/IG) |
| `anio` | int | año de póliza en que ocurre (para el panel) |
| `coste_total` | num (€) | suma de costes de la póliza (nivel póliza) → prima pura |
| `coste_medio` | num (€) | coste medio por parte (si hubo) |

### 4.8 · SUPERVIVENCIA

| Variable | Tipo | Significado |
|---|---|---|
| `t_primer_sin` | num (años) | tiempo continuo al **primer** siniestro (o censura) |
| `evento` | 0/1 | 1 = primer siniestro observado; 0 = censura |
| `t_baja` | num (años) | tiempo hasta la **baja** de la póliza (churn) |
| `baja` | 0/1 | 1 = baja observada; 0 = sigue activa al fin del estudio |
| `motivo_fin` | factor | activa / baja / fin de estudio (censura administrativa) |

Nota: `antiguedad = min(t_baja, T_admin)`; la baja determina cuánto observamos → coherencia
exposición-censura. Los tiempos **entre** siniestros (`t_k − t_{k-1}`) dan el análisis recurrente.

### 4.9 · BONUS-MALUS (tabla cuadrada, endógeno)

| Variable | Tipo | Significado |
|---|---|---|
| `bonus_malus_prev` | factor ord. 1–5 (o escala 50–230) | clase el año anterior |
| `bonus_malus_act` | factor ord. 1–5 | clase el año actual |

Deriva del **historial**: cada año con parte → sube (malus), sin parte → baja (bonus), con persistencia.
Sirve para simetría / homogeneidad marginal / movilidad (2.1) y como predictor con la salvedad de su
**endogeneidad**.

### 4.10 · PANEL PÓLIZA-AÑO (counting process)

| Variable | Tipo | Significado |
|---|---|---|
| `id_poliza` | id | póliza |
| `anio` | int | año de póliza (1, 2, …) |
| `tstart`,`tstop` | num | límites del intervalo en la escala de antigüedad |
| `expo_anual` | num | años expuestos en el intervalo (puede ser < 1 el primero/último) |
| `n_partes_anual` | int | partes en el intervalo |
| `bm_anual` | int | clase bonus-malus vigente en el intervalo |
| `edad_t`, `antig_veh_t` | num | covariables **dependientes del tiempo** |
| `en_riesgo` | 0/1 | si la póliza está activa en el intervalo |

`offset = log(expo_anual)` **variable** → exponencial a trozos con offset variable y covariables
t-dependientes (extensión del 2.5, para callouts del Caso 3).

---

## 5 · El proceso generador, paso a paso

1. **Estructura y efectos aleatorios.** Regiones y agencias anidadas; `u_region ~ N(0,σ_reg²)`,
   `u_agencia ~ N(0,σ_a²)`. Por póliza, **fragilidad** `Z_i ~ Gamma(media 1, var 1/θ)` (→ NB marginal).
2. **Covariables.** Se muestrean los factores de §4.2–4.5 con distribuciones realistas por dominio.
   `densidad` continua (log-normal); `zona_circulacion` = corte de `densidad`. `sexo` con su efecto.
3. **Baja / churn (AFT no-PH).** `log(T_baja) = x'β_lapse + σ_l · ε`, con `ε` **logística** (log-logística)
   → **no** proporcional-riesgos, para contrastar con Cox. `antiguedad = min(T_baja, T_admin)`.
4. **Siniestros (Weibull PH + fragilidad, marcado).** Intensidad
   `λ_i(t) = h0(t)·exp(x'β_claim)·Z_i` con `h0(t)` **Weibull** (forma `k_w`). Se generan los instantes
   `t_k` en `[0, antiguedad_i]` por inversión del hazard acumulado; a cada `t_k` se le asigna
   `coste_k ~ Gamma(exp(x'β_sev), ν)`. De aquí: `t_primer_sin`, `evento`, `n_siniestros`,
   `coste_total`, `coste_medio`, y el historial por año.
5. **Otros conteos.** `n_asistencia` (Poisson pura), `n_fraude` (zero-inflated), `n_gestiones`
   (Poisson con `u_agencia`+`u_region`), con `offset=log(exposicion)`.
6. **Bonus-malus.** Recorriendo el historial anual: parte→malus, sin parte→bonus, con persistencia;
   `bm_prev`,`bm_act` = dos últimos años.
7. **Panel.** Expansión a filas póliza-año con `tstart,tstop`, exposición e historial por intervalo y
   covariables t-dependientes.
8. **`attr("verdad")`.** Todos los parámetros (§8).

---

## 6 · Escala de tiempo y censura

- **Escala base: antigüedad de la póliza** (natural para duración y churn). El primer siniestro y la
  baja se miden desde el alta. Alternativas disponibles como eje: edad del conductor y antigüedad del
  carnet (para discutir la elección de escala en Cox).
- **Censura:** administrativa (fin de estudio `T_admin`) y por baja. Para el análisis del **primer
  siniestro**, la baja actúa como censura (potencialmente **informativa** si correla con el riesgo →
  callout avanzado de riesgos competitivos). Por defecto la baja se genera con dependencia **suave** del
  riesgo para mantener el ejemplo limpio, con opción de reforzarla.

---

## 7 · Mapeo a modelos (resumen operativo)

- **Caso 2:** `n_siniestros`+offset (Poisson/IRR, 2.1) · fragilidad→NB/quasi (2.2) · `n_fraude` ZI (2.3)
  · `n_gestiones` GLMM (2.4) · `t_primer_sin` persona-periodo/cloglog≡Poisson (2.5) · todo+ruido para
  CV/lasso (2.6).
- **Caso 3:** `coste_k` Gamma **y** IG con enlace log (O4) · `t_baja` **AFT** log-logística/log-normal
  (O4) · **Cox** sobre `t_baja`/`t_primer_sin` y test de PH (O5) · recurrentes en panel con offset
  variable (O5, callout) · regularización sobre severidad/Tweedie (O3·O4) · prima pura (N).
- **Proyecto final:** «tres lentes» + política de primas (frecuencia×severidad, recargo, bonus-malus),
  con `sexo` como caso de *fairness*.

---

## 8 · `attr(., "verdad")`

Guardará: `β_claim`, `β_sev` y `ν` (y su versión IG), `β_lapse` y `σ_l` (forma AFT), `k_w` y escala de
`h0` (Weibull), `θ` de la fragilidad, `σ_reg`, `σ_a`, parámetros del logit de ceros (`n_fraude`), betas
de `n_asistencia`/`n_gestiones`, matriz de transición del bonus-malus, `T_max`/`T_admin`, el **efecto
real de `sexo`** (marcado «prohibido»), nombres de β, y la semilla. Base del control de calidad.

---

## 9 · Variantes por equipo

`generar_datos_equipos_auto.R` producirá ~10 carteras **coherentes y distintas**. Se diferencian por:

- **semilla** (obligatorio: cambia todas las realizaciones);
- y un **subconjunto de parámetros** dentro de rangos plausibles, para que las *conclusiones* difieran
  (no solo el ruido muestral): base de frecuencia `λ0`, algunas `β_claim` (p. ej. fuerza del efecto
  urbano o de la potencia), `θ` de fragilidad (más/menos sobredispersión), forma Weibull `k_w`
  (hazard creciente/decreciente), `σ_l` y `β_lapse` (churn más/menos dependiente del precio), `ν` de la
  Gamma (más/menos cola en el coste), y `σ_a` (efecto de agencia). Cada equipo recibe un `equipo_XX/` con
  su `.rds` y su `attr("verdad")`. Enunciado común; números y decisiones propios.

---

## 10 · Fuentes (verificadas; edición/páginas a fijar antes de `references.bib`)

- **Anclas empíricas:** *count ratios* GLM-Poisson sobre **freMTPL** (edad 22–26=0.68, 26–42=0.49…;
  densidad 40–200=1.19 … >4500=2.01); `CASdatasets` (Dutang & Charpentier).
- **Marco actuarial:** Goldburd, Khare, Tevet & Guller — *GLM for Insurance Rating*, CAS Monograph 5,
  2.ª ed. [VERIFICAR año] · de Jong & Heller (2008) · Ohlsson & Johansson (2010) · Denuit et al. (2007)
  · Charpentier (2014) · Wüthrich & Merz (2023).
- **Factores/telemática:** Verbelen, Antonio & Claeskens (2018, *JRSS-C* 67(5):1275–1304) · Henckaerts
  et al. (2018, *Scand. Actuar. J.* [VERIFICAR]) · Boucher, Denuit & Guillén; Boucher & Turcotte (2020).
- **Legal:** TJUE C-236/09 (*Test-Achats*), 1-mar-2011; primas unisex desde 21-dic-2012; Dir. 2004/113/CE.

---

## 11 · Decisiones menores pendientes (no bloquean la programación)

- Escala del bonus-malus: 5 clases (como hasta ahora) vs escala 50–230 tipo freMTPL. *(Propongo 5 clases
  + etiqueta orientativa; fácil de reescalar.)*
- Severidad: ¿generamos también la variante **Inverse-Gaussian** del coste, o solo Gamma y dejamos IG
  como reajuste en clase? *(Propongo generar Gamma; IG se ajusta en Caso 3 sobre el mismo coste.)*
- Riesgos competitivos baja↔siniestro: ¿los dejamos como callout avanzado (recomendado) o los
  integramos ya en el enunciado base?
