# Diseño del banco de datos del Caso 3 · Averías en una línea de fabricación

> **Estado: APROBADO · DGP construido y verificado** (`R/dgp_averias.R`). Sustituye al banco de
> «degradación» (`dgp_continuas.R`). Contexto industrial concreto y **respuestas realistas y medibles**,
> con una espina única: la **avería**. Simulado con **verdad conocida** (`attr(., "verdad")`).
>
> **Calibración verificada** (semilla 20252026): ~1.200 averías / 400 máquinas / 4 años; conteo por
> máquina binomial negativo (var≈6,7 > media≈3,0); censura de gaps ~25%; φ del modelo fijo inflada
> (~0,41 vs 0,167) → motiva el GLMM; panel Tweedie con ~52% de máquina-años a cero. Pendiente:
> reconstruir el documento madre y las seis unidades sobre este banco.

## 1. Contexto y unidad de análisis

Una fábrica de **mobiliario (mesas de escritorio)** opera una **línea de producción** organizada en
procesos sucesivos: **Corte → Mecanizado (CNC) → Lijado → Ensamblaje → Acabado/Barniz**. Cada máquina
está asignada a un **proceso**. Las máquinas sufren **averías** a lo largo de su vida; de cada avería se
registra **cuándo** ocurre, **de qué tipo** es y **cuánto cuesta** repararla.

El bloque se organiza en tres preguntas sobre ese mismo suceso:

- **¿Cuánto cuesta una avería?** → `coste_euros` (€, positivo y asimétrico → **Gamma**). Unidades 3.1–3.3.
- **¿Cuánto costará el año que viene?** → `coste_total` por máquina-año, **con ceros** (→ **Tweedie**). Unidad 3.4.
- **¿Cada cuánto se avería?** → **tiempo entre fallos** (días, positivo continuo → **AFT / Cox**). Unidades 3.5–3.6.

Las cierran la Unidad 3.7 (las **tres lentes** del fallo y las fronteras del marco) y la 3.8 (estudio
de caso del estudiante, sobre el banco del Caso 2).

Las une un **efecto latente de máquina** (fragilidad): una máquina «mala» **falla más a menudo y cuesta
más de reparar**. Es el efecto aleatorio del GLMM (3.3), la $\sigma_u$ del coste anual (3.4) y la
fragilidad de la supervivencia (3.6).

## 2. Entidades y tablas

**`maquinas`** — una fila por máquina (basal):
`id_maquina`, `proceso` (Corte/Mecanizado/Lijado/Ensamblaje/Acabado — **factor fijo**, contexto de
etapa), `criticidad` (Crítica/Importante/Auxiliar), `fabricante` (A/B/C — control interno),
`fecha_alta` (puesta en servicio, **escalonada** → exposición variable), `potencia_kw`, `carga`
(nivel de uso), `plan_mantenimiento` (Correctivo/Preventivo, **asignado no al azar** → confusión);
si preventivo, `intervalo_mant` (días entre mantenimientos, **constante** por máquina).
*Latente (no se entrega al estudiante):* `Z_maquina` (fragilidad, varianza θ > 0).

**`averias`** — tabla estrella, **una fila por avería**:
`id_averia`, `id_maquina`, `fecha`, `n_orden` (1.ª, 2.ª… avería de esa máquina),
`antiguedad_anios` (edad de la máquina en la fecha), `tiempo_desde_anterior` (días desde la avería
previa; desde el alta si es la 1.ª), `tipo_averia` (Mecánica/Eléctrica/Hidráulica/Electrónica),
`dias_desde_mant` (días desde el último mantenimiento preventivo), `turno` (Mañana/Tarde/Noche —
control interno, sin efecto), `tiempo_parada` (horas de parada de línea que provoca — **etiqueta
descriptiva**, no se modela), `coste_euros` (**respuesta Gamma**).

**Tablas de análisis derivadas** (cada unidad lee lo justo):

| Tabla | Una fila es… | Respuesta | Unidades |
|---|---|---|---|
| `costes` (= averías) | una avería | `coste_euros` | 3.1 GLM Gamma · 3.2 familia/escala · 3.3 GLMM Gamma (agrupa `id_maquina`) |
| `panel` | una máquina × periodo (año) | `coste_total`, `n_averias`, `exposicion` | 3.4 Tweedie (y GLMM Tweedie) |
| `intervalos` | un intervalo entre fallos | `tiempo_entre`, `evento` (1 fallo / 0 censura) | 3.5 AFT · 3.6 Cox (fragilidad, no-PH) |
| `seguimiento` | una máquina × mes (*start-stop*) | `fallo` (+ `offset`) | 3.6 Cox extendido · 3.7 lentes discreta/a-trozos |

## 3. Variables y su papel

| Papel | Variables |
|---|---|
| Estructura | `id_maquina`, `proceso`, `id_averia`, `fecha`, `fecha_alta`, `n_orden` |
| Respuestas | `coste_euros` (Gamma); `tiempo_entre`/`evento` (supervivencia); `coste_total`/`n_averias` (panel, Tweedie) |
| Predictores de la avería | `tipo_averia`, `antiguedad_anios` (a la fecha), `dias_desde_mant` (dependiente del tiempo) |
| Predictores de la máquina | `proceso`, `criticidad`, `fabricante`, `potencia_kw`, `carga`, `plan_mantenimiento`, `intervalo_mant` |
| Control interno (sin efecto) | `turno`, `fabricante` |
| Etiqueta descriptiva (no se modela) | `tiempo_parada` (horas que la línea se detiene por la avería; impacto operativo, crece con la criticidad) |

> **`tiempo_parada` ≠ `exposicion`.** `exposicion` es el tiempo *observado / en riesgo* (offset,
> denominador de tasas); `tiempo_parada` es la *consecuencia* de la avería (parada de línea). No se
> solapan. `tiempo_parada` se registra para un análisis posterior de tiempos muertos, no como respuesta.

**Cambios respecto de la v1** (tus puntos): fuera `id_planta`/`zona`/`calidad_mantenimiento`/`color`/
`tipo_maquina`/`carga_media`/sensor con ausencias; `planta`→`proceso` (fijo); + `criticidad`; +
mantenimiento periódico explícito con `intervalo_mant` y `dias_desde_mant`; una sola `carga`.

## 4. Mecanismos generadores (la «verdad»)

1. **Fragilidad de máquina LIGADA.** Un único efecto latente `Z_maquina` (media 1, **varianza θ > 0**;
   calibrada en θ ≈ 0,12). Sube **a la vez** la intensidad de fallo (máquina que falla más) y el coste
   (a través de `log(Z)` como intercepto aleatorio). `θ` gobierna la correlación «falla más ↔ cuesta
   más». *Nota:* una fragilidad Gamma sobre el proceso de fallos hace que el **nº de averías por
   máquina sea binomial negativa** — el mecanismo del Caso 2, reencontrado.

2. **Proceso de averías recurrente.** Intensidad `λ_i(t) = λ0(t)·exp(xᵢ'β_haz)·Z_i`, con `λ0(t)`
   **creciente con la edad** (desgaste). Riesgo según antigüedad, proceso, criticidad, carga y
   mantenimiento. Los **tiempos entre fallos** salen de aquí; la última espera de cada máquina queda
   **censurada** al cierre de la ventana.

3. **Mantenimiento preventivo periódico y explícito.** Las máquinas con plan preventivo se mantienen
   **cada `intervalo_mant` días** (constante por máquina). Se registran las fechas → `dias_desde_mant`
   como **covariable dependiente del tiempo** que **reduce el riesgo** justo tras el mantenimiento y
   **decae** con el tiempo (efecto **no proporcional** → lo detecta `cox.zph` en 3.6). El **intervalo**
   es una **palanca de decisión**: «¿acortar `intervalo_mant` retrasa el fallo?» (pregunta prescriptiva
   de 3.5–3.6). El plan se asigna **no al azar** (máquinas críticas/viejas lo usan más) → **confusión**.

4. **Coste por avería** `~ Gamma(μ, φ)`, con
   `log μ = α + f(tipo_averia) + f(proceso) + f(criticidad) + β·antiguedad + β·carga + log(Z_i)`.
   `φ` constante (CV condicional constante = firma Gamma). `criticidad` es un motor fuerte (la parada
   de línea encarece). `log(Z_i)` es el **intercepto aleatorio de máquina** que recupera el GLMM (3.3).

5. **Censura** por **ventana de observación** (p. ej. 4 años); **altas escalonadas** → exposición
   variable (offset natural del panel/Tweedie). **Datos completos** (sin ausencias).

Todo parámetro es argumento del simulador y queda en `attr(., "verdad")`: `β_coste`, `β_haz`, `φ`,
`θ` (fragilidad), `λ0`, efecto y `τ` del mantenimiento, semilla.

## 5. Cómo lo usa cada unidad

- **3.1 GLM Gamma** — `coste_euros ~ tipo_averia + proceso + criticidad + antiguedad + carga`. Respuesta
  concreta (€), lectura multiplicativa; el gaussiano falla (costes negativos).
- **3.2 Elegir familia y escala** — Gamma vs inversa gaussiana vs lognormal (log-OLS) sobre el coste.
- **3.3 GLMM Gamma** — `... + (1 | id_maquina)`. El efecto aleatorio = «máquinas crónicamente caras»;
  averías repetidas por máquina = medidas correlacionadas. Recupera θ. (`proceso` va como fijo; el
  anidamiento se menciona como extensión.)
- **3.4 Tweedie** — `coste_total` por máquina-año (con ceros) = **frecuencia × severidad = presupuesto
  esperado**, como mezcla Poisson–Gamma. Sitúa la familia en la recta `Var = φ·μ^p` y estima el índice
  `p`; predicción con intervalo; **GLMM Tweedie** por máquina y **validación cruzada agrupada**;
  regularización como límite conceptual. Cierra el arco con la prima pura del Caso 2, en clave industrial.
- **3.5 AFT** — `tiempo_entre` con censura; KM descriptivo; Weibull/lognormal; factor de aceleración;
  **predecir el siguiente fallo** y el efecto de acortar el intervalo de mantenimiento.
- **3.6 Cox como frontera** — Cox de tiempos entre fallos con **fragilidad** (correlación intra-máquina)
  y diagnóstico de proporcionalidad (el mantenimiento la rompe, por diseño); **Cox extendido** sobre el
  panel mensual con la protección del mantenimiento, que decae.
- **3.7 Cierre** — las **tres lentes** sobre el mismo fallo (cloglog persona-periodo / Poisson a trozos /
  Cox) sobre `seguimiento`, y el mapa de **fronteras** del marco GLM.
- **3.8 Estudio de caso** — encargo del estudiante, sobre la cartera de la aseguradora del **Caso 2**
  (coste y baja de pólizas): no consume el banco de averías.

## 6. Decisiones (todas cerradas)

- Avería como unidad; supervivencia = tiempos entre fallos (recurrente); fragilidad **ligada**;
  Tweedie como cierre del arco del coste; `proceso` (factor fijo) en vez de planta; `criticidad` **solo en el coste**
  (no en el riesgo), con interacción `criticidad × proceso-cuello`; riesgo **creciente con la edad**;
  GLMM con **intercepto y pendiente aleatorios** por máquina.
- Mantenimiento preventivo **periódico** con fechas (`intervalo_mant`, `dias_desde_mant`); plan
  `Correctivo`/`Preventivo` asignado **no al azar** (confusión). El intervalo es palanca de decisión.
- **Altas escalonadas** (parte de la flota preexistente, parte puesta en servicio durante el estudio)
  → **exposición variable** y offset real para el panel/Tweedie.
- Contexto: **mobiliario (mesas de escritorio)**, línea Corte→Mecanizado→Lijado→Ensamblaje→Acabado.
- Tamaños: **400 máquinas** (fábrica mediana-grande, varias líneas), ventana **4 años**, ~1.000 averías.
- `tipo_averia`: **4** (Mecánica/Eléctrica/Hidráulica/Electrónica); `criticidad`: **3**
  (Crítica/Importante/Auxiliar).
- Fuera: `zona`, `calidad_mantenimiento`, `color`, `tipo_maquina`, `carga_media`, sensor MAR.
- `turno` y `fabricante` como controles internos; `tiempo_parada` como etiqueta descriptiva.

## 7. Qué implica reconstruir

- Nuevo DGP `R/dgp_averias.R`, con la arquitectura de eventos recurrentes + fragilidad ya validada en
  `caso2/R/dgp_auto.R` como plantilla; regenerar la caché `caso3/datos`.
- Re-contextualizar y **re-verificar** las unidades (la estadística no cambia: Gamma, GLMM Gamma,
  AFT, Cox; cambian el banco y las cifras); la narrativa se **simplifica** (una espina, dos mitades).
- Actualizar el documento madre y `PROPUESTA_CASO3.md`; archivar el banco «degradación» anterior.
