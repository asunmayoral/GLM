# Scripts de `caso2/R`: qué genera cada uno y para qué se usa

En el Caso 2 conviven **dos procesos generadores de datos (DGP) distintos** que comparten
contexto (seguro de automóvil) y nombres de variables, pero **no** son el mismo banco de datos:

| | **Teoría** (unidades 2.1–2.7) | **Estudio de caso** (Unidad 2.8) y Caso 3 |
|---|---|---|
| Script generador | `dgp_conteos.R` | `dgp_auto.R` |
| Función que simula | `simular_cartera("auto")` | `simular_cartera_auto()` |
| Función que carga (con caché) | `cargar_cartera("auto")` | `cargar_cartera_auto()` |
| Caché / ficheros | `caso2/datos/cartera_auto_20252026.rds` | `caso2/datos_equipos_auto/equipo_XX/` (una cartera por equipo) |
| Nº de pólizas | 3071 | varía por equipo (p. ej., 3093 en el equipo 01; ver `resumen_equipos.csv`) |
| `exposicion` | `runif(N, 0.30, 1.00)`: fracción de tiempo **sin anclaje en años** | `antiguedad / T_max`, con `T_max = 10` años: la antigüedad **en décadas**; rango 0.01–1 |
| Respuestas de conteo | `n_asistencia`, `n_danos`, `n_fraude`, `n_gestiones` | `n_siniestros`, `n_asistencia`, `n_fraude`, `n_gestiones` (no hay `n_danos`) |
| Además | tabla cuadrada `bonus_malus_prev`/`_act`, tiempo al primer siniestro | coste de cada parte, tiempo hasta la baja, panel póliza-año, tabla cuadrada, *proxy* de `sexo` |

> **Ojo con el nombre «auto».** El `"auto"` de `cargar_cartera("auto")` es el **contexto**
> del diccionario `.dic_auto` dentro de `dgp_conteos.R` (la alternativa es `"salud"`); **no** tiene
> nada que ver con el fichero `dgp_auto.R`. `dgp_conteos.R` no carga `dgp_auto.R` en ningún momento.

> **Ojo con las variables homónimas.** `exposicion`, `n_asistencia`, `n_fraude` o `n_gestiones`
> existen en las dos carteras, pero se generan con parámetros distintos. Ningún resultado numérico
> de la teoría es trasladable a la cartera de un equipo. En particular, la tasa
> `n / exposicion` solo se lee en años («por cada 10 años de póliza») en la cartera de `dgp_auto.R`.

---

## `dgp_conteos.R` — DGP de la teoría

- **Qué hace.** Un mismo motor con dos contextos (`"auto"` y `"salud"`). Cada una de las cuatro
  respuestas de conteo **aísla un fenómeno**: `y_limpio` (Poisson bien portada), `y_sobre`
  (sobredispersión), `y_ceros` (exceso de ceros, mezcla con ceros estructurales) e `y_grupo`
  (efecto aleatorio de agencia). En el contexto `"auto"` se llaman `n_asistencia`, `n_danos`,
  `n_fraude` y `n_gestiones`.
- **Funciones.** `simular_cartera()`, `cargar_cartera()`, `betas_defecto()`,
  `expandir_poliza_tramo()` (persona-periodo).
- **Caché.** `cargar_cartera()` lee `caso2/datos/cartera_<contexto>_<semilla>.rds` y solo vuelve a
  simular si el fichero no existe, si se pide `refrescar = TRUE` o si `dgp_conteos.R` es más reciente
  que la caché.
- **La verdad** de la simulación va en `attr(cartera, "verdad")`.
- **Quién lo usa.** `caso2_conteos.qmd` (y, por inclusión, las unidades 2.1–2.7) y los scripts
  `caso2/scripts/unidad_2_X.R`.
- **Peculiaridad (exceso de ceros).** La parte de conteo de `n_fraude` tiene media alta
  (`log(3.5)` en el intercepto) a propósito: así los ceros estructurales no se confunden con los ceros
  que ya produciría la Poisson, y el exceso de ceros es **visible**. El exceso se juzga comparando los
  ceros observados con los que predice una Poisson de la misma media, no por el porcentaje bruto de
  ceros.

## `dgp_auto.R` — DGP de la cartera de auto (estudio de caso)

- **Qué hace.** Proceso puntual **marcado** con **fragilidad** por póliza: los siniestros llegan
  según un riesgo Weibull, cada uno con su coste Gamma, y la póliza puede darse de baja (AFT
  log-logística). Produce un `data.frame` a nivel póliza con tres atributos:
  `attr(., "siniestros")` (una fila por parte), `attr(., "panel")` (póliza-año, proceso de conteo con
  *offset* variable) y `attr(., "verdad")` (todos los parámetros generadores).
- **Funciones.** `simular_cartera_auto()`, `cargar_cartera_auto()`, `betas_auto_defecto()`.
- **`exposicion`.** Es `antiguedad / T_max` con `T_max = 10`: la misma información que `antiguedad`,
  en décadas.
- **`cargar_cartera_auto()`** genera (y guarda en `caso2/datos/cartera_auto_full_<semilla>.rds`) una
  cartera con los parámetros por defecto. **No** es la cartera de teoría ni la de ningún equipo.
- **Quién lo usa.** `generar_datos_equipos_auto.R` y `validar_dgp_auto.R`. Es la base del estudio de
  caso de la Unidad 2.8 y del estudio de caso del Caso 3 (Unidad 3.8).
- **Peculiaridades.**
  - `tipo_vehiculo` está asociado a `sexo` por construcción (`p_moto_sexo`): es el *proxy* del factor
    prohibido.
  - `n_fraude` tiene una tasa base baja (`lambda_fraude = 0.10` por año); según la configuración del
    equipo, su exceso de ceros puede quedar poco visible.

## `generar_datos_equipos_auto.R` — las 20 carteras de los equipos

- **Qué hace.** Llama a `simular_cartera_auto()` 20 veces, con configuraciones coherentes (mismos
  signos) pero distintas (semilla y magnitudes: `mult_tasa`, `mult_efecto`, `theta_frail`,
  `weib_shape`, `sigma_ag`, `nu_gamma`, `mult_coste`, `persistencia`). Se puede ejecutar desde la raíz,
  desde `caso2/` o desde `caso2/scripts/`.
- **Escribe** en `caso2/datos_equipos_auto/`:
  - `configuraciones_equipos.csv`: parámetros de cada equipo.
  - `resumen_equipos.csv`: indicadores por equipo (N, media y dispersión de `n_siniestros`, % de ceros
    de `n_fraude`, % con siniestro, % de bajas, coste medio).
  - `equipo_XX/cartera.rds`: nivel póliza **con** los atributos `verdad`, `siniestros` y `panel`.
  - `equipo_XX/cartera.csv`: nivel póliza **sin** atributos (para trabajar a ciegas).
  - `equipo_XX/panel.csv`: póliza-año (`tstart`, `tstop`, `expo_anual`, `n_partes_anual`, `edad_t`,
    `antig_veh_t`, `en_riesgo`).
  - `equipo_XX/siniestros.csv`: un parte por fila (`k`, `t_k`, `coste_k`, `anio`).
- **Chequeo de coherencia.** Al final avisa si algún equipo queda con indicadores fuera de rango.
  El criterio para `n_fraude` es el **porcentaje bruto de ceros** (entre 40 y 97 %), que no garantiza
  que haya exceso de ceros respecto a una Poisson.

## `validar_dgp_auto.R` y `validacion_salida.txt` — control de calidad

- `validar_dgp_auto.R` simula la cartera de auto con la semilla del curso (`20252026`), ajusta modelos
  de referencia (binomial negativa para la frecuencia, Gamma para la severidad, AFT y Cox para la baja,
  Weibull para el primer siniestro, tablas para el bonus-malus y el *proxy*) y compara las estimaciones
  con la verdad. Requiere `survival` y `MASS`.
- `validacion_salida.txt` es la salida guardada de una ejecución de ese script.
- Valida **`dgp_auto.R`**, no `dgp_conteos.R`.
