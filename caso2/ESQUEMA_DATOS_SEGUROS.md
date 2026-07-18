# Esquema relacional · Cartera de seguros simulada (Caso 2)

> Propuesta **previa a la simulación**. Objetivo: un mismo banco de datos que sirva de esqueleto para
> la teoría y para el estudio de caso, con **predictores de sobra**, **fenómenos separables** por
> unidad, una **variable de agrupación** para el mixto, y **parámetros modificables** para generar
> variantes por equipo.

## Contexto

Aseguradora de **auto**. Unidad de análisis: la **póliza**, con su **exposición** (tiempo de
cobertura en el periodo). El ramo auto da mucho juego de predictores realistas.

## Diagrama (relacional, tipo estrella)

```
        agencias (dim)                      polizas (hecho, 1 fila/póliza)
        ------------                        --------------------------------
        id_agencia  (PK)  <──── FK ──────   id_agencia
        zona                                id_poliza (PK)
        canal                               exposicion            → offset = log(exposicion)
        (u_agencia = efecto                 [predictores activos]
         aleatorio verdadero)              [predictores de ruido/extra]
                                            bonus_malus_prev, bonus_malus_act   (tabla cuadrada)
                                            n_asistencia   (LIMPIA)             → 2.1
                                            n_danos        (SOBREDISPERSIÓN)    → 2.2
                                            n_fraude       (EXCESO DE CEROS)    → 2.3
                                            n_gestiones        (EFECTO DE AGENCIA)  → 2.4
                                            tiempo_primer_sin, evento           → 2.5
```

## Tabla 1 · `agencias` (agrupación, para el GLMM)

| Campo | Tipo | Papel |
|---|---|---|
| `id_agencia` | id (PK) | clave |
| `zona` | factor (p. ej. 5 zonas) | predictor de nivel-grupo |
| `canal` | factor (oficina/online/mediador) | contexto |
| `u_agencia` | num (**verdad**) | efecto aleatorio ~ N(0, σ_a²), no observable |

~**30 agencias**, de tamaño desigual (realismo).

## Tabla 2 · `polizas` (principal, 1 fila por póliza)

Claves: `id_poliza` (PK), `id_agencia` (FK). Exposición: `exposicion` (0 < e ≤ 1) → **offset**.

**Predictores activos** (influyen; para ilustrar modelos):

`edad_conductor`, `antiguedad_carnet`, `potencia_cv`, `antiguedad_vehiculo`,
`zona_circulacion` (urbana/mixta/rural), `uso` (particular/comercial), `tipo_vehiculo`
(turismo/moto/furgoneta).

**Predictores de ruido / extra** (para que el alumno explore y para que el **lasso** tenga qué
descartar): `sexo`, `estado_civil`, `color_vehiculo`, `tiene_garaje`, `forma_pago`
(anual/mensual), `financiado`, `km_declarados`, `valor_vehiculo`, `n_conductores`,
`antiguedad_cliente`. (Relación nula o muy débil con las respuestas.)

**Tabla de contingencia** (2.1): cruces de categóricas (`tipo_vehiculo` × `zona_circulacion` × `uso`)
con un indicador `tuvo_siniestro` (0/1) → independencia/asociación (log-lineal).

**Tabla cuadrada / modelo de cambio** (2.1): `bonus_malus_prev` y `bonus_malus_act` (mismos niveles,
p. ej. 1–5), medidos en dos años consecutivos → tabla 5×5 para **simetría / homogeneidad marginal /
movilidad**. Se simula con una **matriz de transición** (persistencia + deriva según siniestralidad).

**Respuestas de conteo** (cada una aísla un fenómeno; comparten predictores y exposición):

| Columna | Fenómeno | Proceso generador | Unidad |
|---|---|---|---|
| `n_asistencia` | **Limpia** | Poisson(exp(η)), solo efectos fijos | 2.1 |
| `n_danos` | **Sobredispersión** | Poisson-Gamma / log-normal (heterogeneidad individual) | 2.2 |
| `n_fraude` | **Exceso de ceros** | *zero-inflated*: masa estructural (logit) + Poisson | 2.3 |
| `n_gestiones` | **Agrupación** | Poisson(exp(η + u_agencia)) | 2.4 |

donde η = β₀ + Xβ + log(exposicion). (Cada respuesta con sus **propias β**; ver nota de diseño.)

**Estructura tiempo-a-evento** (2.5, riesgos a trozos): `tiempo_primer_sin` (tiempo hasta el primer
siniestro) y `evento` (1 = observado, 0 = censura al fin de cobertura), derivados de un **hazard base
por tramo de antigüedad** de la póliza. De aquí se obtiene, por expansión, un `polizas_pp` (una fila
por póliza y tramo, con `offset = log(tiempo expuesto)`), análogo al `pp` del Caso 1.

## `attr(., "verdad")` — parámetros generadores

β de cada respuesta, σ_a (agencia), dispersión de la NB (θ), parámetros del logit de ceros
estructurales, matriz de transición del bonus-malus, hazard base por tramo, semilla. Para el
**control de calidad** contra el DGP, como en el Caso 1.

## Cómo se separan los fenómenos (tu petición)

- **Sin ceros + contingencia:** `n_asistencia` + las categóricas + bonus-malus prev/act.
- **Exceso de ceros:** `n_fraude`.
- **Sobredispersión:** `n_danos`.
- **Agrupación (mixto):** `n_gestiones` con `(1 | id_agencia)`.

Cada unidad trabaja su columna; el alumno puede además cruzar predictores extra y ampliar.

## Parámetros modificables → variantes por equipo

`simular_cartera(...)` recibirá **todos** los parámetros (β por respuesta, σ_a, θ, π-ceros, matriz de
transición, hazard base, tamaños, semilla). Cambiarlos genera **datos distintos con relaciones
distintas** —igual que en el Caso 1—. Un `generar_datos_equipos_conteos.R` producirá 10
configuraciones coherentes y diferentes para el estudio de caso.

## Nota de diseño (a decidir)

Propongo **cuatro** respuestas de conteo (una por fenómeno) para **máxima separabilidad**. Alternativa
más compacta: **tres** respuestas (limpia/sobre/ceros) metiendo el efecto de agencia dentro de
`n_danos` (entonces 2.2 vería la sobredispersión y 2.4 explicaría parte de ella con `(1|agencia)`
—un arco bonito, pero mezcla dos fenómenos en una respuesta—).
