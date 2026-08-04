# Plan de reorganización del Caso 3

> Documento de trabajo. Recoge lo acordado en la revisión de agosto de 2026: reparto de unidades,
> poda de Cox, nueva unidad de síntesis y estudio de caso. Pensado para ir pidiendo la faena por
> partes; cada bloque es autónomo.
>
> **Estado:** ninguna de las cuatro partes está ejecutada todavía.

---

## 0. El diagnóstico, en corto

El material del Caso 3 son **6 unidades de contenido** (6914 líneas) repartidas en **4 sesiones de
teoría** (semanas 10–13). El reparto actual está muy desequilibrado:

| Semana (guía) | Material que le toca hoy | Líneas |
|---|---|---:|
| 10 · Gamma e IG | 3.1 + 3.2 | 1130 |
| 11 · Selección y regularización | 3.3 | 544 |
| 12 · AFT | 3.4 | 642 |
| 13 · Cox + síntesis | **3.5 + 3.6** | **4539** |

La media por sesión sería 1714 líneas; la semana 13 carga **2,6 sesiones**. Podar solo Cox no
arregla nada: aun quitándole el 40 %, la semana 13 seguiría en 3850.

**La clave.** El bloque de la Tweedie (actuales §6.2–6.4) **no depende de AFT ni de Cox**: se apoya
solo en la Gamma de 3.1, la relación media-varianza de 3.2 y el GLMM de 3.3. Quien exige haber visto
Cox es §6.1 (las tres lentes). El material estaba encadenado como si todo dependiera de la
supervivencia, y no era cierto. Eso permite adelantar la Tweedie y equilibrar el curso.

**Dos huecos que lo hacen viable.** La semana 11 declaraba «selección y regularización», contenido
que ya migró —la selección vive dentro de cada unidad y la regularización quedó como límite
conceptual en la síntesis—, así que esa sesión está de hecho libre. Y 3.3 encaja mejor en la semana
10, pegada a la Gamma de la que es extensión.

---

## 1. Replanteamiento de las unidades

### 1.1 Estructura nueva

Ocho unidades, con una idea clave: **la última de contenido (3.7) es una unidad de referencia**, no
de clase. Eso libera la presión sobre el calendario y evita tener que mutilar Cox.

| Nueva | Título | Procede de | Semana | Líneas |
|:--:|:-----------------------------|:-----------------|:--:|---:|
| **3.1** | GLM Gamma | 3.1 (sin cambios) | 10 | 771 |
| **3.2** | Elegir familia y escala | 3.2 (sin cambios) | 10 | 359 |
| **3.3** | Efectos aleatorios y modelos mixtos | 3.3 (sin cambios) | 10 | 544 |
| **3.4** | **El coste agregado: la familia Tweedie** | §6.2 + §6.3 + §6.4 de la actual 3.6 | 11 | 2009 |
| **3.5** | Del coste al tiempo: AFT | 3.4 (sin cambios) | 12 | 642 |
| **3.6** | Cox como frontera del marco | 3.5 (poda **opcional**) | 13 | 1719 |
| **3.7** | **Más allá: las tres lentes y las fronteras** | §6.1 + §6.5 de la actual 3.6 | — *(referencia)* | 662 |
| **3.8** | Estudio de caso, exposición y evaluación | 3.7 (a escribir) | 14 | — |

Reparto por sesión: **1674 / 2009 / 642 / 1719**, con media 1511.

### 1.2 Dos matices sobre este reparto

**La semana 12 queda holgada** (AFT, 642 líneas, menos de la mitad de la media). No es
necesariamente un problema —el AFT es denso por línea: censura en la verosimilitud, Kaplan-Meier,
log-rank, factor de aceleración y elección de distribución— pero conviene tenerlo previsto: si sobra
tiempo, el desbordamiento natural es **empezar Cox en la semana 12**, que además descarga la 13.

**Cuidado con relegar §6.1 del todo.** Las tres lentes son *el pago* de haber enseñado Cox: el
momento en que tres maquinarias que parecían mundos distintos estiman lo mismo hasta la tercera
cifra. Si 3.7 se deja como lectura, esa inversión se pierde en parte. Son solo 437 líneas de las 662
de la unidad, así que la sugerencia es **reservarle el tramo final de la semana 13**, cuando Cox
cierre; el resto de 3.7 (las fronteras, §6.5) sí funciona perfectamente como referencia.

Con este reparto la **poda de Cox deja de ser necesaria** y pasa a ser opcional (ver punto 2).

### 1.3 Trabajo que implica

1. **Partir `_unidad_3_6.qmd`** en dos ficheros:
   - `_unidad_3_4.qmd` ← §6.2 + §6.3 + §6.4, renumerando §6.2→§4.1, §6.3→§4.2, §6.4→§4.3.
   - `_unidad_3_7.qmd` ← §6.1 + §6.5, renumerando §6.1→§7.1, §6.5→§7.2.
2. **Renumerar** los ficheros de supervivencia: actual 3.4 → `_unidad_3_5.qmd`; actual 3.5 →
   `_unidad_3_6.qmd`. Y el estudio de caso, actual 3.7 → `_unidad_3_8.qmd`.
   *Ojo al orden de las operaciones para no pisar ficheros.*
3. **Recablear referencias cruzadas.** Es el punto delicado, y está inventariado:
   - **Diez referencias** del bloque Tweedie apuntan hoy a §6.1 (`#sec-u36-lentes`). Al adelantarse
     la Tweedie, pasan a ser **reenvíos hacia delante** («lo veremos en 3.7») o se reconducen a 3.3
     (el efecto aleatorio) y al Caso 2 (la Poisson de frecuencia). Las más comprometidas son las de
     la validación de §6.3 —«el log-hazard de las tres lentes»— y la de §6.4 sobre la fragilidad
     «estimada en 6.1», que ya no se habrá estimado.
   - Las **anclas** `#sec-u36-*` del bloque Tweedie pasan a `#sec-u34-*`; las de §6.1/§6.5, a
     `#sec-u37-*`. Hay que actualizar también las referencias **desde otras unidades** y desde 3.5
     (que enlaza con la síntesis).
   - Las llamadas cruzadas de tablas y figuras (`@tbl-u36-*`, `@fig-u36-*`) siguen la misma suerte.
4. **Dependencias de objetos entre unidades.**
   - §6.1 usa `sg`, `sg$prot_mant`, `cox_mes_opt` y `cox_mes_opt_frag`, todos de Cox. Como §6.1 pasa
     a 3.7 —después de Cox—, **la dependencia se mantiene y no hay nada que arreglar**. Es un
     argumento más a favor de este orden.
   - La Tweedie usa `pn` y la edad agregada desde `sg`. Como ahora va **antes** de Cox, **habrá que
     construir `sg` (o solo la edad) dentro de la unidad nueva**, leyendo `banco$seguimiento`
     directamente. Es un chunk de tres líneas.
5. **Actualizar** `caso3_continuas_positivas.qmd`: orden de los `{{< include >}}` y el **Mapa del
   caso** (punto 3).
6. **Regenerar** los scripts de `caso3/scripts/` con `_scripts/generar_scripts_unidades.R`.
7. **Actualizar la guía docente**: cronograma del Caso 3 y, de paso, reflejar que son **siete**
   unidades de contenido; 3.3 no figura hoy en el cronograma.

---

## 2. La poda de Cox (nueva 3.6) — ahora **opcional**

Con Cox disfrutando de la semana 13 entera, la poda deja de ser una necesidad de calendario. Sigue
teniendo sentido por otros dos motivos, más débiles:

- la unidad es la segunda más larga del caso y su tramo final (`tt()`, Cox extendido, predicción con
  escenarios) es el más técnico y el que menos transfiere al marco GLM;
- ese material es **exactamente** la ampliación de la propuesta C del estudio de caso, así que
  sacarlo del temario no lo pierde: lo reubica donde rinde más.

Recomendación: **poda ligera** (de 1719 a ~1400), o ninguna si prefieres dar Cox completo y dejar la
propuesta C apoyada en lo ya visto.

### 2.1 Qué saldría

| Bloque | Líneas aprox. | Destino |
|:--------------------------|---:|:----------------------------------|
| §5.8 · `tt()` y el formato exacto (la «segunda arbitrariedad») | ~250 | **Tarea del estudiante** (propuesta C de 3.7) |
| §5.8 · desarrollo largo del Cox extendido (perfil de τ, dos cribas sucesivas) | ~200 | Comprimir a un callout plegable |
| §5.9 · predicción con `survfit` y escenarios | ~180 | Comprimir; el grueso pasa a la tarea |
| Repeticiones del recetario final | ~60 | Fundir con el callout «En R» existente |

Los tres primeros son material excelente, y por eso **no se tira**: se convierte en la ampliación del
estudio de caso, que es donde rinde más. La propuesta C de la sección 4 lo recoge explícitamente.

### 2.2 Qué se queda, intacto

- Riesgos proporcionales y verosimilitud parcial; **por qué Cox no es un GLM**.
- Interpretación de los *hazard ratios* y su relación con el AFT.
- Inferencia: LRT por término, el *score* como log-rank.
- **Fragilidad** (la del hilo en espiral) y el falso positivo del `fabricante`.
- **Diagnóstico de proporcionalidad**: Schoenfeld y `cox.zph`.
- Una versión **breve** del Cox extendido: lo justo para que §6.1 pueda montar la comparación.
- Validación contra el DGP.

### 2.3 Cuidado con

- §6.1 (que pasa a 3.7) necesita `cox_mes_opt` y `cox_mes_opt_frag`. Si se poda, hay que
  **garantizar que esos dos objetos siguen ajustándose**, o reajustarlos en 3.7.
- La unidad conserva su numeración interna (§5.1–§5.9) pero pasa a ser la **3.6**: hay que renumerar
  los encabezados y las anclas `#sec-u35-*` → `#sec-u36-*`. Ojo: esas anclas colisionan con las que
  hoy usa la unidad 3.6 actual, así que **el renombrado debe hacerse en el orden correcto** (primero
  partir la 3.6 actual, luego renumerar supervivencia).

---

## 3. La nueva Unidad 3.7 · Más allá

### 3.1 Contenido

| Sección | Procede de | Contenido | Líneas |
|:--:|:---|:---|---:|
| 7.1 | §6.1 actual | **Las tres lentes** sobre el mismo fallo: `cloglog` persona-periodo, Poisson a trozos y Cox extendido. Coincidencia numérica, el AFT como lente distinta, el riesgo de base plano, la fragilidad y la validación con intervalos. | 437 |
| 7.2 | §6.5 actual | **Las fronteras**: tabla de preguntas abiertas (hurdle, GAM, eventos recurrentes, riesgos competitivos, PAMM/Royston–Parmar, Aalen, bayesiano) y el límite de la regularización. | 225 |

Es una unidad **de referencia**: cierra el bloque y sirve de consulta, sin exigir sesión propia. Con
la salvedad del punto 1.2: §7.1 merece el tramo final de la semana 13.

### 3.2 Qué se elimina

**La subsección «El caso, de un vistazo»** (el recopilatorio con la tabla de las unidades, añadido al
inicio de §6.5). Su sitio natural no es el final del bloque sino el **principio**: pasa al Mapa del
caso. Los tres hilos que la acompañaban —«la respuesta manda», «casi todo es el mismo marco», «el
mismo latente cuatro veces»— **se conservan**, pero como cierre del curso al final de la unidad, no
como recapitulación tabulada.

### 3.3 El Mapa del caso, reorganizado

Va en `caso3_continuas_positivas.qmd`, sustituyendo al actual. Mantiene el formato de dos tablas
—mapa y caja de herramientas de R— y absorbe el recopilatorio eliminado. Borrador de la tabla
principal:

| Unidad | La pregunta | Datos y respuesta | El modelo |
|:--:|:--------------------|:-----------------|:--------------|
| 3.1 | ¿Cuánto cuesta una avería? | `averias`: importe de cada reparación (€) | GLM Gamma, enlace log |
| 3.2 | ¿Es de verdad una Gamma, y en qué escala? | el mismo importe | Inversa gaussiana y log-OLS frente a Gamma |
| 3.3 | ¿Hay máquinas crónicamente caras? | `averias` agrupadas por máquina | GLMM Gamma |
| 3.4 | ¿Cuánto costará el mantenimiento el año que viene? | `panel`: máquina-año, coste total **con ceros** | Tweedie, con efecto aleatorio de máquina |
| 3.5 | ¿Cada cuánto se avería? | `intervalos`: tiempo entre fallos, con censura | AFT exponencial, Weibull y lognormal |
| 3.6 | ¿Es proporcional el riesgo a lo largo del tiempo? | `intervalos` y `seguimiento` | Cox, con fragilidad y covariable temporal |
| 3.7 | ¿Dicen lo mismo las tres lecturas del fallo? ¿Y qué queda fuera? | `seguimiento`: máquina-mes | `cloglog`, Poisson a trozos y Cox; mapa de fronteras |
| 3.8 | ¿Qué recomendamos, y es reproducible? | cartera de auto (Caso 2) | el que cada equipo justifique |

**Aviso sobre la narrativa del mapa.** Hoy el texto introductorio dice: «¿Cuánto cuesta una avería?
Unidades 3.1–3.3. ¿Cada cuánto se avería? Unidades 3.4–3.5». Con el reparto nuevo la secuencia pasa
a ser **severidad → coste agregado → tiempo → síntesis**, de modo que ese párrafo hay que
reescribirlo entero. La estructura en dos mitades sigue valiendo, pero el coste agregado (3.4) es
precisamente **el punto donde las dos se juntan**, y conviene decirlo ahí y no al final.

## 4. Las siete tareas del estudio de caso (Unidad 3.8)

**Formato del entregable:** artículo de investigación (§7.1 de la guía).
**Datos:** la cartera de auto del Caso 2, ya generada por equipos en `caso2/datos_equipos_auto/`.
Cada equipo recibe `cartera.csv` (una fila por póliza), `panel.csv` (póliza-año), `siniestros.csv`
(**una fila por parte, con `t_k` y `coste_k`**); la `.rds` con `attr("verdad")` queda para el
control docente.

**Viabilidad: confirmada.** La cabecera de `dgp_auto.R` declara que cubre «severidad Gamma/IG
(Caso 3-O4), AFT de bajas no-PH y Cox (Caso 3-O5), eventos recurrentes (panel)», el mapa docente de
2.8 reserva explícitamente el coste y la baja para el Caso 3, y `validacion_salida.txt` confirma que
el DGP se ha ejecutado y validado. Tres rasgos del diseño son especialmente aprovechables:

- la **baja se genera log-logística** → AFT verdadero y **no proporcional**, con `cox.zph` que no lo
  detecta;
- el **`coste_total` tiene ceros** (≈69 % de pólizas sin siniestro) → Tweedie o dos partes;
- la **fuga del proxy es mayor por severidad que por frecuencia** (×1,043 frente a ×1,026; ×1,070 en
  prima pura) → gancho al Caso 3 dejado a propósito.

### Las propuestas

**A · La prima pura por tres caminos.**
*Pregunta:* ¿cuánto cuesta asegurar una póliza?
*Modelo del Caso 3:* Gamma sobre `siniestros.csv` (severidad) combinada con la frecuencia del Caso 2;
y Tweedie directa sobre `coste_total`.
*Ampliación:* **modelo en dos partes** binomial + Gamma.
*Validación:* `coste_base`, `nu_gamma` y `betas$sev` del DGP.
*Por qué:* es la síntesis del curso entero, y mete el binomial-Gamma sin gastar calendario.

**B · Por dónde se cuela el sexo en la tarifa.**
*Pregunta:* si la ley prohíbe tarificar con el sexo, ¿basta con quitarlo del modelo?
*Modelo:* Gamma de severidad con y sin `tipo_vehiculo`, midiendo la fuga en frecuencia, severidad y
prima pura.
*Ampliación:* estrategias de mitigación y su coste predictivo.
*Validación:* `attr(...)$proxy`, con las tres cifras de fuga.
*Por qué:* cierra el arco ético abierto en el Caso 2 (Q17–Q18), que allí solo llegó a la frecuencia.

**C · ¿Cuánto dura un cliente?**
*Pregunta:* ¿qué explica la baja, y a qué ritmo se van?
*Modelo:* AFT sobre `t_baja`/`baja`.
*Ampliación:* Cox y Schoenfeld —que **no rechazan** aunque el modelo verdadero no sea PH—, más el
`tt()` y el Cox extendido podados de 3.5.
*Validación:* `lapse_median`, `sigma_lapse`, `betas$lapse`.
*Por qué:* la mejor tarea para enseñar que un contraste no significativo no prueba el supuesto.

**D · ¿Hay agencias caras?**
*Pregunta:* ¿tramitan más caro unas agencias que otras, o es cosa de su cartera?
*Modelo:* GLMM Gamma sobre el coste por parte, con agencia anidada en región.
*Ampliación:* BLUPs, *caterpillar*, encogimiento y predicción para una agencia nueva.
*Validación:* `sigma_ag`, `sigma_reg`, `u_ag`, `u_reg`.
*Por qué:* paralelo exacto de lo que el Caso 2 hizo con conteos (Q9–Q12), ahora con dinero.

**E · ¿Qué distribución describe el coste de un parte?**
*Pregunta:* ¿Gamma, inversa gaussiana o lognormal?
*Modelo:* comparación de familias, relación media-varianza, sesgo de retransformación.
*Ampliación:* modelar la dispersión, o cuasi-verosimilitud.
*Validación:* `nu_gamma` y el coste base.
*Por qué:* la más metodológica; buena para equipos con perfil teórico.

**F · Del primer parte al historial completo.**
*Pregunta:* ¿se comporta el riesgo de los siguientes siniestros como el del primero?
*Modelo:* riesgos a trozos y Cox sobre `panel.csv`.
*Ampliación:* **Andersen–Gill** para eventos recurrentes.
*Validación:* `weib_shape`, `lambda0`, `theta_frail`.
*Cuidado:* solapa con Q13–Q15 del Caso 2. Conviene exigir que la respuesta sea el **coste
acumulado**, no solo el recuento, para que sea Caso 3 y no repetición.

**G · El presupuesto del año que viene.**
*Pregunta:* ¿cuánto hay que reservar, y con qué margen?
*Modelo:* Tweedie con efecto aleatorio sobre el coste anual por póliza (`panel.csv`).
*Ampliación:* intervalos de predicción por simulación y percentil de reserva.
*Validación:* comparación de la predicción agregada con el coste observado.
*Por qué:* transfer directo de la nueva 3.4 a datos nuevos; la más segura y la que mejor prepara el
informe con recomendación.

### Recomendación

Si los equipos trabajan con carteras distintas, las tres más fuertes son **A, B y C**: cada una tiene
una pregunta que un artículo real haría, una validación contra la verdad que funciona como control de
calidad, y una ampliación natural. **C** además da destino al material podado de Cox y **A** al
binomial-Gamma.

Queda por decidir si se ofrecen las siete y cada equipo elige, o si se asignan.

---

## 5. Orden de trabajo sugerido

1. **Partir la unidad 3.6 actual** en `_unidad_3_4.qmd` (Tweedie) y `_unidad_3_7.qmd` (más allá).
   Es el paso que más desbloquea y el que fija la numeración.
2. **Renumerar** las unidades de supervivencia (3.4→3.5, 3.5→3.6) y el estudio de caso (3.7→3.8),
   cuidando el orden para no pisar ficheros ni anclas.
3. **Recablear** las referencias cruzadas inventariadas en 1.3 y hacer la Tweedie independiente de
   `sg`.
4. Reescribir el **Mapa del caso** y el orden de los `include` (punto 3.3).
5. Escribir la **Unidad 3.8** con las tareas (punto 4).
6. Decidir si se aplica la **poda ligera de Cox** (punto 2).
7. Regenerar scripts y actualizar la **guía docente**.
8. Render completo y revisión de cifras.

### Pendientes anteriores que conviene no perder

- La sección `# Reproducibilidad` del documento maestro sigue vacía (`<!-- TODO -->`), y es contenido
  asociado a O6.
- La **tarea del estudiante** de la actual 3.6 nunca se escribió; con la reorganización, sus ítems
  (sensibilidad al troceado del riesgo base, descomposición empírica frecuencia × severidad) se
  reparten entre las unidades nuevas o se integran en 3.7.
- El perfil de verosimilitud del índice $p$ quedó verificado fuera del documento (IC 1,33–1,38) pero
  no incorporado.
