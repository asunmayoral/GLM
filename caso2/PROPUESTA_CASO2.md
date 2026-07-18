# Propuesta de estructura · Caso 2 — «¿Cuántas veces y a qué ritmo?»
### Conteos, tasas e intensidad del riesgo (GLM de Poisson y más allá)

> Borrador para revisar. Refleja el encaje en la guía docente y replica la arquitectura del Caso 1.
> Las **decisiones abiertas** están al final: conviene cerrarlas antes de redactar.

---

## 1. Encaje en la guía docente

- **Semanas 6–9** (bloque Caso 2). Tres semanas de fundamentación (6–8) + una de estudio de caso (9).
- **Contenidos (guía §5):** regresión de Poisson; tasas, offset e interpretación (IRR); modelos
  log-lineales. Sobredispersión: quasi-Poisson y binomial negativa. Exceso de ceros (hurdle,
  zero-inflated). Capa de mixtos: GLMM de Poisson. Supervivencia: riesgos a trozos como Poisson con
  `offset(log tiempo-persona)`, y nexo con la fragilidad (*frailty* ≡ GLMM de Poisson).
- **Librerías (guía):** `stats` (glm), `MASS` (glm.nb), `pscl`, `glmmTMB`, `lme4`, `DHARMa`,
  `performance`, `marginaleffects`, `survival`.
- **Bibliografía (guía):** Hilbe (caps. 1–8, 11–12); Dobson & Barnett (cap. 9 y cap. de
  supervivencia); Agresti (cap. 7); Faraway (cap. 5); McCullagh & Nelder (cap. 6); Aitkin & Clayton
  (1980).

## 2. Objetivos y resultados de aprendizaje

Terminología acordada: **Objetivos generales** del curso (por bloque Caso 1–3) + **Resultados de
aprendizaje específicos** de cada caso (los del Caso 1 ya están; los demás se construyen sobre la
marcha).

**Objetivo general (bloque Caso 2): Conteos y tasas.** Ajustar, interpretar, diagnosticar y
comparar/seleccionar modelos para conteos y tasas —Poisson y binomial negativa, con *offset* para
tasas—: razones de tasas, sobredispersión y exceso de ceros. Reactiva el objetivo de **mixtos y
supervivencia** (riesgos a trozos) y culmina en el de **comunicar y reproducir** (estudio de caso).

Los **RA específicos** del Caso 2 se redactarán más adelante, como en el Caso 1.

## 3. Pregunta vertebradora y narrativa

Título: **«¿Cuántas veces y a qué ritmo?»**. Contexto nuevo (no clínico): una **aseguradora** y su
**siniestralidad**. El desenlace es un evento **repetible**: el nº de **siniestros** por póliza a lo
largo de su **tiempo de cobertura** (exposición). Esto abre de forma natural: la **tasa** con
*offset* (Poisson), la **sobredispersión**, el **exceso de ceros** (pólizas sin siniestros), la
variación por **agencia/zona** (mixtos) y el nexo con el reloj del riesgo vía **riesgos a trozos**.

## 4. Mapa del caso (propuesta de unidades)

| Unidad | Contenido | Objetivo |
|---|---|---|
| **2.1** · Poisson y log-lineal | Unidad **fundacional**, con la estructura del Caso 1: *Contexto* (recuentos, tasas, tablas de contingencia y **tablas cuadradas / de cambio** —simetría, homogeneidad marginal, movilidad—) · *Modelización y estimación* (Poisson, enlace log, offset; log-lineal e independencia/asociación; estimación como recap) · *Interpretación* (IRR y asociación; equivalencia con logística/multinomial) · *Inferencia y selección* (Wald, LRT, AIC/BIC; nombra CV y regularización → 2.6) · *Bondad de ajuste, diagnóstico y predicción* (deviance/Pearson, residuos, **detección de sobredispersión** → 2.2). | Conteos y tasas |
| **2.2** · Sobredispersión | *Desarrollo exhaustivo:* causas; diagnóstico; **quasi-Poisson**; **binomial negativa** (NB1/NB2); OLRE como puente a mixtos; conexión con los ceros. | Conteos y tasas |
| **2.3** · Exceso de ceros: *hurdle* y *zero-inflated* | Mecanismos de ceros; **ZIP/ZINB** y **hurdle** (`pscl`/`glmmTMB`); interpretación de las dos partes. | Conteos y tasas |
| **2.4** · Conteos agrupados: GLMM de Poisson | Intercepto (y pendiente) aleatorios; varianzas; condicional vs marginal; diagnóstico. Reactiva el hilo de mixtos. | Mixtos (espiral) |
| **2.5** · Del conteo al reloj: riesgos a trozos ≡ Poisson | **Exponencial a trozos** como `glm(poisson)` con `offset(log tiempo-persona)`; nexo con el persona-periodo del Caso 1 y con la **fragilidad**. | Supervivencia (espiral) |
| **2.6** · Selección, validación cruzada y regularización | Recap de selección (AIC/BIC) → **k-fold CV** (*out-of-sample*) → **regularización** (`cv.glmnet`, familia `poisson`), con recuadro que aplica `glmnet(family="binomial")` a la logística del Caso 1. | Conteos y tasas |
| **2.7** · Estudio de caso, exposición y evaluación | Integración; encargo del cliente (aseguradora); checklist; reproducibilidad. | Comunicar y reproducir |

Los **modelos de cambio** (tablas cuadradas) entran en el **Contexto de 2.1** como problemas que
motivan el modelo; su desarrollo detallado queda como sección **opcional/avanzada**.

> **Nota de carga.** El bloque tiene solo 3 semanas de teoría (guía). Con Poisson + log-lineal +
> sobredispersión + ceros + mixtos + trozos + regularización, el material queda **denso**: conviene
> tratarlo como **cuaderno de referencia** (más extenso que las clases) y marcar como
> **opcional/avanzado** lo que no quepa (empezando por los modelos de cambio).

## 5. Hilos en espiral (continuidad con el Caso 1)

- **Mixtos:** de intercepto aleatorio logístico (1.4) → intercepto aleatorio de Poisson (2.4).
- **Supervivencia:** de persona-periodo + cloglog (1.5) → riesgos a trozos ≡ Poisson con offset
  (2.5). Aquí cerramos la otra encarnación GLM de la supervivencia que anunciamos al aplazar Cox:
  la del *ritmo/tasa*. Cox (tiempo continuo, no GLM) sigue reservado al **Caso 3**.

## 6. Datos — dos estrategias, y mi recomendación

Aclaro el discurso (mezclé dos ideas):

- **Estrategia A — un banco (real) por modelo.** Cada unidad usa un dataset real distinto, elegido
  porque exhibe su fenómeno con nitidez. Solo para los ejemplos de teoría. Candidatos clásicos:
  `MASS::Insurance` (offset/tasas), `MASS::quine` (sobredispersión/NB), `pscl::bioChemists` (ceros),
  `MASS::epil` / `lme4::grouseticks` (GLMM), `survival::veteran` (riesgos a trozos), `AER::NMES1988`
  (muchos predictores → regularización).
- **Estrategia B — una base simulada integradora.** Un **único dataset simulado** (la cartera de
  seguros), con **verdad conocida**, que contiene *todas* las patologías y sirve para los ejemplos de
  teoría **y** para el estudio de caso (reparto por equipos + validación contra el DGP).

**Recomendación (dado que te gusta seguros para todo): Estrategia B como columna vertebral.**
Simulamos una cartera de pólizas (`R/dgp_conteos.R`) diseñada para que, sobre el mismo contexto,
aparezcan: la **tasa** con exposición (*offset*), la **sobredispersión** (mezcla Poisson–Gamma → NB),
el **exceso de ceros** (pólizas «sin riesgo»), el **agrupamiento** por agencia/zona (efecto
aleatorio) y un **proceso de tiempo-a-evento** para los riesgos a trozos. Es lo mismo que hicimos en
el Caso 1 con la cohorte, ahora en seguros: una sola historia, con verdad conocida.

Y **opcionalmente**, en una o dos unidades, un **dataset real clásico** como contraste de
autenticidad (p. ej. `MASS::quine` para sobredispersión, o `AER::NMES1988` para la regularización con
muchos predictores). Eso es la Estrategia A en dosis pequeñas, no la vía principal.

En resumen: **sí, hay que simular la base de seguros** —es la única forma de cubrir todos los
contenidos con verdad conocida en un mismo contexto— y el «un banco por modelo» queda como
complemento opcional con datos reales.

## 7. Estudio de caso (2.6) — contexto nuevo (propuesta)

Recojo tu punto 3: **cambiamos de contexto** (ya no OsteoRed/clínico). Propongo un contexto de
**conteos con exposición** que dé juego a las cinco patologías. Recomendado: una **aseguradora** y su
**siniestralidad**.

Cliente: una aseguradora con una cartera de pólizas seguidas un tiempo. De cada póliza registra el
**nº de siniestros**, el **tiempo de cobertura** (exposición → *offset*), rasgos del asegurado y la
**agencia/zona** (agrupamiento). Preguntas del encargo:

1. ¿A qué **tasa** se producen siniestros y qué factores la elevan? (Poisson + offset + IRR → 2.1)
2. Unas pólizas acumulan muchos siniestros y la mayoría ninguno: ¿capta el modelo esa dispersión?
   (sobredispersión / NB → 2.2)
3. ¿Hay pólizas «sin riesgo» o simplemente sin siniestros aún? (exceso de ceros → 2.3)
4. ¿La siniestralidad **varía por agencia/zona**? (GLMM de Poisson → 2.4)
5. ¿Cómo evoluciona el riesgo de siniestro **a lo largo del tiempo de cobertura**? (riesgos a trozos
   ≡ Poisson → 2.5)

Alternativas de contexto: seguridad laboral (incidentes por trabajador-año y planta) o mantenimiento
de flota (averías por vehículo-mes y zona). Formato idéntico a 1.6 (informe técnico, el estudiante
elige y justifica el modelo, checklist y control de calidad contra el DGP).

## 8. Recomendación sobre regularización (tu punto 2)

**Recomiendo incluirla, y hacerlo en el Caso 2, aplicada a Poisson**, por una razón técnica decisiva:
`glmnet` tiene familia nativa para `poisson` (y binomial, gaussian, cox, multinomial) **pero no para
Gamma**, que es la respuesta central del Caso 3. Es decir, el sitio donde la regularización *funciona
de verdad con `glmnet`* es un modelo de conteos, no la Gamma. Además, la **validación cruzada**
encaja como continuación natural del hilo de evaluación del Caso 1 (allí el AUC/accuracy eran
*in-sample*; la CV es su versión *out-of-sample*), y los datasets de conteos con muchos predictores
(p. ej. `NMES1988`) hacen que el **lasso** tenga sentido como selección de variables.

Concreción: una unidad **2.x · Selección, validación cruzada y regularización** (recap AIC/BIC del
Caso 1 → k-fold CV → ridge/lasso/elastic-net con `cv.glmnet`, familia `poisson`). En el **Caso 3**,
la regularización quedaría solo como **nota conceptual** para la Gamma (glmnet no aplica; alternativas
como `mpath`/`penalized`), evitando forzarla donde la herramienta no llega.

*Implicación:* esto adelanta al Caso 2 lo que la guía §5 situaba en el Caso 3 (semana 11). Conviene
reflejarlo en la guía. Coste: el Caso 2 (3 semanas) queda algo más cargado; se puede aligerar tratando
la regularización de forma aplicada y breve.

## 9. Decisiones ya cerradas (según tus aclaraciones)

- **Terminología:** Objetivos generales (curso) + RA específicos por caso. ✔
- **Carpeta:** renombrada a `caso2`. ✔
- **Stub previo:** eliminado. ✔
- **Contexto:** nuevo (no OsteoRed) — propuesta: aseguradora/siniestralidad (§7).
- **Datos:** un banco por modelo (§6) + cohorte simulada para el estudio de caso.

Quedan por confirmar: (a) la numeración/redacción de los RA específicos del Caso 2 (más adelante);
(b) el visto bueno a la unidad de regularización (§8); (c) el contexto definitivo del cliente (§7).

## 10. Esqueleto creado (ya en el sitio)

Esqueleto inicial (parent + unidades) con encabezados, callouts «Sobre esta unidad» y marcadores
`TODO`, y **ya enlazado en la landing y el navbar** (pestaña «Caso 2 · Conteos») para que puedas
navegarlo:

- `caso2_conteos.qmd` (documento madre) — añadido a `_quarto.yml` (render + navbar) e `index.qmd`.
- `_unidad_2_1.qmd` … `_unidad_2_6.qmd` (partials).
