# Notas pendientes · Caso 1

## Respuestas categóricas de la cohorte sin modelar (decisión pospuesta)
Al usar `raterisk` de GLOW como ejemplo **nominal** (3.3) y **ordinal** (3.4), las dos respuestas
categóricas de la cohorte simulada quedan **generadas y documentadas, pero sin modelar** en el texto:

- `sever_ord` — severidad ordinal (Leve < Moderado < Grave), cumulative logit + intercepto aleatorio
  por clínica. Verdad conocida en `attr(cohorte, "verdad")$ordinal`.
- `clase_nom` — nominal A/B/C, logit de categoría base, solo efectos fijos. Verdad en
  `...$nominal`.

**Decisión (12-jun-2026):** NO recortarlas del DGP / Presentación. Se **reservan como material para
plantear problemas al estudiante en el estudio de caso final (Unidad 1.6)** —ambas tienen verdad
conocida, así que sirven para ejercicios de ajuste/interpretación/recuperación de la verdad
(ordinal mixto con `clmm`, nominal con `multinom`/`vglm`)—.

Pendiente: al redactar 1.6, incorporar uno o dos enunciados que las usen.

## Validación cruzada y regularización → Caso 2 (Poisson), para completar FA5
El RA **FA5** ("comparar y seleccionar modelos mediante deviance, AIC/BIC, validación cruzada y
regularización") está cubierto a medias: deviance + AIC/BIC ya se trabajan en el Caso 1 (escalera de
2.5, `dredge`/`stepAIC`), pero **validación cruzada y regularización aún no**.

**Decisión (acordada):** se incorporarán en el **Caso 2 (conteos: Poisson/NB)**, **después de
supervivencia**. Plan:

- Una **sección propia de selección de modelos** que (a) recapitule AIC/BIC enlazando con el Caso 1,
  (b) introduzca **k-fold CV** como evaluación predictiva —enganchando con que el AUC/accuracy de la
  logística (Caso 1) eran *in-sample*; la CV es su versión *out-of-sample*—, y (c) cierre con
  **regularización (ridge/lasso/elastic net)** con $\lambda$ ajustado por **`cv.glmnet`** (CV y
  regularización van juntas).
- Aplicarla al **modelo Poisson/conteos** (`glmnet` soporta `family = "poisson"`).
- Elegir el **dataset del Caso 2 con suficientes predictores** para que el lasso tenga sentido
  (selección de variables).
- **Caveat:** `glmnet` **no** tiene familia **Gamma**; para Gamma, regularización vía alternativas
  (`mpath`/`penalized`) o dejarla con deviance/AIC sin penalización. Ilustrar CV+regularización con
  el Poisson.
