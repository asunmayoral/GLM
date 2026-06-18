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
