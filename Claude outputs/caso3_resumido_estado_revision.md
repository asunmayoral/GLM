# Caso 3 resumido · Estado de la revisión (20-11-2026)

Documento de traspaso para continuar en Claude Code. Planteamiento original en la Doc
«Caso 3 resumido · Planteamiento» (https://claude.ai/artifact/XzfcDGs3oiycrNkMejCsev).
Este fichero lo **completa y corrige** con lo verificado sobre el repositorio local.

Repositorio: `~/Claude/Projects/GLM` (último commit `b05aeae`, 11-11-2026; árbol limpio al empezar).

---

## 1. Hecho

| Paso | Estado | Detalle |
|---|---|---|
| Copia de archivo | ✅ | `_archivo/caso3_completo/` = copia exacta de `caso3/` (`diff -rq` sin diferencias, 2.8 MB) |
| Copia de la caché | ✅ | `_archivo/freeze_caso3_completo/` = copia exacta de `_freeze/caso3/` (5.5 MB) |
| `index.qmd` | ✅ | Cronograma, tarjetas de casos, tabla de distribuciones y lista de unidades actualizados (ver §4) |
| Commit / push | ❌ pendiente | `_archivo/` e `index.qmd` sin commitear. **Decisión: `_archivo/` SÍ se sube a GitHub** (respaldo) |

Nada más se ha tocado: `caso3/` sigue siendo la versión completa (3.1–3.8).

## 2. Decisiones tomadas por la autora

1. `caso3/` pasa a ser la versión resumida (se mantienen las rutas; así no se rompen enlaces). La
   completa queda congelada en `_archivo/caso3_completo/`. Se trabaja **sobre los ficheros existentes**
   (renombrados con `git mv`), no con ficheros nuevos.
2. `_archivo/` se versiona en GitHub.
3. **La validación contra el DGP de la §6.9 (Cox) NO se rescata** para el resumen.
   ⚠️ [PENDIENTE DE ACLARAR] si esto vale solo para Cox o para **todas** las unidades resumidas
   (el planteamiento original decía que cada unidad conservaba la suya; las normas del proyecto
   también lo piden). Preguntar antes de recortar.
4. Calendario real: semana 10 se terminó el Caso 2 (2.5 y 2.6); semana 11 trabajo en el aula;
   semana 12 exposiciones del Caso 2. El Caso 3 empieza el **miércoles 25-11 (semana 12)** y
   termina en la 15: **7 sesiones** (1 en la sem. 12, 2 en cada una de las 13, 14 y 15).
   Sin fechas en el cronograma.

## 3. Estructura resumida (7 unidades)

| Sesión | Sem. | Unidad resumida | Fichero (tras renombrar) | Procede de |
|:--:|:--:|---|---|---|
| 1 | 12 | 3.1 GLM Gamma | `_unidad_3_1.qmd` | 3.1 §1.1–1.5 enteras |
| 2 | 13 | 3.2 Elegir familia y escala | `_unidad_3_2.qmd` | 3.2 §2.1–2.3 y §2.5 (sale §2.4) |
| 2 | 13 | 3.3 GLMM Gamma | `_unidad_3_3.qmd` | 3.3 §3.1–3.5 enteras |
| 3 | 13 | 3.4 AFT: del coste al tiempo | `_unidad_3_5.qmd` → `_unidad_3_4.qmd` | 3.5 §5.1–5.6 |
| 4 | 14 | 3.5 Cox: del tiempo al riesgo | `_unidad_3_6.qmd` → `_unidad_3_5.qmd` | 3.6 §6.1–6.7 |
| 5 | 14 | 3.6 Las tres lentes | `_unidad_3_7.qmd` → `_unidad_3_6.qmd` | 3.6 §6.8 (paso previo) + 3.7 §7.1 + §7.2 breve |
| 6–7 | 15 | 3.7 Estudio de caso (aula + evaluación) | `_unidad_3_8.qmd` → `_unidad_3_7.qmd` | 3.8 replanteada |

Sale entera: `_unidad_3_4.qmd` (Tweedie) y `scripts/unidad_3_4.R`.

Renumeración de prefijos: `u35→u34`, `u36→u35`, `u37→u36`, `u38→u37` (chunks, `fig-`, `tbl-`,
`#sec-`) y encabezados `5.x→4.x`, `6.x→5.x`, `7.x→6.x`, `8.x→7.x`. **Orden del script**: de menor a
mayor (`u35→u34` primero, luego `u36→u35`…), para no pisar etiquetas. Tras renombrar, la 3.6
resumida recibe contenido de la antigua 3.6 (que ya será `u35`): esas etiquetas deben pasar a `u36`.

## 4. Cambios aplicados en `index.qmd`

- Tarjetas: «Caso 2 · Semanas 6–12», «Caso 3 · Semanas 12–15»; descripción del Caso 3 sin Tweedie
  (ahora: Gamma e IG, GLMM Gamma, AFT, Cox como frontera, tres lentes).
- Tabla «¿Qué mides?»: quitado «Tweedie»; «¿Cuándo ocurre?» remite a `1.5 · 2.6 · 3.4–3.5`.
- Cronograma, semanas 8–15:
  - 8–9 Caso 2 · Exceso de ceros · 2.4
  - 10 Caso 2 · Conteos agrupados; riesgos a trozos; fragilidad · 2.5, 2.6
  - 11 Caso 2 · Estudio de caso: trabajo en el aula · 2.7
  - 12 Caso 2 · Caso 3 · Exposiciones del Caso 2 + Gamma · 2.7 · 3.1
  - 13 Caso 3 · Elegir familia; GLMM Gamma; AFT · 3.2, 3.3, 3.4
  - 14 Caso 3 · Cox como frontera; tres lentes · 3.5, 3.6
  - 15 Caso 3 · Estudio de caso: aula, evaluación y cierre · 3.7
  - ⚠️ [VERIFICAR con la autora] Fusioné las semanas 8 y 9 en «8–9 · 2.4» porque me dijo que
    2.5 y 2.6 se vieron en la 10; no sé qué se hizo exactamente en la 9.
- Lista de unidades (antigua l. 468): 3.1 … 3.7 con los títulos nuevos.
- No tocado: l. 164 («tras cada bloque se destina una semana para las defensas»), que ya no
  describe exactamente el Caso 3. Revisar.
- Nota: `index.qmd` ya anuncia las unidades nuevas aunque `caso3/` aún no esté recortado.

## 5. Verificaciones hechas sobre el repositorio (todas ✅ salvo indicación)

- Lista explícita `render:` en `_quarto.yml` (l. 8–14): `_archivo/` no se renderiza. Además Quarto
  ignora ficheros/carpetas con prefijo `_` (confirmado en fuentes secundarias; docs oficiales no
  accesibles desde la sesión).
- Barra lateral `_quarto.yml` l. 93–107 enlaza `#sec-u31` … `#sec-u38` → actualizar.
- `recursos.qmd` l. 204 («3.1 a 3.8») y l. 226 → actualizar.
- Casos 1 y 2 no referencian nada del Caso 3.
- `include` del maestro en l. 314–324.
- Líneas citadas en el planteamiento, todas correctas: 3.1 l. 561, 3.2 l. 114, 3.3 l. 176,
  3.7 l. 287 y l. 553–556, errata 3.6 l. 1363.
- `sg` creado en 3.4 l. 325 y 3.6 l. 832 → queda solo el de §6.8.
- `cox_mes_opt_frag` definido en 3.6 l. 1078 y 3.7 l. 365 → dejar una sola definición.
- `m_disp`, `base_disp` (3.2 §2.4) solo se usan dentro de §2.4 → se puede quitar limpio.

## 6. Correcciones al planteamiento

1. **§6.8, contradicción.** La lista «Qué sale» dice «desde *Dos arbitrariedades que conviene
   quitar*», pero `prot_mant`, `cox_mes_opt`, `cox_mes_opt_frag` y `u36-comparar` están **dentro** de
   esa sección (l. 1010–1160). Corte correcto: se conserva hasta `u36-comparar` / `u36-comparar-aic`
   incluidos; **sale desde «La segunda arbitrariedad: dejar de trocear, con `tt()`» (l. 1162)**
   hasta el final del callout `tt()` (l. 1314–1339).
2. **Referencias huérfanas no listadas en el planteamiento:**
   - 3.6 l. 1104, 1273, 1662 → `#sec-u36-reglas` (en §6.9, sale).
   - 3.6 l. 64 (cabecera) menciona `#sec-u36-cierre`.
   - 3.6 l. 1575/1588 `tbl-u36-dgp` está en §6.9 (sale); se cita desde 3.7 l. 386 (comentario).
   - 3.2 l. 360 → `#sec-u32-dispersion` (sale).
   - 3.7 l. 40 anuncia la §7.2 completa; 3.7 l. 291 callout «¿Y por qué no el `tt()` de 6.8…?»
     pierde sentido sin `tt()`.
   - 3.8 l. 88, 150, 227, 233, 415 → `#sec-u34` (se reescribe la 3.8 entera).
   - Maestro l. 125–130 (caja de herramientas): `statmod::tweedie`, `#sec-u34-mapa`, etc. (se
     reescribe el mapa).
3. **`_freeze` en CI.** `_scripts/invalidar_freeze.R` se salta en GitHub Actions (`CI`): allí manda
   el `_freeze/` versionado. Tras borrar `_freeze/caso3/` y renderizar en local, **commitear la
   caché nueva** o la web fallará.
4. Título real de la 3.3: «Efectos Aleatorios y Modelos Mixtos» (no «GLMM Gamma»).
5. En el entorno remoto, git no podía borrar sus `.lock` sin permiso de borrado: en Code esto no
   aplica.

## 7. Próximos pasos (en orden; render del maestro tras cada uno)

1. Commit de `_archivo/` + `index.qmd` (y push).
2. Aclarar el alcance de la validación contra el DGP (§2.3 de este fichero).
3. `git rm caso3/_unidad_3_4.qmd caso3/scripts/unidad_3_4.R`; `git mv` 3_5→3_4, 3_6→3_5,
   3_7→3_6, 3_8→3_7 (y sus scripts); actualizar `include` del maestro.
4. Script de renumeración de prefijos y encabezados; comprobar que todo `@fig-`/`@tbl-`/`#sec-`
   resuelve y que no hay etiquetas duplicadas.
5. Recortes, uno a uno: 3.2 (quitar §2.4 y la remisión de l. 360), 3.5 (quitar §6.8–6.9),
   3.6 (montar el paso previo desde §6.8 hasta `u36-comparar`, una sola `cox_mes_opt_frag`,
   quitar callout `tt()` de l. 291, acortar §7.2 a tabla breve). Retocar las frases Tweedie de
   3.1, 3.2, 3.3 y 3.6 (antigua 3.7). Actualizar cabeceras HTML de cada parcial.
6. Reescribir el mapa del caso en el maestro (objetivos, RA, tabla de unidades, caja de herramientas).
7. Reescribir la 3.7 (estudio de caso: encargo común A1–A3, B1–B3; ver Doc) y los callouts
   «Tarea del estudiante» de cada unidad.
8. Actualizar `_quarto.yml` (barra lateral `#sec-u31`…`#sec-u37`) y `recursos.qmd`; regenerar
   scripts `.R` (`_scripts/generar_scripts_unidades.R`); borrar `_freeze/caso3/`, render completo,
   revisión de cifras y referencias (normas 29–31); commitear `_freeze/caso3/` nuevo.
