# =============================================================================
# Caso 3 · Unidad 3.4 — 4 · Efectos aleatorios y credibilidad
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_4.qmd.
# Cada bloque va precedido de su LABEL y de la ruta de encabezados
# (sección > subsección > apartado) en la que aparece dentro del documento.
#
# GENERADO AUTOMÁTICAMENTE por _scripts/generar_scripts_unidades.R:
# no editar a mano; los cambios se pierden al regenerar. Edita el .qmd.
#
# EJECUCIÓN: autónomo. Guárdalo donde quieras y ejecútalo; los datos y los
# ficheros del proceso generador se descargan del repositorio del curso.
# =============================================================================

# --- Datos y funciones del curso, servidos desde GitHub ----------------------
# El script es autónomo: no hace falta clonar el repositorio ni abrir GLM.Rproj,
# y no depende de en qué carpeta del ordenador esté guardado. Solo necesita
# conexión a internet. GLM_REF es la rama o etiqueta del repositorio que se lee.
GLM_REPO <- "https://raw.githubusercontent.com/asunmayoral/GLM"
GLM_REF  <- "master"

#' URL de un fichero del repositorio, a partir de su ruta dentro del proyecto.
url_glm <- function(ruta) paste(GLM_REPO, GLM_REF, ruta, sep = "/")

#' Lee un .rds del repositorio. gzcon() descomprime al vuelo lo que saveRDS()
#' comprimió: sin él, readRDS() no reconoce el flujo que llega por http.
leer_datos_glm <- function(ruta) {
  con <- gzcon(url(url_glm(ruta), open = "rb"))
  on.exit(close(con))
  readRDS(con)
}

# --- Reproducibilidad (ver index.qmd §10) -----------------------------------
# Paquetes que usa esta unidad, con la versión con la que se preparó el
# material. Si te falta alguno el script lo dice ahora, en vez de fallar a
# mitad de un ajuste; si tu versión difiere, avisa y sigue.
PROBADO_R <- "R 4.6.0 (2026-04-24) · x86_64-apple-darwin20 · medido el 2026-09-07"
PAQUETES  <- c(
  DHARMa = "0.5.0", MASS = "7.3.65", broom = "1.0.13",
  broom.mixed = "0.2.9.7", car = "3.1.5", dplyr = "1.2.1", emmeans = "2.0.3",
  glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  marginaleffects = "0.32.0", patchwork = "1.3.2", performance = "0.17.0",
  rsample = "1.3.2", sessioninfo = "1.2.4", tibble = "3.3.1", tidyr = "1.3.2",
  tidyverse = "2.0.0")
message("Material preparado con ", PROBADO_R)

.falta <- names(PAQUETES)[!vapply(names(PAQUETES), requireNamespace, logical(1), quietly = TRUE)]
if (length(.falta))
  stop("Faltan paquetes: ", paste(.falta, collapse = ", "),
       ". Instálalos con install.packages() y vuelve a ejecutar.")

.instalada <- vapply(names(PAQUETES), function(p) as.character(packageVersion(p)), character(1))
.otra <- names(PAQUETES)[.instalada != PAQUETES]
if (length(.otra))
  message("Versiones distintas a las probadas:\n  ",
          paste(sprintf("%s: tienes %s, probado %s", .otra, .instalada[.otra], PAQUETES[.otra]),
                collapse = "\n  "))
rm(.falta, .instalada, .otra)

# --- Preámbulo del caso (librerías y datos, como en el documento) ------------
# Núcleo del Caso 3 (respuestas continuas positivas + mixtos).
library(MASS)            # se carga ANTES que tidyverse para que dplyr::select() no quede enmascarada
library(tidyverse)       # manipulación, visualización y descriptivos
library(broom)           # resultados ordenados de glm
library(broom.mixed)     # tidy() para modelos mixtos (glmmTMB, lme4)
library(patchwork)       # combinación de gráficos

library(car)             # contrastes, VIF y diagnóstico
library(DHARMa)          # residuos simulados para GLM/GLMM
library(performance)     # diagnóstico y comparación de modelos
library(marginaleffects) # efectos, contrastes y predicciones ajustadas
library(emmeans)         # medias marginales y comparaciones

library(lme4)            # lmer y glmer (mixtos)
library(glmmTMB)         # GLMM Gamma, y familia Tweedie (3.4)
library(glmnet)          # regularización (en 3.7, como límite conceptual en Tweedie)
library(rsample)         # validación cruzada AGRUPADA (group_vfold_cv por máquina)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso3/R/dgp_averias.R"))   # funciones del proceso generador
banco <- leer_datos_glm("caso3/datos/banco_averias_20252026.rds")   # banco de averías del curso

glimpse(banco$averias)

# -----------------------------------------------------------------------------
# [u34-datos]
#   4 · Efectos aleatorios y credibilidad
#     > 4.1 Contexto: por qué las averías de una máquina no son independientes
# -----------------------------------------------------------------------------
c(averias        = nrow(base),
  maquinas       = dplyr::n_distinct(base$id_maquina),
  media_por_maq  = round(nrow(base) / dplyr::n_distinct(base$id_maquina), 2),
  max_por_maq    = max(table(base$id_maquina)))

# -----------------------------------------------------------------------------
# [tbl-u34-icc-crudo]
#   4 · Efectos aleatorios y credibilidad
#     > 4.1 Contexto: por qué las averías de una máquina no son independientes
# -----------------------------------------------------------------------------
base |>
  dplyr::mutate(lc = log(coste_euros)) |>
  dplyr::group_by(id_maquina) |>
  dplyr::mutate(media_maq = mean(lc)) |>
  dplyr::ungroup() |>
  dplyr::summarise(sd_total  = round(sd(lc), 3),
                   sd_entre  = round(sd(media_maq), 3),
                   sd_dentro = round(sd(lc - media_maq), 3))

# -----------------------------------------------------------------------------
# [fig-u34-trayectorias]
#   4 · Efectos aleatorios y credibilidad
#     > 4.1 Contexto: por qué las averías de una máquina no son independientes
# -----------------------------------------------------------------------------
set.seed(1)
frecuentes <- names(which(table(base$id_maquina) >= 5))
muestra    <- sample(frecuentes, 24)

base |>
  dplyr::filter(id_maquina %in% muestra) |>
  ggplot(aes(antiguedad_anios, coste_euros, group = id_maquina, colour = id_maquina)) +
  geom_line(alpha = 0.5) + geom_point(alpha = 0.6, size = 0.9) +
  scale_y_log10() +
  labs(x = "edad de la máquina (años)", y = "coste de la avería (€, escala log)") +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# [u34-ajuste]
#   4 · Efectos aleatorios y credibilidad > 4.2 El modelo mixto
# -----------------------------------------------------------------------------
base <- dplyr::mutate(base, edad_c = antiguedad_anios - mean(antiguedad_anios))

g_int <- glmmTMB::glmmTMB(
  coste_euros ~ tipo_averia + criticidad * fase_proceso + edad_c + carga + I(carga^2) +
    fabricante + (1 | id_maquina),
  family = Gamma(link = "log"), data = base)

summary(g_int)

# -----------------------------------------------------------------------------
# [u34-cifras]
#   4 · Efectos aleatorios y credibilidad > 4.3 Interpretación
# -----------------------------------------------------------------------------
# Componentes del modelo y cifras en línea de esta sección
sd_u_int <- sqrt(glmmTMB::VarCorr(g_int)$cond$id_maquina[1, 1])
icc_int  <- performance::icc(g_int)

# -----------------------------------------------------------------------------
# [tbl-u34-fijos]
#   4 · Efectos aleatorios y credibilidad
#     > 4.3 Interpretación
#       > Los efectos fijos
# -----------------------------------------------------------------------------
broom.mixed::tidy(g_int, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  dplyr::transmute(term,
                   `exp(beta)` = round(estimate, 3),
                   ic95 = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]"),
                   p = signif(p.value, 3))

# -----------------------------------------------------------------------------
# [u34-icc]
#   4 · Efectos aleatorios y credibilidad
#     > 4.3 Interpretación
#       > El componente de varianza
# -----------------------------------------------------------------------------
icc_int

# -----------------------------------------------------------------------------
# [fig-u34-encogimiento]
#   4 · Efectos aleatorios y credibilidad
#     > 4.3 Interpretación
#       > Los efectos de cada máquina: encogimiento y credibilidad
# -----------------------------------------------------------------------------
eta_fijo <- predict(g_int, type = "link", re.form = NA)
desv <- base |>
  dplyr::mutate(r = log(coste_euros) - eta_fijo) |>
  dplyr::group_by(id_maquina) |>
  dplyr::summarise(desv_media = mean(r), n = dplyr::n(), .groups = "drop")
blup_sev <- glmmTMB::ranef(g_int)$cond$id_maquina
desv$blup <- blup_sev[desv$id_maquina, "(Intercept)"]

ggplot(desv, aes(desv_media, blup, colour = n)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(alpha = 0.7) +
  scale_colour_gradient(low = "grey80", high = "navy") +
  labs(x = "desviación media observada de la máquina (log)", y = "BLUP de la máquina",
       colour = "nº de averías")

# -----------------------------------------------------------------------------
# [u34-lrt-re]
#   4 · Efectos aleatorios y credibilidad
#     > 4.4 Bondad de ajuste, diagnóstico y selección
#       > ¿Hace falta el efecto aleatorio?
# -----------------------------------------------------------------------------
g_sinre <- update(g_int, . ~ . - (1 | id_maquina))
lrt_re  <- anova(g_sinre, g_int)
lrt_re

# -----------------------------------------------------------------------------
# [tbl-u34-cuarentena]
#   4 · Efectos aleatorios y credibilidad
#     > 4.4 Bondad de ajuste, diagnóstico y selección
#       > Las cuentas pendientes de la 3.1
# -----------------------------------------------------------------------------
g_sinfab   <- update(g_int, . ~ . - fabricante)
g_sincarga <- update(g_sinfab, . ~ . - I(carga^2))
cuarentena <- tibble::tibble(
  termino = c("fabricante", "carga^2"),
  gl      = c(anova(g_sinfab, g_int)$"Chi Df"[2], anova(g_sincarga, g_sinfab)$"Chi Df"[2]),
  p       = c(anova(g_sinfab, g_int)$"Pr(>Chisq)"[2], anova(g_sincarga, g_sinfab)$"Pr(>Chisq)"[2]))
cuarentena |> dplyr::mutate(p = signif(p, 2))

# -----------------------------------------------------------------------------
# [u34-phi]
#   4 · Efectos aleatorios y credibilidad
#     > 4.4 Bondad de ajuste, diagnóstico y selección
#       > Las cuentas pendientes de la 3.1
# -----------------------------------------------------------------------------
c(phi_glm_3.1 = summary(m_gamma)$dispersion,
  phi_glmm    = sigma(g_sincarga)^2)       # en la Gamma de glmmTMB, sigma^2 = phi

# -----------------------------------------------------------------------------
# [u34-dharma-int]
#   4 · Efectos aleatorios y credibilidad
#     > 4.4 Bondad de ajuste, diagnóstico y selección
#       > El diagnóstico, y la pendiente aleatoria
# -----------------------------------------------------------------------------
sim_int <- DHARMa::simulateResiduals(g_sincarga, n = 1000)
c(uniformidad = DHARMa::testUniformity(sim_int, plot = FALSE)$p.value,
  dispersion  = DHARMa::testDispersion(sim_int, plot = FALSE)$p.value) |> signif(2)

# -----------------------------------------------------------------------------
# [u34-lrt-pendiente]
#   4 · Efectos aleatorios y credibilidad
#     > 4.4 Bondad de ajuste, diagnóstico y selección
#       > El diagnóstico, y la pendiente aleatoria
# -----------------------------------------------------------------------------
g_pend   <- update(g_sincarga, . ~ . - (1 | id_maquina) + (1 | id_maquina) + (0 + edad_c | id_maquina))
lrt_pend <- anova(g_sincarga, g_pend)
lrt_pend

# -----------------------------------------------------------------------------
# [fig-u34-dharma]
#   4 · Efectos aleatorios y credibilidad
#     > 4.4 Bondad de ajuste, diagnóstico y selección
#       > El diagnóstico, y la pendiente aleatoria
# -----------------------------------------------------------------------------
sim_pend <- DHARMa::simulateResiduals(g_pend, n = 1000)
plot(sim_pend)

# -----------------------------------------------------------------------------
# [tbl-u34-dharma]
#   4 · Efectos aleatorios y credibilidad
#     > 4.4 Bondad de ajuste, diagnóstico y selección
#       > El diagnóstico, y la pendiente aleatoria
# -----------------------------------------------------------------------------
tests_pend <- c(uniformidad = DHARMa::testUniformity(sim_pend, plot = FALSE)$p.value,
                dispersion  = DHARMa::testDispersion(sim_pend, plot = FALSE)$p.value,
                atipicos    = DHARMa::testOutliers(sim_pend, plot = FALSE)$p.value)
cuant_pend <- quantile(residuals(sim_pend), c(0.05, 0.25, 0.5, 0.75, 0.95))
tibble::tibble(contraste = names(tests_pend), p = signif(tests_pend, 2))
round(rbind(residuos = cuant_pend, uniforme = c(0.05, 0.25, 0.5, 0.75, 0.95)), 3)

# -----------------------------------------------------------------------------
# [u34-r2]
#   4 · Efectos aleatorios y credibilidad
#     > 4.4 Bondad de ajuste, diagnóstico y selección
#       > El diagnóstico, y la pendiente aleatoria
# -----------------------------------------------------------------------------
r2_pend <- performance::r2_nakagawa(g_pend)
r2_pend

# -----------------------------------------------------------------------------
# [u34-tw-mixto]
#   4 · Efectos aleatorios y credibilidad
#     > 4.5 El historial en el coste anual
# -----------------------------------------------------------------------------
t_mix <- update(m_tw, . ~ . + (1 | id_maquina))
lrt_tw <- anova(m_tw, t_mix)
lrt_tw

# -----------------------------------------------------------------------------
# [u34-tw-fab]
#   4 · Efectos aleatorios y credibilidad
#     > 4.5 El historial en el coste anual
# -----------------------------------------------------------------------------
t_mix2 <- update(t_mix, . ~ . - fabricante)
lrt_tw_fab <- anova(t_mix2, t_mix)
lrt_tw_fab

# -----------------------------------------------------------------------------
# [fig-u34-cond-marg]
#   4 · Efectos aleatorios y credibilidad
#     > 4.6 Modelo validado y predicción
#       > Predicción marginal y condicional
# -----------------------------------------------------------------------------
media_edad <- mean(base$antiguedad_anios)
rej_edad <- tidyr::expand_grid(
  edad_c       = seq(min(base$edad_c), max(base$edad_c), length.out = 40),
  tipo_averia  = "Mecanica", criticidad = "Auxiliar",
  fase_proceso = "fase1",    carga      = mean(base$carga))

marg <- rej_edad
marg$pred <- predict(g_pend, newdata = rej_edad, type = "response", re.form = NA)

set.seed(7)
seis <- sample(unique(base$id_maquina), 6)
cond <- tidyr::expand_grid(rej_edad, id_maquina = seis)
cond$pred <- predict(g_pend, newdata = cond, type = "response", re.form = NULL)

ggplot(marg, aes(edad_c + media_edad, pred)) +
  geom_line(data = cond, aes(colour = factor(id_maquina)), alpha = 0.7) +
  geom_line(linewidth = 1.2) +
  labs(x = "edad de la máquina (años)", y = "coste medio predicho (€)", colour = "máquina")

# -----------------------------------------------------------------------------
# [fig-u34-caterpillar]
#   4 · Efectos aleatorios y credibilidad
#     > 4.6 Modelo validado y predicción
#       > ¿Qué máquinas son crónicamente caras?
# -----------------------------------------------------------------------------
re_tw <- as.data.frame(glmmTMB::ranef(t_mix2, condVar = TRUE))
re_tw <- re_tw |>
  dplyr::filter(grpvar == "id_maquina") |>
  dplyr::transmute(id_maquina = as.character(grp), blup = condval, ee = condsd,
                   factor = exp(blup), inf = exp(blup - 1.96 * ee), sup = exp(blup + 1.96 * ee),
                   cara = inf > 1) |>
  dplyr::arrange(blup) |>
  dplyr::mutate(orden = dplyr::row_number())

ggplot(re_tw, aes(orden, factor, colour = cara)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_errorbar(aes(ymin = inf, ymax = sup), width = 0, alpha = 0.4) +
  geom_point(size = 0.8) +
  scale_y_log10() +
  scale_colour_manual(values = c("grey50", "firebrick"), guide = "none") +
  labs(x = "máquinas, ordenadas por su efecto", y = "factor de gasto propio, exp(BLUP)")

# -----------------------------------------------------------------------------
# [tbl-u34-caras]
#   4 · Efectos aleatorios y credibilidad
#     > 4.6 Modelo validado y predicción
#       > ¿Qué máquinas son crónicamente caras?
# -----------------------------------------------------------------------------
caras <- re_tw |>
  dplyr::filter(cara) |>
  dplyr::left_join(dplyr::summarise(pn, plan = dplyr::first(plan_mantenimiento),
                                    intervalo = dplyr::first(intervalo_mant),
                                    anios = round(sum(exposicion), 1), .by = id_maquina),
                   by = "id_maquina") |>
  dplyr::arrange(dplyr::desc(factor))
caras |> dplyr::transmute(id_maquina, factor = round(factor, 2),
                          ic95 = paste0("[", round(inf, 2), ", ", round(sup, 2), "]"),
                          plan, intervalo, anios)

# -----------------------------------------------------------------------------
# [tbl-u34-clase]
#   4 · Efectos aleatorios y credibilidad
#     > 4.6 Modelo validado y predicción
#       > El historial como tarifa
# -----------------------------------------------------------------------------
hist_pn <- pn |>
  dplyr::arrange(id_maquina, anio) |>
  dplyr::mutate(n_prev = dplyr::lag(n_averias), .by = id_maquina) |>
  dplyr::filter(!is.na(n_prev)) |>                       # el primer año no tiene historial
  dplyr::mutate(clase = factor(pmin(n_prev, 2), labels = c("0", "1", "2+")))

t_clase <- glmmTMB::glmmTMB(coste_total ~ edad + carga + proceso + criticidad + plan_mantenimiento +
                              clase + offset(log(exposicion)),
                            family = glmmTMB::tweedie, data = hist_pn)

blup_clase <- hist_pn |>
  dplyr::left_join(dplyr::select(re_tw, id_maquina, factor), by = "id_maquina") |>
  dplyr::summarise(factor_blup_medio = mean(factor), n = dplyr::n(), .by = clase)

broom.mixed::tidy(t_clase, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  dplyr::filter(grepl("^clase", term)) |>
  dplyr::transmute(clase = sub("clase", "", term), factor_tarifa = round(estimate, 2),
                   ic95 = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]")) |>
  dplyr::right_join(blup_clase, by = "clase") |>
  dplyr::arrange(clase) |>
  dplyr::mutate(factor_tarifa = dplyr::coalesce(factor_tarifa, 1),          # la clase 0 es la referencia
                ic95 = dplyr::coalesce(ic95, "referencia"),
                factor_blup_medio = round(factor_blup_medio, 2))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
