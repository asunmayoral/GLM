# =============================================================================
# Caso 3 · Unidad 3.3 — 3 · Efectos Aleatorios y Modelos Mixtos
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_3.qmd.
# Cada bloque va precedido de su LABEL y de la ruta de encabezados
# (sección > subsección > apartado) en la que aparece dentro del documento.
#
# GENERADO AUTOMÁTICAMENTE por _scripts/generar_scripts_unidades.R:
# no editar a mano; los cambios se pierden al regenerar. Edita el .qmd.
#
# EJECUCIÓN: funciona desde CUALQUIER carpeta dentro del proyecto GLM;
# localiza la raíz por _quarto.yml y resuelve solo las rutas de datos.
# =============================================================================

.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")

# --- Preámbulo del caso (librerías y datos, como en el documento) ------------
# Núcleo del Caso 3 (respuestas continuas positivas + mixtos + supervivencia).
# Cada unidad añadirá lo suyo cuando la desarrollemos (p. ej. glmmTMB::tweedie() en 3.6).
library(MASS)            # se carga ANTES que tidyverse para que dplyr::select() no quede enmascarada
library(tidyverse)       # manipulación, visualización y descriptivos
library(broom)           # resultados ordenados de glm y modelos de supervivencia
library(broom.mixed)     # tidy() para modelos mixtos (glmmTMB, lme4)
library(patchwork)       # combinación de gráficos

library(car)             # contrastes, VIF y diagnóstico
library(DHARMa)          # residuos simulados para GLM/GLMM
library(performance)     # diagnóstico y comparación de modelos
library(marginaleffects) # efectos, contrastes y predicciones ajustadas
library(emmeans)         # medias marginales y comparaciones

library(lme4)            # lmer y glmer (mixtos)
library(glmmTMB)         # GLMM Gamma, y familia Tweedie (3.6)
library(glmnet)          # regularización (en 3.6, como límite conceptual en Gamma)
library(rsample)         # validación cruzada AGRUPADA (group_vfold_cv por máquina)

library(survival)        # Kaplan-Meier, Cox y datos start-stop
library(survminer)       # representación gráfica de supervivencia
library(flexsurv)        # modelos paramétricos de supervivencia (AFT)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(file.path(.raiz, "caso3", "R", "dgp_averias.R"))     # define simular_averias() y cargar_averias()
banco <- cargar_averias()      # lee datos/banco_averias_*.rds si existe; si no, simula y lo guarda

glimpse(banco$averias)

# -----------------------------------------------------------------------------
# [u33-datos]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.1 Contexto: por qué las averías de una máquina no son independientes
# -----------------------------------------------------------------------------
c(averias        = nrow(base),
  maquinas       = dplyr::n_distinct(base$id_maquina),
  media_por_maq  = round(nrow(base) / dplyr::n_distinct(base$id_maquina), 2),
  max_por_maq    = max(table(base$id_maquina)))

# -----------------------------------------------------------------------------
# [tbl-u33-icc-crudo]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.1 Contexto: por qué las averías de una máquina no son independientes
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
# [fig-u33-trayectorias]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.1 Contexto: por qué las averías de una máquina no son independientes
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
# [u33-ajuste]
#   3 · Efectos Aleatorios y Modelos Mixtos > 3.2 El modelo mixto Gamma
# -----------------------------------------------------------------------------
base <- dplyr::mutate(base, edad_c = antiguedad_anios - mean(antiguedad_anios))

m_glmm <- glmmTMB::glmmTMB(
  coste_euros ~ tipo_averia + criticidad * fase_proceso + edad_c + carga + fabricante +
    (1 + edad_c | id_maquina),
  family = Gamma(link = "log"), data = base)

summary(m_glmm)

# -----------------------------------------------------------------------------
# [tbl-u33-fijos]
#   3 · Efectos Aleatorios y Modelos Mixtos > 3.3 Interpretación
# -----------------------------------------------------------------------------
broom.mixed::tidy(m_glmm, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  dplyr::transmute(term,
                   `exp(beta)` = round(estimate, 3),
                   conf.low = round(conf.low, 3), conf.high = round(conf.high, 3),
                   p.value = signif(p.value, 3))

# -----------------------------------------------------------------------------
# [tbl-u33-anova-fijos]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.4 Inferencia y selección
#       > Efectos fijos y selección de variables
# -----------------------------------------------------------------------------
car::Anova(m_glmm)

# -----------------------------------------------------------------------------
# [u33-fabricante]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.4 Inferencia y selección
#       > Efectos fijos y selección de variables
# -----------------------------------------------------------------------------
m_sinfab <- update(m_glmm, . ~ . - fabricante)   # el mismo modelo, sin fabricante
anova(m_sinfab, m_glmm)

# -----------------------------------------------------------------------------
# [tbl-u33-varcomp]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.4 Inferencia y selección
#       > Varianzas de los aleatorios
# -----------------------------------------------------------------------------
broom.mixed::tidy(m_sinfab, effects = "ran_pars") |>
  dplyr::transmute(term, estimate = round(estimate, 3))

# -----------------------------------------------------------------------------
# [u33-lrt-pendiente]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.4 Inferencia y selección
#       > Varianzas de los aleatorios
# -----------------------------------------------------------------------------
# partimos de m_sinfab (ya sin fabricante) y cambiamos solo la estructura aleatoria:
m_int    <- update(m_sinfab, . ~ . - (1 + edad_c | id_maquina) + (1 | id_maquina)) # solo interceptación
m_uncorr <- update(m_sinfab, . ~ . - (1 + edad_c | id_maquina) +
                     (1 | id_maquina) + (0 + edad_c | id_maquina))  # interceptacion + pendiente
anova(m_int, m_uncorr)

# -----------------------------------------------------------------------------
# [u33-bootstrap-pendiente]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.4 Inferencia y selección
#       > Varianzas de los aleatorios
# -----------------------------------------------------------------------------
set.seed(1)
lrt_obs <- as.numeric(2 * (logLik(m_uncorr) - logLik(m_int)))    # LRT observado

sim <- simulate(m_int, nsim = 500)             # respuestas bajo H0 (sin pendiente)
lrt_boot <- vapply(sim, function(y) {
  d <- transform(base, coste_euros = y)
  as.numeric(2 * (logLik(update(m_uncorr, data = d)) -
                  logLik(update(m_int,    data = d))))
}, numeric(1))

mean(lrt_boot >= lrt_obs, na.rm = TRUE)         # p-valor bootstrap

# -----------------------------------------------------------------------------
# [u33-lrt-correlacion]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.4 Inferencia y selección
#       > Correlación intercepto–pendiente
# -----------------------------------------------------------------------------
anova(m_uncorr, m_sinfab)

# -----------------------------------------------------------------------------
# [u33-final]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.4 Inferencia y selección
#       > Correlación intercepto–pendiente
# -----------------------------------------------------------------------------
m_final <- m_uncorr

# -----------------------------------------------------------------------------
# [u33-r2]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
performance::r2_nakagawa(m_final)

# -----------------------------------------------------------------------------
# [u33-icc]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
performance::icc(m_final)

# -----------------------------------------------------------------------------
# [fig-u33-dharma]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Diagnóstico: residuos
# -----------------------------------------------------------------------------
sim <- DHARMa::simulateResiduals(m_final, n = 1000)
plot(sim)

# -----------------------------------------------------------------------------
# [u33-testdisp]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Diagnóstico: residuos
# -----------------------------------------------------------------------------
DHARMa::testDispersion(sim, plot = FALSE)

# -----------------------------------------------------------------------------
# [tbl-u33-verdad]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(banco, "verdad")
rp     <- broom.mixed::tidy(m_final, effects = "ran_pars")
phi_glmm <- sigma(m_final)^2   # dispersión del modelo (Var = phi * mu^2)

tibble::tibble(
  cantidad  = c("sd intercepto (fragilidad de máquina)", "sd pendiente (edad)", "dispersión φ"),
  estimado  = round(c(rp$estimate[rp$term == "sd__(Intercept)"],
                      rp$estimate[rp$term == "sd__edad_c"],
                      phi_glmm), 3),
  verdadero = round(c(verdad$sigma_intercepto_coste, verdad$sigma_pend, verdad$phi_coste), 3))

# -----------------------------------------------------------------------------
# [fig-u33-cond-marg]
#   3 · Efectos Aleatorios y Modelos Mixtos
#     > 3.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Predicción: marginal y condicional
# -----------------------------------------------------------------------------
media_edad <- mean(base$antiguedad_anios)
rej <- tidyr::expand_grid(
  edad_c       = seq(min(base$edad_c), max(base$edad_c), length.out = 40),
  tipo_averia  = "Mecanica", criticidad = "Auxiliar",
  fase_proceso = "fase1",    carga      = mean(base$carga))

marg <- rej
marg$pred <- predict(m_final, newdata = rej, type = "response", re.form = NA)

set.seed(7)
seis <- sample(unique(base$id_maquina), 6)
cond <- tidyr::expand_grid(rej, id_maquina = seis)
cond$pred <- predict(m_final, newdata = cond, type = "response", re.form = NULL)

ggplot(marg, aes(edad_c + media_edad, pred)) +
  geom_line(data = cond, aes(colour = factor(id_maquina)), alpha = 0.7) +
  geom_line(linewidth = 1.2) +
  labs(x = "edad de la máquina (años)", y = "coste medio predicho (€)", colour = "máquina")

