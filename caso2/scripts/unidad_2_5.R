# =============================================================================
# Caso 2 · Unidad 2.5 — 5 · Efectos Aleatorios y Modelos Mixtos
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_5.qmd.
# Cada bloque va precedido de su LABEL y de la sección/subsección donde aparece.
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
# Núcleo.
library(broom)
library(tidyverse)
library(MASS)          # glm.nb (base-recommended)
library(pscl)          # hurdle / zeroinfl
library(glmmTMB)       # conteos mixtos / ceros
library(lme4)          # glmer (Poisson)
library(DHARMa); library(performance); library(marginaleffects)
library(survival)      # riesgos a trozos (base-recommended)
library(MuMIn); library(glmnet)   # selección / regularización 
library(vcd)           # mosaicos para tablas de contingencia (2.2)
library(vcdExtra)      # zero-inflated (2.4) y utilidades de tablas (2.2)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(file.path(.raiz, "caso2", "R", "dgp_conteos.R"))            # define simular_cartera(), cargar_cartera(), expandir_poliza_tramo()
cartera <- cargar_cartera("auto")    # lee datos/cartera_auto_*.rds si existe; si no (o si cambió el DGP), simula y lo guarda
glimpse(cartera)

# -----------------------------------------------------------------------------
# [fig-u25-anidamiento]  ·  5.1 Qué es y de dónde viene: el agrupamiento
# -----------------------------------------------------------------------------
tasa_global <- sum(cartera$n_gestiones) / sum(cartera$exposicion)
cartera |>
  dplyr::group_by(area = region, agencia) |>
  dplyr::summarise(tasa = sum(n_gestiones) / sum(exposicion), .groups = "drop") |>
  ggplot(aes(area, tasa, colour = area)) +
  geom_hline(yintercept = tasa_global, linetype = 2, colour = "grey50") +
  stat_summary(fun = mean, geom = "crossbar", width = 0.6, colour = "grey25") +
  geom_jitter(width = 0.15, height = 0, size = 2, alpha = 0.8) +
  labs(x = "región", y = "tasa de gestiones (por unidad de exposición)") +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# [u25-glmm]  ·  5.2 El modelo mixto de Poisson > El modelo y su estimación
# -----------------------------------------------------------------------------
m_pois <- glm(
  n_gestiones ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)),
  family = poisson, data = cartera
)

m_glmm <- glmer(
  n_gestiones ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)) + (1 | region/agencia),
  family = poisson, data = cartera
)
summary(m_glmm)

# -----------------------------------------------------------------------------
# [u25-vc]  ·  5.2 El modelo mixto de Poisson > El modelo y su estimación
# -----------------------------------------------------------------------------
# Desviaciones típicas de cada nivel (escala log).
vc <- as.data.frame(VarCorr(m_glmm))
sd_reg <- vc$sdcor[vc$grp == "region"]
sd_ag  <- vc$sdcor[vc$grp == "agencia:region"]
c(sigma_region = sd_reg, sigma_agencia = sd_ag)

# -----------------------------------------------------------------------------
# [u25-irr]  ·  Recordatorio · IRR (razón de tasas)
# -----------------------------------------------------------------------------
ef <- summary(m_glmm)$coefficients
data.frame(
  termino   = rownames(ef),
  IRR       = exp(ef[, "Estimate"]),
  conf.low  = exp(ef[, "Estimate"] - 1.96 * ef[, "Std. Error"]),
  conf.high = exp(ef[, "Estimate"] + 1.96 * ef[, "Std. Error"])
) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))

# -----------------------------------------------------------------------------
# [u25-icc]  ·  Recordatorio · ICC (correlación intraclase)
# -----------------------------------------------------------------------------
performance::icc(m_glmm, by_group = TRUE)

# -----------------------------------------------------------------------------
# [fig-u25-blups]  ·  Recordatorio · BLUP (predicción del efecto aleatorio)
# -----------------------------------------------------------------------------
re  <- ranef(m_glmm, condVar = TRUE)$`agencia:region`
re_df <- tibble::tibble(
  agencia = rownames(re),
  b  = re[, 1],
  se = sqrt(attr(re, "postVar")[1, 1, ])
) |>
  dplyr::mutate(agencia = forcats::fct_reorder(agencia, b))
ggplot(re_df, aes(b, agencia)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_pointrange(aes(xmin = b - 1.96 * se, xmax = b + 1.96 * se), size = 0.3) +
  labs(x = "desviación del log-ritmo (BLUP)", y = "agencia")

# -----------------------------------------------------------------------------
# [u25-pred]  ·  Recordatorio · Predicción condicional vs marginal
# -----------------------------------------------------------------------------
nd <- cartera |>
  dplyr::slice(1) |>
  dplyr::mutate(exposicion = 1)                       # ventana de exposición completa, misma póliza tipo

c(
  condicional = predict(m_glmm, nd, type = "response", re.form = NULL),  # su agencia
  marginal    = predict(m_glmm, nd, type = "response", re.form = ~0)     # agencia cualquiera
)

# -----------------------------------------------------------------------------
# [u25-drop1]  ·  5.4 Inferencia y selección > Los efectos fijos: contraste y selección
# -----------------------------------------------------------------------------
drop1(m_glmm, test = "Chisq")

# -----------------------------------------------------------------------------
# [u25-aic]  ·  5.4 Inferencia y selección > La estructura aleatoria: ¿hace falta el agrupamiento?
# -----------------------------------------------------------------------------
m_nb <- glm.nb(
  n_gestiones ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)),
  data = cartera
)
performance::compare_performance(
  Poisson = m_pois, NB = m_nb, GLMM = m_glmm,
  metrics = c("AIC", "BIC", "RMSE")
)

# -----------------------------------------------------------------------------
# [fig-u25-dharma]  ·  5.5 Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
res_glmm <- simulateResiduals(m_glmm)
plot(res_glmm)

# -----------------------------------------------------------------------------
# [u25-r2]  ·  5.5 Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
performance::r2(m_glmm)

# -----------------------------------------------------------------------------
# [u25-olre-disp]  ·  5.6 Cierre: el efecto aleatorio a nivel de observación (OLRE) > Por qué modelizar n_danos como OLRE
# -----------------------------------------------------------------------------
m_pois_danos <- glm(
  n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)),
  family = poisson, data = cartera
)
performance::check_overdispersion(m_pois_danos)

# -----------------------------------------------------------------------------
# [u25-olre]  ·  5.6 Cierre: el efecto aleatorio a nivel de observación (OLRE) > El modelo y su ajuste
# -----------------------------------------------------------------------------
cartera$id_obs <- factor(seq_len(nrow(cartera)))   # un nivel aleatorio por observación
m_olre <- glmer(
  n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)) + (1 | id_obs),
  family = poisson, data = cartera
)
sd_olre <- as.data.frame(VarCorr(m_olre))$sdcor[1]
c(sigma_obs = sd_olre)

# -----------------------------------------------------------------------------
# [fig-u25-olre-diag]  ·  5.6 Cierre: el efecto aleatorio a nivel de observación (OLRE) > Inferencia, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
res_olre <- simulateResiduals(m_olre)
plot(res_olre)

# -----------------------------------------------------------------------------
# [u25-olre-comp]  ·  5.6 Cierre: el efecto aleatorio a nivel de observación (OLRE) > Comparación con la binomial negativa
# -----------------------------------------------------------------------------
m_nb2 <- glm.nb(
  n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)), data = cartera
)
performance::compare_performance(
  Poisson = m_pois_danos, NB2 = m_nb2, OLRE = m_olre, metrics = c("AIC", "BIC", "RMSE")
)

# -----------------------------------------------------------------------------
# [u25-olre-rmse]  ·  5.6 Cierre: el efecto aleatorio a nivel de observación (OLRE) > Comparación con la binomial negativa
# -----------------------------------------------------------------------------
pred_marg <- predict(m_olre, type = "response", re.form = NA)     # población, sin el BLUP
rmse_olre_marginal <- sqrt(mean((cartera$n_danos - pred_marg)^2))
rmse_olre_marginal

