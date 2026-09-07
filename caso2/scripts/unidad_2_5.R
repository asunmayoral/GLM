# =============================================================================
# Caso 2 · Unidad 2.5 — 5 · Efectos Aleatorios y Modelos Mixtos
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_5.qmd.
# Cada bloque va precedido de su LABEL y de la sección/subsección donde aparece.
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
  DHARMa = "0.5.0", MASS = "7.3.65", MuMIn = "1.48.19", broom = "1.0.13",
  dplyr = "1.2.1", forcats = "1.0.1", glmmTMB = "1.1.14", glmnet = "5.0",
  lme4 = "2.0.1", marginaleffects = "0.32.0", performance = "0.17.0",
  pscl = "1.5.9", sessioninfo = "1.2.4", survival = "3.8.6", tibble = "3.3.1",
  tidyverse = "2.0.0", vcd = "1.4.13", vcdExtra = "0.9.6")
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

source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
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


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
