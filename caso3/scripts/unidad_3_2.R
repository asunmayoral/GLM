# =============================================================================
# Caso 3 · Unidad 3.2 — 2 · Elegir familia y escala
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_2.qmd.
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
  glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1", lmtest = "0.9.40",
  marginaleffects = "0.32.0", patchwork = "1.3.2", performance = "0.17.0",
  purrr = "1.2.2", rsample = "1.3.2", sessioninfo = "1.2.4",
  statmod = "1.5.2", tibble = "3.3.1", tidyverse = "2.0.0")
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
# [u32-modelo-referencia]
#   2 · Elegir familia y escala
# -----------------------------------------------------------------------------
f_coste <- coste_euros ~ tipo_averia + criticidad * fase_proceso + antiguedad_anios + carga + fabricante

m_gam <- glm(f_coste, family = Gamma(link = "log"), data = base)   # el m_fab de la Unidad 3.1

# -----------------------------------------------------------------------------
# [fig-u32-potencia]
#   2 · Elegir familia y escala > 2.1 Qué distingue a una familia de otra
# -----------------------------------------------------------------------------
mv <- base |>
  dplyr::mutate(grupo = dplyr::ntile(fitted(m_gam), 14)) |>
  dplyr::group_by(grupo) |>
  dplyr::summarise(media = mean(coste_euros), varianza = var(coste_euros), .groups = "drop")

p_hat <- coef(lm(log(varianza) ~ log(media), data = mv))[2]

# Rectas teóricas de referencia, ancladas al centroide de la nube en escala log10.
# Con scale_*_log10(), geom_abline() dibuja log10(y) = intercept + slope * log10(x).
refs <- tibble::tibble(p = 1:3,
                       familia = c("p = 1", "p = 2 (Gamma)", "p = 3 (inv. gaussiana)"),
                       intercepto = mean(log10(mv$varianza)) - p * mean(log10(mv$media)))

ggplot(mv, aes(media, varianza)) +
  geom_abline(data = refs, aes(slope = p, intercept = intercepto, colour = familia),
              linetype = 2, linewidth = 0.6) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.7) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c("grey55", "firebrick", "steelblue")) +
  labs(x = "media del grupo (€, log)", y = "varianza del grupo (log)", colour = NULL,
       subtitle = paste0("pendiente estimada (línea negra): p = ", round(p_hat, 2)))

# -----------------------------------------------------------------------------
# [tbl-u32-ig]
#   2 · Elegir familia y escala > 2.2 Inversa gaussiana
# -----------------------------------------------------------------------------
m_ig <- glm(f_coste, family = inverse.gaussian(link = "log"), data = base)

broom::tidy(m_ig, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::transmute(term, `exp(beta)` = round(estimate, 3),
                   conf.low = round(conf.low, 3), conf.high = round(conf.high, 3))

# -----------------------------------------------------------------------------
# [tbl-u32-ig-dharma]
#   2 · Elegir familia y escala
#     > 2.2 Inversa gaussiana
#       > Bondad de ajuste, selección, diagnóstico y predicción: el camino de la Gamma
# -----------------------------------------------------------------------------
set.seed(20252026)
mu_ig  <- fitted(m_ig)
phi_ig <- summary(m_ig)$dispersion
sim_y  <- replicate(1000, statmod::rinvgauss(length(mu_ig), mean = mu_ig, dispersion = phi_ig))
sim_ig <- DHARMa::createDHARMa(simulatedResponse = sim_y, observedResponse = base$coste_euros,
                               fittedPredictedResponse = mu_ig, integerResponse = FALSE)
sim_gm <- DHARMa::simulateResiduals(m_gam, n = 1000)

alfa_gm <- 1 / summary(m_gam)$dispersion
cubre <- function(inf, sup) mean(base$coste_euros > inf & base$coste_euros < sup)

purrr::map_dfr(list(`Gamma` = sim_gm, `Inversa gaussiana` = sim_ig), \(s) tibble::tibble(
  uniformidad = DHARMa::testUniformity(s, plot = FALSE)$p.value,
  dispersion  = DHARMa::testDispersion(s, plot = FALSE)$p.value,
  cuantiles   = DHARMa::testQuantiles(s, plot = FALSE)$p.value), .id = "familia") |>
  dplyr::mutate(cobertura_ip95 = c(
    cubre(qgamma(0.025, alfa_gm, scale = fitted(m_gam) / alfa_gm),
          qgamma(0.975, alfa_gm, scale = fitted(m_gam) / alfa_gm)),
    cubre(statmod::qinvgauss(0.025, mu_ig, dispersion = phi_ig),
          statmod::qinvgauss(0.975, mu_ig, dispersion = phi_ig))),
    dplyr::across(uniformidad:cuantiles, \(x) format.pval(x, digits = 2, eps = 0.001)),
    cobertura_ip95 = round(cobertura_ip95, 3))

# -----------------------------------------------------------------------------
# [tbl-u32-lognormal]
#   2 · Elegir familia y escala > 2.3 Lognormal y log-OLS
# -----------------------------------------------------------------------------
m_ln <- lm(update(f_coste, log(coste_euros) ~ .), data = base)   # mismos predictores que m_gam

broom::glance(m_ln) |>
  dplyr::transmute(R2 = round(r.squared, 3), R2_aj = round(adj.r.squared, 3),
                   `F` = round(statistic, 1), p_valor = signif(p.value, 3),
                   sigma = round(sigma, 3), n = nobs)

# -----------------------------------------------------------------------------
# [tbl-u32-retransformacion]
#   2 · Elegir familia y escala
#     > 2.3 Lognormal y log-OLS
#       > Log-OLS y GLM-log no estiman lo mismo
# -----------------------------------------------------------------------------
y_obs    <- base$coste_euros
naive    <- exp(fitted(m_ln))                 # retransformación ingenua
smearing <- mean(exp(resid(m_ln)))            # factor de Duan (1983)
duan     <- naive * smearing

tibble::tibble(
  via = c("observado", "log-OLS sin corregir", "log-OLS + Duan", "GLM Gamma"),
  coste_medio = round(c(mean(y_obs), mean(naive), mean(duan), mean(fitted(m_gam))), 1)) |>
  dplyr::mutate(sesgo_pct = round(100 * (coste_medio / mean(y_obs) - 1), 1))

# -----------------------------------------------------------------------------
# [tbl-u32-duan-ic]
#   2 · Elegir familia y escala
#     > 2.3 Lognormal y log-OLS
#       > El intervalo de la media corregida
# -----------------------------------------------------------------------------
X_perf <- model.matrix(delete.response(terms(m_ln)), perfiles, xlev = m_ln$xlevels)
eta_ln <- drop(X_perf %*% coef(m_ln))                     # x0' beta del log-OLS

# Método delta: varianza de x0' beta más la de log(s)
var_log_duan <- rowSums((X_perf %*% vcov(m_ln)) * X_perf) +
                var(exp(resid(m_ln))) / (nobs(m_ln) * smearing^2)
ee_log_duan  <- sqrt(var_log_duan)

# Bootstrap: remuestrear filas, reajustar, recalcular s y la predicción
set.seed(20252026)
boot_duan <- replicate(1000, {
  i <- sample(nrow(base), replace = TRUE)
  m <- lm(formula(m_ln), data = base[i, ])
  exp(drop(X_perf %*% coef(m))) * mean(exp(resid(m)))
})

pg <- predict(m_gam, newdata = perfiles, type = "link", se.fit = TRUE)
tg <- qt(0.975, df.residual(m_gam))
ic <- function(inf, sup) paste0("[", round(inf), ", ", round(sup), "]")

duan_ic <- perfiles |>
  dplyr::transmute(tipo_averia, criticidad, fase_proceso,
                   duan       = exp(eta_ln) * smearing,
                   ic_delta   = ic(duan * exp(-1.96 * ee_log_duan), duan * exp(1.96 * ee_log_duan)),
                   ic_boot    = ic(apply(boot_duan, 1, quantile, 0.025),
                                   apply(boot_duan, 1, quantile, 0.975)),
                   gamma      = exp(pg$fit),
                   ic_gamma   = ic(exp(pg$fit - tg * pg$se.fit), exp(pg$fit + tg * pg$se.fit)))
duan_ic |> dplyr::mutate(duan = round(duan), gamma = round(gamma))

# -----------------------------------------------------------------------------
# [fig-u32-ln-diagnostico]
#   2 · Elegir familia y escala
#     > 2.3 Lognormal y log-OLS
#       > Bondad de ajuste, selección, diagnóstico y predicción: el camino del modelo lineal
# -----------------------------------------------------------------------------
diag_ln <- tibble::tibble(ajustado = fitted(m_ln), residuo = resid(m_ln),
                          estandarizado = rstandard(m_ln))
patchwork::wrap_plots(
  ggplot(diag_ln, aes(ajustado, residuo)) +
    geom_point(alpha = 0.3, size = 0.8) + geom_hline(yintercept = 0, linetype = 2) +
    geom_smooth(se = FALSE, colour = "firebrick") +
    labs(x = "valor ajustado (log €)", y = "residuo"),
  ggplot(diag_ln, aes(sample = estandarizado)) +
    stat_qq(alpha = 0.3, size = 0.8) + stat_qq_line(colour = "firebrick") +
    labs(x = "cuantil normal teórico", y = "residuo estandarizado"))

# -----------------------------------------------------------------------------
# [tbl-u32-ln-diagnostico]
#   2 · Elegir familia y escala
#     > 2.3 Lognormal y log-OLS
#       > Bondad de ajuste, selección, diagnóstico y predicción: el camino del modelo lineal
# -----------------------------------------------------------------------------
ip_ln <- exp(predict(m_ln, interval = "prediction"))     # el intervalo, ya en euros
diag_tests_ln <- tibble::tibble(
  shapiro_p       = signif(shapiro.test(resid(m_ln))$p.value, 2),
  breusch_pagan_p = signif(lmtest::bptest(m_ln)$p.value, 2),
  cook_max        = round(max(cooks.distance(m_ln)), 3),
  cobertura_ip95  = round(cubre(ip_ln[, "lwr"], ip_ln[, "upr"]), 3))
diag_tests_ln

# -----------------------------------------------------------------------------
# [tbl-u32-aic]
#   2 · Elegir familia y escala > 2.4 Elegir, comparar y conectar
# -----------------------------------------------------------------------------
ll_ln <- as.numeric(logLik(m_ln)) - sum(log(base$coste_euros))   # + jacobiano
gl_ln <- attr(logLik(m_ln), "df")

tibble::tibble(
  familia = c("Gamma (log)", "Inversa gaussiana (log)", "Lognormal + jacobiano",
              "Lognormal SIN jacobiano [no comparable]"),
  AIC = round(c(AIC(m_gam), AIC(m_ig), -2 * ll_ln + 2 * gl_ln, AIC(m_ln)), 1)) |>
  dplyr::mutate(delta = ifelse(dplyr::row_number() <= 3, round(AIC - min(AIC[1:3]), 1), NA))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
