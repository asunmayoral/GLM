# =============================================================================
# Caso 2 · Unidad 2.4 — 4 · Demasiados ceros
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_4.qmd.
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
  DHARMa = "0.5.0", MASS = "7.3.65", broom = "1.0.13", dplyr = "1.2.1",
  ggplot2 = "4.0.3", glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  lmtest = "0.9.40", marginaleffects = "0.32.0", performance = "0.17.0",
  pscl = "1.5.9", purrr = "1.2.2", sessioninfo = "1.2.4", survival = "3.8.6",
  tibble = "3.3.1", tidyr = "1.3.2", tidyverse = "2.0.0", vcd = "1.4.13",
  vcdExtra = "0.9.6")
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
library(glmnet)       # regularización (2.1)
library(vcd)           # mosaicos para tablas de contingencia (2.2)
library(vcdExtra)      # zero-inflated (2.4) y utilidades de tablas (2.2)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
glimpse(cartera)
head(cartera)

# -----------------------------------------------------------------------------
# [u24-fraude-ceros]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > La huella: más ceros de los que el modelo espera
# -----------------------------------------------------------------------------
m_fp <- glm(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)),
            family = poisson, data = cartera)
obs0 <- mean(cartera$n_fraude == 0)     # proporción observada de ceros
esp0 <- mean(dpois(0, fitted(m_fp)))    # proporción esperada por la Poisson
c(observados = obs0, esperados_poisson = esp0, ratio = obs0 / esp0)

# -----------------------------------------------------------------------------
# [fig-u24-ceros]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > La huella: más ceros de los que el modelo espera
# -----------------------------------------------------------------------------
mu <- fitted(m_fp); K <- 0:8
esperado  <- sapply(K, function(k) if (k < 8) sum(dpois(k, mu)) else sum(1 - ppois(7, mu)))
observado <- as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K)))

tibble::tibble(k = K, Observado = observado, `Poisson ajustada` = esperado) |>
  tidyr::pivot_longer(c(Observado, `Poisson ajustada`), names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "Poisson ajustada" = "darkorange")) +
  labs(x = "nº de alertas antifraude", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [fig-u24-mecanismo]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > De dónde viene: dos procesos de cero
# -----------------------------------------------------------------------------
set.seed(2026)
n <- 5000; mu <- 2; pi_e <- 0.4
dplyr::bind_rows(
  tibble::tibble(poblacion = "Poisson",                       y = rpois(n, mu)),
  tibble::tibble(poblacion = "Con 40 % de ceros estructurales", y = ifelse(rbinom(n, 1, pi_e) == 1, 0, rpois(n, mu)))) |>
  dplyr::filter(y <= 8) |>
  dplyr::count(poblacion, y) |>
  ggplot(aes(y, n, fill = poblacion)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Poisson" = "steelblue", "Con 40 % de ceros estructurales" = "darkorange")) +
  labs(x = "nº de eventos", y = "frecuencia", fill = NULL)

# -----------------------------------------------------------------------------
# [u24-nb-zerotest]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > Cómo se identifica
# -----------------------------------------------------------------------------
m_fnb <- MASS::glm.nb(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                        offset(log(exposicion)), data = cartera)
set.seed(2024)   # el p-valor sale de simular: fijamos la semilla para que se reproduzca
sim_fnb <- DHARMa::simulateResiduals(m_fnb, n = 1000)
zt_fnb  <- DHARMa::testZeroInflation(sim_fnb, plot = FALSE)   # ¿aún sobran ceros tras la NB?
zt_fnb
# cuántas de las 1000 simulaciones igualan o superan los ceros observados
sum(colSums(sim_fnb$simulatedResponse == 0) >= sum(cartera$n_fraude == 0))

# -----------------------------------------------------------------------------
# [u24-scoretest]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > Cómo se identifica
# -----------------------------------------------------------------------------
vcdExtra::zero.test(cartera$n_fraude)   # score test de Van den Broek

# el mismo estadístico, calculado a mano con la fórmula
y_f <- cartera$n_fraude; n_f <- length(y_f); n0_f <- sum(y_f == 0)
lam_f <- mean(y_f); p0_f <- exp(-lam_f)            # tasa común y P(Y = 0) de la Poisson
(n0_f - n_f * p0_f)^2 / (n_f * p0_f * (1 - p0_f) - n_f * lam_f * p0_f^2)

# -----------------------------------------------------------------------------
# [u24-ceros-grupos-tabla]
#   4 · Demasiados ceros
#     > 4.2 Estructuras para los ceros
#       > Pregunta 1 · ¿Sobran ceros incluso frente a la NB?
# -----------------------------------------------------------------------------
ceros_grupo <- tibble::tibble(
  grupo     = dplyr::ntile(fitted(m_fp), 12),          # 12 grupos de riesgo esperado parecido
  media     = fitted(m_fp),
  Observado = cartera$n_fraude == 0,
  Poisson   = dpois(0, fitted(m_fp)),                  # P(Y = 0) de cada póliza según cada modelo
  NB        = dnbinom(0, size = m_fnb$theta, mu = fitted(m_fnb))) |>
  dplyr::group_by(grupo) |>
  dplyr::summarise(dplyr::across(c(media, Observado, Poisson, NB), mean))
ceros_grupo

# -----------------------------------------------------------------------------
# [fig-u24-ceros-grupos]
#   4 · Demasiados ceros
#     > 4.2 Estructuras para los ceros
#       > Pregunta 1 · ¿Sobran ceros incluso frente a la NB?
# -----------------------------------------------------------------------------
ceros_grupo |>
  tidyr::pivot_longer(c(Observado, Poisson, NB), names_to = "fuente", values_to = "p0") |>
  ggplot(aes(media, p0, colour = fuente)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Observado" = "grey20", "Poisson" = "darkorange", "NB" = "steelblue")) +
  labs(x = "nº esperado de alertas en el grupo (Poisson)", y = "proporción de ceros", colour = NULL)

# -----------------------------------------------------------------------------
# [u24-positivos]
#   4 · Demasiados ceros
#     > 4.2 Estructuras para los ceros
#       > Pregunta 2 · ¿Poisson o NB para el conteo? Los positivos
# -----------------------------------------------------------------------------
# solo las pólizas con alguna alerta: ¿su conteo es Poisson (truncada en 0) o va más disperso?
pos   <- subset(cartera, n_fraude > 0)
m_ztp <- glmmTMB::glmmTMB(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                            offset(log(exposicion)),
                          family = glmmTMB::truncated_poisson(), data = pos)
lam  <- exp(predict(m_ztp, type = "link"))   # media de la Poisson SIN truncar
m_tr <- lam / (1 - exp(-lam))                # media de la Poisson truncada en 0
v_tr <- m_tr * (1 + lam - m_tr)              # y su varianza
disp_pos <- sum((pos$n_fraude - m_tr)^2 / v_tr) / df.residual(m_ztp)
disp_pos

# -----------------------------------------------------------------------------
# [u24-zip-explora]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
d_exp <- dplyr::mutate(cartera, potencia_cv_tercil = ggplot2::cut_number(potencia_cv, 3))
explora <- function(var) {
  d <- dplyr::mutate(d_exp, nivel = factor(.data[[var]]))
  # ritmo de las vigiladas: Poisson truncada en 0 sobre las pólizas con alguna alerta, un ritmo por nivel
  m_tr  <- glmmTMB::glmmTMB(n_fraude ~ 0 + nivel + offset(log(exposicion)),
                            family = glmmTMB::truncated_poisson(), data = subset(d, n_fraude > 0))
  ritmo <- exp(glmmTMB::fixef(m_tr)$cond)
  d |>
    dplyr::mutate(ritmo = ritmo[as.integer(nivel)]) |>
    dplyr::group_by(nivel) |>
    dplyr::summarise(ceros_obs    = mean(n_fraude == 0),
                     ceros_conteo = mean(exp(-ritmo * exposicion)),   # ceros que daría ese ritmo
                     ritmo        = dplyr::first(ritmo), .groups = "drop") |>
    dplyr::mutate(predictor = var, nivel = as.character(nivel),
                  pi_est = (ceros_obs - ceros_conteo) / (1 - ceros_conteo))
}
explora_zip <- purrr::map_dfr(c("potencia_cv_tercil", "uso", "zona_circulacion", "tipo_vehiculo"),
                              explora)
explora_zip

# -----------------------------------------------------------------------------
# [fig-u24-zip-explora]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
explora_zip |>
  tidyr::pivot_longer(c(pi_est, ritmo), names_to = "parte", values_to = "valor") |>
  dplyr::mutate(parte = factor(parte, levels = c("pi_est", "ritmo"),
                               labels = c("Estructural: ceros estructurales (pi)",
                                          "Conteo: ritmo de las vigiladas"))) |>
  ggplot(aes(nivel, valor, group = 1)) +
  geom_line(colour = "grey60") +
  geom_point(size = 2.5, colour = "steelblue") +
  facet_grid(parte ~ predictor, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# -----------------------------------------------------------------------------
# [u24-zip]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zip <- pscl::zeroinfl(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "poisson", data = cartera)
summary(m_zip)

# -----------------------------------------------------------------------------
# [u24-zip-edad]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zip_edad <- pscl::zeroinfl(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + edad_conductor + antiguedad_carnet +
             offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "poisson", data = cartera)
lmtest::lrtest(m_zip, m_zip_edad)   # H0: edad y carnet no aportan a la parte de conteo

# -----------------------------------------------------------------------------
# [u24-zinb]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zinb <- pscl::zeroinfl(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "negbin", data = cartera)
AIC(m_zip, m_zinb)

# -----------------------------------------------------------------------------
# [u24-zip-diag]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
c(observados    = sum(cartera$n_fraude == 0),
  esperados_zip = round(sum(predict(m_zip, type = "prob")[, 1])))

# -----------------------------------------------------------------------------
# [fig-u24-zip-root]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
esp <- colSums(predict(m_zip, type = "prob")); K <- 0:8
tibble::tibble(k = K,
               Observado      = as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K))),
               `ZIP ajustado` = c(esp[1:8], sum(esp[-(1:8)]))) |>
  tidyr::pivot_longer(-k, names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "ZIP ajustado" = "darkorange")) +
  labs(x = "nº de alertas", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [u24-zip-tmb]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
m_zip_tmb <- glmmTMB::glmmTMB(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)),
  ziformula = ~ uso + potencia_cv, family = poisson, data = cartera)
cbind(pscl    = coef(m_zip),
      glmmTMB = c(glmmTMB::fixef(m_zip_tmb)$cond, glmmTMB::fixef(m_zip_tmb)$zi))

# -----------------------------------------------------------------------------
# [fig-u24-zip-dharma]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
set.seed(2025)   # la simulación es aleatoria: fijamos la semilla para que el diagnóstico se reproduzca
res_zip <- DHARMa::simulateResiduals(m_zip_tmb, n = 1000)
plot(res_zip)

# -----------------------------------------------------------------------------
# [u24-zip-dharma-tests]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
contrastes_zip <- list(
  uniformidad     = DHARMa::testUniformity(res_zip, plot = FALSE),
  dispersion      = DHARMa::testDispersion(res_zip, plot = FALSE),
  exceso_de_ceros = DHARMa::testZeroInflation(res_zip, plot = FALSE))
tibble::tibble(contraste   = names(contrastes_zip),
               estadistico = sapply(contrastes_zip, \(t) unname(t$statistic)),
               p_valor     = sapply(contrastes_zip, \(t) t$p.value))

# -----------------------------------------------------------------------------
# [fig-u24-zip-dharma-pred]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
par(mfrow = c(1, 2))   # los dos paneles en una sola figura
DHARMa::plotResiduals(res_zip, cartera$uso, xlab = "uso")
DHARMa::plotResiduals(res_zip, cartera$potencia_cv, xlab = "potencia (CV)")

# -----------------------------------------------------------------------------
# [u24-zip-chi2-boot]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
frec_chi2 <- function(m, y, K = 8) {               # celdas 0, 1, ..., K-1 y "K o más"
  P <- predict(m, type = "prob", at = 0:(K - 1))   # P(Y_i = k) de cada póliza
  E <- c(colSums(P), length(y) - sum(P))
  O <- c(tabulate(y + 1, nbins = K), sum(y >= K))
  sum((O - E)^2 / E)
}
X2_obs <- frec_chi2(m_zip, cartera$n_fraude)

set.seed(2025)
y_boot  <- simulate(m_zip_tmb, nsim = 200)         # 200 carteras simuladas desde el ZIP
X2_boot <- vapply(y_boot, function(ys)
  frec_chi2(update(m_zip, data = transform(cartera, n_fraude = ys)), ys), numeric(1))

c(X2 = X2_obs,
  p_bootstrap = mean(X2_boot >= X2_obs),
  p_chi2_ingenuo = pchisq(X2_obs, df = 8, lower.tail = FALSE))   # 9 celdas - 1, sin más ajuste

# -----------------------------------------------------------------------------
# [u24-hurdle-explora]
#   4 · Demasiados ceros > 4.4 Modelos hurdle > Ajuste e interpretación
# -----------------------------------------------------------------------------
explora_zip |>
  dplyr::transmute(predictor, nivel, cruza_barrera = 1 - ceros_obs, ritmo)

# -----------------------------------------------------------------------------
# [u24-hurdle]
#   4 · Demasiados ceros > 4.4 Modelos hurdle > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_hp <- pscl::hurdle(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "poisson", data = cartera)
summary(m_hp)

# -----------------------------------------------------------------------------
# [u24-hurdle-nb]
#   4 · Demasiados ceros > 4.4 Modelos hurdle > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_hnb <- pscl::hurdle(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "negbin", data = cartera)
aic_h <- AIC(m_hp, m_hnb)
aic_h

# -----------------------------------------------------------------------------
# [u24-hurdle-diag]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
c(observados       = sum(cartera$n_fraude == 0),
  esperados_hurdle = round(sum(predict(m_hp, type = "prob")[, 1])))   # coinciden por construcción

# -----------------------------------------------------------------------------
# [fig-u24-hurdle-root]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
esp <- colSums(predict(m_hp, type = "prob")); K <- 0:8
tibble::tibble(k = K,
               Observado         = as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K))),
               `Hurdle ajustado` = c(esp[1:8], sum(esp[-(1:8)]))) |>
  tidyr::pivot_longer(-k, names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "Hurdle ajustado" = "darkorange")) +
  labs(x = "nº de alertas", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [u24-hurdle-tmb]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
m_hp_tmb <- glmmTMB::glmmTMB(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)),
  ziformula = ~ uso + potencia_cv, family = glmmTMB::truncated_poisson(), data = cartera)
cbind(pscl    = coef(m_hp),
      glmmTMB = c(glmmTMB::fixef(m_hp_tmb)$cond, -glmmTMB::fixef(m_hp_tmb)$zi))

# -----------------------------------------------------------------------------
# [fig-u24-hurdle-dharma]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
set.seed(2025)   # la simulación es aleatoria: fijamos la semilla para que el diagnóstico se reproduzca
res_hp <- DHARMa::simulateResiduals(m_hp_tmb, n = 1000)
plot(res_hp)

# -----------------------------------------------------------------------------
# [u24-hurdle-dharma-tests]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
contrastes_hp <- list(
  uniformidad = DHARMa::testUniformity(res_hp, plot = FALSE),
  dispersion  = DHARMa::testDispersion(res_hp, plot = FALSE))
tibble::tibble(contraste = names(contrastes_hp),
               p_valor   = sapply(contrastes_hp, \(t) t$p.value))

# -----------------------------------------------------------------------------
# [u24-hurdle-res-exposicion]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
# mediana de los residuos simulados por cuartil de exposición: debería rondar 0.5 en todos
tapply(residuals(res_hp), ggplot2::cut_number(cartera$exposicion, 4), median)

# -----------------------------------------------------------------------------
# [fig-u24-hurdle-dharma-exp]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::plotResiduals(res_hp, cartera$exposicion, xlab = "exposición")

# -----------------------------------------------------------------------------
# [u24-comparacion]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Los seis modelos, lado a lado
# -----------------------------------------------------------------------------
mods <- list(Poisson = m_fp, NB = m_fnb, ZIP = m_zip, ZINB = m_zinb,
             `Hurdle-P` = m_hp, `Hurdle-NB` = m_hnb)
comparacion <- data.frame(
  modelo = names(mods),
  df     = sapply(mods, function(m) attr(logLik(m), "df")),
  AIC    = sapply(mods, AIC),
  BIC    = sapply(mods, BIC),
  RMSE   = sapply(mods, function(m) sqrt(mean((cartera$n_fraude - fitted(m))^2)))) |>
  dplyr::mutate(peso_AIC = exp(-(AIC - min(AIC)) / 2) / sum(exp(-(AIC - min(AIC)) / 2))) |>
  dplyr::arrange(AIC)
comparacion

# -----------------------------------------------------------------------------
# [u24-pred-tabla]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Predicción: la misma media, distinta distribución
# -----------------------------------------------------------------------------
perfiles_u24 <- data.frame(perfil = c("particular, 80 CV", "comercial, 160 CV"),
                           potencia_cv = c(80, 160), uso = c("particular", "comercial"),
                           zona_circulacion = "urbana", tipo_vehiculo = "turismo",
                           exposicion = 1)
K <- 0:10
# P(Y = k) de cada perfil según cada modelo: una fila por perfil, una columna por k
prob_modelo <- list(
  Poisson    = t(sapply(predict(m_fp,  perfiles_u24, type = "response"), \(m) dpois(K, m))),
  NB         = t(sapply(predict(m_fnb, perfiles_u24, type = "response"),
                        \(m) dnbinom(K, size = m_fnb$theta, mu = m))),
  ZIP        = predict(m_zip, perfiles_u24, type = "prob", at = K),
  `Hurdle-P` = predict(m_hp,  perfiles_u24, type = "prob", at = K))
pred_u24 <- purrr::imap_dfr(prob_modelo, \(P, modelo)
  tibble::tibble(modelo = modelo, perfil = rep(perfiles_u24$perfil, times = length(K)),
                 k = rep(K, each = nrow(perfiles_u24)), prob = as.vector(P)))
resumen_pred <- pred_u24 |>
  dplyr::group_by(perfil, modelo) |>
  dplyr::summarise(media = sum(k * prob), P_cero = prob[k == 0], .groups = "drop")
resumen_pred
p0_pred <- function(m, p = perfiles_u24$perfil[1])   # P(Y = 0) de un perfil según un modelo
  resumen_pred$P_cero[resumen_pred$modelo == m & resumen_pred$perfil == p]

# -----------------------------------------------------------------------------
# [fig-u24-pred-dist]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Predicción: la misma media, distinta distribución
# -----------------------------------------------------------------------------
pred_u24 |>
  dplyr::mutate(modelo = factor(modelo, levels = names(prob_modelo))) |>
  ggplot(aes(k, prob, fill = modelo)) +
  geom_col(position = "dodge") +
  facet_wrap(~ perfil) +
  scale_x_continuous(breaks = K) +
  labs(x = "nº de alertas", y = "probabilidad predicha", fill = NULL)

# -----------------------------------------------------------------------------
# [u24-validacion]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
v <- attr(cartera, "verdad")
v$g_cero   # DGP de la parte estructural: intercepto, efecto de uso y de potencia
# el DGP usa la potencia tipificada: llevamos el coeficiente del ZIP (por CV) a esa escala
c(usocomercial_ZIP          = coef(m_zip)[["zero_usocomercial"]],
  potencia_tipificada_ZIP   = coef(m_zip)[["zero_potencia_cv"]] * sd(cartera$potencia_cv))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
