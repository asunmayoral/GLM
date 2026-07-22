# =============================================================================
# Caso 2 · Unidad 2.3 — 3 · Demasiados ceros
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_3.qmd.
# Cada bloque va precedido de su LABEL y de la sección/subsección donde aparece.
#
# EJECUCIÓN: funciona desde CUALQUIER carpeta dentro del proyecto GLM; localiza
# la raíz por _quarto.yml y resuelve solo las rutas del DGP y de la caché.
# =============================================================================

# --- Librerías (idénticas al setup del documento del caso) -------------------
library(broom)
library(tidyverse)
library(MASS)          # glm.nb
library(pscl)          # hurdle / zeroinfl
library(glmmTMB)       # conteos mixtos / ceros
library(lme4)          # glmer (Poisson)
library(DHARMa); library(performance); library(marginaleffects)
library(survival)      # riesgos a trozos
library(MuMIn); library(glmnet)
library(vcdExtra)      # zero-inflated

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

# --- Datos: cartera de auto (misma llamada que el documento del caso) --------
# Localiza la raíz del proyecto (donde está _quarto.yml), sea cual sea el wd:
.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")
source(file.path(.raiz, "caso2", "R", "dgp_conteos.R"))  # simular_cartera(), cargar_cartera(), expandir_poliza_tramo()
cartera <- cargar_cartera("auto")    # lee datos/ si existe; si no (o si cambió el DGP), simula y cachea

# -----------------------------------------------------------------------------
# [u23-fraude-ceros]  ·  3.1 Qué es, de dónde viene y cómo se identifica > La huella: más ceros de los que el modelo espera
# -----------------------------------------------------------------------------
m_fp <- glm(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)),
            family = poisson, data = cartera)
obs0 <- mean(cartera$n_fraude == 0)     # proporción observada de ceros
esp0 <- mean(dpois(0, fitted(m_fp)))    # proporción esperada por la Poisson
c(observados = obs0, esperados_poisson = esp0, ratio = obs0 / esp0)

# -----------------------------------------------------------------------------
# [fig-u23-ceros]  ·  3.1 Qué es, de dónde viene y cómo se identifica > La huella: más ceros de los que el modelo espera
# -----------------------------------------------------------------------------
mu <- fitted(m_fp); K <- 0:8
esperado  <- sapply(K, function(k) if (k < 8) sum(dpois(k, mu)) else sum(1 - ppois(7, mu)))
observado <- as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K)))

tibble::tibble(k = K, Observado = observado, `Poisson ajustada` = esperado) |>
  tidyr::pivot_longer(c(Observado, `Poisson ajustada`), names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "Poisson ajustada" = "darkorange")) +
  labs(x = "nº de reclamaciones en revisión antifraude", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [fig-u23-mecanismo]  ·  3.1 Qué es, de dónde viene y cómo se identifica > De dónde viene: dos procesos de cero
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
# [u23-nb-zerotest]  ·  3.1 Qué es, de dónde viene y cómo se identifica > Cómo se identifica
# -----------------------------------------------------------------------------
m_fnb <- MASS::glm.nb(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                        offset(log(exposicion)), data = cartera)
DHARMa::testZeroInflation(m_fnb, plot = FALSE)   # ¿aún sobran ceros tras la NB?

# -----------------------------------------------------------------------------
# [u23-scoretest]  ·  3.1 Qué es, de dónde viene y cómo se identifica > Cómo se identifica
# -----------------------------------------------------------------------------
vcdExtra::zero.test(cartera$n_fraude)   # score test de Van den Broek

# -----------------------------------------------------------------------------
# [fig-u23-zip-motiv]  ·  3.2 Modelos zero-inflated (ZIP / ZINB) > Ajuste e interpretación
# -----------------------------------------------------------------------------
cartera |>
  dplyr::mutate(tramo_pot = dplyr::ntile(potencia_cv, 5)) |>
  dplyr::group_by(tramo_pot, uso) |>
  dplyr::summarise(`Parte estructural: P(cero)` = mean(n_fraude == 0),
                   `Parte de conteo: media`     = mean(n_fraude), .groups = "drop") |>
  tidyr::pivot_longer(-c(tramo_pot, uso), names_to = "parte", values_to = "v") |>
  dplyr::mutate(parte = factor(parte, levels = c("Parte estructural: P(cero)", "Parte de conteo: media"))) |>
  ggplot(aes(tramo_pot, v, colour = uso)) +
  geom_line() + geom_point(size = 2) +
  facet_wrap(~ parte, scales = "free_y") +
  labs(x = "quintil de potencia (CV)", y = NULL, colour = NULL)

# -----------------------------------------------------------------------------
# [fig-u23-zip-motiv2]  ·  3.2 Modelos zero-inflated (ZIP / ZINB) > Ajuste e interpretación
# -----------------------------------------------------------------------------
dplyr::bind_rows(
  cartera |> dplyr::group_by(predictor = "zona_circulacion", nivel = as.character(zona_circulacion)) |>
    dplyr::summarise(tasa = sum(n_fraude) / sum(exposicion), .groups = "drop"),
  cartera |> dplyr::group_by(predictor = "tipo_vehiculo",    nivel = as.character(tipo_vehiculo)) |>
    dplyr::summarise(tasa = sum(n_fraude) / sum(exposicion), .groups = "drop")) |>
  ggplot(aes(nivel, tasa)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ predictor, scales = "free_x") +
  labs(x = NULL, y = "tasa de reclamaciones (por unidad de exposición)")

# -----------------------------------------------------------------------------
# [u23-zip]  ·  3.2 Modelos zero-inflated (ZIP / ZINB) > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zip <- pscl::zeroinfl(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "poisson", data = cartera)
summary(m_zip)

# -----------------------------------------------------------------------------
# [u23-zinb]  ·  3.2 Modelos zero-inflated (ZIP / ZINB) > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zinb <- pscl::zeroinfl(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "negbin", data = cartera)
AIC(m_zip, m_zinb)

# -----------------------------------------------------------------------------
# [u23-zip-diag]  ·  3.2 Modelos zero-inflated (ZIP / ZINB) > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
c(observados    = sum(cartera$n_fraude == 0),
  esperados_zip = round(sum(predict(m_zip, type = "prob")[, 1])))

# -----------------------------------------------------------------------------
# [fig-u23-zip-root]  ·  3.2 Modelos zero-inflated (ZIP / ZINB) > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
esp <- colSums(predict(m_zip, type = "prob")); K <- 0:8
tibble::tibble(k = K,
               Observado      = as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K))),
               `ZIP ajustado` = c(esp[1:8], sum(esp[-(1:8)]))) |>
  tidyr::pivot_longer(-k, names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "ZIP ajustado" = "darkorange")) +
  labs(x = "nº de reclamaciones", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [u23-hurdle]  ·  3.3 Modelos hurdle > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_hp <- pscl::hurdle(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "poisson", data = cartera)
summary(m_hp)

# -----------------------------------------------------------------------------
# [u23-hurdle-diag]  ·  3.3 Modelos hurdle > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
c(observados       = sum(cartera$n_fraude == 0),
  esperados_hurdle = round(sum(predict(m_hp, type = "prob")[, 1])))   # coinciden por construcción

# -----------------------------------------------------------------------------
# [fig-u23-hurdle-root]  ·  3.3 Modelos hurdle > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
esp <- colSums(predict(m_hp, type = "prob")); K <- 0:8
tibble::tibble(k = K,
               Observado         = as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K))),
               `Hurdle ajustado` = c(esp[1:8], sum(esp[-(1:8)]))) |>
  tidyr::pivot_longer(-k, names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "Hurdle ajustado" = "darkorange")) +
  labs(x = "nº de reclamaciones", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [u23-hurdle-nb]  ·  3.3 Modelos hurdle > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
m_hnb <- pscl::hurdle(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "negbin", data = cartera)
AIC(m_hp, m_hnb)

# -----------------------------------------------------------------------------
# [u23-aic]  ·  3.4 Elegir, comparar y conectar > Los modelos, lado a lado
# -----------------------------------------------------------------------------
mods <- list(Poisson = m_fp, NB = m_fnb, ZIP = m_zip, ZINB = m_zinb,
             `Hurdle-P` = m_hp, `Hurdle-NB` = m_hnb)
data.frame(modelo = names(mods),
           df  = sapply(mods, function(m) attr(logLik(m), "df")),
           AIC = round(sapply(mods, AIC), 1)) |>
  dplyr::arrange(AIC)

# -----------------------------------------------------------------------------
# [u23-perf]  ·  3.4 Elegir, comparar y conectar > Los modelos, lado a lado
# -----------------------------------------------------------------------------
performance::compare_performance(Poisson = m_fp, NB = m_fnb, ZIP = m_zip, ZINB = m_zinb,
                                 `Hurdle-P` = m_hp, `Hurdle-NB` = m_hnb,
                                 metrics = c("AIC", "BIC", "RMSE"))

# -----------------------------------------------------------------------------
# [u23-pred]  ·  3.4 Elegir, comparar y conectar > Predicción: dos recetas, casi el mismo número
# -----------------------------------------------------------------------------
nuevas <- cartera[1:5, ]
cbind(ZIP    = predict(m_zip, nuevas, type = "response"),
      Hurdle = predict(m_hp,  nuevas, type = "response"))

# -----------------------------------------------------------------------------
# [u23-validacion]  ·  3.4 Elegir, comparar y conectar > Validación contra el DGP
# -----------------------------------------------------------------------------
v <- attr(cartera, "verdad")
v$g_cero   # DGP de la parte estructural: intercepto, efecto de uso y de potencia
