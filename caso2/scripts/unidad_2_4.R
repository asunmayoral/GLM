# =============================================================================
# Caso 2 · Unidad 2.4 — 4 · Demasiados ceros
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_4.qmd.
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
# [u24-fraude-ceros]  ·  4.1 Qué es, de dónde viene y cómo se identifica > La huella: más ceros de los que el modelo espera
# -----------------------------------------------------------------------------
m_fp <- glm(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)),
            family = poisson, data = cartera)
obs0 <- mean(cartera$n_fraude == 0)     # proporción observada de ceros
esp0 <- mean(dpois(0, fitted(m_fp)))    # proporción esperada por la Poisson
c(observados = obs0, esperados_poisson = esp0, ratio = obs0 / esp0)

# -----------------------------------------------------------------------------
# [fig-u24-ceros]  ·  4.1 Qué es, de dónde viene y cómo se identifica > La huella: más ceros de los que el modelo espera
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
# [fig-u24-mecanismo]  ·  4.1 Qué es, de dónde viene y cómo se identifica > De dónde viene: dos procesos de cero
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
# [u24-nb-zerotest]  ·  4.1 Qué es, de dónde viene y cómo se identifica > Cómo se identifica
# -----------------------------------------------------------------------------
m_fnb <- MASS::glm.nb(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                        offset(log(exposicion)), data = cartera)
DHARMa::testZeroInflation(m_fnb, plot = FALSE)   # ¿aún sobran ceros tras la NB?

# -----------------------------------------------------------------------------
# [u24-scoretest]  ·  4.1 Qué es, de dónde viene y cómo se identifica > Cómo se identifica
# -----------------------------------------------------------------------------
vcdExtra::zero.test(cartera$n_fraude)   # score test de Van den Broek

# -----------------------------------------------------------------------------
# [fig-u24-zip-motiv]  ·  4.2 Modelos zero-inflated (ZIP / ZINB) > Ajuste e interpretación
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
# [fig-u24-zip-motiv2]  ·  4.2 Modelos zero-inflated (ZIP / ZINB) > Ajuste e interpretación
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
# [u24-zip]  ·  4.2 Modelos zero-inflated (ZIP / ZINB) > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zip <- pscl::zeroinfl(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "poisson", data = cartera)
summary(m_zip)

# -----------------------------------------------------------------------------
# [u24-zinb]  ·  4.2 Modelos zero-inflated (ZIP / ZINB) > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zinb <- pscl::zeroinfl(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "negbin", data = cartera)
AIC(m_zip, m_zinb)

# -----------------------------------------------------------------------------
# [u24-zip-diag]  ·  🔧 En R. Ajustar un ZIP / ZINB > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
c(observados    = sum(cartera$n_fraude == 0),
  esperados_zip = round(sum(predict(m_zip, type = "prob")[, 1])))

# -----------------------------------------------------------------------------
# [fig-u24-zip-root]  ·  🔧 En R. Ajustar un ZIP / ZINB > Bondad de ajuste y diagnóstico
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
# [u24-hurdle]  ·  4.3 Modelos hurdle > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_hp <- pscl::hurdle(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "poisson", data = cartera)
summary(m_hp)

# -----------------------------------------------------------------------------
# [u24-hurdle-diag]  ·  4.3 Modelos hurdle > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
c(observados       = sum(cartera$n_fraude == 0),
  esperados_hurdle = round(sum(predict(m_hp, type = "prob")[, 1])))   # coinciden por construcción

# -----------------------------------------------------------------------------
# [fig-u24-hurdle-root]  ·  4.3 Modelos hurdle > Bondad de ajuste y diagnóstico
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
# [u24-hurdle-nb]  ·  4.3 Modelos hurdle > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
m_hnb <- pscl::hurdle(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)) |
             uso + potencia_cv,
  dist = "negbin", data = cartera)
AIC(m_hp, m_hnb)

# -----------------------------------------------------------------------------
# [u24-aic]  ·  4.4 Elegir, comparar y conectar > Los modelos, lado a lado
# -----------------------------------------------------------------------------
mods <- list(Poisson = m_fp, NB = m_fnb, ZIP = m_zip, ZINB = m_zinb,
             `Hurdle-P` = m_hp, `Hurdle-NB` = m_hnb)
data.frame(modelo = names(mods),
           df  = sapply(mods, function(m) attr(logLik(m), "df")),
           AIC = round(sapply(mods, AIC), 1)) |>
  dplyr::arrange(AIC)

# -----------------------------------------------------------------------------
# [u24-perf]  ·  4.4 Elegir, comparar y conectar > Los modelos, lado a lado
# -----------------------------------------------------------------------------
performance::compare_performance(Poisson = m_fp, NB = m_fnb, ZIP = m_zip, ZINB = m_zinb,
                                 `Hurdle-P` = m_hp, `Hurdle-NB` = m_hnb,
                                 metrics = c("AIC", "BIC", "RMSE"))

# -----------------------------------------------------------------------------
# [u24-pred]  ·  🔧 En R. Bondad de ajuste con exceso de ceros > Predicción: dos recetas, casi el mismo número
# -----------------------------------------------------------------------------
nuevas <- cartera[1:5, ]
cbind(ZIP    = predict(m_zip, nuevas, type = "response"),
      Hurdle = predict(m_hp,  nuevas, type = "response"))

# -----------------------------------------------------------------------------
# [u24-validacion]  ·  ZI o hurdle: cuándo cada uno > Validación contra el DGP
# -----------------------------------------------------------------------------
v <- attr(cartera, "verdad")
v$g_cero   # DGP de la parte estructural: intercepto, efecto de uso y de potencia

