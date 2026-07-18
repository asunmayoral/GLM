# =============================================================================
# Caso 2 · Unidad 2.1 — 1 · Poisson y modelos log-lineales
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_1.qmd.
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
# [fig-u21-exposicion]  ·  1.1 Contexto: qué problemas resuelve este modelo
# -----------------------------------------------------------------------------
cartera |>
  mutate(tramo = cut(exposicion, c(0, 0.5, 0.75, 1),
                     labels = c("< 0.5", "0.5–0.75", "> 0.75"), include.lowest = TRUE)) |>
  group_by(tramo) |>
  summarise(media_danos = mean(n_danos), .groups = "drop") |>
  ggplot(aes(tramo, media_danos)) +
  geom_col(fill = "steelblue") +
  labs(x = "exposición (fracción de año en vigor)", y = "media de partes por daños")

# -----------------------------------------------------------------------------
# [tbl-u21-contingencia]  ·  1.1 Contexto: qué problemas resuelve este modelo
# -----------------------------------------------------------------------------
cartera |>
  mutate(danos = ifelse(n_danos > 0, "con daños", "sin daños")) |>
  count(zona_circulacion, danos) |>
  tidyr::pivot_wider(names_from = danos, values_from = n) |>
  mutate(pct_con_danos = round(`con daños` / (`con daños` + `sin daños`) * 100))

# -----------------------------------------------------------------------------
# [tbl-u21-cambio]  ·  1.1 Contexto: qué problemas resuelve este modelo
# -----------------------------------------------------------------------------
addmargins(table(previo = cartera$bonus_malus_prev, actual = cartera$bonus_malus_act))

# -----------------------------------------------------------------------------
# [tbl-u21-cambio-marg]  ·  1.1 Contexto: qué problemas resuelve este modelo
# -----------------------------------------------------------------------------
rbind(previo = prop.table(table(cartera$bonus_malus_prev)),
      actual = prop.table(table(cartera$bonus_malus_act))) |> round(3)

# -----------------------------------------------------------------------------
# [fig-u21-eda-modelo]  ·  1.2 Modelización y estimación > Aplicado a los cuatro problemas del contexto
# -----------------------------------------------------------------------------
dplyr::bind_rows(
  purrr::map_dfr(c("edad_conductor", "potencia_cv"), ~ cartera |>
    dplyr::transmute(predictor = .x, nivel = factor(dplyr::ntile(.data[[.x]], 4)),
                     y = n_asistencia, e = exposicion) |>
    dplyr::group_by(predictor, nivel) |> dplyr::summarise(tasa = sum(y) / sum(e), .groups = "drop")),
  purrr::map_dfr(c("zona_circulacion", "uso", "tipo_vehiculo"), ~ cartera |>
    dplyr::transmute(predictor = .x, nivel = factor(.data[[.x]]),
                     y = n_asistencia, e = exposicion) |>
    dplyr::group_by(predictor, nivel) |> dplyr::summarise(tasa = sum(y) / sum(e), .groups = "drop"))) |>
  ggplot(aes(nivel, tasa)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) +
  labs(x = "nivel (cuartil, en los continuos)", y = "tasa de asistencias (por año)")

# -----------------------------------------------------------------------------
# [u21-fit-conteo]  ·  1.2 Modelización y estimación > Aplicado a los cuatro problemas del contexto
# -----------------------------------------------------------------------------
m_conteo <- glm(n_asistencia ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo,
                family = poisson, data = cartera)
broom::tidy(m_conteo)

# -----------------------------------------------------------------------------
# [u21-fit-tasa]  ·  1.2 Modelización y estimación > Aplicado a los cuatro problemas del contexto
# -----------------------------------------------------------------------------
m_tasa <- glm(n_asistencia ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                offset(log(exposicion)),
              family = poisson, data = cartera)
broom::tidy(m_tasa)

# -----------------------------------------------------------------------------
# [u21-fit-loglineal]  ·  1.2 Modelización y estimación > Aplicado a los cuatro problemas del contexto
# -----------------------------------------------------------------------------
tabla <- cartera |>
  dplyr::mutate(danos = ifelse(n_danos > 0, "con", "sin")) |>
  dplyr::count(zona_circulacion, danos)                 # una fila por celda: n = frecuencia

m_indep <- glm(n ~ zona_circulacion + danos, family = poisson, data = tabla)  # independencia
m_sat   <- glm(n ~ zona_circulacion * danos, family = poisson, data = tabla)  # con asociación

# el saturado reproduce la tabla; el de independencia, no
cbind(tabla, indep = round(fitted(m_indep), 1), sat = round(fitted(m_sat), 1))

# -----------------------------------------------------------------------------
# [u21-fit-simetria]  ·  1.2 Modelización y estimación > Aplicado a los cuatro problemas del contexto
# -----------------------------------------------------------------------------
cuad <- as.data.frame(table(prev = cartera$bonus_malus_prev, act = cartera$bonus_malus_act)) |>
  dplyr::mutate(i = as.integer(prev), j = as.integer(act),
                par = factor(paste(pmin(i, j), pmax(i, j), sep = "-")))   # empareja (i,j) y (j,i)

m_sim <- glm(Freq ~ par, family = poisson, data = cuad)                   # modelo de simetría
head(broom::tidy(m_sim), 5)

# -----------------------------------------------------------------------------
# [u21-irr]  ·  1.3 Interpretación > En regresión: razones de tasas (IRR)
# -----------------------------------------------------------------------------
broom::tidy(m_tasa, exponentiate = TRUE, conf.int = TRUE)

# -----------------------------------------------------------------------------
# [u21-pred]  ·  1.3 Interpretación > En regresión: razones de tasas (IRR)
# -----------------------------------------------------------------------------
perfil <- data.frame(edad_conductor = 45, potencia_cv = 110, zona_circulacion = "urbana",
                     uso = "particular", tipo_vehiculo = "turismo", exposicion = 1)
predict(m_tasa, perfil, type = "response")   # nº esperado de asistencias en un año

# -----------------------------------------------------------------------------
# [u21-abs]  ·  1.3 Interpretación > Efecto relativo vs absoluto
# -----------------------------------------------------------------------------
perfiles <- data.frame(edad_conductor = 45, potencia_cv = 110,
                       zona_circulacion = c("urbana", "rural"), uso = "particular",
                       tipo_vehiculo = "turismo", exposicion = 1)
predict(m_tasa, perfiles, type = "response")   # esperadas: urbana vs rural

# -----------------------------------------------------------------------------
# [u21-loglin-or]  ·  1.3 Interpretación > En el log-lineal: la asociación
# -----------------------------------------------------------------------------
m_logit <- glm(I(n_danos > 0) ~ zona_circulacion, family = binomial, data = cartera)
exp(coef(m_logit))   # odds ratios de 'con daños' respecto a la zona de referencia

# -----------------------------------------------------------------------------
# [u21-drop1]  ·  1.4 Inferencia y selección > Contraste de los efectos
# -----------------------------------------------------------------------------
drop1(m_tasa, test = "LRT")   # aporte de cada predictor por razón de verosimilitudes

# -----------------------------------------------------------------------------
# [u21-indep]  ·  1.4 Inferencia y selección > El contraste de independencia es un LRT
# -----------------------------------------------------------------------------
anova(m_indep, m_sat, test = "LRT")   # H0: zona y daños son independientes

# -----------------------------------------------------------------------------
# [u21-simetria-test]  ·  1.4 Inferencia y selección > El contraste de simetría
# -----------------------------------------------------------------------------
c(deviance = deviance(m_sim), gl = df.residual(m_sim),
  p_valor  = pchisq(deviance(m_sim), df.residual(m_sim), lower.tail = FALSE))

# -----------------------------------------------------------------------------
# [u21-seleccion]  ·  1.4 Inferencia y selección > Selección de modelos
# -----------------------------------------------------------------------------
m_red <- update(m_tasa, . ~ . - tipo_vehiculo)
AIC(m_tasa, m_red)
anova(m_red, m_tasa, test = "LRT")

# -----------------------------------------------------------------------------
# [u21-dispersion]  ·  1.5 Bondad de ajuste, diagnóstico y predicción > Bondad de ajuste y el índice de dispersión
# -----------------------------------------------------------------------------
disp <- function(m) sum(residuals(m, type = "pearson")^2) / df.residual(m)
c(deviance_gl = deviance(m_tasa) / df.residual(m_tasa),
  indice_dispersion = disp(m_tasa))

# -----------------------------------------------------------------------------
# [u21-dharma]  ·  1.5 Bondad de ajuste, diagnóstico y predicción > Diagnóstico de residuos
# -----------------------------------------------------------------------------
library(DHARMa)
simulateResiduals(m_tasa, plot = TRUE)      # residuos escalados; QQ y dispersión
performance::check_overdispersion(m_tasa)   # test formal del índice de dispersión

# -----------------------------------------------------------------------------
# [u21-sobredispersion]  ·  1.5 Bondad de ajuste, diagnóstico y predicción > El puente a la sobredispersión (Unidad 2.2)
# -----------------------------------------------------------------------------
m_danos <- glm(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                 offset(log(exposicion)), family = poisson, data = cartera)
c(asistencia = disp(m_tasa), danos = disp(m_danos))

# -----------------------------------------------------------------------------
# [u21-pred-celda]  ·  1.5 Bondad de ajuste, diagnóstico y predicción > Predicción
# -----------------------------------------------------------------------------
transform(tabla, esperado_indep = round(fitted(m_indep), 1))   # observado (n) vs esperado si independientes
