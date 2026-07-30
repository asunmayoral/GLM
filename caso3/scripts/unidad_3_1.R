# =============================================================================
# Caso 3 · Unidad 3.1 — 1 · GLM Gamma
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_1.qmd.
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
# [u31-datos]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
base <- banco$averias

c(n = nrow(base),
  media   = mean(base$coste_euros),
  mediana = median(base$coste_euros),
  minimo  = min(base$coste_euros),
  maximo  = max(base$coste_euros)) |> round(1)

# -----------------------------------------------------------------------------
# [fig-u31-asimetria]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
p1 <- ggplot(base, aes(coste_euros)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  labs(x = "coste de la avería (€)", y = "nº de averías")

p2 <- ggplot(base, aes(coste_euros)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  scale_x_log10() +
  labs(x = "coste (€, escala log)", y = NULL)

p1 + p2

# -----------------------------------------------------------------------------
# [u31-ols-falla]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
ols   <- lm(coste_euros ~ tipo_averia + proceso + criticidad + antiguedad_anios + carga, data = base)
sigma <- summary(ols)$sigma

c(prediccion_minima    = round(min(fitted(ols))),
  ip95_inferior_minimo = round(min(fitted(ols) - 1.96 * sigma)),
  pct_ip95_bajo_cero   = round(mean(fitted(ols) - 1.96 * sigma < 0) * 100),
  prob_media_coste_neg = round(mean(pnorm(0, fitted(ols), sigma)) * 100, 1))

# -----------------------------------------------------------------------------
# [fig-u31-heterocedasticidad]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
d_ols <- tibble::tibble(ajustado = fitted(ols), residuo = resid(ols))

q1 <- ggplot(d_ols, aes(ajustado, residuo)) +
  geom_point(alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, colour = "firebrick") +
  labs(x = "valor ajustado (€)", y = "residuo")

q2 <- ggplot(d_ols, aes(sample = residuo)) +
  stat_qq(alpha = 0.25) + stat_qq_line(colour = "firebrick") +
  labs(x = "cuantiles teóricos", y = "cuantiles muestrales")

q1 + q2

# -----------------------------------------------------------------------------
# [tbl-u31-cv]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
base |>
  dplyr::mutate(tramo = dplyr::ntile(fitted(ols), 5)) |>
  dplyr::group_by(tramo) |>
  dplyr::summarise(n = dplyr::n(),
                   media = mean(coste_euros), sd = sd(coste_euros),
                   cv = sd / media, .groups = "drop") |>
  dplyr::mutate(dplyr::across(c(media, sd), \(x) round(x, 1)), cv = round(cv, 3))

# -----------------------------------------------------------------------------
# [fig-u31-cat]
#   1 · GLM Gamma > 1.1 Contexto: qué modela > ¿De qué depende el coste?
# -----------------------------------------------------------------------------
base |>
  dplyr::select(coste_euros, tipo_averia, proceso, criticidad) |>
  tidyr::pivot_longer(-coste_euros, names_to = "predictor", values_to = "nivel") |>
  ggplot(aes(nivel, coste_euros)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~ predictor, scales = "free_x", nrow = 1) +
  scale_y_log10() +
  labs(x = NULL, y = "coste (€, escala log)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# -----------------------------------------------------------------------------
# [fig-u31-cont]
#   1 · GLM Gamma > 1.1 Contexto: qué modela > ¿De qué depende el coste?
# -----------------------------------------------------------------------------
base |>
  dplyr::select(coste_euros, criticidad, antiguedad_anios, carga) |>
  tidyr::pivot_longer(c(antiguedad_anios, carga), names_to = "predictor", values_to = "valor") |>
  ggplot(aes(valor, coste_euros, colour = criticidad)) +
  geom_point(alpha = 0.20, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ predictor, scales = "free_x") +
  scale_y_log10() +
  labs(x = NULL, y = "coste (€, escala log)", colour = NULL) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# [fig-u31-interaccion-eda]
#   1 · GLM Gamma > 1.1 Contexto: qué modela > ¿De qué depende el coste?
# -----------------------------------------------------------------------------
ggplot(base, aes(proceso, coste_euros, fill = criticidad)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  labs(x = NULL, y = "coste (€, escala log)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# -----------------------------------------------------------------------------
# [u31-fase]
#   1 · GLM Gamma > 1.1 Contexto: qué modela > ¿De qué depende el coste?
# -----------------------------------------------------------------------------
base <- base |>
  dplyr::mutate(fase_proceso = factor(
    ifelse(proceso %in% c("Ensamblaje", "Acabado"), "fase2", "fase1"),
    levels = c("fase1", "fase2")))

# -----------------------------------------------------------------------------
# [tbl-u31-coef-ee]
#   1 · GLM Gamma > 1.2 Modelización y estimación > El modelo del caso
# -----------------------------------------------------------------------------
m_gamma <- glm(coste_euros ~ tipo_averia + criticidad * fase_proceso + antiguedad_anios + carga,
               family = Gamma(link = "log"), data = base)

broom::tidy(m_gamma) |>
  dplyr::transmute(term, estimate = round(estimate, 3), std.error = round(std.error, 3))

# -----------------------------------------------------------------------------
# [u31-dispersion]
#   1 · GLM Gamma > 1.2 Modelización y estimación > El modelo del caso
# -----------------------------------------------------------------------------
c(phi_pearson  = summary(m_gamma)$dispersion,
  cv_implicado = sqrt(summary(m_gamma)$dispersion)) |> round(3)

# -----------------------------------------------------------------------------
# [tbl-u31-coeficientes]
#   1 · GLM Gamma > 1.3 Interpretación
# -----------------------------------------------------------------------------
broom::tidy(m_gamma, exponentiate = TRUE) |>
  dplyr::transmute(term, `exp(beta)` = round(estimate, 3))

# -----------------------------------------------------------------------------
# [tbl-u31-interaccion]
#   1 · GLM Gamma > 1.3 Interpretación
# -----------------------------------------------------------------------------
emmeans::emmeans(m_gamma, ~ criticidad | fase_proceso, type = "response") |>
  as.data.frame() |>
  dplyr::transmute(fase_proceso, criticidad, coste_predicho = round(response)) |>
  tidyr::pivot_wider(names_from = criticidad, values_from = coste_predicho)

# -----------------------------------------------------------------------------
# [fig-u31-prediccion]
#   1 · GLM Gamma > 1.3 Interpretación
# -----------------------------------------------------------------------------
marginaleffects::plot_predictions(m_gamma,
    condition = c("antiguedad_anios", "criticidad", "fase_proceso")) +
  labs(x = "antigüedad de la máquina (años)", y = "coste medio predicho (€)",
       colour = NULL, fill = NULL)

# -----------------------------------------------------------------------------
# [tbl-u31-inferencia]
#   1 · GLM Gamma > 1.4 Inferencia y selección
# -----------------------------------------------------------------------------
broom::tidy(m_gamma, conf.int = TRUE) |>
  dplyr::transmute(term,
                   beta = round(estimate, 3), ee = round(std.error, 3),
                   t = round(statistic, 2), p = signif(p.value, 3),
                   `exp(beta)` = round(exp(estimate), 3),
                   ic95 = paste0("[", round(exp(conf.low), 2), ", ", round(exp(conf.high), 2), "]"))

# -----------------------------------------------------------------------------
# [u31-anova-f]
#   1 · GLM Gamma > 1.4 Inferencia y selección
# -----------------------------------------------------------------------------
anova(m_gamma, test = "F")

# -----------------------------------------------------------------------------
# [u31-lrt-interaccion]
#   1 · GLM Gamma > 1.4 Inferencia y selección
# -----------------------------------------------------------------------------
m_sin_int <- glm(coste_euros ~ tipo_averia + criticidad + fase_proceso + antiguedad_anios + carga,
                 family = Gamma(link = "log"), data = base)
anova(m_sin_int, m_gamma, test = "F")

# -----------------------------------------------------------------------------
# [u31-control]
#   1 · GLM Gamma > 1.4 Inferencia y selección
# -----------------------------------------------------------------------------
m_fab <- update(m_gamma, . ~ . + fabricante)
anova(m_gamma, m_fab, test = "F")

# -----------------------------------------------------------------------------
# [tbl-u31-aic]
#   1 · GLM Gamma > 1.4 Inferencia y selección
# -----------------------------------------------------------------------------
AIC(m_sin_int, m_gamma, m_fab) |> round(1)

# -----------------------------------------------------------------------------
# [fig-u31-dharma]
#   1 · GLM Gamma
#     > 1.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Residuos: cuáles usar
# -----------------------------------------------------------------------------
DHARMa::simulateResiduals(m_fab, n = 1000, plot = TRUE)

# -----------------------------------------------------------------------------
# [fig-u31-varianza]
#   1 · GLM Gamma
#     > 1.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > ¿Es correcta la función de varianza?
# -----------------------------------------------------------------------------
mv <- base |>
  dplyr::mutate(grupo = dplyr::ntile(fitted(m_fab), 12)) |>
  dplyr::group_by(grupo) |>
  dplyr::summarise(media = mean(coste_euros), varianza = var(coste_euros), .groups = "drop")

pendiente <- coef(lm(log(varianza) ~ log(media), data = mv))[2]

ggplot(mv, aes(media, varianza)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  scale_x_log10() + scale_y_log10() +
  labs(x = "media del grupo (€, log)", y = "varianza del grupo (log)",
       subtitle = paste0("pendiente estimada p = ", round(pendiente, 2)))

# -----------------------------------------------------------------------------
# [tbl-u31-verdad]
#   1 · GLM Gamma
#     > 1.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(banco, "verdad")$beta_coste

comparacion <- tibble::tibble(
  term = c("tipo_averiaElectrica", "tipo_averiaHidraulica", "tipo_averiaElectronica",
           "antiguedad_anios", "carga"),
  verdadero = c(verdad[["Electrica"]], verdad[["Hidraulica"]], verdad[["Electronica"]],
                verdad[["antiguedad"]], verdad[["carga"]]))

broom::tidy(m_fab, conf.int = TRUE) |>
  dplyr::inner_join(comparacion, by = "term") |>
  dplyr::transmute(term,
                   estimado = round(estimate, 3), verdadero = round(verdadero, 3),
                   ic = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]"),
                   cubre = ifelse(conf.low <= verdadero & verdadero <= conf.high, "sí", "NO"))

# -----------------------------------------------------------------------------
# [tbl-u31-verdad-crit]
#   1 · GLM Gamma
#     > 1.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
co <- coef(m_fab)
crit_log <- c(fase1 = co[["criticidadCritica"]],
              fase2 = co[["criticidadCritica"]] + co[["criticidadCritica:fase_procesofase2"]])
verd_log <- c(fase1 = verdad[["Critica"]],
              fase2 = verdad[["Critica"]] + verdad[["critxcuello.Critica"]])

tibble::tibble(
  fase             = c("fase 1", "fase 2"),
  log_estimado     = round(crit_log, 2),      log_verdadero    = round(verd_log, 2),
  factor_estimado  = round(exp(crit_log), 2), factor_verdadero = round(exp(verd_log), 2))

# -----------------------------------------------------------------------------
# [tbl-u31-verdad-fab]
#   1 · GLM Gamma
#     > 1.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
broom::tidy(m_fab) |>
  dplyr::filter(grepl("fabricante", term)) |>
  dplyr::transmute(term,
                   estimado = round(estimate, 3), verdadero = 0,
                   p_value = signif(p.value, 3),
                   veredicto = ifelse(p.value < 0.05, "«significativo» (falso +)", "no signif."))

# -----------------------------------------------------------------------------
# [tbl-u31-pred-marginal]
#   1 · GLM Gamma
#     > 1.5 Bondad de ajuste, diagnóstico, validación y predicción
#       > Predicción
# -----------------------------------------------------------------------------
marginaleffects::predictions(
  m_fab,
  newdata = tidyr::expand_grid(
    tipo_averia      = c("Mecanica", "Electronica"),
    criticidad       = "Critica", fase_proceso = "fase2",
    antiguedad_anios = mean(base$antiguedad_anios),
    carga            = mean(base$carga), fabricante = "A")) |>
  dplyr::transmute(tipo_averia,
                   coste_predicho = round(estimate),
                   ic95 = paste0("[", round(conf.low), ", ", round(conf.high), "]"))

