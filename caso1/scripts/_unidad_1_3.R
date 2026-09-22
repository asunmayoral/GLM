# =============================================================================
# Caso 1 · Unidad 1.3 — 3 · Extensión de la respuesta binaria: binomial y politómica
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_1_3.qmd.
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
  DHARMa = "0.5.0", GGally = "2.4.0", MASS = "7.3.65", MuMIn = "1.48.19",
  aplore3 = "0.9", arm = "1.15.3", brant = "0.3.0", broom = "1.0.13",
  lmtest = "0.9.40", marginaleffects = "0.32.0", nnet = "7.3.20",
  ordinal = "2025.12.29", pROC = "1.19.0.1", patchwork = "1.3.2",
  performance = "0.17.0", readr = "2.2.0", see = "0.14.0",
  sessioninfo = "1.2.4", sure = "0.2.0", tibble = "3.3.1", tidyr = "1.3.2",
  tidyverse = "2.0.0", yardstick = "1.4.0")
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
# Núcleo de software (cada unidad carga además lo suyo: lme4, nnet, ordinal,
# pROC, performance, DHARMa, marginaleffects, car, survival...).

library(broom)
library(aplore3)
library(patchwork)
library(see)
library(DHARMa)
library(arm)
library(performance)
library(tidyverse)
library(MuMIn)
library(readr)
library(GGally)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

data(glow500)
glow <- glow500 |>
  as_tibble() |>
  mutate(fractura01 = as.integer(fracture) - 1L)   # No -> 0, Yes -> 1

glow |> count(fracture) |> mutate(prop = round(n / sum(n), 3))

library(readr)
source(url_glm("caso1/R/dgp_cohorte.R"))   # funciones del proceso generador
cohorte <- leer_datos_glm("caso1/datos/cohorte_20252026.rds")   # cohorte simulada del curso
# y guardamos las simulaciones
#write_csv(cohorte, "cohorte.csv")        # nivel individuo
# resumen
glimpse(cohorte)
head(cohorte)

# -----------------------------------------------------------------------------
# [u13-agrupar]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
# -----------------------------------------------------------------------------
glow_agg <- glow |>
  count(priorfrac, momfrac, raterisk, fracture) |>
  tidyr::pivot_wider(names_from = fracture, values_from = n, values_fill = 0) |>
  mutate(total = No + Yes)            # ensayos por celda; Yes = nº de fracturas
glow_agg

# -----------------------------------------------------------------------------
# [fig-u13-barras]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
# -----------------------------------------------------------------------------
glow_agg |>
  mutate(prop = Yes / total) |>
  ggplot(aes(x = priorfrac, y = prop, fill = momfrac)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = total), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  facet_wrap(~ raterisk, labeller = label_both) +
  labs(x = "Fractura previa (priorfrac)", y = "Proporción de fractura (Yes/total)",
       fill = "Antecedente materno\n(momfrac)") +
  ylim(0, NA)

# -----------------------------------------------------------------------------
# [u13-binomial-fit]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
fit_bin <- glm(cbind(Yes, No) ~ priorfrac + momfrac + raterisk,
               family = binomial, data = glow_agg)
fit_ind <- glm(fracture ~ priorfrac + momfrac + raterisk,
               family = binomial, data = glow)

cbind(agrupado = coef(fit_bin), individual = coef(fit_ind))   # idénticos

# -----------------------------------------------------------------------------
# [u13-or-ic]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
tidy(fit_bin, exponentiate = TRUE, conf.int = TRUE)

# -----------------------------------------------------------------------------
# [u13-ame-binomial]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
library(marginaleffects)
# AME en el modelo binomial (ponderado por el tamaño de las celdas)
avg_comparisons(fit_bin, variables = "priorfrac", wts = "total")  # ponderado por el tamaño de celda
# AME en el modelo Bernoulli"
avg_comparisons(fit_ind, variables = "priorfrac")                 # el mismo AME, dato individual

# Comparación de las probabilidades (Yes-No), celda a celda"
comparisons(fit_bin, variables = "priorfrac")   # el contraste celda a celda: no es constante

# -----------------------------------------------------------------------------
# [(sin etiqueta)]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
avg_predictions(fit_bin, variables = "priorfrac", wts = "total")

# -----------------------------------------------------------------------------
# [u13-deviance-celda]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Bondad de ajuste y diagnóstico
#         > Estadísticos globales: deviance y Pearson
# -----------------------------------------------------------------------------
aporta <- glow_agg |>
  mutate(
    esperado = total * fitted(fit_bin),
    # El ifelse implementa el convenio 0*log(0) = 0: la celda con Yes = 0 lo necesita.
    d_i  = 2 * (ifelse(Yes > 0, Yes * log(Yes / esperado), 0) +
                ifelse(No  > 0, No  * log(No  / (total - esperado)), 0)),
    r_Di = sign(Yes - esperado) * sqrt(d_i)
  )

aporta |> select(priorfrac, momfrac, raterisk, y = Yes, m = total, esperado, d_i, r_Di)

c(suma_d_i = sum(aporta$d_i), deviance = deviance(fit_bin))   # la suma ES la deviance

# -----------------------------------------------------------------------------
# [u13-gof]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Bondad de ajuste y diagnóstico
#         > Estadísticos globales: deviance y Pearson
# -----------------------------------------------------------------------------
gl <- df.residual(fit_bin)                       # k - p = nº de celdas - nº de parámetros
c(deviance = deviance(fit_bin),
  pearson  = sum(residuals(fit_bin, type = "pearson")^2),
  gl       = gl)

# p-valores del contraste de bondad de ajuste (H0: el modelo ajusta)
c(p_deviance = pchisq(deviance(fit_bin), gl, lower.tail = FALSE),
  p_pearson  = pchisq(sum(residuals(fit_bin, "pearson")^2), gl, lower.tail = FALSE))

# -----------------------------------------------------------------------------
# [fig-u13-residuos]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Bondad de ajuste y diagnóstico
#         > Residuos e influencia
# -----------------------------------------------------------------------------
#library(patchwork)

diag_bin <- glow_agg |>
  mutate(pred  = fitted(fit_bin),
         w     = total * pred * (1 - pred),   # peso de trabajo del IWLS
         r_std = rstandard(fit_bin, type = "pearson"),
         cook  = cooks.distance(fit_bin),
         h     = hatvalues(fit_bin),
         cell  = row_number())

p_res <- ggplot(diag_bin, aes(pred, r_std, size = total)) +
  geom_hline(yintercept = c(-2, 0, 2), linetype = c("dotted", "dashed", "dotted")) +
  geom_point(alpha = 0.7) +
  labs(x = "Probabilidad ajustada", y = "Residuo de Pearson estandarizado",
       size = "n por celda")

p_cook <- ggplot(diag_bin, aes(cell, cook, size = total)) +
  geom_hline(yintercept = 4 / nrow(diag_bin), linetype = "dotted") +
  geom_segment(aes(xend = cell, yend = 0), linewidth = 0.3) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(breaks = diag_bin$cell) +
  labs(x = "Celda", y = "Distancia de Cook", size = "n por celda")

p_res + p_cook + plot_layout(guides = "collect")

# -----------------------------------------------------------------------------
# [u13-diagnosticos-tabla]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Bondad de ajuste y diagnóstico
#         > Residuos e influencia
# -----------------------------------------------------------------------------
diag_bin |>
  mutate(p_obs=Yes/total) |>
  select( priorfrac, momfrac, raterisk, m = total, p_obs, pred, w, h, r_std, cook,celda = cell)

c(referencia_cook = 4 / nrow(diag_bin))   # 4/k: nrow() cuenta CELDAS, no mujeres

# -----------------------------------------------------------------------------
# [u13-sobredisp]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Bondad de ajuste y diagnóstico
#         > Sobredispersión
# -----------------------------------------------------------------------------
# Índice de sobredispersión (cociente de Pearson)
sum(residuals(fit_bin, type = "pearson")^2) / df.residual(fit_bin)

# -----------------------------------------------------------------------------
# [fig-u13-calibracion]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Calibración
# -----------------------------------------------------------------------------
# Calibración DIRECTA: cada celda = (probabilidad predicha, proporción observada)
calib_bin <- glow_agg |>
  mutate(pred = fitted(fit_bin), obs = Yes / total)

ggplot(calib_bin, aes(pred, obs, size = total)) +
  geom_abline(linetype = "dashed") +                 # calibración perfecta
  geom_point(alpha = 0.7) +
  labs(x = "Probabilidad predicha", y = "Proporción observada (Yes/total)",
       size = "n por celda")

# -----------------------------------------------------------------------------
# [u13-ecm-calibracion]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Calibración
#         > Un número para la calibración
# -----------------------------------------------------------------------------
m_i  <- calib_bin$total
pi_h <- calib_bin$pred
n    <- sum(m_i)
ecm  <- sum(m_i * (calib_bin$obs - pi_h)^2) / n

c(ecm_calibracion = ecm, raiz = sqrt(ecm))

# -----------------------------------------------------------------------------
# [fig-u13-roc-binomial]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Discriminación y clasificación
# -----------------------------------------------------------------------------
library(pROC)

# Cada celda aporta sus Yes casos y sus No controles, todos con la misma probabilidad
# predicha: desplegar la tabla devuelve los ensayos sin recurrir a la cohorte individual
ensayos <- glow_agg |>
  mutate(pred = fitted(fit_bin)) |>
  tidyr::pivot_longer(c(Yes, No), names_to = "fractura", values_to = "n") |>
  tidyr::uncount(n) |>
  mutate(y = as.integer(fractura == "Yes"))

roc_bin <- roc(ensayos$y, ensayos$pred,          quiet = TRUE)
roc_ind <- roc(glow$fractura01, fitted(fit_ind), quiet = TRUE)

c(ensayos                 = nrow(ensayos),
  puntuaciones_distintas  = length(unique(ensayos$pred)),
  auc_agrupado            = as.numeric(auc(roc_bin)),
  auc_individual          = as.numeric(auc(roc_ind)))

ggroc(roc_bin) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  labs(x = "Especificidad", y = "Sensibilidad")

# -----------------------------------------------------------------------------
# [u13-confusion-binomial]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Discriminación y clasificación
# -----------------------------------------------------------------------------
library(yardstick)

eval_agg <- glow_agg |>
  mutate(pred = factor(if_else(fitted(fit_bin) >= 0.5, "Yes", "No"), levels = c("No", "Yes"))) |>
  tidyr::pivot_longer(c(Yes, No), names_to = "obs", values_to = "n") |>
  mutate(obs = factor(obs, levels = c("No", "Yes")))

conf_mat(eval_agg, truth = obs, estimate = pred, case_weights = n)

metric_set(accuracy, sensitivity, specificity)(
  eval_agg, truth = obs, estimate = pred, case_weights = n, event_level = "second")

# -----------------------------------------------------------------------------
# [u13-youden-binomial]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Discriminación y clasificación
# -----------------------------------------------------------------------------
umbral <- coords(roc_bin, "best", best.method = "youden",
                 ret = c("threshold", "sensitivity", "specificity"))
umbral

# -----------------------------------------------------------------------------
# [u13-confusion-youden]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Discriminación y clasificación
# -----------------------------------------------------------------------------
c_y <- umbral$threshold

eval_youden <- glow_agg |>
  mutate(pred = factor(if_else(fitted(fit_bin) >= c_y, "Yes", "No"), levels = c("No", "Yes"))) |>
  tidyr::pivot_longer(c(Yes, No), names_to = "obs", values_to = "n") |>
  mutate(obs = factor(obs, levels = c("No", "Yes")))

conf_mat(eval_youden, truth = obs, estimate = pred, case_weights = n)

metric_set(accuracy, sensitivity, specificity)(
  eval_youden, truth = obs, estimate = pred, case_weights = n, event_level = "second")

# -----------------------------------------------------------------------------
# [u13-nominal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Más de dos categorías sin orden: politómica nominal
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
library(nnet)
m_nom <- multinom(relevel(raterisk, ref = "Same") ~ age + priorfrac,
                  data = glow, trace = FALSE)
summary(m_nom) # dos bloques de coeficientes: Less|Same y Greater|Same
tidy(m_nom) # visualización ordenada, con z y p
tidy(m_nom, exponentiate = TRUE, conf.int = TRUE)   # odds ratios relativos, con intervalo

# -----------------------------------------------------------------------------
# [fig-u13-pred-nominal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Más de dos categorías sin orden: politómica nominal
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
# predictions() añade el intervalo de confianza, que predict() no da para multinom.
# La columna `group` trae el nivel de la respuesta.
pred_nom <- predictions(
  m_nom,
  newdata = datagrid(age       = seq(min(glow$age), max(glow$age), length.out = 100),
                     priorfrac = levels(glow$priorfrac))
) |>
  mutate(nivel = factor(group, levels = c("Less", "Same", "Greater")))

ggplot(pred_nom, aes(age, estimate, colour = nivel, fill = nivel)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ priorfrac, labeller = label_both) +
  labs(x = "Edad (años)", y = "Probabilidad predicha",
       colour = "raterisk", fill = "raterisk") +
  ylim(0, NA)

# -----------------------------------------------------------------------------
# [u13-pred-nominal-tabla]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Más de dos categorías sin orden: politómica nominal
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
nd_tabla <- expand.grid(
  age       = c(55, 70, 90),
  priorfrac = factor(levels(glow$priorfrac), levels = levels(glow$priorfrac))
)

bind_cols(nd_tabla, as_tibble(predict(m_nom, newdata = nd_tabla, type = "probs"))) |>
  relocate(Less, Same, Greater, .after = priorfrac)

# -----------------------------------------------------------------------------
# [u13-ame-nominal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Más de dos categorías sin orden: politómica nominal
#       > Ajuste e interpretación
#         > El AME en el politómico: un efecto por categoría, y suman cero
# -----------------------------------------------------------------------------
library(marginaleffects)

avg_slopes(m_nom)   # una fila por (término, categoría); la columna `group` da el nivel

# Comprobación del reparto: los efectos de cada predictor suman cero
avg_slopes(m_nom) |>
  group_by(term) |>
  summarise(suma_efectos = sum(estimate))

# -----------------------------------------------------------------------------
# [u13-nominal-omnibus]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Más de dos categorías sin orden: politómica nominal
#       > Bondad de ajuste y diagnóstico
#         > ¿Aporta algo el modelo? El contraste omnibus
# -----------------------------------------------------------------------------
m_nulo <- multinom(relevel(raterisk, ref = "Same") ~ 1, data = glow, trace = FALSE)

lmtest::lrtest(m_nulo, m_nom)

# -----------------------------------------------------------------------------
# [u13-nominal-seleccion]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Más de dos categorías sin orden: politómica nominal
#       > Bondad de ajuste y diagnóstico
#         > Selección de variables: ¿compensa mantener la edad?
# -----------------------------------------------------------------------------
m_sin_age <- multinom(relevel(raterisk, ref = "Same") ~ priorfrac, data = glow, trace = FALSE)

# Cada fila contrasta un modelo contra el ANTERIOR, no contra el nulo
lrt <- lmtest::lrtest(m_nulo, m_sin_age, m_nom)

data.frame(
  modelo = c("nulo", "sin age", "completo"),
  par    = lrt$`#Df`,                            # nº de parámetros del modelo
  AIC    = AIC(m_nulo, m_sin_age, m_nom)$AIC,
  BIC    = BIC(m_nulo, m_sin_age, m_nom)$BIC,
  gl     = lrt$Df,                               # gl del contraste con la fila anterior
  Chisq  = lrt$Chisq,
  p      = lrt$`Pr(>Chisq)`
)

# -----------------------------------------------------------------------------
# [fig-u13-calib-nominal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Más de dos categorías sin orden: politómica nominal
#       > Calibración
# -----------------------------------------------------------------------------
calib_nom <- as_tibble(fitted(m_nom)) |>
  mutate(obs = glow$raterisk) |>
  pivot_longer(-obs, names_to = "nivel", values_to = "pred") |>
  mutate(ocurre = as.integer(obs == nivel)) |>
  group_by(nivel) |>
  mutate(tramo = ntile(pred, 5)) |>
  group_by(nivel, tramo) |>
  summarise(pred_media = mean(pred), obs_frec = mean(ocurre), n = n(), .groups = "drop") |>
  mutate(nivel = factor(nivel, levels = c("Less", "Same", "Greater")))

ggplot(calib_nom, aes(pred_media, obs_frec)) +
  geom_abline(linetype = "dashed") +
  geom_line(linewidth = 0.3) +
  geom_point(aes(size = n), alpha = 0.8) +
  facet_wrap(~ nivel) +
  labs(x = "Probabilidad media predicha", y = "Frecuencia observada", size = "n del tramo")

# -----------------------------------------------------------------------------
# [u13-nominal-confusion]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Más de dos categorías sin orden: politómica nominal
#       > Discriminación y clasificación
# -----------------------------------------------------------------------------
# `predict` hereda los niveles del relevel, así que los realineamos: solo con truth y
# estimate en el mismo orden los aciertos caen sobre la diagonal
library(yardstick)
eval_nom <- tibble(
  obs  = factor(glow$raterisk,                  levels = c("Less", "Same", "Greater")),
  pred = factor(predict(m_nom, type = "class"), levels = c("Less", "Same", "Greater"))
)

conf_mat(eval_nom, truth = obs, estimate = pred, dnn = c("Predicho", "Observado"))

# En relativo por columna: qué hace el modelo con cada categoría realmente observada
prop.table(table(Predicho = eval_nom$pred, Observado = eval_nom$obs), margin = 2) |>
  round(3)

# En relativo por fila: cuánto vale cada predicción que el modelo emite
prop.table(table(Predicho = eval_nom$pred, Observado = eval_nom$obs), margin = 1) |>
  round(3)

c(acierto     = accuracy(eval_nom, truth = obs, estimate = pred)$.estimate,
  clase_modal = max(prop.table(table(glow$raterisk))),   # acierto sin modelo
  kappa       = kap(eval_nom, truth = obs, estimate = pred)$.estimate)

# -----------------------------------------------------------------------------
# [u13-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
glow_ord <- glow |>
  mutate(raterisk = ordered(raterisk, levels = c("Less", "Same", "Greater")))
m_ord <- MASS::polr(raterisk ~ age + priorfrac, data = glow_ord, Hess = TRUE)
tidy(m_ord)        # dos umbrales (theta) y los efectos (beta), comunes a ambos cortes
tidy(m_ord, exponentiate = TRUE, conf.int = TRUE) |>
  filter(coef.type == "coefficient")   # OR acumulados con IC; los umbrales no son OR

# -----------------------------------------------------------------------------
# [u13-ame-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Ajuste e interpretación
#         > El AME en el ordinal: el mismo reparto, ahora ordenado
# -----------------------------------------------------------------------------
avg_slopes(m_ord)   # un efecto por categoría, con la columna `group`

avg_slopes(m_ord) |>
  group_by(term) |>
  summarise(suma_efectos = sum(estimate))

# -----------------------------------------------------------------------------
# [fig-u13-pred-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Ajuste e interpretación
#         > El AME en el ordinal: el mismo reparto, ahora ordenado
# -----------------------------------------------------------------------------
pred_ord <- predictions(
  m_ord,
  newdata = datagrid(age       = seq(min(glow_ord$age), max(glow_ord$age), length.out = 100),
                     priorfrac = levels(glow_ord$priorfrac))
) |>
  mutate(nivel = factor(group, levels = c("Less", "Same", "Greater")))

ggplot(pred_ord, aes(age, estimate, colour = nivel, fill = nivel)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ priorfrac, labeller = label_both) +
  labs(x = "Edad (años)", y = "Probabilidad predicha",
       colour = "raterisk", fill = "raterisk") +
  ylim(0, NA)

# -----------------------------------------------------------------------------
# [u13-ordinal-omnibus]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Bondad de ajuste y diagnóstico
#         > ¿Aporta algo el modelo? El contraste omnibus
# -----------------------------------------------------------------------------
m_ord_nulo <- MASS::polr(raterisk ~ 1, data = glow_ord, Hess = TRUE)

lmtest::lrtest(m_ord_nulo, m_ord)

# -----------------------------------------------------------------------------
# [u13-comparacion-nom-ord]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Bondad de ajuste y diagnóstico
#         > ¿Compensa el ordinal frente al nominal?
# -----------------------------------------------------------------------------
# Ajuste y parsimonia (el ordinal usa menos parámetros)
ajuste <- tibble::tibble(
  Modelo     = c("Nominal (multinom)", "Ordinal (polr)"),
  Parametros = c(length(coef(m_nom)), length(coef(m_ord)) + length(m_ord$zeta)),
  logLik     = c(as.numeric(logLik(m_nom)), as.numeric(logLik(m_ord))),
  AIC        = c(AIC(m_nom), AIC(m_ord)),
  BIC        = c(BIC(m_nom), BIC(m_ord))
)

# Odds ratios: el nominal da uno por categoría; el ordinal, uno (acumulado) común
or_nom <- exp(coef(m_nom))      # filas Less/Greater x (Intercept, age, priorfracYes)
or_ord <- exp(coef(m_ord))      # age, priorfracYes (acumulado)
discrepancias <- tibble::tibble(
  Covariable              = c("age", "priorfracYes"),
  `Nominal Less|Same`     = or_nom["Less",    c("age", "priorfracYes")],
  `Nominal Greater|Same`  = or_nom["Greater", c("age", "priorfracYes")],
  `Ordinal (acumulado)`   = or_ord[c("age", "priorfracYes")]
)

ajuste
discrepancias

# -----------------------------------------------------------------------------
# [u13-proporcionalidad]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Bondad de ajuste y diagnóstico
#         > ¿Se sostiene la proporcionalidad de odds?
# -----------------------------------------------------------------------------
glow_cortes <- glow_ord |>
  mutate(corte1 = as.integer(raterisk <= "Less"),   # P(Y <= Less)
         corte2 = as.integer(raterisk <= "Same"))   # P(Y <= Same)

c1 <- glm(corte1 ~ age + priorfrac, family = binomial, data = glow_cortes)
c2 <- glm(corte2 ~ age + priorfrac, family = binomial, data = glow_cortes)

# polr parametriza logit P(Y<=k) = theta_k - eta, así que estas logísticas estiman -beta
rbind(corte_1            = coef(c1)[-1],
      corte_2            = coef(c2)[-1],
      `-beta (ordinal)`  = -coef(m_ord))

# -----------------------------------------------------------------------------
# [u13-brant]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Bondad de ajuste y diagnóstico
#         > ¿Se sostiene la proporcionalidad de odds?
# -----------------------------------------------------------------------------
library(brant)
brant(m_ord)          # Wald: omnibus + una fila por covariable; H0 = proporcionalidad

# -----------------------------------------------------------------------------
# [u13-nominal-test]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Bondad de ajuste y diagnóstico
#         > ¿Se sostiene la proporcionalidad de odds?
# -----------------------------------------------------------------------------
library(ordinal)
m_clm <- clm(raterisk ~ age + priorfrac, data = glow_ord)
nominal_test(m_clm)   # LRT: relaja la proporcionalidad término a término

# -----------------------------------------------------------------------------
# [fig-u13-surrogate]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Bondad de ajuste y diagnóstico
#         > ¿Ajusta el modelo? El camino surrogate
# -----------------------------------------------------------------------------
library(sure)

set.seed(SEMILLA_CURSO)   # el surrogate se sortea: sin semilla el gráfico cambia en cada render

diag_ord <- glow_ord |>
  mutate(r = resids(m_ord))   # un sorteo de residuos surrogate, uno por mujer

p_qq <- ggplot(diag_ord, aes(sample = r)) +
  stat_qq(distribution = qlogis, alpha = 0.5) +
  stat_qq_line(distribution = qlogis, linetype = "dashed") +
  labs(x = "Cuantiles teóricos (logística)", y = "Residuo surrogate")

p_age <- ggplot(diag_ord, aes(age, r)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  labs(x = "Edad (años)", y = "Residuo surrogate")

p_pf <- ggplot(diag_ord, aes(priorfrac, r)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() +
  labs(x = "Fractura previa", y = "Residuo surrogate")

p_qq + p_age + p_pf

# -----------------------------------------------------------------------------
# [fig-u13-calib-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Calibración
# -----------------------------------------------------------------------------
calib_ord <- as_tibble(predict(m_ord, type = "probs")) |>
  mutate(obs = as.character(glow_ord$raterisk)) |>
  pivot_longer(-obs, names_to = "nivel", values_to = "pred") |>
  mutate(ocurre = as.integer(obs == nivel)) |>
  group_by(nivel) |>
  mutate(tramo = ntile(pred, 5)) |>
  group_by(nivel, tramo) |>
  summarise(pred_media = mean(pred), obs_frec = mean(ocurre), n = n(), .groups = "drop") |>
  mutate(nivel = factor(nivel, levels = c("Less", "Same", "Greater")))

ggplot(calib_ord, aes(pred_media, obs_frec)) +
  geom_abline(linetype = "dashed") +
  geom_line(linewidth = 0.3) +
  geom_point(aes(size = n), alpha = 0.8) +
  facet_wrap(~ nivel) +
  labs(x = "Probabilidad media predicha", y = "Frecuencia observada", size = "n del tramo")

# -----------------------------------------------------------------------------
# [u13-hl-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Calibración
# -----------------------------------------------------------------------------
# H0: el modelo ajusta. Interesa un p alto.
generalhoslem::lipsitz.test(m_ord)                          # agrupa por riesgo predicho (g = 10)
generalhoslem::pulkrob.chisq(m_ord, catvars = "priorfrac")  # agrupa por patrón categórico

# -----------------------------------------------------------------------------
# [fig-u13-roc-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Discriminación y clasificación
# -----------------------------------------------------------------------------
probs_ord <- predict(m_ord, type = "probs")   # n x K

s_less <- 1 - probs_ord[, "Less"]       # Pr(Y > Less)
s_same <- probs_ord[, "Greater"]        # Pr(Y > Same)

roc_c1 <- roc(as.integer(glow_ord$raterisk > "Less"), s_less, quiet = TRUE)
roc_c2 <- roc(as.integer(glow_ord$raterisk > "Same"), s_same, quiet = TRUE)

c(auc_corte_Less = as.numeric(auc(roc_c1)),
  auc_corte_Same = as.numeric(auc(roc_c2)))

ggroc(list(`Y > Less` = roc_c1, `Y > Same` = roc_c2)) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  labs(x = "Especificidad", y = "Sensibilidad", colour = "Corte")

# -----------------------------------------------------------------------------
# [u13-concordancia]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Discriminación y clasificación
# -----------------------------------------------------------------------------
y_num <- as.integer(glow_ord$raterisk)   # 1 = Less < 2 = Same < 3 = Greater
# s_same = Pr(Y > Same), el score del segundo corte, definido en el chunk de la ROC

# OBSERVADO: el par (i,j) es comparable si i está en una categoría real más alta que j
comparable  <- outer(y_num,  y_num,  ">")
# PREDICHO: ¿el modelo le da más score a i que a j?, ¿o se lo da igual?
concordante <- outer(s_same, s_same, ">")
empate      <- outer(s_same, s_same, "==")

c(concordancia = (sum(concordante & comparable) + 0.5 * sum(empate & comparable)) /
                  sum(comparable)) |>
  round(4)

# Comprobación contra la implementación de referencia
Hmisc::rcorr.cens(s_same, y_num)[c("C Index", "Relevant Pairs", "Concordant")]

# -----------------------------------------------------------------------------
# [u13-confusion-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Categorías ordenadas: el modelo de odds proporcionales
#       > Discriminación y clasificación
# -----------------------------------------------------------------------------
eval_ord <- tibble(
  obs  = factor(glow_ord$raterisk,              levels = c("Less", "Same", "Greater")),
  pred = factor(predict(m_ord, type = "class"), levels = c("Less", "Same", "Greater"))
)

conf_mat(eval_ord, truth = obs, estimate = pred, dnn = c("Predicho", "Observado"))

c(acierto     = accuracy(eval_ord, truth = obs, estimate = pred)$.estimate,
  clase_modal = max(prop.table(table(glow_ord$raterisk))))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
