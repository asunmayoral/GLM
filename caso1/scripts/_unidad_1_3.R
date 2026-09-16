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
  ggeffects = "2.3.2", lmtest = "0.9.40", marginaleffects = "0.32.0",
  nnet = "7.3.20", ordinal = "2025.12.29", pROC = "1.19.0.1",
  patchwork = "1.3.2", performance = "0.17.0", readr = "2.2.0",
  see = "0.14.0", sessioninfo = "1.2.4", sure = "0.2.0", tibble = "3.3.1",
  tidyr = "1.3.2", tidyverse = "2.0.0")
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
# -----------------------------------------------------------------------------
tidy(fit_bin, exponentiate = TRUE, conf.int = TRUE)

# -----------------------------------------------------------------------------
# [u13-ame-binomial]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.1 Del dato individual al agrupado: respuesta binomial
#       > Del odds ratio a la probabilidad: el AME con datos agrupados
# -----------------------------------------------------------------------------
library(marginaleffects)

avg_comparisons(fit_bin, variables = "priorfrac", wts = "total")  # ponderado por el tamaño de celda
avg_comparisons(fit_ind, variables = "priorfrac")                 # el mismo AME, dato individual
avg_comparisons(fit_bin, variables = "priorfrac")                 # sin ponderar: promedia celdas, no mujeres

comparisons(fit_bin, variables = "priorfrac")   # el contraste celda a celda: no es constante

# -----------------------------------------------------------------------------
# [u13-deviance-celda]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Bondad de ajuste
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
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
gl <- df.residual(fit_bin)                       # n - p = nº de celdas - nº de parámetros
c(deviance = deviance(fit_bin),
  pearson  = sum(residuals(fit_bin, type = "pearson")^2),
  gl       = gl)

# p-valores del contraste de bondad de ajuste (H0: el modelo ajusta)
c(p_deviance = pchisq(deviance(fit_bin), gl, lower.tail = FALSE),
  p_pearson  = pchisq(sum(residuals(fit_bin, "pearson")^2), gl, lower.tail = FALSE))

# -----------------------------------------------------------------------------
# [fig-u13-residuos]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Residuos
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
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Residuos
# -----------------------------------------------------------------------------
diag_bin |>
  mutate(p_obs=Yes/total) |>
  select( priorfrac, momfrac, raterisk, m = total, p_obs, pred, w, h, r_std, cook,celda = cell)

c(referencia_cook = 4 / nrow(diag_bin))   # la línea de puntos del panel derecho

# -----------------------------------------------------------------------------
# [fig-u13-calibracion]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
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
# [u13-brier-binomial]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Calibración
#         > Un número para la calibración: el Brier score
# -----------------------------------------------------------------------------
m_i   <- glow_agg$total
pi_h  <- fitted(fit_bin)
p_obs <- glow_agg$Yes / m_i
n     <- sum(m_i)

c(binomial   = sum(glow_agg$Yes * (1 - pi_h)^2 + (m_i - glow_agg$Yes) * pi_h^2) / n,
  individual = mean((fitted(fit_ind) - glow$fractura01)^2))

# -----------------------------------------------------------------------------
# [u13-brier-descomposicion]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Calibración
#         > Un número para la calibración: el Brier score
# -----------------------------------------------------------------------------
c(calibracion = sum(m_i * (p_obs - pi_h)^2) / n,
  irreducible = sum(m_i * p_obs * (1 - p_obs)) / n,
  nulo        = mean(glow$fractura01) * (1 - mean(glow$fractura01)))

# -----------------------------------------------------------------------------
# [fig-u13-roc-binomial]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Calibración
#         > Discriminación: la ROC con datos agrupados
# -----------------------------------------------------------------------------
library(pROC)

# El binomial aplicado mujer a mujer: cada una recibe la probabilidad de su perfil
p_mujer <- predict(fit_bin, newdata = glow, type = "response")

roc_bin <- roc(glow$fractura01, p_mujer,            quiet = TRUE)
roc_ind <- roc(glow$fractura01, fitted(fit_ind),    quiet = TRUE)

c(auc_binomial            = as.numeric(auc(roc_bin)),
  auc_individual          = as.numeric(auc(roc_ind)),
  puntuaciones_distintas  = length(unique(p_mujer)))

ggroc(roc_bin) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  labs(x = "Especificidad", y = "Sensibilidad")

# -----------------------------------------------------------------------------
# [u13-sobredisp]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Sobredispersión
# -----------------------------------------------------------------------------
# Índice de sobredispersión (cociente de Pearson)
sum(residuals(fit_bin, type = "pearson")^2) / df.residual(fit_bin)

# -----------------------------------------------------------------------------
# [u13-nominal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Más de dos categorías sin orden: politómica nominal
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
#     > 3.3 Más de dos categorías sin orden: politómica nominal
# -----------------------------------------------------------------------------
# `ggeffects` no calcula intervalos para `multinom`; `predictions()` sí, y trae el nivel de la
# respuesta en la columna `group`.
pred_nom_No <- predictions(
  m_nom,
  newdata = datagrid(age       = seq(min(glow$age), max(glow$age), length.out = 100),
                     priorfrac = "No")
) |>
  mutate(nivel = factor(group, levels = c("Less", "Same", "Greater")))

ggplot(pred_nom_No, aes(age, estimate, colour = nivel, fill = nivel)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  labs(x = "Edad (años)", y = "Probabilidad predicha",
       colour = "raterisk", fill = "raterisk") +
  ylim(0, NA)

# -----------------------------------------------------------------------------
# [fig-u13-pred-nominal-priorfrac]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Más de dos categorías sin orden: politómica nominal
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
#     > 3.3 Más de dos categorías sin orden: politómica nominal
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
#     > 3.3 Más de dos categorías sin orden: politómica nominal
#       > El AME en el politómico: un efecto por categoría, y suman cero
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
#     > 3.3 Más de dos categorías sin orden: politómica nominal
#       > Bondad de ajuste, diagnóstico y comparación
#         > ¿Aporta algo el modelo? El contraste omnibus
# -----------------------------------------------------------------------------
m_nulo <- multinom(relevel(raterisk, ref = "Same") ~ 1, data = glow, trace = FALSE)

lmtest::lrtest(m_nulo, m_nom)

# -----------------------------------------------------------------------------
# [u13-nominal-seleccion]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Más de dos categorías sin orden: politómica nominal
#       > Bondad de ajuste, diagnóstico y comparación
#         > Selección de variables: ¿compensa mantener la edad?
# -----------------------------------------------------------------------------
m_sin_age <- multinom(relevel(raterisk, ref = "Same") ~ priorfrac, data = glow, trace = FALSE)

# Cada fila contrasta un modelo contra el ANTERIOR, no contra el nulo
lrt=lmtest::lrtest(m_nulo, m_sin_age, m_nom)
aic=AIC(m_nulo, m_sin_age, m_nom)
bic=BIC(m_nulo, m_sin_age, m_nom)
cbind(aic,bic,lrt)

# -----------------------------------------------------------------------------
# [fig-u13-calib-nominal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Más de dos categorías sin orden: politómica nominal
#       > Bondad de ajuste, diagnóstico y comparación
#         > Calibración por categoría
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
#     > 3.3 Más de dos categorías sin orden: politómica nominal
#       > Bondad de ajuste, diagnóstico y comparación
#         > Acierto clasificatorio: matriz de confusión y Brier
# -----------------------------------------------------------------------------
pred_clase <- predict(m_nom, type = "class")

table(Predicho = pred_clase, Observado = glow$raterisk)

c(acierto      = mean(pred_clase == glow$raterisk),
  clase_modal  = max(prop.table(table(glow$raterisk))))   # acierto sin modelo

# -----------------------------------------------------------------------------
# [u13-nominal-brier]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Más de dos categorías sin orden: politómica nominal
#       > Bondad de ajuste, diagnóstico y comparación
#         > Acierto clasificatorio: matriz de confusión y Brier
# -----------------------------------------------------------------------------
P   <- fitted(m_nom)                          # n x K; columnas = niveles de la respuesta
obs <- factor(glow$raterisk, levels = colnames(P))
Y   <- model.matrix(~ 0 + obs)                # one-hot
colnames(Y) <- levels(obs)

# Referencia sin covariables: las proporciones marginales para todas
P0 <- matrix(colMeans(Y), nrow = nrow(Y), ncol = ncol(Y), byrow = TRUE)

c(brier_modelo = mean(rowSums((P  - Y)^2)),
  brier_nulo   = mean(rowSums((P0 - Y)^2)))

# -----------------------------------------------------------------------------
# [u13-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
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
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > El AME en el ordinal: el mismo reparto, ahora ordenado
# -----------------------------------------------------------------------------
avg_slopes(m_ord)   # un efecto por categoría, con la columna `group`

avg_slopes(m_ord) |>
  group_by(term) |>
  summarise(suma_efectos = sum(estimate))

# -----------------------------------------------------------------------------
# [fig-u13-pred-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > El AME en el ordinal: el mismo reparto, ahora ordenado
# -----------------------------------------------------------------------------
library(ggeffects)
plot(ggpredict(m_ord, terms = "age [all]"))

# -----------------------------------------------------------------------------
# [u13-comparacion-nom-ord]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > El AME en el ordinal: el mismo reparto, ahora ordenado
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
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > Contrastar la proporcionalidad de odds
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
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > Contrastar la proporcionalidad de odds
# -----------------------------------------------------------------------------
library(brant)
brant(m_ord)          # Wald: omnibus + una fila por covariable; H0 = proporcionalidad

# -----------------------------------------------------------------------------
# [u13-nominal-test]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > Contrastar la proporcionalidad de odds
# -----------------------------------------------------------------------------
library(ordinal)
m_clm <- clm(raterisk ~ age + priorfrac, data = glow_ord)
nominal_test(m_clm)   # LRT: relaja la proporcionalidad término a término

# -----------------------------------------------------------------------------
# [fig-u13-surrogate]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > Diagnóstico con residuos surrogate
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
# [fig-u13-roc-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > Discriminación en el ordinal: ROC por corte y concordancia
# -----------------------------------------------------------------------------
probs_ord <- predict(m_ord, type = "probs")   # n x K

s_less <- 1 - probs_ord[, "Less"]       # Pr(Y > Less)
s_same <- probs_ord[, "Greater"]        # Pr(Y > Same)

roc_c1 <- roc(as.integer(glow_ord$raterisk > "Less"), s_less, quiet = TRUE)
roc_c2 <- roc(as.integer(glow_ord$raterisk > "Same"), s_same, quiet = TRUE)

c(auc_corte_Less   = as.numeric(auc(roc_c1)),
  auc_corte_Same   = as.numeric(auc(roc_c2)),
  correlacion_rangos = cor(s_less, s_same, method = "spearman"))

ggroc(list(`Y > Less` = roc_c1, `Y > Same` = roc_c2)) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  labs(x = "Especificidad", y = "Sensibilidad", colour = "Corte")

# -----------------------------------------------------------------------------
# [u13-concordancia]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
#       > Discriminación en el ordinal: ROC por corte y concordancia
# -----------------------------------------------------------------------------
y_num <- as.integer(glow_ord$raterisk)   # 1 = Less < 2 = Same < 3 = Greater

comparable <- outer(y_num,  y_num,  ">")   # pares en que i está por encima de j
concordante <- outer(s_same, s_same, ">")  # y el modelo le da más score
empate      <- outer(s_same, s_same, "==")

c(concordancia = (sum(concordante & comparable) + 0.5 * sum(empate & comparable)) /
                  sum(comparable)) |>
  round(4)


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
