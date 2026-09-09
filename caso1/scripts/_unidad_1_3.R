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
  aplore3 = "0.9", arm = "1.15.3", broom = "1.0.13", ggeffects = "2.3.2",
  nnet = "7.3.20", patchwork = "1.3.2", performance = "0.17.0",
  readr = "2.2.0", see = "0.14.0", sessioninfo = "1.2.4", tibble = "3.3.1",
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
# [fig-u13-residuos]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.2 Bondad de ajuste y diagnóstico en datos binomiales
#       > Residuos
# -----------------------------------------------------------------------------
#library(patchwork)

diag_bin <- glow_agg |>
  mutate(pred  = fitted(fit_bin),
         r_std = rstandard(fit_bin, type = "pearson"),
         cook  = cooks.distance(fit_bin),
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
tidy(m_nom) # visualización ordenada
exp(coef(m_nom))      # odds ratios relativos

# -----------------------------------------------------------------------------
# [fig-u13-pred-nominal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.3 Más de dos categorías sin orden: politómica nominal
# -----------------------------------------------------------------------------
library(ggeffects)
plot(ggpredict(m_nom, terms = "age [all]"))

# -----------------------------------------------------------------------------
# [u13-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
# -----------------------------------------------------------------------------
glow_ord <- glow |>
  mutate(raterisk = ordered(raterisk, levels = c("Less", "Same", "Greater")))
m_ord <- MASS::polr(raterisk ~ age + priorfrac, data = glow_ord, Hess = TRUE)
tidy(m_ord)        # dos umbrales (theta) y los efectos (beta), comunes a ambos cortes
exp(coef(m_ord))      # odds ratios acumulados

# -----------------------------------------------------------------------------
# [fig-u13-pred-ordinal]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
# -----------------------------------------------------------------------------
library(ggeffects)
plot(ggpredict(m_ord, terms = "age [all]"))

# -----------------------------------------------------------------------------
# [u13-comparacion-nom-ord]
#   3 · Extensión de la respuesta binaria: binomial y politómica
#     > 3.4 Categorías ordenadas: el modelo de odds proporcionales
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


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
