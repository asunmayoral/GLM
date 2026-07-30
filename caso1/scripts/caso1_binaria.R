# =============================================================================
# Caso 1 · Documento maestro — Caso 1 · ¿Ocurre el evento? ¿Y cuándo?
# -----------------------------------------------------------------------------
# Código del propio caso1_binaria.qmd: setup, carga de datos y
# descriptivos de presentación del caso. El código de las unidades está
# en los scripts unidad_*.R de esta misma carpeta.
#
# GENERADO AUTOMÁTICAMENTE por _scripts/generar_scripts_unidades.R:
# no editar a mano; los cambios se pierden al regenerar. Edita el .qmd.
# =============================================================================

.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")

# -----------------------------------------------------------------------------
# [setup]  ·  
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# [carga-datos]  ·  Dato real — GLOW
# -----------------------------------------------------------------------------
data(glow500)
glow <- glow500 |>
  as_tibble() |>
  mutate(fractura01 = as.integer(fracture) - 1L)   # No -> 0, Yes -> 1

glow |> count(fracture) |> mutate(prop = round(n / sum(n), 3))

# -----------------------------------------------------------------------------
# [fig-glow-eda]  ·  Dato real — GLOW
# -----------------------------------------------------------------------------
# Covariables sustantivas de GLOW (dejo fuera los identificadores)
vars <- c("age", "weight", "height", "bmi", "fracscore",   # continuas / discretas
          "priorfrac", "premeno", "momfrac",               # factores de riesgo binarios
          "armassist", "smoke", "raterisk")                # binarios + ordinal

glow |>
  select(fracture, all_of(vars)) |>
  ggpairs(
    columns = vars,
    mapping = aes(color = fracture, alpha = 0.6),
    legend=1,
    upper = list(continuous = wrap("cor", size = 2.3),     # correlaciones (global y por grupo)
                 combo      = wrap("box_no_facet"),         # continua ~ factor: boxplots
                 discrete   = wrap("count")),               # factor ~ factor: recuento
    lower = list(continuous = wrap("points", alpha = 0.25, size = 0.4),
                 combo      = wrap("facethist", bins = 20),
                 discrete   = wrap("facetbar")),
    diag  = list(continuous = wrap("densityDiag", alpha = 0.5),
                 discrete   = wrap("barDiag")),
    progress = FALSE
  ) +
  theme_bw(base_size = 7) +
  theme(strip.text = element_text(size = 6))+
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# [cohorte-sim]  ·  Cohorte simulada
# -----------------------------------------------------------------------------
library(readr)
source(file.path(.raiz, "caso1", "R", "dgp_cohorte.R"))                        # define simular_cohorte(), cargar_cohorte(), expandir_persona_periodo()
cohorte <- cargar_cohorte()                      # lee datos/cohorte_*.rds si existe; si no (o si cambió el DGP), simula y guarda
# y guardamos las simulaciones
#write_csv(cohorte, "cohorte.csv")        # nivel individuo
# resumen
glimpse(cohorte)
head(cohorte)

# -----------------------------------------------------------------------------
# [fig-cohorte-eda]  ·  Cohorte simulada
# -----------------------------------------------------------------------------
# Preparo factores legibles (x2, tiempo y el desenlace como categóricos)
cohorte_eda <- cohorte |>
  mutate(
    evento = factor(evento, levels = c(0, 1), labels = c("Sin fractura", "Fractura")),
    x2     = factor(x2,     levels = c(0, 1), labels = c("No", "Sí")),
    tiempo = factor(tiempo, levels = 1:6)
  )

# Variables sustantivas (fuera 'id', 'centro' y 'ever' = duplicado de 'evento')
vars <- c("x1", "x2", "tiempo", "clase_nom", "sever_ord")

cohorte_eda |>
  select(evento, all_of(vars)) |>
  ggpairs(
    columns = vars,
    mapping = aes(color = evento, alpha = 0.6),
    legend=1,
    upper = list(continuous = wrap("cor", size = 2.5),
                 combo      = wrap("box_no_facet"),
                 discrete   = wrap("count")),
    lower = list(continuous = wrap("points", alpha = 0.25, size = 0.4),
                 combo      = wrap("facethist", bins = 20),
                 discrete   = wrap("facetbar")),
    diag  = list(continuous = wrap("densityDiag", alpha = 0.5),
                 discrete   = wrap("barDiag")),
    progress = FALSE
  ) +
  theme_bw(base_size = 8)+
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# [session-info]  ·  Reproducibilidad
# -----------------------------------------------------------------------------
sessionInfo()
# renv::snapshot()   # fijar el estado del entorno al cerrar el caso

