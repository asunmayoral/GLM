# =============================================================================
# Script extraído de: caso1_binaria_tiempo_evento.qmd
# Sintaxis de los chunks R, con etiquetas y estructura de secciones.
# =============================================================================

# -----------------------------------------------------------------------------
# Chunk 1: setup
# Sección: (sin sección)
# Identificadores: (sin identificador)
# Cabecera original: ```{r}
# -----------------------------------------------------------------------------
#| label: setup
#| include: false

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

#
# ## Resultados de aprendizaje del Caso 1
# Identificador de sección: sec-ra
# Ruta: Resultados de aprendizaje del Caso 1

#
# ## Mapa del caso
# Ruta: Mapa del caso

#
# # Presentación del caso
# Identificador de sección: sec-presentacion
# Ruta: Presentación del caso

#
# ## Dato real — GLOW
# Identificador de sección: sec-datos-glow
# Ruta: Presentación del caso > Dato real — GLOW

# -----------------------------------------------------------------------------
# Chunk 2: carga-datos
# Sección: Presentación del caso > Dato real — GLOW
# Identificadores: sec-presentacion > sec-datos-glow
# Cabecera original: ```{r}
# -----------------------------------------------------------------------------
#| label: carga-datos
data(glow500)
glow <- glow500 |>
  as_tibble() |>
  mutate(fractura01 = as.integer(fracture) - 1L)   # No -> 0, Yes -> 1

glow |> count(fracture) |> mutate(prop = round(n / sum(n), 3))

# -----------------------------------------------------------------------------
# Chunk 3: fig-glow-eda
# Sección: Presentación del caso > Dato real — GLOW
# Identificadores: sec-presentacion > sec-datos-glow
# Cabecera original: ```{r}
# -----------------------------------------------------------------------------
#| label: fig-glow-eda
#| fig-width: 10
#| fig-height: 10
#| out-width: "100%"
#| fig-cap: "Descripción de GLOW. En la diagonal distribución de las variables, diferenciadas en color por `evento`. En los cruces la relación entre ellas."

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

#
# ## Cohorte simulada
# Identificador de sección: sec-datos-cohorte
# Ruta: Presentación del caso > Cohorte simulada

# -----------------------------------------------------------------------------
# Chunk 4: cohorte-sim
# Sección: Presentación del caso > Cohorte simulada
# Identificadores: sec-presentacion > sec-datos-cohorte
# Cabecera original: ```{r}
# -----------------------------------------------------------------------------
#| label: cohorte-sim
#| 
library(readr)
source("R/dgp_cohorte.R")                        # define simular_cohorte(), expandir_persona_periodo()
cohorte <- simular_cohorte()                     # nivel individuo (binaria, nominal, ordinal)
# y guardamos las simulaciones
#write_csv(cohorte, "cohorte.csv")        # nivel individuo
# resumen
glimpse(cohorte)
head(cohorte)

# -----------------------------------------------------------------------------
# Chunk 5: fig-cohorte-eda
# Sección: Presentación del caso > Cohorte simulada
# Identificadores: sec-presentacion > sec-datos-cohorte
# Cabecera original: ```{r}
# -----------------------------------------------------------------------------
#| label: fig-cohorte-eda
#| fig-width: 10
#| fig-height: 10
#| out-width: "100%"
#| fig-cap: "Descripción de `cohorte`. En la diagonal distribución de las variables, diferenciadas en color por `evento`. En los cruces la relación entre ellas."

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

#
# # Reproducibilidad
# Identificador de sección: sec-repro
# Ruta: Reproducibilidad

# -----------------------------------------------------------------------------
# Chunk 6: session-info
# Sección: Reproducibilidad
# Identificadores: sec-repro
# Cabecera original: ```{r}
# -----------------------------------------------------------------------------
#| label: session-info
sessionInfo()
# renv::snapshot()   # fijar el estado del entorno al cerrar el caso

#
# # Referencias
# Ruta: Referencias

