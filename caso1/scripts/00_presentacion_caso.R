# =============================================================================
#  Caso 1 · Presentación del caso  —  código R
#  Fuente: caso1_binaria_tiempo_evento.qmd
#  Genera los objetos que usan TODAS las unidades: glow, cohorte, pp.
#  Nota: ejecutar con el directorio de trabajo en 'caso1/' (por el source()).
# =============================================================================

# ---- Setup: núcleo de software ------------------------------------ (chunk: setup) ----
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

# ---- Dato real: GLOW --------------------------------- (Sec. "Dato real — GLOW") ----
data(glow500)
glow <- glow500 |>
  as_tibble() |>
  mutate(fractura01 = as.integer(fracture) - 1L)   # No -> 0, Yes -> 1

glow |> count(fracture) |> mutate(prop = round(n / sum(n), 3))

# ---- EDA de GLOW: matriz de dispersión por la respuesta ------- (fig: glow-eda) ----
vars <- c("age", "weight", "height", "bmi", "fracscore",   # continuas / discretas
          "priorfrac", "premeno", "momfrac",               # factores de riesgo binarios
          "armassist", "smoke", "raterisk")                # binarios + ordinal

glow |>
  select(fracture, all_of(vars)) |>
  ggpairs(
    columns = vars,
    mapping = aes(color = fracture, alpha = 0.6),
    legend  = 1,
    upper = list(continuous = wrap("cor", size = 2.3),
                 combo      = wrap("box_no_facet"),
                 discrete   = wrap("count")),
    lower = list(continuous = wrap("points", alpha = 0.25, size = 0.4),
                 combo      = wrap("facethist", bins = 20),
                 discrete   = wrap("facetbar")),
    diag  = list(continuous = wrap("densityDiag", alpha = 0.5),
                 discrete   = wrap("barDiag")),
    progress = FALSE
  ) +
  theme_bw(base_size = 7) +
  theme(strip.text = element_text(size = 6)) +
  theme(legend.position = "bottom")

# ---- Cohorte simulada -------------------------------- (Sec. "Cohorte simulada") ----
source("R/dgp_cohorte.R")            # define simular_cohorte(), expandir_persona_periodo()
cohorte <- simular_cohorte()         # nivel individuo (binaria, nominal, ordinal)
# write_csv(cohorte, "cohorte.csv")
glimpse(cohorte)
head(cohorte)

# Versión persona-periodo (para supervivencia, Unidad 1.5)
pp <- expandir_persona_periodo(cohorte)
# write_csv(pp, "cohorte_pp.csv")
head(pp, 6)

# ---- EDA de la cohorte: matriz de dispersión por desenlace --- (fig: cohorte-eda) ----
cohorte_eda <- cohorte |>
  mutate(
    evento = factor(evento, levels = c(0, 1), labels = c("Sin fractura", "Fractura")),
    x2     = factor(x2,     levels = c(0, 1), labels = c("No", "Sí")),
    tiempo = factor(tiempo, levels = 1:6)
  )

vars <- c("x1", "x2", "tiempo", "clase_nom", "sever_ord")

cohorte_eda |>
  select(evento, all_of(vars)) |>
  ggpairs(
    columns = vars,
    mapping = aes(color = evento, alpha = 0.6),
    legend  = 1,
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
  theme_bw(base_size = 8) +
  theme(legend.position = "bottom")

# ---- Variación entre clínicas (centro, 24 niveles) -------- (fig: cohorte-centro) ----
cohorte |>
  group_by(centro) |>
  summarise(tasa_fractura = mean(evento), n = n(), .groups = "drop") |>
  ggplot(aes(reorder(centro, tasa_fractura), tasa_fractura)) +
  geom_col(aes(fill = n)) +
  coord_flip() +
  labs(x = "Centro (ordenado por tasa)", y = "Proporción de fractura",
       fill = "n", title = "Variación entre clínicas")

# ---- Reproducibilidad ----------------------------------- (chunk: session-info) ----
sessionInfo()
# renv::snapshot()   # fijar el estado del entorno al cerrar el caso
