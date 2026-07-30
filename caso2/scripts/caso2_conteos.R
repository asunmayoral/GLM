# =============================================================================
# Caso 2 · Documento maestro — Caso 2 · ¿Cuántas veces y a qué ritmo?
# -----------------------------------------------------------------------------
# Código del propio caso2_conteos.qmd: setup, carga de datos y
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

# -----------------------------------------------------------------------------
# [cartera-sim]  ·  El contexto y los datos
# -----------------------------------------------------------------------------
source(file.path(.raiz, "caso2", "R", "dgp_conteos.R"))            # define simular_cartera(), cargar_cartera(), expandir_poliza_tramo()
cartera <- cargar_cartera("auto")    # lee datos/cartera_auto_*.rds si existe; si no (o si cambió el DGP), simula y lo guarda
glimpse(cartera)

# -----------------------------------------------------------------------------
# [tbl-cartera-resp]  ·  El contexto y los datos
# -----------------------------------------------------------------------------
resp <- c("n_asistencia", "n_danos", "n_fraude", "n_gestiones")
purrr::map_dfr(resp, ~ tibble::tibble(
  respuesta = .x,
  media     = round(mean(cartera[[.x]]), 2),
  pct_ceros = round(mean(cartera[[.x]] == 0) * 100),
  maximo    = max(cartera[[.x]]),
  var_media = round(var(cartera[[.x]]) / mean(cartera[[.x]]), 2)))

# -----------------------------------------------------------------------------
# [fig-cartera-eda]  ·  El contexto y los datos
# -----------------------------------------------------------------------------
cartera |>
  dplyr::select(dplyr::all_of(resp)) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "respuesta", values_to = "conteo") |>
  dplyr::filter(conteo <= 8) |>
  ggplot(aes(conteo)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ respuesta, scales = "free_y") +
  labs(x = "nº de eventos por póliza", y = "nº de pólizas")

