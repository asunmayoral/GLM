# =============================================================================
# Caso 1 · Unidad 1.6 — 6 · Estudio de caso, exposición y evaluación
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_1_6.qmd.
# Cada bloque va precedido de su LABEL y de la sección/subsección donde aparece.
#
# EJECUCIÓN: funciona desde CUALQUIER carpeta del proyecto GLM (localiza la raíz
# por _quarto.yml). Cada unidad carga además en sus chunks sus librerías propias.
# =============================================================================

# --- Librerías base (setup del documento del caso) ---------------------------
library(broom); library(aplore3); library(patchwork); library(see)
library(DHARMa); library(arm); library(performance); library(tidyverse)
library(MuMIn); library(readr); library(GGally)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

# --- Raíz del proyecto (robusto al directorio de trabajo) --------------------
.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")

# --- Dato real GLOW ----------------------------------------------------------
data(glow500)
glow <- glow500 |>
  as_tibble() |>
  mutate(fractura01 = as.integer(fracture) - 1L)   # No -> 0, Yes -> 1

# --- Cohorte simulada con CACHÉ (misma que el documento del caso) ------------
source(file.path(.raiz, "caso1", "R", "dgp_cohorte.R"))  # simular_cohorte(), cargar_cohorte(), expandir_persona_periodo()
cohorte <- cargar_cohorte()          # lee datos/ si existe; si no (o si cambió el DGP), simula y cachea
