# =============================================================================
# Caso 2 · Unidad 2.5 — 5 · Del conteo al reloj. Supervivencia
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_5.qmd.
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
# [u25-datos]  ·  5.1 El hazard como una tasa: el problema del tiempo a evento > Los datos y la pregunta
# -----------------------------------------------------------------------------
cartera |>
  dplyr::select(id_poliza, tiempo_primer_sin, evento, exposicion,
                zona_circulacion, uso, potencia_cv) |>
  head(8)

# -----------------------------------------------------------------------------
# [fig-u25-km]  ·  5.1 El hazard como una tasa: el problema del tiempo a evento > Una primera mirada: la curva de supervivencia
# -----------------------------------------------------------------------------
km <- survfit(Surv(tiempo_primer_sin, evento) ~ zona_circulacion, data = cartera)
broom::tidy(km) |>
  ggplot(aes(time, estimate, colour = strata)) +
  geom_step(linewidth = 0.8) +
  labs(x = "tramo de cobertura", y = "supervivencia estimada  S(t)", colour = "zona")

# -----------------------------------------------------------------------------
# [u25-pp]  ·  5.1 El hazard como una tasa: el problema del tiempo a evento > La transformación a persona-periodo (y por qué)
# -----------------------------------------------------------------------------
pp <- expandir_poliza_tramo(cartera, "tiempo_primer_sin", "evento", col_id = "id_poliza")
# la primera póliza, desplegada en sus tramos en riesgo:
head(pp[pp$id == pp$id[1], c("id", "tramo", "y", "zona_circulacion", "uso")], 8)
