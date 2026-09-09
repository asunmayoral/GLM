# =============================================================================
# Caso 2 · Documento maestro — Caso 2 · ¿Cuántas veces y a qué ritmo?
# -----------------------------------------------------------------------------
# Código del propio caso2_conteos.qmd: setup, carga de datos y
# descriptivos de presentación del caso. El código de las unidades está
# en los scripts unidad_*.R de esta misma carpeta.
#
# Cada bloque lleva su LABEL y la ruta de encabezados donde aparece.
#
# GENERADO AUTOMÁTICAMENTE por _scripts/generar_scripts_unidades.R:
# no editar a mano; los cambios se pierden al regenerar. Edita el .qmd.
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
  DHARMa = "0.5.0", MASS = "7.3.65", MuMIn = "1.48.19", broom = "1.0.13",
  dplyr = "1.2.1", glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  marginaleffects = "0.32.0", performance = "0.17.0", pscl = "1.5.9",
  purrr = "1.2.2", sessioninfo = "1.2.4", survival = "3.8.6",
  tibble = "3.3.1", tidyr = "1.3.2", tidyverse = "2.0.0", vcd = "1.4.13",
  vcdExtra = "0.9.6")
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

# -----------------------------------------------------------------------------
# [setup]
#   (sin sección)
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
# [cartera-sim]
#   Presentación del caso > El contexto y los datos
# -----------------------------------------------------------------------------
source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
glimpse(cartera)

# -----------------------------------------------------------------------------
# [tbl-cartera-resp]
#   Presentación del caso > El contexto y los datos
# -----------------------------------------------------------------------------
resp <- c("n_asistencia", "n_danos", "n_fraude", "n_gestiones")
purrr::map_dfr(resp, ~ tibble::tibble(
  respuesta = .x,
  media     = round(mean(cartera[[.x]]), 2),
  pct_ceros = round(mean(cartera[[.x]] == 0) * 100),
  maximo    = max(cartera[[.x]]),
  var_media = round(var(cartera[[.x]]) / mean(cartera[[.x]]), 2)))

# -----------------------------------------------------------------------------
# [fig-cartera-eda]
#   Presentación del caso > El contexto y los datos
# -----------------------------------------------------------------------------
cartera |>
  dplyr::select(dplyr::all_of(resp)) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "respuesta", values_to = "conteo") |>
  dplyr::filter(conteo <= 8) |>
  ggplot(aes(conteo)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ respuesta, scales = "free_y") +
  labs(x = "nº de eventos por póliza", y = "nº de pólizas")


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
