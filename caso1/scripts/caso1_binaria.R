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
  DHARMa = "0.5.0", GGally = "2.4.0", MuMIn = "1.48.19", aplore3 = "0.9",
  arm = "1.15.3", broom = "1.0.13", patchwork = "1.3.2",
  performance = "0.17.0", readr = "2.2.0", see = "0.14.0",
  sessioninfo = "1.2.4", tidyverse = "2.0.0")
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
source(url_glm("caso1/R/dgp_cohorte.R"))   # funciones del proceso generador
cohorte <- leer_datos_glm("caso1/datos/cohorte_20252026.rds")   # cohorte simulada del curso
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


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
