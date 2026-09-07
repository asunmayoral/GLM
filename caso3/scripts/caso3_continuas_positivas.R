# =============================================================================
# Caso 3 · Documento maestro — Caso 3 · ¿Cuánto y hasta cuándo?
# -----------------------------------------------------------------------------
# Código del propio caso3_continuas_positivas.qmd: setup, carga de datos y
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
  DHARMa = "0.5.0", MASS = "7.3.65", broom = "1.0.13",
  broom.mixed = "0.2.9.7", car = "3.1.5", emmeans = "2.0.3",
  flexsurv = "2.3.2", glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  marginaleffects = "0.32.0", patchwork = "1.3.2", performance = "0.17.0",
  rsample = "1.3.2", sessioninfo = "1.2.4", survival = "3.8.6",
  survminer = "0.5.2", tibble = "3.3.1", tidyverse = "2.0.0")
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
# Núcleo del Caso 3 (respuestas continuas positivas + mixtos + supervivencia).
# Cada unidad añadirá lo suyo cuando la desarrollemos (p. ej. glmmTMB::tweedie() en 3.4).
library(MASS)            # se carga ANTES que tidyverse para que dplyr::select() no quede enmascarada
library(tidyverse)       # manipulación, visualización y descriptivos
library(broom)           # resultados ordenados de glm y modelos de supervivencia
library(broom.mixed)     # tidy() para modelos mixtos (glmmTMB, lme4)
library(patchwork)       # combinación de gráficos

library(car)             # contrastes, VIF y diagnóstico
library(DHARMa)          # residuos simulados para GLM/GLMM
library(performance)     # diagnóstico y comparación de modelos
library(marginaleffects) # efectos, contrastes y predicciones ajustadas
library(emmeans)         # medias marginales y comparaciones

library(lme4)            # lmer y glmer (mixtos)
library(glmmTMB)         # GLMM Gamma, y familia Tweedie (3.4)
library(glmnet)          # regularización (en 3.7, como límite conceptual en Tweedie)
library(rsample)         # validación cruzada AGRUPADA (group_vfold_cv por máquina)

library(survival)        # Kaplan-Meier, Cox y datos start-stop
library(survminer)       # representación gráfica de supervivencia
library(flexsurv)        # modelos paramétricos de supervivencia (AFT)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

# -----------------------------------------------------------------------------
# [banco-averias]
#   Presentación del caso > El contexto y los datos > Un primer vistazo
# -----------------------------------------------------------------------------
source(url_glm("caso3/R/dgp_averias.R"))   # funciones del proceso generador
banco <- leer_datos_glm("caso3/datos/banco_averias_20252026.rds")   # banco de averías del curso

glimpse(banco$averias)

# -----------------------------------------------------------------------------
# [tbl-averias-coste]
#   Presentación del caso > El contexto y los datos > Un primer vistazo
# -----------------------------------------------------------------------------
tibble::tibble(
  n       = nrow(banco$averias),
  media   = round(mean(banco$averias$coste_euros)),
  mediana = round(median(banco$averias$coste_euros)),
  minimo  = round(min(banco$averias$coste_euros)),
  maximo  = round(max(banco$averias$coste_euros)),
  cv      = round(sd(banco$averias$coste_euros) / mean(banco$averias$coste_euros), 2))

# -----------------------------------------------------------------------------
# [tbl-averias-frecuencia]
#   Presentación del caso > El contexto y los datos > Un primer vistazo
# -----------------------------------------------------------------------------
n_por_maq <- as.integer(table(factor(banco$averias$id_maquina,
                                      levels = banco$maquinas$id_maquina)))
tibble::tibble(
  averias_media    = round(mean(n_por_maq), 2),
  averias_var      = round(var(n_por_maq), 2),
  var_media        = round(var(n_por_maq) / mean(n_por_maq), 2),
  intervalos       = nrow(banco$intervalos),
  eventos          = sum(banco$intervalos$evento),
  censura_pct      = round(mean(banco$intervalos$evento == 0) * 100),
  gap_mediano_dias = round(median(banco$intervalos$tiempo_entre)))

# -----------------------------------------------------------------------------
# [fig-averias-eda]
#   Presentación del caso > El contexto y los datos > Un primer vistazo
# -----------------------------------------------------------------------------
p_coste <- ggplot(banco$averias, aes(coste_euros)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  scale_x_log10() +
  labs(x = "coste de la avería (€, escala log)", y = "nº de averías")

p_gap <- ggplot(subset(banco$intervalos, evento == 1), aes(tiempo_entre)) +
  geom_histogram(bins = 40, fill = "darkorange") +
  labs(x = "tiempo entre fallos (días)", y = "nº de intervalos")

p_coste + p_gap


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
