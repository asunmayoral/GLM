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

.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")

# -----------------------------------------------------------------------------
# [setup]
#   (sin sección)
# -----------------------------------------------------------------------------
# Núcleo del Caso 3 (respuestas continuas positivas + mixtos + supervivencia).
# Cada unidad añadirá lo suyo cuando la desarrollemos (p. ej. glmmTMB::tweedie() en 3.6).
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
library(glmmTMB)         # GLMM Gamma, y familia Tweedie (3.6)
library(glmnet)          # regularización (en 3.6, como límite conceptual en Gamma)
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
source(file.path(.raiz, "caso3", "R", "dgp_averias.R"))     # define simular_averias() y cargar_averias()
banco <- cargar_averias()      # lee datos/banco_averias_*.rds si existe; si no, simula y lo guarda

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

