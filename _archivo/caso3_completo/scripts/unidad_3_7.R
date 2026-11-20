# =============================================================================
# Caso 3 · Unidad 3.7 — 7 · Más allá: las tres lentes y las fronteras
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_7.qmd.
# Cada bloque va precedido de su LABEL y de la ruta de encabezados
# (sección > subsección > apartado) en la que aparece dentro del documento.
#
# GENERADO AUTOMÁTICAMENTE por _scripts/generar_scripts_unidades.R:
# no editar a mano; los cambios se pierden al regenerar. Edita el .qmd.
#
# EJECUCIÓN: autónomo. Guárdalo donde quieras y ejecútalo; los datos y los
# ficheros del proceso generador se descargan del repositorio del curso.
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
  broom.mixed = "0.2.9.7", car = "3.1.5", dplyr = "1.2.1", emmeans = "2.0.3",
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

# --- Preámbulo del caso (librerías y datos, como en el documento) ------------
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

source(url_glm("caso3/R/dgp_averias.R"))   # funciones del proceso generador
banco <- leer_datos_glm("caso3/datos/banco_averias_20252026.rds")   # banco de averías del curso

glimpse(banco$averias)

# -----------------------------------------------------------------------------
# [u37-datos]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > La pregunta, y una tabla que ya conocemos
# -----------------------------------------------------------------------------
c(tramos          = nrow(sg),
  maquinas        = length(unique(sg$id_maquina)),
  meses_con_fallo = sum(sg$fallo),
  averias         = sum(sg$n_fallos),
  altas_tardias   = sum(tapply(sg$tstart, sg$id_maquina, min) > 0))

# -----------------------------------------------------------------------------
# [u37-head]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > La pregunta, y una tabla que ya conocemos
# -----------------------------------------------------------------------------
head(subset(sg, id_maquina == "M227",
            select = c(id_maquina, tstart, tstop, exposicion, fallo, n_fallos,
                       antiguedad, carga, prot_mant)), 8)

# -----------------------------------------------------------------------------
# [u37-tres-lentes]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > Tres lentes, un solo riesgo
# -----------------------------------------------------------------------------
# definimos el semestre en que se encuentra cada ventana (mes), con un máximo de 8 
# el semestre 8 agrupa desde los días 1281 en adelante
sg$semestre <- factor(1 + pmin(sg$tstart %/% 183, 7))

# Lente 1 (Caso 1) · TIEMPO DISCRETO: binomial cloglog sobre el indicador                
m_cll <- glm(fallo ~ antiguedad + carga + prot_mant + semestre + offset(log(exposicion)),
             family = binomial("cloglog"), data = sg)

# Lente 2 (Caso 2) · RIESGOS A TROZOS: Poisson sobre el conteo, con offset               
m_poi <- glm(n_fallos ~ antiguedad + carga + prot_mant + semestre + offset(log(exposicion)),
             family = poisson, data = sg)

# Lente 3 (Caso 3) · COX extendido (start-stop), riesgo de base libre: cox_mes_opt   
cox_mes_opt  <- coxph(Surv(tstart, tstop, fallo) ~ antiguedad + carga + prot_mant,
                      data = sg)

# -----------------------------------------------------------------------------
# [tbl-u37-lentes]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > Qué estima cada una, y cuánto coinciden
# -----------------------------------------------------------------------------
sacar <- function(m, nombre) broom::tidy(m) |>
  dplyr::filter(term %in% c("antiguedad", "carga", "prot_mant")) |>
  dplyr::transmute(term, !!nombre := round(estimate, 3))

verdad_hz <- attr(banco, "verdad")$beta_hazard
tab_verdad <- tibble::tibble(
  term       = c("antiguedad", "carga", "prot_mant"),
  verdad_DGP = c(verdad_hz[["antiguedad"]], verdad_hz[["carga"]], verdad_hz[["mant_inmediato"]]))

sacar(m_cll, "cloglog") |>
  dplyr::left_join(sacar(m_poi, "Poisson_trozos"), by = "term") |>
  dplyr::left_join(sacar(cox_mes_opt, "Cox"), by = "term") |>
  dplyr::left_join(tab_verdad, by = "term")

# -----------------------------------------------------------------------------
# [u37-lectura-aplicada]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > Qué estima cada una, y cuánto coinciden
# -----------------------------------------------------------------------------
b <- coef(cox_mes_opt)
round(c(
  riesgo_10_anios   = exp(10 * b[["antiguedad"]]),          # 10 años vs recién estrenada
  riesgo_carga_01   = exp(0.1 * b[["carga"]]),              # +0.1 de nivel de uso
  revision_dia_0    = exp(b[["prot_mant"]] * exp(-0 / 60)),   # hazard ratio vs sin protección...
  revision_dia_30   = exp(b[["prot_mant"]] * exp(-30 / 60)),  # ...según los días desde la revisión
  revision_dia_60   = exp(b[["prot_mant"]] * exp(-60 / 60)),
  revision_dia_120  = exp(b[["prot_mant"]] * exp(-120 / 60))), 2)

# -----------------------------------------------------------------------------
# [u37-base-plana]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > El riesgo de base que no estaba
# -----------------------------------------------------------------------------
rbind(cloglog = round(coef(m_cll)[grep("semestre", names(coef(m_cll)))], 3),
      Poisson = round(coef(m_poi)[grep("semestre", names(coef(m_poi)))], 3))
anova(update(m_cll, . ~ . - semestre), m_cll, test = "Chisq")
anova(update(m_poi, . ~ . - semestre), m_poi, test = "Chisq")

# -----------------------------------------------------------------------------
# [tbl-u37-fragilidad]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > ¿Y la fragilidad?
# -----------------------------------------------------------------------------
# el cloglog es numéricamente delicado: se arranca del ajuste fijo (etastart);
# bobyqa evita en ambos un aviso de convergencia del optimizador por defecto
mm_cll <- glmer(fallo ~ antiguedad + carga + prot_mant + semestre + offset(log(exposicion)) +
                  (1 | id_maquina), family = binomial("cloglog"), data = sg,
                etastart = predict(m_cll), control = glmerControl(optimizer = "bobyqa"))
mm_poi <- glmer(n_fallos ~ antiguedad + carga + prot_mant + semestre + offset(log(exposicion)) +
                  (1 | id_maquina), family = poisson, data = sg,
                control = glmerControl(optimizer = "bobyqa"))
cox_mes_opt_frag <- coxph(Surv(tstart, tstop, fallo) ~ antiguedad + carga + prot_mant +
                   frailty(id_maquina), data = sg)
sel <- c("antiguedad", "carga", "prot_mant")
tibble::tibble(term = sel,
  cloglog_GLMM = round(fixef(mm_cll)[sel], 3),
  Poisson_GLMM = round(fixef(mm_poi)[sel], 3),
  Cox_frailty  = round(coef(cox_mes_opt_frag)[sel], 3),
  verdad_DGP   = c(verdad_hz[["antiguedad"]], verdad_hz[["carga"]], verdad_hz[["mant_inmediato"]]))

# -----------------------------------------------------------------------------
# [u37-fragilidad-var]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > ¿Y la fragilidad?
# -----------------------------------------------------------------------------
# theta_Cox va transcrita de la salida impresa de cox_mes_opt_frag ("Variance of
# random effect"): el accesor history[[1]]$theta no es estable (ver 3.6, @tbl-u36-dgp)
c(sd_cloglog  = round(sqrt(unlist(VarCorr(mm_cll)))[[1]], 3),
  sd_Poisson  = round(sqrt(unlist(VarCorr(mm_poi)))[[1]], 3),
  theta_Cox   = 0.143,
  sd_logZ_DGP = round(sqrt(trigamma(1 / attr(banco, "verdad")$theta_frail)), 3))

# -----------------------------------------------------------------------------
# [tbl-u37-validacion]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
ic_lente <- function(m, nombre) broom::tidy(m) |>
  dplyr::filter(term %in% c("antiguedad", "carga", "prot_mant")) |>
  dplyr::transmute(term,
    !!nombre := sprintf("%.3f (%.3f, %.3f)", estimate,
                        estimate - 1.96 * std.error, estimate + 1.96 * std.error))

ic_lente(m_cll, "cloglog") |>
  dplyr::left_join(ic_lente(m_poi, "Poisson_trozos"), by = "term") |>
  dplyr::left_join(ic_lente(cox_mes_opt, "Cox"), by = "term") |>
  dplyr::left_join(tab_verdad, by = "term")

# -----------------------------------------------------------------------------
# [u37-carga-se]
#   7 · Más allá: las tres lentes y las fronteras
#     > 7.1 El mismo fallo, tres lentes
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
c(se_fijo    = round(summary(cox_mes_opt)$coefficients["carga", "se(coef)"], 2),
  se_frailty = round(summary(cox_mes_opt_frag)$coefficients["carga", "se(coef)"], 2))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
