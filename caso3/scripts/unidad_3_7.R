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
# EJECUCIÓN: funciona desde CUALQUIER carpeta dentro del proyecto GLM;
# localiza la raíz por _quarto.yml y resuelve solo las rutas de datos.
# =============================================================================

.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")

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

source(file.path(.raiz, "caso3", "R", "dgp_averias.R"))     # define simular_averias() y cargar_averias()
banco <- cargar_averias()      # lee datos/banco_averias_*.rds si existe; si no, simula y lo guarda

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

