# =============================================================================
# Caso 3 · Unidad 3.5 — 5 · Del coste al tiempo: modelos de vida acelerada (AFT)
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_5.qmd.
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
  purrr = "1.2.2", rsample = "1.3.2", sessioninfo = "1.2.4",
  survival = "3.8.6", survminer = "0.5.2", tibble = "3.3.1",
  tidyverse = "2.0.0")
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
# [u35-datos]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.1 Contexto: el tiempo entre fallos y la censura
# -----------------------------------------------------------------------------
iv <- banco$intervalos
head(iv,3)

c(intervalos = nrow(iv),
  maquinas   = length(unique(iv$id_maquina)),
  fallos     = sum(iv$evento),
  censuras    = sum(iv$evento == 0),
  prop_censura = round(mean(iv$evento == 0), 3),
  mediana_dias = round(median(iv$tiempo_entre), 1))

# -----------------------------------------------------------------------------
# [u35-surv]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.1 Contexto: el tiempo entre fallos y la censura
# -----------------------------------------------------------------------------
library(survival)
head(Surv(iv$tiempo_entre, iv$evento), 12)   # un "+" marca las esperas censuradas

# -----------------------------------------------------------------------------
# [fig-u35-eda]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.1 Contexto: el tiempo entre fallos y la censura
#       > ¿Y qué covariables la mueven?
# -----------------------------------------------------------------------------
fallos <- iv[iv$evento == 1, ]
fallos$proceso <- factor(fallos$proceso,
  levels = c("Acabado", "Ensamblaje", "Mecanizado", "Corte", "Lijado"))

e_plan <- ggplot(fallos, aes(plan_mantenimiento, log(tiempo_entre))) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(x = "plan de mantenimiento", y = "log(tiempo entre fallos)",
       subtitle = "Plan: el preventivo alarga")

e_proc <- ggplot(fallos, aes(proceso, log(tiempo_entre))) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(x = "proceso", y = NULL, subtitle = "Proceso: diferencias leves") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

e_edad <- ggplot(fallos, aes(antiguedad_ini, log(tiempo_entre))) +
  geom_point(size = 0.5, alpha = 0.3, colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(x = "antigüedad al inicio del intervalo (años)", y = "log(tiempo entre fallos)",
       subtitle = "Edad: a más años, menos espera")

e_carga <- ggplot(fallos, aes(carga, log(tiempo_entre))) +
  geom_point(size = 0.5, alpha = 0.3, colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
  labs(x = "carga de uso", y = NULL, subtitle = "Carga: a más uso, menos espera")

(e_plan + e_proc) / (e_edad + e_carga)   # patchwork

# -----------------------------------------------------------------------------
# [fig-u35-km]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.2 Kaplan–Meier: la mirada descriptiva
# -----------------------------------------------------------------------------
km <- survfit(Surv(tiempo_entre, evento) ~ plan_mantenimiento, data = iv)

survminer::ggsurvplot(km, data = iv, conf.int = TRUE, risk.table = TRUE,
                      censor = TRUE, legend.labs = c("Correctivo", "Preventivo"),
                      xlab = "días desde el último fallo",
                      ylab = "S(t) = probabilidad de seguir sin averiarse")

# -----------------------------------------------------------------------------
# [u35-logrank]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.2 Kaplan–Meier: la mirada descriptiva
# -----------------------------------------------------------------------------
km                                            # imprime las medianas por grupo
survdiff(Surv(tiempo_entre, evento) ~ plan_mantenimiento, data = iv)   # log-rank

# -----------------------------------------------------------------------------
# [fig-u35-motiva]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.3 El modelo AFT y la censura en la verosimilitud
#       > ¿Por qué empezar por la Weibull?
# -----------------------------------------------------------------------------
fallos <- iv[iv$evento == 1, ]   # los fallos observados (la censura no tiene un tiempo de fallo que dibujar)

p_t <- ggplot(fallos, aes(tiempo_entre)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  labs(x = "tiempo entre fallos (días)", y = "frecuencia",
       subtitle = "T: positiva, cola larga a la derecha")

p_logt <- ggplot(fallos, aes(log(tiempo_entre))) +
  geom_histogram(bins = 40, fill = "darkorange", colour = "white") +
  labs(x = "log(tiempo entre fallos)", y = "frecuencia",
       subtitle = "log T: asimétrica hacia la izquierda")

p_t + p_logt   # patchwork

# -----------------------------------------------------------------------------
# [u35-aft-weibull]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.3 El modelo AFT y la censura en la verosimilitud
#       > ¿Por qué empezar por la Weibull?
# -----------------------------------------------------------------------------
formula_aft <- Surv(tiempo_entre, evento) ~ plan_mantenimiento + antiguedad_ini +
  carga + proceso

aft_w <- survreg(formula_aft, dist = "weibull", data = iv)
summary(aft_w)

# -----------------------------------------------------------------------------
# [tbl-u35-af]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.4 Interpretación: el factor de aceleración
# -----------------------------------------------------------------------------
broom::tidy(aft_w, conf.int = TRUE) |>
  dplyr::filter(term != "(Intercept)", term != "Log(scale)") |>
  dplyr::transmute(term,
                   factor_aceleracion = round(exp(estimate), 3),
                   conf.low = round(exp(conf.low), 3),
                   conf.high = round(exp(conf.high), 3),
                   p.value = signif(p.value, 3))

# -----------------------------------------------------------------------------
# [u35-pred-plan]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.4 Interpretación: el factor de aceleración
# -----------------------------------------------------------------------------
# Días de espera predichos (mediana) para una máquina promedio, según el plan
nd <- data.frame(plan_mantenimiento = factor(c("Correctivo", "Preventivo"),
                                             levels = c("Correctivo", "Preventivo")),
                 antiguedad_ini = mean(iv$antiguedad_ini),
                 carga = mean(iv$carga),
                 proceso = factor("Corte", levels = levels(iv$proceso)))
pred <- predict(aft_w, newdata = nd, type = "quantile", p = 0.5)
data.frame(plan = nd$plan_mantenimiento, mediana_dias = round(pred),
           dias_ganados = c(NA, round(diff(pred))))

# -----------------------------------------------------------------------------
# [u35-criticidad]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.4 Interpretación: el factor de aceleración
# -----------------------------------------------------------------------------
# ¿Aporta la criticidad algo a la explicación del TIEMPO entre fallos?
aft_crit <- survreg(update(formula_aft, . ~ . + criticidad), dist = "weibull", data = iv)
anova(aft_w, aft_crit)   # contraste de razón de verosimilitudes

# -----------------------------------------------------------------------------
# [u35-forma]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.4 Interpretación: el factor de aceleración
# -----------------------------------------------------------------------------
forma_weibull <- 1 / aft_w$scale   # forma k = 1/sigma; k>1 creciente, k<1 decreciente, k=1 constante
round(c(sigma_escala = aft_w$scale, forma_k = forma_weibull), 3)

# -----------------------------------------------------------------------------
# [u35-lrt-anova]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.5 Inferencia: del error estándar al contraste
# -----------------------------------------------------------------------------
# ¿Son significativas todas las variables
anova(aft_w)

# -----------------------------------------------------------------------------
# [u35-lrt-forma]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.5 Inferencia: del error estándar al contraste
# -----------------------------------------------------------------------------
# ¿Aporta la forma libre de la Weibull, o basta la exponencial? H0: sigma = 1 (k = 1)
aft_exp <- survreg(formula_aft, dist = "exponential", data = iv)
anova(aft_exp, aft_w)

# -----------------------------------------------------------------------------
# [tbl-u35-comparacion]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.6 Elección de distribución, diagnóstico y validación
# -----------------------------------------------------------------------------
purrr::map_dfr(c("exponential", "weibull", "lognormal", "loglogistic"), \(dd) {
  m <- survreg(formula_aft, dist = dd, data = iv)
  tibble::tibble(distribucion = dd, logLik = round(as.numeric(logLik(m)), 1), AIC = round(AIC(m), 1))
}) |>
  dplyr::arrange(AIC)

# -----------------------------------------------------------------------------
# [fig-u35-linealizado]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.6 Elección de distribución, diagnóstico y validación
# -----------------------------------------------------------------------------
km1 <- survfit(Surv(tiempo_entre, evento) ~ 1, data = iv)
d <- data.frame(t = km1$time, S = km1$surv)
d <- subset(d, t > 0 & S > 1e-6 & S < 1 - 1e-6)

niveles <- c("Weibull:  log(-log S)", "Log-logística:  logit(1-S)", "Log-normal:  probit(1-S)")
lin <- rbind(
  data.frame(logt = log(d$t), y = log(-log(d$S)),       familia = niveles[1]),
  data.frame(logt = log(d$t), y = log((1 - d$S) / d$S), familia = niveles[2]),
  data.frame(logt = log(d$t), y = qnorm(1 - d$S),       familia = niveles[3]))
lin$familia <- factor(lin$familia, levels = niveles)

ggplot(lin, aes(logt, y)) +
  geom_point(size = 0.6, alpha = 0.5, colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "firebrick", linewidth = 0.8) +
  facet_wrap(~ familia, scales = "free_y") +
  labs(x = "log(tiempo entre fallos)", y = "transformada de S(t)")

# -----------------------------------------------------------------------------
# [fig-u35-diag]
#   5 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 5.6 Elección de distribución, diagnóstico y validación
# -----------------------------------------------------------------------------
# Residuo estandarizado de valor extremo; su exponencial es un residuo de Cox–Snell ~ Exp(1)
res_cs <- exp((log(iv$tiempo_entre) - aft_w$linear.predictors) / aft_w$scale)
km_res <- survfit(Surv(res_cs, iv$evento) ~ 1)

plot(km_res, conf.int = FALSE, xlim = c(0, 4),
     xlab = "residuo de Cox–Snell", ylab = "supervivencia del residuo")
curve(exp(-x), from = 0, to = 4, add = TRUE, col = "firebrick", lwd = 2)
legend("topright", c("KM de los residuos", "exponencial estándar teórica"),
       lty = 1, col = c("black", "firebrick"), bty = "n")


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
