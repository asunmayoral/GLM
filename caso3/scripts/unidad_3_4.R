# =============================================================================
# Caso 3 · Unidad 3.4 — 4 · Del coste al tiempo: modelos de vida acelerada (AFT)
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_4.qmd.
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

source(file.path(.raiz, "caso3", "R", "dgp_averias.R"))     # define simular_averias() y cargar_averias()
banco <- cargar_averias()      # lee datos/banco_averias_*.rds si existe; si no, simula y lo guarda

glimpse(banco$averias)

# -----------------------------------------------------------------------------
# [u34-datos]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.1 Contexto: el tiempo entre fallos y la censura
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
# [u34-surv]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.1 Contexto: el tiempo entre fallos y la censura
# -----------------------------------------------------------------------------
library(survival)
head(Surv(iv$tiempo_entre, iv$evento), 12)   # un "+" marca las esperas censuradas

# -----------------------------------------------------------------------------
# [fig-u34-eda]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.1 Contexto: el tiempo entre fallos y la censura
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
# [fig-u34-km]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.2 Kaplan–Meier: la mirada descriptiva
# -----------------------------------------------------------------------------
km <- survfit(Surv(tiempo_entre, evento) ~ plan_mantenimiento, data = iv)

survminer::ggsurvplot(km, data = iv, conf.int = TRUE, risk.table = TRUE,
                      censor = TRUE, legend.labs = c("Correctivo", "Preventivo"),
                      xlab = "días desde el último fallo",
                      ylab = "S(t) = probabilidad de seguir sin averiarse")

# -----------------------------------------------------------------------------
# [u34-logrank]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.2 Kaplan–Meier: la mirada descriptiva
# -----------------------------------------------------------------------------
km                                            # imprime las medianas por grupo
survdiff(Surv(tiempo_entre, evento) ~ plan_mantenimiento, data = iv)   # log-rank

# -----------------------------------------------------------------------------
# [fig-u34-motiva]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.3 El modelo AFT y la censura en la verosimilitud
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
# [u34-aft-weibull]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.3 El modelo AFT y la censura en la verosimilitud
#       > ¿Por qué empezar por la Weibull?
# -----------------------------------------------------------------------------
formula_aft <- Surv(tiempo_entre, evento) ~ plan_mantenimiento + antiguedad_ini +
  carga + proceso

aft_w <- survreg(formula_aft, dist = "weibull", data = iv)
summary(aft_w)

# -----------------------------------------------------------------------------
# [tbl-u34-af]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.4 Interpretación: el factor de aceleración
# -----------------------------------------------------------------------------
broom::tidy(aft_w, conf.int = TRUE) |>
  dplyr::filter(term != "(Intercept)", term != "Log(scale)") |>
  dplyr::transmute(term,
                   factor_aceleracion = round(exp(estimate), 3),
                   conf.low = round(exp(conf.low), 3),
                   conf.high = round(exp(conf.high), 3),
                   p.value = signif(p.value, 3))

# -----------------------------------------------------------------------------
# [u34-pred-plan]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.4 Interpretación: el factor de aceleración
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
# [u34-criticidad]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.4 Interpretación: el factor de aceleración
# -----------------------------------------------------------------------------
# ¿Aporta la criticidad algo a la explicación del TIEMPO entre fallos?
aft_crit <- survreg(update(formula_aft, . ~ . + criticidad), dist = "weibull", data = iv)
anova(aft_w, aft_crit)   # contraste de razón de verosimilitudes

# -----------------------------------------------------------------------------
# [u34-forma]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.4 Interpretación: el factor de aceleración
# -----------------------------------------------------------------------------
forma_weibull <- 1 / aft_w$scale   # forma k = 1/sigma; k>1 creciente, k<1 decreciente, k=1 constante
round(c(sigma_escala = aft_w$scale, forma_k = forma_weibull), 3)

# -----------------------------------------------------------------------------
# [u34-lrt-anova]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.5 Inferencia: del error estándar al contraste
# -----------------------------------------------------------------------------
# ¿Son significativas todas las variables
anova(aft_w)

# -----------------------------------------------------------------------------
# [u34-lrt-forma]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.5 Inferencia: del error estándar al contraste
# -----------------------------------------------------------------------------
# ¿Aporta la forma libre de la Weibull, o basta la exponencial? H0: sigma = 1 (k = 1)
aft_exp <- survreg(formula_aft, dist = "exponential", data = iv)
anova(aft_exp, aft_w)

# -----------------------------------------------------------------------------
# [tbl-u34-comparacion]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.6 Elección de distribución, diagnóstico y validación
# -----------------------------------------------------------------------------
purrr::map_dfr(c("exponential", "weibull", "lognormal", "loglogistic"), \(dd) {
  m <- survreg(formula_aft, dist = dd, data = iv)
  tibble::tibble(distribucion = dd, logLik = round(as.numeric(logLik(m)), 1), AIC = round(AIC(m), 1))
}) |>
  dplyr::arrange(AIC)

# -----------------------------------------------------------------------------
# [fig-u34-linealizado]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.6 Elección de distribución, diagnóstico y validación
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
# [fig-u34-diag]
#   4 · Del coste al tiempo: modelos de vida acelerada (AFT)
#     > 4.6 Elección de distribución, diagnóstico y validación
# -----------------------------------------------------------------------------
# Residuo estandarizado de valor extremo; su exponencial es un residuo de Cox–Snell ~ Exp(1)
res_cs <- exp((log(iv$tiempo_entre) - aft_w$linear.predictors) / aft_w$scale)
km_res <- survfit(Surv(res_cs, iv$evento) ~ 1)

plot(km_res, conf.int = FALSE, xlim = c(0, 4),
     xlab = "residuo de Cox–Snell", ylab = "supervivencia del residuo")
curve(exp(-x), from = 0, to = 4, add = TRUE, col = "firebrick", lwd = 2)
legend("topright", c("KM de los residuos", "exponencial estándar teórica"),
       lty = 1, col = c("black", "firebrick"), bty = "n")

