# =============================================================================
# Caso 1 · Unidad 1.5 — 5 · Supervivencia: el tiempo hasta el evento como GLM
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_1_5.qmd.
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
  DHARMa = "0.5.0", GGally = "2.4.0", MuMIn = "1.48.19", aplore3 = "0.9",
  arm = "1.15.3", broom = "1.0.13", dplyr = "1.2.1", lme4 = "2.0.1",
  lmtest = "0.9.40", pROC = "1.19.0.1", patchwork = "1.3.2",
  performance = "0.17.0", readr = "2.2.0", see = "0.14.0",
  sessioninfo = "1.2.4", survival = "3.8.6", survminer = "0.5.2",
  tidyverse = "2.0.0", varTestnlme = "1.3.5")
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

data(glow500)
glow <- glow500 |>
  as_tibble() |>
  mutate(fractura01 = as.integer(fracture) - 1L)   # No -> 0, Yes -> 1

glow |> count(fracture) |> mutate(prop = round(n / sum(n), 3))

library(readr)
source(url_glm("caso1/R/dgp_cohorte.R"))   # funciones del proceso generador
cohorte <- leer_datos_glm("caso1/datos/cohorte_20252026.rds")   # cohorte simulada del curso
# y guardamos las simulaciones
#write_csv(cohorte, "cohorte.csv")        # nivel individuo
# resumen
glimpse(cohorte)
head(cohorte)

# -----------------------------------------------------------------------------
# [u15-cohorte-head]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.1 La función de supervivencia y la función de riesgo en tiempo discreto
# -----------------------------------------------------------------------------
head(cohorte[, c("id", "centro", "x1", "x2", "tiempo", "evento")])

# -----------------------------------------------------------------------------
# [u15-pp]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.1 La función de supervivencia y la función de riesgo en tiempo discreto
# -----------------------------------------------------------------------------
pp <- expandir_persona_periodo(cohorte)
pp[pp$id %in% c(2, 4), ]   # una mujer que fractura y otra censurada

# -----------------------------------------------------------------------------
# [fig-u15-km]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.2 Kaplan–Meier y riesgos proporcionales
#       > Kaplan–Meier: dejar hablar a los datos
# -----------------------------------------------------------------------------
library(survival)
library(survminer)

km <- survfit(Surv(tiempo, evento) ~ x2, data = cohorte)
ggsurvplot(km, data = cohorte, conf.int = TRUE, pval = TRUE,
           legend.title = "Tratamiento (x2)",
           legend.labs  = c("No", "Sí"),
           xlab = "Periodo de revisión", ylab = "Supervivencia S(t)")

# -----------------------------------------------------------------------------
# [fig-u15-ph-ilustra]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.2 Kaplan–Meier y riesgos proporcionales
#       > Riesgos proporcionales, en tiempo discreto
# -----------------------------------------------------------------------------
beta <- log(2)                                  # efecto de la covariable: HR = exp(beta) = 2
base <- tibble(periodo = 1:6,
               h0 = c(0.05, 0.07, 0.09, 0.11, 0.13, 0.15))  # hazard base creciente (ilustrativo)
ph <- base |>
  mutate(h1 = 1 - (1 - h0)^exp(beta),                        # PH discreto exacto
         c0 = log(-log(1 - h0)), c1 = log(-log(1 - h1)))     # escala cloglog
colores <- c("Referencia (x = 0)" = "grey65", "Grupo (x = 1)" = "#2c7fb8")

p_cll <- ggplot(ph, aes(x = periodo)) +
  geom_segment(aes(xend = periodo, y = c0, yend = c1), color = "grey40") +   # la distancia beta
  geom_point(aes(y = c0, color = "Referencia (x = 0)"), size = 3) +
  geom_point(aes(y = c1, color = "Grupo (x = 1)"), size = 3) +
  scale_color_manual(values = colores, breaks = names(colores)) +
  scale_x_continuous(breaks = 1:6) +
  labs(x = "Periodo de revisión", y = NULL, color = NULL,
       title = "Escala cloglog: log(-log(1 - h_t))")
p_h <- ph |>
  pivot_longer(c(h0, h1), names_to = "grupo", values_to = "hazard") |>
  mutate(grupo = factor(if_else(grupo == "h0", "Referencia (x = 0)", "Grupo (x = 1)"),
                        levels = names(colores))) |>
  ggplot(aes(periodo, hazard, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = colores, guide = "none") +
  scale_x_continuous(breaks = 1:6) +
  labs(x = "Periodo de revisión", y = NULL, title = "Escala del riesgo: h_t")

(p_cll + p_h) + plot_layout(guides = "collect") &
  theme_minimal(base_size = 12) & theme(legend.position = "top")

# -----------------------------------------------------------------------------
# [fig-u15-km-loglog]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.2 Kaplan–Meier y riesgos proporcionales
#       > Riesgos proporcionales, en tiempo discreto
# -----------------------------------------------------------------------------
ggsurvplot(km, data = cohorte, fun = "cloglog",
           legend.title = "Tratamiento", legend.labs = c("No", "Sí"),
           xlab = "log(periodo)", ylab = "log(-log S(t))")

# -----------------------------------------------------------------------------
# [fig-u15-enlaces]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Modelización
# -----------------------------------------------------------------------------
tibble(eta = seq(-4, 4, by = 0.02)) |>
  mutate(logit   = plogis(eta),
         cloglog = 1 - exp(-exp(eta))) |>
  pivot_longer(c(logit, cloglog), names_to = "enlace", values_to = "p") |>
  ggplot(aes(eta, p, color = enlace)) +
  geom_line(linewidth = 1) +
  labs(x = expression(eta), y = expression(p == g^{-1}(eta)), color = "Enlace")

# -----------------------------------------------------------------------------
# [u15-ajuste]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_pp <- glm(y ~ periodo + x1 + x2, family = binomial("cloglog"), data = pp)
summary(m_pp)

# hazard ratios con su IC al 95 % (perfil de verosimilitud)
exp(cbind(HR = coef(m_pp), confint(m_pp)))[c("x1", "x2"), ]

# -----------------------------------------------------------------------------
# [fig-u15-hazard-base]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
base <- tibble(periodo = factor(levels(pp$periodo), levels = levels(pp$periodo)),
               x1 = 0, x2 = 0)
base$h0 <- predict(m_pp, newdata = base, type = "response")   # hazard base por periodo
base$t  <- seq_len(nrow(base))
base$S  <- cumprod(1 - base$h0)                               # supervivencia base

pH <- ggplot(base, aes(t, h0)) +
  geom_col(fill = "grey70") +
  labs(x = "Periodo", y = expression(hazard~base~~h[0][t]), title = "Riesgo base por periodo")
pS <- ggplot(bind_rows(tibble(t = 0, S = 1), base), aes(t, S)) +   # S_0 = 1 al inicio
  geom_step(linewidth = 1) + geom_point(size = 2) +              # constante entre revisiones
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Periodo", y = expression(supervivencia~~S[t]), title = "Supervivencia base")
pH + pS

# -----------------------------------------------------------------------------
# [fig-u15-perfiles]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
perfiles <- expand_grid(periodo = factor(levels(pp$periodo), levels = levels(pp$periodo)),
                        x2 = c(0, 1), x1 = c(-2, -1, 0, 1, 2))
perfiles$h <- predict(m_pp, newdata = perfiles, type = "response")
perfiles <- perfiles |>
  group_by(x2, x1) |>                       # la supervivencia se acumula dentro de cada perfil
  mutate(S = cumprod(1 - h), t = as.integer(periodo)) |>
  ungroup()

perfiles |>
  bind_rows(distinct(perfiles, x1, x2) |> mutate(t = 0L, S = 1)) |>   # S_0 = 1 al inicio
  ggplot(aes(t, S, color = factor(x1), group = x1)) +
  geom_step(linewidth = 1) + geom_point(size = 1.8) +                 # constante entre revisiones
  scale_x_continuous(breaks = 0:6) +
  facet_wrap(~ x2, labeller = as_labeller(c(`0` = "Sin tratamiento (x2 = 0)",
                                            `1` = "Con tratamiento (x2 = 1)"))) +
  scale_color_viridis_d(option = "plasma", end = 0.85, direction = -1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Periodo de revisión", y = "Supervivencia predicha S(t)", color = "Fragilidad (x1)")

# -----------------------------------------------------------------------------
# [u15-perfiles-final]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
perfiles |>
  filter(t == max(t)) |>
  dplyr::select(x1, x2, S) |>
  pivot_wider(names_from = x2, values_from = S, names_prefix = "S6_x2_") |>
  mutate(ganancia_tratamiento = S6_x2_1 - S6_x2_0)

# -----------------------------------------------------------------------------
# [u15-base]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Comparación de modelos y evaluación
#         > ¿Qué forma tiene el riesgo base?
# -----------------------------------------------------------------------------
pp$t  <- as.integer(pp$periodo)   # el periodo como número, para las tendencias
m_0   <- update(m_pp, . ~ . - periodo)
m_lin <- update(m_pp, . ~ . - periodo + t)
m_log <- update(m_pp, . ~ . - periodo + log(t))
anova(m_0,   m_pp, test = "LRT")   # 1. ¿cambia el riesgo base?
anova(m_lin, m_pp, test = "LRT")   # 2. ¿basta la tendencia lineal?
anova(m_log, m_pp, test = "LRT")   #    ¿y la logarítmica?
cbind(AIC(m_0, m_lin, m_log, m_pp), BIC = BIC(m_0, m_lin, m_log, m_pp)$BIC)   # 3.
anova(m_0,   m_log, test = "LRT")  # 4. ¿hace falta la tendencia logarítmica?
rbind(factor = coef(m_pp)[c("x1", "x2")], log = coef(m_log)[c("x1", "x2")])   # ¿cambian los efectos?

# -----------------------------------------------------------------------------
# [u15-fragilidad]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Comparación de modelos y evaluación
#         > ¿Hace falta la fragilidad de centro?
# -----------------------------------------------------------------------------
library(lme4)
m_frail <- glmer(y ~ log(t) + x1 + x2 + (1 | centro),
                 family = binomial("cloglog"), data = pp)
VarCorr(m_frail)   # sigma_u: cuánto varía el riesgo base entre centros
exp(cbind(HR_glm  = coef(m_log)[c("x1", "x2")],        # hazard ratios sin el centro
          HR_glmm = fixef(m_frail)[c("x1", "x2")]))    # y condicionales al centro

# -----------------------------------------------------------------------------
# [u15-frail-test]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Comparación de modelos y evaluación
#         > ¿Hace falta la fragilidad de centro?
# -----------------------------------------------------------------------------
# lrtest avisa de que compara un glm con un glmer: es inocuo, porque las dos
# verosimilitudes son la binomial completa y, por tanto, comparables
library(lmtest)
lrtest(m_log, m_frail)   # estadístico Lambda (su p, contra la chi2_1, sin corregir)

library(varTestnlme)
varCompTest(m_frail, m_log)   # p corregido por la frontera (H0: sigma_u^2 = 0)

# -----------------------------------------------------------------------------
# [u15-calibracion]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Comparación de modelos y evaluación
#         > Calibración: supervivencia observada frente a predicha
# -----------------------------------------------------------------------------
Tmax <- nlevels(pp$periodo)
rej  <- expand_grid(id = cohorte$id, t = seq_len(Tmax)) |>            # 1. cada paciente, 6 revisiones
  left_join(cohorte[, c("id", "centro", "x1", "x2")], by = "id")
rej$h <- predict(m_frail, newdata = rej, type = "response")            #    incluye su centro
rej <- rej |> group_by(id) |> mutate(S = cumprod(1 - h)) |> ungroup()  # 2. S_i(t)

cal <- rej |>                                                          # 3. terciles de S_i(6)
  filter(t == Tmax) |>
  dplyr::select(id, S_fin = S) |>
  left_join(cohorte[, c("id", "tiempo", "evento")], by = "id") |>
  mutate(tercil = cut(S_fin, quantile(S_fin, 0:3 / 3), include.lowest = TRUE,
                      labels = c("alto", "medio", "bajo")))            # menor S, más riesgo

pred <- rej |>                                                         # 4. media de las S_i(t)
  left_join(cal[, c("id", "tercil")], by = "id") |>
  group_by(tercil, t) |>
  summarise(S_modelo = mean(S), .groups = "drop")
km <- summary(survfit(Surv(tiempo, evento) ~ tercil, data = cal),      #    y Kaplan–Meier
              times = seq_len(Tmax), extend = TRUE)
obs <- tibble(tercil = factor(sub("tercil=", "", km$strata), levels = levels(cal$tercil)),
              t = km$time, S_KM = km$surv)
calib <- left_join(pred, obs, by = c("tercil", "t"))
calib |> filter(t == Tmax)

# -----------------------------------------------------------------------------
# [fig-u15-calibracion]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Comparación de modelos y evaluación
#         > Calibración: supervivencia observada frente a predicha
# -----------------------------------------------------------------------------
calib |>
  bind_rows(tibble(tercil = factor(levels(calib$tercil), levels = levels(calib$tercil)),
                   t = 0L, S_modelo = 1, S_KM = 1)) |>                # S_0 = 1 al inicio
  pivot_longer(c(S_KM, S_modelo), names_to = "fuente", values_to = "S") |>
  mutate(fuente = if_else(fuente == "S_KM", "Observada (Kaplan–Meier)",
                                            "Predicha (media del modelo)")) |>
  ggplot(aes(t, S, color = tercil, linetype = fuente, shape = fuente)) +
  geom_step(linewidth = 0.9) + geom_point(size = 2.2) +               # constante entre revisiones
  scale_x_continuous(breaks = 0:6) +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_viridis_d(option = "plasma", end = 0.8, direction = -1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Periodo de revisión", y = "Supervivencia S(t)", color = "Riesgo predicho",
       linetype = NULL, shape = NULL)

# -----------------------------------------------------------------------------
# [u15-concordancia]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Comparación de modelos y evaluación
#         > Discriminación: índice C y AUC por periodo
# -----------------------------------------------------------------------------
lp <- predict(m_frail, newdata = transform(cohorte, t = 1))   # puntuación por paciente (log 1 = 0)
concordance(Surv(tiempo, evento) ~ lp, data = cohorte, reverse = TRUE)

# -----------------------------------------------------------------------------
# [u15-discriminacion]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Comparación de modelos y evaluación
#         > Discriminación: índice C y AUC por periodo
# -----------------------------------------------------------------------------
pp$h <- fitted(m_frail)   # riesgo ajustado de cada fila persona-periodo
pp |>
  group_by(periodo) |>
  summarise(en_riesgo = n(), fracturas = sum(y),
            AUC = as.numeric(pROC::auc(y, h, quiet = TRUE)))

# -----------------------------------------------------------------------------
# [u15-ph-test]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Diagnóstico
#         > ¿Se sostiene la proporcionalidad?
# -----------------------------------------------------------------------------
m_ph_x1 <- update(m_frail, . ~ . + x1:t)   # ¿cambia el efecto de la fragilidad con t?
m_ph_x2 <- update(m_frail, . ~ . + x2:t)   # ¿y el del tratamiento?
anova(m_frail, m_ph_x1)
anova(m_frail, m_ph_x2)

# -----------------------------------------------------------------------------
# [fig-u15-martingala]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Diagnóstico
#         > ¿Es lineal el efecto de la fragilidad? Residuos de martingala
# -----------------------------------------------------------------------------
res <- data.frame(x1   = cohorte$x1,
                  mart = cohorte$evento - tapply(pp$h, pp$id, sum)[as.character(cohorte$id)])
ggplot(res, aes(x1, mart)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Fragilidad (x1)", y = "Residuo de martingala")

# -----------------------------------------------------------------------------
# [u15-cuadratico]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Diagnóstico
#         > ¿Es lineal el efecto de la fragilidad? Residuos de martingala
# -----------------------------------------------------------------------------
anova(m_frail, update(m_frail, . ~ . + I(x1^2)))

# -----------------------------------------------------------------------------
# [u15-prediccion]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Implicaciones y predicción
# -----------------------------------------------------------------------------
nueva <- tibble(t = seq_len(Tmax), x1 = 1.5, x2 = 1)
nueva$h <- predict(m_frail, newdata = nueva, re.form = NA,      # centro típico (u_j = 0)
                   type = "response")                             # riesgo en cada revisión
nueva$S <- cumprod(1 - nueva$h)                                 # sin fractura tras t revisiones
nueva$riesgo_acum <- 1 - nueva$S
nueva

# -----------------------------------------------------------------------------
# [fig-u15-perfiles-final]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Implicaciones y predicción
# -----------------------------------------------------------------------------
perfiles_f <- expand_grid(t = seq_len(Tmax), x2 = c(0, 1), x1 = c(-2, -1, 0, 1, 2))
perfiles_f$h <- predict(m_frail, newdata = perfiles_f, re.form = NA,   # centro típico (u_j = 0)
                        type = "response")
perfiles_f <- perfiles_f |>
  arrange(x2, x1, t) |>
  group_by(x2, x1) |>                       # la supervivencia se acumula dentro de cada perfil
  mutate(S = cumprod(1 - h)) |>
  ungroup()

perfiles_f |>
  bind_rows(distinct(perfiles_f, x1, x2) |> mutate(t = 0L, S = 1)) |>   # S_0 = 1 al inicio
  ggplot(aes(t, S, color = factor(x1), group = x1)) +
  geom_step(linewidth = 1) + geom_point(size = 1.8) +                   # constante entre revisiones
  scale_x_continuous(breaks = 0:6) +
  facet_wrap(~ x2, labeller = as_labeller(c(`0` = "Sin tratamiento (x2 = 0)",
                                            `1` = "Con tratamiento (x2 = 1)"))) +
  scale_color_viridis_d(option = "plasma", end = 0.85, direction = -1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Periodo de revisión", y = "Supervivencia predicha S(t)", color = "Fragilidad (x1)")

# -----------------------------------------------------------------------------
# [u15-implicaciones]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.3 El modelo de riesgos proporcionales como GLM
#       > Implicaciones y predicción
# -----------------------------------------------------------------------------
b  <- fixef(m_frail)
ic <- confint(m_frail, parm = c("log(t)", "x1", "x2"), method = "Wald")
exp(cbind(HR = b[c("x1", "x2")], ic[c("x1", "x2"), ]))   # hazard ratios, condicionales al centro
exp(b["x1"] + b["x2"])                                    # +1 DT de fragilidad y tratada, frente a la referencia
Tmax^c(b["log(t)"], ic["log(t)", ])                      # aporte base: última revisión frente a la primera
exp(attr(VarCorr(m_frail)$centro, "stddev"))              # centro a +1 DT frente al típico

# -----------------------------------------------------------------------------
# [u15-dgp]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.4 Validación contra el DGP: ¿recuperamos la verdad?
#       > La comparación
# -----------------------------------------------------------------------------
v  <- attr(cohorte, "verdad")$binaria   # la verdad que guardó el simulador
ic <- confint(m_frail, parm = c("x1", "x2"), method = "Wald")
data.frame(verdad = v$beta, glm = coef(m_log)[c("x1", "x2")],
           glmm = fixef(m_frail)[c("x1", "x2")], ic)

# -----------------------------------------------------------------------------
# [u15-dgp-base]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.4 Validación contra el DGP: ¿recuperamos la verdad?
#       > La comparación
# -----------------------------------------------------------------------------
tibble(t      = seq_len(Tmax),
       verdad = v$alpha_base,
       glmm   = fixef(m_frail)[1] + fixef(m_frail)["log(t)"] * log(seq_len(Tmax))) |>
  mutate(diferencia = glmm - verdad)
mean(v$u)   # media de los 24 efectos de centro sorteados

# -----------------------------------------------------------------------------
# [u15-dgp-centros]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.4 Validación contra el DGP: ¿recuperamos la verdad?
#       > La comparación
# -----------------------------------------------------------------------------
c(proceso = v$sigma_u, muestra = sd(v$u),                   # DT del proceso y de los 24 sorteados
  glmm = attr(VarCorr(m_frail)$centro, "stddev"))
u_hat <- ranef(m_frail)$centro[, "(Intercept)"]             # centros en el orden 1, ..., 24
c(correlacion = cor(v$u, u_hat), pendiente = unname(coef(lm(u_hat ~ v$u))[2]))

# -----------------------------------------------------------------------------
# [fig-u15-dgp-centros]
#   5 · Supervivencia: el tiempo hasta el evento como GLM
#     > 5.4 Validación contra el DGP: ¿recuperamos la verdad?
#       > La comparación
# -----------------------------------------------------------------------------
tibble(u = v$u, u_hat = u_hat) |>
  ggplot(aes(u, u_hat)) +
  geom_abline(linetype = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#2c7fb8") +
  geom_point(size = 2.5) +
  coord_equal() +
  labs(x = "Efecto verdadero u_j", y = "Predicción del GLMM")


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
