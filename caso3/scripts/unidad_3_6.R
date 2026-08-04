# =============================================================================
# Caso 3 · Unidad 3.6 — 6 · Cox: del tiempo al riesgo
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_6.qmd.
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
# [u36-datos]
#   6 · Cox: del tiempo al riesgo > 6.1 Contexto: el reto y los datos
# -----------------------------------------------------------------------------
library(survival)
iv <- banco$intervalos
c(intervalos = nrow(iv), maquinas = length(unique(iv$id_maquina)),
  fallos = sum(iv$evento), prop_censura = round(mean(iv$evento == 0), 3))
head(iv,5)

# -----------------------------------------------------------------------------
# [u36-cox]
#   6 · Cox: del tiempo al riesgo
#     > 6.3 Estimación: la verosimilitud parcial (y por qué Cox no es un GLM)
# -----------------------------------------------------------------------------
cox_esp <- coxph(Surv(tiempo_entre, evento) ~ plan_mantenimiento + antiguedad_ini +
                   carga + proceso + fabricante, data = iv)
summary(cox_esp)

# -----------------------------------------------------------------------------
# [tbl-u36-hr]
#   6 · Cox: del tiempo al riesgo > 6.4 Interpretación
# -----------------------------------------------------------------------------
broom::tidy(cox_esp, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::transmute(term,
                   HR = round(estimate, 3),
                   conf.low = round(conf.low, 3),
                   conf.high = round(conf.high, 3),
                   p.value = signif(p.value, 3))

# -----------------------------------------------------------------------------
# [u36-aft-cox]
#   6 · Cox: del tiempo al riesgo > 6.4 Interpretación
# -----------------------------------------------------------------------------
hr_implicado_aft <- exp(-coef(aft_w)["plan_mantenimientoPreventivo"] / aft_w$scale)
hr_cox <- exp(coef(cox_esp)["plan_mantenimientoPreventivo"])

round(c(HR_implicado_por_AFT = unname(hr_implicado_aft),
        HR_estimado_por_Cox  = unname(hr_cox)), 3)

# -----------------------------------------------------------------------------
# [u36-lrt-terminos]
#   6 · Cox: del tiempo al riesgo
#     > 6.5 Inferencia y bondad de ajuste
#       > Efectos de los predictores
# -----------------------------------------------------------------------------
car::Anova(cox_esp, test.statistic = "LR")   # LRT tipo II: cada término ajustado por los demás

# -----------------------------------------------------------------------------
# [u36-aic-terminos]
#   6 · Cox: del tiempo al riesgo
#     > 6.5 Inferencia y bondad de ajuste
#       > Efectos de los predictores
# -----------------------------------------------------------------------------
# Crear el modelo actualizado
cox_esp_opt <- update(cox_esp, . ~ . - proceso)

# Combinar AIC y BIC en dos columnas
criterios <- cbind(AIC = AIC(cox_esp, cox_esp_opt)$AIC,
                   BIC = BIC(cox_esp, cox_esp_opt)$BIC)
rownames(criterios) <- c("cox_esp", "cox_esp_opt")
# Mostrar resultado
criterios

# y recalculamos la tabla de ANOVA
car::Anova(cox_esp_opt, test.statistic = "LR")

# -----------------------------------------------------------------------------
# [u36-score-logrank]
#   6 · Cox: del tiempo al riesgo
#     > 6.5 Inferencia y bondad de ajuste
#       > Efectos de los predictores
#         > El log-rank: comparar dos curvas (no medir un efecto)
# -----------------------------------------------------------------------------
# las dos vías, con el plan como único predictor
cox_plan <- coxph(Surv(tiempo_entre, evento) ~ plan_mantenimiento, data = iv)
sd_plan  <- survdiff(Surv(tiempo_entre, evento) ~ plan_mantenimiento, data = iv)

sc <- summary(cox_plan)$sctest        # test de score del Cox: test, df, pvalue
gl <- length(sd_plan$n) - 1           # log-rank: k - 1 grados de libertad

data.frame(
  via         = c("Cox · test de score", "Log-rank · survdiff"),
  estadistico = round(c(sc["test"], sd_plan$chisq), 3),
  gl          = c(sc["df"], gl),
  p_valor     = signif(c(sc["pvalue"], pchisq(sd_plan$chisq, gl, lower.tail = FALSE)), 3),
  row.names   = NULL)

# -----------------------------------------------------------------------------
# [u36-frailty]
#   6 · Cox: del tiempo al riesgo
#     > 6.6 Fragilidad: el diagnóstico de independencia
# -----------------------------------------------------------------------------
cox_esp_opt_frag <- coxph(Surv(tiempo_entre, evento) ~ plan_mantenimiento + antiguedad_ini +
                     carga + fabricante + frailty(id_maquina), data = iv)
cox_esp_opt_frag

# -----------------------------------------------------------------------------
# [u36-robust-screen]
#   6 · Cox: del tiempo al riesgo
#     > 6.6 Fragilidad: el diagnóstico de independencia
# -----------------------------------------------------------------------------
cox_esp_opt_clus <- coxph(Surv(tiempo_entre, evento) ~ plan_mantenimiento + antiguedad_ini +
                  carga + fabricante + cluster(id_maquina), data = iv)
car::Anova(cox_esp_opt_clus, test.statistic = "Wald")   # Wald por término, con SE robustos por máquina

# -----------------------------------------------------------------------------
# [u36-frailty-opt]
#   6 · Cox: del tiempo al riesgo
#     > 6.6 Fragilidad: el diagnóstico de independencia
# -----------------------------------------------------------------------------
cox_esp_final_frag <- coxph(Surv(tiempo_entre, evento) ~ plan_mantenimiento + antiguedad_ini +
                     carga + frailty(id_maquina), data = iv)
summary(cox_esp_final_frag)

# -----------------------------------------------------------------------------
# [u36-zph]
#   6 · Cox: del tiempo al riesgo
#     > 6.7 Diagnóstico de la proporcionalidad
#       > El diagnóstico sobre nuestro modelo
# -----------------------------------------------------------------------------
ph <- cox.zph(cox_esp_final_frag)
ph

# -----------------------------------------------------------------------------
# [fig-u36-zph]
#   6 · Cox: del tiempo al riesgo
#     > 6.7 Diagnóstico de la proporcionalidad
#       > El diagnóstico sobre nuestro modelo
# -----------------------------------------------------------------------------
par(mfrow = c(1, 3))
for (v in c("plan_mantenimiento", "antiguedad_ini", "carga")) {
  plot(ph, var = v)
  abline(h = 0, lty = 3, col = "red")
}

# -----------------------------------------------------------------------------
# [u36-datos-tv]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Contexto y datos
# -----------------------------------------------------------------------------
sg <- banco$seguimiento

c(maquina_meses = nrow(sg),
  meses_con_fallo = sum(sg$fallo),
  maquinas = length(unique(sg$id_maquina)))

# los ocho primeros meses de una máquina preventiva que sí se avería
head(subset(sg, id_maquina == "M227",
            select = c(tstart, tstop, antiguedad,intervalo_mant, dias_desde_mant, mant_reciente, fallo)), 8)

# -----------------------------------------------------------------------------
# [u36-fase-fallos]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Contexto y datos
# -----------------------------------------------------------------------------
sg_prev <- subset(sg, !is.na(dias_desde_mant))   # solo preventivas: las correctivas no tienen reloj
sg_prev$tramo_fase <- cut(sg_prev$dias_desde_mant / sg_prev$intervalo_mant,
                          breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)

res <- aggregate(cbind(fallos = n_fallos, exposicion) ~ tramo_fase, data = sg_prev, sum)
res

# -----------------------------------------------------------------------------
# [fig-u36-mant-fallo]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Contexto y datos
# -----------------------------------------------------------------------------
ggplot(res, aes(tramo_fase, fallos)) +
  geom_col(fill = "firebrick", alpha = 0.85) +
  labs(x = "fracción de la fase transcurrida (días desde revisión / intervalo)",
       y = "fallos acumulados")

# -----------------------------------------------------------------------------
# [fig-u36-relojes]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Contexto y datos
# -----------------------------------------------------------------------------
# Reloj 1 — edad (todas las máquinas)
d1 <- sg
d1$bin <- cut(d1$antiguedad, c(0, 2, 4, 6, 8, 10, 13))
r1 <- aggregate(cbind(n_fallos, exposicion) ~ bin, data = d1, sum)
r1$tasa <- 100 * r1$n_fallos / r1$exposicion
r1$reloj <- "Reloj 1 · edad de la máquina (años)"

# Reloj 2 — días desde el mantenimiento (preventivo)
d2 <- subset(sg, plan_mantenimiento == "Preventivo")
d2$bin <- cut(d2$dias_desde_mant, c(0, 15, 30, 45, 60, 90, 120))
r2 <- aggregate(cbind(n_fallos, exposicion) ~ bin, data = d2, sum)
r2$tasa <- 100 * r2$n_fallos / r2$exposicion
r2$reloj <- "Reloj 2 · días desde el mantenimiento"

rr <- rbind(r1[, c("bin", "tasa", "reloj")], r2[, c("bin", "tasa", "reloj")])
ggplot(rr, aes(bin, tasa)) +
  geom_col(fill = "firebrick", alpha = 0.85) +
  facet_wrap(~reloj, scales = "free_x") +
  labs(x = NULL, y = "fallos por 100 días de exposición")

# -----------------------------------------------------------------------------
# [u36-cox-ext]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > El modelo extendido
# -----------------------------------------------------------------------------
cox_mes_ind_frag <- coxph(Surv(tstart, tstop, fallo) ~ plan_mantenimiento + antiguedad +
                   carga + mant_reciente + frailty(id_maquina), data = sg)
cox_mes_ind_frag

# -----------------------------------------------------------------------------
# [u36-ext-screen]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > El modelo extendido
# -----------------------------------------------------------------------------
cox_mes_ind_clus <- coxph(Surv(tstart, tstop, fallo) ~ plan_mantenimiento + antiguedad +
                      carga + mant_reciente + cluster(id_maquina), data = sg)
car::Anova(cox_mes_ind_clus, test.statistic = "Wald")

# -----------------------------------------------------------------------------
# [u36-perfil-tau]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > La primera arbitrariedad: dar forma en vez de poner un umbral
# -----------------------------------------------------------------------------
taus <- c(20, 30, 40, 50, 60, 70, 80, 100, 120)
perfil <- sapply(taus, function(tau) {
  sg$p <- ifelse(is.na(sg$dias_desde_mant), 0, exp(-sg$dias_desde_mant / tau))
  m <- coxph(Surv(tstart, tstop, fallo) ~ antiguedad + carga + p, data = sg)
  c(AIC = AIC(m), coef_prot = coef(m)[["p"]])   # el coeficiente, para ver cómo depende de tau
})
data.frame(tau = taus, AIC = round(perfil["AIC", ], 1),
           coef_prot = round(perfil["coef_prot", ], 3))

# -----------------------------------------------------------------------------
# [u36-ext-screen2]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > La primera arbitrariedad: dar forma en vez de poner un umbral
# -----------------------------------------------------------------------------
# definimos la variable de protección por mantenimiento
sg$prot_mant <- ifelse(is.na(sg$dias_desde_mant), 0, exp(-sg$dias_desde_mant / 60))
# Ajustamos el modelo con ella
cox_mes_prot_clus <- coxph(Surv(tstart, tstop, fallo) ~ plan_mantenimiento + antiguedad +
                       carga + prot_mant + cluster(id_maquina), data = sg)
car::Anova(cox_mes_prot_clus, test.statistic = "Wald")

# -----------------------------------------------------------------------------
# [u36-cox-final]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > La primera arbitrariedad: dar forma en vez de poner un umbral
# -----------------------------------------------------------------------------
cox_mes_opt_frag <- coxph(Surv(tstart, tstop, fallo) ~ antiguedad + carga + prot_mant +
                   frailty(id_maquina), data = sg)
cox_mes_opt_frag

# -----------------------------------------------------------------------------
# [u36-comparar]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > ¿Etiqueta o reloj? La comparación
# -----------------------------------------------------------------------------
cox_mes_plan <- coxph(Surv(tstart, tstop, fallo) ~ plan_mantenimiento + antiguedad + carga,
                      data = sg)
cox_mes_opt  <- coxph(Surv(tstart, tstop, fallo) ~ antiguedad + carga + prot_mant,
                      data = sg)

# ¿altera la fragilidad los coeficientes? (cox_mes_opt_frag = cox_mes_opt + frailty)
data.frame(
  efecto         = names(coef(cox_mes_opt)),
  sin_fragilidad = round(coef(cox_mes_opt), 3),
  con_fragilidad = round(coef(cox_mes_opt_frag)[names(coef(cox_mes_opt))], 3),
  row.names      = NULL)

# -----------------------------------------------------------------------------
# [u36-comparar-aic]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > ¿Etiqueta o reloj? La comparación
# -----------------------------------------------------------------------------
data.frame(
  modelo       = c("M_plan  (5.6 trasladado al panel)", "M_reloj (final)"),
  k            = c(length(coef(cox_mes_plan)), length(coef(cox_mes_opt))),
  loglik       = round(c(logLik(cox_mes_plan), logLik(cox_mes_opt)), 1),
  AIC          = round(c(AIC(cox_mes_plan), AIC(cox_mes_opt)), 1),
  concordancia = round(c(summary(cox_mes_plan)$concordance[1],
                         summary(cox_mes_opt)$concordance[1]), 3))

# -----------------------------------------------------------------------------
# [u36-tt]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > La segunda arbitrariedad: dejar de trocear, con tt()
# -----------------------------------------------------------------------------
# 1) Calendario de revisiones de cada máquina. Para evaluar la protección en un
#    instante t cualquiera basta saber en qué día del ciclo caen las revisiones
#    (su "fase"), que leemos del panel: día de revisión = punto medio - dias_desde_mant.
cal <- do.call(rbind, lapply(split(sg, sg$id_maquina), function(d) {
  med <- (d$tstart + d$tstop) / 2
  data.frame(id_maquina     = d$id_maquina[1],
             entry          = min(d$tstart),          # primer día observado
             intervalo_mant = d$intervalo_mant[1],
             fase = if (is.na(d$intervalo_mant[1])) NA_real_ else
                      median((med - d$dias_desde_mant) %% d$intervalo_mant[1], na.rm = TRUE))
}))

# 2) Los datos, en su forma exacta: una fila por espera, en tiempo de calendario.
# recuperados de la tabla iv=banco$intervalos
ag <- do.call(rbind, lapply(split(iv, iv$id_maquina), function(d) {
  d <- d[order(d$n_intervalo), ]
  d$tstart <- cal$entry[match(d$id_maquina[1], cal$id_maquina)] +
              cumsum(c(0, head(d$tiempo_entre, -1)))
  d$tstop  <- d$tstart + d$tiempo_entre
  d
}))
ag$idx <- match(ag$id_maquina, cal$id_maquina)   # clave para consultar `cal`

# Solo para MIRARLO: los días desde la revisión al cerrarse el tramo. El modelo
# NO usa esta columna —la recalcula `tt()` en cada instante—; la añadimos para
# ver qué valor manejará. `NA` en las correctivas, que no tienen revisiones.
ag$dias_desde_mant <- round((ag$tstop - cal$fase[ag$idx]) %% cal$intervalo_mant[ag$idx], 1)

subset(ag, id_maquina == "M227",
       select = c(n_intervalo, tstart, tstop, evento, antiguedad_ini, dias_desde_mant))

# 3) El ajuste. `tt` se invoca en CADA instante de fallo con las filas en riesgo:
#    x = los `idx` de esas filas, t = el instante del fallo.
prot_tt <- function(x, t, ...) {
  fs <- cal$fase[x]; ivm <- cal$intervalo_mant[x]
  ifelse(is.na(ivm), 0, exp(-((t - fs) %% ivm) / 60))   # protección en el instante t
}

cox_tt_opt <- coxph(Surv(tstart, tstop, evento) ~ antiguedad_ini + carga + tt(idx),
                data = ag, tt = prot_tt)
round(coef(cox_tt_opt), 3)

# -----------------------------------------------------------------------------
# [fig-u36-tt-viz]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > La segunda arbitrariedad: dejar de trocear, con tt()
# -----------------------------------------------------------------------------
fase_m <- cal$fase[cal$id_maquina == "M227"]
prot_m <- function(t) exp(-((t - fase_m) %% 60) / 60)   # la misma función que pasamos a tt()

tj      <- sort(ag$tstop[ag$evento == 1])               # instantes de fallo de TODO el banco
tj      <- tj[tj <= 210]                                # ventana: dos primeras esperas de M227
propios <- ag$tstop[ag$id_maquina == "M227" & ag$evento == 1 & ag$tstop <= 210]

curva <- data.frame(t = seq(0, 210, by = 0.5)); curva$prot <- prot_m(curva$t)
riesgo <- data.frame(t = tj);                   riesgo$prot <- prot_m(riesgo$t)
panel  <- subset(sg, id_maquina == "M227" & tstop <= 210)

ggplot() +
  geom_line(data = curva, aes(t, prot), color = "grey65") +
  geom_segment(data = panel, aes(x = tstart, xend = tstop,
                                 y = prot_mant, yend = prot_mant),
               color = "steelblue", linewidth = 1) +
  geom_point(data = riesgo, aes(t, prot), color = "firebrick", size = 0.9) +
  geom_point(data = data.frame(t = propios, prot = prot_m(propios)),
             aes(t, prot), shape = 4, size = 3, stroke = 1.2) +
  labs(x = "día de seguimiento (M227)", y = "protección del mantenimiento")

# -----------------------------------------------------------------------------
# [u36-tt-frailty]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > La segunda arbitrariedad: dejar de trocear, con tt()
# -----------------------------------------------------------------------------
cox_tt_opt_frag <- coxph(Surv(tstart, tstop, evento) ~ antiguedad_ini + carga + tt(idx) +
                     frailty(id_maquina), data = ag, tt = prot_tt)
cox_tt_opt_frag

# -----------------------------------------------------------------------------
# [u36-tt-comparativa]
#   6 · Cox: del tiempo al riesgo
#     > 6.8 Covariables que cambian con el tiempo: el Cox extendido
#       > Dos arbitrariedades que conviene quitar
#         > La segunda arbitrariedad: dejar de trocear, con tt()
# -----------------------------------------------------------------------------
modelos <- list("panel mensual · sin fragilidad" = cox_mes_opt,
                "panel mensual · con fragilidad" = cox_mes_opt_frag,
                "tt() exacto   · sin fragilidad" = cox_tt_opt,
                "tt() exacto   · con fragilidad" = cox_tt_opt_frag)

# la protección es el último coeficiente en los cuatro modelos
data.frame(
  coef_prot    = sapply(modelos, function(m) round(unname(rev(coef(m))[1]), 3)),
  concordancia = sapply(modelos, function(m) round(unname(summary(m)$concordance[1]), 3)))

# -----------------------------------------------------------------------------
# [u36-pred-relativo]
#   6 · Cox: del tiempo al riesgo
#     > 6.9 Cierre: comparar, predecir, validar
#       > Predicción: del modelo a la decisión
# -----------------------------------------------------------------------------
perfiles <- data.frame(
  perfil     = c("recién mantenida", "sin protección",
                 "vieja y cargada, recién mantenida", "vieja y cargada, sin protección"),
  antiguedad = c(3, 3, 10, 10),
  carga      = c(0.50, 0.50, 0.90, 0.90),
  prot_mant  = c(1, 0, 1, 0))

perfiles$lp <- predict(cox_mes_opt, newdata = perfiles, type = "lp")
perfiles$riesgo_relativo <- round(exp(perfiles$lp - perfiles$lp[1]), 2)
perfiles[, c("perfil", "antiguedad", "carga", "prot_mant", "riesgo_relativo")]

# -----------------------------------------------------------------------------
# [u36-escenarios]
#   6 · Cox: del tiempo al riesgo
#     > 6.9 Cierre: comparar, predecir, validar
#       > Predicción: del modelo a la decisión
# -----------------------------------------------------------------------------
# construye la trayectoria de una máquina bajo una política de mantenimiento dada
escenario <- function(nombre, edad0, carga, intervalo, dias = 360) {
  t <- 0:(dias - 1)
  prot <- if (is.na(intervalo)) 0 else exp(-(t %% intervalo) / 60)
  data.frame(id = nombre, tstart = t, tstop = t + 1,
             fallo = 0,          # survfit reconstruye Surv(): la respuesta debe estar
             antiguedad = edad0 + t / 365, carga = carga, prot_mant = prot)
}

nd <- rbind(escenario("preventivo cada 60 d",  5, 0.75,  60),
            escenario("preventivo cada 120 d", 5, 0.75, 120),
            escenario("correctivo (sin revisiones)", 5, 0.75, NA))
head(nd, 4)

# -----------------------------------------------------------------------------
# [fig-u36-pred]
#   6 · Cox: del tiempo al riesgo
#     > 6.9 Cierre: comparar, predecir, validar
#       > Predicción: del modelo a la decisión
# -----------------------------------------------------------------------------
sf <- survfit(cox_mes_opt, newdata = nd, id = id)

# survfit devuelve las tres curvas encadenadas; `sf$strata` dice cuántos
# instantes aporta cada escenario, y con eso las separamos en formato largo
pred <- data.frame(tiempo    = sf$time,
                   surv      = as.vector(sf$surv),
                   escenario = rep(names(sf$strata), sf$strata))

ggplot(pred, aes(tiempo, surv, color = escenario)) +
  geom_step(linewidth = 0.8) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "días de seguimiento", y = "P(sin avería hasta t)", color = NULL)

# -----------------------------------------------------------------------------
# [u36-pred-tabla]
#   6 · Cox: del tiempo al riesgo
#     > 6.9 Cierre: comparar, predecir, validar
#       > Predicción: del modelo a la decisión
# -----------------------------------------------------------------------------
# la supervivencia al final del año: el último valor de cada curva
aggregate(surv ~ escenario, data = pred, FUN = function(s) round(tail(s, 1), 3))

# -----------------------------------------------------------------------------
# [u36-verdad]
#   6 · Cox: del tiempo al riesgo
#     > 6.9 Cierre: comparar, predecir, validar
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(banco, "verdad")
c(verdad$beta_hazard[c("antiguedad", "carga", "mant_inmediato", "tau_mant")],
  theta_frail = verdad$theta_frail)

# -----------------------------------------------------------------------------
# [tbl-u36-dgp]
#   6 · Cox: del tiempo al riesgo
#     > 6.9 Cierre: comparar, predecir, validar
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
# Las varianzas de fragilidad van transcritas de la salida impresa de cada ajuste
# (la línea "Variance of random effect"): el objeto coxph penalizado no ofrece un
# accesor estable (history[[1]]$theta llegó a devolver 0.185 para el ajuste de
# esperas, en contra de su propia salida, que dice 0.1418).
data.frame(
  parametro = c("antigüedad (por año)", "carga (0 a 1)", "protección mantenimiento",
                "varianza de fragilidad"),
  DGP     = c(verdad$beta_hazard[["antiguedad"]], verdad$beta_hazard[["carga"]],
              verdad$beta_hazard[["mant_inmediato"]], verdad$theta_frail),
  esperas = c(round(coef(cox_esp_final_frag)[["antiguedad_ini"]], 3),
              round(coef(cox_esp_final_frag)[["carga"]], 3), NA, 0.142),
  panel   = c(round(coef(cox_mes_opt_frag)[["antiguedad"]], 3),
              round(coef(cox_mes_opt_frag)[["carga"]], 3),
              round(coef(cox_mes_opt_frag)[["prot_mant"]], 3), 0.143),
  exacto  = c(round(coef(cox_tt_opt_frag)[["antiguedad_ini"]], 3),
              round(coef(cox_tt_opt_frag)[["carga"]], 3),
              round(coef(cox_tt_opt_frag)[["tt(idx)"]], 3), 0.098))

