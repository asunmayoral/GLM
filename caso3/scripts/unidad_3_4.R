# =============================================================================
# Caso 3 · Unidad 3.4 — 4 · El coste agregado: la familia Tweedie
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
# [u34-panel]
#   4 · El coste agregado: la familia Tweedie
#     > 4.1 El hueco en la recta de las varianzas
#       > Un dato que no encaja en ninguna familia
# -----------------------------------------------------------------------------
pn <- banco$panel
c(maquinas_anio = nrow(pn), maquinas = length(unique(pn$id_maquina)),
  pct_cero = round(mean(pn$coste_total == 0) * 100, 1),
  media = round(mean(pn$coste_total)),
  media_positivos = round(mean(pn$coste_total[pn$coste_total > 0])),
  maximo = round(max(pn$coste_total)))

# -----------------------------------------------------------------------------
# [u34-panel-head]
#   4 · El coste agregado: la familia Tweedie
#     > 4.1 El hueco en la recta de las varianzas
#       > Un dato que no encaja en ninguna familia
# -----------------------------------------------------------------------------
subset(pn, id_maquina == "M002",
       select = c(id_maquina, anio, coste_total, n_averias, exposicion))

# -----------------------------------------------------------------------------
# [fig-u34-panel]
#   4 · El coste agregado: la familia Tweedie
#     > 4.1 El hueco en la recta de las varianzas
#       > Un dato que no encaja en ninguna familia
# -----------------------------------------------------------------------------
p_cero <- ggplot(pn, aes(factor(coste_total > 0, levels = c(FALSE, TRUE),
                                labels = c("cero", "positivo")))) +
  geom_bar(fill = "steelblue") +
  labs(x = "coste anual", y = "nº de máquina-años")

p_pos <- ggplot(dplyr::filter(pn, coste_total > 0), aes(coste_total)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  scale_x_log10() +
  labs(x = "coste anual positivo (€, escala log)", y = NULL)

p_cero + p_pos + patchwork::plot_layout(widths = c(1, 2))

# -----------------------------------------------------------------------------
# [u34-ceros]
#   4 · El coste agregado: la familia Tweedie
#     > 4.1 El hueco en la recta de las varianzas
#       > Un dato que no encaja en ninguna familia
# -----------------------------------------------------------------------------
lambda_anual <- sum(pn$n_averias) / sum(pn$exposicion)
round(c(lambda_anual = lambda_anual, p0_poisson = exp(-lambda_anual) * 100,
        p0_observado = mean(pn$coste_total == 0) * 100), 1)

# -----------------------------------------------------------------------------
# [u34-taylor]
#   4 · El coste agregado: la familia Tweedie
#     > 4.1 El hueco en la recta de las varianzas
#       > La pendiente del panel
# -----------------------------------------------------------------------------
# un punto por maquina: media y varianza de sus (hasta 4) costes anuales
mv_panel <- pn |>
  dplyr::group_by(id_maquina) |>
  dplyr::summarise(media = mean(coste_total), varianza = var(coste_total), .groups = "drop") |>
  dplyr::filter(media > 0, varianza > 0)   # sin averia en todo el seguimiento no hay
                                           # punto: media y varianza 0 no tienen logaritmo

# en log-log, la pendiente de la recta estima la potencia p de Var = phi * mu^p
ajuste_p <- lm(log(varianza) ~ log(media), data = mv_panel)
round(c(maquinas = nrow(mv_panel), pendiente = coef(ajuste_p)[["log(media)"]],
        IC_inf = confint(ajuste_p)["log(media)", 1],
        IC_sup = confint(ajuste_p)["log(media)", 2]), 2)

# -----------------------------------------------------------------------------
# [fig-u34-taylor]
#   4 · El coste agregado: la familia Tweedie
#     > 4.1 El hueco en la recta de las varianzas
#       > La pendiente del panel
# -----------------------------------------------------------------------------
# rectas de referencia: misma pendiente p que la familia, intercepto anclado al
# centroide de la nube (asi la comparacion visual es solo de inclinacion)
refs <- tibble::tibble(p = c(1, 2),
                       familia = c("p = 1 (Poisson)", "p = 2 (Gamma)"),
                       intercepto = mean(log10(mv_panel$varianza)) - p * mean(log10(mv_panel$media)))

ggplot(mv_panel, aes(media, varianza)) +
  geom_abline(data = refs, aes(slope = p, intercept = intercepto, colour = familia),
              linetype = 2, linewidth = 0.6) +
  geom_point(alpha = 0.45, size = 1.3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.7) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c("grey55", "firebrick")) +
  labs(x = "media del coste anual por máquina (€, log)",
       y = "varianza por máquina (log)", colour = NULL)

# -----------------------------------------------------------------------------
# [u34-edad]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Los datos: cambiamos de tabla
# -----------------------------------------------------------------------------
# de granularidad mensual (sg) a anual (pn): edad media del año observado.
# El año de calendario del estudio es el mismo en las dos tablas.
sg <- banco$seguimiento          # panel mensual; aquí solo lo usamos para la edad
sg$anio <- sg$tstart %/% 365 + 1
edad_my <- aggregate(antiguedad ~ id_maquina + anio, data = sg, FUN = mean)
names(edad_my)[3] <- "edad"

pn <- merge(pn, edad_my, by = c("id_maquina", "anio"), all.x = TRUE)
c(filas = nrow(pn), edad_NA = sum(is.na(pn$edad)))

# -----------------------------------------------------------------------------
# [tbl-u34-eda]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Qué predictores, y por qué
# -----------------------------------------------------------------------------
resumen <- function(v) {
  pn |>
    dplyr::mutate(nivel = if (is.numeric(pn[[v]]))
                    dplyr::ntile(pn[[v]], 4) |> factor(labels = paste("Q", 1:4)) else
                    factor(pn[[v]])) |>
    dplyr::group_by(nivel) |>
    dplyr::summarise(eur_por_anio = round(sum(coste_total) / sum(exposicion)),
                     pct_cero = round(mean(coste_total == 0) * 100, 1), .groups = "drop") |>
    dplyr::mutate(covariable = v, .before = 1)
}

purrr::map_dfr(c("edad", "carga", "proceso", "criticidad",
                 "plan_mantenimiento", "fabricante"), resumen)

# -----------------------------------------------------------------------------
# [fig-u34-eda]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Qué predictores, y por qué
# -----------------------------------------------------------------------------
p1 <- pn |>
  dplyr::mutate(bin = cut(edad, breaks = c(0, 2, 4, 6, 8, 12))) |>
  dplyr::group_by(bin) |>
  dplyr::summarise(eur = sum(coste_total) / sum(exposicion), .groups = "drop") |>
  ggplot(aes(bin, eur)) +
  geom_col(fill = "steelblue") +
  labs(x = "edad de la máquina (años)", y = "€ por año de exposición")

p2 <- pn |>
  dplyr::group_by(criticidad, plan_mantenimiento) |>
  dplyr::summarise(eur = sum(coste_total) / sum(exposicion), .groups = "drop") |>
  ggplot(aes(criticidad, eur, fill = plan_mantenimiento)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("firebrick", "steelblue")) +
  labs(x = NULL, y = NULL, fill = NULL)

p1 + p2

# -----------------------------------------------------------------------------
# [u34-tweedie-fit]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > El ajuste: verosimilitud y estimación
# -----------------------------------------------------------------------------
m_tw <- glmmTMB(coste_total ~ edad + carga + proceso + criticidad + plan_mantenimiento +
                  fabricante + offset(log(exposicion)),
                family = tweedie, data = pn)
summary(m_tw)

# -----------------------------------------------------------------------------
# [u34-tweedie-p]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > El ajuste: verosimilitud y estimación
# -----------------------------------------------------------------------------
c(phi = sigma(m_tw),                  # dispersión: Var = phi * mu^p
  p   = family_params(m_tw)[[1]])     # índice de la familia (family_params no lo llama "p")

# -----------------------------------------------------------------------------
# [tbl-u34-tweedie]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Interpretación
# -----------------------------------------------------------------------------
broom.mixed::tidy(m_tw, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::transmute(term, factor = round(estimate, 3),
                   IC95 = sprintf("(%.2f, %.2f)", conf.low, conf.high),
                   p_valor = round(p.value, 4))

# -----------------------------------------------------------------------------
# [u34-canales]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > El precio del predictor único
# -----------------------------------------------------------------------------
mu     <- fitted(m_tw)
p_hat  <- family_params(m_tw)[[1]]   # el índice p (family_params no lo llama "p")
phi_hat <- sigma(m_tw)               # en tweedie, sigma() ES phi: Var = phi * mu^p
lambda <- mu^(2 - p_hat) / (phi_hat * (2 - p_hat))   # averías esperadas al año
mu_Y   <- (2 - p_hat) * phi_hat * mu^(p_hat - 1)     # coste esperado por avería

# frecuencia: averías esperadas al año, frente a la tasa observada
c(lambda_modelo = round(mean(lambda), 2),
  observada     = round(sum(pn$n_averias) / sum(pn$exposicion), 2))

# severidad: coste esperado por avería, frente al observado en la tabla de averías
c(coste_averia_modelo = round(mean(mu_Y)),
  observado           = round(mean(banco$averias$coste_euros)))

# -----------------------------------------------------------------------------
# [u34-reparto]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > El precio del predictor único
# -----------------------------------------------------------------------------
b <- fixef(m_tw)$cond[c("edad", "criticidadCritica", "plan_mantenimientoPreventivo")]
round(cbind(total = b, a_frecuencia = (2 - p_hat) * b, a_severidad = (p_hat - 1) * b), 3)

# -----------------------------------------------------------------------------
# [tbl-u34-lrt]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Inferencia
# -----------------------------------------------------------------------------
drop1(m_tw, test = "Chisq")

# -----------------------------------------------------------------------------
# [u34-ic-varianza]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Inferencia
# -----------------------------------------------------------------------------
ic <- confint(m_tw, full = TRUE)          # más allá de los coeficientes: dispersión y familia
ic[!grepl("^cond\\.", rownames(ic)), , drop = FALSE]

# -----------------------------------------------------------------------------
# [tbl-u34-bondad]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
m_nulo <- glmmTMB(coste_total ~ 1 + offset(log(exposicion)), family = tweedie, data = pn)

k0 <- length(fixef(m_nulo)$cond); k1 <- length(fixef(m_tw)$cond)
lrt <- as.numeric(2 * (logLik(m_tw) - logLik(m_nulo)))

data.frame(modelo  = c("nulo", "con covariables"),
           k       = c(k0, k1),
           logLik  = round(c(logLik(m_nulo), logLik(m_tw)), 1),
           AIC     = round(c(AIC(m_nulo), AIC(m_tw)), 1),
           LRT     = c(NA, round(lrt, 1)),
           gl      = c(NA, k1 - k0),
           p_valor = c(NA, signif(pchisq(lrt, k1 - k0, lower.tail = FALSE), 3)))

# -----------------------------------------------------------------------------
# [tbl-u34-calibracion]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
pn |>
  dplyr::mutate(ajustado = as.numeric(fitted(m_tw)),
                decil = dplyr::ntile(ajustado, 10)) |>
  dplyr::group_by(decil) |>
  dplyr::summarise(predicho = round(mean(ajustado)),
                   observado = round(mean(coste_total)), .groups = "drop")

# -----------------------------------------------------------------------------
# [fig-u34-calibracion]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
pn |>
  dplyr::mutate(ajustado = as.numeric(fitted(m_tw)),
                decil = dplyr::ntile(ajustado, 10)) |>
  dplyr::group_by(decil) |>
  dplyr::summarise(predicho = mean(ajustado), observado = mean(coste_total),
                   .groups = "drop") |>
  ggplot(aes(predicho, observado)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey55") +
  geom_point(size = 2.6, colour = "steelblue") +
  scale_x_log10() + scale_y_log10() +
  labs(x = "coste anual medio predicho (€, escala log)",
       y = "coste anual medio observado (€)")

# -----------------------------------------------------------------------------
# [u34-ceros-ajuste]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
round(c(ceros_observados = mean(pn$coste_total == 0) * 100,
        ceros_esperados  = mean(exp(-lambda)) * 100), 1)

# -----------------------------------------------------------------------------
# [fig-u34-dharma]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Diagnóstico
# -----------------------------------------------------------------------------
res_tw <- DHARMa::simulateResiduals(m_tw, n = 250, seed = SEMILLA_CURSO)
plot(res_tw)

# -----------------------------------------------------------------------------
# [u34-dharma-tests]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Diagnóstico
# -----------------------------------------------------------------------------
DHARMa::testDispersion(res_tw, plot = FALSE)

# -----------------------------------------------------------------------------
# [u34-agrupamiento]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Diagnóstico
# -----------------------------------------------------------------------------
u <- residuals(res_tw)                       # residuos cuantil, uniformes si el modelo es correcto
obs <- var(tapply(u, pn$id_maquina, mean))   # ¿cuánto varían las medias por máquina?

set.seed(SEMILLA_CURSO)                      # referencia: lo mismo, rompiendo el agrupamiento
nulo <- replicate(200, var(tapply(runif(nrow(pn)), pn$id_maquina, mean)))

round(c(varianza_observada = obs, esperada_si_independientes = mean(nulo),
        p_valor = mean(nulo >= obs)), 4)

# -----------------------------------------------------------------------------
# [u34-presupuesto]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Predicción: el presupuesto
# -----------------------------------------------------------------------------
nd <- subset(pn, anio == 4)   # el parque completo en su último año observado
nd$edad <- nd$edad + 1        # un año más viejas: así llegan al año 5
nd$exposicion <- 1            # presupuestamos el año entero

# predecimos en la escala del ENLACE y volvemos con exp(): así el intervalo
# respeta el soporte positivo y sale asimétrico, como debe
pr    <- predict(m_tw, newdata = nd, type = "link", se.fit = TRUE)
pred5 <- exp(pr$fit)
lo5   <- exp(pr$fit - 1.96 * pr$se.fit)
hi5   <- exp(pr$fit + 1.96 * pr$se.fit)

round(c(maquinas = nrow(nd), total_eur = sum(pred5), media_eur = mean(pred5),
        historico_eur_maq_anio = sum(pn$coste_total) / sum(pn$exposicion)))

# -----------------------------------------------------------------------------
# [u34-presupuesto-ic]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Predicción: el presupuesto
# -----------------------------------------------------------------------------
X <- model.matrix(~ edad + carga + proceso + criticidad + plan_mantenimiento + fabricante,
                  data = nd)                       # misma fórmula; el offset vale log(1) = 0
g <- as.vector(t(X) %*% pred5)                      # gradiente de la suma
se_total <- sqrt(as.numeric(t(g) %*% vcov(m_tw)$cond %*% g))

round(c(total_eur = sum(pred5), SE = se_total,
        IC_inf = sum(pred5) - 1.96 * se_total,
        IC_sup = sum(pred5) + 1.96 * se_total,
        suma_ingenua_inf = sum(lo5), suma_ingenua_sup = sum(hi5)))

# -----------------------------------------------------------------------------
# [tbl-u34-presupuesto-desglose]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Predicción: el presupuesto
# -----------------------------------------------------------------------------
data.frame(criticidad = nd$criticidad, prevision = pred5) |>
  dplyr::group_by(criticidad) |>
  dplyr::summarise(maquinas = dplyr::n(), media_eur = round(mean(prevision)),
                   total_eur = round(sum(prevision)), .groups = "drop")

# -----------------------------------------------------------------------------
# [tbl-u34-tweedie-dgp]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(banco, "verdad")
bh <- verdad$beta_hazard; bc <- verdad$beta_coste

tab_dgp <- tibble::tibble(
  term      = c("edad", "carga", "procesoMecanizado", "procesoLijado", "procesoEnsamblaje",
                "procesoAcabado", "criticidadImportante", "criticidadCritica",
                "plan_mantenimientoPreventivo", "fabricanteB", "fabricanteC"),
  frecuencia = c(bh[["antiguedad"]], bh[["carga"]], bh[["Mecanizado"]], bh[["Lijado"]],
                 bh[["Ensamblaje"]], bh[["Acabado"]], 0, 0, NA, 0, 0),
  severidad  = c(bc[["antiguedad"]], bc[["carga"]], bc[["Mecanizado"]], bc[["Lijado"]],
                 bc[["Ensamblaje"]], bc[["Acabado"]], bc[["Importante"]], bc[["Critica"]],
                 0, 0, 0)) |>
  dplyr::mutate(suma_DGP = frecuencia + severidad)

broom.mixed::tidy(m_tw, effects = "fixed") |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::transmute(term, estimado = round(estimate, 3), SE = round(std.error, 3)) |>
  dplyr::left_join(tab_dgp, by = "term")

# -----------------------------------------------------------------------------
# [u34-plan-derivado]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
# la protección decae con escala tau, y ambos valores están en la verdad del DGP
tau  <- verdad$beta_hazard[["tau_mant"]]
prot <- ifelse(is.na(sg$dias_desde_mant), 0, exp(-sg$dias_desde_mant / tau))
prot_prev <- mean(prot[sg$plan_mantenimiento == "Preventivo"])

round(c(tau = tau, E_prot_preventivo = prot_prev,
        beta_plan_derivado = verdad$beta_hazard[["mant_inmediato"]] * prot_prev), 3)

# -----------------------------------------------------------------------------
# [u34-p-dgp]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
alfa <- 1 / verdad$phi_coste          # forma de la Gamma del coste por avería
round(c(phi_coste = verdad$phi_coste, alfa = alfa,
        p_mecanismo = (alfa + 2) / (alfa + 1),
        p_estimado = p_hat,
        pendiente_marginal_6.2 = coef(ajuste_p)[["log(media)"]]), 3)

# -----------------------------------------------------------------------------
# [tbl-u34-reparto-dgp]
#   4 · El coste agregado: la familia Tweedie
#     > 4.2 El presupuesto: la Tweedie en acción
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
tab_dgp |>
  dplyr::filter(!is.na(frecuencia), suma_DGP != 0) |>
  dplyr::transmute(term,
                   pct_frecuencia_DGP = round(100 * frecuencia / suma_DGP),
                   pct_frecuencia_modelo = round(100 * (2 - p_hat)))

# -----------------------------------------------------------------------------
# [u34-varia-dentro]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > El agrupamiento, dentro del modelo
#         > ¿Solo el intercepto?
# -----------------------------------------------------------------------------
sapply(pn[c("edad", "carga", "proceso", "criticidad", "plan_mantenimiento")],
       \(v) sum(tapply(v, pn$id_maquina, \(x) length(unique(x)) > 1)))   # nº de máquinas

# -----------------------------------------------------------------------------
# [tbl-u34-pendiente]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > El agrupamiento, dentro del modelo
#         > ¿Solo el intercepto?
# -----------------------------------------------------------------------------
m_tw_re <- glmmTMB(coste_total ~ edad + carga + proceso + criticidad + plan_mantenimiento +
                     fabricante + (1 | id_maquina) + offset(log(exposicion)),
                   family = tweedie, data = pn)

m_tw_ris <- glmmTMB(coste_total ~ edad + carga + proceso + criticidad + plan_mantenimiento +
                      fabricante + (1 + edad | id_maquina) + offset(log(exposicion)),
                    family = tweedie, data = pn)

anova(m_tw_re, m_tw_ris)

# -----------------------------------------------------------------------------
# [u34-pendiente-sd]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > El agrupamiento, dentro del modelo
#         > ¿Solo el intercepto?
# -----------------------------------------------------------------------------
VarCorr(m_tw_ris)$cond$id_maquina    # desviaciones típicas y correlación de los efectos

# -----------------------------------------------------------------------------
# [tbl-u34-glmm]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > El agrupamiento, dentro del modelo
#         > ¿Solo el intercepto?
# -----------------------------------------------------------------------------
comparar <- function(m, sufijo) broom.mixed::tidy(m, effects = "fixed") |>
  dplyr::transmute(term,
                   "estim{sufijo}" := round(estimate, 3),
                   "SE{sufijo}"    := round(std.error, 3),
                   "p{sufijo}"     := round(p.value, 4))

comparar(m_tw, "_fijo") |> dplyr::left_join(comparar(m_tw_re, "_mixto"), by = "term")

# -----------------------------------------------------------------------------
# [u34-glmm-summary]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > El agrupamiento, dentro del modelo
#         > ¿Solo el intercepto?
# -----------------------------------------------------------------------------
summary(m_tw_re)

# -----------------------------------------------------------------------------
# [u34-glmm-varianza]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > El agrupamiento, dentro del modelo
#         > ¿Solo el intercepto?
# -----------------------------------------------------------------------------
c(sd_maquina = round(sqrt(unlist(VarCorr(m_tw_re)$cond)), 3),  # el parámetro nuevo
  p_fijo     = round(family_params(m_tw)[[1]], 3),             # índice sin efecto aleatorio
  p_mixto    = round(family_params(m_tw_re)[[1]], 3))          # ...y con él

# -----------------------------------------------------------------------------
# [tbl-u34-re-lrt]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Inferencia y selección
# -----------------------------------------------------------------------------
drop1(m_tw_re, test = "Chisq")

# -----------------------------------------------------------------------------
# [u34-re-final]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Inferencia y selección
# -----------------------------------------------------------------------------
m_re <- glmmTMB(coste_total ~ edad + carga + proceso + criticidad + plan_mantenimiento +
                  (1 | id_maquina) + offset(log(exposicion)),
                family = tweedie, data = pn)

broom.mixed::tidy(m_re, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::transmute(term, factor = round(estimate, 3),
                   IC95 = sprintf("(%.2f, %.2f)", conf.low, conf.high))

# -----------------------------------------------------------------------------
# [u34-re-varianzas]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Inferencia y selección
# -----------------------------------------------------------------------------
ic <- confint(m_re, full = TRUE)
# se descartan las filas de los COEFICIENTES; el resto es lo que buscamos.
# (ojo: glmmTMB prefija con "cond." tanto los coeficientes como la sd del efecto aleatorio)
ic[!rownames(ic) %in% paste0("cond.", names(fixef(m_re)$cond)), , drop = FALSE]

c(p = round(family_params(m_re)[[1]], 3))              # el índice, sin IC por esta vía

# -----------------------------------------------------------------------------
# [tbl-u34-re-aic]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
# gemelo de efectos fijos de nuestro modelo final: mismo predictor, sin (1 | id_maquina)
m_fijo_sf <- glmmTMB(coste_total ~ edad + carga + proceso + criticidad + plan_mantenimiento +
                       offset(log(exposicion)), family = tweedie, data = pn)

lrt_re <- as.numeric(2 * (logLik(m_re) - logLik(m_fijo_sf)))   # solo difieren en sigma_u

data.frame(
  modelo = c("efectos fijos (6.3, con fabricante)", "efectos fijos, sin fabricante",
             "+ efecto aleatorio (final)"),
  k      = c(attr(logLik(m_tw), "df"), attr(logLik(m_fijo_sf), "df"), attr(logLik(m_re), "df")),
  logLik = round(c(logLik(m_tw), logLik(m_fijo_sf), logLik(m_re)), 1),
  AIC    = round(c(AIC(m_tw), AIC(m_fijo_sf), AIC(m_re)), 1),
  BIC    = round(c(BIC(m_tw), BIC(m_fijo_sf), BIC(m_re)), 1),
  LRT    = c(NA, NA, round(lrt_re, 1)),
  # varianza en la frontera: la referencia es una mezcla, y el p-valor se divide por dos
  p_valor = c(NA, NA, signif(0.5 * pchisq(lrt_re, 1, lower.tail = FALSE), 3)))

# -----------------------------------------------------------------------------
# [u34-re-ceros]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
mu_re  <- fitted(m_re)
p_re   <- family_params(m_re)[[1]]; phi_re <- sigma(m_re)
lam_re <- mu_re^(2 - p_re) / (phi_re * (2 - p_re))   # averías esperadas en cada máquina-año

round(c(ceros_observados = mean(pn$coste_total == 0) * 100,
        ceros_esperados  = mean(exp(-lam_re)) * 100), 1)

# -----------------------------------------------------------------------------
# [tbl-u34-re-calibracion]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
calibrar <- function(ajuste, etiqueta) {
  tibble::tibble(modelo = etiqueta, ajustado = as.numeric(ajuste),
                 observado = pn$coste_total) |>
    dplyr::mutate(decil = dplyr::ntile(ajustado, 10)) |>
    dplyr::group_by(modelo, decil) |>
    dplyr::summarise(predicho = round(mean(ajustado)),
                     observado = round(mean(observado)), .groups = "drop")
}

calibracion <- dplyr::bind_rows(calibrar(fitted(m_tw), "efectos fijos"),
                                calibrar(mu_re, "efecto aleatorio"))
tidyr::pivot_wider(calibracion, names_from = modelo,
                   values_from = c(predicho, observado))

# -----------------------------------------------------------------------------
# [fig-u34-re-calibracion-diag]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
calibracion |>
  ggplot(aes(predicho, observado, colour = modelo)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey55") +
  geom_path(alpha = 0.5, linewidth = 0.6) +
  geom_point(size = 2.6) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c(`efectos fijos` = "grey35",
                                 `efecto aleatorio` = "steelblue")) +
  labs(x = "coste medio predicho en el decil (€, escala log)",
       y = "coste medio observado (€, escala log)", colour = NULL)

# -----------------------------------------------------------------------------
# [fig-u34-re-calibracion]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
pn |>
  dplyr::mutate(
    tramo = dplyr::if_else(coste_total == 0, "sin gasto",
                           paste0("Q", dplyr::ntile(dplyr::na_if(coste_total, 0), 5))),
    tramo = factor(tramo, levels = c("sin gasto", paste0("Q", 1:5))),
    `efectos fijos`    = as.numeric(fitted(m_tw)),
    `efecto aleatorio` = as.numeric(mu_re)) |>
  dplyr::group_by(tramo) |>
  dplyr::summarise(observado          = mean(coste_total),
                   `efectos fijos`    = mean(`efectos fijos`),
                   `efecto aleatorio` = mean(`efecto aleatorio`), .groups = "drop") |>
  tidyr::pivot_longer(-tramo, names_to = "serie", values_to = "eur") |>
  ggplot(aes(tramo, eur, colour = serie, group = serie)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.4) +
  scale_colour_manual(values = c(observado = "firebrick",
                                 `efectos fijos` = "grey55",
                                 `efecto aleatorio` = "steelblue")) +
  labs(x = "grupo según el gasto realmente observado", y = "coste anual medio (€)", colour = NULL)

# -----------------------------------------------------------------------------
# [fig-u34-re-dharma]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Diagnóstico
# -----------------------------------------------------------------------------
res_re <- DHARMa::simulateResiduals(m_re, n = 250, seed = SEMILLA_CURSO)
plot(res_re)

# -----------------------------------------------------------------------------
# [u34-re-dharma-tests]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Diagnóstico
# -----------------------------------------------------------------------------
DHARMa::testDispersion(res_re, plot = FALSE)

# -----------------------------------------------------------------------------
# [u34-re-diagnostico]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Diagnóstico
# -----------------------------------------------------------------------------
u_re <- residuals(res_re)
obs_re <- var(tapply(u_re, pn$id_maquina, mean))

set.seed(SEMILLA_CURSO)
nulo_re <- replicate(200, var(tapply(runif(nrow(pn)), pn$id_maquina, mean)))

round(c(varianza_observada = obs_re, esperada_si_independientes = mean(nulo_re),
        p_valor = mean(nulo_re >= obs_re)), 4)

# -----------------------------------------------------------------------------
# [u34-re-presupuesto]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Predicción: el presupuesto revisado
# -----------------------------------------------------------------------------
nd_re <- subset(pn, anio == 4); nd_re$edad <- nd_re$edad + 1; nd_re$exposicion <- 1

cond <- predict(m_re, newdata = nd_re, type = "response")                   # con su u_i
marg <- predict(m_re, newdata = nd_re, type = "response", re.form = NA)     # con u = 0
sd_u <- unname(sqrt(unlist(VarCorr(m_re)$cond)))

round(c(condicional        = sum(cond),
        marginal_u_cero    = sum(marg),
        marginal_corregida = sum(marg) * exp(sd_u^2 / 2),   # corrección de Jensen
        efectos_fijos_6.3  = sum(pred5)))

# -----------------------------------------------------------------------------
# [u34-re-predictiva]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Predicción: el presupuesto revisado
#         > Del valor esperado al intervalo de predicción
# -----------------------------------------------------------------------------
alfa_re  <- (2 - p_re) / (p_re - 1)                     # forma de la Gamma de cada avería
lambda_i <- cond^(2 - p_re) / (phi_re * (2 - p_re))     # averías esperadas de cada máquina
theta_i  <- phi_re * (p_re - 1) * cond^(p_re - 1)       # escala de su coste

set.seed(SEMILLA_CURSO)
totales <- replicate(4000, {
  N <- rpois(length(cond), lambda_i)                    # ¿cuántas averías tiene cada máquina?
  sum(rgamma(length(cond), shape = N * alfa_re, scale = theta_i))   # ...y cuánto cuestan
})

round(c(prevision      = sum(cond),
        media_simulada = mean(totales),
        IC90_inf       = unname(quantile(totales, 0.05)),
        IC90_sup       = unname(quantile(totales, 0.95)),
        percentil90    = unname(quantile(totales, 0.90))))

# -----------------------------------------------------------------------------
# [u34-re-predictiva-maquina]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Predicción: el presupuesto revisado
#         > Del valor esperado al intervalo de predicción
# -----------------------------------------------------------------------------
i <- order(cond)[length(cond) %/% 2]                    # una máquina de previsión mediana
una <- rgamma(4000, shape = rpois(4000, lambda_i[i]) * alfa_re, scale = theta_i[i])

round(c(prevision = cond[i], prob_cero_pct = mean(una == 0) * 100,
        mediana = median(una), percentil90 = unname(quantile(una, 0.90))))

# -----------------------------------------------------------------------------
# [tbl-u34-validacion-re]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
sd_logZ <- sqrt(trigamma(1 / verdad$theta_frail))   # sd(log Z) de la fragilidad del DGP

tibble::tibble(
  cantidad = c("sd del efecto de máquina", "índice p"),
  estimado = c(round(sqrt(unlist(VarCorr(m_re)$cond)), 3),
               round(family_params(m_re)[[1]], 3)),
  verdad   = c(round(2 * sd_logZ, 3),
               round((1/verdad$phi_coste + 2) / (1/verdad$phi_coste + 1), 3)),
  de_donde = c("2 × sd(log Z): la fragilidad entra dos veces",
               "(alfa+2)/(alfa+1), con alfa = 1/phi del coste"))

# -----------------------------------------------------------------------------
# [tbl-u34-cv]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > ¿El efecto de máquina predice, o solo describe?
# -----------------------------------------------------------------------------
# la devianza se evalúa con un p COMÚN: cada ajuste del bucle estima el suyo y,
# con índices distintos, las devianzas no serían promediables
p_cv <- family_params(m_re)[[1]]
dev_tw <- function(y, mu, p = p_cv) {
  mean(2 * (y^(2-p)/((1-p)*(2-p)) - y*mu^(1-p)/(1-p) + mu^(2-p)/(2-p)))
}

set.seed(SEMILLA_CURSO)
pliegues <- rsample::vfold_cv(pn, v = 5)

purrr::map_dfr(pliegues$splits, function(s) {
  ent <- rsample::analysis(s); pru <- rsample::assessment(s)
  m   <- glmmTMB(formula(m_re), family = tweedie, data = ent)
  c(condicional = dev_tw(pru$coste_total,
                         predict(m, pru, type = "response", allow.new.levels = TRUE)),
    marginal    = dev_tw(pru$coste_total,
                         predict(m, pru, type = "response", re.form = NA)))
}) |>
  dplyr::summarise(dplyr::across(dplyr::everything(),
                                 list(media = mean, ee = \(x) sd(x) / sqrt(length(x))))) |>
  round(1)

# -----------------------------------------------------------------------------
# [tbl-u34-cv-grupos]
#   4 · El coste agregado: la familia Tweedie
#     > 4.3 Datos agrupados: modelo y validación
#       > ¿El efecto de máquina predice, o solo describe?
#         > Y si la máquina fuera nueva: la partición por grupos
# -----------------------------------------------------------------------------
set.seed(SEMILLA_CURSO)
pliegues_maq <- rsample::group_vfold_cv(pn, group = id_maquina, v = 5)

d_nuevas <- purrr::map_dbl(pliegues_maq$splits, function(s) {
  ent <- rsample::analysis(s); pru <- rsample::assessment(s)
  m   <- glmmTMB(formula(m_re), family = tweedie, data = ent)
  dev_tw(pru$coste_total, predict(m, pru, type = "response", re.form = NA))
})

round(c(maquina_nueva_media = mean(d_nuevas),
        maquina_nueva_ee    = sd(d_nuevas) / sqrt(length(d_nuevas))), 1)

