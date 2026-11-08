# =============================================================================
# Caso 2 · Unidad 2.5 — 5 · Efectos Aleatorios y Modelos Mixtos
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_5.qmd.
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
  DHARMa = "0.5.0", MASS = "7.3.65", broom = "1.0.13", dplyr = "1.2.1",
  forcats = "1.0.1", ggplot2 = "4.0.3", glmmTMB = "1.1.14", glmnet = "5.0",
  insight = "1.5.1", marginaleffects = "0.32.0", performance = "0.17.0",
  purrr = "1.2.2", sessioninfo = "1.2.4", survival = "3.8.6",
  tibble = "3.3.1", tidyverse = "2.0.0", vcd = "1.4.13", vcdExtra = "0.9.6")
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
# Núcleo.
library(broom)
library(tidyverse)
library(MASS)          # glm.nb (base-recommended)
library(glmmTMB)       # conteos mixtos / ceros
library(DHARMa); library(performance); library(marginaleffects)
library(survival)      # riesgos a trozos (base-recommended)
library(glmnet)       # regularización (2.1)
library(vcd)           # mosaicos para tablas de contingencia (2.2)
library(vcdExtra)      # zero-inflated (2.4) y utilidades de tablas (2.2)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
source(url_glm("caso2/R/dgp_panel.R"))   # funciones del proceso generador
panel <- cargar_panel_partes(cartera)
glimpse(cartera)
head(cartera)

# -----------------------------------------------------------------------------
# [fig-u25-anidamiento]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.1 Agrupamiento y anidamiento
# -----------------------------------------------------------------------------
tasa_global <- sum(cartera$n_gestiones) / sum(cartera$exposicion)
cartera |>
  dplyr::group_by(area = region, agencia) |>
  dplyr::summarise(tasa = sum(n_gestiones) / sum(exposicion), .groups = "drop") |>
  ggplot(aes(area, tasa, colour = area)) +
  geom_hline(yintercept = tasa_global, linetype = 2, colour = "grey50") +
  stat_summary(fun = mean, geom = "crossbar", width = 0.6, colour = "grey25") +
  geom_jitter(width = 0.15, height = 0, size = 2, alpha = 0.8) +
  scale_y_log10() +                       # la escala del modelo: los efectos son multiplicativos
  labs(x = "región", y = "tasa de gestiones (por unidad de exposición, escala log)") +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# [fig-u25-eda]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.2 El modelo mixto anidado y su estimación
#       > Qué predictores incluir: análisis exploratorio
# -----------------------------------------------------------------------------
# sum(y)/sum(e) es el EMV de la tasa Poisson del grupo; su error estándar aproximado, sqrt(sum y)/sum(e)
tasa_por_nivel <- function(var, nivel) {
  tibble::tibble(nivel = nivel, y = cartera$n_gestiones, e = cartera$exposicion) |>
    dplyr::group_by(nivel) |>
    dplyr::summarise(tasa = sum(y) / sum(e), ee = sqrt(sum(y)) / sum(e), .groups = "drop") |>
    dplyr::mutate(predictor = var, nivel = as.character(nivel))
}
continuos   <- c("edad_conductor", "antiguedad_carnet", "potencia_cv", "antiguedad_vehiculo",
                 "km_declarados", "valor_vehiculo", "antiguedad_cliente")
categoricos <- c("zona_circulacion", "uso", "tipo_vehiculo", "sexo", "estado_civil",
                 "color_vehiculo", "tiene_garaje", "forma_pago", "financiado")
tasas_eda <- dplyr::bind_rows(
  purrr::map(continuos,   \(v) tasa_por_nivel(v, ggplot2::cut_number(cartera[[v]], 4, dig.lab = 6))),
  purrr::map(categoricos, \(v) tasa_por_nivel(v, cartera[[v]]))) |>
  dplyr::mutate(nivel     = forcats::fct_inorder(nivel),
                predictor = factor(predictor, levels = c(continuos, categoricos)))
ggplot(tasas_eda, aes(nivel, tasa)) +
  geom_hline(yintercept = sum(cartera$n_gestiones) / sum(cartera$exposicion),
             linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = tasa - 1.96 * ee, ymax = tasa + 1.96 * ee), colour = "steelblue") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 4) +
  labs(x = NULL, y = "tasa de gestiones (por unidad de exposición)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7))

# -----------------------------------------------------------------------------
# [u25-cribado]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.2 El modelo mixto anidado y su estimación
#       > Qué predictores incluir: análisis exploratorio
# -----------------------------------------------------------------------------
m_nulo <- glmmTMB(n_gestiones ~ offset(log(exposicion)) + (1 | region/agencia),
                  family = poisson, data = cartera)
p_sin_agrupar <- function(v) {          # la misma comparación, ignorando el agrupamiento
  g0 <- glm(n_gestiones ~ offset(log(exposicion)), family = poisson, data = cartera)
  anova(g0, update(g0, as.formula(paste(". ~ . +", v))), test = "Chisq")[2, "Pr(>Chi)"]
}
cribado <- t(sapply(c(continuos, categoricos), \(v) {
  m_v <- update(m_nulo, as.formula(paste(". ~ . +", v)))
  LR  <- 2 * as.numeric(logLik(m_v) - logLik(m_nulo))
  gl  <- attr(logLik(m_v), "df") - attr(logLik(m_nulo), "df")
  c(gl = gl, LR = LR, p = pchisq(LR, gl, lower.tail = FALSE),
    dif_AIC = AIC(m_v) - AIC(m_nulo), p_sin_agrupar = p_sin_agrupar(v))
}))
round(cribado[order(cribado[, "p"]), ], 4)

# -----------------------------------------------------------------------------
# [u25-cribado-edad]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.2 El modelo mixto anidado y su estimación
#       > Qué predictores incluir: análisis exploratorio
# -----------------------------------------------------------------------------
cor(cartera$edad_conductor, cartera$antiguedad_carnet)
m_edad_carnet <- update(m_nulo, . ~ . + edad_conductor + antiguedad_carnet)
LR_ec <- 2 * as.numeric(logLik(m_edad_carnet) - logLik(m_nulo))
c(LR = LR_ec, p = pchisq(LR_ec, 2, lower.tail = FALSE))

# -----------------------------------------------------------------------------
# [u25-glmm]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.2 El modelo mixto anidado y su estimación
#       > Ajuste del modelo
# -----------------------------------------------------------------------------
cartera$potencia_c <- cartera$potencia_cv - mean(cartera$potencia_cv)   # CV respecto a la media
m_pois <- glm(n_gestiones ~ potencia_c + uso + offset(log(exposicion)),
              family = poisson, data = cartera)

m_glmm <- glmmTMB(
  n_gestiones ~ potencia_c + uso + offset(log(exposicion)) + (1 | region/agencia),
  family = poisson, data = cartera
)
# el optimizador terminó bien y el hessiano es definido positivo (errores estándar fiables)
c(convergencia = m_glmm$fit$convergence == 0, hessiano_def_pos = m_glmm$sdr$pdHess)
summary(m_glmm)

# -----------------------------------------------------------------------------
# [u25-irr]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Efectos fijos: IRR condicionales
# -----------------------------------------------------------------------------
ef <- summary(m_glmm)$coefficients$cond
data.frame(
  #termino   = rownames(ef),
  IRR       = exp(ef[, "Estimate"]),
  conf.low  = exp(ef[, "Estimate"] - 1.96 * ef[, "Std. Error"]),
  conf.high = exp(ef[, "Estimate"] + 1.96 * ef[, "Std. Error"])
) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))

# -----------------------------------------------------------------------------
# [u25-vc]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Componentes de varianza: cuánto varía cada nivel
# -----------------------------------------------------------------------------
VarCorr(m_glmm)
vc <- VarCorr(m_glmm)$cond
sd_reg <- attr(vc$region, "stddev")[[1]]
sd_ag  <- attr(vc$`agencia:region`, "stddev")[[1]]
# intervalos de Wald, construidos en la escala log de cada desviación típica
ic_sd <- confint(m_glmm, parm = "theta_")
ic_sd
# correlación entre las ESTIMACIONES de las dos desviaciones típicas (escala log)
V_full <- vcov(m_glmm, full = TRUE)
theta  <- grep("^theta", colnames(V_full))
round(cov2cor(V_full)[theta, theta], 3)

# -----------------------------------------------------------------------------
# [u25-varcorr]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Componentes de varianza: cuánto varía cada nivel
# -----------------------------------------------------------------------------
round(c(misma_agencia = sd_reg^2 + sd_ag^2, misma_region_otra_agencia = sd_reg^2,
        distinta_region = 0), 4)

# -----------------------------------------------------------------------------
# [u25-particion]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Componentes de varianza: cuánto varía cada nivel
# -----------------------------------------------------------------------------
var_comp <- insight::get_variance(m_glmm)
particion <- c(efectos_fijos = var_comp$var.fixed,
               region        = var_comp$var.intercept[["region"]],
               agencia       = var_comp$var.intercept[["agencia:region"]],
               poisson       = var_comp$var.distribution)
round(rbind(varianza = particion, fraccion = particion / sum(particion)), 3)

# -----------------------------------------------------------------------------
# [u25-r2-icc]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Componentes de varianza: cuánto varía cada nivel
# -----------------------------------------------------------------------------
performance::r2(m_glmm)                     # marginal: fijos; condicional: fijos + región + agencia
performance::icc(m_glmm, by_group = TRUE)   # cada nivel, sin contar los efectos fijos

# -----------------------------------------------------------------------------
# [u25-test-varianza]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Componentes de varianza: cuánto varía cada nivel
# -----------------------------------------------------------------------------
m_sin_reg <- update(m_glmm, . ~ . - (1 | region/agencia) + (1 | agencia))   # solo agencia
m_sin_ag  <- update(m_glmm, . ~ . - (1 | region/agencia) + (1 | region))    # solo región
LR <- c(region  = 2 * (as.numeric(logLik(m_glmm)) - as.numeric(logLik(m_sin_reg))),
        agencia = 2 * (as.numeric(logLik(m_glmm)) - as.numeric(logLik(m_sin_ag))))
data.frame(LR          = round(LR, 2),
           p_ingenuo   = signif(pchisq(LR, df = 1, lower.tail = FALSE), 3),
           p_corregido = signif(0.5 * pchisq(LR, df = 1, lower.tail = FALSE), 3))   # mezcla 50:50

# -----------------------------------------------------------------------------
# [fig-u25-blups]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Efectos aleatorios predichos (BLUP): el factor de cada agencia
# -----------------------------------------------------------------------------
re_glmm <- ranef(m_glmm, condVar = TRUE)$cond
re    <- re_glmm$`agencia:region`
re_df <- tibble::tibble(
  agencia = rownames(re),
  b  = re[, 1],
  se = sqrt(as.numeric(attr(re, "condVar")))
) |>
  dplyr::mutate(agencia = forcats::fct_reorder(agencia, b))
ggplot(re_df, aes(b, agencia)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_pointrange(aes(xmin = b - 1.96 * se, xmax = b + 1.96 * se), size = 0.3) +
  labs(x = "desviación del log-ritmo (BLUP)", y = "agencia")

# -----------------------------------------------------------------------------
# [u25-blups-resumen]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Efectos aleatorios predichos (BLUP): el factor de cada agencia
# -----------------------------------------------------------------------------
# agencias cuyo intervalo excluye el 0, y el rango de sus factores multiplicativos
c(agencias = nrow(re_df), excluyen_cero = sum(abs(re_df$b) > 1.96 * re_df$se),
  factor_min = round(exp(min(re_df$b)), 2), factor_max = round(exp(max(re_df$b)), 2))

# -----------------------------------------------------------------------------
# [u25-shrinkage]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Efectos aleatorios predichos (BLUP): el factor de cada agencia
# -----------------------------------------------------------------------------
re_reg   <- re_glmm$region
eta_fijo <- drop(getME(m_glmm, "X") %*% fixef(m_glmm)$cond) + log(cartera$exposicion)
shrink <- cartera |>
  dplyr::mutate(esperado = exp(eta_fijo + re_reg[as.character(region), 1]),   # agencia típica de su región
                mu       = fitted(m_glmm)) |>
  dplyr::group_by(agencia, region) |>
  dplyr::summarise(polizas = dplyr::n(), observado = sum(n_gestiones),
                   esperado = sum(esperado), I = sum(mu), .groups = "drop") |>
  dplyr::mutate(crudo  = log(observado / esperado),
                lambda = I * sd_ag^2 / (1 + I * sd_ag^2),
                blup   = re[paste(agencia, region, sep = ":"), 1]) |>
  dplyr::arrange(I)
# las 3 agencias con menos información y las 3 con más
dplyr::bind_rows(head(shrink, 3), tail(shrink, 3)) |>
  dplyr::select(agencia, region, polizas, observado, I, crudo, lambda, blup) |>
  dplyr::mutate(dplyr::across(c(I, crudo, lambda, blup), \(x) round(x, 2)))

# -----------------------------------------------------------------------------
# [u25-pred]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.3 Interpretación y predicción
#       > Predicción de la tasa y su incertidumbre
# -----------------------------------------------------------------------------
nd <- cartera |>
  dplyr::slice(1) |>
  dplyr::mutate(exposicion = 1)                       # ventana de exposición completa
p_obs <- predict(m_glmm, nd, type = "link", se.fit = TRUE)                 # su agencia
p_tip <- predict(m_glmm, nd, type = "link", se.fit = TRUE, re.form = NA)   # agencia típica
s2_u  <- sd_reg^2 + sd_ag^2
ic <- \(eta, se) exp(eta + c(-1.96, 0, 1.96) * se)    # IC en escala log, devuelto a la tasa
pred_tabla <- rbind(
  agencia_observada  = ic(p_obs$fit, p_obs$se.fit),
  agencia_tipica     = ic(p_tip$fit, p_tip$se.fit),
  agencia_nueva_media = ic(p_tip$fit, p_tip$se.fit) * exp(s2_u / 2),
  agencia_nueva_pred = ic(p_tip$fit, sqrt(p_tip$se.fit^2 + s2_u))
)
colnames(pred_tabla) <- c("inferior", "tasa", "superior")
round(pred_tabla, 3)
c(u_region = re_reg[as.character(nd$region), 1])   # BLUP de la región de la póliza

# -----------------------------------------------------------------------------
# [u25-drop1]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.4 Selección de variables y distribución
#       > Selección de variables
# -----------------------------------------------------------------------------
drop1(m_glmm, test = "Chisq")

# -----------------------------------------------------------------------------
# [u25-anadir]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.4 Selección de variables y distribución
#       > Selección de variables
# -----------------------------------------------------------------------------
continuos_rest   <- setdiff(continuos, "potencia_cv")
categoricos_rest <- setdiff(categoricos, "uso")
anadir <- t(sapply(c(continuos_rest, categoricos_rest), \(v) {
  termino <- if (v %in% continuos_rest) paste0("scale(", v, ")") else v
  m_v <- update(m_glmm, as.formula(paste(". ~ . +", termino)))
  LR  <- 2 * as.numeric(logLik(m_v) - logLik(m_glmm))
  gl  <- attr(logLik(m_v), "df") - attr(logLik(m_glmm), "df")
  c(gl = gl, LR = LR, p = pchisq(LR, gl, lower.tail = FALSE),
    dif_AIC = AIC(m_v) - AIC(m_glmm), dif_BIC = BIC(m_v) - BIC(m_glmm))
}))
round(anadir[order(anadir[, "p"]), ], 4)

# -----------------------------------------------------------------------------
# [u25-pendientes-lrt]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.4 Selección de variables y distribución
#       > Selección de variables
# -----------------------------------------------------------------------------
cartera_pend <- dplyr::mutate(cartera,
  comercial_c = as.numeric(uso == "comercial") - mean(uso == "comercial"))
m_pend <- list(
  potencia  = update(m_glmm, . ~ . + (0 + potencia_c | region), data = cartera_pend),
  comercial = update(m_glmm, . ~ . + (0 + comercial_c | region), data = cartera_pend))
LR_pend <- pmax(sapply(m_pend, \(m) 2 * as.numeric(logLik(m) - logLik(m_glmm))), 0)
round(cbind(LR = LR_pend,
            sd_pendiente = sapply(m_pend, \(m) attr(VarCorr(m)$cond[[3]], "stddev")[[1]]),
            p_corregido  = 0.5 * pchisq(LR_pend, 1, lower.tail = FALSE)), 4)

# -----------------------------------------------------------------------------
# [u25-aic]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.4 Selección de variables y distribución
#       > Agrupamiento versus sobredispersión
# -----------------------------------------------------------------------------
m_nb <- glm.nb(n_gestiones ~ potencia_c + uso + offset(log(exposicion)), data = cartera)
performance::compare_performance(
  Poisson = m_pois, NB = m_nb, GLMM = m_glmm,
  metrics = c("AIC", "BIC")
)

# -----------------------------------------------------------------------------
# [u25-disp-glmm]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.4 Selección de variables y distribución
#       > Agrupamiento versus sobredispersión
# -----------------------------------------------------------------------------
disp_pois <- performance::check_overdispersion(m_pois)
disp_glmm <- performance::check_overdispersion(m_glmm)
round(c(ratio_poisson = disp_pois$dispersion_ratio, p_poisson = disp_pois$p_value,
        ratio_glmm    = disp_glmm$dispersion_ratio, p_glmm    = disp_glmm$p_value), 3)

# -----------------------------------------------------------------------------
# [u25-deviance]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.5 Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
c(deviance = round(deviance(m_glmm), 1), gl_residuales = df.residual(m_glmm),
  media_ajustada = round(mean(fitted(m_glmm)), 3),
  pct_ceros = round(100 * mean(cartera$n_gestiones == 0), 1))

# -----------------------------------------------------------------------------
# [fig-u25-dharma]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.5 Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
res_glmm <- simulateResiduals(m_glmm)
plot(res_glmm)

# -----------------------------------------------------------------------------
# [u25-dharma-tests]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.5 Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
# p-valores de los contrastes de DHARMa sobre el GLMM
round(c(uniformidad = testUniformity(res_glmm, plot = FALSE)$p.value,
        dispersion  = testDispersion(res_glmm, plot = FALSE)$p.value,
        atipicos    = testOutliers(res_glmm, plot = FALSE)$p.value,
        ceros       = testZeroInflation(res_glmm, plot = FALSE)$p.value,
        cuantiles   = testQuantiles(res_glmm, plot = FALSE)$p.value), 3)

# -----------------------------------------------------------------------------
# [u25-dgp]
#   5 · Efectos Aleatorios y Modelos Mixtos > 5.6 Validación contra el DGP
# -----------------------------------------------------------------------------
verdad   <- attr(cartera, "verdad")
b_verdad <- setNames(verdad$betas$grupo, verdad$nombres_beta)
se_fijos <- summary(m_glmm)$coefficients$cond[, "Std. Error"]
dt_pot   <- sd(cartera$potencia_cv)                     # para pasar la potencia a «por desviación típica»
validacion <- data.frame(
  verdad   = c(verdad$sigma_reg, verdad$sigma_a, b_verdad[["z_pot"]], b_verdad[["comercial"]]),
  estimado = c(sd_reg, sd_ag, fixef(m_glmm)$cond[["potencia_c"]] * dt_pot,
               fixef(m_glmm)$cond[["usocomercial"]]),
  inferior = c(ic_sd["Std.Dev.(Intercept)|region", "2.5 %"],
               ic_sd["Std.Dev.(Intercept)|agencia:region", "2.5 %"],
               (fixef(m_glmm)$cond[["potencia_c"]] - 1.96 * se_fijos[["potencia_c"]]) * dt_pot,
               fixef(m_glmm)$cond[["usocomercial"]] - 1.96 * se_fijos[["usocomercial"]]),
  superior = c(ic_sd["Std.Dev.(Intercept)|region", "97.5 %"],
               ic_sd["Std.Dev.(Intercept)|agencia:region", "97.5 %"],
               (fixef(m_glmm)$cond[["potencia_c"]] + 1.96 * se_fijos[["potencia_c"]]) * dt_pot,
               fixef(m_glmm)$cond[["usocomercial"]] + 1.96 * se_fijos[["usocomercial"]]),
  row.names = c("sigma_region", "sigma_agencia", "potencia (por DT)", "uso comercial"))
validacion$verdad_en_IC <- with(validacion, verdad >= inferior & verdad <= superior)
validacion |> dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))

# -----------------------------------------------------------------------------
# [u25-olre-disp]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.7 El efecto aleatorio a nivel de observación (OLRE)
#       > El OLRE en la cartera
# -----------------------------------------------------------------------------
m_pois_danos <- glm(
  n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)),
  family = poisson, data = cartera
)
performance::check_overdispersion(m_pois_danos)

# -----------------------------------------------------------------------------
# [u25-olre]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.7 El efecto aleatorio a nivel de observación (OLRE)
#       > El OLRE en la cartera
# -----------------------------------------------------------------------------
cartera$id_obs <- factor(seq_len(nrow(cartera)))   # un nivel aleatorio por observación
m_olre <- glmmTMB(
  n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)) + (1 | id_obs),
  family = poisson, data = cartera
)
c(convergencia = m_olre$fit$convergence == 0, hessiano_def_pos = m_olre$sdr$pdHess)
sd_olre <- attr(VarCorr(m_olre)$cond$id_obs, "stddev")[[1]]
ic_olre <- confint(m_olre, parm = "theta_")         # IC de Wald en escala log, devuelto a sigma
ic_olre

# -----------------------------------------------------------------------------
# [u25-olre-test]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.7 El efecto aleatorio a nivel de observación (OLRE)
#       > El OLRE en la cartera
# -----------------------------------------------------------------------------
LR_olre <- 2 * (as.numeric(logLik(m_olre)) - as.numeric(logLik(m_pois_danos)))
c(LR = LR_olre, p_corregido = 0.5 * pchisq(LR_olre, 1, lower.tail = FALSE))

# -----------------------------------------------------------------------------
# [fig-u25-olre-diag]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.7 El efecto aleatorio a nivel de observación (OLRE)
#       > El OLRE en la cartera
# -----------------------------------------------------------------------------
set.seed(2025)                                                   # simulaciones reproducibles
n_pol    <- nrow(cartera)
lam_olre <- predict(m_olre, re.form = NA, type = "response")     # tasa sin el impulso de la póliza
sim_olre <- replicate(250, rpois(n_pol, lam_olre * exp(rnorm(n_pol, 0, sd_olre))))
res_olre <- DHARMa::createDHARMa(
  simulatedResponse       = sim_olre,
  observedResponse        = cartera$n_danos,
  fittedPredictedResponse = lam_olre * exp(sd_olre^2 / 2),      # la media de la Poisson-lognormal
  integerResponse         = TRUE)
plot(res_olre)

# -----------------------------------------------------------------------------
# [u25-olre-dharma-tests]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.7 El efecto aleatorio a nivel de observación (OLRE)
#       > El OLRE en la cartera
# -----------------------------------------------------------------------------
# p-valores de los contrastes de DHARMa sobre el OLRE, con la simulación incondicional
tests_olre <- round(c(uniformidad = testUniformity(res_olre, plot = FALSE)$p.value,
                      dispersion  = testDispersion(res_olre, plot = FALSE)$p.value,
                      atipicos    = testOutliers(res_olre, plot = FALSE)$p.value,
                      ceros       = testZeroInflation(res_olre, plot = FALSE)$p.value), 3)
tests_olre

# -----------------------------------------------------------------------------
# [u25-olre-condicional]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.7 El efecto aleatorio a nivel de observación (OLRE)
#       > El OLRE en la cartera
# -----------------------------------------------------------------------------
v_hat <- ranef(m_olre)$cond$id_obs[, 1]                       # impulso predicho de cada póliza
round(tapply(v_hat, pmin(cartera$n_danos, 3), mean), 2)       # por partes observados (3 = «3 o más»)

residuo <- \(y, sim) {                 # posición de y entre sus réplicas, con los empates al azar
  bajo <- rowMeans(sim < y); hasta <- rowMeans(sim <= y)
  bajo + runif(length(y)) * (hasta - bajo)
}
resumen <- \(y, sim) {
  r <- residuo(y, sim); media <- rowMeans(sim)
  c(entre_025_075 = mean(r > 0.25 & r < 0.75), desv_tipica = sd(r),
    dispersion    = var(y - media) / mean(apply(sim, 2, \(s) var(s - media))))
}
sim_glmm_cond <- replicate(250, rpois(n_pol, fitted(m_glmm)))            # GLMM, con sus u predichos
sim_olre_cond <- replicate(250, rpois(n_pol, lam_olre * exp(v_hat)))     # OLRE, con sus v predichos
comp_sim <- rbind(
  glmm_condicional   = resumen(cartera$n_gestiones, sim_glmm_cond),
  olre_condicional   = resumen(cartera$n_danos, sim_olre_cond),
  olre_incondicional = resumen(cartera$n_danos, sim_olre))
round(comp_sim, 3)

# -----------------------------------------------------------------------------
# [u25-olre-comp]
#   5 · Efectos Aleatorios y Modelos Mixtos
#     > 5.7 El efecto aleatorio a nivel de observación (OLRE)
#       > El OLRE en la cartera
# -----------------------------------------------------------------------------
m_nb2 <- glm.nb(
  n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
    offset(log(exposicion)), data = cartera
)
comparacion_olre <- sapply(list(Poisson = m_pois_danos, NB2 = m_nb2, OLRE = m_olre),
  \(m) c(parametros = attr(logLik(m), "df"), AIC = AIC(m), BIC = BIC(m)))
round(comparacion_olre, 1)


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
