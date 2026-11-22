# =============================================================================
# Caso 3 · Unidad 3.3 — 3 · El coste agregado: la familia Tweedie
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_3.qmd.
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
  glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  marginaleffects = "0.32.0", patchwork = "1.3.2", performance = "0.17.0",
  purrr = "1.2.2", rsample = "1.3.2", sessioninfo = "1.2.4", tibble = "3.3.1",
  tidyr = "1.3.2", tidyverse = "2.0.0")
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
# Núcleo del Caso 3 (respuestas continuas positivas + mixtos).
library(MASS)            # se carga ANTES que tidyverse para que dplyr::select() no quede enmascarada
library(tidyverse)       # manipulación, visualización y descriptivos
library(broom)           # resultados ordenados de glm
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

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso3/R/dgp_averias.R"))   # funciones del proceso generador
banco <- leer_datos_glm("caso3/datos/banco_averias_20252026.rds")   # banco de averías del curso

glimpse(banco$averias)

# -----------------------------------------------------------------------------
# [u33-panel]
#   3 · El coste agregado: la familia Tweedie
#     > 3.1 El hueco en la recta de las varianzas
#       > Un dato que no encaja en ninguna familia
# -----------------------------------------------------------------------------
pn <- banco$panel
c(maquinas_anio = nrow(pn), maquinas = length(unique(pn$id_maquina)),
  pct_cero = round(mean(pn$coste_total == 0) * 100, 1),
  media = round(mean(pn$coste_total)),
  media_positivos = round(mean(pn$coste_total[pn$coste_total > 0])),
  maximo = round(max(pn$coste_total)))

# -----------------------------------------------------------------------------
# [u33-panel-head]
#   3 · El coste agregado: la familia Tweedie
#     > 3.1 El hueco en la recta de las varianzas
#       > Un dato que no encaja en ninguna familia
# -----------------------------------------------------------------------------
subset(pn, id_maquina == "M002",
       select = c(id_maquina, anio, coste_total, n_averias, exposicion))

# -----------------------------------------------------------------------------
# [fig-u33-panel]
#   3 · El coste agregado: la familia Tweedie
#     > 3.1 El hueco en la recta de las varianzas
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
# [u33-ceros]
#   3 · El coste agregado: la familia Tweedie
#     > 3.1 El hueco en la recta de las varianzas
#       > Un dato que no encaja en ninguna familia
# -----------------------------------------------------------------------------
lambda_anual <- sum(pn$n_averias) / sum(pn$exposicion)
round(c(lambda_anual = lambda_anual, p0_poisson = exp(-lambda_anual) * 100,
        p0_observado = mean(pn$coste_total == 0) * 100), 1)

# -----------------------------------------------------------------------------
# [u33-taylor]
#   3 · El coste agregado: la familia Tweedie
#     > 3.1 El hueco en la recta de las varianzas
#       > La pendiente del panel
# -----------------------------------------------------------------------------
# un punto por maquina: media y varianza de sus (hasta 4) costes anuales
mv_panel <- pn |>
  dplyr::group_by(id_maquina) |>
  dplyr::summarise(media = mean(coste_total), varianza = var(coste_total), .groups = "drop") |>
  dplyr::filter(media > 0, varianza > 0)   # sin averia en todo el seguimiento no hay
                                           # punto: media y varianza 0 no tienen logaritmo

# con logaritmo en los dos ejes, la pendiente de la recta estima la potencia p de Var = phi * mu^p
ajuste_p <- lm(log(varianza) ~ log(media), data = mv_panel)
round(c(maquinas = nrow(mv_panel), pendiente = coef(ajuste_p)[["log(media)"]],
        IC_inf = confint(ajuste_p)["log(media)", 1],
        IC_sup = confint(ajuste_p)["log(media)", 2]), 2)

# -----------------------------------------------------------------------------
# [fig-u33-taylor]
#   3 · El coste agregado: la familia Tweedie
#     > 3.1 El hueco en la recta de las varianzas
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
# [u33-edad]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
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
# [tbl-u33-eda]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
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
# [fig-u33-eda]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
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
# [u33-tweedie-fit]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > El ajuste: verosimilitud y estimación
# -----------------------------------------------------------------------------
m_tw <- glmmTMB(coste_total ~ edad + carga + proceso + criticidad + plan_mantenimiento +
                  fabricante + offset(log(exposicion)),
                family = tweedie, data = pn)
summary(m_tw)

# -----------------------------------------------------------------------------
# [u33-tweedie-p]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > El ajuste: verosimilitud y estimación
# -----------------------------------------------------------------------------
c(phi = sigma(m_tw),                  # dispersión: Var = phi * mu^p
  p   = family_params(m_tw)[[1]])     # índice de la familia (family_params no lo llama "p")

# -----------------------------------------------------------------------------
# [tbl-u33-tweedie]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > Interpretación
# -----------------------------------------------------------------------------
broom.mixed::tidy(m_tw, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::transmute(term, factor = round(estimate, 3),
                   IC95 = sprintf("(%.2f, %.2f)", conf.low, conf.high),
                   p_valor = round(p.value, 4))

# -----------------------------------------------------------------------------
# [u33-canales]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
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
# [u33-reparto]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > El precio del predictor único
# -----------------------------------------------------------------------------
b <- fixef(m_tw)$cond[c("edad", "criticidadCritica", "plan_mantenimientoPreventivo")]
round(cbind(total = b, a_frecuencia = (2 - p_hat) * b, a_severidad = (p_hat - 1) * b), 3)

# -----------------------------------------------------------------------------
# [tbl-u33-lrt]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > Inferencia
# -----------------------------------------------------------------------------
drop1(m_tw, test = "Chisq")

# -----------------------------------------------------------------------------
# [u33-ic-varianza]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > Inferencia
# -----------------------------------------------------------------------------
ic <- confint(m_tw, full = TRUE)          # más allá de los coeficientes: dispersión y familia
ic[!grepl("^cond\\.", rownames(ic)), , drop = FALSE]

# -----------------------------------------------------------------------------
# [tbl-u33-bondad]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
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
# [tbl-u33-calibracion]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
pn |>
  dplyr::mutate(ajustado = as.numeric(fitted(m_tw)),
                decil = dplyr::ntile(ajustado, 10)) |>
  dplyr::group_by(decil) |>
  dplyr::summarise(predicho = round(mean(ajustado)),
                   observado = round(mean(coste_total)), .groups = "drop")

# -----------------------------------------------------------------------------
# [fig-u33-calibracion]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
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
# [u33-ceros-ajuste]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > Bondad de ajuste
# -----------------------------------------------------------------------------
round(c(ceros_observados = mean(pn$coste_total == 0) * 100,
        ceros_esperados  = mean(exp(-lambda)) * 100), 1)

# -----------------------------------------------------------------------------
# [fig-u33-dharma]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > Diagnóstico
# -----------------------------------------------------------------------------
res_tw <- DHARMa::simulateResiduals(m_tw, n = 250, seed = SEMILLA_CURSO)
plot(res_tw)

# -----------------------------------------------------------------------------
# [u33-dharma-tests]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > Diagnóstico
# -----------------------------------------------------------------------------
DHARMa::testDispersion(res_tw, plot = FALSE)

# -----------------------------------------------------------------------------
# [u33-agrupamiento]
#   3 · El coste agregado: la familia Tweedie
#     > 3.2 La Tweedie en acción
#       > Diagnóstico
# -----------------------------------------------------------------------------
u <- residuals(res_tw)                       # residuos cuantil, uniformes si el modelo es correcto
obs <- var(tapply(u, pn$id_maquina, mean))   # ¿cuánto varían las medias por máquina?

set.seed(SEMILLA_CURSO)                      # referencia: lo mismo, rompiendo el agrupamiento
nulo <- replicate(200, var(tapply(runif(nrow(pn)), pn$id_maquina, mean)))

round(c(varianza_observada = obs, esperada_si_independientes = mean(nulo),
        p_valor = mean(nulo >= obs)), 4)

# -----------------------------------------------------------------------------
# [u33-reconstruir]
#   3 · El coste agregado: la familia Tweedie
#     > 3.3 Los tres caminos
#       > Los datos de cada camino
# -----------------------------------------------------------------------------
coste_rec <- banco$averias |>
  dplyr::mutate(anio = dia %/% 365 + 1) |>                       # año del estudio de cada avería
  dplyr::summarise(coste_rec = sum(coste_euros), .by = c(id_maquina, anio))

chk <- pn |>
  dplyr::left_join(coste_rec, by = c("id_maquina", "anio")) |>
  dplyr::mutate(coste_rec = tidyr::replace_na(coste_rec, 0))   # años sin avería: cero, no NA

c(filas = nrow(chk), coinciden = all(abs(chk$coste_total - chk$coste_rec) < 1e-6))

# -----------------------------------------------------------------------------
# [u33-caminos-fit]
#   3 · El coste agregado: la familia Tweedie
#     > 3.3 Los tres caminos
#       > El ajuste
# -----------------------------------------------------------------------------
f_x <- ~ edad + carga + proceso + criticidad + plan_mantenimiento + fabricante   # covariables comunes

# 1 · Frecuencia (binomial negativa, con exposición) × severidad (Gamma, una fila por avería)
m_frec <- MASS::glm.nb(update(f_x, n_averias ~ . + offset(log(exposicion))), data = pn)
m_sev  <- glm(update(f_x, coste_euros ~ .), family = Gamma(link = "log"),
              data = dplyr::mutate(banco$averias, edad = antiguedad_anios))

# 2 · Dos partes: ¿hubo avería? (enlace cloglog, con exposición) × coste del año si la hubo
m_hay  <- glm(update(f_x, I(coste_total > 0) ~ . + offset(log(exposicion))),
              family = binomial(link = "cloglog"), data = pn)
m_pos  <- glm(update(f_x, coste_total ~ . + log(exposicion)), family = Gamma(link = "log"),
              data = subset(pn, coste_total > 0))

# Coste anual esperado de cada máquina-año por los tres caminos
caminos <- pn |>
  dplyr::mutate(frec_x_sev = predict(m_frec, pn, type = "response") *
                             predict(m_sev,  pn, type = "response"),
                dos_partes = predict(m_hay,  pn, type = "response") *
                             predict(m_pos,  pn, type = "response"),
                tweedie    = predict(m_tw,   pn, type = "response"))

# -----------------------------------------------------------------------------
# [tbl-u33-caminos-total]
#   3 · El coste agregado: la familia Tweedie
#     > 3.3 Los tres caminos
#       > ¿Dan lo mismo?
# -----------------------------------------------------------------------------
seg <- caminos |>
  dplyr::group_by(criticidad, plan_mantenimiento) |>
  dplyr::summarise(observado  = sum(coste_total), frec_x_sev = sum(frec_x_sev),
                   dos_partes = sum(dos_partes),  tweedie = sum(tweedie), .groups = "drop") |>
  dplyr::mutate(dplyr::across(c(criticidad, plan_mantenimiento), as.character)) |>
  dplyr::bind_rows(dplyr::summarise(caminos, criticidad = "Total", plan_mantenimiento = "",
                                    observado = sum(coste_total), frec_x_sev = sum(frec_x_sev),
                                    dos_partes = sum(dos_partes), tweedie = sum(tweedie)))
seg |> dplyr::mutate(dplyr::across(observado:tweedie, round))

# -----------------------------------------------------------------------------
# [tbl-u33-caminos-reparto]
#   3 · El coste agregado: la familia Tweedie
#     > 3.3 Los tres caminos
#       > Lo que solo un camino permite leer
# -----------------------------------------------------------------------------
reparto <- tibble::tibble(
  termino    = names(fixef(m_tw)$cond),
  tweedie    = fixef(m_tw)$cond,
  frecuencia = coef(m_frec)[termino],
  severidad  = coef(m_sev)[termino]) |>
  dplyr::mutate(suma = frecuencia + severidad) |>
  dplyr::filter(termino != "(Intercept)")
reparto |> dplyr::mutate(dplyr::across(tweedie:suma, \(x) round(x, 3)))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
