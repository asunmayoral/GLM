# =============================================================================
# Caso 2 · Unidad 2.6 — 6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_6.qmd.
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
  forcats = "1.0.1", glmmTMB = "1.1.14", glmnet = "5.0",
  marginaleffects = "0.32.0", patchwork = "1.3.2", performance = "0.17.0",
  purrr = "1.2.2", sessioninfo = "1.2.4", tibble = "3.3.1", tidyr = "1.3.2",
  tidyverse = "2.0.0", vcd = "1.4.13", vcdExtra = "0.9.6")
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
library(glmnet)       # regularización (2.1)
library(vcd)           # mosaicos para tablas de contingencia (2.2)
library(vcdExtra)      # zero-inflated (2.4) y utilidades de tablas (2.2)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
source(url_glm("caso2/R/dgp_panel.R"))   # funciones del proceso generador
panel <- leer_datos_glm("caso2/datos/panel_partes_20252032.rds")   # panel póliza-año de partes (Unidad 2.6)
glimpse(cartera)
head(cartera)

# -----------------------------------------------------------------------------
# [u26-panel]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.1 El problema y los datos
#       > Del historial de cada cliente a un panel póliza-año
# -----------------------------------------------------------------------------
dplyr::glimpse(panel)

# -----------------------------------------------------------------------------
# [tbl-u26-panel]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.1 El problema y los datos
#       > Del historial de cada cliente a un panel póliza-año
# -----------------------------------------------------------------------------
# póliza elegida porque recorre años con 0, 1 y 2 partes y termina con un año incompleto
panel |>
  dplyr::filter(id_poliza == 94) |>
  dplyr::select(id_poliza, antiguedad, anio, expo_anual, n_partes_anual)

# -----------------------------------------------------------------------------
# [u26-ej]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.1 El problema y los datos
#       > Del historial de cada cliente a un panel póliza-año
# -----------------------------------------------------------------------------
ej <- dplyr::filter(panel, id_poliza == 94)

# -----------------------------------------------------------------------------
# [tbl-u26-npartes]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.1 El problema y los datos
#       > Por qué no basta con contar: exposición y tasa
# -----------------------------------------------------------------------------
dplyr::count(panel, n_partes_anual)

# -----------------------------------------------------------------------------
# [tbl-u26-tasa]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.3 Exploración: qué trozos y qué estructura
#       > La tasa año a año: ¿cuántos trozos?
# -----------------------------------------------------------------------------
tasa_anio <- panel |>
  dplyr::group_by(anio) |>
  dplyr::summarise(filas      = dplyr::n(),
                   exposicion = round(sum(expo_anual), 1),
                   partes     = sum(n_partes_anual),
                   tasa       = round(partes / exposicion, 3),
                   media_fila = round(mean(n_partes_anual), 3))
tasa_anio

# -----------------------------------------------------------------------------
# [fig-u26-tasa]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.3 Exploración: qué trozos y qué estructura
#       > La tasa año a año: ¿cuántos trozos?
# -----------------------------------------------------------------------------
tasa_anio |>
  dplyr::mutate(inf = tasa * exp(-1.96 / sqrt(partes)),
                sup = tasa * exp( 1.96 / sqrt(partes))) |>
  ggplot(aes(anio, tasa, colour = exposicion)) +
  geom_pointrange(aes(ymin = inf, ymax = sup), linewidth = 0.8) +
  scale_colour_gradient(low = "#9ecae1", high = "#08306b") +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "año de antigüedad", y = "partes por año-póliza", colour = "exposición\n(años-póliza)")

# -----------------------------------------------------------------------------
# [u26-trozos]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.3 Exploración: qué trozos y qué estructura
#       > La tasa año a año: ¿cuántos trozos?
# -----------------------------------------------------------------------------
panel <- panel |>
  dplyr::mutate(trozo = cut(anio, breaks = c(0:6, 10), labels = c(1:6, "7-10")))

tasa_trozo <- panel |>
  dplyr::group_by(trozo) |>
  dplyr::summarise(partes = sum(n_partes_anual),
                   tasa   = round(partes / sum(expo_anual), 3))
tasa_trozo

# -----------------------------------------------------------------------------
# [u26-trozos-alt]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.3 Exploración: qué trozos y qué estructura
#       > La tasa año a año: ¿cuántos trozos?
# -----------------------------------------------------------------------------
panel |>
  dplyr::mutate(trozo_alt = cut(anio, breaks = c(0, 1, 2, 3, 5, 10),
                                labels = c("1", "2", "3", "4-5", "6-10"))) |>
  dplyr::group_by(trozo_alt) |>
  dplyr::summarise(partes = sum(n_partes_anual),
                   tasa   = round(partes / sum(expo_anual), 3))

# -----------------------------------------------------------------------------
# [u26-tasa-xt]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.3 Exploración: qué trozos y qué estructura
#       > La tasa según las características del cliente
# -----------------------------------------------------------------------------
tercil <- function(x) factor(dplyr::ntile(x, 3), labels = c("bajo", "medio", "alto"))
clientes_t <- panel |>
  dplyr::distinct(id_poliza, edad_conductor, antiguedad_carnet, potencia_cv) |>
  dplyr::transmute(id_poliza, edad = tercil(edad_conductor), carnet = tercil(antiguedad_carnet),
                   potencia = tercil(potencia_cv))
panel_t <- dplyr::left_join(panel, clientes_t, by = "id_poliza")

predictores <- c(edad = "edad", carnet = "carnet", potencia = "potencia", zona = "zona_circulacion",
                 uso = "uso", vehiculo = "tipo_vehiculo", sexo = "sexo")
tasa_xt <- purrr::imap_dfr(predictores, \(v, nombre) panel_t |>
  dplyr::group_by(trozo, nivel = as.character(.data[[v]])) |>
  dplyr::summarise(partes = sum(n_partes_anual), tasa = partes / sum(expo_anual), .groups = "drop") |>
  dplyr::mutate(predictor = nombre)) |>
  # orden natural en la leyenda: bajo, medio, alto (los factores conservan el suyo)
  dplyr::mutate(nivel = forcats::fct_relevel(nivel, intersect(c("bajo", "medio", "alto"), unique(nivel))))

# -----------------------------------------------------------------------------
# [fig-u26-tasa-x]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.3 Exploración: qué trozos y qué estructura
#       > La tasa según las características del cliente
# -----------------------------------------------------------------------------
patchwork::wrap_plots(purrr::map(names(predictores), \(p)
  ggplot(dplyr::filter(tasa_xt, predictor == p), aes(trozo, tasa, colour = nivel, group = nivel)) +
    geom_line() +
    geom_point(size = 1.5) +
    scale_y_log10() +
    labs(title = p, x = "trozo", y = NULL, colour = NULL) +
    theme(legend.position = "bottom", plot.title = element_text(size = 10))),
  ncol = 4)

# -----------------------------------------------------------------------------
# [u26-pw]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.4 Ajuste, interpretación y predicción
#       > Ajuste
# -----------------------------------------------------------------------------
panel <- panel |>
  dplyr::mutate(edad_10 = edad_conductor / 10, carnet_10 = antiguedad_carnet / 10,
                potencia_10 = potencia_cv / 10)

m_pw <- glm(n_partes_anual ~ trozo + edad_10 + carnet_10 + potencia_10 + zona_circulacion + uso +
              tipo_vehiculo + sexo + offset(log(expo_anual)),
            family = poisson, data = panel)

# -----------------------------------------------------------------------------
# [tbl-u26-pw]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.4 Ajuste, interpretación y predicción
#       > Ajuste
# -----------------------------------------------------------------------------
tab_pw <- broom::tidy(m_pw, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::transmute(termino = term, razon = round(estimate, 3), inf = round(conf.low, 3),
                   sup = round(conf.high, 3), p = signif(p.value, 2))
tab_pw

# -----------------------------------------------------------------------------
# [u26-pw-aux]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.4 Ajuste, interpretación y predicción
#       > Ajuste
# -----------------------------------------------------------------------------
rt <- function(t, col = "razon") tab_pw[[col]][tab_pw$termino == t]   # cifra de la tabla, para el texto
pct <- function(t) round(100 * abs(1 - rt(t)))                          # cambio porcentual

# -----------------------------------------------------------------------------
# [tbl-u26-perfiles]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.4 Ajuste, interpretación y predicción
#       > Predicción
# -----------------------------------------------------------------------------
medianas <- panel |>                                  # medianas por cliente, no por fila
  dplyr::distinct(id_poliza, edad_conductor, antiguedad_carnet, potencia_cv) |>
  dplyr::summarise(dplyr::across(-id_poliza, median))

perfiles <- tibble::tibble(
  perfil      = c("joven_urbano", "mediano", "veterano_rural"),
  edad_10     = c(2.5, medianas$edad_conductor / 10, 6),
  carnet_10   = c(0.5, medianas$antiguedad_carnet / 10, 3.5),
  potencia_10 = c(16, medianas$potencia_cv / 10, 9),
  zona_circulacion = c("urbana", "urbana", "rural"), uso = "particular", tipo_vehiculo = "turismo",
  sexo = "H", expo_anual = 1)                       # un año completo: la predicción es la tasa

pred <- tidyr::crossing(perfiles, trozo = factor(levels(panel$trozo), levels = levels(panel$trozo)))
pred$tasa <- predict(m_pw, newdata = pred, type = "response")

pred_tabla <- pred |>
  dplyr::select(trozo, perfil, tasa) |>
  tidyr::pivot_wider(names_from = perfil, values_from = tasa) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))
pred_tabla

# -----------------------------------------------------------------------------
# [u26-perfiles-aux]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.4 Ajuste, interpretación y predicción
#       > Predicción
# -----------------------------------------------------------------------------
anchos <- c(rep(1, 6), 4)                             # años que abarca cada trozo
partes_esp <- pred |>
  dplyr::group_by(perfil) |>
  dplyr::summarise(cinco = sum(tasa[1:5]),           # los trozos 1 a 5 son de un año cada uno
                   diez  = sum(tasa * anchos))       # todo el seguimiento: tasa por años del trozo
pe <- function(p, col) round(partes_esp[[col]][partes_esp$perfil == p], 2)
tasa_p <- function(p, k) pred$tasa[pred$perfil == p & pred$trozo == k]

# -----------------------------------------------------------------------------
# [fig-u26-perfiles]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.4 Ajuste, interpretación y predicción
#       > Predicción
# -----------------------------------------------------------------------------
ic <- predict(m_pw, newdata = pred, type = "link", se.fit = TRUE)   # IC en escala log
limites <- c(0:6, 10)                                               # bordes de los trozos, en años

pred |>
  dplyr::mutate(inf = exp(ic$fit - 1.96 * ic$se.fit), sup = exp(ic$fit + 1.96 * ic$se.fit),
                desde = limites[as.integer(trozo)], hasta = limites[as.integer(trozo) + 1]) |>
  ggplot() +
  geom_rect(aes(xmin = desde, xmax = hasta, ymin = inf, ymax = sup, fill = perfil), alpha = 0.25) +
  geom_segment(aes(x = desde, xend = hasta, y = tasa, yend = tasa, colour = perfil), linewidth = 1) +
  scale_y_log10() +
  scale_x_continuous(breaks = limites) +
  scale_colour_discrete(labels = c("joven, urbano", "mediano", "veterano, rural"),
                        aesthetics = c("colour", "fill")) +
  labs(x = "antigüedad (años)", y = "partes por año (escala log)", colour = NULL, fill = NULL)

# -----------------------------------------------------------------------------
# [u26-mix]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > Un efecto aleatorio por póliza
# -----------------------------------------------------------------------------
m_pw_mix <- glmmTMB::glmmTMB(
  n_partes_anual ~ trozo + edad_10 + carnet_10 + potencia_10 + zona_circulacion + uso +
    tipo_vehiculo + sexo + offset(log(expo_anual)) + (1 | id_poliza),
  family = poisson, data = panel)

sigma_pol <- sqrt(glmmTMB::VarCorr(m_pw_mix)$cond$id_poliza[1])
lrt_pol   <- 2 * (as.numeric(logLik(m_pw_mix)) - as.numeric(logLik(m_pw)))   # mixto frente a GLM
round(c(sigma = sigma_pol, exp_sigma = exp(sigma_pol), razon_verosimilitudes = lrt_pol), 3)

# -----------------------------------------------------------------------------
# [tbl-u26-mix]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > Qué cambia y qué no
# -----------------------------------------------------------------------------
tab_mix <- tibble::tibble(
  termino  = names(coef(m_pw))[-1],
  razon    = round(exp(coef(m_pw))[-1], 3),
  ee       = round(sqrt(diag(vcov(m_pw)))[-1], 3),
  razon_mix = round(exp(glmmTMB::fixef(m_pw_mix)$cond)[-1], 3),
  ee_mix   = round(sqrt(diag(vcov(m_pw_mix)$cond))[-1], 3))
tab_mix

# -----------------------------------------------------------------------------
# [tbl-u26-seleccion]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > ¿Qué variables se quedan?
# -----------------------------------------------------------------------------
seleccion <- dplyr::full_join(
  tibble::as_tibble(drop1(m_pw, test = "Chisq"), rownames = "variable") |>
    dplyr::select(variable, gl = Df, p_glm = `Pr(>Chi)`),
  tibble::as_tibble(drop1(m_pw_mix, test = "Chisq"), rownames = "variable") |>
    dplyr::select(variable, chi2_mixto = LRT, p_mixto = `Pr(>Chi)`),
  by = "variable") |>
  dplyr::filter(variable != "<none>") |>
  dplyr::mutate(chi2_mixto = round(chi2_mixto, 2), dplyr::across(c(p_glm, p_mixto), \(x) signif(x, 2)))
seleccion

# -----------------------------------------------------------------------------
# [u26-seleccion-aux]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > ¿Qué variables se quedan?
# -----------------------------------------------------------------------------
pv <- function(v, col) seleccion[[col]][seleccion$variable == v]   # valor p de la tabla, para el texto

# -----------------------------------------------------------------------------
# [u26-final]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > ¿Qué variables se quedan?
# -----------------------------------------------------------------------------
m_pw_fin <- update(m_pw_mix, . ~ . - sexo)

# -----------------------------------------------------------------------------
# [u26-pred-mix]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > Predicción: qué cliente
# -----------------------------------------------------------------------------
s2_pol <- glmmTMB::VarCorr(m_pw_fin)$cond$id_poliza[1]          # varianza de la fragilidad
nd_mix <- dplyr::mutate(pred, id_poliza = NA)                     # perfiles y trozos de la §6.4
p_tip  <- predict(m_pw_fin, newdata = nd_mix, type = "link", se.fit = TRUE, re.form = NA)
pred_mix <- nd_mix |>
  dplyr::transmute(perfil, trozo,
                   efectos_fijos = tasa,                                   # modelo de la §6.4
                   tipico        = exp(p_tip$fit),
                   tipico_inf    = exp(p_tip$fit - 1.96 * p_tip$se.fit),
                   tipico_sup    = exp(p_tip$fit + 1.96 * p_tip$se.fit),
                   nuevo_media   = exp(p_tip$fit + s2_pol / 2),
                   nuevo_inf     = exp(p_tip$fit - 1.96 * sqrt(p_tip$se.fit^2 + s2_pol)),
                   nuevo_sup     = exp(p_tip$fit + 1.96 * sqrt(p_tip$se.fit^2 + s2_pol)))

# -----------------------------------------------------------------------------
# [fig-u26-pred-mix]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > Predicción: qué cliente
# -----------------------------------------------------------------------------
pred_mix |>
  dplyr::mutate(desde = limites[as.integer(trozo)], hasta = limites[as.integer(trozo) + 1],
                perfil = factor(perfil, c("joven_urbano", "mediano", "veterano_rural"),
                                c("joven, urbano", "mediano", "veterano, rural"))) |>
  ggplot() +
  geom_rect(aes(xmin = desde, xmax = hasta, ymin = nuevo_inf, ymax = nuevo_sup, fill = perfil), alpha = 0.12) +
  geom_rect(aes(xmin = desde, xmax = hasta, ymin = tipico_inf, ymax = tipico_sup, fill = perfil), alpha = 0.35) +
  geom_segment(aes(x = desde, xend = hasta, y = tipico, yend = tipico, colour = perfil), linewidth = 1) +
  geom_segment(aes(x = desde, xend = hasta, y = nuevo_media, yend = nuevo_media, colour = perfil),
               linetype = 3, linewidth = 1) +
  geom_segment(aes(x = desde, xend = hasta, y = efectos_fijos, yend = efectos_fijos), linetype = 2) +
  facet_wrap(~ perfil) +
  scale_y_log10() +
  scale_x_continuous(breaks = limites) +
  labs(x = "antigüedad (años)", y = "partes por año (escala log)") +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# [u26-pred-mix-aux]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > Predicción: qué cliente
# -----------------------------------------------------------------------------
pm <- function(per, k, col) round(pred_mix[[col]][pred_mix$perfil == per & pred_mix$trozo == k], 3)

# -----------------------------------------------------------------------------
# [tbl-u26-pred-94]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.5 Los años de un mismo cliente no son independientes
#       > Predicción: qué cliente
# -----------------------------------------------------------------------------
nd_94 <- panel |>
  dplyr::filter(id_poliza == 94) |>
  dplyr::distinct(id_poliza, .keep_all = TRUE) |>
  dplyr::select(-trozo, -expo_anual) |>
  tidyr::crossing(trozo = factor(levels(panel$trozo), levels = levels(panel$trozo))) |>
  dplyr::mutate(expo_anual = 1)
p_obs_94 <- predict(m_pw_fin, newdata = nd_94, type = "link", se.fit = TRUE)               # con su u_i
p_tip_94 <- predict(m_pw_fin, newdata = nd_94, type = "link", se.fit = TRUE, re.form = NA)  # u = 0
pred_94 <- tibble::tibble(trozo     = nd_94$trozo,
                          observado = exp(p_obs_94$fit),
                          inf       = exp(p_obs_94$fit - 1.96 * p_obs_94$se.fit),
                          sup       = exp(p_obs_94$fit + 1.96 * p_obs_94$se.fit),
                          tipico    = exp(p_tip_94$fit)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))
u_94 <- glmmTMB::ranef(m_pw_fin)$cond$id_poliza["94", 1]          # su fragilidad estimada (escala log)
pred_94

# -----------------------------------------------------------------------------
# [tbl-u26-prop]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.6 Diagnóstico
#       > ¿Actúan los predictores por igual en todos los trozos?
# -----------------------------------------------------------------------------
predictores_prop <- c("edad_10", "carnet_10", "potencia_10", "zona_circulacion", "uso", "tipo_vehiculo")
prop <- purrr::map_dfr(predictores_prop, \(v) {
  m_int <- update(m_pw_fin, as.formula(paste(". ~ . + trozo:", v)))
  a <- anova(m_pw_fin, m_int)
  tibble::tibble(predictor = v, gl = a$`Chi Df`[2], chi2 = round(a$Chisq[2], 2),
                 p = round(a$`Pr(>Chisq)`[2], 3))
})
prop

# -----------------------------------------------------------------------------
# [u26-na]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.6 Diagnóstico
#       > ¿Reproduce el modelo la evolución de los partes?
# -----------------------------------------------------------------------------
partes   <- attr(panel, "partes")
clientes <- dplyr::distinct(panel, id_poliza, antiguedad)   # una fila por cliente
na <- partes |>
  dplyr::count(t, name = "d") |>                                        # partes en cada instante
  dplyr::mutate(
    en_obs = vapply(t, \(s) sum(clientes$antiguedad >= s), numeric(1)),  # clientes que seguían en la cartera
    nelson = cumsum(d / en_obs))
na_en <- function(s) max(c(0, na$nelson[na$t <= s]))   # tasa acumulada empírica en la antigüedad s

# -----------------------------------------------------------------------------
# [u26-acum-modelo]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.6 Diagnóstico
#       > ¿Reproduce el modelo la evolución de los partes?
# -----------------------------------------------------------------------------
nd <- tidyr::crossing(
  dplyr::distinct(panel, id_poliza, .keep_all = TRUE) |> dplyr::select(-trozo, -expo_anual),
  trozo = factor(levels(panel$trozo), levels = levels(panel$trozo)))
nd$expo_anual <- 1
nd$tasa <- predict(m_pw, newdata = nd, type = "response")
tasa_cartera <- dplyr::group_by(nd, trozo) |> dplyr::summarise(tasa = mean(tasa))

# tasa acumulada del modelo en la antigüedad s: tasa de cada trozo por el tiempo pasado en él hasta s
acum_modelo <- function(s) sum(tasa_cartera$tasa * pmax(0, pmin(s, limites[-1]) - limites[-length(limites)]))

# -----------------------------------------------------------------------------
# [fig-u26-calibracion]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.6 Diagnóstico
#       > ¿Reproduce el modelo la evolución de los partes?
# -----------------------------------------------------------------------------
rejilla <- tibble::tibble(t = seq(0, 10, by = 0.05))
rejilla$modelo <- vapply(rejilla$t, acum_modelo, numeric(1))

ggplot() +
  geom_step(data = na, aes(t, nelson, colour = "Nelson–Aalen"), linewidth = 0.7) +
  geom_line(data = rejilla, aes(t, modelo, colour = "modelo de riesgos a trozos"),
            linetype = 2, linewidth = 0.8) +
  scale_x_continuous(breaks = limites) +
  scale_colour_manual(values = c("Nelson–Aalen" = "steelblue", "modelo de riesgos a trozos" = "black")) +
  labs(x = "antigüedad (años)", y = "partes acumulados por cliente", colour = NULL)

# -----------------------------------------------------------------------------
# [tbl-u26-dharma]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.6 Diagnóstico
#       > ¿Se ajustan los partes a una Poisson?
# -----------------------------------------------------------------------------
set.seed(SEMILLA_CURSO)                                    # simulaciones reproducibles
sim_fin <- as.matrix(simulate(m_pw_fin, nsim = 250))       # fragilidad nueva en cada simulación
res_fin <- DHARMa::createDHARMa(simulatedResponse = sim_fin,
                                observedResponse  = panel$n_partes_anual,
                                fittedPredictedResponse = rowMeans(sim_fin),
                                integerResponse   = TRUE)
res_fin_pol <- DHARMa::recalculateResiduals(res_fin, group = panel$id_poliza)

contrastes <- function(r) {
  disp <- DHARMa::testDispersion(r, plot = FALSE)
  c(uniformidad  = DHARMa::testUniformity(r, plot = FALSE)$p.value,
    dispersion   = unname(disp$statistic),
    p_dispersion = disp$p.value,
    p_ceros      = DHARMa::testZeroInflation(r, plot = FALSE)$p.value)
}
dharma <- tibble::tibble(nivel = c("por fila", "por póliza"),
                         dplyr::bind_rows(contrastes(res_fin), contrastes(res_fin_pol))) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))
dharma

# -----------------------------------------------------------------------------
# [u26-verdad]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.7 Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(panel, "verdad")
b_fin  <- glmmTMB::fixef(m_pw_fin)$cond
ee_fin <- sqrt(diag(vcov(m_pw_fin)$cond))

# -----------------------------------------------------------------------------
# [tbl-u26-dgp-trozos]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.7 Validación contra el DGP
#       > El riesgo base
# -----------------------------------------------------------------------------
expo_anio <- tapply(panel$expo_anual, panel$anio, sum)
h0_trozo  <- c(verdad$h0[1:6], sum(verdad$h0[7:10] * expo_anio[7:10]) / sum(expo_anio[7:10]))
tz <- grep("^trozo", names(b_fin))
val_trozo <- tibble::tibble(
  trozo    = levels(panel$trozo)[-1],
  verdad   = h0_trozo[-1] / h0_trozo[1],
  estimado = exp(b_fin[tz]),
  inf      = exp(b_fin[tz] - 1.96 * ee_fin[tz]),
  sup      = exp(b_fin[tz] + 1.96 * ee_fin[tz])) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))
val_trozo

# -----------------------------------------------------------------------------
# [tbl-u26-dgp-beta]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.7 Validación contra el DGP
#       > Los efectos de las características del cliente
# -----------------------------------------------------------------------------
sd_cli <- c(edad_10     = sd(cartera$edad_conductor),       # la cartera tiene una fila por cliente
            carnet_10   = sd(cartera$antiguedad_carnet),
            potencia_10 = sd(cartera$potencia_cv))
b_dgp <- verdad$beta
verdad_beta <- c(
  edad_10                = b_dgp[["z_edad"]]   * 10 / sd_cli[["edad_10"]],
  carnet_10              = b_dgp[["z_carnet"]] * 10 / sd_cli[["carnet_10"]],
  potencia_10            = b_dgp[["z_pot"]]    * 10 / sd_cli[["potencia_10"]],
  zona_circulacionmixta  = b_dgp[["mixta"]] - b_dgp[["urbana"]],   # el generador mide frente a rural
  zona_circulacionrural  = -b_dgp[["urbana"]],
  usocomercial           = b_dgp[["comercial"]],
  tipo_vehiculomoto      = b_dgp[["moto"]],
  tipo_vehiculofurgoneta = b_dgp[["furgoneta"]])
val_beta <- tibble::tibble(
  termino   = names(verdad_beta),
  verdad    = verdad_beta,
  estimado  = b_fin[names(verdad_beta)],
  ee        = ee_fin[names(verdad_beta)],
  distancia = (estimado - verdad) / ee) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))
val_beta

# -----------------------------------------------------------------------------
# [u26-dgp-sigma]
#   6 · Riesgos a trozos: la siniestralidad a lo largo de la antigüedad
#     > 6.7 Validación contra el DGP
#       > La fragilidad
# -----------------------------------------------------------------------------
ic_sigma <- confint(m_pw_fin, parm = "theta_")      # intervalo de la desviación típica de la fragilidad
round(c(verdad = verdad$sigma_frag, ic_sigma[1, c("Estimate", "2.5 %", "97.5 %")]), 3)


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
