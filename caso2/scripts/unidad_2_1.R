# =============================================================================
# Caso 2 · Unidad 2.1 — 1 · Regresión de Poisson: conteos y tasas
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_1.qmd.
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
  lme4 = "2.0.1", marginaleffects = "0.32.0", performance = "0.17.0",
  pscl = "1.5.9", purrr = "1.2.2", sessioninfo = "1.2.4", survival = "3.8.6",
  tibble = "3.3.1", tidyr = "1.3.2", tidyverse = "2.0.0", vcd = "1.4.13",
  vcdExtra = "0.9.6")
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
library(pscl)          # hurdle / zeroinfl
library(glmmTMB)       # conteos mixtos / ceros
library(lme4)          # glmer (Poisson)
library(DHARMa); library(performance); library(marginaleffects)
library(survival)      # riesgos a trozos (base-recommended)
library(glmnet)       # regularización (2.7)
library(vcd)           # mosaicos para tablas de contingencia (2.2)
library(vcdExtra)      # zero-inflated (2.4) y utilidades de tablas (2.2)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
glimpse(cartera)
head(cartera)

# -----------------------------------------------------------------------------
# [fig-u21-exposicion]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.1 Contexto: qué problema resuelve este modelo
# -----------------------------------------------------------------------------
cartera |>
  mutate(tramo = cut(exposicion, c(0, 0.5, 0.75, 1),
                     labels = c("< 0.5", "0.5–0.75", "> 0.75"), include.lowest = TRUE)) |>
  group_by(tramo) |>
  summarise(`media por póliza` = mean(n_asistencia),
            `tasa (por unidad de exposición)` = sum(n_asistencia) / sum(exposicion),
            .groups = "drop") |>
  tidyr::pivot_longer(-tramo, names_to = "medida", values_to = "valor") |>
  ggplot(aes(tramo, valor)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ medida) +
  labs(x = "exposición (fracción de la antigüedad máxima)", y = "asistencias")

# -----------------------------------------------------------------------------
# [fig-u21-eda-modelo]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.2 Modelización y estimación
#       > Aplicado a la cartera
# -----------------------------------------------------------------------------
# sum(y)/sum(e) es el EMV de la tasa Poisson del grupo; como Var(sum y) = sum mu,
# su error estándar aproximado es sqrt(sum y)/sum(e)
tasa_por_nivel <- function(var, nivel) {
  tibble::tibble(nivel = nivel, y = cartera$n_asistencia, e = cartera$exposicion) |>
    dplyr::group_by(nivel) |>
    dplyr::summarise(tasa = sum(y) / sum(e), ee = sqrt(sum(y)) / sum(e), .groups = "drop") |>
    dplyr::mutate(predictor = var, nivel = as.character(nivel))
}
continuos   <- c("edad_conductor", "antiguedad_carnet", "potencia_cv", "antiguedad_vehiculo")
categoricos <- c("zona_circulacion", "uso", "tipo_vehiculo")
tasas_eda <- dplyr::bind_rows(
  # cut_number: cuartiles etiquetados con su rango, más legibles que "1, 2, 3, 4"
  purrr::map(continuos,   \(v) tasa_por_nivel(v, ggplot2::cut_number(cartera[[v]], 4))),
  purrr::map(categoricos, \(v) tasa_por_nivel(v, cartera[[v]]))) |>
  dplyr::mutate(nivel     = forcats::fct_inorder(nivel),
                predictor = factor(predictor, levels = c(continuos, categoricos)))
tasa_global <- sum(cartera$n_asistencia) / sum(cartera$exposicion)

ggplot(tasas_eda, aes(nivel, tasa)) +
  geom_hline(yintercept = tasa_global, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = tasa - 1.96 * ee, ymax = tasa + 1.96 * ee), colour = "steelblue") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) +
  labs(x = NULL, y = "tasa de asistencias (por unidad de exposición)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# -----------------------------------------------------------------------------
# [u21-cor-edad-carnet]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.2 Modelización y estimación
#       > Aplicado a la cartera
# -----------------------------------------------------------------------------
cor(cartera$edad_conductor, cartera$antiguedad_carnet)

# -----------------------------------------------------------------------------
# [u21-fit-tasa]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.2 Modelización y estimación
#       > Aplicado a la cartera
# -----------------------------------------------------------------------------
m_tasa <- glm(n_asistencia ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                offset(log(exposicion)),
              family = poisson, data = cartera)
broom::tidy(m_tasa)

# -----------------------------------------------------------------------------
# [u21-irr]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.3 Interpretación y predicción
#       > Razones de tasas (IRR)
# -----------------------------------------------------------------------------
irr <- broom::tidy(m_tasa, exponentiate = TRUE, conf.int = TRUE)
irr
irr_moto <- irr[irr$term == "tipo_vehiculomoto", ]   # el ejemplo que se interpreta abajo

# -----------------------------------------------------------------------------
# [u21-pred]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.3 Interpretación y predicción
#       > Razones de tasas (IRR)
# -----------------------------------------------------------------------------
perfil <- data.frame(edad_conductor = 45, potencia_cv = 110, zona_circulacion = "urbana",
                     uso = "particular", tipo_vehiculo = "turismo", exposicion = 1)
predict(m_tasa, perfil, type = "response")   # nº esperado de asistencias en la ventana de exposición

# -----------------------------------------------------------------------------
# [u21-pred-ic]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.3 Interpretación y predicción
#       > Razones de tasas (IRR)
# -----------------------------------------------------------------------------
p  <- predict(m_tasa, perfil, type = "link", se.fit = TRUE)        # eta y su error estándar
ic <- exp(p$fit + c(estimacion = 0, inf = -1.96, sup = 1.96) * p$se.fit)
ic                                                                 # IC 95 % de la tasa esperada
marginaleffects::predictions(m_tasa, newdata = perfil)             # la misma cuenta, en una línea

# -----------------------------------------------------------------------------
# [u21-abs]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.3 Interpretación y predicción
#       > Razones de tasas (IRR)
# -----------------------------------------------------------------------------
# mismo cambio (turismo -> moto) sobre dos perfiles con tasas de partida muy distintas
base <- data.frame(perfil = c("bajo riesgo", "alto riesgo"),
                   edad_conductor = c(65, 25), potencia_cv = c(80, 160),
                   zona_circulacion = c("rural", "urbana"), uso = c("particular", "comercial"),
                   exposicion = 1)
turismo <- predict(m_tasa, transform(base, tipo_vehiculo = "turismo"), type = "response")
moto    <- predict(m_tasa, transform(base, tipo_vehiculo = "moto"),    type = "response")
data.frame(perfil = base$perfil, turismo, moto,
           cociente   = moto / turismo,    # relativo: la IRR de la moto, igual en los dos perfiles
           diferencia = moto - turismo)    # absoluto: crece con la tasa de partida

# -----------------------------------------------------------------------------
# [u21-drop1]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.4 Inferencia y selección
#       > Contraste de los efectos
# -----------------------------------------------------------------------------
d1 <- drop1(m_tasa, test = "LRT")   # aporte de cada predictor por razón de verosimilitudes
d1

# -----------------------------------------------------------------------------
# [u21-seleccion]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.4 Inferencia y selección
#       > Selección de modelos
# -----------------------------------------------------------------------------
m_red <- update(m_tasa, . ~ . - tipo_vehiculo)
AIC(m_tasa, m_red)
lrt_tipo <- anova(m_red, m_tasa, test = "LRT")
lrt_tipo

# -----------------------------------------------------------------------------
# [u21-gof]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > Deviance y $\chi^2$ de Pearson
# -----------------------------------------------------------------------------
gl <- df.residual(m_tasa)
D  <- deviance(m_tasa)
X2 <- sum(residuals(m_tasa, type = "pearson")^2)
c(D = D, X2 = X2, gl = gl, D_gl = D / gl, X2_gl = X2 / gl,
  mediana_mu = median(fitted(m_tasa)))
c(pvalor_D = pchisq(D,gl,lower.tail = FALSE), pvalor_X2 = pchisq(X2,gl,lower.tail = FALSE))

# -----------------------------------------------------------------------------
# [u21-dispersion]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > El índice de dispersión
# -----------------------------------------------------------------------------
disp <- function(m) sum(residuals(m, type = "pearson")^2) / df.residual(m)
disp(m_tasa)

# -----------------------------------------------------------------------------
# [u21-sobredispersion]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > El índice de dispersión
# -----------------------------------------------------------------------------
m_danos <- glm(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                 offset(log(exposicion)), family = poisson, data = cartera)
c(asistencia = disp(m_tasa), danos = disp(m_danos))

# -----------------------------------------------------------------------------
# [u21-rootograma-tabla]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > El rootograma
# -----------------------------------------------------------------------------
mu_hat <- fitted(m_tasa)
K <- 0:max(cartera$n_asistencia)
raiz <- tibble::tibble(
  k         = K,
  observado = as.numeric(table(factor(cartera$n_asistencia, levels = K))),
  esperado  = sapply(K, \(k) sum(dpois(k, mu_hat))),
  raiz_observado = sqrt(observado),
  raiz_esperado = sqrt(esperado))
raiz

# -----------------------------------------------------------------------------
# [fig-u21-rootograma]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > El rootograma
# -----------------------------------------------------------------------------
ggplot(raiz) +
  geom_rect(aes(xmin = k - 0.4, xmax = k + 0.4,
                ymin = sqrt(esperado) - sqrt(observado), ymax = sqrt(esperado)),
            fill = "steelblue", alpha = 0.7) +
  geom_line(aes(k, sqrt(esperado)), colour = "darkorange", linewidth = 0.8) +
  geom_point(aes(k, sqrt(esperado)), colour = "darkorange") +
  geom_hline(yintercept = 0) +
  labs(x = "nº de asistencias", y = "raíz de la frecuencia")

# -----------------------------------------------------------------------------
# [fig-u21-residuos-crudos]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > Residuos de Pearson y de deviance
# -----------------------------------------------------------------------------
res <- tibble::tibble(ajustado = fitted(m_tasa),
                      r        = residuals(m_tasa, type = "deviance"),
                      y        = factor(cartera$n_asistencia))
paneles <- c("Residuos frente a ajustados", "QQ normal")
dplyr::bind_rows(
  res |> dplyr::transmute(panel = paneles[1], x = ajustado, r, y),
  res |> dplyr::arrange(r) |>
    dplyr::transmute(panel = paneles[2], x = qnorm(ppoints(dplyr::n())), r, y)) |>
  dplyr::mutate(panel = factor(panel, levels = paneles)) |>
  ggplot(aes(x, r, colour = y)) +
  geom_point(size = 0.6, alpha = 0.5) +
  geom_abline(data = data.frame(panel = factor(paneles[2], levels = paneles)),
              aes(intercept = 0, slope = 1), inherit.aes = FALSE, linetype = "dashed") +
  facet_wrap(~ panel, scales = "free_x") +
  labs(x = NULL, y = "residuo de deviance", colour = "asistencias")

# -----------------------------------------------------------------------------
# [u21-residuos-ceros]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > Residuos de Pearson y de deviance
# -----------------------------------------------------------------------------
c(prop_ceros = mean(cartera$n_asistencia == 0),
  prop_residuos_negativos = mean(res$r < 0))

# -----------------------------------------------------------------------------
# [fig-u21-dharma]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > Residuos simulados (DHARMa)
# -----------------------------------------------------------------------------
set.seed(2025)   # la simulación es aleatoria: fijamos la semilla para que el diagnóstico se reproduzca
res_sim <- DHARMa::simulateResiduals(m_tasa, n = 1000)
plot(res_sim)

# -----------------------------------------------------------------------------
# [u21-dharma-tests]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > Residuos simulados (DHARMa)
# -----------------------------------------------------------------------------
contrastes_dharma <- list(
  uniformidad     = DHARMa::testUniformity(res_sim, plot = FALSE),
  dispersion      = DHARMa::testDispersion(res_sim, plot = FALSE),
  exceso_de_ceros = DHARMa::testZeroInflation(res_sim, plot = FALSE))
tibble::tibble(contraste = names(contrastes_dharma),
               p_valor   = sapply(contrastes_dharma, \(t) t$p.value))

# -----------------------------------------------------------------------------
# [u21-validacion]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.5 Bondad de ajuste y diagnóstico
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(cartera, "verdad")
b_dgp  <- setNames(verdad$betas$limpio, verdad$nombres_beta)
terminos <- c("edad_conductor", "potencia_cv", "zona_circulacionmixta", "zona_circulacionrural",
              "usocomercial", "tipo_vehiculomoto", "tipo_vehiculofurgoneta")
# continuas: del efecto por unidad al efecto por desviación típica; factores: sin cambio
escala <- c(sd(cartera$edad_conductor), sd(cartera$potencia_cv), rep(1, 5))
est <- coef(m_tasa)[terminos] * escala
ee  <- sqrt(diag(vcov(m_tasa)))[terminos] * escala
# en la zona el DGP mide frente a 'rural' y R frente a 'urbana': hay que restar
dgp <- c(b_dgp[["z_edad"]], b_dgp[["z_pot"]], b_dgp[["mixta"]] - b_dgp[["urbana"]],
         -b_dgp[["urbana"]], b_dgp[["comercial"]], b_dgp[["moto"]], b_dgp[["furgoneta"]])
validacion <- tibble::tibble(termino = terminos, estimado = est, verdadero = dgp,
                             distancia_en_EE = (est - dgp) / ee) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))
validacion


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
