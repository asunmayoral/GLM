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
  lme4 = "2.0.1", marginaleffects = "0.32.0", patchwork = "1.3.2",
  performance = "0.17.0", pscl = "1.5.9", purrr = "1.2.2",
  sessioninfo = "1.2.4", survival = "3.8.6", tibble = "3.3.1",
  tidyr = "1.3.2", tidyverse = "2.0.0", vcd = "1.4.13", vcdExtra = "0.9.6")
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
library(glmnet)       # regularización (2.1)
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
# [u21-log-lm]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.3 Interpretación y predicción
#       > ¿Y si transformamos la respuesta con el logaritmo?
# -----------------------------------------------------------------------------
c(prop_ceros = round(mean(cartera$n_asistencia == 0), 3))
# La respuesta transformada se construye fuera de la fórmula: log((y + cte) / t)
ajusta_log <- function(cte) {
  d <- dplyr::mutate(cartera, y_log = log((n_asistencia + cte) / exposicion))
  lm(y_log ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo, data = d)
}
m_log1  <- ajusta_log(1)
m_log05 <- ajusta_log(0.5)
tibble::tibble(termino     = names(coef(m_tasa))[-1],
               IRR_poisson = exp(coef(m_tasa))[-1],
               exp_b_c1    = exp(coef(m_log1))[-1],
               exp_b_c05   = exp(coef(m_log05))[-1]) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

# -----------------------------------------------------------------------------
# [u21-log-media]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.3 Interpretación y predicción
#       > ¿Y si transformamos la respuesta con el logaritmo?
# -----------------------------------------------------------------------------
round(c(observada = mean(cartera$n_asistencia),
        poisson   = mean(fitted(m_tasa)),
        log_c1    = mean(cartera$exposicion * exp(fitted(m_log1)) - 1),
        log_c05   = mean(cartera$exposicion * exp(fitted(m_log05)) - 0.5)), 3)

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

# -----------------------------------------------------------------------------
# [u21-reg-candidatos]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ajustar bien no es predecir bien. Validación cruzada
# -----------------------------------------------------------------------------
# Los candidatos, tal como los agrupa la tabla de variables del caso. El papel de cada uno en el
# DGP no se usa hasta la validación.
riesgo      <- c("edad_conductor", "antiguedad_carnet", "potencia_cv", "antiguedad_vehiculo",
                 "zona_circulacion", "uso", "tipo_vehiculo")
adicionales <- c("sexo", "estado_civil", "color_vehiculo", "tiene_garaje", "forma_pago",
                 "financiado", "km_declarados", "valor_vehiculo", "n_conductores",
                 "antiguedad_cliente")
candidatos  <- c(riesgo, adicionales)

formulas <- list(
  nulo   = n_asistencia ~ offset(log(exposicion)),
  m_tasa = formula(m_tasa),
  riesgo = reformulate(c(riesgo, "offset(log(exposicion))"), response = "n_asistencia"),
  todos  = reformulate(c(candidatos, "offset(log(exposicion))"), response = "n_asistencia"))
ajustes <- lapply(formulas, \(f) glm(f, family = poisson, data = cartera))

comparacion <- tibble::tibble(
  modelo                 = names(ajustes),
  parametros             = sapply(ajustes, \(m) length(coef(m))),
  deviance               = sapply(ajustes, deviance) / nrow(cartera),   # de entrenamiento, por póliza
  AIC                    = sapply(ajustes, AIC),
  BIC                    = sapply(ajustes, BIC))
comparacion

# -----------------------------------------------------------------------------
# [u21-cv-contribuciones]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ajustar bien no es predecir bien. Validación cruzada
# -----------------------------------------------------------------------------
# d_i de cada póliza, rellenado bloque a bloque con el modelo ajustado SIN ese bloque.
# poisson()$dev.resids() es la fórmula que R suma en deviance(fit), aquí evaluada en pólizas
# que el modelo no vio.
cv_contribuciones <- function(formula, bloque) {
  d <- rep(NA_real_, nrow(cartera))
  for (b in sort(unique(bloque))) {
    test <- bloque == b
    fit  <- glm(formula, family = poisson, data = cartera[!test, ])
    mu   <- predict(fit, newdata = cartera[test, ], type = "response")  # lleva el offset
    d[test] <- poisson()$dev.resids(cartera$n_asistencia[test], mu, wt = 1)
  }
  d
}

set.seed(SEMILLA_CURSO)
bloque5 <- sample(rep(1:5, length.out = nrow(cartera)))   # asignación aleatoria a 5 bloques
d5 <- cv_contribuciones(formula(m_tasa), bloque5)

# comprobación: cada póliza está en validación exactamente una vez y recibe su contribución
veces <- tabulate(unlist(lapply(1:5, \(b) which(bloque5 == b))), nbins = nrow(cartera))
table(veces_en_validacion = veces)
c(polizas = nrow(cartera), con_contribucion = sum(!is.na(d5)), deviance_cv = mean(d5))

# -----------------------------------------------------------------------------
# [fig-u21-cv-esquema]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ajustar bien no es predecir bien. Validación cruzada
# -----------------------------------------------------------------------------
# 25 pólizas para el dibujo: las cinco primeras de cada bloque, ordenadas por bloque
muestra <- unlist(lapply(1:5, \(b) head(which(bloque5 == b), 5)))
etiq    <- factor(cartera$id_poliza[muestra], levels = cartera$id_poliza[muestra])

papel <- expand.grid(poliza = etiq, iteracion = 1:5) |>
  dplyr::mutate(papel = ifelse(bloque5[muestra][as.integer(poliza)] == iteracion,
                               "validación: se predice", "entrenamiento: se ajusta"))
p_papel <- ggplot(papel, aes(poliza, factor(iteracion, levels = 5:1), fill = papel)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  scale_fill_manual(values = c("grey80", "darkorange")) +
  labs(x = NULL, y = "iteración", fill = NULL) +
  theme(axis.text.x = element_blank(), legend.position = "top")

contrib <- tibble::tibble(poliza = etiq, y = cartera$n_asistencia[muestra], d = d5[muestra],
                          iteracion = factor(bloque5[muestra]))
p_contrib <- ggplot(contrib, aes(poliza, d, fill = iteracion)) +
  geom_col() +
  geom_text(aes(label = y), vjust = -0.3, size = 3) +
  labs(x = "póliza (id_poliza)", y = expression(d[i]), fill = "iteración en que se predice") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom")

patchwork::wrap_plots(p_papel, p_contrib, ncol = 1, heights = c(1, 1.3))

# -----------------------------------------------------------------------------
# [u21-cv]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ajustar bien no es predecir bien. Validación cruzada
# -----------------------------------------------------------------------------
# Una única partición en 10 bloques para toda la sección: todos los modelos (también cv.glmnet,
# más abajo) se evalúan sobre los mismos repartos, así que sus errores son comparables.
set.seed(SEMILLA_CURSO)
bloque <- sample(rep(1:10, length.out = nrow(cartera)))
cv_deviance <- function(formula) mean(cv_contribuciones(formula, bloque))
comparacion <- dplyr::mutate(comparacion, deviance_cv = sapply(formulas, cv_deviance))
comparacion

# -----------------------------------------------------------------------------
# [u21-glmnet-matriz]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ridge, lasso y elastic-net
# -----------------------------------------------------------------------------
x  <- model.matrix(reformulate(candidatos), data = cartera)[, -1]   # sin intercepto: glmnet lo trata aparte
y  <- cartera$n_asistencia
os <- log(cartera$exposicion)
dim(x)

# -----------------------------------------------------------------------------
# [fig-u21-ruta-lasso]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ridge, lasso y elastic-net
# -----------------------------------------------------------------------------
fit_lasso <- glmnet(x, y, family = "poisson", offset = os, alpha = 1)
plot(fit_lasso, xvar = "lambda", label = TRUE)

# -----------------------------------------------------------------------------
# [fig-u21-cvglmnet]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ridge, lasso y elastic-net
# -----------------------------------------------------------------------------
cv_lasso <- cv.glmnet(x, y, family = "poisson", offset = os, alpha = 1,
                      type.measure = "deviance", foldid = bloque)
plot(cv_lasso)
# la regla 1se se lee contra estas tres alturas: el mínimo y su banda de ± 1 EE
i_min <- cv_lasso$index["min", 1]
abline(h = cv_lasso$cvm[i_min] + c(-1, 0, 1) * cv_lasso$cvsd[i_min],
       lty = c(2, 1, 2), lwd = 1.5, col = "darkorange")

# -----------------------------------------------------------------------------
# [u21-cv-lasso]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ridge, lasso y elastic-net
# -----------------------------------------------------------------------------
# cvm: deviance de validación media en cada lambda; cvsd: su error estándar entre bloques
n_vivos <- function(cvfit, s) sum(as.matrix(coef(cvfit, s = s))[-1, 1] != 0)
i_ref <- cv_lasso$index[, 1]                       # posiciones de lambda.min y lambda.1se
lasso_ref <- tibble::tibble(
  criterio    = c("lambda.min", "lambda.1se"),
  lambda      = cv_lasso$lambda[i_ref],
  deviance_cv = cv_lasso$cvm[i_ref],
  ee_cv       = cv_lasso$cvsd[i_ref],
  coef_vivos  = c(n_vivos(cv_lasso, "lambda.min"), n_vivos(cv_lasso, "lambda.1se")))
lasso_ref

# -----------------------------------------------------------------------------
# [fig-u21-coef-lasso]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ridge, lasso y elastic-net
# -----------------------------------------------------------------------------
asignacion <- attr(model.matrix(reformulate(candidatos), data = cartera), "assign")[-1]  # variable de cada columna
continua   <- !apply(x, 2, \(z) all(z %in% c(0, 1)))
escala_x   <- ifelse(continua, apply(x, 2, sd), 1)       # continuas por desviación típica
coef_lasso <- tibble::tibble(
  variable   = factor(candidatos[asignacion], levels = candidatos),
  nivel      = substring(colnames(x), nchar(candidatos[asignacion]) + 1),   # el nivel, sin el nombre
  columna    = dplyr::case_when(continua ~ "(por 1 DT)", nivel == "" ~ "(0/1)", .default = nivel),
  lambda.min = as.matrix(coef(cv_lasso, s = "lambda.min"))[-1, 1] * escala_x,
  lambda.1se = as.matrix(coef(cv_lasso, s = "lambda.1se"))[-1, 1] * escala_x) |>
  tidyr::pivot_longer(c(lambda.min, lambda.1se), names_to = "criterio", values_to = "coef")

ggplot(coef_lasso, aes(coef, columna, colour = criterio)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = 0, xmax = coef), position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6), size = 1.8) +
  facet_grid(variable ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(lambda.min = "steelblue", lambda.1se = "darkorange")) +
  labs(x = "coeficiente (escala log de la tasa)", y = NULL, colour = NULL) +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1), strip.placement = "outside",
        legend.position = "top")

# -----------------------------------------------------------------------------
# [u21-coef-parciales]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ridge, lasso y elastic-net
# -----------------------------------------------------------------------------
# factores que lambda.min deja partidos: alguna indicadora viva y alguna a cero
coef_lasso |>
  dplyr::filter(criterio == "lambda.min") |>
  dplyr::summarise(columnas = dplyr::n(), vivas = sum(coef != 0), .by = variable) |>
  dplyr::filter(vivas > 0, vivas < columnas)

# -----------------------------------------------------------------------------
# [u21-tres-penalizaciones]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Ridge, lasso y elastic-net
# -----------------------------------------------------------------------------
cv_ridge <- cv.glmnet(x, y, family = "poisson", offset = os, alpha = 0,
                      type.measure = "deviance", foldid = bloque)
cv_enet  <- cv.glmnet(x, y, family = "poisson", offset = os, alpha = 0.5,
                      type.measure = "deviance", foldid = bloque)
penalizaciones <- list(`ridge (alpha = 0)` = cv_ridge, `elastic-net (alpha = 0.5)` = cv_enet,
                       `lasso (alpha = 1)` = cv_lasso)
tibble::tibble(
  metodo          = names(penalizaciones),
  deviance_cv_min = sapply(penalizaciones, \(m) m$cvm[m$index["min", 1]]),
  vivos_min       = sapply(penalizaciones, n_vivos, s = "lambda.min"),
  deviance_cv_1se = sapply(penalizaciones, \(m) m$cvm[m$index["1se", 1]]),
  vivos_1se       = sapply(penalizaciones, n_vivos, s = "lambda.1se"))

# -----------------------------------------------------------------------------
# [u21-reajuste]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Reajustar con las variables seleccionadas
# -----------------------------------------------------------------------------
vars_sel   <- function(cvfit, s) unique(candidatos[asignacion[as.matrix(coef(cvfit, s = s))[-1, 1] != 0]])
formula_de <- function(v) reformulate(c(v, "offset(log(exposicion))"), response = "n_asistencia")
v_min <- vars_sel(cv_lasso, "lambda.min")
v_1se <- vars_sel(cv_lasso, "lambda.1se")

# anidada: selección y reajuste se repiten sin ver el bloque que se predice
d_anidada <- matrix(NA_real_, nrow(cartera), 2, dimnames = list(NULL, c("lambda.min", "lambda.1se")))
set.seed(SEMILLA_CURSO)
for (b in 1:10) {
  ent  <- bloque != b
  cv_b <- cv.glmnet(x[ent, ], y[ent], family = "poisson", offset = os[ent],
                    type.measure = "deviance", nfolds = 10)
  for (s in colnames(d_anidada)) {
    fit_b <- glm(formula_de(vars_sel(cv_b, s)), family = poisson, data = cartera[ent, ])
    d_anidada[!ent, s] <- poisson()$dev.resids(y[!ent], predict(fit_b, cartera[!ent, ], type = "response"),
                                               wt = 1)
  }
}

reajuste <- tibble::tibble(
  criterio     = c("lambda.min", "lambda.1se"),
  variables    = c(length(v_min), length(v_1se)),
  deviance_lasso      = lasso_ref$deviance_cv,
  deviance_cv_ingenua = c(mean(cv_contribuciones(formula_de(v_min), bloque)),
                          mean(cv_contribuciones(formula_de(v_1se), bloque))),
  deviance_cv_anidada = colMeans(d_anidada))
reajuste

# -----------------------------------------------------------------------------
# [u21-reg-validacion]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
# la verdad, columna a columna de x (b_dgp viene de la validación de la Sección 1.5)
verdad_x <- setNames(rep(0, ncol(x)), colnames(x))
verdad_x[c("edad_conductor", "antiguedad_carnet", "potencia_cv", "antiguedad_vehiculo",
           "zona_circulacionmixta", "zona_circulacionrural", "usocomercial",
           "tipo_vehiculomoto", "tipo_vehiculofurgoneta")] <-
  c(b_dgp[["z_edad"]], b_dgp[["z_exp"]], b_dgp[["z_pot"]], b_dgp[["z_antv"]],
    b_dgp[["mixta"]] - b_dgp[["urbana"]], -b_dgp[["urbana"]], b_dgp[["comercial"]],
    b_dgp[["moto"]], b_dgp[["furgoneta"]])
# el glm reajustado, con su IC de Wald al 95 %, en las columnas y la escala de x (escala_x);
# NA en las columnas de las variables no retenidas
coef_glm <- function(v, criterio) {
  m   <- glm(formula_de(v), family = poisson, data = cartera)
  ic  <- cbind(coef(m), confint.default(m))[-1, , drop = FALSE]
  out <- matrix(NA_real_, ncol(x), 3, dimnames = list(colnames(x), c("est", "inf", "sup")))
  out[rownames(ic), ] <- ic
  tibble::tibble(termino = colnames(x), criterio = criterio, as.data.frame(out * escala_x))
}
val_reg <- dplyr::bind_rows(coef_glm(v_min, "lambda.min"), coef_glm(v_1se, "lambda.1se")) |>
  dplyr::mutate(verdadero = unname(verdad_x[termino]),
                en_el_dgp = ifelse(verdadero != 0, "activa", "nula"),
                cubre     = inf <= verdadero & verdadero <= sup)

# recuento: columnas retenidas, intervalos que cubren la verdad e intervalos que excluyen el 0
resumen_sel <- val_reg |>
  dplyr::summarise(columnas = dplyr::n(), retenidas = sum(!is.na(est)),
                   cubren = sum(cubre, na.rm = TRUE),
                   excluyen_cero = sum(inf > 0 | sup < 0, na.rm = TRUE),
                   .by = c(criterio, en_el_dgp))
resumen_sel

# -----------------------------------------------------------------------------
# [fig-u21-reg-validacion]
#   1 · Regresión de Poisson: conteos y tasas
#     > 1.6 ¿Qué predictores retener? Validación cruzada y regularización
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
etiquetas <- tibble::tibble(termino  = colnames(x),
                            variable = factor(candidatos[asignacion], levels = candidatos),
                            columna  = coef_lasso$columna[coef_lasso$criterio == "lambda.min"])
val_reg |>
  dplyr::left_join(etiquetas, by = "termino") |>
  ggplot(aes(est, columna, colour = criterio)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(xmin = inf, xmax = sup), position = position_dodge(width = 0.6),
                  size = 0.25, na.rm = TRUE) +
  geom_point(aes(x = verdadero, shape = "verdadero (DGP)"), colour = "black", size = 2.5) +
  facet_grid(variable ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(lambda.min = "steelblue", lambda.1se = "darkorange")) +
  scale_shape_manual(values = c("verdadero (DGP)" = 18)) +
  labs(x = "coeficiente (escala log de la tasa)", y = NULL, colour = "glm() reajustado", shape = NULL) +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1), strip.placement = "outside",
        legend.position = "top")


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
