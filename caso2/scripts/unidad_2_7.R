# =============================================================================
# Caso 2 · Unidad 2.7 — 7 · Selección, validación cruzada y regularización
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_7.qmd.
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
  DHARMa = "0.5.0", MASS = "7.3.65", MuMIn = "1.48.19", broom = "1.0.13",
  dplyr = "1.2.1", glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  marginaleffects = "0.32.0", performance = "0.17.0", pscl = "1.5.9",
  sessioninfo = "1.2.4", survival = "3.8.6", tibble = "3.3.1",
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
library(pscl)          # hurdle / zeroinfl
library(glmmTMB)       # conteos mixtos / ceros
library(lme4)          # glmer (Poisson)
library(DHARMa); library(performance); library(marginaleffects)
library(survival)      # riesgos a trozos (base-recommended)
library(MuMIn); library(glmnet)   # selección / regularización 
library(vcd)           # mosaicos para tablas de contingencia (2.2)
library(vcdExtra)      # zero-inflated (2.4) y utilidades de tablas (2.2)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
glimpse(cartera)

# -----------------------------------------------------------------------------
# [u27-datos]
#   7 · Selección, validación cruzada y regularización
# -----------------------------------------------------------------------------
# Predictores del riesgo (activos en el DGP) + bloque de ruido (relación nula por diseño).
activos <- c("edad_conductor", "antiguedad_carnet", "potencia_cv", "antiguedad_vehiculo",
             "zona_circulacion", "uso", "tipo_vehiculo")
ruido   <- c("sexo", "estado_civil", "color_vehiculo", "tiene_garaje", "forma_pago",
             "financiado", "km_declarados", "valor_vehiculo", "n_conductores", "antiguedad_cliente")
predictores <- c(activos, ruido)

cartera |> dplyr::select(n_asistencia, exposicion, dplyr::all_of(predictores)) |> dplyr::glimpse()

# -----------------------------------------------------------------------------
# [u27-optimismo]
#   7 · Selección, validación cruzada y regularización
#     > 7.1 De la selección clásica a la validación cruzada
#       > Ajustar bien no es predecir bien
# -----------------------------------------------------------------------------
f_activos <- reformulate(c(activos, "offset(log(exposicion))"), response = "n_asistencia")
f_todo    <- reformulate(c(predictores, "offset(log(exposicion))"), response = "n_asistencia")

m_activos <- glm(f_activos, family = poisson, data = cartera)
m_todo    <- glm(f_todo,    family = poisson, data = cartera)

c(dev_activos = deviance(m_activos), dev_todo = deviance(m_todo),
  gl_activos = m_activos$df.residual, gl_todo = m_todo$df.residual)

# -----------------------------------------------------------------------------
# [u27-lrt]
#   7 · Selección, validación cruzada y regularización
#     > 7.1 De la selección clásica a la validación cruzada
#       > Ajustar bien no es predecir bien
# -----------------------------------------------------------------------------
anova(m_activos, m_todo, test = "LRT")

# -----------------------------------------------------------------------------
# [u27-aic-bic]
#   7 · Selección, validación cruzada y regularización
#     > 7.1 De la selección clásica a la validación cruzada
#       > El apaño analítico: AIC y BIC
# -----------------------------------------------------------------------------
tibble::tibble(
  modelo = c("solo activos", "activos + ruido"),
  gl     = c(length(coef(m_activos)), length(coef(m_todo))),
  AIC    = c(AIC(m_activos), AIC(m_todo)),
  BIC    = c(BIC(m_activos), BIC(m_todo)))

# -----------------------------------------------------------------------------
# [u27-cv-funcion]
#   7 · Selección, validación cruzada y regularización
#     > 7.1 De la selección clásica a la validación cruzada
#       > La validación cruzada: medir el error donde importa
# -----------------------------------------------------------------------------
# Deviance de Poisson OUT-OF-SAMPLE = deviance de la familia aplicada a (y de test, mu predicha).
# poisson()$dev.resids(y, mu, w) da las contribuciones a la deviance que R suma en deviance(fit);
# aquí las sumamos sobre las filas que el modelo NO vio. No hay que rehacer la fórmula a mano.
dev_poisson <- function(y, mu) sum(poisson()$dev.resids(y, mu, wt = 1))

# Validación cruzada de k = 10 bloques para un glm de Poisson con offset. Devuelve la deviance
# de predicción (out-of-sample) por observación.
cv_deviance <- function(formula, datos, k = 10, semilla = SEMILLA_CURSO) {
  set.seed(semilla)
  bloque <- sample(rep(seq_len(k), length.out = nrow(datos)))  # partición aleatoria en k bloques
  dev_test <- 0
  for (b in seq_len(k)) {
    ent <- datos[bloque != b, ]; test <- datos[bloque == b, ]  # entrena con k-1 bloques, evalúa en el b
    fit <- glm(formula, family = poisson, data = ent)
    mu  <- predict(fit, newdata = test, type = "response")     # incluye el offset
    dev_test <- dev_test + dev_poisson(test$n_asistencia, mu)
  }
  dev_test / nrow(datos)      # deviance de predicción por observación
}

# -----------------------------------------------------------------------------
# [u27-cv-comparar]
#   7 · Selección, validación cruzada y regularización
#     > 7.1 De la selección clásica a la validación cruzada
#       > La validación cruzada: medir el error donde importa
# -----------------------------------------------------------------------------
f_nulo <- n_asistencia ~ offset(log(exposicion))
tibble::tibble(
  modelo       = c("nulo", "solo activos", "activos + ruido"),
  cv_deviance  = c(cv_deviance(f_nulo, cartera),
                   cv_deviance(f_activos, cartera),
                   cv_deviance(f_todo, cartera)))

# -----------------------------------------------------------------------------
# [u27-glmnet-matriz]
#   7 · Selección, validación cruzada y regularización
#     > 7.2 Regularización: ridge, lasso y elastic-net
#       > glmnet sobre la Poisson
# -----------------------------------------------------------------------------
library(glmnet)
x  <- model.matrix(reformulate(predictores), data = cartera)[, -1]  # quita el intercepto
y  <- cartera$n_asistencia
os <- log(cartera$exposicion)                                       # offset de la exposición
dim(x)

# -----------------------------------------------------------------------------
# [fig-u27-ruta-lasso]
#   7 · Selección, validación cruzada y regularización
#     > 7.2 Regularización: ridge, lasso y elastic-net
#       > glmnet sobre la Poisson
# -----------------------------------------------------------------------------
fit_lasso <- glmnet(x, y, family = "poisson", offset = os, alpha = 1)
plot(fit_lasso, xvar = "lambda", label = TRUE)

# -----------------------------------------------------------------------------
# [fig-u27-cvglmnet]
#   7 · Selección, validación cruzada y regularización
#     > 7.2 Regularización: ridge, lasso y elastic-net
#       > Elegir $\lambda$: cv.glmnet
# -----------------------------------------------------------------------------
set.seed(SEMILLA_CURSO)
cv_lasso <- cv.glmnet(x, y, family = "poisson", offset = os, alpha = 1,
                      type.measure = "deviance", nfolds = 10)
plot(cv_lasso)
c(lambda_min = cv_lasso$lambda.min, lambda_1se = cv_lasso$lambda.1se)

# -----------------------------------------------------------------------------
# [u27-lasso-coef]
#   7 · Selección, validación cruzada y regularización
#     > 7.2 Regularización: ridge, lasso y elastic-net
#       > El lasso como selección: ¿acierta con la verdad?
# -----------------------------------------------------------------------------
coef_1se <- coef(cv_lasso, s = "lambda.1se")
coef_min <- coef(cv_lasso, s = "lambda.min")

retenidas <- function(coefs) {
  m <- as.matrix(coefs); nz <- rownames(m)[m[, 1] != 0]
  setdiff(nz, "(Intercept)")
}

# Imprime, para cada lambda, la lista de variables retenidas (con su número).
lista_retenidas <- function(nombre, vars) {
  cat("**`", nombre, "`** — ", length(vars), " variables retenidas:\n\n", sep = "")
  cat(paste0("- `", vars, "`"), sep = "\n")
  cat("\n\n")
}
lista_retenidas("lambda.1se", retenidas(coef_1se))
lista_retenidas("lambda.min", retenidas(coef_min))

# -----------------------------------------------------------------------------
# [u27-lasso-irr]
#   7 · Selección, validación cruzada y regularización
#     > 7.2 Regularización: ridge, lasso y elastic-net
#       > El lasso como selección: ¿acierta con la verdad?
# -----------------------------------------------------------------------------
# Coeficientes no nulos del modelo 1se, en escala IRR (exp del coeficiente).
m1 <- as.matrix(coef_1se)
tibble::tibble(termino = rownames(m1), coef = m1[, 1]) |>
  dplyr::filter(coef != 0, termino != "(Intercept)") |>
  dplyr::mutate(IRR = exp(coef)) |>
  dplyr::arrange(dplyr::desc(abs(coef)))

# -----------------------------------------------------------------------------
# [u27-verdad]
#   7 · Selección, validación cruzada y regularización
#     > 7.2 Regularización: ridge, lasso y elastic-net
#       > El lasso como selección: ¿acierta con la verdad?
# -----------------------------------------------------------------------------
verdad <- attr(cartera, "verdad")
tibble::tibble(termino = verdad$nombres_beta, beta = verdad$betas$limpio) |>
  dplyr::mutate(IRR = exp(beta))

# -----------------------------------------------------------------------------
# [u27-tres-penalizaciones]
#   7 · Selección, validación cruzada y regularización
#     > 7.2 Regularización: ridge, lasso y elastic-net
#       > Ridge, lasso y elastic-net, comparados
# -----------------------------------------------------------------------------
set.seed(SEMILLA_CURSO)
cv_ridge <- cv.glmnet(x, y, family = "poisson", offset = os, alpha = 0,   type.measure = "deviance", nfolds = 10)
cv_enet  <- cv.glmnet(x, y, family = "poisson", offset = os, alpha = 0.5, type.measure = "deviance", nfolds = 10)

n_no_nulos <- function(cvfit) sum(as.matrix(coef(cvfit, s = "lambda.1se"))[, 1] != 0) - 1
tibble::tibble(
  metodo          = c("ridge (a=0)", "elastic-net (a=0.5)", "lasso (a=1)"),
  dev_cv_1se      = c(cv_ridge$cvm[cv_ridge$index["1se", ]],
                      cv_enet$cvm[cv_enet$index["1se", ]],
                      cv_lasso$cvm[cv_lasso$index["1se", ]]),
  variables_vivas = c(n_no_nulos(cv_ridge), n_no_nulos(cv_enet), n_no_nulos(cv_lasso)))

# -----------------------------------------------------------------------------
# [u27-binomial]
#   7 · Selección, validación cruzada y regularización
#     > 7.3 La misma máquina en la binaria
# -----------------------------------------------------------------------------
set.seed(SEMILLA_CURSO)
cv_bin <- cv.glmnet(x, cartera$evento, family = "binomial",
                    type.measure = "deviance", alpha = 1, nfolds = 10)

# Nº de predictores retenidos en cada criterio (la señal binaria de 'evento' es débil).
n_sel <- function(s) sum(as.matrix(coef(cv_bin, s = s))[, 1] != 0) - 1L
c(no_nulos_1se = n_sel("lambda.1se"), no_nulos_min = n_sel("lambda.min"))

# -----------------------------------------------------------------------------
# [u27-binomial-min]
#   7 · Selección, validación cruzada y regularización
#     > 7.3 La misma máquina en la binaria
# -----------------------------------------------------------------------------
# Coeficientes que sobreviven a lambda.min, en escala ODDS RATIO (exp del coeficiente).
m_bin <- as.matrix(coef(cv_bin, s = "lambda.min"))
tibble::tibble(termino = rownames(m_bin), coef = m_bin[, 1]) |>
  dplyr::filter(coef != 0, termino != "(Intercept)") |>
  dplyr::mutate(odds_ratio = exp(coef)) |>          # en binomial, exp(coef) es un ODDS RATIO
  dplyr::arrange(dplyr::desc(abs(coef)))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
