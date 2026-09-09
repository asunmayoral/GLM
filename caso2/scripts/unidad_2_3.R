# =============================================================================
# Caso 2 · Unidad 2.3 — 3 · Sobredispersión
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_3.qmd.
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
  AER = "1.2.17", DHARMa = "0.5.0", MASS = "7.3.65", MuMIn = "1.48.19",
  broom = "1.0.13", dplyr = "1.2.1", glmmTMB = "1.1.14", glmnet = "5.0",
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
# [fig-u23-eda-modelo]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > La huella: media frente a varianza
# -----------------------------------------------------------------------------
dplyr::bind_rows(
  purrr::map_dfr(c("edad_conductor", "potencia_cv"), ~ cartera |>
    dplyr::transmute(predictor = .x, nivel = factor(dplyr::ntile(.data[[.x]], 4)),
                     y = n_danos, e = exposicion) |>
    dplyr::group_by(predictor, nivel) |> dplyr::summarise(tasa = sum(y) / sum(e), .groups = "drop")),
  purrr::map_dfr(c("zona_circulacion", "uso", "tipo_vehiculo"), ~ cartera |>
    dplyr::transmute(predictor = .x, nivel = factor(.data[[.x]]),
                     y = n_danos, e = exposicion) |>
    dplyr::group_by(predictor, nivel) |> dplyr::summarise(tasa = sum(y) / sum(e), .groups = "drop"))) |>
  ggplot(aes(nivel, tasa)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) +
  labs(x = "nivel (cuartil, en los continuos)", y = "tasa de partes por daños (por unidad de exposición)")

# -----------------------------------------------------------------------------
# [fig-u23-media-varianza]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > La huella: media frente a varianza
# -----------------------------------------------------------------------------
m_pois <- glm(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                offset(log(exposicion)), family = poisson, data = cartera)

mu_i  <- fitted(m_pois); y <- cartera$n_danos
phi   <- sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)   # pendiente lineal
alpha <- coef(lm(((y - mu_i)^2 - y) / mu_i ~ mu_i - 1))[[1]]                # término cuadrático

pts <- tibble::tibble(mu = mu_i, y = y) |>
  dplyr::mutate(grupo = dplyr::ntile(mu, 12)) |>           # 12 grupos de riesgo esperado parecido
  dplyr::group_by(grupo) |>
  dplyr::summarise(media = mean(y), varianza = var(y), .groups = "drop")

rango  <- seq(min(pts$media), max(pts$media), length.out = 100)
curvas <- tibble::tibble(
  media = rango,
  `Poisson: Var = media`               = rango,
  `Lineal: phi·media (quasi/NB1)`      = phi * rango,
  `Cuadratica: media+a·media² (NB2)`   = rango + alpha * rango^2) |>
  tidyr::pivot_longer(-media, names_to = "forma", values_to = "var")

ggplot() +
  geom_line(data = curvas, aes(media, var, colour = forma), linewidth = 0.7) +
  geom_point(data = pts, aes(media, varianza), size = 2.5, colour = "grey20") +
  scale_colour_manual(values = c(
    "Poisson: Var = media"             = "grey55",
    "Lineal: phi·media (quasi/NB1)"    = "darkorange",
    "Cuadratica: media+a·media² (NB2)" = "steelblue")) +
  labs(x = "media observada (por grupo)", y = "varianza observada", colour = NULL)

# -----------------------------------------------------------------------------
# [fig-u23-heterogeneidad]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > De dónde viene
# -----------------------------------------------------------------------------
set.seed(2026)
n <- 5000; media <- 2
dplyr::bind_rows(
  tibble::tibble(poblacion = "Poisson homogénea",  y = rpois(n, media)),
  tibble::tibble(poblacion = "Mezcla heterogénea", y = rpois(n, media * rgamma(n, 1, 1)))) |>
  dplyr::filter(y <= 8) |>
  dplyr::count(poblacion, y) |>
  ggplot(aes(y, n, fill = poblacion)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Poisson homogénea" = "steelblue",
                               "Mezcla heterogénea" = "darkorange")) +
  labs(x = "nº de eventos", y = "frecuencia", fill = NULL)

# -----------------------------------------------------------------------------
# [u23-indice]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > El índice de dispersión
# -----------------------------------------------------------------------------
phi_pearson <- sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)
phi_dev     <- deviance(m_pois) / df.residual(m_pois)
c(pearson = phi_pearson, deviance = phi_dev)

# -----------------------------------------------------------------------------
# [fig-u23-rootograma]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > El rootograma: ver dónde falla el ajuste
# -----------------------------------------------------------------------------
mu <- fitted(m_pois); K <- 0:8
esperado  <- sapply(K, function(k) if (k < 8) sum(dpois(k, mu)) else sum(1 - ppois(7, mu)))
observado <- as.numeric(table(factor(pmin(cartera$n_danos, 8), levels = K)))

tibble::tibble(k = K, Observado = observado, `Poisson ajustada` = esperado) |>
  tidyr::pivot_longer(c(Observado, `Poisson ajustada`), names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "Poisson ajustada" = "darkorange")) +
  labs(x = "nº de partes por daños", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [u23-tests]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > Los contrastes: ¿hay sobredispersión y de qué tipo?
# -----------------------------------------------------------------------------
# Tests de sobredispersión
print(performance::check_overdispersion(m_pois))
print(AER::dispersiontest(m_pois))
print(DHARMa::testDispersion(m_pois, plot = FALSE))

# -----------------------------------------------------------------------------
# [u23-trafo]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > Los contrastes: ¿hay sobredispersión y de qué tipo?
# -----------------------------------------------------------------------------
print(AER::dispersiontest(m_pois, trafo = 1))   # Var = mu + alpha·mu    (lineal, NB1)
print(AER::dispersiontest(m_pois, trafo = 2))   # Var = mu + alpha·mu^2  (cuadratica, NB2)

# -----------------------------------------------------------------------------
# [u23-trafo-manual]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > Los contrastes: ¿hay sobredispersión y de qué tipo?
# -----------------------------------------------------------------------------
mu <- fitted(m_pois); y <- cartera$n_danos
r  <- ((y - mu)^2 - y) / mu                   # residuo de Cameron–Trivedi (media 0 bajo la Poisson)
test_var <- function(formula, etiqueta) {
  s <- coef(summary(lm(formula)))[1, ]        # coeficiente = alpha estimado
  data.frame(estructura = etiqueta, alpha = round(s[1], 3),
             t = round(s[3], 1), p_valor = signif(s[4], 3), row.names = NULL)
}
rbind(test_var(r ~ 1,      "lineal (NB1): Var = mu + a*mu"),        # alpha = media de r
      test_var(r ~ mu - 1, "cuadratica (NB2): Var = mu + a*mu^2"))  # pendiente de r sobre mu

# -----------------------------------------------------------------------------
# [u23-quasi-fit]
#   3 · Sobredispersión > 3.2 Quasi-Poisson > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_quasi <- glm(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                 offset(log(exposicion)), family = quasipoisson, data = cartera)
summary(m_quasi)$dispersion   # phi estimado

# -----------------------------------------------------------------------------
# [u23-quasi-comp]
#   3 · Sobredispersión > 3.2 Quasi-Poisson > Ajuste e interpretación
# -----------------------------------------------------------------------------
dplyr::left_join(
  broom::tidy(m_pois)  |> dplyr::select(term, estimacion = estimate, se_poisson = std.error),
  broom::tidy(m_quasi) |> dplyr::select(term, se_quasi = std.error),
  by = "term")

# -----------------------------------------------------------------------------
# [fig-u23-quasi-resid]
#   3 · Sobredispersión > 3.2 Quasi-Poisson > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
tibble::tibble(ajustado = fitted(m_quasi),
               residuo  = residuals(m_quasi, type = "pearson")) |>
  ggplot(aes(ajustado, residuo)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, colour = "darkorange") +
  labs(x = "valor ajustado", y = "residuo de Pearson")

# -----------------------------------------------------------------------------
# [u23-nb-fit]
#   3 · Sobredispersión > 3.3 Binomial negativa > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_nb <- MASS::glm.nb(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                       offset(log(exposicion)), data = cartera)
c(theta = m_nb$theta, se_theta = m_nb$SE.theta)   # dispersion estimada

# -----------------------------------------------------------------------------
# [u23-nb1]
#   3 · Sobredispersión > 3.3 Binomial negativa > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_nb1 <- glmmTMB::glmmTMB(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso +
                            tipo_vehiculo + offset(log(exposicion)),
                          family = glmmTMB::nbinom1, data = cartera)
sigma(m_nb1)   # dispersion alpha de la NB1

# -----------------------------------------------------------------------------
# [u23-nb1-quasi]
#   3 · Sobredispersión > 3.3 Binomial negativa > Ajuste e interpretación
# -----------------------------------------------------------------------------
se_q   <- summary(m_quasi)$coefficients[, "Std. Error"]
se_nb1 <- summary(m_nb1)$coefficients$cond[, "Std. Error"]
data.frame(term = names(se_q), se_quasi = round(se_q, 4),
           se_nb1 = round(se_nb1[names(se_q)], 4))

# -----------------------------------------------------------------------------
# [u23-nb-disp]
#   3 · Sobredispersión
#     > 3.3 Binomial negativa
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
disp <- function(m) sum(residuals(m, type = "pearson")^2) / df.residual(m)
round(c(NB2 = disp(m_nb), NB1 = disp(m_nb1)), 3)

# -----------------------------------------------------------------------------
# [fig-u23-nb2-dharma]
#   3 · Sobredispersión
#     > 3.3 Binomial negativa
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::simulateResiduals(m_nb, plot = TRUE)

# -----------------------------------------------------------------------------
# [fig-u23-nb1-dharma]
#   3 · Sobredispersión
#     > 3.3 Binomial negativa
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::simulateResiduals(m_nb1, plot = TRUE)

# -----------------------------------------------------------------------------
# [fig-u23-nb-rootograma]
#   3 · Sobredispersión
#     > 3.3 Binomial negativa
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
mu_p <- fitted(m_pois); mu_n <- fitted(m_nb); th <- m_nb$theta; K <- 0:8
esp_pois <- sapply(K, function(k) if (k < 8) sum(dpois(k, mu_p))              else sum(1 - ppois(7, mu_p)))
esp_nb   <- sapply(K, function(k) if (k < 8) sum(dnbinom(k, size = th, mu = mu_n)) else sum(1 - pnbinom(7, size = th, mu = mu_n)))
observado <- as.numeric(table(factor(pmin(cartera$n_danos, 8), levels = K)))

tibble::tibble(k = K, Observado = observado, Poisson = esp_pois, `NB2` = esp_nb) |>
  tidyr::pivot_longer(c(Observado, Poisson, `NB2`), names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, colour = fuente, group = fuente)) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c("Observado" = "grey30", "Poisson" = "darkorange", "NB2" = "steelblue")) +
  labs(x = "nº de partes por daños", y = "nº de pólizas", colour = NULL)

# -----------------------------------------------------------------------------
# [fig-u23-comp-modelos]
#   3 · Sobredispersión
#     > 3.4 Elegir, comparar y conectar
#       > Los cuatro modelos, lado a lado
# -----------------------------------------------------------------------------
ee <- function(m) {                                   # estimación y EE, robusto a glm/glm.nb/glmmTMB
  s <- if (inherits(m, "glmmTMB")) summary(m)$coefficients$cond else summary(m)$coefficients
  data.frame(term = rownames(s), estimate = s[, 1], std.error = s[, 2])
}
mods <- list(Poisson = m_pois, `Quasi-Poisson` = m_quasi, NB1 = m_nb1, NB2 = m_nb)

purrr::imap_dfr(mods, ~ dplyr::mutate(ee(.x), modelo = .y)) |>
  dplyr::filter(term %in% c("potencia_cv", "zona_circulacionrural", "usocomercial")) |>
  dplyr::mutate(modelo = factor(modelo, levels = names(mods))) |>
  ggplot(aes(modelo, estimate, colour = modelo)) +
  geom_hline(yintercept = 0, linetype = 3, colour = "grey60") +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  facet_wrap(~ term, scales = "free_y") +
  labs(x = NULL, y = "estimación (± 1 EE)", colour = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# -----------------------------------------------------------------------------
# [u23-aic-comp]
#   3 · Sobredispersión
#     > 3.4 Elegir, comparar y conectar
#       > La decisión formal: AIC y LRT
# -----------------------------------------------------------------------------
mods_lik <- list(Poisson = m_pois, NB1 = m_nb1, NB2 = m_nb)
data.frame(modelo = names(mods_lik),
           df  = sapply(mods_lik, function(m) attr(logLik(m), "df")),
           AIC = round(sapply(mods_lik, AIC), 1)) |>
  dplyr::arrange(AIC)

# -----------------------------------------------------------------------------
# [u23-lrt]
#   3 · Sobredispersión
#     > 3.4 Elegir, comparar y conectar
#       > La decisión formal: AIC y LRT
# -----------------------------------------------------------------------------
LR <- 2 * (as.numeric(logLik(m_nb)) - as.numeric(logLik(m_pois)))
c(LR = round(LR, 1), p_valor = pchisq(LR, df = 1, lower.tail = FALSE) / 2)   # /2 por el borde

# -----------------------------------------------------------------------------
# [u23-perf]
#   3 · Sobredispersión
#     > 3.4 Elegir, comparar y conectar
#       > Una lectura estándar de bondad de ajuste
# -----------------------------------------------------------------------------
performance::compare_performance(Poisson = m_pois, NB1 = m_nb1, NB2 = m_nb,
                                 metrics = c("AIC", "BIC", "RMSE"))

# -----------------------------------------------------------------------------
# [u23-pred-comp]
#   3 · Sobredispersión
#     > 3.4 Elegir, comparar y conectar
#       > Predicciones: misma media, distinto riesgo
# -----------------------------------------------------------------------------
i    <- which.max(fitted(m_nb))                          # la póliza de mayor riesgo esperado
mu_p <- unname(predict(m_pois, cartera[i, ], type = "response"))   # media predicha (Poisson)
mu_n <- unname(predict(m_nb,   cartera[i, ], type = "response"))   # media predicha (NB2)
round(c(media_pois = mu_p, media_nb = mu_n,                        # casi iguales
        P0_pois    = dpois(0, mu_p),        P0_nb    = dnbinom(0, size = m_nb$theta, mu = mu_n),      # P(Y = 0)
        Pge10_pois = 1 - ppois(9, mu_p),    Pge10_nb = 1 - pnbinom(9, size = m_nb$theta, mu = mu_n)), # P(Y >= 10)
      3)

# -----------------------------------------------------------------------------
# [fig-u23-pred-dist]
#   3 · Sobredispersión
#     > 3.4 Elegir, comparar y conectar
#       > Predicciones: misma media, distinto riesgo
# -----------------------------------------------------------------------------
K <- 0:16
mu_n1 <- unname(predict(m_nb1, cartera[i, ], type = "response"))
dist <- tibble::tibble(k = K,
                       Poisson = dpois(K, mu_p),
                       NB1 = dnbinom(K, size = mu_n1 / sigma(m_nb1), mu = mu_n1),   # NB1: Var = mu(1+alpha)
                       NB2 = dnbinom(K, size = m_nb$theta, mu = mu_n))
dist |>
  tidyr::pivot_longer(-k, names_to = "modelo", values_to = "prob") |>
  ggplot(aes(k, prob, colour = modelo, group = modelo)) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.6) +
  scale_colour_manual(values = c("Poisson" = "darkorange", "NB1" = "seagreen", "NB2" = "steelblue")) +
  labs(x = "nº de partes", y = "probabilidad", colour = NULL)

# -----------------------------------------------------------------------------
# [u23-validacion]
#   3 · Sobredispersión
#     > 3.4 Elegir, comparar y conectar
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(cartera, "verdad")
c(theta_DGP = verdad$theta_nb, theta_estimado = round(m_nb$theta, 3))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
