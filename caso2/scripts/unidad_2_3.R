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
  AER = "1.2.17", DHARMa = "0.5.0", MASS = "7.3.65", broom = "1.0.13",
  dplyr = "1.2.1", glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  marginaleffects = "0.32.0", performance = "0.17.0", pscl = "1.5.9",
  purrr = "1.2.2", sessioninfo = "1.2.4", survival = "3.8.6",
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
# [fig-u23-eda-modelo]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > El problema: qué queremos predecir y con qué
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
# [u23-pois-fit]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > El problema: qué queremos predecir y con qué
# -----------------------------------------------------------------------------
m_pois <- glm(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                offset(log(exposicion)), family = poisson, data = cartera)
c(deviance = round(deviance(m_pois), 1), gl = df.residual(m_pois))

# -----------------------------------------------------------------------------
# [u23-indice]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > Indicios de sobredispersión
# -----------------------------------------------------------------------------
phi_pearson <- sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)
phi_dev     <- deviance(m_pois) / df.residual(m_pois)
c(pearson = phi_pearson, deviance = phi_dev)

# -----------------------------------------------------------------------------
# [u23-rootograma-tabla]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > Indicios de sobredispersión
# -----------------------------------------------------------------------------
mu_pois <- fitted(m_pois); K <- 0:8
# la última clase agrupa 8 o más partes, para que las esperadas sumen n
rootograma <- tibble::tibble(
  k         = K,
  observado = as.numeric(table(factor(pmin(cartera$n_danos, 8), levels = K))),
  Poisson   = sapply(K, \(k) if (k < 8) sum(dpois(k, mu_pois)) else sum(1 - ppois(7, mu_pois))))
rootograma |> dplyr::mutate(Poisson = round(Poisson, 1))

# -----------------------------------------------------------------------------
# [fig-u23-rootograma]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > Indicios de sobredispersión
# -----------------------------------------------------------------------------
ggplot(rootograma) +
  geom_rect(aes(xmin = k - 0.4, xmax = k + 0.4,
                ymin = sqrt(Poisson) - sqrt(observado), ymax = sqrt(Poisson)),
            fill = "steelblue", alpha = 0.7) +
  geom_line(aes(k, sqrt(Poisson)), colour = "darkorange", linewidth = 0.8) +
  geom_point(aes(k, sqrt(Poisson)), colour = "darkorange") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = K, labels = c(0:7, "8+")) +
  labs(x = "nº de partes por daños", y = "raíz de la frecuencia")

# -----------------------------------------------------------------------------
# [fig-u23-heterogeneidad]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > Causas: de dónde viene y dónde aparece
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
# [u23-tests]
#   3 · Sobredispersión
#     > 3.1 Qué es, de dónde viene y cómo se identifica
#       > Contrastes: ¿hay sobredispersión?
# -----------------------------------------------------------------------------
# Tests de sobredispersión
print(performance::check_overdispersion(m_pois))
print(DHARMa::testDispersion(m_pois, plot = FALSE))
print(AER::dispersiontest(m_pois))

# -----------------------------------------------------------------------------
# [fig-u23-media-varianza]
#   3 · Sobredispersión
#     > 3.2 Estructuras para la varianza
#       > Las opciones: qué estructura dar a la varianza
# -----------------------------------------------------------------------------
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
# [u23-trafo]
#   3 · Sobredispersión
#     > 3.2 Estructuras para la varianza
#       > Contrastes: ¿de qué tipo es la sobredispersión?
# -----------------------------------------------------------------------------
print(AER::dispersiontest(m_pois, trafo = 1))   # Var = mu + alpha·mu    (lineal, NB1)
print(AER::dispersiontest(m_pois, trafo = 2))   # Var = mu + alpha·mu^2  (cuadratica, NB2)

# -----------------------------------------------------------------------------
# [u23-quasi-fit]
#   3 · Sobredispersión > 3.3 Quasi-Poisson > Modelización y ajuste
# -----------------------------------------------------------------------------
m_quasi <- glm(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                 offset(log(exposicion)), family = quasipoisson, data = cartera)
broom::tidy(m_quasi) # ajuste
summary(m_quasi)$dispersion   # phi estimado

# -----------------------------------------------------------------------------
# [u23-quasi-comp]
#   3 · Sobredispersión > 3.3 Quasi-Poisson > Modelización y ajuste
# -----------------------------------------------------------------------------
dplyr::left_join(
  broom::tidy(m_pois)  |> dplyr::select(term, estimacion = estimate, se_poisson = std.error),
  broom::tidy(m_quasi) |> dplyr::select(term, se_quasi = std.error),
  by = "term")

# -----------------------------------------------------------------------------
# [u23-quasi-sel]
#   3 · Sobredispersión
#     > 3.3 Quasi-Poisson
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
c(poisson = deviance(m_pois), quasi = deviance(m_quasi))   # la cuasi-deviance es la de la Poisson

m_quasi2 <- update(m_quasi, . ~ (edad_conductor + potencia_cv + zona_circulacion + uso +
                                   tipo_vehiculo)^2 + offset(log(exposicion)))   # todas las dobles
drop1(m_quasi2, test = "F")                 # cada interacción, una a una

m_quasi_eu <- update(m_quasi, . ~ . + edad_conductor:uso)   # la única con p < 0.05
anova(m_quasi, m_quasi_eu, test = "F")

drop1(m_quasi, test = "F")                  # ¿hace falta cada efecto principal?

# QAIC a mano: log-verosimilitud de la Poisson con la misma media, c común (el del modelo más completo)
c_hat <- summary(m_quasi2)$dispersion
qaic  <- function(m) -2 * as.numeric(logLik(update(m, family = poisson))) / c_hat +
  2 * (length(coef(m)) + 1)
round(c_hat, 2)
round(c(principales = qaic(m_quasi), mas_edad_uso = qaic(m_quasi_eu),
        todas_dobles = qaic(m_quasi2)), 1)

# -----------------------------------------------------------------------------
# [fig-u23-quasi-resid]
#   3 · Sobredispersión
#     > 3.3 Quasi-Poisson
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
tibble::tibble(ajustado = fitted(m_quasi),
               residuo  = residuals(m_quasi, type = "pearson")) |>
  ggplot(aes(ajustado, residuo)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, colour = "darkorange") +
  labs(x = "valor ajustado", y = "residuo de Pearson")

# -----------------------------------------------------------------------------
# [u23-quasi-var]
#   3 · Sobredispersión
#     > 3.3 Quasi-Poisson
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
tibble::tibble(ajustado = fitted(m_quasi), r2 = residuals(m_quasi, type = "pearson")^2) |>
  dplyr::mutate(quintil = dplyr::ntile(ajustado, 5)) |>
  dplyr::summarise(mu_medio = mean(ajustado), var_pearson = mean(r2), .by = quintil) |>
  dplyr::arrange(quintil)

# -----------------------------------------------------------------------------
# [fig-u23-quasi-dharma]
#   3 · Sobredispersión
#     > 3.3 Quasi-Poisson
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::simulateResiduals(m_pois, plot = TRUE)

# -----------------------------------------------------------------------------
# [fig-u23-quasi-pred]
#   3 · Sobredispersión > 3.3 Quasi-Poisson > Predicción
# -----------------------------------------------------------------------------
perfil <- tidyr::expand_grid(
  edad_conductor   = 18:85,
  potencia_cv      = median(cartera$potencia_cv),
  zona_circulacion = factor("urbana",     levels(cartera$zona_circulacion)),
  uso              = factor("particular", levels(cartera$uso)),
  tipo_vehiculo    = factor("turismo",    levels(cartera$tipo_vehiculo)),
  exposicion       = 1)                                  # un año: la media es la frecuencia anual

# IC en la escala del predictor lineal y vuelta con exp(): la banda no puede bajar de 0
banda <- function(m, modelo) {
  p <- predict(m, perfil, type = "link", se.fit = TRUE)
  dplyr::mutate(perfil, modelo = modelo, mu = exp(p$fit),
                inf = exp(p$fit - 1.96 * p$se.fit), sup = exp(p$fit + 1.96 * p$se.fit))
}
pred_q <- dplyr::bind_rows(banda(m_pois, "Poisson"), banda(m_quasi, "quasi-Poisson"))

ggplot(pred_q, aes(edad_conductor, mu, colour = modelo, fill = modelo)) +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.25, colour = NA) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Poisson" = "darkorange", "quasi-Poisson" = "steelblue"),
                      aesthetics = c("colour", "fill")) +
  labs(x = "edad del conductor", y = "nº medio de partes al año", colour = NULL, fill = NULL)

# -----------------------------------------------------------------------------
# [u23-quasi-pred-tabla]
#   3 · Sobredispersión > 3.3 Quasi-Poisson > Predicción
# -----------------------------------------------------------------------------
pred_q |>
  dplyr::filter(edad_conductor %in% c(25, 45, 65)) |>
  dplyr::select(modelo, edad_conductor, mu, inf, sup)
sqrt(summary(m_quasi)$dispersion)   # factor que ensancha la banda en la escala log

# -----------------------------------------------------------------------------
# [fig-u23-nb-mezcla]
#   3 · Sobredispersión > 3.4 Binomial negativa > Modelización y ajuste
# -----------------------------------------------------------------------------
alpha_ej <- 1                                   # mismo alfa en las dos: solo cambia el reparto
perfiles <- c("riesgo bajo (mu = 0.5)" = 0.5, "riesgo alto (mu = 2.5)" = 2.5)
mezclas  <- tidyr::expand_grid(modelo = c("NB1", "NB2"), perfil = factor(names(perfiles), names(perfiles))) |>
  dplyr::mutate(mu    = unname(perfiles[as.character(perfil)]),
                forma = ifelse(modelo == "NB1", mu / alpha_ej, 1 / alpha_ej))   # shape = rate

mezclas |>
  tidyr::expand_grid(v = seq(0.01, 3, length.out = 300)) |>
  dplyr::mutate(densidad = dgamma(v, shape = forma, rate = forma)) |>
  ggplot(aes(v, densidad, colour = perfil, linetype = perfil)) +   # trazo distinto: en la NB2 se solapan
  geom_line(linewidth = 0.8) +
  facet_wrap(~ modelo) +
  coord_cartesian(ylim = c(0, 1.6)) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "multiplicador de riesgo V", y = "densidad", colour = NULL, linetype = NULL) +
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# [u23-nb-mezcla-tabla]
#   3 · Sobredispersión > 3.4 Binomial negativa > Modelización y ajuste
# -----------------------------------------------------------------------------
mezclas |>
  dplyr::mutate(var_V   = 1 / forma,                 # varianza de una Ga(shape = rate = forma)
                var_Y   = mu + mu^2 * var_V,
                indice  = var_Y / mu) |>
  dplyr::select(modelo, mu, var_V, var_Y, indice)

# -----------------------------------------------------------------------------
# [u23-nb-fit]
#   3 · Sobredispersión > 3.4 Binomial negativa > Modelización y ajuste
# -----------------------------------------------------------------------------
m_nb <- MASS::glm.nb(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                       offset(log(exposicion)), data = cartera)
c(theta = m_nb$theta, se_theta = m_nb$SE.theta)   # dispersion estimada

# -----------------------------------------------------------------------------
# [u23-nb1]
#   3 · Sobredispersión > 3.4 Binomial negativa > Modelización y ajuste
# -----------------------------------------------------------------------------
m_nb1 <- glmmTMB::glmmTMB(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso +
                            tipo_vehiculo + offset(log(exposicion)),
                          family = glmmTMB::nbinom1, data = cartera)
sigma(m_nb1)   # dispersion alpha de la NB1

# -----------------------------------------------------------------------------
# [u23-nb1-quasi]
#   3 · Sobredispersión > 3.4 Binomial negativa > Modelización y ajuste
# -----------------------------------------------------------------------------
b_q    <- coef(m_quasi)
b_nb1  <- glmmTMB::fixef(m_nb1)$cond
se_q   <- summary(m_quasi)$coefficients[, "Std. Error"]
se_nb1 <- summary(m_nb1)$coefficients$cond[, "Std. Error"]
data.frame(term = names(b_q),
           coef_quasi = round(b_q, 4), coef_nb1 = round(b_nb1[names(b_q)], 4),
           se_quasi   = round(se_q, 4), se_nb1   = round(se_nb1[names(b_q)], 4),
           variacion  = round((se_nb1[names(b_q)] - se_q) / se_q, 3))

# -----------------------------------------------------------------------------
# [u23-nb-sel]
#   3 · Sobredispersión
#     > 3.4 Binomial negativa
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
m_nb_dobles <- update(m_nb, . ~ (edad_conductor + potencia_cv + zona_circulacion + uso +
                                   tipo_vehiculo)^2 + offset(log(exposicion)))
drop1(m_nb_dobles, test = "Chisq")   # LRT de cada interacción, con theta fijo en el del modelo mayor
anova(m_nb, m_nb_dobles)             # LRT global, reestimando theta en cada modelo
drop1(m_nb, test = "Chisq")          # ¿hace falta cada efecto principal?
round(c(AIC_principales = AIC(m_nb), AIC_dobles = AIC(m_nb_dobles)), 1)

# -----------------------------------------------------------------------------
# [u23-nb-disp]
#   3 · Sobredispersión
#     > 3.4 Binomial negativa
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
disp <- function(m) sum(residuals(m, type = "pearson")^2) / df.residual(m)
round(c(NB2 = disp(m_nb), NB1 = disp(m_nb1)), 3)

# -----------------------------------------------------------------------------
# [fig-u23-nb2-dharma]
#   3 · Sobredispersión
#     > 3.4 Binomial negativa
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::simulateResiduals(m_nb, plot = TRUE)

# -----------------------------------------------------------------------------
# [fig-u23-nb1-dharma]
#   3 · Sobredispersión
#     > 3.4 Binomial negativa
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::simulateResiduals(m_nb1, plot = TRUE)

# -----------------------------------------------------------------------------
# [u23-nb-rootograma-tabla]
#   3 · Sobredispersión
#     > 3.4 Binomial negativa
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
# añade a la tabla de la Sección 3.1 las frecuencias esperadas por la NB2
raiz_nb <- dplyr::mutate(rootograma,
  NB2 = sapply(K, \(k) if (k < 8) sum(dnbinom(k, size = m_nb$theta, mu = fitted(m_nb)))
                       else sum(1 - pnbinom(7, size = m_nb$theta, mu = fitted(m_nb)))))
raiz_nb |> dplyr::mutate(dplyr::across(c(Poisson, NB2), \(x) round(x, 1)))

# -----------------------------------------------------------------------------
# [fig-u23-nb-rootograma]
#   3 · Sobredispersión
#     > 3.4 Binomial negativa
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
raiz_nb |>
  tidyr::pivot_longer(c(Poisson, NB2), names_to = "modelo", values_to = "esperado") |>
  dplyr::mutate(modelo = factor(modelo, c("Poisson", "NB2"))) |>
  ggplot() +
  geom_rect(aes(xmin = k - 0.4, xmax = k + 0.4,
                ymin = sqrt(esperado) - sqrt(observado), ymax = sqrt(esperado)),
            fill = "steelblue", alpha = 0.7) +
  geom_line(aes(k, sqrt(esperado)), colour = "darkorange", linewidth = 0.8) +
  geom_point(aes(k, sqrt(esperado)), colour = "darkorange") +
  geom_hline(yintercept = 0) +
  facet_wrap(~ modelo) +
  scale_x_continuous(breaks = K, labels = c(0:7, "8+")) +
  labs(x = "nº de partes por daños", y = "raíz de la frecuencia")

# -----------------------------------------------------------------------------
# [u23-aic-comp]
#   3 · Sobredispersión
#     > 3.4 Binomial negativa
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
mods_lik <- list(Poisson = m_pois, NB1 = m_nb1, NB2 = m_nb)
data.frame(modelo = names(mods_lik),
           df  = sapply(mods_lik, function(m) attr(logLik(m), "df")),
           AIC = round(sapply(mods_lik, AIC), 1)) |>
  dplyr::arrange(AIC)

# -----------------------------------------------------------------------------
# [u23-lrt]
#   3 · Sobredispersión
#     > 3.4 Binomial negativa
#       > Selección, bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
LR <- 2 * (as.numeric(logLik(m_nb)) - as.numeric(logLik(m_pois)))
c(LR = round(LR, 1), p_valor = pchisq(LR, df = 1, lower.tail = FALSE) / 2)   # /2 por el borde

# -----------------------------------------------------------------------------
# [fig-u23-nb-pred]
#   3 · Sobredispersión > 3.4 Binomial negativa > Predicción
# -----------------------------------------------------------------------------
pred_nb <- dplyr::bind_rows(banda(m_pois, "Poisson"), banda(m_nb1, "NB1"), banda(m_nb, "NB2"))

ggplot(pred_nb, aes(edad_conductor, mu, colour = modelo, fill = modelo)) +
  geom_ribbon(aes(ymin = inf, ymax = sup), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Poisson" = "darkorange", "NB1" = "seagreen", "NB2" = "steelblue"),
                      aesthetics = c("colour", "fill")) +
  labs(x = "edad del conductor", y = "nº medio de partes al año", colour = NULL, fill = NULL)

# -----------------------------------------------------------------------------
# [u23-nb-pred-tabla]
#   3 · Sobredispersión > 3.4 Binomial negativa > Predicción
# -----------------------------------------------------------------------------
pred_nb |>
  dplyr::filter(edad_conductor %in% c(25, 45, 65)) |>
  dplyr::select(modelo, edad_conductor, mu, inf, sup)

# -----------------------------------------------------------------------------
# [u23-pred-sim]
#   3 · Sobredispersión > 3.4 Binomial negativa > Predicción
# -----------------------------------------------------------------------------
i   <- which.max(fitted(m_nb))                     # la póliza de mayor riesgo esperado
x_i <- model.matrix(m_nb)[i, ]
t_i <- cartera$exposicion[i]
B   <- 4000
k_pred <- 0:16
set.seed(SEMILLA_CURSO)

# 1. parámetros simulados; beta y theta por separado: en la NB2 son asintóticamente independientes
beta_nb  <- MASS::mvrnorm(B, coef(m_nb),   vcov(m_nb))
theta_b  <- rnorm(B, m_nb$theta, m_nb$SE.theta)
beta_poi <- MASS::mvrnorm(B, coef(m_pois), vcov(m_pois))

# 2. media de la póliza en cada réplica
mu_sim_nb  <- t_i * exp(drop(beta_nb  %*% x_i))
mu_sim_poi <- t_i * exp(drop(beta_poi %*% x_i))

# 3. probabilidad de cada conteo en cada réplica (filas: réplicas; columnas: k = 0, 1, ..., 16)
p_sim_nb  <- sapply(k_pred, \(k) dnbinom(k, size = theta_b, mu = mu_sim_nb))
p_sim_poi <- sapply(k_pred, \(k) dpois(k, mu_sim_poi))

# 4. un conteo por réplica: suma la incertidumbre de la estimación y la del propio conteo
y_sim_nb  <- rnbinom(B, size = theta_b, mu = mu_sim_nb)
y_sim_poi <- rpois(B, mu_sim_poi)

ic95  <- \(v) quantile(v, c(0.025, 0.975))
mu_nb <- unname(fitted(m_nb)[i]); mu_poi <- unname(fitted(m_pois)[i])
round(rbind(
  media_Poisson   = c(estimacion = mu_poi, ic95(mu_sim_poi)),
  media_NB2       = c(mu_nb, ic95(mu_sim_nb)),
  P0_Poisson      = c(dpois(0, mu_poi), ic95(p_sim_poi[, 1])),
  P0_NB2          = c(dnbinom(0, size = m_nb$theta, mu = mu_nb), ic95(p_sim_nb[, 1])),
  P_ge_10_Poisson = c(1 - ppois(9, mu_poi), ic95(1 - ppois(9, mu_sim_poi))),
  P_ge_10_NB2     = c(1 - pnbinom(9, size = m_nb$theta, mu = mu_nb),
                      ic95(1 - pnbinom(9, size = theta_b, mu = mu_sim_nb)))), 3)

# intervalo de predicción al 90 % para el número de partes de esta póliza
rbind(Poisson = quantile(y_sim_poi, c(0.05, 0.95)), NB2 = quantile(y_sim_nb, c(0.05, 0.95)))

# -----------------------------------------------------------------------------
# [fig-u23-pred-dist]
#   3 · Sobredispersión > 3.4 Binomial negativa > Predicción
# -----------------------------------------------------------------------------
dist_pred <- dplyr::bind_rows(
  tibble::tibble(modelo = "Poisson", k = k_pred, prob = dpois(k_pred, mu_poi),
                 inf = apply(p_sim_poi, 2, quantile, 0.025), sup = apply(p_sim_poi, 2, quantile, 0.975)),
  tibble::tibble(modelo = "NB2", k = k_pred, prob = dnbinom(k_pred, size = m_nb$theta, mu = mu_nb),
                 inf = apply(p_sim_nb, 2, quantile, 0.025), sup = apply(p_sim_nb, 2, quantile, 0.975)))

ggplot(dist_pred, aes(k, prob, colour = modelo)) +
  geom_line(linewidth = 0.6) +
  geom_pointrange(aes(ymin = inf, ymax = sup), size = 0.3) +
  scale_colour_manual(values = c("Poisson" = "darkorange", "NB2" = "steelblue")) +
  labs(x = "nº de partes", y = "probabilidad", colour = NULL)

# -----------------------------------------------------------------------------
# [u23-validacion]
#   3 · Sobredispersión
#     > 3.5 Conclusiones y validación
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(cartera, "verdad")
c(theta_DGP = verdad$theta_nb, theta_estimado = round(m_nb$theta, 3))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
