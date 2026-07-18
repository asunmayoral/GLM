# =============================================================================
# Caso 2 · Unidad 2.2 — 2 · Sobredispersión
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_2.qmd.
# Cada bloque va precedido de su LABEL y de la sección/subsección donde aparece.
#
# EJECUCIÓN: funciona desde CUALQUIER carpeta dentro del proyecto GLM; localiza
# la raíz por _quarto.yml y resuelve solo las rutas del DGP y de la caché.
# =============================================================================

# --- Librerías (idénticas al setup del documento del caso) -------------------
library(broom)
library(tidyverse)
library(MASS)          # glm.nb
library(pscl)          # hurdle / zeroinfl
library(glmmTMB)       # conteos mixtos / ceros
library(lme4)          # glmer (Poisson)
library(DHARMa); library(performance); library(marginaleffects)
library(survival)      # riesgos a trozos
library(MuMIn); library(glmnet)
library(vcdExtra)      # zero-inflated

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

# --- Datos: cartera de auto (misma llamada que el documento del caso) --------
# Localiza la raíz del proyecto (donde está _quarto.yml), sea cual sea el wd:
.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")
source(file.path(.raiz, "caso2", "R", "dgp_conteos.R"))  # simular_cartera(), cargar_cartera(), expandir_poliza_tramo()
cartera <- cargar_cartera("auto")    # lee datos/ si existe; si no (o si cambió el DGP), simula y cachea

# -----------------------------------------------------------------------------
# [fig-u22-eda-modelo]  ·  2.1 Qué es, de dónde viene y cómo se identifica > La huella: media frente a varianza
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
  labs(x = "nivel (cuartil, en los continuos)", y = "tasa de partes por daños (por año)")

# -----------------------------------------------------------------------------
# [fig-u22-media-varianza]  ·  2.1 Qué es, de dónde viene y cómo se identifica > La huella: media frente a varianza
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
    "Lineal: phi·media"    = "darkorange",
    "Cuadratica: media+a·media²" = "steelblue")) +
  labs(x = "media observada (por grupo)", y = "varianza observada", colour = NULL)

# -----------------------------------------------------------------------------
# [fig-u22-heterogeneidad]  ·  2.1 Qué es, de dónde viene y cómo se identifica > De dónde viene
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
# [u22-indice]  ·  2.1 Qué es, de dónde viene y cómo se identifica > El índice de dispersión
# -----------------------------------------------------------------------------
phi_pearson <- sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)
phi_dev     <- deviance(m_pois) / df.residual(m_pois)
c(pearson = phi_pearson, deviance = phi_dev)

# -----------------------------------------------------------------------------
# [fig-u22-rootograma]  ·  2.1 Qué es, de dónde viene y cómo se identifica > El rootograma: ver dónde falla el ajuste
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
# [u22-tests]  ·  2.1 Qué es, de dónde viene y cómo se identifica > Los contrastes: ¿hay sobredispersión y de qué tipo?
# -----------------------------------------------------------------------------
# Tests de sobredispersión
print(performance::check_overdispersion(m_pois))
print(AER::dispersiontest(m_pois))
print(DHARMa::testDispersion(m_pois, plot = FALSE))

# -----------------------------------------------------------------------------
# [u22-trafo]  ·  2.1 Qué es, de dónde viene y cómo se identifica > Los contrastes: ¿hay sobredispersión y de qué tipo?
# -----------------------------------------------------------------------------
print(AER::dispersiontest(m_pois, trafo = 1))   # Var = mu + alpha·mu    (lineal, NB1)
print(AER::dispersiontest(m_pois, trafo = 2))   # Var = mu + alpha·mu^2  (cuadratica, NB2)

# -----------------------------------------------------------------------------
# [u22-trafo-manual]  ·  2.1 Qué es, de dónde viene y cómo se identifica > Los contrastes: ¿hay sobredispersión y de qué tipo?
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
# [u22-quasi-fit]  ·  2.2 Quasi-Poisson > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_quasi <- glm(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                 offset(log(exposicion)), family = quasipoisson, data = cartera)
summary(m_quasi)$dispersion   # phi estimado

# -----------------------------------------------------------------------------
# [u22-quasi-comp]  ·  2.2 Quasi-Poisson > Ajuste e interpretación
# -----------------------------------------------------------------------------
dplyr::left_join(
  broom::tidy(m_pois)  |> dplyr::select(term, estimacion = estimate, se_poisson = std.error),
  broom::tidy(m_quasi) |> dplyr::select(term, se_quasi = std.error),
  by = "term")

# -----------------------------------------------------------------------------
# [fig-u22-quasi-resid]  ·  2.2 Quasi-Poisson > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
tibble::tibble(ajustado = fitted(m_quasi),
               residuo  = residuals(m_quasi, type = "pearson")) |>
  ggplot(aes(ajustado, residuo)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, colour = "darkorange") +
  labs(x = "valor ajustado", y = "residuo de Pearson")

# -----------------------------------------------------------------------------
# [u22-nb-fit]  ·  2.3 Binomial negativa > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_nb <- MASS::glm.nb(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                       offset(log(exposicion)), data = cartera)
c(theta = m_nb$theta, se_theta = m_nb$SE.theta)   # dispersion estimada

# -----------------------------------------------------------------------------
# [u22-nb1]  ·  2.3 Binomial negativa > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_nb1 <- glmmTMB::glmmTMB(n_danos ~ edad_conductor + potencia_cv + zona_circulacion + uso +
                            tipo_vehiculo + offset(log(exposicion)),
                          family = glmmTMB::nbinom1, data = cartera)
sigma(m_nb1)   # dispersion alpha de la NB1

# -----------------------------------------------------------------------------
# [u22-nb1-quasi]  ·  2.3 Binomial negativa > Ajuste e interpretación
# -----------------------------------------------------------------------------
se_q   <- summary(m_quasi)$coefficients[, "Std. Error"]
se_nb1 <- summary(m_nb1)$coefficients$cond[, "Std. Error"]
data.frame(term = names(se_q), se_quasi = round(se_q, 4),
           se_nb1 = round(se_nb1[names(se_q)], 4))

# -----------------------------------------------------------------------------
# [u22-nb-disp]  ·  2.3 Binomial negativa > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
disp <- function(m) sum(residuals(m, type = "pearson")^2) / df.residual(m)
round(c(NB2 = disp(m_nb), NB1 = disp(m_nb1)), 3)

# -----------------------------------------------------------------------------
# [fig-u22-nb2-dharma]  ·  2.3 Binomial negativa > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::simulateResiduals(m_nb, plot = TRUE)

# -----------------------------------------------------------------------------
# [fig-u22-nb1-dharma]  ·  2.3 Binomial negativa > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::simulateResiduals(m_nb1, plot = TRUE)

# -----------------------------------------------------------------------------
# [fig-u22-nb-rootograma]  ·  2.3 Binomial negativa > Bondad de ajuste y diagnóstico
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
# [fig-u22-comp-modelos]  ·  2.4 Elegir, comparar y conectar > Los cuatro modelos, lado a lado
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
# [u22-aic-comp]  ·  2.4 Elegir, comparar y conectar > La decisión formal: AIC y LRT
# -----------------------------------------------------------------------------
mods_lik <- list(Poisson = m_pois, NB1 = m_nb1, NB2 = m_nb)
data.frame(modelo = names(mods_lik),
           df  = sapply(mods_lik, function(m) attr(logLik(m), "df")),
           AIC = round(sapply(mods_lik, AIC), 1)) |>
  dplyr::arrange(AIC)

# -----------------------------------------------------------------------------
# [u22-lrt]  ·  2.4 Elegir, comparar y conectar > La decisión formal: AIC y LRT
# -----------------------------------------------------------------------------
LR <- 2 * (as.numeric(logLik(m_nb)) - as.numeric(logLik(m_pois)))
c(LR = round(LR, 1), p_valor = pchisq(LR, df = 1, lower.tail = FALSE) / 2)   # /2 por el borde

# -----------------------------------------------------------------------------
# [u22-perf]  ·  2.4 Elegir, comparar y conectar > Una lectura estándar de bondad de ajuste
# -----------------------------------------------------------------------------
performance::compare_performance(Poisson = m_pois, NB1 = m_nb1, NB2 = m_nb,
                                 metrics = c("AIC", "BIC", "RMSE"))

# -----------------------------------------------------------------------------
# [u22-pred-comp]  ·  2.4 Elegir, comparar y conectar > Predicciones: misma media, distinto riesgo
# -----------------------------------------------------------------------------
i    <- which.max(fitted(m_nb))                          # la póliza de mayor riesgo esperado
mu_p <- unname(predict(m_pois, cartera[i, ], type = "response"))   # media predicha (Poisson)
mu_n <- unname(predict(m_nb,   cartera[i, ], type = "response"))   # media predicha (NB2)
round(c(media_pois = mu_p, media_nb = mu_n,                        # casi iguales
        P0_pois    = dpois(0, mu_p),        P0_nb    = dnbinom(0, size = m_nb$theta, mu = mu_n),      # P(Y = 0)
        Pge10_pois = 1 - ppois(9, mu_p),    Pge10_nb = 1 - pnbinom(9, size = m_nb$theta, mu = mu_n)), # P(Y >= 10)
      3)

# -----------------------------------------------------------------------------
# [fig-u22-pred-dist]  ·  2.4 Elegir, comparar y conectar > Predicciones: misma media, distinto riesgo
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
# [u22-validacion]  ·  2.4 Elegir, comparar y conectar > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(cartera, "verdad")
c(theta_DGP = verdad$theta_nb, theta_estimado = round(m_nb$theta, 3))
