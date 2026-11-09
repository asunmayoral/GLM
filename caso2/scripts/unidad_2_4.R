# =============================================================================
# Caso 2 · Unidad 2.4 — 4 · Demasiados ceros
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_4.qmd.
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
  broom.mixed = "0.2.9.7", dplyr = "1.2.1", forcats = "1.0.1",
  ggplot2 = "4.0.3", glmmTMB = "1.1.14", glmnet = "5.0",
  marginaleffects = "0.32.0", performance = "0.17.0", purrr = "1.2.2",
  sessioninfo = "1.2.4", tibble = "3.3.1", tidyr = "1.3.2",
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
# [u24-fraude-ceros]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > La huella: más ceros de los que el modelo espera
# -----------------------------------------------------------------------------
m_fp <- glm(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)),
            family = poisson, data = cartera)
obs0 <- mean(cartera$n_fraude == 0)     # proporción observada de ceros
esp0 <- mean(dpois(0, fitted(m_fp)))    # proporción esperada por la Poisson
c(observados = obs0, esperados_poisson = esp0, ratio = obs0 / esp0)

# -----------------------------------------------------------------------------
# [fig-u24-ceros]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > La huella: más ceros de los que el modelo espera
# -----------------------------------------------------------------------------
mu <- fitted(m_fp); K <- 0:8
esperado  <- sapply(K, function(k) if (k < 8) sum(dpois(k, mu)) else sum(1 - ppois(7, mu)))
observado <- as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K)))

tibble::tibble(k = K, Observado = observado, `Poisson ajustada` = esperado) |>
  tidyr::pivot_longer(c(Observado, `Poisson ajustada`), names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "Poisson ajustada" = "darkorange")) +
  labs(x = "nº de alertas antifraude", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [fig-u24-mecanismo]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > De dónde viene: dos procesos de cero
# -----------------------------------------------------------------------------
set.seed(2026)
n <- 5000; mu <- 2; pi_e <- 0.4
dplyr::bind_rows(
  tibble::tibble(poblacion = "Poisson",                       y = rpois(n, mu)),
  tibble::tibble(poblacion = "Con 40 % de ceros estructurales", y = ifelse(rbinom(n, 1, pi_e) == 1, 0, rpois(n, mu)))) |>
  dplyr::filter(y <= 8) |>
  dplyr::count(poblacion, y) |>
  ggplot(aes(y, n, fill = poblacion)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Poisson" = "steelblue", "Con 40 % de ceros estructurales" = "darkorange")) +
  labs(x = "nº de eventos", y = "frecuencia", fill = NULL)

# -----------------------------------------------------------------------------
# [u24-nb-zerotest]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > Cómo se identifica
# -----------------------------------------------------------------------------
m_fnb <- MASS::glm.nb(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                        offset(log(exposicion)), data = cartera)
set.seed(2024)   # el p-valor sale de simular: fijamos la semilla para que se reproduzca
sim_fnb <- DHARMa::simulateResiduals(m_fnb, n = 1000)
zt_fnb  <- DHARMa::testZeroInflation(sim_fnb, plot = FALSE)   # ¿aún sobran ceros tras la NB?
zt_fnb
n0_obs <- sum(cartera$n_fraude == 0)
n0_sim <- colSums(sim_fnb$simulatedResponse == 0)   # ceros de cada una de las 1000 simulaciones
c(observados = n0_obs, media_sim = mean(n0_sim), dt_sim = sd(n0_sim),
  sims_que_alcanzan = sum(n0_sim >= n0_obs))

# -----------------------------------------------------------------------------
# [fig-u24-nb-zerodist]
#   4 · Demasiados ceros
#     > 4.1 Qué es, de dónde viene y cómo se identifica
#       > Cómo se identifica
# -----------------------------------------------------------------------------
ggplot(tibble::tibble(n0 = n0_sim), aes(n0)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = n0_obs, linetype = "dashed", linewidth = 0.8) +
  labs(x = "nº de pólizas con cero alertas (simulaciones bajo la NB)", y = "nº de simulaciones")

# -----------------------------------------------------------------------------
# [u24-positivos]
#   4 · Demasiados ceros
#     > 4.2 Estructuras para los ceros
#       > Pregunta 1 · ¿Poisson o NB para el conteo?
# -----------------------------------------------------------------------------
# solo las pólizas con alguna alerta: ¿su conteo es Poisson (truncada en 0) o va más disperso?
pos   <- subset(cartera, n_fraude > 0)
m_ztp <- glmmTMB::glmmTMB(n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo +
                            offset(log(exposicion)),
                          family = glmmTMB::truncated_poisson(), data = pos)
lam  <- exp(predict(m_ztp, type = "link"))   # media de la Poisson SIN truncar
m_tr <- lam / (1 - exp(-lam))                # media de la Poisson truncada en 0
v_tr <- m_tr * (1 + lam - m_tr)              # y su varianza
disp_pos <- sum((pos$n_fraude - m_tr)^2 / v_tr) / df.residual(m_ztp)
disp_pos

# -----------------------------------------------------------------------------
# [u24-deflacion]
#   4 · Demasiados ceros
#     > 4.2 Estructuras para los ceros
#       > Pregunta 2 · ¿Mezcla o barrera?
# -----------------------------------------------------------------------------
ceros_grupo <- tibble::tibble(
  grupo      = dplyr::ntile(fitted(m_fp), 12),   # 12 grupos de riesgo esperado parecido
  observados = cartera$n_fraude == 0,
  esperados  = dpois(0, fitted(m_fp))) |>        # P(Y = 0) de cada póliza según la Poisson
  dplyr::group_by(grupo) |>
  dplyr::summarise(dplyr::across(c(observados, esperados), mean)) |>
  dplyr::mutate(diferencia = observados - esperados)
ceros_grupo

# -----------------------------------------------------------------------------
# [u24-zip-explora]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Exploración: qué predictor va en cada parte
# -----------------------------------------------------------------------------
d_exp <- dplyr::mutate(cartera, potencia_cv_tercil = ggplot2::cut_number(potencia_cv, 3))
explora <- function(var) {
  d <- dplyr::mutate(d_exp, predictor_cat = factor(.data[[var]]))
  # 1 · ritmo de las vigiladas: Poisson truncada en 0 sobre las pólizas con alguna alerta, un ritmo por nivel
  m_tr  <- glmmTMB::glmmTMB(n_fraude ~ 0 + predictor_cat + offset(log(exposicion)),
                            family = glmmTMB::truncated_poisson(), data = subset(d, n_fraude > 0))
  # fixef(m_tr)$cond: vector con nombres de los coeficientes de la fórmula (escala log), uno por nivel;
  # exponenciados, el ritmo de cada nivel en alertas por unidad de exposición
  ritmo <- exp(glmmTMB::fixef(m_tr)$cond)
  d |>
    dplyr::mutate(ritmo = ritmo[as.integer(predictor_cat)]) |>
    dplyr::group_by(predictor_cat) |>
    dplyr::summarise(tasa_bruta   = sum(n_fraude) / sum(exposicion),
                     ceros_obs    = mean(n_fraude == 0),                  # 2 · ceros observados
                     ceros_conteo = mean(exp(-ritmo * exposicion)),       # 3 · ceros que daría el conteo
                     ritmo        = dplyr::first(ritmo), .groups = "drop") |>
    dplyr::transmute(predictor = var, nivel = as.character(predictor_cat), tasa_bruta, ceros_obs,
                     ceros_conteo, ritmo,
                     pi_est = (ceros_obs - ceros_conteo) / (1 - ceros_conteo))   # 4 · ceros estructurales
}
explora_zip <- purrr::map_dfr(c("potencia_cv_tercil", "uso", "zona_circulacion", "tipo_vehiculo"),
                              explora)
# para mostrarla: predictor y nivel en una columna, y cifras redondeadas a 5 decimales
explora_zip |>
  dplyr::transmute(predictor_nivel = paste(predictor, nivel, sep = "-"),
                   dplyr::across(c(tasa_bruta, ceros_obs, ceros_conteo, ritmo, pi_est), \(x) round(x, 5)))

# -----------------------------------------------------------------------------
# [fig-u24-zip-explora]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Exploración: qué predictor va en cada parte
# -----------------------------------------------------------------------------
explora_zip |>
  tidyr::pivot_longer(c(pi_est, ritmo), names_to = "parte", values_to = "valor") |>
  dplyr::mutate(parte = factor(parte, levels = c("pi_est", "ritmo"),
                               labels = c("Estructural:\nceros estructurales (pi)",
                                          "Conteo:\nritmo de las vigiladas")),
                # orden de la tabla (el natural de cada predictor), no el alfabético
                nivel = forcats::fct_inorder(nivel)) |>
  ggplot(aes(nivel, valor, group = 1)) +
  geom_line(colour = "grey60") +
  geom_point(size = 2.5, colour = "steelblue") +
  facet_grid(parte ~ predictor, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text.y = element_text(size = 9))   # títulos de fila en dos líneas, para que no se corten

# -----------------------------------------------------------------------------
# [u24-zip]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zip <- glmmTMB::glmmTMB(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)),
  ziformula = ~ uso + potencia_cv,
  family    = poisson, data = cartera)
summary(m_zip)

# -----------------------------------------------------------------------------
# [u24-zip-or]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
# efectos exponenciados con su IC de Wald al 95 %, sin los interceptos
tabla_exp <- function(m, etiqueta_ceros) {
  broom.mixed::tidy(m, exponentiate = TRUE, conf.int = TRUE) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::transmute(parte      = ifelse(component == "cond", "conteo: IRR", etiqueta_ceros),
                     termino    = term,
                     estimacion = round(estimate, 3),
                     ic_inf     = round(conf.low, 3),
                     ic_sup     = round(conf.high, 3))
}
or_zip <- tabla_exp(m_zip, etiqueta_ceros = "ceros: OR de estar fuera del perímetro")
or_zip

# -----------------------------------------------------------------------------
# [u24-zip-edad]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
# update() cambia solo la fórmula principal (el conteo); ziformula se mantiene
m_zip_edad <- update(m_zip, . ~ . + edad_conductor + antiguedad_carnet)
anova(m_zip, m_zip_edad)   # H0: edad y carnet no aportan a la parte de conteo

# -----------------------------------------------------------------------------
# [u24-zinb]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_zinb <- update(m_zip, family = glmmTMB::nbinom2)
AIC(m_zip, m_zinb)

# -----------------------------------------------------------------------------
# [u24-zip-diag]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
# P(Y_i = k) del ZIP para cada póliza (filas) y cada k de K (columnas)
prob_zip <- function(m, newdata = NULL, K = 0:8) {
  pi_i <- predict(m, newdata, type = "zprob")                # probabilidad de cero estructural
  mu_i <- exp(predict(m, newdata, type = "link"))            # media del conteo, con el offset
  P <- (1 - pi_i) * outer(mu_i, K, \(mu, k) dpois(k, mu))    # (1 - pi) f(k)
  P[, K == 0] <- P[, K == 0] + pi_i                          # el cero suma además pi
  P
}
c(observados    = sum(cartera$n_fraude == 0),
  esperados_zip = round(sum(prob_zip(m_zip, K = 0))))

# -----------------------------------------------------------------------------
# [fig-u24-zip-root]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
P <- prob_zip(m_zip, K = 0:7); K <- 0:8
tibble::tibble(k = K,
               Observado      = as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K))),
               `ZIP ajustado` = c(colSums(P), nrow(P) - sum(P))) |>   # la última celda es «8 o más»
  tidyr::pivot_longer(-k, names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "ZIP ajustado" = "darkorange")) +
  labs(x = "nº de alertas", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [fig-u24-zip-dharma]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
set.seed(2025)   # la simulación es aleatoria: fijamos la semilla para que el diagnóstico se reproduzca
res_zip <- DHARMa::simulateResiduals(m_zip, n = 1000)
plot(res_zip)

# -----------------------------------------------------------------------------
# [u24-zip-dharma-tests]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
contrastes_zip <- list(
  uniformidad     = DHARMa::testUniformity(res_zip, plot = FALSE),
  dispersion      = DHARMa::testDispersion(res_zip, plot = FALSE),
  exceso_de_ceros = DHARMa::testZeroInflation(res_zip, plot = FALSE))
tibble::tibble(contraste   = names(contrastes_zip),
               estadistico = sapply(contrastes_zip, \(t) unname(t$statistic)),
               p_valor     = sapply(contrastes_zip, \(t) t$p.value))

# -----------------------------------------------------------------------------
# [fig-u24-zip-dharma-pred]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
par(mfrow = c(1, 2))   # los dos paneles en una sola figura
DHARMa::plotResiduals(res_zip, cartera$uso, xlab = "uso")
DHARMa::plotResiduals(res_zip, cartera$potencia_cv, xlab = "potencia (CV)")

# -----------------------------------------------------------------------------
# [u24-zip-chi2-boot]
#   4 · Demasiados ceros
#     > 4.3 Modelos zero-inflated (ZIP / ZINB)
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
frec_chi2 <- function(m, y, K = 8) {   # celdas 0, 1, ..., K-1 y "K o más"
  P <- prob_zip(m, K = 0:(K - 1))      # P(Y_i = k) de cada póliza
  E <- c(colSums(P), length(y) - sum(P))
  O <- c(tabulate(y + 1, nbins = K), sum(y >= K))
  sum((O - E)^2 / E)
}
X2_obs <- frec_chi2(m_zip, cartera$n_fraude)

set.seed(2025)
y_boot  <- simulate(m_zip, nsim = 200)   # 200 carteras simuladas desde el ZIP
X2_boot <- vapply(y_boot, function(ys)
  frec_chi2(update(m_zip, data = transform(cartera, n_fraude = ys)), ys), numeric(1))

c(X2 = X2_obs,
  p_bootstrap = mean(X2_boot >= X2_obs),
  p_chi2_ingenuo = pchisq(X2_obs, df = 8, lower.tail = FALSE))   # 9 celdas - 1, sin más ajuste

# -----------------------------------------------------------------------------
# [u24-hurdle-explora]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Exploración: qué predictor va en cada parte
# -----------------------------------------------------------------------------
explora_zip |>
  dplyr::transmute(predictor_nivel = paste(predictor, nivel, sep = "-"),
                   ceros_obs = round(ceros_obs, 5), ritmo = round(ritmo, 5))

# -----------------------------------------------------------------------------
# [u24-hurdle]
#   4 · Demasiados ceros > 4.4 Modelos hurdle > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_hp <- glmmTMB::glmmTMB(
  n_fraude ~ potencia_cv + zona_circulacion + uso + tipo_vehiculo + offset(log(exposicion)),
  ziformula = ~ uso + potencia_cv,
  family    = glmmTMB::truncated_poisson(), data = cartera)
summary(m_hp)
tabla_exp(m_hp, etiqueta_ceros = "barrera: OR de no tener ninguna alerta")   # efectos exponenciados e IC 95 %

# -----------------------------------------------------------------------------
# [u24-hurdle-nb]
#   4 · Demasiados ceros > 4.4 Modelos hurdle > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_hnb <- update(m_hp, family = glmmTMB::truncated_nbinom2)
aic_h <- AIC(m_hp, m_hnb)
aic_h

# -----------------------------------------------------------------------------
# [u24-hurdle-diag]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
# P(Y_i = k) del hurdle-Poisson para cada póliza (filas) y cada k de K (columnas)
prob_hurdle <- function(m, newdata = NULL, K = 0:8) {
  pi_i <- predict(m, newdata, type = "zprob")                # probabilidad de no cruzar la barrera
  mu_i <- exp(predict(m, newdata, type = "link"))            # media de la Poisson SIN truncar
  P <- (1 - pi_i) * outer(mu_i, K, \(mu, k) dpois(k, mu) / (1 - exp(-mu)))   # conteo truncado
  P[, K == 0] <- pi_i                                        # el cero sale solo de la barrera
  P
}
c(observados       = sum(cartera$n_fraude == 0),
  esperados_hurdle = round(sum(prob_hurdle(m_hp, K = 0))))   # coinciden por construcción

# -----------------------------------------------------------------------------
# [fig-u24-hurdle-root]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
P <- prob_hurdle(m_hp, K = 0:7); K <- 0:8
tibble::tibble(k = K,
               Observado         = as.numeric(table(factor(pmin(cartera$n_fraude, 8), levels = K))),
               `Hurdle ajustado` = c(colSums(P), nrow(P) - sum(P))) |>   # la última celda es «8 o más»
  tidyr::pivot_longer(-k, names_to = "fuente", values_to = "frec") |>
  ggplot(aes(k, frec, fill = fuente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Observado" = "steelblue", "Hurdle ajustado" = "darkorange")) +
  labs(x = "nº de alertas", y = "nº de pólizas", fill = NULL)

# -----------------------------------------------------------------------------
# [fig-u24-hurdle-dharma]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
set.seed(2025)   # la simulación es aleatoria: fijamos la semilla para que el diagnóstico se reproduzca
res_hp <- DHARMa::simulateResiduals(m_hp, n = 1000)
plot(res_hp)

# -----------------------------------------------------------------------------
# [u24-hurdle-dharma-tests]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
contrastes_hp <- list(
  uniformidad = DHARMa::testUniformity(res_hp, plot = FALSE),
  dispersion  = DHARMa::testDispersion(res_hp, plot = FALSE))
tibble::tibble(contraste = names(contrastes_hp),
               p_valor   = sapply(contrastes_hp, \(t) t$p.value))

# -----------------------------------------------------------------------------
# [u24-hurdle-res-exposicion]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
# mediana de los residuos simulados por cuartil de exposición: debería rondar 0.5 en todos
tapply(residuals(res_hp), ggplot2::cut_number(cartera$exposicion, 4), median)

# -----------------------------------------------------------------------------
# [fig-u24-hurdle-dharma-exp]
#   4 · Demasiados ceros
#     > 4.4 Modelos hurdle
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
DHARMa::plotResiduals(res_hp, cartera$exposicion, xlab = "exposición")

# -----------------------------------------------------------------------------
# [u24-comparacion]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Los seis modelos, lado a lado
# -----------------------------------------------------------------------------
mods <- list(Poisson = m_fp, NB = m_fnb, ZIP = m_zip, ZINB = m_zinb,
             `Hurdle-P` = m_hp, `Hurdle-NB` = m_hnb)
comparacion <- data.frame(
  modelo = names(mods),
  df     = sapply(mods, function(m) attr(logLik(m), "df")),
  AIC    = sapply(mods, AIC),
  BIC    = sapply(mods, BIC),
  RMSE   = sapply(mods, function(m) sqrt(mean((cartera$n_fraude - fitted(m))^2)))) |>
  dplyr::mutate(peso_AIC = exp(-(AIC - min(AIC)) / 2) / sum(exp(-(AIC - min(AIC)) / 2))) |>
  dplyr::arrange(AIC)
comparacion

# -----------------------------------------------------------------------------
# [u24-pred-tabla]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Predicción: la misma media, distinta distribución
# -----------------------------------------------------------------------------
perfiles_u24 <- data.frame(perfil = c("particular, 80 CV", "comercial, 160 CV"),
                           potencia_cv = c(80, 160), uso = c("particular", "comercial"),
                           zona_circulacion = "urbana", tipo_vehiculo = "turismo",
                           exposicion = 1)
K <- 0:10
# P(Y = k) de cada perfil según cada modelo: una fila por perfil, una columna por k
prob_modelo <- list(
  Poisson    = t(sapply(predict(m_fp,  perfiles_u24, type = "response"), \(m) dpois(K, m))),
  NB         = t(sapply(predict(m_fnb, perfiles_u24, type = "response"),
                        \(m) dnbinom(K, size = m_fnb$theta, mu = m))),
  ZIP        = prob_zip(m_zip, perfiles_u24, K = K),
  `Hurdle-P` = prob_hurdle(m_hp, perfiles_u24, K = K))
pred_u24 <- purrr::imap_dfr(prob_modelo, \(P, modelo)
  tibble::tibble(modelo = modelo, perfil = rep(perfiles_u24$perfil, times = length(K)),
                 k = rep(K, each = nrow(perfiles_u24)), prob = as.vector(P)))
# la media, con predict(): sumar k * P(Y = k) hasta k = 10 la infraestimaría (cola de la NB)
medias_pred <- sapply(list(Poisson = m_fp, NB = m_fnb, ZIP = m_zip, `Hurdle-P` = m_hp),
                      \(m) predict(m, perfiles_u24, type = "response"))
resumen_pred <- pred_u24 |>
  dplyr::filter(k == 0) |>
  dplyr::transmute(perfil, modelo, P_cero = prob) |>
  dplyr::mutate(media = medias_pred[cbind(match(perfil, perfiles_u24$perfil),
                                          match(modelo, colnames(medias_pred)))])
resumen_pred
p0_pred <- function(m, p = perfiles_u24$perfil[1])   # P(Y = 0) de un perfil según un modelo
  resumen_pred$P_cero[resumen_pred$modelo == m & resumen_pred$perfil == p]

# -----------------------------------------------------------------------------
# [fig-u24-pred-dist]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Predicción: la misma media, distinta distribución
# -----------------------------------------------------------------------------
pred_u24 |>
  dplyr::mutate(modelo = factor(modelo, levels = names(prob_modelo))) |>
  ggplot(aes(k, prob, fill = modelo)) +
  geom_col(position = "dodge") +
  facet_wrap(~ perfil) +
  scale_x_continuous(breaks = K) +
  labs(x = "nº de alertas", y = "probabilidad predicha", fill = NULL)

# -----------------------------------------------------------------------------
# [u24-validacion]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
v <- attr(cartera, "verdad")
b_conteo <- setNames(v$betas$ceros, v$nombres_beta)   # parte de conteo del DGP
verdad_u24 <- tibble::tibble(
  component = c(rep("cond", 7), rep("zi", 3)),
  term = c("(Intercept)", "z_pot", "zona_circulacionurbana", "zona_circulacionmixta",
           "usocomercial", "tipo_vehiculomoto", "tipo_vehiculofurgoneta",
           "(Intercept)", "usocomercial", "z_pot"),
  dgp  = unname(c(b_conteo[c("(int)", "z_pot", "urbana", "mixta", "comercial", "moto", "furgoneta")],
                  v$g_cero[c("int", "b_uso", "b_pot")])))

# el mismo ZIP en la escala del DGP: potencia tipificada y zona rural como referencia
cartera_dgp <- dplyr::mutate(cartera, z_pot = as.numeric(scale(potencia_cv)),
                             zona_circulacion = relevel(zona_circulacion, ref = "rural"))
m_zip_dgp <- update(m_zip, . ~ . - potencia_cv + z_pot, ziformula = ~ uso + z_pot,
                    data = cartera_dgp)
c(logLik_zip = logLik(m_zip), logLik_zip_dgp = logLik(m_zip_dgp))   # mismo modelo

validacion_u24 <- broom.mixed::tidy(m_zip_dgp, conf.int = TRUE) |>
  dplyr::inner_join(verdad_u24, by = c("component", "term")) |>
  dplyr::transmute(parte      = ifelse(component == "cond", "conteo (log)", "estructural (logit)"),
                   termino    = term,
                   dgp        = round(dgp, 3),
                   estimacion = round(estimate, 3),
                   ic_inf     = round(conf.low, 3),
                   ic_sup     = round(conf.high, 3),
                   contiene   = conf.low <= dgp & dgp <= conf.high)   # ¿el IC 95 % cubre la verdad?
validacion_u24

# proporción media de pólizas fuera del perímetro: la del DGP y la que estima el ZIP
pi_dgp <- plogis(v$g_cero[["int"]] + v$g_cero[["b_uso"]] * (cartera$uso == "comercial") +
                   v$g_cero[["b_pot"]] * cartera_dgp$z_pot)
c(dgp = mean(pi_dgp), zip = mean(predict(m_zip, type = "zprob")))

# -----------------------------------------------------------------------------
# [u24-validacion-fila]
#   4 · Demasiados ceros
#     > 4.5 Elegir, comparar y conectar
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
val <- function(parte, termino)   # una fila de la tabla de validación, para citarla en el texto
  validacion_u24[validacion_u24$parte == parte & validacion_u24$termino == termino, ]


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
