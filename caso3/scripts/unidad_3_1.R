# =============================================================================
# Caso 3 · Unidad 3.1 — 1 · GLM Gamma
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_1.qmd.
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
# [u31-datos]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
base <- banco$averias

c(n = nrow(base),
  media   = mean(base$coste_euros),
  mediana = median(base$coste_euros),
  minimo  = min(base$coste_euros),
  maximo  = max(base$coste_euros)) |> round(1)

# -----------------------------------------------------------------------------
# [fig-u31-asimetria]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
p1 <- ggplot(base, aes(coste_euros)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  labs(x = "coste de la avería (€)", y = "nº de averías")

p2 <- ggplot(base, aes(coste_euros)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  scale_x_log10() +
  labs(x = "coste (€, escala log)", y = NULL)

p1 + p2

# -----------------------------------------------------------------------------
# [u31-ols-falla]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
ols   <- lm(coste_euros ~ tipo_averia + proceso + criticidad + antiguedad_anios + carga, data = base)
sigma <- summary(ols)$sigma

c(prediccion_minima    = round(min(fitted(ols))),
  ip95_inferior_minimo = round(min(fitted(ols) - 1.96 * sigma)),
  pct_ip95_bajo_cero   = round(mean(fitted(ols) - 1.96 * sigma < 0) * 100),
  prob_media_coste_neg = round(mean(pnorm(0, fitted(ols), sigma)) * 100, 1))

# -----------------------------------------------------------------------------
# [fig-u31-heterocedasticidad]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
d_ols <- tibble::tibble(ajustado = fitted(ols), residuo = resid(ols))

q1 <- ggplot(d_ols, aes(ajustado, residuo)) +
  geom_point(alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, colour = "firebrick") +
  labs(x = "valor ajustado (€)", y = "residuo")

q2 <- ggplot(d_ols, aes(sample = residuo)) +
  stat_qq(alpha = 0.25) + stat_qq_line(colour = "firebrick") +
  labs(x = "cuantiles teóricos", y = "cuantiles muestrales")

q1 + q2

# -----------------------------------------------------------------------------
# [tbl-u31-cv]
#   1 · GLM Gamma > 1.1 Contexto: qué modela
# -----------------------------------------------------------------------------
cv_tramos <- base |>   # se guarda: la §1.2 cita su rango de CV
  dplyr::mutate(tramo = dplyr::ntile(fitted(ols), 5)) |>
  dplyr::group_by(tramo) |>
  dplyr::summarise(n = dplyr::n(),
                   media = mean(coste_euros), sd = sd(coste_euros),
                   cv = sd / media, .groups = "drop") |>
  dplyr::mutate(dplyr::across(c(media, sd), \(x) round(x, 1)), cv = round(cv, 3))
cv_tramos

# -----------------------------------------------------------------------------
# [fig-u31-gammas]
#   1 · GLM Gamma > 1.2 Gamma en la familia exponencial
# -----------------------------------------------------------------------------
media_c <- mean(base$coste_euros)
cv_c    <- sd(base$coste_euros) / media_c
alfas   <- c(1, 1 / cv_c^2, 4, 10)      # misma media: la escala de cada una es media / alfa
etiq    <- paste0("α = ", round(alfas, 2), c(" (exponencial)", " (momentos)", "", ""))
rejilla <- seq(1, max(base$coste_euros), length.out = 400)

curvas <- purrr::map2_dfr(alfas, etiq, \(a, e) tibble::tibble(
  y = rejilla, forma = e,
  densidad = dgamma(rejilla, shape = a, scale = media_c / a),
  cola     = pgamma(rejilla, shape = a, scale = media_c / a, lower.tail = FALSE)))
curvas$forma <- factor(curvas$forma, levels = etiq)

# Cola observada: proporción de averías que superan cada coste
obs <- tibble::tibble(y = sort(base$coste_euros),
                      cola = 1 - (seq_along(y) - 0.5) / length(y))
p99 <- quantile(base$coste_euros, 0.99)

p_dens <- ggplot() +
  geom_histogram(data = subset(base, coste_euros <= p99),
                 aes(coste_euros, after_stat(density)), bins = 50, fill = "grey85") +
  geom_line(data = subset(curvas, y <= p99), aes(y, densidad, colour = forma), linewidth = 0.8) +
  labs(x = "coste de la avería (€)", y = "densidad", colour = NULL)

p_cola <- ggplot() +
  geom_point(data = obs, aes(y, cola), size = 0.6, colour = "grey40") +
  geom_line(data = curvas, aes(y, cola, colour = forma), linewidth = 0.8) +
  scale_y_log10(limits = c(1e-4, 1)) +
  labs(x = "coste de la avería (€)", y = "P(Y > y), escala log", colour = NULL)

(p_dens + p_cola) + patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# [fig-u31-cat]
#   1 · GLM Gamma > 1.3 ¿De qué depende el coste?
# -----------------------------------------------------------------------------
base |>
  dplyr::select(coste_euros, tipo_averia, proceso, criticidad) |>
  tidyr::pivot_longer(-coste_euros, names_to = "predictor", values_to = "nivel") |>
  ggplot(aes(nivel, coste_euros)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~ predictor, scales = "free_x", nrow = 1) +
  scale_y_log10() +
  labs(x = NULL, y = "coste (€, escala log)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# -----------------------------------------------------------------------------
# [fig-u31-cont]
#   1 · GLM Gamma > 1.3 ¿De qué depende el coste?
# -----------------------------------------------------------------------------
base |>
  dplyr::select(coste_euros, criticidad, antiguedad_anios, carga) |>
  tidyr::pivot_longer(c(antiguedad_anios, carga), names_to = "predictor", values_to = "valor") |>
  ggplot(aes(valor, coste_euros, colour = criticidad)) +
  geom_point(alpha = 0.20, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ predictor, scales = "free_x") +
  scale_y_log10() +
  labs(x = NULL, y = "coste (€, escala log)", colour = NULL) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# [fig-u31-interaccion-eda]
#   1 · GLM Gamma > 1.3 ¿De qué depende el coste?
# -----------------------------------------------------------------------------
ggplot(base, aes(proceso, coste_euros, fill = criticidad)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  labs(x = NULL, y = "coste (€, escala log)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# -----------------------------------------------------------------------------
# [u31-fase]
#   1 · GLM Gamma > 1.3 ¿De qué depende el coste?
# -----------------------------------------------------------------------------
base <- base |>
  dplyr::mutate(fase_proceso = factor(
    ifelse(proceso %in% c("Ensamblaje", "Acabado"), "fase2", "fase1"),
    levels = c("fase1", "fase2")))

# -----------------------------------------------------------------------------
# [u31-ajuste]
#   1 · GLM Gamma > 1.4 Modelización y estimación > El modelo del caso
# -----------------------------------------------------------------------------
m_gamma <- glm(coste_euros ~ tipo_averia + criticidad * fase_proceso + antiguedad_anios + carga,
               family = Gamma(link = "log"), data = base)

# -----------------------------------------------------------------------------
# [u31-dispersion]
#   1 · GLM Gamma > 1.5 Interpretación > La dispersión $\phi$
# -----------------------------------------------------------------------------
c(phi_pearson  = summary(m_gamma)$dispersion,
  cv_implicado = sqrt(summary(m_gamma)$dispersion)) |> round(3)

# -----------------------------------------------------------------------------
# [u31-cifras-efectos]
#   1 · GLM Gamma > 1.5 Interpretación > Los efectos de las variables
# -----------------------------------------------------------------------------
# Coeficientes, su lectura en % y sus contrastes de Wald: alimentan las cifras en línea de esta sección
bg  <- coef(m_gamma)
pct <- function(b) round(100 * (exp(b) - 1))   # incremento exacto de la media, en %
wald <- summary(m_gamma)$coefficients   # contrastes de Wald: el texto cita sus valores

# -----------------------------------------------------------------------------
# [tbl-u31-inferencia]
#   1 · GLM Gamma > 1.5 Interpretación > Los efectos de las variables
# -----------------------------------------------------------------------------
broom::tidy(m_gamma, conf.int = TRUE) |>
  dplyr::transmute(term,
                   beta = round(estimate, 3), ee = round(std.error, 3),
                   t = round(statistic, 2), p = signif(p.value, 3),
                   `exp(beta)` = round(exp(estimate), 3),
                   ic95 = paste0("[", round(exp(conf.low), 2), ", ", round(exp(conf.high), 2), "]"))

# -----------------------------------------------------------------------------
# [tbl-u31-interaccion]
#   1 · GLM Gamma > 1.5 Interpretación > Interacciones y efecto marginal
# -----------------------------------------------------------------------------
emmeans::emmeans(m_gamma, ~ criticidad | fase_proceso, type = "response") |>
  as.data.frame() |>
  dplyr::transmute(fase_proceso, criticidad, coste_predicho = round(response)) |>
  tidyr::pivot_wider(names_from = criticidad, values_from = coste_predicho)

# -----------------------------------------------------------------------------
# [fig-u31-prediccion]
#   1 · GLM Gamma > 1.5 Interpretación > Interacciones y efecto marginal
# -----------------------------------------------------------------------------
marginaleffects::plot_predictions(m_gamma,
    condition = c("antiguedad_anios", "criticidad", "fase_proceso")) +
  labs(x = "antigüedad de la máquina (años)", y = "coste medio predicho (€)",
       colour = NULL, fill = NULL)

# -----------------------------------------------------------------------------
# [tbl-u31-deviance]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
y_obs  <- base$coste_euros
mu_hat <- fitted(m_gamma)
bondad <- c(deviance_formula = 2 * sum(-log(y_obs / mu_hat) + (y_obs - mu_hat) / mu_hat),
            deviance_R       = deviance(m_gamma),
            gl               = df.residual(m_gamma),
            phi_deviance     = deviance(m_gamma) / df.residual(m_gamma),
            phi_pearson      = summary(m_gamma)$dispersion)
tibble::enframe(round(bondad, 3), name = "cantidad", value = "valor")

# -----------------------------------------------------------------------------
# [fig-u31-dharma]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > Bondad de ajuste y diagnóstico
# -----------------------------------------------------------------------------
sim_g <- DHARMa::simulateResiduals(m_gamma, n = 1000)   # se guarda: lo usan el texto y la figura por fabricante
plot(sim_g)

# -----------------------------------------------------------------------------
# [tbl-u31-anova]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > Selección de variables: el contraste $F$
# -----------------------------------------------------------------------------
f_t2 <- car::Anova(m_gamma, type = "II", test.statistic = "F")

tibble::tibble(term = rownames(f_t2)[-nrow(f_t2)],      # la última fila es el residuo
               gl   = f_t2$Df[-nrow(f_t2)],
               F    = round(f_t2$`F value`[-nrow(f_t2)], 1),
               p    = signif(f_t2$`Pr(>F)`[-nrow(f_t2)], 3))

# -----------------------------------------------------------------------------
# [fig-u31-res-fabricante]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
tibble::tibble(residuo = residuals(sim_g), fabricante = base$fabricante) |>
  ggplot(aes(fabricante, residuo)) +
  geom_boxplot(fill = "grey90", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, colour = "firebrick") +
  geom_hline(yintercept = 0.5, linetype = 2) +
  labs(x = "fabricante", y = "residuo cuantil (DHARMa)")

# -----------------------------------------------------------------------------
# [u31-control]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
m_fab <- update(m_gamma, . ~ . + fabricante)
f_fab <- anova(m_gamma, m_fab, test = "F")
f_fab

# -----------------------------------------------------------------------------
# [tbl-u31-aic]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
AIC(m_gamma, m_fab) |> round(1)

# -----------------------------------------------------------------------------
# [fig-u31-spline]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
rejilla_c <- seq(min(base$carga), max(base$carga), length.out = 200)
base_ns   <- predict(splines::ns(base$carga, df = 4), rejilla_c)

tibble::tibble(carga   = rep(rejilla_c, 4),
               valor   = as.vector(base_ns),
               funcion = rep(paste0("B", 1:4), each = length(rejilla_c))) |>
  ggplot(aes(carga, valor, colour = funcion)) +
  geom_line(linewidth = 0.8) +
  labs(x = "carga", y = "valor de la función base", colour = NULL)

# -----------------------------------------------------------------------------
# [tbl-u31-falta-ajuste]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
m_spl_edad  <- update(m_fab, . ~ . - antiguedad_anios + splines::ns(antiguedad_anios, 4))
m_spl_carga <- update(m_fab, . ~ . - carga + splines::ns(carga, 4))
m_int_extra <- update(m_fab, . ~ . + tipo_averia:criticidad + tipo_averia:fase_proceso)

falta <- purrr::map_dfr(
  list(`antigüedad no lineal` = m_spl_edad, `carga no lineal` = m_spl_carga,
       `interacciones del tipo de avería` = m_int_extra),
  \(m1) { a <- anova(m_fab, m1, test = "F")
          tibble::tibble(gl = a$Df[2], F = round(a$F[2], 2), p = signif(a$`Pr(>F)`[2], 2)) },
  .id = "ampliacion")
falta

# -----------------------------------------------------------------------------
# [u31-carga-cuad]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
m_final <- update(m_fab, . ~ . + I(carga^2))
f_cuad  <- anova(m_fab, m_final, test = "F")
f_cuad

# -----------------------------------------------------------------------------
# [fig-u31-carga-formas]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
perfil_c <- data.frame(tipo_averia = "Mecanica", criticidad = "Auxiliar", fase_proceso = "fase1",
                       antiguedad_anios = mean(base$antiguedad_anios), fabricante = "A",
                       carga = rejilla_c)
purrr::map_dfr(list(lineal = m_fab, spline = m_spl_carga, `cuadrática` = m_final),
               \(m) tibble::tibble(carga = rejilla_c,
                                   coste = predict(m, perfil_c, type = "response")),
               .id = "forma") |>
  ggplot(aes(carga, coste, colour = forma)) +
  geom_line(linewidth = 0.9) +
  geom_rug(data = base, aes(x = carga), inherit.aes = FALSE, alpha = 0.2) +
  labs(x = "carga", y = "coste medio predicho (€)", colour = NULL)

# -----------------------------------------------------------------------------
# [tbl-u31-deviance-final]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
bondad_fin <- c(deviance     = deviance(m_final),
                gl           = df.residual(m_final),
                phi_deviance = deviance(m_final) / df.residual(m_final),
                phi_pearson  = summary(m_final)$dispersion)
tibble::enframe(round(bondad_fin, 3), name = "cantidad", value = "valor")

# -----------------------------------------------------------------------------
# [fig-u31-dharma-final]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
sim_f <- DHARMa::simulateResiduals(m_final, n = 1000)
plot(sim_f)

# -----------------------------------------------------------------------------
# [tbl-u31-dharma-tests]
#   1 · GLM Gamma
#     > 1.6 Bondad de ajuste y selección
#       > ¿Le falta algo al modelo?
# -----------------------------------------------------------------------------
tests_f <- tibble::tibble(
  contraste = c("uniformidad (KS)", "dispersión", "atípicos"),
  p         = c(DHARMa::testUniformity(sim_f, plot = FALSE)$p.value,
                DHARMa::testDispersion(sim_f, plot = FALSE)$p.value,
                DHARMa::testOutliers(sim_f, plot = FALSE)$p.value))
tests_f |> dplyr::mutate(p = signif(p, 2))

# -----------------------------------------------------------------------------
# [tbl-u31-final]
#   1 · GLM Gamma > 1.7 Modelo validado y predicción > El modelo final
# -----------------------------------------------------------------------------
bf <- coef(m_final)                                 # coeficientes del modelo final (cifras en línea)
wf <- summary(m_final)$coefficients

broom::tidy(m_final, conf.int = TRUE) |>
  dplyr::transmute(term,
                   `exp(beta)` = round(exp(estimate), 3),
                   ic95        = paste0("[", round(exp(conf.low), 2), ", ", round(exp(conf.high), 2), "]"),
                   p           = signif(p.value, 3))

# -----------------------------------------------------------------------------
# [tbl-u31-pred-perfiles]
#   1 · GLM Gamma
#     > 1.7 Modelo validado y predicción
#       > Predicción
#         > Predicciones para distintos perfiles de máquina
# -----------------------------------------------------------------------------
perfiles <- tidyr::expand_grid(
  tipo_averia  = c("Mecanica", "Electronica"),
  criticidad   = c("Auxiliar", "Critica"),
  fase_proceso = c("fase1", "fase2"),
  antiguedad_anios = mean(base$antiguedad_anios),
  carga        = mean(base$carga),
  fabricante   = "A")

pr    <- predict(m_final, newdata = perfiles, type = "link", se.fit = TRUE)  # eta y su ee
t_975 <- qt(0.975, df.residual(m_final))
alfa  <- 1 / summary(m_final)$dispersion                                  # forma: alfa = 1/phi

pred_perfiles <- perfiles |>
  dplyr::transmute(tipo_averia, criticidad, fase_proceso,
                   media    = exp(pr$fit),
                   ic_inf   = exp(pr$fit - t_975 * pr$se.fit),
                   ic_sup   = exp(pr$fit + t_975 * pr$se.fit),
                   ip_inf   = qgamma(0.025, shape = alfa, scale = media / alfa),
                   ip_sup   = qgamma(0.975, shape = alfa, scale = media / alfa))

pred_perfiles |>
  dplyr::transmute(tipo_averia, criticidad, fase_proceso,
                   coste_medio = round(media),
                   ic95_media  = paste0("[", round(ic_inf), ", ", round(ic_sup), "]"),
                   ip95_averia = paste0("[", round(ip_inf), ", ", round(ip_sup), "]"))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
