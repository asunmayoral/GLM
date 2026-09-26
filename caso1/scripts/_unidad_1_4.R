# =============================================================================
# Caso 1 · Unidad 1.4 — 4 · Efectos Aleatorios y Modelos Mixtos
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_1_4.qmd.
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
  DHARMa = "0.5.0", GGally = "2.4.0", MASS = "7.3.65", MuMIn = "1.48.19",
  aplore3 = "0.9", arm = "1.15.3", broom = "1.0.13", lme4 = "2.0.1",
  marginaleffects = "0.32.0", pROC = "1.19.0.1", patchwork = "1.3.2",
  performance = "0.17.0", readr = "2.2.0", see = "0.14.0",
  sessioninfo = "1.2.4", tidyr = "1.3.2", tidyverse = "2.0.0")
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
# Núcleo de software (cada unidad carga además lo suyo: lme4, nnet, ordinal,
# pROC, performance, DHARMa, marginaleffects, car, survival...).

library(broom)
library(aplore3)
library(patchwork)
library(see)
library(DHARMa)
library(arm)
library(performance)
library(tidyverse)
library(MuMIn)
library(readr)
library(GGally)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

data(glow500)
glow <- glow500 |>
  as_tibble() |>
  mutate(fractura01 = as.integer(fracture) - 1L)   # No -> 0, Yes -> 1

glow |> count(fracture) |> mutate(prop = round(n / sum(n), 3))

library(readr)
source(url_glm("caso1/R/dgp_cohorte.R"))   # funciones del proceso generador
cohorte <- leer_datos_glm("caso1/datos/cohorte_20252026.rds")   # cohorte simulada del curso
# y guardamos las simulaciones
#write_csv(cohorte, "cohorte.csv")        # nivel individuo
# resumen
glimpse(cohorte)
head(cohorte)

# -----------------------------------------------------------------------------
# [fig-u14-prop-centro]
#   4 · Efectos Aleatorios y Modelos Mixtos
# -----------------------------------------------------------------------------
prop_coh <- cohorte |>
  group_by(centro) |>
  summarise(prop = mean(ever), .groups = "drop")
ggplot(prop_coh, aes(reorder(factor(centro), prop), prop)) +
  geom_hline(yintercept = mean(cohorte$ever), linetype = "dashed", color = "grey50") +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  labs(x = "Centro", y = "Proporción de fractura (ever)")

# -----------------------------------------------------------------------------
# [u14-eda-prop]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Qué sugieren los datos
# -----------------------------------------------------------------------------
# proporción de fractura por brazo de tratamiento, con su error estándar binomial
prop_trat <- cohorte |>
  mutate(Tratamiento = factor(x2, levels = c(0, 1), labels = c("No", "Sí"))) |>
  group_by(Tratamiento) |>
  summarise(n = n(), fracturas = sum(ever), prop = mean(ever), .groups = "drop") |>
  mutate(ee = sqrt(prop * (1 - prop) / n))
prop_trat

# -----------------------------------------------------------------------------
# [fig-u14-eda]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Qué sugieren los datos
# -----------------------------------------------------------------------------
p_frag <- cohorte |>
  mutate(Desenlace = factor(ever, levels = c(0, 1), labels = c("Sin fractura", "Fractura"))) |>
  ggplot(aes(x = x1, y = Desenlace)) +
  geom_boxplot(fill = "#2c7fb8", alpha = 0.6, outlier.alpha = 0.25) +
  labs(x = "Fragilidad ósea (x1, en z)", y = NULL, title = "Fragilidad")

p_trat <- ggplot(prop_trat, aes(Tratamiento, prop)) +
  geom_col(fill = "#2c7fb8", alpha = 0.6, width = 0.6) +
  geom_errorbar(aes(ymin = prop - 1.96 * ee, ymax = prop + 1.96 * ee), width = 0.15) +
  labs(x = "Tratamiento preventivo (x2)", y = "Proporción de fractura", title = "Tratamiento")

p_frag + p_trat

# -----------------------------------------------------------------------------
# [fig-u14-curvas-centro]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Qué sugieren los datos
# -----------------------------------------------------------------------------
# Un glm por centro (no pooling): cada centro, solo con sus propios datos
por_centro <- cohorte |>
  group_by(centro) |>
  group_modify(\(d, k) {
    m <- glm(ever ~ x1, family = binomial, data = d)
    tibble(term = c("Intercepto", "Pendiente de x1"), est = unname(coef(m)))
  }) |>
  ungroup()

m_pool_x1 <- glm(ever ~ x1, family = binomial, data = cohorte)   # referencia: todas juntas

x_grid <- seq(min(cohorte$x1), max(cohorte$x1), length.out = 100)
curvas <- por_centro |>
  select(centro, term, est) |>
  pivot_wider(names_from = term, values_from = est) |>
  crossing(x1 = x_grid) |>
  mutate(p = plogis(Intercepto + `Pendiente de x1` * x1))
curva_pool <- tibble(x1 = x_grid,
                     p  = predict(m_pool_x1, newdata = tibble(x1 = x_grid), type = "response"))

ggplot(curvas, aes(x1, p, group = centro)) +
  geom_line(color = "grey60", alpha = 0.6) +
  geom_line(data = curva_pool, aes(x1, p), inherit.aes = FALSE,
            color = "#2c7fb8", linewidth = 1.3) +
  labs(x = "Fragilidad ósea (x1, en z)", y = "Probabilidad de fractura ajustada")

# -----------------------------------------------------------------------------
# [u14-pooled]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Sin efecto aleatorio: el modelo pooled
# -----------------------------------------------------------------------------
m_pool <- glm(ever ~ x1 + x2, family = binomial, data = cohorte)   # complete pooling
summary(m_pool)

# -----------------------------------------------------------------------------
# [u14-glmm-int]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
# -----------------------------------------------------------------------------
library(lme4)
m_int <- glmer(ever ~ x1 + x2 + (1 | centro), family = binomial, data = cohorte)
summary(m_int)

# -----------------------------------------------------------------------------
# [u14-int-singular]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
# -----------------------------------------------------------------------------
isSingular(m_int)

# -----------------------------------------------------------------------------
# [u14-glmm-varcov]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > $\sigma_u$ y el ICC: cuánto difieren los centros
# -----------------------------------------------------------------------------
vc <- as.data.frame(VarCorr(m_int))
vc$vcov[1]                                   # varianza  sigma_u^2
vc$sdcor[1]                                  # desviación típica  sigma_u
# razón de odds de un centro frente al centro medio: a +1 DT, y el 95 % central de centros
exp(c(mas_1DT = 1, extremo_inf = -1.96, extremo_sup = 1.96) * vc$sdcor[1])

# -----------------------------------------------------------------------------
# [u14-glmm-icc]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > $\sigma_u$ y el ICC: cuánto difieren los centros
# -----------------------------------------------------------------------------
vc$vcov[1] / (vc$vcov[1] + pi^2 / 3)         # ICC latente (logit)

# -----------------------------------------------------------------------------
# [u14-int-niveles]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > Dos niveles de estimación: global y por centro
# -----------------------------------------------------------------------------
fixef(m_int)                                   # nivel global: común a todos los centros

# nivel de centro, para los 24 centros observados
efectos_centro <- data.frame(
  n   = as.vector(table(cohorte$centro)),      # pacientes del centro
  u_j = ranef(m_int)$centro[, 1],              # su desviación: el efecto aleatorio
  coef(m_int)$centro,                          # fijos + u_j: los coeficientes del centro
  check.names = FALSE)
round(efectos_centro, 3)

# -----------------------------------------------------------------------------
# [fig-u14-cond-marg]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > Dos niveles de estimación: global y por centro
# -----------------------------------------------------------------------------
xs <- seq(-3, 3, length.out = 200)

# promedio de plogis(eta + u) sobre u ~ N(0, s^2): la curva marginal
marginal <- function(eta, s)
  sapply(eta, \(e) integrate(\(u) plogis(e + u) * dnorm(u, 0, s), -Inf, Inf)$value)

# curvas de centro, centro tipico y marginal, para unos coeficientes dados
curvas_panel <- function(b0, b1, u, s, panel) {
  centros <- tidyr::expand_grid(x = xs, j = seq_along(u)) |>
    mutate(p = plogis(b0 + b1 * x + u[j]), tipo = "curva de cada centro", grupo = paste0("c", j))
  resumen <- bind_rows(
    tibble(x = xs, p = plogis(b0 + b1 * xs),       tipo = "efectos fijos (centro típico, u = 0)"),
    tibble(x = xs, p = marginal(b0 + b1 * xs, s),  tipo = "marginal (promedio)")
  ) |> mutate(grupo = tipo)
  bind_rows(centros, resumen) |> mutate(panel = panel)
}

b_int <- fixef(m_int)
s_int <- as.data.frame(VarCorr(m_int))$sdcor[1]
curvas_cm <- bind_rows(
  curvas_panel(b_int[["(Intercept)"]], b_int[["x1"]], ranef(m_int)$centro[1:6, 1], s_int,
               "Cohorte: seis primeros centros"),
  curvas_panel(0, 1.5, c(-4, -2, 0, 2, 4), 2,          # valores de juguete
               "Esquema con sigma_u = 2")
)

ggplot(curvas_cm, aes(x, p, group = grupo, color = tipo, linewidth = tipo)) +
  geom_line() +
  facet_wrap(~ panel) +
  scale_color_manual(values = c("curva de cada centro"  = "grey70",
                                "efectos fijos (centro típico, u = 0)" = "#2c7fb8",
                                "marginal (promedio)"   = "#e6810a")) +
  scale_linewidth_manual(values = c("curva de cada centro"  = 0.5,
                                    "efectos fijos (centro típico, u = 0)" = 1.2,
                                    "marginal (promedio)"   = 1.2)) +
  labs(x = "Fragilidad ósea (x1, en z)", y = "Probabilidad de fractura",
       color = NULL, linewidth = NULL) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# [fig-u14-glmm-coef]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > Dos niveles de estimación: global y por centro
# -----------------------------------------------------------------------------
sp <- coef(summary(m_pool))   # tabla de efectos fijos del pooled
si <- coef(summary(m_int))    # tabla de efectos fijos del mixto

coefs <- rbind(
  data.frame(modelo = "pooled (glm)",  term = rownames(sp),
             estimate = sp[, "Estimate"], se = sp[, "Std. Error"]),
  data.frame(modelo = "mixto (glmer)", term = rownames(si),
             estimate = si[, "Estimate"], se = si[, "Std. Error"])
)

ggplot(coefs, aes(estimate, term, color = modelo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
                  position = position_dodge(width = 0.5)) +
  labs(x = "Coeficiente (log-odds), ± 1,96·EE", y = NULL, color = NULL)

# -----------------------------------------------------------------------------
# [fig-u14-cohorte-caterpillar]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > Lo que hacen los efectos aleatorios: el encogimiento
# -----------------------------------------------------------------------------
re_coh <- as.data.frame(ranef(m_int, condVar = TRUE))   # columnas: grp, condval, condsd
ggplot(re_coh, aes(reorder(grp, condval), condval)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = condval - 1.96 * condsd,
                      ymax = condval + 1.96 * condsd), linewidth = 0.3) +
  coord_flip() +
  labs(x = "Centro", y = "Intercepto aleatorio (log-odds)")

# -----------------------------------------------------------------------------
# [u14-shrinkage]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > Lo que hacen los efectos aleatorios: el encogimiento
# -----------------------------------------------------------------------------
s2_u <- as.data.frame(VarCorr(m_int))$vcov[1]

shrink <- re_coh |>
  mutate(n      = as.numeric(table(cohorte$centro)[as.character(grp)]),
         lambda = 1 - condsd^2 / s2_u,      # fraccion de senal propia que conserva el centro
         crudo  = condval / lambda) |>      # la desviacion del no pooling, implicita
  select(centro = grp, n, crudo, lambda, encogido = condval) |>
  arrange(n)

bind_rows(head(shrink, 3), tail(shrink, 3)) |>   # los 3 centros menores y los 3 mayores
  mutate(across(where(is.numeric), \(x) round(x, 2)))

# -----------------------------------------------------------------------------
# [u14-ame-int]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > Efectos en probabilidad: el efecto marginal medio (AME)
# -----------------------------------------------------------------------------
library(marginaleffects)

# centros observados (re.form por defecto), centro típico (re.form = NA) y pooled:
# marginaleffects da estimación, EE e IC por el método delta
fila_me <- \(me, efecto, version)
  data.frame(efecto, version, estimate = me$estimate, se = me$std.error,
             conf.low = me$conf.low, conf.high = me$conf.high)
ames <- rbind(
  fila_me(avg_slopes(m_int,  variables = "x1"),               "fragilidad", "observados"),
  fila_me(avg_slopes(m_int,  variables = "x1", re.form = NA), "fragilidad", "típico"),
  fila_me(avg_slopes(m_pool, variables = "x1"),               "fragilidad", "pooled"),
  fila_me(avg_comparisons(m_int,  variables = list(x2 = 0:1)),               "tratamiento", "observados"),
  fila_me(avg_comparisons(m_int,  variables = list(x2 = 0:1), re.form = NA), "tratamiento", "típico"),
  fila_me(avg_comparisons(m_pool, variables = list(x2 = 0:1)),               "tratamiento", "pooled"))

# poblacional: se integra sobre u ~ N(0, sigma_u^2) el riesgo de cada paciente
trat    <- cohorte$x2
X_int   <- model.matrix(m_int)
media_u <- \(f) integrate(\(u) f(u) * dnorm(u, 0, s_int), -Inf, Inf)$value
ame_pob <- function(b) {                          # AME poblacional para unos efectos fijos b
  eta <- drop(X_int %*% b)                        # predictor lineal sin efecto de centro
  c(fragilidad  = mean(sapply(eta, \(e) media_u(\(u) b[["x1"]] * dlogis(e + u)))),
    tratamiento = mean(mapply(\(e, t) media_u(\(u) plogis(e + b[["x2"]] * (1 - t) + u) -
                                                     plogis(e - b[["x2"]] * t + u)),
                              eta, trat)))
}

# su EE, también por el método delta: gradiente numérico respecto a los efectos fijos
ee_delta <- function(f, b, V, h = 1e-4) {
  J <- sapply(seq_along(b), \(k) {
    sube <- baja <- b; sube[k] <- sube[k] + h; baja[k] <- baja[k] - h
    (f(sube) - f(baja)) / (2 * h)
  })
  sqrt(diag(J %*% V %*% t(J)))
}
fila_pob <- \(est, se)
  data.frame(efecto = names(est), version = "poblacional", estimate = est, se = se,
             conf.low = est - 1.96 * se, conf.high = est + 1.96 * se)

est_pob <- ame_pob(b_int)
ames <- rbind(ames, fila_pob(est_pob, ee_delta(ame_pob, b_int, as.matrix(vcov(m_int)))))

# presentación: agrupado por versión, en puntos porcentuales y en una tabla compacta
etiquetas_v <- c(observados  = "centros observados",
                 "típico"    = "centro típico (u = 0)",
                 poblacional = "poblacional",
                 pooled      = "pooled (glm)")
presenta_ame <- function(d) d |>
  mutate(`versión` = factor(etiquetas_v[version], levels = etiquetas_v),
         efecto    = ifelse(efecto == "fragilidad", "fragilidad (+1 DT)", "tratamiento (sí / no)"),
         across(c(estimate, se, conf.low, conf.high), \(x) 100 * x),
         ic        = sprintf("[%.1f, %.1f]", conf.low, conf.high)) |>
  arrange(`versión`, efecto) |>
  select(`versión`, efecto, `AME (puntos %)` = estimate, EE = se, `IC 95 %` = ic) |>
  knitr::kable(digits = 1, row.names = FALSE)

presenta_ame(ames)

# -----------------------------------------------------------------------------
# [u14-prediccion-nuevo]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto aleatorio
#         > Predecir para un centro nuevo
# -----------------------------------------------------------------------------
nd  <- tibble(x1 = 0, x2 = 0)                               # perfil de referencia
eta <- unname(predict(m_int, newdata = nd, re.form = NA))   # predictor lineal, sin efecto de centro
s_u <- as.data.frame(VarCorr(m_int))$sdcor[1]

p_tipico   <- plogis(eta)                                   # centro típico, u = 0
p_marginal <- integrate(\(u) plogis(eta + u) * dnorm(u, 0, s_u), -Inf, Inf)$value  # promedio sobre u
p_rango    <- plogis(eta + c(-1.96, 1.96) * s_u)            # 95 % central de los centros

round(c(tipico = p_tipico, marginal = p_marginal,
        rango_inf = p_rango[1], rango_sup = p_rango[2]), 3)

# -----------------------------------------------------------------------------
# [u14-glmm-slope]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto y pendiente aleatorias
# -----------------------------------------------------------------------------
m_slope <- glmer(ever ~ x1 + x2 + (1 + x1 | centro), family = binomial, data = cohorte)

isSingular(m_slope)     # ¿sostienen los datos la estructura?
VarCorr(m_slope)        # sigma_u0, sigma_u1 y su correlacion

# -----------------------------------------------------------------------------
# [fig-u14-icc-x1]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto y pendiente aleatorias
# -----------------------------------------------------------------------------
S      <- VarCorr(m_slope)$centro                    # matriz Sigma (2 x 2)
icc_de <- \(s2) s2 / (s2 + pi^2 / 3)                 # ICC latente de una varianza entre centros

xs    <- seq(-2.5, 2.5, length.out = 200)
icc_x <- icc_de(S[1, 1] + 2 * S[1, 2] * xs + S[2, 2] * xs^2)

# valores de referencia: en x1 = 0, y donde la varianza entre centros es minima
x_min <- -S[1, 2] / S[2, 2]                          # vertice de la parabola: -sigma_01 / sigma^2_u1
round(c(ICC_en_0   = icc_de(S[1, 1]),
        x1_minimo  = x_min,
        ICC_minimo = icc_de(S[1, 1] - S[1, 2]^2 / S[2, 2])), 3)

niveles <- c("m_slope: ICC según x1",
             "m_slope: solo la varianza del intercepto",
             "m_int: ICC constante")
icc_df <- bind_rows(
  tibble(x1 = xs, icc = icc_x,                curva = niveles[1]),
  tibble(x1 = xs, icc = icc_de(S[1, 1]),      curva = niveles[2]),
  tibble(x1 = xs, icc = icc_de(vc$vcov[1]),   curva = niveles[3])
) |> mutate(curva = factor(curva, levels = niveles))

ggplot(icc_df, aes(x1, icc, color = curva, linetype = curva)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#2c7fb8", "grey40", "#e6810a")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Fragilidad ósea (x1, en z)",
       y = "ICC latente (fracción de la variación\ndebida al centro)",
       color = NULL, linetype = NULL) +
  guides(color = guide_legend(ncol = 1)) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# [u14-glmm-slope-summary]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto y pendiente aleatorias
# -----------------------------------------------------------------------------
summary(m_slope)

# -----------------------------------------------------------------------------
# [u14-ame-slope]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Tres modelos para el efecto de centro
#       > Intercepto y pendiente aleatorias
# -----------------------------------------------------------------------------
# poblacional: 2000 centros simulados de N(0, Sigma), los mismos para todo el cálculo
set.seed(SEMILLA_CURSO)
U   <- MASS::mvrnorm(2000, mu = c(0, 0), Sigma = matrix(S, 2, 2))
X_s <- model.matrix(m_slope)
ame_pob_s <- function(b) {                        # AME poblacional para unos efectos fijos b
  lin <- outer(drop(X_s %*% b), U[, 1], "+") + outer(cohorte$x1, U[, 2])  # paciente x centro
  c(fragilidad  = mean(sweep(dlogis(lin), 2, b[["x1"]] + U[, 2], "*")),
    tratamiento = mean(plogis(lin + b[["x2"]] * (1 - trat)) - plogis(lin - b[["x2"]] * trat)))
}

b_s <- fixef(m_slope)
est_pob_s <- ame_pob_s(b_s)

rbind(
  fila_me(avg_slopes(m_slope, variables = "x1"),               "fragilidad", "observados"),
  fila_me(avg_slopes(m_slope, variables = "x1", re.form = NA), "fragilidad", "típico"),
  fila_me(avg_comparisons(m_slope, variables = list(x2 = 0:1)),               "tratamiento", "observados"),
  fila_me(avg_comparisons(m_slope, variables = list(x2 = 0:1), re.form = NA), "tratamiento", "típico"),
  fila_pob(est_pob_s, ee_delta(ame_pob_s, b_s, as.matrix(vcov(m_slope))))) |>
  presenta_ame()

# -----------------------------------------------------------------------------
# [u14-glmm-comparacion]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > Qué modelo: la comparación de los tres
# -----------------------------------------------------------------------------
AIC(m_pool, m_int, m_slope)   # los tres, de menos a más estructura aleatoria
BIC(m_pool, m_int, m_slope)
# LRT de cada salto (p sin corregir). El mixto va primero: con el glm delante,
# anova() aplicaría el método de glm e ignoraría al mixto
anova(m_int, m_pool)           # pooled -> intercepto aleatorio
anova(m_slope, m_int)          # intercepto -> pendiente aleatoria

# -----------------------------------------------------------------------------
# [u14-lrt-frontera]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > Qué modelo: la comparación de los tres
# -----------------------------------------------------------------------------
library(varTestnlme)
varCompTest(m_int, m_pool, output = FALSE)    # H0: sigma_u^2 = 0
varCompTest(m_slope, m_int, output = FALSE)   # H0: sigma_u1^2 = 0 (y, con ella, sigma_01 = 0)

# -----------------------------------------------------------------------------
# [u14-r2]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Cuánto explica? $R^2$ marginal y condicional
# -----------------------------------------------------------------------------
performance::r2(m_int)     # R2 marginal (fijos) y condicional (fijos + aleatorios)
performance::icc(m_int)    # icc no ajustado = R2(cond)-R2(marg)

# -----------------------------------------------------------------------------
# [u14-evaluacion]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Qué tal predice? Calibración y discriminación
# -----------------------------------------------------------------------------
library(pROC)
# la misma paciente, predicha con el efecto de su centro y como si viniera de un centro nuevo
pred_eval <- tibble(
  y        = cohorte$ever,
  conocido = fitted(m_int),
  nuevo    = predict(m_int, re.form = NA, type = "response")
) |>
  pivot_longer(c(conocido, nuevo), names_to = "prediccion", values_to = "p")

# deciles de riesgo de cada predicción: observados (O) y esperados (E), como en la Sección 2.6
calib_mix <- pred_eval |>
  group_by(prediccion) |>
  mutate(decil = ntile(p, 10)) |>
  group_by(prediccion, decil) |>
  summarise(n = n(), O = sum(y), E = sum(p), pred = mean(p), obs = mean(y), .groups = "drop")

auc_mix <- pred_eval |>
  group_by(prediccion) |>
  summarise(AUC = as.numeric(auc(roc(y, p, quiet = TRUE))))

calib_mix |>
  group_by(prediccion) |>
  summarise(prop_observada = sum(O) / sum(n),
            prop_predicha  = sum(E) / sum(n),
            HL             = sum((O - E)^2 / (n * pred * (1 - pred))),
            p_HL           = pchisq(HL, df = 10 - 2, lower.tail = FALSE)) |>
  left_join(auc_mix, by = "prediccion") |>
  mutate(across(where(is.numeric), \(v) round(v, 3)))

# -----------------------------------------------------------------------------
# [fig-u14-calibracion]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Qué tal predice? Calibración y discriminación
# -----------------------------------------------------------------------------
ggplot(calib_mix, aes(pred, obs, colour = prediccion)) +
  geom_abline(linetype = "dashed") +
  geom_line() + geom_point(size = 1.8) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Probabilidad ajustada (media por decil)", y = "Frecuencia observada",
       colour = "Centro")

# -----------------------------------------------------------------------------
# [fig-u14-dharma]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Se cumplen los supuestos? Diagnóstico
#         > Nivel de paciente: residuos simulados
# -----------------------------------------------------------------------------
set.seed(SEMILLA_CURSO)   # los residuos se simulan: la semilla los hace reproducibles
sim <- simulateResiduals(m_int)
plot(sim)

# -----------------------------------------------------------------------------
# [u14-dharma-uniformidad]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Se cumplen los supuestos? Diagnóstico
#         > Nivel de paciente: residuos simulados
# -----------------------------------------------------------------------------
testUniformity(sim, plot = FALSE)   # Q-Q: ¿residuos uniformes?
testQuantiles(sim, plot = FALSE)    # panel derecho: ¿cuantiles planos frente a lo predicho?

# -----------------------------------------------------------------------------
# [fig-u14-dharma-x1]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Se cumplen los supuestos? Diagnóstico
#         > Nivel de paciente: residuos simulados
# -----------------------------------------------------------------------------
plotResiduals(sim, form = cohorte$x1)
testQuantiles(sim, predictor = cohorte$x1, plot = FALSE)   # el mismo contraste, en texto

# -----------------------------------------------------------------------------
# [fig-u14-dharma-centro]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Se cumplen los supuestos? Diagnóstico
#         > Nivel de centro: homogeneidad, dispersión y normalidad
# -----------------------------------------------------------------------------
# factor: con el código numérico del centro, DHARMa lo trataría como una covariable continua
tc <- testCategorical(sim, catPred = factor(cohorte$centro))
min(tc$uniformity$p.value.cor)   # el KS más desfavorable de los 24, ya ajustado
tc$homogeneity                   # Levene: ¿misma varianza en todos los centros?

# -----------------------------------------------------------------------------
# [u14-dispersion-centro]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Se cumplen los supuestos? Diagnóstico
#         > Nivel de centro: homogeneidad, dispersión y normalidad
# -----------------------------------------------------------------------------
sim_centro <- recalculateResiduals(sim, group = cohorte$centro)
testDispersion(sim_centro, plot = FALSE)

# -----------------------------------------------------------------------------
# [fig-u14-qq-ranef]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Se cumplen los supuestos? Diagnóstico
#         > Nivel de centro: homogeneidad, dispersión y normalidad
# -----------------------------------------------------------------------------
ggplot(re_coh, aes(sample = condval)) +
  stat_qq() +
  stat_qq_line(linetype = "dashed") +
  labs(x = "Cuantil teórico N(0, 1)", y = "Intercepto aleatorio estimado (log-odds)")
shapiro.test(re_coh$condval)

# -----------------------------------------------------------------------------
# [u14-ranef-atipico]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Comparación, evaluación y diagnóstico
#       > ¿Se cumplen los supuestos? Diagnóstico
#         > Nivel de centro: homogeneidad, dispersión y normalidad
# -----------------------------------------------------------------------------
extremo <- which.max(abs(re_coh$condval))
re_coh[extremo, c("grp", "condval")]        # el centro más alejado del típico
shapiro.test(re_coh$condval[-extremo])      # el contraste, sin él

# -----------------------------------------------------------------------------
# [u14-zcp]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.5 Extensiones: estructuras aleatorias más ricas
# -----------------------------------------------------------------------------
m_zcp <- glmer(ever ~ x1 + x2 + (1 + x1 || centro), family = binomial, data = cohorte)
anova(m_zcp, m_slope)   # LRT de rho_01 = 0


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
