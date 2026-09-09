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
  DHARMa = "0.5.0", GGally = "2.4.0", MuMIn = "1.48.19", aplore3 = "0.9",
  arm = "1.15.3", broom = "1.0.13", dplyr = "1.2.1", ggplot2 = "4.0.3",
  lme4 = "2.0.1", patchwork = "1.3.2", performance = "0.17.0",
  readr = "2.2.0", see = "0.14.0", sessioninfo = "1.2.4", tibble = "3.3.1",
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
library(patchwork)
lims     <- c(0, 0.62)
col_coh  <- "#2c7fb8"   # azul  → cohorte
col_glow <- "#e6810a"   # naranja → GLOW

prop_coh <- cohorte |>
  group_by(centro) |>
  summarise(prop = mean(ever), n = n(), .groups = "drop")
p_coh <- ggplot(prop_coh, aes(reorder(factor(centro), prop), prop)) +
  geom_hline(yintercept = mean(cohorte$ever), linetype = "dashed", color = "grey50") +
  geom_col(fill = col_coh) +
  coord_flip() + scale_y_continuous(limits = lims) +
  labs(x = "Centro", y = "Proporción de fractura (ever)", title = "Cohorte (24 centros)")

prop_glow <- glow |>
  mutate(frac01 = as.integer(fracture == "Yes")) |>
  group_by(site_id) |>
  summarise(prop = mean(frac01), n = n(), .groups = "drop")
p_glow <- ggplot(prop_glow, aes(reorder(factor(site_id), prop), prop)) +
  geom_hline(yintercept = mean(glow$fracture == "Yes"), linetype = "dashed", color = "grey50") +
  geom_col(fill = col_glow) +
  coord_flip() + scale_y_continuous(limits = c(0,0.35)) +
  labs(x = "Centro (site_id)", y = "Proporción de fractura", title = "GLOW (6 centros)")

p_coh + p_glow

# -----------------------------------------------------------------------------
# [fig-cohorte-eda-box]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.2 Intercepto aleatorio
#       > Intercepto aleatorio en cohorte
# -----------------------------------------------------------------------------
cohorte |>
  mutate(Desenlace   = factor(evento, levels = c(0, 1), labels = c("Sin fractura", "Fractura")),
         Tratamiento = factor(x2,     levels = c(0, 1), labels = c("No", "Sí"))) |>
  ggplot(aes(x = x1, y = Desenlace, fill = Tratamiento)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.25) +
  labs(x = "Fragilidad ósea (x1, en z)", y = "Desenlace", fill = "Tratamiento")

# -----------------------------------------------------------------------------
# [u14-glmm-pool-int]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.2 Intercepto aleatorio
#       > Intercepto aleatorio en cohorte
# -----------------------------------------------------------------------------
library(lme4)
m_pool <- glm  (ever ~ x1 + x2,                family = binomial, data = cohorte)  # complete pooling
m_int  <- glmer(ever ~ x1 + x2 + (1 | centro),  family = binomial, data = cohorte)  # intercepto aleatorio

summary(m_pool)

# -----------------------------------------------------------------------------
# [u14-glmm-int]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.2 Intercepto aleatorio
#       > Intercepto aleatorio en cohorte
# -----------------------------------------------------------------------------
summary(m_int)

# -----------------------------------------------------------------------------
# [u14-glmm-varcov]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.2 Intercepto aleatorio
#       > 1. Componentes de varianza
# -----------------------------------------------------------------------------
# matriz de varianzas-covarianzas de los efectos aleatorios
vc <- as.data.frame(VarCorr(m_int))
vc$vcov[1]    # varianza  sigma_u^2
vc$sdcor[1]   # desviación sigma_u
# y de ahí el ICC latente:
vc$vcov[1] / (vc$vcov[1] + pi^2/3)

# verdad simulada
attr(cohorte, "verdad")$binaria[c("sigma_u", "icc_latente")]

# -----------------------------------------------------------------------------
# [fig-u14-glmm-coef]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.2 Intercepto aleatorio
#       > 2. Condicional vs marginal
# -----------------------------------------------------------------------------
# verdad
attr(cohorte,"verdad")$binaria[c("beta")]
# coeficientes estimados
sp <- coef(summary(m_pool))   # tabla de efectos fijos del pooled
si <- coef(summary(m_int))    # tabla de efectos fijos del mixto

coefs <- rbind(
  data.frame(modelo = "pooled (glm)",  term = rownames(sp),
             estimate = sp[, "Estimate"], se = sp[, "Std. Error"]),
  data.frame(modelo = "mixto (glmer)", term = rownames(si),
             estimate = si[, "Estimate"], se = si[, "Std. Error"])
)

library(ggplot2)
ggplot(coefs, aes(estimate, term, color = modelo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
                  position = position_dodge(width = 0.5)) +
  labs(x = "Coeficiente (log-odds), ± 1,96·EE", y = NULL, color = NULL)

# -----------------------------------------------------------------------------
# [fig-u14-cond-marg]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.2 Intercepto aleatorio
#       > 2. Condicional vs marginal
# -----------------------------------------------------------------------------
b0 <- 0; b1 <- 1.5; sigma_u <- 2          # sigma_u exagerado para que el aplanamiento se vea
x  <- seq(-6, 6, length.out = 200)

# Familia de curvas condicionales: varios centros (distinto u_j), MISMA pendiente
cond_fam <- tidyr::expand_grid(x = x, u = c(-4, -2, 0, 2, 4)) |>
  dplyr::mutate(p = plogis(b0 + b1 * x + u))

# Curva marginal: promedio de plogis(b0 + b1*x + u) sobre u ~ N(0, sigma_u^2)
ug <- seq(-4 * sigma_u, 4 * sigma_u, length.out = 401)
w  <- dnorm(ug, 0, sigma_u); w <- w / sum(w)
destacadas <- dplyr::bind_rows(
  tibble::tibble(x = x, p = plogis(b0 + b1 * x),
                 tipo = "condicional (un centro)"),
  tibble::tibble(x = x, p = sapply(x, \(xi) sum(w * plogis(b0 + b1 * xi + ug))),
                 tipo = "marginal (promedio)")
)

ggplot() +
  geom_line(data = cond_fam,   aes(x, p, group = u), color = "grey75", linewidth = 0.5) +
  geom_line(data = destacadas, aes(x, p, color = tipo), linewidth = 1.2) +
  scale_color_manual(values = c("condicional (un centro)" = "#2c7fb8",
                                "marginal (promedio)"     = "#e6810a")) +
  labs(x = "covariable", y = "Probabilidad de fractura", color = NULL) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# [fig-u14-cohorte-caterpillar]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.2 Intercepto aleatorio
#       > 3. Encogimiento o shrinkage (partial pooling).
# -----------------------------------------------------------------------------
re_coh <- as.data.frame(ranef(m_int, condVar = TRUE))   # columnas: grp, condval, condsd
ggplot(re_coh, aes(reorder(grp, condval), condval)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = condval - 1.96 * condsd,
                      ymax = condval + 1.96 * condsd), linewidth = 0.3) +
  coord_flip() +
  labs(x = "Centro", y = "Intercepto aleatorio (log-odds)")

# -----------------------------------------------------------------------------
# [u14-glmm-pool-int-compare]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.2 Intercepto aleatorio
#       > 3. Encogimiento o shrinkage (partial pooling).
# -----------------------------------------------------------------------------
# 1) Varianza entre centros: sigma_u^2 (y sigma_u)
vc   <- as.data.frame(VarCorr(m_int))
s2_u <- vc$vcov[1]          # sigma_u^2
s_u  <- vc$sdcor[1]         # sigma_u
c(sigma2_u = s2_u, sigma_u = s_u)

# 2) Varianza residual (latente, fija) e ICC
resid_var <- pi^2 / 3       # ~3.29  (en logit)
icc       <- s2_u / (s2_u + resid_var)
c(var_residual = resid_var, ICC = icc)
# (equivalente con paquete:  performance::icc(m_int)  -> 'adjusted ICC')

# 3) AIC de los dos modelos (menor = mejor)
AIC(m_pool, m_int)          # data.frame con df y AIC de cada uno

# -----------------------------------------------------------------------------
# [fig-u14-x1-centro]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Pendiente aleatoria: cuando el efecto varía por grupo
# -----------------------------------------------------------------------------
library(tidyverse)
n_bins <- 4
br  <- seq(min(cohorte$x1), max(cohorte$x1), length.out = n_bins + 1)
mid <- (head(br, -1) + tail(br, -1)) / 2              # punto medio de cada bin (posición en X)
cohorte |>
  mutate(bin    = cut(x1, breaks = br, include.lowest = TRUE),  # bins de igual longitud
         x1_bin = mid[as.integer(bin)]) |>
  group_by(centro, x1_bin) |>
  summarise(prop = mean(evento), .groups = "drop") |>           # proporción de fracturas por centro y bin
  ggplot(aes(x1_bin, prop, color = factor(centro), group = centro)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 1.6, alpha = 0.8) +
  facet_wrap(~ centro, ncol = 6) +                              # 6 columnas × 4 filas
  labs(x = "x1 (fragilidad ósea)", y = "Proporción de fracturas") +
  guides(color = "none")                                        # sin leyenda de centros

# -----------------------------------------------------------------------------
# [u14-glmm-slope]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Pendiente aleatoria: cuando el efecto varía por grupo
# -----------------------------------------------------------------------------
m_slope  <- glmer(ever ~ x1 + x2 + (1 + x1 | centro), family = binomial, data = cohorte)  # intercepto + pendiente

VarCorr(m_slope)        # sigma_u0, sigma_u1 y su correlacion

# -----------------------------------------------------------------------------
# [u14-glmm-comparacion]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.3 Pendiente aleatoria: cuando el efecto varía por grupo
# -----------------------------------------------------------------------------
AIC(m_pool, m_int, m_slope)   # pooled  <  intercepto  <  intercepto + pendiente
anova(m_int, m_slope)          # LRT de la pendiente

# -----------------------------------------------------------------------------
# [fig-u14-dharma]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Bondad de ajuste y diagnóstico del modelo mixto
# -----------------------------------------------------------------------------
library(DHARMa)
sim <- simulateResiduals(m_int)
plot(sim)

# -----------------------------------------------------------------------------
# [u14-dharma-tests]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Bondad de ajuste y diagnóstico del modelo mixto
# -----------------------------------------------------------------------------
testDispersion(sim)                          # sobre/infradispersión
plotResiduals(sim, form = cohorte$centro)    # residuos agregados por centro

# -----------------------------------------------------------------------------
# [u14-r2]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Bondad de ajuste y diagnóstico del modelo mixto
# -----------------------------------------------------------------------------
performance::r2(m_int)     # R2 marginal (fijos) y condicional (fijos + aleatorios)
performance::icc(m_int)    # icc no ajustado = R2(cond)-R2(marg)

# -----------------------------------------------------------------------------
# [u14-singular]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.4 Bondad de ajuste y diagnóstico del modelo mixto
# -----------------------------------------------------------------------------
isSingular(m_int)                    # TRUE = ajuste singular (estructura aleatoria no sostenida)
performance::check_singularity(m_int)

# -----------------------------------------------------------------------------
# [u14-glow-mixto]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.6 El modelo mixto sobre datos reales: GLOW por centro
# -----------------------------------------------------------------------------
library(lme4)
glow_m <- glow |> mutate(site_id = factor(site_id))

m_glow_pool <- glm  (fracture ~ age + priorfrac,                 family = binomial, data = glow_m)
m_glow_mix  <- glmer(fracture ~ age + priorfrac + (1 | site_id), family = binomial, data = glow_m)

summary(m_glow_mix)

# Varianza entre centros e ICC latente
vc <- as.data.frame(VarCorr(m_glow_mix))
c(sigma_u = sqrt(vc$vcov[1]), ICC = vc$vcov[1] / (vc$vcov[1] + pi^2 / 3))

# Efectos fijos: agrupado (glm) vs mixto (glmer)
cbind(pooled = coef(m_glow_pool), mixto = fixef(m_glow_mix))

# -----------------------------------------------------------------------------
# [fig-u14-glow-caterpillar]
#   4 · Efectos Aleatorios y Modelos Mixtos
#     > 4.6 El modelo mixto sobre datos reales: GLOW por centro
# -----------------------------------------------------------------------------
re_glow <- as.data.frame(ranef(m_glow_mix, condVar = TRUE))   # grp, condval, condsd
ggplot(re_glow, aes(reorder(grp, condval), condval)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = condval - 1.96 * condsd,
                      ymax = condval + 1.96 * condsd)) +
  coord_flip() +
  labs(x = "Centro (site_id)", y = "Intercepto aleatorio (log-odds)")


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
