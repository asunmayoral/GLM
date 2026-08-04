# =============================================================================
# Caso 3 · Unidad 3.2 — 2 · Elegir familia y escala
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_2.qmd.
# Cada bloque va precedido de su LABEL y de la ruta de encabezados
# (sección > subsección > apartado) en la que aparece dentro del documento.
#
# GENERADO AUTOMÁTICAMENTE por _scripts/generar_scripts_unidades.R:
# no editar a mano; los cambios se pierden al regenerar. Edita el .qmd.
#
# EJECUCIÓN: funciona desde CUALQUIER carpeta dentro del proyecto GLM;
# localiza la raíz por _quarto.yml y resuelve solo las rutas de datos.
# =============================================================================

.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")

# --- Preámbulo del caso (librerías y datos, como en el documento) ------------
# Núcleo del Caso 3 (respuestas continuas positivas + mixtos + supervivencia).
# Cada unidad añadirá lo suyo cuando la desarrollemos (p. ej. glmmTMB::tweedie() en 3.4).
library(MASS)            # se carga ANTES que tidyverse para que dplyr::select() no quede enmascarada
library(tidyverse)       # manipulación, visualización y descriptivos
library(broom)           # resultados ordenados de glm y modelos de supervivencia
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

library(survival)        # Kaplan-Meier, Cox y datos start-stop
library(survminer)       # representación gráfica de supervivencia
library(flexsurv)        # modelos paramétricos de supervivencia (AFT)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(file.path(.raiz, "caso3", "R", "dgp_averias.R"))     # define simular_averias() y cargar_averias()
banco <- cargar_averias()      # lee datos/banco_averias_*.rds si existe; si no, simula y lo guarda

glimpse(banco$averias)

# -----------------------------------------------------------------------------
# [u32-modelo-referencia]
#   2 · Elegir familia y escala
# -----------------------------------------------------------------------------
f_coste <- coste_euros ~ tipo_averia + criticidad * fase_proceso + antiguedad_anios + carga + fabricante

m_gam <- glm(f_coste, family = Gamma(link = "log"), data = base)   # el m_fab de la Unidad 3.1

# -----------------------------------------------------------------------------
# [fig-u32-potencia]
#   2 · Elegir familia y escala > 2.1 Qué distingue a una familia de otra
# -----------------------------------------------------------------------------
mv <- base |>
  dplyr::mutate(grupo = dplyr::ntile(fitted(m_gam), 14)) |>
  dplyr::group_by(grupo) |>
  dplyr::summarise(media = mean(coste_euros), varianza = var(coste_euros), .groups = "drop")

p_hat <- coef(lm(log(varianza) ~ log(media), data = mv))[2]

# Rectas teóricas de referencia, ancladas al centroide de la nube en escala log10.
# Con scale_*_log10(), geom_abline() dibuja log10(y) = intercept + slope * log10(x).
refs <- tibble::tibble(p = 1:3,
                       familia = c("p = 1", "p = 2 (Gamma)", "p = 3 (inv. gaussiana)"),
                       intercepto = mean(log10(mv$varianza)) - p * mean(log10(mv$media)))

ggplot(mv, aes(media, varianza)) +
  geom_abline(data = refs, aes(slope = p, intercept = intercepto, colour = familia),
              linetype = 2, linewidth = 0.6) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.7) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c("grey55", "firebrick", "steelblue")) +
  labs(x = "media del grupo (€, log)", y = "varianza del grupo (log)", colour = NULL,
       subtitle = paste0("pendiente estimada (línea negra): p = ", round(p_hat, 2)))

# -----------------------------------------------------------------------------
# [tbl-u32-ig]
#   2 · Elegir familia y escala > 2.2 Inversa gaussiana
# -----------------------------------------------------------------------------
m_ig <- glm(f_coste, family = inverse.gaussian(link = "log"), data = base)

broom::tidy(m_ig, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::transmute(term, `exp(beta)` = round(estimate, 3),
                   conf.low = round(conf.low, 3), conf.high = round(conf.high, 3))

# -----------------------------------------------------------------------------
# [tbl-u32-lognormal]
#   2 · Elegir familia y escala > 2.3 Lognormal y log-OLS
# -----------------------------------------------------------------------------
m_ln <- lm(update(f_coste, log(coste_euros) ~ .), data = base)   # mismos predictores que m_gam

broom::glance(m_ln) |>
  dplyr::transmute(R2 = round(r.squared, 3), R2_aj = round(adj.r.squared, 3),
                   `F` = round(statistic, 1), p_valor = signif(p.value, 3),
                   sigma = round(sigma, 3), n = nobs)

# -----------------------------------------------------------------------------
# [tbl-u32-retransformacion]
#   2 · Elegir familia y escala > 2.3 Lognormal y log-OLS
# -----------------------------------------------------------------------------
y_obs    <- base$coste_euros
naive    <- exp(fitted(m_ln))                 # retransformación ingenua
smearing <- mean(exp(resid(m_ln)))            # factor de Duan (1983)
duan     <- naive * smearing

tibble::tibble(
  via = c("observado", "log-OLS sin corregir", "log-OLS + Duan", "GLM Gamma"),
  coste_medio = round(c(mean(y_obs), mean(naive), mean(duan), mean(fitted(m_gam))), 1)) |>
  dplyr::mutate(sesgo_pct = round(100 * (coste_medio / mean(y_obs) - 1), 1))

# -----------------------------------------------------------------------------
# [tbl-u32-dispersion]
#   2 · Elegir familia y escala > 2.4 Modelar la dispersión
# -----------------------------------------------------------------------------
base_disp <- base |>
  dplyr::mutate(pearson2 = residuals(m_gam, type = "pearson")^2)

m_disp <- glm(pearson2 ~ tipo_averia + criticidad + fase_proceso + antiguedad_anios + carga,
              family = Gamma(link = "log"), data = base_disp)

broom::tidy(m_disp) |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::transmute(term, estimate = round(estimate, 3), p.value = round(p.value, 3))

# -----------------------------------------------------------------------------
# [tbl-u32-aic]
#   2 · Elegir familia y escala > 2.5 Elegir, comparar y conectar
# -----------------------------------------------------------------------------
ll_ln <- as.numeric(logLik(m_ln)) - sum(log(base$coste_euros))   # + jacobiano
gl_ln <- attr(logLik(m_ln), "df")

tibble::tibble(
  familia = c("Gamma (log)", "Inversa gaussiana (log)", "Lognormal + jacobiano",
              "Lognormal SIN jacobiano [no comparable]"),
  AIC = round(c(AIC(m_gam), AIC(m_ig), -2 * ll_ln + 2 * gl_ln, AIC(m_ln)), 1)) |>
  dplyr::mutate(delta = ifelse(dplyr::row_number() <= 3, round(AIC - min(AIC[1:3]), 1), NA))

# -----------------------------------------------------------------------------
# [tbl-u32-verdad]
#   2 · Elegir familia y escala
#     > 2.5 Elegir, comparar y conectar
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(banco, "verdad")

tibble::tibble(
  cantidad  = c("familia del coste", "potencia p de Var(Y) ∝ μ^p", "dispersión φ"),
  verdadero = c("Gamma con enlace log", "2", as.character(round(verdad$phi_coste, 3))),
  estimado  = c("Gamma (AIC más bajo de las tres)",
                as.character(round(p_hat, 2)),
                as.character(round(summary(m_gam)$dispersion, 3))))

