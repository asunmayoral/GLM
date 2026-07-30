# =============================================================================
# Caso 1 · Unidad 1.3 — 3 · Extensión de la respuesta binaria: binomial y politómica
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_1_3.qmd.
# Cada bloque va precedido de su LABEL y de la sección/subsección donde aparece.
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
source(file.path(.raiz, "caso1", "R", "dgp_cohorte.R"))                        # define simular_cohorte(), cargar_cohorte(), expandir_persona_periodo()
cohorte <- cargar_cohorte()                      # lee datos/cohorte_*.rds si existe; si no (o si cambió el DGP), simula y guarda
# y guardamos las simulaciones
#write_csv(cohorte, "cohorte.csv")        # nivel individuo
# resumen
glimpse(cohorte)
head(cohorte)

# -----------------------------------------------------------------------------
# [u13-agrupar]  ·  🔧 En R. El glm() con datos binomiales
# -----------------------------------------------------------------------------
glow_agg <- glow |>
  count(priorfrac, momfrac, raterisk, fracture) |>
  tidyr::pivot_wider(names_from = fracture, values_from = n, values_fill = 0) |>
  mutate(total = No + Yes)            # ensayos por celda; Yes = nº de fracturas
glow_agg

# -----------------------------------------------------------------------------
# [fig-u13-barras]  ·  🔧 En R. El glm() con datos binomiales
# -----------------------------------------------------------------------------
glow_agg |>
  mutate(prop = Yes / total) |>
  ggplot(aes(x = priorfrac, y = prop, fill = momfrac)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = total), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  facet_wrap(~ raterisk, labeller = label_both) +
  labs(x = "Fractura previa (priorfrac)", y = "Proporción de fractura (Yes/total)",
       fill = "Antecedente materno\n(momfrac)") +
  ylim(0, NA)

# -----------------------------------------------------------------------------
# [u13-binomial-fit]  ·  🔧 En R. El glm() con datos binomiales
# -----------------------------------------------------------------------------
fit_bin <- glm(cbind(Yes, No) ~ priorfrac + momfrac + raterisk,
               family = binomial, data = glow_agg)
fit_ind <- glm(fracture ~ priorfrac + momfrac + raterisk,
               family = binomial, data = glow)

cbind(agrupado = coef(fit_bin), individual = coef(fit_ind))   # idénticos

# -----------------------------------------------------------------------------
# [fig-u13-residuos]  ·  La matriz sombrero en un GLM
# -----------------------------------------------------------------------------
#library(patchwork)

diag_bin <- glow_agg |>
  mutate(pred  = fitted(fit_bin),
         r_std = rstandard(fit_bin, type = "pearson"),
         cook  = cooks.distance(fit_bin),
         cell  = row_number())

p_res <- ggplot(diag_bin, aes(pred, r_std, size = total)) +
  geom_hline(yintercept = c(-2, 0, 2), linetype = c("dotted", "dashed", "dotted")) +
  geom_point(alpha = 0.7) +
  labs(x = "Probabilidad ajustada", y = "Residuo de Pearson estandarizado",
       size = "n por celda")

p_cook <- ggplot(diag_bin, aes(cell, cook, size = total)) +
  geom_hline(yintercept = 4 / nrow(diag_bin), linetype = "dotted") +
  geom_segment(aes(xend = cell, yend = 0), linewidth = 0.3) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(breaks = diag_bin$cell) +
  labs(x = "Celda", y = "Distancia de Cook", size = "n por celda")

p_res + p_cook + plot_layout(guides = "collect")

# -----------------------------------------------------------------------------
# [u13-gof]  ·  🔧 En R. Residuos e influencia > Bondad de ajuste
# -----------------------------------------------------------------------------
gl <- df.residual(fit_bin)                       # n - p = nº de celdas - nº de parámetros
c(deviance = deviance(fit_bin),
  pearson  = sum(residuals(fit_bin, type = "pearson")^2),
  gl       = gl)

# p-valores del contraste de bondad de ajuste (H0: el modelo ajusta)
c(p_deviance = pchisq(deviance(fit_bin), gl, lower.tail = FALSE),
  p_pearson  = pchisq(sum(residuals(fit_bin, "pearson")^2), gl, lower.tail = FALSE))

# -----------------------------------------------------------------------------
# [fig-u13-calibracion]  ·  🔧 En R. El contraste χ² de bondad de ajuste (deviance y Pearson) > Calibración
# -----------------------------------------------------------------------------
# Calibración DIRECTA: cada celda = (probabilidad predicha, proporción observada)
calib_bin <- glow_agg |>
  mutate(pred = fitted(fit_bin), obs = Yes / total)

ggplot(calib_bin, aes(pred, obs, size = total)) +
  geom_abline(linetype = "dashed") +                 # calibración perfecta
  geom_point(alpha = 0.7) +
  labs(x = "Probabilidad predicha", y = "Proporción observada (Yes/total)",
       size = "n por celda")

# -----------------------------------------------------------------------------
# [u13-sobredisp]  ·  🔧 En R. El data frame de observados vs predichos para la calibración > Sobredispersión
# -----------------------------------------------------------------------------
# Índice de sobredispersión (cociente de Pearson)
sum(residuals(fit_bin, type = "pearson")^2) / df.residual(fit_bin)

# -----------------------------------------------------------------------------
# [u13-nominal]  ·  🔧 En R. Ajustar e inspeccionar el multinomial nominal
# -----------------------------------------------------------------------------
library(nnet)
m_nom <- multinom(relevel(raterisk, ref = "Same") ~ age + priorfrac,
                  data = glow, trace = FALSE)
summary(m_nom) # dos bloques de coeficientes: Less|Same y Greater|Same
tidy(m_nom) # visualización ordenada
exp(coef(m_nom))      # odds ratios relativos

# -----------------------------------------------------------------------------
# [fig-u13-pred-nominal]  ·  🔧 En R. Ajustar e inspeccionar el multinomial nominal
# -----------------------------------------------------------------------------
library(ggeffects)
plot(ggpredict(m_nom, terms = "age [all]"))

# -----------------------------------------------------------------------------
# [u13-ordinal]  ·  3.4 Categorías ordenadas: el modelo de odds proporcionales
# -----------------------------------------------------------------------------
glow_ord <- glow |>
  mutate(raterisk = ordered(raterisk, levels = c("Less", "Same", "Greater")))
m_ord <- MASS::polr(raterisk ~ age + priorfrac, data = glow_ord, Hess = TRUE)
tidy(m_ord)        # dos umbrales (theta) y los efectos (beta), comunes a ambos cortes
exp(coef(m_ord))      # odds ratios acumulados

# -----------------------------------------------------------------------------
# [fig-u13-pred-ordinal]  ·  🔧 En R. Ajustar e inspeccionar el ordinal (odds proporcionales)
# -----------------------------------------------------------------------------
library(ggeffects)
plot(ggpredict(m_ord, terms = "age [all]"))

# -----------------------------------------------------------------------------
# [u13-comparacion-nom-ord]  ·  🔧 En R. Ajustar e inspeccionar el ordinal (odds proporcionales)
# -----------------------------------------------------------------------------
# Ajuste y parsimonia (el ordinal usa menos parámetros)
ajuste <- tibble::tibble(
  Modelo     = c("Nominal (multinom)", "Ordinal (polr)"),
  Parametros = c(length(coef(m_nom)), length(coef(m_ord)) + length(m_ord$zeta)),
  logLik     = c(as.numeric(logLik(m_nom)), as.numeric(logLik(m_ord))),
  AIC        = c(AIC(m_nom), AIC(m_ord)),
  BIC        = c(BIC(m_nom), BIC(m_ord))
)

# Odds ratios: el nominal da uno por categoría; el ordinal, uno (acumulado) común
or_nom <- exp(coef(m_nom))      # filas Less/Greater x (Intercept, age, priorfracYes)
or_ord <- exp(coef(m_ord))      # age, priorfracYes (acumulado)
discrepancias <- tibble::tibble(
  Covariable              = c("age", "priorfracYes"),
  `Nominal Less|Same`     = or_nom["Less",    c("age", "priorfracYes")],
  `Nominal Greater|Same`  = or_nom["Greater", c("age", "priorfracYes")],
  `Ordinal (acumulado)`   = or_ord[c("age", "priorfracYes")]
)

ajuste
discrepancias

