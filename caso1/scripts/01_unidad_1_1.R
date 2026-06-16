# =============================================================================
#  Caso 1 · Unidad 1.1 — Cuando la recta no llega: del dato binario al marco GLM
#  Fuente: _unidad_1_1.qmd
#  Requiere el entorno de 00_presentacion_caso.R (objetos: glow, fractura01).
# =============================================================================

# ============================ 1.1 El modelo lineal en GLOW ===================

# Edad por fractura, condicionada por fractura previa        (fig: fig-glow-eda-box)
glow |>
  ggplot(aes(x = fracture, y = age, fill = fracture)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3) +
  facet_wrap(~ priorfrac,
             labeller = labeller(priorfrac = c(No = "Sin fractura previa",
                                                Yes = "Con fractura previa"))) +
  labs(x = "Fractura en el primer año", y = "Edad (años)") +
  guides(fill = "none")

# Proporción de fractura por tramos de edad + rectas de regresión   (fig: fig-u11-eda)
glow |>
  mutate(edad_grupo = cut(age, breaks = seq(55, 95, by = 5))) |>
  filter(!is.na(edad_grupo)) |>
  group_by(edad_grupo, priorfrac) |>
  summarise(age = mean(age), p_fractura = mean(fractura01), n = n(), .groups = "drop") |>
  ggplot(aes(age, p_fractura, color = priorfrac)) +
  geom_point(aes(size = n)) +
  geom_smooth(aes(weight = n), method = "lm", se = FALSE, linewidth = 1) +
  labs(x = "Edad (media del tramo)", y = "Proporción de fractura",
       color = "Fractura previa", size = "n")

# Modelo lineal (OLS) sobre la respuesta 0/1
fit_ols <- lm(fractura01 ~ age + priorfrac, data = glow)

# Problema 1: predicciones fuera de [0,1] al extrapolar     (fig: fig-u11-ols-extrapola)
rango <- range(glow$age)
grid_ext <- expand_grid(
  age       = seq(35, 110, length.out = 200),
  priorfrac = factor(c("No", "Yes"), levels = levels(glow$priorfrac)))
grid_ext$p_hat <- predict(fit_ols, newdata = grid_ext)

ggplot(grid_ext, aes(age, p_hat, color = priorfrac)) +
  annotate("rect", xmin = rango[1], xmax = rango[2], ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey40") +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  labs(x = "Edad (rango extrapolado; sombreado = rango observado)",
       y = "Probabilidad predicha (OLS)", color = "Fractura previa")

# Problemas 2-3: diagnóstico de residuos del OLS         (fig: fig-u11-diagnostico-ols)
g_res <- tibble(ajustado = fitted(fit_ols),
                residuo  = glow$fractura01 - fitted(fit_ols),
                y = factor(glow$fractura01, labels = c("No fractura (y=0)", "Fractura (y=1)"))) |>
  ggplot(aes(ajustado, residuo, color = y)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Valores ajustados", y = "Residuo de respuesta (y − mu_hat)",
       title = "Residuos vs ajustados (OLS)", color = NULL) +
  theme(legend.position = "bottom")

g_qplot <- tibble(residuo = residuals(fit_ols)) |>
  ggplot(aes(sample = residuo)) +
  stat_qq(alpha = 0.4, size = 0.6) +
  stat_qq_line(color = "firebrick") +
  labs(x = "Cuantiles teóricos (Normal)", y = "Cuantiles de los residuos de OLS")

patchwork::wrap_plots(g_res, g_qplot, ncol = 2)

# ====================== 1.3 Los tres componentes de un GLM ===================

# logit y su inversa (sigmoide)                          (fig: fig-u11-logit-inversa)
library(patchwork)
g1 <- tibble(p = seq(0.001, 0.999, by = 0.001)) |>
  mutate(y = qlogis(p)) |>
  ggplot(aes(p, y)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "logit:\n estira (0,1) sobre la recta real",
       x = expression(pi), y = expression(log(pi/(1 - pi))))

g2 <- tibble(eta = seq(-6, 6, by = 0.05)) |>
  mutate(p = plogis(eta)) |>
  ggplot(aes(eta, p)) +
  geom_line(linewidth = 1, color = "firebrick") +
  geom_hline(yintercept = c(0, 1), linetype = "dotted") +
  labs(title = "sigmoide:\n devuelve el predictor a (0,1)",
       x = expression(eta), y = expression(e^eta/(1 + e^eta)))

g1 + g2

# ============================ 1.4 GLM en datos binarios ======================

# 🔧 En R — ajuste del GLM logístico (callout tip):
#   glm(y ~ x1 + x2 + ..., family = binomial(link = "logit"), data = d)
#   La respuesta puede ir como 0/1 o como factor (se modela el 2º nivel = "éxito").

# Primer GLM logístico                                        (chunk: u11-primer-glm)
fit_glm <- glm(fracture ~ age + priorfrac, family = binomial, data = glow)
summary(fit_glm)

# Salida ordenada con broom                                          (chunk: u11-broom)
tidy(fit_glm)                                     # coeficientes (escala log-odds)
glance(fit_glm)                                   # deviance, AIC, BIC, gl...
augment(fit_glm, type.predict = "response") |>    # ajustes en escala probabilidad
  dplyr::select(fracture, age, priorfrac, .fitted) |>
  slice_head(n = 5)

# 🔧 En R — predicción en escala de probabilidad vs log-odds (callout tip):
#   predict(fit_glm, newdata = ..., type = "response")   # probabilidad en [0,1]
#   predict(fit_glm, newdata = ...)                      # por defecto: log-odds (type = "link")

# Predicciones OLS vs logística sobre los datos        (fig: fig-u11-ols-vs-logistica)
grid <- expand_grid(
  age       = seq(min(glow$age), max(glow$age), length.out = 100),
  priorfrac = factor(c("No", "Yes"), levels = levels(glow$priorfrac))
)
grid$OLS <- predict(fit_ols, newdata = grid)                    # escala probabilidad
grid$GLM <- predict(fit_glm, newdata = grid, type = "response") # escala probabilidad
grid_long <- pivot_longer(grid, c(OLS, GLM),
                          names_to = "modelo", values_to = "p_hat") |>
  mutate(modelo = factor(modelo, levels = c("OLS", "GLM")))

grupos <- glow |>
  mutate(edad_grupo = cut(age, breaks = seq(55, 95, by = 5))) |>
  filter(!is.na(edad_grupo)) |>
  group_by(edad_grupo, priorfrac) |>
  summarise(age = mean(age), p_fractura = mean(fractura01), n = n(), .groups = "drop")

ggplot() +
  geom_point(data = grupos,
             aes(age, p_fractura, color = priorfrac, size = n)) +
  geom_line(data = grid_long,
            aes(age, p_hat, color = priorfrac), linewidth = 1) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  facet_wrap(~ modelo) +
  labs(x = "Edad", y = "Proporción / probabilidad de fractura",
       color = "Fractura previa", size = "n")

# Residuos medios por tramos (binned): OLS vs GLM      (fig: fig-u11-residuos-ols-glm)
p_ols <- plot(performance::binned_residuals(fit_ols)) +
  labs(title = "OLS (modelo lineal de probabilidad)")
p_glm <- plot(performance::binned_residuals(fit_glm)) +
  labs(title = "GLM logístico")
patchwork::wrap_plots(p_ols, p_glm, nrow = 2) +
  patchwork::plot_annotation(title = "Residuos medios por tramos: OLS vs GLM")
