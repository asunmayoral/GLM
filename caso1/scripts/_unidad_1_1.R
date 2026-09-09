# =============================================================================
# Caso 1 · Unidad 1.1 — 1 · Cuando la recta no llega: del dato binario al marco GLM
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_1_1.qmd.
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
  arm = "1.15.3", broom = "1.0.13", dplyr = "1.2.1", patchwork = "1.3.2",
  performance = "0.17.0", readr = "2.2.0", see = "0.14.0",
  sessioninfo = "1.2.4", tidyverse = "2.0.0")
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
# [fig-glow-eda-box]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.1 El modelo lineal en GLOW
# -----------------------------------------------------------------------------
glow |>
  ggplot(aes(x = fracture, y = age, fill = fracture)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3) +
  facet_wrap(~ priorfrac,
             labeller = labeller(priorfrac = c(No = "Sin fractura previa",
                                                Yes = "Con fractura previa"))) +
  labs(x = "Fractura en el primer año", y = "Edad (años)") +
  guides(fill = "none")

# -----------------------------------------------------------------------------
# [fig-u11-eda]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.1 El modelo lineal en GLOW
# -----------------------------------------------------------------------------
glow |>
  mutate(edad_grupo = cut(age, breaks = seq(55, 95, by = 5))) |>
  filter(!is.na(edad_grupo)) |>
  group_by(edad_grupo, priorfrac) |>
  summarise(age = mean(age), p_fractura = mean(fractura01), n = n(), .groups = "drop") |>
  ggplot(aes(age, p_fractura, color = priorfrac)) +
  #geom_line() + 
  geom_point(aes(size = n)) +
  geom_smooth(aes(weight = n), method = "lm", se = FALSE, linewidth = 1) +
  labs(x = "Edad (media del tramo)", y = "Proporción de fractura",
       color = "Fractura previa", size = "n")

# -----------------------------------------------------------------------------
# [fig-u11-ols-extrapola]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.1 El modelo lineal en GLOW
# -----------------------------------------------------------------------------
fit_ols <- lm(fractura01 ~ age + priorfrac, data = glow)
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

# -----------------------------------------------------------------------------
# [fig-u11-diagnostico-ols]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.1 El modelo lineal en GLOW
# -----------------------------------------------------------------------------
fit_ols <- lm(fractura01 ~ age + priorfrac, data = glow)

g_res <- tibble(ajustado = fitted(fit_ols),
                residuo  = glow$fractura01 - fitted(fit_ols),
                y = factor(glow$fractura01, labels = c("No fractura (y=0)", "Fractura (y=1)"))) |>
  ggplot(aes(ajustado, residuo, color = y)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Valores ajustados", y = "Residuo de respuesta (y − μ̂)",
       title = "Residuos vs ajustados (OLS)", color = NULL)+
  theme(legend.position="bottom")

g_qplot <- tibble(residuo = residuals(fit_ols)) |>
  ggplot(aes(sample = residuo)) +
  stat_qq(alpha = 0.4, size = 0.6) +
  stat_qq_line(color = "firebrick") +
  labs(x = "Cuantiles teóricos (Normal)", y = "Cuantiles de los residuos de OLS")

patchwork::wrap_plots(g_res, g_qplot, ncol = 2)

# -----------------------------------------------------------------------------
# [fig-u11-logit-inversa]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.3 Los tres componentes de un GLM
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# [u11-primer-glm]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.4 GLM en datos binarios
# -----------------------------------------------------------------------------
fit_glm <- glm(fracture ~ age + priorfrac, family = binomial, data = glow)
# Resultado del ajuste
summary(fit_glm)

# -----------------------------------------------------------------------------
# [u11-broom]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.4 GLM en datos binarios
# -----------------------------------------------------------------------------
tidy(fit_glm)                                     # coeficientes (escala log-odds)
glance(fit_glm)                                   # deviance, AIC, BIC, gl...
augment(fit_glm, type.predict = "response") |>    # ajustes en escala probabilidad
  dplyr::select(fracture, age, priorfrac, .fitted) |>
  slice_head(n = 5)

# -----------------------------------------------------------------------------
# [fig-u11-ols-vs-logistica]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.4 GLM en datos binarios
# -----------------------------------------------------------------------------
# Rejilla de predicción de los modelos 
grid <- expand_grid(
  age       = seq(min(glow$age), max(glow$age), length.out = 100),
  priorfrac = factor(c("No", "Yes"), levels = levels(glow$priorfrac))
)
grid$OLS <- predict(fit_ols, newdata = grid)                    # escala probabilidad
grid$GLM <- predict(fit_glm, newdata = grid, type = "response") # escala probabilidad
grid_long <- pivot_longer(grid, c(OLS, GLM),
                          names_to = "modelo", values_to = "p_hat") |>
  mutate(modelo = factor(modelo, levels = c("OLS", "GLM")))

# Proporciones observadas por grupo de edad y priorfrac (las de la EDA),
# situadas en la edad media de cada grupo
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

# -----------------------------------------------------------------------------
# [fig-u11-residuos-ols-glm]
#   1 · Cuando la recta no llega: del dato binario al marco GLM
#     > 1.4 GLM en datos binarios
# -----------------------------------------------------------------------------
p_ols <- plot(performance::binned_residuals(fit_ols)) +
  labs(title = "OLS (modelo lineal de probabilidad)")
p_glm <- plot(performance::binned_residuals(fit_glm)) +
  labs(title = "GLM logístico")
patchwork::wrap_plots( p_ols, p_glm, nrow = 2) +
  patchwork::plot_annotation(title = "Residuos medios por tramos: OLS vs GLM")


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
