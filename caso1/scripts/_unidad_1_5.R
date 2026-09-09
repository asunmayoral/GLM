# =============================================================================
# Caso 1 · Unidad 1.5 — 5 · Supervivencia: del hazard al GLM en tiempo discreto
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_1_5.qmd.
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
  arm = "1.15.3", broom = "1.0.13", lme4 = "2.0.1", patchwork = "1.3.2",
  performance = "0.17.0", readr = "2.2.0", scales = "1.4.0", see = "0.14.0",
  sessioninfo = "1.2.4", survival = "3.8.6", survminer = "0.5.2",
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
# [u15-cohorte-head]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.2 Precedentes en clave de supervivencia: Kaplan–Meier y Cox
#       > Kaplan–Meier: dejar hablar a los datos
# -----------------------------------------------------------------------------
head(cohorte[, c("id", "centro", "x1", "x2", "tiempo", "evento")])

# -----------------------------------------------------------------------------
# [fig-u15-km]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.2 Precedentes en clave de supervivencia: Kaplan–Meier y Cox
#       > Kaplan–Meier: dejar hablar a los datos
# -----------------------------------------------------------------------------
library(survival)
library(survminer)

km <- survfit(Surv(tiempo, evento) ~ x2, data = cohorte)
ggsurvplot(km, data = cohorte, conf.int = TRUE, pval = TRUE,
           legend.title = "Tratamiento (x2)",
           legend.labs  = c("No", "Sí"),
           xlab = "Periodo de revisión", ylab = "Supervivencia S(t)")

# -----------------------------------------------------------------------------
# [fig-u15-ph-ilustra]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.2 Precedentes en clave de supervivencia: Kaplan–Meier y Cox
#       > Riesgos proporcionales, en tiempo discreto
# -----------------------------------------------------------------------------
beta <- log(2)                                  # efecto de la covariable: HR = exp(beta) = 2
base <- tibble(periodo = 1:6,
               h0 = c(0.05, 0.07, 0.09, 0.11, 0.13, 0.15))  # hazard base creciente (ilustrativo)

base |>
  mutate(`Referencia (x = 0)` = h0,
         `Grupo (x = 1)`      = 1 - (1 - h0)^exp(beta)) |>   # PH discreto exacto
  pivot_longer(c(`Referencia (x = 0)`, `Grupo (x = 1)`),
               names_to = "grupo", values_to = "hazard") |>
  mutate(`Escala hazard: h_t`                 = hazard,
         `Escala cloglog: log(-log(1 - h_t))` = log(-log(1 - hazard))) |>
  pivot_longer(c(`Escala hazard: h_t`, `Escala cloglog: log(-log(1 - h_t))`),
               names_to = "escala", values_to = "valor") |>
  ggplot(aes(periodo, valor, color = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~escala, scales = "free_y") +
  scale_color_manual(values = c("Referencia (x = 0)" = "grey55",
                                "Grupo (x = 1)"      = "#2c7fb8")) +
  labs(x = "Periodo de revisión", y = NULL, color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# [fig-u15-km-loglog]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.2 Precedentes en clave de supervivencia: Kaplan–Meier y Cox
#       > Riesgos proporcionales, en tiempo discreto
# -----------------------------------------------------------------------------
ggsurvplot(km, data = cohorte, fun = "cloglog",
           legend.title = "Tratamiento", legend.labs = c("No", "Sí"),
           xlab = "log(periodo)", ylab = "log(-log S(t))")

# -----------------------------------------------------------------------------
# [u15-pp]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.3 La modelización GLM del problema de supervivencia
#       > De los datos al modelo: qué es $T$ y qué asumimos
# -----------------------------------------------------------------------------
pp <- expandir_persona_periodo(cohorte)   # de 1 fila por mujer a 1 fila por mujer y periodo en riesgo
head(pp, 8)

# -----------------------------------------------------------------------------
# [fig-u15-enlaces]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.3 La modelización GLM del problema de supervivencia
#       > El enlace complementary log-log y por qué es el natural
# -----------------------------------------------------------------------------
tibble(eta = seq(-4, 4, by = 0.02)) |>
  mutate(logit   = plogis(eta),
         cloglog = 1 - exp(-exp(eta))) |>
  pivot_longer(c(logit, cloglog), names_to = "enlace", values_to = "p") |>
  ggplot(aes(eta, p, color = enlace)) +
  geom_line(linewidth = 1) +
  labs(x = expression(eta), y = expression(p == g^{-1}(eta)), color = "Enlace")

# -----------------------------------------------------------------------------
# [u15-ajuste]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.3 La modelización GLM del problema de supervivencia
#       > Ajuste en tiempo discreto e interpretación
# -----------------------------------------------------------------------------
m_pp <- glm(y ~ periodo + x1 + x2, family = binomial("cloglog"), data = pp)
summary(m_pp)

exp(coef(m_pp)[c("x1", "x2")])               # hazard ratios estimados
exp(attr(cohorte, "verdad")$binaria$beta)    # HR verdaderos: e^0.85, e^-0.65

# -----------------------------------------------------------------------------
# [fig-u15-hazard-base]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.3 La modelización GLM del problema de supervivencia
#       > Ajuste en tiempo discreto e interpretación
# -----------------------------------------------------------------------------
base <- tibble(periodo = factor(levels(pp$periodo), levels = levels(pp$periodo)),
               x1 = 0, x2 = 0)
base$h0 <- predict(m_pp, newdata = base, type = "response")   # hazard base por periodo
base$t  <- seq_len(nrow(base))
base$S  <- cumprod(1 - base$h0)                               # supervivencia base

pH <- ggplot(base, aes(t, h0)) +
  geom_col(fill = "grey70") +
  labs(x = "Periodo", y = expression(hazard~base~~h[0][t]), title = "Riesgo base por periodo")
pS <- ggplot(base, aes(t, S)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Periodo", y = expression(supervivencia~~S[t]), title = "Supervivencia base")
pH + pS

# -----------------------------------------------------------------------------
# [u15-fragilidad]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.3 La modelización GLM del problema de supervivencia
#       > Ajuste en tiempo discreto e interpretación
# -----------------------------------------------------------------------------
library(lme4)
m_frail <- glmer(y ~ periodo + x1 + x2 + (1 | centro),
                 family = binomial("cloglog"), data = pp)
VarCorr(m_frail)               # sigma de la fragilidad (verdad del DGP: 0.70)
fixef(m_frail)[c("x1", "x2")]  # efectos fijos, ahora condicionales al centro

# -----------------------------------------------------------------------------
# [u15-logit]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.3 La modelización GLM del problema de supervivencia
#       > Cloglog frente a logit: ¿cuánto importa el enlace?
# -----------------------------------------------------------------------------
m_pp_logit <- glm(y ~ periodo + x1 + x2, family = binomial("logit"), data = pp)

# cloglog -> hazard ratios ; logit -> odds ratios
cbind(cloglog_HR = exp(coef(m_pp)[c("x1", "x2")]),
      logit_OR   = exp(coef(m_pp_logit)[c("x1", "x2")]))
c(AIC_cloglog = AIC(m_pp), AIC_logit = AIC(m_pp_logit))

# -----------------------------------------------------------------------------
# [fig-u15-logit-cmp]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.3 La modelización GLM del problema de supervivencia
#       > Cloglog frente a logit: ¿cuánto importa el enlace?
# -----------------------------------------------------------------------------
base2 <- tibble(periodo = factor(levels(pp$periodo), levels(pp$periodo)), x1 = 0, x2 = 0)
pred2 <- base2 |>
  mutate(cloglog = predict(m_pp,       newdata = base2, type = "response"),
         logit   = predict(m_pp_logit, newdata = base2, type = "response")) |>
  pivot_longer(c(cloglog, logit), names_to = "enlace", values_to = "h") |>
  mutate(t = as.integer(periodo)) |>
  arrange(enlace, t) |>
  group_by(enlace) |>
  mutate(S = cumprod(1 - h)) |>
  ungroup()

ggplot(pred2, aes(t, S, color = enlace)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Periodo", y = "Supervivencia base S(t)", color = "Enlace")

# -----------------------------------------------------------------------------
# [fig-u15-clinica]
#   5 · Supervivencia: del hazard al GLM en tiempo discreto
#     > 5.3 La modelización GLM del problema de supervivencia
#       > Conclusiones para la práctica clínica
# -----------------------------------------------------------------------------
arquetipos <- tibble(
  arquetipo = factor(c("Alta fragilidad, sin tratamiento",
                       "Alta fragilidad, con tratamiento",
                       "Fragilidad media, con tratamiento"),
                     levels = c("Alta fragilidad, sin tratamiento",
                                "Alta fragilidad, con tratamiento",
                                "Fragilidad media, con tratamiento")),
  x1 = c(1, 1, 0), x2 = c(0, 1, 1))

grid <- tidyr::expand_grid(periodo = factor(levels(pp$periodo), levels(pp$periodo)),
                           arquetipo = arquetipos$arquetipo) |>
  left_join(arquetipos, by = "arquetipo")
grid$h <- predict(m_pp, newdata = grid, type = "response")
grid <- grid |>
  mutate(t = as.integer(periodo)) |>
  arrange(arquetipo, t) |>
  group_by(arquetipo) |>
  mutate(S = cumprod(1 - h), riesgo_acum = 1 - S) |>
  ungroup()

ggplot(grid, aes(t, riesgo_acum, color = arquetipo)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Periodo de revisión", y = "Riesgo acumulado de fractura (1 - S)",
       color = NULL) +
  theme(legend.position = "top")


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
