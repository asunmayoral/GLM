# =============================================================================
# Caso 3 · Unidad 3.5 — 5 · Del modelo al presupuesto
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_3_5.qmd.
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
# [u35-escenario]
#   5 · Del modelo al presupuesto > 5.2 El escenario del año próximo
# -----------------------------------------------------------------------------
nd5 <- subset(pn, anio == max(anio))   # el último año observado: una fila por máquina
nd5$edad       <- nd5$edad + 1         # un año más vieja
nd5$exposicion <- 1                    # el año completo
nrow(nd5)

# -----------------------------------------------------------------------------
# [u35-media]
#   5 · Del modelo al presupuesto
#     > 5.3 La media del presupuesto
#       > Condicional o marginal
# -----------------------------------------------------------------------------
sd_u5  <- sqrt(glmmTMB::VarCorr(t_mix2)$cond$id_maquina[1, 1])   # sigma_u del coste anual
mu_c   <- predict(t_mix2, newdata = nd5, type = "response")                  # condicional
mu_m   <- predict(t_mix2, newdata = nd5, type = "response", re.form = NA)    # marginal (b = 0)
jensen <- exp(sd_u5^2 / 2)                                                   # corrección

pres_media <- tibble::tibble(
  prediccion = c("condicional (con el BLUP de cada máquina)", "marginal, b = 0",
                 "marginal corregida, × exp(σ²/2)"),
  total = round(c(sum(mu_c), sum(mu_m), sum(mu_m) * jensen)))

# -----------------------------------------------------------------------------
# [tbl-u35-media]
#   5 · Del modelo al presupuesto
#     > 5.3 La media del presupuesto
#       > Condicional o marginal
# -----------------------------------------------------------------------------
pres_media

# -----------------------------------------------------------------------------
# [u35-delta]
#   5 · Del modelo al presupuesto
#     > 5.3 La media del presupuesto
#       > La incertidumbre de la media: el método delta
# -----------------------------------------------------------------------------
X5 <- model.matrix(~ edad + carga + proceso + criticidad + plan_mantenimiento, data = nd5)
V5 <- vcov(t_mix2)$cond                    # varianza de los efectos fijos
g5 <- as.vector(t(X5) %*% mu_c)            # gradiente del total: X' mu
ee_total <- sqrt(drop(t(g5) %*% V5 %*% g5))
ic_media <- sum(mu_c) * exp(c(-1, 1) * 1.96 * ee_total / sum(mu_c))   # IC en escala log
round(c(total = sum(mu_c), ee = ee_total, inf = ic_media[1], sup = ic_media[2]))

# -----------------------------------------------------------------------------
# [u35-simulacion]
#   5 · Del modelo al presupuesto
#     > 5.4 Lo que se gastará: el intervalo de predicción
#       > Simular la predictiva
# -----------------------------------------------------------------------------
set.seed(20252026)
p5   <- glmmTMB::family_params(t_mix2)[[1]]       # índice de potencia
phi5 <- sigma(t_mix2)                             # dispersión
alfa5 <- (2 - p5) / (p5 - 1)

rtweedie_pg <- function(mu) {                     # una realización Tweedie por máquina
  n <- rpois(length(mu), mu^(2 - p5) / (phi5 * (2 - p5)))
  rgamma(length(mu), shape = n * alfa5, scale = phi5 * (p5 - 1) * mu^(p5 - 1))
}                                                 # con n = 0, rgamma devuelve 0

b5    <- log(mu_c) - drop(X5 %*% glmmTMB::fixef(t_mix2)$cond)   # el BLUP de cada máquina
beta5 <- MASS::mvrnorm(4000, glmmTMB::fixef(t_mix2)$cond, V5)  # coeficientes posibles

sim5 <- tibble::tibble(
  solo_azar = replicate(4000, sum(rtweedie_pg(mu_c))),                       # beta fijo
  total     = apply(beta5, 1, \(b) sum(rtweedie_pg(exp(X5 %*% b + b5)))))     # beta y azar

# -----------------------------------------------------------------------------
# [tbl-u35-margenes]
#   5 · Del modelo al presupuesto
#     > 5.4 Lo que se gastará: el intervalo de predicción
#       > Simular la predictiva
# -----------------------------------------------------------------------------
margenes <- tibble::tibble(
  cifra = c("media estimada", "IC 95 % de la media (delta)",
            "gasto, solo azar: percentil 90", "gasto, azar + estimación: percentil 90",
            "gasto, azar + estimación: intervalo 90 %"),
  euros = c(sprintf("%.0f", sum(mu_c)),
            paste0("[", sprintf("%.0f", ic_media[1]), ", ", sprintf("%.0f", ic_media[2]), "]"),
            sprintf("%.0f", quantile(sim5$solo_azar, 0.9)),
            sprintf("%.0f", quantile(sim5$total, 0.9)),
            paste0("[", sprintf("%.0f", quantile(sim5$total, 0.05)), ", ",
                   sprintf("%.0f", quantile(sim5$total, 0.95)), "]")))
margenes

# -----------------------------------------------------------------------------
# [fig-u35-predictiva]
#   5 · Del modelo al presupuesto
#     > 5.4 Lo que se gastará: el intervalo de predicción
#       > Simular la predictiva
# -----------------------------------------------------------------------------
sim5 |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "fuente", values_to = "gasto") |>
  dplyr::mutate(fuente = factor(fuente, levels = c("solo_azar", "total"),
                                labels = c("solo azar", "azar + estimación"))) |>
  ggplot(aes(gasto / 1000, fill = fuente)) +
  geom_density(alpha = 0.5, colour = NA) +
  geom_vline(xintercept = sum(mu_c) / 1000) +
  geom_vline(xintercept = quantile(sim5$total, 0.9) / 1000, linetype = 2) +
  scale_fill_manual(values = c("grey75", "grey30")) +
  labs(x = "gasto total del año 5 (miles de €)", y = "densidad", fill = NULL) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# [tbl-u35-una]
#   5 · Del modelo al presupuesto
#     > 5.4 Lo que se gastará: el intervalo de predicción
#       > Una máquina no es la planta
# -----------------------------------------------------------------------------
set.seed(20252026)
dos <- c(mediana = which.min(abs(mu_c - median(mu_c))), mas_cara = which.max(mu_c))
purrr::map_dfr(dos, \(i) {
  s <- replicate(20000, rtweedie_pg(mu_c[i]))
  tibble::tibble(id_maquina = nd5$id_maquina[i], media = round(mu_c[i]),
                 prob_cero = round(mean(s == 0), 2), mediana = round(median(s)),
                 p90 = round(quantile(s, 0.9)))
}, .id = "maquina")

# -----------------------------------------------------------------------------
# [u35-validacion]
#   5 · Del modelo al presupuesto > 5.5 ¿Nos fiamos? Validación hacia atrás
# -----------------------------------------------------------------------------
set.seed(20252026)
t_val <- update(t_mix2, data = subset(pn, anio <= 3))        # el modelo, sin el año 4
nd4   <- subset(pn, anio == 3)
nd4$edad <- nd4$edad + 1; nd4$exposicion <- 1                 # el escenario del año 4
obs4  <- subset(pn, anio == 4)[match(nd4$id_maquina, subset(pn, anio == 4)$id_maquina), ]

mu4  <- predict(t_val, newdata = nd4, type = "response")
X4   <- model.matrix(~ edad + carga + proceso + criticidad + plan_mantenimiento, data = nd4)
b4   <- log(mu4) - drop(X4 %*% glmmTMB::fixef(t_val)$cond)
p5 <- glmmTMB::family_params(t_val)[[1]]; phi5 <- sigma(t_val); alfa5 <- (2 - p5) / (p5 - 1)
beta4 <- MASS::mvrnorm(4000, glmmTMB::fixef(t_val)$cond, vcov(t_val)$cond)
sim4  <- apply(beta4, 1, \(b) sum(rtweedie_pg(exp(X4 %*% b + b4))))

# Máquina a máquina: ¿cuántas superan su propio percentil 90?
p90_maq <- apply(replicate(4000, rtweedie_pg(mu4)), 1, quantile, 0.9)

validacion <- c(previsto = sum(mu4), observado = sum(obs4$coste_total),
                inf90 = quantile(sim4, 0.05), sup90 = quantile(sim4, 0.95),
                maquinas_sobre_p90 = 100 * mean(obs4$coste_total > p90_maq))
round(validacion)

# Restaurar los parámetros del modelo completo para lo que sigue
p5 <- glmmTMB::family_params(t_mix2)[[1]]; phi5 <- sigma(t_mix2); alfa5 <- (2 - p5) / (p5 - 1)

# -----------------------------------------------------------------------------
# [tbl-u35-asignacion]
#   5 · Del modelo al presupuesto
#     > 5.6 Escenarios: el intervalo de mantenimiento
#       > Qué se puede cambiar, y qué se puede creer
# -----------------------------------------------------------------------------
maq  <- banco$maquinas
prev <- subset(maq, plan_mantenimiento == "Preventivo")
p_asoc <- function(d, grupo) c(
  proceso    = chisq.test(table(d$proceso, d[[grupo]]))$p.value,
  criticidad = chisq.test(table(d$criticidad, d[[grupo]]))$p.value,
  carga      = summary(aov(d$carga ~ factor(d[[grupo]])))[[1]][1, 5],
  edad_alta  = summary(aov(d$edad_alta ~ factor(d[[grupo]])))[[1]][1, 5])
asignacion <- tibble::tibble(variable = names(p_asoc(maq, "plan_mantenimiento")),
                             plan = p_asoc(maq, "plan_mantenimiento"),
                             intervalo = p_asoc(prev, "intervalo_mant"))
asignacion |> dplyr::mutate(dplyr::across(plan:intervalo,
                                          \(x) format(signif(x, 2), scientific = FALSE, drop0trailing = TRUE)))

# -----------------------------------------------------------------------------
# [u35-mant]
#   5 · Del modelo al presupuesto
#     > 5.6 Escenarios: el intervalo de mantenimiento
#       > El modelo con el intervalo
# -----------------------------------------------------------------------------
nivel_mant <- function(x) factor(ifelse(is.na(x), "Correctivo", paste("cada", x, "días")),
                                 levels = c("Correctivo", "cada 120 días", "cada 90 días",
                                            "cada 60 días"))
pn$mantenimiento  <- nivel_mant(pn$intervalo_mant)
nd5$mantenimiento <- nivel_mant(nd5$intervalo_mant)

t_mant <- update(t_mix2, . ~ . - plan_mantenimiento + mantenimiento)
lrt_mant <- anova(t_mix2, t_mant)
lrt_mant

# -----------------------------------------------------------------------------
# [tbl-u35-mant]
#   5 · Del modelo al presupuesto
#     > 5.6 Escenarios: el intervalo de mantenimiento
#       > El modelo con el intervalo
# -----------------------------------------------------------------------------
broom.mixed::tidy(t_mant, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  dplyr::filter(grepl("^mantenimiento", term)) |>
  dplyr::transmute(regimen = sub("mantenimiento", "", term), factor = round(estimate, 2),
                   ic95 = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]"))

# -----------------------------------------------------------------------------
# [u35-escenarios]
#   5 · Del modelo al presupuesto
#     > 5.6 Escenarios: el intervalo de mantenimiento
#       > Los escenarios
# -----------------------------------------------------------------------------
set.seed(20252026)
f_mant <- ~ edad + carga + proceso + criticidad + mantenimiento
Xb    <- model.matrix(f_mant, nd5)
bm    <- log(predict(t_mant, nd5, type = "response")) - drop(Xb %*% glmmTMB::fixef(t_mant)$cond)
betam <- MASS::mvrnorm(4000, glmmTMB::fixef(t_mant)$cond, vcov(t_mant)$cond)
total_esc <- function(nd) {                                   # gasto medio, para cada beta simulado
  X <- model.matrix(f_mant, nd)
  apply(betam, 1, \(b) sum(exp(X %*% b + bm)))
}
visitas <- function(intervalo) ifelse(is.na(intervalo), 0, 365 / intervalo)   # revisiones al año

es_prev <- nd5$mantenimiento != "Correctivo"
es_cara <- nd5$id_maquina %in% caras$id_maquina
cambios <- list(`todas las preventivas`    = es_prev,
                `preventivas caras`        = es_prev & es_cara,
                `correctivas caras (dudoso)` = !es_prev & es_cara)

base_esc <- total_esc(nd5)
escenarios <- purrr::imap_dfr(cambios, \(cambia, nombre) {
  nd <- nd5; nd$mantenimiento[cambia] <- "cada 60 días"
  ahorro <- base_esc - total_esc(nd)
  extra  <- sum(365 / 60 - visitas(nd5$intervalo_mant[cambia]))
  tibble::tibble(escenario = nombre,
                 maquinas = sum(cambia & nd5$intervalo_mant %in% c(NA, 90, 120)),
                 ic95 = paste0("[", round(quantile(ahorro, 0.025)), ", ",
                               round(quantile(ahorro, 0.975)), "]"),
                 ahorro = round(median(ahorro)),
                 revisiones_extra = round(extra),
                 umbral_por_revision = round(median(ahorro) / extra))
})

# -----------------------------------------------------------------------------
# [tbl-u35-escenarios]
#   5 · Del modelo al presupuesto
#     > 5.6 Escenarios: el intervalo de mantenimiento
#       > Los escenarios
# -----------------------------------------------------------------------------
escenarios


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
