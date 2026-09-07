# =============================================================================
# Caso 2 · Unidad 2.6 — 6 · Del conteo al reloj. Supervivencia
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_6.qmd.
# Cada bloque va precedido de su LABEL y de la sección/subsección donde aparece.
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
  DHARMa = "0.5.0", MASS = "7.3.65", MuMIn = "1.48.19", broom = "1.0.13",
  dplyr = "1.2.1", glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  marginaleffects = "0.32.0", patchwork = "1.3.2", performance = "0.17.0",
  pscl = "1.5.9", purrr = "1.2.2", sessioninfo = "1.2.4", survival = "3.8.6",
  tibble = "3.3.1", tidyr = "1.3.2", tidyverse = "2.0.0", vcd = "1.4.13",
  vcdExtra = "0.9.6")
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
library(pscl)          # hurdle / zeroinfl
library(glmmTMB)       # conteos mixtos / ceros
library(lme4)          # glmer (Poisson)
library(DHARMa); library(performance); library(marginaleffects)
library(survival)      # riesgos a trozos (base-recommended)
library(MuMIn); library(glmnet)   # selección / regularización 
library(vcd)           # mosaicos para tablas de contingencia (2.2)
library(vcdExtra)      # zero-inflated (2.4) y utilidades de tablas (2.2)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
glimpse(cartera)

# -----------------------------------------------------------------------------
# [u26-datos]  ·  6.1 El hazard como una tasa: el problema del tiempo a evento > Los datos y la pregunta
# -----------------------------------------------------------------------------
cartera |>
  dplyr::select(id_poliza, tiempo_primer_sin, evento, exposicion,
                zona_circulacion, tipo_vehiculo, sexo) |>
  head(8)

# -----------------------------------------------------------------------------
# [fig-u26-km]  ·  Cómo leer una fila: (exposicion,tiempo_primer_sin, evento) > Una primera mirada: la curva de supervivencia
# -----------------------------------------------------------------------------
km <- survfit(Surv(tiempo_primer_sin, evento) ~ zona_circulacion, data = cartera)
broom::tidy(km) |>
  ggplot(aes(time, estimate, colour = strata)) +
  geom_step(linewidth = 0.8) +
  labs(x = "tramo de antigüedad", y = "supervivencia estimada  S(t)", colour = "zona")

# -----------------------------------------------------------------------------
# [u26-pp]  ·  Cómo leer una fila: (exposicion,tiempo_primer_sin, evento) > La transformación a persona-periodo (y por qué)
# -----------------------------------------------------------------------------
pp <- expandir_poliza_tramo(cartera, "tiempo_primer_sin", "evento", col_id = "id_poliza")
# la primera póliza, desplegada en sus tramos en riesgo:
head(pp[pp$id == pp$id[1], c("id", "tramo", "y", "zona_circulacion", "uso")], 8)

# -----------------------------------------------------------------------------
# [u26-haz-emp]  ·  6.2 El modelo de riesgos a trozos como GLM de Poisson > Qué dicen los datos antes de modelar
# -----------------------------------------------------------------------------
pp |>
  dplyr::group_by(tramo) |>
  dplyr::summarise(en_riesgo = dplyr::n(), eventos = sum(y),
                   hazard = round(mean(y), 4), .groups = "drop")

# -----------------------------------------------------------------------------
# [fig-u26-haz-emp]  ·  6.2 El modelo de riesgos a trozos como GLM de Poisson > Qué dicen los datos antes de modelar
# -----------------------------------------------------------------------------
pp |>
  dplyr::group_by(tramo) |>
  dplyr::summarise(hazard = mean(y), .groups = "drop") |>
  ggplot(aes(as.integer(tramo), hazard)) +
  geom_line(linewidth = 0.8, colour = "steelblue") +
  geom_point(size = 2.5, colour = "steelblue") +
  labs(x = "tramo de antigüedad", y = "hazard empírico (eventos / en riesgo)")

# -----------------------------------------------------------------------------
# [u26-eda-prep]  ·  🔧 En R. Calcular supervivencia y hazard
# -----------------------------------------------------------------------------
vars_cont <- c("edad_conductor", "antiguedad_carnet")
vars_cate <- c("sexo", "tipo_vehiculo", "zona_circulacion")

cartera_eda <- cartera |>
  dplyr::mutate(dplyr::across(dplyr::all_of(vars_cont),
                              \(x) factor(dplyr::ntile(x, 5)),
                              .names = "{.col}_q"))
vars_eda <- c(paste0(vars_cont, "_q"), vars_cate)

pp_eda <- pp |>
  dplyr::left_join(
    dplyr::select(cartera_eda, id_poliza, dplyr::all_of(paste0(vars_cont, "_q"))),
    by = "id_poliza"
  )

km_all <- purrr::map_dfr(vars_eda, \(v) {
  f <- stats::as.formula(paste("Surv(tiempo_primer_sin, evento) ~", v))
  broom::tidy(survfit(f, data = cartera_eda)) |>
    dplyr::mutate(predictor = v, nivel = sub("^[^=]+=", "", strata))
})

# -----------------------------------------------------------------------------
# [fig-u26-eda-km]  ·  🔧 En R. Calcular supervivencia y hazard
# -----------------------------------------------------------------------------
patchwork::wrap_plots(
  purrr::map(vars_eda, \(v) {
    km_all |>
      dplyr::filter(predictor == v) |>
      ggplot(aes(time, estimate, colour = nivel)) +
      geom_step(linewidth = 0.7) +
      labs(title = v, x = "tramo", y = "S(t)", colour = NULL) +
      theme(plot.title = element_text(size = 10),
            legend.key.size = unit(0.35, "cm"),
            legend.text = element_text(size = 8))
  }),
  ncol = 3
)

# -----------------------------------------------------------------------------
# [fig-u26-eda-haz]  ·  🔧 En R. Calcular supervivencia y hazard
# -----------------------------------------------------------------------------
purrr::map_dfr(vars_eda, \(v) pp_eda |>
  dplyr::group_by(nivel = as.character(.data[[v]])) |>
  dplyr::summarise(hazard = mean(y), .groups = "drop") |>
  dplyr::mutate(predictor = v)) |>
  ggplot(aes(nivel, hazard)) +
  geom_hline(yintercept = mean(pp$y), linetype = 2, colour = "grey50") +
  geom_point(size = 2.5, colour = "steelblue") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) +
  labs(x = NULL, y = "hazard empírico (media de y)")

# -----------------------------------------------------------------------------
# [fig-u26-eda-loglog]  ·  🔧 En R. Calcular supervivencia y hazard
# -----------------------------------------------------------------------------
patchwork::wrap_plots(
  purrr::map(vars_eda, \(v) {
    km_all |>
      dplyr::filter(predictor == v, estimate > 0, estimate < 1) |>
      ggplot(aes(time, log(-log(estimate)), colour = nivel)) +
      geom_step(linewidth = 0.7) +
      labs(title = v, x = "tramo", y = "log(-log S)", colour = NULL) +
      theme(plot.title = element_text(size = 10),
            legend.key.size = unit(0.35, "cm"),
            legend.text = element_text(size = 8))
  }),
  ncol = 3
)

# -----------------------------------------------------------------------------
# [u26-pw]  ·  🔧 En R. Calcular supervivencia y hazard > Ajuste e interpretación
# -----------------------------------------------------------------------------
m_pw <- glm(
  y ~ tramo + edad_conductor + antiguedad_carnet + sexo + tipo_vehiculo + zona_circulacion,
  family = poisson, data = pp
)
broom::tidy(m_pw, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(!grepl("^tramo", term))

# -----------------------------------------------------------------------------
# [u26-ph-check]  ·  🔧 En R. El modelo de riesgos a trozos como glm(poisson) > Bondad de ajuste, diagnóstico y predicción
# -----------------------------------------------------------------------------
m_int <- update(m_pw, . ~ . + tramo:zona_circulacion)
anova(m_pw, m_int, test = "Chisq")

# -----------------------------------------------------------------------------
# [fig-u26-pw-surv]  ·  🔧 En R. Contrastar la proporcionalidad (modelo a trozos)
# -----------------------------------------------------------------------------
perfil <- pp[rep(1, nlevels(pp$tramo)), ]
perfil$tramo <- factor(levels(pp$tramo), levels = levels(pp$tramo))

h_hat <- predict(m_pw, newdata = perfil, type = "response")   # hazard por tramo (t = 1)
curva <- tibble::tibble(
  tramo  = seq_along(h_hat),
  hazard = as.numeric(h_hat),
  S      = exp(-cumsum(h_hat))
)

curva |>
  tidyr::pivot_longer(c(hazard, S), names_to = "curva", values_to = "valor") |>
  dplyr::mutate(curva = factor(curva, c("hazard", "S"),
                               c("hazard estimado por tramo", "supervivencia S(t)"))) |>
  ggplot(aes(tramo, valor)) +
  geom_step(direction = "hv", linewidth = 0.8, colour = "steelblue") +
  geom_point(size = 2, colour = "steelblue") +
  facet_wrap(~ curva, scales = "free_y") +
  labs(x = "tramo de antigüedad", y = NULL)

# -----------------------------------------------------------------------------
# [u26-pw-valida]  ·  🔧 En R. Contrastar la proporcionalidad (modelo a trozos) > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad    <- attr(cartera, "verdad")
razon_dgp <- verdad$h0_tramo / verdad$h0_tramo[1]
razon_est <- c(1, exp(coef(m_pw)[grep("^tramo", names(coef(m_pw)))]))

tibble::tibble(
  tramo     = seq_along(razon_dgp),
  estimada  = round(as.numeric(razon_est), 2),
  verdadera = round(razon_dgp, 2)
)

# -----------------------------------------------------------------------------
# [u26-pw-valida-beta]  ·  🔧 En R. Contrastar la proporcionalidad (modelo a trozos) > Validación contra el DGP
# -----------------------------------------------------------------------------
b <- setNames(verdad$betas$evento, verdad$nombres_beta)

# Continuas: el DGP las estandarizó -> llevamos nuestro coeficiente a esa escala
cont <- c(edad_conductor = "z_edad", antiguedad_carnet = "z_exp")
sd_v <- vapply(names(cont), \(v) sd(cartera[[v]]), numeric(1))

# Categóricas; 'sexo' no interviene en el DGP del evento, así que su valor verdadero es 0
cate <- c(grep("^sexo", names(coef(m_pw)), value = TRUE),
          "tipo_vehiculomoto", "tipo_vehiculofurgoneta",
          "zona_circulacionmixta", "zona_circulacionrural")

dplyr::bind_rows(
  tibble::tibble(
    termino   = names(cont),
    estimado  = as.numeric(coef(m_pw)[names(cont)] * sd_v),
    verdadero = as.numeric(b[cont])
  ),
  tibble::tibble(
    termino   = cate,
    estimado  = as.numeric(coef(m_pw)[cate]),
    # el DGP mide la zona frente a 'rural'; R, frente a 'urbana'
    verdadero = as.numeric(c(0, b["moto"], b["furgoneta"],
                             b["mixta"] - b["urbana"], -b["urbana"]))
  )
) |>
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3)))

# -----------------------------------------------------------------------------
# [u26-cloglog]  ·  6.3 Dos lentes del mismo hazard: Poisson y cloglog > Ajuste y comparación
# -----------------------------------------------------------------------------
m_cll <- glm(
  y ~ tramo + edad_conductor + antiguedad_carnet + sexo + tipo_vehiculo + zona_circulacion,
  family = binomial(link = "cloglog"), data = pp
)

comp <- dplyr::inner_join(
  broom::tidy(m_pw)  |> dplyr::select(term, poisson = estimate),
  broom::tidy(m_cll) |> dplyr::select(term, cloglog = estimate),
  by = "term"
) |>
  dplyr::mutate(diferencia = cloglog - poisson,
                dplyr::across(where(is.numeric), \(x) round(x, 3)))
comp

# -----------------------------------------------------------------------------
# [u26-cox]  ·  6.4 ¿Comparable con los clásicos? Cox y Kaplan–Meier > Cox: el mismo efecto, otro trato del hazard base
# -----------------------------------------------------------------------------
m_cox <- coxph(
  Surv(tiempo_primer_sin, evento) ~ edad_conductor + antiguedad_carnet + sexo +
    tipo_vehiculo + zona_circulacion,
  data = cartera, ties = "breslow"
)

comp_cox <- dplyr::inner_join(
  broom::tidy(m_pw) |> dplyr::select(term, poisson = estimate),
  broom::tidy(m_cox) |> dplyr::select(term, cox = estimate),
  by = "term"
) |>
  dplyr::mutate(diferencia = cox - poisson,
                dplyr::across(where(is.numeric), \(x) round(x, 3)))
comp_cox

# -----------------------------------------------------------------------------
# [u26-zph]  ·  🔧 En R. Contrastar la proporcionalidad en un modelo de Cox
# -----------------------------------------------------------------------------
cox.zph(m_cox)

# -----------------------------------------------------------------------------
# [fig-u26-zph]  ·  🔧 En R. Contrastar la proporcionalidad en un modelo de Cox
# -----------------------------------------------------------------------------
par(mfrow = c(2, 3))
plot(cox.zph(m_cox))
par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# [fig-u26-km-vs-pw]  ·  🔧 En R. Contrastar la proporcionalidad en un modelo de Cox > Kaplan–Meier: la curva sin modelo frente a la del modelo
# -----------------------------------------------------------------------------
m_base <- glm(y ~ 0 + tramo, family = poisson, data = pp)   # solo el hazard base
S_pw   <- exp(-cumsum(exp(coef(m_base))))

km0 <- survfit(Surv(tiempo_primer_sin, evento) ~ 1, data = cartera)

dplyr::bind_rows(
  broom::tidy(km0) |> dplyr::transmute(tramo = time, S = estimate, metodo = "Kaplan–Meier"),
  tibble::tibble(tramo = seq_along(S_pw), S = as.numeric(S_pw), metodo = "Poisson a trozos")
) |>
  ggplot(aes(tramo, S, colour = metodo)) +
  geom_step(direction = "hv", linewidth = 0.8) +
  geom_point(size = 2) +
  labs(x = "tramo de antigüedad", y = "supervivencia estimada  S(t)", colour = NULL)

# -----------------------------------------------------------------------------
# [u26-frailty]  ·  6.5 Fragilidad ≡ GLMM de Poisson
# -----------------------------------------------------------------------------
m_frag <- glmer(
  y ~ tramo + edad_conductor + antiguedad_carnet + sexo + tipo_vehiculo + zona_circulacion +
    (1 | agencia),
  family = poisson, data = pp
)
sd_frag <- as.data.frame(VarCorr(m_frag))$sdcor[1]
c(sigma_agencia = sd_frag)

# -----------------------------------------------------------------------------
# [u26-bookend]  ·  6.6 Conteos y supervivencia: dos caras de la tasa
# -----------------------------------------------------------------------------
c(poisson_en_cero = dpois(0, 0.3), exp_supervivencia = exp(-0.3))


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
