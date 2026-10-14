# =============================================================================
# Caso 2 · Unidad 2.2 — 2 · Modelos log-lineales para tablas de contingencia
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_2.qmd.
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
  DHARMa = "0.5.0", MASS = "7.3.65", broom = "1.0.13", dplyr = "1.2.1",
  glmmTMB = "1.1.14", glmnet = "5.0", lme4 = "2.0.1",
  marginaleffects = "0.32.0", patchwork = "1.3.2", performance = "0.17.0",
  pscl = "1.5.9", purrr = "1.2.2", sessioninfo = "1.2.4", survival = "3.8.6",
  tibble = "3.3.1", tidyverse = "2.0.0", vcd = "1.4.13", vcdExtra = "0.9.6")
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
library(glmnet)       # regularización (2.7)
library(vcd)           # mosaicos para tablas de contingencia (2.2)
library(vcdExtra)      # zero-inflated (2.4) y utilidades de tablas (2.2)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

source(url_glm("caso2/R/dgp_conteos.R"))   # funciones del proceso generador
cartera <- leer_datos_glm("caso2/datos/cartera_auto_20252026.rds")   # cartera de auto del curso
glimpse(cartera)
head(cartera)

# -----------------------------------------------------------------------------
# [u22-de-ficha-a-tabla]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > La tabla zona × asistencia
# -----------------------------------------------------------------------------
# Trabajamos sobre una copia con el indicador de asistencia; no modificamos `cartera`, que las
# unidades siguientes siguen usando tal cual. El nivel "con" va segundo: será el "éxito".
cart_tab <- cartera |>
  dplyr::mutate(asistencia = factor(ifelse(n_asistencia > 0, "con", "sin"), levels = c("sin", "con")))
tab2 <- xtabs(~ zona_circulacion + asistencia, data = cart_tab)
addmargins(tab2)

# -----------------------------------------------------------------------------
# [tbl-u22-pct-fila]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > La tabla zona × asistencia
# -----------------------------------------------------------------------------
round(100 * prop.table(tab2, margin = 1), 1) |>
  as.data.frame.matrix() |>
  tibble::rownames_to_column("zona")

# -----------------------------------------------------------------------------
# [u22-gl]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > Grados de libertad: contar celdas y contar parámetros
# -----------------------------------------------------------------------------
d2 <- as.data.frame(tab2)                                   # una fila por celda; columna Freq
m_ind <- glm(Freq ~ zona_circulacion + asistencia, family = poisson, data = d2)
c(celdas = nrow(d2),
  parametros_independencia = length(coef(m_ind)),
  parametros_saturado      = ncol(model.matrix(~ zona_circulacion * asistencia, data = d2)),
  gl_independencia         = df.residual(m_ind))

# -----------------------------------------------------------------------------
# [tbl-u22-esperadas]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > El modelo sin interacción: independencia
# -----------------------------------------------------------------------------
esperadas <- outer(rowSums(tab2), colSums(tab2)) / sum(tab2)
cbind(as.data.frame.matrix(tab2), round(esperadas, 1)) |>
  setNames(c("obs_sin", "obs_con", "esp_sin", "esp_con")) |>
  tibble::rownames_to_column("zona") |>
  dplyr::mutate(ajustado_glm_con = round(fitted(m_ind)[d2$asistencia == "con"], 1))

# -----------------------------------------------------------------------------
# [u22-dosvias-fit]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > El contraste: ¿hace falta la interacción?
# -----------------------------------------------------------------------------
m_sat <- glm(Freq ~ zona_circulacion * asistencia, family = poisson, data = d2)
anova(m_ind, m_sat, test = "LRT")         # H0: independencia (estadístico G2 de razón de verosimilitudes)
chisq.test(tab2, correct = FALSE)         # el chi-cuadrado clásico de Pearson, para comparar

# -----------------------------------------------------------------------------
# [u22-dosvias-gof]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > El contraste: ¿hace falta la interacción?
# -----------------------------------------------------------------------------
G2 <- deviance(m_ind)                              # = LRT frente al saturado, cuya deviance es 0
X2 <- sum(residuals(m_ind, type = "pearson")^2)    # = estadístico de chisq.test()
gl <- df.residual(m_ind)
round(c(G2 = G2, p_G2 = pchisq(G2, gl, lower.tail = FALSE),
        X2 = X2, p_X2 = pchisq(X2, gl, lower.tail = FALSE),
        gl = gl, min_esperada = min(fitted(m_ind))), 4)

# -----------------------------------------------------------------------------
# [u22-or-a-mano]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > Leer la asociación: odds ratios
# -----------------------------------------------------------------------------
mu   <- xtabs(fitted(m_sat) ~ zona_circulacion + asistencia, data = d2)   # ajustadas, en forma de tabla
odds <- mu[, "con"] / mu[, "sin"]                                         # odds de asistencia por zona
round(c(odds_urbana = odds[["urbana"]], odds_mixta = odds[["mixta"]], odds_rural = odds[["rural"]],
        OR_mixta_vs_urbana = odds[["mixta"]] / odds[["urbana"]],
        OR_rural_vs_urbana = odds[["rural"]] / odds[["urbana"]]), 3)

# -----------------------------------------------------------------------------
# [u22-dosvias-or]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > Leer la asociación: odds ratios
# -----------------------------------------------------------------------------
broom::tidy(m_sat, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(grepl(":", term)) |>
  dplyr::select(term, estimate, conf.low, conf.high)

# -----------------------------------------------------------------------------
# [u22-puente-logit]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > El puente con la logística
# -----------------------------------------------------------------------------
m_logit <- glm(asistencia ~ zona_circulacion, family = binomial, data = cart_tab)
rbind(loglineal = coef(m_sat)[grepl(":", names(coef(m_sat)))],
      logistica = coef(m_logit)[-1]) |> round(4)

# -----------------------------------------------------------------------------
# [u22-residuos-2v]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.1 Tablas de dos vías: ¿asociación o independencia?
#       > Dónde está la asociación: residuos ajustados
# -----------------------------------------------------------------------------
d2 |> dplyr::mutate(esperado = round(fitted(m_ind), 1),
                    r_ajustado = round(rstandard(m_ind, type = "pearson"), 2))

# -----------------------------------------------------------------------------
# [u22-tab3]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > La tabla zona × tipo × asistencia
# -----------------------------------------------------------------------------
tab3 <- xtabs(~ zona_circulacion + tipo_vehiculo + asistencia, data = cart_tab)
d3   <- as.data.frame(tab3)
ftable(tab3)

# -----------------------------------------------------------------------------
# [fig-u22-grafos]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Un modelo, un grafo
# -----------------------------------------------------------------------------
# Posiciones fijas de los nodos: el mismo triángulo en todos los paneles, para comparar a simple vista
nodos_abc <- tibble::tibble(nodo = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 0.87))
dibuja_grafo <- function(aristas, titulo, triple = FALSE, nodos = nodos_abc) {
  seg <- purrr::map_dfr(aristas, \(par) tibble::tibble(
    x = nodos$x[nodos$nodo == par[1]], y = nodos$y[nodos$nodo == par[1]],
    xend = nodos$x[nodos$nodo == par[2]], yend = nodos$y[nodos$nodo == par[2]]))
  g <- ggplot()
  if (triple) g <- g + geom_polygon(data = nodos, aes(x, y), fill = "steelblue", alpha = 0.2)
  if (nrow(seg) > 0) g <- g + geom_segment(data = seg, aes(x, y, xend = xend, yend = yend), linewidth = 1)
  g + geom_point(data = nodos, aes(x, y), size = 11, colour = "steelblue") +
    geom_text(data = nodos, aes(x, y, label = nodo), colour = "white", fontface = "bold") +
    coord_equal(xlim = c(-0.25, 1.25), ylim = c(-0.25, 1.1)) +
    labs(title = titulo) + theme_void() + theme(plot.title = element_text(size = 10, hjust = 0.5))
}
patchwork::wrap_plots(
  dibuja_grafo(list(), "Mutua [A][B][C]"),
  dibuja_grafo(list(c("A", "B")), "Conjunta [AB][C]"),
  dibuja_grafo(list(c("A", "C"), c("B", "C")), "Condicional [AC][BC]"),
  dibuja_grafo(list(c("A", "B"), c("A", "C"), c("B", "C")), "Homogénea [AB][AC][BC]"),
  dibuja_grafo(list(c("A", "B"), c("A", "C"), c("B", "C")), "Saturado [ABC]", triple = TRUE),
  nrow = 1)

# -----------------------------------------------------------------------------
# [u22-jerarquia]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Ajustar los cinco modelos y elegir
# -----------------------------------------------------------------------------
f <- function(fml) glm(fml, family = poisson, data = d3)
mods <- list(
  mutua       = f(Freq ~ zona_circulacion + tipo_vehiculo + asistencia),
  conjunta    = f(Freq ~ zona_circulacion * tipo_vehiculo + asistencia),
  condicional = f(Freq ~ zona_circulacion * asistencia + tipo_vehiculo * asistencia),
  homogenea   = f(Freq ~ (zona_circulacion + tipo_vehiculo + asistencia)^2),
  saturado    = f(Freq ~ zona_circulacion * tipo_vehiculo * asistencia))

purrr::map_dfr(names(mods), ~ {
  m <- mods[[.x]]; gl <- df.residual(m)
  tibble::tibble(
    modelo   = .x,
    gl       = gl,
    deviance = round(deviance(m), 2),
    # el saturado no tiene gl residuales: no hay contraste de ajuste que hacerle
    p_ajuste = if (gl > 0) round(pchisq(deviance(m), gl, lower.tail = FALSE), 4) else NA_real_,
    AIC      = round(AIC(m), 1))})

# -----------------------------------------------------------------------------
# [u22-lrt-jerarquia]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Ajustar los cinco modelos y elegir
# -----------------------------------------------------------------------------
anova(mods$condicional, mods$homogenea, test = "LRT")     # ¿hay asociación zona-tipo dada la asistencia?

# -----------------------------------------------------------------------------
# [u22-drop1-condicional]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Responder a las tres preguntas
# -----------------------------------------------------------------------------
drop1(mods$condicional, test = "LRT")

# -----------------------------------------------------------------------------
# [u22-residuos]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Diagnóstico: residuos y mosaico
# -----------------------------------------------------------------------------
d3$r_aj <- rstandard(mods$condicional, type = "pearson")   # residuos AJUSTADOS
d3 |>
  dplyr::mutate(esperado = round(fitted(mods$condicional), 1), r_aj = round(r_aj, 2)) |>
  dplyr::arrange(dplyr::desc(abs(r_aj))) |>
  head(6)

# -----------------------------------------------------------------------------
# [fig-u22-mosaico-mutua]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > El mosaico
# -----------------------------------------------------------------------------
vcd::mosaic(tab3, shade = TRUE, legend = TRUE,   # por defecto: ~ zona + tipo + asistencia
            main = "Independencia mutua",
            labeling_args = list(rot_labels = c(bottom = 0, right = 0)))

# -----------------------------------------------------------------------------
# [fig-u22-mosaico-condicional]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > El mosaico
# -----------------------------------------------------------------------------
vcd::mosaic(tab3, shade = TRUE, legend = TRUE,
            expected = ~ zona_circulacion * asistencia + tipo_vehiculo * asistencia,
            main = "Independencia condicional",
            labeling_args = list(rot_labels = c(bottom = 0, right = 0)))

# -----------------------------------------------------------------------------
# [u22-berkeley-datos]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
ucb <- UCBAdmissions
dimnames(ucb)                    # dimensión 1, 2 y 3, en este orden, con sus niveles
sum(ucb)                         # solicitudes en total
ftable(ucb, row.vars = "Dept", col.vars = c("Gender", "Admit"))   # las 24 celdas, departamento a departamento

# -----------------------------------------------------------------------------
# [u22-berkeley-marginal]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
or2 <- function(m) (m[1, 1] * m[2, 2]) / (m[1, 2] * m[2, 1])
round(100 * prop.table(margin.table(ucb, c(2, 1)), 1), 1)   # % admitido por sexo
c(OR_marginal_hombres_vs_mujeres = round(or2(margin.table(ucb, c(1, 2))), 2))

# -----------------------------------------------------------------------------
# [u22-berkeley-condicional]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
round(apply(ucb, 3, or2), 2)                                # OR hombres vs mujeres dentro de cada departamento

# -----------------------------------------------------------------------------
# [u22-berkeley-dos-vs-tres]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
m_ucb2 <- glm(Freq ~ Admit * Gender, family = poisson,
              data = as.data.frame(margin.table(ucb, c(1, 2))))   # tabla colapsada, 4 celdas
m_ucb3 <- glm(Freq ~ (Admit + Gender + Dept)^2, family = poisson,
              data = as.data.frame(ucb))                          # tres vías, OR común
dplyr::bind_rows(marginal_2vias    = broom::tidy(m_ucb2, exponentiate = TRUE, conf.int = TRUE),
                 condicional_3vias = broom::tidy(m_ucb3, exponentiate = TRUE, conf.int = TRUE),
                 .id = "modelo") |>
  dplyr::filter(term == "AdmitRejected:GenderFemale") |>
  dplyr::select(modelo, OR = estimate, conf.low, conf.high)

# -----------------------------------------------------------------------------
# [u22-berkeley-lrt-AdS]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
anova(update(m_ucb2, . ~ Admit + Gender), m_ucb2, test = "LRT")                # tabla colapsada
anova(update(m_ucb3, . ~ Admit * Dept + Gender * Dept), m_ucb3, test = "LRT")  # dentro de los departamentos

# -----------------------------------------------------------------------------
# [u22-berkeley-mecanismo]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
round(100 * prop.table(margin.table(ucb, c(2, 3)), 1), 1)  # a qué departamentos solicita cada sexo (% por fila)
round(100 * prop.table(margin.table(ucb, c(1, 3)), 2)[1, ], 1)   # % admitido en cada departamento

# -----------------------------------------------------------------------------
# [fig-u22-berkeley-grafo]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
nodos_ucb <- tibble::tibble(nodo = c("S", "Ad", "D"), x = c(0, 1, 0.5), y = c(0, 0, 0.87))
dibuja_grafo(list(c("S", "D"), c("Ad", "D")), "Berkeley", nodos = nodos_ucb) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), linetype = "dashed", colour = "grey50")

# -----------------------------------------------------------------------------
# [u22-colapsabilidad]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.2 Tres vías: ¿quién se asocia con quién?
#       > ¿Y en nuestra cartera?
# -----------------------------------------------------------------------------
or_zona <- function(m) exp(coef(m)[grep("^zona_circulacion.*:asistenciacon$", names(coef(m)))])
tibble::tibble(comparacion          = c("mixta vs urbana", "rural vs urbana"),
               OR_marginal_2vias    = or_zona(m_sat),
               OR_condicional_3vias = or_zona(mods$condicional)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

# -----------------------------------------------------------------------------
# [u22-tab4]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.3 Más de tres vías
#       > Las preguntas
# -----------------------------------------------------------------------------
tab4 <- xtabs(~ zona_circulacion + tipo_vehiculo + uso + asistencia, data = cart_tab)
d4   <- as.data.frame(tab4)
ftable(tab4, row.vars = c("zona_circulacion", "tipo_vehiculo"))

# -----------------------------------------------------------------------------
# [u22-cuatro-vias]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.3 Más de tres vías
#       > Los modelos, por orden de interacción
# -----------------------------------------------------------------------------
ordenes <- list(
  "1 · solo principales"  = Freq ~ zona_circulacion + tipo_vehiculo + uso + asistencia,
  "2 · todas las dobles"  = Freq ~ (zona_circulacion + tipo_vehiculo + uso + asistencia)^2,
  "3 · todas las triples" = Freq ~ (zona_circulacion + tipo_vehiculo + uso + asistencia)^3,
  "4 · saturado"          = Freq ~ zona_circulacion * tipo_vehiculo * uso * asistencia)
m4v <- purrr::map(ordenes, \(fml) glm(fml, family = poisson, data = d4))

purrr::imap_dfr(m4v, \(m, nombre) {
  gl <- df.residual(m)
  tibble::tibble(orden = nombre, parametros = length(coef(m)), gl = gl,
                 deviance = round(deviance(m), 2),
                 # el saturado no tiene gl residuales: no hay contraste de ajuste que hacerle
                 p_ajuste = if (gl > 0) round(pchisq(deviance(m), gl, lower.tail = FALSE), 4)
                            else NA_real_)})

# -----------------------------------------------------------------------------
# [u22-cuatro-vias-ordenes]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.3 Más de tres vías
#       > Elegir el orden: contrastar órdenes consecutivos
# -----------------------------------------------------------------------------
anova(m4v[[1]], m4v[[2]], m4v[[3]], m4v[[4]], test = "LRT")   # cada fila: ¿aporta el orden siguiente?

# -----------------------------------------------------------------------------
# [u22-cuatro-vias-poda]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.3 Más de tres vías
#       > Podar dentro del orden elegido
# -----------------------------------------------------------------------------
drop1(m4v[[2]], test = "LRT")    # cada doble, quitada de una en una del modelo de orden 2

# -----------------------------------------------------------------------------
# [u22-cuatro-vias-final]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.3 Más de tres vías
#       > Podar dentro del orden elegido
# -----------------------------------------------------------------------------
m4_final <- glm(Freq ~ zona_circulacion * asistencia + tipo_vehiculo * asistencia + uso * asistencia,
                family = poisson, data = d4)
anova(m4_final, m4v[[2]], test = "LRT")                  # ¿sobran a la vez las tres dobles descartadas?
c(deviance = deviance(m4_final), gl = df.residual(m4_final),
  p_ajuste = pchisq(deviance(m4_final), df.residual(m4_final), lower.tail = FALSE)) |> round(3)
drop1(m4_final, test = "LRT")                            # y lo que queda, ¿sigue haciendo falta?
broom::tidy(m4_final, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(grepl(":", term)) |>
  dplyr::select(term, OR = estimate, conf.low, conf.high)

# -----------------------------------------------------------------------------
# [fig-u22-grafo-4vias]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.3 Más de tres vías
#       > Podar dentro del orden elegido
# -----------------------------------------------------------------------------
nodos_abcd <- tibble::tibble(nodo = c("A", "B", "C", "D"), x = c(0, 1, 0.5, 0.5), y = c(0, 0, 1, 0.38))
dibuja_grafo(list(c("A", "D"), c("B", "D"), c("C", "D")), "[AD][BD][CD]", nodos = nodos_abcd)

# -----------------------------------------------------------------------------
# [u22-escasez]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.3 Más de tres vías
#       > Tablas escasas y ceros
# -----------------------------------------------------------------------------
c(celdas = length(tab4), minimo = min(tab4),
  menores_de_5 = sum(tab4 < 5), vacias = sum(tab4 == 0))

# -----------------------------------------------------------------------------
# [u22-cuadrada]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Los datos: de las pólizas a la tabla de cambio
# -----------------------------------------------------------------------------
cuad <- as.data.frame(xtabs(~ bonus_malus_prev + bonus_malus_act, data = cartera)) |>
  dplyr::rename(prev = bonus_malus_prev, act = bonus_malus_act) |>
  dplyr::mutate(i = as.integer(prev), j = as.integer(act),
                par = factor(paste(pmin(i, j), pmax(i, j), sep = "-")))
dplyr::filter(cuad, par %in% c("1-2", "2-2"))   # dos celdas espejo y una de la diagonal

# -----------------------------------------------------------------------------
# [u22-cuadrada-tabla]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > La tabla y sus marginales
# -----------------------------------------------------------------------------
xtabs(Freq ~ prev + act, data = cuad) |> addmargins()

# -----------------------------------------------------------------------------
# [fig-u22-cambio-heatmap]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > La tabla y sus marginales
# -----------------------------------------------------------------------------
N <- xtabs(Freq ~ prev + act, data = cuad)
pct_fila <- as.data.frame(as.table(100 * prop.table(N, 1)))
neto     <- as.data.frame(as.table(N - t(N))) |>
  # cada par una sola vez (i < j): así el signo dice el sentido, + hacia malus, − hacia bonus
  dplyr::mutate(Freq = ifelse(as.integer(prev) < as.integer(act), Freq, NA))
mapa <- function(d, relleno, titulo, etiqueta) {
  ggplot(d, aes(act, prev, fill = Freq)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = round(Freq)), na.rm = TRUE) +
    relleno +
    scale_y_discrete(limits = rev) +            # nivel 1 arriba: la diagonal baja de izquierda a derecha
    coord_equal() +
    labs(title = titulo, x = "nivel actual", y = "nivel previo", fill = etiqueta) +
    theme_minimal()
}
patchwork::wrap_plots(
  mapa(pct_fila, scale_fill_gradient(low = "white", high = "steelblue"),
       "Adónde van (% de la fila)", "%"),
  mapa(neto, scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                                   na.value = "grey95"),
       "Flujo neto: suben − bajan", "pólizas"))

# -----------------------------------------------------------------------------
# [tbl-u22-cambio-marg]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > La tabla y sus marginales
# -----------------------------------------------------------------------------
pct_prev <- 100 * prop.table(table(cartera$bonus_malus_prev))
pct_act  <- 100 * prop.table(table(cartera$bonus_malus_act))
rbind(previo = pct_prev, actual = pct_act, cambio = pct_act - pct_prev) |>
  round(1) |>
  as.data.frame.matrix() |>
  tibble::rownames_to_column("año")

# -----------------------------------------------------------------------------
# [u22-simetria]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Simetría
# -----------------------------------------------------------------------------
m_sim <- glm(Freq ~ par, family = poisson, data = cuad)
c(deviance = round(deviance(m_sim), 2), gl = df.residual(m_sim),
  p = signif(pchisq(deviance(m_sim), df.residual(m_sim), lower.tail = FALSE), 3))

# -----------------------------------------------------------------------------
# [u22-cuasisimetria]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Cuasi-simetría y homogeneidad marginal
# -----------------------------------------------------------------------------
m_qs <- glm(Freq ~ prev + act + par, family = poisson, data = cuad)
c(deviance = round(deviance(m_qs), 2), gl = df.residual(m_qs),
  p = signif(pchisq(deviance(m_qs), df.residual(m_qs), lower.tail = FALSE), 3))

# -----------------------------------------------------------------------------
# [u22-homogeneidad-marginal]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Cuasi-simetría y homogeneidad marginal
# -----------------------------------------------------------------------------
anova(m_sim, m_qs, test = "LRT")   # H0: homogeneidad marginal (dada la cuasi-simetría)

# -----------------------------------------------------------------------------
# [u22-deriva]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Cuánta deriva, y en qué sentido
# -----------------------------------------------------------------------------
niv <- as.integer(cartera$bonus_malus_prev); nva <- as.integer(cartera$bonus_malus_act)
c(nivel_medio_previo = round(mean(niv), 3),
  nivel_medio_actual = round(mean(nva), 3),
  deriva             = round(mean(nva - niv), 3),
  pct_empeora        = round(100 * mean(nva > niv), 1),
  pct_mejora         = round(100 * mean(nva < niv), 1))

# -----------------------------------------------------------------------------
# [u22-ordinal]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Asociación ordinal: aprovechar que los niveles están ordenados
# -----------------------------------------------------------------------------
m_indep_c <- glm(Freq ~ prev + act,               family = poisson, data = cuad)
m_unif    <- glm(Freq ~ prev + act + I(i * j),    family = poisson, data = cuad)
anova(m_indep_c, m_unif, test = "LRT")
c(beta_asociacion = round(coef(m_unif)["I(i * j)"], 4),
  OR_local        = round(exp(coef(m_unif)["I(i * j)"]), 3))

# -----------------------------------------------------------------------------
# [u22-ordinal-ajuste]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Asociación ordinal: aprovechar que los niveles están ordenados
# -----------------------------------------------------------------------------
c(deviance  = round(deviance(m_unif), 1), gl = df.residual(m_unif),
  p         = signif(pchisq(deviance(m_unif), df.residual(m_unif), lower.tail = FALSE), 3),
  chi2_q95  = round(qchisq(0.95, df.residual(m_unif)), 1))   # lo máximo «razonable» si el modelo fuera cierto

# -----------------------------------------------------------------------------
# [u22-movilidad]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Inercia: las pólizas que no cambian de nivel
# -----------------------------------------------------------------------------
cuad$diagonal <- factor(ifelse(cuad$i == cuad$j, "queda", "mueve"))
m_mov <- glm(Freq ~ prev + act + I(i * j) + diagonal, family = poisson, data = cuad)
anova(m_unif, m_mov, test = "LRT")
c(deviance      = round(deviance(m_mov), 1), gl = df.residual(m_mov),
  pct_se_quedan = round(100 * sum(cuad$Freq[cuad$i == cuad$j]) / sum(cuad$Freq), 1))
# Solo los dos términos que interesan; los de prev y act únicamente reproducen los marginales
broom::tidy(m_mov, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(term %in% c("I(i * j)", "diagonalqueda")) |>
  dplyr::mutate(efecto = c("gradiente (e^beta)", "inercia (e^delta)")) |>
  dplyr::select(efecto, estimate, conf.low, conf.high, p.value) |>
  dplyr::mutate(dplyr::across(c(estimate, conf.low, conf.high), ~ round(.x, 3))) |>
  as.data.frame()                  # para ver los tres decimales

# -----------------------------------------------------------------------------
# [u22-validacion-tablas]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(cartera, "verdad")
# Predictor lineal verdadero, construido como en R/dgp_conteos.R (continuas estandarizadas; rural y turismo, referencias)
X <- with(cartera, cbind(1, scale(edad_conductor), scale(antiguedad_carnet), scale(potencia_cv),
                         scale(antiguedad_vehiculo), zona_circulacion == "urbana",
                         zona_circulacion == "mixta", uso == "comercial", tipo_vehiculo == "moto",
                         tipo_vehiculo == "furgoneta"))
p_con <- 1 - exp(-cartera$exposicion * exp(drop(X %*% verdad$betas$limpio)))   # P(al menos una asistencia)

d4_dgp <- dplyr::bind_rows(dplyr::mutate(cartera, asistencia = "con", Freq = p_con),
                           dplyr::mutate(cartera, asistencia = "sin", Freq = 1 - p_con)) |>
  dplyr::mutate(asistencia = factor(asistencia, levels = c("sin", "con"))) |>
  dplyr::count(zona_circulacion, tipo_vehiculo, uso, asistencia, wt = Freq, name = "Freq")
# Frecuencias esperadas, no enteras: quasipoisson da las mismas estimaciones que poisson, sin avisos
m4_dgp <- glm(formula(m4_final), family = quasipoisson, data = d4_dgp)

broom::tidy(m4_final, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(grepl(":", term)) |>
  dplyr::transmute(term, OR_DGP = exp(coef(m4_dgp))[term], OR_estimado = estimate, conf.low, conf.high,
                   dentro_IC = OR_DGP >= conf.low & OR_DGP <= conf.high) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

# -----------------------------------------------------------------------------
# [u22-validacion]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
c(persistencia = verdad$persistencia)

# ¿Saltos cortos? Distancia de los cambios observados frente a la que darían saltos al azar
# niv y nva (nivel previo y actual) vienen del chunk de la deriva
mueve   <- nva != niv
p_prev  <- prop.table(table(factor(niv[mueve], levels = 1:5)))    # de dónde parten los que cambian
al_azar <- sapply(1:4, \(d) sum(sapply(1:5, \(i) p_prev[[i]] * sum(abs((1:5)[-i] - i) == d) / 4)))
rbind(observado = prop.table(table(factor(abs(nva - niv)[mueve], levels = 1:4))),
      al_azar   = al_azar) |> round(3)

# -----------------------------------------------------------------------------
# [u22-validacion-qs]
#   2 · Modelos log-lineales para tablas de contingencia
#     > 2.4 Tablas de cambio: el bonus-malus
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
xtabs(round(rstandard(m_qs, type = "pearson"), 2) ~ prev + act, data = cuad)   # filas: nivel previo


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
