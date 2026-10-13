# =============================================================================
# Caso 2 · Unidad 2.2 — 2 · Modelos log-lineales: cuando el dato es la celda
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
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.1 De la póliza a la celda
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
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.1 De la póliza a la celda
#       > La tabla zona × asistencia
# -----------------------------------------------------------------------------
round(100 * prop.table(tab2, margin = 1), 1) |>
  as.data.frame.matrix() |>
  tibble::rownames_to_column("zona")

# -----------------------------------------------------------------------------
# [u22-gl]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.1 De la póliza a la celda
#       > Grados de libertad: contar celdas y contar parámetros
# -----------------------------------------------------------------------------
d2 <- as.data.frame(tab2)                                   # una fila por celda; columna Freq
m_ind <- glm(Freq ~ zona_circulacion + asistencia, family = poisson, data = d2)
c(celdas = nrow(d2), parametros = length(coef(m_ind)), gl_residuales = df.residual(m_ind))

# -----------------------------------------------------------------------------
# [tbl-u22-esperadas]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.2 Dos vías: ¿independientes o asociadas?
#       > Qué significa «independencia» en una tabla
# -----------------------------------------------------------------------------
esperadas <- outer(rowSums(tab2), colSums(tab2)) / sum(tab2)
cbind(as.data.frame.matrix(tab2), round(esperadas, 1)) |>
  setNames(c("obs_sin", "obs_con", "esp_sin", "esp_con")) |>
  tibble::rownames_to_column("zona") |>
  dplyr::mutate(ajustado_glm_con = round(fitted(m_ind)[d2$asistencia == "con"], 1))

# -----------------------------------------------------------------------------
# [u22-dosvias-fit]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.2 Dos vías: ¿independientes o asociadas?
#       > Del modelo saturado al contraste
# -----------------------------------------------------------------------------
m_sat <- glm(Freq ~ zona_circulacion * asistencia, family = poisson, data = d2)
anova(m_ind, m_sat, test = "LRT")         # H0: independencia (estadístico G2 de razón de verosimilitudes)
chisq.test(tab2, correct = FALSE)         # el chi-cuadrado clásico de Pearson, para comparar

# -----------------------------------------------------------------------------
# [u22-dosvias-or]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.2 Dos vías: ¿independientes o asociadas?
#       > Leer la asociación: odds ratios
# -----------------------------------------------------------------------------
broom::tidy(m_sat, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(grepl(":", term)) |>
  dplyr::select(term, estimate, conf.low, conf.high)

# -----------------------------------------------------------------------------
# [u22-puente-logit]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.2 Dos vías: ¿independientes o asociadas?
#       > El puente con la logística
# -----------------------------------------------------------------------------
m_logit <- glm(asistencia ~ zona_circulacion, family = binomial, data = cart_tab)
rbind(loglineal = coef(m_sat)[grepl(":", names(coef(m_sat)))],
      logistica = coef(m_logit)[-1]) |> round(4)

# -----------------------------------------------------------------------------
# [u22-residuos-2v]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.2 Dos vías: ¿independientes o asociadas?
#       > Dónde está la asociación: residuos ajustados
# -----------------------------------------------------------------------------
d2 |> dplyr::mutate(esperado = round(fitted(m_ind), 1),
                    r_ajustado = round(rstandard(m_ind, type = "pearson"), 2))

# -----------------------------------------------------------------------------
# [u22-tab3]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > La tabla zona × tipo × asistencia
# -----------------------------------------------------------------------------
tab3 <- xtabs(~ zona_circulacion + tipo_vehiculo + asistencia, data = cart_tab)
d3   <- as.data.frame(tab3)
ftable(tab3)

# -----------------------------------------------------------------------------
# [fig-u22-grafos]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
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
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
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
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > La selección, término a término
# -----------------------------------------------------------------------------
anova(mods$homogenea, mods$saturado, test = "LRT")        # ¿hay interacción de 3 vías?
anova(mods$condicional, mods$homogenea, test = "LRT")     # ¿hay asociación zona-tipo dada la asistencia?

# -----------------------------------------------------------------------------
# [u22-residuos]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > Diagnóstico: residuos y mosaico
# -----------------------------------------------------------------------------
d3$r_aj <- rstandard(mods$condicional, type = "pearson")   # residuos AJUSTADOS
d3 |>
  dplyr::mutate(esperado = round(fitted(mods$condicional), 1), r_aj = round(r_aj, 2)) |>
  dplyr::arrange(dplyr::desc(abs(r_aj))) |>
  head(6)

# -----------------------------------------------------------------------------
# [fig-u22-mosaico-mutua]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > El mosaico
# -----------------------------------------------------------------------------
vcd::mosaic(tab3, shade = TRUE, legend = TRUE,   # por defecto: ~ zona + tipo + asistencia
            main = "Independencia mutua",
            labeling_args = list(rot_labels = c(bottom = 0, right = 0)))

# -----------------------------------------------------------------------------
# [fig-u22-mosaico-condicional]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > El mosaico
# -----------------------------------------------------------------------------
vcd::mosaic(tab3, shade = TRUE, legend = TRUE,
            expected = ~ zona_circulacion * asistencia + tipo_vehiculo * asistencia,
            main = "Independencia condicional",
            labeling_args = list(rot_labels = c(bottom = 0, right = 0)))

# -----------------------------------------------------------------------------
# [u22-berkeley-marginal]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
ucb <- UCBAdmissions                                     # Admit × Gender × Dept
or2 <- function(m) (m[1, 1] * m[2, 2]) / (m[1, 2] * m[2, 1])
round(100 * prop.table(margin.table(ucb, c(2, 1)), 1), 1)   # % admitido por sexo
c(OR_marginal_hombres_vs_mujeres = round(or2(margin.table(ucb, c(1, 2))), 2))

# -----------------------------------------------------------------------------
# [u22-berkeley-condicional]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
round(apply(ucb, 3, or2), 2)                                # OR hombres vs mujeres dentro de cada departamento

# -----------------------------------------------------------------------------
# [u22-berkeley-mecanismo]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
round(100 * prop.table(margin.table(ucb, c(2, 3)), 1), 1)  # a qué departamentos solicita cada sexo (% por fila)
round(100 * prop.table(margin.table(ucb, c(1, 3)), 2)[1, ], 1)   # % admitido en cada departamento

# -----------------------------------------------------------------------------
# [fig-u22-berkeley-grafo]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
nodos_ucb <- tibble::tibble(nodo = c("S", "Ad", "D"), x = c(0, 1, 0.5), y = c(0, 0, 0.87))
dibuja_grafo(list(c("S", "D"), c("Ad", "D")), "Berkeley", nodos = nodos_ucb) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), linetype = "dashed", colour = "grey50")

# -----------------------------------------------------------------------------
# [u22-berkeley-modelos]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > Colapsar una tabla: la paradoja de Simpson
# -----------------------------------------------------------------------------
d_ucb <- as.data.frame(ucb)
g <- function(fml) glm(fml, family = poisson, data = d_ucb)
m_ucb <- list(
  mutua       = g(Freq ~ Admit + Gender + Dept),
  condicional = g(Freq ~ Admit * Dept + Gender * Dept),
  homogenea   = g(Freq ~ (Admit + Gender + Dept)^2))
purrr::map_dfr(names(m_ucb), ~ tibble::tibble(
  modelo = .x, gl = df.residual(m_ucb[[.x]]), deviance = round(deviance(m_ucb[[.x]]), 1),
  p_ajuste = signif(pchisq(deviance(m_ucb[[.x]]), df.residual(m_ucb[[.x]]), lower.tail = FALSE), 2)))

# El mismo modelo condicional, quitando el departamento A
m_sin_A <- glm(Freq ~ Admit * Dept + Gender * Dept, family = poisson, data = as.data.frame(ucb[, , -1]))
c(deviance = round(deviance(m_sin_A), 1), gl = df.residual(m_sin_A),
  p_ajuste = round(pchisq(deviance(m_sin_A), df.residual(m_sin_A), lower.tail = FALSE), 2))

# -----------------------------------------------------------------------------
# [u22-colapsabilidad]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.3 Tres vías: ¿quién se asocia con quién?
#       > ¿Y en nuestra cartera?
# -----------------------------------------------------------------------------
rbind(marginal    = exp(coef(m_sat)[grepl(":", names(coef(m_sat)))]),
      condicional = exp(coef(mods$condicional)[grepl("zona_circulacion.*:asistencia",
                                                     names(coef(mods$condicional)))])) |>
  round(3)

# -----------------------------------------------------------------------------
# [u22-cuatro-vias]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.4 Más de tres vías (ampliación)
# -----------------------------------------------------------------------------
tab4 <- xtabs(~ zona_circulacion + tipo_vehiculo + uso + asistencia, data = cart_tab)
d4   <- as.data.frame(tab4)

ordenes <- list(
  "1 · solo principales"  = Freq ~ zona_circulacion + tipo_vehiculo + uso + asistencia,
  "2 · todas las dobles"  = Freq ~ (zona_circulacion + tipo_vehiculo + uso + asistencia)^2,
  "3 · todas las triples" = Freq ~ (zona_circulacion + tipo_vehiculo + uso + asistencia)^3,
  "4 · saturado"          = Freq ~ zona_circulacion * tipo_vehiculo * uso * asistencia)

purrr::map_dfr(names(ordenes), ~ {
  m <- glm(ordenes[[.x]], family = poisson, data = d4); gl <- df.residual(m)
  tibble::tibble(orden = .x, parametros = length(coef(m)), gl = gl,
                 deviance = round(deviance(m), 2),
                 p_ajuste = if (gl > 0) round(pchisq(deviance(m), gl, lower.tail = FALSE), 4)
                            else NA_real_)})

# -----------------------------------------------------------------------------
# [u22-escasez]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.4 Más de tres vías (ampliación)
#       > Tablas escasas y ceros
# -----------------------------------------------------------------------------
c(celdas = length(tab4), minimo = min(tab4),
  menores_de_5 = sum(tab4 < 5), vacias = sum(tab4 == 0))

# -----------------------------------------------------------------------------
# [u22-cuadrada]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > La tabla y sus marginales
# -----------------------------------------------------------------------------
cuad <- as.data.frame(xtabs(~ bonus_malus_prev + bonus_malus_act, data = cartera)) |>
  dplyr::rename(prev = bonus_malus_prev, act = bonus_malus_act) |>
  dplyr::mutate(i = as.integer(prev), j = as.integer(act),
                par = factor(paste(pmin(i, j), pmax(i, j), sep = "-")))
xtabs(Freq ~ prev + act, data = cuad) |> addmargins()

# -----------------------------------------------------------------------------
# [tbl-u22-cambio-marg]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > La tabla y sus marginales
# -----------------------------------------------------------------------------
rbind(previo = prop.table(table(cartera$bonus_malus_prev)),
      actual = prop.table(table(cartera$bonus_malus_act))) |>
  round(3) |>
  as.data.frame.matrix() |>
  tibble::rownames_to_column("año")

# -----------------------------------------------------------------------------
# [u22-simetria]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > Simetría
# -----------------------------------------------------------------------------
m_sim <- glm(Freq ~ par, family = poisson, data = cuad)
c(deviance = round(deviance(m_sim), 2), gl = df.residual(m_sim),
  p = signif(pchisq(deviance(m_sim), df.residual(m_sim), lower.tail = FALSE), 3))

# -----------------------------------------------------------------------------
# [u22-cuasisimetria]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > Cuasi-simetría y homogeneidad marginal
# -----------------------------------------------------------------------------
m_qs <- glm(Freq ~ prev + act + par, family = poisson, data = cuad)
c(deviance = round(deviance(m_qs), 2), gl = df.residual(m_qs))

# -----------------------------------------------------------------------------
# [u22-homogeneidad-marginal]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > Cuasi-simetría y homogeneidad marginal
# -----------------------------------------------------------------------------
anova(m_sim, m_qs, test = "LRT")   # H0: homogeneidad marginal (dada la cuasi-simetría)

# -----------------------------------------------------------------------------
# [u22-deriva]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
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
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > Asociación ordinal: aprovechar que los niveles están ordenados
# -----------------------------------------------------------------------------
m_indep_c <- glm(Freq ~ prev + act,               family = poisson, data = cuad)
m_unif    <- glm(Freq ~ prev + act + I(i * j),    family = poisson, data = cuad)
anova(m_indep_c, m_unif, test = "LRT")
c(beta_asociacion = round(coef(m_unif)["I(i * j)"], 4),
  OR_local        = round(exp(coef(m_unif)["I(i * j)"]), 3))

# -----------------------------------------------------------------------------
# [u22-ordinal-ajuste]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > Asociación ordinal: aprovechar que los niveles están ordenados
# -----------------------------------------------------------------------------
c(deviance = round(deviance(m_unif), 1), gl = df.residual(m_unif))

# -----------------------------------------------------------------------------
# [u22-movilidad]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > Asociación ordinal: aprovechar que los niveles están ordenados
# -----------------------------------------------------------------------------
cuad$diagonal <- factor(ifelse(cuad$i == cuad$j, "queda", "mueve"))
m_mov <- glm(Freq ~ prev + act + I(i * j) + diagonal, family = poisson, data = cuad)
anova(m_unif, m_mov, test = "LRT")
c(deviance = round(deviance(m_mov), 1), gl = df.residual(m_mov))

# -----------------------------------------------------------------------------
# [u22-validacion]
#   2 · Modelos log-lineales: cuando el dato es la celda
#     > 2.5 Tablas de cambio: el bonus-malus
#       > Validación contra el DGP
# -----------------------------------------------------------------------------
verdad <- attr(cartera, "verdad")
c(persistencia = verdad$persistencia)

# ¿Saltos cortos? Distancia de los cambios observados frente a la que darían saltos al azar
# niv y nva (nivel previo y actual) vienen del chunk de la deriva
mueve   <- nva != niv
p_prev  <- prop.table(table(factor(niv[mueve], levels = 1:5)))    # de dónde parten los que cambian
al_azar <- sapply(1:4, \(d) sum(sapply(1:5, \(i) p_prev[[i]] * sum(abs((1:5)[-i] - i) == d) / 4)))
rbind(observado = prop.table(table(factor(abs(nva - niv)[mueve], levels = 1:4))),
      al_azar   = al_azar) |> round(3)


# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------
# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.
# session_info() añade a sessionInfo() la fecha y la procedencia de cada
# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.
sessioninfo::session_info()
