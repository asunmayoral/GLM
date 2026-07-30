# =============================================================================
# Caso 2 · Unidad 2.2 — 2 · Modelos log-lineales para tablas
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_2_2.qmd.
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

source(file.path(.raiz, "caso2", "R", "dgp_conteos.R"))            # define simular_cartera(), cargar_cartera(), expandir_poliza_tramo()
cartera <- cargar_cartera("auto")    # lee datos/cartera_auto_*.rds si existe; si no (o si cambió el DGP), simula y lo guarda
glimpse(cartera)

# -----------------------------------------------------------------------------
# [u22-de-ficha-a-tabla]  ·  2.1 Contexto: cuando el dato es la celda > De la ficha a la tabla
# -----------------------------------------------------------------------------
# Trabajamos sobre una copia con el indicador de daños; no modificamos `cartera`, que las
# unidades siguientes siguen usando tal cual. El nivel "con" va segundo: será el "éxito".
cart_tab <- cartera |> dplyr::mutate(danos = factor(ifelse(n_danos > 0, "con", "sin"),
                                                    levels = c("sin", "con")))
tab2 <- xtabs(~ zona_circulacion + danos, data = cart_tab)
addmargins(tab2)

# -----------------------------------------------------------------------------
# [u22-gl]  ·  La regla práctica: fija en el modelo lo que estaba fijo en el diseño > Grados de libertad: contar celdas y contar parámetros
# -----------------------------------------------------------------------------
d2 <- as.data.frame(tab2)                                   # una fila por celda; columna Freq
m_ind <- glm(Freq ~ zona_circulacion + danos, family = poisson, data = d2)
c(celdas = nrow(d2), parametros = length(coef(m_ind)), gl_residuales = df.residual(m_ind))

# -----------------------------------------------------------------------------
# [u22-dosvias-fit]  ·  2.2 Dos vías: independencia, asociación y el puente con la logística > El modelo y su lectura
# -----------------------------------------------------------------------------
m_sat <- glm(Freq ~ zona_circulacion * danos, family = poisson, data = d2)
anova(m_ind, m_sat, test = "LRT")     # H0: independencia

# -----------------------------------------------------------------------------
# [u22-dosvias-or]  ·  2.2 Dos vías: independencia, asociación y el puente con la logística > El modelo y su lectura
# -----------------------------------------------------------------------------
broom::tidy(m_sat, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(grepl(":", term)) |>
  dplyr::select(term, estimate, conf.low, conf.high)

# -----------------------------------------------------------------------------
# [u22-puente-logit]  ·  2.2 Dos vías: independencia, asociación y el puente con la logística > El puente con la logística
# -----------------------------------------------------------------------------
m_logit <- glm(danos ~ zona_circulacion, family = binomial, data = cart_tab)
rbind(loglineal = coef(m_sat)[grepl(":", names(coef(m_sat)))],
      logistica = coef(m_logit)[-1]) |> round(4)

# -----------------------------------------------------------------------------
# [u22-tab3]  ·  2.3 Tres vías: la jerarquía de modelos
# -----------------------------------------------------------------------------
tab3 <- xtabs(~ zona_circulacion + tipo_vehiculo + danos, data = cart_tab)
d3   <- as.data.frame(tab3)
ftable(tab3)

# -----------------------------------------------------------------------------
# [u22-jerarquia]  ·  2.3 Tres vías: la jerarquía de modelos > Los cinco modelos
# -----------------------------------------------------------------------------
f <- function(fml) glm(fml, family = poisson, data = d3)
mods <- list(
  mutua       = f(Freq ~ zona_circulacion + tipo_vehiculo + danos),
  conjunta    = f(Freq ~ zona_circulacion * tipo_vehiculo + danos),
  condicional = f(Freq ~ zona_circulacion * danos + tipo_vehiculo * danos),
  homogenea   = f(Freq ~ (zona_circulacion + tipo_vehiculo + danos)^2),
  saturado    = f(Freq ~ zona_circulacion * tipo_vehiculo * danos))

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
# [u22-lrt-jerarquia]  ·  Ajuste absoluto: aquí sí, y por qué > La selección, término a término
# -----------------------------------------------------------------------------
anova(mods$homogenea, mods$saturado, test = "LRT")        # ¿hay interacción de 3 vías?
anova(mods$condicional, mods$homogenea, test = "LRT")     # ¿hay asociación zona-tipo dados los daños?

# -----------------------------------------------------------------------------
# [u22-simpson]  ·  Ajuste absoluto: aquí sí, y por qué > Simpson: la asociación marginal puede mentir
# -----------------------------------------------------------------------------
# DGP explícito: Z está asociado a la vez con X y con Y. En z1 son raros tanto X=sí como
# Y=sí; en z2 son frecuentes los dos. Dentro de CADA estrato X protege levemente (OR < 1),
# pero al colapsar sobre Z la mezcla desigual invierte el signo.
toy <- expand.grid(Z = c("z1", "z2"), X = c("no", "si"), Y = c("no", "si"))
toy$Freq <- c(900,  50,  150, 300,      # Y = no
              225, 200,   15, 480)      # Y = sí
tt <- xtabs(Freq ~ X + Y + Z, data = toy)

or <- function(m) (m[1,1] * m[2,2]) / (m[1,2] * m[2,1])
round(c(condicional_z1 = or(tt[, , "z1"]),
        condicional_z2 = or(tt[, , "z2"]),
        marginal       = or(margin.table(tt, c(1, 2)))), 3)

# -----------------------------------------------------------------------------
# [u22-simpson-modelos]  ·  Ajuste absoluto: aquí sí, y por qué > Simpson: la asociación marginal puede mentir
# -----------------------------------------------------------------------------
anova(glm(Freq ~ X*Z + Y*Z, family = poisson, data = toy),    # X _||_ Y | Z
      glm(Freq ~ (X + Y + Z)^2, family = poisson, data = toy), test = "LRT")

# -----------------------------------------------------------------------------
# [u22-cuatro-vias]  ·  Cuándo se puede colapsar una tabla > Cuatro factores o más: trabajar por orden de interacción
# -----------------------------------------------------------------------------
tab4 <- xtabs(~ zona_circulacion + tipo_vehiculo + uso + danos, data = cart_tab)
d4   <- as.data.frame(tab4)

ordenes <- list(
  "1 · solo principales"  = Freq ~ zona_circulacion + tipo_vehiculo + uso + danos,
  "2 · todas las dobles"  = Freq ~ (zona_circulacion + tipo_vehiculo + uso + danos)^2,
  "3 · todas las triples" = Freq ~ (zona_circulacion + tipo_vehiculo + uso + danos)^3,
  "4 · saturado"          = Freq ~ zona_circulacion * tipo_vehiculo * uso * danos)

purrr::map_dfr(names(ordenes), ~ {
  m <- glm(ordenes[[.x]], family = poisson, data = d4); gl <- df.residual(m)
  tibble::tibble(orden = .x, parametros = length(coef(m)), gl = gl,
                 deviance = round(deviance(m), 2),
                 p_ajuste = if (gl > 0) round(pchisq(deviance(m), gl, lower.tail = FALSE), 4)
                            else NA_real_)})

# -----------------------------------------------------------------------------
# [u22-residuos]  ·  2.4 Diagnóstico: dónde falla el modelo > Residuos ajustados
# -----------------------------------------------------------------------------
d3$r_aj <- rstandard(mods$condicional, type = "pearson")   # residuos AJUSTADOS
d3 |>
  dplyr::mutate(esperado = round(fitted(mods$condicional), 1), r_aj = round(r_aj, 2)) |>
  dplyr::arrange(dplyr::desc(abs(r_aj))) |>
  head(6)

# -----------------------------------------------------------------------------
# [fig-u22-mosaico-mutua]  ·  2.4 Diagnóstico: dónde falla el modelo > El mosaico
# -----------------------------------------------------------------------------
vcd::mosaic(tab3, shade = TRUE, legend = TRUE,   # por defecto: ~ zona + tipo + danos
            main = "Independencia mutua",
            labeling_args = list(rot_labels = c(bottom = 0, right = 0)))

# -----------------------------------------------------------------------------
# [fig-u22-mosaico-condicional]  ·  2.4 Diagnóstico: dónde falla el modelo > El mosaico
# -----------------------------------------------------------------------------
vcd::mosaic(tab3, shade = TRUE, legend = TRUE,
            expected = ~ zona_circulacion * danos + tipo_vehiculo * danos,
            main = "Independencia condicional",
            labeling_args = list(rot_labels = c(bottom = 0, right = 0)))

# -----------------------------------------------------------------------------
# [u22-escasez]  ·  Pearson frente a ajustados > Tablas escasas y ceros
# -----------------------------------------------------------------------------
c(celdas = length(tab4), minimo = min(tab4),
  menores_de_5 = sum(tab4 < 5), vacias = sum(tab4 == 0))

# -----------------------------------------------------------------------------
# [u22-cuadrada]  ·  2.5 Tablas cuadradas: modelizar el cambio
# -----------------------------------------------------------------------------
cuad <- as.data.frame(xtabs(~ bonus_malus_prev + bonus_malus_act, data = cartera)) |>
  dplyr::rename(prev = bonus_malus_prev, act = bonus_malus_act) |>
  dplyr::mutate(i = as.integer(prev), j = as.integer(act),
                par = factor(paste(pmin(i, j), pmax(i, j), sep = "-")))
xtabs(Freq ~ prev + act, data = cuad) |> addmargins()

# -----------------------------------------------------------------------------
# [u22-simetria]  ·  2.5 Tablas cuadradas: modelizar el cambio > Simetría
# -----------------------------------------------------------------------------
m_sim <- glm(Freq ~ par, family = poisson, data = cuad)
c(deviance = round(deviance(m_sim), 2), gl = df.residual(m_sim),
  p = signif(pchisq(deviance(m_sim), df.residual(m_sim), lower.tail = FALSE), 3))

# -----------------------------------------------------------------------------
# [u22-cuasisimetria]  ·  2.5 Tablas cuadradas: modelizar el cambio > Cuasi-simetría y homogeneidad marginal
# -----------------------------------------------------------------------------
m_qs <- glm(Freq ~ prev + act + par, family = poisson, data = cuad)
c(deviance = round(deviance(m_qs), 2), gl = df.residual(m_qs))

# -----------------------------------------------------------------------------
# [u22-homogeneidad-marginal]  ·  2.5 Tablas cuadradas: modelizar el cambio > Cuasi-simetría y homogeneidad marginal
# -----------------------------------------------------------------------------
anova(m_sim, m_qs, test = "LRT")   # H0: homogeneidad marginal (dada la cuasi-simetría)

# -----------------------------------------------------------------------------
# [u22-deriva]  ·  2.5 Tablas cuadradas: modelizar el cambio > Cuánta deriva, y en qué sentido
# -----------------------------------------------------------------------------
niv <- as.integer(cartera$bonus_malus_prev); nva <- as.integer(cartera$bonus_malus_act)
c(nivel_medio_previo = round(mean(niv), 3),
  nivel_medio_actual = round(mean(nva), 3),
  deriva             = round(mean(nva - niv), 3),
  pct_empeora        = round(100 * mean(nva > niv), 1),
  pct_mejora         = round(100 * mean(nva < niv), 1))

# -----------------------------------------------------------------------------
# [u22-ordinal]  ·  Significativo no es lo mismo que importante > Asociación ordinal: aprovechar que los niveles están ordenados
# -----------------------------------------------------------------------------
m_indep_c <- glm(Freq ~ prev + act,               family = poisson, data = cuad)
m_unif    <- glm(Freq ~ prev + act + I(i * j),    family = poisson, data = cuad)
anova(m_indep_c, m_unif, test = "LRT")
c(beta_asociacion = round(coef(m_unif)["I(i * j)"], 4),
  OR_local        = round(exp(coef(m_unif)["I(i * j)"]), 3))

# -----------------------------------------------------------------------------
# [u22-ordinal-ajuste]  ·  Significativo no es lo mismo que importante > Asociación ordinal: aprovechar que los niveles están ordenados
# -----------------------------------------------------------------------------
c(deviance = round(deviance(m_unif), 1), gl = df.residual(m_unif))

# -----------------------------------------------------------------------------
# [u22-movilidad]  ·  Significativo no es lo mismo que importante > Asociación ordinal: aprovechar que los niveles están ordenados
# -----------------------------------------------------------------------------
cuad$diagonal <- factor(ifelse(cuad$i == cuad$j, "queda", "mueve"))
m_mov <- glm(Freq ~ prev + act + I(i * j) + diagonal, family = poisson, data = cuad)
anova(m_unif, m_mov, test = "LRT")
c(deviance = round(deviance(m_mov), 1), gl = df.residual(m_mov))

# -----------------------------------------------------------------------------
# [u22-proxy-control]  ·  2.6 Aplicación: detectar proxies del factor prohibido
# -----------------------------------------------------------------------------
tp <- as.data.frame(xtabs(~ sexo + tipo_vehiculo + danos, data = cart_tab))
m_ci  <- glm(Freq ~ sexo * danos + tipo_vehiculo * danos, family = poisson, data = tp)
m_hom <- glm(Freq ~ (sexo + tipo_vehiculo + danos)^2,      family = poisson, data = tp)
anova(m_ci, m_hom, test = "LRT")     # H0: sexo _||_ tipo | danos  (no hay proxy)

# -----------------------------------------------------------------------------
# [u22-proxy-control-tamano]  ·  2.6 Aplicación: detectar proxies del factor prohibido
# -----------------------------------------------------------------------------
broom::tidy(m_hom, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(grepl("^sexo.*:tipo", term)) |>
  dplyr::select(term, estimate, conf.low, conf.high) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

# -----------------------------------------------------------------------------
# [u22-proxy-simulado]  ·  Este es un falso positivo, y lo sabemos con certeza
# -----------------------------------------------------------------------------
set.seed(SEMILLA_CURSO)
n <- 20000        # n grande y efectos exagerados a propósito: queremos VER la fuga
# Niveles EXPLÍCITOS: "M" es la referencia, así el coeficiente se llama `sH` y se lee
# "hombres frente a mujeres". Sin declararlos, R ordena alfabéticamente (H antes que M)
# y el coeficiente saldría invertido.
s <- factor(sample(c("M", "H"), n, TRUE), levels = c("M", "H"))        # factor prohibido
v <- factor(ifelse(runif(n) < ifelse(s == "H", 0.30, 0.05), "moto", "otro"),
            levels = c("otro", "moto"))                               # PERMITIDA, asociada a s
y <- rbinom(n, 1, plogis(-0.8 + 1.20 * (v == "moto") + 0.20 * (s == "H")))
sim <- as.data.frame(xtabs(~ s + v + y))

anova(glm(Freq ~ s*y + v*y, family = poisson, data = sim),
      glm(Freq ~ (s + v + y)^2, family = poisson, data = sim), test = "LRT")

# -----------------------------------------------------------------------------
# [u22-proxy-fuga]  ·  Este es un falso positivo, y lo sabemos con certeza
# -----------------------------------------------------------------------------
d_sim <- data.frame(y = y, s = s, v = v)
or_s <- function(fml) exp(coef(glm(fml, family = binomial, data = d_sim))[["sH"]])
c(OR_marginal    = round(or_s(y ~ s),     3),   # lo que se ve sin controlar el proxy
  OR_ajustado    = round(or_s(y ~ s + v), 3),   # el efecto directo del factor prohibido
  verdad_directa = round(exp(0.20),       3))   # el valor con el que se generaron los datos

