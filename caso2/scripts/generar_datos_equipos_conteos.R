# =============================================================================
#  generar_datos_equipos_conteos.R · Caso 2 (estudio de caso, contexto SALUD)
# -----------------------------------------------------------------------------
#  Genera 10 cohortes sanitarias distintas (una por equipo) a partir del DGP de
#  conteos, con configuraciones de parámetros COHERENTES (mismos signos) pero
#  DIFERENTES (magnitudes, semillas) -> cada grupo obtiene resultados propios.
#  Ejecutar con el directorio de trabajo en 'caso2/'.
#  Los .rds conservan attr(.,"verdad"); los .csv van sin ella (trabajar a ciegas).
# =============================================================================

# Directorio robusto: localiza R/dgp_conteos.R desde las ubicaciones habituales
# (wd en 'caso2/', en la raíz del proyecto, o en 'caso2/scripts/') y fija el wd en 'caso2/'.
.cand <- c("R/dgp_conteos.R", "caso2/R/dgp_conteos.R",
           "../R/dgp_conteos.R", "../caso2/R/dgp_conteos.R")
.dgp  <- .cand[file.exists(.cand)][1]
if (is.na(.dgp))
  stop("No encuentro 'R/dgp_conteos.R'. Abre el proyecto (GLM.Rproj) y ejecuta el script, ",
       "o fija el directorio de trabajo en 'caso2/'.")
setwd(dirname(dirname(.dgp)))   # sube a 'caso2/' (la carpeta que contiene R/)
source("R/dgp_conteos.R")

K_eq    <- 10L
out_dir <- "datos_equipos_salud"
dir.create(out_dir, showWarnings = FALSE)

# --- 10 configuraciones EXPLÍCITAS (coherentes y distintas) -------------------
par_eq <- data.frame(
  equipo       = sprintf("equipo_%02d", 1:K_eq),
  semilla      = 250101L:250110L,
  mult_tasa    = c(0.80,1.20,1.00,0.90,1.30,0.70,1.10,0.95,1.25,0.85),  # escala las tasas base
  mult_efecto  = c(0.90,1.20,1.00,0.80,1.30,0.70,1.10,0.95,1.25,0.85),  # escala la fuerza de los efectos
  theta_nb     = c(0.80,1.20,1.00,1.50,0.70,1.30,0.90,1.60,1.10,1.40),  # sobredispersión
  sigma_a      = c(0.50,0.70,0.45,0.80,0.60,0.55,0.75,0.40,0.65,0.50),  # efecto de centro
  cero_int     = c(0.20,0.40,0.10,0.50,0.30,0.35,0.15,0.45,0.25,0.10),  # logit de ceros estructurales (pi ~0.5, exceso VISIBLE)
  persistencia = c(0.50,0.60,0.55,0.62,0.48,0.58,0.52,0.65,0.50,0.56),  # tabla cuadrada
  stringsAsFactors = FALSE
)

# betas por defecto del contexto SALUD (se escalan de forma coherente, sin cambiar signos)
.def <- betas_defecto("salud")
escala <- function(b, mt, me) { b[1] <- b[1] + log(mt); b[-1] <- b[-1] * me; b }

config_de <- function(r) list(
  contexto = "salud", semilla = r$semilla,
  sigma_a = r$sigma_a, theta_nb = r$theta_nb,
  b_limpio = escala(.def$limpio, r$mult_tasa, r$mult_efecto),
  b_sobre  = escala(.def$sobre,  r$mult_tasa, r$mult_efecto),
  b_ceros  = escala(.def$ceros,  r$mult_tasa, r$mult_efecto),
  b_grupo  = escala(.def$grupo,  r$mult_tasa, r$mult_efecto),
  b_evento = escala(.def$evento, r$mult_tasa, r$mult_efecto),
  g_cero   = c(int = r$cero_int, b_uso = -0.5, b_pot = -0.8),
  h0_tramo = c(0.05,0.06,0.07,0.08,0.09,0.10) * r$mult_tasa,
  persistencia = r$persistencia)

# --- Generar, guardar y resumir ----------------------------------------------
disp <- function(y, off) {                       # índice de dispersión (robusto)
  m <- mean(y) / mean(off)
  if (!is.finite(m) || m == 0) return(NA_real_)
  sum((y - off * m)^2 / (off * m)) / (length(y) - 1)
}
resumen <- vector("list", K_eq)
for (k in seq_len(K_eq)) {
  d  <- do.call(simular_cartera, config_de(par_eq[k, ]))
  ex <- d$anios_seguimiento
  resumen[[k]] <- data.frame(
    equipo = par_eq$equipo[k], N = nrow(d),
    m_visitas = round(mean(d$n_visitas_ap), 2),
    disp_urg  = round(disp(d$n_urgencias, ex), 2),               # >1 = sobredispersión
    p0_ingr   = round(mean(d$n_ingresos_graves == 0) * 100),     # % ceros (ingresos)
    p_evento  = round(mean(d$evento) * 100),                     # % con evento
    row.names = NULL)
  dd <- file.path(out_dir, par_eq$equipo[k]); dir.create(dd, showWarnings = FALSE)
  saveRDS(d, file.path(dd, "cohorte.rds"))                       # conserva la verdad
  write.csv(d, file.path(dd, "cohorte.csv"), row.names = FALSE)
}
resumen <- do.call(rbind, resumen)
cat("\n== Resumen de las cohortes (salud) ==\n"); print(resumen)   # se imprime SIEMPRE

# --- Chequeo de coherencia (robusto a NA: nunca detiene el script) -----------
ok <- with(resumen, disp_urg >= 1.2 & p_evento >= 10 & p_evento <= 60 &
                     p0_ingr >= 35 & p0_ingr <= 90)
ok[is.na(ok)] <- FALSE
malos <- resumen$equipo[!ok]
if (length(malos) == 0) message("Coherencia OK: las 10 cohortes muestran sus fenómenos en rango razonable.")
if (length(malos) >  0) warning("Revisar equipos (indicadores extremos o NA): ", paste(malos, collapse = ", "))

write.csv(par_eq,  file.path(out_dir, "configuraciones_equipos.csv"), row.names = FALSE)
write.csv(resumen, file.path(out_dir, "resumen_equipos.csv"),         row.names = FALSE)
cat("\nDatos por equipo en:", normalizePath(out_dir), "\n")

setwd("..") # volvemos al directorio raíz