# =============================================================================
#  generar_datos_equipos_auto.R · Caso 2/3 (estudio de caso, cartera de AUTO)
# -----------------------------------------------------------------------------
#  Genera 10 carteras de auto distintas (una por equipo) con dgp_auto.R, con
#  configuraciones COHERENTES (mismos signos) pero DIFERENTES (magnitudes,
#  semillas) -> cada grupo obtiene resultados propios.
#  Se puede ejecutar desde cualquier wd (raíz, caso2/ o caso2/scripts/): localiza
#  el DGP por ruta absoluta y ESCRIBE en caso2/datos_equipos_auto/.
#  Por equipo se guardan:
#     cartera.rds     -> nivel póliza CON attr("verdad"/"siniestros"/"panel")  [para QC docente]
#     cartera.csv     -> nivel póliza SIN la verdad                            [trabajar a ciegas]
#     panel.csv       -> panel póliza-año (counting process, offset variable)
#     siniestros.csv  -> tabla larga (un parte por fila: t_k, coste_k)
# =============================================================================

# --- Localiza R/dgp_auto.R robustamente: prueba rutas relativas y SUBE por los
#     directorios padres, así funciona desde caso2/, caso2/scripts/, la raíz, etc.
.buscar_dgp <- function() {
  rel <- c("dgp_auto.R", "R/dgp_auto.R", "caso2/R/dgp_auto.R")
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 0:8) {
    for (r in rel) { p <- file.path(d, r); if (file.exists(p)) return(normalizePath(p)) }
    padre <- dirname(d); if (identical(padre, d)) break; d <- padre
  }
  NA_character_
}
.dgp <- .buscar_dgp()
if (is.na(.dgp))
  stop("No encuentro 'R/dgp_auto.R'. Directorio de trabajo actual: ", getwd(),
       "\n  Solución: setwd('<ruta>/GLM/caso2') o abre GLM.Rproj, y vuelve a ejecutar.")
caso2_dir <- dirname(dirname(.dgp))                 # .../caso2  (no se cambia el wd)
source(.dgp)

K_eq    <- 10L
out_dir <- file.path(caso2_dir, "datos_equipos_auto")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- 10 configuraciones EXPLÍCITAS (coherentes y distintas) -------------------
#  mult_tasa  escala las tasas base (lambda0 y las de asistencia/fraude/gestiones)
#  mult_efecto escala la FUERZA de todos los efectos (betas), sin cambiar signos
#  theta_frail fragilidad (sobredispersión) · weib_shape forma del hazard base
#  sigma_ag efecto de agencia · nu_gamma dispersión del coste · mult_coste nivel de coste
#  persistencia inercia del bonus-malus
par_eq <- data.frame(
  equipo       = sprintf("equipo_%02d", 1:K_eq),
  semilla      = 260101L:260110L,
  mult_tasa    = c(0.80,1.20,1.00,0.90,1.30,0.70,1.10,0.95,1.25,0.85),
  mult_efecto  = c(0.90,1.20,1.00,0.85,1.30,0.75,1.10,0.95,1.25,0.80),
  theta_frail  = c(0.80,1.30,1.20,1.60,0.90,1.40,1.00,1.50,1.10,0.70),
  weib_shape   = c(1.05,1.25,1.15,1.30,1.00,1.35,1.10,1.20,1.05,1.15),
  sigma_ag     = c(0.45,0.65,0.50,0.75,0.55,0.60,0.70,0.40,0.60,0.50),
  nu_gamma     = c(1.60,2.00,1.80,2.20,1.50,1.90,1.70,2.10,1.60,1.40),
  mult_coste   = c(0.90,1.15,1.00,1.20,0.95,1.10,1.05,1.25,0.90,0.85),
  persistencia = c(0.55,0.60,0.58,0.62,0.52,0.60,0.56,0.64,0.54,0.58),
  stringsAsFactors = FALSE
)

# Escala TODOS los vectores de betas por 'me' (mantiene signos y nombres) -------
escala_betas <- function(betas, me) lapply(betas, function(v) v * me)

config_de <- function(r) list(
  semilla     = r$semilla,
  lambda0     = 0.15 * r$mult_tasa,
  lambda_asist= 0.45 * r$mult_tasa,
  lambda_fraude = 0.10 * r$mult_tasa,
  lambda_gest = 0.30 * r$mult_tasa,
  theta_frail = r$theta_frail,
  weib_shape  = r$weib_shape,
  sigma_ag    = r$sigma_ag,
  nu_gamma    = r$nu_gamma,
  coste_base  = 1800 * r$mult_coste,
  persistencia= r$persistencia,
  betas       = escala_betas(betas_auto_defecto(), r$mult_efecto)
)

# --- Índice de dispersión robusto (respecto a la exposición) ------------------
disp <- function(y, off) {
  m <- mean(y) / mean(off)
  if (!is.finite(m) || m == 0) return(NA_real_)
  sum((y - off * m)^2 / (off * m)) / (length(y) - 1)
}

# --- Generar, guardar y resumir ----------------------------------------------
resumen <- vector("list", K_eq)
for (k in seq_len(K_eq)) {
  d  <- do.call(simular_cartera_auto, config_de(par_eq[k, ]))
  S  <- attr(d, "siniestros"); P <- attr(d, "panel")
  ex <- d$exposicion

  resumen[[k]] <- data.frame(
    equipo   = par_eq$equipo[k], N = nrow(d),
    m_sin    = round(mean(d$n_siniestros), 3),
    disp_sin = round(disp(d$n_siniestros, ex), 2),          # >1 = sobredispersión (fragilidad)
    p0_fraude= round(mean(d$n_fraude == 0) * 100),          # % ceros (fraude)
    p_evento = round(mean(d$evento) * 100),                 # % con primer siniestro
    p_baja   = round(mean(d$baja) * 100),                   # % de bajas (churn)
    m_coste  = round(mean(d$coste_medio, na.rm = TRUE)),    # coste medio por parte (€)
    row.names = NULL)

  dd <- file.path(out_dir, par_eq$equipo[k]); dir.create(dd, showWarnings = FALSE)
  saveRDS(d, file.path(dd, "cartera.rds"))                  # conserva la verdad y las tablas
  write.csv(d, file.path(dd, "cartera.csv"), row.names = FALSE)   # a ciegas (sin attr)
  write.csv(P, file.path(dd, "panel.csv"),      row.names = FALSE)
  write.csv(S, file.path(dd, "siniestros.csv"), row.names = FALSE)
}
resumen <- do.call(rbind, resumen)
cat("\n== Resumen de las carteras (auto) ==\n"); print(resumen)

# --- Chequeo de coherencia (robusto a NA: nunca detiene el script) -----------
ok <- with(resumen, disp_sin >= 1.2 & p_evento >= 15 & p_evento <= 60 &
                     p0_fraude >= 40 & p0_fraude <= 97 & p_baja >= 15 & p_baja <= 60)
ok[is.na(ok)] <- FALSE
malos <- resumen$equipo[!ok]
if (length(malos) == 0) message("Coherencia OK: las 10 carteras muestran sus fenómenos en rango razonable.")
if (length(malos) >  0) warning("Revisar equipos (indicadores extremos o NA): ", paste(malos, collapse = ", "))

write.csv(par_eq,  file.path(out_dir, "configuraciones_equipos.csv"), row.names = FALSE)
write.csv(resumen, file.path(out_dir, "resumen_equipos.csv"),         row.names = FALSE)
cat("\nDatos por equipo en:", normalizePath(out_dir), "\n")
