# =============================================================================
#  generar_datos_equipos.R  ·  Caso 1
#  Genera 10 bases de datos distintas (una por equipo) a partir del mismo DGP,
#  con configuraciones de parámetros coherentes pero diferentes.
# -----------------------------------------------------------------------------
#  - Ejecutar con el directorio de trabajo en 'caso1/'  (source de R/dgp_cohorte.R).
#  - Cada equipo recibe su cohorte y su expansión persona-periodo.
#  - Los .rds conservan attr(.,"verdad") (para el control de calidad contra el DGP);
#    los .csv van sin la "verdad" (para trabajar a ciegas).
#  - 'configuraciones_equipos.csv' es la CLAVE del docente (parámetros verdaderos).
#
#  Coherencia garantizada en las 10 configuraciones:
#    beta_x1 > 0 (fragilidad = factor de riesgo);  beta_x2 < 0 (tratamiento protector);
#    riesgo base creciente (paso > 0);  sigma_u, sigma_w > 0;
#    ordinal: bord_x1 > 0, bord_x2 < 0, umbrales theta1 < theta2.
# =============================================================================

# simular_cohorte(), expandir_persona_periodo()
# Directorio robusto: funciona con el wd en 'caso1/', 'caso1/R/' o la raíz del proyecto.
if (file.exists("dgp_cohorte.R")) setwd("..")            # wd en 'caso1/R/' -> sube a 'caso1/'
if (!file.exists("R/dgp_cohorte.R")) {
  if (file.exists("caso1/R/dgp_cohorte.R")) setwd("caso1") else
    stop("Ejecuta este script con el directorio de trabajo en 'caso1/', 'caso1/R/' o la raíz del proyecto.")
}
source("R/dgp_cohorte.R")

K       <- 10L
out_dir <- "datos_equipos"
dir.create(out_dir, showWarnings = FALSE)

# --- 10 configuraciones EXPLÍCITAS (coherentes y distintas) -------------------
par_eq <- data.frame(
  equipo  = sprintf("equipo_%02d", 1:K),
  semilla = c(240101L,240102L,240103L,240104L,240105L,
              240106L,240107L,240108L,240109L,240110L),
  beta_x1 = c(0.70,0.85,1.00,0.60,0.90,0.75,1.05,0.65,0.95,0.80),   # > 0
  beta_x2 = c(-0.55,-0.65,-0.45,-0.75,-0.50,-0.60,-0.40,-0.70,-0.55,-0.65), # < 0
  sigma_u = c(0.55,0.70,0.45,0.80,0.60,0.50,0.75,0.65,0.85,0.40),
  a0      = c(-3.00,-2.90,-3.15,-2.80,-3.10,-2.95,-3.20,-2.85,-3.05,-2.75), # riesgo base (periodo 1)
  paso    = c(0.14,0.16,0.20,0.12,0.18,0.15,0.22,0.13,0.19,0.10),   # incremento por periodo > 0
  gB_int  = c(-0.30,-0.20,-0.40,-0.10,-0.35,-0.25,-0.45,-0.15,-0.50,-0.05),
  gB_x1   = c(0.60,0.70,0.80,0.50,0.75,0.65,0.85,0.55,0.90,0.45),
  gB_x2   = c(-0.35,-0.40,-0.30,-0.50,-0.25,-0.45,-0.20,-0.55,-0.30,-0.40),
  gC_int  = c(-0.70,-0.80,-0.60,-0.90,-0.75,-1.00,-0.55,-0.85,-0.65,-0.95),
  gC_x1   = c(1.00,1.10,1.20,0.90,1.15,1.05,1.25,0.95,1.30,0.85),
  gC_x2   = c(0.40,0.50,0.30,0.60,0.45,0.55,0.35,0.65,0.25,0.50),
  theta1  = c(-0.60,-0.55,-0.70,-0.45,-0.65,-0.50,-0.80,-0.40,-0.75,-0.60),
  theta2  = c(0.90,0.95,0.80,1.05,0.85,1.00,0.75,1.10,0.90,0.95),
  bord_x1 = c(0.70,0.80,0.90,0.65,0.85,0.75,0.95,0.60,1.00,0.70),   # > 0
  bord_x2 = c(-0.45,-0.50,-0.40,-0.60,-0.55,-0.35,-0.50,-0.65,-0.45,-0.70), # < 0
  sigma_w = c(0.50,0.60,0.40,0.70,0.55,0.45,0.65,0.75,0.35,0.60),
  stringsAsFactors = FALSE
)

# --- Traductor fila -> argumentos de simular_cohorte() -----------------------
config_de <- function(r) list(
  n_centros        = 24L,
  media_por_centro = 35L,
  beta       = c(x1 = r$beta_x1, x2 = r$beta_x2),
  sigma_u    = r$sigma_u,
  alpha_base = r$a0 + r$paso * (0:5),                 # 6 periodos, creciente
  cens_rango = c(3L, 6L),
  gamma_nom  = list(B = c(int = r$gB_int, x1 = r$gB_x1, x2 = r$gB_x2),
                    C = c(int = r$gC_int, x1 = r$gC_x1, x2 = r$gC_x2)),
  theta_ord  = c(r$theta1, r$theta2),
  beta_ord   = c(x1 = r$bord_x1, x2 = r$bord_x2),
  sigma_w    = r$sigma_w,
  semilla    = r$semilla
)

# --- Generar, guardar y resumir ----------------------------------------------
resumen <- vector("list", K)
for (k in seq_len(K)) {
  r   <- par_eq[k, ]
  cfg <- config_de(r)
  cohorte <- do.call(simular_cohorte, cfg)
  pp      <- expandir_persona_periodo(cohorte)

  d <- file.path(out_dir, r$equipo)
  dir.create(d, showWarnings = FALSE)
  saveRDS(cohorte, file.path(d, "cohorte.rds"))  # conserva attr(.,"verdad")
  saveRDS(pp,      file.path(d, "pp.rds"))
  vars <- c("id","centro","x1","x2","tiempo","evento","ever","clase_nom","sever_ord")
  write.csv(cohorte[, vars], file.path(d, "cohorte.csv"), row.names = FALSE)
  write.csv(pp,              file.path(d, "pp.csv"),      row.names = FALSE)

  nom <- prop.table(table(cohorte$clase_nom))
  ord <- prop.table(table(cohorte$sever_ord))
  resumen[[k]] <- data.frame(
    equipo      = r$equipo,
    N           = nrow(cohorte),
    tasa_evento = round(mean(cohorte$evento), 3),
    p_censura   = round(mean(cohorte$evento == 0), 3),
    HR_x1       = round(exp(r$beta_x1), 2),
    HR_x2       = round(exp(r$beta_x2), 2),
    A = round(nom["A"], 2), B = round(nom["B"], 2), Cc = round(nom["C"], 2),
    Leve = round(ord["Leve"], 2), Moderado = round(ord["Moderado"], 2),
    Grave = round(ord["Grave"], 2),
    row.names = NULL
  )
}
resumen <- do.call(rbind, resumen)

# --- Comprobación automática de coherencia -----------------------------------
avisos <- with(resumen,
  (tasa_evento < 0.15 | tasa_evento > 0.55) |
  (A < 0.05 | B < 0.05 | Cc < 0.05) |
  (Leve < 0.05 | Moderado < 0.05 | Grave < 0.05))
if (any(avisos)) {
  warning("Revisar configuraciones con distribuciones extremas: ",
          paste(resumen$equipo[avisos], collapse = ", "))
} else {
  message("Coherencia OK: las 10 cohortes tienen tasas y categorías en rango razonable.")
}

# --- Clave del docente (parámetros verdaderos) y resumen ----------------------
par_eq$HR_x1 <- round(exp(par_eq$beta_x1), 3)   # hazard ratios verdaderos
par_eq$HR_x2 <- round(exp(par_eq$beta_x2), 3)
write.csv(par_eq,   file.path(out_dir, "configuraciones_equipos.csv"), row.names = FALSE)
write.csv(resumen,  file.path(out_dir, "resumen_equipos.csv"),         row.names = FALSE)

cat("\n== Resumen de las cohortes generadas ==\n"); print(resumen)
cat("\nDatos por equipo en:", normalizePath(out_dir), "\n")
cat("Clave del docente: configuraciones_equipos.csv (no repartir a los equipos).\n")

setwd("..") # volvemos al directorio raíz