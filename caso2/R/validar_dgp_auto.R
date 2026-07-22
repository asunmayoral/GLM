# =============================================================================
#  validar_dgp_auto.R · Control de calidad del DGP de auto contra la "verdad".
#  Ejecutar desde la raíz del proyecto (o desde caso2/). Requiere: survival, MASS.
#  Ajusta modelos de referencia y comprueba que recuperan los parámetros generados.
#  (El entorno de desarrollo no tiene R; este script es para que lo corras tú.)
# =============================================================================

suppressPackageStartupMessages({ library(survival); library(MASS) })
# Localiza dgp_auto.R robustamente: prueba rutas relativas y SUBE por los directorios padres,
# así funciona tanto si el wd es caso2/R, caso2/, la raíz del proyecto o caso2/scripts.
.buscar_dgp <- function() {
  rel <- c("dgp_auto.R", "R/dgp_auto.R", "caso2/R/dgp_auto.R", "caso2/dgp_auto.R")
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 0:7) {
    for (r in rel) { p <- file.path(d, r); if (file.exists(p)) return(p) }
    padre <- dirname(d); if (identical(padre, d)) break; d <- padre
  }
  NA_character_
}
.ok <- .buscar_dgp()
if (is.na(.ok)) stop("No encuentro dgp_auto.R. Directorio de trabajo actual: ", getwd(),
                     "\n  Solución: setwd('<ruta>/GLM/caso2/R') y vuelve a ejecutar,",
                     "\n  o abre GLM.Rproj antes de correr el script.")
source(.ok)

# Guarda TODA la salida en un fichero (además de imprimirla en consola), para revisión.
.log <- file.path(dirname(.ok), "validacion_salida.txt")
sink(.log, split = TRUE)

cat("\n== Generando cartera de auto ==\n")
d  <- simular_cartera_auto(semilla = 20252026L)
v  <- attr(d, "verdad"); S <- attr(d, "siniestros"); P <- attr(d, "panel")

# ---- 0 · Sanidad estructural ------------------------------------------------
cat("\n[0] Dimensiones y rangos\n")
cat(sprintf("  pólizas: %d | siniestros: %d | filas panel: %d\n", nrow(d), nrow(S), nrow(P)))
cat(sprintf("  exposicion [%.2f, %.2f] | antiguedad [%.2f, %.2f]\n",
            min(d$exposicion), max(d$exposicion), min(d$antiguedad), max(d$antiguedad)))
cat(sprintf("  %% con siniestro: %.1f | %% baja: %.1f | media n_siniestros: %.3f\n",
            100*mean(d$tuvo_siniestro), 100*mean(d$baja), mean(d$n_siniestros)))
cat(sprintf("  var/media n_siniestros = %.2f  (>1 => sobredispersión por fragilidad; verdad θ=%.2f)\n",
            var(d$n_siniestros)/mean(d$n_siniestros), v$theta_frail))

# ---- 1 · Frecuencia: NB recupera betas de claim y θ de fragilidad -----------
cat("\n[1] Frecuencia (binomial negativa, offset=log(exposicion))\n")
d$edad_tramo <- relevel(factor(d$edad_tramo), ref = "18-25")
m_nb <- tryCatch(glm.nb(n_siniestros ~ edad_tramo + scale(antiguedad_carnet) + scale(potencia_cv) +
                          scale(antiguedad_vehiculo) + scale(log(km_anuales)) + scale(log(densidad)) +
                          uso + tipo_vehiculo + combustible + sexo + offset(log(exposicion)), data = d),
                 error = function(e) { cat("  [glm.nb falló]", conditionMessage(e), "\n"); NULL })
if (!is.null(m_nb)) {
  cat(sprintf("  θ estimado = %.2f  (verdad = %.2f)\n", m_nb$theta, v$theta_frail))
  cat("  IRR estimadas vs verdad (algunas):\n")
  irr <- exp(coef(m_nb))
  comparar <- function(nombre_est, beta_verdad)
    cat(sprintf("    %-22s est %.3f | verdad %.3f\n", nombre_est, irr[nombre_est], exp(beta_verdad)))
  comparar("edad_tramo35-65", v$betas$claim["edad_3565"])
  comparar("usocomercial",    v$betas$claim["uso_com"])
  comparar("scale(log(densidad))", v$betas$claim["z_dens"])
  comparar("tipo_vehiculomoto",   v$betas$claim["tipo_moto"])
  comparar("sexoH",              v$betas$claim["sexo_h"])
}

# ---- 2 · Severidad: Gamma recupera betas de coste ---------------------------
cat("\n[2] Severidad (Gamma, enlace log) a nivel de PARTE\n")
Sm <- merge(S, d[, c("id_poliza","potencia_cv","valor_vehiculo","edad_conductor",
                     "tipo_vehiculo","densidad","sexo")], by = "id_poliza")
Sm$edad_joven <- as.integer(Sm$edad_conductor < 25)
m_g <- tryCatch(glm(coste_k ~ scale(valor_vehiculo) + scale(potencia_cv) + edad_joven +
                      tipo_vehiculo + scale(log(densidad)) + sexo,
                    family = Gamma(link = "log"), data = Sm),
                error = function(e) { cat("  [Gamma falló]", conditionMessage(e), "\n"); NULL })
if (!is.null(m_g)) {
  cat(sprintf("  coste medio estimado (intercepto) = %.0f €  (verdad base = %.0f €)\n",
              exp(coef(m_g)[1]), v$coste_base))
  cat(sprintf("  IRR valor_vehiculo est %.3f | verdad %.3f\n",
              exp(coef(m_g)["scale(valor_vehiculo)"]), exp(v$betas$sev["val_z"])))
  cat(sprintf("  IRR tipo moto      est %.3f | verdad %.3f\n",
              exp(coef(m_g)["tipo_vehiculomoto"]), exp(v$betas$sev["tipo_moto"])))
}

# ---- 3 · Baja: AFT log-logística recupera betas de lapse; Cox viola PH ------
cat("\n[3] Baja / churn: AFT log-logística (no-PH) y test de PH de Cox\n")
m_aft <- tryCatch(survreg(Surv(antiguedad, baja) ~ scale(log(km_anuales)) + scale(potencia_cv) +
                            edad_tramo + uso + financiado, data = d, dist = "loglogistic"),
                  error = function(e) { cat("  [survreg falló]", conditionMessage(e), "\n"); NULL })
if (!is.null(m_aft)) {
  cat(sprintf("  coef AFT uso=comercial est %.3f | verdad %.3f (log-tiempo; - = dura menos)\n",
              coef(m_aft)["usocomercial"], v$betas$lapse["uso_com"]))
  cat(sprintf("  escala AFT est %.3f | verdad σ_l = %.3f\n", m_aft$scale, v$sigma_lapse))
}
m_cox <- tryCatch(coxph(Surv(antiguedad, baja) ~ scale(potencia_cv) + uso + edad_tramo, data = d),
                  error = function(e) NULL)
if (!is.null(m_cox)) {
  zph <- tryCatch(cox.zph(m_cox), error = function(e) NULL)
  if (!is.null(zph)) { cat("  cox.zph (esperamos p pequeño = PH NO se cumple, por diseño AFT):\n")
    print(round(zph$table, 4)) }
}

# ---- 4 · Primer siniestro: forma Weibull del hazard base --------------------
cat("\n[4] Tiempo al primer siniestro: forma Weibull\n")
m_w <- tryCatch(survreg(Surv(t_primer_sin, evento) ~ 1, data = d, dist = "weibull"),
                error = function(e) NULL)
if (!is.null(m_w))
  cat(sprintf("  forma Weibull estimada = %.2f | verdad = %.2f  (survreg scale = 1/forma)\n",
              1 / m_w$scale, v$weib_shape))

# ---- 5 · Bonus-malus: tabla cuadrada de movilidad ---------------------------
cat("\n[5] Bonus-malus: tabla de transición prev -> act\n")
print(table(prev = d$bonus_malus_prev, act = d$bonus_malus_act))

cat("\n== Fin de la validación ==\n")
sink()   # cierra el volcado a fichero
cat("\n(Salida guardada en:", .log, ")\n")
