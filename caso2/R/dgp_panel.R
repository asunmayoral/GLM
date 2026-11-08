# =============================================================================
#  caso2/R/dgp_panel.R — Panel póliza-año de PARTES (Unidad 2.6, riesgos a trozos)
# -----------------------------------------------------------------------------
#  Proceso INDEPENDIENTE de la cartera de teoría: solo LEE de `cartera` el
#  identificador, la `exposicion` y las covariables; no modifica ninguna columna
#  ni consume el flujo aleatorio de `simular_cartera()`. Las unidades 2.1–2.5 no
#  dependen de este fichero.
#
#  Requiere haber cargado antes R/dgp_conteos.R (usa SEMILLA_CURSO y .base_caso()).
#
#  Modelo generador (simplificado a propósito):
#   - Antigüedad del cliente en años: antiguedad = T_max * exposicion (3 a 10 años).
#     Depende solo de cuándo entró el cliente, NO de su riesgo: no hay abandono
#     selectivo (los clientes de más riesgo no se van antes).
#   - Tasa de partes por año de antigüedad k, constante dentro del año:
#         lambda_ik = h0_k * exp(x_i' beta + u_i),   u_i ~ N(0, sigma_frag^2)
#     h0_k decrece con la antigüedad con mejoras anuales menguantes
#     (7%, 5%, 4%, ... hasta 1% el año 10), según la alternativa realista de
#     Feldblum (1996, nota 38). u_i es la fragilidad de la póliza.
#   - n_partes_anual ~ Poisson(lambda_ik * expo_anual); el último año de cada
#     cliente es incompleto (expo_anual < 1).
#   - `sexo` no interviene (control negativo).
#
#  Salida: data.frame póliza-año con
#    attr(., "partes") -> una fila por parte, con su instante t (años desde el alta)
#    attr(., "verdad") -> parámetros generadores
# =============================================================================

SEMILLA_PANEL <- SEMILLA_CURSO + 6L   # semilla propia: no comparte flujo con la cartera

#' Coeficientes por defecto (escala: continuas estandarizadas; factores frente a
#' rural / particular / turismo)
betas_panel_defecto <- function() {
  c(z_edad = -0.15, z_carnet = -0.10, z_pot = 0.20,
    urbana = 0.20, mixta = 0.08, comercial = 0.15, moto = 0.20, furgoneta = 0.08)
}

#' Simular el panel póliza-año de partes a partir de la cartera de teoría
simular_panel_partes <- function(cartera, semilla = SEMILLA_PANEL, T_max = 10,
                                 tasa_1 = 0.12,
                                 mejora = c(0.07, 0.05, 0.04, 0.03, 0.025, 0.02,
                                            0.015, 0.0125, 0.01),
                                 sigma_frag = 0.6, beta = betas_panel_defecto()) {
  set.seed(semilla)
  N <- nrow(cartera)
  z <- function(v) as.numeric(scale(v))
  X <- with(cartera, cbind(
    z_edad = z(edad_conductor), z_carnet = z(antiguedad_carnet), z_pot = z(potencia_cv),
    urbana = zona_circulacion == "urbana", mixta = zona_circulacion == "mixta",
    comercial = uso == "comercial", moto = tipo_vehiculo == "moto",
    furgoneta = tipo_vehiculo == "furgoneta"))
  eta <- as.numeric(X[, names(beta)] %*% beta)

  h0 <- tasa_1 * cumprod(c(1, 1 - mejora))            # tasa base por año de antigüedad
  u  <- rnorm(N, 0, sigma_frag)                         # fragilidad de cada póliza
  antiguedad <- T_max * cartera$exposicion

  g <- expand.grid(i = seq_len(N), anio = seq_len(T_max))
  g$expo_anual <- pmin(pmax(antiguedad[g$i] - (g$anio - 1), 0), 1)
  g <- g[g$expo_anual > 0, ]
  g <- g[order(g$i, g$anio), ]
  g$n_partes_anual <- rpois(nrow(g), h0[g$anio] * exp(eta[g$i] + u[g$i]) * g$expo_anual)

  # Con tasa constante dentro del año, los instantes de los partes son uniformes
  # en la parte observada del año: permite reconstruir el tiempo al primer parte.
  con <- g[g$n_partes_anual > 0, ]
  rep_i <- rep(seq_len(nrow(con)), con$n_partes_anual)
  partes <- data.frame(
    id_poliza = cartera$id_poliza[con$i[rep_i]],
    anio      = con$anio[rep_i],
    t         = round(con$anio[rep_i] - 1 + runif(length(rep_i)) * con$expo_anual[rep_i], 4))
  partes <- partes[order(partes$id_poliza, partes$t), ]
  rownames(partes) <- NULL

  covs <- c("edad_conductor", "antiguedad_carnet", "potencia_cv", "zona_circulacion",
            "uso", "tipo_vehiculo", "sexo")
  panel <- data.frame(id_poliza = cartera$id_poliza[g$i],
                      antiguedad = round(antiguedad[g$i], 4),
                      anio = g$anio, expo_anual = round(g$expo_anual, 4),
                      n_partes_anual = g$n_partes_anual,
                      cartera[g$i, covs], row.names = NULL)

  attr(panel, "partes") <- partes
  attr(panel, "verdad") <- list(h0 = h0, beta = beta, sigma_frag = sigma_frag,
                                T_max = T_max, tasa_1 = tasa_1, mejora = mejora,
                                u = setNames(u, cartera$id_poliza), semilla = semilla)
  panel
}

#' Cargar el panel con CACHÉ a fichero (mismo criterio que cargar_cartera()).
#' Regenera si no existe, si `refrescar = TRUE`, o si dgp_panel.R o la caché de la
#' cartera son más recientes que el panel guardado. Escribe además un CSV sin
#' atributos (para trabajar a ciegas y para las réplicas en Python).
cargar_panel_partes <- function(cartera, refrescar = FALSE, semilla = SEMILLA_PANEL, ...) {
  base     <- .base_caso("caso2")
  ruta_dgp <- file.path(base, "R", "dgp_panel.R")
  ruta_car <- file.path(base, "datos", sprintf("cartera_auto_%s.rds", SEMILLA_CURSO))
  archivo  <- file.path(base, "datos", sprintf("panel_partes_%s.rds", semilla))
  fuentes  <- c(ruta_dgp, ruta_car)[file.exists(c(ruta_dgp, ruta_car))]
  al_dia   <- file.exists(archivo) && all(file.mtime(archivo) >= file.mtime(fuentes))

  if (!refrescar && al_dia) {
    message("cargar_panel_partes(): leyendo caché  -> ", archivo)
    return(readRDS(archivo))
  }
  message("cargar_panel_partes(): generando datos -> ", archivo)
  panel <- simular_panel_partes(cartera, semilla = semilla, ...)
  saveRDS(panel, archivo)
  utils::write.csv(panel, sub("\\.rds$", ".csv", archivo), row.names = FALSE)
  utils::write.csv(attr(panel, "partes"), sub("panel_partes", "partes", sub("\\.rds$", ".csv", archivo)),
                   row.names = FALSE)
  panel
}
