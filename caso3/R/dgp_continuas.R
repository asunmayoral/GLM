# =============================================================================
#  dgp_continuas.R · DGP de respuestas continuas positivas (Caso 3)
# -----------------------------------------------------------------------------
#  Un sistema de MANTENIMIENTO INDUSTRIAL simulado: una empresa opera máquinas
#  repartidas en varias plantas y las sigue en el tiempo. Al ser simulado
#  conocemos su "verdad" (proceso generador), lo que permite VALIDAR los ajustes.
#
#  De un mismo motor salen las tablas que vertebran el caso; cada una AÍSLA un
#  fenómeno y da soporte a una unidad:
#     modelo_base          -> coste de la intervención (Gamma, enlace log)   (3.1-3.2)
#     modelo_longitudinal  -> degradación repetida por máquina (mixtos)      (3.3)
#     modelo_supervivencia -> tiempo al primer fallo, CONTINUO y censurado   (3.4-3.5)
#     modelo_intervalos    -> máquina-intervalo en formato START-STOP:       (3.5-3.6)
#                             covariables dependientes del tiempo para Cox (3.5) y
#                             soporte de las lentes discreta y a trozos (3.6)
#  Y las tablas madre de las que derivan: plantas, maquinas, mediciones,
#  intervenciones.
#
#  Mecanismos incorporados a propósito (verdad conocida para el diagnóstico):
#     - respuestas positivas y asimétricas (Gamma con enlace log; Var = phi*mu^2)
#     - efecto no lineal del tiempo (cuadrático) en degradación y hazard basal
#     - confusión: el plan preventivo NO se asigna al azar (logística)
#     - intercepto y pendiente aleatorios por máquina (dependencia longitudinal)
#     - fragilidad no observada de planta (heterogeneidad contextual)
#     - covariables dependientes del tiempo (carga, temperatura, vibración)
#     - efecto protector del mantenimiento que DECAE con el tiempo (no proporcional)
#     - censura administrativa a t_max; datos ausentes NO completamente al azar
#
#  Toda la "verdad" (coeficientes, sigmas, phi, censura) va en attr(., "verdad").
#  Todos los parámetros del DGP son argumentos de simular_industrial().
#
#  Dependencias de SIMULACIÓN: dplyr, tidyr, purrr, tibble (se invocan con ::).
#  No se attacha MASS: su select() enmascararía a dplyr::select().
# =============================================================================

SEMILLA_CURSO <- 20252026L

# --- Parámetros por defecto del DGP (documentados como argumentos) -----------
#  Se agrupan aquí para tenerlos a la vista; simular_industrial() los expone uno
#  a uno para poder generar variantes de diagnóstico (ver secciones al final).

#' Simular el sistema de mantenimiento industrial del Caso 3
#'
#' @param n_plantas        nº de plantas (nivel superior de agrupación).
#' @param n_maquinas       nº de máquinas seguidas.
#' @param t_max            horizonte de seguimiento (días); censura administrativa.
#' @param ancho_intervalo  anchura (días) de los intervalos de supervivencia.
#' @param sigma_planta     sd de la fragilidad no observada de planta (log-hazard).
#' @param sigma_b0,sigma_b1 sd del intercepto y de la pendiente aleatorios de máquina.
#' @param phi_degrad       dispersión Gamma de la degradación (Var = phi*mu^2).
#' @param forma_coste      "shape" Gamma del coste (mayor -> menos disperso).
#' @param semilla          semilla del DGP (forma parte del nombre de la caché).
#' @return lista de tibbles (plantas, maquinas, mediciones, intervenciones,
#'   modelo_base, modelo_longitudinal, modelo_supervivencia, modelo_intervalos)
#'   con attr(., "verdad").
simular_industrial <- function(
    n_plantas       = 12L,
    n_maquinas      = 600L,
    t_max           = 720L,
    ancho_intervalo = 30L,
    sigma_planta    = 0.28,
    sigma_b0        = 0.28,
    # Pendiente aleatoria: calibrada para ser IDENTIFICABLE en el GLMM de la unidad 3.3.
    # Con el valor anterior (0.018) el efecto acumulado en 2 años era sd ~0.036 frente a
    # 0.32 de residuo, y la componente colapsaba a ~0 al ajustar. Con 0.10 se recupera.
    sigma_b1        = 0.10,
    phi_degrad      = 0.10,
    forma_coste     = 6,
    semilla         = SEMILLA_CURSO) {

  set.seed(semilla)

  visitas_programadas <- seq(0L, t_max, by = ancho_intervalo)

  # ---- 1. Plantas: características observadas + fragilidad no observada -------
  plantas <- tibble::tibble(
    id_planta = factor(sprintf("P%02d", seq_len(n_plantas))),
    zona = factor(sample(c("Norte", "Centro", "Sur"), n_plantas, replace = TRUE),
                  levels = c("Norte", "Centro", "Sur")),
    calidad_mantenimiento = pmin(pmax(rnorm(n_plantas, 0.65, 0.12), 0.30), 0.95),
    temperatura_ambiental_media = rnorm(n_plantas, 24, 4),
    u_planta = rnorm(n_plantas, 0, sigma_planta)   # fragilidad NO observable
  )

  # ---- 2. Máquinas: variables basales + efectos aleatorios individuales ------
  maquinas <- tibble::tibble(
    id_maquina = factor(sprintf("M%04d", seq_len(n_maquinas))),
    id_planta = factor(sample(levels(plantas$id_planta), n_maquinas, replace = TRUE),
                       levels = levels(plantas$id_planta)),
    tipo = factor(sample(c("Prensa", "Torno", "Fresadora"), n_maquinas, TRUE,
                         prob = c(0.35, 0.35, 0.30))),
    fabricante = factor(sample(c("A", "B", "C"), n_maquinas, TRUE,
                               prob = c(0.40, 0.35, 0.25))),
    antiguedad_anios = pmin(pmax(rlnorm(n_maquinas, log(6), 0.55), 0.5), 20),
    capacidad_kw = pmax(rnorm(n_maquinas, 80, 18), 30),
    plan_mantenimiento = factor(sample(c("Correctivo", "Preventivo"), n_maquinas, TRUE,
                                       prob = c(0.45, 0.55)),
                                levels = c("Correctivo", "Preventivo")),
    carga_media = pmin(pmax(rbeta(n_maquinas, 5, 2.5), 0.20), 0.98),
    calidad_inicial = rnorm(n_maquinas, 0, 1),
    b0_maquina = rnorm(n_maquinas, 0, sigma_b0),   # intercepto aleatorio
    b1_maquina = rnorm(n_maquinas, 0, sigma_b1)    # pendiente aleatoria
  ) |>
    dplyr::left_join(plantas, by = "id_planta")

  # Confusión: el plan preventivo NO es aleatorio (máquinas viejas y plantas
  # organizadas lo usan más). Sobreescribimos la asignación con una logística.
  logit_p_prev <- with(maquinas,
    -0.30 + 0.08 * (antiguedad_anios - 6) +
      1.10 * (calidad_mantenimiento - 0.65) + 0.45 * (tipo == "Prensa"))
  maquinas <- maquinas |>
    dplyr::mutate(plan_mantenimiento = factor(
      dplyr::if_else(rbinom(dplyr::n(), 1, plogis(logit_p_prev)) == 1,
                     "Preventivo", "Correctivo"),
      levels = c("Correctivo", "Preventivo")))

  # ---- 3. Mediciones longitudinales: sensores y degradación ------------------
  mediciones_pot <- tidyr::crossing(id_maquina = maquinas$id_maquina,
                                    dia = visitas_programadas) |>
    dplyr::left_join(maquinas, by = "id_maquina") |>
    dplyr::arrange(id_maquina, dia) |>
    dplyr::group_by(id_maquina) |>
    dplyr::mutate(tiempo_anios = dia / 365, indice_visita = dplyr::row_number()) |>
    dplyr::ungroup()

  # Sensores (evolucionan en el tiempo: covariables dependientes del tiempo)
  mediciones_pot <- mediciones_pot |>
    dplyr::mutate(
      estacionalidad = sin(2 * pi * dia / 365),
      carga = pmin(pmax(carga_media + 0.06 * estacionalidad +
                          rnorm(dplyr::n(), 0, 0.055), 0.10), 1.00),
      temperatura = temperatura_ambiental_media + 18 * carga +
        0.25 * antiguedad_anios + 2.5 * estacionalidad + rnorm(dplyr::n(), 0, 2.2),
      vibracion_latente = -1.1 + 0.065 * antiguedad_anios + 1.35 * carga +
        0.022 * tiempo_anios * 365 / 30 + b0_maquina +
        # El 2.2 reescala la pendiente aleatoria para la VIBRACIÓN: al subir sigma_b1 de
        # 0.018 a 0.10 el factor original (12) habría multiplicado la vibración por ~11 y
        # arrastrado el hazard. Con 2.2 la vibración conserva su comportamiento anterior.
        b1_maquina * tiempo_anios * 2.2 + 0.18 * u_planta,
      vibracion = exp(vibracion_latente + rnorm(dplyr::n(), 0, 0.18)))

  # Programación de intervenciones preventivas (~cada 180 días; se realiza
  # según la calidad de mantenimiento de la planta)
  mediciones_pot <- mediciones_pot |>
    dplyr::mutate(
      visita_prog_mant = plan_mantenimiento == "Preventivo" & dia > 0 & dia %% 180 == 0,
      mantenimiento_realizado = visita_prog_mant &
        rbinom(dplyr::n(), 1, pmin(pmax(calidad_mantenimiento, 0.20), 0.98)) == 1) |>
    dplyr::group_by(id_maquina) |>
    dplyr::mutate(
      ultimo_dia_mant = dplyr::if_else(mantenimiento_realizado, as.numeric(dia), NA_real_),
      ultimo_dia_mant = cummax(tidyr::replace_na(ultimo_dia_mant, -Inf)),
      ultimo_dia_mant = dplyr::if_else(is.infinite(ultimo_dia_mant), NA_real_, ultimo_dia_mant),
      dias_desde_mant = dplyr::if_else(is.na(ultimo_dia_mant), as.numeric(dia),
                                       dia - ultimo_dia_mant),
      mantenimiento_reciente = as.integer(!is.na(ultimo_dia_mant) & dias_desde_mant <= 90)) |>
    dplyr::ungroup()

  # Degradación media (efecto del mantenimiento que decae) y respuesta Gamma
  mediciones_pot <- mediciones_pot |>
    dplyr::mutate(
      efecto_mant_degrad = dplyr::if_else(is.na(ultimo_dia_mant), 0,
                                          -0.55 * exp(-dias_desde_mant / 120)),
      eta_degrad = -0.35 + b0_maquina + 0.115 * tiempo_anios +
        b1_maquina * tiempo_anios + 0.018 * tiempo_anios^2 +
        0.034 * antiguedad_anios + 1.15 * carga + 0.010 * (temperatura - 35) +
        0.20 * (tipo == "Prensa") - 0.10 * (fabricante == "B") +
        # Efecto de PLANTA sobre la degradación: calibrado para dar un ICC ~0.11, detectable
        # en el modelo mixto (con 0.12 el ICC era 0.03 y el anidamiento no se identificaba).
        0.45 * u_planta + efecto_mant_degrad,
      media_degradacion = exp(eta_degrad),
      degradacion = rgamma(dplyr::n(), shape = 1 / phi_degrad,
                           scale = media_degradacion * phi_degrad))

  # ---- 4. Tiempo hasta el primer fallo grave (intervalos mensuales) ----------
  seg_pot <- mediciones_pot |>
    dplyr::filter(dia < t_max) |>
    dplyr::transmute(
      id_maquina, id_planta, tipo, fabricante, plan_mantenimiento,
      antiguedad_anios, capacidad_kw, carga_media, calidad_inicial,
      calidad_mantenimiento, u_planta, intervalo = indice_visita,
      tstart = dia, tstop_programado = pmin(dia + ancho_intervalo, t_max),
      exposicion_programada = pmin(dia + ancho_intervalo, t_max) - dia,
      carga, temperatura, vibracion, degradacion,
      mantenimiento_realizado, ultimo_dia_mant, dias_desde_mant, mantenimiento_reciente)

  # Hazard por intervalo: basal creciente y no lineal + efecto del mantenimiento
  # que decae (no proporcional) + covariables (algunas dependientes del tiempo).
  seg_pot <- seg_pot |>
    dplyr::mutate(
      tiempo_central_anios = ((tstart + tstop_programado) / 2) / 365,
      # Intercepto basal calibrado para ~37% de censura administrativa a t_max
      # (antes -7.10 dejaba fallar al ~95% de las máquinas: censura irrealmente baja).
      log_hazard_basal = -8.60 + 0.80 * tiempo_central_anios +
        0.22 * tiempo_central_anios^2,
      efecto_mant_hazard = dplyr::if_else(is.na(ultimo_dia_mant), 0,
                                          -1.45 * exp(-dias_desde_mant / 115)),
      log_hazard = log_hazard_basal + 0.060 * antiguedad_anios +
        1.30 * (carga - 0.65) + 0.028 * (temperatura - 35) +
        0.34 * log(vibracion) + 0.28 * (tipo == "Prensa") -
        0.18 * (fabricante == "B") + 0.22 * u_planta + efecto_mant_hazard,
      hazard_dia = exp(log_hazard),
      prob_fallo_intervalo = 1 - exp(-hazard_dia * exposicion_programada))

  # Generación secuencial del primer fallo por máquina (para cada intervalo)
  simular_fallo_maquina <- function(dm) {
    dm <- dplyr::arrange(dm, tstart)
    fallo_ocurrido <- FALSE
    salida <- vector("list", nrow(dm))
    for (j in seq_len(nrow(dm))) {
      if (fallo_ocurrido) break
      fila <- dm[j, ]
      if (rbinom(1, 1, fila$prob_fallo_intervalo) == 1) {
        u <- runif(1)
        t_local <- -log(1 - u * (1 - exp(-fila$hazard_dia * fila$exposicion_programada))) /
          fila$hazard_dia
        t_local <- min(max(t_local, 0.001), fila$exposicion_programada)
        fila$tstop <- fila$tstart + t_local
        fila$exposicion <- t_local
        fila$fallo <- 1L
        fallo_ocurrido <- TRUE
      } else {
        fila$tstop <- fila$tstop_programado
        fila$exposicion <- fila$exposicion_programada
        fila$fallo <- 0L
      }
      salida[[j]] <- fila
    }
    dplyr::bind_rows(salida)
  }

  seguimiento_intervalos <- seg_pot |>
    dplyr::group_split(id_maquina) |>
    purrr::map_dfr(simular_fallo_maquina)

  # Resumen de supervivencia por máquina (censura administrativa a t_max)
  superv_maq <- seguimiento_intervalos |>
    dplyr::group_by(id_maquina) |>
    dplyr::summarise(tiempo = max(tstop), fallo = max(fallo),
                     n_intervalos_observados = dplyr::n(), .groups = "drop")
  maquinas <- maquinas |> dplyr::left_join(superv_maq, by = "id_maquina")

  # ---- 5. Truncar mediciones tras el fallo -----------------------------------
  mediciones <- mediciones_pot |>
    dplyr::select(id_maquina, id_planta, dia, tiempo_anios, indice_visita,
                  tipo, fabricante, plan_mantenimiento, antiguedad_anios,
                  capacidad_kw, carga_media, carga, temperatura, vibracion,
                  degradacion, media_degradacion, mantenimiento_realizado,
                  ultimo_dia_mant, dias_desde_mant, mantenimiento_reciente) |>
    dplyr::left_join(dplyr::select(superv_maq, id_maquina, tiempo, fallo),
                     by = "id_maquina") |>
    dplyr::filter(dia <= tiempo)

  # ---- 6. Coste de la intervención (respuesta Gamma, enlace log) -------------
  interv_prev <- mediciones |>
    dplyr::filter(mantenimiento_realizado) |>
    dplyr::transmute(id_maquina, id_planta, dia, tipo, fabricante,
                     antiguedad_anios, plan_mantenimiento,
                     tipo_intervencion = "Preventiva",
                     carga, temperatura, vibracion, degradacion)
  interv_corr <- seguimiento_intervalos |>
    dplyr::filter(fallo == 1) |>
    dplyr::transmute(id_maquina, id_planta, dia = tstop, tipo, fabricante,
                     antiguedad_anios, plan_mantenimiento,
                     tipo_intervencion = "Correctiva",
                     carga, temperatura, vibracion, degradacion)
  efecto_coste_planta <- tibble::tibble(
    id_planta = plantas$id_planta,
    u_coste_planta = rnorm(n_plantas, 0, 0.10))   # efecto contextual NO observado

  intervenciones <- dplyr::bind_rows(interv_prev, interv_corr) |>
    dplyr::mutate(tipo_intervencion = factor(tipo_intervencion,
                                             levels = c("Preventiva", "Correctiva"))) |>
    dplyr::left_join(efecto_coste_planta, by = "id_planta") |>
    dplyr::mutate(
      eta_coste = 6.00 + 0.045 * antiguedad_anios + 0.36 * log(degradacion) +
        0.25 * (tipo == "Prensa") + 0.18 * (tipo_intervencion == "Correctiva") +
        0.22 * log(degradacion) * (tipo_intervencion == "Correctiva") + u_coste_planta,
      media_coste = exp(eta_coste),
      coste_euros = rgamma(dplyr::n(), shape = forma_coste, scale = media_coste / forma_coste)) |>
    dplyr::select(id_maquina, id_planta, dia, tipo, fabricante, antiguedad_anios,
                  plan_mantenimiento, tipo_intervencion, carga, temperatura,
                  vibracion, degradacion, coste_euros)

  # ---- 7. Datos ausentes NO completamente al azar (MAR) ----------------------
  p_miss_temp <- plogis(-4.2 + 0.045 * (mediciones$temperatura - 35) +
                          0.20 * log(mediciones$degradacion))
  p_miss_vib  <- plogis(-4.0 + 0.30 * log(mediciones$degradacion))
  mediciones <- mediciones |>
    dplyr::mutate(
      temperatura_observada = dplyr::if_else(
        rbinom(dplyr::n(), 1, p_miss_temp) == 1, NA_real_, temperatura),
      vibracion_observada = dplyr::if_else(
        rbinom(dplyr::n(), 1, p_miss_vib) == 1, NA_real_, vibracion))

  # ---- 8. Tablas específicas por bloque --------------------------------------
  modelo_base <- intervenciones |>
    dplyr::arrange(id_maquina, dia) |>
    dplyr::group_by(id_maquina) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  modelo_longitudinal <- mediciones |>
    dplyr::select(id_maquina, id_planta, dia, tiempo_anios, tipo, fabricante,
                  plan_mantenimiento, antiguedad_anios, carga,
                  temperatura_observada, vibracion_observada, degradacion,
                  mantenimiento_realizado, dias_desde_mant, mantenimiento_reciente,
                  tiempo, fallo)

  modelo_supervivencia <- maquinas |>
    dplyr::select(id_maquina, id_planta, tipo, fabricante, antiguedad_anios,
                  capacidad_kw, carga_media, plan_mantenimiento,
                  calidad_mantenimiento, tiempo, fallo)

  modelo_intervalos <- seguimiento_intervalos |>
    dplyr::mutate(intervalo_factor = factor(intervalo),
                  log_exposicion = log(exposicion), fallo_periodo = fallo) |>
    dplyr::select(id_maquina, id_planta, intervalo, intervalo_factor, tstart, tstop,
                  exposicion, log_exposicion, fallo, fallo_periodo, tipo, fabricante,
                  plan_mantenimiento, antiguedad_anios, carga, temperatura, vibracion,
                  degradacion, mantenimiento_realizado, dias_desde_mant, mantenimiento_reciente)

  # ---- Ensamblado + "verdad" -------------------------------------------------
  banco <- list(
    plantas = plantas, maquinas = maquinas, mediciones = mediciones,
    intervenciones = intervenciones, modelo_base = modelo_base,
    modelo_longitudinal = modelo_longitudinal,
    modelo_supervivencia = modelo_supervivencia, modelo_intervalos = modelo_intervalos)

  attr(banco, "verdad") <- list(
    n_plantas = n_plantas, n_maquinas = n_maquinas, t_max = t_max,
    ancho_intervalo = ancho_intervalo,
    sigma_planta = sigma_planta, sigma_b0 = sigma_b0, sigma_b1 = sigma_b1,
    phi_degrad = phi_degrad, forma_coste = forma_coste,
    # betas del log de la MEDIA de degradación (Gamma, enlace log)
    beta_degrad = c(int = -0.35, tiempo_anios = 0.115, tiempo_anios2 = 0.018,
                    antiguedad = 0.034, carga = 1.15, temp = 0.010,
                    tipoPrensa = 0.20, fabricanteB = -0.10, u_planta = 0.45,
                    mant_inmediato = -0.55, tau_mant_degrad = 120),
    # betas del log de la MEDIA del coste (Gamma, enlace log)
    beta_coste = c(int = 6.00, antiguedad = 0.045, log_degrad = 0.36,
                   tipoPrensa = 0.25, correctiva = 0.18,
                   log_degrad_x_correctiva = 0.22, sd_u_coste_planta = 0.10),
    # log-hazard del primer fallo (Poisson a trozos / cloglog)
    beta_hazard = c(int = -8.60, t = 0.80, t2 = 0.22, antiguedad = 0.060,
                    carga = 1.30, temp = 0.028, log_vibracion = 0.34,
                    tipoPrensa = 0.28, fabricanteB = -0.18, u_planta = 0.22,
                    mant_inmediato = -1.45, tau_mant_hazard = 115),
    semilla = semilla)
  banco
}

# =============================================================================
#  Localización de la carpeta del caso y CACHÉ a fichero (patrón del Caso 2)
# =============================================================================

#' Localiza la raíz del proyecto subiendo hasta `_quarto.yml`.
.raiz_glm <- function(desde = getwd()) {
  d <- normalizePath(desde, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "_quarto.yml"))) return(d)
    p <- dirname(d); if (identical(p, d)) return(NA_character_); d <- p
  }
}
.base_caso <- function(caso) {
  raiz <- .raiz_glm()
  if (is.na(raiz)) stop("No encuentro la raíz del proyecto (_quarto.yml). Abre el proyecto GLM.")
  file.path(raiz, caso)
}

#' Carga el banco industrial con CACHÉ a fichero.
#'
#' Genera el sistema con `simular_industrial()` y lo guarda en `caso3/datos`. En
#' llamadas posteriores relee el `.rds` si existe y está al día. Regenera solo
#' si: el fichero no existe, `refrescar = TRUE`, o `dgp_continuas.R` es más
#' reciente que la caché (has tocado el DGP). El nombre del fichero depende de la
#' semilla, así que distintas versiones no se pisan. Además, escribe cada tabla
#' de análisis como CSV en la misma carpeta para que los scripts sean autónomos.
#'
#' @param refrescar TRUE fuerza regenerar aunque exista la caché.
#' @param semilla   Semilla del DGP (forma parte del nombre del fichero).
#' @param exportar_csv TRUE escribe también las tablas en CSV.
#' @param ...       Otros argumentos para `simular_industrial()`.
#' @return lista de tibbles con attr(., "verdad").
cargar_industrial <- function(refrescar = FALSE, semilla = SEMILLA_CURSO,
                              exportar_csv = TRUE, ...) {
  base      <- .base_caso("caso3")
  ruta_dgp  <- file.path(base, "R", "dgp_continuas.R")
  dir_cache <- file.path(base, "datos")
  archivo   <- file.path(dir_cache, sprintf("banco_industrial_%s.rds", semilla))

  al_dia <- file.exists(archivo) &&
    (!file.exists(ruta_dgp) || file.mtime(archivo) >= file.mtime(ruta_dgp))

  if (!refrescar && al_dia) {
    message("cargar_industrial(): leyendo caché  -> ", archivo)
    return(readRDS(archivo))
  }
  message("cargar_industrial(): generando datos -> ", archivo)
  banco <- simular_industrial(semilla = semilla, ...)
  dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)
  saveRDS(banco, archivo)

  if (exportar_csv) {
    utils::write.csv(banco$plantas[setdiff(names(banco$plantas), "u_planta")],
                     file.path(dir_cache, "plantas.csv"), row.names = FALSE)
    utils::write.csv(
      banco$maquinas[setdiff(names(banco$maquinas),
                             c("u_planta", "b0_maquina", "b1_maquina", "calidad_inicial"))],
      file.path(dir_cache, "maquinas.csv"), row.names = FALSE)
    utils::write.csv(banco$modelo_base,
                     file.path(dir_cache, "modelo_base.csv"), row.names = FALSE)
    utils::write.csv(banco$modelo_longitudinal,
                     file.path(dir_cache, "modelo_longitudinal.csv"), row.names = FALSE)
    utils::write.csv(banco$modelo_supervivencia,
                     file.path(dir_cache, "modelo_supervivencia.csv"), row.names = FALSE)
    utils::write.csv(banco$modelo_intervalos,
                     file.path(dir_cache, "modelo_intervalos.csv"), row.names = FALSE)
    utils::write.csv(banco$intervenciones,
                     file.path(dir_cache, "intervenciones.csv"), row.names = FALSE)
  }
  banco
}
