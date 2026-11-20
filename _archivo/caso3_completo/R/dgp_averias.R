# =============================================================================
#  dgp_averias.R · DGP del Caso 3 · Averías en una línea de fabricación
# -----------------------------------------------------------------------------
#  Fábrica de mobiliario (mesas de escritorio). Línea de procesos sucesivos
#  (Corte -> Mecanizado -> Lijado -> Ensamblaje -> Acabado). Cada máquina sufre
#  AVERÍAS recurrentes; de cada avería se registra cuándo, de qué tipo y cuánto
#  cuesta repararla. Todo el caso gira sobre la avería, con dos preguntas:
#     ¿cuánto cuesta una avería?  -> coste_euros (Gamma)          (3.1-3.3)
#     ¿cada cuánto se avería?      -> tiempo entre fallos (AFT/Cox) (3.4-3.6)
#  Las une una FRAGILIDAD de máquina (Z): una máquina "mala" falla más Y cuesta
#  más de reparar (efecto aleatorio del GLMM = fragilidad de la supervivencia).
#
#  Mecanismos (verdad conocida en attr(., "verdad")):
#   - fragilidad Gamma Z por máquina (var theta>0): liga frecuencia y coste;
#     el nº de averías por máquina resulta binomial negativa (guiño al Caso 2).
#   - riesgo de fallo creciente con la EDAD (desgaste); depende de proceso, carga,
#     mantenimiento. La CRITICIDAD NO entra en el riesgo, solo en el coste.
#   - mantenimiento preventivo PERIÓDICO (cada intervalo_mant días): efecto
#     protector que DECAE con dias_desde_mant (no proporcional). Plan asignado
#     de forma NO aleatoria (confusión). El intervalo es una palanca de decisión.
#   - coste Gamma(enlace log) con interacción criticidad x proceso, intercepto
#     aleatorio de máquina (= log Z) y pendiente aleatoria en la edad.
#   - censura por ventana de observación; altas escalonadas -> exposición variable.
#
#  Tablas: maquinas · averias · intervalos (gap) · seguimiento (start-stop
#  mensual, covariable temporal) · panel (máquina-año, para Tweedie).
#  Generación en R base (data.frame) para no depender de dplyr.
# =============================================================================

SEMILLA_CURSO <- 20252026L

#' Simular el banco de averías del Caso 3
#' @return lista de data.frames con attr(., "verdad")
simular_averias <- function(
    n_maquinas   = 400L,
    ventana_dias = 1460L,          # 4 años de observación
    lam0         = 0.0013,         # riesgo base diario (calibrado ~25% censura)
    theta_frail  = 0.12,           # varianza de la fragilidad Gamma (Z): calibrada para que
                                   # el conteo siga sobredisperso (var/media ~1.9, nexo NB con el
                                   # Caso 2) e infle phi (~2x) SIN desestabilizar los efectos fijos

    phi_coste    = 1/6,            # dispersión Gamma del coste (shape = 6)
    sigma_pend   = 0.05,           # sd de la pendiente aleatoria (coste ~ edad)
    fecha_inicio = as.Date("2021-01-01"),
    semilla      = SEMILLA_CURSO) {

  set.seed(semilla)
  procesos <- c("Corte", "Mecanizado", "Lijado", "Ensamblaje", "Acabado")
  crits    <- c("Auxiliar", "Importante", "Critica")
  tipos_av <- c("Mecanica", "Electrica", "Hidraulica", "Electronica")
  turnos   <- c("Mañana", "Tarde", "Noche")
  cuello   <- c("Ensamblaje", "Acabado")   # procesos "cuello de botella"

  # ---- Parámetros (la "verdad") ---------------------------------------------
  # Riesgo de fallo (log-hazard diario). La criticidad NO entra aquí.
  b_age_h   <- 0.10                                   # por año de edad (desgaste)
  proc_h    <- c(Corte = 0, Mecanizado = 0.10, Lijado = -0.10,
                 Ensamblaje = 0.15, Acabado = 0.25)
  b_carga_h <- 0.80
  th_mant   <- 1.20; tau_mant <- 60                   # protector que decae
  # Coste (log-media Gamma).
  alpha     <- 6.00
  b_tipo    <- c(Mecanica = 0, Electrica = 0.15, Hidraulica = 0.30, Electronica = 0.45)
  proc_c    <- c(Corte = 0, Mecanizado = 0.10, Lijado = -0.05,
                 Ensamblaje = 0.20, Acabado = 0.15)
  crit_c    <- c(Auxiliar = 0, Importante = 0.35, Critica = 0.80)
  inter_crit_cuello <- c(Auxiliar = 0, Importante = 0.15, Critica = 0.40)  # x proceso cuello
  b_age_c   <- 0.03; b_carga_c <- 0.50

  # ---- Máquinas (basal) -----------------------------------------------------
  proc <- sample(procesos, n_maquinas, replace = TRUE, prob = c(.22, .22, .20, .18, .18))
  # criticidad INDEPENDIENTE del proceso (celdas equilibradas -> la interacción
  # criticidad × fase se estima de forma estable; el EFECTO de la criticidad sí
  # varía por proceso, pero su FRECUENCIA no)
  p_crit <- function(pr) c(.45, .32, .23)
  crit <- vapply(proc, function(pr) sample(crits, 1, prob = p_crit(pr)), character(1))
  fab  <- sample(c("A", "B", "C"), n_maquinas, replace = TRUE, prob = c(.40, .35, .25))
  # ALTAS ESCALONADAS: t_alta = puesta en servicio (días respecto al inicio de la ventana).
  #   t_alta < 0  -> máquina ya operando antes del estudio (flota vieja, edad > 0 al entrar).
  #   t_alta >= 0 -> máquina nueva puesta en servicio DURANTE el estudio (edad 0 al entrar).
  t_alta   <- round(runif(n_maquinas, -8 * 365, 0.60 * ventana_dias))
  entry    <- pmax(0L, as.integer(t_alta))            # día en que empieza la observación
  edad_ent <- (entry - t_alta) / 365                  # edad (años) al ENTRAR en observación
  expo_dias <- ventana_dias - entry                   # exposición (días observados)
  potencia <- round(pmax(rnorm(n_maquinas, 12, 4), 3), 1)
  carga    <- pmin(pmax(rbeta(n_maquinas, 5, 2.5), 0.20), 0.98)
  # Plan NO aleatorio: máquinas viejas y críticas -> más preventivo (confusión)
  logit_prev <- -0.30 + 0.15 * (edad_ent - 4) +
    0.60 * (crit == "Critica") + 0.30 * (crit == "Importante")
  plan <- ifelse(runif(n_maquinas) < plogis(logit_prev), "Preventivo", "Correctivo")
  intervalo_mant <- ifelse(plan == "Preventivo",
                           sample(c(60L, 90L, 120L), n_maquinas, TRUE), NA_integer_)
  # Fragilidad y efectos aleatorios de máquina
  Z <- rgamma(n_maquinas, shape = 1 / theta_frail, scale = theta_frail)  # media 1, var theta
  a_int <- log(Z)                                     # intercepto aleatorio del coste (= log Z)
  b_pend <- rnorm(n_maquinas, 0, sigma_pend)          # pendiente aleatoria (coste ~ edad)
  fecha_alta <- fecha_inicio + t_alta                 # fecha real de puesta en servicio

  maquinas <- data.frame(
    id_maquina = sprintf("M%03d", seq_len(n_maquinas)),
    proceso = factor(proc, procesos), criticidad = factor(crit, crits),
    fabricante = factor(fab), fecha_alta = fecha_alta,
    edad_alta = round(edad_ent, 2),                   # edad al entrar en observación
    exposicion_dias = expo_dias,                      # días observados (varía por el alta)
    potencia_kw = potencia, carga = round(carga, 3),
    plan_mantenimiento = factor(plan, c("Correctivo", "Preventivo")),
    intervalo_mant = intervalo_mant, stringsAsFactors = FALSE)

  # ---- Proceso de averías recurrente (por máquina, minimal repair) ----------
  av_list <- vector("list", n_maquinas)
  gap_list <- vector("list", n_maquinas)
  seg_list <- vector("list", n_maquinas)

  edad_en <- function(t, i) (t - t_alta[i]) / 365     # edad de la máquina i en el día t

  for (i in seq_len(n_maquinas)) {
    d <- entry[i]:(ventana_dias - 1L)                 # días observados de esta máquina
    nd <- length(d)
    edad_t <- edad_en(d, i)
    if (plan[i] == "Preventivo") {
      # mantenimientos periódicos desde la puesta en servicio (incluye los previos al estudio)
      mant <- seq(t_alta[i] + intervalo_mant[i], ventana_dias - 1L, by = intervalo_mant[i])
      k <- findInterval(d, mant)
      dsm <- ifelse(k >= 1, d - mant[pmax(k, 1)], d - t_alta[i])
      meff <- -th_mant * exp(-dsm / tau_mant)
    } else {
      dsm <- rep(NA_real_, nd); meff <- rep(0, nd)
    }
    loglam <- log(lam0) + b_age_h * edad_t + proc_h[[proc[i]]] +
      b_carga_h * (carga[i] - 0.60) + meff + log(Z[i])
    fdays <- d[runif(nd) < 1 - exp(-exp(loglam))]     # días con avería

    # ---- Averías de la máquina i --------------------------------------------
    if (length(fdays) > 0) {
      tf   <- fdays + runif(length(fdays))            # instante continuo dentro del día
      edad_f <- edad_en(tf, i)
      tipo <- sample(tipos_av, length(fdays), replace = TRUE)
      dsm_f <- if (plan[i] == "Preventivo") dsm[match(fdays, d)] else rep(NA_real_, length(fdays))
      turno <- sample(turnos, length(fdays), replace = TRUE)   # control interno (sin efecto)
      inter_term <- ifelse(proc[i] %in% cuello, inter_crit_cuello[[crit[i]]], 0)
      logmu <- alpha + b_tipo[tipo] + proc_c[[proc[i]]] + crit_c[[crit[i]]] + inter_term +
        b_age_c * edad_f + b_carga_c * (carga[i] - 0.60) + a_int[i] + b_pend[i] * (edad_f - 4)
      coste <- rgamma(length(fdays), shape = 1 / phi_coste, scale = exp(logmu) * phi_coste)
      # tiempo_parada: horas que la línea se detiene por la avería (impacto operativo,
      # crece con la criticidad). Etiqueta descriptiva para análisis de tiempos muertos;
      # NO es la exposición (tiempo observado) ni se modela como respuesta.
      media_parada <- c(Auxiliar = 0.5, Importante = 3, Critica = 12)[[crit[i]]]
      tiempo_parada <- round(rgamma(length(fdays), shape = 2, scale = media_parada / 2), 2)
      gaps <- diff(c(entry[i], tf))                   # tiempos entre fallos (1.º desde la entrada)

      av_list[[i]] <- data.frame(
        id_maquina = maquinas$id_maquina[i], dia = tf,
        fecha = fecha_inicio + floor(tf), n_orden = seq_along(fdays),
        antiguedad_anios = round(edad_f, 3), tiempo_desde_anterior = round(gaps, 2),
        tipo_averia = factor(tipo, tipos_av), dias_desde_mant = round(dsm_f, 1),
        turno = factor(turno, turnos), tiempo_parada = tiempo_parada,
        coste_euros = round(coste, 2), stringsAsFactors = FALSE)
    }

    # ---- Intervalos entre fallos (gap-level, para AFT/Cox) ------------------
    t_ev  <- if (length(fdays) > 0) tf else numeric(0)
    starts <- c(entry[i], t_ev)
    stops  <- c(t_ev, ventana_dias)
    evento <- c(rep(1L, length(t_ev)), 0L)            # último gap: censurado
    gap_list[[i]] <- data.frame(
      id_maquina = maquinas$id_maquina[i], n_intervalo = seq_along(starts),
      tiempo_entre = round(stops - starts, 2), evento = evento,
      antiguedad_ini = round(edad_en(starts, i), 3), stringsAsFactors = FALSE)

    # ---- Seguimiento mensual (start-stop, covariable temporal) --------------
    bordes <- seq(entry[i], ventana_dias, by = 30)
    if (bordes[length(bordes)] < ventana_dias) bordes <- c(bordes, ventana_dias)
    ns <- length(bordes) - 1L
    ts0 <- bordes[-length(bordes)]; ts1 <- bordes[-1]
    nf_mes <- vapply(seq_len(ns), function(m) sum(t_ev > ts0[m] & t_ev <= ts1[m]), integer(1))
    mid <- (ts0 + ts1) / 2
    if (plan[i] == "Preventivo") {
      mant <- seq(t_alta[i] + intervalo_mant[i], ventana_dias - 1L, by = intervalo_mant[i])
      km <- findInterval(mid, mant)
      dsm_m <- ifelse(km >= 1, mid - mant[pmax(km, 1)], mid - t_alta[i])
    } else dsm_m <- rep(NA_real_, ns)
    seg_list[[i]] <- data.frame(
      id_maquina = maquinas$id_maquina[i], mes = seq_len(ns),
      tstart = ts0, tstop = ts1, exposicion = ts1 - ts0,
      n_fallos = nf_mes, fallo = as.integer(nf_mes > 0),
      antiguedad = round(edad_en(mid, i), 3),
      dias_desde_mant = round(dsm_m, 1),
      mant_reciente = as.integer(!is.na(dsm_m) & dsm_m <= 45),
      stringsAsFactors = FALSE)
  }

  averias <- do.call(rbind, av_list); rownames(averias) <- NULL
  averias$id_averia <- sprintf("A%05d", seq_len(nrow(averias)))
  intervalos <- do.call(rbind, gap_list); rownames(intervalos) <- NULL
  seguimiento <- do.call(rbind, seg_list); rownames(seguimiento) <- NULL

  # Adjuntar covariables de máquina a las tablas de análisis
  cov_maq <- maquinas[, c("id_maquina", "proceso", "criticidad", "fabricante",
                          "carga", "plan_mantenimiento", "intervalo_mant")]
  averias     <- merge(averias, cov_maq, by = "id_maquina")
  intervalos  <- merge(intervalos, cov_maq, by = "id_maquina")
  seguimiento <- merge(seguimiento, cov_maq, by = "id_maquina")

  # ---- Panel máquina-año (para Tweedie: coste total con ceros y offset) ------
  #  Solo años con exposición > 0 (la máquina estaba en servicio). La exposición
  #  (fracción de año observada) varía por las altas escalonadas -> offset real.
  anios <- ceiling(ventana_dias / 365)
  panel_list <- vector("list", n_maquinas)
  for (i in seq_len(n_maquinas)) {
    av_i <- averias[averias$id_maquina == maquinas$id_maquina[i], ]
    filas <- vector("list", anios)
    for (y in seq_len(anios)) {
      y0 <- (y - 1L) * 365; y1 <- y * 365
      expo <- max(0, min(y1, ventana_dias) - max(y0, entry[i])) / 365   # años observados en el año y
      if (expo <= 0) next                             # aún no estaba en servicio
      en_y <- av_i$dia > y0 & av_i$dia <= y1
      filas[[y]] <- data.frame(
        id_maquina = maquinas$id_maquina[i], anio = y,
        coste_total = round(sum(av_i$coste_euros[en_y]), 2),
        n_averias = sum(en_y), exposicion = round(expo, 3), stringsAsFactors = FALSE)
    }
    panel_list[[i]] <- do.call(rbind, filas)
  }
  panel <- do.call(rbind, panel_list); rownames(panel) <- NULL
  panel <- merge(panel, cov_maq, by = "id_maquina")

  # ---- Ensamblado + "verdad" -------------------------------------------------
  banco <- list(maquinas = maquinas, averias = averias, costes = averias,
                intervalos = intervalos, seguimiento = seguimiento, panel = panel)
  attr(banco, "verdad") <- list(
    n_maquinas = n_maquinas, ventana_dias = ventana_dias,
    theta_frail = theta_frail, phi_coste = phi_coste, sigma_pend = sigma_pend,
    beta_coste = c(intercepto = alpha, b_tipo, proc_c, crit_c,
                   critxcuello = inter_crit_cuello, antiguedad = b_age_c, carga = b_carga_c),
    beta_hazard = c(log_lam0 = log(lam0), antiguedad = b_age_h, proc_h, carga = b_carga_h,
                    mant_inmediato = -th_mant, tau_mant = tau_mant),
    sigma_intercepto_coste = sqrt(theta_frail),   # aprox: sd(log Z)
    semilla = semilla)
  banco
}

# =============================================================================
#  Localización del proyecto y CACHÉ a fichero (patrón de los Casos 1-2)
# =============================================================================
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

#' Carga el banco de averías con CACHÉ a fichero.
#'
#' Genera con `simular_averias()` y guarda en `caso3/datos`. Relee el `.rds` si
#' existe y está al día; regenera si no existe, `refrescar = TRUE`, o
#' `dgp_averias.R` es más reciente que la caché. Exporta además las tablas en CSV.
cargar_averias <- function(refrescar = FALSE, semilla = SEMILLA_CURSO,
                           exportar_csv = TRUE, ...) {
  base      <- .base_caso("caso3")
  ruta_dgp  <- file.path(base, "R", "dgp_averias.R")
  dir_cache <- file.path(base, "datos")
  archivo   <- file.path(dir_cache, sprintf("banco_averias_%s.rds", semilla))

  al_dia <- file.exists(archivo) &&
    (!file.exists(ruta_dgp) || file.mtime(archivo) >= file.mtime(ruta_dgp))
  if (!refrescar && al_dia) {
    message("cargar_averias(): leyendo caché  -> ", archivo)
    return(readRDS(archivo))
  }
  message("cargar_averias(): generando datos -> ", archivo)
  banco <- simular_averias(semilla = semilla, ...)
  dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)
  saveRDS(banco, archivo)
  if (exportar_csv) {
    for (tb in c("maquinas", "averias", "intervalos", "seguimiento", "panel"))
      utils::write.csv(banco[[tb]], file.path(dir_cache, paste0(tb, ".csv")), row.names = FALSE)
  }
  banco
}
