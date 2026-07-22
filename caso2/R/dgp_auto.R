# =============================================================================
#  dgp_auto.R · DGP de la cartera de AUTO (Casos 2 y 3 + proyecto final)
# -----------------------------------------------------------------------------
#  Proceso puntual MARCADO con FRAGILIDAD por póliza. Ver DISENO_DGP_AUTO.md.
#  Un único generador que produce:
#    - un data.frame nivel PÓLIZA (una fila por póliza), con
#        attr(., "siniestros")  -> tabla larga (una fila por parte: t_k, coste_k)
#        attr(., "panel")       -> póliza-año (counting process, offset variable)
#        attr(., "verdad")      -> todos los parámetros generadores
#  Cubre: conteos/tasas (2.1), sobredispersión vía fragilidad (2.2), exceso de
#  ceros (2.3), agrupación (2.4), riesgos a trozos (2.5), selección (2.6),
#  severidad Gamma/IG (Caso 3-O4), AFT de bajas no-PH y Cox (Caso 3-O5),
#  eventos recurrentes (panel), y la síntesis "tres lentes" del proyecto final.
#
#  AVISO: no ejecutado en el entorno de desarrollo. Validar con validar_dgp_auto.R.
# =============================================================================

SEMILLA_CURSO <- 20252026L

# ---- utilidades -------------------------------------------------------------
.z    <- function(v) as.numeric(scale(v))
.clip <- function(v, lo, hi) pmin(pmax(v, lo), hi)

#' Betas por defecto (log-IRR). Todo es argumento; esto es solo el punto de partida.
betas_auto_defecto <- function() list(
  # Intensidad de siniestros (frecuencia). Referencias de edad: tramo joven 18-25.
  claim = c(edad_2535 = log(0.60), edad_3565 = log(0.45), edad_65 = log(0.55),
            z_carnet = -0.05, z_pot = 0.12, z_antv = 0.03, z_km = 0.30, z_dens = 0.35,
            uso_com = 0.22, tipo_moto = 0.18, tipo_furgo = 0.10, comb_diesel = 0.08,
            sexo_h = 0.10),
  # Severidad (coste por parte).
  sev   = c(val_z = 0.26, z_pot = 0.10, edad_joven = 0.22, edad_mayor = 0.14,
            tipo_moto = 0.30, z_dens = 0.05, sexo_h = 0.10),
  # Baja / churn. AFT: efecto sobre log-duración (+ = permanece más).
  lapse = c(z_km = -0.10, z_pot = -0.05, edad_2535 = 0.10, edad_3565 = 0.20,
            edad_65 = 0.15, uso_com = -0.15, financiado = -0.10),
  # Otros conteos.
  asist  = c(edad_2535 = -0.20, edad_3565 = -0.35, edad_65 = -0.10, z_km = 0.25, uso_com = 0.20),
  fraude = c(z_pot = 0.15, uso_com = 0.15, tipo_moto = 0.10),
  gest   = c(edad_3565 = -0.10, z_pot = 0.10, uso_com = 0.15)
)

#' Simular la cartera de auto.
#'
#' @return data.frame nivel póliza con attr("siniestros"), attr("panel"), attr("verdad").
simular_cartera_auto <- function(
    n_regiones = 6L, n_agencias = 30L, media_por_agencia = 100L,
    T_max = 10,                       # horizonte máximo de antigüedad (años)
    sigma_reg = 0.35, sigma_ag = 0.50,
    theta_frail = 1.2,                # fragilidad Gamma: media 1, varianza 1/theta
    weib_shape = 1.15, lambda0 = 0.15,# hazard base de siniestros (Weibull, tasa base/año)
    coste_base = 1800, nu_gamma = 1.8,# severidad Gamma
    lapse_median = 6, sigma_lapse = 0.6,  # baja: mediana (años) y dispersión log-logística
    lambda_asist = 0.45, lambda_fraude = 0.10, lambda_gest = 0.30,
    g_cero = c(int = 0.2, uso = -0.6, pot = -0.3),  # logit de ceros estructurales (fraude)
    persistencia = 0.60,              # inercia del bonus-malus
    betas = NULL, semilla = SEMILLA_CURSO) {

  if (is.null(betas)) betas <- betas_auto_defecto()
  set.seed(semilla)

  # ---- 1 · Estructura y efectos aleatorios ----------------------------------
  n_j    <- rpois(n_agencias, media_por_agencia); n_j[n_j < 20L] <- 20L
  N      <- sum(n_j)
  agencia <- factor(rep(seq_len(n_agencias), times = n_j))
  region_de_ag <- sort(rep_len(seq_len(n_regiones), n_agencias))   # cada agencia -> una región
  region  <- factor(region_de_ag[as.integer(agencia)])
  u_ag    <- rnorm(n_agencias, 0, sigma_ag)
  u_reg   <- rnorm(n_regiones, 0, sigma_reg)
  Z       <- rgamma(N, shape = theta_frail, rate = theta_frail)   # fragilidad, media 1

  # ---- 2 · Covariables ------------------------------------------------------
  edad   <- round(.clip(rnorm(N, 45, 15), 18, 88))
  carnet <- pmax(0, round((edad - 18) * runif(N, 0.35, 1)))
  pot    <- round(.clip(rnorm(N, 110, 38), 55, 320))
  cilind <- round(.clip(pot * runif(N, 12, 18), 800, 5000))         # correlada con potencia
  antv   <- round(.clip(rnorm(N, 8, 5), 0, 25))
  valor  <- round(.clip(rnorm(N, 18000, 8000) + 60 * pot, 1500, 90000))  # ~ potencia
  dens   <- round(.clip(rlnorm(N, log(300), 1.1), 5, 25000))        # hab/km²
  km     <- round(.clip(rlnorm(N, log(12000), 0.5), 1000, 60000))
  uso    <- sample(c("particular", "comercial"), N, TRUE, c(.85, .15))
  tipo   <- sample(c("turismo", "moto", "furgoneta"), N, TRUE, c(.75, .13, .12))
  comb   <- sample(c("gasolina", "diesel"), N, TRUE, c(.45, .55))
  sexo   <- sample(c("M", "H"), N, TRUE, c(.5, .5))

  zona <- cut(dens, breaks = c(-Inf, 150, 1000, Inf), labels = c("rural", "mixta", "urbana"))
  edad_tramo <- cut(edad, breaks = c(-Inf, 25, 35, 65, Inf),
                    labels = c("18-25", "25-35", "35-65", "65+"))

  # Ruido (relación nula por diseño)
  color   <- sample(c("blanco","negro","gris","rojo","otro"), N, TRUE)
  ecivil  <- sample(c("soltero","casado","otro"), N, TRUE, c(.4,.5,.1))
  fpago   <- sample(c("anual","mensual"), N, TRUE, c(.6,.4))
  financ  <- rbinom(N, 1, .35)
  nconduc <- 1L + rpois(N, .4)
  estudios<- sample(c("basico","medio","superior"), N, TRUE, c(.4,.4,.2))
  contacto<- sample(c("presencial","telefonico","telematico"), N, TRUE, c(.6,.25,.15))

  # ---- Matriz de diseño con columnas NOMBRADAS ------------------------------
  X <- cbind(
    edad_2535  = as.integer(edad_tramo == "25-35"),
    edad_3565  = as.integer(edad_tramo == "35-65"),
    edad_65    = as.integer(edad_tramo == "65+"),
    edad_joven = as.integer(edad < 25),
    edad_mayor = as.integer(edad > 65),
    z_carnet   = .z(carnet), z_pot = .z(pot), z_antv = .z(antv),
    z_km       = .z(log(km)), z_dens = .z(log(dens)), val_z = .z(valor),
    uso_com    = as.integer(uso == "comercial"),
    tipo_moto  = as.integer(tipo == "moto"),
    tipo_furgo = as.integer(tipo == "furgoneta"),
    comb_diesel= as.integer(comb == "diesel"),
    sexo_h     = as.integer(sexo == "H"),
    financiado = financ)
  eta <- function(b) as.numeric(X[, names(b), drop = FALSE] %*% b)

  # ---- 3 · Baja / churn (AFT log-logística, NO proporcional) ----------------
  T_baja <- exp(log(lapse_median) + eta(betas$lapse) + sigma_lapse * rlogis(N))
  C_adm  <- runif(N, 1, T_max)                       # ventana administrativa (entrada escalonada)
  antiguedad <- pmax(pmin(T_baja, C_adm), 0.1)   # suelo ~1 mes: evita offsets log(exposicion) degenerados
  baja   <- as.integer(T_baja <= C_adm)
  motivo_fin <- factor(ifelse(baja == 1L, "baja", "fin_estudio"))
  exposicion <- round(antiguedad / T_max, 4)

  # ---- 4 · Siniestros (Weibull PH + fragilidad, marcado con coste) ----------
  A     <- exp(eta(betas$claim)) * Z                 # multiplicador constante en t
  Lam   <- A * lambda0 * antiguedad^weib_shape       # media del nº de partes en [0, antiguedad]
  M     <- rpois(N, Lam)                             # nº de partes por póliza
  mu_sev<- coste_base * exp(eta(betas$sev))          # coste medio por póliza

  sinis <- do.call(rbind, lapply(which(M > 0L), function(i) {
    u  <- sort(runif(M[i], 0, Lam[i]))               # puntos de un Poisson unitario en (0, Lam)
    tk <- (u / (A[i] * lambda0))^(1 / weib_shape)    # inversa del hazard acumulado -> tiempos
    data.frame(id_poliza = i, k = seq_len(M[i]), t_k = round(tk, 4),
               coste_k = round(rgamma(M[i], shape = nu_gamma, rate = nu_gamma / mu_sev[i]), 2),
               anio = pmin(as.integer(ceiling(tk)), as.integer(ceiling(antiguedad[i]))),
               row.names = NULL)
  }))
  if (is.null(sinis)) sinis <- data.frame(id_poliza=integer(), k=integer(), t_k=numeric(),
                                          coste_k=numeric(), anio=integer())

  # Resúmenes nivel póliza
  n_siniestros <- M
  primer <- tapply(sinis$t_k, sinis$id_poliza, min)
  t_primer_sin <- antiguedad
  evento       <- integer(N)
  idx <- as.integer(names(primer)); t_primer_sin[idx] <- primer; evento[idx] <- 1L
  coste_total <- rep(0, N)
  ct <- tapply(sinis$coste_k, sinis$id_poliza, sum); coste_total[as.integer(names(ct))] <- ct
  coste_medio <- ifelse(n_siniestros > 0, coste_total / pmax(n_siniestros, 1), NA_real_)

  # ---- 5 · Otros conteos (offset = log(exposicion)) -------------------------
  mu_asist <- lambda_asist * T_max * exp(eta(betas$asist)) * exposicion
  n_asistencia <- rpois(N, mu_asist)

  pi_cero <- plogis(g_cero["int"] + g_cero["uso"] * X[, "uso_com"] + g_cero["pot"] * X[, "z_pot"])
  mu_fra  <- lambda_fraude * T_max * exp(eta(betas$fraude)) * exposicion
  n_fraude <- ifelse(rbinom(N, 1, pi_cero) == 1L, 0L, rpois(N, mu_fra))

  reg_i <- as.integer(region); ag_i <- as.integer(agencia)
  mu_gest <- lambda_gest * T_max * exp(eta(betas$gest) + u_ag[ag_i] + u_reg[reg_i]) * exposicion
  n_gestiones <- rpois(N, mu_gest)

  # ---- 6 · Bonus-malus por historial (tabla cuadrada) -----------------------
  # partes por póliza-año
  partes_anuales <- function(i) {
    Y <- max(1L, as.integer(ceiling(antiguedad[i])))
    tab <- integer(Y)
    if (M[i] > 0L) {
      yy <- sinis$anio[sinis$id_poliza == i]
      for (y in yy) if (y >= 1L && y <= Y) tab[y] <- tab[y] + 1L
    }
    tab
  }
  bm_prev <- integer(N); bm_act <- integer(N)
  for (i in seq_len(N)) {
    tab <- partes_anuales(i); Y <- length(tab)
    s <- sample(1:5, 1L, prob = c(.15,.25,.30,.20,.10)); path <- integer(Y)
    for (y in seq_len(Y)) {
      if (tab[y] > 0L) s <- min(5L, s + tab[y])
      else if (runif(1) > persistencia) s <- max(1L, s - 1L)
      path[y] <- s
    }
    bm_act[i]  <- path[Y]
    bm_prev[i] <- if (Y >= 2L) path[Y - 1L] else path[Y]
  }

  # ---- 7 · Ensamblado nivel PÓLIZA ------------------------------------------
  d <- data.frame(
    id_poliza = seq_len(N), id_agencia = agencia, id_region = region,
    antiguedad = round(antiguedad, 3), exposicion = exposicion,
    edad_conductor = edad, edad_tramo = edad_tramo, antiguedad_carnet = carnet,
    potencia_cv = pot, cilindrada = cilind, antiguedad_vehiculo = antv,
    valor_vehiculo = valor, densidad = dens, zona_circulacion = zona,
    km_anuales = km,
    uso = factor(uso, levels = c("particular", "comercial")),
    tipo_vehiculo = factor(tipo, levels = c("turismo", "moto", "furgoneta")),
    combustible = factor(comb, levels = c("gasolina", "diesel")),
    sexo = factor(sexo, levels = c("M", "H")),
    color_vehiculo = color, estado_civil = ecivil, forma_pago = fpago,
    financiado = financ, n_conductores = nconduc, nivel_estudios = estudios,
    medio_contacto = contacto,
    n_siniestros = n_siniestros, n_asistencia = n_asistencia,
    n_fraude = n_fraude, n_gestiones = n_gestiones,
    t_primer_sin = round(t_primer_sin, 3), evento = evento,
    t_baja = round(pmin(T_baja, T_max), 3), baja = baja, motivo_fin = motivo_fin,
    coste_total = round(coste_total, 2), coste_medio = round(coste_medio, 2),
    tuvo_siniestro = as.integer(n_siniestros > 0),
    bonus_malus_prev = factor(bm_prev, levels = 1:5, ordered = TRUE),
    bonus_malus_act  = factor(bm_act,  levels = 1:5, ordered = TRUE),
    stringsAsFactors = FALSE)

  # ---- 8 · Panel póliza-año (counting process, offset variable) -------------
  panel <- do.call(rbind, lapply(seq_len(N), function(i) {
    Y <- max(1L, as.integer(ceiling(antiguedad[i]))); tab <- partes_anuales(i)
    tstart <- 0:(Y - 1L); tstop <- pmin(seq_len(Y), antiguedad[i])
    data.frame(
      id_poliza = i, anio = seq_len(Y), tstart = tstart, tstop = round(tstop, 3),
      expo_anual = round(tstop - tstart, 3), n_partes_anual = tab,
      edad_t = edad[i] + (seq_len(Y) - 1L), antig_veh_t = antv[i] + (seq_len(Y) - 1L),
      en_riesgo = 1L, row.names = NULL)
  }))

  # ---- 9 · attr("verdad") ---------------------------------------------------
  attr(d, "siniestros") <- sinis
  attr(d, "panel")      <- panel
  attr(d, "verdad") <- list(
    dominio = "auto", betas = betas,
    theta_frail = theta_frail, weib_shape = weib_shape, lambda0 = lambda0,
    coste_base = coste_base, nu_gamma = nu_gamma,
    lapse_median = lapse_median, sigma_lapse = sigma_lapse,
    sigma_reg = sigma_reg, sigma_ag = sigma_ag, u_ag = u_ag, u_reg = u_reg,
    g_cero = g_cero, persistencia = persistencia,
    T_max = T_max, lambda_asist = lambda_asist, lambda_fraude = lambda_fraude,
    lambda_gest = lambda_gest, N = N, semilla = semilla,
    prohibido = "sexo",
    nota = "sexo tiene efecto REAL (claim y sev ~1.10) pero NO es factor de tarificación legal (Test-Achats).")
  d
}

# ---- Caché a fichero (análoga a cargar_cartera del Caso 2) ------------------
.raiz_glm <- function(desde = getwd()) {
  d <- normalizePath(desde, winslash = "/", mustWork = FALSE)
  repeat { if (file.exists(file.path(d, "_quarto.yml"))) return(d)
    p <- dirname(d); if (identical(p, d)) return(NA_character_); d <- p }
}

cargar_cartera_auto <- function(refrescar = FALSE, semilla = SEMILLA_CURSO, ...) {
  raiz <- .raiz_glm(); base <- if (is.na(raiz)) "." else file.path(raiz, "caso2")
  dir_cache <- file.path(base, "datos")
  archivo <- file.path(dir_cache, sprintf("cartera_auto_full_%s.rds", semilla))
  ruta_dgp <- file.path(base, "R", "dgp_auto.R")
  al_dia <- file.exists(archivo) &&
    (!file.exists(ruta_dgp) || file.mtime(archivo) >= file.mtime(ruta_dgp))
  if (!refrescar && al_dia) { message("cargar_cartera_auto(): caché -> ", archivo); return(readRDS(archivo)) }
  message("cargar_cartera_auto(): generando -> ", archivo)
  d <- simular_cartera_auto(semilla = semilla, ...)
  dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)
  saveRDS(d, archivo); d
}
