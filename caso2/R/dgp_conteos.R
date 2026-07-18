# =============================================================================
#  dgp_conteos.R · DGP de conteos (Caso 2)
# -----------------------------------------------------------------------------
#  Un mismo motor estadístico con dos contextos REALISTAS e independientes:
#    - contexto = "auto"  -> cartera de seguro de AUTO      (teoría / esqueleto)
#    - contexto = "salud" -> cohorte de utilización SANITARIA (estudio de caso)
#  Cada contexto tiene sus propias distribuciones de predictores, variables de
#  ruido y COEFICIENTES (signos coherentes con su dominio).
#
#  Cuatro respuestas de conteo, cada una AÍSLA un fenómeno (comparten
#  predictores y exposición):
#     y_limpio -> Poisson bien portada            (2.1 + tablas de contingencia)
#     y_sobre  -> sobredispersión (binomial neg.) (2.2)
#     y_ceros  -> exceso de ceros (zero-inflated) (2.3)
#     y_grupo  -> efecto de grupo aleatorio       (2.4, GLMM)
#  Más: tiempo-a-evento con censura (2.5) y una tabla CUADRADA de cambio (2.1).
#  Toda la "verdad" va en attr(., "verdad"). Todos los parámetros son argumentos.
#
#  Orden de las betas: (intercepto, z_edad, z_exp, z_pot, z_antv,
#                       amb_1, amb_2, uso_2, tip_2, tip_3)
# =============================================================================

SEMILLA_CURSO <- 20252026L

.dic_auto <- list(
  grupo = "agencia", region = "region", id = "id_poliza", exposicion = "exposicion",
  cont  = c(x_edad = "edad_conductor", x_exp = "antiguedad_carnet",
            x_pot = "potencia_cv", x_antv = "antiguedad_vehiculo"),
  f_amb = list(nombre = "zona_circulacion", niveles = c("urbana","mixta","rural"), prob = c(.45,.35,.20)),
  f_uso = list(nombre = "uso",              niveles = c("particular","comercial"), prob = c(.85,.15)),
  f_tip = list(nombre = "tipo_vehiculo",    niveles = c("turismo","moto","furgoneta"), prob = c(.75,.13,.12)),
  y = c(y_limpio = "n_asistencia", y_sobre = "n_danos", y_ceros = "n_fraude", y_grupo = "n_gestiones"),
  tiempo = "tiempo_primer_sin", evento = "evento",
  cat_prev = "bonus_malus_prev", cat_act = "bonus_malus_act",
  betas = list(  # signos AUTO: joven +, experiencia -, potencia +, urbana +, comercial +, moto +
    limpio = c(log(0.50), -0.15, -0.05, 0.15, 0.03, 0.30, 0.12, 0.25, 0.20, 0.10),
    sobre  = c(log(1.50), -0.10, -0.12, 0.20, 0.05, 0.25, 0.10, 0.15, 0.30, 0.12),
    ceros  = c(log(3.50),  0.00,  0.00, 0.25, 0.00, 0.20, 0.05, 0.10, 0.15, 0.05),  # media de conteo alta: exceso de ceros VISIBLE
    grupo  = c(log(0.20), -0.08,  0.00, 0.15, 0.02, 0.20, 0.08, 0.25, 0.10, 0.05),
    evento = c( 0.00,      0.15, -0.10, 0.20, 0.05, 0.20, 0.08, 0.15, 0.20, 0.08)),
  g_cero = c(int = 0.2, b_uso = -0.7, b_pot = -0.4)   # pi estructural ~0.5 (antes ~0.8)
)
.dic_salud <- list(
  grupo = "centro", region = "area_salud", id = "id_paciente", exposicion = "anios_seguimiento",
  cont  = c(x_edad = "edad", x_exp = "anios_desde_dx",
            x_pot = "indice_comorbilidad", x_antv = "imc"),
  f_amb = list(nombre = "ambito",       niveles = c("urbano","mixto","rural"), prob = c(.55,.30,.15)),
  f_uso = list(nombre = "regimen",      niveles = c("privado","publico"),      prob = c(.20,.80)),
  f_tip = list(nombre = "grupo_riesgo", niveles = c("bajo","medio","alto"),    prob = c(.60,.28,.12)),
  y = c(y_limpio = "n_visitas_ap", y_sobre = "n_urgencias",
        y_ceros = "n_ingresos_graves", y_grupo = "n_pruebas"),
  tiempo = "tiempo_primer_ingreso", evento = "evento",
  cat_prev = "nivel_riesgo_prev", cat_act = "nivel_riesgo_act",
  betas = list(  # signos SALUD: mayor edad +, más años desde dx +, comorbilidad +, público +, alto riesgo ++
    limpio = c(log(2.50), 0.20, 0.05, 0.25, 0.05, 0.05, 0.02, 0.10, 0.20, 0.40),
    sobre  = c(log(0.80), 0.15, 0.05, 0.30, 0.05, 0.05, 0.02, 0.20, 0.25, 0.45),
    ceros  = c(log(2.20), 0.10, 0.05, 0.30, 0.05, 0.05, 0.02, 0.15, 0.25, 0.50),  # media de conteo moderada: exceso de ceros VISIBLE (realista para ingresos graves)
    grupo  = c(log(1.00), 0.10, 0.00, 0.20, 0.03, 0.05, 0.02, 0.15, 0.15, 0.30),
    evento = c( 0.00,     0.20, 0.05, 0.30, 0.05, 0.05, 0.02, 0.15, 0.25, 0.45)),
  g_cero = c(int = 0.2, b_uso = -0.5, b_pot = -0.8)   # pi estructural ~0.5 (antes ~0.7)
)

#' Betas por defecto de un contexto (para el generador de variantes)
betas_defecto <- function(contexto = c("auto","salud")) {
  contexto <- match.arg(contexto)
  (if (contexto == "auto") .dic_auto else .dic_salud)$betas
}

#' Simular la cartera/cohorte de conteos del Caso 2
#' @return data.frame (1 fila por póliza/paciente) con attr(., "verdad")
simular_cartera <- function(
    contexto = c("auto", "salud"),
    n_grupos = 30L, media_por_grupo = 100L, n_regiones = 6L,
    sigma_a = 0.60, sigma_reg = 0.50, theta_nb = 1.0,
    b_limpio = NULL, b_sobre = NULL, b_ceros = NULL, b_grupo = NULL, b_evento = NULL,
    g_cero = NULL, h0_tramo = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.10),
    persistencia = 0.55, semilla = SEMILLA_CURSO) {

  contexto <- match.arg(contexto)
  dic <- if (contexto == "auto") .dic_auto else .dic_salud
  if (is.null(b_limpio)) b_limpio <- dic$betas$limpio
  if (is.null(b_sobre))  b_sobre  <- dic$betas$sobre
  if (is.null(b_ceros))  b_ceros  <- dic$betas$ceros
  if (is.null(b_grupo))  b_grupo  <- dic$betas$grupo
  if (is.null(b_evento)) b_evento <- dic$betas$evento
  if (is.null(g_cero))   g_cero   <- dic$g_cero
  set.seed(semilla)
  clip <- function(v, lo, hi) pmin(pmax(v, lo), hi)

  # ---- Grupos y efecto aleatorio --------------------------------------------
  n_j   <- rpois(n_grupos, media_por_grupo); n_j[n_j < 20L] <- 20L
  N     <- sum(n_j)
  grupo <- factor(rep(seq_len(n_grupos), times = n_j))
  u_j   <- rnorm(n_grupos, 0, sigma_a)
  region_ag <- factor(sort(rep_len(seq_len(n_regiones), n_grupos)))   # cada agencia -> una región (determinista, sin RNG)
  exposicion <- round(runif(N, 0.30, 1.00), 3)

  # ---- Predictores continuos ACTIVOS (distribuciones por contexto) ----------
  if (contexto == "auto") {
    v_edad <- round(clip(rnorm(N, 45, 14), 18, 88))               # edad del conductor
    v_exp  <- pmax(0, round((v_edad - 18) * runif(N, 0.30, 1.00)))# años de carnet
    v_pot  <- round(clip(rnorm(N, 110, 38), 55, 320))            # potencia (CV)
    v_antv <- round(clip(rnorm(N, 8, 5), 0, 25))                 # antigüedad del vehículo
  } else {
    v_edad <- round(clip(rnorm(N, 52, 18), 18, 95))              # edad del paciente
    v_exp  <- round(clip(rgamma(N, shape = 1.6, scale = 4), 0, 30))   # años desde el diagnóstico
    v_pot  <- pmin(rpois(N, 2.5), 12L)                          # índice de comorbilidad (0-12)
    v_antv <- round(clip(rnorm(N, 27, 5), 16, 45), 1)           # IMC
  }
  z <- function(v) as.numeric(scale(v))
  z_edad <- z(v_edad); z_exp <- z(v_exp); z_pot <- z(v_pot); z_antv <- z(v_antv)

  # ---- Predictores categóricos ACTIVOS --------------------------------------
  amb <- sample(dic$f_amb$niveles, N, TRUE, dic$f_amb$prob)
  uso <- sample(dic$f_uso$niveles, N, TRUE, dic$f_uso$prob)
  tip <- sample(dic$f_tip$niveles, N, TRUE, dic$f_tip$prob)
  d_a1 <- as.integer(amb == dic$f_amb$niveles[1])
  d_a2 <- as.integer(amb == dic$f_amb$niveles[2])
  d_u2 <- as.integer(uso == dic$f_uso$niveles[2])
  d_t2 <- as.integer(tip == dic$f_tip$niveles[2])
  d_t3 <- as.integer(tip == dic$f_tip$niveles[3])
  X   <- cbind(1, z_edad, z_exp, z_pot, z_antv, d_a1, d_a2, d_u2, d_t2, d_t3)
  eta <- function(b) as.numeric(X %*% b)

  # ---- Predictores de RUIDO / extra (realistas por contexto) ----------------
  if (contexto == "auto") {
    ruido <- data.frame(
      sexo = sample(c("H","M"), N, TRUE), estado_civil = sample(c("soltero","casado","otro"), N, TRUE, c(.4,.5,.1)),
      color_vehiculo = sample(c("blanco","negro","gris","rojo","otro"), N, TRUE),
      tiene_garaje = rbinom(N, 1, .55), forma_pago = sample(c("anual","mensual"), N, TRUE, c(.6,.4)),
      financiado = rbinom(N, 1, .35), km_declarados = round(pmax(0, rnorm(N, 12000, 5000))),
      valor_vehiculo = round(pmax(1500, rnorm(N, 18000, 8000))), n_conductores = 1L + rpois(N, .4),
      antiguedad_cliente = round(runif(N, 0, 20)), stringsAsFactors = FALSE)
  } else {
    ruido <- data.frame(
      sexo = sample(c("H","M"), N, TRUE), estado_civil = sample(c("soltero","casado","otro"), N, TRUE, c(.35,.5,.15)),
      nivel_estudios = sample(c("basico","medio","superior"), N, TRUE, c(.4,.4,.2)),
      habito_tabaco = sample(c("no","exfumador","fumador"), N, TRUE, c(.55,.25,.20)),
      seguro_privado = rbinom(N, 1, .22),
      medio_contacto = sample(c("presencial","telefonico","telematico"), N, TRUE, c(.6,.25,.15)),
      n_farmacos_cronicos = rpois(N, 2), gasto_farmacia_anual = round(pmax(0, rnorm(N, 350, 300))),
      n_convivientes = rpois(N, 1.5), anios_afiliado = round(runif(N, 0, 30)), stringsAsFactors = FALSE)
  }

  # ---- Respuestas de conteo (cada una aísla un fenómeno) --------------------
  y_limpio <- rpois(N, exposicion * exp(eta(b_limpio)))                         # Poisson limpia
  y_sobre  <- rnbinom(N, size = theta_nb, mu = exposicion * exp(eta(b_sobre)))  # sobredispersión (NB)
  pi_cero  <- plogis(g_cero["int"] + g_cero["b_uso"] * d_u2 + g_cero["b_pot"] * z_pot)
  y_ceros  <- ifelse(rbinom(N, 1, pi_cero) == 1L, 0L, rpois(N, exposicion * exp(eta(b_ceros))))
  u_reg    <- rnorm(n_regiones, 0, sigma_reg)                          # efecto aleatorio de REGIÓN (justo antes de y_grupo: no altera respuestas previas)
  reg_obs  <- as.integer(region_ag)[as.integer(grupo)]                # región de cada póliza
  y_grupo  <- rpois(N, exposicion * exp(eta(b_grupo) + u_reg[reg_obs] + u_j[as.integer(grupo)]))  # anidado: región > agencia > póliza

  # ---- Tiempo-a-evento (tramos discretos) -----------------------------------
  K <- length(h0_tramo); lp_e <- eta(b_evento)
  tiempo <- integer(N); evento <- integer(N)
  for (i in seq_len(N)) {
    horiz <- max(1L, ceiling(exposicion[i] * K)); ok <- FALSE
    for (k in seq_len(horiz)) {
      if (rbinom(1L, 1L, 1 - exp(-h0_tramo[k] * exp(lp_e[i]))) == 1L) {
        tiempo[i] <- k; evento[i] <- 1L; ok <- TRUE; break }
    }
    if (!ok) { tiempo[i] <- horiz; evento[i] <- 0L }
  }

  # ---- Tabla CUADRADA de cambio (nivel de riesgo prev -> act) ----------------
  P <- matrix((1 - persistencia) / 4, 5, 5); diag(P) <- persistencia
  cat_prev <- sample(1:5, N, TRUE, c(.15,.20,.30,.20,.15)); cat_act <- integer(N)
  for (i in seq_len(N)) {
    p <- P[cat_prev[i], ]
    if (evento[i] == 1L && cat_prev[i] < 5L) {
      p[cat_prev[i]] <- p[cat_prev[i]] - 0.15; p[cat_prev[i] + 1L] <- p[cat_prev[i] + 1L] + 0.15 }
    cat_act[i] <- sample(1:5, 1L, prob = pmax(p, 0))
  }

  # ---- Ensamblado (nombres genéricos) + renombrado por contexto -------------
  d <- data.frame(
    id = seq_len(N), region = region_ag[as.integer(grupo)], grupo = grupo, exposicion = exposicion,
    x_edad = v_edad, x_exp = v_exp, x_pot = v_pot, x_antv = v_antv,
    f_amb = factor(amb, dic$f_amb$niveles), f_uso = factor(uso, dic$f_uso$niveles),
    f_tip = factor(tip, dic$f_tip$niveles), ruido,
    cat_prev = factor(cat_prev, 1:5, ordered = TRUE), cat_act = factor(cat_act, 1:5, ordered = TRUE),
    y_limpio = y_limpio, y_sobre = y_sobre, y_ceros = y_ceros, y_grupo = y_grupo,
    tiempo = tiempo, evento = evento, stringsAsFactors = FALSE)

  ren <- c(id = dic$id, region = dic$region, grupo = dic$grupo, exposicion = dic$exposicion,
           x_edad = dic$cont[["x_edad"]], x_exp = dic$cont[["x_exp"]],
           x_pot = dic$cont[["x_pot"]], x_antv = dic$cont[["x_antv"]],
           f_amb = dic$f_amb$nombre, f_uso = dic$f_uso$nombre, f_tip = dic$f_tip$nombre,
           cat_prev = dic$cat_prev, cat_act = dic$cat_act,
           y_limpio = dic$y[["y_limpio"]], y_sobre = dic$y[["y_sobre"]],
           y_ceros = dic$y[["y_ceros"]], y_grupo = dic$y[["y_grupo"]],
           tiempo = dic$tiempo, evento = dic$evento)
  for (viejo in names(ren)) names(d)[names(d) == viejo] <- ren[[viejo]]

  attr(d, "verdad") <- list(
    contexto = contexto,
    betas = list(limpio = b_limpio, sobre = b_sobre, ceros = b_ceros, grupo = b_grupo, evento = b_evento),
    nombres_beta = c("(int)","z_edad","z_exp","z_pot","z_antv",
                     paste0(dic$f_amb$niveles[1]), paste0(dic$f_amb$niveles[2]),
                     dic$f_uso$niveles[2], dic$f_tip$niveles[2], dic$f_tip$niveles[3]),
    sigma_a = sigma_a, sigma_reg = sigma_reg, n_regiones = n_regiones, theta_nb = theta_nb, g_cero = g_cero,
    h0_tramo = h0_tramo, K = K, persistencia = persistencia, semilla = semilla)
  d
}

#' Expandir a formato póliza-tramo (persona-periodo) para los riesgos a trozos (2.5)
expandir_poliza_tramo <- function(d, col_tiempo, col_evento, col_id = names(d)[1]) {
  K <- attr(d, "verdad")$K
  Ti <- d[[col_tiempo]]; ev <- d[[col_evento]]
  do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    data.frame(id = d[[col_id]][i], tramo = factor(seq_len(Ti[i]), levels = seq_len(K)),
               y = c(rep(0L, Ti[i] - 1L), ev[i]),
               d[i, setdiff(names(d), c(col_tiempo, col_evento)), drop = FALSE], row.names = NULL)
  }))
}

#' Localiza la carpeta de un caso subiendo hasta la raíz del proyecto
#' (donde está `_quarto.yml`). Robusto a cualquier directorio de trabajo.
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

#' Carga la cartera con CACHÉ a fichero.
#'
#' Genera los datos con `simular_cartera()` y los guarda en `datos/`. En llamadas
#' posteriores relee el `.rds` si existe y está al día. Regenera solo si: el
#' fichero no existe, `refrescar = TRUE`, o `dgp_conteos.R` es más reciente que la
#' caché (es decir, si has tocado el DGP). El fichero de caché depende del
#' contexto y de la semilla, así que distintas cohortes no se pisan.
#'
#' @param contexto  "auto" (teoría) o "salud" (estudio de caso).
#' @param refrescar TRUE fuerza regenerar aunque exista la caché.
#' @param semilla   Semilla del DGP (forma parte del nombre del fichero).
#' @param ...       Otros argumentos para `simular_cartera()`.
cargar_cartera <- function(contexto = "auto", refrescar = FALSE,
                           semilla = SEMILLA_CURSO, ...) {
  base      <- .base_caso("caso2")
  ruta_dgp  <- file.path(base, "R", "dgp_conteos.R")
  dir_cache <- file.path(base, "datos")
  archivo   <- file.path(dir_cache, sprintf("cartera_%s_%s.rds", contexto, semilla))

  al_dia <- file.exists(archivo) &&
    (!file.exists(ruta_dgp) || file.mtime(archivo) >= file.mtime(ruta_dgp))

  if (!refrescar && al_dia) {
    message("cargar_cartera(): leyendo caché  -> ", archivo)
    return(readRDS(archivo))
  }
  message("cargar_cartera(): generando datos -> ", archivo)
  datos <- simular_cartera(contexto, semilla = semilla, ...)
  dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)
  saveRDS(datos, archivo)
  datos
}
