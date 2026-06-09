# =============================================================================
#  dgp_cohorte.R  ·  Proceso generador de datos (DGP) de la cohorte simulada
#  Curso: Modelos Lineales Generalizados con R · Caso 1
# -----------------------------------------------------------------------------
#  Propósito
#  ---------
#  Generar una cohorte con "verdad conocida" que sirve de columna vertebral a
#  los hilos del Caso 1. De un único proceso generador salen CUATRO respuestas:
#
#    (1) BINARIA / TIEMPO-A-EVENTO  (ever, tiempo, evento)
#        Riesgo en tiempo discreto con enlace cloglog e intercepto aleatorio por
#        centro -> GLMM logístico (Unidad 1.3) y supervivencia discreta (1.4).
#
#    (2) NOMINAL  (clase_nom: A/B/C, sin orden)
#        Logit de categoría base (multinomial), SOLO efectos fijos x1, x2 ->
#        multinomial logit de efectos fijos con nnet::multinom (Unidad 1.3).
#
#    (3) ORDINAL  (sever_ord: Leve < Moderado < Grave)
#        Cumulative logit (odds proporcionales) con intercepto aleatorio por
#        centro -> modelo ordinal mixto con ordinal::clmm (Unidad 1.3).
#
#  Las respuestas categóricas (2) y (3) son INDEPENDIENTES del reloj de
#  supervivencia (opción de diseño: evitan los riesgos competitivos, fuera del
#  alcance del Caso 1). Comparten covariables x1, x2 y la estructura de centros.
#
#  Modelos generadores
#  -------------------
#  Binaria/tiempo:  cloglog(h_{ijt}) = alpha_t + b1*x1 + b2*x2 + u_j,  u_j~N(0,su^2)
#  Nominal:         log(P(C=k)/P(C=A)) = g0_k + g1_k*x1 + g2_k*x2,  k in {B,C}
#  Ordinal:         logit P(S<=k) = theta_k - (bo1*x1 + bo2*x2 + w_j),  w_j~N(0,sw^2)
#
#  Toda la "verdad" (parámetros generadores) se adjunta en attr(.,"verdad").
# =============================================================================

# Semilla del curso: fija toda la cadena generadora -> reproducibilidad total.
SEMILLA_CURSO <- 20252026L

#' Simular la cohorte del Caso 1
#'
#' @param n_centros        Número de centros (grupos para los efectos aleatorios).
#' @param media_por_centro Tamaño medio de centro (Poisson -> desbalance realista).
#' @param beta             Efectos fijos verdaderos c(x1=, x2=) de la BINARIA (cloglog).
#' @param sigma_u          DT del intercepto aleatorio por centro (binaria/tiempo).
#' @param alpha_base       Riesgo base por periodo (su longitud fija Tmax).
#' @param cens_rango       Rango (min,max) del periodo de censura administrativa.
#' @param gamma_nom        Lista con los coef. del NOMINAL: $B y $C, cada uno
#'                         c(int=, x1=, x2=). Categoría base = "A".
#' @param theta_ord        Umbrales del ORDINAL c(theta1, theta2), theta1 < theta2.
#' @param beta_ord         Efectos fijos del ORDINAL c(x1=, x2=).
#' @param sigma_w          DT del intercepto aleatorio por centro del ORDINAL.
#' @param semilla          Semilla aleatoria.
#' @return data.frame a nivel de individuo, con attr(.,"verdad").
simular_cohorte <- function(n_centros        = 24L,
                            media_por_centro = 35L,
                            beta             = c(x1 = 0.85, x2 = -0.65),
                            sigma_u          = 0.70,
                            alpha_base       = c(-3.0, -2.7, -2.5, -2.4, -2.3, -2.2),
                            cens_rango       = c(3L, 6L),
                            gamma_nom        = list(B = c(int = -0.30, x1 = 0.70, x2 = -0.40),
                                                    C = c(int = -0.80, x1 = 1.10, x2 =  0.50)),
                            theta_ord        = c(-0.60, 0.90),
                            beta_ord         = c(x1 = 0.80, x2 = -0.50),
                            sigma_w          = 0.60,
                            semilla          = SEMILLA_CURSO) {
  set.seed(semilla)
  Tmax <- length(alpha_base)

  # ---- Estructura de agrupamiento -------------------------------------------
  n_j <- rpois(n_centros, media_por_centro); n_j[n_j < 5L] <- 5L
  N   <- sum(n_j)
  centro <- factor(rep(seq_len(n_centros), times = n_j))
  u <- rnorm(n_centros, 0, sigma_u)            # intercepto aleatorio (binaria/tiempo)

  # ---- Covariables a nivel de individuo -------------------------------------
  x1 <- rnorm(N)                               # continua estandarizada (p.ej. edad z)
  x2 <- rbinom(N, 1L, 0.45)                    # binaria (p.ej. brazo de tratamiento)

  # ===========================================================================
  # (1) RESPUESTA BINARIA / TIEMPO-A-EVENTO  (cloglog, intercepto aleatorio u_j)
  # ===========================================================================
  eta_ind <- beta["x1"] * x1 + beta["x2"] * x2 + u[as.integer(centro)]
  C <- sample(seq.int(cens_rango[1], cens_rango[2]), N, replace = TRUE)
  tiempo <- integer(N); evento <- integer(N)
  for (i in seq_len(N)) {
    ocurre <- FALSE; horizonte <- min(C[i], Tmax)
    for (t in seq_len(horizonte)) {
      h <- 1 - exp(-exp(alpha_base[t] + eta_ind[i]))   # cloglog^{-1}
      if (rbinom(1L, 1L, h) == 1L) { tiempo[i] <- t; evento[i] <- 1L; ocurre <- TRUE; break }
    }
    if (!ocurre) { tiempo[i] <- horizonte; evento[i] <- 0L }
  }

  # ===========================================================================
  # (2) RESPUESTA NOMINAL  (logit de categoría base; SOLO efectos fijos)
  # ===========================================================================
  eta_B <- gamma_nom$B["int"] + gamma_nom$B["x1"] * x1 + gamma_nom$B["x2"] * x2
  eta_C <- gamma_nom$C["int"] + gamma_nom$C["x1"] * x1 + gamma_nom$C["x2"] * x2
  den   <- 1 + exp(eta_B) + exp(eta_C)
  pA <- 1 / den; pB <- exp(eta_B) / den       # pC = 1 - pA - pB
  un <- runif(N)
  clase_nom <- ifelse(un < pA, "A", ifelse(un < pA + pB, "B", "C"))
  clase_nom <- factor(clase_nom, levels = c("A", "B", "C"))

  # ===========================================================================
  # (3) RESPUESTA ORDINAL  (cumulative logit; intercepto aleatorio w_j)
  # ===========================================================================
  w <- rnorm(n_centros, 0, sigma_w)            # intercepto aleatorio del ordinal
  lp_ord <- beta_ord["x1"] * x1 + beta_ord["x2"] * x2 + w[as.integer(centro)]
  P_le1 <- plogis(theta_ord[1] - lp_ord)       # P(S <= 1)
  P_le2 <- plogis(theta_ord[2] - lp_ord)       # P(S <= 2)
  pO1 <- P_le1; pO2 <- P_le2 - P_le1           # pO3 = 1 - P_le2
  uo <- runif(N)
  sev <- ifelse(uo < pO1, 1L, ifelse(uo < pO1 + pO2, 2L, 3L))
  sever_ord <- factor(sev, levels = 1:3, labels = c("Leve", "Moderado", "Grave"),
                      ordered = TRUE)

  # ---- Ensamblado -----------------------------------------------------------
  d <- data.frame(
    id        = seq_len(N),
    centro    = centro,
    x1        = x1,
    x2        = x2,
    tiempo    = tiempo,
    evento    = evento,
    ever      = as.integer(evento == 1L),   # binaria "ocurrió el evento"
    clase_nom = clase_nom,                  # nominal A/B/C
    sever_ord = sever_ord                   # ordinal Leve<Moderado<Grave
  )

  attr(d, "verdad") <- list(
    binaria = list(beta = beta, sigma_u = sigma_u, alpha_base = alpha_base, Tmax = Tmax,
                   icc_latente = unname(sigma_u^2 / (sigma_u^2 + pi^2 / 3))),
    nominal = list(gamma_B = gamma_nom$B, gamma_C = gamma_nom$C, ref = "A"),
    ordinal = list(theta = theta_ord, beta = beta_ord, sigma_w = sigma_w,
                   icc_latente = unname(sigma_w^2 / (sigma_w^2 + pi^2 / 3))),
    semilla = semilla
  )
  d
}

#' Expandir a formato persona-periodo (una fila por individuo y periodo en riesgo)
#' para el ajuste de supervivencia discreta (Unidad 1.4).
expandir_persona_periodo <- function(d) {
  Tmax <- d |> attr("verdad") |> (\(v) v$binaria$Tmax)()
  do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    Ti <- d$tiempo[i]
    data.frame(id = d$id[i], centro = d$centro[i], x1 = d$x1[i], x2 = d$x2[i],
               periodo = factor(seq_len(Ti), levels = seq_len(Tmax)),
               y = c(rep(0L, Ti - 1L), d$evento[i]))
  }))
}
