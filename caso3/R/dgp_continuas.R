# =============================================================================
#  dgp_continuas.R  ·  Caso 3 (respuestas continuas positivas)  ·  PLACEHOLDER
# -----------------------------------------------------------------------------
#  A CONSTRUIR (como se hizo dgp_conteos.R del Caso 2). Esquema previsto:
#
#  simular_cartera_c3(contexto = c("auto","salud"), ..., semilla = SEMILLA_CURSO)
#    -> data.frame (1 fila por póliza/paciente) con attr(., "verdad")
#
#  Responde a "¿cuánto?" (magnitud positiva). Columnas de respuesta previstas:
#    - coste_siniestro  : IMPORTE del parte (Gamma; asimétrico; CV constante)      -> 3.1, 3.2
#    - coste_total      : coste por póliza con MASA EN 0 (sin siniestro) + positivo -> 3.3 (Tweedie)
#                         = frecuencia (Poisson, reutiliza el Caso 2) x severidad (Gamma)
#    - tiempo_*         : duración positiva continua (con censura)                  -> 3.5 (AFT)
#
#  Predictores: reutilizar los de la cartera del Caso 2 (edad, potencia, zona, uso,
#  tipo, ...) y la jerarquía region > agencia para los mixtos (3.4).
#
#  Parámetros modificables por equipo (betas de la media, dispersión phi, sigma de
#  los efectos aleatorios, índice p de Tweedie, ...), con attr(., "verdad") para el
#  control de calidad, igual que en el Caso 2.
# =============================================================================

# TODO: implementar simular_cartera_c3() y los generadores por equipo.
