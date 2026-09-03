# ---------------------------------------------------------------------------
#  Factor individual de implicación (w) a partir de la coevaluación 360
#  Modelos Lineales Generalizados · Grado en Ciencia de Datos e IA
#
#  Entrada : exportación CSV de la hoja de respuestas del formulario, con las
#            columnas A./C1./C2./C3. (NOMBRE, CUMPLIMIENTO, COLABORACIÓN, ...)
#  Salida  : una fila por (proyecto, equipo, estudiante) con R, w y los avisos
#            que exigen revisión con la bitácora.
# ---------------------------------------------------------------------------

library(tidyverse)

ruta_respuestas <- "evaluacion/coevaluacion_respuestas.csv"
ruta_salida     <- "evaluacion/coevaluacion_factores.csv"

PESO_P1 <- 2      # 'su parte' recoge cumplimiento y contribución
PESO_P2 <- 1      # 'la parte de los demás' recoge colaboración
TOL     <- 0.80   # franja de indiferencia
TOPE_SIN_RESPUESTA <- 0.80

# --- 1. Lectura y limpieza de claves -----------------------------------------
# Los nombres viajan como texto libre entre el bloque A (autoevaluación) y los
# bloques C: hay que normalizarlos o el mismo estudiante genera dos identidades.
normalizar <- function(x) {
  x |>
    iconv(to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all("[^A-Z, ]", "") |>
    str_squish()
}

nivel <- function(x) as.numeric(str_extract(x, "\\d"))   # "2. En gran parte" -> 2

respuestas <- read_csv(ruta_respuestas, show_col_types = FALSE) |>
  rename(marca = `Marca temporal`) |>
  mutate(evaluador = normalizar(`A. NOMBRE`)) |>
  arrange(marca) |>
  distinct(evaluador, PROYECTO, .keep_all = TRUE)   # si reenvía, vale el último

# --- 2. De formato ancho a un registro por par (evaluador, evaluado) ----------
# El bloque A es la autoevaluación: entra en el cálculo como un voto más, y es
# lo que hace que cada estudiante reciba tres valoraciones en equipos de tres.
largo <- map_dfr(c("A", "C1", "C2", "C3"), \(b) {
  respuestas |>
    transmute(
      proyecto = PROYECTO, equipo = EQUIPO, evaluador,
      evaluado = normalizar(.data[[str_glue("{b}. NOMBRE")]]),
      p1       = nivel(.data[[str_glue("{b}. CUMPLIMIENTO")]]),
      p2       = nivel(.data[[str_glue("{b}. COLABORACIÓN")]]),
      matiz    = .data[[str_glue("{b}. MATIZ")]]
    )
}) |>
  filter(!is.na(evaluado), evaluado != "", !is.na(p1), !is.na(p2)) |>
  mutate(s = (PESO_P1 * p1 + PESO_P2 * p2) / (PESO_P1 + PESO_P2))

# --- 3. Normalización por evaluador (algoritmo WebPA) ------------------------
# m es el número de personas que ESE evaluador ha valorado, no el tamaño del
# equipo: así los índices que emite promedian 1 aunque haya dejado a alguien sin
# valorar. Quien puntúa igual a todos emite omega = 1 para todos (incluido el
# caso de puntuarlos a todos con 0): no diferencia, luego no informa.
indices <- largo |>
  group_by(proyecto, equipo, evaluador) |>
  mutate(
    m     = n(),
    suma  = sum(s),
    omega = if_else(suma == 0, 1, s / suma * m)
  ) |>
  ungroup()

# --- 4. Agregación por evaluado y factor -------------------------------------
factores <- indices |>
  group_by(proyecto, equipo, evaluado) |>
  summarise(
    n_votos      = n(),
    R            = median(omega),
    discrepancia = max(omega) - min(omega),
    .groups      = "drop"
  ) |>
  mutate(w = pmin(1, R / TOL))

# --- 5. Salvaguardas ---------------------------------------------------------
emisores <- indices |>
  distinct(proyecto, equipo, evaluado = evaluador) |>
  mutate(respondio = TRUE)

factores <- factores |>
  left_join(emisores, by = c("proyecto", "equipo", "evaluado")) |>
  mutate(
    respondio = coalesce(respondio, FALSE),
    w         = if_else(respondio, w, pmin(w, TOPE_SIN_RESPUESTA)),
    # Ningún descuento se aplica solo con votos: todo w < 1 pasa por bitácora.
    # La discrepancia alta señala desacuerdo entre evaluadores, no free-riding.
    aviso = case_when(
      !respondio          ~ "no cumplimentó la coevaluación",
      n_votos < 3         ~ "menos de tres votos: la mediana no protege",
      discrepancia > 0.60 ~ "evaluadores muy discrepantes",
      w < 1               ~ "descuento pendiente de confirmar con bitácora",
      TRUE                ~ ""
    )
  ) |>
  arrange(proyecto, equipo, desc(w))

write_csv(factores, ruta_salida)

# --- 6. Justificaciones asociadas a las valoraciones bajas -------------------
# El formulario exige una línea cuando se marca 1 o 0; es el material con el que
# se resuelve la revisión.
justificaciones <- largo |>
  filter(p1 <= 1 | p2 <= 1) |>
  select(proyecto, equipo, evaluado, evaluador, p1, p2, matiz)

print(factores, n = Inf)
print(justificaciones, n = Inf)
