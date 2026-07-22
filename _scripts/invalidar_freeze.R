#!/usr/bin/env Rscript
# =============================================================================
#  _scripts/invalidar_freeze.R  ·  pre-render de Quarto
# -----------------------------------------------------------------------------
#  PROBLEMA que resuelve: con `execute: freeze: auto`, Quarto sólo re-ejecuta un
#  documento cuando cambia SU fichero .qmd. Pero cada caso es un documento madre
#  (caso3/caso3_continuas_positivas.qmd) que se compone de PARCIALES incluidos
#  ({{< include _unidad_3_1.qmd >}}, ...) y se apoya en scripts de R
#  (R/dgp_*.R). Si editas un parcial o el DGP, el hash del documento madre no
#  cambia, así que Quarto reutiliza la caché congelada y tu edición NO aparece.
#
#  SOLUCIÓN: este script se ejecuta ANTES de cada render (project > pre-render).
#  Para cada caso comprueba si algún fichero del que depende (el .qmd madre,
#  cualquier _unidad_*.qmd, o cualquier .R de R/ y scripts/) es MÁS RECIENTE que
#  su caché en _freeze/. Si lo es, borra esa caché, forzando la re-ejecución de
#  ESE caso —y solo de ese caso: los demás conservan su caché y su rapidez—.
#
#  No necesita paquetes. No toca nada si no hay cambios.
# =============================================================================

# Raíz del proyecto. Quarto ejecuta el pre-render desde la raíz del proyecto y
# expone su ruta en QUARTO_PROJECT_DIR; si no estuviera, usamos el directorio
# de trabajo actual.
raiz <- Sys.getenv("QUARTO_PROJECT_DIR", unset = getwd())
if (nzchar(raiz)) setwd(raiz)

mtime_max <- function(ficheros) {
  ficheros <- ficheros[file.exists(ficheros)]
  if (length(ficheros) == 0) return(NA)
  max(file.mtime(ficheros))
}

# Ficheros dentro de una carpeta de caso de los que depende su render:
#   - el .qmd madre y todos los parciales _unidad_*.qmd
#   - los scripts de R (DGP y utilidades) en R/ y scripts/
dependencias_caso <- function(dir_caso) {
  c(list.files(dir_caso, pattern = "\\.qmd$", full.names = TRUE),
    list.files(file.path(dir_caso, "R"),       pattern = "\\.R$", full.names = TRUE),
    list.files(file.path(dir_caso, "scripts"), pattern = "\\.R$", full.names = TRUE))
}

# Descubre los documentos madre a partir de la lista `render:` del _quarto.yml
# (líneas "    - casoX/....qmd"). Nos quedamos sólo con rutas limpias: sin
# espacios ni ":" —así excluimos las líneas del navbar tipo "- href: caso1/...".
lineas <- readLines("_quarto.yml", warn = FALSE)
madre  <- trimws(sub("^\\s*-\\s*", "", grep("^\\s*-\\s+\\S+\\.qmd\\s*$", lineas, value = TRUE)))
madre  <- unique(madre[grepl("^[^ :]+/[^ :]+\\.qmd$", madre)])   # ruta con carpeta, sin espacios ni ":"

invalidados <- character(0)

for (doc in madre) {
  dir_caso  <- dirname(doc)                       # p. ej. "caso3"
  stem      <- sub("\\.qmd$", "", basename(doc))  # p. ej. "caso3_continuas_positivas"
  dir_freeze <- file.path("_freeze", dir_caso, stem)

  if (!dir.exists(dir_freeze)) next               # sin caché: Quarto ya lo renderiza fresco

  t_freeze <- mtime_max(list.files(dir_freeze, recursive = TRUE, full.names = TRUE))
  t_deps   <- mtime_max(dependencias_caso(dir_caso))

  if (!is.na(t_deps) && !is.na(t_freeze) && t_deps > t_freeze) {
    unlink(dir_freeze, recursive = TRUE, force = TRUE)
    invalidados <- c(invalidados, doc)
  }
}

if (length(invalidados) > 0) {
  message("[invalidar_freeze] caché invalidada (se re-ejecutarán): ",
          paste(invalidados, collapse = ", "))
} else {
  message("[invalidar_freeze] sin cambios: se conserva toda la caché freeze.")
}
