# =============================================================================
#  generar_scripts_unidades.R · Regenera los scripts de unidad de CUALQUIER caso
# -----------------------------------------------------------------------------
#  Extrae los chunks de código de cada _unidad_C_U.qmd y los vuelca a un script
#  ejecutable. Cada bloque va precedido de su LABEL y de la RUTA COMPLETA de
#  encabezados donde aparece (niveles # / ## / ### / ####), de modo que se sepa
#  siempre de qué punto del documento procede el código.
#
#  USO (desde cualquier carpeta DENTRO del proyecto; la ruta del source() sí
#  depende de tu directorio de trabajo, el resto no):
#      source("_scripts/generar_scripts_unidades.R")   # define la función
#      generar_scripts()                # todos los casos que encuentre
#      generar_scripts(casos = 2)       # solo el caso 2
#      generar_scripts(casos = 3, solo_faltantes = TRUE)   # sin pisar lo existente
#      generar_scripts(simular = TRUE)  # enseña qué haría, sin escribir nada
#
#  QUÉ HACE, por caso:
#    · localiza el documento maestro (el .qmd que no empieza por `_unidad_`);
#    · toma como PREÁMBULO todos los chunks del maestro anteriores al primer
#      `{{< include ... >}}` —es decir, setup de librerías y carga de datos—,
#      reescribiendo las rutas relativas `source("R/x.R")` para que funcionen
#      desde cualquier carpeta;
#    · respeta la convención de nombres ya existente en `casoC/scripts/`
#      (con o sin guion bajo inicial), para no crear duplicados.
#
#  AVISO: sobrescribe los scripts con una plantilla uniforme. Si un script tiene
#  una cabecera escrita a mano que quieres conservar, usa `solo_faltantes = TRUE`
#  o pásalo en `excluir`.
# =============================================================================

generar_scripts <- function(casos = NULL,
                            solo_faltantes = FALSE,
                            excluir = character(0),
                            simular = FALSE,
                            maestro_tambien = TRUE,
                            raiz = NULL) {

  # ---- 1 · Raíz del proyecto (independiente del directorio de trabajo) ------
  if (is.null(raiz)) {
    raiz <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!file.exists(file.path(raiz, "_quarto.yml")) && dirname(raiz) != raiz)
      raiz <- dirname(raiz)
  }
  if (!file.exists(file.path(raiz, "_quarto.yml")))
    stop("No encuentro _quarto.yml. Abre el proyecto GLM o pasa `raiz = '<ruta>'`.")

  if (is.null(casos)) {
    casos <- as.integer(sub("^caso", "",
                            basename(Sys.glob(file.path(raiz, "caso[0-9]")))))
    casos <- sort(casos[!is.na(casos)])
  }

  sep    <- strrep("-", 77)
  limpia <- function(x) {
    x <- sub("\\s*\\{#[^}]*\\}\\s*$", "", x)   # ancla
    x <- gsub("\\*\\*|\\*|`", "", x)           # negrita / cursiva / código
    trimws(x)
  }

  # ---- Utilidades compartidas ----------------------------------------------
  # Reescribe source("R/x.R") -> source(file.path(.raiz, "casoC", "R", "x.R"))
  reescribe_rutas <- function(lineas, cs) {
    vapply(lineas, function(x) {
      m <- regmatches(x, regexec('source\\("R/([^"]+)"\\)', x))[[1]]
      if (length(m) == 2L) {
        partes <- strsplit(m[2], "/", fixed = TRUE)[[1]]
        sub(m[1], sprintf('source(file.path(.raiz, "caso%s", "R", %s))',
                          cs, paste0('"', partes, '"', collapse = ", ")),
            x, fixed = TRUE)
      } else x
    }, character(1), USE.NAMES = FALSE)
  }

  recorta <- function(x) {                     # quita blancos al principio y al final
    while (length(x) && !nzchar(trimws(x[1])))          x <- x[-1]
    while (length(x) && !nzchar(trimws(x[length(x)])))  x <- x[-length(x)]
    x
  }

  # Formatea la ruta de encabezados: en una línea si cabe, si no en cascada.
  cabecera_ruta <- function(ruta) {
    ruta <- ruta[nzchar(ruta)]
    if (!length(ruta)) return("#   (sin sección)")
    una <- paste(ruta, collapse = " > ")
    if (nchar(una) <= 72) return(paste0("#   ", una))
    paste0("#   ", strrep("  ", seq_along(ruta) - 1L),
           ifelse(seq_along(ruta) == 1L, "", "> "), ruta)
  }

  # ---- Extractor único: chunks + ruta de encabezados ------------------------
  #  Devuelve una lista con `bloques` (líneas ya formateadas) y `n` (nº chunks).
  #  Rastrea CUATRO niveles de encabezado y olvida los hijos al subir de nivel.
  #
  #  Dos fuentes de ruido que se ignoran a propósito:
  #    · las líneas interiores de un chunk (el bucle salta por encima), así que
  #      los comentarios "# ..." del código R no contaminan la ruta;
  #    · los TÍTULOS DE LOS CALLOUTS, que en Quarto se escriben con "##" dentro
  #      de un bloque ":::" y no son secciones del documento.
  extraer_bloques <- function(txt, cs) {
    niveles <- rep("", 4L)
    div <- 0L                                   # profundidad de bloques ":::"
    out <- character(0); n <- 0L; i <- 1L
    while (i <= length(txt)) {
      l <- txt[i]

      # ¿apertura o cierre de un div (callout, columnas, panel-tabset...)?
      if (grepl("^:::+\\s*\\{", l))      div <- div + 1L
      else if (grepl("^:::+\\s*$", l))   div <- max(0L, div - 1L)

      # ¿encabezado markdown? (1 a 4 almohadillas seguidas de espacio)
      m <- regmatches(l, regexec("^(#{1,4})[ \t]+(.+)$", l))[[1]]
      if (length(m) == 3L && div == 0L) {
        k <- nchar(m[2])
        niveles[k] <- limpia(m[3])
        if (k < 4L) niveles[(k + 1L):4L] <- ""    # al subir, se olvidan los hijos
      }

      # ¿apertura de chunk de R?
      if (grepl("^```\\{r\\}\\s*$", l)) {
        j <- i + 1L
        while (j <= length(txt) && !grepl("^```\\s*$", txt[j])) j <- j + 1L
        cuerpo <- txt[(i + 1L):(j - 1L)]
        lab <- grep("^#\\| label:", cuerpo, value = TRUE)
        lab <- if (length(lab)) trimws(sub("^#\\| label:", "", lab[1])) else "(sin etiqueta)"
        codigo <- recorta(reescribe_rutas(cuerpo[!grepl("^#\\|", cuerpo)], cs))
        out <- c(out,
                 paste0("# ", sep),
                 sprintf("# [%s]", lab),
                 cabecera_ruta(niveles),
                 paste0("# ", sep),
                 codigo, "")
        n <- n + 1L; i <- j
      }
      i <- i + 1L
    }
    list(bloques = out, n = n)
  }

  resumen <- data.frame()

  for (cs in casos) {
    dir_caso <- file.path(raiz, paste0("caso", cs))
    if (!dir.exists(dir_caso)) { warning("No existe ", dir_caso); next }
    dir_scripts <- file.path(dir_caso, "scripts")
    dir.create(dir_scripts, showWarnings = FALSE)

    # ---- 2 · Maestro y preámbulo -------------------------------------------
    qmds    <- Sys.glob(file.path(dir_caso, "*.qmd"))
    maestro <- qmds[!grepl("^_unidad_", basename(qmds))]
    if (length(maestro) != 1L) {
      warning("Caso ", cs, ": esperaba 1 documento maestro y encuentro ",
              length(maestro), ". Lo salto."); next
    }
    txt_m <- readLines(maestro, warn = FALSE)
    fin   <- grep("\\{\\{< include", txt_m)
    fin   <- if (length(fin)) fin[1] - 1L else length(txt_m)
    txt_m <- txt_m[seq_len(fin)]

    pre <- character(0); i <- 1L
    while (i <= length(txt_m)) {
      if (grepl("^```\\{r\\}\\s*$", txt_m[i])) {
        j <- i + 1L
        while (j <= length(txt_m) && !grepl("^```\\s*$", txt_m[j])) j <- j + 1L
        cuerpo <- txt_m[(i + 1L):(j - 1L)]
        lab <- grep("^#\\| label:", cuerpo, value = TRUE)
        lab <- if (length(lab)) trimws(sub("^#\\| label:", "", lab[1])) else ""
        # el preámbulo es SETUP + DATOS: fuera los descriptivos del caso
        if (!grepl("^(fig|tbl)-", lab))
          pre <- c(pre, cuerpo[!grepl("^#\\|", cuerpo)], "")
        i <- j
      }
      i <- i + 1L
    }
    pre <- recorta(reescribe_rutas(pre, cs))

    # ---- 2b · Script del propio documento maestro ---------------------------
    # (presentación del caso: setup, datos, descriptivos y cierre). Recoge TODOS
    # los chunks del maestro: los `{{< include >}}` solo son directivas, así que
    # el fichero no contiene código de las unidades.
    if (maestro_tambien) {
      txt_full <- readLines(maestro, warn = FALSE)
      dest_m <- file.path(dir_scripts, sub("\\.qmd$", ".R", basename(maestro)))
      if (!(basename(dest_m) %in% excluir) &&
          !(solo_faltantes && file.exists(dest_m))) {
        titulo_m <- limpia(sub("^title:\\s*", "",
                               grep("^title:", txt_full, value = TRUE)[1]))
        titulo_m <- gsub('^"|"$', "", titulo_m)
        ex <- extraer_bloques(txt_full, cs)
        out <- c(paste0("# ", strrep("=", 77)),
                 sprintf("# Caso %s · Documento maestro — %s", cs, titulo_m),
                 paste0("# ", sep),
                 sprintf("# Código del propio %s: setup, carga de datos y", basename(maestro)),
                 "# descriptivos de presentación del caso. El código de las unidades está",
                 "# en los scripts unidad_*.R de esta misma carpeta.",
                 "#",
                 "# Cada bloque lleva su LABEL y la ruta de encabezados donde aparece.",
                 "#",
                 "# GENERADO AUTOMÁTICAMENTE por _scripts/generar_scripts_unidades.R:",
                 "# no editar a mano; los cambios se pierden al regenerar. Edita el .qmd.",
                 paste0("# ", strrep("=", 77)), "",
                 ".raiz <- getwd()",
                 'while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)',
                 'if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")',
                 "",
                 ex$bloques)
        accion <- if (file.exists(dest_m)) "sobrescrito" else "creado"
        if (!simular) writeLines(out, dest_m)
        resumen <- rbind(resumen, data.frame(caso = cs, unidad = "maestro", chunks = ex$n,
                                             fichero = basename(dest_m),
                                             accion = if (simular) paste0("[simulado] ", accion) else accion))
      }
    }

    # ---- 3 · Convención de nombres ya usada en este caso -------------------
    previos <- basename(Sys.glob(file.path(dir_scripts, "*unidad_*.R")))
    prefijo <- if (any(startsWith(previos, "_unidad_"))) "_unidad_" else "unidad_"

    # ---- 4 · Un script por partial -----------------------------------------
    for (q in sort(Sys.glob(file.path(dir_caso, sprintf("_unidad_%s_*.qmd", cs))))) {
      u    <- sub(sprintf(".*_unidad_%s_(\\d+)\\.qmd$", cs), "\\1", q)
      dest <- file.path(dir_scripts, sprintf("%s%s_%s.R", prefijo, cs, u))
      if (basename(dest) %in% excluir) next
      if (solo_faltantes && file.exists(dest)) {
        resumen <- rbind(resumen, data.frame(caso = cs, unidad = paste0(cs, ".", u),
                                             chunks = NA_integer_,
                                             fichero = basename(dest), accion = "conservado"))
        next
      }

      txt    <- readLines(q, warn = FALSE)
      titulo <- limpia(sub("^# ", "", grep("^# ", txt, value = TRUE)[1]))
      ex     <- extraer_bloques(txt, cs)

      out <- c(paste0("# ", strrep("=", 77)),
               sprintf("# Caso %s · Unidad %s.%s — %s", cs, cs, u, titulo),
               paste0("# ", sep),
               sprintf("# Todos los chunks de código de la unidad, extraídos de %s.", basename(q)),
               "# Cada bloque va precedido de su LABEL y de la ruta de encabezados",
               "# (sección > subsección > apartado) en la que aparece dentro del documento.",
               "#",
               "# GENERADO AUTOMÁTICAMENTE por _scripts/generar_scripts_unidades.R:",
               "# no editar a mano; los cambios se pierden al regenerar. Edita el .qmd.",
               "#",
               "# EJECUCIÓN: funciona desde CUALQUIER carpeta dentro del proyecto GLM;",
               "# localiza la raíz por _quarto.yml y resuelve solo las rutas de datos.",
               paste0("# ", strrep("=", 77)), "",
               ".raiz <- getwd()",
               'while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)',
               'if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")',
               "",
               "# --- Preámbulo del caso (librerías y datos, como en el documento) ------------",
               pre, "",
               ex$bloques)

      if (ex$n == 0L)
        out <- c(out, "# (Esta unidad no contiene chunks de código: es de encargo y evaluación.)")

      accion <- if (file.exists(dest)) "sobrescrito" else "creado"
      if (!simular) writeLines(out, dest)
      resumen <- rbind(resumen, data.frame(caso = cs, unidad = paste0(cs, ".", u),
                                           chunks = ex$n, fichero = basename(dest),
                                           accion = if (simular) paste0("[simulado] ", accion) else accion))
    }
  }

  cat("\n== Scripts de unidad ==\n"); print(resumen, row.names = FALSE)
  cat("\nChunks volcados:", sum(resumen$chunks, na.rm = TRUE), "\n")
  invisible(resumen)
}
