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
#      traduciendo el acceso local a datos (`source("R/x.R")`, `cargar_*()`) a
#      descargas del repositorio, para que el script funcione esté donde esté;
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
                            raiz = NULL,
                            GLM_REPO = "https://raw.githubusercontent.com/asunmayoral/GLM",
                            GLM_REF  = "master") {

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

  # ---- Cabecera de acceso al repositorio (idéntica en todos los scripts) -----
  # Los scripts .R no dependen de dónde estén guardados ni de que el proyecto
  # esté abierto: descargan datos y DGP del repositorio. Antes localizaban la
  # raíz con getwd(), y al abrir el fichero fuera del proyecto el bucle subía
  # hasta "/" y las rutas quedaban como "//caso1/R/...".
  cabecera_github <- c(
    "# --- Datos y funciones del curso, servidos desde GitHub ----------------------",
    "# El script es autónomo: no hace falta clonar el repositorio ni abrir GLM.Rproj,",
    "# y no depende de en qué carpeta del ordenador esté guardado. Solo necesita",
    "# conexión a internet. GLM_REF es la rama o etiqueta del repositorio que se lee.",
    sprintf('GLM_REPO <- "%s"', GLM_REPO),
    sprintf('GLM_REF  <- "%s"', GLM_REF),
    "",
    "#' URL de un fichero del repositorio, a partir de su ruta dentro del proyecto.",
    'url_glm <- function(ruta) paste(GLM_REPO, GLM_REF, ruta, sep = "/")',
    "",
    "#' Lee un .rds del repositorio. gzcon() descomprime al vuelo lo que saveRDS()",
    "#' comprimió: sin él, readRDS() no reconoce el flujo que llega por http.",
    "leer_datos_glm <- function(ruta) {",
    '  con <- gzcon(url(url_glm(ruta), open = "rb"))',
    "  on.exit(close(con))",
    "  readRDS(con)",
    "}")

  # ---- Reproducibilidad (index.qmd §10) -------------------------------------
  # Entorno con el que se preparó el material. Se rellena a mano ejecutando
  # R.version.string y packageVersion() sobre la máquina de referencia: son
  # cifras medidas, no supuestas. Al actualizar paquetes, actualizar aquí.
  PROBADO_R <- "R 4.6.0 (2026-04-24) · x86_64-apple-darwin20 · medido el 2026-09-07"
  VERSIONES <- c(
    AER = "1.2.17", aplore3 = "0.9", arm = "1.15.3", broom = "1.0.13",
    broom.mixed = "0.2.9.7", car = "3.1.5", DHARMa = "0.5.0", dplyr = "1.2.1",
    emmeans = "2.0.3", flexsurv = "2.3.2", forcats = "1.0.1", GGally = "2.4.0",
    ggeffects = "2.3.2", ggplot2 = "4.0.3", glmmTMB = "1.1.14", glmnet = "5.0",
    lme4 = "2.0.1", logistf = "1.26.1", marginaleffects = "0.32.0",
    MASS = "7.3.65", MuMIn = "1.48.19", nnet = "7.3.20", patchwork = "1.3.2",
    performance = "0.17.0", pROC = "1.19.0.1", pscl = "1.5.9", purrr = "1.2.2",
    readr = "2.2.0", rsample = "1.3.2", scales = "1.4.0", see = "0.14.0",
    sessioninfo = "1.2.4",
    survival = "3.8.6", survminer = "0.5.2", tibble = "3.3.1", tidyr = "1.3.2",
    tidyverse = "2.0.0", vcd = "1.4.13", vcdExtra = "0.9.6", yardstick = "1.4.0")

  # Quita los comentarios respetando las comillas, para no confundir un paquete
  # citado en un comentario (`# o brglm2::brglm_fit`) con una dependencia real.
  sin_comentarios <- function(lineas) {
    vapply(lineas, function(l) {
      ch <- strsplit(l, "", fixed = TRUE)[[1]]
      q <- ""; fin <- length(ch)
      for (k in seq_along(ch)) {
        if (nzchar(q)) { if (ch[k] == q) q <- "" }
        else if (ch[k] %in% c('"', "'")) q <- ch[k]
        else if (ch[k] == "#") { fin <- k - 1L; break }
      }
      if (fin < 1L) "" else paste(ch[seq_len(fin)], collapse = "")
    }, character(1), USE.NAMES = FALSE)
  }

  paquetes_de <- function(lineas) {
    txt <- sin_comentarios(lineas)
    m1 <- unlist(regmatches(txt, gregexpr(
      '(?:library|require|requireNamespace)\\(\\s*"?[A-Za-z][A-Za-z0-9._]*', txt, perl = TRUE)))
    m1 <- sub('^[A-Za-z]+\\(\\s*"?', "", m1)
    m2 <- unlist(regmatches(txt, gregexpr(
      '[A-Za-z][A-Za-z0-9._]*(?=::)', txt, perl = TRUE)))
    base_r <- c("stats", "utils", "graphics", "grDevices", "methods", "datasets",
                "base", "tools", "parallel", "compiler", "splines", "grid")
    # radix: orden independiente de la configuración regional, para que el
    # fichero generado no cambie según la locale de quien ejecuta el generador.
    sort(setdiff(unique(c(m1, m2)), base_r), method = "radix")
  }

  # Empaqueta los pares `nombre = "version"` en líneas de ancho legible.
  envuelve <- function(pares, sangria) {
    out <- character(0); linea <- ""
    for (p in pares) {
      cand <- if (nzchar(linea)) paste0(linea, " ", p) else paste0(sangria, p)
      if (nchar(cand) > 78 && nzchar(linea)) { out <- c(out, linea); linea <- paste0(sangria, p) }
      else linea <- cand
    }
    c(out, linea)
  }

  bloque_reproducibilidad <- function(codigo, unidad) {
    # sessioninfo no aparece en el código de la unidad: lo usa el cierre del
    # script, así que entra en la lista para que la comprobación lo cubra.
    paq <- sort(unique(c(paquetes_de(codigo), "sessioninfo")), method = "radix")
    fuera <- setdiff(paq, names(VERSIONES))
    if (length(fuera))
      warning(unidad, ": paquetes sin versión registrada en VERSIONES -> ",
              paste(fuera, collapse = ", "), ". Añádelos y vuelve a generar.")
    paq <- intersect(paq, names(VERSIONES))
    pares <- sprintf('%s = "%s",', paq, VERSIONES[paq])
    pares[length(pares)] <- sub(",$", ")", pares[length(pares)])
    c("# --- Reproducibilidad (ver index.qmd §10) -----------------------------------",
      "# Paquetes que usa esta unidad, con la versión con la que se preparó el",
      "# material. Si te falta alguno el script lo dice ahora, en vez de fallar a",
      "# mitad de un ajuste; si tu versión difiere, avisa y sigue.",
      sprintf('PROBADO_R <- "%s"', PROBADO_R),
      "PAQUETES  <- c(",
      envuelve(pares, "  "),
      'message("Material preparado con ", PROBADO_R)',
      "",
      '.falta <- names(PAQUETES)[!vapply(names(PAQUETES), requireNamespace, logical(1), quietly = TRUE)]',
      "if (length(.falta))",
      '  stop("Faltan paquetes: ", paste(.falta, collapse = ", "),',
      '       ". Instálalos con install.packages() y vuelve a ejecutar.")',
      "",
      '.instalada <- vapply(names(PAQUETES), function(p) as.character(packageVersion(p)), character(1))',
      ".otra <- names(PAQUETES)[.instalada != PAQUETES]",
      "if (length(.otra))",
      '  message("Versiones distintas a las probadas:\\n  ",',
      '          paste(sprintf("%s: tienes %s, probado %s", .otra, .instalada[.otra], PAQUETES[.otra]),',
      '                collapse = "\\n  "))',
      "rm(.falta, .instalada, .otra)")
  }

  bloque_entorno <- c(
    "# --- Entorno de ejecución (index.qmd §10.3) ---------------------------------",
    "# Todo trabajo del curso cierra dejando constancia de con qué se ejecutó.",
    "# session_info() añade a sessionInfo() la fecha y la procedencia de cada",
    "# paquete, que es lo que hace falta para reinstalar exactamente estas versiones.",
    "sessioninfo::session_info()")

  # ---- Utilidades compartidas ----------------------------------------------
  # De rutas locales a URLs del repositorio. Los .qmd leen de la carpeta local
  # (render rápido y sin red); los .R son autónomos y descargan. `ruta` debe
  # coincidir con el .rds versionado en caso*/datos/.
  datos_caso <- list(
    `1` = c(patron = "cargar_cohorte\\(\\)",
            ruta   = "caso1/datos/cohorte_20252026.rds",
            nota   = "cohorte simulada del curso"),
    `2` = c(patron = "cargar_cartera\\(\"auto\"\\)",
            ruta   = "caso2/datos/cartera_auto_20252026.rds",
            nota   = "cartera de auto del curso"),
    `3` = c(patron = "cargar_averias\\(\\)",
            ruta   = "caso3/datos/banco_averias_20252026.rds",
            nota   = "banco de averías del curso"))

  reescribe_rutas <- function(lineas, cs) {
    lineas <- sub('source\\("R/([^"]+)"\\)\\s*(#.*)?$',
                  sprintf('source(url_glm("caso%s/R/\\1"))   # funciones del proceso generador', cs),
                  lineas)
    d <- datos_caso[[as.character(cs)]]
    if (!is.null(d))
      lineas <- sub(paste0(d[["patron"]], "\\s*(#.*)?$"),
                    sprintf('leer_datos_glm("%s")   # %s', d[["ruta"]], d[["nota"]]),
                    lineas)
    lineas
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
                 cabecera_github,
                 "",
                 bloque_reproducibilidad(ex$bloques, sprintf("caso%s maestro", cs)),
                 "",
                 ex$bloques,
                 "",
                 bloque_entorno)
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
               "# EJECUCIÓN: autónomo. Guárdalo donde quieras y ejecútalo; los datos y los",
               "# ficheros del proceso generador se descargan del repositorio del curso.",
               paste0("# ", strrep("=", 77)), "",
               cabecera_github,
               "",
               bloque_reproducibilidad(c(pre, ex$bloques), sprintf("unidad %s.%s", cs, u)),
               "",
               "# --- Preámbulo del caso (librerías y datos, como en el documento) ------------",
               pre, "",
               ex$bloques)

      if (ex$n == 0L)
        out <- c(out, "# (Esta unidad no contiene chunks de código: es de encargo y evaluación.)")

      out <- c(out, "", bloque_entorno)

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
