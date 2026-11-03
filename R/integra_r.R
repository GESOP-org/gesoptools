# =============================================================================
# Herramientas para trabajar en R
#
# Independiente de integra_export.R: llama a sus helpers internos sin
# modificarlos.
#
# Uso minimo (detalles en cada funcion, mas abajo):
#   estudio <- load_integra_r("1877_bici_cat_cawi", db_config)
#   split_multiples(estudio$data, estudio$metadata$codebook)
#   consultar_incidencias_rechazadas("1877_bici_cat_cawi", db_config)
#   calc_loi(estudio$data)
#   crear_libros_codigos("1877_bici_cat_cawi", db_config, "libros")
# =============================================================================

# -----------------------------------------------------------------------------
# Carga
# -----------------------------------------------------------------------------

#' Carga un estudio de Integra para trabajar en R
#'
#' Sigue los pasos de [load_integra_study()] y devuelve la misma estructura.
#' Incluye las variables definidas solo en otro idioma y, con
#' `split_multi = TRUE`, divide las multiples en dicotomicas (ver
#' [split_multiples()]). Los metadatos describen las variables resultantes.
#'
#' @param study_id    Identificador del estudio
#' @param db_config   Lista con host, port, user, password, dbname
#' @param split_multi Si TRUE, divide las multiples en dicotomicas
#' @param sep         Separador de codigos en las multiples
#' @param ...         Argumentos de [load_integra_study()]: `valid_states`,
#'   `idioma`, `clean_html`, `clean_instructions` y `ns_nc_recode`
#'
#' @return Lista con `$data` y `$metadata`, como [load_integra_study()].
#' @export
load_integra_r <- function(study_id, db_config, split_multi = TRUE,
                           sep = ";", ...) {
  estudio <- .cargar_estudio_r(study_id, db_config, ...)

  if (split_multi) {
    cb         <- estudio$metadata$codebook
    variables  <- estudio$metadata$variables
    multi_vars <- .detectar_multiples(estudio$data, cb, sep)

    existentes   <- names(estudio$data)
    estudio$data <- split_multiples(estudio$data, cb, variables,
                                    vars = multi_vars, sep = sep)
    meta <- .split_metadata(variables, cb, multi_vars, existentes)
    estudio$metadata$variables <- meta$variables
    estudio$metadata$codebook  <- meta$codebook
  }

  estudio
}


# Mismos pasos que load_integra_study(), mas las variables definidas solo en
# otro idioma. Replica el flujo para no alterar la salida de
# load_integra_study(), que usan las apps.
.cargar_estudio_r <- function(study_id,
                              db_config,
                              valid_states       = c(1, 5, 6, 9),
                              idioma             = 1L,
                              clean_html         = TRUE,
                              clean_instructions = TRUE,
                              ns_nc_recode       = TRUE) {

  # -- 1. Datos brutos y variables de otros idiomas ---------------------------
  message("[load_integra_r] Conectando a BD para estudio: ", study_id)
  raw <- .integra_fetch(study_id, db_config, valid_states, idioma)
  raw <- .completar_otros_idiomas(raw, study_id, db_config, sin_fechas = TRUE)
  message("[load_integra_r] ", nrow(raw$data), " registros, ",
          nrow(raw$codebook), " entradas en codebook")

  # -- 2. Limpiar etiquetas ---------------------------------------------------
  raw$variables <- .clean_labels(raw$variables, clean_html,
                                 strip_parens       = FALSE,
                                 clean_instructions = clean_instructions)
  raw$codebook  <- .clean_labels(raw$codebook,  clean_html,
                                 strip_parens       = FALSE,
                                 clean_instructions = clean_instructions,
                                 cols               = "label")

  # -- 3. Unificar NS/NC ------------------------------------------------------
  if (ns_nc_recode) raw$codebook <- .recode_ns_nc(raw$codebook)

  # -- 4. Data frame unificado ------------------------------------------------
  df <- .build_unified_df(raw$data, raw$variables, raw$codebook, study_id)

  # -- 5. Codebook con label_short y entradas de MES --------------------------
  month_labels <- c("Gener", "Febrer", "Mar\u00E7", "Abril", "Maig", "Juny",
                    "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")
  codebook <- dplyr::bind_rows(
    dplyr::mutate(raw$codebook, label_short = label),
    tibble::tibble(
      nomvar      = "MES",
      value       = as.character(1:12),
      label       = month_labels,
      label_short = substr(month_labels, 1, 3)
    )
  )

  list(data = df, metadata = list(variables = raw$variables, codebook = codebook))
}


# -----------------------------------------------------------------------------
# Respuestas multiples
# -----------------------------------------------------------------------------

#' Divide las respuestas multiples en variables dicotomicas
#'
#' Convierte las variables con valores tipo `"1;3;95"` en una variable
#' `NOMVAR_codigo` por cada codigo del codebook: 1 elegida, 0 no elegida, NA
#' pregunta no realizada. Las etiquetas de valor son "No" y la etiqueta de la
#' opcion. Las nuevas columnas ocupan el lugar de la original, que se elimina.
#'
#' @param data      Data frame con las multiples en una sola columna de texto
#' @param codebook  Tibble con `nomvar`, `value`, `label`
#'   (p.ej. `estudio$metadata$codebook`)
#' @param variables Tibble con `nomvar`, `label` (p.ej.
#'   `estudio$metadata$variables`); opcional, etiqueta las nuevas variables
#' @param vars      Variables a dividir; si es NULL se detectan las que
#'   contienen el separador
#' @param sep       Separador de codigos
#'
#' @return Data frame con las multiples sustituidas por sus dicotomicas.
#' @export
split_multiples <- function(data, codebook, variables = NULL, vars = NULL,
                            sep = ";") {
  if (is.null(vars)) vars <- .detectar_multiples(data, codebook, sep)
  if (length(vars) == 0) return(data)

  dicotomicas <- function(v) {
    cb <- codebook[codebook$nomvar == v, ]
    cb <- cb[order(suppressWarnings(as.numeric(cb$value))), ]
    txt <- as.character(data[[v]])
    elegidos <- strsplit(txt, sep, fixed = TRUE)
    lab_var <- if (!is.null(variables)) variables$label[variables$nomvar == v][1] else NA_character_

    out <- lapply(seq_len(nrow(cb)), function(i) {
      x <- ifelse(is.na(txt), NA_integer_,
                  vapply(elegidos, function(e) as.integer(cb$value[i] %in% trimws(e)),
                         integer(1)))
      lbl <- c(0L, 1L)
      names(lbl) <- c("No", cb$label[i])
      haven::labelled(
        x, labels = lbl,
        label = if (is.na(lab_var)) cb$label[i] else paste0(lab_var, " : ", cb$label[i])
      )
    })
    names(out) <- .nombres_dicotomicas(v, cb$value, existentes)
    tibble::as_tibble(out)
  }

  # Los nombres nuevos se comparan con todos los originales, multiples incluidas
  existentes <- names(data)
  nuevas <- stats::setNames(lapply(vars, dicotomicas), vars)
  orden  <- unlist(lapply(names(data), function(n) {
    if (n %in% vars) names(nuevas[[n]]) else n
  }))

  if (anyDuplicated(orden) > 0)
    stop("[split_multiples] Nombres duplicados tras dividir: ",
         paste(unique(orden[duplicated(orden)]), collapse = ", "))

  # Las originales salen antes de unir para evitar nombres repetidos
  data[setdiff(names(data), vars)] |>
    dplyr::bind_cols(dplyr::bind_cols(unname(nuevas))) |>
    dplyr::select(dplyr::all_of(orden))
}


# Detecta las variables multiples: contienen el separador y tienen codebook
.detectar_multiples <- function(data, codebook, sep = ";") {
  tiene_sep <- vapply(data, function(x) {
    any(grepl(sep, as.character(x), fixed = TRUE), na.rm = TRUE)
  }, logical(1))
  intersect(names(data)[tiene_sep], unique(codebook$nomvar))
}


# Nombra las dicotomicas de una multiple como NOMVAR_codigo. Si alguno de esos
# nombres ya existe, usa NOMVAR_c<codigo> para toda la multiple.
.nombres_dicotomicas <- function(v, valores, existentes) {
  nombres <- paste0(v, "_", valores)
  if (any(nombres %in% existentes)) {
    nombres <- paste0(v, "_c", valores)
    message("[split_multiples] ", v, ": ya existen variables ", v,
            "_<codigo>; las dicotomicas se llaman ", v, "_c<codigo>")
  }
  nombres
}


# Sustituye cada multiple por sus dicotomicas en variables y codebook. Refleja
# lo que hace split_multiples(); `existentes` son las columnas antes de dividir.
.split_metadata <- function(variables, codebook, vars, existentes) {
  if (length(vars) == 0) return(list(variables = variables, codebook = codebook))

  nuevas_vars <- list()
  nuevo_cb    <- list()

  for (v in vars) {
    cb <- codebook[codebook$nomvar == v, ]
    cb <- cb[order(suppressWarnings(as.numeric(cb$value))), ]
    lab_var <- variables$label[variables$nomvar == v][1]
    nombres <- .nombres_dicotomicas(v, cb$value, existentes)

    nuevas_vars[[v]] <- tibble::tibble(
      nomvar = nombres,
      label  = if (is.na(lab_var)) cb$label else paste0(lab_var, " : ", cb$label)
    )
    nuevo_cb[[v]] <- tibble::tibble(
      nomvar = rep(nombres, each = 2),
      value  = rep(c("0", "1"), times = nrow(cb)),
      label  = as.vector(rbind("No", cb$label))
    )
    # El codebook de load_integra_study incluye label_short (= label)
    if ("label_short" %in% names(codebook))
      nuevo_cb[[v]]$label_short <- nuevo_cb[[v]]$label
  }

  # Cada multiple ocupa la posicion de la original
  variables_out <- dplyr::bind_rows(lapply(seq_len(nrow(variables)), function(i) {
    nv <- variables$nomvar[i]
    if (nv %in% vars) nuevas_vars[[nv]] else variables[i, ]
  }))

  codebook_out <- dplyr::bind_rows(
    codebook[!codebook$nomvar %in% vars, ],
    dplyr::bind_rows(nuevo_cb)
  )

  list(variables = variables_out, codebook = codebook_out)
}


# -----------------------------------------------------------------------------
# Incidencias
# -----------------------------------------------------------------------------

# Pega a `data` el codigo y la etiqueta de la incidencia de cada registro.
# Etiquetas: tipos estandar (tipos_incidencias) e incidencias particulares del
# estudio (inciestudio). Un registro sin incidencia queda en NA.
.add_incidencia <- function(data, study_id, db_config,
                            cual         = c("ultima", "primera"),
                            excluir      = NULL,
                            idioma       = 1L,
                            registro_var = "REGISTRO") {
  cual <- match.arg(cual)

  if (!registro_var %in% names(data))
    stop("[incidencias] No existe la columna de registro: ",
         registro_var)
  ya <- intersect(c("tipo_incidencia", "incidencia"), names(data))
  if (length(ya) > 0)
    stop("[incidencias] data ya tiene las columnas: ",
         paste(ya, collapse = ", "))

  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    dbname   = db_config$dbname,
    host     = db_config$host,
    port     = db_config$port,
    user     = db_config$user,
    password = db_config$password
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tabla <- paste0("incidencias_", study_id)
  if (!DBI::dbExistsTable(con, tabla))
    stop("[incidencias] Tabla no encontrada: ", tabla)

  incidencias <- tibble::as_tibble(DBI::dbReadTable(con, tabla))
  tipos       <- tibble::as_tibble(DBI::dbReadTable(con, "tipos_incidencias"))
  inciestudio <- tibble::as_tibble(DBI::dbReadTable(con, "inciestudio"))
  estudios    <- tibble::as_tibble(DBI::dbReadTable(con, "estudios"))

  # cod_estudio va en mayusculas en la tabla estudios
  id_est <- estudios$id_estudio[toupper(estudios$cod_estudio) == toupper(study_id)]
  if (length(id_est) != 1)
    stop("[incidencias] No se encontro un unico estudio '",
         study_id, "' en la tabla estudios")

  ultima <- .elegir_incidencia(incidencias, tipos, inciestudio, id_est,
                               idioma, cual, excluir)

  clave <- as.integer(data[[registro_var]])
  idx   <- match(clave, ultima$registro)

  data$tipo_incidencia <- ultima$tipo_incidencia[idx]
  data$incidencia      <- ultima$incidencia[idx]

  sin_etiqueta <- !is.na(data$tipo_incidencia) & is.na(data$incidencia)
  if (any(sin_etiqueta))
    warning("[incidencias] Codigos sin etiqueta: ",
            paste(sort(unique(data$tipo_incidencia[sin_etiqueta])), collapse = ", "))

  data
}


#' Descarga las entrevistas rechazadas de un estudio con su incidencia
#'
#' Carga los registros no finalizados (por defecto los rechazos de Integra) y
#' pega el codigo y la etiqueta de su incidencia. Para los finalizados usa
#' [load_integra_r()].
#'
#' Estados de entrevista (`ESTADO`) segun el manual de Integra: completas 1, 5,
#' 6 y 9; parciales 0 y 2; rechazos 3, 4, 7, 8 y 10.
#'
#' @param study_id  Identificador del estudio
#' @param db_config Lista con host, port, user, password, dbname
#' @param estados   Valores de `ESTADO` que se descargan. Por defecto los
#'   rechazos, `c(3, 4, 7, 8, 10)`. Para incluir los parciales:
#'   `c(0, 2, 3, 4, 7, 8, 10)`
#' @param cual      `"ultima"` o `"primera"` incidencia de cada registro
#' @param excluir   Codigos de incidencia que se ignoran al elegir
#' @param idioma    Codigo de idioma de etiquetas e incidencias particulares
#' @param ...       Otros argumentos de [load_integra_study()]
#'
#' @return Tibble con los registros y las columnas `tipo_incidencia` e
#'   `incidencia`.
#' @export
consultar_incidencias_rechazadas <- function(study_id, db_config,
                                             estados = c(3, 4, 7, 8, 10),
                                             cual    = c("ultima", "primera"),
                                             excluir = NULL,
                                             idioma  = 1L,
                                             ...) {
  cual <- match.arg(cual)

  estudio <- .cargar_estudio_r(study_id, db_config,
                               valid_states = estados, idioma = idioma, ...)

  if (nrow(estudio$data) == 0) {
    message("[consultar_incidencias_rechazadas] No hay registros con ESTADO en: ",
            paste(estados, collapse = ", "))
    return(estudio$data)
  }

  .add_incidencia(estudio$data, study_id, db_config,
                  cual = cual, excluir = excluir, idioma = idioma)
}


# Elige una incidencia por registro y la etiqueta. Devuelve registro,
# tipo_incidencia e incidencia.
.elegir_incidencia <- function(incidencias, tipos, inciestudio, id_est,
                               idioma, cual, excluir) {
  particulares <- inciestudio |>
    dplyr::filter(id_estudio == id_est, id_idioma == idioma) |>
    dplyr::transmute(tipo_incidencia = id_incidencia, incidencia = descripcion)

  catalogo <- dplyr::bind_rows(
    tipos |> dplyr::transmute(tipo_incidencia = id_incidencia,
                              incidencia = descripcion),
    particulares
  ) |>
    dplyr::distinct(tipo_incidencia, .keep_all = TRUE)

  # registro 0: eventos sin entrevista asociada
  inc <- incidencias |>
    dplyr::filter(registro > 0, !tipo_incidencia %in% excluir) |>
    dplyr::arrange(registro, fecha, id_incidencia)

  inc <- if (cual == "ultima") {
    dplyr::slice_tail(dplyr::group_by(inc, registro), n = 1)
  } else {
    dplyr::slice_head(dplyr::group_by(inc, registro), n = 1)
  }

  inc |>
    dplyr::ungroup() |>
    dplyr::left_join(catalogo, by = "tipo_incidencia") |>
    dplyr::select(registro, tipo_incidencia, incidencia)
}


# -----------------------------------------------------------------------------
# Metricas de campo
# -----------------------------------------------------------------------------

#' Calcula el LOI (duracion) de las entrevistas finalizadas
#'
#' Devuelve la media y la mediana de la duracion, en segundos y en minutos,
#' para cada variable de duracion indicada, con la opcion de agrupar.
#'
#' Las duraciones deben estar en segundos. Los NA no cuentan: `n` es el numero
#' de entrevistas con duracion valida.
#'
#' @param data       Data frame con las entrevistas (p.ej. `estudio$data`)
#' @param vars       Variables de duracion, en segundos. Por defecto las dos de
#'   duracion final de Integra
#' @param by         Variables de agrupacion (nombres de columna), o NULL para
#'   el total
#' @param estados    Valores de `ESTADO` que cuentan como finalizada. NULL no
#'   filtra
#' @param estado_var Nombre de la columna de estado
#'
#' @return Tibble con una fila por grupo y variable: `n`, `media_seg`,
#'   `mediana_seg`, `media_min`, `mediana_min`.
#' @export
calc_loi <- function(data,
                     vars       = c("DURADA_FINAL", "CONTROL_DURADA"),
                     by         = NULL,
                     estados    = NULL,
                     estado_var = "ESTADO") {

  faltan <- setdiff(c(vars, by), names(data))
  if (length(faltan) > 0)
    stop("[calc_loi] Columnas no encontradas en data: ",
         paste(faltan, collapse = ", "))

  if (!is.null(estados)) {
    if (!estado_var %in% names(data))
      stop("[calc_loi] No existe la columna de estado: ", estado_var)
    # En los datos de load_integra_r() el estado es un factor con etiquetas:
    # se compara con el texto
    antes <- data
    data <- data[as.character(data[[estado_var]]) %in% as.character(estados), ]
    if (nrow(data) == 0)
      stop("[calc_loi] Ningun registro con ", estado_var, " en: ",
           paste(estados, collapse = ", "),
           "\nValores presentes: ",
           paste(unique(as.character(antes[[estado_var]])), collapse = ", "))
  }

  resumen <- lapply(vars, function(v) {
    d <- data
    d$.seg <- suppressWarnings(as.numeric(as.character(d[[v]])))
    if (any(is.na(d$.seg) & !is.na(d[[v]])))
      warning("[calc_loi] ", v, " tiene valores no numericos; se tratan como NA")

    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::summarise(
        n           = sum(!is.na(.seg)),
        media_seg   = mean(.seg, na.rm = TRUE),
        mediana_seg = stats::median(.seg, na.rm = TRUE),
        .groups     = "drop"
      ) |>
      dplyr::mutate(
        variable    = v,
        media_min   = media_seg / 60,
        mediana_min = mediana_seg / 60
      )
  })

  dplyr::bind_rows(resumen) |>
    dplyr::select(dplyr::all_of(c("variable", by, "n", "media_seg",
                                  "mediana_seg", "media_min", "mediana_min")))
}


# -----------------------------------------------------------------------------
# Codificacion de semiabiertas
# -----------------------------------------------------------------------------

#' Crea los libros de codigos de las preguntas semiabiertas
#'
#' Una pregunta semiabierta tiene la opcion 95 ("Altres") en el codebook y una
#' variable asociada con sufijo `_O` con el texto de esa opcion. Para cada una
#' crea un Excel `codif_auto_<VARIABLE>.xlsx` (con el nombre de la variable tal
#' cual) en `dir` con dos pestanas:
#' `EXEMPLES` (solo cabeceras) y `LLIBRE DE CODIS`, que se rellena con los
#' codigos y etiquetas de la variable, la variable de literales (`<VAR>_O`), el
#' codigo a sustituir (95) y el tipo (`simple` o `multiple`). `LLINDAR` y
#' `CONTEXT` quedan vacios. Si ya existe un archivo con ese nombre no se
#' sobrescribe: se crea con el sufijo `_1`, `_2`...
#'
#' Las multiples se detectan en los datos sin dividir (valores con `sep`), por
#' lo que una multiple en la que nadie marco mas de una opcion se trata como
#' simple.
#'
#' @param study_id  Identificador del estudio
#' @param db_config Lista con host, port, user, password, dbname. Por defecto
#'   se lee de las variables `INTEGRA_*` con [db_config_from_env()]
#' @param dir       Carpeta de salida (se crea si no existe)
#' @param sep       Separador de codigos en las multiples
#' @param ...       Argumentos de [load_integra_study()]: `valid_states`,
#'   `idioma`, `clean_html`, `clean_instructions` y `ns_nc_recode`
#'
#' @return Invisible: vector con las rutas de los archivos creados.
#' @export
crear_libros_codigos <- function(study_id, db_config = db_config_from_env(),
                                 dir, sep = ";", ...) {
  estudio  <- .cargar_estudio_r(study_id, db_config, ...)
  semi     <- .detectar_semiabiertas(estudio$data, estudio$metadata$codebook)
  multiples <- .detectar_multiples(estudio$data, estudio$metadata$codebook, sep)

  if (length(semi) == 0) {
    message("[crear_libros_codigos] No hay preguntas semiabiertas en ", study_id)
    return(invisible(character(0)))
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  rutas <- vapply(semi, function(v) {
    libro <- .libro_codigos(v, estudio$metadata$codebook,
                            tipo = if (v %in% multiples) "multiple" else "simple")
    ruta <- .ruta_libro_libre(dir, v)
    .escribir_libro_codigos(libro, ruta)
    ruta
  }, character(1))

  message("[crear_libros_codigos] ", length(rutas), " archivos en: ", dir)
  invisible(unname(rutas))
}


# Variables con la opcion 95 en el codebook y la columna <VAR>_O en los datos
.detectar_semiabiertas <- function(data, codebook) {
  con_95 <- unique(codebook$nomvar[as.character(codebook$value) == "95"])
  con_95[paste0(con_95, "_O") %in% names(data)]
}


# Contenido de la pestana LLIBRE DE CODIS de una variable
.libro_codigos <- function(v, codebook, tipo) {
  cb <- codebook[codebook$nomvar == v, ]
  cb <- cb[order(suppressWarnings(as.numeric(cb$value))), ]
  n  <- nrow(cb)
  # Las columnas de variable solo llevan valor en la primera fila
  primera <- function(x) c(x, rep(NA, n - 1))

  data.frame(
    ETIQUETA                = as.character(cb$label),
    CODI                    = suppressWarnings(as.numeric(cb$value)),
    VARIABLES_PER_CODIFICAR = primera(v),
    VARIABLES_LITERALS      = primera(paste0(v, "_O")),
    SUBSTITUIR_CODI         = primera(95),
    TIPUS                   = primera(tipo),
    LLINDAR                 = NA,
    CONTEXT                 = NA,
    stringsAsFactors = FALSE
  )
}


.escribir_libro_codigos <- function(libro, ruta) {
  wb <- openxlsx::createWorkbook()
  estilo <- openxlsx::createStyle(textDecoration = "bold")

  openxlsx::addWorksheet(wb, "EXEMPLES")
  openxlsx::writeData(wb, "EXEMPLES",
                      data.frame(REGISTRO = character(), VARIABLE = character(),
                                 LITERAL = character(),
                                 `Predicted Category` = character(),
                                 CODI = character(), check.names = FALSE),
                      headerStyle = estilo)

  openxlsx::addWorksheet(wb, "LLIBRE DE CODIS")
  openxlsx::writeData(wb, "LLIBRE DE CODIS", libro, headerStyle = estilo)

  openxlsx::saveWorkbook(wb, ruta, overwrite = FALSE)
}


# Ruta codif_auto_<VAR>.xlsx; si ya existe, anade _1, _2... para no sobrescribir
.ruta_libro_libre <- function(dir, v) {
  base <- file.path(dir, paste0("codif_auto_", v))
  ruta <- paste0(base, ".xlsx")
  i <- 0L
  while (file.exists(ruta)) {
    i <- i + 1L
    ruta <- paste0(base, "_", i, ".xlsx")
  }
  if (i > 0L)
    message("[crear_libros_codigos] ", v, ": ya existia el archivo; se crea ",
            basename(ruta))
  ruta
}
