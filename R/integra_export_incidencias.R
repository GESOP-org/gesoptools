# =============================================================================
# integra_export_incidencias.R
# Exportacion a SPSS o Excel de las entrevistas rechazadas con su incidencia
#
# Funcion principal:
#   export_incidencias_rechazadas(study_id, db_config, format, ...)
#
# Sigue el flujo de export_integra() y llama a sus helpers internos sin
# modificarlos. Los ficheros llevan el sufijo "_rechazadas" para no pisar los
# de export_integra():
#   <study_id>_rechazadas.sav          (format = "spss")
#   <study_id>_rechazadas_datos.xlsx   (format = "excel")
#   variables_<study_id>_rechazadas.xlsx
#
# Uso minimo:
#   export_incidencias_rechazadas("1877_bici_cat_cawi", db_config, format = "spss")
# =============================================================================


#' Exporta a SPSS o Excel las entrevistas rechazadas con su incidencia
#'
#' Carga los registros no finalizados (por defecto los rechazos de Integra),
#' les pega la incidencia y escribe el fichero. La incidencia entra como la
#' variable numerica `INCIDENCIA`: el codigo de Integra, con su etiqueta como
#' etiqueta de valor.
#'
#' Estados de entrevista (`ESTADO`) segun el manual de Integra: completas 1, 5,
#' 6 y 9; parciales 0 y 2; rechazos 3, 4, 7, 8 y 10. Para los finalizados usa
#' [export_integra()].
#'
#' @param study_id             Identificador del estudio
#' @param db_config            Lista con host, port, user, password, dbname
#' @param format               "spss" o "excel". Con NULL devuelve los datos sin
#'                             escribir ficheros.
#' @param output_dir           Directorio de salida (por defecto: directorio de trabajo)
#' @param estados              Valores de `ESTADO` que se descargan. Por defecto
#'                             los rechazos, `c(3, 4, 7, 8, 10)`.
#' @param cual                 "ultima" o "primera" incidencia de cada registro
#' @param excluir              Codigos de incidencia que se ignoran al elegir
#' @param clean_html           Si TRUE, elimina etiquetas HTML y entidades de las etiquetas
#' @param strip_parens         Si TRUE, elimina texto entre parentesis de las etiquetas
#' @param clean_instructions   Si TRUE, elimina textos de instruccion de entrevistador
#' @param clean_specific_texts Vector de textos adicionales a eliminar
#' @param include_admin        Si TRUE, incluye variables administrativas (REGISTRO, ESTADO, etc.)
#' @param idioma               Codigo de idioma en variablescodigos (1 = principal)
#' @param overwrite            Si FALSE, se detiene cuando el fichero ya existe
#'
#' @return Lista invisible con las rutas de los ficheros escritos y
#'   `$data`, `$variables` y `$niveles`, como en [export_integra()].
#' @export
export_incidencias_rechazadas <- function(study_id,
                                          db_config,
                                          format               = NULL,
                                          output_dir           = getwd(),
                                          estados              = c(3, 4, 7, 8, 10),
                                          cual                 = c("ultima", "primera"),
                                          excluir              = NULL,
                                          clean_html           = TRUE,
                                          strip_parens         = FALSE,
                                          clean_instructions   = TRUE,
                                          clean_specific_texts = NULL,
                                          include_admin        = TRUE,
                                          idioma               = 1L,
                                          overwrite            = TRUE) {

  if (!is.null(format)) format <- match.arg(format, c("spss", "excel"))
  cual <- match.arg(cual)
  if (!is.null(format)) dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # -- 1. Conexion y carga bruta, con variables de otros idiomas --------------
  message("[export_incidencias] Conectando a BD para estudio: ", study_id)
  raw <- .integra_fetch(study_id, db_config, estados, idioma)
  message("[export_incidencias] ", nrow(raw$data), " registros con ESTADO en: ",
          paste(estados, collapse = ", "))

  if (nrow(raw$data) == 0) {
    message("[export_incidencias] No hay registros que exportar")
    return(invisible(list()))
  }

  raw <- .completar_otros_idiomas(raw, study_id, db_config)

  # -- 2. Limpieza de etiquetas ----------------------------------------------
  raw$variables <- .clean_labels(raw$variables, clean_html, strip_parens,
                                 clean_instructions, clean_specific_texts)
  raw$codebook  <- .clean_labels(raw$codebook,  clean_html, strip_parens,
                                 clean_instructions, clean_specific_texts,
                                 cols = "label")

  # -- 3. Data frame codificado, con las multiples como texto -----------------
  study_coded <- setdiff(raw$variables$nomvar,
                         raw$variables$nomvar[stringr::str_ends(raw$variables$nomvar, "_O")])
  multi_vars <- character(0)
  for (.v in intersect(study_coded, names(raw$data))) {
    vals <- as.character(raw$data[[.v]])
    if (any(grepl(";", vals, fixed = TRUE), na.rm = TRUE))
      multi_vars <- c(multi_vars, .v)
  }

  df_codes <- .build_coded_df(raw, include_admin, multi_vars = multi_vars)
  multi_en_df <- intersect(multi_vars, names(df_codes))
  if (length(multi_en_df) > 0)
    df_codes <- .encode_multi_as_text(df_codes, multi_en_df,
                                      raw$variables, raw$codebook)

  # -- 4. Incidencia: variable INCIDENCIA con codigo y etiqueta de valor -------
  # La union necesita REGISTRO aunque include_admin sea FALSE
  if (!"REGISTRO" %in% names(df_codes))
    df_codes <- dplyr::bind_cols(REGISTRO = raw$data$REGISTRO, df_codes)

  con_inc <- .add_incidencia(df_codes, study_id, db_config,
                             cual = cual, excluir = excluir, idioma = idioma)

  niveles_inc <- con_inc |>
    dplyr::distinct(tipo_incidencia, incidencia) |>
    dplyr::filter(!is.na(tipo_incidencia)) |>
    dplyr::arrange(tipo_incidencia)

  df_codes <- con_inc |>
    dplyr::mutate(INCIDENCIA = as.numeric(tipo_incidencia)) |>
    dplyr::select(-tipo_incidencia, -incidencia)

  # Orden de la tabla de datos; INCIDENCIA va detras de ESTADO
  orden <- c(intersect(names(raw$data), names(df_codes)),
             setdiff(names(df_codes), names(raw$data)))
  df_codes <- df_codes[orden]
  ancla <- if ("ESTADO" %in% names(df_codes)) "ESTADO" else "REGISTRO"
  df_codes <- dplyr::relocate(df_codes, INCIDENCIA, .after = dplyr::all_of(ancla))

  raw$variables <- dplyr::bind_rows(
    raw$variables,
    tibble::tibble(
      nomvar = "INCIDENCIA",
      label  = if (cual == "ultima") "Ultima incidencia del registro"
               else "Primera incidencia del registro"
    )
  )
  raw$codebook <- dplyr::bind_rows(
    raw$codebook,
    tibble::tibble(
      nomvar = "INCIDENCIA",
      value  = as.character(niveles_inc$tipo_incidencia),
      label  = niveles_inc$incidencia
    )
  )

  df_labels <- .build_labelled_df(raw, df_codes)

  # -- 5. Exportar datos ------------------------------------------------------
  out_files <- list()

  if (!is.null(format)) {
    sufijo <- paste0(study_id, "_rechazadas")

    if (format == "spss") {
      path_spss <- file.path(output_dir, paste0(sufijo, ".sav"))
      .comprobar_sobrescribir(path_spss, overwrite)
      .export_spss(df_codes, raw$variables, raw$codebook, path_spss)
      out_files$spss <- path_spss
      message("[export_incidencias] SPSS exportado: ", path_spss)

    } else {
      path_xlsx <- file.path(output_dir, paste0(sufijo, "_datos.xlsx"))
      .comprobar_sobrescribir(path_xlsx, overwrite)
      .export_excel_data(df_codes, df_labels, raw$variables, path_xlsx)
      out_files$excel_data <- path_xlsx
      message("[export_incidencias] Excel de datos exportado: ", path_xlsx)
    }
  }

  # -- 6. Diccionario de variables --------------------------------------------
  variables_dict <- tibble::tibble(
    nomvar = names(df_codes),
    label  = vapply(names(df_codes), function(v) {
      lbl <- raw$variables$label[raw$variables$nomvar == v]
      if (length(lbl) == 1 && !is.na(lbl) && nzchar(lbl)) lbl else ""
    }, character(1), USE.NAMES = FALSE),
    tipo   = vapply(names(df_codes), function(v) {
      col <- df_codes[[v]]
      if (inherits(col, c("Date", "POSIXct", "POSIXt"))) {
        "FECHA"
      } else if (v %in% multi_vars) {
        "M\u00DALTIPLE"
      } else if (is.character(col)) {
        "TEXTO"
      } else if (v %in% raw$codebook$nomvar) {
        "SIMPLE"
      } else {
        "N\u00DAMERO"
      }
    }, character(1), USE.NAMES = FALSE)
  )

  if (!is.null(format)) {
    path_vars <- file.path(output_dir,
                           paste0("variables_", study_id, "_rechazadas.xlsx"))
    .comprobar_sobrescribir(path_vars, overwrite)
    .export_variables_dict(variables_dict, raw$codebook, path_vars,
                           multi_vars = multi_vars)
    out_files$variables_dict <- path_vars
    message("[export_incidencias] Diccionario exportado: ", path_vars)
  }

  # -- 7. Objetos de retorno --------------------------------------------------
  out_files$data <- .build_spss_df(df_codes, raw$variables, raw$codebook,
                                   context = "[export_incidencias] ")

  out_files$variables <- variables_dict |>
    dplyr::select(variable = nomvar, etiqueta = label, tipo) |>
    dplyr::mutate(tipo_nuevo = NA_character_)

  out_files$niveles <- raw$codebook |>
    dplyr::filter(nomvar %in% names(df_codes)) |>
    dplyr::select(variable = nomvar, codigo = value, etiqueta = label)

  invisible(out_files)
}


# Anade a raw$variables y raw$codebook las variables definidas solo en otro
# idioma. .integra_fetch() lee un idioma; esas variables quedan fuera aunque su
# columna exista en la tabla de datos. Usa las etiquetas del idioma mas bajo.
#
# sin_fechas: si TRUE, omite las columnas Date/POSIXct, que .build_unified_df()
# leeria como texto.
.completar_otros_idiomas <- function(raw, study_id, db_config,
                                     sin_fechas = FALSE) {
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    dbname   = db_config$dbname,
    host     = db_config$host,
    port     = db_config$port,
    user     = db_config$user,
    password = db_config$password
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tabla <- paste0("variablescodigos_", study_id)
  if (!DBI::dbExistsTable(con, tabla)) return(raw)

  nuevas <- tibble::as_tibble(DBI::dbReadTable(con, tabla)) |>
    dplyr::filter(!nomvar %in% raw$variables$nomvar,
                  nomvar %in% names(raw$data))

  if (sin_fechas) {
    fechas <- names(raw$data)[vapply(raw$data, function(x) {
      inherits(x, c("Date", "POSIXct", "POSIXt"))
    }, logical(1))]
    nuevas <- dplyr::filter(nuevas, !nomvar %in% fechas)
  }
  if (nrow(nuevas) == 0) return(raw)

  nuevas <- nuevas |>
    dplyr::group_by(nomvar) |>
    dplyr::filter(idioma == min(idioma)) |>
    dplyr::ungroup()

  raw$variables <- dplyr::bind_rows(
    raw$variables,
    nuevas |>
      dplyr::distinct(nomvar, txtext) |>
      dplyr::transmute(nomvar, label = stringr::str_squish(txtext))
  )
  raw$codebook <- dplyr::bind_rows(
    raw$codebook,
    nuevas |>
      dplyr::distinct(nomvar, nomcod, defcod) |>
      dplyr::filter(nzchar(trimws(nomcod)), nzchar(trimws(defcod))) |>
      dplyr::transmute(nomvar,
                       value = trimws(as.character(defcod)),
                       label = stringr::str_squish(as.character(nomcod)))
  )

  message("[export_incidencias] Variables de otros idiomas agregadas (",
          length(unique(nuevas$nomvar)), "): ",
          paste(unique(nuevas$nomvar), collapse = ", "))
  raw
}


# Detiene la exportacion si el fichero existe y overwrite es FALSE
.comprobar_sobrescribir <- function(path, overwrite) {
  if (!overwrite && file.exists(path))
    stop("[export_incidencias] El fichero ya existe: ", path,
         "\nUsa overwrite = TRUE para sustituirlo.")
}
