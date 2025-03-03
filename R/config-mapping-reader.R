#' Reads the mapping table between SaItems from old and new workspaces and the raw time series
#' 
#' @param path character Path to the file to be loaded
#' 
#' @details The mapping table must be a CSV file encoded in UTF-8 with
#' a first header row.
#' Each subsequent row corresponds to a pair of SaItems being compared.  
#' The file must contain 11 comma-separated variables in the following order:
#' - ws_old: Name of the old workspace containing the original SaItem
#' - sa_item_old: Name of the old SaItem in ws_old
#' - ws_new: Name of the new workspace containing the updated SaItem
#' - sa_item_new: Name of the new SaItem in ws_new
#' - data_file: Name of the file containing the raw time series
#' - serie_name_data_file: Name of the series used to identify it in data_file
#' - serie_group: Identifier for series grouping, used for display in the
#' static site
#' - level: Identifier for the classification level, used for display in the
#' static site
#' - code: Classification identifier, used for display in the static site
#' - label: Series name
#' - weight: Weight associated with the series
#'
#' @return data.frame Mapping table
#' @export
#
read_mapping_file <- function(path = "./data/mapping.csv"){
  if(!fs::file_exists(path)){
    stop("The mapping file does not exist.")
  }
  mapping <- rlang::try_fetch(
    {
      readr::read_csv(
        file = path,
        locale = readr::locale(encoding = "UTF-8"),
        na = "",
        col_types = readr::cols(
          ws_old = readr::col_character(),
          sa_item_old = readr::col_character(),
          ws_new = readr::col_character(),
          sa_item_new = readr::col_character(),
          data_file = readr::col_character(),
          serie_name_data_file = readr::col_character(),
          serie_group = readr::col_character(),
          level  = readr::col_character(),
          code = readr::col_character(),
          label = readr::col_character(),
          weight = readr::col_double()
        )
      )
    }, error = function(e){
      rlang::abort(
        message = paste0("Impossible to load mapping file !"),
        parent = e
      )
    }, warning = function(e){
      rlang::abort(
        message = paste0("Impossible to load mapping file !"),
        parent = e
      )
    }
  )

  if(nrow(mapping) == 0L){
    stop("Mapping file is empty !")
  }

  pb_1 <- mapping |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(serie_group)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_1) > 0){
    stop(
      paste0(
        "The value for 'serie_group' is missing at row ",
        pb_1,
        " in the mapping file."
      )
    )
  }

  pb_2 <- mapping |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(level)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_2) > 0){
    stop(
      paste0(
        "The value for 'level' is missing at row ",
        pb_2,
        " in the mapping file."
      )
    )
  }

  pb_3 <- mapping |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(code)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_3) > 0){
    stop(
      paste0(
        "The value for 'code' is missing at row ",
        pb_3,
        " in the mapping file."
      )
    )
  }

  pb_4 <- mapping |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(data_file)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_4) > 0){
    stop(
      paste0(
        "The value for 'data_file' is missing at row ",
        pb_4,
        " in the mapping file."
      )
    )
  }

  pb_5 <- mapping |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(serie_name_data_file)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_5) > 0){
    stop(
      paste0(
        "The value for 'serie_name_data_file' is missing at row ",
        pb_5,
        " in the mapping file."
      )
    )
  }

  
  pb_6 <- mapping |>
    dplyr::group_by(serie_group, level, code) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1L) |>
    dplyr::slice_head(n = 1L)

  if(nrow(pb_6) > 0){
    stop(
      paste0(
        "The triplet (serie_group=",
        pb_6$serie_group,
        ", level=",
        pb_6$level,
        ", code=",
        pb_6$code,
        ") is not unique in the mapping file."
      )
    )
  }

   pb_7 <- mapping |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(
      (is.na(ws_old) & !is.na(sa_item_old)) |
      (!is.na(ws_old) & is.na(sa_item_old))
    ) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_7) > 0){
    stop(
      paste0(
        "'ws_old' and 'sa_item_old' values must either be both filled ",
        "or both missing. This condition is not met at line ",
        pb_7
      )
    )
  }

  pb_8 <- mapping |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(
      (is.na(ws_new) & !is.na(sa_item_new)) |
      (!is.na(ws_new) & is.na(sa_item_new))
    ) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_8) > 0){
    stop(
      paste0(
        "'ws_new' and 'sa_item_new' values must either be both filled ",
        "or both missing. This condition is not met at line ",
        pb_8
      )
    )
  }

  pb_9 <- mapping |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(
      (is.na(ws_old) & is.na(sa_item_old)) &
      (is.na(ws_new) & is.na(sa_item_new))
    ) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_9) > 0){
    stop(
      paste0(
        "'ws_old', 'sa_item_old', 'ws_new', and 'sa_item_new' values ",
        "cannot all be missing, as seen at line ",
        pb_9
      )
    )
  }

  return(mapping)
}
