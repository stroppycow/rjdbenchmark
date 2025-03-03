#' Reads the nomenclature level labels file
#' 
#' @param path character Path to the CSV file to be loaded.
#' 
#' @details The mapping table must be a UTF-8 encoded CSV file with  
#' a header row. Each subsequent row corresponds to a nomenclature level
#' The file must contain three comma-separated columns in the following order:
#' - **level**: Identifier of the nomenclature level
#' - **label**: Label or description of the nomenclature level
#' - **priority**: Order of priority.
#'
#' @return data.frame Nomenclature level table.
#' @export
#
read_levels_label_file <- function(path = "./data/levels.csv"){
  if(!fs::file_exists(path)){
    stop("Nomenclature level labels file does not exist !")
  }
  data_levels <- rlang::try_fetch(
    {
      readr::read_csv(
        file = path,
        locale = readr::locale(encoding = "UTF-8"),
        na = "",
        col_types = readr::cols(
          level = readr::col_character(),
          label = readr::col_character(),
          priority = readr::col_integer()
        )
      )
    }, error = function(e){
      rlang::abort(
        message = paste0("Unable to read the nomenclature level labels file."),
        parent = e
      )
    }, warning = function(e){
      rlang::abort(
        message = paste0("Unable to read the nomenclature level labels file."),
        parent = e
      )
    }
  )

  pb_1 <- data_levels |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(level)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_1) > 0){
    stop(
      paste0(
        "The value for 'level' is missing at row ",
        pb_1,
        " in the nomenclature level labels file."
      )
    )
  }

  pb_2 <- data_levels |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(label)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_2) > 0){
    stop(
      paste0(
        "The value for 'label' is missing at row ",
        pb_2,
        " in the nomenclature level labels file."
      )
    )
  }

  pb_3 <- data_levels |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(priority)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_3) > 0){
    stop(
      paste0(
        "The value for 'priority' is missing at row ",
        pb_3,
        " in the nomenclature level labels file."
      )
    )
  }

  pb_4 <- data_levels |>
    dplyr::group_by(level) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1L) |>
    dplyr::slice_head(n = 1L)

  if(nrow(pb_4) > 0){
    stop(
      paste0(
        "Values in the 'level' column are not unique ",
        "in the nomenclature level labels file."
      )
    )
  }

  return(data_levels)
}
