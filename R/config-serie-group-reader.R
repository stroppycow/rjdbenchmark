#' Reads the time series group labels file
#' 
#' @param path character Path to the CSV file to be loaded.
#' 
#' @details The mapping table must be a UTF-8 encoded CSV file with  
#' a header row. Each subsequent row corresponds to a time series group
#' The file must contain three comma-separated columns in the following order:
#' - **serie_group**: Identifier of the time series group
#' - **label**: Label or description of the time series group
#' - **priority**: Order of priority.
#'
#' @return data.frame Time series group table.
#' @export
#
read_serie_group_label_file <- function(path = "./data/serie_group.csv"){
  if(!fs::file_exists(path)){
    stop("Time series group labels file does not exist !")
  }
  data_series_group <- rlang::try_fetch(
    {
      readr::read_csv(
        file = path,
        locale = readr::locale(encoding = "UTF-8"),
        na = "",
        col_types = readr::cols(
          serie_group = readr::col_character(),
          label = readr::col_character(),
          priority = readr::col_integer()
        )
      )
    }, error = function(e){
      rlang::abort(
        message = "Unable to read the time series group labels file!",
        parent = e
      )
    }, warning = function(e){
      rlang::abort(
        message = "Unable to read the time series group labels file!",
        parent = e
      )
    }
  )

  pb_1 <- data_series_group |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(serie_group)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_1) > 0){
    stop(
      paste0(
        "The value for 'level' is missing at row ",
        pb_1,
        " in the time series group labels file."
      )
    )
  }

  pb_2 <- data_series_group |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(label)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_2) > 0){
    stop(
      paste0(
        "The value for 'label' is missing at row ",
        pb_2,
        " in the time series group labels file."
      )
    )
  }

  pb_3 <- data_series_group |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(priority)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_3) > 0){
    stop(
      paste0(
        "The value for 'priority' is missing at row ",
        pb_3,
        " in the time series group labels file."
      )
    )
  }

  pb_4 <- data_series_group |>
    dplyr::group_by(serie_group) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1L) |>
    dplyr::slice_head(n = 1L)

  if(nrow(pb_4) > 0){
    stop(
      paste0(
        "Values in the 'serie_group' column are not unique ",
        "in the time series group labels file."
      )
    )
  }

  return(data_series_group)
}
