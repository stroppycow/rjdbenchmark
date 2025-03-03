#' Reads the calendar regression labels file
#' 
#' @param path character Path to the CSV file to be loaded.
#' 
#' @details The mapping table must be a UTF-8 encoded CSV file with  
#' a header row. Each subsequent row corresponds to a 
#' calendar regression variable.  
#' The file must contain three comma-separated columns in the following order:
#' - **td**: Identifier of the calendar regression variable
#' in the JDemetra+ workspace.
#' - **label**: Label or description of the calendar
#' regression variable.
#' - **priority**: Order of priority.
#'
#' @return data.frame Calendar regression variable table.
#' @export
#
read_trading_days_label_file <- function(path = "./data/trading_days.csv"){
  if(!fs::file_exists(path)){
    stop("The calendar regression labels file does not exist.")
  }
  td_labels <- rlang::try_fetch(
    {
      readr::read_csv(
        file = path,
        locale = readr::locale(encoding = "UTF-8"),
        na = "",
        col_types = readr::cols(
          td = readr::col_character(),
          label = readr::col_character(),
          priority = readr::col_integer()
        )
      )
    }, error = function(e){
      rlang::abort(
        message = paste0("Unable to read the calendar regression labels file."),
        parent = e
      )
    }, warning = function(e){
      rlang::abort(
        message = paste0("Unable to read the calendar regression labels file."),
        parent = e
      )
    }
  )

  pb_1 <- td_labels |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(td)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_1) > 0){
    stop(
      paste0(
        "The value for 'td' is missing at row ",
        pb_1,
        " in the calendar regression labels file."
      )
    )
  }

  pb_2 <- td_labels |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(label)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_2) > 0){
    stop(
      paste0(
        "The value for 'label' is missing at row ",
        pb_2,
        " in the calendar regression labels file."
      )
    )
  }

  pb_3 <- td_labels |>
    dplyr::mutate(r = as.character(dplyr::row_number() + 1L)) |>
    dplyr::filter(is.na(priority)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(r)

  if(length(pb_3) > 0){
    stop(
      paste0(
        "The value for 'priority' is missing at row ",
        pb_3,
        " in the calendar regression labels file."
      )
    )
  }

  pb_4 <- td_labels |>
    dplyr::group_by(td) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1L) |>
    dplyr::slice_head(n = 1L)

  if(nrow(pb_4) > 0){
    stop(
      paste0(
        "Values in the td column are not unique ",
        "in the calendar regression labels file."
      )
    )
  }

  return(td_labels)
}
