#' Extraction of final JDemetra+ series after decomposition
#'
#' @param series character Mapping of extracted series (names are the series 
#' names for JDemetra+ and values are aliases)
#' @param output_folder_ws character(1) Folder containing the output of 
#' JDemetra+ processing
#' @param frequency character(1) Frequency of the time series
#' 
#' @return Named list of index sets, each element of the list contains 
#' a requested JDemetra+ component type (ycal, sa...)
#' @export
#'
extract_all_series_jdemetra <- function(
    series,
    output_folder_ws,
    frequency
){
  res <- lapply(
    X = series,
    FUN = extract_single_serie_jdemetra,
    output_folder_ws,
    frequency
  )
  names(res) <- series
  return(res)
}

#' Extraction of a final JDemetra+ series after decomposition
#'
#' @param series character(1) Series name in JDemetra+ (y_cal, sa...)
#' @param output_folder_ws character(1) Folder containing the output of 
#' JDemetra+ processing
#' @param frequency character(1) Frequency of the time series
#' 
#' @return Named list of index sets, each element of the list contains 
#' a requested JDemetra+ component type (ycal, sa...)
#' 
#' @export
#'
extract_single_serie_jdemetra <- function(
    serie,
    output_folder_ws,
    frequency
){
  d <- data.table::fread(
    file = fs::path(output_folder_ws, paste0("series_", serie, ".csv")),
    sep = ";",
    header = TRUE,
    colClasses = "character"
  )
  
  d <- data.table::melt.data.table(
    data = d,
    id.vars = "V1",
    variable.name = "date"
  )
  
  data.table::setnames(
    x = d,
    old = "V1",
    new = "code"
  )
  
  d[, date := as.character(date)]
  d[, date := as.Date(lubridate::fast_strptime(date, "%Y-%m-%d"))]
  d[, date := as.Date(
    lubridate::floor_date(
      date,
      unit = substr(frequency, 1, nchar(frequency) -2)
    ))
  ]
  d[, value := stringr::str_replace(value, pattern = ",", replacement = ".")]
  d[, value := as.double(value)]
  
  return(methods::new(
    Class = "TimeseriesSet",
    data = d,
    frequency = frequency
  ))
}
