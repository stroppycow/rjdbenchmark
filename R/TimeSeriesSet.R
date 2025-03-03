#' Set of indices
#'
#' @slot data data.table Table containing index values with variables:
#' - `code`: Code of the nomenclature associated with the level
#' - `date`: Period/Date corresponding to the index point
#' - `value`: Index value corresponding to this level of the 
#' nomenclature and this code by date
#' @slot frequency character(1) Data frequency: allowed values are:
#' - `year`: annual
#' - `quarter`: quarterly
#' - `month`: monthly
#' - `week`: weekly
#' - `day`: daily
#'
#' @include utils-date.R
#' @include utils-s4.R
methods::setClass(
  Class = "TimeseriesSet",
  slots = c(
    id = "characterOrNull",
    data = "data.table",
    frequency = "character"
  ),
  prototype = list(
    id = NULL,
    data = data.table::data.table(
      code = character(0),
      date = as.Date(integer()),
      value = double(),
      key = c("code", "date")
    ),
    frequency = "day"
  )
)

methods::setValidity(
  Class = "TimeseriesSet",
  method = function(object){
    if(!is.null(object@id)){
      if(length(object@id) != 1L ){
        return("@id does not contain exactly one element")
      }
      
      if(is.na(object@id)){
        return("@id cannot be NA")
      }
      
      if(object@id == ""){
        return("@id cannot be an empty string")
      }
      
      if(!stringr::str_detect(string = object@id, pattern = "^[a-z0-9_]+$")){
        return("@id does not match the pattern ^[a-z0-9_]+$")
      }
    }
    if(length(object@frequency) != 1){
      return("The 'frequency' attribute is not of length 1")
    }
    frequency_allowed <- sapply(
      X = c("year", "quarter", "month", "week", "day"),
      FUN = clean_frequency_input
    )
    
    if(!(object@frequency %in% frequency_allowed)){
      return(paste0(
         "The 'frequency' attribute cannot have the value '",
        object@frequency,
        "'; only the following values are allowed: ",
        paste0(frequency_allowed, collapse=", ")
      ))
    }
    
    if(ncol(object@data) != 3L){
      return(paste0(
        "Invalid number of columns in the 'data' attribute: expected 3 but found ",
        as.character(ncol(object@data))
      ))
    }
    
    columns_expected <- c("code","date","value")
    if(!all(colnames(object@data) == columns_expected)){
      return(paste0(
        "The columns of the 'data' attribute are invalid: ",
        paste0(colnames(object@data), collapse = ", "),
        " found but expected ",
        paste0(columns_expected, collapse = ", ")
      ))
    }
    
    # Checking column types in data
    column_class <- lapply(object@data, class)
    if(column_class[["code"]] != "character"){
      return("The column \"code\" is not of type character")
    }
    
    if(column_class[["date"]] != "Date"){
       return("The column \"date\" is not of type Date")
    }
    
    if(column_class[["value"]] != "numeric"){
      return("The column \"value\" is not of type numeric/double")
    }
    
    dt_key <- data.table::key(object@data)
    if(is.null(dt_key) || length(dt_key) != 3L ||
      any(dt_key != c("code", "date"))
    ){
      data.table::setkey(x = object@data, code, date)
    }
    data.table::setindex(object@data, date)
    
    
    # Checking date consistency
    dt_dates_test <- object@data[,.(
      test = check_dates_validity(
        dates = date,
        frequency = object@frequency,
        unique_sorted_dates = TRUE)
    ), by = .(code)][test == FALSE, ]
    
    if(nrow(dt_dates_test) != 0L){
      return(paste0(
        "Dates are inconsistent for the series ",
        as.character(dt_dates_test[1, code])
      ))
    }
  }
)

methods::setAs(
  from = "data.frame",
  to = "TimeseriesSet",
  def = function(from){
    data <- data.table::as.data.table(from)
    column_class <- lapply(data, class)
    if(column_class[["code"]] != "character"){
      data[ , code := as.character(code)]
    }
    
    if(column_class[["date"]] == "character"){
      data[ , date :=  as.Date(date)]
    }
    
    if(column_class[["value"]] == "character"){
      data[ , value := as.double(value)]
    }
    
    data.table::setcolorder(data, c("code", "date", "value"))
    data.table::setkey(data , code, date)
    
    # Inferring frequency from the first two dates
    dt_dates <- data[,.(.N), keyby=.(date)]
    frequency <- deduce_frequency(dates = dt_dates$date)
    
    if(is.na(frequency)){
      return(methods::new(
        Class = "TimeseriesSet",
        data = data
      ))
    } else {
      return(methods::new(
        Class = "TimeseriesSet",
        data = data,
        frequency = frequency
      ))
    }
  }
)
