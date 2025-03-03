#' Formats a date and a frequency into a character string
#' describing the period
#'
#' @param date Date: Start date of the period
#' @param frequency : character(1)/numeric Periodicity in the data
#'
#' @return character(1) Character string describing the period
#' in natural language
#' @export
#'
#' @examples format_period(as.Date("2023-01-01"), "month")
format_period <- function(date, frequency){
  frequency <- clean_frequency_input(frequency)
  if(frequency == "day"){
    return(paste0("on  ", format(date, "%A %d %B %Y")))
  } else if (frequency == "week"){
    return(paste0(
      "week ", format(date, "%V"), " of ",  format(date, "%G")
    ))
  } else if (frequency == "month"){
    return(paste0("the month of ", format(date, "%B"), " ", format(date, "%Y")))
  } else if (frequency == "quarter"){
    trim_rank <- (as.integer(format(date,"%m")) %/% 4L)+1L
    trim <- c("first", "second", "third", "fourth")[trim_rank]
    return(paste0("the ", trim, " quarter ", format(date, "%Y")))
  } else if (frequency == "year"){
    return(paste0("the year ",  format(date, "%Y")))
  } else {
    stop("Invalid \"frequency\" parameter")
  }
}

#' Formats a frequency into a character string
#'
#' @param frequency  character(1)/numeric Periodicity in the data
#'
#' @return character(1) Character string describing the frequency
#' in natural language
#' @export
#'
#' @examples format_frequency("day")
format_frequency <- function(frequency){
  frequency <- clean_frequency_input(frequency)
  if(frequency == "day"){
    return("daily")
  }else if(frequency == "week"){
    return("weekly")
  }else if(frequency == "month"){
    return("monthly")
  }else if(frequency == "quarter"){
    return("quarterly")
  }else if(frequency == "year"){
    return("yearly")
  } else {
    stop("Invalid \"frequency\" parameter")
  }
}


#' Standardization of frequency input
#'
#' @param frequency character(1)/numeric Character string describing the
#' data frequency or number of days (average or exact) between each point
#'
#' @return character(1) One of the following values:
#' - `year` : yearly
#' - `quarter` : quarterly
#' - `month` : monthly
#' - `week` : weekly
#' - `day` : daily
#' @export
#'
#' @examples clean_frequency_input(frequency = 365)
clean_frequency_input <- function(frequency){
  if(is.character(frequency) && length(frequency) == 1L){
    if(frequency %in% c("day","days","dayly")){
      return("day")
    } else if(frequency %in% c("week", "weeks", "weekly")){
      return("week")
    } else if(frequency %in% c("month", "months", "monthly")){
      return("month")
    } else if(frequency %in% c("quarter", "quarters", "quarterly")){
      return("quarter")
    } else if(frequency %in% c("year", "years", "yearly")){
      return("year")
    } else {
      stop(paste0("Le paramètre \"frequency\" avec la valeur ",
                  as.character(frequency),
                  " est invalide"))
    }
  } else if(is.numeric(frequency) && length(frequency) > 0L){
    if(all(frequency == 1)){
      return("day")
    } else if(all(frequency == 7)){
      return("week")
    } else if(all(frequency %in% c(28, 29, 30, 31))){
      return("month")
    } else if(all(frequency %in% c(90, 91, 92))){
      return("quarter")
    } else if(all(frequency %in% c(365, 366))){
      return("year")
    } else {
      stop(
        paste0(
          "The parameter \"frequency\" with the value ",
          paste0(as.character(frequency), collapse =", "),
          " is invalid"
        )
      )
    }
  } else {
    stop(
      paste0(
      "The parameter \"frequency\" is invalid. ",
      "It is impossible to interpret the frequency."
      )
    )
  }
}

#' Validates date consistency with a given frequency
#'
#' @param dates Dates vector of dates
#' @param frequency character(1)/numeric Periodicity in the data
#'
#' @return logical TRUE if dates are consistent with the frequency,
#' FALSE otherwise
#' @export
#'
check_dates_validity <- function(dates, frequency, unique_sorted_dates = FALSE){
  frequency <- clean_frequency_input(frequency)
  if(length(dates) >= 2L){
    if(unique_sorted_dates){
      dates_found <- dates
    } else {
      dates_found <- sort(unique(dates))
    }
    dates_expected <- seq.Date(
      from = lubridate::floor_date(dates_found[1], unit = frequency),
      by = frequency,
      length.out = length(dates_found)
    )

    if(!all(dates_found == dates_expected)){
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Infers the frequency of data from a vector of dates
#'
#' @param dates Dates vector of dates
#'
#' @return character A frequency
#' @export
#'
#' @examples deduce_frequency(
#'  dates = as.Date("2000-01-01", "2000-01-01", "2000-03-01")
#' )
deduce_frequency <- function(dates){
  if(!lubridate::is.Date(dates)){
    stop("Dates are expected")
  }

  if(is.unsorted(x = dates, strictly = TRUE)){
    stop("The given dates are not strictly increasing")
  }

  if(length(dates) <= 1L){
    return(as.character(NA))
  } else {
    first_dates <- dates[1:2]
    if(first_dates[2] == first_dates[1] + lubridate::years(1)){
      frequency <- "year"
    } else if(first_dates[2] == first_dates[1] + months(3)){
      frequency <- "quarter"
    } else if(first_dates[2] == first_dates[1] + months(1)){
      frequency <- "month"
    } else if(first_dates[2] == first_dates[1] + lubridate::weeks(1)){
      frequency <- "week"
    } else if(first_dates[2] == first_dates[1] + lubridate::days(1)){
      frequency <- "day"
    } else {
      stop("Unable to detect frequency in the data")
    }
    if(!check_dates_validity(dates = dates, frequency = frequency)){
      stop(
        paste0(
          "Unable to detect frequency in the data, ",
          "even though the first two dates suggested it was ",
          format_frequency(frequency)
        )
      )
    }
    return(frequency)
  }
}
