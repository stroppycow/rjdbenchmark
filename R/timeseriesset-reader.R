#' Reading a file containing a set of time series
#'
#' @param file character(1) Path to the file to be loaded
#' @param mode character(1) Representation mode of the data in the file.
#' Possible values are:
#' - `vtable`: Data is stored vertically (each row in the file
#' represents the value of a series for a given period)
#' - `htable1`: Data is stored horizontally (each row in the file
#' contains all values for a given date)
#' - `htable2`: Data is stored horizontally (each row in the file
#' contains all values for a given series)
#' @param separator character(1) Column separator if the loaded file is
#' a flat file
#' @param mapping character Column mapping
#' @param date_format character(1) Date format
#' @param encoding character(1) File encoding
#' @param decimal.mark character(1) Decimal separator
#' @param big.mark character(1) Thousands separator
#' @param sheet_name character(1) Sheet name in case of an import
#' from a spreadsheet
#' @param sheet_index character(1) Sheet index in case of an import
#' from a spreadsheet
#'
#' @return TimeseriesSet Set of indices
#' @export
read_timeseries_set_file <- function(
    file,
    mode = "vtable",
    separator = NULL,
    mapping = c(
      "code" = "code",
      "date" = "date",
      "value" = "value"
    ),
    date_format = "%Y-%m-%d",
    encoding = NULL,
    decimal.mark = ".",
    big.mark = "",
    sheet_name = NULL,
    sheet_index = NULL
){
  if(!fs::file_exists(file)){
    stop(paste0(
      "The file ",
      file,
      " does not exist."
    ))
  }
  
  if(is.null(mode)){
    stop(
      paste0(
        "The \"mode\" argument related to data representation ",
        "cannot be null."
      )
    )
  }
  
  if(!is.character(mode)){
    stop("The \"mode\" argument is not a character string.")
  }
  
  if(length(mode) != 1L){
    stop(
      paste0(
        "The \"mode\" argument related to data representation ",
        "must have a length of 1."
      )
    )
  }

  if(!(mode %in% c("vtable", "htable1", "htable2"))){
    stop(paste0(
      "The \"mode\" argument related to data representation ",
      mode,
      " in the file to be loaded is invalid.",
      ' "vtable" (one row per series x date)',
      ' "htable1" (one row per date: columns are series names)',
      ' "htable2" (one row per series: columns are dates).'
    ))
  }

  # Chargement des données
  extension <- fs::path_ext(file)
  if(extension == "sas7bdat"){
    logging::logdebug(
      msg = paste0(
        "Loading index file in SAS format from path ",
        fs::path_abs(file)
      ),
      logger = logger
    )
    data <- data.table::as.data.table(haven::read_sas(file))
  } else if (extension %in% c("", "csv", "txt")){
    if(!is.null(encoding)){
      encoding_csv <- encoding
    } else {
      encoding_csv <- formals(data.table::fread)$encoding
    }
    encoding_txt <- NULL
    data <- data.table::fread(
      file = file,
      colClasses = "character",
      sep = separator,
      header = TRUE,
      encoding = encoding_csv,
      showProgress = FALSE
    )
  } else if (extension == "parquet"){
    data <- data.table::as.data.table(
      arrow::read_parquet(file = file)
    )
  } else if(extension == "xls" || extension == "xlsx"){
    if(!is.null(sheet_name)){
      sheet_excel <- sheet_name
    } else if(!is.null(sheet_index)){
      sheet_excel <- sheet_index
    } else {
      sheet_excel <- 1L
    }
    data <- data.table::as.data.table(
      readxl::read_excel(
        path = file,
        sheet = sheet_excel,
        progress = FALSE,
        .name_repair = "unique_quiet"
      )
    )
  } else {
    stop(paste0(
      "File extension \"",
      extension,
      "\" not supported."
    ))
  }

  if(length(mapping) == 0L){
    mapping <- c(
      "code" = "code",
      "date" = "date",
      "value" = "value"
    )
  }

  # Formatage de l'attribut data

  expected_colnames <- c("code", "date", "value") # Colonnes attendues

  if( mode == "vtable" ){
    # Renommage eventuel de colonnes en fonction du mapping
    old_colnames <- mapping[
      names(mapping) %in% expected_colnames
    ]
    new_colnames <- names(mapping)[
      names(mapping) %in% expected_colnames
    ]
    data.table::setnames(data, old = old_colnames, new = new_colnames)
  } else if(mode == "htable2") {
    id.vars <- "date"
    old_colnames <- mapping[
      names(mapping) %in% id.vars
    ]
    new_colnames <- names(mapping)[
      names(mapping) %in% id.vars
    ]
    test_col <- sapply(X = old_colnames, FUN = function(x,noms){
      return(x %in% noms)
    }, colnames(data))
    
    data.table::setnames(data, old = old_colnames[test_col], new = new_colnames[test_col])
    
    id.vars_found <- intersect(colnames(data),id.vars)
    if(length(id.vars_found) == 0L){
      stop("No column related to date identification found")
    }
    
    measure.vars_found <- setdiff(colnames(data), id.vars_found)
    
    data <- data.table::melt.data.table(
      data = data,
      id.vars = id.vars_found ,
      measure.vars = measure.vars_found,
      variable.name = "code",
      value.name = "value",
      variable.factor = FALSE
    )
  } else {
    # Possible renaming of identifier columns based on mapping
    id.vars <- "code"
    old_colnames <- mapping[
      names(mapping) %in% id.vars
    ]
    new_colnames <- names(mapping)[
      names(mapping) %in% id.vars
    ]
    test_col <- sapply(X = old_colnames, FUN = function(x,noms){
      return(x %in% noms)
    }, colnames(data))

    data.table::setnames(data, old = old_colnames[test_col], new = new_colnames[test_col])

    id.vars_found <- intersect(colnames(data),id.vars)
    if(length(id.vars_found) == 0L){
      stop("No column related to index identification found")
    }

    measure.vars_found <- setdiff(colnames(data), id.vars_found)
    
    data <- data.table::melt.data.table(
      data = data,
      id.vars = id.vars_found ,
      measure.vars = measure.vars_found,
      variable.name = "date",
      value.name = "value",
      variable.factor = FALSE
    )
  }

  column_class <- lapply(data, class)

  if(column_class[["date"]] == "character"){
    data[ , date :=  as.Date(lubridate::fast_strptime(date, date_format))]
  }


  if(column_class[["value"]] == "character"){
    if(!is.character(big.mark)){
      stop(
        paste0(
          "The \"big.mark\" argument for the thousands separator ",
          "is not a character string."
        )
      )
    }

    if(length(big.mark) != 1L){
      stop(
        paste0(
          "The \"big.mark\" argument for the thousands separator ",
          "is not a character string."
        )
      )
    }

    big.mark_raw <- iconv(big.mark, toRaw = TRUE)[[1]]
    if(length(big.mark_raw) > 1L ){
      stop(
        paste0(
          "The thousands separator ",
          big.mark,
          " is not supported."
        )
      )
    } else if(length(big.mark_raw) == 1L){
      data[ , value :=  stringr::str_replace_all(
        string = value,
        pattern = paste0("\\u00", big.mark_raw),
        replacement = ""
      )
      ]
    }

    if(!is.character(decimal.mark)){
      stop(
        paste0(
          "The \"decimal.mark\" argument for the decimal separator ",
          "is not a character string."
        )
      )
    }

    if(length(decimal.mark) != 1){
      stop(
        paste0(
          "The \"decimal.mark\" argument for the decimal separator ",
          "is not a character string."
        )
      )
    }
    decimal.mark_raw <- iconv(decimal.mark, toRaw = TRUE)[[1]]

    data[value == "" , value := as.character(NA)]
    data[ , value_num := as.double(NA)]

    data[!is.na(value) , value := stringr::str_replace_all(
      string = value,
      pattern = paste0("\\u00", decimal.mark_raw),
      replacement = "."
    )]
    data[!is.na(value) , value_num := as.double(value)]
    data[, value := NULL]
    data.table::setnames(
      x = data,
      old = "value_num",
      new = "value"
    )
  }

  data_ts <- methods::as(
    object = data[,.(code, date, value)],
    Class = "TimeseriesSet"
  )
  
  return(data_ts)
}
