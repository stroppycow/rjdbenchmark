#' Reading the configuration file
#'
#' @param path character(1) Path to the YAML metadata file to load
#' @export
#'
read_metadata_file <- function(path = "./data/metadata.yaml"){
  if(!fs::file_exists(path)){
    stop("The metadata file does not exist.")
  }
  metadata <- rlang::try_fetch(
    {
      yaml::read_yaml(
        file = path,
        fileEncoding = "UTF-8"
      )
    }, error = function(e){
      rlang::abort(
        message = "Unable to load metadata file!",
        parent = e
      )
    }
  )

  valid_refresh  <- c(
    "complete",
    "concurrent",
    "stochastic",
    "outliers",
    "lastoutliers",
    "parameters",
    "fixedarparameters",
    "fixedparameters",
    "fixed",
    "current"
  )

  valid_lang <- c("fr-FR")

  if(is.null(metadata$ws)){
    stop(
      paste0(
        "There is no 'ws' key at the first level ",
        "of the YAML metadata file."
      )
    )
  }

  if(is.null(metadata$ws$new)){
    stop(
      paste0(
        "There is no 'new' key within the 'ws' object ",
        "of the YAML metadata file."
      )
    )
  }

  if(!is.null(metadata$ws$old)){
    if(!is.list(metadata$ws$old)){
      stop(
        paste0(
          "The 'old' object within the 'ws' object ",
          "is not a list."
        )
      )
    }

    lapply(
      X = seq.int(along.with = metadata$ws$old),
      FUN = function(i){
        if(!is.list(metadata$ws$old[[i]])){
          stop(
            paste0(
              "Element no.",
              as.character(i),
              " within the ws$old object ",
              "is not a list."
            )
          )
        }

        if(!("name" %in% names(metadata$ws$old[[i]]))){
          stop(
            paste0(
              "The 'name' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is missing."
            )
          )
        }

        if(!is.character(metadata$ws$old[[i]]$name)){
          stop(
            paste0(
              "The 'name' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is not a character string."
            )
          )
        }

        if(length(metadata$ws$old[[i]]$name) != 1L){
          stop(
            paste0(
              "The 'name' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is not a single character string."
            )
          )
        }

        if(is.na(metadata$ws$old[[i]]$name)){
          stop(
            paste0(
              "The 'name' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is not a valid character string."
            )
          )
        }

        if(!("refresh" %in% names(metadata$ws$old[[i]]))){
          stop(
            paste0(
              "The 'refresh' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is missing."
            )
          )
        }

        if(!is.character(metadata$ws$old[[i]]$refresh)){
          stop(
            paste0(
              "The 'refresh' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is not a character string."
            )
          )
        }

        if(length(metadata$ws$old[[i]]$refresh) != 1L){
          stop(
            paste0(
              "The 'refresh' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is not a single character string."
            )
          )
        }

        if(is.na(metadata$ws$old[[i]]$refresh)){
          stop(
            paste0(
              "The 'refresh' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is not a valid character string."
            )
          )
        }

        if(!(metadata$ws$old[[i]]$refresh %in% valid_refresh)){
          stop(
            paste0(
              "The 'refresh' attribute in element no.",
              as.character(i),
              " within the ws$old object ",
              "is not one of the valid values: ",
              paste0(valid_refresh, collapse = ", ")
            )
          )
        }
      }
    ) |> invisible()
  }

  if(!is.list(metadata$ws$new)){
    stop("The 'new' object within 'ws' is not a list.")
  }

  if(length(metadata$ws$new) == 0L){
    stop("The 'ws$new' list is empty.")
  }

  lapply(
    X = seq.int(along.with = metadata$ws$new),
    FUN = function(i){
      if(!is.list(metadata$ws$new[[i]])){
        stop(paste0("Element no. ", i, " within 'ws$new' is not a list."))
      }

      if(!("name" %in% names(metadata$ws$new[[i]]))){
        stop(
          paste0(
            "The 'name' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is missing."
          )
        )
      }

      if(!is.character(metadata$ws$new[[i]]$name)){
        stop(
          paste0(
            "The 'name' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is not a valid character string."
          )
        )
      }

      if(length(metadata$ws$new[[i]]$name) != 1L){
        stop(
          paste0(
            "The 'name' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is not a valid character string."
          )
        )
      }

      if(is.na(metadata$ws$new[[i]]$name)){
        stop(
          paste0(
            "The 'name' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is not a valid character string."
          )
        )
      }

      if(!("refresh" %in% names(metadata$ws$new[[i]]))){
        stop(
          paste0(
            "The 'refresh' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is missing."
          )
        )
      }

      if(!is.character(metadata$ws$new[[i]]$refresh)){
        stop(
          paste0(
            "The 'refresh' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is not a valid character string."
          )
        )
      }

      if(length(metadata$ws$new[[i]]$refresh) != 1L){
        stop(
          paste0(
            "The 'refresh' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is not a valid character string."
          )
        )
      }

      if(is.na(metadata$ws$new[[i]]$refresh)){
        stop(
          paste0(
            "The 'refresh' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is not a valid character string."
          )
        )
      }

      if(!(metadata$ws$new[[i]]$refresh %in% valid_refresh)){
        stop(
          paste0(
            "The 'refresh' attribute in element no. ",
            as.character(i),
            " within 'ws$new' is not one of the valid values: ",
            paste0(valid_refresh, collapse = ", ")
          )
        )
      }
    }
  )

  if(is.null(metadata$lang)){
    stop(
      paste0(
        "There is no 'lang' key at the ",
        "first level of the YAML metadata file."
      )
    )
  }

  if(!is.character(metadata$lang)){
    stop(
      paste0(
        "The 'lang' value ",
        "is not a valid character string."
      )
    )
  }

  if(length(metadata$lang) != 1L){
    stop(
      paste0(
        "The 'lang' value ",
        "is not a valid character string."
      )
    )
  }

  if(is.na(metadata$lang)){
    stop(
      paste0(
        "The 'lang' value ",
        "is not a valid character string."
      )
    )
  }

  if(!(metadata$lang %in% valid_lang)){
    stop(
      paste0(
        "The 'lang' attribute ",
        "is not one of the valid values: ",
        paste0(valid_lang, collapse = ", ")
      )
    )
  }

  if(is.null(metadata$series)){
    stop(
      paste0(
        "There is no 'series' key at the first ",
        "level of the YAML metadata file."
      )
    )
  }

  if(!is.list(metadata$series)){
    stop("The 'series' object is not a valid non-empty list.")
  }

  if(length(metadata$series) == 0L){
    stop("The 'series' object is not a valid non-empty list.")
  }

  metadata$series <- lapply(
    X = seq.int(along.with = metadata$series),
    FUN = function(i){
      if(!is.list(metadata$series[[i]])){
        stop(
          paste0(
            "Element no. ",
            as.character(i),
            " within 'series' is not a list."
          )
        )
      }

      if(!("name" %in% names(metadata$series[[i]]))){
        stop(
          paste0(
            "The 'name' attribute in element no. ",
            as.character(i),
            " within 'series' is missing."
          )
        )
      }

      if(!("mode" %in% names(metadata$series[[i]]))){
        stop(
          paste0(
            "The 'mode' attribute in element no. ",
            as.character(i),
            " within 'series' is missing."
          )
        )
      }

      lapply(
        X = c(
          "name",
          "mode",
          "separator",
          "date_format",
          "encoding",
          "decimal.mark",
          "big.mark",
          "sheet_name"
        ),
        FUN = function(x){
          if(x %in% names(metadata$series[[i]])){
            if(!is.character(metadata$series[[i]][[x]])){
              stop(
                paste0(
                  "The attribute '",
                  x,
                  "' in element #",
                  as.character(i),
                  " within the 'series' object ",
                  "is not a character string."
                )
              )
            }

            if(length(metadata$series[[i]][[x]]) != 1L){
              stop(
                paste0(
                  "The attribute '",
                  x,
                  "' in element #",
                  as.character(i),
                  " within the 'series' object ",
                  "is not a character string."
                )
              )
            }

            if(is.na(metadata$series[[i]][[x]])){
              stop(
                paste0(
                  "The attribute '",
                  x,
                  "' in element #",
                  as.character(i),
                  " within the 'series' object ",
                  "is not a character string."
                )
              )
            }
          }
        }
      )

      if("mapping" %in% names(metadata$series[[i]])){
        if(!is.list(metadata$series[[i]]$mapping)){
          stop(
            paste0(
              "The attribute 'mapping'",
              " in element #",
              as.character(i),
              " within the 'series' object ",
              "is not a list."
            )
          )
        }
        if(length(unique(names(metadata$series[[i]]$mapping))) != length(names(metadata$series[[i]]$mapping))){
          stop(
            paste0(
              "The attribute 'mapping'",
              " in element #",
              as.character(i),
              " within the 'series' object ",
              "is not a valid list."
            )
          )
        }

        if(length(setdiff(names(metadata$series[[i]]$mapping), c("code", "date", "value"))) != 0L){
          stop(
            paste0(
              "The attribute 'mapping'",
              " in element #",
              as.character(i),
              " within the 'series' object ",
              "is not a valid list."
            )
          )
        }

        lapply(
          X = c(
            "code",
            "date",
            "value"
          ),
          FUN = function(x){
            if(x %in% names(metadata$series[[i]]$mapping)){
              if(!is.character(metadata$series[[i]]$mapping[[x]])){
                stop(
                  paste0(
                    "The attribute mapping$",
                    x,
                    " in element #",
                    as.character(i),
                    " within the 'series' object ",
                    "is not a character string."
                  )
                )
              }

              if(length(metadata$series[[i]]$mapping[[x]]) != 1L){
                stop(
                  paste0(
                    "The attribute mapping$",
                    x,
                    " in element #",
                    as.character(i),
                    " within the 'series' object ",
                    "is not a character string."
                  )
                )
              }

              if(is.na(metadata$series[[i]]$mapping[[x]])){
                stop(
                  paste0(
                    "The attribute mapping$",
                    x,
                    " in element #",
                    as.character(i),
                    " within the 'series' object ",
                    "is not a character string."
                  )
                )
              }
            }
          }
        )
        metadata$series[[i]]$mapping <- rlang::try_fetch(
          unlist(metadata$series[[i]]$mapping),
          error = function(e){
            stop(
              paste0(
                "The attribute 'mapping'",
                " in element #",
                as.character(i),
                " within the 'series' object ",
                "is not a valid list."
              )
            )
          }
        )
      }
      return(metadata$series[[i]])
    }
  )

  # Check for uniqueness of names in ws$old
  if(!is.null(metadata$ws$old)){
    pb_1 <- sapply(
      X = metadata$ws$old,
      FUN = function(x){
        return(x$name)
      }
    ) |> table()
    if(!is.na(names(pb_1)[pb_1 > 1][1])){
      stop(
        paste0(
          "The value '",
          names(pb_1)[pb_1 > 1][1],
          "' for the attribute 'name'",
          " is present multiple times in the ws$old list."
        )
      )
    }
  }

  # Check for uniqueness of names in ws$new
  if(!is.null(metadata$ws$new)){
    pb_2 <- sapply(
      X = metadata$ws$new,
      FUN = function(x){
        return(x$name)
      }
    ) |> table()
    if(!is.na(names(pb_2)[pb_2 > 1][1])){
      stop(
        paste0(
          "The value '",
          names(pb_2)[pb_2 > 1][1],
          "' for the attribute 'name'",
          " is present multiple times in the ws$new list."
        )
      )
    }
  }

  # Check for uniqueness of names in series
  if(!is.null(metadata$series)){
    pb_3 <- sapply(
      X = metadata$series,
      FUN = function(x){
        return(x$name)
      }
    ) |> table()
    if(!is.na(names(pb_3)[pb_3 > 1][1])){
      stop(
        paste0(
          "The value '",
          names(pb_3)[pb_3 > 1][1],
          "' for the attribute 'name'",
          " is present multiple times in the 'series' list."
        )
      )
    }
  }

  return(metadata)
}