check_consistency_config <- function(
  mapping,
  metadata,
  data_levels,
  data_series_group
){
  old_ws_mapping <- mapping |>
    dplyr::filter(!is.na(ws_old)) |>
    dplyr::pull(ws_old) |>
    unique()

  old_ws_metadata <- character()
  if(!is.null(metadata$ws$old)){
    old_ws_metadata <- sapply(
      X = metadata$ws$old,
      FUN = function(x){
        return(x$name)
      }
    )
  }

  pb_1 <- setdiff(old_ws_mapping, old_ws_metadata)
  if(length(pb_1) != 0L){
    stop(
      paste0(
        "The metadata and mapping files are inconsistent ",
        "because the old workspace ",
        pb_1[1],
        " is present in the mapping file but not in the ",
        "metadata file."
      )
    )
  }

  pb_2 <- setdiff(old_ws_metadata, old_ws_mapping)
  if(length(pb_2) != 0L){
    stop(
      paste0(
        "The metadata and mapping files are inconsistent ",
        "because the old workspace ",
        pb_2[1],
        " is present in the metadata file but not in the ",
        "mapping file."
      )
    )
  }

  new_ws_mapping <- mapping |>
    dplyr::filter(!is.na(ws_new)) |>
    dplyr::pull(ws_new) |>
    unique()

  new_ws_metadata <- character()
  if(!is.null(metadata$ws$new)){
    new_ws_metadata <- sapply(
      X = metadata$ws$new,
      FUN = function(x){
        return(x$name)
      }
    )
  }

  pb_3 <- setdiff(new_ws_mapping, new_ws_metadata)
  if(length(pb_3) != 0L){
    stop(
      paste0(
        "The metadata and mapping files are inconsistent ",
        "because the new workspace ",
        pb_3[1],
        " is present in the mapping file but not in the ",
        "metadata file."
      )
    )
  }

  pb_4 <- setdiff(new_ws_metadata, new_ws_mapping)
  if(length(pb_4) != 0L){
    stop(
      paste0(
        "The metadata and mapping files are inconsistent ",
        "because the new workspace ",
        pb_4[1],
        " is present in the metadata file but not in the ",
        "mapping file."
      )
    )
  }

  data_file_mapping <- mapping |>
    dplyr::filter(!is.na(data_file)) |>
    dplyr::pull(data_file) |>
    unique()

  data_file_metadata <- character()
  if(!is.null(metadata$ws$old)){
    data_file_metadata <- sapply(
      X = metadata$series,
      FUN = function(x){
        return(x$name)
      }
    )
  }

  pb_5 <- setdiff(data_file_mapping, data_file_metadata)
  if(length(pb_5) != 0L){
    stop(
      paste0(
        "The metadata and mapping files are inconsistent ",
        "because the data_file ",
        pb_5[1],
        " is present in the mapping file but not in the ",
        "metadata file."
      )
    )
  }

  pb_6 <- setdiff(data_file_metadata, data_file_mapping)
  if(length(pb_6) != 0L){
    stop(
      paste0(
        "The metadata and mapping files are inconsistent ",
        "because the data_file ",
        pb_6[1],
        " is present in the metadata file but not in the ",
        "mapping file."
      )
    )
  }

  level_mapping <- mapping |>
    dplyr::filter(!is.na(level)) |>
    dplyr::pull(level) |>
    unique()
  
  label_levels <- data_levels |>
    dplyr::pull(level)

  pb_7 <- setdiff(level_mapping, label_levels)
  if(length(pb_7) != 0L){
    stop(
      paste0(
        "The levels and mapping files are inconsistent ",
        "because the level ",
        pb_7[1],
        " is present in the mapping file but not in the ",
        "level file."
      )
    )
  }

  pb_8 <- setdiff(label_levels, level_mapping)
  if(length(pb_8) != 0L){
    stop(
      paste0(
        "The level and mapping files are inconsistent ",
        "because the data_file ",
        pb_8[1],
        " is present in the level file but not in the ",
        "mapping file."
      )
    )
  }

  serie_group_mapping <- mapping |>
    dplyr::filter(!is.na(serie_group)) |>
    dplyr::pull(serie_group) |>
    unique()
  
  label_serie_group <- data_series_group |>
    dplyr::pull(serie_group)

  pb_9 <- setdiff(serie_group_mapping, label_serie_group)
  if(length(pb_9) != 0L){
    stop(
      paste0(
        "The serie_group and mapping files are inconsistent ",
        "because the serie_group ",
        pb_9[1],
        " is present in the mapping file but not in the ",
        "serie_group file."
      )
    )
  }

  pb_10 <- setdiff(label_serie_group, serie_group_mapping)
  if(length(pb_10) != 0L){
    stop(
      paste0(
        "The serie_group and mapping files are inconsistent ",
        "because the serie_group ",
        pb_10[1],
        " is present in the serie_group file but not in the ",
        "mapping file."
      )
    )
  }
  return(NULL)
}
