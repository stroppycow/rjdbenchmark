#' Configure a JDemetra SaItem for standardizing raw series reading
#' using the JDemetra+ cruncher.
#' 
#' @param sa_item jobjRef Pointer to the SaItem object
#' @param path character(1) New file path for the data
#' @param rank integer(1) Series rank (i.e., column number in raw data)
#' @param jdemetra_version character(1) JDemetra version used
#'
#' @export
#'
jdemetra_configure_sa_item_before_crunch <- function(
    sa_item,
    path,
    rank,
    jdemetra_version
){
  if(jdemetra_version == "2"){
    # Retrieve the SaItem definition
    sa_definition <- rJava::.jcall(
      obj = sa_item,
      returnSig = "Ljd2/datatypes/sa/SaItemType;",
      method = "getSaDefinition"
    )
    
    # Retrieve the associated time series object
    ts <- rJava::.jcall(
      obj = sa_definition,
      returnSig = "Ljd2/datatypes/Ts;",
      method = "getTs"
    )
  } else if(jdemetra_version == "3"){
    # Retrieve the SaItem definition
    sa_definition <- rJava::.jcall(
      obj = sa_item,
      returnSig = "Ljdplus/sa/base/api/SaDefinition;",
      method = "getDefinition"
    )
    
    # Retrieve the associated time series objec
    ts <- rJava::.jcall(
      obj = sa_definition,
      returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
      method = "getTs"
    )
  }
  
  # Update the file path
  path_encoded <- utils::URLencode(
    fs::path_norm(fs::path_abs(fs::path(path))),
    reserved = TRUE
  )
  
  new_id <- paste0(
    "demetra://tsprovider/Txt/20111201/SERIES",
    "?",
    "cleanMissing=true",
    "&",
    "datePattern=dd%2FMM%2Fyyyy",
    "&",
    "delimiter=SEMICOLON",
    "&",
    paste0("file=", path_encoded),
    "&",
    "locale=en",
    "&",
    "numberPattern=#seriesIndex=",
    as.character(rank-1L)
  )
  
   # Update metadata stored as a hash table with keys: @id, @timestamp, and @source
  new_metadata <- rJava::.jnew(class = "java/util/HashMap")
  rJava::.jcall(
    obj = new_metadata,
    returnSig="Ljava/lang/Object;",
    method = "put",
    rJava::.jcast(
      rJava::.jnew("java/lang/String","@timestamp"),
      new.class = "java/lang/Object"
    ),
    rJava::.jcast(
      rJava::.jnew("java/lang/String",""),
      new.class = "java/lang/Object"
    )
  )
  
  rJava::.jcall(
    obj = new_metadata,returnSig="Ljava/lang/Object;",
    method = "put",
    rJava::.jcast(
      rJava::.jnew("java/lang/String","@source"),
      new.class = "java/lang/Object"
    ),
    rJava::.jcast(
      rJava::.jnew("java/lang/String","Txt"),
      new.class = "java/lang/Object"
    )
  )
  
  rJava::.jcall(
    obj = new_metadata,
    returnSig = "Ljava/lang/Object;",
    method = "put",
    rJava::.jcast(
      rJava::.jnew("java/lang/String", "@id"),
      new.class = "java/lang/Object"
    ),
    rJava::.jcast(
      rJava::.jnew("java/lang/String", new_id),
      new.class = "java/lang/Object"
    )
  )
  
  if(jdemetra_version == "2"){
    rJava::`.jfield<-`(
      o = ts,
      name = "metaData",
      value = rJava::.jcast(
        new_metadata,
        new.class = "java/util/Map"
      )
    )
  } else if(jdemetra_version == "3"){
    rJava::`.jfield<-`(
      o = ts,
      name = "meta",
      value = rJava::.jcast(
        new_metadata,
        new.class = "java/util/Map"
      )
    )
  }
}

#' Generation of a new JDemetra+ workspace with the same version of JDemetra
#' and the same method specifications, but using standardized raw data in txt format.
#'
#' @param workspace_jdemetra JDemetraWorkspace JDemetra workspace
#' @param raw_data_list list Named list of TimeseriesSet objects
#' containing raw time series data
#' @param dt_mappping_series data.table Mapping table between
#' the series grouping codes and the SaItem names in the workspace
#' @param old_new character(1) Indicates whether it is an old or new
#' workspace
#' @param ws_name character(1) Name of the JDemetra+ workspace
#' @param temp_folder character(1) Temporary folder for storing raw data,
#' JDematra+ workspaces and related outputs
#'
#' @return Returns a list containing the workspace file path and
#' its version
#' @export
#'
configure_single_jdemetra_ws_and_data <- function(
    workspace_jdemetra,
    raw_data_list,
    dt_mappping_series,
    old_new,
    ws_name,
    temp_folder
){
  # Create a directory to store input time series data for the cruncher
  path_dir_data <- fs::path(temp_folder, old_new, ws_name, "data")
  fs::dir_create(path_dir_data)

  # Create a directory for the combined workspace used by the cruncher
  path_dir_ws <- fs::path(temp_folder, old_new, ws_name, "ws_jd")
  fs::dir_create(path_dir_ws)

  path_output_ws <- fs::path(path_dir_ws, "temp_ws.xml")
  
  # Save data to CSV files
  lapply(
    X = dt_mappping_series |> dplyr::pull(data_file) |> unique(),
    FUN = function(data_path_name){
      data <- dt_mappping_series |>
        dplyr::filter(data_file == data_path_name) |>
        dplyr::left_join(
          y = {
            raw_data_list[[data_path_name]]@data |>
              tibble::as_tibble() |>
              dplyr::rename(serie_name_data_file = "code")
          },
          by = "serie_name_data_file"
        )
      
      pb <- data |>
        dplyr::filter(is.na(rang)) |>
        dplyr::pull(serie_name_data_file)
      
      if(length(pb) > 0){
        stop(
          paste0(
            "Impossible de trouver la série ",
            pb[1],
            " dans le fichier ",
            data_path_name
          )
        )
      }
      
      data_save <- data |>
        dplyr::select(serie_name_data_file, date, value) |>
        tidyr::pivot_wider(id_cols = date, names_from = serie_name_data_file) |>
        dplyr::select(
          c(
            "date",
            dt_mappping_series |>
              dplyr::filter(data_file == data_path_name) |>
              dplyr::arrange(rang) |>
              dplyr::pull(serie_name_data_file)
          )
        ) |>
        dplyr::mutate(date = format(date, "%d/%m/%Y"))
      
      data.table::fwrite(
        x = data.table::as.data.table(data_save),
        file = fs::path(
          path_dir_data,
          paste0(fs::path_ext_remove(data_path_name), ".csv")
        ),
        na = "",
        dec = ".",
        sep = ";",
        quote = FALSE,
        scipen = 50,
        row.names = FALSE
      )
    }
  ) |> invisible()
  
  # Select relevant SaItems from the workspace
  multipro_jdemetra <- jdemetra_get_multiprocessing(workspace_jdemetra)
  sa_items_init <- jdemetra_get_sa_items(workspace_jdemetra)
  
  # Check if all required SaItems exist
  pb <- setdiff(
    dt_mappping_series  |> dplyr::pull(sa_names_ws ),
    names(sa_items_init)
  )
  
  if(length(pb) > 0){
    stop(
      paste0(
        "Impossible de trouver le SaItem ",
        pb[1],
        " dans le workspace ",
        old_new,
        " ",
        dt_mappping_series |> dplyr::pull(ws_name ) |> unique()
      )
    )
  }
  
  sa_items <- sa_items_init[dt_mappping_series  |> dplyr::pull(sa_names_ws )]
  
  rank_sa_items <- sapply(
    X = names(sa_items),
    FUN = function(x){
      dt_mappping_series|>
        dplyr::filter(sa_names_ws == x) |>
        dplyr::pull(rang)
    }
  )
  
  path_sa_items <- sapply(
    X = names(sa_items),
    FUN = function(x){
      fs::path(
        path_dir_data,
        paste0(fs::path_ext_remove(
          {
            dt_mappping_series|>
              dplyr::filter(sa_names_ws == x) |>
              dplyr::pull(data_file)
          }
        ), ".csv")
      )
    }
  )
  
  # Remove unnecessary SaItems from the workspace
  to_rm <- lapply(
    X = sa_items_init[setdiff(names(sa_items_init), names(sa_items))],
    FUN = function(x){
      methods::new(
        Class = "jobjRef",
        jobj = x@jobj,
        jclass = x@jclass
      )
    }
  )

  if(length(to_rm) > 0L){
    to_rm <- rJava::.jcall(
      obj = "java/util/Arrays",
      returnSig = "Ljava/util/List;",
      method = "asList",
      rJava::.jarray(to_rm)
    )
    
    to_rm <- rJava::.jcast(
      obj = to_rm,
      new.class = "java/util/Collection"
    )
    
    rJava::.jcall(
      obj = rJava::.jcall(
        obj = multipro_jdemetra,
        returnSig = "Ljava/util/List;",
        method = "getItems"
      ),
      returnSig = "Z",
      method = "removeAll",
      to_rm
    )
  }
  
  # Rename the SAProcessing 
  rJava::`.jfield<-`(
    o = methods::new(
      Class = "jobjRef",
      jobj = multipro_jdemetra@jobj,
      jclass = multipro_jdemetra@jclass
    ),
    name = "name",
    value = "SaProcessing-1"
  )
  
  # Update metadata and data for each retained SaItem
  new_sa_items <- mapply(
    FUN = jdemetra_configure_sa_item_before_crunch,
    sa_items,
    path_sa_items,
    rank_sa_items,
    workspace_jdemetra@version
  )
  
  # Save the new workspace
  jdemetra_save_workspace(
    workspace = workspace_jdemetra,
    path = path_output_ws
  )

  return(
    list(
      path = path_output_ws,
      version = workspace_jdemetra@version
    )
  )
}
