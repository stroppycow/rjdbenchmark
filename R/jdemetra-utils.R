#' Generic class for a JDemetra Workspace
#'
#' @slot object Workspace object
#' @slot version Version used
#'
methods::setClass(
  Class = "JDemetraWorkspace",
  slots = c(
    object = "jobjRef",
    version = "character"
  )
)

#' Checks if a series type exists in JDemetra+
#'
#' @param x character Series type to check
#'
#' @return logical TRUE if the type is valid, FALSE otherwise
#' @export
#'
#' @examples jdemetra_is_allowed_series_type(c("ycal", "sa"))
jdemetra_is_allowed_series_type <- function(x){
  if(!is.character(x)){
    stop(
      "The provided value for argument \"x\" is not of type character"
    )
  }
  
  valid_series_jdemetra <- c(
    "y", # Original series
    "y_f", # Forecasts of the original series
    "y_ef", # Standard errors of the forecasts of the original series
    "y_c", # Interpolated series
    "yc_f", # Forecasts of the interpolated series
    "yc_ef", # Standard errors of the forecasts of the interpolated series
    "y_lin", # Linearised series (not transformed)
    "l", # Linearised series (transformed)
    "ycal", # Series corrected for calendar effects
    "ycal_f", # Forecasts of the series corrected for calendar effects
    "l_f", # Forecasts of the linearised series
    "l_b", # Backcasts of the linearised series
    "t", # Trend (including deterministic effects)
    "t_f", # Forecasts of the trend
    "sa", # Seasonally adjusted series (including deterministic effects)
    'sa_f', # Forecasts of the seasonally adjusted series
    "s", # Seasonal component (including deterministic effects)
    "s_f", # Forecasts of the seasonal component
    "i", # Irregular component (including deterministic effects)
    "i_f", # Forecasts of the irregular component
    "det", # All deterministic effects
    "det_f", # Forecasts of the deterministic effects
    "cal", # Calendar effects
    "cal_f", # Forecasts of the calendar effects
    "tde", # Trading day effect
    "tde_f", # Forecasts of the trading day effect
    "mhe", # Moving holidays effects
    "mhe_f", # Forecasts of the moving holidays effects
    "ee", # Easter effect
    "ee_f", # Forecasts of the Easter effect
    "omhe", # Other moving holidays effects
    "omhe_f", # Forecasts of the other moving holidays effects
    "out", # All outliers effects
    "out_f", # Forecasts of all outliers effects
    "out_i", # Outliers effects related to irregular (AO, TC)
    "out_i_f", # Forecasts of outliers effects related to irregular (TC)
    "out_t", # Outliers effects related to trend (LS)
    "out_t_f", # Forecasts of outliers effects related to trend (LS)
    "out_s", # Outliers effects related to seasonal (SO),
    "out_s_f", # Forecasts of outliers effects related to seasonal (SO)
    "reg", # All other regression effects
    "reg_f", # Forecasts of all other regression effects
    "reg_i", # Regression effects related to irregular
    "reg_i_f", # Forecasts of regression effects related to irregular
    "reg_t", # Regression effects related to trend
    "reg_t_f", # Forecasts of regression effects related to trend
    "reg_s", # Regression effects related to seasonal
    "reg_s_f", # Forecasts of regression effects related to seasonal
    "reg_sa", # Regression effects related to seasonally adjusted (sa) series
    "reg_sa_f", # Forecasts of regression effects related to sa series
    "reg_y", # Separate regression effects
    "reg_y_f", # Forecasts of separate regression effects
    "fullresiduals" # Full residuals of the RegARIMA model
  )
  
  return(x %in% valid_series_jdemetra)
}

#' Loads a JDemetra+ workspace by detecting version 2 or 3 of JDemetra
#'
#' @param path JDemetra+ workspace
#'
#' @return rJava object if loaded via r
#' @export
#'
jdemetra_load_workspace <- function(path){
  workspace_v2 <- rlang::try_fetch(
    {
      RJDemetra::load_workspace(path)
    }, error = function(e){
      return(NULL)
    }
  )
  
  workspace_v3 <- rlang::try_fetch(
    {
      rjd3workspace::.jws_open(path)
    }, error = function(e){
      return(NULL)
    }
  )
  
  if(is.null(workspace_v2) & is.null(workspace_v3)){
    stop(
      paste0(
        "Unable to load workspace ",
        fs::path_abs(path)
      )
    )
  }
  
  if(!is.null(workspace_v2)){
    multiprocessing_V2 <- rlang::try_fetch(
      RJDemetra::get_all_objects(workspace_v2),
      error = function(e){
        return(NULL)
      }
    )
    nb_multipro_v2 <- rlang::try_fetch(
      length(multiprocessing_V2),
      error = function(e){
        return(0L)
      }
    )
  } else {
    nb_multipro_v2 <- 0L
  }
  
  nb_multipro_v2 <- 0L
  if(!is.null(workspace_v2)){
    nb_multipro_v2 <- rlang::try_fetch(
      {
        length(RJDemetra::get_all_objects(workspace_v2))
      },
      error = function(e){
        return(0L)
      }
    )
  }
  
  nb_multipro_v3 <- 0L
  if(!is.null(workspace_v3)){
    nb_multipro_v3 <- rlang::try_fetch(
      {
        rjd3workspace::.jws_sap_count(workspace_v3)
      },
      error = function(e){
        return(0L)
      }
    )
  }
  
  if(nb_multipro_v2  == 0L & nb_multipro_v3 == 0L){
    stop(paste0(
      "No preprocessing found in workspace ",
      fs::path_abs(path)
    ))
  }
  
  if(nb_multipro_v3  > 1L & nb_multipro_v3 > 1L){
    stop(paste0(
      "Multiple preprocessing instances found in workspace ",
      fs::path_abs(path)
    ))
  }
  
  nb_sa_items_v2 <- 0L
  if(nb_multipro_v2 == 1L){
    nb_sa_items_v2 <- rlang::try_fetch(
      {
        multipro_v2 <- RJDemetra::get_all_objects(workspace_v2)
        sa_items <- RJDemetra::get_all_objects(multipro_v2[[1L]])
        length(sa_items)
      }, error = function(e){
        return(0L)
      }
    )
  }
  
  nb_sa_items_v3 <- 0L
  if(nb_sa_items_v3 == 1L){
    nb_sa_items_v3 <- rlang::try_fetch(
      {
        multipro_v3 <- rjd3workspace::.jws_sap(workspace_v3, 1)
        rjd3workspace::.jsap_sa_count(multipro_v3)
      }, error = function(e){
        return(0L)
      }
    )
  }
  
  if(nb_sa_items_v2 > 0L){
    return(
      methods::new(
        "JDemetraWorkspace",
        object = workspace_v2,
        version = "2"
      )
    )
  } else {
    return(
      methods::new(
        "JDemetraWorkspace",
        object = workspace_v3,
        version = "3"
      )
    )
  }
}

#' Counts the number of SA Processing in a workspace
#'
#' @param workspace JDemetra+ workspace
#'
#' @return integer Number of SA Processing in the workspace
#' @export
#'
jdemetra_count_sa_processing <- function(workspace){
  if(!inherits(x = workspace, what = "JDemetraWorkspace")){
    workspace <- rlang::try_fetch(
      {
        jdemetra_load_workspace(workspace)
      }, error = function(e){
        rlang::abort(
          message = "Failed to load JDemetra+ workspace",
          parent = e
        )
      }
    )
  }
  if(workspace@version == "2"){
    multiprocessing <- RJDemetra::get_all_objects(workspace@object)
    return(length(multiprocessing))
  } else if(workspace@version == "3"){
    return(rjd3workspace::.jws_sap_count(workspace@object))
  } else {
    stop(paste0("Version ", workspace@version, " is not supported"))
  }
}


#' Retrieves a multiprocessing
#'
#' @param workspace JDemetra+ workspace
#' @param rank Rank of the multiprocessing to retrieve or NULL to ensure 
#' there is only one and retrieve it
#'
#' @return JDemetra multiprocessing
#' @export
#'
jdemetra_get_multiprocessing <- function(workspace, rank = NULL){
  if(!inherits(x = workspace, what = "JDemetraWorkspace")){
    workspace <- rlang::try_fetch(
      {
        jdemetra_load_workspace(workspace)
      }, error = function(e){
        rlang::abort(
          message = "Failed to load JDemetra+ workspace",
          parent = e
        )
      }
    )
  }
  nb_sa_processing <- jdemetra_count_sa_processing(workspace)
  if(is.null(rank)){
    if(nb_sa_processing == 0L){
      stop("No preprocessing found in the JDemetra+ workspace")
    } else if(nb_sa_processing > 1L){
      stop("Multiple preprocessings found in the JDemetra+ workspace")
    }
    r <- 1L
  } else if(rank > nb_sa_processing){
    stop(paste0("There are only ", as.character(nb_sa_processing), " multiprocessings."))
  } else {
    r <- rank
  }
  
  if(workspace@version == "2"){
    multiprocessing <- RJDemetra::get_all_objects(workspace@object)[[r]]
  } else if(workspace@version == "3"){
    multiprocessing <- rjd3workspace::.jws_sap(workspace@object, r)
  } else {
    stop(
      paste0(
        "Version ",
        workspace@version ,
        " of JDemetra+ is not supported."
      )
    )
  }
  return(multiprocessing)
}


#' Save a JDemetra workspace
#'
#' @param workspace JDemetra+ workspace
#' @param path Export file path
#'
#' @export
#'
jdemetra_save_workspace <- function(
    workspace,
    path
){
  if(workspace@version == "2"){
    RJDemetra::save_workspace(
      workspace = workspace@object,
      file = path
    )
  } else if(workspace@version == "3"){
    rjd3workspace::save_workspace(
      jws = workspace@object,
      file = path
    )
  } else {
    stop(
      paste0(
        "Version ",
        workspace@version ,
        " of JDemetra+ is not supported."
      )
    )
  }
  return(NULL)
}

#' Retrieves JDemetra SaItems as an R list of Java objects
#'
#' @param workspace JDemetra+ workspace
#'
#' @return List of SaItems
#' @export
#'
jdemetra_get_sa_items <- function(workspace){
  if(!inherits(x = workspace, what = "JDemetraWorkspace")){
    workspace <- rlang::try_fetch(
      {
        jdemetra_load_workspace(workspace)
      }, error = function(e){
        rlang::abort(
          message = "Failed to load JDemetra+ workspace",
          parent = e
        )
      }
    )
  }
  
  multiprocessing <- jdemetra_get_multiprocessing(
    workspace = workspace,
    rank = NULL
  )
  
  if(workspace@version == "2"){
    sa_items <- RJDemetra::get_all_objects(multiprocessing)
  } else if(workspace@version == "3"){
    sa_items <- lapply(
      X = seq.int(length.out = rjd3workspace::.jsap_sa_count(multiprocessing)),
      FUN = function(i){
        return(rjd3workspace::.jsap_sai(multiprocessing, i))
      }
    )
    names(sa_items) <- sapply(
      X = sa_items,
      FUN = function(sa_item){
        sa_item_name <- rJava::.jcall(
          obj = sa_item,
          returnSig = "Ljava/lang/String;",
          method = "getName"
        )
      }
    )
  } else {
    stop(
      paste0(
        "Version ",
        workspace@version ,
        " of JDemetra+ is not supported."
      )
    )
  }
  return(sa_items)
}


#' Returns all JDemetra SaItems names
#'
#' @param workspace JDemetra+ workspace
#'
#' @return character Vector of names
#' @export
#'
jdemetra_get_sa_names <- function(workspace){
  return(names(jdemetra_get_sa_items(workspace)))
}

#' Returns the method used for each SAItem in the workspace
#'
#' @param workspace JDemetra+ workspace
#'
#' @return Character vector of the method
#' @export
#'
jdemetra_get_sa_items_methods <- function(workspace){
  if(!inherits(x = workspace, what = "JDemetraWorkspace")){
    workspace <- rlang::try_fetch(
      {
        jdemetra_load_workspace(workspace)
      }, error = function(e){
        rlang::abort(
          message = "Failed to load JDemetra+ workspace",
          parent = e
        )
      }
    )
  }
  sa_items_before_ws <- jdemetra_get_sa_items(workspace)
  type_models <- sapply(
    X = sa_items_before_ws,
    FUN = function(sa_item){
      if(workspace@version == "2"){
        sa_definition <- rJava::.jcall(
          obj = sa_item,
          returnSig = "Ljd2/datatypes/sa/SaItemType;",
          method = "getSaDefinition"
        )
        
        if(!is.null(sa_definition)){
          estim_spec <- rJava::.jcall(
            obj = sa_definition,
            returnSig = "Lec/satoolkit/ISaSpecification;",
            method = "getEstimationSpec"
          )
          
          point_spec <- rJava::.jcall(
            obj = sa_definition,
            returnSig = "Lec/satoolkit/ISaSpecification;",
            method = "getPointSpec"
          )
        } else {
          estim_spec <- NULL
          point_spec <- NULL
        }
        
        estim_spec_str <- as.character(NA)
        if(!is.null(estim_spec)){
          estim_spec_str <- rJava::.jclass(estim_spec)
        }
        
        point_spec_str <- as.character(NA)
        if(!is.null(point_spec)){
          point_spec_str <- rJava::.jclass(point_spec)
        }
        spec_str <- c(estim_spec_str, point_spec_str)
        spec_str <- spec_str[!is.na(spec_str)]
        spec_str <- unique(spec_str)
        if(length(spec_str) != 1L){
          return(as.character(NA))
        } else {
          if(spec_str == "ec.satoolkit.x13.X13Specification"){
            return("X13")
          } else if(spec_str == "ec.satoolkit.tramoseats.TramoSeatsSpecification"){
            return("TS")
          } else {
            return(as.character(NA))
          }
        }
      } else if(workspace@version == "3"){
        sa_definition <- rJava::.jcall(
          obj = sa_item,
          returnSig = "Ljdplus/sa/base/api/SaDefinition;",
          method = "getDefinition"
        )
        
        if(!is.null(sa_definition)){
          estim_spec <- rJava::.jcall(
            obj = sa_definition,
            returnSig = "Ljdplus/sa/base/api/SaSpecification;",
            method = "getEstimationSpec"
          )
        } else {
          estim_spec <- NULL
        }
        
        sa_estimation <- rJava::.jcall(
          obj = sa_item,
          returnSig = "Ljdplus/sa/base/api/SaEstimation;",
          method = "getEstimation"
        )
        
        if(!is.null(sa_estimation)){
          point_spec <- rJava::.jcall(
            obj = sa_estimation,
            returnSig = "Ljdplus/sa/base/api/SaSpecification;",
            method = "getPointSpec"
          )
        } else {
          point_spec <- NULL
        }
        
        estim_spec_str <- as.character(NA)
        if(!is.null(estim_spec)){
          estim_spec_str <- rJava::.jclass(estim_spec)
        }
        
        point_spec_str <- as.character(NA)
        if(!is.null(point_spec)){
          point_spec_str <- rJava::.jclass(point_spec)
        }
        spec_str <- c(estim_spec_str, point_spec_str)
        spec_str <- spec_str[!is.na(spec_str)]
        spec_str <- unique(spec_str)
        if(length(spec_str) != 1L){
          return(as.character(NA))
        } else {
          if(spec_str == "jdplus.x13.base.api.x13.X13Spec"){
            return("X13")
          } else if(spec_str == "jdplus.tramoseats.base.api.tramoseats.TramoSeatsSpec"){
            return("TS")
          } else {
            return(as.character(NA))
          }
        }
      } else {
        stop(
          paste0(
            "Version ",
            workspace@version ,
            " of JDemetra+ is not supported."
          )
        )
      }
    }
  )
  names(type_models) <- names(sa_items_before_ws)
  return(type_models)
}
