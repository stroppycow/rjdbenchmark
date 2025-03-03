#' Retrieving trading days regressors used for JDemetra+
#'
#' @param workspace JDemetra+ workspace
#' (object of class workspace or path to the file to load
#'  and interpret)
#'
#' @return data.frame Table containing all trading days present in the
#' workspace. The table contains 2 columns:
#' - `series`: Name of the series in the workspace
#' - `td`: Name of the regressor
#'
jdemetra_list_tradingdays <- function(workspace){
  if(!inherits(x = workspace, what = "JDemetraWorkspace")){
    workspace <- rlang::try_fetch(
      {
        jdemetra_load_workspace(workspace)
      }, error = function(e){
        rlang::abort(
          message =  "Unable to load the JDemetra+ workspace",
          parent = e
        )
      }
    )
  }

  nb_sa_processing <- jdemetra_count_sa_processing(workspace)

  if(nb_sa_processing != 1L){
    stop("The JDemetra+ workspace contains multiple multiprocessing")
  }

  # Retrieving SaItems: deseasonalization configuration and
  # estimated parameters for each series
  # Iterating over them
  sa_items <- jdemetra_get_sa_items(workspace)
  trading_days <- list()
  if(workspace@version == "2"){
    trading_days  <- lapply(X = sa_items, FUN = function(sa_item){
      # Retrieving the saDefinition attribute
      sa_definition <- rJava::.jcall(
        obj = sa_item,
        returnSig = "Ljd2/datatypes/sa/SaItemType;",
        method = "getSaDefinition"
      )

      estim_spec <- rJava::.jcall(
        obj = sa_definition,
        returnSig = "Lec/satoolkit/ISaSpecification;",
        method = "getEstimationSpec"
      )

      ts <- rJava::.jcall(
        obj = sa_definition,
        returnSig = "Ljd2/datatypes/Ts;",
        method = "getTs"
      )

      sa_item_name <- rJava::.jcall(
        obj = ts,
        returnSig = "Ljava/lang/String;",
        method = "getName"
      )

      if(rJava::.jclass(estim_spec) == "ec.satoolkit.x13.X13Specification"){
         # Retrieving the configuration and pre-adjustment model
        reg_arima <- rJava::.jcall(
          obj = estim_spec,
          returnSig = "Lec/tstoolkit/modelling/arima/x13/RegArimaSpecification;",
          method = "getRegArimaSpecification"
        )

        # Retrieving the pre-adjustment model
        regression <- rJava::.jcall(
          obj = reg_arima,
          returnSig = "Lec/tstoolkit/modelling/arima/x13/RegressionSpec;",
          method = "getRegression"
        )

        td_spec <- rJava::.jcall(
          obj = regression,
          returnSig = "Lec/tstoolkit/modelling/arima/x13/TradingDaysSpec;",
          method = "getTradingDays"
        )

        user_variables <- rJava::.jcall(
          obj = td_spec,
          returnSig = "[Ljava/lang/String;",
          method = "getUserVariables"
        )
      } else if(rJava::.jclass(estim_spec) == "ec.satoolkit.tramoseats.TramoSeatsSpecification"){
        tramo_spec <- rJava::.jcall(
          obj = estim_spec,
          returnSig = "Lec/tstoolkit/modelling/arima/tramo/TramoSpecification;",
          method = "getTramoSpecification"
        )

        regression <- rJava::.jcall(
          obj = tramo_spec,
          returnSig = "Lec/tstoolkit/modelling/arima/tramo/RegressionSpec;",
          method = "getRegression"
        )

        calendar_spec <- rJava::.jcall(
          obj = regression,
          returnSig = "Lec/tstoolkit/modelling/arima/tramo/CalendarSpec;",
          method = "getCalendar"
        )

        td_spec <- rJava::.jcall(
          obj = calendar_spec,
          returnSig = "Lec/tstoolkit/modelling/arima/tramo/TradingDaysSpec;",
          method = "getTradingDays"
        )

        user_variables <- rJava::.jcall(
          obj = td_spec,
          returnSig = "[Ljava/lang/String;",
          method = "getUserVariables"
        )
      } else {
        stop(
          paste0(
            "The class ",
            rJava::.jclass(estim_spec),
            " is not supported."
          )
        )
      }
      if(length(user_variables) == 0){
        return(
          data.frame(
            series = character(),
            td = character()
          )
        )
      }

      return(
        data.frame(
          series = rep(sa_item_name, length.out=length(user_variables)),
          td = user_variables
        )
      )
    })
  } else if(workspace@version == "3"){
    trading_days <- lapply(
        X = sa_items ,
        FUN = function(sa_item){
          # Retrieving the saDefinition attribute
          sa_definition <- rJava::.jcall(
            obj = sa_item,
            returnSig = "Ljdplus/sa/base/api/SaDefinition;",
            method = "getDefinition"
          )

          sa_item_name <- rJava::.jcall(
            obj = sa_item,
            returnSig = "Ljava/lang/String;",
            method = "getName"
          )

          estim_spec <- rJava::.jcall(
            obj = sa_definition,
            returnSig = "Ljdplus/sa/base/api/SaSpecification;",
            method = "getEstimationSpec"
          )

          if(rJava::.jclass(estim_spec) == "jdplus.tramoseats.base.api.tramoseats.TramoSeatsSpec"){
            tramo_spec <- rJava::.jcall(
              obj = estim_spec,
              returnSig = "Ljdplus/tramoseats/base/api/tramo/TramoSpec;",
              method = "getTramo"
            )

            # Retrieving the pre-adjustment model
            regression <- rJava::.jcall(
              obj = tramo_spec,
              returnSig = "Ljdplus/tramoseats/base/api/tramo/RegressionSpec;",
              method = "getRegression"
            )

            calendar_spec <- rJava::.jcall(
              obj = regression,
              returnSig = "Ljdplus/tramoseats/base/api/tramo/CalendarSpec;",
              method = "getCalendar"
            )

            td_spec <- rJava::.jcall(
              obj = calendar_spec,
              returnSig = "Ljdplus/tramoseats/base/api/tramo/TradingDaysSpec;",
              method = "getTradingDays"
            )

            user_variables <- rJava::.jcall(
              obj = td_spec,
              returnSig = "[Ljava/lang/String;",
              method = "getUserVariables"
            )
          } else if(rJava::.jclass(estim_spec) == "jdplus.x13.base.api.x13.X13Spec"){
            x13_spec <- rJava::.jcall(
              obj = estim_spec,
              returnSig = "Ljdplus/x13/base/api/regarima/RegArimaSpec;",
              method = "getRegArima"
            )

            regression <- rJava::.jcall(
              obj = x13_spec,
              returnSig = "Ljdplus/x13/base/api/regarima/RegressionSpec;",
              method = "getRegression"
            )

            td_spec <- rJava::.jcall(
              obj = regression,
              returnSig = "Ljdplus/x13/base/api/regarima/TradingDaysSpec;",
              method = "getTradingDays"
            )

            user_variables <- rJava::.jcall(
              obj = td_spec,
              returnSig = "[Ljava/lang/String;",
              method = "getUserVariables"
            )
          } else {
            stop(
              paste0(
                "The class ",
                rJava::.jclass(estim_spec),
                " is not supported."
              )
            )
          }

          if(length(user_variables) == 0){
            return(
              data.frame(
                series = character(),
                td = character()
              )
            )
          }
          
          return(
            data.frame(
              series = rep(sa_item_name, length.out=length(user_variables)),
              td = user_variables
            )
          )
        }
      )
  } else {
    stop(paste0("The version ", workspace@version, " is not supported"))
  }

  # Formatting as data.frame
  if(length(trading_days) == 0L){
    return(data.frame(
      series = character(0L),
      td = character(0L)
    ))
  } else {
     x <- as.data.frame(do.call(what = rbind, args = trading_days))
     row.names(x) <- as.character(seq.int(length.out = nrow(x)))
     return(x)
  }
}
