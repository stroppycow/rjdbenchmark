#' Configure a temporary JDemetra workspace for calculation, then run the JDemetra cruncher
#' and format the outputs
#'
#' @param ws_name character(1) Name of the JDemetra+ workspace
#' @param raw_data_list TimeseriesSet raw time series
#' @param full_mapping data.table Mapping table between levels and
#' series codes and the name of the JDemetra+ SaItem
#' @param old_new character(1) Indicates if it is an old or new workspace
#' @param cruncher_bin_directory character(1) Path to the JDemetra+ cruncher
#' @param refresh character(1) Refresh policy in JDemetra+
#' @param temp_folder character(1) Temporary folder for storing raw data,
#' JDematra+ workspaces and related outputs
#' @param input_folder character(1) Input folder
#' @export
#'
configure_jdemetra_and_run_cruncher <- function(
    ws_name,
    raw_data_list,
    full_mapping,
    old_new,
    cruncher_bin_directory,
    refresh,
    temp_folder,
    input_folder
){
  ws_path <- fs::path(
    input_folder,
    paste0(old_new, "_ws"),
    paste0(ws_name, ".xml")
  )
  
  workspace_jdemetra <- jdemetra_load_workspace(ws_path)

  dt_mappping_series <- tibble::tibble(
    ws_name = ws_name,
    sa_names_ws = jdemetra_get_sa_names(workspace_jdemetra)
  ) |>
    dplyr::left_join(
      y = {
        full_mapping |>
          dplyr::rename(
            ws_name = paste0("ws_", old_new),
            sa_names_ws = paste0("sa_item_", old_new)
          ) |>
          dplyr::select(ws_name, sa_names_ws, data_file, serie_name_data_file)
      },
      by = c("ws_name", "sa_names_ws")
    ) |>
    dplyr::group_by(data_file) |>
    dplyr::reframe(ws_name, sa_names_ws, serie_name_data_file, rang = dplyr::row_number())
  
  res_conf <- configure_single_jdemetra_ws_and_data(
    workspace_jdemetra = workspace_jdemetra,
    raw_data_list = raw_data_list,
    dt_mappping_series = dt_mappping_series,
    old_new = old_new,
    ws_name = ws_name,
    temp_folder = temp_folder
  )
  
  frequency <- sapply(
    X = dt_mappping_series |> dplyr::pull(data_file) |> unique(),
    FUN = function(x){
      raw_data_list[[x]]@frequency
    }
  ) |> unique()
  
  if(length(frequency) > 1 ){
    stop("Multiple frequencies detected in raw data")
  }
  
  
  work_ws_path <- fs::path_abs(res_conf$path)
  log_file <- fs::path(fs::path_dir(work_ws_path), "temp_ws.log")
  
  # Check if all seasonal adjustments are of type X13 or not
  sa_items_methods <- jdemetra_get_sa_items_methods(fs::path_abs(work_ws_path))
  
  add_extra_data <- FALSE
  default_matrix_item <- character()
  tsmatrix_series <- c("y", "sa", "t", "ycal", "s", "i", "cal")
  if(length(sa_items_methods[is.na(sa_items_methods)]) == 0L){
    if(!any(sa_items_methods != "X13")){
      add_extra_data <- TRUE
      tsmatrix_series <- union(
        tsmatrix_series,
        c("y", "sa", "t", "ycal", "s", "i", "cal")
      )
      default_matrix_item <- c(
        "period",
        "span.start",
        "span.end",
        "span.n",
        "span.missing",
        "espan.start",
        "espan.end",
        "espan.n",
        "espan.missing",
        "regression.espan.start",
        "regression.espan.end",
        "regression.espan.n",
        "regression.espan.missing",
        "log",
        "adjust",
        "regression.lp",
        "regression.ntd",
        "regression.nmh",
        "regression.td-derived",
        "regression.td-ftest",
        "regression.easter",
        "regression.nout",
        "regression.noutao",
        "regression.noutls",
        "regression.nouttc",
        "regression.noutso",
        "regression.td(*)",
        "regression.out(*)",
        "regression.user(*)",
        "likelihood.neffectiveobs",
        "likelihood.np",
        "likelihood.logvalue",
        "likelihood.adjustedlogvalue",
        "likelihood.ssqerr",
        "likelihood.aic",
        "likelihood.aicc",
        "likelihood.bic",
        "likelihood.bicc",
        "residuals.ser",
        "residuals.ser-ml",
        "residuals.mean",
        "residuals.skewness:3",
        "residuals.kurtosis:3",
        "residuals.doornikhansen:3",
        "residuals.dh:3",
        "residuals.lb:3", 
        "residuals.lb2:3",
        "residuals.seaslb",
        "residuals.bp",
        "residuals.bp2",
        "residuals.seasbp",
        "residuals.nudruns",
        "residuals.ludruns",
        "residuals.nruns",
        "residuals.lruns",
        "arima",
        "arima.mean",
        "arima.p",
        "arima.d",
        "arima.q",
        "arima.bp",
        "arima.bd",
        "arima.bq",
        "arima.phi(*)",
        "arima.bphi(*)",
        "arima.th(*)",
        "arima.bth(*)",
        "decomposition.seasonality",
        "decomposition.parameters_cutoff",
        "decomposition.model_changed",
        "decomposition.tvar-estimator",
        "decomposition.tvar-estimate",
        "decomposition.tvar-pvalue",
        "decomposition.savar-estimator",
        "decomposition.savar-estimate",
        "decomposition.savar-pvalue",
        "decomposition.svar-estimator",
        "decomposition.svar-estimate",
        "decomposition.svar-pvalue",
        "decomposition.ivar-estimator",
        "decomposition.ivar-estimate",
        "decomposition.ivar-pvalue",
        "decomposition.tscorr-estimator",
        "decomposition.tscorr-estimate",
        "decomposition.tscorr-pvalue",
        "decomposition.ticorr-estimator",
        "decomposition.ticorr-estimate",
        "decomposition.ticorr-pvalue",
        "decomposition.sicorr-estimator",
        "decomposition.sicorr-estimate",
        "decomposition.sicorr-pvalue",
        "decomposition.ar_root(*)",
        "decomposition.ma_root(*)", "method",
        "variancedecomposition.cycle",
        "variancedecomposition.seasonality",
        "variancedecomposition.irregular",
        "variancedecomposition.tdh",
        "variancedecomposition.others",
        "variancedecomposition.total",
        "diagnostics.logstat",
        "diagnostics.levelstat",
        "diagnostics.fcast-insample-mean",
        "diagnostics.fcast-outsample-mean",
        "diagnostics.fcast-outsample-variance",
        "diagnostics.seas-lin-f",
        "diagnostics.seas-lin-qs",
        "diagnostics.seas-lin-kw",
        "diagnostics.seas-lin-friedman",
        "diagnostics.seas-lin-periodogram",
        "diagnostics.seas-lin-spectralpeaks",
        "diagnostics.seas-si-combined",
        "diagnostics.seas-si-evolutive",
        "diagnostics.seas-si-stable",
        "diagnostics.seas-res-f",
        "diagnostics.seas-res-qs",
        "diagnostics.seas-res-kw",
        "diagnostics.seas-res-friedman",
        "diagnostics.seas-res-periodogram",
        "diagnostics.seas-res-spectralpeaks",
        "diagnostics.seas-res-combined",
        "diagnostics.seas-res-combined3",
        "diagnostics.seas-res-evolutive",
        "diagnostics.seas-res-stable",
        "diagnostics.seas-i-f",
        "diagnostics.seas-i-f:2",
        "diagnostics.seas-i-qs",
        "diagnostics.seas-i-qs:2",
        "diagnostics.seas-i-kw",
        "diagnostics.seas-i-periodogram",
        "diagnostics.seas-i-spectralpeaks",
        "diagnostics.seas-i-combined",
        "diagnostics.seas-i-combined3",
        "diagnostics.seas-i-evolutive",
        "diagnostics.seas-i-stable",
        "diagnostics.seas-sa-f",
        "diagnostics.seas-sa-f:2",
        "diagnostics.seas-sa-qs",
        "diagnostics.seas-sa-qs:2",
        "diagnostics.seas-sa-kw",
        "diagnostics.seas-sa-friedman",
        "diagnostics.seas-sa-periodogram",
        "diagnostics.seas-sa-spectralpeaks",
        "diagnostics.seas-sa-combined",
        "diagnostics.seas-sa-combined3",
        "diagnostics.seas-sa-evolutive",
        "diagnostics.seas-sa-stable", 
        "diagnostics.seas-sa-ac1",
        "diagnostics.td-sa-all",
        "diagnostics.td-sa-last",
        "diagnostics.td-sa-last:2",
        "diagnostics.td-i-all",
        "diagnostics.td-i-last",
        "diagnostics.td-i-last:2",
        "diagnostics.td-res-all",
        "diagnostics.td-res-last",
        "diagnostics.ic-ratio-henderson",
        "diagnostics.ic-ratio",
        "diagnostics.msr-global",
        "diagnostics.msr(*)",
        "decomposition.trendfilter",
        "decomposition.seasfilter",
        "m-statistics.m1",
        "m-statistics.m2",
        "m-statistics.m3",
        "m-statistics.m4",
        "m-statistics.m5",
        "m-statistics.m6",
        "m-statistics.m7",
        "m-statistics.m8",
        "m-statistics.m9",
        "m-statistics.m10",
        "m-statistics.m11",
        "m-statistics.q",
        "m-statistics.q-m2",
        "diagnostics.basic checks.definition:2",
        "diagnostics.basic checks.annual totals:2",
        "diagnostics.visual spectral analysis.spectral seas peaks",
        "diagnostics.visual spectral analysis.spectral td peaks",
        "diagnostics.regarima residuals.normality:2",
        "diagnostics.regarima residuals.independence:2",
        "diagnostics.regarima residuals.spectral td peaks:2",
        "diagnostics.regarima residuals.spectral seas peaks:2",
        "diagnostics.outliers.number of outliers:2",
        "diagnostics.out-of-sample.mean:2",
        "diagnostics.out-of-sample.mse:2",
        "diagnostics.m-statistics.q:2",
        "diagnostics.m-statistics.q-m2:2",
        "diagnostics.seats.seas variance:2",
        "diagnostics.seats.irregular variance:2",
        "diagnostics.seats.seas/irr cross-correlation:2",
        "diagnostics.residual seasonality tests.qs test on sa:2",
        "diagnostics.residual seasonality tests.qs test on i:2",
        "diagnostics.residual seasonality tests.f-test on sa (seasonal dummies):2",
        "diagnostics.residual seasonality tests.f-test on i (seasonal dummies):2",
        "diagnostics.combined seasonality test.combined seasonality test on sa:2",
        "diagnostics.combined seasonality test.combined seasonality test on sa (last 3 years):2",
        "diagnostics.combined seasonality test.combined seasonality test on irregular:2",
        "diagnostics.residual trading days tests.f-test on sa (td):2",
        "diagnostics.residual trading days tests.f-test on i (td):2",
        "diagnostics.quality",
        "quality.summary"
      )
    }
  }
  
  output_folder_ws <- fs::path_abs(fs::path(fs::path_dir(work_ws_path), "output"))
  
  v3 <- FALSE
  if(res_conf$version == "3"){
    v3 <- TRUE
  }

  # Get cruncher version
  jd_version_str <- jdemetra_get_cruncher_version(
    cruncher_bin_directory[[res_conf$version]]
  )
  
  # Execution du cruncher
  rjwsacruncher::cruncher_and_param(
    workspace = work_ws_path,
    output = output_folder_ws,
    rename_multi_documents = FALSE,
    csv_layout = "htable",
    csv_separator = ";",
    delete_existing_file = FALSE,
    log_file = log_file,
    cruncher_bin_directory = cruncher_bin_directory[[res_conf$version]],
    matrix_item = default_matrix_item,
    tsmatrix_series  = tsmatrix_series,
    policy = refresh,
    v3 = v3
  )
  
  if(add_extra_data){
    path_demetra_m <- fs::path(
      output_folder_ws,
      "SaProcessing-1",
      "demetra_m.csv"
    )
    
    qr <- jdemetra_load_demetram(path_demetra_m)
    data_series <- extract_all_series_jdemetra(
      series = tsmatrix_series,
      output_folder_ws = fs::path(output_folder_ws, "SaProcessing-1"),
      frequency = frequency
    )
    
    trading_days <-  jdemetra_list_tradingdays(fs::path_abs(work_ws_path))
    
    path_analyse <- fs::path(fs::path_dir(fs::path_dir(work_ws_path)), "analyse")
    
    fs::dir_create(
      path = path_analyse
    )

    infos <- data.table::as.data.table(qr$infos)
    infos[, version := jd_version_str]
    infos[, refresh_policy := refresh]
    data.table::fwrite(
      x = infos,
      file = fs::path(path_analyse, "infos.csv"),
      sep = ",",
      na = "",
      row.names = FALSE
    )
    
    outliers <- data.table::as.data.table(qr$outliers)
    data.table::setcolorder(
      x = outliers,
      neworder = c("series", "date_outlier", "type_outlier")
    )
    data.table::setkey(x = outliers, series, date_outlier, type_outlier)
    
    data.table::fwrite(
      x = outliers,
      file = fs::path(path_analyse, "outliers.csv"),
      sep = ",",
      na = "",
      row.names = FALSE
    )
    
    trading_days <- data.table::as.data.table(trading_days)
    trading_days[, td := stringr::str_split_i(
      string = td,
      pattern = "[.]",
      i = 2
    )]
    
    data.table::setcolorder(
      x = trading_days,
      neworder = c("series", "td")
    )
    data.table::setkey(x = trading_days, series)
    
    data.table::fwrite(
      x = trading_days,
      file = fs::path(path_analyse, "trading_days.csv"),
      sep = ",",
      na = "",
      row.names = FALSE
    )
    
    data_series <- data.table::rbindlist(lapply(
      X = names(data_series),
      FUN = function(x, data){
        return(data_series[[x]]@data[,.(component=x, code, date, value)])
      },
      data_series
    ))
    data.table::setkey(x = data_series, component, code)
    
    arrow::write_parquet(
      x = data_series,
      compression = "snappy",
      sink = fs::path(path_analyse, "series.parquet"),
    )
  }
  return(NULL)
}

#' Configure a temporary JDemetra workspace for calculation,
#' then run the JDemetra cruncher
#' and format the outputs
#'
#' @param ws_name character(1) Name of the JDemetra+ workspace
#' @param raw_data_list TimeseriesSet raw time series
#' 
#' @export
#'
jdemetra_get_cruncher_version <- function(path){
  cmd <- fs::path(path, "jwsacruncher") |>
    fs::path_abs()
  res <- rlang::try_fetch(
    system2(
      command = cmd,
      args = "--version",
      stdout = TRUE
    ),
    error = function(e){
      rlang::abort(
        message = paste0(
          "Unable to extract the ",
          "JDemetra+ cruncher version"
        ),
        parent = e
      )
    }
  )
  return(res[1])
}
