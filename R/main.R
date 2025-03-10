#' Comparison of two JDemetra+ Workspaces and
#' generation of a static site for analyzing differences
#'
#' @param temp_folder character(1) Temporary folder for computation
#' @param input_folder character(1) Folder containing configuration files,
#' raw series, and JDemetra+ workspaces
#' @param cruncher_bin_directory character Path to the JDemetra cruncher(s)
#' @param preview logical(1) Indicates whether a preview of the static site should be generated
#' 
#' @export
#'
render_jdemetra_benchmark <- function(
  temp_folder = "./tmp",
  input_folder = "./data",
  cruncher_bin_directory = NULL,
  preview = FALSE
){
  logger::log_info("Start of workspsace comparison")
  if(is.null(cruncher_bin_directory)){
    cruncher_bin_directory <- character()
    if(Sys.getenv("JWSACRUNCHER_BIN_DIRECTORY_V2") != ""){
      cruncher_bin_directory["2"] <- Sys.getenv("JWSACRUNCHER_BIN_DIRECTORY_V2")
    }

    if(Sys.getenv("JWSACRUNCHER_BIN_DIRECTORY_V3") != ""){
      cruncher_bin_directory["3"] <- Sys.getenv("JWSACRUNCHER_BIN_DIRECTORY_V3")
    }
  }
  if(!is.character(cruncher_bin_directory)){
    stop(
      "'cruncher_bin_directory' is not a character vector"
    )
  }

  if(length(cruncher_bin_directory) == 0L){
    stop(
      "'cruncher_bin_directory' is empty"
    )
  }

  if(sum(is.na(cruncher_bin_directory)) != 0L){
    stop(
      "'cruncher_bin_directory' must not contain NA"
    )
  }

  versions_jdemetra <- rlang::try_fetch(
    sapply(
      X = cruncher_bin_directory,
      FUN = jdemetra_get_cruncher_version
    ), error = function(e){
      stop(
        paste0(
          "'cruncher_bin_directory' contain invalid bin directory"
        )
      )
    }
  )

  logger::log_info("Loading the mapping file")
  full_mapping <- read_mapping_file(
    path = fs::path(input_folder, "mapping.csv")
  )
  logger::log_info("Loading the metadata file")
  metadata <- read_metadata_file(
    path = fs::path(input_folder, "metadata.yaml")
  )
  logger::log_info("Loading the calendar regression labels file")
  data_labels_td <- read_trading_days_label_file(
    path = fs::path(input_folder, "trading_days.csv")
  )
  logger::log_info("Loading the time series group labels file")
  data_labels_serie_group <- read_serie_group_label_file(
    path = fs::path(input_folder, "serie_group.csv")
  )
  logger::log_info("Loading the nomenclature level labels file")
  data_labels_levels <- read_levels_label_file(
    path = fs::path(input_folder, "levels.csv")
  )
  logger::log_info("Check consistency between different configuration files")
  check_consistency_config(
    mapping = full_mapping,
    metadata = metadata,
    data_levels = data_labels_levels,
    data_series_group = data_labels_serie_group
  )

  logger::log_info("Reading raw time series")
  raw_data_list <- lapply(
    X = metadata$series,
    FUN = function(x){
      path_serie <-  fs::path(input_folder, "series", x$name)
      logger::log_info("Reading the file {path_serie}")
      args_func <- x
      args_func$name <- NULL
      args_func$file <- path_serie
      do.call(
        what = read_timeseries_set_file,
        args = args_func
      )
    }
  )

  names(raw_data_list) <- sapply(
    X = metadata$series,
    FUN = function(x){return(x$name)}
  )

  if(fs::dir_exists(temp_folder)){
    rlang::try_fetch(
      fs::dir_delete(temp_folder),
      error = function(e){
        rlang::abort(
          message = "Unable to initialise temporary folder",
          parent = e
        )
      }
    )
  }
  rlang::try_fetch(
    fs::dir_create(temp_folder),
    error = function(e){
      rlang::abort(
        message = "Unable to initialise temporary folder",
        parent = e
      )
    }
  )

  logger::log_info("Configuring old JDemetra+ workspaces")
  config_old <- lapply(
    X = metadata$ws$old,
    FUN = function(x){
      ws_name <- x$name
      logger::log_info("Reading the old JDemetra+ Workspace {ws_name}")
      configure_jdemetra_and_run_cruncher(
        ws_name = ws_name,
        raw_data_list = raw_data_list,
        full_mapping = full_mapping,
        old_new = "old",
        cruncher_bin_directory = cruncher_bin_directory,
        refresh = x$refresh,
        temp_folder = temp_folder,
        input_folder = input_folder
      )
    }
  )

  logger::log_info("Configuring new JDemetra+ workspaces")
  config_new <- lapply(
    X = metadata$ws$new,
    FUN = function(x){
      ws_name <- x$name
      logger::log_info("Reading the new JDemetra+ Workspace {ws_name}")
      configure_jdemetra_and_run_cruncher(
        ws_name = ws_name,
        raw_data_list = raw_data_list,
        full_mapping = full_mapping,
        old_new = "new",
        cruncher_bin_directory = cruncher_bin_directory,
        refresh = x$refresh,
        temp_folder = temp_folder,
        input_folder = input_folder
      )
    }
  )

  fs::dir_create(
    path = fs::path(temp_folder, "output")
  )

  fichiers_series <- fs::dir_ls(
    path = temp_folder,
    type = "file",
    recurse = TRUE,
    regexp = "/(old|new)/.+/analyse/series.parquet"
  )

  # Copy time series
  logger::log_info("Copy time series")
  arrow::open_dataset(
    sources = fichiers_series
  ) |>
    dplyr::mutate(
      file = arrow::add_filename(),
      type_ws = stringr::str_sub(stringr::str_remove(
        string = file,
        pattern = "/[^/]+/analyse/series[.]parquet$"
      ), start = -3, end = -1),
      ws_name = stringr::str_remove(stringr::str_remove(
        string = file,
        pattern = "/analyse/series[.]parquet$"
      ), pattern = ".+/")
    ) |>
    dplyr::rename(series = "code") |>
    dplyr::select(type_ws, ws_name, component, series, date, value) |>
    arrow::write_parquet(fs::path(temp_folder, "output", "series.parquet"))

  # Copy trading days
  logger::log_info("Copy trading days")
  fichiers_td <- fs::dir_ls(
    path = temp_folder,
    type = "file",
    recurse = TRUE,
    regexp = "/(old|new)/.+/analyse/trading_days.csv"
  )

  arrow::open_dataset(
    sources = fichiers_td,
    format = "text",
    delimiter = ","
  ) |>
    dplyr::mutate(
      file = arrow::add_filename(),
      type_ws = stringr::str_sub(stringr::str_remove(
        string = file,
        pattern = "/[^/]+/analyse/trading_days[.]csv$"
      ), start = -3, end = -1),
      ws_name = stringr::str_remove(stringr::str_remove(
        string = file,
        pattern = "/analyse/trading_days[.]csv$"
      ), pattern = ".+/")
    ) |>
    dplyr::select(type_ws, ws_name, series, td) |>
    arrow::write_parquet(fs::path(temp_folder, "output", "trading_days.parquet"))

  # Copy outliers
  logger::log_info("Copy outliers")
  fichiers_outliers <- fs::dir_ls(
    path = temp_folder,
    type = "file",
    recurse = TRUE,
    regexp = "/(old|new)/.+/analyse/outliers.csv"
  )

  arrow::open_dataset(
    sources = fichiers_outliers,
    format = "text",
    delimiter = ","
  ) |>
    dplyr::mutate(
      file = arrow::add_filename(),
      type_ws = stringr::str_sub(stringr::str_remove(
        string = file,
        pattern = "/[^/]+/analyse/outliers[.]csv$"
      ), start = -3, end = -1),
      ws_name = stringr::str_remove(stringr::str_remove(
        string = file,
        pattern = "/analyse/outliers[.]csv$"
      ), pattern = ".+/")
    ) |>
    dplyr::select(type_ws, ws_name, series, date_outlier, type_outlier) |>
    arrow::write_parquet(fs::path(temp_folder, "output", "outliers.parquet"))

  # Copy infos
  logger::log_info("Copy infos")
  fichiers_infos <- fs::dir_ls(
    path = temp_folder,
    type = "file",
    recurse = TRUE,
    regexp = "/(old|new)/.+/analyse/infos.csv"
  )

  arrow::open_dataset(
    sources = fichiers_infos,
    format = "text",
    delimiter = ",",
    skip = 1L,
    schema = arrow::schema(
      series = arrow::string(),
      period = arrow::int64(),
      serie_span_start = arrow::string(),
      serie_span_end = arrow::string(),
      serie_span_n = arrow::int64(),
      serie_span_missing = arrow::int64(),
      estimation_span_start = arrow::string(),
      estimation_span_end = arrow::string(),
      estimation_span_n = arrow::int64(),
      estimation_span_missing = arrow::int64(),
      p = arrow::int64(),
      d = arrow::int64(),
      q = arrow::int64(),
      bp = arrow::int64(),
      bd = arrow::int64(),
      bq = arrow::int64(),
      arima_model = arrow::string(),
      log = arrow::int64(),
      easter = arrow::int64(),
      pct_outliers = arrow::float64(),
      pct_outliers_quality = arrow::string(),
      f_residuals_sa_on_i_pvalue = arrow::float64(),
      qs_residuals_sa_on_i_pvalue = arrow::float64(),
      f_residuals_sa_on_sa_pvalue = arrow::float64(),
      qs_residuals_sa_on_sa_pvalue = arrow::float64(),
      f_residuals_td_on_sa_pvalue = arrow::float64(),
      f_residuals_td_on_i_pvalue = arrow::float64(),
      f_residuals_sa_on_i_modality = arrow::string(),
      qs_residuals_sa_on_i_modality = arrow::string(),
      f_residuals_sa_on_sa_modality = arrow::string(),
      qs_residuals_sa_on_sa_modality = arrow::string(),
      f_residuals_td_on_sa_modality = arrow::string(),
      f_residuals_td_on_i_modality = arrow::string(),
      q_stat = arrow::float64(),
      q_m2_stat = arrow::float64(),
      q_stat_modality = arrow::string(),
      q_m2_stat_modality = arrow::string(),
      fcast_outsample_mean_pvalue = arrow::float64(),
      fcast_outsample_variance_pvalue = arrow::float64(),
      fcast_outsample_mean_modality = arrow::string(),
      fcast_outsample_variance_modality = arrow::string(),
      residuals_skewness_pvalue = arrow::float64(),
      residuals_kurtosis_pvalue = arrow::float64(),
      residuals_doornikhansen_pvalue = arrow::float64(),
      residuals_lb_pvalue = arrow::float64(),
      residuals_lb2_pvalue = arrow::float64(),
      residuals_skewness_modality = arrow::string(),
      residuals_kurtosis_modality = arrow::string(),
      residuals_doornikhansen_modality = arrow::string(),
      residuals_lb_modality = arrow::string(),
      residuals_lb2_modality = arrow::string(),
      m7 = arrow::float64(),
      m7_quality = arrow::string(),
      summary = arrow::string(),
      version =  arrow::string(),
      refresh_policy = arrow::string()
    )
  ) |>
    dplyr::mutate(
      file = arrow::add_filename(),
      type_ws = stringr::str_sub(stringr::str_remove(
        string = file,
        pattern = "/[^/]+/analyse/infos[.]csv$"
      ), start = -3, end = -1),
      ws_name = stringr::str_remove(stringr::str_remove(
        string = file,
        pattern = "/analyse/infos[.]csv$"
      ), pattern = ".+/")
    ) |>
    dplyr::select(-file) |>
    arrow::write_parquet(fs::path(temp_folder, "output", "infos.parquet"))

  # Copy mapping.csv
  logger::log_info("Copy mapping.csv")
  fs::file_copy(
    path = fs::path(input_folder, "mapping.csv"),
    new_path = fs::path(temp_folder, "output", "mapping.csv")
  )

  # Copy levels.csv
  logger::log_info("Copy levels.csv")
  fs::file_copy(
    path = fs::path(input_folder, "levels.csv"),
    new_path = fs::path(temp_folder, "output", "levels.csv")
  )

  # Copy serie_group.csv
  logger::log_info("Copy serie_group.csv")
  fs::file_copy(
    path = fs::path(input_folder, "serie_group.csv"),
    new_path = fs::path(temp_folder, "output", "serie_group.csv")
  )

  # Copy trading_days.csv
  logger::log_info("Copy trading_days.csv")
  fs::file_copy(
    path = fs::path(input_folder, "trading_days.csv"),
    new_path = fs::path(temp_folder, "output", "trading_days.csv")
  )

  logger::log_info("Static site generation with quarto")
  if(preview){
    quarto::quarto_preview(
      file = fs::path_abs(".")
    )
  } else {
    quarto::quarto_render(
      input = fs::path_abs("."),
      as_job = FALSE
    )
  }
  return(NULL)
}
