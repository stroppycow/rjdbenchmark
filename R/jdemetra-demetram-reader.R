#' Loading a demetra_m file output from JDemetra+
#' and extracting relevant statistics
#'
#' @param path_demetra_m Path to the demetra_m CVS file
#'
#' @return list of data.frames with outliers, main diagnostics,
#' and calendar regressors
jdemetra_load_demetram  <- function(path_demetra_m){
  demetra_m <- data.table::fread(
    file = path_demetra_m,
    sep = ";",
    colClasses = "character"
  )

  demetra_m_output <- demetra_m[,1, with = FALSE]
  data.table::setnames(
    x = demetra_m_output,
    new = "series"
  )

  demetra_m_output <- cbind(
    demetra_m_output,
    .jdemetra_extract_from_demetram_span(demetra_m),
    .jdemetra_extract_from_demetram_arima(demetra_m),
    .jdemetra_extract_from_demetram_log(demetra_m),
    .jdemetra_extract_from_demetram_easter(demetra_m),
    .jdemetra_extract_from_demetram_pct_ouliers(demetra_m),
    .jdemetra_extract_from_demetram_test_residuals_sa_td(demetra_m),
    .jdemetra_extract_from_demetram_stats_q(demetra_m),
    .jdemetra_extract_from_demetram_oos_tests(demetra_m),
    .jdemetra_extract_from_demetram_normality_tests(demetra_m),
    .jdemetra_extract_from_demetram_m7(demetra_m),
    .jdemetra_extract_global_quality(demetra_m)
  )

  cols_td <- which(
    stringr::str_detect(
      string = colnames(demetra_m),
      pattern = "^td[(][0-9]+[)]$"
    )
  )

  data_td <- data.table::rbindlist(apply(
    X = demetra_m[,c(1,cols_td), with= FALSE],
    MARGIN = 1,
    FUN = function(y){
      x <- y[2:length(y)]
      x <- x[!is.na(x)]
      x <- x[x!=""]
      if(length(x)>0){
        return(data.frame(series=unname(y[1]), td=unname(x)))
      } else {
        return(data.frame(series=character(0), td=character(0)))
      }
      paste0(x, collapse = "+")
    }
  ))

  cols_out <- which(
    stringr::str_detect(
      string = colnames(demetra_m), pattern = "^out[(][0-9]+[)]$"
    )
  )

  data_outlier <- data.table::rbindlist(apply(
    X = demetra_m[,c(1,2,cols_out), with = FALSE],
    MARGIN = 1,
    FUN = function(y){
      x <- y[3:length(y)]
      x <- x[!is.na(x)]
      x <- x[x!=""]
      if(length(x)>0){
        return(data.frame(
          series= unname(y[1]),
          frequency = as.integer(unname(y[2])),
          outlier = unname(x)
        ))
      } else {
        return(data.frame(
          series = character(0),
          frequency = integer(0),
          outlier = character(0)
        ))
      }
      paste0(x, collapse = "+")
    }
  ))

  data_outlier[,
    type_outlier := stringr::str_extract(
      string = outlier,
      pattern = "^([A-Za-z0-9]+) ",
      group = 1
    )
  ]

  data_outlier[
    , date_outlier := as.character(NA)
  ]

  data_outlier[
    frequency == 12L, date_outlier := format(as.Date(paste0(stringr::str_pad(
      string = stringr::str_extract(
        string = outlier,
        pattern = "[(]([0-9]+-[0-9]+)[)]$",
        group = 1
      ),
      side = "left",
      pad = "0",
      width = 7
    ),"-01"), tryFormats  = "%m-%Y-%d"), "%Y-%m")
  ]

  data_outlier[
    frequency == 4L, date_outlier := paste0(
      string = stringr::str_extract(
        string = outlier,
        pattern = "[(](([0-9]+)-([0-9]+))[)]$",
        group = 3
      ),
      "-Q",
      string = stringr::str_extract(
        string = chaine,
        pattern = "[(](([0-9]+)-([0-9]+))[)]$",
        group = 2
      )
    )
  ]

  data_outlier[, outlier:= NULL]
  data_outlier[, frequency := NULL]

  return(list(
    infos = demetra_m_output,
    outliers = data_outlier,
    trading_days = data_td
  ))
}


#' Récupérer les seuils des indicateurs qualité pour JDemetra
#'
#' @return Liste des différents seuils
#' @export
#'
.jdemetra_get_thresholds_quality <- function() {

  default_thresholds <- list(
    qs_residual_sa_on_sa = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf),
    qs_residual_sa_on_i = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf),

    f_residual_sa_on_sa = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf),
    f_residual_sa_on_i = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf),

    f_residual_td_on_sa = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf),
    f_residual_td_on_i = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf),

    residuals_independency = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
    residuals_homoskedasticity = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),

    residuals_skewness = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
    residuals_kurtosis = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
    residuals_normality = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),

    oos_mean = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
    oos_mse = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),

    m7 = c(Good = 1, Bad = 2, Severe = Inf),
    q = c(Good = 1, Bad = Inf),
    q_m2 = c(Good = 1, Bad = Inf),
    pct_outliers = c(Good = 0.03, Uncertain = 0.05, Bad = Inf)
  )

  return(default_thresholds)
}

.jdemetra_apply_threshold <- function(x, threshold){
  threshold <- sort(threshold)
  Reduce(
    f = function(x1, x2){
      return(ifelse(x2$data < x2$level, x2$label, x1))
    },
    init = rep("", length(x)),
    x = lapply(
      X = rev(seq.int(along.with = threshold)),
      FUN = function(i){
        list(
          data = x,
          label = names(threshold)[i],
          level = threshold[i]
        )
      }
    )
  )
}

.jdemetra_extract_from_demetram_arima <- function(demetra_m){
  if(ncol(demetra_m) < 6L){
    stop("Impossible d'extraire le modèle ARIMA")
  }

  p_rank <- which(sapply(
    X = 1:(ncol(demetra_m) - 6L),
    FUN = function(i){
      if(all(colnames(demetra_m)[i:(i+5L)] == c("p", "d", "q", "bp", "bd", "bq"))){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  ))[1]

  if(length(p_rank) != 1L){
    stop("Impossible d'extraire le modèle ARIMA")
  }

  indices_columns_arima <- seq.int(from = p_rank, length.out = 6)
  demetra_m_arima <- demetra_m[, indices_columns_arima, with = FALSE]
  demetra_m_arima[, p := ifelse(
    test = stringr::str_detect(string = p, pattern = "[0-9]+"),
    yes = as.integer(p),
    no = as.integer(NA)
  )]
  demetra_m_arima[, d := ifelse(
    test = stringr::str_detect(string = d, pattern = "[0-9]+"),
    yes = as.integer(d),
    no = as.integer(NA)
  )]
  demetra_m_arima[, q := ifelse(
    test = stringr::str_detect(string = q, pattern = "[0-9]+"),
    yes = as.integer(q),
    no = as.integer(NA)
  )]
  demetra_m_arima[, bp := ifelse(
    test = stringr::str_detect(string = bp, pattern = "[0-9]+"),
    yes = as.integer(bp),
    no = as.integer(NA)
  )]
  demetra_m_arima[, bd := ifelse(
    test = stringr::str_detect(string = bd, pattern = "[0-9]+"),
    yes = as.integer(bd),
    no = as.integer(NA)
  )]
  demetra_m_arima[, bq := ifelse(
    test = stringr::str_detect(string = bq, pattern = "[0-9]+"),
    yes = as.integer(bq),
    no = as.integer(NA)
  )]

  demetra_m_arima[, arima_model := ifelse(
      test = (
        is.na(p) | is.na(d) | is.na(q) | is.na(bp) | is.na(bd) | is.na(bq)
      ),
      yes = as.character(NA),
      no = paste0("(", p, ",", d, ",", q, ")(", bp, ",", bd, ",", bq, ")")
    )
  ]
  return(demetra_m_arima)
}

.jdemetra_extract_global_quality <- function(demetra_m){
  j_quality <- which(colnames(demetra_m) == "quality")
  j_summary <- which(colnames(demetra_m) == "summary")

  if(length(j_quality) == 0L){
    stop("Impossible d'extraire la qualité globale")
  } else {
    j_quality <- max(j_quality)
  }
  if(length(j_summary) == 0L){
    stop("Impossible d'extraire la qualité globale")
  } else {
    j_summary <- max(j_summary)
  }

  demetra_m_quality <- demetra_m[,
    c(j_summary,j_quality), with = FALSE
  ]

  data.table::setnames(
    x = demetra_m_quality,
    new = c(
      "summary",
      "quality"
    ),
    old = colnames(demetra_m_quality)
  )

  demetra_m_quality[,
    summary := ifelse(
      test = summary == "",
      yes = quality,
      no = summary
    )
  ]
  demetra_m_quality[, quality := NULL]
  return(demetra_m_quality)
}

.jdemetra_extract_from_demetram_normality_tests <- function(demetra_m){
  j_skewness <- which(colnames(demetra_m) == "skewness")
  j_kurtosis <- which(colnames(demetra_m) == "kurtosis")
  j_doornikhansen <- which(colnames(demetra_m) == "doornikhansen")
  j_dh <- which(colnames(demetra_m) == "dh")
  j_lb <- which(colnames(demetra_m) == "lb")
  j_lb2 <- which(colnames(demetra_m) == "lb2")

  if(length(j_skewness) != 1L){
    stop("Impossible to extract statistics skewness")
  }
  if(length(j_kurtosis) != 1L){
    stop("Impossible to extract statistics kurtosis")
  }
  if(length(j_doornikhansen) != 1L){
    stop("Impossible to extract statistics doornikhansen")
  }
  if(length(j_dh) != 1L){
    stop("Impossible to extract statistics dh")
  }
  if(length(j_lb) != 1L){
    stop("Impossible to extract statistics lb")
  }
  if(length(j_lb2) != 1L){
    stop("Impossible to extract statistics lb2")
  }

  gap <- j_kurtosis - j_skewness -1L

  if(j_skewness+ gap > ncol(demetra_m)){
    stop("Impossible to extract statistics skewness")
  }


  if(!any(demetra_m[,j_skewness+gap, with = FALSE] != "")){
    gap <- gap -1L
  }

  if(j_kurtosis+gap > ncol(demetra_m)){
    stop("Impossible to extract statistics kurtosis")
  }
  if(j_doornikhansen+gap > ncol(demetra_m)){
    stop("Impossible to extract statistics doornikhansen")
  }
  if(j_dh+gap > ncol(demetra_m)){
    stop("Impossible to extract statistics dh")
  }
  if(j_lb+gap > ncol(demetra_m)){
    stop("Impossible to extract statistics lb")
  }
  if(j_lb2+gap > ncol(demetra_m)){
    stop("Impossible to extract statistics lb2")
  }
  demetra_m_normality_tests <- demetra_m[
    ,c(
      j_skewness+gap,
      j_kurtosis+gap,
      j_doornikhansen+gap,
      j_dh+gap,
      j_lb+gap,
      j_lb2+gap
    ), with = FALSE
  ]

  data.table::setnames(
    x = demetra_m_normality_tests,
    new = c(
      "residuals_skewness_pvalue",
      "residuals_kurtosis_pvalue",
      "residuals_doornikhansen_pvalue",
      "residuals_dh_pvalue",
      "residuals_lb_pvalue",
      "residuals_lb2_pvalue"
    ),
    old = colnames(demetra_m_normality_tests)
  )

  demetra_m_normality_tests[,
    residuals_doornikhansen_pvalue := ifelse(
      test = residuals_doornikhansen_pvalue == "",
      yes = residuals_dh_pvalue,
      no = residuals_doornikhansen_pvalue
    )
  ]

  demetra_m_normality_tests[,residuals_dh_pvalue := NULL]

  demetra_m_normality_tests <- demetra_m_normality_tests[,
    lapply(
      X = .SD,
      FUN = function(x){
        as.numeric(stringr::str_replace(
          string = x,
          pattern = "[,]",
          replacement = "."
        ))
      }
    ),
    .SDcols = colnames(demetra_m_normality_tests)
  ]

  demetra_m_normality_tests[,
    residuals_skewness_modality := .jdemetra_apply_threshold(
      x = residuals_skewness_pvalue,
      threshold = .jdemetra_get_thresholds_quality()[["residuals_skewness"]]
    )
  ]

  demetra_m_normality_tests[,
    residuals_kurtosis_modality := .jdemetra_apply_threshold(
      x = residuals_kurtosis_pvalue,
      threshold = .jdemetra_get_thresholds_quality()[["residuals_kurtosis"]]
    )
  ]

  demetra_m_normality_tests[,
    residuals_doornikhansen_modality := .jdemetra_apply_threshold(
      x = residuals_doornikhansen_pvalue,
      threshold = .jdemetra_get_thresholds_quality()[["residuals_normality"]]
    )
  ]

  demetra_m_normality_tests[,
    residuals_lb_modality := .jdemetra_apply_threshold(
      x = residuals_lb_pvalue,
      threshold = .jdemetra_get_thresholds_quality()[["residuals_independency"]]
    )
  ]

  demetra_m_normality_tests[,
    residuals_lb2_modality := .jdemetra_apply_threshold(
      x = residuals_lb2_pvalue,
      threshold = .jdemetra_get_thresholds_quality()[["residuals_homoskedasticity"]]
    )
  ]
  return(demetra_m_normality_tests)
}


.jdemetra_extract_from_demetram_oos_tests <- function(demetra_m){
  j_fcast_outsample_mean <- which(colnames(demetra_m) == "fcast-outsample-mean")
  j_fcast_outsample_variance <- which(colnames(demetra_m) == "fcast-outsample-variance")

  if(length(j_fcast_outsample_mean) != 1L){
    stop("Impossible to extract statistics fcast-outsample-mean")
  }
  if(length(j_fcast_outsample_variance) != 1L){
    stop("Impossible to extract statistics fcast-outsample-variance")
  }

  demetra_m_outsample <- demetra_m[
    ,c(
      j_fcast_outsample_mean,
      j_fcast_outsample_variance
    ), with = FALSE
  ]

  data.table::setnames(
    x = demetra_m_outsample,
    new = c(
      "fcast_outsample_mean_pvalue",
      "fcast_outsample_variance_pvalue"
    ),
    old = colnames(demetra_m_outsample)
  )

  demetra_m_outsample <- demetra_m_outsample[,
    lapply(
      X = .SD,
      FUN = function(x){
        as.numeric(stringr::str_replace(
          string = x,
          pattern = "[,]",
          replacement = "."
        ))
      }
    ),
    .SDcols = colnames(demetra_m_outsample)
  ]

  demetra_m_outsample[,
    fcast_outsample_mean_modality := .jdemetra_apply_threshold(
      x = fcast_outsample_mean_pvalue,
      threshold = .jdemetra_get_thresholds_quality()[["oos_mean"]]
    )
  ]
  demetra_m_outsample[,
    fcast_outsample_variance_modality := .jdemetra_apply_threshold(
      x = fcast_outsample_variance_pvalue,
      threshold = .jdemetra_get_thresholds_quality()[["oos_mse"]]
    )
  ]

  return(demetra_m_outsample)
}

.jdemetra_extract_from_demetram_stats_q <- function(demetra_m) {
  if(ncol(demetra_m) < 2L){
    stop("Impossible to extract statistics Q et Q-m2")
  }

  q_rank <- which(sapply(
    X = 1:(ncol(demetra_m) - 2L),
    FUN = function(i){
      if(all(colnames(demetra_m)[i:(i+1L)] == c("q", "q-m2"))){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  ))[1]

  if(length(q_rank) != 1L){
    stop("Impossible to extract statistics Q et Q-m2")
  }

  indices_columns_qstat <- seq.int(from = q_rank, length.out = 2L)
  demetra_m_qstat <- demetra_m[, indices_columns_qstat, with = FALSE]

  data.table::setnames(
    x = demetra_m_qstat,
    new = c("q_stat", "q_m2_stat"),
    old = colnames(demetra_m_qstat)
  )

  demetra_m_qstat <- demetra_m_qstat[,
    lapply(
      X = .SD,
      FUN = function(x){
        as.numeric(stringr::str_replace(
          string = x,
          pattern = "[,]",
          replacement = "."
        ))
      }
    ),
    .SDcols = colnames(demetra_m_qstat)
  ]

  demetra_m_qstat[,
    q_stat_modality := .jdemetra_apply_threshold(
      x = q_stat,
      threshold = .jdemetra_get_thresholds_quality()[["q"]]
    )
  ]

  demetra_m_qstat[,
    q_m2_stat_modality := .jdemetra_apply_threshold(
      x = q_m2_stat,
      threshold = .jdemetra_get_thresholds_quality()[["q_m2"]]
    )
  ]
  return(demetra_m_qstat)
}

.jdemetra_extract_from_demetram_log <- function(demetra_m){
  j_log <- which(colnames(demetra_m) == "log")

  if(length(j_log) != 1L){
    stop("Impossible to extract statistics log")
  }

  demetra_m_log <- demetra_m[,j_log, with = FALSE]
  demetra_m_log[, log := as.integer(log)]
  return(demetra_m_log)
}


.jdemetra_extract_from_demetram_easter<- function(demetra_m){
  j_easter <- which(colnames(demetra_m) == "easter")

  if(length(j_easter) != 1L){
    stop("Impossible to extract statistics easter")
  }

  demetra_m_easter <- demetra_m[,j_easter, with = FALSE]
  demetra_m_easter[, easter := as.integer(easter != "")]
  return(demetra_m_easter)
}


.jdemetra_extract_from_demetram_span <- function(demetra_m) {
  j_period <- which(colnames(demetra_m) == "period")
  j_start <- which(colnames(demetra_m) == "start")
  j_end <- which(colnames(demetra_m) == "end")
  j_n <- which(colnames(demetra_m) == "n")
  j_missing <- which(colnames(demetra_m) == "missing")

  if(length(j_period) != 1L){
    stop("Impossible to extract statistics start")
  }
  if(length(j_start) != 3L){
    stop("Impossible to extract statistics start")
  }
  if(length(j_end) != 3L){
    stop("Impossible to extract statistics end")
  }
  if(length(j_n) != 3L){
    stop("Impossible to extract statistics n")
  }
  if(length(j_missing) != 3L){
    stop("Impossible to extract statistics missing")
  }

  demetra_m_span <- demetra_m[
    ,c(
      j_period,
      j_start[1],
      j_end[1],
      j_n[1],
      j_missing[1],
      j_start[2],
      j_end[2],
      j_n[2],
      j_missing[2],
      j_start[3],
      j_end[3],
      j_n[3],
      j_missing[3]
    ), with = FALSE
  ]

  data.table::setnames(
    x = demetra_m_span,
    new = c(
      "period",
      "serie_span_start",
      "serie_span_end",
      "serie_span_n",
      "serie_span_missing",
      "estimation_span_start",
      "estimation_span_end",
      "estimation_span_n",
      "estimation_span_missing",
      "estimation_span_start2",
      "estimation_span_end2",
      "estimation_span_n2",
      "estimation_span_missing2"
    )
  )

  demetra_m_span[,
    estimation_span_start := ifelse(
      test = estimation_span_start == "",
      yes = estimation_span_start2,
      no = estimation_span_start
    )
  ]
  demetra_m_span[,
    estimation_span_end := ifelse(
      test = estimation_span_end == "",
      yes = estimation_span_end2,
      no = estimation_span_end
    )
  ]
  demetra_m_span[,
    estimation_span_n := ifelse(
      test = estimation_span_n == "",
      yes = estimation_span_n2,
      no = estimation_span_n
    )
  ]
  demetra_m_span[,
    estimation_span_missing := ifelse(
      test = estimation_span_missing == "",
      yes = estimation_span_missing2,
      no = estimation_span_missing
    )
  ]

  demetra_m_span[,estimation_span_start2:=NULL]
  demetra_m_span[,estimation_span_end2:=NULL]
  demetra_m_span[,estimation_span_n2:=NULL]
  demetra_m_span[,estimation_span_missing2:=NULL]
  demetra_m_span[,
      serie_span_start := as.Date(x = serie_span_start, format = "%Y-%m-%d")
  ]
  demetra_m_span[,
    serie_span_end := as.Date(x = serie_span_end, format = "%Y-%m-%d")
  ]
  demetra_m_span[,
    estimation_span_start := as.Date(x = estimation_span_start, format = "%Y-%m-%d")
  ]
  demetra_m_span[,
    estimation_span_end := as.Date(x = estimation_span_end, format = "%Y-%m-%d")
  ]
  demetra_m_span[,
    serie_span_n := as.integer(serie_span_n)
  ]
  demetra_m_span[,
    serie_span_missing := as.integer(serie_span_missing)
  ]
  demetra_m_span[,
    estimation_span_n := as.integer(estimation_span_n)
  ]
  demetra_m_span[,
    estimation_span_missing := as.integer(estimation_span_missing)
  ]

  demetra_m_span[,
    period := as.integer(period)
  ]

  return(demetra_m_span)
}

.jdemetra_extract_from_demetram_test_residuals_sa_td <- function(demetra_m){
  j_seas_i_f <- which(colnames(demetra_m) == "seas-i-f")
  j_seas_i_qs <- which(colnames(demetra_m) == "seas-i-qs")
  j_seas_sa_f <- which(colnames(demetra_m) == "seas-sa-f")
  j_seas_sa_qs <- which(colnames(demetra_m) == "seas-sa-qs")
  j_td_sa_last<- which(colnames(demetra_m) == "td-sa-last")
  j_td_i_last <- which(colnames(demetra_m) == "td-i-last")

  if(length(j_seas_i_f) != 2L){
    stop("Impossible to extract statistics seas-i-f")
  }
  if(length(j_seas_i_qs) != 2L){
    stop("Impossible to extract statistics seas-i-qs")
  }
  if(length(j_seas_sa_f) != 2L){
    stop("Impossible to extract statistics seas-sa-f")
  }
  if(length(j_seas_sa_qs) != 2L){
    stop("Impossible to extract statistics seas-sa-qs")
  }
  if(length(j_td_sa_last) != 2L){
    stop("Impossible to extract statistics td-sa-last")
  }
  if(length(j_td_i_last) != 2L){
    stop("Impossible to extract statistics td-i-last")
  }

  demetram_test_residuals_sa_td <-  demetra_m[
    ,c(
      j_seas_i_f[1],
      j_seas_i_f[2]+1L,
      j_seas_i_qs[1],
      j_seas_i_qs[2]+1L,
      j_seas_sa_f[1],
      j_seas_sa_f[2]+1L,
      j_seas_sa_qs[1],
      j_seas_sa_qs[2]+1L,
      j_td_sa_last[1],
      j_td_sa_last[2]+1L,
      j_td_i_last[1],
      j_td_i_last[2]+1L
    ), with = FALSE
  ]

  data.table::setnames(
    x = demetram_test_residuals_sa_td,
    new = c(
      "f_residuals_sa_on_i_pvalue",
      "f_residuals_sa_on_i_pvalue2",
      "qs_residuals_sa_on_i_pvalue",
      "qs_residuals_sa_on_i_pvalue2",
      "f_residuals_sa_on_sa_pvalue",
      "f_residuals_sa_on_sa_pvalue2",
      "qs_residuals_sa_on_sa_pvalue",
      "qs_residuals_sa_on_sa_pvalue2",
      "f_residuals_td_on_sa_pvalue",
      "f_residuals_td_on_sa_pvalue2",
      "f_residuals_td_on_i_pvalue",
      "f_residuals_td_on_i_pvalue2"
    )
  )

  demetram_test_residuals_sa_td[,
    f_residuals_sa_on_i_pvalue := ifelse(
      test = f_residuals_sa_on_i_pvalue2 == "",
      yes = f_residuals_sa_on_i_pvalue,
      no = f_residuals_sa_on_i_pvalue2
    )
  ]
  demetram_test_residuals_sa_td[,
    qs_residuals_sa_on_i_pvalue := ifelse(
      test = qs_residuals_sa_on_i_pvalue2 == "",
      yes = qs_residuals_sa_on_i_pvalue,
      no = qs_residuals_sa_on_i_pvalue2
    )
  ]
  demetram_test_residuals_sa_td[,
    f_residuals_sa_on_sa_pvalue := ifelse(
      test = f_residuals_sa_on_sa_pvalue2 == "",
      yes = f_residuals_sa_on_sa_pvalue,
      no = f_residuals_sa_on_sa_pvalue2
    )
  ]
  demetram_test_residuals_sa_td[,
    qs_residuals_sa_on_sa_pvalue := ifelse(
      test = qs_residuals_sa_on_sa_pvalue2 == "",
      yes = qs_residuals_sa_on_sa_pvalue,
      no = qs_residuals_sa_on_sa_pvalue2
    )
  ]
  demetram_test_residuals_sa_td[,
    f_residuals_td_on_sa_pvalue := ifelse(
      test = f_residuals_td_on_sa_pvalue2 == "",
      yes = f_residuals_td_on_sa_pvalue,
      no = f_residuals_td_on_sa_pvalue2
    )
  ]
  demetram_test_residuals_sa_td[,
    f_residuals_td_on_i_pvalue := ifelse(
      test = f_residuals_td_on_i_pvalue2 == "",
      yes = f_residuals_td_on_i_pvalue,
      no = f_residuals_td_on_i_pvalue2
    )
  ]

  demetram_test_residuals_sa_td[, f_residuals_sa_on_i_pvalue2 := NULL]
  demetram_test_residuals_sa_td[, qs_residuals_sa_on_i_pvalue2 := NULL]
  demetram_test_residuals_sa_td[, f_residuals_sa_on_sa_pvalue2 := NULL]
  demetram_test_residuals_sa_td[, qs_residuals_sa_on_sa_pvalue2 := NULL]
  demetram_test_residuals_sa_td[, f_residuals_td_on_sa_pvalue2 := NULL]
  demetram_test_residuals_sa_td[, f_residuals_td_on_i_pvalue2 := NULL]


  demetram_test_residuals_sa_td <- demetram_test_residuals_sa_td[,
    lapply(
      X = .SD,
      FUN = function(x){
        as.numeric(stringr::str_replace(
          string = x,
          pattern = "[,]",
          replacement = "."
        ))
      }
    ),
    .SDcols = colnames(demetram_test_residuals_sa_td)
  ]

  thresholds <- .jdemetra_get_thresholds_quality()

  demetram_test_residuals_sa_td[,
    f_residuals_sa_on_i_modality := .jdemetra_apply_threshold(
      x = f_residuals_sa_on_i_pvalue,
      threshold = thresholds[["f_residual_sa_on_i"]]
    )
  ]
  demetram_test_residuals_sa_td[,
    qs_residuals_sa_on_i_modality := .jdemetra_apply_threshold(
      x = qs_residuals_sa_on_i_pvalue,
      threshold = thresholds[["qs_residual_sa_on_i"]]
    )
  ]
  demetram_test_residuals_sa_td[,
    f_residuals_sa_on_sa_modality := .jdemetra_apply_threshold(
      x = f_residuals_sa_on_sa_pvalue,
      threshold = thresholds[["f_residual_sa_on_sa"]]
    )
  ]
  demetram_test_residuals_sa_td[,
    qs_residuals_sa_on_sa_modality := .jdemetra_apply_threshold(
      x = qs_residuals_sa_on_sa_pvalue,
      threshold = thresholds[["qs_residual_sa_on_sa"]]
    )
  ]
  demetram_test_residuals_sa_td[,
    f_residuals_td_on_sa_modality := .jdemetra_apply_threshold(
      x = f_residuals_td_on_sa_pvalue,
      threshold = thresholds[["f_residual_td_on_sa"]]
    )
  ]
  demetram_test_residuals_sa_td[,
    f_residuals_td_on_i_modality := .jdemetra_apply_threshold(
      x = f_residuals_td_on_i_pvalue,
      threshold = thresholds[["f_residual_td_on_i"]]
    )
  ]

  return(demetram_test_residuals_sa_td)

}

.jdemetra_extract_from_demetram_pct_ouliers <- function(demetra_m){
  j_n <- which(colnames(demetra_m) == "n")
  j_nout <- which(colnames(demetra_m) == "nout")

  if(length(j_n) != 3L){
    stop("Impossible to extract statistics n")
  }
  if(length(j_nout) != 1L){
    stop("Impossible to extract statistics nout")
  }

  demetra_m_pct_outliers <- demetra_m[,c(j_n[1], j_nout), with = FALSE]
  demetra_m_pct_outliers[, pct_outliers := as.integer(nout)/as.integer(n)]
  demetra_m_pct_outliers[, pct_outliers_quality := .jdemetra_apply_threshold(
    x = pct_outliers,
    threshold = .jdemetra_get_thresholds_quality()[["pct_outliers"]]
  )]
  demetra_m_pct_outliers[,n:=NULL]
  demetra_m_pct_outliers[,nout:=NULL]

  return(demetra_m_pct_outliers)
}

.jdemetra_extract_from_demetram_m7 <- function(demetra_m){
  j_m7 <- which(colnames(demetra_m) == "m7")


  if(length(j_m7) != 1L){
    stop("Impossible to extract statistics m7")
  }

  demetra_m_m7 <- demetra_m[,j_m7, with = FALSE]


  demetra_m_m7 <- demetra_m_m7[,
    lapply(
      X = .SD,
      FUN = function(x){
        as.numeric(stringr::str_replace(
          string = x,
          pattern = "[,]",
          replacement = "."
        ))
      }
    ),
    .SDcols = colnames(demetra_m_m7)
  ]

  demetra_m_m7[, m7_quality := .jdemetra_apply_threshold(
    x = m7,
    threshold = .jdemetra_get_thresholds_quality()[["m7"]]
  )]
  return(demetra_m_m7)
}
