#' Return table of descriptive statistics.
#'
#' `desc_stats` takes a data frame as an input and returns a table with
#' descriptive statistics for variables in the data frame. For variables of
#' class `numeric` and `integer`, the mean or median and the standard deviation
#' or interquartile range is returned. For variables of class `factor`
#' and `character`, absolute numbers and proportions are returned. Each metric
#' is returned as a separate column.
#'
#' @param df The input data frame.
#' @param variables An array of column names, indicating the variables for which
#' descriptive statistics should be generated. If `NULL`, all variables in the
#' data frame are used.
#' @param central_tendency Character indicating whether the mean and
#' standard deviation (`'mean'`) or the median and interquartile range (`'median'`)
#' should be presented. Alternatively, an array of column names, indicating the
#' variables for which the median/IQR should be presented; for all others,
#' the mean/SD will be presented.
#' @param stratify_by An array of column names, indicating the variables to use
#' to stratify the presentation of the statistics. E.g., if
#' `stratify_by = c('var_a')`, separate columns will be created for different
#' levels of 'var_a'. If `NULL`, no stratification occurs.
#' @param useNA Logical; whether the numbers of missing values should be presented as
#' a separate column.
#' @param NA_stratify Logical; applies only when `useNA = TRUE` and indicates
#' whether the numbers of missing values should be presented separately for
#' each level of the variable that are used for stratification as indicated by
#' the argument `stratify_by`.
#' @param sig_fig Integer; how many significant figures the results should be
#' rounded to.
#' @param write Logical; whether a .csv file should be written to disk. If so,
#' the presentation is reformatted. See `details` for more.
#' @param out_path Character; file path for the .csv output. Required when
#' `write = TRUE`.
#' @details
#' When `write = TRUE`, proportions are transformed to percentages (%), and
#' the mean/median and SD/IQR columns (for `numeric` and `integer` variables)
#' and the number and proportion columns (for `factor` and `character` variables)
#' are combined into a single column, with a space (' ') separating the two values
#' and the second value placed in parentheses. E.g., if the R file has two
#' separate columns with mean and SD '22.5' and '2.91', respectively, the
#' written table will contain a single column with the entry '22.5 (2.91)'. As
#' another example, if the R file has two separate columns with n and proportion
#' '12498' and '0.873', the written table will contain a single column with the
#' entry '12498 (87.3%)'. Note that in both above examples, `sig_fig = 3`.
#'
#' The function always returns a list with two elements: `raw` (separate columns
#' per metric) and `formatted` (combined columns as described above). When
#' `write = TRUE`, the formatted version is additionally written to disk.
#' @return A list with two data frames: `raw` containing separate columns for
#' each metric, and `formatted` containing combined presentation-ready columns.
#' @export

desc_stats <- function(df,
                       variables = NULL,
                       central_tendency = 'mean',
                       stratify_by = NULL,
                       useNA = TRUE,
                       NA_stratify = FALSE,
                       sig_fig = 3,
                       write = TRUE,
                       out_path = NULL) {

  # if user select none, select all
  if (is.null(variables)) variables <- setdiff(colnames(df), stratify_by)

  # differentiate numeric from factors
  numeric_vars <- variables[sapply(df[variables], function(x)
    is.numeric(x) || is.integer(x))]
  categ_vars <- setdiff(variables, numeric_vars)

  # determine which numeric variables use median/IQR
  if (length(central_tendency) == 1 && central_tendency == 'mean') {
    median_vars <- character(0)
  } else if (length(central_tendency) == 1 && central_tendency == 'median') {
    median_vars <- numeric_vars
  } else {
    all_numeric <- names(df)[sapply(df, function(x)
      is.numeric(x) || is.integer(x))]
    non_num <- central_tendency[!central_tendency %in% all_numeric]
    if (length(non_num) > 0)
      warning('Non-numeric variables in `central_tendency` ignored: ',
              paste(non_num, collapse = ', '))
    median_vars <- intersect(central_tendency, numeric_vars)
  }

  # pre-compute all levels for categorical variables from the full data,
  # so that strata with missing levels still get a row
  all_levels <- lapply(setNames(categ_vars, categ_vars), function(v)
    sort(unique(as.character(df[[v]][!is.na(df[[v]])]))))

  # helper: compute stats for one data subset
  compute_one <- function(data) {
    rows <- list()
    for (v in variables) {
      col <- data[[v]]
      if (v %in% numeric_vars) {
        use_med <- v %in% median_vars # whether mean or median should be used
        if (use_med) {
          ct <- signif(median(col, na.rm = TRUE), sig_fig)
          sp <- signif(IQR(col, na.rm = TRUE), sig_fig)
        } else {
          ct <- signif(mean(col, na.rm = TRUE), sig_fig)
          sp <- signif(sd(col, na.rm = TRUE), sig_fig)
        }
        rows[[length(rows) + 1]] <- data.frame(
          variable = v, level = NA_character_,
          central = ct, spread = sp,
          n = NA_integer_, proportion = NA_real_,
          n_missing = sum(is.na(col)),
          stringsAsFactors = FALSE)
      } else {
        lvls <- all_levels[[v]]
        total_valid <- sum(!is.na(col))
        nm <- sum(is.na(col))
        tbl <- table(factor(as.character(col), levels = lvls))
        for (i in seq_along(lvls)) {
          count <- as.integer(tbl[lvls[i]])
          prop <- if (total_valid > 0) signif(count / total_valid, sig_fig)
                  else NA_real_
          rows[[length(rows) + 1]] <- data.frame(
            variable = v, level = lvls[i],
            central = NA_real_, spread = NA_real_,
            n = count, proportion = prop,
            n_missing = if (i == 1) nm else NA_integer_,
            stringsAsFactors = FALSE)
        }
      }
    }
    do.call(rbind, rows)
  }

  # compute stats (stratified or not)
  if (is.null(stratify_by)) {
    raw <- compute_one(df)
    if (!useNA) raw$n_missing <- NULL

  } else {
    group_df <- df[stratify_by]
    complete_mask <- complete.cases(group_df)
    group_labels <- do.call(paste, c(as.list(group_df), sep = '.'))
    groups <- sort(unique(group_labels[complete_mask]))

    # overall stats (used for row skeleton and overall n_missing)
    base <- compute_one(df)
    raw <- base[, c('variable', 'level')]

    for (g in groups) {
      grp <- compute_one(df[group_labels == g & complete_mask, , drop = FALSE])
      for (col in c('central', 'spread', 'n', 'proportion', 'n_missing')) {
        raw[[paste0(col, '_', g)]] <- grp[[col]]
      }
    }

    # handle n_missing columns
    if (useNA) {
      if (!NA_stratify) {
        raw[grep('^n_missing_', colnames(raw))] <- NULL
        raw$n_missing <- base$n_missing
      }
    } else {
      raw[grep('n_missing', colnames(raw))] <- NULL
    }
  }

  # formatted version
  formatted <- raw[, c('variable', 'level')]

  if (is.null(stratify_by)) {
    formatted$stat <- ifelse(
      raw$variable %in% numeric_vars,
      paste0(raw$central, ' (', raw$spread, ')'),
      paste0(raw$n, ' (', signif(raw$proportion * 100, sig_fig), '%)'))

  } else {
    for (g in groups) {
      ct_col <- paste0('central_', g)
      sp_col <- paste0('spread_', g)
      n_col  <- paste0('n_', g)
      pr_col <- paste0('proportion_', g)

      formatted[[paste0('stat_', g)]] <- ifelse(
        raw$variable %in% numeric_vars,
        paste0(raw[[ct_col]], ' (', raw[[sp_col]], ')'),
        paste0(raw[[n_col]], ' (', signif(raw[[pr_col]] * 100, sig_fig), '%)'))
    }
  }

  # carry over n_missing columns
  nm_cols <- grep('^n_missing', colnames(raw), value = TRUE)
  for (nm in nm_cols) formatted[[nm]] <- raw[[nm]]

  # write
  if (write) {
    if (is.null(out_path)) {
      warning('`write = TRUE` but `out_path` is NULL; no file written.')
    } else {
      write.csv(formatted, out_path, row.names = FALSE)
    }
  }

  return(list(raw = raw, formatted = formatted))
}
