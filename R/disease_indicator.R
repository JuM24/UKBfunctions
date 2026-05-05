#' Create a disease indicator from disease date
#'
#' `disease_indicator` takes a data frame and a string indicating the column
#' containing the dates of an event, and returns the same data frame with
#' a new indicator column for that event. The original event column names is
#' changed; the new column has the same name as the original column for the
#' event.
#' @param df Input data frame.
#' @param colname Character; Name of the column with the dates for the event.
#' @param date_form Character; format of the date. Ignored for arguments that
#'  are already of class `Date`.
#' @param baseline_date String indicating the baseline date. The string should
#' match a variable in `df` that contains character strings or Date values
#' convertible to dates.
#' @param invalid_dates Character or Date vector; invalid dates in the form
#' `date_form` (or already of class `Date`). Events with these dates will
#' be converted to NA.
#' @param disease_time Logical; whether the time between the baseline
#' date and the disease date should be calculated and added to the data frame.
#' Units are controlled by `time_units`.
#' @param time_units Character; unit for the disease time column. One of
#' `'days'`, `'weeks'`, `'months'`, `'years'`. Default is `'days'`.
#' @param threshold_date Single character/Date or vector of same length as
#'  `nrow(df)`; should be in the same format as `colname` (or already of class
#'  `Date`). If not `NULL`, events occurring after this date are considered
#'  non-events and changed to NA in the returned data frame.
#' @return The input data frame with the original event column renamed to
#' `<colname>_date` and a new integer indicator column `<colname>` (1 = event,
#' 0 = no event, `NA` = invalid date).
#' @export

disease_indicator <- function(df,
                              colname,
                              date_form = NULL,
                              baseline_date = NULL,
                              invalid_dates = NULL,
                              disease_time = FALSE,
                              time_units = 'days',
                              threshold_date = NULL){

  # validate disease_time requirements
  if (disease_time && is.null(baseline_date))
    stop('`baseline_date` must be provided when `disease_time = TRUE`.')
  time_units <- match.arg(time_units, c('days', 'weeks', 'months', 'years'))

  # to date type
  if (!inherits(df[[colname]], 'Date'))
    df[[colname]] <- as.Date(df[[colname]], format = date_form)
  if (!is.null(threshold_date)){
    # if list provide, unlist
    if (is.list(threshold_date)) threshold_date <- unlist(threshold_date,
                                                          use.names = FALSE)
    if (!inherits(threshold_date, 'Date'))
      threshold_date <- as.Date(threshold_date, format = date_form)
    # throw error if neither a single character or vector of correct length
    n <- nrow(df)
    if (!(length(threshold_date) %in% c(1, n))) {
      stop(sprintf('`threshold_date` must have length 1 or nrow(df) (%d); got %d.',
                   n, length(threshold_date)))
      }
  } else{
    threshold_date <- max(df[[colname]], na.rm = TRUE)
    if (!is.finite(threshold_date)) threshold_date <- as.Date(NA)
  }

  # set invalid dates to NA
  invalid_rows <- rep(FALSE, nrow(df))
  if (!is.null(invalid_dates)) {
    if (!inherits(invalid_dates, 'Date'))
      invalid_dates <- as.Date(invalid_dates, format = date_form)
    invalid_rows <- df[[colname]] %in% invalid_dates
    df[invalid_rows, colname] <- NA
  }

  # add '_date' suffix to column name
  col_j <- which(colnames(df) == colname)
  if (length(col_j) == 0) stop('`colname` not found in `df`.')
  if (length(col_j) > 1) stop('Multiple columns match `colname`; column names must be unique.')
  date_colname <- paste0(colname, '_date')
  colnames(df)[col_j] <- date_colname

  # create a separate indicator column; all dates after threshold date do not count
  # indicators for invalid dates are set to NA
  df[[colname]] <- 0L
  df[invalid_rows, colname] <- NA
  df[!is.na(df[[date_colname]]) &
        df[[date_colname]] <= threshold_date, colname] <- 1L
  df[!is.na(df[[date_colname]]) &
        df[[date_colname]] > threshold_date, date_colname] <- NA

  # compute time between baseline and disease date
  if (disease_time) {
    baseline <- df[[baseline_date]]
    if (!inherits(baseline, 'Date'))
      baseline <- as.Date(baseline, format = date_form)
    days_diff <- as.numeric(difftime(df[[date_colname]], baseline, units = 'days'))
    divisor <- switch(time_units, days = 1, weeks = 7, months = 30.44,
                      years = 365.25)
    df[[paste0(colname, '_time')]] <- days_diff / divisor
  }

  return(df)
}
