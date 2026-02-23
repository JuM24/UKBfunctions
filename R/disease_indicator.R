#' Create a disease indicator from disease date
#'
#' `disease_indicator` takes a data frame and a string indicating the column
#' containing the dates of an event, and returns the same data frame with
#' a new indicator column for that event. The original event column names is
#' changed; the new column has the same name as the original column for the
#' event.
#' @param df Input data frame.
#' @param colname Character; Name of the column with the dates for the event.
#' @param date_form Character; format of the date.
#' @param threshold_date Single character or vector of same length as `nrow(df)`;
#'  should be in the same format as `colname`. If not `NULL`, events occurring
#'  after this date are considered non-events and changed to NA in the returned
#'  data frame.
#' @export

disease_indicator <- function(df,
                              colname,
                              date_form = NULL,
                              threshold_date = NULL){

  # to date type
  df[[colname]] <- as.Date(df[[colname]], format = date_form)
  if (!is.null(threshold_date)){
    # if list provide, unlist
    if (is.list(threshold_date)) threshold_date <- unlist(threshold_date,
                                                          use.names = FALSE)
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

  # add '_date' suffix to column name
  col_j <- which(colnames(df) == colname)
  if (length(col_j) == 0) stop('`colname` not found in `df`.')
  if (length(col_j) > 1) stop('Multiple columns match `colname`; column names must be unique.')
  date_colname <- paste0(colname, '_date')
  colnames(df)[col_j] <- date_colname

  # create a separate indicator column; all dates after threshold date do not count
  df[[colname]] <- 0L
  df[!is.na(df[[date_colname]]) &
        df[[date_colname]] <= threshold_date, colname] <- 1L
  df[!is.na(df[[date_colname]]) &
        df[[date_colname]] > threshold_date, date_colname] <- NA

  return(df)
}
