#' Remove numerical outliers
#'
#' `remove_outliers` takes an array, sets outliers to NA, and returns the array.
#' @param x Input array of numerical values.
#' @param method Character; indicates which statistic is used to calculate the
#' spread. Must be either 'sd' or 'iqr'.
#' @param var_metric Numerical; indicates the threshold of variance units -
#' standard deviations or interquartile ranges - from the mean/median that are
#' after which observations are considered outliers.
#' @export

remove_outliers <- function(x, method, var_metric) {
  if (method == 'sd'){
    maximum <- (mean(x, na.rm=T)) + (var_metric * sd(x, na.rm=T))
    minimum <- (mean(x, na.rm=T)) - (var_metric * sd(x, na.rm=T))
  }
  else if (method == 'iqr'){
    maximum <- (quantile(x, 0.75, na.rm=T)) + (var_metric * IQR(x, na.rm=T))
    minimum <- (quantile(x, 0.25, na.rm=T)) - (var_metric * IQR(x, na.rm=T))
  } else {
    stop("`method` must be either 'sd' or 'iqr'.")
  }
  x[(x > maximum) | (x < minimum)] <- NA
  return(x)
}
