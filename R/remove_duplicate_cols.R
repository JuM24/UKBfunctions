#' Change duplicate entries to NA
#' 
#' `remove_duplicate_cols` takes a vector or array and returns it with
#' duplicated elements replaced by NAs.
#' @param x A vector or array.
#' @return The input vector with duplicated elements replaced by `NA`.
#' @export

remove_duplicate_cols <- function(x) {
  duplicate_cols <- duplicated(x)
  x[duplicate_cols] <- NA
  return(x)
}