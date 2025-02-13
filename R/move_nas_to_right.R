#' Move NAs to the right side
#' 
#' `move_nas_to_right` takes a vector or array and rearranges the elements such
#' that all non-NA values will - in unchanged order - appear before the NAs.
#' @param x A vector or array.
#' @export

move_nas_to_right <- function(x) {
  non_nas <- x[!is.na(x)]
  n_nas <- sum(is.na(x))
  return(c(non_nas, rep(NA, n_nas)))
}