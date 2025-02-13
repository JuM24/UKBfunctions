#' Classify education
#' 
#' `education_classify` takes an array of coded qualifications answers
#' according to UKB data field 6138 and classifies it into three classes. 
#' The higher activity level is prioritised in cases of answers fitting into 
#' multiple categories per participant.
#' @param x The input array.
#' @export

education_classify <- function(x){
  if (any((x == 1 | x == 6) & !is.na(x), na.rm = TRUE)){
    return(3)
  } else if (any((x == 2 | x == 3 | x == 4 | x == 5) & !is.na(x), na.rm = TRUE)){
    return(2)
  } else if (any (x == -7  & !is.na(x), na.rm = TRUE)){
    return(1)
  } else {
    return(NA)
  }
}