#' Classify physical activity
#'
#' `phys_act_classify` takes an array of coded physical activity answers
#' according to UKB data field 6164 and classifies it into "none", "mild",
#' "moderate", or "heavy" physical activity. The higher activity level is
#' prioritised in cases of answers fitting into multiple categories per
#' participant.
#' @param x The input array.
#'
#' 0 - none, low light household tasks only
#' 1 - medium heavy household tasks and/or walking for pleasure and/or other exercise,
#' 2 - high strenuous sports.
#' @export

phys_act_classify <- function(x){
  if (any(x == 3 & !is.na(x), na.rm = TRUE)){
    return(3)
  } else if (any((x == 1 | x == 2 | x == 5) & !is.na(x), na.rm = TRUE)){
    return(2)
  } else if(any(x == 4 & !is.na(x), na.rm = TRUE)){
    return(1)
  } else if (any (x == -7  & !is.na(x), na.rm = TRUE)){
    return(0)
  } else {
    return(NA)
  }
}
