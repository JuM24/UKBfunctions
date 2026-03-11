#' Reclassify various UKB variables
#'
#' `ukb_reclassify` takes an array of coded observations for various UKB
#' variables and re-classifies them. See `details` for more.
#' qualifications answers

#' @param x The input array.
#' @param field_id Integer; field ID for the variable.
#' @details
#' 6138 (qualifications): classification into three classes: higher,
#' secondary/vocational, none. The higher activity level is prioritised in
#' cases of answers fitting into multiple categories per participant.
#'
#' 6164 (physical activity): classification into four classes:
#' 0 - none (-7, none of the above),
#' 1 - low (code 4: light household tasks only),
#' 2 - medium (codes 1/2/5: heavy household tasks, walking for pleasure, other exercise),
#' 3 - high (code 3: strenuous sports).
#' @export

ukb_reclassify <- function(x, field_id){

  if (field_id == 6138){
    if (any((x == 1 | x == 6) & !is.na(x), na.rm = TRUE)){
      return(3)
    } else if (any((x == 2 | x == 3 | x == 4 | x == 5) & !is.na(x), na.rm = TRUE)){
      return(2)
    } else if (any (x == -7  & !is.na(x), na.rm = TRUE)){
      return(1)
    } else {
      return(NA)
    }
  } else if (field_id == 6164){
    # as per 10.1370/afm.2501
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
  } else {
    stop("`field_id` must be either 6138 or 6164.")
  }
}

