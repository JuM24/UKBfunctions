#' Create a data frame with multimorbidity columns
#' 
#' `create_diseases_self` takes a data frame with an id column and the 20002 or
#' 20001 UKB columns and returns a data frame with relevant disorders as rows
#' @param df The input data frame.
#' @param mm_codes A data frame of disorder codes with the columns `code`,
#' `disorder_long`, `field_id`, and `disorder`, indicating the self-report code
#' in UKB, the custom name of the disorder, the UKB field ID (20002 or 20001),
#' and a short name for the disorder, respectively.
#' @param cancer TRUE or FALSE; indicates whether field ID 20001 or 20002 is 
#' used.
#' @export

create_diseases_self <- function(df, 
                                 mm_codes,
                                 cancer = FALSE) {
  
  if (cancer == FALSE){
    field_id <- '20002'
    mm_codes <- mm_codes %>%
      filter(field_id == 20002)
    
  } else if (cancer == TRUE){
    field_id <- '20001'
    mm_codes <- mm_codes %>%
      filter(field_id == 20001)
  }
  
  field_id_baseline <- paste0('X', field_id, '.0')
  
  # self-reported disorders
  df <- df %>%
    # select only those disorders reported during UKB baseline
    select(id, starts_with(field_id_baseline)) %>%
    # turn irrelevant entries into NAs
    mutate(across(starts_with(field_id_baseline), 
                  ~ ifelse(. %in% mm_codes$code, ., NA)))
  df[] <- lapply(df, as.character)
  
  
  # fill in the correct code
  for (mm_code in mm_codes$code){
    # logical matrix identifying the code of interesting while ignoring first column
    log_mat <- (df[, -1] == mm_code) & !is.na(df[, -1])
    # add the first column as FALSE labels so that IDs don't get replaced
    log_mat <- cbind(FALSE, log_mat)
    # replace number with term
    df[log_mat] <- mm_codes$disorder[which(mm_codes$code == mm_code)]
  }
  
  
  # remove multiple disease occurrences per participant
  df <- as.data.frame(t(apply(df, 1, remove_duplicate_cols)),
                      stringsAsFactors = FALSE)
  
  # move NAs to the right and remove empty columns
  df <- as.data.frame(t(apply(df, 1, move_nas_to_right)), 
                      stringsAsFactors = FALSE)
  df <- df[, colSums(is.na(df)) < nrow(df)]
  
  
  # pivot to long, with rows for each id-disorder pair
  first_col <- colnames(df)[1]
  df_long <- data.frame(df[, c(first_col, colnames(df)[2])])
  df_long <- df_long[complete.cases(df_long), ]
  if (ncol(df) >= 3){
    for (col in colnames(df[3:ncol(df)])){
      new_df <- data.frame(df[, c(first_col, col)])
      new_df <- new_df[complete.cases(new_df), ]
      colnames(new_df) <- colnames(df_long)
      df_long <- rbind(df_long, new_df)
    }
  }
  df_long$n <- 1
  
  # pivot to wide, creating a column for each disorder
  df_wide <- df_long %>%
    pivot_wider(names_from = colnames(df_long)[2],
                values_from = n)
  colnames(df_wide)[1] <- 'id'
  df_wide[is.na(df_wide)] <- 0
  
  return(df_wide)
}