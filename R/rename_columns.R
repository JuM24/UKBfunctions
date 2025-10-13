#' Rename columns based on dictionary
#'
#' `rename_columns` takes a data frame and a .csv file that acts as a dictionary
#' to change column names of the data frame.
#' @param df Input data frame whose column names will be changed.
#' @param colname_file Data frame that contains as separate columns both old
#' and new column names of `df`.
#' @param old_cols Name of the column with old column names.
#' @param new_cols Name of the column with new column names
#' @export

rename_columns <- function(df,
                           colname_file,
                           old_cols,# vector, same with new_cols
                           new_cols){

  # remove NA columns and throw warning
  if (sum(is.na(colname_file[, c(old_cols, new_cols)])) > 0){
    colname_file <-
      colname_file[complete.cases(colname_file[, c(old_cols, new_cols)]), ]
    warning('Some old/new column names were NA and were removed.')
  }
  # build lookup vector
  name_dict <- setNames(colname_file[[old_cols]], colname_file[[new_cols]])
  # list of simple names to simplify column name later on
  new_names <- c()
  new_names_whole <- c()
  # rename iteratively
  for (name in colnames(df)){
    # main part of name
    name_simple <- sub('^[^0-9]*([0-9]+).*', '\\1', name)
    # the instance suffix
    suffix = str_replace(name, '^[^._]*', "")
    # fetch new name from dictionary
    new_name <- names(name_dict[name_dict == name_simple])
    new_names <- c(new_names, new_name)
    # paste together new name and replace old with new
    new_name_whole <- paste0(new_name, suffix)
    colnames(df)[which(colnames(df) == name)] <- new_name_whole
    new_names_whole <- c(new_names_whole, new_name_whole)
  }
  # select indices of IDs with just a single instance
  simple_ids <- which(new_names %in% names(table(new_names)[table(new_names) == 1]))
  # use the indices above to further simplify those IDs
  colnames(df)[simple_ids] <- new_names[simple_ids]

  return(df)
}
