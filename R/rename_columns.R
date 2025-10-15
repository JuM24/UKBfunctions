#' Rename columns based on dictionary
#'
#' `rename_columns` takes a data frame and a .csv file that acts as a dictionary
#' to change column names of the data frame.
#' @param df Input data frame whose column names will be changed.
#' @param colname_file Data frame that contains as separate columns both old
#' and new column names of `df`.
#' @param field_names Name of the column with UKB field names names.
#' @param new_cols Name of the column with new column names
#' @details
#' One of two options is allowd:
#' 1: `field_names`, `new_cols`, and `colname_file` are `NULL`, in which case
#' the RAP-based column naming will be replae with the "old" local naming.
#' 2: `field_names`, `new_cols`, and `colname_file` are provided by the user,
#' in which case the latter must contain as columns `field_names` and `new_cols`
#' that refer to the old to-be-replaced field names and the new names, respectively.
#'
#' @export

rename_columns <- function(df,
                           colname_file = NULL,
                           field_names = NULL,
                           new_cols = NULL){

  # set aside 'eid' column because it's special
  cn <- colnames(df)
  stopifnot(identical(cn[1], 'eid'))
  rest <- cn[-1]

  ## in the default setting, we just change to old 'X'-based formatting
  if (is.null(field_names) & is.null(new_cols)){
    # replace all 'p' with 'X', all '_' with '.', and remove all 'i'
    rest <- gsub('p', 'X', rest, fixed = TRUE)
    rest <- gsub('_', '.', rest, fixed = TRUE)
    rest <- gsub('[ia]', '', rest)
    # add repetition digit if lacking
    dot_counts <- lengths(regmatches(rest, gregexpr('\\.', rest)))
    rest <- ifelse(dot_counts == 0, paste0(rest, '.0.0'), rest)
    rest <- ifelse(dot_counts == 1, paste0(rest, '.0'), rest)
    colnames(df) <- c('eid', rest)

    ## throw error if colnames are incorrectly specified
  } else if (is.null(field_names) + is.null(new_cols) == 1 ||
             (!field_names %in% colnames(colname_file) |
              !new_cols %in% colnames(colname_file))){
    stop('`field_names` and `new_cols` must either both be `NULL` or both present
         as `colnames` in `df`.')

    ## condition when both `field_names` and `new_cols` are provided by user
    } else{
    # remove NA columns and throw warning
    if (sum(is.na(colname_file[, c(field_names, new_cols)])) > 0){
      colname_file <-
        colname_file[complete.cases(colname_file[, c(field_names, new_cols)]), ]
      warning('Some old/new column names were NA and were removed.')
    }
    # remove all all 'i'
    colnames(df) <- c('eid', gsub('i', '', rest, fixed = TRUE))
    # build lookup vector
    name_dict <- setNames(colname_file[[field_names]], colname_file[[new_cols]])
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
  }
  return(df)
}
