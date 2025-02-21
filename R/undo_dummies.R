#' Undo hot-one-encoding
#'
#' `undo_dummies` takes a data frame with dummy variables created by
#' hot-one-encoding, often through the use of KNN-based methods, and
#' converts the dummy variables into their original multi-level form.
#' @param df The input data frame.
#' @param remove_dummies Whether the dummy variables should be removed.
#' @param verbose Whether to print progress to console.
#' @export

undo_dummies <- function(df,
                         remove_dummies = TRUE,
                         verbose = FALSE){

  # find all dummy columns (ending with '.<digit>')
  dummy_cols <- grep('\\.[0-9]+$', names(df), value = TRUE)

  # get base variable names (everything before the dot)
  base_vars <- unique(sub('\\.[0-9]+$', '', dummy_cols))

  if (verbose) {
    cat('Undoing hot-one-encoding: ')
    flush.console()
  }

  for (var in base_vars) {
    # find all dummy columns for this base variable
    if (verbose) {
      cat(var, ' ')
      flush.console()
    }
    cols <- grep(paste0('^', var, '\\.[0-9]+$'), names(df), value = TRUE)

    # determine which dummy column is 1 (if any):
    df[[var]] <- apply(df[cols], 1, function(row) {
      # evaluate each row at a time
      # catch potential errors
      if (all(row == 0)) {
        stop('Error: Row has no active dummy variable for ', var, '.
            Something might have gone wrong during the creation of dummy
            variables or in the conversion of characters to factors.')
      }
      if (sum(row == 1) != 1) {
        print(sum(row == 1))
        stop('Error: Row has ', sum(row == 1), ' active dummy variables for ', var,
             ' (expected exactly 1).')
      }

      # among the dummy variables (columns) for this variable for this row,
      # select the one that was 1 (indicating the level of the original
      # pre-SMOTE variable)
      level_index <- which(row == 1)
      # extract the level (the part after the dot)
      level <- sub('.*\\.', '', cols[level_index])
      return(level)
    })

    # convert to factor
    df[[var]] <- factor(df[[var]])

    if (remove_dummies == TRUE){
      # remove the dummy columns
      df[cols] <- NULL
    }
  }

  if (verbose) cat('\n')

  return(df)
}
