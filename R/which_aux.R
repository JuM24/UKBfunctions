#' Return table with relevant metrics to choose auxiliary variables
#'
#' `which_aux` takes a data frame and returns a table with metrics relevant for
#' choosing auxiliary variables for multiple imputation of a missing variable.
#' These include for each variable the signed and absolute correlation
#' coefficients for the correlation with the missing variable; whether this
#' correlation exceeds the minimum threshold as defined by the user; the number of
#' missing observations; whether the missingness proportion exceeds a user-defined
#' threshold; whether any correlation with other variables exceeds a user-defined
#' threshold and - if so - how many.
#' @param df The input data frame.
#' @param missing_var The column name referring to the missing variable.
#' @param model_vars A list of column names for variables that are to be included
#' in the eventual analysis.
#' @param missing_n_flag The maximum proportion of missing values before the
#' variable is flagged.
#' @param inter_cor_flag The maximum absolute correlation with another variable
#' - excluding the missing variable - before the variable is flagged.
#' @param missing_cor_flag The minimum correlation with the missing variable
#' among pairwise-complete cases before the variable is flagged.
#' @export


# converts all columns to numeric
# adds a count of highly correlated variables

which_aux <- function(df,
                       missing_var,
                       model_vars,
                       missing_n_flag = 0.2,
                       inter_cor_flag = 0.9,
                       missing_cor_flag = 0.05){

  # check if variable of interest in `df`
    if (!missing_var %in% names(df)) {
    stop('`Missing_var` must be a column name in `df`.')
  }

  # all columns to numeric
  df[] <- lapply(df, as.numeric)

  # correlation matrix
  cor_mat <- cor(df, use = 'pairwise.complete.obs')
  diag(cor_mat) <- NA # remove self-correlations
  cor_abs <- abs(cor_mat)


  # all variables except the missing one
  vars <- setdiff(colnames(df), missing_var)
  r_vec <- cor_mat[missing_var, vars] # signed
  r_abs <- abs(r_vec)

  # missing counts
  n_missing <- sapply(df[vars], function(x) sum(is.na(x)))

  # correlations with missing_var
  var_tbl <- data.frame(
    variable   = vars,
    r          = as.numeric(r_vec),
    r_abs      = as.numeric(r_abs),
    n_missing  = n_missing,
    stringsAsFactors = FALSE
  )

  # missingness flags
  var_tbl$missing_n_prop <- var_tbl$n_missing/nrow(df)
  var_tbl$missing_n_flag <- as.integer(var_tbl$missing_n_prop > missing_n_flag)

  # inter-variable correlations
  inter_cor_max <- numeric(length(vars))
  inter_cor_n   <- integer(length(vars))
  inter_cor_max_var <- character(length(vars))

  # rows to use for correlation table (drop missing_var)
  inter_rows <- setdiff(rownames(cor_abs), missing_var)

  for (i in seq_along(vars)) {
    v <- vars[i]
    col_vals <- cor_abs[inter_rows, v]

    # ignore self-cor (already NA on diagonal), but be robust to all-NA column
    if (all(is.na(col_vals))) {
      inter_cor_max[i] <- NA_real_
      inter_cor_max_var[i] <- NA_character_
      inter_cor_n[i]   <- NA_integer_
    } else {
      inter_cor_max[i] <- max(col_vals, na.rm = TRUE)
      # name of the variable achieving that max (take first if there are ties)
      which_max <- which(col_vals == inter_cor_max[i] & !is.na(col_vals))
      inter_cor_max_var[i] <- names(col_vals)[which_max[1]]
      inter_cor_n[i]   <- sum(col_vals > inter_cor_flag, na.rm = TRUE)
    }
  }

  var_tbl$inter_cor_max  <- inter_cor_max
  var_tbl$inter_cor_max_var <- inter_cor_max_var
  var_tbl$inter_cor_n <- inter_cor_n


  # correlation with missing_var from cor_abs
  var_tbl$missing_cor_flag  <- as.integer(!is.na(var_tbl$r_abs) &
                                            var_tbl$r_abs < missing_cor_flag)

  # order by |r|
  var_tbl <- var_tbl[order(-var_tbl$r_abs), ]

  # indicator for being in causal model
  var_tbl$modelling_var <- as.integer(var_tbl$variable %in% model_vars)

  return(var_tbl)
}



