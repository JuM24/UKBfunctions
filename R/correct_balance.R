#' Correct class imbalance
#'
#' `correct_balance` takes a data frame and corrects class imbalance for a
#' chosen binary variable. It returns the balanced data frame.
#' @param df The input data frame.
#' @param target_var The target variable for which the class imbalance should be
#' corrected.
#' @param approach The approach by which class imbalance should be corrected.
#' The possible choices are 'downsample' and 'SMOTE'.
#' @param balance_prop The desired proportion of the total for the
#' current minority class. 0.5 will completely balance the classes.
#' @param random_seed The random seed run at the start of the function.
#' @param ordinals Relevant for SMOTE. A list of column names indicating
#' the ordinal variables in the data frame. All character/factor columns
#' not included in this list will be considered as nominal variables, and
#' all numerical variables as numerical.
#' @param undo_dummies Relevant for SMOTE. Whether dummy variables created for
#' nominal categorical variables should be turned back into their original form.
#' This reduces the number of variables so that e.g., we have "sex" instead of
#' "sex.0" and "sex.1".
#' @param K Relevant for SMOTE. The number of nearest neighbours for KNN.
#' @export

correct_balance <- function(df,
                            target_var,
                            approach = NULL,
                            balance_prop = 0.5,
                            random_seed,
                            ordinals,
                            undo_dummies = TRUE,
                            K = 5){

  # the numbers of minority and majority class observations
  num_minority <- min(table(df[[target_var]]))
  num_majority <- max(table(df[[target_var]]))

  # name of the class levels
  majority_name <- names(which(table(df[[target_var]]) == num_majority))
  minority_name <- names(which(table(df[[target_var]]) == num_minority))

  # implement SMOTE to increase sample size of minority class
  if (approach == 'SMOTE'){
    library(caret)

    df_new <- df %>%
      mutate_if(is.integer, as.numeric)

    # normalise numerical variables
    numeric_columns <- sapply(df_new, is.numeric)
    df_new[numeric_columns] <- lapply(df_new[numeric_columns], function(x) as.vector(scale(x)))

    # ordinal variables to integers (required for KNN)
    df_new[ordinals] <- lapply(df_new[ordinals], as.integer)

    # all other variables are factors
    df_new <- df_new %>%
      mutate_if(is.character, as.factor)

    # if user wants complete balance, leave `dup_size` argument at default value
    if (balance_prop == 0.5){
      dup_size <- 0
    } else{
      # calculate the desired number of minority observations based on `balance_prop`
      desired_minority <- round(num_majority*balance_prop / (1 - balance_prop))
      # determine the `dup_size` argument for SMOTE based on above
      dup_size <- desired_minority/num_minority
      if (desired_minority < num_minority) {
        stop('Error: when using SMOTE, the desired proportion of the minority class
         must be greater than the current proportion of the minority class.
         Check the class proportions and amend the `balance_prop` argument.')
      }
    }

    # one-hot encoding for nominal variables
    dmy <- dummyVars(' ~ .', data = df_new[, -which(names(df_new) == target_var)])
    X <- data.frame(predict(dmy, newdata = df_new[, -which(names(df_new) == target_var)]))
    target_var_value <- df_new[[target_var]]

    # apply SMOTE
    set.seed(random_seed)
    data_smote <- smotefamily::SMOTE(X = X,
                                     target = target_var_value,
                                     K = K,
                                     dup_size = dup_size)

    # create new data frame
    df_smote <- data_smote$data

    # select all variables except the target (the only one still categorical)
    numeric_vars <- colnames(df_smote %>%
                               select_if((is.numeric)))

    # if a variable was not in the pre-SMOTE df, it means it was dummy-coded
    # and must be a nominal variable
    categ_vars <- numeric_vars[!numeric_vars %in% colnames(df_new)]

    # categorical variables were assigned values that might not exist;
    # we have to round them to get plausible values
    df_smote <- df_smote %>%
      mutate(across(starts_with(c(categ_vars, ordinals)), round))

    # potentially undo hot-one-encoding and get original variables
    if (undo_dummies == TRUE){
      df <- undo_dummies(df = df,
                         remove_dummies = TRUE,
                         verbose = verbose)
      # ordinals back to factors
      df_smote <- df_smote %>%
        mutate(across(starts_with(ordinals), as.factor))

    } else if (undo_dummies == FALSE){
      # ordinals and dummy categoricals back to factors
      df_smote <- df_smote %>%
        mutate(across(starts_with(c(ordinals, categor_vars)), as.factor))
    }

    # rename back outcome variable
    colnames(df_smote)[colnames(df_smote) == 'class'] <- target_var
    df_smote[[target_var]] <- as.factor(df_smote[[target_var]])
    df_smote <- df_smote %>%
      mutate_if(is.character, as.factor)

    return(df_smote)

  } else if (approach == 'downsample'){
    # calculate the desired number of majority observations based on `balance_prop`
    desired_majority <- num_minority/balance_prop - num_minority
    majority_cases <- subset(df, df[[target_var]] == majority_name)
    minority_cases <- subset(df, df[[target_var]] == minority_name)

    # random sample of the majority class
    set.seed(random_seed)
    majority_sample <- sample(majority_cases$id, desired_majority)

    # downsample and merge the two classes
    majority_cases <- majority_cases[majority_cases$id %in% majority_sample, ]
    df_donwsampled <- rbind(majority_cases, minority_cases)

    return(df_downsampled)
  } else if (is.null(approach)){
    return(df)
  }
}
