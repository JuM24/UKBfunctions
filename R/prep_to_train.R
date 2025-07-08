#' Preparation of the data for training and testing. Inludes potential feature
#' amendments, data filtering, and imbalance correction.
#'
#' `prep_to_train` Takes the train and test sets and returns them ready to be
#' input into a predictive model. All numerical variables are scaled.
#' @param train_set The training set.
#' @param test_set The test set.
#' @param target_var The target variable to be predicted.
#' @param amend_features Whether features should be amended; relevant only for
#' self-report data.
#' @param max_followup The time within which the outcome that is to be
#' predicted should occur.
#' @param remove_censored Whether participants that were lost to follow-up
#' before max_followup should be removed. Assumes the presence of the columns
#' `asc_date` and `censor_date` in the data frame.
#' @param imbalance_correct Whether imbalance correction should be performed.
#' Either `NULL`, `'SMOTE'` or `'downsample'`.
#' @param remove_vars A list of column names that should be removed prior to
#' balance correction. Especially useful for SMOTE.
#' @param min_age The minimum age of the sample. Determines whether participants
#' below a certain age should be dropped prior to training.
#' @param balance_prop Only relevant if `imbalance_correct = 'SMOTE'`. The
#' proportion of observations that will belong to the erstwhile minority class
#' after SMOTE.
#' @param random_seed The random seed for SMOTE and downsampling.
#' @param ordinals Only relevant if `imbalance_correct = 'SMOTE'`. A vector of
#' ordinal categorical variables in the training data.
#' @param K Only relevant if `imbalance_correct = 'SMOTE'`. The K for KNN.
#' @param normalise Whether to scale numerical data.
#' @param verbose Whether to print progress to console.
#' @export

prep_to_train <- function(train_set,
                          test_set,
                          target_var,
                          amend_features,
                          max_followup = NULL,
                          remove_censored = FALSE,
                          imbalance_correct = NULL,
                          remove_vars = NULL,
                          min_age = NULL,
                          balance_prop,
                          random_seed,
                          ordinals,
                          smote_K,
                          normalise = FALSE,
                          verbose = verbose){
  # TODO CHECK THIS WHOLE FUNCTION
  # potentially remove participants with loss to follow-up before max_followup
  if (remove_censored == TRUE){
    old_nrow_train <- nrow(train_set)
    old_nrow_test <- nrow(test_set)
    train_set <- train_set %>%
      filter(status %in% c(1, 2) | followup >= max_followup)
    test_set <- test_set %>%
      filter(status %in% c(1, 2) | followup >= max_followup)
    print(paste0('Removed participants with loss to follow-up within ',
          as.character(max_followup), ' years of time 0: ',
          as.character(old_nrow_train - nrow(train_set)), ' in the training set and ',
          as.character(old_nrow_test - nrow(test_set)), ' in the test set.'))
  } else if (remove_censored == FALSE){
    retained_train <- train_set %>%
      filter(followup <= max_followup & status == 0) %>%
      nrow(.) %>%
      as.character(.)
    retained_test <- test_set %>%
      filter(followup <= max_followup & status == 0) %>%
      nrow(.) %>%
      as.character(.)
    warning('Some participants were censored before the maximum follow-up of ',
            as.character(max_followup), ' years: ', retained_train, ' in
            the training set and ', retained_test, ' in the test set.')
  }

  if (!is.null(imbalance_correct)){
    if(imbalance_correct == 'SMOTE'){
      specs <- c(remove_censored, amend_features, min_age, max_followup, random_seed,
                 imbalance_correct, balance_prop, smote_K, list(ordinals))

      names(specs) = c('remove_censored', 'amend_features', 'min_age',
                       'max_followup', 'random_seed',
                       'imbalance_correct', 'balance_prop', 'smote_K', 'ordinals')

    } else if (imbalance_correct == 'downsample'){
      specs <- c(remove_censored, amend_features, min_age, max_followup, random_seed,
                 imbalance_correct, balance_prop)
      names(specs) = c('remove_censored', 'amend_features', 'min_age',
                       'max_followup', 'random_seed',
                       'imbalance_correct', 'balance_prop')
    }
  } else if (is.null(imbalance_correct)){
    specs <- c(remove_censored, amend_features, min_age, max_followup, random_seed)
    names(specs) = c('remove_censored', 'amend_features', 'min_age',
                     'max_followup', 'random_seed')

  }

  if (amend_features == TRUE){
    # amend features with additional data
    train_set <- UKBfunctions::amend_mm_features(df = train_set,
                                                 verbose = verbose)
    test_set <- UKBfunctions::amend_mm_features(df = test_set,
                                                verbose = FALSE)
  }

  # set participants that experienced the outcome after the follow-up cutoff
  # to non-cases
  if (is.null(max_followup)){
    max_followup <- c(NULL, max(train_set[train_set$status == 1, 'followup'],
                                test_set[test_set$status == 1, 'followup'],
                                na.rm = TRUE))
  } else {
    case_n_train <- sum(train_set$status == 1)
    case_n_test <- sum(test_set$status == 1)

    # set cases after maximum follow-up to 0
    train_set[(train_set$status == 1 & train_set$followup > max_followup),
              'status'] <- 0
    train_set[(train_set$status == 2 & train_set$followup > max_followup),
              'status'] <- 0
    train_set$followup[train_set$followup > max_followup] <- max_followup

    test_set[(test_set$status == 1 & test_set$followup > max_followup),
              'status'] <- 0
    test_set[(test_set$status == 2 & test_set$followup > max_followup),
              'status'] <- 0
    test_set$followup[test_set$followup > max_followup] <- max_followup

    if (verbose == TRUE){
      print(paste0('Set cases ascertained more than ', as.character(max_followup),
                   ' years after baseline to non-cases: ',
                   as.character(case_n_train - sum(train_set$status == 1)),
                   ' in the training set and ',
                   as.character(case_n_test - sum(test_set$status == 1)),
                   ' in the test set.'))
    }
  }

  # save age mean and SD to scale later on
  age_mean_train <- mean(train_set$asc_age, na.rm = TRUE)
  age_sd_train <- sd(train_set$asc_age, na.rm = TRUE)

  # potentially correct class imbalance in the training set
  train_set <- UKBfunctions::correct_balance(df = train_set,
                                             target_var = 'status',
                                             approach = imbalance_correct,
                                             remove_vars = remove_vars,
                                             balance_prop = balance_prop,
                                             random_seed = random_seed,
                                             verbose = verbose,
                                             ordinals = ordinals,
                                             K = smote_K,
                                             normalise = normalise)

  # remove younger participants
  if (!is.null(min_age)){
    old_nrow_train <- nrow(train_set)
    old_nrow_test <- nrow(test_set)

    # convert min_age to the corresponding scaled cutoff
    if (normalise == TRUE){
      scaled_min_age_train <- (min_age - age_mean_train) / age_sd_train
    } else if (normalise == FALSE){
      scaled_min_age_train <- min_age
    }
    train_set <- train_set %>%
      filter(asc_age >= scaled_min_age_train)
    test_set <- test_set %>%
      filter(asc_age >= min_age)
    if (verbose == TRUE){
      print(paste0('Removed participants younger than ',
                   as.character(min_age), ' years',
                   ': ', as.character(old_nrow_train - nrow(train_set)),
                   ' in the training set and ',
                   as.character(old_nrow_test - nrow(test_set)),
                   ' in the test set.'))
      cat('\n')
    }
  }

  # test set: potentially normalise and centre
  if (normalise == TRUE){
    num_cols_test <- sapply(test_set, is.numeric)
    test_set[num_cols_test] <- scale(test_set[num_cols_test])
  }
  output_df <- list(train_set, test_set, specs)
  names(output_df) <- c('train_set', 'test_set', 'specs')

  return(output_df)
}
