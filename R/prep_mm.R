#' Combines training preparation and training itself into a single function. To
#' be used with multimorbidity predictors.
#'
#' `prep_mm` Takes the path to the R object that resulted from the
#' pipeline ending with the `train_test_separation` function, prepares the
#' data for training using `prep_to_train` and trains using `train_mm`.
#' @param input_file_path The directory name from which to load the input file.
#' @param output_file_path The directory name into which the output file
#' is to be saved.
#' @param extra_suffix An optional extra suffix to be added to the output file;
#' by default, the MM source and target variable are already in the name to
#' prevent accidental overwrites during export.
#' @param mm_source The source of disease history.
#' @param target_var The target variable to be predicted.
#' @param max_followup The time within which the outcome that is to be
#' predicted should occur. If not set to `NULL`, the input data frame must
#' contain a column called 'followup' which is the time in years between baseline
#' and right-censoring or event occurrence, whichever occurs first.
#' @param remove_censored Whether participants that were lost to follow-up
#' before max_followup should be removed. Assumes the presence of the columns
#' `asc_date` and `censor_date` in the data frame.
#' @param min_age The minimum age of the sample. Determines whether participants
#' below a certain age should be dropped prior to training.
#' @param imbalance_correct The desired proportion of the total for the
#' current minority class. 0.5 will completely balance the classes.
#' @param remove_vars A list of column names that should be removed prior to
#' balance correction. Especially useful for SMOTE.
#' @param balance_prop The desired proportion of the total for the
#' current minority class. 0.5 will completely balance the classes.
#' @param ordinals Relevant for SMOTE. A list of column names indicating
#' the ordinal variables in the data frame. All character/factor columns
#' not included in this list will be considered as nominal variables, and
#' all numerical variables as numerical.
#' @param smote_K Relevant for SMOTE. The number of nearest neighbours for KNN.
#' @param verbose Whether to print progress to console.
#' @param random_seed Random seed for imbalance correction and cross-validation.
#' @export

prep_mm <- function(input_file_path = NULL,
                    output_file_path = NULL,
                    extra_suffix,
                    mm_source,
                    target_var,
                    train_features,
                    max_followup,
                    remove_censored = FALSE,
                    min_age,
                    imbalance_correct,
                    remove_vars = NULL,
                    balance_prop,
                    ordinals,
                    smote_K,
                    train_algorithm,
                    train_metric,
                    train_cv_folds,
                    train_tune_len,
                    train_distribute_cores,
                    train_core_number,
                    verbose,
                    random_seed,
                    prep = TRUE,
                    train = TRUE){

  # read in the data
  if (is.null(input_file_path)){
    train_test <- readRDS(input_file_path)
  } else{
    train_test <- readRDS(paste0(input_file_path, mm_source, '_split_', target_var, '.Rds'))
  }
  train_set <- train_test$train_set
  test_set <- train_test$test_set

  # prepare the data for training
  amend_features = ifelse(mm_source == 'self_report', TRUE, FALSE)
  object_prep <- UKBfunctions::prep_to_train(train_set = train_set,
                                             test_set = test_set,
                                             target_var = target_var,
                                             amend_features = amend_features,
                                             max_followup = max_followup,
                                             remove_censored = remove_censored,
                                             imbalance_correct = imbalance_correct,
                                             remove_vars = remove_vars,
                                             balance_prop = balance_prop,
                                             ordinals = ordinals,
                                             smote_K = smote_K,
                                             min_age = min_age,
                                             verbose = verbose,
                                             random_seed = random_seed)

  # save to disk
  if (is.null(output_file_path)){
    output_file_path <- ''
  }

  saveRDS(object_prep, paste0(output_file_path, mm_source, '_preped_',
                              target_var, extra_suffix, '.Rds'))
  return(object_prep)
}
