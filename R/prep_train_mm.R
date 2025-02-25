#' Combines training preparation and training itself into a single function. To
#' be used with multimorbidity predictors.
#'
#' `prep_train_mm` Takes the path to the R object that resulted from the
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
#' @param min_age The minimum age of the sample. Determines whether participants
#' below a certain age should be dropped prior to training.
#' @param imbalance_correct The desired proportion of the total for the
#' current minority class. 0.5 will completely balance the classes.
#' @param balance_prop The desired proportion of the total for the
#' current minority class. 0.5 will completely balance the classes.
#' @param ordinals Relevant for SMOTE. A list of column names indicating
#' the ordinal variables in the data frame. All character/factor columns
#' not included in this list will be considered as nominal variables, and
#' all numerical variables as numerical.
#' @param smote_K Relevant for SMOTE. The number of nearest neighbours for KNN.
#' @param train_algorithm The algorithm to be used for prediction.
#' @param train_metric Which metric to optimise on. Available options are
#' "Accuracy", "Kappa", "ROC", "Sens", and "Spec".
#' @param train_cv_folds The number of folds to be used during cross-validation;
#' corresponds to the argument `number` in `caret::trainControl`.
#' @param train_tune_len The length of the tuning grid; corresponds to the
#' `tuneLength` argument of `caret::train`.
#' @param train_distribute_cores Whether cross-validation should be performed in
#' parallel.
#' @param train_core_number Relevant only if `distribute_cores = TRUE`. The number of
#' cores to be used in parallel processing.
#' @param verbose Whether to print progress to console.
#' @param random_seed Random seed for imbalance correction and cross-validation.
#' @export

prep_train_mm <- function(input_file_path = '',
                          output_file_path = '',
                          extra_suffix,
                          mm_source,
                          target_var,
                          train_features,
                          max_followup,
                          min_age,
                          imbalance_correct,
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
                          random_seed){

  # read in the data
  train_test <- readRDS(paste0(input_file_path, mm_source, '_split_', target_var, '.Rds'))
  train_set <- train_test$train_set
  test_set <- train_test$test_set

  # remove redundant variables
  train_set <- train_set %>%
    select(-c(asc_date, censor_date, target_date))
  test_set <- test_set %>%
    select(-c(asc_date, censor_date, target_date))

  # prepare the data for training
  amend_features = ifelse(mm_source == 'self_report', TRUE, FALSE)
  object_prep <- UKBfunctions::prep_to_train(train_set = train_set,
                                             test_set = test_set,
                                             target_var = target_var,
                                             amend_features = amend_features,
                                             max_followup = max_followup,
                                             imbalance_correct = imbalance_correct,
                                             balance_prop = balance_prop,
                                             ordinals = ordinals,
                                             smote_K = smote_K,
                                             min_age = min_age,
                                             verbose = verbose,
                                             random_seed = random_seed)

  # update train and test sets
  train_set <- data.frame(object_prep$train_set)
  test_set <- data.frame(object_prep$test_set)

  # train
  model_train <- UKBfunctions::train_mm(df = train_set,
                                        target_var = target_var,
                                        features = train_features,
                                        algorithm = train_algorithm,
                                        train_metric = train_metric,
                                        cv_folds = train_cv_folds,
                                        tune_len = train_cv_folds,
                                        distribute_cores = train_distribute_cores,
                                        core_number = train_core_number,
                                        random_seed = random_seed,
                                        verbose = verbose)

  # combine the output with the preparation specs
  output_object <- list(model_train, train_set, test_set, list(object_prep$specs))
  names(output_object) <- c('model', 'train_set', 'test_set', 'specs')

  # save to disk
  saveRDS(output_object, paste0(output_file_path, mm_source, '_trained_',
                                target_var, extra_suffix, '.Rds'))
  return(output_object)
}
