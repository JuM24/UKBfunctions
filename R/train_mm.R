#' Train the prediction algorithm, potentially correcting for class imbalance.
#'
#' `train_mm` Takes and input data frame, potentially corrects for class
#' imbalance of the target variable, and trains a predictive model using
#' the supplied features.
#' @param df The input data frame.
#' @param target_var The target variable to be predicted.
#' @param features The features used to predict the target.
#' @param prediction_type Type of prediction; possible options are
#' "classification" and "survival".
#' @param algorithm The algorithm to be used for prediction.
#' @param competing_risk Whether there is a competing event to the main target
#' event. If `TRUE`, the `status` column will be used.
#' @param train_metric Which metric to optimise on. Available options are
#' "Accuracy", "Kappa", "ROC", "Sens", and "Spec".
#' @param cv_folds The number of folds to be used during cross-validation;
#' corresponds to the argument `number` in `caret::trainControl`.
#' @param tune_len The length of the tuning grid; corresponds to the
#' `tuneLength` argument of `caret::train`.
#' @param distribute_cores Whether cross-validation should be performed in
#' parallel.
#' @param core_number Relevant only if `distribute_cores = TRUE`. The number of
#' cores to be used in parallel processing.
#' @param random_seed Random seed used in 1. balance adjustment, and in the
#' generation of further seeds to be used in the creationg of the
#' cross-validation folds and of the tuning grid - argument `seeds` in
#' `caret::trainControl`.
#' @param verbose Whether to print progress to console.
#' @export

train_mm <- function(df,
                     target_var,
                     features,
                     prediction_type,
                     algorithm,
                     competing_risk,
                     train_metric = 'ROC',
                     cv_folds,
                     tune_len,
                     distribute_cores,
                     core_number,
                     random_seed,
                     verbose = FALSE){


  ## Training

  # generate random seeds for reproducibility of the training process
  set.seed(random_seed)
  B <- cv_folds
  M <- tune_len
  random_seeds <- vector(mode = 'list', length = B + 1)
  for(i in 1:B) {
    random_seeds[[i]] <- sample.int(1000, M)
  }
  random_seeds[[B + 1]] <- sample.int(1000, 1)

  # put features into form that allows input to formula
  preds <- paste(features, collapse = ' + ')
  formula <- as.formula(paste0(target_var, ' ~ ', preds))

  # cross-validation specification
  if (train_metric %in% c('Sens', 'Spec', 'ROC')) {
    ctrl <- caret::trainControl(
      method = 'cv',
      number = cv_folds,
      verboseIter = verbose,
      search = 'random',
      seeds = random_seeds,
      classProbs = TRUE,
      summaryFunction = caret::twoClassSummary)

  } else {  # 'Kappa' or 'Accuracy'
    ctrl <- caret::trainControl(
      method = 'cv',
      number = cv_folds,
      verboseIter = verbose,
      search = 'random',
      seeds = random_seeds,
      classProbs = FALSE)
  }

  df[[target_var]] <- as.character(df[[target_var]])
  df[[target_var]] <- as.factor(ifelse(df[[target_var]] == '0', 'non_case', 'case'))

  if (distribute_cores == TRUE){

    library(future)
    library(doFuture)

    # ignore the unexpected random number generation warning - it is expected
    # due to the generation of `random_seeds` to be provided to `caret`
    options(doFuture.rng.onMisuse = 'ignore')

    # set doFuture as the backend for foreach
    registerDoFuture()
    plan(multisession, workers = core_number)


  }

  if (verbose == TRUE){
    if (distribute_cores == FALSE){
      core_number_char <- as.character(1)
    } else {
      core_number_char <- as.character(core_number)
    }
    print(paste0('Running cross-validation in training data using ',
                 algorithm, ' across ', core_number_char, ' CPU cores to predict ',
                 target_var, ' using ', as.character(length(features)),
                 ' features.'))
  }

  model_trained <- caret::train(formula,
                                data = df,
                                trControl = ctrl,
                                # the metric by which to optimise
                                metric = train_metric,
                                # which model are we running
                                method = algorithm,
                                tuneGrid = NULL,
                                tuneLength = tune_len)

  if (distribute_cores == TRUE){
    # stop the cluster
    plan(sequential)
  }

  return(model_trained)
}
