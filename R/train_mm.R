#' Train the prediction algorithm, potentially correcting for class imbalance.
#'
#' `train_mm` Takes and input data frame, potentially corrects for class
#' imbalance of the target variable, and trains a predictive model using
#' the supplied features.
#' @param df The input data frame.
#' @param target_var The target variable to be predicted.
#' @param amend_features Whether features should be amended; relevant only for
#' self-report data.
#' @param min_age The minimum age of the sample. Determines whether participants
#' below a certain age should be dropped prior to training.
#' @param algorithm The algorithm to be used for prediction.
#' @param cv_folds The number of folds to be used during cross-validation;
#' corresponds to the argument `number` in `caret::trainControl`.
#' @param tune_len The length of the tuning grid; corresponds to the
#' `tuneLength` argument of `caret::train`.
#' @param distribute_cores Whether cross-validation should be performed in
#' parallel.
#' @param core_number Relevant only if `distribute_cores = TRUE`. The number of
#' cores to be used in parallel processing.
#' #' @param random_seed Random seed used in 1. balance adjustment, 2. registering
#' doRNG for reproducible parallel processing, and 3. generation of further
#' seeds to be used in the generation of the cross-validation folds and of the
#' tuning grid - argument `seeds` in `caret::trainControl`.
#' @param imbalance_correct Whether potential class imbalance between levels
#' of the target variables should be corrected.
#' @param balance_prop Relevant only if `imbalance_correct = TRUE`; the desired
#' proportion of the total observations for the current minority class.
#' @param ordinals Relevant only if `imbalance_correct = TRUE`; a list of all
#' ordinal variables in the data frame.
#' @param smote_K Relevant only if `imbalance_correct = TRUE`; determines the
#' K used in KNN during SMOTE.
#' @export

train_mm <- function(df,
                     target_var,
                     features,
                     amend_features = FALSE,
                     min_age,
                     algorithm,
                     cv_folds,
                     tune_len,
                     distribute_cores,
                     core_number,
                     random_seed,
                     imbalance_correct = NULL,
                     balance_prop,
                     ordinals,
                     smote_K){

  if (amend_features == TRUE){
    # amend features with additional data
    df <- amend_mm_features(df = df)
  }

  # potentially correct class imbalance
  df <- correct_balance(df = df,
                        target_var = target_var,
                        approach = imbalance_correct,
                        balance_prop = balance_prop,
                        random_seed = random_seed,
                        ordinals = ordinals,
                        K = smote_K)
#TODO CHECK SEX
  if (!is.null(min_age)){
    # remove younger participants
    df <- df %>%
      filter(ass_age >= min_age)
  }

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
  ctrl_args <- list(method = 'cv', number = cv_folds,
                    verboseIter = verbose, search = 'random',
                    seeds = random_seeds)
  ctrl <- do.call(caret::trainControl, ctrl_args)


  if (distribute_cores == TRUE){

    library(doParallel)
    library(foreach)

    # create set of workers running in parallel
    cl <- makeCluster(core_number)

    # when a parallelised loop is encountered, distribute the task in the above cluster
    registerDoParallel(cl)
    doRNG::registerDoRNG(random_seed)

  }

  model_trained <- caret::train(formula,
                                data = df,
                                trainControl = ctrl,
                                # the metric by which to optimise
                                metric = ifelse(is.factor(df[[target_var]]),
                                                'Accuracy', 'RMSE'),
                                # which model are we running
                                method = algorithm,
                                tuneGrid = NULL,
                                tuneLength = tune_len)

  if (distribute_cores == TRUE){
    # stop the cluster
    stopCluster(cl)
  }

  return(model_trained)
}
