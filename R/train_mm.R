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
#' @param max_followup The time within which the outcome that is to be
#' predicted should occur. If not set to `NULL`, the input data frame must
#' contain a column called 'followup' which is the time in years between baseline
#' and right-censoring or event occurrence, whichever occurs first.
#' @param algorithm The algorithm to be used for prediction.
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
#' #' @param random_seed Random seed used in 1. balance adjustment, 2. registering
#' doRNG for reproducible parallel processing, and 3. generation of further
#' seeds to be used in the generation of the cross-validation folds and of the
#' tuning grid - argument `seeds` in `caret::trainControl`.
#' @param verbose Whether to print progress to console.
#' @param imbalance_correct Whether potential class imbalance between levels
#' of the target variables should be corrected.
#' @param balance_prop Relevant only in the case of imbalance correction; the desired
#' proportion of the total observations for the current minority class.
#' @param ordinals Relevant only when `imbalance_correct = 'SMOTE'`; a list of all
#' ordinal variables in the data frame.
#' @param smote_K Relevant only when `imbalance_correct = 'SMOTE'`; determines the
#' K used in KNN during SMOTE.
#' @export

train_mm <- function(df,
                     target_var,
                     features,
                     amend_features = FALSE,
                     min_age,
                     max_followup = NULL,
                     algorithm,
                     train_metric = 'ROC',
                     cv_folds,
                     tune_len,
                     distribute_cores,
                     core_number,
                     random_seed,
                     verbose = FALSE,
                     imbalance_correct = NULL,
                     balance_prop = NULL,
                     ordinals = NULL,
                     smote_K = NULL){


  # remove participants that experienced the outcome after the follow-up cutoff
  if (is.null(max_followup)){
    max_followup <- c(NULL, max(df$followup))
  } else {
    df <- df %>%
      filter(followup <= max_followup)
  }

  # remove unused variable
  df <- df %>%
    select(-followup)

  # save model specifications to be returned
  specs <- c(amend_features, min_age, max_followup, random_seed,
             imbalance_correct, balance_prop, smote_K)

  if (!is.null(imbalance_correct)){
    if(imbalance_correct == 'SMOTE'){
      names(specs) = c('amend_features', 'min_age', 'max_followup', 'random_seed',
                       'imbalance_correct', 'balance_prop', 'smote_K')

    } else if (imbalance_correct == 'downsample'){
      specs <- c(amend_features, min_age, max_followup, random_seed,
                 imbalance_correct, balance_prop)
      names(specs) = c('amend_features', 'min_age', 'max_followup', 'random_seed',
                       'imbalance_correct', 'balance_prop')
    }
  } else if (is.null(imbalance_correct)){
    specs <- c(amend_features, min_age, max_followup, random_seed)
    names(specs) = c('amend_features', 'min_age', 'max_followup', 'random_seed')

  }

  if (amend_features == TRUE){
    # amend features with additional data
    df <- amend_mm_features(df = df,
                            verbose = verbose)
  }

  # save age mean and SD to scale later on
  old_age_mean <- mean(df$ass_age, na.rm = TRUE)
  old_age_sd <- sd(df$ass_age, na.rm = TRUE)

  # potentially correct class imbalance
  df <- correct_balance(df = df,
                        target_var = target_var,
                        approach = imbalance_correct,
                        balance_prop = balance_prop,
                        random_seed = random_seed,
                        verbose = verbose,
                        ordinals = ordinals,
                        K = smote_K)

  # normalise and centre (not necessary if SMOTE has been run)
  if (is.null(imbalance_correct)){
    num_cols <- sapply(df, is.numeric)
    df[num_cols] <- scale(df[num_cols])
  } else if (imbalance_correct == 'downsample'){
    num_cols <- sapply(df, is.numeric)
    df[num_cols] <- scale(df[num_cols])
  }

  # remove younger participants
  if (!is.null(min_age)){
    old_nrow <- nrow(df)
    # convert min_age to the corresponding scaled cutoff
    scaled_min_age <- (min_age - old_age_mean) / old_age_sd
    df <- df %>%
      filter(ass_age >= scaled_min_age)
    if (verbose == TRUE){
      print(paste0('Removed ', as.character(old_nrow - nrow(df)), ' participants younger than ', as.character(min_age), '.'))
      cat('\n')
    }
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
  ctrl <- caret::trainControl(method = 'cv', number = cv_folds,
                              verboseIter = verbose, search = 'random',
                              seeds = random_seeds, classProbs = TRUE,
                              summaryFunction = caret::twoClassSummary)
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

  return_object <- list(model_trained, specs)
  names(return_object) <- c('model_trained', 'specs')

  return(return_object)
}
