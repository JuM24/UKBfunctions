#' Separate the data into train and test sets
#'
#' `train_test_separation` takes a data frame with all variables required for
#' training and separates it into train and test sets. The target variable -
#' if present in the data frame - will be removed.
#' @param df The input data frame.
#' @param target The name of the column with the target variable.
#' @param train_prop Proportion of observations to be allocated to train set.
#' @param random_seed Random seed set just prior to train/test split.
#' @param impute_missing Whether missing observations should be imputed with
#' Random Forest using `missRanger::missRanger`.
#' @param imp_threads How many threads should be used for the imputation.
#' @param max_iter Number of iterations during imputation; corresponds to
#' `max.iter` argument in `missRanger` and relevant only when
#' `impute_missing = TRUE`.
#' @param num_trees Number of trees to build during imputation; corresponds to
#' `num.trees` argument in `missRanger` and relevant only when
#' `impute_missing = TRUE`.
#' @param pmm_k Number of values to sample from in predictive mean matching after
#' imputation; corresponds to `pmm.k` argument in `missRanger` and relevant only when
#' `impute_missing = TRUE`.
#' @param imp_seed Seed for the imputation; corresponds to `seed` argument in
#' `missRanger` and relevant only when `impute_missing = TRUE`.
#' @export

train_test_separation <- function(df,
                                  target,
                                  train_prop,
                                  random_seed,
                                  impute_missing,
                                  imp_threads = 1,
                                  max_iter = 10,
                                  num_trees = 500,
                                  pmm_k = 5,
                                  imp_seed){

  # save chosen specs
  if (impute_missing == TRUE){
    specs <- c(train_prop, random_seed, impute_missing)
    names(specs) = c('train_prop', 'random_seed', 'impute_missing')
  } else if (impute_missing == FALSE){
    specs <- c(train_prop, random_seed, impute_missing,
               imp_threads, max_iter, num_trees, pmm_k, imp_seed)
    names(specs) = c('train_prop', 'random_seed', 'impute_missing',
                     'imp_threads', 'max_iter', 'num_trees', 'pmm_k', 'imp_seed')
  }

  set.seed(random_seed)

  # determine the rows of the training set
  train_ids <- sample(df[['id']],
                      size = round(nrow(df) * train_prop))
  train_indices <- which(df[['id']] %in% train_ids)
  train_set <- df %>%
    filter(id %in% train_ids)

  # the rest belong to the test set
  test_ids <- df$id[!df$id %in% train_ids]
  test_indices <- which(df[['id']] %in% test_ids)
  test_set <- df %>%
    filter(id %in% test_ids)

  # remove constants
  train_set <- train_set[, sapply(train_set, function(x) length(unique(x))) > 1]
  test_set <- test_set[, sapply(test_set, function(x) length(unique(x))) > 1]

  # remove outcome if present in data
  if (target %in% colnames(df)){
    train_set[[target]] <- NULL
    test_set[[target]] <- NULL
  }

  if (impute_missing == TRUE){
    imp_train <- missRanger::missRanger(data = train_set,
                                        formula = as.formula('. ~ . - id'), # do not use id column
                                        pmm.k = pmm_k,
                                        num.trees = num_trees,
                                        num.threads = imp_threads,
                                        maxiter = max_iter,
                                        data_only = FALSE,
                                        seed = imp_seed)

    imp_test <- missRanger::missRanger(data = test_set,
                                       formula = as.formula('. ~ . - id'),
                                       pmm.k = pmm_k,
                                       num.trees = num_trees,
                                       num.threads = imp_threads,
                                       maxiter = max_iter,
                                       data_only = FALSE,
                                       seed = imp_seed)
    # create list of relevant objects to return
    return_object <- list(imp_train, train_ids, train_indices,
                          imp_test, test_ids, test_indices, specs)
    names(return_object) <- c('train_set_imputed', 'train_ids', 'train_indices',
                              'test_set_imputed', 'test_ids', 'test_indices',
                              'specs')

  } else if (impute_missing == FALSE){
    return_object <- list(train_set, train_ids, train_indices,
                          test_set, test_ids, test_indices, specs)
    names(return_object) <- c('train_set', 'train_ids', 'train_indices',
                              'test_set', 'test_ids', 'test_indices',
                              'specs')
  }
  return(return_object)
}


