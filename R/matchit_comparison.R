#' Compare performance of multiple distance-method combinations of matching
#'
#' `matchit_comparison` returns a list, that contains three lists, all with
#' different matching approaches as rows. 'Results' is the most comprehensive
#' and contains matchit objects to enable further exploration and plotting.
#' 'metrics_base' and 'metrics_int' are simplified tables with some select metrics
#' for the variables in the model and their interactions and 2-order exponentials,
#' respectively.
#' duplicated elements replaced by NAs.
#' @param df Data frame or `mids` object containing the to-be-matched data.
#' @param exposure A string indicating the name of the column with the exposure
#' variable; must be binary.
#' @param match_vars A list of strings indicating the names of columns with
#' matching variables; by default, all non-exposure columns are used.
#' @param estimand A string for the estimand: 'ATE', 'ATT', or 'ATC'.
#' @param methods A list of methods as specified in `MatchIt`.
#' @param distances A list of distance metrics or algorithms for propensity score
#' calculation as specified in `MatchIt`.
#' @param imputed Logical; whether the input data have been imputed using MI or
#' not and thus whether `MatchThem` or `MatchIt` should be implemented.
#' @param random_seed Numerical; passed to `set.seed` and `dbarts::bart2`.
#' @param verbose Logical; whether the method/distance currently evaluated
#' should be printed out.
#' @param nnet_size Size of the hidden layer for when `distance = nnet`.
#' See `nnet::nnet`.
#' @param nnet_MaxNWts Maximum number of allowed weights for when `distance = nnet`.
#' See `nnet::nnet`.
#' @param smd_thresh The threshold for the max. SMD that implies balance.
#' @export


matchit_comparison <- function(df,
                               exposure,
                               match_vars = setdiff(colnames(df), exposure),
                               estimand,
                               methods,
                               distances,
                               imputed,
                               random_seed = NULL,
                               verbose = FALSE,
                               nnet_size = 100,
                               nnet_MaxNWts = 10e5,
                               cardinality_solver = 'highs',
                               smd_thresh){

  ## matching formula
  match_formula <- as.formula(paste0(exposure,
                                     ' ~ ',
                                     paste0(match_vars, collapse = ' + ')))


  ## spec grid
  specs <- tidyr::crossing(
    method   = methods,
    distance = distances,
    estimand = estimand
  ) |>
    dplyr::mutate(
      distance = dplyr::if_else(method %in% c('cem', 'cardinality'), NA_character_, distance),
      solver = dplyr::if_else(method == 'cardinality', cardinality_solver, NA_character_),
      # special distance options for certain algorithms
      distance_options = dplyr::case_when(
        distance == 'nnet' ~ list(list(size = nnet_size, MaxNWts = nnet_MaxNWts)),
        TRUE ~ list(NULL)
      )
    ) |> dplyr::distinct() |>
  # unique ID for each spec so that seed remains constant for each spec
  dplyr::mutate(spec_id = dplyr::row_number())


  ## Separate approaches for non-MI and MI

  # specify arguments for matchit runner
  run_matching <- purrr::possibly(

    function(spec_id, method, distance, estimand,
             distance_options, solver) {

      # if seed set by user, add the spec-specific ID
      if(!is.null(random_seed)) {
        set.seed(random_seed + spec_id)

        # also pass seed to BART via distance.options
        if (identical(distance, 'bart')) {
          if (is.null(distance_options)) distance_options <- list()
          distance_options$seed <- random_seed + spec_id
        }
      }

      # whether progress should be printed
      if (isTRUE(verbose)) {
        msg_dist <- if (is.na(distance)) '' else paste0('/', distance)
        if(!isTRUE(imputed)){
          message('[MatchIt] ', method, msg_dist)
        } else if (isTRUE(imputed)){
          message('[MatchThem] ', method, msg_dist)
        }
      }

      args <- list(
        formula = match_formula,
        data = df,
        method = method,
        estimand = estimand
      )

      if (!is.na(distance)) args$distance <- distance
      if (!is.null(distance_options)) args$distance.options <- distance_options
      if (!is.na(solver) && method == 'cardinality') args$solver <- solver
      if (isTRUE(imputed)) args$approach <- 'within'

      t <- system.time({
        if(!isTRUE(imputed)){
          fit <- do.call(MatchIt::matchit, args)
        } else if (isTRUE(imputed)){
          fit <- do.call(MatchThem::matchthem, args)
        }
      })

      # return list of matchit object and runtime
      list(m = fit, runtime_sec = unname(t[['elapsed']]))
    },
    otherwise = NULL
  )

  # define a safe bal_tab function to avoid breaking if one approach errors
  bal_tab_safe <- purrr::safely(
    function(x, ...) cobalt::bal.tab(x, ...),
    otherwise = NULL
  )

  # get error message for the potential `safely` error
  err_msg <- function(x) {
    if (is.null(x$error)) '' else x$error$message
  }

  # run matching runner that we defined above
  results <- specs |>
    dplyr::mutate(
      out  = purrr::pmap(list(spec_id, method, distance, estimand,
                              distance_options, solver),
                         run_matching),
      ok = purrr::map_lgl(out, ~ !is.null(.x))
    ) |>
    dplyr::filter(ok) |> # retain just those not NA
    dplyr::mutate(
      # fetch the two objects returned by the function
      m = purrr::map(out, 'm'),
      runtime_sec = purrr::map_dbl(out, 'runtime_sec')
    ) |>
    dplyr::select(-out) |>
    dplyr::mutate(
      # compute matching metrics
      bal_base_out = purrr::map(m, ~ bal_tab_safe(.x,
                                                  int=FALSE,
                                                  poly=1,
                                                  stats=c('m','v'),
                                                  un=TRUE)),
      bal_int_out = purrr::map(m, ~ bal_tab_safe(.x,
                                                 int=TRUE,
                                                 poly=2,
                                                 stats=c('m','v'),
                                                 un=TRUE)),
      # `safely` returns two components; first, we just want the first one (result)
      bal_base = purrr::map(bal_base_out, 'result'),
      bal_int = purrr::map(bal_int_out,  'result'),
      # also extract the second component (potential error message)
      bal_base_err = purrr::map_chr(bal_base_out, err_msg),
      bal_int_err  = purrr::map_chr(bal_int_out,  err_msg)
    ) |>
    dplyr::select(-bal_base_out, -bal_int_out)



  # extract the metrics as a data frame
  metrics_base <- results |>
    dplyr::mutate(
      base = purrr::map(bal_base, matchit_extract, imputed = imputed,
                        smd_thresh = smd_thresh)
    ) |>
    dplyr::select(spec_id, method, distance, estimand, runtime_sec, base) |>
    tidyr::unnest(base, names_sep = '_', keep_empty = TRUE)

  metrics_int <- results |>
    dplyr::mutate(
      int = purrr::map(bal_int, matchit_extract, imputed = imputed,
                        smd_thresh = smd_thresh)
    ) |>
    dplyr::select(spec_id, method, distance, estimand, runtime_sec, int) |>
    tidyr::unnest(int, names_sep = '_', keep_empty = TRUE)

  output <- list(results, metrics_base, metrics_int)
  names(output) <- c('results', 'metrics_base', 'metrics_int')

  return(output)
}
