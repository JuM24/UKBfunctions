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
    ) |> dplyr::distinct()


  ## Separate approaches for non-MI and MI

  # specify arguments for matchit runner
  run_matching <- purrr::possibly(

    function(method, distance, estimand,
             distance_options, solver) {

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
  # run matching runner that we defined above
  results <- specs |>
    dplyr::mutate(
      out  = purrr::pmap(list(method, distance, estimand,
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
      bal_base = purrr::map(m, ~ cobalt::bal.tab(.x,
                                                 int = FALSE,
                                                 poly = 1,
                                                 stats = c('m', 'v'),
                                                 un = TRUE)),

      bal_int = purrr::map(m, ~ cobalt::bal.tab(.x,
                                                int = TRUE,
                                                poly = 2,
                                                stats = c('m', 'v'),
                                                un = TRUE))
    )


  # extract the metrics as a data frame
  metrics_base <- results |>
    dplyr::mutate(
      base = purrr::map(bal_base, matchit_extract, imputed = imputed,
                        smd_thresh = smd_thresh)
    ) |>
    dplyr::select(method, distance, estimand, runtime_sec, base) |>
    tidyr::unnest(base, names_sep = '_')

  metrics_int <- results |>
    dplyr::mutate(
      int = purrr::map(bal_int, matchit_extract, imputed = imputed,
                        smd_thresh = smd_thresh)
    ) |>
    dplyr::select(method, distance, estimand, runtime_sec, int) |>
    tidyr::unnest(int, names_sep = '_')

  output <- list(results, metrics_base, metrics_int)
  names(output) <- c('results', 'metrics_base', 'metrics_int')

  return(output)
}
