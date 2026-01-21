#' Compare performance of multiple distance-method combinations of matching
#'
#' `matchit_comparison` returns a list, with different matching approaches as
#' rows and performance metrics as columns. Implements `MatchIt` and `MatchtThem`.
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
#' @param s.weights Survey weights if appropriate.
#' @param imputed Logical; whether the input data have been imputed using MI or
#' not and thus whether `MatchThem` or `MatchIt` should be implemented.
#' @param verbose Logical; whether the method/distance currently evaluated
#' should be printed out.
#' @param nnet_size Size of the hidden layer for when `distance = nnet`.
#' See `nnet::nnet`.
#' @param nnet_MaxNWts Maximum number of allowed weights for when `distance = nnet`.
#' See `nnet::nnet`.
#' @export


matchit_comparison <- function(df,
                               exposure,
                               match_vars = setdiff(colnames(df), exposure),
                               estimand,
                               methods,
                               distances,
                               s.weights = NULL,
                               imputed,
                               verbose = FALSE,
                               nnet_size = 100,
                               nnet_MaxNWts = 10e5,
                               cardinality_solver = 'highs'){
  library(tidyverse)

  ## matching formula
  match_formula <- as.formula(paste0(exposure,
                                     ' ~ ',
                                     paste0(match_vars, collapse = ' + ')))


  ## spec grid
  specs <- crossing(
    method   = methods,
    distance = distances,
    estimand = estimand,
  ) %>%
    mutate(
      distance = if_else(method %in% c('cem', 'cardinality'), NA_character_, distance),
      # special distance options for certain algorithms
      distance_options = case_when(
        distance == 'nnet' ~ list(list(size = nnet_size, MaxNWts = nnet_MaxNWts)),
        distance == 'cardinality' ~ list(list(solver = cardinality_solver)),
        TRUE ~ list(NULL)
      )
    ) %>% distinct()


  ## Separate approaches for non-MI and MI

  if (imputed == FALSE){

    # specify arguments for matchit runner
    run_matchit <- purrr::possibly(

      function(method, distance, estimand, distance_options) {

        # whether progress should be printed
        if (isTRUE(verbose)) {
          msg_dist <- if (is.na(distance)) '' else paste0('/', distance)
          message('[MatchIt] ', method, msg_dist)
        }

        args <- list(
          formula = match_formula,
          data = df,
          method = method,
          estimand = estimand
        )

        if (!is.na(distance)) args$distance <- distance
        if (!is.null(distance_options)) args$distance.options <- distance_options

        t <- system.time({
          fit <- do.call(MatchIt::matchit, args)
        })

        # return list of matchit object and runtime
        list(m = fit, runtime_sec = unname(t[['elapsed']]))
      },
      otherwise = NULL
    )
    # run matchit runner that we defined above
    results <- specs %>%
      mutate(
        out  = purrr::pmap(list(method, distance, estimand, distance_options), run_matchit),
        ok = purrr::map_lgl(out, ~ !is.null(.x))
      ) %>%
      filter(ok) %>% # retain just those not NA
      mutate(
        # fetch the two objects returned by the function
        m = purrr::map(out, 'm'),
        runtime_sec = purrr::map_dbl(out, 'runtime_sec')
      ) %>%
      select(-out) %>%
      mutate(
        # compute matching metrics
        bal_base = map(m, ~ cobalt::bal.tab(.x,
                                            int = FALSE,
                                            poly = 1,
                                            stats = c('m', 'v'),
                                            un = TRUE)),

        bal_int = map(m, ~ cobalt::bal.tab(.x,
                                           int = TRUE,
                                           poly = 2,
                                           stats = c('m', 'v'),
                                           un = TRUE))
      )


    # for MI
  } else {

    # specify arguments for matchthem runner
    run_matchthem <- purrr::possibly(
      function(method, distance, estimand, distance_options) {

        if (isTRUE(verbose)) {
          msg_dist <- if (is.na(distance)) '' else paste0('/', distance)
          message('[MatchThem] ', method, msg_dist)
        }

        args <- list(
          formula  = match_formula,
          data     = imp,          # mids object
          approach = 'within',
          method   = method,
          estimand = estimand
        )

        if (!is.na(distance)) args$distance <- distance
        if (!is.null(distance_options)) args$distance.options <- distance_options

        t <- system.time({
          fit <- do.call(MatchThem::matchthem, args)
        })

        list(m = fit, runtime_sec = unname(t[['elapsed']]))
      },
      otherwise = NULL
    )
    # run matchthem runner
    results_mi <- specs %>%
      mutate(
        out = purrr::pmap(list(method, distance, estimand, distance_options), run_matchthem),
        ok  = purrr::map_lgl(out, ~ !is.null(.x))
      ) %>%
      filter(ok) %>%
      mutate(
        m = purrr::map(out, 'm'),
        runtime_sec = purrr::map_dbl(out, 'runtime_sec')
      ) %>%
      select(-out) %>%
      mutate(
        bal_base = map(m, ~ cobalt::bal.tab(.x,
                                            int = FALSE,
                                            poly = 1,
                                            stats = c('m', 'v'),
                                            un = TRUE)),
        bal_int  = map(m, ~ cobalt::bal.tab(.x,
                                            int = TRUE,
                                            poly = 2,
                                            stats = c('m', 'v'),
                                            un = TRUE))
      )
  }
}
