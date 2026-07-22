#' Low-memory balance table across imputations
#'
#' Internal helper for `matchit_comparison`. Computes `cobalt::bal.tab` balance
#' one imputation at a time and averages the per-imputation statistics, instead
#' of letting `cobalt` stack all `m` imputations into a single `n * m` design
#' matrix. This removes the `m` factor from the (transient) peak memory of the
#' interaction/polynomial balance step while returning the exact structure that
#' `matchit_extract()` consumes for the imputed (`Balance.Across.Imputations` /
#' `Observations`) path.
#'
#' `x` is a `mimids` object produced by `MatchThem::matchthem(..., approach =
#' 'within')`: `x$models` is a list of per-imputation `matchit` objects and
#' `x$object` is the source `mids`. Each imputation's balance is scored on its
#' own completed dataset, so only one imputation's design matrix is alive at a
#' time.
#'
#' Per-imputation `Balance` tables are aligned **by rowname** (not by position)
#' before averaging: `cobalt` can drop a collinear/constant interaction or
#' polynomial term in one imputation but not another, so the tables can differ in
#' both row count and row order. Rows are unioned across imputations (filling
#' `NA` where a term is absent) and averaged with `na.rm = TRUE`.
#'
#' Known limitation (accepted): for a near-degenerate propensity score — notably
#' `distance = 'rpart'`, whose PS takes only a couple of distinct values — this
#' single-matchit path types the `distance` row as *continuous* and reports a
#' variance ratio, whereas native `cobalt::bal.tab(mimids, ...)` types it as a
#' 2-level *categorical* distance (NA variance ratio). This affects only that one
#' `distance` row for such learners; the VR of a degenerate PS is not meaningful,
#' so the difference is documented and accepted rather than special-cased.
#' @noRd
bal_tab_mi_lowmem <- function(x, int, poly, stats = c('m', 'v'),
                              binary = 'std', un = TRUE) {

  # per-imputation matchit fits + the source mids
  fits <- x[['models']]
  if (is.null(fits)) {
    stop('bal_tab_mi_lowmem: could not find per-imputation matchit fits ',
         '(x$models) in the mimids object')
  }
  mids_obj <- x[['object']]
  m <- length(fits)

  # per-imputation named vectors (names = balance-table rownames = terms), kept
  # so they can be aligned BY NAME below rather than by position
  diff_un_l <- diff_adj_l <- vr_adj_l <- vector('list', m)
  var_names <- character(0)
  unm_ctrl <- numeric(m)
  unm_trt  <- numeric(m)

  for (i in seq_len(m)) {

    # score balance on imputation i only (single design matrix, not n * m)
    dat_i <- mice::complete(mids_obj, i)
    bt <- cobalt::bal.tab(fits[[i]], data = dat_i,
                          int = int, poly = poly, stats = stats,
                          binary = binary, un = un)

    B <- bt[['Balance']]
    O <- bt[['Observations']]

    # keep statistics keyed by term name; cobalt may drop a collinear/constant
    # interaction or poly term in some imputations, so row sets can differ in
    # count AND order across imputations -> never index by position
    rn <- rownames(B)
    diff_un_l[[i]]  <- stats::setNames(B[['Diff.Un']],      rn)
    diff_adj_l[[i]] <- stats::setNames(B[['Diff.Adj']],     rn)
    vr_adj_l[[i]]   <- stats::setNames(B[['V.Ratio.Adj']],  rn)

    # union of term names across imputations, preserving first-seen order
    var_names <- union(var_names, rn)

    # single-matchit Observations uses cols 'Control'/'Treated'
    unm_ctrl[i] <- O['Unmatched', 'Control']
    unm_trt[i]  <- O['Unmatched', 'Treated']

    rm(dat_i, bt, B, O)
  }

  # assemble [union_rows x m] matrices, aligning each imputation's values to the
  # union of term names by NAME (NA where a term is absent in that imputation)
  align <- function(lst) {
    mat <- vapply(lst, function(v) unname(v[var_names]),
                  numeric(length(var_names)))
    dim(mat) <- c(length(var_names), m)
    mat
  }
  diff_un  <- align(diff_un_l)
  diff_adj <- align(diff_adj_l)
  vr_adj   <- align(vr_adj_l)

  # average the per-imputation statistics (= cobalt's across-imputation mean).
  # Mean differences are averaged arithmetically, but variance ratios are
  # averaged on the LOG scale (geometric mean) to match cobalt, which treats
  # V-ratios multiplicatively (its balance deviation is |log(V.Ratio)|).
  # na.rm = TRUE: a term dropped in some imputations is averaged over the ones
  # where it is present; a binary var (NA variance ratio in EVERY imputation)
  # yields NaN, coerced back to NA below so the structure matches native.
  mean_vr <- exp(rowMeans(log(vr_adj), na.rm = TRUE))
  mean_vr[is.nan(mean_vr)] <- NA_real_
  Balance.Across.Imputations <- data.frame(
    Mean.Diff.Un     = rowMeans(diff_un,  na.rm = TRUE),
    Mean.Diff.Adj    = rowMeans(diff_adj, na.rm = TRUE),
    Mean.V.Ratio.Adj = mean_vr,
    row.names        = var_names,
    check.names      = FALSE
  )

  # remap Control/Treated -> 0/1 to match the mimids Observations layout
  Observations <- matrix(
    c(mean(unm_ctrl), mean(unm_trt)),
    nrow = 1,
    dimnames = list('Unmatched', c('0', '1'))
  )

  list(Balance.Across.Imputations = Balance.Across.Imputations,
       Observations = Observations)
}


#' Compare performance of multiple distance-method combinations of matching
#'
#' `matchit_comparison` returns a list, that contains three lists, all with
#' different matching approaches as rows. 'results' is the most comprehensive
#' and contains matchit objects to enable further exploration and plotting.
#' 'metrics_base' and 'metrics_int' are simplified tables with some select metrics
#' for the variables in the model and their interactions and 2-order exponentials,
#' respectively.
#'
#' Intended two-stage workflow for large data. The interaction/2nd-order balance
#' step (`cobalt::bal.tab(int = TRUE, poly = 2)`) and the retained fitted objects
#' are the two big memory costs. A screening pass can therefore run *all*
#' candidate methods with `keep_models = FALSE`, rank them on the returned
#' `metrics_base` / `metrics_int` tables, and then a final pass re-runs only the
#' best subset with `keep_models = TRUE` to obtain the fitted objects needed for
#' love / distributional plots (`cobalt::bal.plot`). For imputed (`mids`) input
#' balance is computed one imputation at a time, so the transient peak no longer
#' scales with the number of imputations.
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
#' @param cores Numerical; number of cores to run the matching on. It is
#' implemented only for matching of MI objects using `approach = 'within'`.
#' @param verbose Logical; whether the method/distance currently evaluated
#' should be printed out.
#' @param nnet_size Size of the hidden layer for when `distance = nnet`.
#' See `nnet::nnet`.
#' @param nnet_MaxNWts Maximum number of allowed weights for when `distance = nnet`.
#' See `nnet::nnet`.
#' @param cardinality_solver The name of the solver for the optimisation problem.
#' @param cardinality_time The max. time before optimisation aborts.
#' @param cardinality_tols Tolerance for balance constraints in cardinality
#' matching; increase to relax feasibility. Passed as `tols` to `matchit`.
#' @param cem_cutpoints Cutpoints specification for CEM matching. Accepts a
#' string algorithm name (e.g. `"sturges"`, `"fd"`, `"scott"`) or a named list
#' of numeric breakpoints per variable. Passed as `cutpoints` to `matchit`.
#' @param smd_thresh The threshold for the max. SMD that implies balance.
#' @param keep_models Logical; whether to retain the fitted matching objects in
#' the `m` column of `results`. Defaults to `TRUE`. Set to `FALSE` for a
#' low-memory screening pass: each fitted object is dropped as soon as its
#' balance metrics have been extracted, so `results$m` is empty but
#' `metrics_base` / `metrics_int` are still produced. The fitted objects are
#' required for distributional / love plots, so keep this `TRUE` for the final
#' run on the chosen methods.
#' @param compute_interactions Logical; whether to compute the interaction and
#' 2nd-order balance table (`metrics_int`). Defaults to `TRUE`. This is the most
#' memory-intensive step; set to `FALSE` only if `metrics_int` is not needed.
#' @return A list with four elements: `results` (a data frame containing
#' matchit objects and balance diagnostics for each method/distance
#' combination), `metrics_base` (a summary table of base-variable balance
#' metrics), `metrics_int` (a summary table of interaction and
#' second-order balance metrics), and `failures` (a table of any specs whose
#' fit errored, with the captured error message). A spec whose fit fails is not
#' dropped: it is kept as an empty dummy row in `results` (`m = NULL`, `NA`
#' runtime, `NA` metrics) so that `spec_id` stays tied to the intended
#' method/distance, and it is additionally listed in `failures`.
#' @export


matchit_comparison <- function(df,
                               exposure,
                               match_vars = setdiff(colnames(df), exposure),
                               estimand,
                               methods,
                               distances,
                               imputed,
                               random_seed = NULL,
                               cores = 1,
                               verbose = FALSE,
                               nnet_size = 100,
                               nnet_MaxNWts = 10e5,
                               cardinality_solver = 'highs',
                               cardinality_time = 1200,
                               cardinality_tols = 0.05,
                               cem_cutpoints = 'sturges',
                               smd_thresh,
                               keep_models = TRUE,
                               compute_interactions = TRUE){

  ## set up parallel processing if requested
  if (cores > 1){
    # save sequential processing to clean up after function exits
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = cores)
  }


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
      time = dplyr::if_else(method == 'cardinality', cardinality_time, NA_real_),
      tols = dplyr::if_else(method == 'cardinality', cardinality_tols, NA_real_),
      cutpoints = lapply(method, function(m) if (m == 'cem') cem_cutpoints else NULL),
      # special distance options for certain algorithms
      distance_options = dplyr::case_when(
        distance == 'nnet' ~ list(list(size = nnet_size, MaxNWts = nnet_MaxNWts)),
        TRUE ~ list(NULL)
      )
    ) |>
    dplyr::distinct() |>
    # unique ID for each spec so that seed remains constant for each spec
    dplyr::mutate(spec_id = dplyr::row_number())


  ## define a safe bal_tab function to avoid breaking if one approach errors
  bal_tab_safe <- purrr::safely(
    function(x, ...) cobalt::bal.tab(x, ...),
    otherwise = NULL
  )

  ## low-memory per-imputation balance for MI objects (see helper above),
  ## wrapped so a single failing approach does not break the run
  bal_lowmem_safe <- purrr::safely(bal_tab_mi_lowmem, otherwise = NULL)

  ## get error message for the potential `safely` error
  err_msg <- function(x) {
    if (is.null(x$error)) '' else x$error$message
  }

  ## captured as a local so `future` exports the function object to workers
  extract_fn <- matchit_extract


  ## Per-combo streaming worker: fit -> balance -> metrics -> (optionally) drop
  ## the fitted object. Doing balance + metric extraction here means that when
  ## `keep_models = FALSE` each fit is freed as soon as its metrics are taken,
  ## so only one fitted object (plus one transient balance matrix) is ever alive.
  process_spec <- function(spec_id, method, distance, estimand,
                           distance_options, solver, time, tols, cutpoints) {
    tryCatch({

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
        message()
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
      if (!is.na(time) && method == 'cardinality') args$time <- time
      if (!is.na(tols) && method == 'cardinality') args$tols <- tols
      if (!is.null(cutpoints) && method == 'cem') args$cutpoints <- cutpoints
      if (isTRUE(imputed)) args$approach <- 'within'

      # capture warnings
      warn <- character(0)

      t0 <- proc.time() # used to time each matching run
      # used to catch (and save) warning messages
      fit <- withCallingHandlers(
        {

          if(!isTRUE(imputed)){
            do.call(MatchIt::matchit, args)
          } else if (isTRUE(imputed)){
            do.call(MatchThem::matchthem, args)
          }
        },
        # this is called when a warning occurs
        warning = function(w){
          # extract the warning text; use '<<-' to assign to parent object outside function
          warn <<- c(warn, conditionMessage(w))
        }
      )
      elapsed <- unname((proc.time() - t0)[['elapsed']])

      if (isTRUE(verbose)) {
        message('  -> ', round(elapsed, 1), 's')
      }

      ## ---- balance (streamed here so the fit can be freed straight after) ----
      # MI objects go through the low-memory per-imputation path; non-MI objects
      # use cobalt directly. Base balance (int = FALSE) is cheap; the interaction
      # table (int = TRUE) is the memory hog and is skipped if not requested.
      if (isTRUE(imputed)) {
        bal_base_out <- bal_lowmem_safe(fit, int = FALSE, poly = 1)
        bal_int_out  <- if (isTRUE(compute_interactions)) {
          bal_lowmem_safe(fit, int = TRUE, poly = 2)
        } else {
          list(result = NULL, error = NULL)
        }
      } else {
        bal_base_out <- bal_tab_safe(fit, int = FALSE, poly = 1,
                                     stats = c('m', 'v'), binary = 'std', un = TRUE)
        bal_int_out  <- if (isTRUE(compute_interactions)) {
          bal_tab_safe(fit, int = TRUE, poly = 2,
                       stats = c('m', 'v'), binary = 'std', un = TRUE)
        } else {
          list(result = NULL, error = NULL)
        }
      }

      # `safely` returns two components; the first is the result (bal.tab object)
      bal_base <- bal_base_out$result
      bal_int  <- bal_int_out$result

      ## ---- metrics (extracted here so bal objects/fit need not be retained) ----
      base_metrics <- extract_fn(bal_base, imputed = imputed, smd_thresh = smd_thresh)
      int_metrics  <- extract_fn(bal_int,  imputed = imputed, smd_thresh = smd_thresh)

      # keep or drop the fitted object; free memory before the next combo
      keep_fit <- if (isTRUE(keep_models)) fit else NULL
      if (!isTRUE(keep_models)) {
        rm(fit)
        gc(verbose = FALSE)
      }

      list(
        m            = keep_fit,
        runtime_sec  = elapsed,
        warnings     = warn,
        bal_base     = bal_base,
        bal_int      = bal_int,
        bal_base_err = err_msg(bal_base_out),
        bal_int_err  = err_msg(bal_int_out),
        base_metrics = base_metrics,
        int_metrics  = int_metrics,
        fit_error    = ''
      )

    }, error = function(e) {
      if (isTRUE(verbose)) {
        msg_dist <- if (is.na(distance)) '' else paste0('/', distance)
        message('[FAILED] ', method, msg_dist, ': ', e$message)
      }
      # return a placeholder (not NULL) so the failed spec keeps its row/spec_id
      # as an empty dummy and its error is recorded, rather than vanishing.
      list(
        m            = NULL,
        runtime_sec  = NA_real_,
        warnings     = character(0),
        bal_base     = NULL,
        bal_int      = NULL,
        bal_base_err = '',
        bal_int_err  = '',
        base_metrics = NULL,
        int_metrics  = NULL,
        fit_error    = conditionMessage(e)
      )
    })
  }


  ## run the per-combo worker (parallel for MI within-approach, else sequential)
  out_list <- if (cores > 1) {
    furrr::future_pmap(
      list(specs$spec_id, specs$method, specs$distance, specs$estimand,
           specs$distance_options, specs$solver, specs$time, specs$tols,
           specs$cutpoints),
      process_spec,
      .options = furrr::furrr_options(
        seed = if (is.null(random_seed)) TRUE else random_seed)
    )
  } else {
    purrr::pmap(
      list(specs$spec_id, specs$method, specs$distance, specs$estimand,
           specs$distance_options, specs$solver, specs$time, specs$tols,
           specs$cutpoints),
      process_spec
    )
  }

  ## assemble the results table from the streamed worker output. Every spec keeps
  ## its row (and `spec_id`) even if its fit failed: a failed spec becomes an
  ## empty dummy (m = NULL, NA runtime, NULL balance/metrics) with its error
  ## captured in `fit_error`, so spec_id stays tied to the intended method and
  ## nothing is silently dropped.
  results <- specs |>
    dplyr::mutate(
      out = out_list,
      # fetch the objects returned by the worker
      m            = purrr::map(out, 'm'),
      runtime_sec  = purrr::map_dbl(out, 'runtime_sec'),
      warnings     = purrr::map(out, 'warnings'),
      # useful to filter out non-warning approaches after the function runs
      has_warning  = purrr::map_lgl(warnings, ~ length(.x) > 0),
      bal_base     = purrr::map(out, 'bal_base'),
      bal_int      = purrr::map(out, 'bal_int'),
      bal_base_err = purrr::map_chr(out, 'bal_base_err'),
      bal_int_err  = purrr::map_chr(out, 'bal_int_err'),
      # non-empty when the fit itself failed and this row is an empty dummy
      fit_error    = purrr::map_chr(out, 'fit_error'),
      # pre-computed metric rows (one-row tibbles, or NULL)
      base         = purrr::map(out, 'base_metrics'),
      int          = purrr::map(out, 'int_metrics')
    ) |>
    dplyr::select(-out)


  ## extract the metrics as a data frame. When a metric list-column is entirely
  ## NULL (e.g. `int` when compute_interactions = FALSE) there is no structure to
  ## unnest, so we drop the column to leave a clean identifiers-only table rather
  ## than a stray list-column.
  metrics_base <- results |>
    dplyr::select(spec_id, method, distance, estimand, runtime_sec, base)
  metrics_base <- if (any(!purrr::map_lgl(metrics_base$base, is.null))) {
    tidyr::unnest(metrics_base, base, names_sep = '_', keep_empty = TRUE)
  } else {
    dplyr::select(metrics_base, -base)
  }

  metrics_int <- results |>
    dplyr::select(spec_id, method, distance, estimand, runtime_sec, int)
  metrics_int <- if (any(!purrr::map_lgl(metrics_int$int, is.null))) {
    tidyr::unnest(metrics_int, int, names_sep = '_', keep_empty = TRUE)
  } else {
    dplyr::select(metrics_int, -int)
  }

  # drop the pre-computed metric list-columns from the returned results table
  results <- results |>
    dplyr::select(-base, -int)

  ## record of specs whose fit failed (and are therefore empty dummy rows in
  ## `results`), so drops are visible rather than silent
  failures <- results |>
    dplyr::filter(fit_error != '') |>
    dplyr::select(spec_id, method, distance, estimand, error = fit_error)

  output <- list(results, metrics_base, metrics_int, failures)
  names(output) <- c('results', 'metrics_base', 'metrics_int', 'failures')

  return(output)
}
