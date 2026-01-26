#' Extract simplified table of matching metrics within `matchit_comparison`.
#'
#' `matchit_extract` takes as input the 'results' object of the `matchit_comparison`
#' function to create a data frame with simplified matching metrics,
#' with matching approaches as rows and metrics as columns.
#' @param bal The input list; should be the 'results' object in the output list
#' of `matchit_comparison`.
#' @param impute Whether the matching was performed on an imputed dataset.
#' @param smd_thresh The SMD threshold for what is considered a good match.
#' @export


matchit_extract <- function(bal, imputed, smd_thresh = 0.05) {

  if (is.null(bal)) return(NULL)

  O <- bal[['Observations']]

  if (isTRUE(imputed)) {
    # MI: use balance across imputations (mean across imputations columns)
    B <- bal[['Balance.Across.Imputations']]

    smd_un  <- abs(B[['Mean.Diff.Un']])
    smd_adj <- abs(B[['Mean.Diff.Adj']])
    vr <- B[['Mean.V.Ratio.Adj']]

    discarded_control <- O['Unmatched', '0']
    discarded_treated <- O['Unmatched', '1']

  } else {
    # non-MI objects: standard balance table
    B <- bal[['Balance']]

    smd_un  <- abs(B[['Diff.Un']])
    smd_adj <- abs(B[['Diff.Adj']])
    vr <- B[['V.Ratio.Adj']]

    discarded_control <- O['Unmatched', 'Control']
    discarded_treated <- O['Unmatched', 'Treated']
  }

  vars <- rownames(B)

  # SMD metrics
  mean_smd <- mean(smd_adj, na.rm = TRUE)
  mean_smd_dif <- mean(smd_un, na.rm = TRUE) - mean_smd

  max_smd <- max(smd_adj, na.rm = TRUE)
  max_smd_idx <- which.max(replace(smd_adj, is.na(smd_adj), -Inf))
  max_smd_var <- if (length(max_smd_idx) == 0 ||
                     is.infinite(max_smd_idx)) NA_character_ else vars[max_smd_idx]


  n_thresh <- sum(smd_adj > smd_thresh, na.rm = TRUE)

  # Variance ratio metrics (drop NAs from binary vars)
  vr_logdev <- abs(log(vr)) # to identify furthest from 1
  vr_logdev[is.na(vr_logdev) | is.infinite(vr_logdev)] <- NA_real_

  worst_vr <- if (all(is.na(vr_logdev))) NA_real_ else exp(max(vr_logdev, na.rm = TRUE))
  worst_vr_idx <- which.max(replace(vr_logdev, is.na(vr_logdev), -Inf))
  worst_vr_var <- if (length(worst_vr_idx) == 0) NA_character_ else vars[worst_vr_idx]

  mean_vr <- if (all(is.na(vr))) NA_real_ else mean(vr, na.rm = TRUE)

  discarded_total <- discarded_control + discarded_treated

  tibble::tibble(
    discarded_treated = discarded_treated,
    discarded_control = discarded_control,
    discarded_total   = discarded_total,
    max_smd           = max_smd,
    max_smd_var  = max_smd_var,
    mean_smd          = mean_smd,
    n_thresh          = n_thresh,
    mean_smd_dif      = mean_smd_dif,
    worst_vr          = worst_vr,
    worst_vr_var = worst_vr_var,
    mean_vr           = mean_vr
  )
}
