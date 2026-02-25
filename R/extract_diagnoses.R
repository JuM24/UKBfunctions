#' Create a long-format inpatient dataset
#'
#' `extract_diagnoses` takes a data frame with UKB data and returns a data frame
#' with select inpatient information.
#' @param df Input data frame with columns containing inpatient data. See details
#' for formatting requirements.
#' @param raw 0 or 1; indicates whether the data is in its raw form - wide format
#' with each participant as a row - or already in the long format.
#' @param colname_id Character; the name of the column with participant IDs.
#' @param code_table Path to a .csv file or a data frame with two columns:
#' the first with diagnostic codes as strings and a second with the coding
#' format, also as string. See details.
#' @param out_path An optional path to the folder to which the output
#' data frame is to be written.
#' @param out_file_name The file name for the data frame if it is to be exported;
#' don't forget to include '.csv' at the end.
#' @details
#' If `raw = 1`, `df` must contain data on UKB field IDs 41270, 41271, 41280,
#' and 41281. The column names must be in the form X.ID.0.a, where X is the
#' letter "X", ID is the field ID, and a is the instance.
#' #' E.g., "X41270.0.1", "X41270.0.2",etc. Alternatively, if `raw = 0`, `df`
#' must contain 4 columns: participant ID column as character, diagnostic code as
#' character, date of diagnosis as character, and coding as character. The coding
#' indicates the coding system - ICD9, ICD10, etc. - for the diagnosis.
#'
#' The allowed inputs for coding systems in the `code_table` data frame are
#' "icd9" and "icd10".
#' @export

extract_diagnoses <- function(df,
                              raw = 1,
                              colname_id = 'eid',
                              code_table = NULL,
                              out_path = NULL,
                              out_file_name = NULL){

  if (raw == 1){

    df <- masterfile_syn |>
      dplyr::select(all_of(colname_id),
                    dplyr::starts_with(c('X41270.', 'X41280.',
                                         'X41271.', 'X41281.')))

    # remove NAs and change everything into characters
    df <- df |>
      dplyr::mutate(dplyr::across(-tidyselect::all_of(colname_id),
                                  as.character)) |>
      dplyr::mutate(dplyr::across(-tidyselect::all_of(colname_id),
                                  ~ dplyr::na_if(.x, '')))


    # separate sources of df (ICD9 vs 10) and dates vs. diagnosis codes
    icd9 <- df |> dplyr::select(all_of(colname_id),
                                  dplyr::starts_with('X41271.'))
    icd9_date <- df |> dplyr::select(all_of(colname_id),
                                       dplyr::starts_with('X41281.'))
    icd10 <- df |> dplyr::select(all_of(colname_id),
                                   dplyr::starts_with('X41270.'))
    icd10_date <- df |> dplyr::select(all_of(colname_id),
                                        dplyr::starts_with('X41280.'))

    # transform to long-type format
    icd9_long <- icd9 |>  tidyr::pivot_longer(-tidyselect::all_of(colname_id),
                                              names_to = 'column',
                                              values_drop_na=TRUE) |>
      dplyr::rename(code = value)
    icd9_long$column <- sub('^X41271\\.', '', icd9_long$column)

    icd9_date_long <- icd9_date |>  tidyr::pivot_longer(-tidyselect::all_of(colname_id),
                                                        names_to = 'column',
                                                        values_drop_na=TRUE) |>
      dplyr::rename(date = value)
    icd9_date_long$column <- sub('^X41281\\.', '', icd9_date_long$column)

    icd10_long <- icd10 |>  tidyr::pivot_longer(-tidyselect::all_of(colname_id),
                                                names_to = 'column',
                                                values_drop_na=TRUE) |>
      dplyr::rename(code = value)
    icd10_long$column <- sub('^X41270\\.', '', icd10_long$column)

    icd10_date_long <- icd10_date |>  tidyr::pivot_longer(-tidyselect::all_of(colname_id),
                                                          names_to = 'column',
                                                          values_drop_na=TRUE) |>
      dplyr::rename(date = value)
    icd10_date_long$column <- sub('^X41280\\.', '', icd10_date_long$column)

    # combine all df
    icd9 <- merge(icd9_long, icd9_date_long,
                  by = c(colname_id, 'column'))
    icd9$column <- NULL; icd9$coding <- 'icd9'
    icd10 <- merge(icd10_long, icd10_date_long,
                   by = c(colname_id, 'column'))
    icd10$column <- NULL; icd10$coding <- 'icd10'

    df <- rbind(icd9, icd10)
  } else if (raw == 0){
    # TODO
    stop('raw=0 not implemented')
  }

  if (!is.null(out_path)) {
    if (is.null(out_file_name)) stop('Provide `out_file_name` when `out_path` is set.')
    write.csv(df, file.path(out_path, out_file_name), row.names = FALSE)
  }
  return(df)
}
