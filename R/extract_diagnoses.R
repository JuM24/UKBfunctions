#' Create a long-format inpatient dataset
#'
#' `extract_diagnoses` takes a data frame with UKB data and returns a data frame
#' with select inpatient information.
#' @param df Input data frame with columns containing inpatient data. See details
#' for formatting requirements.
#' @param source Data source; either 'gp' or 'inpatient'.
#' @param wide 0 or 1; indicates whether the data is in its wide format
#' with each participant as a row - or already in the long format. Valid only
#' when working with inpatient diagnoses; GP diagnoses are always long.
#' @param invalid_dates An array of strings, indicating the diagnosis dates
#' that should be treated as `NA`; their indicators will also be `NA`.
#' Each date must be class character in the date form '%Y-%m-%d'.
#' @param date_form Character; the date format string passed to [as.Date()],
#' e.g. `'%Y-%m-%d'` (default) or `'%d/%m/%Y'`. The strings in `invalid_dates`
#' must be provided in this same format.
#' @param colname_id Character; the name of the column with participant IDs.
#' @param code_table Path to a .csv file or a data frame that contains as the
#' first two columns 1. diagnostic codes as strings and 2. the coding
#' format as string. If `code_table` is `NULL`, no diagnoses are extracted.
#' @param keep_all Logical; if `TRUE`, all instances of the same diagnosis - as
#' indicated in the `code_table` - for any participant are kept; if FALSE,
#' only the earliest one is kept.
#' @param return_code_table Logical; if `TRUE`, `code_table` the output is a list,
#' with the second element the `code_table` with numbers of instances for each
#' diagnosis.
#' @param out_path An optional path to the folder to which the output
#' data frame is to be written.
#' @param out_file_name The file name for the data frame if it is to be exported;
#' don't forget to include '.csv' at the end.
#' @details
#' If `wide = 1`, `df` must contain data on UKB field IDs 41270, 41271, 41280,
#' and 41281. The column names must be in the form X.ID.0.a, where X is the
#' letter "X", ID is the field ID, and a is the instance.
#' #' E.g., "X41270.0.1", "X41270.0.2",etc. Alternatively, if `wide = 0`, `df`
#' must contain 4 columns: participant ID column as character, diagnostic code as
#' character, date of diagnosis as character, and coding as character. The coding
#' indicates the coding system - ICD9, ICD10, etc. - for the diagnosis.
#'
#' For `source = 'gp'`, `df` must be in long format with a participant ID column,
#' an `event_dt` date column, and the raw coding columns `read2`, `ctv3`, and
#' optionally `snomed`. Only the coding columns present in `df` are reshaped, so a
#' missing one is skipped without error.
#'
#' The allowed coding systems in the `code_table` depend on `source`: for
#' `source = 'inpatient'`, "icd9" and "icd10"; for `source = 'gp'`, "read2",
#' "ctv3", and "snomed". The code table need not contain all coding systems; any
#' subset is accepted and missing codings are skipped without error.
#' @return A data frame in long format with columns for participant ID,
#' diagnostic code, date, and coding system. When `return_code_table = TRUE`,
#' a list with elements `diagnoses` (the data frame) and `code_table` (the
#' code table with instance counts).
#' @export

extract_diagnoses <- function(df,
                              source,
                              wide = 1,
                              invalid_dates = c('1900-01-01', '1901-01-01',
                                                '1902-02-02', '1903-03-03',
                                                '2037-07-07'),
                              date_form = '%Y-%m-%d',
                              colname_id = 'eid',
                              code_table = NULL,
                              keep_all = TRUE,
                              return_code_table = FALSE,
                              out_path = NULL,
                              out_file_name = NULL){

  if (wide == 1 & source == 'inpatient'){

    df <- df |>
      dplyr::select(all_of(colname_id),
                    dplyr::starts_with(c('X41270.', 'X41280.',
                                         'X41271.', 'X41281.')))

    # remove NAs and change everything into characters
    df <- df |>
      dplyr::mutate(dplyr::across(-tidyselect::all_of(colname_id),
                                  as.character)) |>
      dplyr::mutate(dplyr::across(-tidyselect::all_of(colname_id),
                                  ~ dplyr::na_if(.x, '')))


    # build only the coding blocks whose UKB field columns are present, so a
    # completely-absent ICD9 or ICD10 block does not break the reshape. The code
    # and date fields of each coding are assumed to ship together (as UKB HES does),
    # so presence is keyed on the code field.
    blocks <- list()

    if (any(startsWith(colnames(df), 'X41271.'))) {
      # separate diagnosis codes and dates, then transform to long-type format
      icd9_long <- df |>
        dplyr::select(all_of(colname_id), dplyr::starts_with('X41271.')) |>
        tidyr::pivot_longer(-tidyselect::all_of(colname_id),
                            names_to = 'column', values_drop_na = TRUE) |>
        dplyr::rename(code = value)
      icd9_long$column <- sub('^X41271\\.', '', icd9_long$column)

      icd9_date_long <- df |>
        dplyr::select(all_of(colname_id), dplyr::starts_with('X41281.')) |>
        tidyr::pivot_longer(-tidyselect::all_of(colname_id),
                            names_to = 'column', values_drop_na = TRUE) |>
        dplyr::rename(date = value)
      icd9_date_long$column <- sub('^X41281\\.', '', icd9_date_long$column)

      icd9 <- merge(icd9_long, icd9_date_long, by = c(colname_id, 'column'))
      icd9$column <- NULL; icd9$coding <- 'icd9'
      blocks$icd9 <- icd9
    }

    if (any(startsWith(colnames(df), 'X41270.'))) {
      icd10_long <- df |>
        dplyr::select(all_of(colname_id), dplyr::starts_with('X41270.')) |>
        tidyr::pivot_longer(-tidyselect::all_of(colname_id),
                            names_to = 'column', values_drop_na = TRUE) |>
        dplyr::rename(code = value)
      icd10_long$column <- sub('^X41270\\.', '', icd10_long$column)

      icd10_date_long <- df |>
        dplyr::select(all_of(colname_id), dplyr::starts_with('X41280.')) |>
        tidyr::pivot_longer(-tidyselect::all_of(colname_id),
                            names_to = 'column', values_drop_na = TRUE) |>
        dplyr::rename(date = value)
      icd10_date_long$column <- sub('^X41280\\.', '', icd10_date_long$column)

      icd10 <- merge(icd10_long, icd10_date_long, by = c(colname_id, 'column'))
      icd10$column <- NULL; icd10$coding <- 'icd10'
      blocks$icd10 <- icd10
    }

    df <- dplyr::bind_rows(blocks) |>
      dplyr::select(eid, date, coding, code)
  }
  if (source == 'gp' && wide == 1) stop("`wide = 1` is not valid when `source = 'gp'`; GP diagnoses are always in long format.")

  if (!is.null(code_table)){
    if (source == 'inpatient'){
      # read in code table as .csv file or data frame and rename its first two columns
      if (is.character(code_table)) code_table <- read.csv(code_table)
      colnames(code_table)[1:2] <- c('code', 'source')

      # retain only the relevant codes
      df <- dplyr::filter(df,
                          (coding == 'icd9'  & code %in% code_table$code[code_table$source == 'icd9']) |
                            (coding == 'icd10' & code %in% code_table$code[code_table$source == 'icd10'])) |>
        dplyr::mutate(date = dplyr::if_else(date %in% invalid_dates, NA_character_, date))
      df$date <- as.Date(df$date, format = date_form)

      # sort by date so that distinct() keeps the earliest occurrence
      df <- df |> dplyr::arrange(date)
      if (!keep_all) {
        df <- dplyr::distinct(df, .data[[colname_id]], .keep_all = TRUE)
      }

      # count instances per code; optionally match codes with descriptions
      code_table$n <- rep(0L, nrow(code_table))
      if (all(c('description', 'variable') %in% colnames(code_table))) {
        df$description <- NA_character_
        df$diagnosis   <- NA_character_
      }
      for (d in intersect(c('icd9', 'icd10'), code_table$source)) {
        for (code_val in code_table$code[code_table$source == d]) {
          rows   <- df$coding == d & df$code == code_val
          ct_row <- code_table$source == d & code_table$code == code_val
          code_table$n[ct_row] <- sum(rows)
          if (all(c('description', 'variable') %in% colnames(code_table))) {
            df$description[rows] <- code_table$description[ct_row]
            df$diagnosis[rows]   <- code_table$variable[ct_row]
          }
        }
      }
    } else if (source == 'gp') {
      # read in code table as .csv file or data frame and rename its first two columns
      if (is.character(code_table)) code_table <- read.csv(code_table)
      colnames(code_table)[1:2] <- c('code', 'source')

      # reshape to long format: pivot the raw coding columns into code + coding
      # columns. Only the columns present in `df` are reshaped, so a missing coding
      # (e.g. no snomed) does not error. `gp_col_map` maps raw UKB GP column name to
      # the coding label used in `code_table`.
      gp_col_map <- c(read2 = 'read2', ctv3 = 'ctv3', snomed = 'snomed')
      gp_cols    <- intersect(names(gp_col_map), colnames(df))

      df <- df |>
        dplyr::select(tidyselect::all_of(colname_id), event_dt,
                      tidyselect::all_of(gp_cols)) |>
        tidyr::pivot_longer(tidyselect::all_of(gp_cols),
                            names_to  = 'coding',
                            values_to = 'code',
                            values_drop_na = TRUE) |>
        dplyr::mutate(coding = unname(gp_col_map[coding])) |>
        dplyr::rename(date = event_dt) |>
        dplyr::mutate(date = as.character(date)) # to allow NA_character inclusion below

      # retain only the relevant codes
      df <- dplyr::filter(df,
                          (coding == 'read2'  & code %in% code_table$code[code_table$source == 'read2']) |
                            (coding == 'ctv3'   & code %in% code_table$code[code_table$source == 'ctv3']) |
                            (coding == 'snomed' & code %in% code_table$code[code_table$source == 'snomed'])) |>
        dplyr::mutate(date = dplyr::if_else(date %in% invalid_dates, NA_character_, date))
      df$date <- as.Date(df$date, format = date_form)

      # sort by date so that distinct() keeps the earliest occurrence
      df <- df |> dplyr::arrange(date)
      if (!keep_all) {
        df <- dplyr::distinct(df, .data[[colname_id]], .keep_all = TRUE)
      }

      # count instances per code; optionally match codes with descriptions
      code_table$n <- rep(0L, nrow(code_table))
      if (all(c('description', 'variable') %in% colnames(code_table))) {
        df$description <- NA_character_
        df$diagnosis   <- NA_character_
      }
      for (d in intersect(c('read2', 'ctv3', 'snomed'), code_table$source)) {
        for (code_val in code_table$code[code_table$source == d]) {
          rows   <- df$coding == d & df$code == code_val
          ct_row <- code_table$source == d & code_table$code == code_val
          code_table$n[ct_row] <- sum(rows)
          if (all(c('description', 'variable') %in% colnames(code_table))) {
            df$description[rows] <- code_table$description[ct_row]
            df$diagnosis[rows]   <- code_table$variable[ct_row]
          }
        }
      }
    }
  }

  if (!is.null(out_path)) {
    if (is.null(out_file_name)) stop('Provide `out_file_name` when `out_path` is set.')
    write.csv(df, file.path(out_path, out_file_name), row.names = FALSE)
  }
  # potentially return the code table populated with number of diagnoses
  if (return_code_table) return(list(diagnoses = df, code_table = code_table))
  return(df)
}
