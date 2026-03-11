#' Create a long-format inpatient dataset
#'
#' `extract_diagnoses` takes a data frame with UKB data and returns a data frame
#' with select inpatient information.
#' @param df Input data frame with columns containing inpatient data. See details
#' for formatting requirements.
#' @param source Data source; either 'gp' or 'inpatient'.
#' @param raw 0 or 1; indicates whether the data is in its raw form - wide format
#' with each participant as a row - or already in the long format. Valid only
#' when working with inpatient diagnoses; GP diagnoses are always long.
#' @param invalid_dates An array of strings, indicating the diagnosis dates
#' that should be treated as `NA`; their indicators will also be `NA`.
#' Each date must be class character in the date form '%Y-%m-%d'.
#' @param colname_id Character; the name of the column with participant IDs.
#' @param code_table Path to a .csv file or a data frame that contains as the
#' first two columns 1. diagnostic codes as strings and 2. the coding
#' format as string. If `code_table` is `NULL`, no diagnoses are extracted.
#' @param keep_all logical; if TRUE, all instances of the same diagnosis - as
#' indicated in the `code_table` - for any participant are kept; if FALSE,
#' only the earliest one is kept.
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
#' "icd9", "icd10", "read2", "read3"
#' @export

extract_diagnoses <- function(df,
                              source,
                              raw = 1,
                              invalid_dates = c('1900-01-01', '1901-01-01',
                                                '1902-02-02', '1903-03-03',
                                                '2037-07-07'),
                              colname_id = 'eid',
                              code_table = NULL,
                              keep_all = TRUE,
                              out_path = NULL,
                              out_file_name = NULL){

  if (raw == 1 & source == 'inpatient'){

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
  }
  if (source == 'gp' && raw == 1) stop("`raw = 1` is not valid when `source = 'gp'`; GP diagnoses are always in long format.")

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
      df$date <- as.Date(df$date, format = '%Y-%m-%d')

      # sort by date so that distinct() keeps the earliest occurrence
      df <- df |> dplyr::arrange(date)
      if (!keep_all) {
        df <- dplyr::distinct(df, .data[[colname_id]], code, coding, .keep_all = TRUE)
      }

      # count instances per code; optionally match codes with descriptions
      code_table$n <- 0L
      if (all(c('description', 'variable') %in% colnames(code_table))) {
        df$description <- NA_character_
        df$diagnosis   <- NA_character_
      }
      for (d in c('icd9', 'icd10')) {
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

      # reshape to long format: pivot read_2 / read_3 into code + coding columns
      df <- df |>
        dplyr::select(tidyselect::all_of(colname_id), event_dt, read_2, read_3) |>
        tidyr::pivot_longer(c(read_2, read_3),
                            names_to  = 'coding',
                            values_to = 'code',
                            values_drop_na = TRUE) |>
        dplyr::mutate(coding = dplyr::recode(coding, read_2 = 'read2', read_3 = 'read3')) |>
        dplyr::rename(date = event_dt)

      # retain only the relevant codes
      df <- dplyr::filter(df,
                          (coding == 'read2' & code %in% code_table$code[code_table$source == 'read2']) |
                            (coding == 'read3' & code %in% code_table$code[code_table$source == 'read3'])) |>
        dplyr::mutate(date = dplyr::if_else(date %in% invalid_dates, NA_character_, date))
      df$date <- as.Date(df$date, format = '%Y-%m-%d')

      # sort by date so that distinct() keeps the earliest occurrence
      df <- df |> dplyr::arrange(date)
      if (!keep_all) {
        df <- dplyr::distinct(df, .data[[colname_id]], code, coding, .keep_all = TRUE)
      }

      # count instances per code; optionally match codes with descriptions
      code_table$n <- 0L
      if (all(c('description', 'variable') %in% colnames(code_table))) {
        df$description <- NA_character_
        df$diagnosis   <- NA_character_
      }
      for (d in c('read2', 'read3')) {
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
  return(df)
}
