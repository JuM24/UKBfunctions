#' Create a subset of the UKB synthetic data for your project
#'
#' `extract_from_synthetic` takes a table with field IDs, required .Rds files
#' with UKB synthetic data, and a data-type lookup table and returns a subset
#' of the synthetic data.
#' table exporter
#' @param field_table .xlsx file. A table of IDs required for the project;
#' must contain a column with numerical field IDs as per the UKB showcase.
#' @param data_dict .csv file. A data-type lookup table that contains two columns:
#' `field_id` which is a numerical field ID as per the UKB showcase and
#' `type` which indicates the variable type for a given field - `datetime`,
#' `int`, `real`, `string`, and `hes`; the latter refers to any fields that use
#' HES inpatient tables.'
#' @param id_column_name The name of the column in `field_table` that contains
#' the field IDs.
#' @param data_path The path for the .Rds files with synthetic UKB data.
#' @param out_path An optional path to write `masterfile_syn.csv` to, the finished
#' data frame that is returned by this function.
#' @export

extract_from_synthetic <- function(field_table,
                                   data_dict,
                                   id_column_name,
                                   data_path,
                                   out_path = NULL){

  # load in field with required variables
  field_ids <- readxl::read_excel(field_table) |>
    dplyr::filter(!is.na(.data[[id_column_name]])) |>
    dplyr::distinct(.data[[id_column_name]], .keep_all = TRUE)
  field_ids[[id_column_name]] <- as.character(field_ids[[id_column_name]])

  # create column that indicates source of field ID
  field_id_types <- read.csv(data_dict)
  field_id_types$field_id <- as.character(field_id_types$field_id)
  field_ids <- merge(field_ids, field_id_types,
                     by.x = id_column_name, by.y = 'field_id',
                     all.x = TRUE)

  # determine sources to be read in and extracted from
  sources_used <- unique(field_ids$type[!is.na(field_ids$type)])



  ## determine variable class to extract from correct data frame

  # dates get read in anyhow to extract ids
  dates <- readRDS(file.path(data_path, 'datetime_fields_ALL.Rds'))
  masterfile <- data.frame(dates$eid)
  colnames(masterfile) <- 'eid'

  if ('datetime' %in% sources_used){
    # determine required IDs for this source
    date_ids <- paste0('X',
                       dplyr::filter(field_ids, type == 'datetime') |>
                         dplyr::pull(.data[[id_column_name]]),
                       '.')
    dates_relevant <- dates |>
      dplyr::select(eid, dplyr::starts_with(date_ids))
    masterfile <- merge(masterfile, dates_relevant, by ='eid', all.x = TRUE,
                        sort = FALSE)

  }
  rm(dates); gc()

  # integer data fields
  if ('int' %in% sources_used){
    integer_ids <- paste0('X',
                          dplyr::filter(field_ids, type == 'int') |>
                            dplyr::pull(.data[[id_column_name]]),
                          '.')
    ints <- readRDS(file.path(data_path, 'integer_fields_ALL.Rds'))
    ints_relevant <- ints |>
      dplyr::select(eid, dplyr::starts_with(integer_ids))
    masterfile <- merge(masterfile, ints_relevant, by ='eid', all.x = TRUE,
                        sort = FALSE)
    rm(ints); gc()
  }

  # real number data fields
  if ('real' %in% sources_used){
    real_ids <- paste0('X',
                       dplyr::filter(field_ids, type == 'real') |>
                         dplyr::pull(.data[[id_column_name]]),
                       '.')
    reals <- readRDS(file.path(data_path, 'real_fields_ALL.Rds'))
    reals_relevant <- reals |>
      dplyr::select(eid, dplyr::starts_with(real_ids))
    masterfile <- merge(masterfile, reals_relevant, by ='eid', all.x = TRUE,
                        sort = FALSE)
    rm(reals); gc()
  }

  # string data fields
  if ('string' %in% sources_used){
    string_ids <- paste0('X',
                         dplyr::filter(field_ids, type == 'string') |>
                           dplyr::pull(.data[[id_column_name]]),
                         '.')
    strings <- readRDS(file.path(data_path, 'string_fields_ALL.Rds'))
    strings_relevant <- strings |>
      dplyr::select(eid, dplyr::starts_with(string_ids))
    masterfile <- merge(masterfile, strings_relevant, by ='eid', all.x = TRUE,
                        sort = FALSE)
    rm(strings); gc()
  }

  # HES files
  if ('hes' %in% sources_used){
    hes_ids <- paste0('X',
                      dplyr::filter(field_ids, type == 'hes') |>
                        dplyr::pull(.data[[id_column_name]]),
                      '.')
    hes <- readRDS(file.path(data_path, 'HES_ALL.Rds'))
    hes_relevant <- hes |>
      dplyr::select(eid, dplyr::starts_with(hes_ids))
    masterfile <- merge(masterfile, hes_relevant, by ='eid', all.x = TRUE,
                        sort = FALSE)
    rm(hes); gc()
  }

  if (!is.null(out_path)){
    write.csv(masterfile, file.path(out_path, 'masterfile_syn.csv'),
              row.names = FALSE)
  }

  return(masterfile)
}
