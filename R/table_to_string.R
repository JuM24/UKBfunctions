#' Create final features out of the variables
#'
#' `table_to_string` takes a table and transforms it into a string for the UKB
#' table exporter
#' @param file_path File path of the table.
#' @param id_column_name The name of the column that contains field IDs.
#' @param instances_low Column name for the starting digit of instances.
#' @param instances_high Column name for the last digit of instances.
#' @param repeats_low Column name for the starting digit for repeats which are
#' 0 or 1 for a0 and a1.
#' @param repeats_high Column name for the last digit for repeats.
#' @param keep_assessments Which assessments are to be kept; accepts 'all' and '0'.
#' @export

table_to_string <- function(file_path,
                            id_column_name,
                            instances_low,
                            instances_high,
                            repeats_low,
                            repeats_high,
                            keep_assessments = 'all'
){

  # import table
  df <- UKBfunctions::read_tbl(file_path)

  # remove duplicate field IDs
  n_before <- nrow(df)
  df <- df |>
    dplyr::distinct(.data[[id_column_name]], .keep_all = TRUE)
  # throw warning if any rows removed
  if (nrow(df) < n_before) {
    warning(sprintf(
      "distinct() removed %d duplicate row(s) based on '%s'.",
      n_before - nrow(df), id_column_name
    ), call. = FALSE)
  }

  # remove rows with NA field IDs
  df <- df[!is.na(df[[id_column_name]]), ]

  # throw warning if high is NA and low is not or vice-versa
  bad_instances <- xor(is.na(df[[instances_low]]),  is.na(df[[instances_high]]))
  bad_repeats   <- xor(is.na(df[[repeats_low]]),  is.na(df[[repeats_high]]))

  if (any(bad_instances)) {
    warning(sprintf(
      'Inconsistent instances range in %d row(s): one of "%s"/"%s" is NA but the other is not.',
      sum(bad_instances), instances_low, instances_high
    ), call. = FALSE)
  }

  if (any(bad_repeats)) {
    warning(sprintf(
      'Inconsistent repeats range in %d row(s): one of "%s"/"%s" is NA but the other is not.',
      sum(bad_repeats), repeats_low, repeats_high
    ), call. = FALSE)
  }

  # create array of field ID names
  id_list <- c()

  # add extra names for variables with multiple instances
  for (id_name in df[[id_column_name]]){
    row <- df[[id_column_name]] == id_name

    start_i <- as.integer(df[[instances_low]][row])
    stop_i <- as.integer(df[[instances_high]][row])

    start_a <- as.integer(df[[repeats_low]][row])
    stop_a <- as.integer(df[[repeats_high]][row])

    # if no instances or id variable, then no suffix
    if (((is.na(start_i) || is.na(stop_i)) && (is.na(start_a) || is.na(stop_a))) ||
        id_name %in% c('id', 'eid')){
      id_list <- c(id_list, id_name)

      # if multiple instances, add digit for each
    } else if (!is.na(start_i) && !is.na(stop_i) && stop_i >= start_i){
      for (i in seq(start_i, stop_i)){
        new_name <- paste0(id_name, '_i', as.character(i))
        # if multiple repeats, also add digit for each
        if (!is.na(start_a) && !is.na(stop_a) && stop_a >= start_a){
          for (a in seq(start_a, stop_a)){
            new_name <- paste0(new_name, '_a', as.character(a))
            id_list <- c(id_list, new_name)
            new_name <- paste0(id_name, '_i', as.character(i))
          }
        } else{ # we add the new element even if no repeats
          id_list <- c(id_list, new_name)
        }
      }
    } else{ # we add the new element even if no repeats
      if (!is.na(start_a) && !is.na(stop_a) && stop_a >= start_a){
        for (a in seq(start_a, stop_a)){
          new_name <- paste0(id_name, '_a', as.character(a))
          id_list <- c(id_list, new_name)
        }
      } else{ # we add the new element even if no repeats
        id_list <- c(id_list, id_name)
      }
    }
  }

  # add "p" to field id
  id_list[!id_list %in% c('eid', 'id')] <-
    paste0('p', id_list[!id_list %in% c('eid', 'id')])

  # add option to keep just a single assessment instance
  if (keep_assessments == '0'){
    mask <- grepl('_i(?!0)\\d+', id_list, perl = TRUE)
    id_list <- id_list[!mask]
  }

  return(paste(unlist(id_list), collapse = ', '))
}
