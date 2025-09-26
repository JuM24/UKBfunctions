#' Create final features out of the variables
#'
#' `table_to_string` takes a table and transforms it into a string for the UKB
#' table exporter
#' @param file_path File path of the table.
#' @param id_column_name The name of the column that contains field IDs.
#' @param instance_column_name The name of the column that contains the number
#' of instances - i0, i1, etc.
#' @param repeats_low Column name for the starting digit for repeats which are
#' 0 or 1 for a0 and a1.
#' @param repeats_high Column name for the last digit for repeats.
#' @param keep_assessments Which assessments are to be kept; accepts 'all' and '0'.
#' @export

table_to_string <- function(file_path,
                            id_column_name,
                            instance_column_name,
                            repeats_low,
                            repeats_high,
                            keep_assessments = 'all'){
  # detect input table format
  suffix <- tools::file_ext(file_path)

  tryCatch({
    if (suffix %in% c('xlsx', 'xls')){
      df <- as.data.frame(readxl::read_excel(file_path))
    } else if (suffix == 'csv'){
      df <- read.csv(file_path)
    } else {
      stop('Unsupported file type: ', suffix, '.')
    }
  }, error = function(e){
    message('Error while reading file: ', e$message)
    NULL
  })

  # remove duplicate field IDs
  df <- distinct(df)

  # those without entries for instances and repeats get 1s
  df[is.na(df[[instance_column_name]]), instance_column_name] <- 1
  df[is.na(df[[repeats_low]]), repeats_low] <- 1
  df[is.na(df[[repeats_high]]), repeats_high] <- 1

  # remove rows with NA field IDs
  df <- df[!is.na(df[[id_column_name]]), ]

  # create array of field ID names
  id_list <- df[[id_column_name]]

  # add extra names for variables with multiple instances
  #df_multi_instance <- df[df[[instance_column_name]] != 1, ]
  for (id_name in df[[id_column_name]]){
    print(paste(as.character(counter), id_name))
    counter <- counter + 1
    # check if instance and repeat suffix needs to be added
    instance_n <- df[df[[id_column_name]] == id_name, instance_column_name]
    start_a <- df[df[[id_column_name]] == id_name, repeats_low]
    stop_a <- df[df[[id_column_name]] == id_name, repeats_high]
    if (instance_n > 1){
      id_list <- id_list[id_list != id_name]
      for (i in seq(0, instance_n-1)){
        new_name <- paste0(id_name, '_i', as.character(i))
        if (stop_a > start_a){
          for (a in seq(start_a, stop_a)){
            new_name <- paste0(new_name, '_a', as.character(a))
            id_list <- c(id_list, new_name)
            new_name <- paste0(id_name, '_i', as.character(i))
          }
        } else{ # we add the new element even if no repeats
          id_list <- c(id_list, new_name)
        }
      }
    } else{ # for the rare cases without multiple instances but with repeats
      if (stop_a > start_a){
        for (a in seq(start_a, stop_a)){
          id_list <- id_list[id_list != id_name]
          new_name <- paste0(id_name, '_a', as.character(a))
          id_list <- c(id_list, new_name)
        }
      }
    }
  }

  # add "p" to field id
  id_list[id_list != 'eid'] <- paste0('p', id_list[id_list != 'eid'])

  # add option to keep just a single assessment instance
  if (keep_assessments == '0'){
    mask <- grepl('i[1-9]', id_list)
    id_list <- id_list[!mask]
  }

  return(paste(unlist(id_list), collapse = ', '))
}
