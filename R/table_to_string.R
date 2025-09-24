#' Create final features out of the variables
#'
#' `table_to_string` takes a table and transforms it into a string for the UKB
#' table exporter
#' @param file_path File path of the table.
#' @param id_column_name The name of the column that contains field IDs.
#' @param instance_column_name The name of the column that contains the number
#' of instances.
#' @param output_path The path, including full file name, where to write the file.
#' @export

table_to_string <- function(file_path,
                            id_column_name,
                            instance_column_name,
                            output_path){
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

  # remove rows with NA field IDs; create array of names
  df[is.na(df[[instance_column_name]]), instance_column_name] <- 1
  df <- df[!is.na(df[[id_column_name]]), ]
  id_list <- df[[id_column_name]]

  # add extra names for variables with multiple instances
  df_multi_instance <- df[df[[instance_column_name]] != 1, ]
  for (id_name in df_multi_instance[[id_column_name]]){
    instance_n <- df_multi_instance[df_multi_instance[[id_column_name]] == id_name,
                                    instance_column_name]
      for (i in seq(0, instance_n-1)){
        id_list <- id_list[id_list != id_name]
        id_list <- c(id_list, paste0(id_name, '_i', as.character(i)))
      }
    }

  # add "p" to field id
  id_list[id_list != 'eid'] <- paste0('p', id_list[id_list != 'eid'])

  return(paste(unlist(id_list), collapse = ', '))
}
