#' Reads in Excel table/sheet; detects format based on file suffix.
#'
#' `read_tbl` takes as input the path to an `.xlsx,` `.xls`, or `.csv` table and
#' returns as the output the same table as an R dataframe.
#' @param tbl_path Path to the table.
#'
#' @export

read_tbl <- function(tbl_path){


  # determine file suffix
  suffix <- tools::file_ext(tbl_path)

  # choose function to load in; throw error if unsupported file suffix
  tryCatch({
    if (suffix %in% c('xlsx', 'xls')){
      tbl_file <- as.data.frame(readxl::read_excel(tbl_path))
    } else if (suffix == 'csv'){
      tbl_file <- read.csv(tbl_path)
    } else {
      stop('Unsupported file type: ', suffix, '.')
    }
  }, error = function(e){
    message('Error while reading file: ', e$message)
    NULL
  })

  return(tbl_file)
}
