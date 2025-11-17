#' Sets birth date.
#'
#' `set_birth_date` takes as input the data frame and the names of columns
#' representing year of birth and month of birth, respectively. It returns
#' the input data frame with a new column for date of birth
#' @param df Data frame.
#' @param yob Year of birth column name.
#' @param mob Month of birth column name.
#' @param day Whether the same day for all participants or a random day for each
#' participant is set. Set to `random` by default; otherwise a number between 1
#' and 28 can be input.
#' @param random_seed Random seed for selecting the day.
#'
#' @export

set_birth_date <- function(df,
                           yob,
                           mob,
                           day = 'random',
                           random_seed = 246){

  if (is.numeric(day)){
    # can at be at most 28 for any month
    if  (length(day) != 1L || day < 1 || day > 28) {
      stop('`day` cannot be higher than 28.')

    } else{
      day_char <- as.character(day)

      df$birth_date <- as.Date(paste0(day_char,
                                      '/',
                                      as.character(df[[mob]]),
                                      '/' ,
                                      as.character(df[[yob]])),
                               format = '%d/%m/%Y')
    }

  } else if (is.character(day) && identical(day, 'random')){

    # temporary birth date with the first day of the month
    df$birth_date <- as.Date(paste0('01',
                                    '/',
                                    as.character(df[[mob]]),
                                    '/' ,
                                    as.character(df[[yob]])),
                             format = '%d/%m/%Y')

    # calculate number of days in each birth month
    df$days_in_birth_month <- lubridate::days_in_month(df$birth_date)

    # 1 plus uniform distribution (0-1) * max number of days
    # gets us a valid random day for that month
    set.seed(random_seed)
    df$random_day <- 1L + floor(runif(nrow(df)) * df$days_in_birth_month)

    # build date
    df$birth_date <- lubridate::make_date(
      year  = df[[yob]],
      month = df[[mob]],
      day   = df$random_day)

    df$days_in_birth_month <- NULL
    df$random_day <- NULL

  } else{
    stop('`day` must be either `random` or a single integer between 1 and 28.')
  }

  return(df)
}
