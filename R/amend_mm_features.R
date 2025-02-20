#' Correct class imbalance
#'
#' `amend_mm_features` takes the data frame and amends the MM features with
#' additional data.
#' @param df The input data frame.
#' @param mm_source The source of disease history.
#' @export

amend_mm_features <- function(df,
                              mm_source = 'self_report'){

  adds <- paste0(rep('add_', 9), as.character(seq(1,9)))
  colnames(select(df, ends_with(adds)))

  # pain
  # TODO: a lot of additions - maybe double-check with Lucy
  df$pain[df$pain_add_1 == '1' |
                   df$pain_add_2 == '1' |
                   df$pain_add_3 == '1' |
                   df$pain_add_4 == '1' |
                   df$pain_add_5 == '1' |
                   df$pain_add_6 == '1' |
                   df$pain_add_7 == '1' |
                   df$pain_add_7 == '1'] <- '1'

  # hearing
  df$hearing <- '0'
  df$hearing[df$hear_add_1 == '1' |
                      df$hear_add_2 == '1' |
                      df$hear_add_3 == '1' |
                      df$hear_add_4 == '1'] <- '1'
  df$hearing <- as.factor(df$hearing)

  # alcohol
  df$alcohol[df$alc_add_1 == '1'] <- '1'

  # drugs
  df$drugs[df$drugs_add_1 == '1' |
                    df$drugs_add_2 == '1'] <- '1'

  # diabetes
  df$diabetes[df$diab_add_1 == '1' |
                       df$diab_add_2 == '1'] <- '1'

  # CHD
  df$CHD[df$chd_add_1 == '1'] <- '1'

  # COPD
  df$COPD[df$copd_add_1 == '1'] <- '1'

  # stroke
  df$stroke[df$stroke_add_1 == '1'] <- '1'

  # hypertension
  df$hypertension[df$hypertension_add_1 == '1'] <- '1'

  # vision
  colnames(df)[colnames(df) == 'vision_add_1'] <- 'vision'

  # glaucoma
  df$glaucoma[df$glaucoma_add_1 == '1'] <- '1'

  # asthma
  df$asthma[df$asthma_add_1 == '1'] <- '1'

  # now that we have the features, remove the variables used to construct them
  df <- df[ , !grepl('_add_[0-9]$', names(df))]

  return(df)
}
