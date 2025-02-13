#' Create the MM data frame
#' 
#' `create_mm_masterfile` returns a data frame with participants as rows
#' and binary indicators for each disorder as columns.
#' @param min_age The minimum allowed age of participants.
#' @param mm_source The source of disease history.
#' @param mm_codes_file The path to the codes for each MM.
#' @param random_seed The number of `set.seed`.
#' @export

create_mm_masterfile <- function(min_age,
                              mm_source,
                              mm_codes_file,
                              random_seed = 24){
  
  # main raw data
  main_vars <- readRDS('main_vars.Rds')
  
  # remove participants that have exited the study
  opt_outs <- read.csv('D://Job/Raw data/participant_opt_out.csv', header = FALSE)
  main_vars <- main_vars %>%
    filter(!eid %in% opt_outs$V1) %>%
    rename(id = eid)
  
  
  
  ## remove participants that were younger than XX at assessment
  main_vars$birth_date <- as.Date(paste0('01/', 
                                         as.character(main_vars$X52.0.0),
                                         '/' ,
                                         as.character(main_vars$X34.0.0)), 
                                  format = '%d/%m/%Y')
  
  # calculate number of days in each birth month
  main_vars$days_in_birth_month <- 
    as.integer(format(main_vars$birth_date + months(1) - days(1), '%d'))
  
  # set random day for day of birth
  set.seed(random_seed)
  main_vars$random_day <- apply(main_vars, 1, function(row) {
    sample(1:row['days_in_birth_month'], 1)
  })
  main_vars$birth_date <- as.Date(paste0(as.character(main_vars$random_day),
                                         '/', 
                                         as.character(main_vars$X52.0.0),
                                         '/' ,
                                         as.character(main_vars$X34.0.0)), 
                                  format = '%d/%m/%Y')
  main_vars$ass_age <- as.numeric(difftime(main_vars$X53.0.0, main_vars$birth_date,
                                           units = 'days'))/365.25
  # restrict to those older or as old as `min_age`
  main_vars <- main_vars[main_vars$ass_age >= min_age, ]
  
  
  
  
  if (mm_source == 'self_report'){
    
    #### ----------------------------------------- ####
    ####  Get field IDs for self-reported illness  ####
    #### ----------------------------------------- ####
    
    ## categories 20001 and 20002
    
    # import self-report disease codes
    mm_codes <- read.csv('mm_self_report.csv') %>%
      filter(!is.na(code)) %>%
      mutate(code = as.character(code))
    # non-cancer self-report disorders from category 20002
    dis_self <- create_diseases_self(df = main_vars,
                                     mm_codes = mm_codes,
                                     cancer = FALSE)
    # cancer self-report cases from category 20001
    dis_self_cancer <- create_diseases_self(df = main_vars,
                                            mm_codes = mm_codes,
                                            cancer = TRUE)
    # merge cancer and other disorders
    dis_self <- merge(dis_self, dis_self_cancer, by = 'id', all = TRUE)
    # NAs introduced through merge to 0
    dis_self[is.na(dis_self)] <- 0
    # to chars
    dis_self <- dis_self %>%
      mutate(across(everything(), as.factor))
    
    
    
    #### -----------------------------------------####
    ####  Create final features from self-report  ####
    #### -----------------------------------------####
    
    
    # select only those disorders reported during UKB baseline
    dis_other <- main_vars %>%
      select(id, starts_with(c('X2443.0', 'X2956.0', 'X6150.0', 'X2247.0', 'X2257.0',
                               'X3404.0', 'X3414.0', 'X3571.0', 'X3741.0', 
                               'X3773.0', 'X6152.0', 'X3799.0',
                               'X4067.0', 'X4792.0', 'X6148.0', 'X5441.0', 
                               'X5912.0', 'X5934.0', 'X6119.0', 'X6159.0', 'X6148.0',
                               'X3393.0', 'X20404.', 'X20406.', 'X20456.', 'X20503.',
                               'X20400.', 'X20401.')))
    ## diabetes
    dis_other$diab_add_1 <- NA
    dis_other$diab_add_1[dis_other$X2443.0.0 == 1] <- 1
    dis_other$diab_add_1[dis_other$X2443.0.0 == 0] <- 0
    # diabetic eye disease (1) gets coded as diabetes
    dis_other <- dis_other %>%
      mutate(diab_add_2 = if_else(rowSums(select(., starts_with('X6148.0.')) == 1, na.rm = TRUE) > 0, 1, 0))
    # non-answer to NA
    dis_other$diab_add_2[dis_other$X6148.0.0 %in% c(-1, -3)] <- NA
    
    ## pain
    dis_other <- dis_other %>%
      rename(pain_add_1 = X2956.0.0, pain_add_2 = X3404.0.0, pain_add_3 = X3414.0.0,
             pain_add_4 = X3571.0.0, pain_add_5 = X3741.0.0, pain_add_6 = X3773.0.0,
             pain_add_7 = X3799.0.0, pain_add_8 = X4067.0.0)
    # determine NAs
    for (col in colnames(select(dis_other, starts_with('pain_add_')))){
      # by default, no answer is no pain
      dis_other[is.na(dis_other[[col]]), col] <- 0
      # those that did not want to answer the questions or 
      # who left the original question (6159) blank are NA
      dis_other[dis_other$X6159.0.0 == -3 | 
                  is.na(dis_other$X6159.0.0) |
                  dis_other[[col]] %in% c(-1, -3), col] <- NA
    }
    
    ## coronary heart disease
    dis_other <- dis_other %>%
      mutate(chd_add_1 = if_else(rowSums(select(., starts_with('X6150.0.')) == 1 |
                                           select(., starts_with('X6150.0.')) == 2, na.rm = TRUE) > 0, 1, 0))
    # non-answer to NA
    dis_other$chd_add_1[dis_other$X6150.0.0 == -3] <- NA
    
    ## COPD
    dis_other <- dis_other %>%
      mutate(copd_add_1 = if_else(rowSums(select(., starts_with('X6152.0.')) == 6, na.rm = TRUE) > 0, 1, 0))
    # non-answer to NA
    dis_other$copd_add_1[dis_other$X6152.0.0 == -3] <- NA
    
    ## stroke
    dis_other <- dis_other %>%
      mutate(stroke_add_1 = if_else(rowSums(select(., starts_with('X6150.0.')) == 3, na.rm = TRUE) > 0, 1, 0))
    # non-answer to NA
    dis_other$stroke_add_1[dis_other$X6150.0.0 == -3] <- NA
    
    ## hypertension
    dis_other <- dis_other %>%
      mutate(hypertension_add_1 = if_else(rowSums(select(., starts_with('X6150.0.')) == 4, na.rm = TRUE) > 0, 1, 0))
    # non-answer to NA
    dis_other$hypertension_add_1[dis_other$X6150.0.0 == -3] <- NA
    
    ## hearing problems
    dis_other <- dis_other %>%
      rename(hear_add_1 = X2247.0.0,
             hear_add_2 = X2257.0.0,
             hear_add_3 = X4792.0.0,
             hear_add_4 = X3393.0.0)
    dis_other$hear_add_1[dis_other$hear_add_1 %in% c(-1, -3)] <- NA
    dis_other$hear_add_1[dis_other$hear_add_1 == 99] <- 1
    dis_other$hear_add_2[dis_other$hear_add_2 %in% c(-1, -3)] <- NA
    dis_other$hear_add_3[dis_other$hear_add_3 == -3] <- NA
    dis_other$hear_add_4[dis_other$hear_add_4 == -3] <- NA
    
    ## vision problems
    dis_other <- dis_other %>%
      mutate(vision_add_1 = if_else(rowSums(select(., starts_with('X6148.0.')) == 3 |
                                              select(., starts_with('X6148.0.')) == 4 |
                                              select(., starts_with('X6148.0.')) == 5 |
                                              select(., starts_with('X6148.0.')) == 6, na.rm = TRUE) > 0, 1, 0))
    # non-answer to NA
    dis_other$vision_add_1[dis_other$X6148.0.0 %in% c(-1, -3)] <- NA
    
    ## glaucoma
    dis_other <- dis_other %>%
      mutate(glaucoma_add_1 = if_else(rowSums(select(., starts_with('X6148.0.')) == 2, na.rm = TRUE) > 0, 1, 0))
    # non-answer to NA
    dis_other$glaucoma_add_1[dis_other$X6148.0.0 %in% c(-1, -3)] <- NA
    
    ## asthma
    dis_other <- dis_other %>%
      mutate(asthma_add_1 = if_else(rowSums(select(., starts_with('X6152.0.')) == 8, na.rm = TRUE) > 0, 1, 0))
    # non-answer to NA
    dis_other$asthma_add_1[dis_other$X6152.0.0 == -3] <- NA
    
    ## alcohol issues
    # TODO: taken from later period due to low original n - discuss with others
    dis_other <- dis_other %>%
      rename(alc_add_1 = X20406.0.0)
    dis_other$alc_add_1[dis_other$alc_add_1 %in% c(-818, -121)] <- NA
    dis_other$alc_add_1[dis_other$X20401.0.0 == 0] <- 0
    
    ## drug issues
    # TODO: taken from later period due to low original n - discuss with others
    dis_other <- dis_other %>%
      rename(drugs_add_1 = X20456.0.0,
             drugs_add_2 = X20503.0.0)
    dis_other$drugs_add_1[dis_other$drugs_add_1 %in% c(-818, -121)] <- NA
    dis_other$drugs_add_2[dis_other$drugs_add_2 %in% c(-818, -121)] <- NA
    dis_other$drugs_add_1[dis_other$X20401.0.0 == 0] <- 0
    dis_other$drugs_add_2[dis_other$X20401.0.0 == 0] <- 0
    
    # remove unnecessary columns and convert to factor
    dis_other <- dis_other %>%
      select(-starts_with('X')) %>%
      mutate(across(everything(), as.factor))
    
    # merge with main disorder file
    # (no NAs will be introduced to variables previously in `dis_other` because `dis_self`
    # is a subset of `dis_other`; however, NAs will be introduced for variables
    # present in `dis_self` before the merge, so we have to change them to 0)
    nas_to_zero <- setdiff(colnames(dis_self), colnames(dis_other))
    dis_self <- merge(dis_self, dis_other, by = 'id', all = TRUE)
    
    dis_self <- dis_self %>%
      mutate(across(all_of(nas_to_zero), ~ replace_na(., '0')))
    
    return(dis_self)
  }
}