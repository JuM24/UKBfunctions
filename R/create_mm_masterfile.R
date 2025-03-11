#' Create the MM data frame
#'
#' `create_mm_masterfile` takes the data frame with all the UKB data and
#' returns a data frame with participants as rows and binary indicators for
#' each disorder as columns.
#' @param df The input data frame.
#' @param time_zero The date at which the diagnoses are to be ascertained; the
#' required format is '%d/%m/%Y' If `NULL`, or when `mm_source = 'self_report`,
#' `time_zero` is set to the date of the UKB baseline assessment.
#' @param mm_source The source of disease history.
#' @param conv_table The path to the ICD10/ICD9 conversion table.
#' @param mm_codes_file The path to the codes for each MM.
#' @param random_seed The number of `set.seed`.
#' @export

create_mm_masterfile <- function(df,
                                 time_zero = NULL,
                                 mm_source,
                                 conv_table = NULL,
                                 mm_codes_file,
                                 random_seed = 24){

  ## remove participants that were younger than XX at assessment
  df$birth_date <- as.Date(paste0('01/',
                                  as.character(df$X52.0.0),
                                  '/' ,
                                  as.character(df$X34.0.0)),
                           format = '%d/%m/%Y')

  # calculate number of days in each birth month
  df$days_in_birth_month <-
    as.integer(format(df$birth_date + months(1) - days(1), '%d'))

  # set random day for day of birth
  set.seed(random_seed)
  df$random_day <- apply(df, 1, function(row) {
    sample(1:row['days_in_birth_month'], 1)
  })
  df$birth_date <- as.Date(paste0(as.character(df$random_day),
                                         '/',
                                         as.character(df$X52.0.0),
                                         '/' ,
                                         as.character(df$X34.0.0)),
                                  format = '%d/%m/%Y')

  # import self-report disease codes
  mm_codes <- read.csv(mm_codes_file) %>%
    filter(!is.na(code)) %>%
    mutate(code = as.character(code))


  if (mm_source == 'self_report'){

    #### ----------------------------------------- ####
    ####  Get field IDs for self-reported illness  ####
    #### ----------------------------------------- ####

    df <- df %>%
      rename(asc_date = X53.0.0)
    df$asc_date <- as.Date(df$asc_date, format = '%Y-%m-%d')
    df$asc_age <- as.numeric(difftime(df$asc_date, df$birth_date,
                                      units = 'days'))/365.25

    ## categories 20001 and 20002

    # non-cancer self-report disorders from category 20002
    dis_self <- create_diseases_self(df = df,
                                     mm_codes = mm_codes,
                                     cancer = FALSE)
    # cancer self-report cases from category 20001
    dis_self_cancer <- create_diseases_self(df = df,
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
    dis_other <- df %>%
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


    dis_self <- merge(subset(df, select = c(id, asc_age, asc_date)),
                      dis_self,
                      by = 'id', all.y = TRUE)

    return(dis_self)

  } else if (mm_source == 'inpatient'){

    # select the inpatient field codes (as seen in GitHub: XX)
    diagnoses <- main_vars %>%
      select(id, starts_with(c('X41270.', 'X41271.', 'X41280.', 'X41281.')))

    diagnoses[diagnoses=='']  <- NA
    diagnoses <- as.data.frame(sapply(diagnoses, as.character))

    # separate sources of diagnoses (ICD9 vs 10) and dates vs. diagnosis codes
    icd9 <- diagnoses %>% select(c('id', starts_with('X41271')))
    icd9_date <- diagnoses %>% select(c('id', starts_with('X41281')))
    icd10 <- diagnoses %>% select(c('id', starts_with('X41270')))
    icd10_date <- diagnoses %>% select(c('id', starts_with('X41280')))

    # keep only rows without NAs
    icd9 <- icd9[rowSums(is.na(icd9))!=ncol(icd9)-1,]
    icd9_date <- icd9_date[rowSums(is.na(icd9_date))!=ncol(icd9_date)-1,]
    icd10 <- icd10[rowSums(is.na(icd10))!=ncol(icd10)-1,]
    icd10_date <- icd10_date[rowSums(is.na(icd10_date))!=ncol(icd10_date)-1,]

    # transform to long-type format
    icd9_long <- icd9 %>%  pivot_longer(-id, names_to = 'code',
                                        values_drop_na=TRUE)
    colnames(icd9_long) <- c('id', 'column', 'code')
    icd9_long$column <- sub('X41271.', '', icd9_long$column)

    icd9_date_long <- icd9_date %>%  pivot_longer(-id, names_to = 'code',
                                                  values_drop_na=TRUE)
    colnames(icd9_date_long) <- c('id', 'column', 'date')
    icd9_date_long$column <- sub('X41281.', '', icd9_date_long$column)

    icd10_long <- icd10 %>%  pivot_longer(-id, names_to = 'code',
                                          values_drop_na=TRUE)
    colnames(icd10_long) <- c('id', 'column', 'code')
    icd10_long$column <- sub('X41270.', '', icd10_long$column)

    icd10_date_long <- icd10_date %>%  pivot_longer(-id, names_to = 'code',
                                                    values_drop_na=TRUE)
    colnames(icd10_date_long) <- c('id', 'column', 'date')
    icd10_date_long$column <- sub('X41280.', '', icd10_date_long$column)

    # combine all diagnoses
    icd9 <- merge(icd9_long, icd9_date_long, by = c('id', 'column'))
    icd9$column <- NULL; icd9$version <- 'icd9'
    icd10 <- merge(icd10_long, icd10_date_long, by = c('id', 'column'))
    icd10$column <- NULL; icd10$version <- 'icd10'

    ## ICD-10: just keep diagnoses from the MM list
    # regular expression to match only codes starting with those in `mm_codes$code`
    icd10$mm_code <- NA
    for (current_code in mm_codes$code) {
      matches <- grepl(paste0('^', current_code), icd10$code)
      icd10$mm_code[matches] <- current_code
    }
    # just keep found codes
    icd10_filtered <- icd10 %>%
      filter(!is.na(mm_code))
    # merge with the code descriptions
    icd10_filtered <- merge(icd10_filtered, mm_codes,
                            by.x = 'mm_code', by.y = 'code',
                            all.x = TRUE) %>%
      rename(diag_date = date) %>%
      mutate(diag_date = as.Date(diag_date, format = '%Y-%m-%d')) %>%
      select(id, diag_date, disorder)


    ## ICD-9: use conversion table to convert from ICD10 to ICD9
    conv_tbl <- read.csv(conv_table)
    conv_tbl <- conv_tbl[conv_tbl$ICD9 != 'UNDEF', ]
    # identify equivalent ICD9 codes in the conversion table
    conv_tbl$mm_code <- NA
    for (current_code in mm_codes$code){
      matches <- grepl(paste0('^', current_code), conv_tbl$ICD10)
      conv_tbl$mm_code[matches] <- current_code
    }
    # combine the diagnoses with the conversion table
    conv_tbl <- conv_tbl %>%
      filter(!is.na(mm_code)) %>%
      distinct(ICD9, .keep_all = TRUE)
    icd9_filtered <- icd9 %>%
      filter(code %in% conv_tbl$ICD9) %>%
      left_join(conv_tbl, join_by(code == ICD9))
    # combine the diagnoses with the MM data frame
    icd9_filtered <- merge(icd9_filtered, mm_codes,
                           by.x = 'mm_code', by.y = 'code',
                           all.x = TRUE) %>%
      rename(diag_date = date) %>%
      mutate(diag_date = as.Date(diag_date, format = '%Y-%m-%d')) %>%
      select(id, diag_date, disorder)


    ## Combine both sources of diagnoses and keep just the ones before time zero
    icd_all <- rbind(icd9_filtered, icd10_filtered)

    # determine time zero - set diagnoses after time zero to non-cases
    if (is.null(time_zero)){
      icd_all <- merge(icd_all, select(df, c(id, X53.0.0, birth_date)),
                       by = 'id', all.y = TRUE) %>%
        rename(asc_date = X53.0.0) %>%
        mutate(asc_date = as.Date(asc_date, format = '%Y-%m-%d'))
      icd_all$disorder[icd_all$diag_date >= icd_all$asc_date] <- NA
      icd_all$diag_date[icd_all$diag_date >= icd_all$asc_date] <- NA
      # calculate age at ascertainment
      icd_all$asc_age <- as.numeric(difftime(icd_all$asc_date,
                                             icd_all$birth_date,
                                             units = 'days'))/365.25
    } else {
      time_zero <- as.Date(time_zero, format = '%d/%m/%Y')
      if (is.na(time_zero)){
        stop('Error: the date of time zero is not in the expected format.')
      }
      icd_all <- merge(icd_all, select(df, c(id, birth_date)),
                       by = 'id', all.y = TRUE)
      icd_all$disorder[icd_all$diag_date >= time_zero] <- NA
      icd_all$diag_date[icd_all$diag_date >= time_zero] <- NA
      # calculate age at ascertainment
      icd_all$asc_age <- as.numeric(difftime(time_zero,
                                             icd_all$birth_date,
                                             units = 'days'))/365.25
    }

    # remove duplicate instance of disorder per id by keeping first instance
    icd_all <- icd_all %>%
      arrange(diag_date) %>%
      distinct(id, disorder, .keep_all = TRUE)

    # transform to wide format
    icd_all_wide <- icd_all %>%
      select(id, disorder, asc_date, asc_age) %>%
      # Create an indicator variable that is 1 for each occurrence.
      mutate(value = 1) %>%

      # each unique disorder becomes a column.
      pivot_wider(names_from = disorder,
                  values_from = value,
                  values_fill = list(value = 0)) %>%
      select(-`NA`)
  }

  return(icd_all_wide)
}
