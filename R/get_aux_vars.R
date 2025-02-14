#' Get auxiliary variables used for imputation and/or SMOTE
#'
#' `get_aux_vars` takes the data frame UKB participants and
#' returns a data frame with additional columns relevant for multimorbidity.
#' @param df The input data frame.
#' @export

get_aux_vars <- function(df){

  #### --------------------------------------------- ####
  ####  Get auxiliary variables used for imputation  ####
  #### --------------------------------------------- ####

  aux_vars_list <- c('X24016.', 'X24017.', 'X24018.', 'X24003.',
                     'X24004.', 'X24019.', 'X2020.',
                     'X24005.', 'X24006.', 'X24007.', 'X24008.',
                     'X20016.0.0', 'X20018.0.0', 'X20023.0.0', 'X399.0.2',
                     'X4282.0.0', 'X5082.0.0',
                     'X30760.0.0', 'X30780.0.0', 'X4079.0.0', 'X4080.0.0',
                     'X21002.0.0', 'X30710.0.0',
                     'X48.0.0', 'X49.0.0', 'X23100.0.0',
                     'X26206.', 'X26283.', 'X26285.', 'X26260.', 'X26244.',
                     'X26223.0.0',
                     'X20107.0', 'X20110.0',
                     'X31.', 'X21002.0.0', 'X50.0.0',
                     'X6138.0', 'X189.', 'X6164.0', 'X1558.0.0',
                     'X709.0.0', 'X1031.0.0', 'X6160.0.0', 'X5201.0.0',
                     'X5208.0.0', 'X3731.0.0', 'X22040.0.0')

  aux_vars <- df %>%
    select(id, starts_with(aux_vars_list)) %>%
    rename(sex = X31.0.0, height_0 = X50.0.0, weight_0 = X21002.0.0,
           waist_circ_0 = X48.0.0, lonely_0 = X2020.0.0,
           hip_circ_0 = X49.0.0, fat_mass = X23100.0.0, hdl = X30760.0.0,
           ldl = X30780.0.0, crp = X30710.0.0,
           VNR_0 = X20016.0.0, ProsMem_0 = X20018.0.0, RT_0 = X20023.0.0,
           VisMem_0 = X399.0.2, NM_0 = X4282.0.0, vis_ac_l = X5208.0.0,
           vis_ac_r = X5201.0.0,
           bp_dia = X4079.0.0, bp_sys = X4080.0.0,
           PRS_AD = X26206.0.0, met = X22040.0.0, alcohol_ex = X3731.0.0,
           PRS_diab_typ_1 = X26283.0.0, PRS_diab_typ_2 = X26285.0.0,
           PRS_PD = X26260.0.0, PRS_hyperten = X26244.0.0,
           PRS_CVD = X26223.0.0, pol_no2_2010 = X24003.0.0,
           pol_nox_2010 = X24004.0.0, pol_pm10_2007 = X24019.0.0,
           pol_pm10_2010 = X24005.0.0, pol_pm25_2010 = X24006.0.0,
           pol_pm25_10_2010 = X24008.0.0, deprivation = X189.0.0, ,
           alcohol_0 = X1558.0.0, household_0 = X709.0.0,
           visits_0 = X1031.0.0, leisure_0 = X6160.0.0)

  # -1 is NA
  aux_vars[, c('VNR_0', 'NM_0')] <-
    lapply(aux_vars[, c('VNR_0', 'NM_0')],
           function(x) ifelse(x == -1, NA, x))

  # -3 is NA (those that indicated that they do drink alcohol, also get a '1' for
  # past drinking question)
  aux_vars$alcohol_ex[is.na(aux_vars$alcohol_ex)] <- 1
  aux_vars[, c('alcohol_0', 'alcohol_ex')] <-
    lapply(aux_vars[, c('alcohol_0', 'alcohol_ex')],
           function(x) ifelse(x == -3, NA, x))

  # -1, -3 is NA
  aux_vars[, c('household_0', 'visits_0', 'leisure_0', 'lonely_0')] <-
    lapply(aux_vars[, c('household_0', 'visits_0', 'leisure_0', 'lonely_0')],
           function(x) ifelse(x %in% c(-1, -3), NA, x))

  # leisure -7 to 0
  aux_vars$leisure_0[aux_vars$leisure_0 == -7] <- 0


  # categorise education
  aux_vars <- aux_vars %>%
    mutate(education_0 = apply(select(., starts_with('X6138.0')), 1, function(x) education_classify(x)))

  # categorise physical activity
  aux_vars <- aux_vars %>%
    mutate(across(starts_with('X6164.0'), ~ sapply(., phys_act_classify))) %>%
    mutate(phys_act = pmax(X6164.0.0, X6164.0.1, X6164.0.2, X6164.0.3, X6164.0.4, na.rm = TRUE))

  # pollution: average NO2 2005-2007
  aux_vars$pol_no2_05_07 <- rowMeans(aux_vars[, c('X24016.0.0', 'X24017.0.0',
                                                  'X24018.0.0')])


  # for family history, we just use mother father, because participants did not
  # give information on number of siblings during baseline
  # mother/father illnesses are NA when the participant didn't know (-11, -21),
  # and preferred not to answer (-13, -23); 'none of the above' is a valid 0 category
  # we can ignore people who explicitly stated that the disorders weren't present
  # (-17, -27), because we will automatically set 0 if they don't list one of the
  # disorders

  fam_ill <- aux_vars %>%
    select(id, starts_with(c('X20107.', 'X20110.')))

  fam_ill[fam_ill == -11 | fam_ill == -21 |
            fam_ill == -13 | fam_ill == -23] <- NA

  fam_ill <- fam_ill %>%
    mutate(
      # prostate_cancer
      prostate_cancer_dad = if_else(rowSums(select(., starts_with('X20107.')) == 13, na.rm = TRUE) > 0, 1, 0),

      # severe depression
      depression_dad = if_else(rowSums(select(., starts_with('X20107.')) == 12, na.rm = TRUE) > 0, 1, 0),
      depression_mum = if_else(rowSums(select(., starts_with('X20110.')) == 12, na.rm = TRUE) > 0, 1, 0),

      # PD
      PD_dad = if_else(rowSums(select(., starts_with('X20107.')) == 11, na.rm = TRUE) > 0, 1, 0),
      PD_mum = if_else(rowSums(select(., starts_with('X20110.')) == 11, na.rm = TRUE) > 0, 1, 0),

      # AD
      AD_dad = if_else(rowSums(select(., starts_with('X20107.')) == 10, na.rm = TRUE) > 0, 1, 0),
      AD_mum = if_else(rowSums(select(., starts_with('X20110.')) == 10, na.rm = TRUE) > 0, 1, 0),

      # diabetes
      diabetes_dad = if_else(rowSums(select(., starts_with('X20107.')) == 9, na.rm = TRUE) > 0, 1, 0),
      diabetes_mum = if_else(rowSums(select(., starts_with('X20110.')) == 9, na.rm = TRUE) > 0, 1, 0),

      # hypertension
      hypertension_dad = if_else(rowSums(select(., starts_with('X20107.')) == 8, na.rm = TRUE) > 0, 1, 0),
      hypertension_mum = if_else(rowSums(select(., starts_with('X20110.')) == 8, na.rm = TRUE) > 0, 1, 0),

      # chronic bronchitis/emphysema
      bronch_dad = if_else(rowSums(select(., starts_with('X20107.')) == 6, na.rm = TRUE) > 0, 1, 0),
      bronch_mum = if_else(rowSums(select(., starts_with('X20110.')) == 6, na.rm = TRUE) > 0, 1, 0),

      # breast cancer
      breast_cancer_mum = if_else(rowSums(select(., starts_with('X20110.')) == 5, na.rm = TRUE) > 0, 1, 0),

      # bowel cancer
      bowel_cancer_dad = if_else(rowSums(select(., starts_with('X20107.')) == 4, na.rm = TRUE) > 0, 1, 0),
      bowel_cancer_mum = if_else(rowSums(select(., starts_with('X20110.')) == 4, na.rm = TRUE) > 0, 1, 0),

      # lung cancer
      lung_cancer_dad = if_else(rowSums(select(., starts_with('X20107.')) == 3, na.rm = TRUE) > 0, 1, 0),
      lung_cancer_mum = if_else(rowSums(select(., starts_with('X20110.')) == 3, na.rm = TRUE) > 0, 1, 0),

      # stroke
      stroke_dad = if_else(rowSums(select(., starts_with('X20107.')) == 2, na.rm = TRUE) > 0, 1, 0),
      stroke_mum = if_else(rowSums(select(., starts_with('X20110.')) == 2, na.rm = TRUE) > 0, 1, 0),

      # heart disease
      cvd_dad = if_else(rowSums(select(., starts_with('X20107.')) == 1, na.rm = TRUE) > 0, 1, 0),
      cvd_mum = if_else(rowSums(select(., starts_with('X20110.')) == 1, na.rm = TRUE) > 0, 1, 0),
    )

  # if there are only NAs in a row, set the new indicator variable to 1
  fam_ill <- fam_ill %>%
    mutate(
      all_NA = if_else(
        rowSums(!is.na(select(., starts_with(c('X20107.', 'X20110.'))))) == 0,
        1, 0
      )
    )
  # family illness to NA if above indicator variable is 1
  for (col in colnames(fam_ill %>% select(ends_with(c('mum', 'dad'))))){
    fam_ill[fam_ill$all_NA == 1, col] <- NA
  }
  # remove unnecessary columns
  fam_ill <- fam_ill %>%
    select(-c(starts_with('X'), all_NA)) %>%
    mutate(across(everything(), as.factor))

  # merge with the rest of auxiliary variables
  aux_vars <- aux_vars %>%
    select(-c(starts_with('X'))) %>%
    mutate(across(c('ProsMem_0', 'sex', 'alcohol_0', 'alcohol_ex', 'leisure_0',
                    'education_0', 'phys_act', 'visits_0', 'lonely_0',
                    'id'),
                  as.factor)) %>%
    inner_join(fam_ill, by = 'id')

  return(aux_vars)
}
