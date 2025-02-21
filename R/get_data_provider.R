#' Determine the data provider and censoring times for each participant
#'
#' `get_data_provider` Takes an input data frame with the UKB field id 40022
#' and 191, path names for registration and clinical event files, and a list
#' of dates to output a data frame with last data provider recorded in the record,
#' the most frequent data provider recorded in the record, and the censoring date.
#' @param df The input data frame.
#' @param hesin_path Path to the "hesin.txt" file, UKB category 2006.
#' @param gp_regs_path Path to GP registration records, UKB field ID 42038.
#' @param gp_clinical_path Path to GP clinical event records, UKB field ID 42040.
#' @param cens_dates A list of three dates in the form dd.mm.YYYY, corresponding
#' @param type Determines the approach used to assign the data provider. 'last'
#' assigns the last data provider that the participant was in contact with,
#' 'freq' assigns the data provider that was the most frequent across the entire
#' period of electronic records.
#' @param fill_NAs For some participants - those that never went to either the
#' hospital or the GP - the data provider will not be found. This argument
#' determines how NAs should be handled. `NULL` leaves the NAs, 'random'
#' assigns a random data provider from the non-NAs, while 'HES', 'SMR', and
#' 'PEDW' replace all NAs with the censoring date of the corresponding data provider.
#' @param random_seed Relevant only when `fill_NAs = 'random'`; the random seed
#' set before the sampling.
#' @export

get_data_provider <- function(df,
                              hesin_path,
                              gp_regs_path,
                              gp_clinical_path,
                              cens_dates,
                              type,
                              fill_NAs = NULL,
                              random_seed){

  # get source from uk field ID 40022 (for those that have been to hospital
  # and have just one data provider, this is the default)
  inpatient_source <- df %>%
    select(c(eid, starts_with(c('X40022.'))))
  inpatient_source[inpatient_source == ''] <- NA

  # set aside those with several sources
  multi_source <- filter(inpatient_source, !is.na(X40022.0.1)) %>% select(eid)

  # get most common source of hospital diagnoses for those with several records
  diagnoses_dates <- read.csv(hesin_path, sep='\t')  # UKB category 2006
  diagnoses_dates$epistart <- as.Date(diagnoses_dates$epistart, format = '%d/%m/%Y')

  # those with just one data provider throughout the entire period
  inpatient_constant <- inpatient_source %>%
    filter(!eid %in% multi_source$eid) %>%
    rename(id = eid, data_provider = X40022.0.0) %>%
    select(id, data_provider)

  # those with several data providers
  inpatient_flux_freq <- diagnoses_dates %>%
    filter(eid %in% multi_source$eid) %>% # just resolve those with several data providers
    group_by(eid, dsource) %>% # group by data provider and id
    summarise(count = n()) %>% # count number of instances for id-source combo
    ungroup() %>%
    arrange(desc(count)) %>% # arrange descending
    distinct(eid, .keep_all = TRUE) %>% # just keep first (more frequent) instance
    rename(id = eid, data_provider = dsource) %>%
    select(id, data_provider)

  # combine all most frequent inpatient data providers (i.e., for those with just one plus those with several)
  inpatient_freq <- rbind(inpatient_constant, inpatient_flux_freq) %>%
    filter(!is.na(data_provider)) # remove those that never went to hospital

  # get latest date of hospital diagnoses for those with multiple data providers (to determine censoring later on)
  diagnoses_dates_dt <- diagnoses_dates %>%
    filter(eid %in% multi_source$eid) %>%
    filter(!is.na(epistart)) %>%
    data.table::as.data.table(.)
  inpatient_flux_last <- as.data.frame(diagnoses_dates_dt[, list(epistart = max(epistart, na.rm = TRUE)), by = eid])
  inpatient_flux_last <- merge(inpatient_flux_last, subset(diagnoses_dates, select = c(eid, epistart, dsource)),
                               by = c('eid', 'epistart'), all.x = TRUE) %>%
    rename(id = eid, data_provider = dsource) %>%
    select(id, data_provider) %>%
    distinct(id, .keep_all = TRUE)

  # combine all latest inpatient data providers
  inpatient_last <- rbind(inpatient_constant, inpatient_flux_last) %>%
    filter(!is.na(data_provider))


  ### Now we have a data frame with the most frequently occurring and latest data providers for each participant.
  ### We still have missing data for those that were never admitted to hospital; for those, we will use primary
  ### care data to impute.


  # For those that do not have inpatient data providers, we will first use GP registrations to fill the gaps
  gp_reg <- read.csv(gp_regs_path, sep='\t', header=TRUE, quote='') %>% rename(id = eid)
  gp_reg[gp_reg == ''] <- NA
  gp_reg <- gp_reg %>%
    select(id, data_provider, reg_date, deduct_date) %>%
    mutate(across(c(reg_date, deduct_date), ~as.Date(., format = '%d/%m/%Y')))

  # set aside those that were always registered with just one data provider
  gp_reg_constant <- gp_reg %>%
    group_by(id, data_provider) %>%
    summarise(count = n()) %>%
    ungroup()
  gp_reg_constant <- gp_reg_constant %>%
    filter(!id %in% gp_reg_constant$id[duplicated(gp_reg_constant$id)]) %>%
    select(id, data_provider)

  # focus on those that changed primary care data providers
  gp_reg_flux <- gp_reg %>%
    filter(!id %in% gp_reg_constant$id)
  gp_reg_flux <- gp_reg_flux %>%
    filter(!is.na(reg_date) | !is.na(deduct_date)) # remove observations without both registration and de-registration date

  # people with registrations but without de-registrations were still registered
  # with their latest GP at time of data fetch, so get dates of data fetch
  # (1= England(Vision), 2= Scotland, 3 = England (TPP), 4 = Wales)
  gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '1'] <- as.Date('30/06/2017', format = '%d/%m/%Y') # England Vision
  gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '2'] <- as.Date('31/05/2017', format = '%d/%m/%Y') # Scotland
  gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '3'] <- as.Date('31/08/2016', format = '%d/%m/%Y') # England TPP
  gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '4'] <- as.Date('30/09/2017', format = '%d/%m/%Y') # Wales
  gp_reg_flux$total_time <- as.numeric((difftime(gp_reg_flux$deduct_date, gp_reg_flux$reg_date, units = 'days')))/365.25

  # for registrations of people that changed data providers, calculate the length of the period of registration with each data provider
  gp_reg_flux_freq <- gp_reg_flux %>%
    group_by(id, data_provider) %>%
    summarise(total_time = sum(total_time)) %>%
    arrange(desc(total_time)) %>%
    distinct(id, .keep_all = TRUE) %>%
    select(id, data_provider) %>%
    ungroup()

  # get the latest registrations (for censoring)
  gp_reg_flux_last <- gp_reg_flux %>%
    group_by(id, data_provider) %>%
    arrange(desc(deduct_date)) %>%
    ungroup() %>%
    distinct(id, .keep_all = TRUE) %>%
    select(id, data_provider)

  # combine most frequent and latest primary care data providers
  gp_reg_freq <- rbind(gp_reg_constant, gp_reg_flux_freq)
  gp_reg_last <- rbind(gp_reg_constant, gp_reg_flux_last)



  # for people without good registration data, we will use primary care diagnosis data
  gp_diagnoses <- data.table::fread(gp_clinical_path, sep='\t', header=TRUE, quote='') %>%
    as.data.frame() %>%
    filter(!eid %in% gp_reg_freq$id) %>%
    select(eid, data_provider, event_dt) %>%
    mutate(across(event_dt, ~as.Date(., format = '%d/%m/%Y')))

  # all that are left were always diagnosed within just one data provider
  gp_diagnoses_constant <- gp_diagnoses %>%
    group_by(eid, data_provider) %>%
    summarise(count = n()) %>%
    ungroup()
  gp_diagnoses_constant <- gp_diagnoses_constant %>%
    filter(!eid %in% gp_diagnoses_constant$eid[duplicated(gp_diagnoses_constant$eid)]) %>%
    select(eid, data_provider) %>%
    rename(id = eid)

  # add to the ones identified using registration data
  gp_reg_freq <- rbind(gp_reg_freq, gp_diagnoses_constant)
  gp_reg_last <- rbind(gp_reg_last, gp_diagnoses_constant)

  # use the primary care data providers to supplement missing inpatient data providers

  # add primary care data providers for participants without inpatient data providers
  data_provider_freq <- rbind(inpatient_freq, filter(gp_reg_freq, !id %in% inpatient_freq$id)) %>%
    rename(data_provider_inpatient_freq = data_provider)
  # repeat for latest data provider
  data_provider_last <- rbind(inpatient_last, filter(gp_reg_last, !id %in% inpatient_last$id)) %>%
    rename(data_provider_inpatient_last = data_provider)

  # harmonise naming of data providers
  data_provider_freq$data_provider_inpatient_freq[
    data_provider_freq$data_provider_inpatient_freq == 1 |
      data_provider_freq$data_provider_inpatient_freq == 3] <- 'HES'

  data_provider_freq$data_provider_inpatient_freq[
    data_provider_freq$data_provider_inpatient_freq == 2] <- 'SMR'

  data_provider_freq$data_provider_inpatient_freq[
    data_provider_freq$data_provider_inpatient_freq == 4] <- 'PEDW'

  data_provider_last$data_provider_inpatient_last[
    data_provider_last$data_provider_inpatient_last == 1 |
      data_provider_last$data_provider_inpatient_last == 3] <- 'HES'

  data_provider_last$data_provider_inpatient_last[
    data_provider_last$data_provider_inpatient_last == 2] <- 'SMR'

  data_provider_last$data_provider_inpatient_last[
    data_provider_last$data_provider_inpatient_last == 4] <- 'PEDW'


  # create file linking the data providers with user-provided dates
  date_frame <- data.frame(cbind(matrix(cens_dates), matrix(c('HES', 'SMR', 'PEDW'))))

  if (type == 'last'){
    colnames(date_frame) <- c('censor_date', 'data_provider_inpatient_last')
    data_provider_last <- merge(data_provider_last, date_frame,
                                by = 'data_provider_inpatient_last')
    output_df <- data_provider_last %>%
      rename(data_provider = data_provider_inpatient_last) %>%
      select(id, data_provider, censor_date)
  } else if (type == 'freq'){
    colnames(date_frame) <- c('censor_date', 'data_provider_inpatient_freq')
    data_provider_freq <- merge(data_provider_freq, date_frame,
                                by = 'data_provider_inpatient_freq')
    output_df <- data_provider_freq %>%
      rename(data_provider = data_provider_inpatient_freq) %>%
      select(id, data_provider, censor_date)
  }

  # determine earliest between censoring and loss to follow-up
  loss_to_followup <- main_vars %>%
    select(id, starts_with('X191.')) %>%
    rename(loss_to_follow_up_date = X191.0.0)

  output_df <- merge(output_df, loss_to_followup,
                     by = 'id', all = TRUE)

  # replace NAs by censoring date of chosen data provider or by random data provider
  if (!is.null(fill_NAs)){
    if (fill_NAs == 'random'){
      random_pool <- output_df$censor_date[!is.na(output_df$censor_date)]
      set.seed(random_seed)
      output_df$censor_date[is.na(output_df$censor_date)] <-
        sample(random_pool, sum(is.na(output_df$censor_date)), replace = TRUE)
    } else {
      output_df$censor_date[is.na(output_df$censor_date)] <-
        date_frame$censor_date[date_frame$data_provider == fill_NAs]
    }
  }

  output_df$censor_date <- as.Date(output_df$censor_date, format = '%d.%m.%Y')

  return(output_df)
}
