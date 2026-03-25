#' Determine the data provider and censoring times for each participant
#'
#' `get_data_provider` takes an input data frame with UKB fields 40022 and 191,
#' data frames for hospital episode statistics, GP registrations, and GP clinical
#' events, and a vector of censoring dates to output a data frame with the
#' assigned data provider (by nation) and the censoring date per participant.
#'
#' The function first attempts to assign a data provider from inpatient field
#' 40022. For participants with multiple providers or no provider recorded, it
#' resolves using hospital episode records (`hesin_df`). Remaining gaps are
#' filled using GP registration data (`gp_regs_df`) and, failing that, GP
#' clinical event data (`gp_clinical_df`). Data provider codes are harmonised
#' to nation-level labels: HES (England), SMR (Scotland), PEDW (Wales).
#'
#' @param df The input data frame containing columns `eid`, `X40022.0.*`, and
#'   `X191.0.0` (loss to follow-up date).
#' @param hesin_df Data frame from UKB category 2006 (hesin.txt), with columns
#'   `eid`, `epistart`, and `dsource`.
#' @param gp_regs_df Data frame of GP registration records (UKB field 42038),
#'   with columns `eid`, `data_provider`, `reg_date`, and `deduct_date`.
#' @param gp_clinical_df Data frame of GP clinical event records (UKB field
#'   42040), with columns `eid`, `data_provider`, and `event_dt`.
#' @param cens_dates A character vector of three dates in the form YYYY-mm-dd,
#'   corresponding to the censoring dates for England, Scotland, and Wales,
#'   as per UKB documentation.
#' @param type Determines the approach used to assign the data provider. `'last'`
#'   assigns the last data provider that the participant was in contact with,
#'   `'freq'` assigns the data provider that was the most frequent across the
#'   entire period of electronic records.
#' @param fill_NAs For some participants — those that never went to either the
#'   hospital or the GP — the data provider will not be found. This argument
#'   determines how NAs should be handled. `NULL` leaves the NAs, `'random'`
#'   assigns a random data provider from the non-NAs, while `'HES'`, `'SMR'`,
#'   and `'PEDW'` replace all NAs with the corresponding data provider.
#' @param random_seed Relevant only when `fill_NAs = 'random'`; the random seed
#'   set before the sampling.
#' @param invalid_dates Optional character vector of dates in ISO format
#'   (YYYY-MM-DD) to treat as invalid in GP registration data (set to NA).
#' @param gp_fetch_dates Named list of four Date values for imputing missing
#'   GP deduct dates, with elements `england_vision`, `scotland`, `england_tpp`,
#'   and `wales`. Defaults to the original UKB data-fetch dates.
#' @return A data frame with columns `eid`, `data_provider`, `censor_date`,
#'   and `loss_to_follow_up_date`.
#' @export

get_data_provider <- function(df,
                              hesin_df,
                              gp_regs_df,
                              gp_clinical_df,
                              cens_dates,
                              type = 'freq',
                              fill_NAs = NULL,
                              random_seed = NULL,
                              invalid_dates = NULL,
                              gp_fetch_dates = list(
                                england_vision = as.Date('2017-06-30'),
                                scotland       = as.Date('2017-05-31'),
                                england_tpp    = as.Date('2016-08-31'),
                                wales          = as.Date('2017-09-30')
                              )){

  # get source from UK field ID 40022 (for those that have been to hospital
  # and have just one data provider, this is the default)
  inpatient_source <- df |>
    dplyr::select(c(eid, tidyselect::starts_with(c('X40022.'))))
  inpatient_source[inpatient_source == ''] <- NA

  # set aside those with several sources
  multi_source <- dplyr::filter(inpatient_source, !is.na(X40022.0.1)) |> dplyr::select(eid)

  # set aside those without any source (never went to hospital per field 40022)
  no_source <- dplyr::filter(inpatient_source, is.na(X40022.0.0)) |> dplyr::select(eid)

  # prepare hospital episode data
  diagnoses_dates <- hesin_df
  diagnoses_dates$epistart <- as.Date(diagnoses_dates$epistart, format = '%d/%m/%Y')

  # those with just one data provider throughout the entire period
  inpatient_constant <- inpatient_source |>
    dplyr::filter(!eid %in% multi_source$eid & !eid %in% no_source$eid) |>
    dplyr::rename(data_provider = X40022.0.0) |>
    dplyr::select(eid, data_provider)

  # those with several data providers — resolve by most frequent
  inpatient_flux_freq <- diagnoses_dates |>
    dplyr::filter(eid %in% multi_source$eid | eid %in% no_source$eid) |>
    dplyr::group_by(eid, dsource) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(count)) |>
    dplyr::distinct(eid, .keep_all = TRUE) |>
    dplyr::rename(data_provider = dsource) |>
    dplyr::select(eid, data_provider)

  # combine all most frequent inpatient data providers (i.e., for those with just one plus those with several)
  inpatient_freq <- rbind(inpatient_constant, inpatient_flux_freq) |>
    dplyr::filter(!is.na(data_provider)) # remove those that never went to hospital

  # those with several data providers — resolve by latest episode
  inpatient_flux_last <- diagnoses_dates |>
    dplyr::filter(eid %in% multi_source$eid | eid %in% no_source$eid) |>
    dplyr::filter(!is.na(epistart)) |>
    dplyr::group_by(eid) |>
    dplyr::slice_max(epistart, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::rename(data_provider = dsource) |>
    dplyr::select(eid, data_provider)

  # combine all latest inpatient data providers
  inpatient_last <- rbind(inpatient_constant, inpatient_flux_last) |>
    dplyr::filter(!is.na(data_provider))


  ### Now we have a data frame with the most frequently occurring and latest data providers for each participant.
  ### We still have missing data for those that were never admitted to hospital; for those, we will use primary
  ### care data to impute.


  # For those that do not have inpatient data providers, we will first use GP registrations to fill the gaps
  gp_reg <- gp_regs_df
  gp_reg[gp_reg == ''] <- NA
  gp_reg <- gp_reg |>
    dplyr::select(eid, data_provider, reg_date, deduct_date) |>
    dplyr::mutate(dplyr::across(c(reg_date, deduct_date), ~as.Date(., format = '%d/%m/%Y')))

  # set aside those that were always registered with just one data provider
  gp_reg_constant <- gp_reg |>
    dplyr::group_by(eid, data_provider) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::ungroup()
  gp_reg_constant <- gp_reg_constant |>
    dplyr::filter(!eid %in% gp_reg_constant$eid[duplicated(gp_reg_constant$eid)]) |>
    dplyr::select(eid, data_provider)

  # focus on those that changed primary care data providers
  gp_reg_flux <- gp_reg |>
    dplyr::filter(!eid %in% gp_reg_constant$eid)

  # set invalid dates to NA before filtering
  if (!is.null(invalid_dates)) {
    invalid_dates <- as.Date(invalid_dates, format = '%Y-%m-%Y')
    gp_reg_flux$reg_date[gp_reg_flux$reg_date %in% invalid_dates] <- NA
    gp_reg_flux$deduct_date[gp_reg_flux$deduct_date %in% invalid_dates] <- NA
  }

  gp_reg_flux <- gp_reg_flux |>
    dplyr::filter(!is.na(reg_date) | !is.na(deduct_date)) # remove rows where both reg_date and deduct_date are NA

  # people with registrations but without de-registrations were still registered
  # with their latest GP at time of data fetch, so get dates of data fetch
  # (1=England(Vision), 2=Scotland, 3=England (TPP), 4=Wales)
  gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '1'] <- gp_fetch_dates$england_vision
  gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '2'] <- gp_fetch_dates$scotland
  gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '3'] <- gp_fetch_dates$england_tpp
  gp_reg_flux$deduct_date[is.na(gp_reg_flux$deduct_date) & gp_reg_flux$data_provider == '4'] <- gp_fetch_dates$wales
  gp_reg_flux$total_time <- as.numeric((difftime(gp_reg_flux$deduct_date, gp_reg_flux$reg_date, units = 'days')))/365.25

  # for registrations of people that changed data providers,
  # calculate the length of the period of registration with each data provider
  gp_reg_flux_freq <- gp_reg_flux |>
    dplyr::group_by(eid, data_provider) |>
    dplyr::summarise(total_time = sum(total_time)) |>
    dplyr::arrange(dplyr::desc(total_time)) |>
    dplyr::distinct(eid, .keep_all = TRUE) |>
    dplyr::select(eid, data_provider) |>
    dplyr::ungroup()

  # get the latest registrations (for censoring)
  gp_reg_flux_last <- gp_reg_flux |>
    dplyr::group_by(eid, data_provider) |>
    dplyr::arrange(dplyr::desc(deduct_date)) |>
    dplyr::ungroup() |>
    dplyr::distinct(eid, .keep_all = TRUE) |>
    dplyr::select(eid, data_provider)

  # combine most frequent and latest primary care data providers
  gp_reg_freq <- rbind(gp_reg_constant, gp_reg_flux_freq)
  gp_reg_last <- rbind(gp_reg_constant, gp_reg_flux_last)



  # for people without good registration data, we will use primary care diagnosis data
  gp_diagnoses <- gp_clinical_df |>
    as.data.frame() |>
    dplyr::filter(!eid %in% gp_reg_freq$eid) |>
    dplyr::select(eid, data_provider, event_dt) |>
    dplyr::mutate(dplyr::across(event_dt, ~as.Date(., format = '%d/%m/%Y')))

  # all that are left were always diagnosed within just one data provider
  gp_diagnoses_constant <- gp_diagnoses |>
    dplyr::group_by(eid, data_provider) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::ungroup()
  gp_diagnoses_constant <- gp_diagnoses_constant |>
    dplyr::filter(!eid %in% gp_diagnoses_constant$eid[duplicated(gp_diagnoses_constant$eid)]) |>
    dplyr::select(eid, data_provider)

  # add to the ones identified using registration data
  gp_reg_freq <- rbind(gp_reg_freq, gp_diagnoses_constant)
  gp_reg_last <- rbind(gp_reg_last, gp_diagnoses_constant)

  # use the primary care data providers to supplement missing inpatient data providers

  # add primary care data providers for participants without inpatient data providers
  data_provider_freq <- rbind(inpatient_freq, dplyr::filter(gp_reg_freq, !eid %in% inpatient_freq$eid)) |>
    dplyr::rename(data_provider_inpatient_freq = data_provider)
  # repeat for latest data provider
  data_provider_last <- rbind(inpatient_last, dplyr::filter(gp_reg_last, !eid %in% inpatient_last$eid)) |>
    dplyr::rename(data_provider_inpatient_last = data_provider)

  # harmonise data provider codes to nation-level labels
  data_provider_freq <- data_provider_freq |>
    dplyr::mutate(data_provider_inpatient_freq = as.character(data_provider_inpatient_freq)) |>
    dplyr::mutate(data_provider_inpatient_freq = dplyr::case_match(
      data_provider_inpatient_freq,
      c('1', '3') ~ 'HES', '2' ~ 'SMR', '4' ~ 'PEDW',
      .default = data_provider_inpatient_freq
    ))

  data_provider_last <- data_provider_last |>
    dplyr::mutate(data_provider_inpatient_last = as.character(data_provider_inpatient_last)) |>
    dplyr::mutate(data_provider_inpatient_last = dplyr::case_match(
      data_provider_inpatient_last,
      c('1', '3') ~ 'HES', '2' ~ 'SMR', '4' ~ 'PEDW',
      .default = data_provider_inpatient_last
    ))


  # create file linking the data providers with user-provided dates
  date_frame <- data.frame(cbind(matrix(cens_dates), matrix(c('HES', 'SMR', 'PEDW'))))

  if (type == 'last'){
    colnames(date_frame) <- c('censor_date', 'data_provider_inpatient_last')
    data_provider_last <- merge(data_provider_last, date_frame,
                                by = 'data_provider_inpatient_last')
    output_df <- data_provider_last |>
      dplyr::rename(data_provider = data_provider_inpatient_last) |>
      dplyr::select(eid, data_provider, censor_date)
  } else if (type == 'freq'){
    colnames(date_frame) <- c('censor_date', 'data_provider_inpatient_freq')
    data_provider_freq <- merge(data_provider_freq, date_frame,
                                by = 'data_provider_inpatient_freq')
    output_df <- data_provider_freq |>
      dplyr::rename(data_provider = data_provider_inpatient_freq) |>
      dplyr::select(eid, data_provider, censor_date)
  }

  # determine earliest between censoring and loss to follow-up
  loss_to_followup <- df |>
    dplyr::select(eid, tidyselect::starts_with('X191.')) |>
    dplyr::rename(loss_to_follow_up_date = X191.0.0)

  output_df <- merge(output_df, loss_to_followup,
                     by = 'eid', all = TRUE)

  # replace NAs by censoring date of chosen data provider or by random data provider
  if (!is.null(fill_NAs)){
    if (fill_NAs == 'random'){
      random_pool <- output_df$data_provider[!is.na(output_df$data_provider)]

      set.seed(random_seed)

      output_df$data_provider[is.na(output_df$data_provider)] <-
        sample(random_pool, sum(is.na(output_df$data_provider)), replace = TRUE)


    } else {

      output_df$data_provider[is.na(output_df$censor_date)] <-
        date_frame$data_provider[date_frame[, 2] == fill_NAs]
    }
    # add censoring date
    output_df$censor_date[is.na(output_df$censor_date) &
                            output_df$data_provider == 'HES'] <-
      date_frame$censor_date[date_frame[, 2] == 'HES']
    output_df$censor_date[is.na(output_df$censor_date) &
                            output_df$data_provider == 'SMR'] <-
      date_frame$censor_date[date_frame[, 2] == 'SMR']
    output_df$censor_date[is.na(output_df$censor_date) &
                            output_df$data_provider == 'PEDW'] <-
      date_frame$censor_date[date_frame[, 2] == 'PEDW']
  }

  output_df$censor_date <- as.Date(output_df$censor_date, format = '%d.%m.%Y')

  return(output_df)
}
