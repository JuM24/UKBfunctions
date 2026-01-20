850.1–850.5, 850.9
851.0–854.1	Intracranial injury, including concussion, confusion, laceration and haemorrhage
959.01	Head injury, unspecified


## This code combines data fields 41270, 41271, 41280, and 41281,
# which refer to inpatient diagnoses and dates of admission.
diagnoses <- data_all %>%
  select(eid, starts_with(c('X41270.', 'X41280.', 'X41271.', 'X41281.'))) %>%
  rename(id = eid)

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
icd9_long <- icd9 %>%  pivot_longer(-id, names_to = 'diagnosis',
                                    values_drop_na=TRUE)
colnames(icd9_long) <- c('id', 'column', 'diagnosis')
icd9_long$column <- sub('X41271.', '', icd9_long$column)

icd9_date_long <- icd9_date %>%  pivot_longer(-id, names_to = 'diagnosis',
                                              values_drop_na=TRUE)
colnames(icd9_date_long) <- c('id', 'column', 'date')
icd9_date_long$column <- sub('X41281.', '', icd9_date_long$column)

icd10_long <- icd10 %>%  pivot_longer(-id, names_to = 'diagnosis',
                                      values_drop_na=TRUE)
colnames(icd10_long) <- c('id', 'column', 'diagnosis')
icd10_long$column <- sub('X41270.', '', icd10_long$column)

icd10_date_long <- icd10_date %>%  pivot_longer(-id, names_to = 'diagnosis',
                                                values_drop_na=TRUE)
colnames(icd10_date_long) <- c('id', 'column', 'date')
icd10_date_long$column <- sub('X41280.', '', icd10_date_long$column)

# combine all diagnoses
icd9 <- merge(icd9_long, icd9_date_long, by = c('id', 'column'))
icd9$column <- NULL; icd9$version <- 'icd9'
icd10 <- merge(icd10_long, icd10_date_long, by = c('id', 'column'))
icd10$column <- NULL; icd10$version <- 'icd10'

inpatient <- rbind(icd9, icd10)

saveRDS(inpatient, 'inpatient.Rds')
system('dx upload inpatient.Rds')
rm(icd10, icd10_date, icd10_date_long, icd10_long,
   icd9, icd9_date, icd9_date_long, icd9_long, diagnoses)
gc()
