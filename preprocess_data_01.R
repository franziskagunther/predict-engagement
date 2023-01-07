#  Data wrangling of raw Breaking Free Online time series data
#
# This script reads in raw assessment and usage data from > 39.900 users  
#   of Breaking Free Online (https://www.breakingfreegroup.com/), a digital  
#   therapy app addressing substance use disorder and related comorbidities. 
#
# The script also prepares the data for further processing (e.g. exclusion of 
#    users with inconsistent data).
# 
# R version: R 4.2.1 GUI RStudio 2022.07.2 Build 576
#   OS: macOS Monterey 12.6 (21G115)

library(dplyr)
library(stringr)
library(purrr)

# read in data
wd <- getwd()
dat_ts <- read.csv(file = paste(wd, "/2022-10-06", sep = ""))

# some character columns in the dataset contain empty strings instead of NA, 
#   which I decided to change to NA
dat_ts <- dat_ts %>% mutate(across(where(is.character), ~ na_if(., "")))

# remove users who declined their data being shared for research, or 
#   commissioned Breaking Free to purge them
dat_ts <- dat_ts %>% filter(!date.created %in% c("DATA NOT SHARED", "PURGED"))

# remove users who don't have the date recorded on which their account was
#   created because these have no other informative data recorded
dat_ts <- dat_ts %>% filter(!is.na(date.created))

# remove users whose service indicates that the account was created for
#   testing and training purposes 
invalid_services <- c(NA, "Training Activations", "Dummy Child Dashboard",
                      "Lifeworks employees", 
                      "Wolverhampton Substance Misuse Service Training", 
                      "Special Access Service", "BDP - Staff Training 1",
                      "Test (Community testing)", "Alberto - Service - August",
                      "CAPSA", "90 day staff demo codes", "Inclusion Training",
                      "Staff training codes", "glyn-emma-test-accesscodes",
                      "Humankind Training", "Wokingham - Training", 
                      "Open Road - Training", 
                      "Turning Point - Tri-Borough Health Trainers", 
                      "Emma Test Bham", "Service Q", "Apple Review Service",
                      "Forward Trust Training", "Manchester A", 
                      "Service Provider", "Breaking Free Online", "Demo Codes",
                      "V4 Service A", "Service G", "V1userseshot", 
                      "Manchester C", "V4 Service C", "Amazon", 
                      "CGL - Dummy/Test")

dat_ts <- dat_ts %>% filter(!service %in% invalid_services)

# remove identifier column for RCT participants as RCT has not been set up at 
#   this time point
dat_ts <- dat_ts[, -4] 

# remove empty access code column
dat_ts <- dat_ts[, -4] 

# recode ethnicity column 
dat_ts <- dat_ts %>% mutate(ethn_coarse = factor(ifelse(grepl("Asian or Asian British", 
                                                              ethnicity), "2",
                                                        ifelse(grepl("Black or Black British", 
                                                                     ethnicity), "3",
                                                               ifelse(grepl("Mixed", 
                                                                            ethnicity), "4",
                                                                      ifelse(grepl("White", 
                                                                                   ethnicity), "1", 
                                                                             ifelse(!is.na(ethnicity), "5", NA)))))))

# remove users younger than 18 and older than 89
dat_ts <- dat_ts %>% filter((age >= 18 & age < 90) | is.na(age))

# remove users who don't have their primary substance category recorded as
#   these have no other informative data recorded
dat_ts <- dat_ts %>% filter(!is.na(addictionCase))

# impute specific drug of abuse for those who have not indicated this with 
#   "prefer not to say"
dat_ts[which(dat_ts$addictionCase != "Alcohol" & is.na(dat_ts$initial.1stDrug)), "initial.1stDrug"] <- "prefer not to say"

# to be able to use data from the SDS as a predictor of engagement, it is 
#   necessary to know which of the SDS we should take the data from with respect
#   to users who have indicated that they abuse both alcohol and drugs

# therefore, we need to determine a primary substance of abuse for each of these
#   users

# if primacy is missing, we can determine it through SDS sum scores or the 
#   weekly number of days on which the substance is consumed

# if this data, however, is missing as well, there is no way to determine 
#   primacy, and we therefore removed users
cols <- colnames(dat_ts)
required_variables <- cols[str_detect(cols, regex("[Ii]nitial\\.(SDS\\.Alc|SDS\\.Drug|(Alc|1stDrug)\\.(Days|Frequency))(?!Goal|Likert).*"))]

to_exclude <- dat_ts %>%
  filter(addictionCase == "Alcohol & Drugs" & is.na(specialAddiction) & is.na(rowSums(across(all_of(required_variables)))))

dat_ts <- dat_ts %>%
  filter(!user.ID %in% to_exclude$user.ID)

# we then retrospectively complete the information for users who don't 
#   indicate primacy, but have enough information ready to determine primacy
#   retrospectively

# checking whether for those who have missing information on this variable they 
#   have unambiguous secondary information on primacy
sA_na <- dat_ts[which(dat_ts$addictionCase == "Alcohol & Drugs" & is.na(dat_ts$specialAddiction)), ]

sA_na <- sA_na %>%
  mutate(SDS.AlcminusSDS.Drug = rowSums(across(matches("Initial\\.SDS\\.(Alc)\\d"))) - rowSums(across(matches("initial\\.SDS\\.(Drug)\\d")))) %>%
  mutate(DaysminusFrequency = Initial.Alc.Days - initial.1stDrug.Frequency)

# set those who have ambiguous information on the relevant variables up for 
#   randomisation, and assign the determined primacy for those whose information
#   is unambiguous
substance <- c("Alcohol", "Drugs")

sA_na <- sA_na %>%
  mutate(specialAddiction = ifelse(SDS.AlcminusSDS.Drug > 0, "Alcohol",
                                   ifelse(SDS.AlcminusSDS.Drug < 0, "Drugs",
                                          ifelse(DaysminusFrequency > 0, "Alcohol",
                                                 ifelse(DaysminusFrequency < 0, "Drugs",
                                                        "prefer not to say")))))

pnts_sA_na <- which(sA_na$specialAddiction == "prefer not to say")

for(i in 1:length(pnts_sA_na)) {
  
  sA_na[pnts_sA_na[i], "specialAddiction"] = sample(substance, 1)
  
}

# assign that information
for(i in 1:nrow(sA_na)) {
  
  dat_ts[which(dat_ts$user.ID == sA_na[i, "user.ID"]), "specialAddiction"] = sA_na[i, "specialAddiction"]
  
}

# create a variable including information about the completeness of the baseline
#   assessment (which excludes sociodemographic information)
dat_ts["my.initial.assess.complete"] <- rep(TRUE, dim(dat_ts)[1])

# determine those who are incorrectly labelled as having complete baseline data
alc_regs <- dat_ts %>% 
  filter(addictionCase == "Alcohol") %>%
  filter(if_any(.cols = matches("[Ii]nitial\\.(?!assess|date|.*Drug).*", perl = TRUE), .fns = ~ is.na(.x)))

dat_ts[which(dat_ts$user.ID %in% alc_regs$user.ID), "my.initial.assess.complete"] <- FALSE

drug_regs <- dat_ts %>% 
  filter(addictionCase == "Drugs") %>%
  filter(if_any(.cols = matches("[Ii]nitial\\.(?!2nd|3rd|assess|date|.*Alc).*", perl = TRUE), .fns = ~ is.na(.x)))

dat_ts[which(dat_ts$user.ID %in% drug_regs$user.ID), "my.initial.assess.complete"] <- FALSE

alcdrug_regs <- dat_ts %>% 
  filter(addictionCase == "Alcohol & Drugs") %>%
  filter(if_any(.cols = matches("[Ii]nitial\\.(?!2nd|3rd|assess|date).*", perl = TRUE), .fns = ~ is.na(.x)))

dat_ts[which(dat_ts$user.ID %in% alcdrug_regs$user.ID), "my.initial.assess.complete"] <- FALSE

# remove users with inconsistent information on select variables
dat_ts <- dat_ts %>% filter(if_all(.cols = c("initial.1stDrug.Frequency", 
                                             "initial.2ndDrug.Frequency",
                                             "Initial.DrugsGoal.Days"),
                                   .fns = ~ is.na(.x) | .x < 8)) # NA is allowed
                                                                 #   because not
                                                                 #   all 
                                                                 #   users
                                                                 #   use drugs

# create a variable reflecting how many assessment updates have been carried out

# index of last assessment column
index_last_var <- which(colnames(dat_ts) == "Current.101.3rdDrug.Days")

# look at a subset of the dataset
dat_ts_test <- dat_ts[, c(1:index_last_var)]

cols_test <- colnames(dat_ts_test)

new_var <- rep(NA, nrow(dat_ts))

for(i in 1:nrow(dat_ts_test)) {
  
  # get index of column of last observed value
  ind_last = tail(which(!is.na(dat_ts_test[i, ])), n = 1)
  new_var[i] = cols_test[ind_last]
  
}

dat_ts["col_last_entry"] <- new_var

dat_ts <- dat_ts %>% 
  mutate(My.Progress.Number.Started = ifelse(str_detect(col_last_entry, "Progress"), 0,
                                             as.integer(str_extract(col_last_entry, regex("(?!Current\\.)[0-9]+")))))

# assign NA as a date for users without an assessment update who have a 
#   date recorded falsely
dat_ts[which(dat_ts$My.Progress.Number.Started == 0), "Progress.Current.Date"] <- NA

# clear all assessment update dates for assessments which weren't taken
for(i in 1:77) {
  
  cols_to_clear = paste("Current.", seq(i + 1, 78), ".time", sep = "")
  dat_ts[which(dat_ts$My.Progress.Number.Started == i), cols_to_clear] <- NA
  
}

# transform all date columns to proper R Date variables
dates <- colnames(dat_ts)[str_detect(colnames(dat_ts), regex("date|time", ignore_case = TRUE))]

dat_ts <- dat_ts %>%
  mutate(across(dates, ~ as.Date(.x, "%d/%m/%Y")))

dat_ts["time_engagement_days"] <- as.integer(difftime(dat_ts$Progress.Current.Date, dat_ts$initial.date, units = "days")) 
dat_ts["time_engagement_weeks"] <- as.integer(difftime(dat_ts$Progress.Current.Date, dat_ts$initial.date, units = "weeks")) 

# create a variable reflecting the percentage of missing data on the baseline
#   assessment    
percentage_all <- rep(NA, nrow(dat_ts))

alc_colnames <- colnames(dat_ts)[str_detect(colnames(dat_ts), regex("[Ii]nitial\\.(?!assess|date|.*Drug).*"))]
alc_n <- length(alc_colnames)

alc_regs <- dat_ts %>% 
  filter(addictionCase == "Alcohol") 

percentage <- rep(NA, nrow(alc_regs))

for(i in 1:nrow(alc_regs)) {
  
  percentage[i] = length(which(is.na(alc_regs[i, alc_colnames])))/alc_n
  
}

percentage_all[which(dat_ts$addictionCase == "Alcohol")] <- percentage

drug_colnames <- colnames(dat_ts)[str_detect(colnames(dat_ts), regex("[Ii]nitial\\.(?!2nd|3rd|assess|date|.*Alc).*"))]
drug_n <- length(drug_colnames)

drug_regs <- dat_ts %>% 
  filter(addictionCase == "Drugs") 

percentage <- rep(NA, nrow(drug_regs))

for(i in 1:nrow(drug_regs)) {
  
  percentage[i] = length(which(is.na(drug_regs[i, drug_colnames])))/drug_n
  
}

percentage_all[which(dat_ts$addictionCase == "Drugs")] <- percentage

alcdrug_colnames <- colnames(dat_ts)[str_detect(colnames(dat_ts), regex("[Ii]nitial\\.(?!2nd|3rd|assess|date).*"))]
alcdrug_n <- length(alcdrug_colnames)

alcdrug_regs <- dat_ts %>% 
  filter(addictionCase == "Alcohol & Drugs") 

percentage <- rep(NA, nrow(alcdrug_regs))

for(i in 1:nrow(alcdrug_regs)) {
  
  percentage[i] = length(which(is.na(alcdrug_regs[i, alcdrug_colnames])))/alcdrug_n
  
}

percentage_all[which(dat_ts$addictionCase == "Alcohol & Drugs")] <- percentage

dat_ts["initial.missing.pct"] <- percentage_all

# remove users who have too much data missing on the initial assessment
dat_ts <- dat_ts %>% filter(initial.missing.pct < 0.8)

# remove users who miss information on all three drug consumption variables - 
#   this information cannot be imputed without knowing the substance of abuse
dat_ts <- dat_ts %>%
  filter(!(addictionCase != "Alcohol" & initial.1stDrug == "prefer not to say" & is.na(initial.1stDrug.Amount) & is.na(initial.1stDrug.Frequency) & is.na(initial.1stDrug.Unit)))

# remove users whose recorded assessment dates are not consecutive 
time_col_names <- colnames(dat_ts)[str_detect(colnames(dat_ts), regex(".+time", ignore_case = TRUE))]
consecutive <- rep(NA, nrow(dat_ts))

for(i in 1:nrow(dat_ts)) {
  
  # extract all assessment dates
  assess_times = dat_ts[i, time_col_names] %>% reduce(c)
  
  # remove NA
  assess_times = assess_times[!is.na(assess_times)]
  
  # see whether dates are consecutive
  consecutive[i] <- length(which(c(NA, diff(assess_times) >= 0) == FALSE)) == 0
  
}

(length(which(consecutive == FALSE)))

dat_ts <- dat_ts[which(consecutive == TRUE), ]

# look at whether the last module completion date always lies temporally before 
#   the last assessment update

mc_date_names <- tail(dates, n = 12)

# users with at least one module completion
pat <- "(?<=Complete\\.).+"
col_names <- colnames(dat_ts)[str_detect(colnames(dat_ts), regex(pat))]

have_module_completed <- dat_ts %>% 
  filter(if_any(.cols = all_of(col_names), .fns = ~ .x > 0))

# subset users who have modules completed, but haven't added assessment updates
have_mc_noupdate <- have_module_completed %>% 
  filter(My.Progress.Number.Started < 1)

# double-check whether the last module completion date of these users lies 
#   within two weeks of the initial assessment date
within_two_weeks <- rep(NA, nrow(have_mc_noupdate))

for(i in 1:nrow(have_mc_noupdate)) { 
  
  v = as.vector(as.matrix(have_mc_noupdate[i, mc_date_names]))
  max_ind = which(v == max(v, na.rm = TRUE))[1]
  col_name = mc_date_names[max_ind]
  
  two_week_frame = as.Date(have_mc_noupdate[i, "initial.date"] + 14)
  
  within_two_weeks[i] = two_week_frame > have_mc_noupdate[i, col_name]
  
}

# remove users whose dates of module completion lies beyond the two weeks mark 
#   of their initial assessment despite not having any assessment update logged
user_ids <- have_mc_noupdate[which(within_two_weeks == FALSE), "user.ID"]

dat_ts <- dat_ts %>% 
  filter(!user.ID %in% user_ids)

# remove users whose last module completion date lies beyond the two weeks mark 
#   after their most recent assessment update

have_mc_update <- have_module_completed %>% 
  filter(My.Progress.Number.Started > 1)

distance <- rep(NA, nrow(have_mc_update))

for(i in 1:nrow(have_mc_update)) { 
  
  v = as.vector(as.matrix(have_mc_update[i, mc_date_names]))
  max_ind = which(v == max(v, na.rm = TRUE))[1]
  col_name = mc_date_names[max_ind]
  
  two_week_frame = as.Date(have_mc_update[i, "Progress.Current.Date"] + 14)
  
  # if this time difference is positive, it means that the date of the last
  #   assessment update lies temporally posterior to the date of the last module
  #   completion
  distance[i] = two_week_frame - have_mc_update[i, col_name]
  
}

user_ids <- have_mc_update[which(distance < 0), "user.ID"]

dat_ts <- dat_ts %>% 
  filter(!user.ID %in% user_ids)

# create indicator variables of assessment update completeness
for(i in 1:78) {
  
  time = paste("Current.", i, ".time", sep = "")
  re_alc = paste("Current\\.", i, "\\.(?!time|.*Drug).*", sep = "")
  re_drugs = paste("Current\\.", i, "\\.(?!2nd|3rd|time|.*Alc).*", sep = "") 
  re_alcdrugs = paste("Current\\.", i, "\\.(?!2nd|3rd|time).*", sep = "")
  
  var_name = paste("my.Current.", i, ".assess.complete", sep = "")
  
  dat_ts[var_name] = rep(FALSE, nrow(dat_ts))
  
  # those who have an assessment completion time recorded
  alc_regs = dat_ts %>% 
    filter(!is.na(time)) %>%
    filter(addictionCase == "Alcohol") %>%
    filter(if_all(.cols = matches(re_alc, perl = TRUE), .fns = ~ !is.na(.x)))
  
  dat_ts[which(dat_ts$user.ID %in% alc_regs$user.ID), var_name] = TRUE
  
  drug_regs = dat_ts %>% 
    filter(!is.na(time)) %>%
    filter(addictionCase == "Drugs") %>%
    filter(if_all(.cols = matches(re_drugs, perl = TRUE), .fns = ~ !is.na(.x)))
  
  dat_ts[which(dat_ts$user.ID %in% drug_regs$user.ID), var_name] = TRUE
  
  alcdrug_regs = dat_ts %>% 
    filter(!is.na(time)) %>%
    filter(addictionCase == "Alcohol & Drugs") %>%
    filter(if_all(.cols = matches(re_alcdrugs, perl = TRUE), .fns = ~ !is.na(.x)))
  
  dat_ts[which(dat_ts$user.ID %in% alcdrug_regs$user.ID), var_name] = TRUE
  
}

# remove alcohol users whose typical daily alcohol units exceed 100 units
pat <- "alc.*units"
col_names <- colnames(dat_ts)[str_detect(colnames(dat_ts), regex(pat, ignore_case = T))][1:79]

dat_ts <- dat_ts %>%
  filter(if_all(.cols = all_of(col_names), .fns = ~ .x <= 100 | is.na(.x)))

# modify the "time_engagement" variables, so that they reflect when the last
#   touchpoint with a user was, and calculate the number of days and weeks 
#   accurately

all_time_col_names <- dates[-c(1, 4:104)]
max_date <- rep(NA, nrow(dat_ts))

for(i in 1:nrow(dat_ts)) { 
  
  v = as.vector(as.matrix(dat_ts[i, all_time_col_names]))
  max_date[i] = max(v, na.rm = TRUE) # only those will get assigned NA who don't
  #   have an initial date recorded, those
  #   with at least an initial date but no
  #   further activity are assigned zero
  
}

max_date <- as.Date(max_date)

dat_ts["time_engagement_days"] <- as.integer(difftime(max_date, dat_ts$initial.date, units = "days"))
dat_ts["time_engagement_weeks"] <- as.integer(difftime(max_date, dat_ts$initial.date, units = "weeks")) 

# determine how much time usually lies between opening up an account and the 
#   initial assessment date
dat_ts["time_to_engagement"] <- as.integer(difftime(dat_ts$initial.date, dat_ts$date.created, units = "days")) 

# remove users who indicate that they want to increase the number of units they
#   want to consume daily, or the number of days they want to consume a 
#   substance for in a week

alc_to_remove <- dat_ts %>% 
  filter(addictionCase == "Alcohol") %>%
  filter(Initial.AlcGoal.Units > Initial.Alc.Units | Initial.AlcGoal.Days < abs(Initial.Alc.Days - 7))

drugs_to_remove <- dat_ts %>%
  filter(addictionCase == "Drugs") %>%
  filter(Initial.DrugsGoal.Units > initial.1stDrug.Amount | Initial.DrugsGoal.Days < abs(initial.1stDrug.Frequency - 7))

alcdrugs_to_remove <- dat_ts %>% 
  filter(addictionCase == "Alcohol & Drugs") %>%
  filter(Initial.AlcGoal.Units > Initial.Alc.Units | Initial.AlcGoal.Days < abs(Initial.Alc.Days - 7) | Initial.DrugsGoal.Units > initial.1stDrug.Amount | Initial.DrugsGoal.Days < abs(initial.1stDrug.Frequency - 7))

ids_to_remove <- c(alc_to_remove$user.ID, drugs_to_remove$user.ID, alcdrugs_to_remove$user.ID)

dat_ts_valid_goals <- dat_ts %>% 
  filter(!user.ID %in% ids_to_remove)

# remove outliers with regards to drug consumption
no_outlier <- which(dat_ts_valid_goals$initial.1stDrug == "nitrous-oxide" & dat_ts_valid_goals$initial.1stDrug.Amount == 50)

m_outliers <- which((dat_ts_valid_goals$initial.1stDrug == "marijuana" | dat_ts_valid_goals$initial.1stDrug == "synthetic-cannabis") & dat_ts_valid_goals$initial.1stDrug.Amount > 499)

f_outlier <- which(dat_ts_valid_goals$initial.1stDrug == "fentanyl" & dat_ts_valid_goals$initial.1stDrug.Amount == 50000)

to_remove <- c(no_outlier, m_outliers, f_outlier)

ids_to_remove <- dat_ts_valid_goals[to_remove, "user.ID"]

dat_ts_valid_goals <- dat_ts_valid_goals %>%
  filter(!user.ID %in% ids_to_remove)

# remove users who indicate struggling only with drug use but have data filled 
#   out on the SDS for alcohol use as well
drug_user_with_SDS.Alc <- dat_ts_valid_goals %>%
  filter(addictionCase == "Drugs") %>%
  filter(if_any(.cols = matches("Initial\\.SDS\\.(Alc)\\d", perl = TRUE), ~ !is.na(.)))

dat_ts_valid_goals <- dat_ts_valid_goals %>%
  filter(!user.ID %in% drug_user_with_SDS.Alc$user.ID)

# save preprocessed dataset
saveRDS(dat_ts_valid_goals, 
        paste(wd, "/2023-01-06.csv", sep = ""),
        compress = FALSE)
