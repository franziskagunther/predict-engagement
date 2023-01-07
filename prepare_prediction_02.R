#  Prepare a dataset for prediction of engagement with BFO
#
# This script reads in preprocessed data from users of Breaking Free Online 
#   (https://www.breakingfreegroup.com/), a digital therapy app addressing 
#   substance use disorder and related comorbidities. 
# 
# The data is then prepared for the engagement prediction pipeline. 
# 
# R version: R 4.2.1 GUI RStudio 2022.07.2 Build 576
#   OS: macOS Monterey 12.6 (21G115)

wd <- getwd()
dat <- readRDS("2023-01-06",
               file = paste(wd, "/2023-10-06.csv", sep = "")
)

depression_names <-c("Initial.Emo.PHQ3", "Initial.Emo.PHQ4")
anxiety_names <- c("Initial.Emo.PHQ1", "Initial.Emo.PHQ2")
AUD_names <- c("Initial.SDS.Alc1", "Initial.SDS.Alc2", "Initial.SDS.Alc3",
               "Initial.SDS.Alc4", "Initial.SDS.Alc5")
DUD_names <- c("initial.SDS.Drug1", "initial.SDS.Drug2", "initial.SDS.Drug3",
               "initial.SDS.Drug4", "initial.SDS.Drug5")

cols <- colnames(dat)

# add baseline diagnoses
dat <- dat %>%
  mutate(Initial.Depression = rowSums(across(all_of(depression_names))) >= 3) %>%
  mutate(Initial.Anxiety = rowSums(across(all_of(anxiety_names))) >= 3) %>%
  mutate(Initial.SUD = ifelse(addictionCase == "Alcohol", rowSums(across(all_of(AUD_names))) >= 3, ifelse(addictionCase == "Drugs", rowSums(across(all_of(DUD_names))) >= 3, 
                                                                                                          ifelse(specialAddiction == "Alcohol", rowSums(across(all_of(AUD_names))) >= 3, rowSums(across(all_of(DUD_names))) >= 3)))) %>%
  # labelling user non-abstinent if there information on the baseline daily or 
  #   weekly consumption is missing
  mutate(Initial.abstinence = ifelse(addictionCase == "Alcohol", Initial.Alc.Days == 0 & Initial.Alc.Units == 0, ifelse(addictionCase == "Drugs", initial.1stDrug.Frequency == 0 & initial.1stDrug.Amount == 0, 
                                                                                                                        ifelse(specialAddiction == "Alcohol", Initial.Alc.Days == 0 & Initial.Alc.Units == 0, initial.1stDrug.Frequency == 0 & initial.1stDrug.Amount == 0)))) %>%
  mutate(modules_completed = rowSums(across(all_of(cols[str_detect(cols, regex("^Complete\\."))])))) %>% 
  mutate(psychoeducation_completed = rowSums(across(all_of(cols[str_detect(cols, regex("Complete\\..*1"))])))) %>% 
  mutate(actionstrategy_completed = rowSums(across(all_of(cols[str_detect(cols, regex("Complete\\..*2"))])))) %>%
  mutate(use_events = modules_completed + My.Progress.Number.Started + 1) %>%
  mutate(time_engagement_days = time_engagement_days + 1) %>%
  mutate(use_rate = use_events/time_engagement_days) 

# find out the percentage of dates of module completion which fall together with 
#   dates of assessment completion 

num_days_module_and_assess_completion <- rep(NA, nrow(dat))
assessments <- rep(NA, nrow(dat))

assessment_date_cols <- cols[str_detect(cols, regex(".+(time|[Dd]ate)(?!.*created|.*engagement)"))]
module_completion_date_cols <- cols[str_detect(cols, regex("Date\\."))]

for(i in 1:nrow(dat)) {
  
  dates = dat[i, assessment_date_cols] %>% reduce(c)
  unique_assessments = unique(dates)[!is.na(unique(dates))]
  
  dates = dat[i, module_completion_date_cols] %>% reduce(c)
  unique_modules = unique(dates)[!is.na(unique(dates))]
  
  if(length(unique_assessments) > 0 & length(unique_modules) > 0) {
    
    num_days_module_and_assess_completion[i] = length(intersect(unique_assessments, unique_modules))#/length(unique_assessments)
    assessments[i] = length(unique_assessments)
    
  }
  
}

(sum(num_days_module_and_assess_completion, na.rm = TRUE)/sum(assessments, na.rm = TRUE))

# days on which assessments were completed are used as a proxy for the days on
#   which users engaged

# compute the mean and sd of the length of intermissions between days of
#   engagement for users - only possible for users which
#   actually had at least one assessment update
intermission_lengths <- list()
assessment_date_cols <- cols[str_detect(cols, regex("(initial\\.date|Current\\.(\\d)+\\.time)"))]
uniques <- rep(NA, nrow(dat))

calculate_date_differences <- function(date_vec) {
  
  idxs = seq_along(date_vec)
  
  idxs_pairs = rbind(idxs[-length(idxs)], idxs[-1])
  
  date_differences = rep(NA, length(date_vec) - 1)
  
  for(i in 1:ncol(idxs_pairs)) {
    
    date_differences[i] = date_vec[idxs_pairs[2, i]] - date_vec[idxs_pairs[1, i]]
    
  }
  
  return(date_differences)
  
}

for(i in 1:nrow(dat)) {
  
  dates = dat[i, assessment_date_cols] %>% reduce(c)
  unique_assessments = sort(dates[!is.na(dates)])
  uniques[i] = length(unique(unique_assessments))
  
  if(length(unique_assessments) > 1) {
    
    intermission_lengths[[i]] = calculate_date_differences(unique_assessments)
    
  } else{
    
    intermission_lengths[[i]] = NA
    
  }
  
}

intermissions <- unlist(intermission_lengths)

median_intermission_length <- unlist(lapply(intermission_lengths, median))
mad_intermission_length <- unlist(lapply(intermission_lengths, mad))

# assign NA for those without an assessment update
dat["median_intermission_length"] <- median_intermission_length
dat["mad_intermission_length"] <- mad_intermission_length

# compute the number of days engaged before dropout
dat <- dat %>%
  mutate(num_unique_assessments = uniques) %>%
  mutate(days_engaged = (num_unique_assessments/(time_engagement_days)) * 100) 

dat <- dat %>%
  mutate(substance = ifelse(addictionCase == "Alcohol", "alcohol", ifelse(addictionCase == "Drugs", initial.1stDrug, 
                                                                          ifelse(specialAddiction == "Alcohol", "alcohol", initial.1stDrug)))) %>% 
  mutate(Initial.SDS.1 = ifelse(addictionCase == "Alcohol", Initial.SDS.Alc1, ifelse(addictionCase == "Drugs", initial.SDS.Drug1, 
                                                                                     ifelse(specialAddiction == "Alcohol", Initial.SDS.Alc1, initial.SDS.Drug1)))) %>%
  mutate(Initial.SDS.2 = ifelse(addictionCase == "Alcohol", Initial.SDS.Alc2, ifelse(addictionCase == "Drugs", initial.SDS.Drug2, 
                                                                                     ifelse(specialAddiction == "Alcohol", Initial.SDS.Alc2, initial.SDS.Drug2)))) %>%
  mutate(Initial.SDS.3 = ifelse(addictionCase == "Alcohol", Initial.SDS.Alc3, ifelse(addictionCase == "Drugs", initial.SDS.Drug3, 
                                                                                     ifelse(specialAddiction == "Alcohol", Initial.SDS.Alc3, initial.SDS.Drug3)))) %>%
  mutate(Initial.SDS.4 = ifelse(addictionCase == "Alcohol", Initial.SDS.Alc4, ifelse(addictionCase == "Drugs", initial.SDS.Drug4, 
                                                                                     ifelse(specialAddiction == "Alcohol", Initial.SDS.Alc4, initial.SDS.Drug4)))) %>%
  mutate(Initial.SDS.5 = ifelse(addictionCase == "Alcohol", Initial.SDS.Alc5, ifelse(addictionCase == "Drugs", initial.SDS.Drug5, 
                                                                                     ifelse(specialAddiction == "Alcohol", Initial.SDS.Alc5, initial.SDS.Drug5)))) %>%
  mutate(Initial.Substance.Likert = ifelse(addictionCase == "Alcohol", Initial.Alc.Likert, ifelse(addictionCase == "Drugs", initial.Drug.Likert, 
                                                                                                  ifelse(specialAddiction == "Alcohol", Initial.Alc.Likert, initial.Drug.Likert)))) %>%
  mutate(Initial.Substance.Days = ifelse(addictionCase == "Alcohol", Initial.Alc.Days, ifelse(addictionCase == "Drugs", initial.1stDrug.Frequency, 
                                                                                              ifelse(specialAddiction == "Alcohol", Initial.Alc.Days, initial.1stDrug.Frequency)))) %>%
  mutate(Initial.Substance.Goal.Days = ifelse(addictionCase == "Alcohol", Initial.AlcGoal.Days, ifelse(addictionCase == "Drugs", Initial.DrugsGoal.Days, 
                                                                                                       ifelse(specialAddiction == "Alcohol", Initial.AlcGoal.Days, Initial.DrugsGoal.Days)))) %>%
  mutate(complexity = rowSums(across(c(Initial.DiffSits.Money, Initial.Phys.Cravings, Initial.Life.Health, Initial.Life.WorkEdu, Initial.Life.Housing)), na.rm = TRUE))

# select variables for prediction
datpred <- dat %>%
  # make some questionnaire items ordered factors
  select(user.ID, 
         gender:age, addictionCase,
         Initial.QoL1:Initial.Emo.Likert,
         Initial.abstinence, 
         time_to_engagement, 
         ethn_coarse, 
         Initial.Anxiety, 
         Initial.Depression, 
         Initial.SUD,
         time_engagement_days,
         time_engagement_weeks,
         modules_completed,
         psychoeducation_completed,
         actionstrategy_completed,
         use_events,
         use_rate,
         days_engaged,
         median_intermission_length,
         mad_intermission_length, 
         substance:Initial.Substance.Goal.Days,
         My.Progress.Number.Started,
         complexity
  ) 

datpred <- datpred %>%
  # turn NaN and "NaN" on the Initial.Alc.Days, Initial.Alc.Units, 
  #   Initial.AlcGoal.Days, Initial.AlcGoal.Units and my.weekly.drinking.1 
  #   variables into NA
  mutate(across(.cols = everything(), ~ na_if(., "NaN"))) %>%
  mutate(across(.cols = everything(), ~ na_if(., NaN))) %>%
  mutate(across(contains("Likert") | contains("Emo") | contains("Difficulties") | contains("QoL") | contains("SDS") | contains("Substance.Days") | contains("Substance.Goal.Days"),
                ordered)) %>%
  mutate(across((contains("user.ID") | contains("gender") | contains("addictionCase") | contains("ethn") | contains("DiffSits") | contains("NegTho") | contains("Phys") | contains("Behav") | contains("Life") | contains("SUD") | contains("Depression") | contains("Anxiety") | contains("abstinence") | contains("assess.complete") | contains("substance")) & !contains("Likert"),
                factor)) %>%
  mutate(across(contains("time") | contains("completed") | contains("use_events") | contains("complexity"), as.numeric))

saveRDS(dat_pred, 
        paste(wd, "/2023-01-07_prep.csv", sep = ""),
        compress = FALSE)