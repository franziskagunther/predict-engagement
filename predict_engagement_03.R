#  Predict engagement with BFO
#
# This script reads in preprocessed data (select features and outcomes) from 
#   users of Breaking Free Online (https://www.breakingfreegroup.com/), a 
#   digital therapy app addressing substance use disorder and related 
#   comorbidities, and attempts to predict various operationalisations of
#   engagement. 
# 
# R version: R 4.2.1 GUI RStudio 2022.07.2 Build 576
#   OS: macOS Monterey 12.6 (21G115)

library(tidymodels)
library(rcompanion)
library(ggcorrplot)

# read in data
wd <- getwd()
dat_pg <- readRDS("2023-01-07_prep.csv",
                  file = paste(wd, "/2023-01-07_prep.csv", sep = ""))

outcomes <- c("time_engagement_days", "modules_completed", "psychoeducation_completed",
              "actionstrategy_completed", "use_events", "use_rate", "days_engaged",
              "median_intermission_length", "mad_intermission_length")

# exclude certain variables from prediction models
to_exclude <- c("user.ID", "time_engagement_weeks", "My.Progress.Number.Started")

exclude_all <- c(outcomes, to_exclude)

# binary outcomes

for(i in 1:8) {
  
  dat_prediction = dat_pg %>%
    mutate(engaged = as.factor(modules_completed >= i)) %>%
    # convert back to numeric
    mutate(across(matches("Days"), ~ as.numeric(as.character(.)))) 
  
  dat_prediction = dat_prediction[, -which(colnames(dat_prediction) %in% exclude_all)]
  
  trsf = dat_prediction[complete.cases(dat_prediction), ]
  
  folds = vfold_cv(trsf, v = 10, strata = engaged)
  
  # 'boost_tree(mode = "classification", engine = "xgboost")' for XGBoost
  model = rand_forest(engine = "ranger", mode = "classification")
  
  wflow = 
    workflow() %>% 
    add_formula(engaged ~ .) %>% 
    add_model(model)
  
  start_time <- Sys.time()
  
  results <-
    fit_resamples(
      wflow,
      resamples = folds,
      metrics = metric_set(roc_auc), 
      control = control_resamples(
        event_level = "second",
        save_pred = TRUE)
      
    )
  
  end_time = Sys.time()

  metrics = collect_metrics(results, summarize = TRUE)
  
  mean = metrics[["mean"]]
  std_err = metrics[["std_err"]]
  
  ci_lower = mean - 1.96*std_err
  ci_higher = mean + 1.96*std_err
  
  print(paste(mean, " [", ci_lower, "-", ci_higher, "]", sep = ""))
  
}

# continuous outcomes on a log scale

plot_data_log <- list()

for(i in seq_along(outcomes)) { 
  
  outcome = outcomes[i]
  not_outcomes = outcomes[-i]
  
  exclude_all = c(to_exclude, not_outcomes)
  
  dat_prediction = dat_pg[, -which(colnames(dat_pg) %in% exclude_all)]
  
  dat_prediction = dat_prediction %>%
    mutate(across(matches("Days"), ~ as.numeric(as.character(.)))) #%>%
  
  trsf = dat_prediction[complete.cases(dat_prediction), ]
  
  if(str_detect(outcome, regex("completed|intermission"))) {
    
    trsf[[outcome]] = log(trsf[[outcome]] + 1)
    
  } else {
    
    trsf[[outcome]] = log(trsf[[outcome]])
    
  }
  
  # Get the resampling folds
  folds = vfold_cv(trsf, v = 10, strata = as.name(outcome))
  
  # Define a model
  model = rand_forest(engine = "ranger", mode = "regression")#boost_tree(mode = "regression", engine = "xgboost")###linear_reg(engine = "glm")
  
  # Define a workflow
  wflow = 
    workflow() %>% 
    add_formula(as.formula(paste(outcome,
                                 "~ ."))) %>% 
    add_model(model)
  
  # Fit the model to the resamples, compute metrics
  start_time <- Sys.time()
  
  results <-
    fit_resamples(
      wflow,
      resamples = folds,
      metrics = metric_set(rmse), 
      control = control_resamples(
        save_pred = TRUE)
      
    )
  
  end_time <- Sys.time()
  
  metrics = collect_metrics(results, summarize = TRUE)
  
  mean = metrics[["mean"]]
  std_err = metrics[["std_err"]]
  
  ci_lower = mean - 1.96*std_err
  ci_higher = mean + 1.96*std_err
  
  print(paste(mean, " [", ci_lower, "-", ci_higher, "]", sep = ""))
  
  predictions = collect_predictions(results, summarize = TRUE)
  colnames(predictions) = c("row", "observed", "config", "predicted")
  
  plot_data_log[[i]] = predictions
  
}

# report correlations
for(i in 1:length(plot_data_log)) {
  
  print(cor(plot_data_log[[i]]["observed"], plot_data_log[[i]]["predicted"]))
  
}

# correlate every feature and the outcome with each other
exclude <- c("user.ID", "time_engagement_weeks", "My.Progress.Number.Started")
corr_df <- dat_pg[, -which(colnames(dat_pg) %in% exclude)]

variable_pairs <- combn(colnames(corr_df), 2, simplify = FALSE)

corr_mat <- matrix(data = NA, nrow = ncol(corr_df), ncol = ncol(corr_df))
rownames(corr_mat) <- colnames(corr_df)
colnames(corr_mat) <- colnames(corr_df)
diag(corr_mat) <- rep(1, ncol(corr_df))

for(i in 1:length(variable_pairs)) {
  
  inside_df = corr_df
  
  var1 = variable_pairs[[i]][1]
  var2 = variable_pairs[[i]][2]
  
  print(variable_pairs[[i]])
  
  if((var1 == "substance" | var2 == "substance") & (str_detect(var1, regex("SDS|SUD")) | str_detect(var2, regex("SDS|SUD")))) {
    
    inside_df = inside_df[-21977, ] # 21873
    inside_df = inside_df %>% droplevels()
    
  }
  
  df_1 = inside_df[[variable_pairs[[i]][1]]]
  df_2 = inside_df[[variable_pairs[[i]][2]]]
  
  if(is.factor(df_1) | is.factor(df_2)) {
    
    if(is.ordered(df_1) | is.ordered(df_2)) {
      
      if(is.ordered(df_1) & is.ordered(df_2)) {
        
        print("polychoric correlation: ordinal - ordinal")
        res = psych::polychoric(table(df_1, df_2))[["rho"]]
        corr_mat[var1, var2] = res
        corr_mat[var2, var1] = res
        
      } else {
        
        if(is.factor(df_1) & is.factor(df_2)) {
          
          print("Cramers V: ordinal - nominal (treat ordinal as nominal)")
          res = unname(cramerV(table(df_1, df_2)))
          corr_mat[var1, var2] = res
          corr_mat[var2, var1] = res
          
        } else {
          
          if(is.factor(df_1)) {
            
            r2 = summary(lm(df_2 ~ df_1, na.action = na.omit))$r.squared
            
            print("sqrt of R^2: ordinal - continuous")
            res = sqrt(r2)
            corr_mat[var1, var2] = res
            corr_mat[var2, var1] = res
            
          } else {
            
            r2 = summary(lm(df_1 ~ df_2, na.action = na.omit))$r.squared
            
            print("sqrt of R^2: ordinal - continuous")
            res = sqrt(r2)
            corr_mat[var1, var2] = res
            corr_mat[var2, var1] = res
            
          }
          
        }
        
      }
      
    } else {
      
      if(is.factor(df_1) & is.factor(df_2)) {
        
        print("Cramers V: nominal - nominal")
        res = unname(cramerV(table(df_1, df_2)))
        corr_mat[var1, var2] = res
        corr_mat[var2, var1] = res
        
      } else {
        
        if(is.factor(df_1)) {
          
          r2 = summary(lm(df_2 ~ df_1, na.action = na.omit))$r.squared
          
          print("sqrt of R^2: nominal - continuous")
          res = sqrt(r2)
          corr_mat[var1, var2] = res
          corr_mat[var2, var1] = res
          
        } else {
          
          r2 = summary(lm(df_1 ~ df_2, na.action = na.omit))$r.squared
          
          print("sqrt of R^2: nominal - continuous")
          res = sqrt(r2)
          corr_mat[var1, var2] = res
          corr_mat[var2, var1] = res
          
        }
        
      }
      
    }
    
  } else {
    
    print("Pearson correlation: continuous - continuous")
    res = cor(df_1, df_2, use="complete.obs")
    corr_mat[var1, var2] = res
    corr_mat[var2, var1] = res
    
  }
  
}

new_names <- c("Gender", "Age", "Substance group", "WHOQOL-BREF: Quality of life",
               "WHOQOL-BREF: Satisfaction with health", "WHOQOL-BREF: Satisfaction with daily functioning",
               "WHOQOL-BREF: Satisfaction with relationships", 
               "WHOQOL-BREF: Satisfaction with professional capacity", 
               "Coping with life's difficulties", "Quality of life Likert",
               "RPM: Conflict with partner/relative/friend", 
               "RPM: Professional problems Diff. Sit.", "RPM: Financial difficulties",
               "RPM: Substance use despite risks", "RPM: Pressure to use substance",
               "RPM: Impact of difficult situations", "RPM: Cannot cope anymore",
               "RPM: Low self-worth", "RPM: Worries about own health", "RPM: Paranoid",
               "RPM: Loss of control", "RPM: Impact of negative thoughts",
               "RPM: Cravings", "RPM: Shakes", "RPM: Cramps", "RPM: Nausea", 
               "RPM: Tired", "RPM: Impact of physical sensations", 
               "RPM: Inactive", "RPM: Aggressive", "RPM: Social withdrawal",
               "RPM: Lack of self care", "RPM: Conflicts with law", 
               "RPM: Impact of unhelpful behaviours", "RPM: Health problems",
               "RPM: Professional problems Lifestyle", "RPM: Problems around leisure activities",
               "RPM: Problems in relationships", "RPM: Housing problems",
               "RPM: Impact of lifestyle", "PHQ-4: Uncontrolled worry",
               "PHQ-4: Hopeless", "PHQ-4: Joyless", "Negative self-perception",
               "PHQ-4: Nervous", "Impact of emotions", "Abstinence", 
               "Days registration - first assessment", "Ethnicity", "Anxiety screen",
               "Depression screen", "SUD screen", "Accessed days", "Strategies completed",
               "Information strategies completed", "Action strategies completed",
               "Use events", "Use rate", "Percentage actively engaged days",
               "Median intermission length", "MAD intermission length",
               "Specific substance", "SDS: Lack of control over substance use",
               "SDS: Worries of missing substance", "SDS: Worries about own substance use",
               "SDS: Wish to quit substance", "SDS: Difficulty to stop", "Impact of substance use",
               "Substance using days (past)", "Substance using days (goal)",
               "Complexity")

rownames(corr_mat) <- new_names
colnames(corr_mat) <- new_names

corr_plot <- ggcorrplot(corr_mat, tl.cex = 6)

ggsave("corr-plot.jpg",
       corr_plot,
       dpi = 300,
       width = 10,
       height = 10)
