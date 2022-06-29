## ====================================================================================
## Purpose: Split hospitalised event counts by covariate level and time period
## 
## ====================================================================================

library(readr)
library(dplyr)
library(data.table) 
library(lubridate)
library(stringr)
library(tidyverse)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort_name <- "vaccinated"
  #cohort_name = "electively_unvaccinated"
}else{
  cohort_name <- args[[1]]
}

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "descriptives"))

#delta period
cohort_start = as.Date("2021-06-01", format="%Y-%m-%d")
cohort_end = as.Date("2021-12-14", format="%Y-%m-%d")

agebreaks <- c(0, 40, 60, 80, 111)
agelabels <- c("18_39", "40_59", "60_79", "80_110")

hosp_event_by_covariate_level <- function(cohort_name){
  
  # define analyses of interests
  active_analyses <- read_rds("lib/active_analyses.rds")
  active_analyses <- active_analyses %>%dplyr::filter(active == "TRUE")
  
  covar_names<-str_split(active_analyses$covariates, ";")[[1]]
  covar_names<-append(covar_names,"patient_id")

  outcomes<-active_analyses$outcome_variable
  
  survival_data <- read_rds(paste0("output/input_",cohort_name,"_stage1.rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort_name,".rds")) 
  end_dates$index_date <- NULL
  
  survival_data<- survival_data %>% left_join(end_dates, by="patient_id")
  rm(end_dates)
  
  results <- as.data.frame(matrix(ncol = 7, nrow = 0))
  colnames(results) <- c("event","Covariate","Level","unexposed_event_counts","days0_28_event_counts","days28_197_event_counts","subgroup")
  
  for(i in 1:length(outcomes)){
    
    print(i)
    event_short = gsub("out_date_", "",outcomes[i])
    setnames(survival_data,
             old = c(paste0("out_date_",event_short),
                     paste0(event_short,"_follow_up_end_unexposed"),
                     paste0(event_short,"_follow_up_end"),
                     paste0(event_short,"_hospitalised_follow_up_end"),
                     paste0(event_short,"_non_hospitalised_follow_up_end"),
                     paste0(event_short,"_hospitalised_date_expo_censor"),
                     paste0(event_short,"_non_hospitalised_date_expo_censor")),
             
             new = c("event_date",
                     "follow_up_end_unexposed",
                     "follow_up_end",
                     "hospitalised_follow_up_end",
                     "non_hospitalised_follow_up_end",
                     "hospitalised_date_expo_censor",
                     "non_hospitalised_date_expo_censor"))
    
    summary <- hosp_event_by_covariate_level_counts(survival_data, 
                                  event=outcomes[i],
                                  covar_names)
    results <- rbind(results, summary)

    setnames(survival_data,
             old = c("event_date",
                     "follow_up_end_unexposed",
                     "follow_up_end",
                     "hospitalised_follow_up_end",
                     "non_hospitalised_follow_up_end",
                     "hospitalised_date_expo_censor",
                     "non_hospitalised_date_expo_censor"),
             
             new = c(paste0("out_date_",event_short),
                     paste0(event_short,"_follow_up_end_unexposed"),
                     paste0(event_short,"_follow_up_end"),
                     paste0(event_short,"_hospitalised_follow_up_end"),
                     paste0(event_short,"_non_hospitalised_follow_up_end"),
                     paste0(event_short,"_hospitalised_date_expo_censor"),
                     paste0(event_short,"_non_hospitalised_date_expo_censor")))
    
    print(paste0("Covariate summary has been produced successfully for ", outcomes[i], " in ", cohort_name, " population!"))
  }
  
  results <- results %>% select(event, Covariate, Level, unexposed_event_counts, days0_28_event_counts, days28_197_event_counts, subgroup)
  
  # write output for table2
  write.csv(results, file=paste0("output/not-for-review/hospitalised_event_counts_by_covariate_level_",cohort_name, ".csv"), row.names = F)
}

hosp_event_by_covariate_level_counts <- function(survival_data, event,covar_names){
  data_active <- survival_data
  
  data_active <- data_active %>% mutate(event_date = replace(event_date, which(event_date>follow_up_end | event_date<index_date), NA))
  data_active <- data_active %>% mutate(exp_date_covid19_confirmed = replace(exp_date_covid19_confirmed, which(exp_date_covid19_confirmed>follow_up_end | exp_date_covid19_confirmed<index_date), NA))
  
  # filter the population to remove those without history of covid
  data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==F)
  
  data_active <- data_active %>% mutate(exp_date_covid19_confirmed = replace(exp_date_covid19_confirmed, which((!is.na(hospitalised_date_expo_censor)) & (exp_date_covid19_confirmed >= hospitalised_date_expo_censor)), NA))
  data_active <- data_active %>% mutate(event_date = replace(event_date, which((!is.na(hospitalised_date_expo_censor)) & (event_date >= hospitalised_date_expo_censor)), NA))
  
  data_active <- data_active %>% filter((index_date != hospitalised_date_expo_censor)|is.na(hospitalised_date_expo_censor))
  
  
  data_active <- data_active %>% filter((index_date != hospitalised_date_expo_censor)|is.na(hospitalised_date_expo_censor))
  data_active <- data_active %>% filter(index_date <= follow_up_end)
  
  data_active <- data_active %>% filter(!is.na(event_date))
  

  data_active$event_expo_status <- NA
  
  data_active$event_expo_status <- ifelse((data_active$event_date >= data_active$index_date & 
                                            data_active$event_date <= data_active$hospitalised_follow_up_end) &
    (data_active$event_date < data_active$exp_date_covid19_confirmed | is.na(data_active$exp_date_covid19_confirmed)),"pre_exposure","post_exposure")
  
  data_active$time_from_expo_to_event <- as.numeric(data_active$event_date - data_active$exp_date_covid19_confirmed)
  
  data_active$event_time_period <- ifelse(data_active$event_expo_status == "post_exposure" & (data_active$time_from_expo_to_event >=0 & data_active$time_from_expo_to_event < 28), "days0_28", "days28_197" )
  data_active$event_time_period <- ifelse(data_active$event_expo_status == "pre_exposure", "pre_exposure",data_active$event_time_period)

  # Pre-exposure event counts
  data_active_pre_expo <- data_active %>% filter(event_time_period == "pre_exposure")

  df <- data_active_pre_expo %>% dplyr::select(c( all_of(covar_names)))
  df <- df %>%  dplyr::select(!c("patient_id",
                                 df %>%  dplyr::select_if(is.numeric) %>% names()))
  
  summary_pre_expo <- as.data.frame(summary(df,maxsum=100))
  summary_pre_expo <- summary_pre_expo %>% filter(startsWith(Freq,"Mode")==F)
  summary_pre_expo$unexposed_event_counts <- summary_pre_expo$Freq
  summary_pre_expo$Freq=gsub(":.*", "",summary_pre_expo$Freq)#Remove everything after:
  summary_pre_expo$unexposed_event_counts=gsub(".*:", "",summary_pre_expo$unexposed_event_counts)#Remove everything before
  summary_pre_expo$Var2 <- gsub("\\s","",summary_pre_expo$Var2)
  summary_pre_expo$Var1 <- NULL
  
  
  # Days 0-28 event counts
  data_active_days0_28 <- data_active %>% filter(event_time_period == "days0_28")
  
  df <- data_active_days0_28 %>% dplyr::select(c( all_of(covar_names)))
  df <- df %>%  dplyr::select(!c("patient_id",
                                 df %>%  dplyr::select_if(is.numeric) %>% names()))
  
  summary_days0_28 <- as.data.frame(summary(df,maxsum=100))
  summary_days0_28 <- summary_days0_28 %>% filter(startsWith(Freq,"Mode")==F)
  summary_days0_28$days0_28_event_counts <- summary_days0_28$Freq
  summary_days0_28$Freq=gsub(":.*", "",summary_days0_28$Freq)#Remove everything after:
  summary_days0_28$days0_28_event_counts=gsub(".*:", "",summary_days0_28$days0_28_event_counts)#Remove everything before
  summary_days0_28$Var2 <- gsub("\\s","",summary_days0_28$Var2)
  summary_days0_28$Var1 <- NULL
  
  # Days 28-197 event counts
  data_active_days28_197 <- data_active %>% filter(event_time_period == "days28_197")
  
  df <- data_active_days28_197 %>% dplyr::select(c( all_of(covar_names)))
  df <- df %>%  dplyr::select(!c("patient_id",
                                 df %>%  dplyr::select_if(is.numeric) %>% names()))
  
  summary_days28_197 <- as.data.frame(summary(df,maxsum=100))
  summary_days28_197 <- summary_days28_197 %>% filter(startsWith(Freq,"Mode")==F)
  summary_days28_197$days28_197_event_counts <- summary_days28_197$Freq
  summary_days28_197$Freq=gsub(":.*", "",summary_days28_197$Freq)#Remove everything after:
  summary_days28_197$days28_197_event_counts=gsub(".*:", "",summary_days28_197$days28_197_event_counts)#Remove everything before
  summary_days28_197$Var2 <- gsub("\\s","",summary_days28_197$Var2)
  summary_days28_197$Var1 <- NULL
  
  summary_pre_expo <- summary_pre_expo %>% left_join(summary_days0_28, by = c("Var2","Freq"))
  summary_pre_expo <- summary_pre_expo %>% left_join(summary_days28_197, by = c("Var2","Freq"))
  
  summary_pre_expo[is.na(summary_pre_expo)] = 0
  summary_pre_expo$event <- event
  summary_pre_expo$subgroup <- "hospitalised"
  
  setnames(summary_pre_expo,
           old = c("Var2",
                   "Freq"),
           new = c("Covariate",
                   "Level"))
  
  
  return(summary_pre_expo)
}


# Run function using specified commandArgs
if(cohort_name == "both"){
  hosp_event_by_covariate_level("vaccinated")
  hosp_event_by_covariate_level("electively_unvaccinated")
}else{
  hosp_event_by_covariate_level(cohort_name)
}

