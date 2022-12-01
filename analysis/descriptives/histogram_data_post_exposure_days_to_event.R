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
days_follow_up = 196

histogram_events <- function(cohort_name){
  
  #----------------------Define analyses of interests---------------------------
  active_analyses <- read_rds("lib/active_analyses.rds")
  
  active_analyses <- active_analyses %>%dplyr::filter(active == "TRUE")
  
  analyses_of_interest <- as.data.frame(matrix(ncol = 5,nrow = 0))
  
  outcomes<-active_analyses$outcome_variable
  
  #--------------------Load data and left join end dates------------------------
  survival_data <- read_rds(paste0("output/input_",cohort_name,"_stage1.rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort_name,".rds")) 
  end_dates$index_date <- NULL
  
  survival_data<- survival_data %>% left_join(end_dates, by="patient_id")
  rm(end_dates)
  
  survival_data<-survival_data[,c("patient_id","index_date","cov_cat_sex",
                                  "cov_num_age","cov_cat_ethnicity",
                                  "sub_bin_covid19_confirmed_history","exp_date_covid19_confirmed","sub_cat_covid19_hospital",
                                  colnames(survival_data)[grepl("out_",colnames(survival_data))],
                                  colnames(survival_data)[grepl("follow_up",colnames(survival_data))],
                                  colnames(survival_data)[grepl("_expo_",colnames(survival_data))],
                                  unique(active_analyses$prior_history_var[active_analyses$prior_history_var !=""]))]
  
  setnames(survival_data, 
           old = c("cov_cat_sex", 
                   "cov_cat_ethnicity"), 
           new = c("sex",
                   "ethnicity"))
  
  analyses_of_interest <- crossing(outcomes, cohort_name, c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
  colnames(analyses_of_interest) <- c("event","cohort","subgroup")
  analyses_of_interest$stratify_by <- NA
  analyses_of_interest$stratify_by <- ifelse(analyses_of_interest$subgroup == "covid_pheno_hospitalised", "hospitalised", ifelse(analyses_of_interest$subgroup == "covid_pheno_nonhospitalised","non_hospitalised","main"))
  
  # Create empty results data frame
  output <- as.data.frame(matrix(ncol = 5, nrow = 0))
  colnames(output) <- c("event_days_post_covid","n","event","subgroup","cohort")
  #-----------Populate analyses_of_interest with events counts/follow up--------
  for(i in 1:nrow(analyses_of_interest)){
    print(paste0("Working on ", analyses_of_interest$event[i]," ", analyses_of_interest$subgroup[i]))
    
    event_short = gsub("out_date_", "",analyses_of_interest$event[i])
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
    
    histogram_output <- histogram_output_calculation(survival_data,
                                         event=analyses_of_interest$event[i],
                                         cohort=analyses_of_interest$cohort[i],
                                         subgroup=analyses_of_interest$subgroup[i],
                                         stratify_by=analyses_of_interest$stratify_by[i])
    
    output <- rbind(output,histogram_output)
    
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
    
    print(paste0("histogram data has been produced successfully for", analyses_of_interest$event[i], " in ", cohort_name, " population!"))
  }
  
  
  # write output for histogram data
  write.csv(output, file=paste0("output/review/descriptives/histogram_data_",cohort_name, ".csv"), row.names = F)
}

histogram_output_calculation <- function(survival_data, event,cohort,subgroup,stratify_by){
  data_active <- as.data.table(survival_data)
  data_active$date_expo_censor <- NA
  
  for(i in c("hospitalised","non_hospitalised")){
    if(stratify_by == i){
      data_active$follow_up_end <- NULL
      data_active$date_expo_censor <- NULL
      setnames(data_active, 
               old = c(c(paste0(i,"_follow_up_end")),
                       c(paste0(i,"_date_expo_censor"))),
               
               new = c("follow_up_end",
                       "date_expo_censor"))
    }
  }
  
  # filter the population to remove those with a history of COVID-19
  data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==F)

  #Filter to subgroup of interest
  if(startsWith(subgroup,"covid_pheno_")){
    data_active <- data_active %>% mutate(exp_date_covid19_confirmed = replace(exp_date_covid19_confirmed, which(!is.na(date_expo_censor) & (exp_date_covid19_confirmed >= date_expo_censor)), NA) )%>%
      mutate(event_date = replace(event_date, which(!is.na(date_expo_censor) & (event_date >= date_expo_censor)), NA)) %>%
      filter((index_date != date_expo_censor)|is.na(date_expo_censor))
    
    data_active[follow_up_end == date_expo_censor, follow_up_end := follow_up_end-1]
    # setDT(data_active)[follow_up_end == date_expo_censor, follow_up_end := follow_up_end-1]
  }
  
  data_active <- data_active %>% mutate(event_date = replace(event_date, which(event_date>follow_up_end | event_date<index_date), NA),
                                        exp_date_covid19_confirmed = replace(exp_date_covid19_confirmed, which(exp_date_covid19_confirmed>follow_up_end | exp_date_covid19_confirmed<index_date), NA))
  
  data_active=data_active%>%filter(follow_up_end>=index_date)
  
  #Filter to only post covid events
  data_active <- data_active %>% filter(data_active$event_date >= data_active$index_date &
                                          data_active$event_date >= data_active$exp_date_covid19_confirmed & 
                                          data_active$event_date <= data_active$follow_up_end)
  
  
  data_active <- data_active %>% 
    mutate(days_post_covid = difftime(event_date, exp_date_covid19_confirmed, units = "days")) %>%
    mutate(days_post_covid = as.numeric(days_post_covid))
  
  data_active <- as.data.frame(data_active$days_post_covid)
  colnames(data_active) <- c("days_post_covid")
  data_active <- data_active %>% count(days_post_covid)
  
  #Ensure all counts are above 5
  data_active <- data_active %>%
    mutate(n = pmax(6, n))
  
  tmp <- as.data.frame(seq(0,days_follow_up,1))
  colnames(tmp) <- "days_post_covid"
  tmp <- tmp %>% left_join(data_active)
  tmp$n <- ifelse(is.na(tmp$n),0,tmp$n)
  
  tmp$event <- event
  tmp$subgroup <- subgroup
  tmp$cohort <- cohort

  return(tmp)
}

# Run function using specified commandArgs
if(cohort_name == "both"){
  histogram_events("vaccinated")
  histogram_events("electively_unvaccinated")
}else{
  histogram_events(cohort_name)
}

