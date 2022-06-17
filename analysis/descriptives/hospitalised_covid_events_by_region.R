#Splits hospitalised COVID events by region

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


hosp_covid_events <- function(cohort_name, group){
  
  # define analyses of interests
  active_analyses <- read_rds("lib/active_analyses.rds")
  active_analyses <- active_analyses %>%dplyr::filter(active == "TRUE")
  
  event<-active_analyses$outcome_variable
  
  survival_data <- read_rds(paste0("output/input_", cohort_name,"_stage1_", group,".rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort_name,"_",group,".rds")) 
  end_dates$index_date <- NULL
  
  survival_data<- survival_data %>% left_join(end_dates, by="patient_id")
  rm(end_dates)
  
  survival_data<-survival_data[,c("patient_id","index_date","cov_cat_sex","cov_cat_region",
                                  "cov_num_age","cov_cat_ethnicity",
                                  "sub_bin_covid19_confirmed_history","exp_date_covid19_confirmed","sub_cat_covid19_hospital",
                                  colnames(survival_data)[grepl("out_",colnames(survival_data))],
                                  colnames(survival_data)[grepl("follow_up",colnames(survival_data))],
                                  colnames(survival_data)[grepl("_expo_",colnames(survival_data))],
                                  active_analyses$prior_history_var[active_analyses$prior_history_var !=""])]
  
  setnames(survival_data, 
           old = c("cov_cat_sex", 
                   "cov_cat_ethnicity"), 
           new = c("sex",
                   "ethnicity"))
  
  setDT(survival_data)
  
  survival_data <- survival_data %>% mutate(cov_cat_region = as.character(cov_cat_region))%>%
    mutate(cov_cat_region = case_when(cov_cat_region=="London" ~ "South East, including London",
                                   cov_cat_region=="South East" ~ "South East, including London",
                                   cov_cat_region=="West Midlands" ~ "Midlands",
                                   cov_cat_region=="East Midlands" ~ "Midlands",
                                   cov_cat_region=="North West" ~ "North West",
                                   cov_cat_region=="North East" ~ "North East",
                                   cov_cat_region=="East" ~ "East",
                                   cov_cat_region=="Yorkshire and The Humber" ~ "Yorkshire and The Humber",
                                   cov_cat_region=="South West" ~ "South West",
    )) %>%
    mutate(cov_cat_region = as.factor(cov_cat_region))%>%
    mutate(cov_cat_region = relevel(cov_cat_region,ref="South East, including London"))
  
  regions <- as.character(levels(survival_data$cov_cat_region))
  analyses_of_interest <- crossing(event,regions)
  analyses_of_interest$subgroup <- "covid_pheno_hospitalised"
  analyses_of_interest$unexposed_event_counts <- NA
  analyses_of_interest$post_exposure_event_counts <- NA
  analyses_of_interest <- as.data.frame(analyses_of_interest)
  
  for(i in 1:nrow(analyses_of_interest)){
    
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
    
    analyses_of_interest[i,4:5] <- hosp_covid_event_counts(survival_data, 
                                                             event=analyses_of_interest$event[i],
                                                             region=analyses_of_interest$regions[i])
    
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
    
    print(paste0("event counts have been produced successfully for ", analyses_of_interest$event[i], " for region ", analyses_of_interest$regions[i]))
  }
  
  
  # analyses_of_interest <- analyses_of_interest %>%
  #   group_by(event) %>%
  #   dplyr::mutate(unexposed_event_counts = case_when(
  #     any(unexposed_event_counts == "[Redacted]") ~ "[Redacted]",
  #     TRUE ~ as.character(unexposed_event_counts)))
  # 
  # analyses_of_interest <- analyses_of_interest %>%
  #   group_by(event) %>%
  #   dplyr::mutate(post_exposure_event_counts = case_when(
  #     any(post_exposure_event_counts == "[Redacted]") ~ "[Redacted]",
  #     TRUE ~ as.character(post_exposure_event_counts)))
  
  # write output for table2
  write.csv(analyses_of_interest, file=paste0("output/not-for-review/hospitalised_covid_event_counts_by_region_",cohort_name, "_",group, "_non_suppressed.csv"), row.names = F)
  
  analyses_of_interest$unexposed_event_counts <- round(analyses_of_interest$unexposed_event_counts, -1)
  analyses_of_interest$post_exposure_event_counts <- round(analyses_of_interest$post_exposure_event_counts, -1)
  
  analyses_of_interest$unexposed_event_counts <- ifelse(analyses_of_interest$unexposed_event_counts<10, "<10",analyses_of_interest$unexposed_event_counts)
  analyses_of_interest$post_exposure_event_counts <- ifelse(analyses_of_interest$post_exposure_event_counts<10, "<10",analyses_of_interest$post_exposure_event_counts)
  
  
  write.csv(analyses_of_interest, file=paste0("output/not-for-review/hospitalised_covid_event_counts_by_region_",cohort_name,"_",group, "_suppressed.csv"), row.names = F)
}

hosp_covid_event_counts <- function(survival_data, event, region){
  print(paste0("Working on ",event, " ",region))
  data_active <- survival_data
  
  data_active <- data_active %>% mutate(event_date = replace(event_date, which(event_date>follow_up_end | event_date<index_date), NA))
  data_active <- data_active %>% mutate(exp_date_covid19_confirmed = replace(exp_date_covid19_confirmed, which(exp_date_covid19_confirmed>follow_up_end | exp_date_covid19_confirmed<index_date), NA))
  
  # filter the population to remove those without history of covid
  data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==F)
  
  
  # filter the population according to the subgroup level
  data_active=data_active%>%filter_at("cov_cat_region",all_vars(.==region))
  
  data_active <- data_active %>% mutate(exp_date_covid19_confirmed = replace(exp_date_covid19_confirmed, which((!is.na(hospitalised_date_expo_censor)) & (exp_date_covid19_confirmed >= hospitalised_date_expo_censor)), NA))
  data_active <- data_active %>% mutate(event_date = replace(event_date, which((!is.na(hospitalised_date_expo_censor)) & (event_date >=hospitalised_date_expo_censor)), NA))
  
  data_active <- data_active %>% filter((index_date != hospitalised_date_expo_censor)|is.na(hospitalised_date_expo_censor))

  event_count_exposed <- length(which(data_active$event_date >= data_active$index_date &
                                        data_active$event_date >= data_active$exp_date_covid19_confirmed & 
                                        data_active$event_date <= data_active$hospitalised_follow_up_end))
    
  event_count_unexposed<- length(which((data_active$event_date >= data_active$index_date & 
                                          data_active$event_date <= data_active$hospitalised_follow_up_end) &
                                          (data_active$event_date < data_active$exp_date_covid19_confirmed | is.na(data_active$exp_date_covid19_confirmed))))
  

  return(c(event_count_unexposed, event_count_exposed))
}


# Run function using specified commandArgs and active analyses for group

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active==TRUE)
group <- unique(active_analyses$outcome_group)


for(i in group){
  if (cohort_name == "both") {
    hosp_covid_events("electively_unvaccinated", i)
    hosp_covid_events("vaccinated", i)
  } else{
    hosp_covid_events(cohort_name, i)
  }
}

# END