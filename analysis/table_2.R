## =============================================================================
## Purpose:  Create Table 2
## 
## Author:   Yinghui Wei
##
## Reviewer: Rochelle Knight
## 
## Date:     13 January 2022; updated 24 Feb 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  Number of outcome events;
##           person years of follow up and rates of events, for each outcome
## =============================================================================

library(readr); library(dplyr); library(data.table); library(lubridate)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  population <- "vaccinated"
  #population = "electively_unvaccinated"
} else {
  population <- args[[1]]
}

cohort_start = as.Date("2021-06-01", format="%Y-%m-%d")
cohort_end = as.Date("2021-12-14", format="%Y-%m-%d")

# indicate active analyses -----------------------------------------------

active_analyses <- read_rds("output/active_analyses.rds")

table_2_output <- function(population, covid_history){
  # read in data------------------------------------------------------------
  
  input <- read_rds(paste0("output/input_",population,"_stage1.rds"))
  if(covid_history == "without_covid_history"){
    input <- filter(input, sub_bin_covid19_confirmed_history==F)
  }
  if(covid_history == "with_covid_history"){
    input <- filter(input, sub_bin_covid19_confirmed_history==T)
  }
  # record variable names for covariate
  vars_names <- tidyselect::vars_select(names(input), !starts_with(c('sub_','cov_','qa_','vax_cat'), ignore.case = TRUE))
  vars_names <- c(vars_names, "sub_cat_covid19_hospital", "exp_date_covid19_confirmed")
  
  # Create a data frame for survival data: to avoid carrying covariates in the calculation
  survival_data <- input[,vars_names] 
  
  # cohort start date and end date
  survival_data <- survival_data %>% 
    mutate(cohort_start_date = cohort_start,
           cohort_end_date = cohort_end)
  
  # automation
  event_dates_names <- active_analyses$outcome_variable[which(active_analyses$active==T)]
  
  event_names<- event_names <- gsub("out_date_","",event_dates_names)
  event_names
  
  col_headings <- c("event", "event_count", "person_years_follow_up", "incidence_rate")
  table_2 <- data.frame(matrix(ncol=length(col_headings), nrow=length(event_dates_names)))
  colnames(table_2) <- col_headings
  table_2$event <- event_names
  
  summary_stats <- function(population, infection_subgroup, survival_data, event_dates_names, index)
  {
    survival_data$event_date <- survival_data[,event_dates_names[index]]
    if(population=="vaccinated"){
      survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(event_date, death_date, cohort_end_date,na.rm = TRUE))
    }else if(population=="electively_unvaccinated"){
      survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
      survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=  min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
      survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
    }

    # follow-up days
    survival_data = survival_data %>% mutate(follow_up_period = as.numeric((as.Date(follow_up_end) - as.Date(index_date)))+1) 
    #hist(survival_data$follow_up_period)  ## check if there are any negative follow-up periods
    survival_data = survival_data %>% filter(follow_up_period >=0 & follow_up_period < 197) # filter out follow up period 
    survival_data = survival_data %>% mutate(follow_up_years = follow_up_period / 365.2) # follow-up years
    # event count for subgroup of individuals who have been infected within the follow-up period
    event_count_post_infection <- length(which(survival_data$event_date   >= survival_data$index_date &
                                                 survival_data$event_date >= survival_data$exp_date_covid19_confirmed & 
                                                 survival_data$event_date <= survival_data$follow_up_end))
    # event count for subgroup of individuals who have not been infected within the follow-up period
    event_count_before_infection <- length(which(survival_data$event_date >= survival_data$index_date &
                                                 survival_data$event_date <  survival_data$exp_date_covid19_confirmed & 
                                                 survival_data$event_date <= survival_data$follow_up_end))
    # event count for subgroup of individuals who have been infected within the follow-up period but with event occurred before infection
    event_count_no_infection <- length(which(survival_data$event_date   >= survival_data$index_date & 
                                             is.na(survival_data$exp_date_covid19_confirmed) == T &
                                             survival_data$event_date <= survival_data$follow_up_end))
    event_count_no_infection = event_count_no_infection + event_count_before_infection
    person_years_follow_up  = round(sum(survival_data$follow_up_years, na.rm = TRUE),1)
    event_count = ifelse(infection_subgroup == "no_infection", event_count_no_infection, event_count_post_infection)
    incidence_rate = round(event_count/person_years_follow_up, 4)
    return(c(event_count,person_years_follow_up, incidence_rate))
  }
  
  for(i in 1:length(event_dates_names)){
   # table_2[i,2:4] <- summary_stats(population, survival_data, event_dates_names, i)
    table_2[i,2:4] <- summary_stats(population, "no_infection", survival_data[survival_data$sub_cat_covid19_hospital=="no_infection",], event_dates_names, i)
    table_2[i,5:7] <- summary_stats(population, "non_hospitalised",survival_data[survival_data$sub_cat_covid19_hospital=="non_hospitalised",], event_dates_names, i)
    table_2[i,8:10] <- summary_stats(population, "hospitalised", survival_data[survival_data$sub_cat_covid19_hospital=="hospitalised",], event_dates_names, i)
    table_2$total_event_count <- table_2[,2] + table_2[,5] + table_2[,8]
    table_2$total_person_yrs <-  table_2[,3] + table_2[,6] + table_2[,9]
    table_2$overall_incidence_rate <- round(table_2$total_event_count/table_2$total_person_yrs,2)
    names(table_2)[2:4] <- c("no_infection_sub_event_count", "no_infection_sub_person_yrs_fp", "no_infection_sub_incidence_rate")
    names(table_2)[5:7] <- c("non_hospitalised_sub_event_count", "non_hospitalised_sub_person_yrs_fp", "non_hospitalised_sub_incidence_rate")
    names(table_2)[8:10] <- c("hospitalised_sub_event_count", "hospitalised_sub_person_yrs_fp", "hospitalised_sub_incidence_rate")
    names(table_2)[11:13] <- c("total_event_count", "total_person_yrs", "overall_incidence_rate")
  }
  table_2[which(table_2$no_infection_sub_event_count <5), c(2,4)] = c("<5", "NA")
  table_2[which(table_2$non_hospitalised_sub_event_count <5),c(5,7)] = c("<5", "NA")
  table_2[which(table_2$hospitalised_sub_event_count <5),c(8,10)] = c("<5", "NA")
  table_2[which(table_2$total_event_count <5),c(11,13)] = c("<5", "NA")
  write.csv(table_2, file= paste0("output/", "table2_", population, "_",covid_history, ".csv"), row.names = F)
}

# Run function using specified commandArgs

if(population == "both"){
  table_2_output("vaccinated", "with_covid_history")
  table_2_output("vaccinated", "without_covid_history")
  table_2_output("electively_unvaccinated", "with_covid_history")
  table_2_output("electively_unvaccinated", "without_covid_history")
}else{
  table_2_output(population, "with_covid_history")
  table_2_output(population, "without_covid_history")
}


