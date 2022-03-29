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
##
## Output:   Four CSV files for table 2:
#            vaccinated and electively unvaccinated populations; 
##           without COVID history (main text); with COVID history (suppl materials)
## =============================================================================

library(readr); library(dplyr); library(data.table); library(lubridate)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  population <- "vaccinated"
  #population = "electively_unvaccinated"
}else{
  population <- args[[1]]
}

cohort_start = as.Date("2021-06-01", format="%Y-%m-%d")
cohort_end = as.Date("2021-12-14", format="%Y-%m-%d")

# indicate active analyses -----------------------------------------------

active_analyses <- read_rds("lib/active_analyses.rds")

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
  #event_names
  
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
    survival_data = survival_data %>% filter(follow_up_period >=1 & follow_up_period <= 197) # filter out follow up period 
    survival_data = survival_data %>% mutate(follow_up_years = follow_up_period / 365.2) # follow-up years
    if(infection_subgroup == "no_infection"){
      event_count <- length(which((survival_data$event_date >= survival_data$index_date & survival_data$event_date <= survival_data$follow_up_end) &
                                   (survival_data$event_date < survival_data$exp_date_covid19_confirmed | is.na(survival_data$exp_date_covid19_confirmed))
                                 ))
     }else{
      event_count <- length(which(survival_data$event_date   >= survival_data$index_date &
                                    survival_data$event_date >= survival_data$exp_date_covid19_confirmed & 
                                    survival_data$event_date <= survival_data$follow_up_end))
     }
    person_years_follow_up  = round(sum(survival_data$follow_up_years, na.rm = TRUE),1)
    incidence_rate = round(event_count/person_years_follow_up, 4)
    incidence_rate_lower = incidence_rate - 1.96 * sqrt(event_count/person_years_follow_up^2)
    incidence_rate_upper = incidence_rate + 1.96 * sqrt(event_count/person_years_follow_up^2)
    return(c(event_count,person_years_follow_up, incidence_rate, incidence_rate_lower, incidence_rate_upper))
    }
  
  for(i in 1:length(event_dates_names)){
    table_2[i,2:6] <- summary_stats(population, "no_infection", survival_data, event_dates_names, i)
    table_2[i,7:11] <- summary_stats(population, "non_hospitalised",survival_data[survival_data$sub_cat_covid19_hospital=="non_hospitalised",], event_dates_names, i)
    table_2[i,12:16] <- summary_stats(population, "hospitalised", survival_data[survival_data$sub_cat_covid19_hospital=="hospitalised",], event_dates_names, i)
    table_2$total_event_count <- table_2[,2] + table_2[,7] + table_2[,12]
    table_2$total_person_yrs <-  table_2[,3] + table_2[,8] + table_2[,13]
    table_2$overall_incidence_rate <- round(table_2$total_event_count/table_2$total_person_yrs,4)
    table_2$overall_incidence_rate_lower <- table_2$overall_incidence_rate - 1.96*sqrt(table_2$total_event_count/table_2$total_person_yrs^2)
    table_2$overall_incidence_rate_upper <- table_2$overall_incidence_rate + 1.96*sqrt(table_2$total_event_count/table_2$total_person_yrs^2)
    names(table_2)[2:6] <- c("no_infection_sub_event_count", "no_infection_sub_person_yrs_fp", "no_infection_sub_incidence_rate", "no_infection_sub_incidence_rate_lower", "no_infection_sub_incidence_rate_upper")
    names(table_2)[7:11] <- c("non_hospitalised_sub_event_count", "non_hospitalised_sub_person_yrs_fp", "non_hospitalised_sub_incidence_rate", "non_hospitalised_sub_incidence_rate_lower","non_hospitalised_sub_incidence_rate_upper")
    names(table_2)[12:16] <- c("hospitalised_sub_event_count", "hospitalised_sub_person_yrs_fp", "hospitalised_sub_incidence_rate", "hospitalised_sub_incidence_rate_lower", "hospitalised_sub_incidence_rate_upper")
    names(table_2)[17:21] <- c("total_event_count", "total_person_yrs", "overall_incidence_rate", "overall_incidence_rate_lower", "overall_incidence_rate_upper")
  }
  # low number check and suppression to "NA" if event count lower than or equal to 5
  table_2[which(table_2$no_infection_sub_event_count <= 5), c(2,4,5,6)] = c("<=5", "NA", "NA", "NA")
  table_2[which(table_2$non_hospitalised_sub_event_count <= 5),c(7,9,10,11)] = c("<=5", "NA", "NA", "NA")
  table_2[which(table_2$hospitalised_sub_event_count <= 5),c(12,14,15,16)] = c("<=5", "NA", "NA", "NA")
  table_2[which(table_2$total_event_count <= 5),c(17,19,20,21)] = c("<=5", "NA", "NA", "NA")
  table_2[which(table_2$no_infection_sub_event_count == "<=5" | table_2$non_hospitalised_sub_event_count == "<=5" | table_2$hospitalised_sub_event_count == "<=5" ),c(17,19,20,21)] = c("<=5", "NA","NA","NA")
  write.csv(table_2, file= paste0("output/", "table2_", population, "_",covid_history, ".csv"), row.names = F)
}

# Run function using specified commandArgs

if(population == "both"){
  table_2_output("vaccinated", "with_covid_history")
  table_2_output("vaccinated", "without_covid_history")
  table_2_output("electively_unvaccinated", "with_covid_history")
  table_2_output("electively_unvaccinated", "without_covid_history")
}else{
  #table_2_output(population, "with_covid_history")
  table_2_output(population, "without_covid_history")
}


