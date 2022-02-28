## =============================================================================
## Purpose:  Calculate person days as input for absolute access risk
## 
## Author:   Yinghui Wei
##
## Date:     28 Feb 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  person days of follow up, for each outcome by subgroup
##
## Output:   One CSV file
## =============================================================================

# adapted from table 2
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

active_analyses <- read_rds("output/active_analyses.rds")

  # read in data------------------------------------------------------------
  
  input <- read_rds(paste0("output/input_",population,"_stage1.rds"))
  input <- filter(input, sub_bin_covid19_confirmed_history==T) # they should all have covid history?
  # if(covid_history == "without_covid_history"){
  #   input <- filter(input, sub_bin_covid19_confirmed_history==F)
  # }
  # if(covid_history == "with_covid_history"){
  #   input <- filter(input, sub_bin_covid19_confirmed_history==T)
  # }
  # record variable names for covariate
  input <- input %>% mutate(sub_bin_sex = cov_cat_sex, sub_num_age = cov_num_age, 
                          sub_cov_ethnicity = cov_cat_ethnicity)
  vars_names <- tidyselect::vars_select(names(input), !starts_with(c('cov_','qa_','vax_cat'), ignore.case = TRUE))

  # variable sub_bin_ate, does this a variable indicate whether indivdiuals have a prior history of ate?
  strata_names <- tidyselect::vars_select(names(active_analyses), !contains(c('active','outcome','outcome_variable','covariates','prior_history_var', 'model', 'cohort'), ignore.case = TRUE))

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
  strata <- "covid_history"
  col_headings <- c("event", "cohort", "strata", "person_days")
  table_person_days <- data.frame(matrix(ncol=length(col_headings), nrow=length(event_dates_names)))
  colnames(table_person_days) <- col_headings
  table_person_days$event <- event_names

  person_days <- function(population, survival_data, event_dates_names, index)
  {
    survival_data$event_date <- survival_data[,event_dates_names[index]]
    if(population=="vaccinated"){
      survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end_unexposed=min(event_date, exp_date_covid19_confirmed, death_date, cohort_end_date,na.rm = TRUE))
    }else if(population=="electively_unvaccinated"){
      survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
      survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end_unexposed = min(vax_date_covid_1,event_date, exp_date_covid19_confirmed, death_date,cohort_end_date,na.rm = TRUE))
      survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
    }
    
    # follow-up days
    survival_data = survival_data %>% mutate(person_days_unexposed = as.numeric((as.Date(follow_up_end_unexposed) - as.Date(index_date)))+1) 
    survival_data = survival_data %>% filter(person_days_unexposed >=1 & person_days_unexposed <= 197) # filter out follow up period 
    person_days_unexposed_total  = round(sum(survival_data$person_days_unexposed, na.rm = TRUE),1)
    x <- survival_data %>% group_by(sub_bin_covid19_confirmed_history) %>% summarise(person_days_unexposed_total = sum(person_days_unexposed))
    len <- length(x[,2])
    print(len)
    outcome_name <- gsub("out_date_", "", event_dates_names[index])
    data <- cbind(rep(outcome_name,len), rep(population,len), rep(strata,len), x[,2])
    #names(data) <- col_headings
    survival_data %>% group_by(sub_bin_sex) %>% summarise(person_days_unexposed_total = sum(person_days_unexposed))
    print(data)
    return(data)
  }

  table_person_days <-person_days(population,survival_data,event_dates_names, 1) 
