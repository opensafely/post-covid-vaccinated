## ====================================================================================
## Purpose:  Table 2 extension to all subgroups + main (all eligible)
## 
## Author:   Yinghui Wei
##
## Reviewer: Rochelle Knight
##
## Date:     29 March 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  Create input data for table 2
##
## Output:   rds file: input_population_event_stage1.rds
## ====================================================================================

library(readr); library(dplyr); library(data.table); library(lubridate)
library(stringr);library(tidyverse); library(htmlTable)

#library(vcdExtra)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  population <- "vaccinated"
  #population = "electively_unvaccinated"
}else{
  population <- args[[1]]
}

#delta period
cohort_start = as.Date("2021-06-01", format="%Y-%m-%d")
cohort_end = as.Date("2021-12-14", format="%Y-%m-%d")

input_table_2 <- function(population){
  # read in data----------------------------------------------------------------
  input <- read_rds(paste0("output/input_",population,"_stage1.rds"))
  active_analyses <- read_rds("lib/active_analyses.rds")
  variables_to_change = c("cov_cat_sex","cov_cat_age_group", "cov_cat_ethnicity")
  variables_to_change <- append(variables_to_change, active_analyses$prior_history_var[grep("cov_",active_analyses$prior_history_var)])

  # Define age groups
  input$cov_cat_age_group <- ""
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_110", input$cov_cat_age_group)
  
  # rename variables to indicate them as subgroups
  setnames(input,
           old = variables_to_change,
           new = gsub("cov_", "sub_", variables_to_change))
  
  setnames(input, old = "sub_cat_sex", new = "sub_bin_sex") # to match the name in active_analyses table

  levels(input$sub_cat_ethnicity) <- c("White", "Mixed", "South_Asian", "Black", "Other", "Missing")
 
  active_analyses <- active_analyses %>%dplyr::filter(active == "TRUE")
  event_dates_names <- active_analyses$outcome_variable
  outcome_names <- tidyselect::vars_select(names(input), starts_with(c("out_"), ignore.case=TRUE))
  outcome_names_not_active <- outcome_names[!outcome_names %in% event_dates_names]
  
  input$sub_main <- "main"
  
  sub_grp_names <- tidyselect::vars_select(names(input), starts_with(c('sub_'), ignore.case = TRUE))
  
  vars_names <- tidyselect::vars_select(names(input), !starts_with(c('cov_','qa_','vax_cat'), ignore.case = TRUE))
  vars_names <- vars_names[!vars_names %in% outcome_names_not_active]
  
  survival_data <- input[,vars_names]
  
  rm(list=c("input"))
  
  survival_data <- survival_data %>% mutate(cohort_start_date = cohort_start,cohort_end_date = cohort_end)
  
  event_dates_names <- active_analyses$outcome_variable
  outcome_names <- tidyselect::vars_select(names(survival_data), starts_with(c("out_"), ignore.case=TRUE))
  outcome_names_not_active <- outcome_names[!outcome_names %in% event_dates_names]
 
  for(event in event_dates_names){
    
    survival_data <- survival_data %>%rename(event_date = event)
  
    # specify the cohort according to vaccination status
    if(population=="vaccinated"){
      survival_data$follow_up_end_unexposed <- apply(survival_data[,c("event_date", "exp_date_covid19_confirmed", "death_date", "cohort_end_date")],1, min,na.rm=TRUE)
      survival_data$follow_up_end_exposed <- apply(survival_data[,c("event_date", "death_date", "cohort_end_date")],1, min, na.rm=TRUE)
      
      survival_data <- survival_data %>% mutate(follow_up_end_unexposed = ymd(follow_up_end_unexposed))
      survival_data <- survival_data %>% mutate(follow_up_end_exposed = ymd(follow_up_end_exposed))
      
      #survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end_unexposed=min(event_date, exp_date_covid19_confirmed, death_date, cohort_end_date,na.rm = TRUE))
      #survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end_exposed=min(event_date, death_date, cohort_end_date,na.rm = TRUE))
    }else if(population=="electively_unvaccinated"){
      #survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end_unexposed = min(vax_date_covid_1,event_date, exp_date_covid19_confirmed, death_date,cohort_end_date,na.rm = TRUE))
      #survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end_exposed = min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
      survival_data$follow_up_end_unexposed <- apply(survival_data[,c("vax_date_covid_1","event_date", "exp_date_covid19_confirmed", "death_date","cohort_end_date")],1, min,na.rm=TRUE)
      survival_data$follow_up_end_exposed <- apply(survival_data[,c("vax_date_covid_1","event_date", "death_date","cohort_end_date")],1, min, na.rm=TRUE)
      
      survival_data <- survival_data %>% mutate(follow_up_end_unexposed = ymd(follow_up_end_unexposed))
      survival_data <- survival_data %>% mutate(follow_up_end_exposed = ymd(follow_up_end_exposed))
    }
      
    
    survival_data <- survival_data %>% filter(follow_up_end_unexposed >= index_date & follow_up_end_unexposed != Inf)
    survival_data <- survival_data %>% filter(follow_up_end_exposed >= index_date & follow_up_end_exposed != Inf)
    
    # calculate follow-up days
    survival_data = survival_data %>% mutate(person_days_unexposed = as.numeric((as.Date(follow_up_end_unexposed) - as.Date(index_date))))
    index <- which(survival_data$follow_up_end_unexposed > survival_data$exp_date_covid19_confirmed | is.na(survival_data$exp_date_covid19_confirmed))
    survival_data$person_days_unexposed[index] = survival_data$person_days_unexposed[index] + 1
    #hist(data_active$person_days_unexposed)
    #survival_data = survival_data %>% filter(person_days_unexposed >=0 & person_days_unexposed <= 197) # filter out follow up period
    
    survival_data = survival_data %>% mutate(person_days_exposed = as.numeric((as.Date(follow_up_end_exposed) - as.Date(index_date)))+1)
    #survival_data = survival_data %>% filter(person_days_exposed >=0 & person_days_exposed <= 197) # filter out follow up period
    
    survival_data$include <- ifelse((survival_data$follow_up_end_unexposed >= survival_data$index_date & survival_data$follow_up_end_unexposed != Inf) & 
                                      (survival_data$follow_up_end_exposed >= survival_data$index_date & survival_data$follow_up_end_exposed != Inf) &
                                      (survival_data$person_days_unexposed >=0 & survival_data$person_days_unexposed <= 197) &
                                      (survival_data$person_days_exposed >=0 & survival_data$person_days_exposed <= 197),1,0)
    
    event_short <- gsub("out_date_","", event)
    
    setnames(survival_data,
             old = c("event_date",
                     "follow_up_end_unexposed",
                     "follow_up_end_exposed",
                     "person_days_unexposed",
                     "person_days_exposed",
                     "include"),
             new = c(paste0("out_date_",event_short),
                     paste0(event_short,"_follow_up_end_unexposed"),
                     paste0(event_short,"_follow_up_end_exposed"),
                     paste0(event_short,"_person_days_unexposed"),
                     paste0(event_short,"_person_days_exposed"),
                     paste0(event_short,"_include")))
                     
    
    #saveRDS(data_active, file=paste0("output/input_table_2_",population,"_", event_short,"_stage1.rds"))
    print(paste0("input for ", event, " in ", population, " population has been produced successfully!"))
  }
  saveRDS(survival_data, file=paste0("output/input_table_2_",population,"_stage1.rds"))
  #write_rds(survival_data, file=paste0("output/input_table_2_",population,"_stage1.rds"))
  #write_csv(survival_data, file=paste0("output/input_table_2_",population,"_stage1.rds"))
  rm(list=c("survival_data"))
  
  
}
# Run function using specified commandArgs
if(population == "both"){
  input_table_2("vaccinated")
  input_table_2("electively_unvaccinated")
}else{
  input_table_2(population)
}