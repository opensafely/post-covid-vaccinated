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
# comment number of rows need to be revised in "data" and "output"
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
  #input <- input %>% mutate(sub_bin_sex = cov_cat_sex, sub_num_age = cov_num_age, 
  #                        sub_cat_ethnicity = cov_cat_ethnicity)
  # need to create age group
  input <- input %>% mutate(sub_bin_sex = cov_cat_sex,
                            sub_cat_ethnicity = cov_cat_ethnicity)
  vars_names <- tidyselect::vars_select(names(input), !starts_with(c('cov_','qa_','vax_cat'), ignore.case = TRUE))

  # variable sub_bin_ate, does this a variable indicate whether indivdiuals have a prior history of ate?
  # strata_names <- tidyselect::vars_select(names(active_analyses), !contains(c('active','outcome','outcome_variable','covariates','prior_history_var', 'model', 'cohort'), ignore.case = TRUE))
  sub_grp_names <- tidyselect::vars_select(names(input), starts_with(c('sub_'), ignore.case = TRUE))
  sub_grp_names <- sub_grp_names[which(sub_grp_names!="sub_bin_ate")]
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
  
  #define data frame for output table
  col_headings <- c("event", "cohort", "strata", "person_days")
  table_person_days <- data.frame(matrix(ncol=length(col_headings), nrow=length(event_dates_names)))
  colnames(table_person_days) <- col_headings
  table_person_days$event <- event_names
  
  person_days <- function(population, survival_data, event_dates_names, sub_grp_names, index)
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
    outcome_name <- gsub("out_date_", "", event_dates_names[index])
    strata <- NULL
    data <- data.frame(matrix(ncol=length(col_headings), nrow=12)) # define data frame for output table for each outcome
    colnames(data) <- col_headings
    index_data = 1
    for(i in 1:length(sub_grp_names)){
      strata[i] <- str_sub(sub_grp_names[i], 9) # remove the first nine characters
      current <- sub_grp_names[i]
      print(current)
      level_names <- names(table(survival_data[,current]))
      x <- tapply(survival_data$person_days_unexposed, survival_data[,current], FUN=sum)
     # print(x)
      strata_level <- NULL # initialization
      for(j in level_names){
        strata_level[j] <- paste0(strata[i], "_",j)
      }
      len = length(x)
      start = index_data
      end = index_data + len - 1
      # column 4 is person days
      data$person_days[start:end] <- as.vector(x)
      data$strata[start:end] <- strata_level
      data$event[start:end] <- outcome_name
      data$cohort[start:end] <- population
      index_data = end+1
    }
    print(data)
    return(data)
  }

  #output <- person_days(population, survival_data, event_dates_names, sub_grp_names, 1)
  output <- data.frame(matrix(ncol=length(col_headings), nrow=120)) 
  index_output = 1
  for(i in 1:length(event_dates_names)){
    start = index_output
    end = index_output + 11
    output[start:end, ] <- person_days(population, survival_data, event_dates_names, sub_grp_names, i) 
    index_output = end+1
  }
  names(output) <- col_headings
  write.csv(output, file= paste0("output/", "table2_suppl_", population, ".csv"), row.names = F)
