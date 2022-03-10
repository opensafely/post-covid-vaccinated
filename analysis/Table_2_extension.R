## ====================================================================================
## Purpose:  Table 2 extension to all subgroups + main (all eligible)
## 
## Author:   Yinghui Wei
##
## Date:     28 Feb 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  person days of follow up, unexposed person days, incidence rate, and 95% CI
##
## Output:   One CSV file
## ====================================================================================

# comment number of rows need to be revised in "data" and "output"
library(readr); library(dplyr); library(data.table); library(lubridate)
library(stringr);library(vcdExtra);library(tidyverse)

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

# indicate active analyses -----------------------------------------------
active_analyses <- read_rds("output/active_analyses.rds")
active_analyses <- active_analyses[which(active_analyses$active==T), ]
# automation
event_dates_names <- active_analyses$outcome_variable
event_names<- event_names <- gsub("out_date_","",event_dates_names)
event_names
col_names <- names(active_analyses)
col_names <- col_names[!col_names %in% c("active","outcome","outcome_variable","covariates")]

strata <-col_names

strata <- strata[!strata %in% c("model", "cohort", "prior_history_var")]

mdl <- NULL
for(i in 1:nrow(active_analyses)){
  if(active_analyses$model[i]=="all"){
    mdl=c(mdl, "mdl_agesex","mdl_max_adj")
  }else{
    mdl=c(mdl, active_analyses$model)
  }
}
mdl

model <-mdl

cohort_to_run <- NULL
for(i in 1:nrow(active_analyses)){
  if(active_analyses$cohort[i]=="all"){
    cohort_to_run=c(cohort_to_run,"vaccinated", "electively_unvaccinated")
  }else{
    cohort_to_run=c(cohort_to_run,active_analyses$cohort)
  }  
}
cohort <- cohort_to_run

table_2_long <- crossing(event_names, model, cohort, strata)

# specify strata level
table_2_long$strata_level <- table_2_long$strata
table_2_long$strata_level <- gsub("agegp_","",table_2_long$strata_level)
table_2_long$strata_level <- gsub("covid_history","TRUE",table_2_long$strata_level)
table_2_long$strata_level <- gsub("covid_pheno_", "", table_2_long$strata_level)
table_2_long$strata_level <- gsub("ethnicity_", "", table_2_long$strata_level)
table_2_long$strata_level <- gsub("prior_history_", "", table_2_long$strata_level)
table_2_long$strata_level <- gsub("sex_", "", table_2_long$strata_level)

# specify subgroup names

table_2_long$sub_grp <- table_2_long$strata
table_2_long <- table_2_long[,c("event_names","model","cohort", "strata", "strata_level", "sub_grp")]

index <- grepl("agegp", table_2_long$strata, fixed = TRUE)
table_2_long$sub_grp[index] <- "sub_cat_age_group"


index <- grepl("covid_history", table_2_long$strata, fixed = TRUE)
table_2_long$sub_grp[index] <- "sub_bin_covid19_confirmed_history"

index <- grepl("covid_pheno_", table_2_long$strata, fixed = TRUE)
table_2_long$sub_grp[index] <- "sub_cat_covid19_hospital"

index <- grepl("ethnicity", table_2_long$strata, fixed = TRUE)
table_2_long$sub_grp[index] <- "sub_cat_ethnicity"

index <- grepl("sex", table_2_long$strata, fixed = TRUE)
table_2_long$sub_grp[index] <- "sub_bin_sex"

index <- grepl("main", table_2_long$strata, fixed = TRUE)
table_2_long$sub_grp[index] <- "sub_main"

index <- grepl("ate", table_2_long$strata, fixed = TRUE)
table_2_long$sub_grp[index] <- "sub_bin_ate"

View(table_2_long)

#ir = incidence rate; ir_lower = lower bound of the 95% CI for ir; ir_upper = upper bound of the 95% CI for ir
person_days <- unexposed_person_days <- event_count <- ir <- ir_lower <- ir_upper <- rep("NA", nrow(table_2_long))

table_2_long <- cbind(table_2_long, unexposed_person_days, person_days, event_count, ir, ir_lower, ir_upper)

# read in data------------------------------------------------------------

input <- read_rds(paste0("output/input_",population,"_stage1.rds"))

# Define age groups
input$cov_cat_age_group <- ""
input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18-39", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40-59", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60-79", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_110", input$cov_cat_age_group)

# rename variables to indicate them as subgroups
setnames(input,
         old = c("cov_cat_sex",
                 "cov_cat_age_group",
                 "cov_cat_ethnicity"),
         new = c("sub_bin_sex",
                 "sub_cat_age_group",
                 "sub_cat_ethnicity"))

outcome_names <- tidyselect::vars_select(names(input), starts_with(c("out_"), ignore.case=TRUE))
outcome_names_not_active <- outcome_names[!outcome_names %in% event_dates_names]

sub_main = rep("main", nrow(input))
input <- input %>% mutate(sub_main = sub_main)

sub_grp_names <- tidyselect::vars_select(names(input), starts_with(c('sub_'), ignore.case = TRUE))

vars_names <- tidyselect::vars_select(names(input), !starts_with(c('cov_','qa_','vax_cat'), ignore.case = TRUE))
vars_names <- vars_names[!vars_names %in% outcome_names_not_active]
# cohort start date and end date

survival_data <- input[,vars_names]

survival_data <- survival_data %>% mutate(cohort_start_date = cohort_start,cohort_end_date = cohort_end)

# rewrite the function
# testing
#event="ami";cohort="vaccinated";strata="covid_history"; strata_level="TRUE"; sub_grp="sub_bin_covid19_confirmed_history"

table_2_subgroup <- function(survival_data, event,cohort,strata, strata_level, sub_grp){
      #survival_data$event_date <- survival_data[,paste0("out_date_","ami")]
     # specify event date for the event of interest, e.g. ami
      data_active <- survival_data
      data_active$event_date <- survival_data[,paste0("out_date_",event)]
      # filter the population according to whether the subgroup is covid_history
      if(strata == "covid_history"){
               data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==T)
      }else{
              data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==F)
      }
      # filter the population according to the strata level
      data_active$active_sub_grp <- data_active[,sub_grp]
      data_active <- data_active%>%filter(active_sub_grp==strata_level)
     
      # specify the cohort according to vaccination status
      if(cohort=="vaccinated"){
        data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_unexposed=min(event_date, exp_date_covid19_confirmed, death_date, cohort_end_date,na.rm = TRUE))
        data_active <- data_active %>% rowwise() %>% mutate(follow_up_end=min(event_date, death_date, cohort_end_date,na.rm = TRUE))
      }else if(cohort=="electively_unvaccinated"){
        data_active <- data_active %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
        data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_unexposed = min(vax_date_covid_1,event_date, exp_date_covid19_confirmed, death_date,cohort_end_date,na.rm = TRUE))
        data_active <- data_active %>% rowwise() %>% mutate(follow_up_end = min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
        data_active <- data_active %>% dplyr::select(!c(vax_date_covid_1))
      }
      # calculate follow-up days
      data_active = data_active %>% mutate(person_days_unexposed = as.numeric((as.Date(follow_up_end_unexposed) - as.Date(index_date)))+1)
      data_active = data_active %>% filter(person_days_unexposed >=1 & person_days_unexposed <= 197) # filter out follow up period
      person_days_unexposed_total  = round(sum(data_active$person_days_unexposed, na.rm = TRUE),1)
      data_active = data_active %>% mutate(person_days = as.numeric((as.Date(follow_up_end) - as.Date(index_date)))+1)
      data_active = data_active %>% filter(person_days >=1 & person_days <= 197) # filter out follow up period
      person_days_total  = round(sum(data_active$person_days, na.rm = TRUE),1)
      # calculate the number of event 
      if(strata == "covid_history"){
        event_count <- length(which(data_active$event_date >= data_active$index_date &
                                    data_active$event_date >= data_active$exp_date_covid19_confirmed & 
                                    data_active$event_date <= data_active$follow_up_end))
      }else{
        event_count<- length(which((data_active$event_date >= data_active$index_date & 
                                    data_active$event_date <= data_active$follow_up_end) &
                                   (data_active$event_date < data_active$exp_date_covid19_confirmed | is.na(survival_data$exp_date_covid19_confirmed))
        ))
      }
      
      person_years_total = person_days_total/365.2
      person_years_unexposed_total = person_days_unexposed_total/365.2
      incidence_rate = round(event_count/person_years_total, 4)
      incidence_rate_lower = incidence_rate - 1.96 * sqrt(event_count/person_days_total^2)
      incidence_rate_upper = incidence_rate + 1.96 * sqrt(event_count/person_days_total^2)
      return(c(person_days_unexposed_total, person_days_total, event_count, incidence_rate, incidence_rate_lower, incidence_rate_upper))
}

col_names <- names(table_2_long)
#grep("unexposed_person_days", col_names)
#ncol(table_2_long)
for(i in 1:nrow(table_2_long)){
  d <- table_2_long
  print(i)
  if(d$strata[i]!="prior_history_FALSE" & d$strata[i]!="prior_history_TRUE"){
    table_2_long[,grep("unexposed_person_days", col_names): ncol(table_2_long) ] <- table_2_subgroup(survival_data, event=d$event_names[i],cohort=d$cohort[i],strata=d$strata[i], strata_level=d$strata_level[i], sub_grp=d$sub_grp[i])
  }
}

#table_2_subgroup(survival_data, event="ami",cohort="vaccinated",strata="covid_history", strata_level="TRUE", sub_grp="sub_bin_covid19_confirmed_history")
  
# total_levels = 0
# for(i in sub_grp_names){
#   survival_data[,i] <- as.factor(survival_data[,i])
#   print(i)
#   total_levels = total_levels + nlevels(survival_data[,i])
# }



# #define data frame for output table
# col_headings <- c("event", "cohort", "strata", "unexposed_person_days", "person_days", "event_count", "ir", "ir_lower", "ir_upper")
# table_person_days <- data.frame(matrix(ncol=length(col_headings), nrow=length(event_dates_names)))
# colnames(table_person_days) <- col_headings
# table_person_days$event <- event_names

# person_days <- function(population, survival_data, event_dates_names, sub_grp_names, index)
# {
#   survival_data$event_date <- survival_data[,event_dates_names[index]]
#   outcome_name <- gsub("out_date_", "", event_dates_names[index])
#   strata <- NULL
#   data <- data.frame(matrix(ncol=length(col_headings), nrow=total_levels)) # define data frame for output table for each outcome
#   colnames(data) <- col_headings
#   index_data = 1
#   for(i in 1:length(sub_grp_names)){
#     current <- strata[i] <- sub_grp_names[i]
#     print(c(outcome_name,current))
#     if(sub_grp_names[i] == "sub_bin_covid19_confirmed_history"){
#        survival_data_subgrp <- survival_data %>% filter(sub_bin_covid19_confirmed_history ==T)
#      }else{
#       survival_data_subgrp <- survival_data %>% filter(sub_bin_covid19_confirmed_history ==F)
#      }
#     if(population=="vaccinated"){
#       survival_data_subgrp <- survival_data_subgrp %>% rowwise() %>% mutate(follow_up_end_unexposed=min(event_date, exp_date_covid19_confirmed, death_date, cohort_end_date,na.rm = TRUE))
#       survival_data_subgrp <- survival_data_subgrp %>% rowwise() %>% mutate(follow_up_end=min(event_date, death_date, cohort_end_date,na.rm = TRUE))
#     }else if(population=="electively_unvaccinated"){
#       survival_data_subgrp <- survival_data_subgrp %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
#       survival_data_subgrp <- survival_data_subgrp %>% rowwise() %>% mutate(follow_up_end_unexposed = min(vax_date_covid_1,event_date, exp_date_covid19_confirmed, death_date,cohort_end_date,na.rm = TRUE))
#       survival_data_subgrp <- survival_data_subgrp %>% rowwise() %>% mutate(follow_up_end = min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
#       survival_data_subgrp <- survival_data_subgrp %>% dplyr::select(!c(vax_date_covid_1))
#     }
#     # follow-up days
#     survival_data_subgrp = survival_data_subgrp %>% mutate(person_days_unexposed = as.numeric((as.Date(follow_up_end_unexposed) - as.Date(index_date)))+1)
#     survival_data_subgrp = survival_data_subgrp %>% filter(person_days_unexposed >=1 & person_days_unexposed <= 197) # filter out follow up period
#     person_days_unexposed_total  = round(sum(survival_data_subgrp$person_days_unexposed, na.rm = TRUE),1)
#     survival_data_subgrp = survival_data_subgrp %>% mutate(person_days = as.numeric((as.Date(follow_up_end) - as.Date(index_date)))+1)
#     survival_data_subgrp = survival_data_subgrp %>% filter(person_days >=1 & person_days <= 197) # filter out follow up period
#     person_days_total  = round(sum(survival_data_subgrp$person_days, na.rm = TRUE),1)
#     level_names <- names(table(survival_data_subgrp[,current]))
#     x <- tapply(survival_data_subgrp$person_days_unexposed, survival_data_subgrp[,current], FUN=sum)
#     # print(x)
#     y <- tapply(survival_data_subgrp$person_days, survival_data_subgrp[,current], FUN=sum)
#     # print(y)
#     strata_level <- NULL # initialization
#     for(j in level_names){
#       strata_level[j] <- paste0(strata[i], "_",j)
#     }
#     len = length(x)
#     start = index_data
#     end = index_data + len - 1
#     # column 4 is person days
#     data$unexposed_person_days[start:end] <- as.vector(x) # by strata level
#     data$person_days[start:end] <- as.vector(y) # by strata level
#     data$strata[start:end] <- strata_level
#     data$event[start:end] <- outcome_name
#     data$cohort[start:end] <- population
#     index_data = end+1
#   }
#   print(data)
#   return(data)
# }
# 
# output <- data.frame(matrix(ncol=length(col_headings), nrow=10*total_levels))
# index_output = 1
# for(i in 1:length(event_dates_names)){
#   start = index_output
#   end = index_output + total_levels-1
#   output[start:end, ] <- person_days(population, survival_data, event_dates_names, sub_grp_names, i)
#   index_output = end+1
# }
# names(output) <- col_headings
# output$strata <- gsub('sub_bin_','', output$strata)
# output$strata <- gsub('sub_cat_','', output$strata)
# output$strata <- gsub('sub_main_main','', output$strata)
# output$ir <- output$event_count/output$person_days
# output$ir_lower <- round(output$ir - 1.96 * sqrt(output$event_count/output$person_days^2),4)
# output$ir_upper <- round(output$ir + 1.96 * sqrt(output$event_count/output$person_days^2),4)
# write.csv(output, file= paste0("output/", "table2_suppl_", population, ".csv"), row.names = F)
