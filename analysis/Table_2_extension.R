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
active_analyses$prior_history_var = gsub("cov_", "sub_", active_analyses$prior_history_var)

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
unexposed_person_days <- unexposed_event_count <- unexposed_ir <- unexposed_ir_lower <- unexposed_ir_upper <- rep("NA", nrow(table_2_long))
exposed_person_days <- exposed_event_count <- exposed_ir <- exposed_ir_lower <- exposed_ir_upper <- rep("NA", nrow(table_2_long))

table_2_long <- cbind(table_2_long, unexposed_person_days, unexposed_event_count, unexposed_ir, unexposed_ir_lower, unexposed_ir_upper,
                                    exposed_person_days, exposed_event_count, exposed_ir, exposed_ir_lower, exposed_ir_upper)

# if(population == "vaccinated"){
#   table_2_long <- table_2_long %>% filter(cohort == "vaccinated")
# }
# if(population == "electively_unvaccinated"){
#   table_2_long <- table_2_long %>% filter(cohort == "electively_unvaccinated")
# }

table_2_long <- table_2_long %>% filter(cohort == population)

# read in data------------------------------------------------------------

input <- read_rds(paste0("output/input_",population,"_stage1.rds"))

# Define age groups
input$cov_cat_age_group <- ""
input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_110", input$cov_cat_age_group)

# rename variables to indicate them as subgroups
setnames(input,
         old = c("cov_cat_sex",
                 "cov_cat_age_group",
                 "cov_cat_ethnicity",
                 "cov_bin_vte"),
         new = c("sub_bin_sex",
                 "sub_cat_age_group",
                 "sub_cat_ethnicity",
                 "sub_bin_vte"))

levels(input$sub_cat_ethnicity) <- c("White", "Mixed", "South_Asian", "Black", "Other", "Missing")

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
# event="ami";cohort="vaccinated";strata="covid_history"; strata_level="TRUE"; sub_grp="sub_bin_covid19_confirmed_history"
#event="vte";cohort="vaccinated";strata="sub_bin_vte"; strata_level="FALSE"; sub_grp="sub_bin_vte"

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
        data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_exposed=min(event_date, death_date, cohort_end_date,na.rm = TRUE))
      }else if(cohort=="electively_unvaccinated"){
        data_active <- data_active %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
        data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_unexposed = min(vax_date_covid_1,event_date, exp_date_covid19_confirmed, death_date,cohort_end_date,na.rm = TRUE))
        data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_exposed = min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
        data_active <- data_active %>% dplyr::select(!c(vax_date_covid_1))
      }

      data_active <- data_active %>% filter(follow_up_end_unexposed >= index_date & follow_up_end_unexposed != Inf)
      data_active <- data_active %>% filter(follow_up_end_exposed >= index_date & follow_up_end_exposed != Inf)
      
      # select_names <- c("index_date","vax_date_covid_1","event_date", "exp_date_covid19_confirmed", "death_date","cohort_end_date", "follow_up_end_unexposed", "follow_up_end")
      # select_names <- c("index_date","event_date", "exp_date_covid19_confirmed", "death_date","cohort_end_date", "follow_up_end_unexposed", "follow_up_end")
      # 
      # data_select <- data_active[,select_names]
      # View(data_select)
      # 
      # 
      #  select_names <- c("index_date", "follow_up_end_unexposed", "follow_up_end")
      # 
      #  data_select <- data_active[,select_names]
      #  View(data_select)
      # calculate follow-up days
      data_active = data_active %>% mutate(person_days_unexposed = as.numeric((as.Date(follow_up_end_unexposed) - as.Date(index_date)))+1)
      #hist(data_active$person_days_unexposed)
      data_active = data_active %>% filter(person_days_unexposed >=1 & person_days_unexposed <= 197) # filter out follow up period
      person_days_total_unexposed  = round(sum(data_active$person_days_unexposed, na.rm = TRUE),1)
  
      data_active = data_active %>% mutate(person_days_exposed = as.numeric((as.Date(follow_up_end_exposed) - as.Date(index_date)))+1)
      data_active = data_active %>% filter(person_days_exposed >=1 & person_days_exposed <= 197) # filter out follow up period
      #hist(data_active$person_days)
       person_days_total_exposed  = round(sum(data_active$person_days_exposed, na.rm = TRUE),1)
 
      # post-exposure event
       event_count_exposed <- length(which(data_active$event_date >= data_active$index_date &
                                    data_active$event_date >= data_active$exp_date_covid19_confirmed & 
                                    data_active$event_date <= data_active$follow_up_end_exposed))

      # pre-exposure event count
        event_count_unexposed<- length(which((data_active$event_date >= data_active$index_date & 
                                    data_active$event_date <= data_active$follow_up_end_unexposed) &
                                   (data_active$event_date < data_active$exp_date_covid19_confirmed | is.na(data_active$exp_date_covid19_confirmed))
        ))
      # incidence rate post exposure
      person_years_total_exposed = person_days_total_exposed/365.2
      incidence_rate_exposed= round(event_count_exposed/person_years_total_exposed, 4)
      ir_lower_exposed = round(incidence_rate_exposed - 1.96 * sqrt(event_count_exposed/person_days_total_exposed^2),4)
      ir_upper_exposed = round(incidence_rate_exposed + 1.96 * sqrt(event_count_exposed/person_days_total_exposed^2),4)
      
      # incidence rate pre exposure
      person_years_total_unexposed = person_days_total_unexposed/365.2
      incidence_rate_unexposed= round(event_count_unexposed/person_years_total_unexposed, 4)
      ir_lower_unexposed = round(incidence_rate_unexposed - 1.96 * sqrt(event_count_unexposed/person_days_total_unexposed^2),4)
      ir_upper_unexposed = round(incidence_rate_unexposed + 1.96 * sqrt(event_count_unexposed/person_days_total_unexposed^2),4)
      
      return(c(person_days_total_unexposed, event_count_unexposed, incidence_rate_unexposed, ir_lower_unexposed, ir_upper_unexposed, person_days_total_exposed, event_count_exposed, incidence_rate_exposed, ir_lower_exposed, ir_upper_exposed))
}

col_names <- names(table_2_long)
start = grep("unexposed_person_days", col_names)
end = ncol(table_2_long)

for(i in 1:nrow(table_2_long)){
# for quick testing
#for(i in 357:358){
#for(i in 87:88){
#for(i in 1:2){
  d <- table_2_long
  print(i)
  if((d$strata[i]=="prior_history_FALSE" | d$strata[i]=="prior_history_TRUE") & (d$event_names[i]=="ate"|d$event_names[i]=="vte")){
    if(d$event_names[i]=="ate"){d$sub_grp[i] = "sub_bin_ate"}
    if(d$event_names[i]=="vte"){d$sub_grp[i] = "sub_bin_vte"}
    table_2_long[i,start:end] <- table_2_subgroup(survival_data, event=d$event_names[i],cohort=d$cohort[i],strata=d$strata[i], strata_level=d$strata_level[i], sub_grp=d$sub_grp[i])
  }
  if(d$strata[i]!="prior_history_FALSE" & d$strata[i]!="prior_history_TRUE"){
    table_2_long[i,start:end] <- table_2_subgroup(survival_data, event=d$event_names[i],cohort=d$cohort[i],strata=d$strata[i], strata_level=d$strata_level[i], sub_grp=d$sub_grp[i])
  }
}

write.csv(table_2_long, file=paste0("output/table_2_subgroups_", population, ".csv"))
