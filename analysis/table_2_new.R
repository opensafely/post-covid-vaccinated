## ====================================================================================
## Purpose:  Table 2 extension to all subgroups + main (all eligible)
## 
## Author:   Yinghui Wei
##
## Reviewer: Rochelle Knight
##
## Date:     28 Feb 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  person days of follow up, unexposed person days, incidence rate, and 95% CI
##
## Output:   CSV files: table2_*.csv, input1_aer_*.csv
## ====================================================================================

library(readr); library(dplyr); library(data.table); library(lubridate)
library(stringr);library(tidyverse)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  population <- "vaccinated"
  #population = "electively_unvaccinated"
  #analyses <- "main"
  analyses <- "subgroups"
}else{
  analyses <- args[[1]]
  population <- args[[2]]
}

#start.time = Sys.time()
#delta period
cohort_start = as.Date("2021-06-01", format="%Y-%m-%d")
cohort_end = as.Date("2021-12-14", format="%Y-%m-%d")

table_2_calculation <- function(survival_data, event,cohort,subgrp, subgrp_level, subgrp_full_name){
  data_active <- survival_data
  data_active <- data_active %>% filter(include == 1)
  
  # filter the population according to whether the subgroup is covid_history
  if(subgrp == "covid_history"){
    data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==T)
  }else{
    data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==F)
  }
  
  # filter the population according to the subgrp level
  setnames(data_active,
           old = paste0(subgrp_full_name),
           new = "active_subgrp_full_name")
  
  
  #data_active$active_subgrp_full_name <- data_active[,subgrp_full_name,by=id]
  data_active <- data_active%>%filter(active_subgrp_full_name==subgrp_level)
  
  setnames(data_active,
           old = "active_subgrp_full_name",
           new = paste0(subgrp_full_name),)

  person_days_total_unexposed  = round(sum(data_active$person_days_unexposed, na.rm = TRUE),1)
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
  # incidence rate post exposure, do not calculate if event_count <=5
  if(event_count_exposed >5){
    person_years_total_exposed = person_days_total_exposed/365.2
    incidence_rate_exposed= round(event_count_exposed/person_years_total_exposed, 4)
    ir_lower_exposed = round(incidence_rate_exposed - 1.96 * sqrt(event_count_exposed/person_days_total_exposed^2),4)
    ir_upper_exposed = round(incidence_rate_exposed + 1.96 * sqrt(event_count_exposed/person_days_total_exposed^2),4)
  }else{
    event_count_exposed = "redacted"
    person_days_total_exposed = person_years_total_exposed = incidence_rate_exposed = ir_lower_exposed = ir_upper_exposed = "redacted"
  }
  
  # incidence rate pre exposure, do not calculate if event_count <=5
  if(event_count_unexposed >5){
    person_years_total_unexposed = person_days_total_unexposed/365.2
    incidence_rate_unexposed= round(event_count_unexposed/person_years_total_unexposed, 4)
    ir_lower_unexposed = round(incidence_rate_unexposed - 1.96 * sqrt(event_count_unexposed/person_days_total_unexposed^2),4)
    ir_upper_unexposed = round(incidence_rate_unexposed + 1.96 * sqrt(event_count_unexposed/person_days_total_unexposed^2),4)
  }else{
    event_count_unexposed = "redacted"
    person_days_total_unexposed = person_years_total_unexposed = incidence_rate_unexposed = ir_lower_unexposed = ir_upper_unexposed = "redacted"
  }
  return(c(person_days_total_unexposed, event_count_unexposed, incidence_rate_unexposed, ir_lower_unexposed, ir_upper_unexposed, person_days_total_exposed, event_count_exposed, incidence_rate_exposed, ir_lower_exposed, ir_upper_exposed))
}

table_2_subgroups_output <- function(population){
  # define analyses of interests
  active_analyses <- read_rds("lib/active_analyses.rds")
  active_analyses <- active_analyses %>%dplyr::filter(active == "TRUE")
  
  analyses_of_interest <- as.data.frame(matrix(ncol = 8,nrow = 0))
  
  outcomes<-active_analyses$outcome_variable
  
  survival_data <- read_rds(paste0("output/input_table_2_",population,"_stage1.rds"))
  #survival_data <- read_csv(paste0("output/input_table_2_",population,"_stage1.rds"))
  survival_data<-setDT(survival_data)
  # for testing: i="out_date_ate"
  for(i in outcomes){
    analyses_to_run <- active_analyses %>% filter(outcome_variable==i)
    
    ##Set which cohorts are required
    if(analyses_to_run$cohort=="all"){
      cohort_to_run=c("vaccinated", "electively_unvaccinated")
    }else{
      analyses_to_run=active_analyses$cohort
    }  
    
    # Transpose active_analyses to single column so can filter to analysis models to run
    analyses_to_run <- as.data.frame(t(analyses_to_run))
    analyses_to_run$subgroup <- row.names(analyses_to_run)
    colnames(analyses_to_run) <- c("run","subgroup")
    
    analyses_to_run<- analyses_to_run %>% filter(run=="TRUE"  & subgroup != "active") 
    rownames(analyses_to_run) <- NULL
    analyses_to_run <- analyses_to_run %>% select(!run)
    analyses_to_run$event=i
    
    # Add in  all possible combinations of the subgroups, models and cohorts
    analyses_to_run <- crossing(analyses_to_run,cohort_to_run)
    
    # Add in which covariates to stratify by
    analyses_to_run$stratify_by_subgroup=NA
    for(j in c("ethnicity","sex")){
      analyses_to_run$stratify_by_subgroup <- ifelse(startsWith(analyses_to_run$subgroup,i),i,analyses_to_run$stratify_by_subgroup)
    }
    index = which(active_analyses$outcome_variable == i)
    analyses_to_run$stratify_by_subgroup <- ifelse(startsWith(analyses_to_run$subgroup,"prior_history"),active_analyses$prior_history_var[index],analyses_to_run$stratify_by_subgroup)
    analyses_to_run$stratify_by_subgroup <- ifelse(is.na(analyses_to_run$stratify_by_subgroup),analyses_to_run$subgroup,analyses_to_run$stratify_by_subgroup)
    
    # Add in relevant subgroup levels to specify which stratum to run for
    analyses_to_run$strata <- NA
    analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="main","main",analyses_to_run$strata)
    analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="covid_history","TRUE",analyses_to_run$strata)
    
    for(i in c("covid_pheno_","agegp_","sex_","ethnicity_","prior_history_")){
      analyses_to_run$strata <- ifelse(startsWith(analyses_to_run$subgroup,i),gsub(i,"",analyses_to_run$subgroup),analyses_to_run$strata)
    }
    analyses_of_interest <- rbind(analyses_of_interest,analyses_to_run)
    
  }
  
  analyses_of_interest <- analyses_of_interest %>% filter(cohort_to_run == population)
  #write.csv(analyses_of_interest, file=paste0("output/analyses_of_interest_", population, ".csv"))
  #ir = incidence rate; ir_lower = lower bound of the 95% CI for ir; ir_upper = upper bound of the 95% CI for ir
  unexposed_person_days <- unexposed_event_count <- unexposed_ir <- unexposed_ir_lower <- unexposed_ir_upper <- rep("NA", nrow(analyses_of_interest))
  exposed_person_days <- exposed_event_count <- exposed_ir <- exposed_ir_lower <- exposed_ir_upper <- rep("NA", nrow(analyses_of_interest))
  
  analyses_of_interest <- cbind(analyses_of_interest, unexposed_person_days, unexposed_event_count, unexposed_ir, unexposed_ir_lower, unexposed_ir_upper,
                                exposed_person_days, exposed_event_count, exposed_ir, exposed_ir_lower, exposed_ir_upper)
  
  # specify subgroup names
  index <- grepl("agegp", analyses_of_interest$subgroup, fixed = TRUE)
  analyses_of_interest$stratify_by_subgroup[index] <- "sub_cat_age_group"
  
  index <- grepl("covid_history", analyses_of_interest$subgroup, fixed = TRUE)
  analyses_of_interest$stratify_by_subgroup[index] <- "sub_bin_covid19_confirmed_history"
  
  index <- grepl("covid_pheno_", analyses_of_interest$subgroup, fixed = TRUE)
  analyses_of_interest$stratify_by_subgroup[index] <- "sub_cat_covid19_hospital"
  
  index <- grepl("ethnicity", analyses_of_interest$subgroup, fixed = TRUE)
  analyses_of_interest$stratify_by_subgroup[index] <- "sub_cat_ethnicity"
  
  index <- grepl("sex", analyses_of_interest$subgroup, fixed = TRUE)
  analyses_of_interest$stratify_by_subgroup[index] <- "sub_bin_sex"
  
  index <- grepl("main", analyses_of_interest$subgroup, fixed = TRUE)
  analyses_of_interest$stratify_by_subgroup[index] <- "sub_main"
  
  analyses_of_interest$stratify_by_subgroup <- gsub("cov_bin","sub_bin",analyses_of_interest$stratify_by_subgroup)
  
  #index <- grepl("ate", analyses_of_interest$subgroup, fixed = TRUE)
  #analyses_of_interest$stratify_by_subgroup[index] <- "sub_bin_ate"
  
  # if only need table2 for the main analyses
  if(analyses == "main"){
    analyses_of_interest <- analyses_of_interest %>% filter(stratify_by_subgroup == "sub_main") 
  }
  if(analyses == "subgroups"){
    analyses_of_interest <- analyses_of_interest %>% filter(stratify_by_subgroup != "sub_main") 
  }
  
  #index <- grepl("cov_bin_vte", analyses_of_interest$stratify_by_subgroup, fixed = TRUE)
  #analyses_of_interest$stratify_by_subgroup[index] <- "sub_bin_vte"
  col_names <- names(analyses_of_interest)
  start = grep("unexposed_person_days", col_names)
  end = ncol(analyses_of_interest)

  for(i in 1:nrow(analyses_of_interest)){
    #for(i in 40:88){
    #d <- analyses_of_interest
    print(i)
    event_short = gsub("out_date_", "",analyses_of_interest$event[i])
    setnames(survival_data,
             old = c(paste0("out_date_",event_short),
                     paste0(event_short,"_follow_up_end_unexposed"),
                     paste0(event_short,"_follow_up_end_exposed"),
                     paste0(event_short,"_person_days_unexposed"),
                     paste0(event_short,"_person_days_exposed"),
                     paste0(event_short,"_include")),
             
               new = c("event_date",
                     "follow_up_end_unexposed",
                     "follow_up_end_exposed",
                     "person_days_unexposed",
                     "person_days_exposed",
                     "include"))
    
    analyses_of_interest[i,start:end] <- table_2_calculation(survival_data, 
                                                             event=analyses_of_interest$event[i],
                                                             cohort=analyses_of_interest$cohort_to_run[i],
                                                             subgrp=analyses_of_interest$subgroup[i], 
                                                             subgrp_level=analyses_of_interest$strata[i], 
                                                             subgrp_full_name=analyses_of_interest$stratify_by_subgroup[i])
    
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
    
    print(paste0("event count and person years have been produced successfully for", analyses_of_interest$event[i], " in ", population, " population!"))
  }
  
  # extract input1_aer
  input1_aer <- analyses_of_interest %>% select(c("event", "cohort_to_run", "subgroup", "strata", "unexposed_person_days"))
  names(input1_aer)[which(names(input1_aer) == "cohort_to_run")] = "cohort"
  input1_aer$event <- ifelse(startsWith(input1_aer$event,"out_"),gsub("out_date_","",input1_aer$event),input1_aer$event)
  
  # write output for table2
  write.csv(analyses_of_interest, file=paste0("output/table2_", analyses, "_", population, ".csv"), row.names = F)
  rmarkdown::render("analysis/compiled_table2_results.Rmd",
                    output_file=paste0("table2_",analyses,"_", population),output_dir="output")
  #write output fir input1_aer
  write.csv(input1_aer, file=paste0("output/input1_aer_", analyses, "_", population, ".csv"), row.names=F)
  rmarkdown::render("analysis/compiled_input1_aer_results.Rmd",
                    output_file=paste0("input1_aer_",analyses,"_", population),output_dir="output")
}

# Run function using specified commandArgs
if(population == "both"){
  table_2_subgroups_output("vaccinated")
  table_2_subgroups_output("electively_unvaccinated")
}else{
  table_2_subgroups_output(population)
}


# end.time=Sys.time()
# 
# run.time = end.time - start.time
# 
# run.time
