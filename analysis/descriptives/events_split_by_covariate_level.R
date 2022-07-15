## ====================================================================================
## Purpose: Split hospitalised event counts by covariate level and time period
## 
## ====================================================================================

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

output_dir <- "output/not-for-review"
scripts_dir <- "analysis/model"


#delta period
cohort_start = as.Date("2021-06-01", format="%Y-%m-%d")
cohort_end = as.Date("2021-12-14", format="%Y-%m-%d")

agebreaks <- c(0, 40, 60, 80, 111)
agelabels <- c("18_39", "40_59", "60_79", "80_110")

time_periods_normal <- c(7, 14, 28, 56, 84, 197) 
time_periods_reduced <- c(28,197)
time_periods_alternative <- c(1,8,43,197)

event_by_covariate_level <- function(cohort_name, time_periods,save_name){
  
  # define analyses of interests
  active_analyses <- read_rds("lib/active_analyses.rds")
  active_analyses <- active_analyses %>%dplyr::filter(active == "TRUE")
  
  analyses_of_interest <- as.data.frame(matrix(ncol = 8,nrow = 0))
  
  #Left join end dates table
  survival_data <- read_rds(paste0("output/input_",cohort_name,"_stage1.rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort_name,".rds")) 
  end_dates$index_date <- NULL
  
  survival_data<- survival_data %>% left_join(end_dates, by="patient_id")
  rm(end_dates)
  
  
  setnames(survival_data, 
           old = c("exp_date_covid19_confirmed",
                   "index_date"), 
           new = c("expo_date",
                   "follow_up_start"))
  
  survival_data$agegroup <- cut(survival_data$cov_num_age, 
                               breaks = agebreaks, 
                               right = FALSE, 
                               labels = agelabels)
  
  #survival_data[ , agegroup = cut(cov_num_age, 
  #                                        breaks = agebreaks, 
  #                                        right = FALSE, 
  #                                        labels = agelabels)]
  
  
  for(i in active_analyses$outcome_variable){
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
      analyses_to_run$stratify_by_subgroup <- ifelse(startsWith(analyses_to_run$subgroup,j),paste0("cov_cat_",j),analyses_to_run$stratify_by_subgroup)
    }
    
    index = which(active_analyses$outcome_variable == i)
    analyses_to_run$stratify_by_subgroup <- ifelse(startsWith(analyses_to_run$subgroup,"prior_history"),active_analyses$prior_history_var[index],analyses_to_run$stratify_by_subgroup)
    analyses_to_run$stratify_by_subgroup <- ifelse(is.na(analyses_to_run$stratify_by_subgroup),analyses_to_run$subgroup,analyses_to_run$stratify_by_subgroup)
    
    # Add in relevant subgroup levels to specify which stratum to run for
    analyses_to_run$strata <- NA
    analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="main","main",analyses_to_run$strata)
    analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="covid_history","TRUE",analyses_to_run$strata)
    
    for(k in c("covid_pheno_","agegp_","sex_","ethnicity_","prior_history_")){
      analyses_to_run$strata <- ifelse(startsWith(analyses_to_run$subgroup,k),gsub(k,"",analyses_to_run$subgroup),analyses_to_run$strata)
    }
    analyses_of_interest <- rbind(analyses_of_interest,analyses_to_run)
    
  }
  
  analyses_of_interest$strata[analyses_of_interest$strata=="South_Asian"]<- "South Asian"
  analyses_of_interest <- analyses_of_interest %>% filter(cohort_to_run == cohort_name)
  

  results <- as.data.frame(matrix(ncol = (5+length(time_periods)) , nrow = 0))
  time_periods_names <- c()
  time_periods_with_zero <- sort(append(time_periods,0))
  
  for(i in 1:(length(time_periods_with_zero)-1)){
    time_periods_names <- append(time_periods_names,paste0("days",time_periods_with_zero[i],"_",time_periods_with_zero[i+1],"_event_counts"))
  }
  
  colnames(results) <- c("event","covariate","level","unexposed_event_counts",time_periods_names,"subgroup")
  
  for(i in 1:nrow(analyses_of_interest)){
    
    print(paste0("Starting work on ",analyses_of_interest$event[i]," ",analyses_of_interest$subgroup[i]))
    
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
    
    summary <- event_by_covariate_level_counts(survival_data,
                                                    event=analyses_of_interest$event[i],
                                                    subgroup=analyses_of_interest$subgroup[i], 
                                                    stratify_by=analyses_of_interest$strata[i], 
                                                    stratify_by_subgroup=analyses_of_interest$stratify_by_subgroup[i],
                                                    time_periods,active_analyses,time_periods_with_zero,time_periods_names)
    
    
    results <- rbind(results, summary)

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
    
    #print(paste0("Covariate summary has been produced successfully for ", outcomes[i], " in ", cohort_name, " population!"))
  }
  
  
  results <- results %>% dplyr::select(event, subgroup, covariate, level, unexposed_event_counts, all_of(time_periods_names))
  
  # write output for covariate count table
  write.csv(results, file=paste0(output_dir,"/event_counts_by_covariate_level_",cohort_name,"_",save_name,"_time_periods.csv"), row.names = F)
  
  select_covariates_for_cox(results, save_name, time_periods_names, active_analyses,cohort_name)
}


event_by_covariate_level_counts <- function(survival_data,event,subgroup,stratify_by,stratify_by_subgroup,time_periods,active_analyses,time_periods_with_zero,time_periods_names){
  covar_names <- str_split(active_analyses %>% filter(outcome_variable == event)%>% select(covariates),";")[[1]]
  covar_names<-append(covar_names,"patient_id")
  
  if(subgroup != "covid_history" ){
    survival_data=survival_data%>%filter(sub_bin_covid19_confirmed_history == FALSE)
  }else {
    survival_data=survival_data%>%filter(sub_bin_covid19_confirmed_history == TRUE)
  }
  
  for(i in c("hospitalised","non_hospitalised")){
    if(stratify_by == i){
      survival_data$follow_up_end <- NULL
      setnames(survival_data, 
               old = c(c(paste0(i,"_follow_up_end")),
                       c(paste0(i,"_date_expo_censor"))),
               
               new = c("follow_up_end",
                       "date_expo_censor"))
    }
  }
  
  # filter the population according to the subgroup level
  
  for(i in c("ethnicity","sex","prior_history")){
    if(startsWith(subgroup,i)){
      survival_data=survival_data%>%filter_at(stratify_by_subgroup,all_vars(.==stratify_by))
    }
  }
  
  if(startsWith(subgroup,"agegp_")){
    survival_data=survival_data %>% filter(agegroup== stratify_by)
  }
  
  # Detect if a column is of date type, if so impose study start/end dates
  # only really interested in event_date and expo_date being within follow-up at this point as all other date variable 
  #have been checked in inclusion/exclusion & QA
  
  survival_data <- survival_data %>% mutate(event_date = replace(event_date, which(event_date>follow_up_end | event_date<follow_up_start), NA))
  survival_data <- survival_data %>% mutate(expo_date = replace(expo_date, which(expo_date>follow_up_end | expo_date<follow_up_start), NA))
  
  # 1.Adjust follow up end date for COVID phenotype dataset to censor at COVID exposure for the
  # phenotype that is not of interest
  # 2.Remove people who's COVID exposure censor date is the same as their follow-up start date as they 
  # have no follow up period (for the pheno not of interest follow up is follow up start to the day before exposure so
  # if follow_up_start = date_expo_censor, follow up end is prior to follow up start).
  # 3.Follow up end being the day before date_expo_censor if the min of follow_up_end/date_expo_censor is date_expo_censor
  # is taken into account in a later script
  # 4.We want to keep people who's exposure censor date is after follow up start or who do not have an exposure data
  
  if(startsWith(subgroup,"covid_pheno_")){
    survival_data <- survival_data %>% mutate(expo_date = replace(expo_date, which(!is.na(date_expo_censor) & (expo_date >= date_expo_censor)), NA) )%>%
      mutate(event_date = replace(event_date, which(!is.na(date_expo_censor) & (event_date >= date_expo_censor)), NA)) %>%
      filter((follow_up_start != date_expo_censor)|is.na(date_expo_censor))
  }
  
  
  survival_data=survival_data%>%filter(follow_up_end>=follow_up_start)
  
  #Filter to only those with an outcome
  if(startsWith(subgroup,"covid_pheno_")){
    survival_data <- survival_data %>% filter((!is.na(event_date)) & 
                                        (
                                          (event_date == follow_up_end) & (event_date < date_expo_censor | is.na(date_expo_censor))
                                        ))
  }else{
    survival_data <- survival_data %>% filter((!is.na(event_date)) & 
                                        (
                                          event_date == follow_up_end
                                        ))
  }
  
  survival_data$event_expo_status <- ifelse((survival_data$event_date >= survival_data$follow_up_start & 
                                               survival_data$event_date <= survival_data$follow_up_end) &
                                              (survival_data$event_date < survival_data$expo_date | is.na(survival_data$expo_date)),"pre_exposure","post_exposure")
  
  post_expo <- survival_data %>% filter(event_expo_status == "post_exposure")
  post_expo$time_from_expo_to_event <- as.numeric(post_expo$event_date - post_expo$expo_date)
  
  post_expo$event_time_periods <- cut(post_expo$time_from_expo_to_event, 
                                      breaks = time_periods_with_zero, 
                                      right = FALSE, 
                                      labels = time_periods_names)
  
  
  # Pre-exposure event counts
  pre_expo <- survival_data %>% filter(event_expo_status == "pre_exposure")

  df <- pre_expo %>% dplyr::select(c( all_of(covar_names)))
  df <- df %>%  dplyr::select(!c("patient_id",
                                 df %>%  dplyr::select_if(is.numeric) %>% names()))
  
  summary_pre_expo <- as.data.frame(summary(df,maxsum=100))
  summary_pre_expo <- summary_pre_expo %>% filter(startsWith(Freq,"Mode")==F)
  summary_pre_expo$unexposed_event_counts <- summary_pre_expo$Freq
  summary_pre_expo$Freq=gsub(":.*", "",summary_pre_expo$Freq)#Remove everything after:
  summary_pre_expo$unexposed_event_counts=gsub(".*:", "",summary_pre_expo$unexposed_event_counts)#Remove everything before
  summary_pre_expo$Var2 <- gsub("\\s","",summary_pre_expo$Var2)
  summary_pre_expo$Var1 <- NULL
  
  for(period in time_periods_names){
    df <- post_expo %>% filter(event_time_periods == period) %>% 
                        select(c( all_of(covar_names))) 
    df <- df %>%  select(!c("patient_id",
                        df %>%  dplyr::select_if(is.numeric) %>% names()))
    df <- as.data.frame(summary(df,maxsum=100))
    df <- df %>% filter(startsWith(Freq,"Mode")==F)
    df[,period] <- df$Freq
    df$Freq=gsub(":.*", "",df$Freq)#Remove everything after:
    df[,period]=gsub(".*:", "",df[,period])#Remove everything before
    df$Var2 <- gsub("\\s","",df$Var2)
    df$Var1 <- NULL
    summary_pre_expo <- summary_pre_expo %>% left_join(df, by = c("Var2","Freq"))
  }
  
  summary_pre_expo[is.na(summary_pre_expo)] = 0
  summary_pre_expo$event <- event
  summary_pre_expo$subgroup <- subgroup
  
  setnames(summary_pre_expo,
           old = c("Var2",
                   "Freq"),
           new = c("covariate",
                   "level"))
  
  
  return(summary_pre_expo)
}


select_covariates_for_cox <- function(results, save_name,time_periods_names, active_analyses,cohort_name){
  print(paste0("Starting work on selecting covariates"))
  
  results <- results %>% mutate(across(c(unexposed_event_counts, all_of(time_periods_names)), as.numeric))
  
  covariates_to_adjust_for <- as.data.frame(matrix(ncol = 3, nrow = 0))
  colnames(covariates_to_adjust_for) <- c("outcome_event", "subgroup", "covariates")
 
  for(i in active_analyses$outcome_variable){
    protected_covariates<-str_split(active_analyses %>% filter(outcome_variable == i)%>% select(covariates),";")[[1]]
    protected_covariates <- protected_covariates[grepl("cov_num|cov_cat",protected_covariates)]
    protected_covariates
    tmp <- results %>% filter(event ==i)
    tmp <- tmp %>% filter(!covariate %in% protected_covariates)
    tmp$keep_covariate <- NA
    
    #I can't think of a better way to do this that doesn't involve have to specify the 
    #column names - thecolumn names are all saved under time_period_names but can't get it to 
    #work dynamically
    
    if(save_name == "normal"){
      tmp <- tmp %>%
        group_by(event, covariate, subgroup) %>%
        dplyr::mutate(keep_covariate = case_when(
          any(unexposed_event_counts == 0 | days0_7_event_counts == 0 | days7_14_event_counts == 0 |
                days14_28_event_counts == 0 | days28_56_event_counts ==0 | days56_84_event_counts ==0 | days84_197_event_counts ==0) ~ "remove_covariate",
          TRUE ~ "keep_covariate"))
    }else if(save_name == "reduced"){
      tmp <- tmp %>%
        group_by(event, covariate, subgroup) %>%
        dplyr::mutate(keep_covariate = case_when(
          any(unexposed_event_counts == 0 | days0_28_event_counts == 0 | days0_28_event_counts == 0) ~ "remove_covariate",
          TRUE ~ "keep_covariate"))
    }else if(save_name == "alternative"){
      tmp <- tmp %>%
        group_by(event, covariate, subgroup) %>%
        dplyr::mutate(keep_covariate = case_when(
          any(unexposed_event_counts == 0 | days0_1_event_counts == 0 | days1_8_event_counts == 0 |
                days8_43_event_counts == 0 | days43_197_event_counts ==0 ) ~ "remove_covariate",
          TRUE ~ "keep_covariate"))
    }
    
    
    for(subgroup_name in unique(tmp$subgroup)){
      df <- tmp %>% filter(keep_covariate == "keep_covariate" & subgroup == subgroup_name)
      covariates_to_adjust_for[nrow(covariates_to_adjust_for)+1,] <- c(i,subgroup_name, paste(c(unique(df$covariate),protected_covariates), collapse = ";"))
    }
    
  }
  
  covariates_to_adjust_for$time_period <-  save_name
  write.csv(covariates_to_adjust_for, file=paste0(output_dir,"/non_zero_selected_covariates_",cohort_name,"_",save_name,"_time_periods.csv"), row.names = F)
  
}



# Run function using specified commandArgs
if(cohort_name == "both"){
  event_by_covariate_level("vaccinated",time_periods_normal,"normal")
  event_by_covariate_level("vaccinated",time_periods_reduced,"reduced")
  event_by_covariate_level("vaccinated",time_periods_alternative,"alternative")
  event_by_covariate_level("electively_unvaccinated",time_periods_normal,"normal")
  event_by_covariate_level("electively_unvaccinated",time_periods_reduced,"reduced")
  event_by_covariate_level("electively_unvaccinated",time_periods_alternative,"alternative")
}else{
  event_by_covariate_level(cohort_name,time_periods_normal,"normal")
  event_by_covariate_level(cohort_name,time_periods_reduced,"reduced")
  event_by_covariate_level(cohort_name,time_periods_alternative,"alternative")
}

