library(readr)
library(dplyr)
library(data.table) 
library(lubridate)
library(stringr)
library(tidyverse)
library(survival)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort <- "vaccinated"
  #cohort_name = "electively_unvaccinated"
}else{
  cohort <- args[[1]]
}

fs::dir_create(here::here("output", "review", "descriptives"))

#delta period
cohort_start_date = as.Date("2021-06-01", format="%Y-%m-%d")
cohort_end_date = as.Date("2021-12-14", format="%Y-%m-%d")

cuts_days_since_expo <- c(7, 14, 28, 56, 84, 197) 
cuts_days_since_expo_reduced <- c(28,197)
cuts_days_since_expo_day_zero <- c(1,28,197)

source("analysis/descriptives/event_counts_by_time_period_functions.R")

event_counts_by_time_period <- function(cohort){
  
  #----------------------Define analyses of interests---------------------------
  active_analyses <- read_rds("lib/active_analyses.rds")
  active_analyses <- active_analyses %>%dplyr::filter(active == "TRUE")
  
  analyses_of_interest <- as.data.frame(matrix(ncol = 8,nrow = 0))
  
  outcomes<-active_analyses$outcome_variable
  
  #--------------------Load data and left join end dates------------------------
  input <- read_rds(paste0("output/input_",cohort,"_stage1.rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort,".rds")) 
  end_dates$index_date <- NULL
  
  input<- input %>% left_join(end_dates, by="patient_id")
  rm(end_dates)
  
  input<-input[,c("patient_id","index_date","cov_cat_sex",
                                  "cov_num_age","cov_cat_ethnicity",
                                  "sub_bin_covid19_confirmed_history","exp_date_covid19_confirmed","sub_cat_covid19_hospital",
                                  colnames(input)[grepl("out_",colnames(input))],
                                  colnames(input)[grepl("follow_up",colnames(input))],
                                  colnames(input)[grepl("_expo_",colnames(input))],
                                  active_analyses$prior_history_var[active_analyses$prior_history_var !=""])]
  
  setnames(input, 
           old = c("cov_cat_sex", 
                   "cov_cat_ethnicity",
                   "exp_date_covid19_confirmed",
                   "index_date"), 
           new = c("sex",
                   "ethnicity",
                   "expo_date",
                   "follow_up_start"))
  
  for(i in outcomes){
    analyses_to_run <- active_analyses %>% filter(outcome_variable==i)
    
    # Transpose active_analyses to single column so can filter to analysis models to run
    analyses_to_run <- as.data.frame(t(analyses_to_run))
    analyses_to_run$subgroup <- row.names(analyses_to_run)
    colnames(analyses_to_run) <- c("run","subgroup")
    
    analyses_to_run<- analyses_to_run %>% filter(run=="TRUE"  & subgroup %in% c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised")) 
    rownames(analyses_to_run) <- NULL
    analyses_to_run <- analyses_to_run %>% select(!run)
    analyses_to_run$event=i
    
    # Add in  all possible combinations of the subgroups, models and cohorts
    analyses_to_run <- crossing(analyses_to_run,cohort)
    
    # Add in which covariates to stratify by
    analyses_to_run$stratify_by_subgroup=NA
    analyses_to_run$stratify_by_subgroup <- ifelse(is.na(analyses_to_run$stratify_by_subgroup),analyses_to_run$subgroup,analyses_to_run$stratify_by_subgroup)
    
    # Add in relevant subgroup levels to specify which stratum to run for
    analyses_to_run$strata <- NA
    analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="main","main",analyses_to_run$strata)
    analyses_to_run$strata <- ifelse(startsWith(analyses_to_run$subgroup,"covid_pheno_"),gsub("covid_pheno_","",analyses_to_run$subgroup),analyses_to_run$strata)
    
    analyses_of_interest <- rbind(analyses_of_interest,analyses_to_run)
    
  }
  
  analyses_of_interest <- analyses_of_interest %>% filter(event %in% c("out_date_ami","out_date_stroke_isch", "out_date_pe","out_date_dvt","out_date_ate","out_date_vte"))
  
  tmp_normal <- analyses_of_interest %>% mutate(reduced_timepoint = "normal")
  tmp_reduced <- analyses_of_interest %>% mutate(reduced_timepoint = "reduced")
  tmp_day_zero <- analyses_of_interest %>% mutate(reduced_timepoint = "day_zero")
  analyses_to_run <- rbind(tmp_normal,tmp_reduced,tmp_day_zero)
  rm(tmp_normal,tmp_reduced,tmp_day_zero,analyses_of_interest)
  
  for(i in unique(analyses_to_run$event)){
    tmp <- analyses_to_run %>% filter(event == i)
    
    event_short = gsub("out_date_", "",i)
    setnames(input,
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
                     "hospitalised_censor_date",
                     "non_hospitalised_censor_date"))
    event=tmp$event[7]           
    subgroup=tmp$subgroup[7]          
    stratify_by_subgroup=tmp$stratify_by_subgroup[7]          
    stratify_by=tmp$strata[7]      
    time_point=tmp$reduced_timepoint[7]
    
    lapply(split(tmp,seq(nrow(tmp))),
           function(tmp)
             subgroup_data(           
               event=tmp$event,           
               subgroup=tmp$subgroup,           
               stratify_by_subgroup=tmp$stratify_by_subgroup,           
               stratify_by=tmp$strata,           
               time_point=tmp$reduced_timepoint,       
               input))
    
    setnames(input,
             old = c("event_date",
                     "follow_up_end_unexposed",
                     "follow_up_end",
                     "hospitalised_follow_up_end",
                     "non_hospitalised_follow_up_end",
                     "hospitalised_censor_date",
                     "non_hospitalised_censor_date"),
             
             new = c(paste0("out_date_",event_short),
                     paste0(event_short,"_follow_up_end_unexposed"),
                     paste0(event_short,"_follow_up_end"),
                     paste0(event_short,"_hospitalised_follow_up_end"),
                     paste0(event_short,"_non_hospitalised_follow_up_end"),
                     paste0(event_short,"_hospitalised_date_expo_censor"),
                     paste0(event_short,"_non_hospitalised_date_expo_censor"))
             
             )
  }
  
  results_done <- c()
  for (i in 1:nrow(analyses_to_run)) {
    row <- analyses_to_run[i,]
    fpath <- file.path(paste0("output/tbl_event_count_",
                              row$event, "_",
                              row$subgroup, "_",
                              row$cohort, "_",
                              row$reduced_timepoint,"_time_periods.csv"))
    results_done <- c(results_done, fpath)
  }
  
  result_file_paths <- pmap(list(results_done), 
                            function(fpath){ 
                              df <- fread(fpath) 
                              return(df)
                            })
  event_counts <- rbindlist(result_file_paths, fill=TRUE)
  event_counts$V1 <- NULL
  
  #-----------------Add subgroup category for low count redaction---------------
  event_counts <- event_counts %>% 
    dplyr::mutate(subgroup_cat = case_when(
      startsWith(subgroup, "covid_pheno") ~ "covid_pheno",
      startsWith(subgroup, "main") ~ "main",
      TRUE ~ as.character(subgroup)))
  
  #Redact all subgroups levels if one level is redacted so that back calculation 
  #is not possible
  event_counts <- event_counts %>%
    group_by(subgroup_cat,event,time_points) %>%
    dplyr::mutate(person_time = case_when(
      any(events_total <=5 ) ~ "[Redacted]",
      TRUE ~ as.character(events_total)))#
  
  event_counts <- event_counts %>%
    group_by(subgroup_cat,event,time_points) %>%
    dplyr::mutate(`incidence rate (per 1000 person years)` = case_when(
      any(events_total <=5 ) ~ "[Redacted]",
      TRUE ~ as.character(events_total)))
  
  event_counts <- event_counts %>%
    group_by(subgroup_cat,event,time_points) %>%
    dplyr::mutate(events_total = case_when(
      any(events_total <=5 ) ~ "[Redacted]",
      TRUE ~ as.character(events_total)))
  
  event_counts$subgroup_cat <- NULL
  
  event_counts$subgroup <- factor(event_counts$subgroup, levels = c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
  event_counts$time_points <- factor(event_counts$time_points, levels = c("normal","reduced","day_zero"))
  event_counts$event <- factor(event_counts$event, levels = c("out_date_ami","out_date_stroke_isch", "out_date_pe","out_date_dvt","out_date_ate","out_date_vte"))
  
  event_counts <- event_counts[order(event_counts$event,event_counts$time_points,event_counts$subgroup),]
  
  write.csv(event_counts, file=paste0("output/review/descriptives/event_counts_by_time_period_",cohort, ".csv"), row.names = F)
}


# Run function using specified commandArgs
if(cohort == "both"){
  event_counts_by_time_period("vaccinated")
  event_counts_by_time_period("electively_unvaccinated")
}else{
  event_counts_by_time_period(cohort)
}

