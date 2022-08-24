## =============================================================================
## Format results into combined HR and event counts files
## =============================================================================

print("Working on formating tables")

rm(list=setdiff(ls(), c("cohort","mdl","output_dir","scripts_dir","analyses_to_run","event_name")))

results_needed=analyses_to_run

results_done <- c()
results_missing=data.frame()

for (i in 1:nrow(results_needed)) {
  row <- results_needed[i,]
  fpath <- file.path(output_dir,
                     paste0("tbl_hr_",
                            row$event, "_",
                            row$subgroup, "_",
                            row$cohort, "_",
                            row$reduced_timepoint,"_time_periods.csv"))
  
  if (!file.exists(fpath)) {
    results_missing <- rbind(results_missing, row)
  } else {
    results_done <- c(results_done, fpath)
  }
}

result_file_paths <- pmap(list(results_done), 
               function(fpath){ 
                 df <- fread(fpath) 
                 return(df)
               })


if(length(results_done)>0){
  df_hr <- rbindlist(result_file_paths, fill=TRUE)
  df_hr <- df_hr %>% mutate_if(is.numeric, round, digits=5)%>%select(-V1)
  write.csv(df_hr, paste0(output_dir,"/compiled_HR_results_", event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Compiled HR's saved: ", output_dir,"/compiled_HR_results_", event_name,"_", cohort,".csv"))
}else{
  df_hr <- as.data.frame(matrix(ncol = 16))
  colnames(df_hr) <- c("term", "estimate", "conf_low", "conf_high", "std_error_ln_hr", "robust_se_ln_hr", "covariates_removed",
                       "cat_covars_collapsed","results_fitted", "model","subgroup", "event", "cohort", "time_period", "total_covid19_cases","cox_weight")
  write.csv(df_hr, paste0(output_dir,"/compiled_HR_results_", event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Compiled HR's saved: ", output_dir,"/compiled_HR_results_", event_name,"_", cohort,".csv"))
}


# =============================  R events count ================================

event_count_missing <- data.frame()
event_count_done <- c()

for (i in 1:nrow(results_needed)) {
  row <- results_needed[i,]
  fpath <- file.path(output_dir,
                     paste0("tbl_event_count_",
                            row$event, "_",
                            row$subgroup, "_",
                            row$cohort, "_",
                            row$reduced_timepoint,"_time_periods.csv"))
  
  if (!file.exists(fpath)) {
    event_count_missing <- rbind(event_count_missing, row)
  } else {
    event_count_done <- c(event_count_done, fpath)
  }
}


event_counts_completed <- pmap(list(event_count_done), 
                                 function(fpath){ 
                                   df <- fread(fpath) 
                                   return(df)
                                 })

if(length(event_count_done)>0){
  df_event_counts <- rbindlist(event_counts_completed, fill=TRUE)  %>% dplyr::select(!"V1")
  df_event_counts <- df_event_counts %>% select(event, cohort, subgroup, time_points, expo_week, events_total, person_days_follow_up, `incidence rate (per 1000 person years)`, median_follow_up)
  write.csv(df_event_counts, paste0(output_dir,"/compiled_event_counts_", event_name, "_", cohort,".csv") , row.names=F)
  print(paste0("Compiled event counts saved: ", output_dir,"/compiled_event_counts_", event_name,"_", cohort,".csv"))
  
  # Add in suppression for counts <=5
  df_event_counts$redacted_results <- "NA"
  df_event_counts$person_days_follow_up <- NULL
  df_event_counts$`incidence rate (per 1000 person years)` <- NULL
  
  supressed_df_event_counts <- df_event_counts[0,]
  
  for(i in 1:nrow(analyses_to_run)){
    subgroup_of_interest=analyses_to_run$subgroup[i]
    cohort_of_interest=analyses_to_run$cohort[i]
    time_points_of_interest=analyses_to_run$reduced_timepoint[i]
    
    tmp <- df_event_counts %>% filter(subgroup == subgroup_of_interest & cohort == cohort_of_interest & time_points == time_points_of_interest)
    
    tmp$events_total <- as.numeric(tmp$events_total)
    tmp <- tmp %>% 
      mutate(events_total = replace(events_total, expo_week=="all post expo", sum(tmp[which(tmp$events_total >5 & !(tmp$expo_week %in% c("pre expo", "all post expo"))),events_total])))   
    tmp <- tmp %>% 
      mutate(median_follow_up = replace(median_follow_up,events_total <=5, "[Redacted]"),
             events_total = replace(events_total, events_total <=5, "[Redacted]"))
    
    tmp$events_total <- as.character(tmp$events_total)
    tmp$redacted_results <- ifelse(any(tmp$events_total == "[Redacted]", na.rm = T), "Redacted results", "No redacted results")
    supressed_df_event_counts <- rbind(supressed_df_event_counts,tmp)
  }
  
  supressed_df_event_counts$redacted_results <- factor(supressed_df_event_counts$redacted_results, levels = c("Redacted results",
                                                                                                              "No redacted results"))
  supressed_df_event_counts <- supressed_df_event_counts[order(supressed_df_event_counts$redacted_results),]
  
  write.csv(supressed_df_event_counts, paste0(output_dir,"/suppressed_compiled_event_counts_", event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Supressed event counts saved: ", output_dir,"/suppressed_compiled_event_counts_", event_name,"_", cohort,".csv"))
  
}else{
  df_event_counts <- as.data.frame(matrix(ncol = 9))
  colnames(df_event_counts)<- c("event","cohort","subgroup","time_points","expo_week", "events_total",
                                "person_days_follow_up", "incidence rate (per 1000 person years)", "median_follow_up")
  
  write.csv(df_event_counts, paste0(output_dir,"/compiled_event_counts_", event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Compiled event counts saved: ", output_dir,"/compiled_event_counts_", event_name,"_", cohort,".csv"))
 
  df_event_counts <- as.data.frame(matrix(ncol = 8))
  colnames(df_event_counts)<- c("event","cohort","subgroup","time_points","expo_week", "events_total",
                                "median_follow_up","redacted_results")
  
  write.csv(df_event_counts, paste0(output_dir,"/suppressed_compiled_event_counts_", event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Supressed event counts saved: ", output_dir,"/suppressed_compiled_event_counts_", event_name,"_", cohort,".csv"))
  
}

#=========================COMBINE EVENT COUNTS AND HRS==========================

if(length(results_done)>0){
  supressed_df_event_counts <- supressed_df_event_counts %>% rename(term=expo_week)
  df_hr=df_hr%>%left_join(supressed_df_event_counts, by=c("term","event","subgroup","cohort","time_points")) %>%
                mutate(across(where(is.numeric), as.character))
  df_hr[which(df_hr$events_total == "[Redacted]"),c("estimate","conf_low","conf_high","std_error_ln_hr","robust_se_ln_hr","median_follow_up")] = "[Redacted]"
  
  supressed_df_hr <- df_hr[0,]
  
  for(i in 1:nrow(analyses_to_run)){
    subgroup_of_interest=analyses_to_run$subgroup[i]
    cohort_of_interest=analyses_to_run$cohort[i]
    time_points_of_interest=analyses_to_run$reduced_timepoint[i]
    
    tmp <- df_hr %>% filter(subgroup==subgroup_of_interest & cohort == cohort_of_interest & time_points == time_points_of_interest)
    tmp$redacted_results <- replace_na(tmp$redacted_results,"No redacted results")
    
    tmp$redacted_results <- ifelse(any(tmp$redacted_results == "Redacted results"),"Redacted results", "No redacted results")
    supressed_df_hr <- rbind(supressed_df_hr,tmp)
  }

  supressed_df_hr$redacted_results <- factor(supressed_df_hr$redacted_results, levels = c("Redacted results",
                                                                                          "No redacted results"))
  supressed_df_hr <- supressed_df_hr[order(supressed_df_hr$redacted_results),]
  
  supressed_df_hr=supressed_df_hr%>%select(event,cohort,subgroup,model,time_points,term,estimate,conf_low,conf_high,std_error_ln_hr,robust_se_ln_hr,
                                           events_total, median_follow_up,results_fitted,covariates_removed,cat_covars_collapsed,redacted_results,cox_weight,total_covid19_cases)
  
  write.csv(supressed_df_hr,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Supressed HR with event counts saved: ", output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,".csv"))
  
  supressed_df_hr <- supressed_df_hr %>% select(!c("events_total")) %>%
                                  filter(!(term %in% c("days_pre","all post expo")))
  write.csv(supressed_df_hr,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,"_to_release.csv") , row.names=F)
  
}else{
  supressed_combined_hr_event_counts <- as.data.frame(matrix(ncol = 19))
  colnames(supressed_combined_hr_event_counts) <- c("event","cohort","subgroup","model","time_points","term","estimate","conf_low","conf_high","std_error_ln_hr","robust_se_ln_hr",
                                                    "events_total", "median_follow_up","results_fitted","covariates_removed","cat_covars_collapsed","redacted_results","cox_weight","total_covid19_cases")
  
  write.csv(supressed_combined_hr_event_counts,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Supressed HR with event counts saved: ", output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,".csv"))
  
  supressed_combined_hr_event_counts <- supressed_combined_hr_event_counts[!colnames(supressed_combined_hr_event_counts) %in% c("events_total")]
  write.csv(supressed_combined_hr_event_counts,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,"_to_release.csv") , row.names=F) 
}

