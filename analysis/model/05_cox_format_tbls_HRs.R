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
  df_hr <- as.data.frame(matrix(ncol = 14))
  colnames(df_hr) <- c("term", "estimate", "conf.low", "conf.high", "std.error", "robust.se", "covariate", "P","model",
                       "subgroup", "event", "cohort", "time_period", "total_covid19_cases")
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
      mutate(events_total = replace(events_total, events_total <=5, "[Redacted]"))
    
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
  df_event_counts <- as.data.frame(matrix(ncol = 7))
  colnames(df_event_counts)<- c("expo_week", "events_total", "event", "subgroup", "cohort", "time_points","redacted_results")
  write.csv(df_event_counts, paste0(output_dir,"/compiled_event_counts_", event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Compiled event counts saved: ", output_dir,"/compiled_event_counts_", event_name,"_", cohort,".csv"))
  write.csv(df_event_counts, paste0(output_dir,"/suppressed_compiled_event_counts_", event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Supressed event counts saved: ", output_dir,"/suppressed_compiled_event_counts_", event_name,"_", cohort,".csv"))
  
}

#=========================COMBINE EVENT COUNTS AND HRS==========================

if(length(results_done)>0){
  event_counts_to_left_join=data.frame(matrix(nrow=0,ncol=8))
  colnames(event_counts_to_left_join)=c("term","subgroup","event","expo_week","events_total","cohort","time_points","model")
  
  
  for(i in 1:nrow(analyses_to_run)){
    subgroup_of_interest=analyses_to_run$subgroup[i]
    cohort_of_interest=analyses_to_run$cohort[i]
    time_points_of_interest=analyses_to_run$reduced_timepoint[i]
    
    for(model_of_interest in unique(df_hr$model)){
      df_hr_subgroup=df_hr%>%filter(subgroup==subgroup_of_interest & cohort == cohort_of_interest & time_points == time_points_of_interest, model==model_of_interest)
      df_counts_subgroup=supressed_df_event_counts%>%filter(subgroup==subgroup_of_interest & cohort == cohort_of_interest & time_points == time_points_of_interest)
      df_hr_subgroup=df_hr_subgroup[1:nrow(df_counts_subgroup),]
      df_hr_subgroup$expo_week=df_counts_subgroup$expo_week
      df_hr_subgroup$events_total=df_counts_subgroup$events_total
      df_hr_subgroup=df_hr_subgroup%>%select(term,subgroup,event,expo_week,events_total,cohort,time_points,model)
      event_counts_to_left_join=rbind(event_counts_to_left_join,df_hr_subgroup)
    }
  }
    
  combined_hr_event_counts=df_hr%>%left_join(event_counts_to_left_join, by=c("term","event","subgroup","cohort","time_points","model"))
  
  combined_hr_event_counts=combined_hr_event_counts%>%select(term,estimate,conf.low,conf.high,std.error,robust.se,expo_week,events_total,
                                                               event,subgroup,model,cohort,time_points,total_covid19_cases,results_fitted,covariates_removed,cat_covars_collapsed,covariates_fitted)
  

  # Add in suppression for counts <=5
  combined_hr_event_counts$redacted_results <- "NA"

  supressed_combined_hr_event_counts <- combined_hr_event_counts[0,]

  for(i in 1:nrow(analyses_to_run)){
    subgroup_of_interest=analyses_to_run$subgroup[i]
    cohort_of_interest=analyses_to_run$cohort[i]
    time_points_of_interest=analyses_to_run$reduced_timepoint[i]
    for(model_of_interest in unique(combined_hr_event_counts$model)){
      tmp <- combined_hr_event_counts %>% filter(subgroup==subgroup_of_interest & cohort == cohort_of_interest & time_points == time_points_of_interest, model==model_of_interest)
      tmp <- tmp %>% mutate(across(where(is.numeric), as.character))
      redacted_counts <- tmp[which(tmp$events_total == "[Redacted]"),expo_week]
      tmp[which(tmp$term %in% redacted_counts),2:6] = "[Redacted]"
      tmp$redacted_results <- ifelse(any(tmp$events_total == "[Redacted]", na.rm = T), "Redacted results", "No redacted results")
      supressed_combined_hr_event_counts <- rbind(supressed_combined_hr_event_counts,tmp)
    }
  }

  supressed_combined_hr_event_counts$redacted_results <- factor(supressed_combined_hr_event_counts$redacted_results, levels = c("Redacted results",
                                                                                                                                "No redacted results"))
  supressed_combined_hr_event_counts <- supressed_combined_hr_event_counts[order(supressed_combined_hr_event_counts$redacted_results),]

  
  write.csv(supressed_combined_hr_event_counts,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Supressed HR with event counts saved: ", output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,".csv"))
  
  supressed_combined_hr_event_counts <- supressed_combined_hr_event_counts %>% select(!c("expo_week","events_total"))
  write.csv(supressed_combined_hr_event_counts,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,"_to_release.csv") , row.names=F)
  
}else{
  supressed_combined_hr_event_counts <- as.data.frame(matrix(ncol = 16))
  colnames(supressed_combined_hr_event_counts) <- c("term","estimate","conf.low","conf.high","std.error","robust.se","P","expo_week","events_total",
                                                    "event","subgroup","model","cohort","time_points,","total_covid19_cases","redacted_results")
  write.csv(supressed_combined_hr_event_counts,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,".csv") , row.names=F)
  print(paste0("Supressed HR with event counts saved: ", output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,".csv"))
  
  supressed_combined_hr_event_counts <- supressed_combined_hr_event_counts[!colnames(supressed_combined_hr_event_counts) %in% c("expo_week","events_total")]
  write.csv(supressed_combined_hr_event_counts,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name,"_", cohort,"_to_release.csv") , row.names=F) 
}

