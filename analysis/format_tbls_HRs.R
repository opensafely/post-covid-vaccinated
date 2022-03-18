## =============================================================================
## Format results into combined HR and event counts files
## =============================================================================

print("Working on formating tables")

rm(list=setdiff(ls(), c("cohort_to_run","mdl","output_dir","scripts_dir","analyses_to_run","event_name")))

results_needed=analyses_to_run
       
result_file_paths <- pmap(list(results_needed$event, results_needed$subgroup,results_needed$cohort_to_run, results_needed$mdl),
               function(event, subgroup, cohort, mdl)
                 file.path(output_dir,
                           paste0("tbl_hr_",
                                  event, "_",
                                  subgroup, "_",
                                  cohort, "_",
                                  mdl,".csv"))
)

results_should_have <- unlist(result_file_paths)
results_done <- c()
results_missing=data.frame()

for (i in 1:nrow(results_needed)) {
  row <- results_needed[i,]
  fpath <- file.path(output_dir,
                     paste0("tbl_hr_",
                            row$event, "_",
                            row$subgroup, "_",
                            row$cohort_to_run, "_",
                            row$mdl,".csv"))
  
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
  write.csv(df_hr, paste0(output_dir,"/compiled_HR_results_", event_name, ".csv") , row.names=F)
  print(paste0("Compiled HR's saved: ", output_dir,"/compiled_HR_results_", event_name, ".csv"))
}else{
  df_hr <- as.data.frame(matrix(ncol = 13))
  colnames(df_hr) <- c("term", "estimate", "conf.low", "conf.high", "std.error", "robust.se", "covariate", "P", "subgroup", "event",
                       "cohort", "model", "total_covid19_cases")
  write.csv(df_hr, paste0(output_dir,"/compiled_HR_results_", event_name, ".csv") , row.names=F)
  print(paste0("Compiled HR's saved: ", output_dir,"/compiled_HR_results_", event_name, ".csv"))
}


# =============================  R events count ================================
event_count_file_paths<- pmap(list(results_needed$event, results_needed$subgroup,results_needed$cohort_to_run, results_needed$mdl),
               function(event, subgroup, cohort, mdl)
                 file.path(output_dir,
                           paste0("tbl_event_count_",
                                  event, "_",
                                  subgroup, "_",
                                  cohort, "_",
                                  mdl,"_", ".csv")
                 )
)
event_count_should_have <- unlist(event_count_file_paths)

event_count_missing <- data.frame()
event_count_done <- c()

for (i in 1:nrow(results_needed)) {
  row <- results_needed[i,]
  fpath <- file.path(output_dir,
                     paste0("tbl_event_count_",
                            row$event, "_",
                            row$subgroup, "_",
                            row$cohort_to_run, "_",
                            row$mdl,".csv"))
  
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
  write.csv(df_event_counts, paste0(output_dir,"/compiled_event_counts_", event_name, ".csv") , row.names=F)
  print(paste0("Compiled event counts saved: ", output_dir,"/compiled_event_counts_", event_name, ".csv"))
  
  # Add in suppression for counts <=5
  df_event_counts$redacted_results <- "NA"
  
  subgroup <- unique(df_event_counts$subgroup)
  cohort <- unique(df_event_counts$cohort)
  model <- unique(df_event_counts$model)
  
  supressed_df_event_counts <- df_event_counts[0,]
  
  for (i in subgroup){
    for (j in cohort ){
      for (k in model){
        tmp <- df_event_counts %>% filter(subgroup == i & cohort == j & model == k)
        tmp$events_total <- as.numeric(tmp$events_total)
        tmp <- tmp %>% 
          mutate(events_total = replace(events_total, expo_week=="all post expo", sum(tmp[which(tmp$events_total >5 & !(tmp$expo_week %in% c("pre expo", "all post expo"))),events_total])))
        tmp <- tmp %>% 
          mutate(events_total = replace(events_total, events_total <=5, "[Redacted]"))
        tmp$events_total <- as.character(tmp$events_total)
        tmp$redacted_results <- ifelse(any(tmp$events_total == "[Redacted]", na.rm = T), "Redacted results", "No redacted results")
        supressed_df_event_counts <- rbind(supressed_df_event_counts,tmp)
        
      }
    }
  }
  
  supressed_df_event_counts$redacted_results <- factor(supressed_df_event_counts$redacted_results, levels = c("Redacted results",
                                                                                                              "No redacted results"))
  supressed_df_event_counts <- supressed_df_event_counts[order(supressed_df_event_counts$redacted_results),]
  
  write.csv(supressed_df_event_counts, paste0(output_dir,"/suppressed_compiled_event_counts_", event_name, ".csv") , row.names=F)
  print(paste0("Supressed event counts saved: ", output_dir,"/suppressed_compiled_event_counts_", event_name, ".csv"))
  
}else{
  df_event_counts <- as.data.frame(matrix(ncol = 6))
  colnames(df_event_counts)<- c("expo_week", "events_total", "event", "subgroup", "cohort", "model")
  write.csv(df_event_counts, paste0(output_dir,"/compiled_event_counts_", event_name, ".csv") , row.names=F)
  print(paste0("Compiled event counts saved: ", output_dir,"/compiled_event_counts_", event_name, ".csv"))
  write.csv(df_event_counts, paste0(output_dir,"/suppressed_compiled_event_counts_", event_name, ".csv") , row.names=F)
  print(paste0("Supressed event counts saved: ", output_dir,"/suppressed_compiled_event_counts_", event_name, ".csv"))
  
}

rmarkdown::render("analysis/compiled_event_counts.Rmd",output_file=paste0("/suppressed_compiled_event_counts_",event_name),output_dir="output")


#=========================COMBINE EVENT COUNTS AND HRS==========================

if(length(results_done)>0){
  event_counts_to_left_join=data.frame(matrix(nrow=0,ncol=7))
  colnames(event_counts_to_left_join)=c("term","subgroup","event","expo_week","events_total","cohort","model")
  subgroup=unique(df_hr$subgroup)
  cohort=unique(df_hr$cohort)
  model=unique(df_hr$model)
  
  for(i in subgroup){
    for(j in cohort){
      for(k in model){
        df_hr_subgroup=df_hr%>%filter(subgroup==i & cohort == j & model == k)
        df_counts_subgroup=supressed_df_event_counts%>%filter(subgroup==i & cohort == j & model == k)
        df_hr_subgroup=df_hr_subgroup[1:nrow(df_counts_subgroup),]
        df_hr_subgroup$expo_week=df_counts_subgroup$expo_week
        df_hr_subgroup$events_total=df_counts_subgroup$events_total
        df_hr_subgroup=df_hr_subgroup%>%select(term,subgroup,event,expo_week,events_total,cohort,model)
        event_counts_to_left_join=rbind(event_counts_to_left_join,df_hr_subgroup)
      }
    }
  }
  
  combined_hr_event_counts=df_hr%>%left_join(event_counts_to_left_join, by=c("term","event","subgroup","cohort","model"))
  
  combined_hr_event_counts=combined_hr_event_counts%>%select(term,estimate,conf.low,conf.high,std.error,robust.se,P,expo_week,events_total,
                                                             event,subgroup,model,cohort,total_covid19_cases)
  
  # Add in suppression for counts <=5
  combined_hr_event_counts$redacted_results <- "NA"
  
  subgroup <- unique(combined_hr_event_counts$subgroup)
  cohort <- unique(combined_hr_event_counts$cohort)
  model <- unique(combined_hr_event_counts$model)
  
  supressed_combined_hr_event_counts <- combined_hr_event_counts[0,]
  
  for (i in subgroup){
    for (j in cohort ){
      for (k in model){
        tmp <- combined_hr_event_counts %>% filter(subgroup == i & cohort == j & model == k)
        tmp <- tmp %>% mutate(across(where(is.numeric), as.character))
        redacted_counts <- tmp[which(tmp$events_total == "[Redacted]"),expo_week]
        tmp[which(tmp$term %in% redacted_counts),2:7] = "[Redacted]"
        tmp$redacted_results <- ifelse(any(tmp$events_total == "[Redacted]", na.rm = T), "Redacted results", "No redacted results")
        supressed_combined_hr_event_counts <- rbind(supressed_combined_hr_event_counts,tmp)
      }
    }
  }
  
  supressed_combined_hr_event_counts$redacted_results <- factor(supressed_combined_hr_event_counts$redacted_results, levels = c("Redacted results",
                                                                                                                                "No redacted results"))
  supressed_combined_hr_event_counts <- supressed_combined_hr_event_counts[order(supressed_combined_hr_event_counts$redacted_results),]
  
  write.csv(supressed_combined_hr_event_counts,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name ,".csv") , row.names=F)
  print(paste0("Supressed HR with event counts saved: ", output_dir,"/suppressed_compiled_HR_results_",event_name ,".csv"))
  
}else{
  supressed_combined_hr_event_counts <- as.data.frame(matrix(ncol = 15))
  colnames(supressed_combined_hr_event_counts) <- c("term","estimate","conf.low","conf.high","std.error","robust.se","P","expo_week","events_total",
                                                    "event","subgroup","model","cohort","total_covid19_cases")
  write.csv(supressed_combined_hr_event_counts,paste0(output_dir,"/suppressed_compiled_HR_results_",event_name ,".csv") , row.names=F)
  print(paste0("Supressed HR with event counts saved: ", output_dir,"/suppressed_compiled_HR_results_",event_name ,".csv"))
}
rmarkdown::render("analysis/compiled_HR_results.Rmd",output_file=paste0("/suppressed_compiled_HR_results_",event_name),output_dir="output")

#==============================ANALYSES NOT RUN=================================
analyses_not_run=data.frame(matrix(nrow=0,ncol = 8))
colnames(analyses_not_run)=c("event","subgroup","cohort","model", "any exposures?", "any exposure events?", "any non exposed?", "more than 400 post exposure events?")

for(cohort in cohort_to_run){
  analyses_not_run=rbind(analyses_not_run,read_csv(paste0("output/analyses_not_run_",event_name,"_",cohort,".csv")))
}

write.csv(analyses_not_run,paste0(output_dir,"/analyses_not_run_",event_name, ".csv") , row.names=F)
print(paste0("Analyses not run saved: ", output_dir,"/analyses_not_run_",event_name, ".csv"))


