## =============================================================================
## FORMATS RESULTS from RSTUDIO
##
## Author: Samantha Ip
## =============================================================================

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


if(length(event_count_done)>0){
  event_counts_completed <- pmap(list(event_count_done), 
                                 function(fpath){ 
                                   df <- fread(fpath) 
                                   return(df)
                                 })
  
  
  
  df_event_counts <- rbindlist(event_counts_completed, fill=TRUE)  %>% dplyr::select(!"V1")
  write.csv(df_event_counts, paste0(output_dir,"/compiled_event_counts_", event_name, ".csv") , row.names=F)
  
}else if(length(event_count_done)==0){
  no_event_counts=data.frame(matrix(nrow = 1,ncol = 1))
  colnames(no_event_counts)="no_results"
  write.csv(no_event_counts,paste0(output_dir,"/compiled_event_counts_",event_name, ".csv") , row.names=F)
}  


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
        df_counts_subgroup=df_event_counts%>%filter(subgroup==i & cohort == j & model == k)
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
                                                               event,subgroup,model,cohort,covariates_removed,cat_covars_collapsed,total_covid19_cases)
  
  write.csv(combined_hr_event_counts,paste0(output_dir,"/compiled_HR_results_",event_name ,".csv") , row.names=F)
  
}else if(length(results_done)==0){
  no_results=data.frame(matrix(nrow = 1,ncol = 1))
  colnames(no_results)="no_results"
  write.csv(no_results,paste0(output_dir,"/compiled_HR_results_",event_name, ".csv") , row.names=F)
  
}


#==============================ANALYSES NOT RUN=================================
analyses_not_run=data.frame(matrix(nrow=0,ncol = 8))
colnames(analyses_not_run)=c("event","subgroup","cohort","model", "any exposures?", "any exposure events?", "any non exposed?", "more than 400 post exposure events?")

for(cohort in cohort_to_run){
  analyses_not_run=rbind(analyses_not_run,read_csv(paste0("output/analyses_not_run_",event_name,"_",cohort,".csv")))
}

write.csv(analyses_not_run,paste0(output_dir,"/analyses_not_run_",event_name, ".csv") , row.names=F)

