## =============================================================================
## FORMATS RESULTS from RSTUDIO
##
## Author: Samantha Ip
## =============================================================================

rm(list=setdiff(ls(), c("mdl","project","output_dir","scripts_dir","ls_events_missing","save_name","covid_history")))


results_needed=ls_events_missing
results_needed$project=project
results_needed$mdl=mdl
results_needed$save_name=save_name
results_needed$covid_history=covid_history

       
result_file_paths <- pmap(list(results_needed$event, results_needed$save_name, results_needed$which_strata,results_needed$project, results_needed$mdl,results_needed$covid_history ),
               function(event, save_name, which_strata, project, mdl,covid_history)
                 file.path(output_dir,
                           paste0("tbl_hr_",
                                  save_name, "_",
                                  which_strata, "_",
                                  event, "_",
                                  project, "_",
                                  mdl,"_",
                                  covid_history,".csv"))
)

results_should_have <- unlist(result_file_paths)
results_done <- c()
results_missing=data.frame()

for (i in 1:nrow(results_needed)) {
  row <- results_needed[i,]
  fpath <- file.path(output_dir,
                     paste0("tbl_hr_",
                            row$save_name, "_",
                            row$which_strata, "_",
                            row$event, "_",
                            row$project, "_",
                            row$mdl,"_",
                            row$covid_history,".csv"))
  
  if (!file.exists(fpath)) {
    results_missing <- rbind(results_missing, row)
  } else {
    results_done <- c(results_done, fpath)
  }
}



if(length(results_missing)>0){
  results_needed <- anti_join(results_needed, results_missing)
}


result_file_paths <- pmap(list(results_done), 
               function(fpath){ 
                 df <- fread(fpath) 
                 return(df)
               })


if(length(results_done)>0){
  df_hr <- rbindlist(result_file_paths, fill=TRUE)
  df_hr <- df_hr %>% mutate_if(is.numeric, round, digits=5)%>%select(-V1)
  
  #write.csv(df_hr, paste0(output_dir,"/compiled_HR_results_",save_name ,"_",project,"_", mdl,"_",covid_history, ".csv"), row.names = F)
  
}


# =============================  R events count =====================================
event_count_file_paths<- pmap(list(results_needed$event,results_needed$save_name, results_needed$which_strata,results_needed$project, results_needed$mdl,results_needed$covid_history),
               function(event, save_name, which_strata, project, mdl, covid_history)
                 file.path(output_dir,
                           paste0("tbl_event_count_",
                                  save_name, "_",
                                  which_strata, "_",
                                  event, "_",
                                  project, "_",
                                  mdl, "_",
                                  covid_history, ".csv")
                 )
)
event_count_should_have <- unlist(event_count_file_paths)

event_count_missing <- data.frame()
event_count_done <- c()
for (i in 1:nrow(results_needed)) {
  row <- results_needed[i,]
  fpath <- file.path(output_dir,
                     paste0("tbl_event_count_",
                            row$save_name, "_",
                            row$which_strata, "_",
                            row$event, "_",
                            row$project, "_",
                            row$mdl, "_",
                            row$covid_history, ".csv"))
  
  if (!file.exists(fpath)) {
    event_count_missing <- rbind(event_count_missing, row)
  } else {
    event_count_done <- c(event_count_done, fpath)
  }
}


if(length(event_count_done)>0){
  #  fread completed ones
  event_counts_completed <- pmap(list(event_count_done, results_needed$event, results_needed$save_name, results_needed$which_strata,results_needed$project, results_needed$mdl,results_needed$covid_history), 
                                 function(fpath, event, save_name, which_strata, project, mdl,covid_history){ 
                                   df <- fread(fpath) 
                                   return(df)
                                 })
  
  
  
  df_event_counts <- rbindlist(event_counts_completed, fill=TRUE)  %>% dplyr::select(!"V1")
  write.csv(df_event_counts, paste0(output_dir,"/compiled_event_counts_",save_name ,"_",project,"_", mdl,"_",covid_history, ".csv") , row.names=F)
  
}

if(length(results_done)>0){
  #not sure if there's an easier way to join all the event counts on?
  outcomes=unique(df_hr$event)
  event_counts_to_left_join=data.frame(matrix(nrow=0,ncol=5))
  colnames(event_counts_to_left_join)=c("term","strata","event","expo_week","events_total")
  
  for(event_name in outcomes){
    df_hr_event=df_hr%>%filter(event==event_name)
    subgroup=unique(df_hr_event$strata)
    for(subgroup_name in subgroup){
      df_hr_event_subgroup=df_hr_event%>%filter(strata==subgroup_name)
      df_counts_subgroup=df_event_counts%>%filter(event==event_name & strata==subgroup_name)
      df_hr_event_subgroup=df_hr_event_subgroup[1:nrow(df_counts_subgroup),]
      df_hr_event_subgroup$expo_week=df_counts_subgroup$expo_week
      df_hr_event_subgroup$events_total=df_counts_subgroup$events_total
      df_hr_event_subgroup=df_hr_event_subgroup%>%select(term,strata,event,expo_week,events_total)
      event_counts_to_left_join=rbind(event_counts_to_left_join,df_hr_event_subgroup)
    }
  }
  
  combined_hr_event_counts=df_hr%>%left_join(event_counts_to_left_join, by=c("term","event","strata"))
  
  
  
  if(mdl=="mdl_max_adj"){
    combined_hr_event_counts=combined_hr_event_counts%>%select(term,estimate,conf.low,conf.high,std.error,robust.se,P,expo_week,events_total,
                                                               event,strata,project,model,covid_history,covariates_removed,cat_covars_collapsed)
  }else{
    combined_hr_event_counts=combined_hr_event_counts%>%select(term,estimate,conf.low,conf.high,std.error,robust.se,P,expo_week,events_total,
                                                               event,strata,project,model,covid_history)
    
  }
  
  write.csv(combined_hr_event_counts,paste0(output_dir,"/compiled_HR_results_",save_name ,"_",project,"_", mdl,"_",covid_history, ".csv") , row.names=F)
  
}else if(length(results_done)==0){
  no_results=data.frame(matrix(nrow = 1,ncol = 1))
  colnames(no_results)="no_results"
  write.csv(no_results,paste0(output_dir,"/compiled_HR_results_",save_name ,"_",project,"_", mdl,"_",covid_history, ".csv") , row.names=F)
  
}
