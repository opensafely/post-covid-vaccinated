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
  
  write.csv(df_hr, paste0(output_dir,"/compiled_HR_results_",save_name ,"_",project,"_", mdl,"_",covid_history, ".csv"), row.names = F)
  
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
                                   df$event <- event
                                   df$subgroup <- save_name
                                   df$strata <- which_strata
                                   df$project <- project
                                   df$mdl <- mdl
                                   df$covid_history <- covid_history
                                   return(df)
                                 })
  
  
  
  df_hr <- rbindlist(event_counts_completed, fill=TRUE)  %>% dplyr::select(!"V1")
  
  write.csv(df_hr, paste0(output_dir,"/compiled_event_counts_",save_name ,"_",project,"_", mdl,"_",covid_history, ".csv") , row.names=F)
  
}




