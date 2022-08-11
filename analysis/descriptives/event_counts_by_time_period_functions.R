
subgroup_data <- function(event,subgroup,stratify_by_subgroup,stratify_by,time_point,input){
  print(paste0("Working on subgroup: ", subgroup, " ", cohort))
  print(paste0("Using ",time_point," time point"))
  
  #Reduce dataset to those who do NOT have a prior history of COVID 
  survival_data=input%>%filter(sub_bin_covid19_confirmed_history == FALSE)
  
  for(i in c("hospitalised","non_hospitalised")){
    if(stratify_by == i){
      survival_data$follow_up_end <- NULL
      setnames(survival_data, 
               old = c(c(paste0(i,"_follow_up_end")),
                       c(paste0(i,"_censor_date"))),
               
               new = c("follow_up_end",
                       "date_expo_censor"))
    }
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
  

  if(time_point == "reduced"){
    event_counts <- event_counts(event,subgroup,stratify_by_subgroup,stratify_by,survival_data,cuts_days_since_expo_reduced,time_point)
  }else if(time_point == "normal"){
    event_counts <- event_counts(event,subgroup,stratify_by_subgroup,stratify_by, survival_data,cuts_days_since_expo,time_point)
  }else if(time_point == "day_zero"){
    event_counts <- event_counts(event,subgroup,stratify_by_subgroup,stratify_by, survival_data,cuts_days_since_expo_day_zero,time_point)
  }
  
  print(paste0("Finished working on subgroup: ", subgroup, " ", cohort))
}


event_counts <- function(event,subgroup, stratify_by_subgroup, stratify_by, survival_data,cuts_days_since_expo,time_point){
  print(paste0("Starting formatting survival data"))
  
  survival_data$days_to_start <- as.numeric(survival_data$follow_up_start-cohort_start_date)
  survival_data$days_to_end <- as.numeric(survival_data$follow_up_end-cohort_start_date)
  
  if(startsWith(subgroup,"covid_pheno_")){
    survival_data$days_to_end <- ifelse((!is.na(survival_data$date_expo_censor)) & (survival_data$follow_up_end == survival_data$date_expo_censor), survival_data$days_to_end, (survival_data$days_to_end +1 ))
  }else{
    survival_data$days_to_end <- (survival_data$days_to_end +1) 
  }
  
  #===============================================================================
  # WITH COVID
  #-------------------------------------------------------------------------------
  with_expo <- survival_data %>% filter(!is.na(expo_date))
  
  if(startsWith(subgroup,"covid_pheno_")==T){
    with_expo <- with_expo %>% 
      dplyr::select(patient_id, expo_date, follow_up_end, event_date, days_to_start, days_to_end, date_expo_censor) %>%  
      mutate(event_status = if_else( (!is.na(event_date)) & 
                                       (
                                         ((event_date <= follow_up_end) & ((follow_up_end != date_expo_censor) | is.na(date_expo_censor ))) | 
                                           ((event_date < follow_up_end) & (follow_up_end == date_expo_censor)) 
                                       ), 
                                     1, 0))
  }else{
    with_expo <- with_expo %>% 
      dplyr::select(patient_id, expo_date, follow_up_end, event_date, days_to_start, days_to_end) %>%  
      mutate(event_status = if_else( (!is.na(event_date)) 
                                     , 1, 0)) 
    
  }
  
  # ......................................
  # CHUNK UP FOLLOW-UP PERIOD by CHANGE OF STATE OF EXPOSURE
  
  with_expo$day_to_expo <- as.numeric(with_expo$expo_date - cohort_start_date)
  
  d1 <- with_expo %>% dplyr::select(patient_id, expo_date, event_date)
  d2 <- with_expo %>% dplyr::select(patient_id, days_to_start, day_to_expo, days_to_end, event_status)
  with_expo <- tmerge(data1=d1, data2=d2, id=patient_id,
                      event=event(days_to_end, event_status), tstart=days_to_start, tstop = days_to_end,
                      expo=tdc(day_to_expo)) 
  
  # with_expo <- with_expo %>% dplyr::select(!id)
  with_expo$id <- NULL
  
  if(startsWith(subgroup,"covid_pheno_")){
    rm(list=c("d1", "d2"))
  }else{
    rm(list=c("d1", "d2"))
  }
  
  
  # ----------------------- SPLIT POST-COVID TIME------------------------------
  with_expo_postexpo <- with_expo %>% filter(expo==1)
  
  with_expo_postexpo <- with_expo_postexpo %>% rename(t0=tstart, t=tstop) %>% mutate(tstart=0, tstop=t-t0)
  
  
  with_expo_postexpo <- survSplit(Surv(tstop, event)~., 
                                  with_expo_postexpo,
                                  cut=cuts_days_since_expo,
                                  episode="days_cat"
  )
  
  with_expo_postexpo <- with_expo_postexpo %>% mutate(tstart=tstart+t0, tstop=tstop+t0) %>% dplyr::select(-c(t0,t))
  
  # ................... CONCAT BACK PRE-COVID TIME...................
  with_expo_preexpo <- with_expo %>% filter(expo==0)
  with_expo_preexpo$days_cat <- 0
  ls_with_expo <- list(with_expo_preexpo, with_expo_postexpo)
  with_expo <- do.call(rbind, lapply(ls_with_expo, function(x) x[match(names(ls_with_expo[[1]]), names(x))]))
  
  rm(list=c("ls_with_expo", "with_expo_preexpo", "with_expo_postexpo"))
   
    
  #===============================================================================
  #-   WITHOUT COVID
  #-------------------------------------------------------------------------------
  without_expo <- survival_data %>%filter(is.na(expo_date))
  
  if(startsWith(subgroup,"covid_pheno_")==T){
    without_expo <- without_expo %>% 
      dplyr::select(patient_id, expo_date, follow_up_end, event_date, days_to_start, days_to_end, date_expo_censor) %>%  
      mutate(event = if_else( (!is.na(event_date)) & 
                                (
                                  ((event_date <= follow_up_end) & ((follow_up_end != date_expo_censor) | is.na(date_expo_censor ))) | 
                                    ((event_date < follow_up_end) & (follow_up_end == date_expo_censor)) 
                                ), 
                              1, 0))
  }else{
    without_expo <- without_expo %>%
      dplyr::select(patient_id, expo_date, follow_up_end, event_date, days_to_start, days_to_end) %>% 
      mutate(event = if_else( (!is.na(event_date)), 
                              1, 0))
  }
  
  # ......................................
  
  without_expo$tstart<- without_expo$days_to_start
  without_expo$tstop <- without_expo$days_to_end
  without_expo$expo<- c(0)
  without_expo$days_cat <- c(0)
  
  #===============================================================================
  #-   RBIND WITH & WITHOUT COVID
  #-------------------------------------------------------------------------------
  common_cols <- intersect(colnames(without_expo), colnames(with_expo))
  without_expo <- without_expo %>% dplyr::select(all_of(common_cols))
  with_expo <- with_expo %>% dplyr::select(all_of(common_cols))
  data_surv <-rbind(without_expo, with_expo)
  
  # Define episode labels --------------------------------------------------------
  episode_labels <- data.frame(days_cat = 0:length(cuts_days_since_expo),
                               expo_week = c("days_pre",paste0("days", c("0", cuts_days_since_expo[1:(length(cuts_days_since_expo)-1)]),"_", cuts_days_since_expo)),
                               stringsAsFactors = FALSE)
  
  # Add indicators for episode -------------------------------------------------
  for (i in 1:max(episode_labels$days_cat)) {
    
    preserve_cols <- colnames(data_surv) 
    
    data_surv$tmp <- as.numeric(data_surv$days_cat==i)
    
    colnames(data_surv) <- c(preserve_cols,episode_labels[episode_labels$days_cat==i,]$expo_week)
    
  }
  
  # Calculate number of events per episode -------------------------------------
  
  events <- data_surv[data_surv$event=="1", c("patient_id","days_cat")]
  
  events <- aggregate(days_cat ~ patient_id, data = events, FUN = max)
  
  events <- data.frame(table(events$days_cat), 
                       stringsAsFactors = FALSE)
  
  events <- dplyr::rename(events, "days_cat" = "Var1", "events_total" = "Freq")
  
  # Add number of events to episode info table ---------------------------------
  
  episode_info <- merge(episode_labels, events, by = "days_cat", all.x = TRUE)
  episode_info$events_total <- ifelse(is.na(episode_info$events_total),0,episode_info$events_total)
  episode_info[nrow(episode_info) + 1,] = c(max(episode_info$days_cat)+1,"all post expo",  sum(episode_info[which(episode_info$days_cat != 0),"events_total"]))
  
  # Calculate person-time in each episode --------------------------------------
  
  tmp <- data_surv[,c("days_cat","tstart","tstop")]
  tmp$person_time <- (tmp$tstop - tmp$tstart)
  tmp <- rbind(tmp,tmp %>% filter(days_cat !=0) %>% mutate(days_cat = max(episode_info$days_cat)))
  tmp[,c("tstart","tstop")] <- NULL
  tmp <- aggregate(person_time ~ days_cat, data = tmp, FUN = sum)
  
  episode_info <- merge(episode_info, tmp, by = "days_cat", all.x = TRUE)
  
  # Calculate incidence ------------------------------------------------------
  episode_info <- episode_info %>% mutate(across(c(person_time,events_total),as.numeric))
  episode_info <- episode_info %>% mutate("incidence rate (per 1000 person years)" = (events_total/(person_time/365.2))*1000 )
  episode_info$days_cat <- NULL
  
  episode_info$event <- event
  episode_info$subgroup <- subgroup
  episode_info$cohort <- cohort
  episode_info$time_points <- time_point
  
  episode_info <- episode_info %>% select(event,cohort,subgroup,time_points,expo_week,events_total,person_time,`incidence rate (per 1000 person years)`)
  
  write.csv(episode_info, paste0("output/tbl_event_count_" ,event,"_", subgroup,"_", cohort,"_", time_point,"_time_periods.csv"), row.names = T)
  print(paste0("Event counts saved: output/tbl_event_count_" ,event,"_", subgroup,"_", cohort,"_", time_point,"_time_periods.csv"))

}

