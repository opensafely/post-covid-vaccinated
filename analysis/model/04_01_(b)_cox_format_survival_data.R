## =============================================================================
## 1. Format the survival data for the cox model
## 2. Calculate pre/post exposure event counts
## =============================================================================

fit_get_data_surv <- function(event,subgroup, stratify_by_subgroup, stratify_by, survival_data,cuts_days_since_expo,time_point){
  print(paste0("Starting survival data"))
  #------------------ RANDOM SAMPLE NON-CASES for IP WEIGHING ------------------
  set.seed(137)
  
  print(paste0("Total number in survival data: ", nrow(survival_data)))
  
  if(nrow(survival_data) > 4000000){
    print("Sample size above 4 million - sampling starting")
    
    # Select all event cases as controls per case is determined by the number of events
    
    cases <- survival_data %>% filter((!is.na(event_date)) & 
                                        (
                                          event_date == follow_up_end
                                        ))

    print(paste0("Number of cases: ", nrow(cases)))
    
    controls_per_case <- ifelse(nrow(cases)<100000,20,ifelse(nrow(cases)<500000,10,5))
    
    print(paste0("Number of controls per case: ", controls_per_case))
    
    # All cases and all exposed to COVID-19 are included in the data set. Non-cases-non-exposed are randomly sampled.
    
    non_cases_exposed <- survival_data %>% filter((!patient_id %in% cases$patient_id) & (!is.na(expo_date)))
    non_cases_unexposed <- survival_data %>% filter((!patient_id %in% cases$patient_id) & (is.na(expo_date)))
    
    # If number of cases * controls per cases > non-cases-non-exposed then use all of non-cases-non-exposed
    # otherwise randomly sample non-cases-non-exposed
    
    if(nrow(cases)*controls_per_case < nrow(non_cases_unexposed)){
      non_cases_unexposed <- non_cases_unexposed[sample(1:nrow(non_cases_unexposed), nrow(cases)*controls_per_case,replace=FALSE), ]
      print("Non-cases sampled")
    }else if (nrow(cases)*controls_per_case >= nrow(non_cases_unexposed)){
      non_cases_unexposed=non_cases_unexposed
      print("Non-cases not sampled - all non-cases used")
    }
    
    # Weight of those who are sampled, used in cox model to weight the non_cases_unexposed so that 'whole' population is analysed
    non_case_inverse_weight=(nrow(survival_data)-nrow(cases)-nrow(non_cases_exposed))/nrow(non_cases_unexposed)
    survival_data <- bind_rows(cases,non_cases_exposed,non_cases_unexposed)
    
    noncase_ids <- unique(non_cases_unexposed$patient_id)
    
    print(paste0("Number of controls (exposed): ", nrow(non_cases_exposed)))
    print(paste0("Number of controls (non exposed): ", nrow(non_cases_unexposed)))
    print(paste0("Controls (non exposed) weight: ", non_case_inverse_weight))
    
    #Add inverse probability weights for non-cases
    survival_data$cox_weights <- ifelse(survival_data$patient_id %in% noncase_ids, non_case_inverse_weight, 1)
  }else{
    print("Sample size below 4 million - no sampling")
    # No sampling therefore cox weight of 1
    non_case_inverse_weight <- 1
    survival_data$cox_weights <- 1
    
  }
  
  # The sampled df is used in stata to run hospitalised analyses
  sampled_data <- as.data.frame(survival_data)
  
  survival_data$days_to_start <- as.numeric(survival_data$follow_up_start-cohort_start_date)
  survival_data$days_to_end <- as.numeric(survival_data$follow_up_end-cohort_start_date)
  
  # Add one to follow up as this allows people the day to have an event and ensures that days of follow up are correct
  # eg as.Date("2020-01-01) - as.Date("2020-01-01) gives zero days but we want this as one day of follow-up 
  
  survival_data$days_to_end <- (survival_data$days_to_end +1)
  
  #===============================================================================
  #   CACHE some features
  #-------------------------------------------------------------------------------  
  df_sex_cox_weights <- survival_data %>% dplyr::select(patient_id, sex, cox_weights)
  df_age_region_ethnicity <- survival_data %>% dplyr::select(patient_id, AGE_AT_COHORT_START, region_name, ethnicity) %>% rename(age = AGE_AT_COHORT_START)
  df_age_region_ethnicity$age_sq <- df_age_region_ethnicity$age^2
  
  #===============================================================================
  # WITH COVID
  #-------------------------------------------------------------------------------
  with_expo <- survival_data %>% filter(!is.na(expo_date))
  
  # Check whether there are any people with COVID exposure
  any_exposures <- nrow(with_expo)>0
  
  # Check whether there are any people with post-expo events
  any_exposed_events <- nrow(with_expo %>% filter(!is.na(event_date)))>0
  
  if(any_exposures==T & any_exposed_events ==T ){
    with_expo <- with_expo %>% 
      dplyr::select(patient_id, expo_date, follow_up_end, event_date, days_to_start, days_to_end, DATE_OF_DEATH) %>%  
      mutate(event_status = if_else( (!is.na(event_date)) 
                                     , 1, 0)) 
    
    # ......................................
    # CHUNK UP FOLLOW-UP PERIOD by CHANGE OF STATE OF EXPOSURE
   
    with_expo$day_to_expo <- as.numeric(with_expo$expo_date - cohort_start_date)
    
    d1 <- with_expo %>% dplyr::select(patient_id, expo_date, event_date, DATE_OF_DEATH)
    d2 <- with_expo %>% dplyr::select(patient_id, days_to_start, day_to_expo, days_to_end, event_status)
    with_expo <- tmerge(data1=d1, data2=d2, id=patient_id,
                        event=event(days_to_end, event_status), tstart=days_to_start, tstop = days_to_end,
                        expo=tdc(day_to_expo)) 
    
    # with_expo <- with_expo %>% dplyr::select(!id)
    with_expo$id <- NULL

    
    rm(list=c("d1", "d2"))
   

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
    
    with_expo  <- with_expo %>%
      group_by(patient_id) %>% arrange(days_cat) 

  }
  
  
  #===============================================================================
  #-   WITHOUT COVID
  #-------------------------------------------------------------------------------
  without_expo <- survival_data %>%filter(is.na(expo_date)) 
  any_no_expo <- nrow(with_expo)>0
  
  if(any_no_expo == T & any_exposures== T & any_exposed_events == T ){
    without_expo <- without_expo %>%
      dplyr::select(patient_id, expo_date, follow_up_end, event_date, days_to_start, days_to_end, DATE_OF_DEATH) %>% 
      mutate(event = if_else( (!is.na(event_date)), 
                              1, 0))
    
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
    
    interval_names <- episode_labels$expo_week[episode_labels$expo_week != "days_pre"]
    
    # Add indicators for episode -------------------------------------------------
    for (i in 1:max(episode_labels$days_cat)) {
      
      preserve_cols <- colnames(data_surv) 
      
      data_surv$tmp <- as.numeric(data_surv$days_cat==i)
      
      colnames(data_surv) <- c(preserve_cols,episode_labels[episode_labels$days_cat==i,]$expo_week)
      
    }
    
    
    #===============================================================================
    # FINALIZE age, region, data_surv
    #-------------------------------------------------------------------------------
    data_surv <- data_surv %>% left_join(df_age_region_ethnicity)
    data_surv <- data_surv %>% left_join(df_sex_cox_weights)
    print(paste0("Finished survival data"))
    
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
    
    tmp <- data_surv[,c("days_cat","tstart","tstop","cox_weights")]
    tmp$person_days_follow_up <- (tmp$tstop - tmp$tstart)*tmp$cox_weights
    tmp <- rbind(tmp,tmp %>% filter(days_cat !=0) %>% mutate(days_cat = max(episode_info$days_cat)))
    tmp[,c("tstart","tstop","cox_weights")] <- NULL
    tmp <- aggregate(person_days_follow_up ~ days_cat, data = tmp, FUN = sum)
    
    episode_info <- merge(episode_info, tmp, by = "days_cat", all.x = TRUE)
    
    # Calculate incidence ------------------------------------------------------
    episode_info <- episode_info %>% mutate(across(c(person_days_follow_up,events_total),as.numeric))
    episode_info <- episode_info %>% mutate("incidence rate (per 1000 person years)" = (events_total/(person_days_follow_up/365.2))*1000 )
    
    # Calculate median person-time -----------------------------------------------
    
    #Median person time of those with an event within each time period used for plotting figures.
    
    tmp <- data_surv[data_surv$event ==1,c("patient_id","days_cat","tstart","tstop")]
    tmp$person_time <- tmp$tstop - tmp$tstart
    
    tmp[,c("patient_id","tstart","tstop")] <- NULL
    
    tmp <- tmp %>% group_by(days_cat) %>% 
      summarise(median_follow_up = median(person_time, na.rm = TRUE))
    
    episode_info <- merge(episode_info, tmp, by = "days_cat", all.x = TRUE)
    episode_info $days_cat <- NULL
    
    episode_info$event <- event
    episode_info$subgroup <- subgroup
    episode_info$cohort <- cohort
    episode_info$time_points <- time_point
    episode_info$events_total <- as.numeric(episode_info$events_total)
    
    print(episode_info)
    
    write.csv(episode_info, paste0(output_dir,"/tbl_event_count_" ,event,"_", subgroup,"_",cohort,"_",time_point,"_time_periods.csv"), row.names = F)
    print(paste0("Event counts saved: ", output_dir,"/tbl_event_count_" ,event,"_", subgroup,"_",cohort,"_",time_point,"_time_periods.csv"))
    
    
    return(list(data_surv,interval_names, non_case_inverse_weight, sampled_data))
    
  }else{
    analyses_not_run[nrow(analyses_not_run)+1,]<- c(event,subgroup,cohort,any_exposures,any_exposed_events,any_no_expo,"FALSE")
    
    return(list(analyses_not_run))
  }
  
 
}
