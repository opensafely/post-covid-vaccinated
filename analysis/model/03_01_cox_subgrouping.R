## =============================================================================
## 1.Creates the base survival data that includes the event date for the outcome of interest
## 2.Stratify to relevant subgroup if necessary
## 3.Add follow up start and end dates
## =============================================================================
source(file.path(scripts_dir,"04_01_(a)_cox_fit_model.R"))

get_vacc_res <- function(event,subgroup,stratify_by_subgroup,stratify_by,time_point,input,covar_names,cuts_days_since_expo,cuts_days_since_expo_reduced,cuts_days_since_expo_day_zero,cuts_days_since_expo_reduced_day_zero,cuts_days_since_expo_month1_split,cuts_days_since_expo_reduced_month1_split,mdl){
  print(paste0("Working on subgroup: ", subgroup, " ", cohort))
  print(paste0("Using ",time_point," time point"))
  
  #Reduce dataset to those who do NOT have a prior history of COVID unless running the subgroup
  #analysis for this with a prior history
  
  if(subgroup != "covid_history" ){
    input=input%>%filter(sub_bin_covid19_confirmed_history == FALSE)
  }else {
    input=input%>%filter(sub_bin_covid19_confirmed_history == TRUE)
  }
  
  # Select the relevant cohort columns required to stratify by subgroup if necessary
  if(startsWith(subgroup,"prior_history")){
    survival_data <- input %>% dplyr::select(all_of(cohort_cols),all_of(stratify_by_subgroup))
  }else{
    survival_data <- input %>% dplyr::select(all_of(cohort_cols))
  }

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
  
  # Stratify to the relevant subgroup if either sex/ethnicity/prior history subgroup
  # COVID pheno subgroup is filtered later in this script
  
  for(i in c("ethnicity","sex","prior_history")){
    if(startsWith(subgroup,i)){
      survival_data=survival_data%>%filter_at(stratify_by_subgroup,all_vars(.==stratify_by))
    }
  }
  
  # Filter for age group of interest -------------------------------------------
  
  # If a age group subgroup analysis then use the age subgroup otherwise analyse for all ages
  if(startsWith(subgroup,"agegp") | startsWith(subgroup,"aer")){
    agebreaks=agebreaks_strata
    agelabels=agelabels_strata
  }else{
    agebreaks=agebreaks_all
    agelabels=agelabels_all
  }
  
  # Create new variable agegroup which splits AGE_AT_COHORT_START into the required age groups
  # and give these group the matching age labels
  setDT(survival_data)[ , agegroup := cut(AGE_AT_COHORT_START, 
                                          breaks = agebreaks, 
                                          right = FALSE, 
                                          labels = agelabels)]
  
  
  if(startsWith(subgroup,"agegp_")){
    survival_data=survival_data %>% filter(agegroup== stratify_by)
  }
  
  # Age/sex subgroups for AER
  
  if(startsWith(subgroup,"aer_")){
    aer_subgroup <- sub("aer_","",subgroup)
    aer_subgroup <- sub("_","",aer_subgroup)
    aer_sex <- sub("(\\D+).*", "\\1", aer_subgroup)
    aer_age <-  sub(".*?(\\d+.*)", "\\1", aer_subgroup)
    
    survival_data=survival_data %>% filter(sex == aer_sex & agegroup== aer_age)
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
    
    setDT(survival_data)[follow_up_end == date_expo_censor, follow_up_end := follow_up_end-1]
  }
  
  survival_data=survival_data%>%filter(follow_up_end>=follow_up_start)
  
  # add statement for reduced time cutoffs
  if(time_point == "reduced"){
    res_vacc <- fit_model_reducedcovariates(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo=cuts_days_since_expo_reduced,covar_names,time_point)
  }else if(time_point == "normal"){
    res_vacc <- fit_model_reducedcovariates(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo,covar_names,time_point)
  }else if(time_point == "day_zero_reduced"){
    res_vacc <- fit_model_reducedcovariates(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo=cuts_days_since_expo_reduced_day_zero,covar_names,time_point)
  }else if(time_point == "day_zero_normal"){
    res_vacc <- fit_model_reducedcovariates(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo=cuts_days_since_expo_day_zero,covar_names,time_point)
  }else if(time_point == "month1_split_reduced"){
    res_vacc <- fit_model_reducedcovariates(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo=cuts_days_since_expo_reduced_month1_split,covar_names,time_point)
  }else if(time_point == "month1_split_normal"){
    res_vacc <- fit_model_reducedcovariates(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo=cuts_days_since_expo_month1_split,covar_names,time_point)
  }

  print(paste0("Finished working on subgroup: ", subgroup, ", ",mdl,", ", cohort))
  return(res_vacc)
}
  
    
