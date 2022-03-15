## =============================================================================
## 1.Creates the base survival data that includes the event date for the outcome of interest
## 2.Stratify to relevant subgroup if necessary
## 3.Add follow up start and end dates
## =============================================================================
source(file.path(scripts_dir,"fit_model.R"))

get_vacc_res <- function(event,subgroup,stratify_by_subgroup,stratify_by,mdl,input,cuts_days_since_expo,cuts_days_since_expo_reduced,covar_names){
  print(paste0("Working on subgroup: ", subgroup, ", ",mdl,", ", cohort))
  
  #Reduce dataset to those who do NOT have a prior history of COVID unless running the subgroup
  #analysis for this with a prior history
  
  if(subgroup != "covid_history" ){
    input=input%>%filter(sub_bin_covid19_confirmed_history == FALSE)
  }else {
    input=input%>%filter(sub_bin_covid19_confirmed_history == TRUE)
  }
  
  
  # read in event dates for outcome-of-interest
  outcomes <-input%>%dplyr::select(c("patient_id", 
                              paste0("out_date_", event)))
  
  # wrangle columns for naming convention 
  setnames(outcomes, 
           old = c(paste0("out_date_", event)), 
           new = c("event_date"))
  outcomes$name <- event
  
  # Select the relevant cohort columns required to stratify by subgroup if necessary
  if(startsWith(subgroup,"prior_history")){
    survival_data <- input %>% dplyr::select(all_of(cohort_cols),all_of(stratify_by_subgroup))
  }else{
    survival_data <- input %>% dplyr::select(all_of(cohort_cols))
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
  if(startsWith(subgroup,"agegp")){
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
  
  # join core data with outcomes
  survival_data <- survival_data %>% left_join(outcomes)

  # add follow up end dates
  
  if(cohort=="vaccinated"){
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(event_date, DATE_OF_DEATH,cohort_end_date,na.rm = TRUE))
  }else if(cohort=="electively_unvaccinated"){
    survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(vax_date_covid_1,event_date, DATE_OF_DEATH,cohort_end_date,na.rm = TRUE))
    survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
  }
  
  # Detect if a column is of date type, if so impose study start/end dates
  # only really interested in event_date and expo_date being within follow-up at this point as all other date variable 
  #have been checked in inclusion/exclusion & QA
  
  survival_data <- survival_data %>% mutate(event_date = replace(event_date, which(event_date>follow_up_end | event_date<follow_up_start), NA))
  survival_data <- survival_data %>% mutate(expo_date = replace(expo_date, which(expo_date>follow_up_end | expo_date<follow_up_start), NA))
  
  #schema <- sapply(survival_data, is.Date) 
  #schema = names(schema)[schema==TRUE]
  #schema=schema[!schema %in% c("follow_up_start","follow_up_end")]
  
  # The function set_dates_outofrange_na can be found in the script extra_functions_for_cox_model.R which contains
  # additional functions that are used throughout the modelling scripts
  
  #for (colname in schema){
  #  survival_data <- set_dates_outofrange_na(survival_data, colname)
  #}
  
  # Update COVID phenotypes after setting COVID exposure dates to NA that lie
  # outside follow up
  survival_data$expo_pheno=as.character(survival_data$expo_pheno)
  survival_data=survival_data%>%rowwise()%>%mutate(expo_pheno =ifelse(is.na(expo_date), "no_infection",expo_pheno))

  
  # Get COVID pheno specific dataset if necessary
  # Adds in variable date_expo_censor which is the COVID exposure date for the phenotype  not of interest
  # We want to be able to include follow up time prior to exposure for the pheno no of interest which uses date_expo_censor
  # to find this time period
  
  if(startsWith(subgroup,"covid_pheno")){
    survival_data <- get_pheno_specific_dataset(survival_data, pheno_of_interest=stratify_by)
  }
  
  # 1.Adjust follow up end date for COVID phenotype dataset to censor at COVID exposure for the
  # phenotype that is not of interest
  # 2.Remove people who's COVID exposure censor date is the same as their follow-up start date as they 
  # have no follow up period (for the pheno not of interest follow up is follow up start to the day before exposure so
  # if follow_up_start = date_expo_censor, follow up end is prior to follow up start).
  # 3.Follow up end being the day before date_expo_censor if the min of follow_up_end/date_expo_censor is date_expo_censor
  # is taken into account in a later script
  # 4.We want to keep people who's exposure censor date is after follow up start or who do not have an exposure data
  
  if(startsWith(subgroup,"covid_pheno_")){
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(follow_up_end, date_expo_censor,na.rm = TRUE))
    survival_data <- survival_data %>% filter((follow_up_start != date_expo_censor)|is.na(date_expo_censor))
  }
    
  survival_data=survival_data%>%filter(follow_up_end>=follow_up_start)
  
  total_covid_cases=nrow(survival_data %>% filter(!is.na(expo_date)))
    
  res_vacc <- fit_model_reducedcovariates(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo,cuts_days_since_expo_reduced,covar_names,total_covid_cases)
  print(paste0("Finished working on subgroup: ", subgroup, ", ",mdl,", ", cohort))
  return(res_vacc)
}
  
    
