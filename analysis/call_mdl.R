## =============================================================================
## 1.Creates the base survival data that includes the event date for the outcome of interest
## 2.Stratify to relevant subgroup if necessary
## 3.Sets the names of the covariates that need to be included if performing a subgroup analysis
## 4.If running COVID pheno subgroup will get pheno-specific dataset
## 5.Add follow up start and end dates
##
## Based on scripts by Samantha Ip
## =============================================================================
source(file.path(scripts_dir,"fit_model.R"))

get_vacc_res <- function(event,subgroup,stratify_by_subgroup,stratify_by,mdl,input,cuts_days_since_expo,cuts_days_since_expo_reduced,covar_names){
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
  
  # Filter for age group of interest ----
  survival_data$AGE_AT_COHORT_START <- as.numeric(survival_data$AGE_AT_COHORT_START)
  
  if(startsWith(subgroup,"agegp")){
    agebreaks=agebreaks_strata
    agelabels=agelabels_strata
  }else{
    agebreaks=agebreaks_all
    agelabels=agelabels_all
  }
  
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
  
  # detect if a column is of date type, if so impose study start/end dates
  schema <- sapply(survival_data, is.Date) #only really interested in event_date and expo_date being within follow-up at this point
  schema = names(schema)[schema==TRUE]
  schema=schema[!schema %in% c("follow_up_start","follow_up_end")]
  
  for (colname in schema){
    survival_data <- set_dates_outofrange_na(survival_data, colname)
  }
  
  #Update COVID phenotypes after setting COVID exposure dates to NA that lie
  #outisde follow up
  survival_data$expo_pheno=as.character(survival_data$expo_pheno)
  survival_data=survival_data%>%rowwise()%>%mutate(expo_pheno =ifelse(is.na(expo_date), "no_infection",expo_pheno))

  
  #get COVID pheno specific dataset if necessary
  if(startsWith(subgroup,"covid_pheno")){
    survival_data <- get_pheno_specific_dataset(survival_data, pheno_of_interest=stratify_by)
  }
  
  #Adjust follow up end date for COVID phenotype dataset to censor at COVID exposure for the
  #phenotype that is not of interest
  if(startsWith(subgroup,"covid_pheno_")){
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(follow_up_end, date_expo_censor,na.rm = TRUE))
  }
    
  survival_data=survival_data%>%filter(follow_up_end>=follow_up_start)
  
  total_covid_cases=nrow(survival_data %>% filter(!is.na(expo_date)))
    
  res_vacc <- fit_model_reducedcovariates(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo,cuts_days_since_expo_reduced,covar_names,total_covid_cases)
  return(res_vacc)
}
  
    
