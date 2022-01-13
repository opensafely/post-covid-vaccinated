## =============================================================================
## 1.Creates the base survival data that includes the event date for the outcoem of interest
## 2.Stratify to relevant subgroup if necessary
## 3.Sets the names of the covariates that need to be included if performing a subgroup analysis
## 4.If running COVID pheno subgroup will get pheno-specific dataset
## 5.Add follow up start and end dates
##
## Author: Samantha Ip
## =============================================================================
source(file.path(scripts_dir,"fit_model.R"))

get_vacc_res <- function(event,stratify_by_subgroup,stratify_by,input,cuts_days_since_expo,cuts_days_since_expo_reduced){
  # read in event dates for outcome-of-interest
  outcomes <-input%>%dplyr::select(c("patient_id", 
                              paste0("out_date_", event)))
  
  # wrangle columns for naming convention 
  setnames(outcomes, 
           old = c(paste0("out_date_", event)), 
           new = c("event_date"))
  outcomes$name <- event
  
  # Select the relevant cohort columns required to stratify by subgroup if necessary
  if(startsWith(strata,"prior_history")==T){
    survival_data <- input %>% dplyr::select(all_of(cohort_cols),all_of(stratify_by_subgroup))
  }else{
    survival_data <- input %>% dplyr::select(all_of(cohort_cols))
  }
  
  # Stratify to the relevant subgroup if either sex/ethnicity/prior history subgroup
  # Age subgroups are filtered in a later script; COVID pheno subgroup is filtered later in this script
  
  if(stratify_by_subgroup!="cov_cat_ethnicity" & stratify_by_subgroup!="SEX" & strata !="prior_history_all"){
    covar_names <- c(colnames(input)[grepl("cov_", colnames(input))],"patient_id")
    covar_names = covar_names[!covar_names %in% c("cov_bin_healthcare_worker","cov_bin_carehome_status","cov_bin_diabetes_type1","cov_bin_diabetes_type2","cov_bin_diabetes_other","cov_bin_diabetes_gestational","cov_bin_lipid_medications_dmd")]
  }else if(stratify_by_subgroup=="cov_cat_ethnicity"| stratify_by_subgroup=="SEX"){
    survival_data=survival_data%>%filter_at(stratify_by_subgroup,all_vars(.==stratify_by))
    covar_names <- c(colnames(input)[grepl("cov_", colnames(input))],"patient_id")
    covar_names <- covar_names[!covar_names==stratify_by_subgroup]
    covar_names = covar_names[!covar_names %in% c("cov_bin_healthcare_worker","cov_bin_carehome_status","cov_bin_diabetes_type1","cov_bin_diabetes_type2","cov_bin_diabetes_other","cov_bin_diabetes_gestational","cov_bin_lipid_medications_dmd")]
  }else if(startsWith(strata,"prior_history")==TRUE){
    survival_data=survival_data%>%filter_at(stratify_by_subgroup,all_vars(.==stratify_by))
    survival_data=survival_data%>%dplyr::select(!stratify_by_subgroup)
    covar_names <- c(colnames(input)[grepl("cov_", colnames(input))],"patient_id")
    covar_names = covar_names[!covar_names %in% c("cov_bin_healthcare_worker","cov_bin_carehome_status","cov_bin_diabetes_type1","cov_bin_diabetes_type2","cov_bin_diabetes_other","cov_bin_diabetes_gestational","cov_bin_lipid_medications_dmd")]
  }
 
  # join core data with outcomes
  survival_data <- survival_data %>% left_join(outcomes)
  survival_data$event_date=as.Date(survival_data$event_date)
  
  #add follow up start and end dates
  
  if(project=="vaccinated_delta"){
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(event_date, DATE_OF_DEATH,cohort_end_date,na.rm = TRUE))
  }else if(project=="electively_unvaccinated_delta"){
    survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(vax_date_covid_1,event_date, DATE_OF_DEATH,cohort_end_date,na.rm = TRUE))
    survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
  }else if (project == "unvaccinated"){
    survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
    survival_data$follow_up_start=cohort_start_date
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(event_date, vax_date_covid_1, DATE_OF_DEATH,cohort_end_date,date_expo_censor,na.rm = TRUE))
    survival_data <- survival_data %>% dplyr::select(!vax_date_covid_1)
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
  if(startsWith(strata,"covid_pheno")==T){
    survival_data$expo_pheno=as.character(survival_data$expo_pheno)
    survival_data=survival_data%>%rowwise()%>%mutate(expo_pheno =ifelse(is.na(expo_date), "no_infection",expo_pheno))
    survival_data$expo_pheno=as.factor(survival_data$expo_pheno)
  }
  
  #get COVID pheno specific dataset if necessary
  if(startsWith(strata,"covid_pheno_")==T){
    survival_data <- get_pheno_specific_dataset(survival_data, pheno_of_interest=stratify_by)
  }
  
  #Adjust follow up end date for COVID phenotype dataset to censor at COVID exposure for the
  #phenotype that is not of interest
  if(startsWith(strata,"covid_pheno_")==T){
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(follow_up_end, date_expo_censor,na.rm = TRUE))
  }
    
  survival_data=survival_data%>%filter(follow_up_end>=follow_up_start)#can remove once studydef is updated
  
  res_vacc <- fit_model_reducedcovariates(event, stratify_by_subgroup, stratify_by, survival_data,covar_names,input)
  return(res_vacc)
}
  
  
