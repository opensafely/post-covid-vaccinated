## =============================================================================
## Pipeline (2): Reads in analysis-specific data, loads parameters, 
##
## Author: Samantha Ip
## =============================================================================


# specify path to data
if(project=="vaccinated_delta"){
  master_df_fpath <- "output/input_vaccinated_stage1.rds"
}else if(project=="electively_unvaccinated_delta"){
  master_df_fpath <- "output/input_electively_unvaccinated_stage1.rds"
}


# specify study parameters
#For all analysis aside from age stratifed, analysis is performed across all ages 
agebreaks_all <- c(0, 110)
agelabels_all <- c("all")

#Age breaks and labels for age sub group analysis
agebreaks_strata <- c(0, 40, 60, 80, 111)
agelabels_strata <- c("18_39", "40_59", "60_79", "80_110")

cohort_start_date <- as.Date("2021-06-01")
cohort_end_date <- as.Date("2021-12-14")

#Used to split time since COVID exposure; when there are time periods with no events then
#a reduced number of time periods is used
#cuts_days_since_expo=c(28,365) 

cuts_days_since_expo <- c(14, 28, 56, 84, 196) 
cuts_days_since_expo_reduced <- c(28,196) 


# ---------------------- READ IN DATA ------------------------------------------
# read in core analysis information
input=read_rds(master_df_fpath)
master_names=colnames(input)


setnames(input, 
         old = c("death_date",  
                 "cov_cat_sex", 
                 "cov_num_age", 
                 "exp_date_covid19_confirmed",
                 "sub_cat_covid19_hospital",
                 "cov_cat_region",
                 "index_date"), 
         new = c("DATE_OF_DEATH", 
                 "SEX",
                 "AGE_AT_COHORT_START", 
                 "expo_date",
                 "expo_pheno",
                 "region_name",
                 "follow_up_start"))

#Reduce dataset to those who do NOT have a prior hsitory of COVID unless running the sensitivity
#analysis for this with a prior history

if(covid_history =="covid_history_false"){
  input=input%>%filter(sub_bin_covid19_confirmed_history == FALSE)
}else if (covid_history =="covid_history_true"){
  input=input%>%filter(sub_bin_covid19_confirmed_history == TRUE)
}


#--------------------Determine which combinations need to be run----------------

#Agebreaks=agebreaks_all in all analysis except age subgroup analysis
agebreaks=agebreaks_all
agelabels=agelabels_all


#Set which outcomes need to be run; either all outcomes are run or only one single specified one 
if(event_name =="all"){
  event=colnames(input)[grepl("out_",colnames(input))]
  event=gsub("out_date_","",event)
  event=event[!event %in% c("diabetes_type1","diabetes_type2","diabetes_other","diabetes_gestational")]
}else{
  event=event_name
}

#Set variables for which subgroup needs to be run, which levels of the subgroup and a name for all the outputs 
#to identify the results

if(strata == "main"){
  subgroup="main"
  which_strata="main"
  save_name="main"
}else if(strata == "covid_pheno_all"){
  subgroup="expo_pheno"
  which_strata=c("hospitalised","non_hospitalised")
  save_name="subgroup_covid_pheno"
}else if (startsWith(strata, "covid_pheno")==TRUE & strata != "covid_pheno_all"){
  subgroup="expo_pheno"
  which_strata=gsub("covid_pheno_","",strata)
  save_name="subgroup_covid_pheno"
}else if(strata == "agegp_all"){
  agebreaks=agebreaks_strata
  agelabels=agelabels_strata
  subgroup="agegp"
  which_strata=agelabels
  save_name="subgroup_agegp"
}else if(startsWith(strata,"agegp")==TRUE & strata !="agegp_all"){
  agebreaks=agebreaks_strata
  agelabels=agelabels_strata
  subgroup="agegp"
  which_strata=gsub("agegp_","",strata)
  save_name="subgroup_agegp"
}else if (strata == "sex_all"){
  subgroup="SEX"
  which_strata=c("F","M")
  save_name="subgroup_sex"
}else if (startsWith(strata,"sex")==TRUE & strata !="sex_all"){
  subgroup="SEX"
  which_strata=gsub("sex_","",strata)
  save_name="subgroup_sex"
}else if (strata == "ethnicity_all"){
  subgroup="cov_cat_ethnicity"
  which_strata = as.character(unique(input$cov_cat_ethnicity))
  save_name="subgroup_ethnicity"
}else if (startsWith(strata,"ethnicity")==TRUE & strata !="ethnicity_all"){
  subgroup="cov_cat_ethnicity"
  which_strata = gsub("ethnicity_","",strata)
  save_name="subgroup_ethnicity"
}else if (strata == "prior_history_all"){
  subgroup=NA
  which_strata = c("TRUE","FALSE")
  save_name="subgroup_prior_history"
}

#Create dataframe of all models that need to be run
ls_events_missing=crossing(event,subgroup,which_strata)

#Add in covariates to stratify by for prior history
if(strata == "prior_history_all"){
  ls_events_missing=ls_events_missing %>%
    mutate(subgroup = case_when(event %in% c("ami","stroke_isch","tia","angina","ate") ~ 'sub_bin_ate',
                                event %in% c("pe","dvt","vte") ~ 'cov_bin_vte',
                                event == "stroke_sah_hs" ~ 'cov_bin_all_stroke',
                                event == "hf" ~ 'cov_bin_hf'))
}


#Set the main cohort columns required to create the survival data 
#covariates are added later as these are loaded dependent on which model is being run

if(startsWith(strata, "covid_pheno")==TRUE){
 cohort_cols <- c("patient_id", 
                      "SEX",
                      "cov_cat_ethnicity",
                      "DATE_OF_DEATH", 
                      "AGE_AT_COHORT_START", 
                      "expo_date",
                      "expo_pheno",
                      "region_name",
                      "follow_up_start")
}else{
 cohort_cols <-c("patient_id", 
                     "SEX",
                     "cov_cat_ethnicity",
                     "DATE_OF_DEATH", 
                     "AGE_AT_COHORT_START", 
                     "expo_date",
                     "region_name",
                     "follow_up_start")
}
 


analyses_not_run=data.frame(matrix(nrow=0,ncol = 7))
colnames(analyses_not_run)=c("event","subgroup","strata","any exposures?", "any exposure events?", "any non exposed?", "more than 400 post exposure events?")


#------------------------ SET DATES OUTSIDE RANGE AS NA ------------------------


set_dates_outofrange_na <- function(df, colname)
{
  df=df%>%rowwise()%>%mutate(!!sym(colname):=as.Date(ifelse(!!sym(colname)<follow_up_start | !!sym(colname) > follow_up_end, NA,!!sym(colname)),origin='1970-01-01'))
  return(df)
}


#----------------------- GET COVID PHENO-SPECIFIC DATASET ----------------------
get_pheno_specific_dataset <- function(survival_data, pheno_of_interest){
  survival_data$date_expo_censor <- as.Date(ifelse(!(survival_data$expo_pheno %in% pheno_of_interest),
                                                   survival_data$expo_date, 
                                                   NA), origin='1970-01-01')
  
  
  survival_data$expo_date <- as.Date(ifelse((!is.na(survival_data$date_expo_censor)) & (survival_data$expo_date >= survival_data$date_expo_censor), NA, survival_data$expo_date), origin='1970-01-01')
  survival_data$event_date <- as.Date(ifelse((!is.na(survival_data$date_expo_censor)) & (survival_data$event_date >= survival_data$date_expo_censor), NA, survival_data$event_date), origin='1970-01-01')
  return(survival_data)
}


