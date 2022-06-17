## =============================================================================
## Pipeline (2): Reads in analysis-specific data, loads/sets any parameters,
## renames any variables used throughout the following scripts
## =============================================================================

# ---------------------- READ IN DATA ------------------------------------------
# read in core analysis information
read_in_cols <- c("patient_id",
                  "death_date",  
                  "cov_cat_sex", 
                  "cov_num_age", 
                  "exp_date_covid19_confirmed",
                  "sub_cat_covid19_hospital",
                  "cov_cat_region",
                  "index_date",
                  "cov_cat_ethnicity",
                  "sub_bin_covid19_confirmed_history",
                  "vax_date_covid_1",
                  paste0("out_date_",event_name))

if(active_analyses$prior_history_var != ""){
  read_in_cols <- unique(append(read_in_cols, c(active_analyses$prior_history_var, covar_names)))
}else{
  read_in_cols <- unique(append(read_in_cols, c(covar_names)))
}

if(event_name == "out_date_ami" | event_name == "out_date_stroke_isch" | event_name == "out_date_dvt" |
   event_name == "out_date_pe" | event_name == "out_date_tia" | event_name == "out_date_stroke_sah_hs" |
   event_name == "out_date_hf" | event_name == "out_date_angina" | event_name == "out_date_ate" |
   event_name == "out_date_vte" | event_name == "out_date_ami_primary_position" | event_name == "out_date_stroke_isch_primary_position" |
   event_name == "out_date_dvt_primary_position" | event_name == "out_date_pe_primary_position" | event_name == "out_date_tia_primary_position" |
   event_name == "out_date_stroke_sah_hs_primary_position" | event_name == "out_date_hf_primary_position" | event_name == "out_date_angina_primary_position" |
   event_name == "out_date_ate_primary_position" | event_name == "out_date_vte_primary_position"){
  input <- read_rds(paste0("output/input_",cohort,"_stage1_CVD.rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort,"_CVD.rds"))
  
} else if(event_name == "t1dm" | event_name == "t2dm" | event_name == "otherdm"){
  input <- read_rds(paste0("output/input_",cohort,"_stage1_diabetes.rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort,"_diabetes.rds"))
  
} else if (event_name == "gestationaldm"){
  input <- read_rds(paste0("output/input_",cohort,"_stage1_diabetes_gestational.rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort,"_diabetes_gestational.rds"))
  
} else if (event_name == "depression" | event_name == "anxiety_general" | event_name == "anxiety_ocd" |
           event_name == "anxiety_ptsd" | event_name == "eating_disorders" | event_name == "serious_mental_illness" |
           event_name == "self_harm_10plus" | event_name == "self_harm_15plus" | event_name == "suicide" | event_name == "addiction"){
  input <- read_rds(paste0("output/input_",cohort,"stage1_mental_health.rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort,"_mental_health.rds"))
  
} 

input <- input %>% select(all_of(read_in_cols))

# ADD END DATES -----------------------------------------------------------

end_dates <- end_dates[,c("patient_id",
                          colnames(end_dates)[grepl(paste0(event_name,"_follow_up_end"),colnames(end_dates))],
                          colnames(end_dates)[grepl(paste0(event_name,"_hospitalised_follow_up_end"),colnames(end_dates))],
                          colnames(end_dates)[grepl(paste0(event_name,"_non_hospitalised_follow_up_end"),colnames(end_dates))],
                          colnames(end_dates)[grepl(paste0(event_name,"_hospitalised_date_expo_censor"),colnames(end_dates))],
                          colnames(end_dates)[grepl(paste0(event_name,"_non_hospitalised_date_expo_censor"),colnames(end_dates))])] 

input <- input %>% left_join(end_dates, by = "patient_id")

rm(end_dates)

#---------------------------SPECIFY MAIN PARAMETERS-----------------------------
# specify study parameters
#For all analysis aside from age stratifed, analysis is performed across all ages 
agebreaks_all <- c(0, 111)
agelabels_all <- c("all")

#Age breaks and labels for age sub group analysis
agebreaks_strata <- c(0, 40, 60, 80, 111)
agelabels_strata <- c("18_39", "40_59", "60_79", "80_110")

#These are the study start and end dates for the Delta era
cohort_start_date <- as.Date("2021-06-01")
cohort_end_date <- as.Date("2021-12-14")

#Used to split time since COVID exposure; when there are time periods with no events then
#a reduced number of time periods is used (need 197 instead of 196 as time periods are split using [ , ) 

#cuts_days_since_expo <- c(28, 197) 
cuts_days_since_expo <- c(7, 14, 28, 56, 84, 197) 
cuts_days_since_expo_reduced <- c(28,197) 

#Rename input variable names (by renaming here it means that these scripts can be used for other datasets without
## having to keep updating all the variable names throughout the following scripts)
setnames(input, 
         old = c("death_date",  
                 "cov_cat_sex", 
                 "cov_num_age", 
                 "exp_date_covid19_confirmed",
                 "sub_cat_covid19_hospital",
                 "cov_cat_region",
                 "index_date",
                 "cov_cat_ethnicity",
                 c(paste0("out_date_", event_name)),
                 c(paste0(event_name,"_follow_up_end")),
                 c(paste0(event_name,"_hospitalised_follow_up_end")),
                 c(paste0(event_name,"_non_hospitalised_follow_up_end")),
                 c(paste0(event_name,"_hospitalised_date_expo_censor")),
                 c(paste0(event_name,"_non_hospitalised_date_expo_censor"))),
         
         new = c("DATE_OF_DEATH", 
                 "sex",
                 "AGE_AT_COHORT_START", 
                 "expo_date",
                 "expo_pheno",
                 "region_name",
                 "follow_up_start",
                 "ethnicity",
                 "event_date",
                 "follow_up_end",
                 "hospitalised_follow_up_end",
                 "non_hospitalised_follow_up_end",
                 "hospitalised_censor_date",
                 "non_hospitalised_censor_date"))



#Set the main cohort columns required to create the survival data 
#covariates are added later as these are loaded dependent on which model is being run

cohort_cols <- c("patient_id", 
                 "sex",
                 "ethnicity",
                 "DATE_OF_DEATH", 
                 "AGE_AT_COHORT_START", 
                 "expo_date",
                 "expo_pheno",
                 "region_name",
                 "follow_up_start",
                 "event_date",
                 "follow_up_end",
                 "hospitalised_follow_up_end",
                 "non_hospitalised_follow_up_end",
                 "hospitalised_censor_date",
                 "non_hospitalised_censor_date")
 

#-----------------------CREATE EMPTY ANALYSES NOT RUN DF------------------------
analyses_not_run=data.frame(matrix(nrow=0,ncol = 7))
colnames(analyses_not_run)=c("event","subgroup","cohort", "any exposures?", "any exposure events?", "any non exposed?", "more than 50 post exposure events?")
