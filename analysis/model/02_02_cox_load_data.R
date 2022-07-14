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

input <- read_rds(paste0("output/input_",cohort,"_stage1.rds"))
input <- input %>% select(all_of(read_in_cols))

end_dates <- read_rds(paste0("output/follow_up_end_dates_",cohort,".rds"))
end_dates <- end_dates[,c("patient_id",
                          colnames(end_dates)[grepl(paste0(event_name,"_follow_up_end"),colnames(end_dates))],
                          colnames(end_dates)[grepl(paste0(event_name,"_hospitalised_follow_up_end"),colnames(end_dates))],
                          colnames(end_dates)[grepl(paste0(event_name,"_non_hospitalised_follow_up_end"),colnames(end_dates))],
                          colnames(end_dates)[grepl(paste0(event_name,"_hospitalised_date_expo_censor"),colnames(end_dates))],
                          colnames(end_dates)[grepl(paste0(event_name,"_non_hospitalised_date_expo_censor"),colnames(end_dates))])] 


input <- input %>% left_join(end_dates, by = "patient_id")
rm(end_dates)

#---------------------Set region reference level--------------------------------
#input$cov_cat_region <- relevel(input$cov_cat_region, ref = "London")

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

cuts_days_since_expo <- c(7, 14, 28, 56, 84, 197) 
cuts_days_since_expo_reduced <- c(28,197)
cuts_days_since_expo_alternative <- c(1,8,43,197)

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
