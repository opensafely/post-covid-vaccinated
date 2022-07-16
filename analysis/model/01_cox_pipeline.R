## =============================================================================
## Pipeline (1): Control center, calls relevant analysis scripts, sets working 
## and saving directories, parallelises processes
##
## Based on scripts written by Samantha Ip, see the following repo's for 
## original scripts: https://github.com/BHFDSC/CCU002_01 & https://github.com/BHFDSC/CCU002_03
## =============================================================================


library(data.table)
library(dplyr)
library(survival)
library(broom)
library(DBI)
library(ggplot2)
library(nlme)
library(tidyverse)
#library(R.utils)
library(lubridate)
library(purrr)
library(parallel)
library(stats)
library(utils)
library(stringr)
library(rms)
#library(multcomp)
library(readr)
library(Hmisc)


args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  event_name="ate"
  cohort="electively_unvaccinated"
}else{
  event_name  = args[[1]]
  cohort = args[[2]]
}

# Specify directories ----------------------------------------------------------

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "model"))
fs::dir_create(here::here("output", "review", "model","fit-individual-covariates"))
output_dir <- "output/review/model"
scripts_dir <- "analysis/model"

# Source relevant files --------------------------------------------------------

source(file.path(scripts_dir,"02_01_cox_analyses_to_run.R"))
source(file.path(scripts_dir,"02_02_cox_load_data.R")) # Prepare dataset for model
source(file.path(scripts_dir,"06_cox_extra_functions.R"))

# Add time point parameter to analyses to run  ----------------------------

source(file.path(scripts_dir,"02_03_cox_timepoint_param.R")) # Prepare dataset for model

# add reduced time point column 

analyses_to_run$reduced_timepoint <- lapply(split(analyses_to_run,seq(nrow(analyses_to_run))),
                                            function(analyses_to_run) 
                                              get_timepoint(
                                                event=analyses_to_run$event,
                                                subgroup=analyses_to_run$subgroup,
                                                stratify_by_subgroup=analyses_to_run$stratify_by_subgroup,
                                                stratify_by=analyses_to_run$strata,
                                                input, cuts_days_since_expo,cuts_days_since_expo_reduced)
)

analyses_to_run$reduced_timepoint <-  as.character(analyses_to_run$reduced_timepoint)
analyses_to_run <- analyses_to_run %>% filter(reduced_timepoint != "remove")
analyses_to_run_normal_timepoint <- analyses_to_run %>% filter(reduced_timepoint == "normal")
analyses_to_run$reduced_timepoint <- "reduced"
analyses_to_run <- rbind(analyses_to_run, analyses_to_run_normal_timepoint)

rm(analyses_to_run_normal_timepoint)

# Join in reduced covariates

analyses_to_run <- analyses_to_run %>% left_join(non_zero_covar_names, by= c("event"="outcome_event","subgroup","reduced_timepoint"="time_period"))
rm(non_zero_covar_names)

#if(event_name %in% c("ate","vte") & cohort == "vaccinated"){
#  analyses_to_run_hosp_alternative <- analyses_to_run %>% filter(subgroup == "covid_pheno_hospitalised")
#  analyses_to_run_hosp_alternative$reduced_timepoint <- "alternative"
#  analyses_to_run_hosp_alternative <- distinct(analyses_to_run_hosp_alternative)
#  analyses_to_run <- rbind(analyses_to_run, analyses_to_run_hosp_alternative)
#}


# Source remainder of relevant files --------------------------------------------------------

source(file.path(scripts_dir,paste0("03_01_cox_subgrouping.R"))) # Model specification

# ------------------------------------ LAUNCH JOBS -----------------------------
if(nrow(analyses_to_run>0)){
  lapply(split(analyses_to_run,seq(nrow(analyses_to_run))),
         function(analyses_to_run)
           get_vacc_res(           
             event=analyses_to_run$event,           
             subgroup=analyses_to_run$subgroup,           
             stratify_by_subgroup=analyses_to_run$stratify_by_subgroup,           
             stratify_by=analyses_to_run$strata,           
             time_point=analyses_to_run$reduced_timepoint,       
             input,covar_names,
             reduced_covar_names=analyses_to_run$covariates,#
             cuts_days_since_expo,cuts_days_since_expo_reduced,mdl))
}

#Save csv of anlayses not run
write.csv(analyses_not_run, paste0(output_dir,"/analyses_not_run_" , event_name ,"_",cohort,".csv"), row.names = T)

if(nrow(analyses_to_run)==0){
  sink(paste0("output/not-for-review/describe_data_surv_",event_name,"__",cohort,"__time_periods.txt"))
  sink()
  
  df <- as.data.frame(matrix(ncol = 2))
  write.csv(df, paste0("output/input_",event_name,"__",cohort,"__time_periods.csv"))
  write.csv(df, paste0("output/input_sampled_data_",event_name,"__",cohort,"__time_periods.csv"))
  
}
  

#Combine all results into one .csv
source(file.path(scripts_dir, "05_cox_format_tbls_HRs.R"))

