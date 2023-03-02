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


args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  event_name="ami"
  cohort="vaccinated"
}else{
  event_name  = args[[1]]
  cohort = args[[2]]
}

# Specify directories ----------------------------------------------------------

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "model"))
output_dir <- "output/review/model"
scripts_dir <- "analysis/model"

# Source relevant files --------------------------------------------------------

source(file.path(scripts_dir,"02_01_cox_analyses_to_run.R"))
source(file.path(scripts_dir,"02_02_cox_load_data.R")) # Prepare dataset for model
source(file.path(scripts_dir,"06_cox_extra_functions.R"))

# Add time point parameter to analyses to run  ----------------------------

source(file.path(scripts_dir,"02_03_cox_timepoint_param.R")) # Prepare dataset for model

analyses_to_run_timepoints <- analyses_to_run %>% filter(mdl=="mdl_max_adj")

# add reduced time point column 

analyses_to_run_timepoints$reduced_timepoint <- NA

analyses_to_run_timepoints$reduced_timepoint <- lapply(split(analyses_to_run_timepoints,seq(nrow(analyses_to_run_timepoints))),
                                            function(analyses_to_run_timepoints) 
                                              get_timepoint(
                                                event=analyses_to_run_timepoints$event,
                                                subgroup=analyses_to_run_timepoints$subgroup,
                                                stratify_by_subgroup=analyses_to_run_timepoints$stratify_by_subgroup,
                                                stratify_by=analyses_to_run_timepoints$strata,
                                                mdl=analyses_to_run_timepoints$mdl,
                                                input, cuts_days_since_expo,cuts_days_since_expo_reduced,covar_names)
)

analyses_to_run_timepoints <- analyses_to_run_timepoints %>% select(subgroup, reduced_timepoint)

analyses_to_run <- analyses_to_run %>% left_join(analyses_to_run_timepoints, by="subgroup")
analyses_to_run <- analyses_to_run %>% filter(reduced_timepoint != "remove")

# If one subgroup category is "reduced" then make sure all of the subgroup categories are "reduced" for comparison purposes

analyses_to_run <- analyses_to_run %>%
  group_by(subgroup_cat) %>%
  dplyr::mutate(reduced_timepoint = case_when(
    any(reduced_timepoint == "reduced") ~ "reduced",
    TRUE ~ as.character(reduced_timepoint)))


#Remove hospitalised analysis with normal timepoints as this will be run in stata and we don't need the saved data sets
#from this.

analyses_to_run <- analyses_to_run %>% filter(subgroup != "covid_pheno_hospitalised" | reduced_timepoint != "normal")

# Add day zero analyses
day_zero_analyses <- analyses_to_run %>% filter(subgroup %in% c("main","covid_pheno_non_hospitalised"))
day_zero_analyses$reduced_timepoint <- paste0("day_zero_",day_zero_analyses$reduced_timepoint)
analyses_to_run <- rbind(analyses_to_run, day_zero_analyses)
rm(day_zero_analyses)
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
             mdl=analyses_to_run$mdl,   
             time_point=analyses_to_run$reduced_timepoint,       
             input,covar_names,
             cuts_days_since_expo,cuts_days_since_expo_reduced,
             cuts_days_since_expo_day_zero,cuts_days_since_expo_reduced_day_zero,
             mdl))
}

#Save csv of anlayses not run
write.csv(analyses_not_run, paste0(output_dir,"/analyses_not_run_" , event_name ,"_",cohort, ".csv"), row.names = T)


#Combine all results into one .csv
source(file.path(scripts_dir, "05_cox_format_tbls_HRs.R"))

