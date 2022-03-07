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
  event_name="ate"
}else{
  event_name  = args[[1]] 
}

########
#To Test
########

#event="ate"
#subgroup="main"
#stratify_by_subgroup="main"
#stratify_by="main"
#mdl="mdl_max_adj"


# Specify directories ----------------------------------------------------------
output_dir <- "output"
scripts_dir <- "analysis"

# Source relevant files --------------------------------------------------------
source(file.path(scripts_dir,"analyses_to_run.R"))

for(i in cohort_to_run){
  cohort<-i
  source(file.path(scripts_dir,"02_pipe.R")) # Prepare dataset for model
  source(file.path(scripts_dir,"extra_functions_for_cox_models.R"))
  source(file.path(scripts_dir,paste0("call_mdl.R"))) # Model specification
  
  ls_events_missing <- get(paste0("analyses_to_run_",cohort)) 
  
  # ------------------------------------ LAUNCH JOBS -----------------------------
  
  lapply(split(ls_events_missing,seq(nrow(ls_events_missing))),
         function(ls_events_missing) 
           get_vacc_res(
             event=ls_events_missing$event,
             subgroup=ls_events_missing$subgroup,
             stratify_by_subgroup=ls_events_missing$stratify_by_subgroup,
             stratify_by=ls_events_missing$strata,
             mdl=ls_events_missing$mdl,
             input, cuts_days_since_expo,cuts_days_since_expo_reduced,covar_names)
  )
  
  
  #Save csv of anlayses not run
  write.csv(analyses_not_run, paste0(output_dir,"/analyses_not_run_" , event_name ,"_",cohort, ".csv"), row.names = T)
}


#Combine all results into one .csv
source(file.path(scripts_dir, "format_tbls_HRs.R"))

