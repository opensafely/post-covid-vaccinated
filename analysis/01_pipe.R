## =============================================================================
## Pipeline (1): Control center, calls relevant analysis scripts, sets working 
## and saving directories, parallelises processes
##
## Author: Samantha Ip
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
mdl  = args[[1]] # "mdl_agesex", "mdl_max_adj"
event_name  = args[[2]] # "all" or individual outcome
strata  = args[[3]] 
#strata: main, covid_pheno_all, covid_pheno_hospitalised, covid_pheno_non_hospitalised, agegp_all, agegp_18_39, agegp_40_59, agegp_60_79, agegp_80_110
#sex_all,sex_M, sex_F, ethnicity_all,ethnicity_1, ethnicity_2, ethnicity_3, ethnicity_4, ethnicity_5, ethnicity_6
#prior_history_all
project = args[[4]] #vaccinated_delta, electively_unvaccinated_delta, unvaccinated
covid_history = args[[5]] #covid_history_false, covid_history_true
########
#To Test
########

#mdl="mdl_agesex"
#event_name="all"
#strata="agegp_all"
#project="vaccinated_delta"
#covid_history="covid_history_false"
#event="ami"
#stratify_by_subgroup="main"
#stratify_by="main"

# Specify directories ----------------------------------------------------------
output_dir <- "output"
scripts_dir <- "analysis"

# Source relavant files --------------------------------------------------------
source(file.path(scripts_dir,"02_pipe.R")) # Prepare dataset for model
source(file.path(scripts_dir,paste0("call_mdl.R"))) # Model specification


#input$expo_pheno[input$expo_pheno=="hospitalised_after28days"]="non_hospitalised"
#input$expo_pheno[input$expo_pheno=="hospitalised_within28days"]="hospitalised"

# ------------------------------------ LAUNCH JOBS -----------------------------

#mclapply(split(ls_events_missing,seq(nrow(ls_events_missing))), mc.cores = 2,
lapply(split(ls_events_missing,seq(nrow(ls_events_missing))),
       function(ls_events_missing) 
         get_vacc_res(
           event=ls_events_missing$event,
           stratify_by_subgroup=ls_events_missing$subgroup,
           stratify_by=ls_events_missing$which_strata,
           input, cuts_days_since_expo,cuts_days_since_expo_reduced)
)
   

#Save csv of anlayses not run
write.csv(analyses_not_run, paste0(output_dir,"/analyses_not_run_" , save_name ,"_",project,"_", mdl,"_",covid_history, ".csv"), row.names = T)


#Combine all results into one .csv
source(file.path(scripts_dir, "format_tbls_HRs.R"))

