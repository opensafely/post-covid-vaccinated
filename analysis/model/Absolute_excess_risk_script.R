####  IMPORTANT - PLEASE READ ####
# This script will save results directly to the EHR sharepoint so will save over any results that are already there
# The lifetables will be saved into the directory aer_raw_output_dir. 
# To create a new folder, please change the 'date' variable to avoid overwriting any previous results
# To save from your own OneDrive, update the staff ID (currenlty zy21123) to your own
library(tidyverse)

# Calculates AER within age/sex subgroups

# Set file locations
date <- "15_06_2023"

aer_raw_output_dir <- paste0("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-cardiovascular/pre-vax & delta/OS output/AER/lifetables/",date,"/")
aer_compiled_output_dir <- paste0("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-cardiovascular/pre-vax & delta/OS output/AER/compiled_results_for_plotting/",date,"/")

table_2_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-cardiovascular/pre-vax & delta/OS output/Table 2/"
scripts_dir <- "analysis/model"

dir.create(file.path(aer_raw_output_dir), recursive =TRUE, showWarnings = FALSE)
dir.create(file.path(aer_compiled_output_dir), recursive =TRUE, showWarnings = FALSE)


#-------------------------Call AER function-------------------------------------
source(file.path(scripts_dir,"Absolute_excess_risk_function.R"))

library(purrr)
library(data.table)
library(tidyverse)


#--------------------which analyses to calculate AER for------------------------
#Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")                             
active <- active[active$outcome_variable %in% c("out_date_ate","out_date_vte"),]   

active$event <- gsub("out_date_","",active$outcome_variable)                                                                                        
active[,c("active","outcome","outcome_variable","prior_history_var","covariates","model","cohort")] <- NULL       

active <- active %>%
  mutate_all(as.character)

#Converts to long-format
active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event")), 
                              names_to = "strata")                                                                          
active <- active[active$value==TRUE, c("event","strata")]                               

active$model <- "mdl_max_adj"                 

#Add cohorts
active <- crossing(active, c("pre_vaccination","vaccinated","electively_unvaccinated"))
colnames(active) <- c("event", "subgroup", "model", "cohort")

#Add time points
active$time_points <- "reduced"

#Focus only on aer analyses
active <- active %>% filter(startsWith(subgroup,"aer_") & model=="mdl_max_adj")

#Add HR time point terms so that results can be left joined
term <- c("days0_28","days28_197","days197_365", "days365_714")
results <- crossing(active,term)


#------------------------------------ Load results------------------------------
# to run change zy21123 to your personal staff ID
# In this file the t2dm_extended_follow_up outcomes for prevax have been renamed t2dm
input <- read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-cardiovascular/pre-vax & delta/OS output/Hazard ratios/hr_output_formatted.csv")
unique(input$cohort)
#-------------------Select required columns and term----------------------------
input <- input %>% 
  filter(str_detect(term, "^days")
         & (subgroup == "main")
         & model=="mdl_max_adj"
         & estimate != "[Redacted]"
         & time_points == "reduced"
         & ((event %in% c("ate","vte") & cohort %in% c("vaccinated","electively_unvaccinated")) | (event %in% c("ate_extended_follow_up","vte_extended_follow_up") & cohort %in% c("pre_vaccination")))) %>%
  select(event,cohort,model,time_points,term,estimate)

input$event <- gsub("_extended_follow_up","",input$event)

#---------------------------------Input Table 2---------------------------------
table2_pre_vax_extended <- read.csv(paste0(table_2_output_dir,"table2_pre_vaccination_extended_follow_up_any_position_events.csv" ))
table2_pre_vax_extended <- table2_pre_vax_extended %>% rename(cohort_to_run = cohort_name)
table2_pre_vax_extended$total_person_days_to_day_197 <- NULL
table2_pre_vax_extended$event <- gsub("_extended_follow_up","",table2_pre_vax_extended$event)

table2_vax <- read.csv(paste0(table_2_output_dir,"table2_vaccinated.csv" ))
table2_unvax <- read.csv(paste0(table_2_output_dir,"table2_electively_unvaccinated.csv" ))


table_2 <- rbind(table2_pre_vax_extended,table2_vax,table2_unvax)
table_2 <- table_2 %>% rename(cohort = cohort_to_run)
rm(table2_pre_vax_extended,table2_vax,table2_unvax)

#-------------------Select required columns and term----------------------------

table_2 <- table_2 %>% select(subgroup, event, cohort,unexposed_person_days,unexposed_event_count,total_covid19_cases, N_population_size) %>%
  filter(startsWith(subgroup, "aer_"))


table_2$event <- gsub("out_date_","",table_2$event)

# Join HRs onto active df

results <- results %>% left_join(input, by=c("event","cohort","model","time_points","term"))
results <- results %>% filter(!is.na(estimate))

#Join on table 2 event counts
results <- results %>% left_join(table_2, by=c("event","cohort","subgroup"))

results <- results %>% mutate(across(c(estimate, unexposed_person_days, unexposed_event_count, total_covid19_cases), as.numeric))

#-------------------------Run AER function--------------------------------------

lapply(split(active,seq(nrow(active))),
       function(active)
         excess_risk(   
           event_of_interest = active$event,
           cohort_of_interest = active$cohort,
           model_of_interest = active$model,
           subgroup_of_interest = active$subgroup,
           time_point_of_interest = active$time_points,
           results))


#------------------------------Compile the results------------------------------
AER_files=list.files(path = aer_raw_output_dir, pattern = "lifetable_")
AER_files=paste0(aer_raw_output_dir,"/",AER_files)
AER_compiled_results <- purrr::pmap(list(AER_files),
                                    function(fpath){
                                      df <- fread(fpath)
                                      return(df)})
AER_compiled_results=rbindlist(AER_compiled_results, fill=TRUE)

# Calculate overall AER
AER_combined <- AER_compiled_results %>% select(days, event, cohort, subgroup, time_points, cumulative_difference_absolute_excess_risk)
table_2 <- table_2 %>% select(event, cohort, subgroup, N_population_size)

# AER_combined <- AER_combined %>% left_join(table_2, by=c("event","cohort","subgroup"))

#Standardize AER and use pre-vax subgroup sizes for all cohorts
table_2 <- table_2 %>% filter(cohort == "pre_vaccination") %>% select(!cohort)
table_2$weight <- table_2$N_population_size/sum(table_2$N_population_size)

AER_combined <- AER_combined %>% left_join(table_2 %>% select(event,subgroup,weight), by=c("event","subgroup"))

AER_combined <- AER_combined %>% filter(!is.na(cumulative_difference_absolute_excess_risk))

AER_combined <- AER_combined %>% 
  dplyr::group_by(days, event, cohort,time_points) %>%
  dplyr::summarise(weighted_mean = weighted.mean(cumulative_difference_absolute_excess_risk,weight))


#Join all results together 
AER_combined$subgroup <- "aer_overall"

AER_combined <- AER_combined %>% dplyr::rename(cumulative_difference_absolute_excess_risk = weighted_mean )

AER_compiled_results <- rbind(AER_compiled_results,AER_combined, fill = TRUE)

write.csv(AER_compiled_results, paste0(aer_compiled_output_dir,"/AER_compiled_results.csv"), row.names = F)
