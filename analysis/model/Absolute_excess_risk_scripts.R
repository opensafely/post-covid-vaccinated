library(dplyr)
library(purrr)
library(data.table)
library(tidyverse)
# Calculates AER within age/sex subgroups

# Set file locations
results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
aer_raw_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/raw_results/"
aer_compiled_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/compiled_results/"
scripts_dir <- "analysis/model"

dir.create(file.path(aer_raw_output_dir), recursive =TRUE, showWarnings = FALSE)
dir.create(file.path(aer_compiled_output_dir), recursive =TRUE, showWarnings = FALSE)


#-------------------------Call AER function-------------------------------------
source(file.path(scripts_dir,"Absolute_excess_risk_function.R"))

#--------------------which analyses to calculate AER for------------------------

#Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")                             
active <- active[active$active==TRUE,]   

active$event <- gsub("out_date_","",active$outcome_variable)                                                                                        
active[,c("active","outcome","outcome_variable","prior_history_var","covariates","model","cohort")] <- NULL       

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
time_points <- c("reduced","normal")
active <- crossing(active,time_points)

#Focus only on aer analyses
active <- active %>% filter(startsWith(subgroup,"aer_") & model=="mdl_max_adj")

#Add HR time point terms so that results can be left joined
term <- c("days0_28","days28_197","days197_365","days365_714")
active_reduced <- crossing(active[active$time_points == "reduced",],term )

term <- c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_365","days365_714")
active_normal <- crossing(active[active$time_points == "normal",],term )  

results <- rbind(active_reduced,active_normal)

rm(active_reduced,active_normal)

#------------------------------------ Load results------------------------------
input <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

#-------------------Select required columns and term----------------------------
input <- input %>% 
  filter(str_detect(term, "^days")
         & subgroup == "main"
         & model=="mdl_max_adj"
         & time_points %in% c("reduced","normal")
         & ((cohort == "pre_vaccination" & event %in% c("ate_extended_follow_up","vte_extended_follow_up","ate_primary_position_extended_follow_up","vte_primary_position_extended_follow_up"))
            | (cohort != "pre_vaccination" & event %in% c("ate","vte","ate_primary_position","vte_primary_position")))) %>%
  select(event,cohort,subgroup,model,time_points,term,estimate)

input$event <- gsub("_extended_follow_up","",input$event)
#---------------------------------Input Table 2---------------------------------
table2_pre_vax <- read.csv(paste0(results_dir,"table2_pre_vaccination_extended_follow_up_any_position_events.csv"))
table2_pre_vax_primary <- read.csv(paste0(results_dir,"table2_pre_vaccination_extended_follow_up_primary_position_events.csv"))
table2_pre_vax <- rbind(table2_pre_vax,table2_pre_vax_primary)

table2_vax <- read.csv(paste0(results_dir,"table2_vaccinated.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_electively_unvaccinated.csv"))

table2_pre_vax <- dplyr::rename(table2_pre_vax, cohort = cohort_name) %>% select(!total_person_days_to_day_197)
table2_vax <- dplyr::rename(table2_vax, cohort = cohort_to_run)
table2_unvax <- dplyr::rename(table2_unvax, cohort = cohort_to_run)

table_2 <- rbind(table2_pre_vax, table2_vax,table2_unvax)
rm(table2_pre_vax,table2_vax,table2_unvax,table2_pre_vax_primary)

#-------------------Select required columns and term----------------------------

table_2 <- table_2 %>% select(subgroup, event, cohort,unexposed_person_days,unexposed_event_count,total_covid19_cases, N_population_size) %>%
  filter(startsWith(subgroup, "aer_"))

table_2$event <- gsub("out_date_","",table_2$event)
table_2$event <- gsub("_extended_follow_up","",table_2$event)

input$subgroup <- NULL
results <- results %>% left_join(input, by=c("event","cohort","model","time_points","term"))
results <- results %>% left_join(table_2, by=c("event","cohort","subgroup"))
results <- results %>% filter(!is.na(estimate))

results <- results %>% mutate(across(c(estimate, unexposed_person_days, unexposed_event_count, total_covid19_cases), as.numeric))

#Determine which analyses have a complete set of results so that AER can be calculated
df <- results %>% select(event, subgroup, model, cohort, time_points) %>% distinct
active_available <- merge(active,df)
active_unavailable <- active %>% anti_join(active_available)
rm(active,df,input)

input <- results
rm(results)

#-------------------------Run AER function--------------------------------------
lapply(split(active_available,seq(nrow(active_available))),
       function(active_available)
         excess_risk(   
           event_of_interest = active_available$event,
           cohort_of_interest = active_available$cohort,
           model_of_interest = active_available$model,
           subgroup_of_interest = active_available$subgroup,
           time_point_of_interest = active_available$time_points,
           input))


#------------------------------Compile the results------------------------------
AER_files=list.files(path = aer_raw_output_dir, pattern = "lifetable_")
AER_files=paste0(aer_raw_output_dir,"/",AER_files)
AER_compiled_results <- purrr::pmap(list(AER_files),
                                    function(fpath){
                                      df <- fread(fpath)
                                      return(df)})
AER_compiled_results=rbindlist(AER_compiled_results, fill=TRUE)

# Calculate overall AER
AER_combined <- AER_compiled_results %>% select(days, event, cohort, subgroup, time_points, excess_risk, AER)
table_2 <- table_2 %>% select(event, cohort, subgroup, N_population_size)

#Standardize AER and use pre-vax subgroup sizes for all cohorts
table_2 <- table_2 %>% filter(cohort == "pre_vaccination") %>% select(!cohort)
AER_combined <- AER_combined %>% left_join(table_2, by=c("event","subgroup"))

AER_combined_overall <- AER_combined %>% filter(!is.na(excess_risk))

AER_combined_overall <- AER_combined_overall %>% 
  group_by(days, event, cohort,time_points) %>%
  mutate(weight = N_population_size/sum(N_population_size))

AER_combined_overall <- AER_combined_overall %>% 
  dplyr::group_by(days, event, cohort,time_points) %>%
  dplyr::summarise(weighted_mean = weighted.mean(excess_risk,weight))


AER_combined_overall <- AER_combined_overall %>% dplyr::rename(excess_risk = weighted_mean )
AER_combined_overall$subgroup <- "aer_overall"
AER_combined_overall$AER <- NA
AER_combined$N_population_size <- NULL

AER_combined_overall <- rbind(AER_combined,AER_combined_overall)

write.csv(AER_combined_overall, paste0(aer_compiled_output_dir,"/AER_compiled_results.csv"), row.names = F)
