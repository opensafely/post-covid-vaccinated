
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

library(purrr)
library(data.table)
library(tidyverse)


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
term <- c("days0_28","days28_197","days197_535")
active_reduced <- crossing(active[active$time_points == "reduced",],term )

term <- c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_535")
active_normal <- crossing(active[active$time_points == "normal",],term )  

results <- rbind(active_reduced,active_normal)

rm(active_reduced,active_normal)

#------------------------------------ Load results------------------------------

#-----------------------------Input hazard ratios-------------------------------
hr_files=list.files(path = results_dir, pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
input <- rbindlist(hr_file_paths, fill=TRUE)

# Read in stata ouptut

tmp <- read.csv(paste0(results_dir, "/stata_output_formatted"))
tmp <- tmp %>% select(intersect(colnames(input),colnames(tmp)))
input <- rbind(input, tmp, fill = TRUE)
rm(tmp)

#-------------------Select required columns and term----------------------------
input <- input %>% 
  filter(str_detect(term, "^days")
         & (startsWith(subgroup, "aer_") | subgroup == "main")
         & model=="mdl_max_adj"
         & results_fitted == "fitted_successfully"
         & estimate != "[Redacted]") %>%
  select(event,cohort,subgroup,model,time_points,term,estimate)
  
#---------------------------------Input Table 2---------------------------------
table2_pre_vax <- read.csv(paste0(results_dir,"table2_pre_vaccination_cvd.csv"))
table2_vax <- read.csv(paste0(results_dir,"table2_vaccinated.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_electively_unvaccinated.csv"))

table2_pre_vax <- dplyr::rename(table2_pre_vax, cohort = cohort_name)
table2_vax <- dplyr::rename(table2_vax, cohort = cohort_to_run)
table2_unvax <- dplyr::rename(table2_unvax, cohort = cohort_to_run)

table_2 <- rbind(table2_pre_vax, table2_vax,table2_unvax)
rm(table2_pre_vax,table2_vax,table2_unvax)

#-------------------Select required columns and term----------------------------

table_2 <- table_2 %>% select(subgroup, event, cohort,unexposed_person_days,unexposed_event_count,total_covid19_cases, N_population_size) %>%
                      filter(startsWith(subgroup, "aer_"))
            
table_2$event <- gsub("out_date_","",table_2$event)

# Split HR results into age/sex subgroups and overall main results
# We calculate AER using both types of HR currently to see the difference and will later decide which HRs to use

tmp <- input %>% filter(subgroup == "main")
tmp$subgroup <- NULL
results <- results %>% left_join(tmp, by=c("event","cohort","model","time_points","term"))
results <- results %>% dplyr::rename(estimate_main = estimate)

tmp <- input %>% filter(subgroup != "main")
results <- results %>% left_join(tmp, by=c("event","cohort","model","time_points","term","subgroup"))
results <- results %>% dplyr::rename(estimate_subgroup = estimate)

#Remove any rows where both HR estimates are NA
results <- results %>% filter(!is.na(estimate_main) | !is.na(estimate_subgroup))

results <- results %>% left_join(table_2, by=c("event","cohort","subgroup"))


results <- results %>% mutate(across(c(estimate_main,estimate_subgroup, unexposed_person_days, unexposed_event_count, total_covid19_cases), as.numeric))

#Determine which analyses have a complete set of results so that AER can be calculated
df <- results %>% select(event, subgroup, model, cohort, time_points) %>% distinct
active_available <- merge(active,df)
active_unavailable <- active %>% anti_join(active_available)
rm(active,df,input,tmp)

input <- results
rm(results)

#Need to update once we've decided if we're using age/sex subgroups HRs or overall HRs
# Some events have results for both normal and reduced time points
# Where possible use normal time points, otherwise used reduced

# input <- input %>%
#   group_by(event,subgroup,cohort,time_points) %>%
#   dplyr::mutate(time_points_to_use = case_when(
#     any(time_points == "normal") ~ "normal",
#     TRUE ~ "reduced"))
# 
# input <- input %>% filter(time_points == time_points_to_use) %>%
#                   select(-time_points_to_use)
# event_of_interest = active_available$event[1]
# cohort_of_interest = active_available$cohort[1]
# model_of_interest = active_available$model[1]
# subgroup_of_interest = active_available$subgroup[1]
# time_point_of_interest = active_available$time_points[1]
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
AER_combined <- AER_compiled_results %>% select(days, event, cohort, subgroup, time_points, excess_risk_main, excess_risk_subgroup)
table_2 <- table_2 %>% select(event, cohort, subgroup, N_population_size)

AER_combined <- AER_combined %>% left_join(table_2, by=c("event","cohort","subgroup"))

AER_combined_overall <- AER_combined %>% filter(!is.na(excess_risk_main))
AER_combined_subgroup <- AER_combined %>% filter(!is.na(excess_risk_subgroup))

AER_combined_overall <- AER_combined_overall %>% 
  group_by(days, event, cohort,time_points) %>%
  mutate(weight = N_population_size/sum(N_population_size))

AER_combined_overall <- AER_combined_overall %>% 
  dplyr::group_by(days, event, cohort,time_points) %>%
  dplyr::summarise(weighted_mean = weighted.mean(excess_risk_main,weight))

AER_combined_subgroup <- AER_combined_subgroup %>% 
  group_by(days, event, cohort,time_points) %>%
  mutate(weight = N_population_size/sum(N_population_size))

AER_combined_subgroup <- AER_combined_subgroup %>% 
  dplyr::group_by(days, event, cohort,time_points) %>%
  dplyr::summarise(weighted_mean = weighted.mean(excess_risk_subgroup,weight))

#Join all results together 
AER_combined_overall$subgroup <- "aer_overall"
AER_combined_subgroup$subgroup <- "aer_overall"

AER_combined_overall <- AER_combined_overall %>% dplyr::rename(excess_risk_main = weighted_mean )
AER_combined_subgroup <- AER_combined_subgroup %>% dplyr::rename(excess_risk_subgroup = weighted_mean )

AER_compiled_results <- rbind(AER_compiled_results,AER_combined_overall,AER_combined_subgroup, fill = TRUE)

write.csv(AER_compiled_results, paste0(aer_compiled_output_dir,"/AER_compiled_results.csv"), row.names = F)
