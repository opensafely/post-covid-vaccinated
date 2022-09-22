#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang, Venexia Walker, Lucy Teece
#Reviewer: Genevieve Cezard

#TO RUN OUTSIDE OPENSAFELY
# 1. load the right input data and make sure of the file names and variable structure
# 2. Cntrl+A run the whole script and find the results in working directory

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
aer_raw_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/raw_results/"
aer_compiled_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/compiled_results/"
scripts_dir <- "analysis/model"

dir.create(file.path(aer_raw_output_dir), recursive =TRUE, showWarnings = FALSE)
dir.create(file.path(aer_compiled_output_dir), recursive =TRUE, showWarnings = FALSE)

# fs::dir_create(here::here("output", "review", "AER_results"))
# fs::dir_create(here::here("output", "not-for-review", "AER_results"))
# 
# hr_dir <- "output/review/model"
# table_2_dir <- "output/review/descriptives"
# aer_raw_results_dir <- "output/not-for-review/AER_results"
# aer_results_dir <- "output/review/AER_results"

#-------------------------Call AER function-------------------------------------
source(file.path(scripts_dir,"Absolute_excess_risk_function.R"))

#USE - TO CHECK SINGLE AER
# event_of_interest="ate"
# cohort_of_interest="vaccinated"
# subgroup_of_interest="covid_pheno_non_hospitalised"
# model_of_interest="mdl_max_adj"

library(purrr)
library(data.table)
library(tidyverse)

#-------------------------------
#Step 1. AER for active analyses
#-------------------------------
#1. Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")                             
active <- active[active$active==TRUE,]   

#Preprocess the active analyses
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

#Focus only on aer analyses
active <- active %>% filter(startsWith(subgroup,"aer_") & model=="mdl_max_adj")

#Add HR timepoint terms 
active_reduced <- crossing(active, c("days0_28","days28_197","days197_535"))
colnames(active_reduced) <- c("event", "subgroup", "model", "cohort","term")
active_reduced$time_points <- "reduced"

active_normal <- crossing(active, c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_535"))  
colnames(active_normal) <- c("event", "subgroup", "model", "cohort","term")
active_normal$time_points <- "normal"
results <- rbind(active_reduced,active_normal)

rm(active_reduced,active_normal)
#----------------------
#Step 2. Load results
#----------------------
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

                                                            
#-------------------Select required columns and term----------------------------
input <- input %>% 
  filter(str_detect(term, "^days")
         & (startsWith(subgroup, "aer_") | subgroup == "main")
         & model=="mdl_max_adj"
         & results_fitted == "fitted_successfully"
         & estimate != "[Redacted]") %>%
  select(event,cohort,subgroup,model,time_points,term,estimate)
  

#Focus for aer analyses
#input <- input %>% filter(startsWith(subgroup, "aer_"))                             
#input <- subset(input, input$model=="mdl_max_adj")                              # only require aer for full model
#input <- input %>% select(-c("subgroup","total_covid19_cases"))                 # duplicate variables also available in Table 2 (more relevent)

#---------------------------------Input Table 2---------------------------------
table2_pre_vax <- read.csv(paste0(results_dir,"table2_pre_vaccination_cvd.csv"))
table2_vax <- read.csv(paste0(results_dir,"table2_vaccinated.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_electively_unvaccinated.csv"))

table2_pre_vax <- rename(table2_pre_vax, cohort = cohort_name)
table2_vax <- rename(table2_vax, cohort = cohort_to_run)
table2_unvax <- rename(table2_unvax, cohort = cohort_to_run)

table_2 <- rbind(table2_pre_vax, table2_vax,table2_unvax)
rm(table2_pre_vax,table2_vax,table2_unvax)

#-------------------Select required columns and term----------------------------
#Focus for aer analyses
table_2 <- table_2 %>% select(subgroup, event, cohort,unexposed_person_days,unexposed_event_count,total_covid19_cases) %>%
                      filter(startsWith(subgroup, "aer_"))
            
table_2$event <- gsub("out_date_","",table_2$event)

# # Non-hospitalised/hospitalised unexposed person days are the same as in the
# # main analysis so copy these values and add onto table
# 
# for(i in unique(table_2$event)){
#   tmp <- table_2 %>% filter(event==i & cohort=="vaccinated" & subgroup=="covid_pheno_non_hospitalised")
#   if(nrow(tmp)>0){
#     table_2[nrow(table_2)+1,] <- c("main", tmp[1,2:5])
#   }
#   
#   tmp <- table_2 %>% filter(event==i & cohort=="electively_unvaccinated" & subgroup=="covid_pheno_non_hospitalised")
#   if(nrow(tmp)>0){
#     table_2[nrow(table_2)+1,] <- c("main", tmp[1,2:5])
#   }
# }


tmp <- input %>% filter(subgroup == "main")
tmp$subgroup <- NULL
results <- results %>% left_join(tmp, by=c("event","cohort","model","time_points","term"))
results <- results %>% rename(estimate_main = estimate)

tmp <- input %>% filter(subgroup != "main")
results <- results %>% left_join(tmp, by=c("event","cohort","model","time_points","term","subgroup"))
results <- results %>% rename(estimate_subgroup = estimate)

#Remove any rows where both HR estimates are NA
results <- results %>% filter(!is.na(estimate_main) | !is.na(estimate_subgroup))

results <- results %>% left_join(table_2, by=c("event","cohort","subgroup"))


results <- results %>% mutate(across(c(estimate_main,estimate_subgroup, unexposed_person_days, unexposed_event_count, total_covid19_cases), as.numeric))
#input <- input %>% filter(!is.na(unexposed_person_days) & unexposed_event_count != "[Redacted]")

#Determine which analyses have a complete set of results so that AER can be calculated
df <- results %>% select(event, subgroup, model, cohort) %>% distinct
active_available <- merge(active,df)
active_unavailable <- active %>% anti_join(active_available)
rm(active,table_2,df,input,tmp)

input <- results
rm(results)

input <- input %>% filter(time_points == "reduced")
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
#-------------------------
#Step 3. Run AER function
#-------------------------

lapply(split(active_available,seq(nrow(active_available))),
       function(active_available)
         excess_risk(   
           event_of_interest = active_available$event,
           cohort_of_interest = active_available$cohort,
           model_of_interest = active_available$model,
           subgroup_of_interest = active_available$subgroup,
           input))


#----------------------
#Step 4. Compile results
#----------------------
#------------------------------Compile the results------------------------------
AER_files=list.files(path = aer_raw_output_dir, pattern = "lifetable_")
AER_files=paste0(aer_raw_output_dir,"/",AER_files)
AER_compiled_results <- purrr::pmap(list(AER_files),
                                    function(fpath){
                                      df <- fread(fpath)
                                      return(df)})
AER_compiled_results=rbindlist(AER_compiled_results, fill=TRUE)
write.csv(AER_compiled_results, paste0(aer_compiled_output_dir,"/AER_compiled_results.csv"), row.names = F)


#--------------------------Compile results for AER figure-----------------------

# lt_files=list.files(path = aer_raw_results_dir, pattern = "lifetable_*")
# lt_files=paste0(aer_raw_results_dir,"/",lt_files)
# compiled_lifetables <- purrr::pmap(list(lt_files),
#                                    function(fpath){
#                                      df <- fread(fpath)
#                                      return(df)
#                                    })
# compiled_lifetables=rbindlist(compiled_lifetables, fill=TRUE)
# 
# #3.output the csv
# write.csv(compiled_lifetables, paste0(aer_results_dir,"/Figure4_compiled_lifetables.csv"), row.names = F)



#3.Clear the folder(except compiled results)
#if (file.exists(AER_files)) { file.remove(AER_files)}
#4.Sample the results
#print(AER_compiled_results)                                                      #-ve AERs not expected with actual data, but possible.                                                    
#table(AER_compiled_results$AER_196<0)                                            #264 obs with 5 variables as per active analysis list.

