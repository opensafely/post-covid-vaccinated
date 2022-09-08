#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang, Venexia Walker, Lucy Teece
#Reviewer: Genevieve Cezard

#TO RUN OUTSIDE OPENSAFELY
# 1. load the right input data and make sure of the file names and variable structure
# 2. Cntrl+A run the whole script and find the results in working directory

fs::dir_create(here::here("output", "review", "AER_results"))
fs::dir_create(here::here("output", "not-for-review", "AER_results"))
scripts_dir <- "analysis/model"
hr_dir <- "output/review/model"
table_2_dir <- "output/review/descriptives"
aer_raw_results_dir <- "output/not-for-review/AER_results"
aer_results_dir <- "output/review/AER_results"

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

agelabels <- c("18_39", "40_59", "60_79", "80_110")

#-------------------------------
#Step 1. AER for active analyses
#-------------------------------
#1. Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")                             # selects active analyses
active <- active[active$active==TRUE,]   

#Preprocess the active analyses
active$event <- gsub("out_date_","",active$outcome_variable)                                     # refine event name                                                    
active[,c("active","outcome","outcome_variable","prior_history_var","covariates")] <- NULL       # removes un used columns
active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event","model","cohort")), 
                              names_to = "strata")                                               # converts to long data                                        
active <- active[active$value==TRUE, c("event","model","cohort","strata")]                       # refines to active models                         
active$model <- ifelse(active$model=="all","mdl_agesex;mdl_max_adj",active$model)                # includes 2 model types     
active <- tidyr::separate_rows(active, model, sep = ";")                                         # separate rows for each model
active$cohort <- ifelse(active$cohort=="all","vaccinated;electively_unvaccinated",active$cohort) # includes 2 cohorts
active <- tidyr::separate_rows(active, cohort, sep = ";")                                        # separate rows for each cohort

#Focus only on aer analyses
active <- subset(active, startsWith(active$strata,"aer_"))                      # keep only aer subgroups
active <- subset(active, active$model=="mdl_max_adj")                           # only require aer for full model

colnames(active) <- c("event","model","cohort","subgroup")
active <- active %>% select(-model, everything())                                                  #Order the columns 


#----------------------
#Step 2. Load results
#----------------------
#-----------------------------Input hazard ratios-------------------------------
hr_files=list.files(path = hr_dir, pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,"_to_release.csv")]
hr_files=paste0(hr_dir,"/",hr_files)
hr_files
input <- purrr::pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)})
input=rbindlist(input, fill=TRUE)

                                                            
#-------------------Select required columns and term----------------------------
input <- input %>% 
  select(-conf.low, -conf.high, -std.error,-robust.se, -P, -redacted_results) %>%
  filter(str_detect(term, "^days"))

#Focus for aer analyses
input <- subset(input, input$subgroup=="main")                                  # only aer in main analyses
input <- subset(input, input$model=="mdl_max_adj")                              # only require aer for full model
input <- input %>% select(-c("subgroup","total_covid19_cases"))                 # duplicate variables also available in Table 2 (more relevent)

#---------------------------------Input Table 2---------------------------------
table_2_vaccinated <- read_csv(paste0(table_2_dir,"/table2_vaccinated.csv"))
table_2_electively_unvaccinated <- read_csv(paste0(table_2_dir,"/table2_electively_unvaccinated.csv"))
table_2 <- rbind(table_2_vaccinated,table_2_electively_unvaccinated)

#-------------------Select required columns and term----------------------------
table_2 <- table_2 %>% select(subgroup, event, cohort_to_run,unexposed_person_days,unexposed_event_count,total_covid19_cases)
table_2$event <- gsub("out_date_","",table_2$event)
colnames(table_2)<- c("subgroup","event","cohort","unexposed_person_days","unexposed_event_count","total_covid19_cases")
rm(table_2_vaccinated,table_2_electively_unvaccinated)

#Focus for aer analyses
table_2 <- subset(table_2, startsWith(table_2$subgroup,"aer_"))                                       # keep only aer values

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

input <- input %>% left_join(table_2, by=c("event","cohort"))
input <- input %>% filter(!is.na(unexposed_person_days) & unexposed_event_count != "[Redacted]")

#Determine which analyses have a complete set of results so that AER can be calculated
df <- input %>% select(event, subgroup, model, cohort) %>% distinct
active_available <- merge(active,df)
results_unavailable <- active %>% anti_join(active_available)
rm(active,table_2,df)
# rm(tmp)

#-------------------------
#Step 3. Run AER function
#-------------------------

lapply(split(active_available,seq(nrow(active_available))),
       function(active_available)
         excess_risk(   
           event_of_interest = active_available$event,
           cohort_of_interest = active_available$cohort,
           model_of_interest = active_available$model,
           input))


#----------------------
#Step 4. Compile results
#----------------------
#------------------------------Compile the results------------------------------
AER_files=list.files(path = aer_raw_results_dir, pattern = "AER_raw_results_*")
AER_files=paste0(aer_raw_results_dir,"/",AER_files)
AER_compiled_results <- purrr::pmap(list(AER_files),
                                    function(fpath){
                                      df <- fread(fpath)
                                      return(df)})
AER_compiled_results=rbindlist(AER_compiled_results, fill=TRUE)
write.csv(AER_compiled_results, paste0(aer_results_dir,"/AER_compiled_results.csv"), row.names = F)


#--------------------------Compile results for AER figure-----------------------

lt_files=list.files(path = aer_raw_results_dir, pattern = "lifetable_*")
lt_files=paste0(aer_raw_results_dir,"/",lt_files)
compiled_lifetables <- purrr::pmap(list(lt_files),
                                   function(fpath){
                                     df <- fread(fpath)
                                     return(df)
                                   })
compiled_lifetables=rbindlist(compiled_lifetables, fill=TRUE)

#3.output the csv
write.csv(compiled_lifetables, paste0(aer_results_dir,"/Figure4_compiled_lifetables.csv"), row.names = F)



#3.Clear the folder(except compiled results)
#if (file.exists(AER_files)) { file.remove(AER_files)}
#4.Sample the results
#print(AER_compiled_results)                                                      #-ve AERs not expected with actual data, but possible.                                                    
#table(AER_compiled_results$AER_196<0)                                            #264 obs with 5 variables as per active analysis list.

