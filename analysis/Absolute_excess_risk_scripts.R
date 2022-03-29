#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang, Venexia Walker
#Reviewer: Genevieve Cezard

#USE - TO CHECK SINGLE AER
outcome <- "ate" 
group <- "vaccinated" 
strata <- "prior_history_FALSE"
fit <- "mdl_max_adj"

library(purrr)
library(data.table)
library(tidyverse)

#CALCULATE THE EXCESS RISK
excess_risk <- function(outcome, group, strata, fit) {
  
  #Load data 
  #1.Input1 - 1.unexposed person days
  input1.1 <- readr::read_csv("output/input1_aer_vaccinated.csv")
  input1.2 <- readr::read_csv("output/input1_aer_electively_unvaccinated.csv") 
  
  #Preprocess input1                                                             #ADDS TWO MODEL FITS WITH SAME PERSON DAYS
  input1.3 <- rbind(input1.1,input1.2)                                           
  input1.3$fit <- "mdl_agesex"                                                   
  
  input1.4 <- input1.3                                                           
  input1.4$fit <- "mdl_max_adj"                                                  
  
  input1 <-rbind(input1.3,input1.4)                                              
  input1 <- input1 %>% select(-strata)                                           
  rm(input1.1, input1.2, input1.3, input1.4)
  
  #input2 - 2.unexposed events, 3.total population cases, 4.HR                   #COMBINES THE HR TABLES
  hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")
  hr_files=hr_files[endsWith(hr_files,".csv")]
  hr_files=paste0("output/",hr_files)
  input2 <- purrr::pmap(list(hr_files),
                        function(fpath){
                          df <- fread(fpath)
                          return(df)})
  input2=rbindlist(input2, fill=TRUE)
  #Preprocess input2                                                            #SELECTS REQUIRED COLUMNS&TERMS
  input2 <- input2 %>% select(-conf.low, -conf.high, -std.error,-robust.se, -P, -covariates_removed, -cat_covars_collapsed)
  input2 <- input2 %>% filter(term == "days0_14" |
                                term == "days14_28" |
                                term == "days28_56" |
                                term == "days56_84" |
                                term == "days84_197"|
                                term == "days0_28"|
                                term == "days28_197")
  #--------------------------------------
  # Step1: Extract the required variables
  #--------------------------------------
  #1. Person days
  fp_person_days <- input1[input1$event == outcome & input1$fit == fit  &
                             input1$cohort == group & input1$subgroup == strata,]$unexposed_person_days
  #2.unexposed events
  unexposed_events <- input2[input2$event == outcome & input2$model == fit  & 
                               input2$cohort == group & input2$subgroup == strata & 
                               input2$expo_week== "pre expo",]$events_total
  #3.Total cases
  total_cases <-  input2[input2$event == outcome & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata & 
                           input2$expo_week== "pre expo",]$total_covid19_cases
  #4.locate the estimates
  #0-14 days
  hr_14 <- input2[input2$event == outcome  & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata & input2$term == "days0_14",]$estimate
  #14-28 days
  hr_28 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days14_28",]$estimate
  #28-56 days
  hr_56 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days28_56",]$estimate
  #56-84 days
  hr_84 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days56_84",]$estimate
  #84-196 days
  hr_196 <- input2[input2$event == outcome & input2$model == fit  & 
                     input2$cohort == group & input2$subgroup == strata& input2$term == "days84_197",]$estimate
  #Alternative 0-28 days
  hr0_28 <- input2[input2$event == outcome  & input2$model == fit  & 
                     input2$cohort == group & input2$subgroup == strata& input2$term == "days0_28",]$estimate
  #Alternative 28_196 days
  hr28_196 <- input2[input2$event == outcome  & input2$model == fit  & 
                       input2$cohort == group & input2$subgroup == strata& input2$term == "days28_197",]$estimate
  #-----------------------------------------------------
  #Step2.Average daily CVD incidence - in the unexposed
  #-----------------------------------------------------
  #Number of new events / sum of person-time at risk
  incidence_rate <- unexposed_events/fp_person_days
  #-------------------------------------------------------------
  #Step3. Make life table to calculate cumulative risk over time
  #-------------------------------------------------------------
  #Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, - with and without COVID-19. 
  lifetable <- data.frame(c(1:196))
  colnames(lifetable) <- c("days")
  lifetable$event <- outcome
  lifetable$model <- fit
  lifetable$cohort <- group
  lifetable$subgroup <- strata 
  lifetable$q <- incidence_rate 
  lifetable$'1-q' <- 1 - lifetable$q 
  lifetable$s <- cumprod(lifetable$`1-q`)
  #-------------------------
  #Step4.Daily CVD incidence
  #-------------------------
  #Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
  # for that day to derive the incidence on each day after COVID-19.
  
  #assign the hr estimates
  lifetable$h <- ifelse(lifetable$days < 15, rep(hr_14),0)
  lifetable$h <- ifelse(lifetable$days > 14 & lifetable$days < 29, rep(hr_28),lifetable$h)
  lifetable$h <- ifelse(lifetable$days < 29 & is.na(lifetable$h), rep(hr0_28),lifetable$h)#alternative for 0-28 days
  
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 57, rep(hr_56),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 56 & lifetable$days < 85, rep(hr_84),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 84 & lifetable$days < 197, rep(hr_196),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 197 & is.na(lifetable$h), rep(hr28_196),lifetable$h)#alternative for 28-196 days
  
  lifetable$qh <- lifetable$q*lifetable$h
  lifetable$'1-qh' <- 1 - lifetable$qh
  lifetable$sc <- cumprod(lifetable$`1-qh`)
  #---------------------------
  #Step5. Absolute excess risk
  #---------------------------
  #Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
  #compared with no COVID-19 diagnosis. 
  #1.AER =difference in absolute risk
  lifetable$'s-sc' <- lifetable$s - lifetable$sc
  AER_196 <- lifetable[nrow(lifetable),]$'s-sc' * total_cases
  
  results <- data.frame(event=outcome,
                        cohort=group,
                        subgroup=strata,
                        model=fit,
                        AER_196=AER_196)
  write.csv(results, paste0("output/AER_" , group, "_", fit, "_", strata, "_", outcome,".csv"), row.names = F)
  return(results)
  #return(print(results)) 
}
#-------------------------------
#Step6. AER for active analyses
#-------------------------------
#1. Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")                             # selects active analyses
#active <- active_analyses # Manual alternative,
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

colnames(active)[colnames(active) == 'group'] <- 'strata'                                        #Tweaks the column names                   
colnames(active)[colnames(active) == 'cohort'] <- 'group'
colnames(active)[colnames(active) == 'model'] <- 'fit'
colnames(active)[colnames(active) == 'event'] <- 'outcome'
active <- active %>% select(-fit, everything())                                                  #Order the columns 

active <- active %>% filter(!strata == "ethnicity_Missing")                                      #Remove not available models                  
input1 <- input1 %>% filter(!subgroup== "ethnicity_Missing")                                     #matches above with input1

#----------------------
#Step7. Output results
#----------------------
#1.For Loop the function.
for (i in 1:nrow(active)) {excess_risk(active$outcome[i], active$group[i],active$strata[i], active$fit[i])}
#2.Compile the results
AER_files=list.files(path = "output", pattern = "AER_*")
AER_files=AER_files[endsWith(AER_files,".csv")]
AER_files=paste0("output/",AER_files)
AER_compiled_results <- purrr::pmap(list(AER_files),
                                    function(fpath){
                                      df <- fread(fpath)
                                      return(df)})
AER_compiled_results=rbindlist(AER_compiled_results, fill=TRUE)
write.csv(AER_compiled_results, "output/AER_compiled_results.csv", row.names = F)
#3.Clear the folder(except compiled results)
if (file.exists(AER_files)) { file.remove(AER_files)}
#4.Sample the results
print(AER_compiled_results) #-ve AERs not expected with actual data
table(AER_compiled_results$AER_196<0)