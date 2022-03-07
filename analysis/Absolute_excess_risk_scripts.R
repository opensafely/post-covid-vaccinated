#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang, Venexia Walker

#use only to check the function
outcome <- "vte" 
group <- "vaccinated" 
strata <- "prior_history_FALSE"
fit <- "mdl_max_adj"

#Create the function
excess_risk <- function(outcome, group, strata, fit) {
  
  #Call the library
  library(purrr)
  library(data.table)
  library(tidyverse)
  
  #Load data 
  input1 <- readr::read_csv("output/input1_aer.csv") #1.person days
  #input2 <- readr::read_csv("output/input2_aer.csv") #2.unexposed events, 3.total cases, 4.hr
  #input2- Import data
  hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")
  hr_files=hr_files[endsWith(hr_files,".csv")]
  hr_files=paste0("output/",hr_files)
  input2 <- purrr::pmap(list(hr_files),
                        function(fpath){
                          df <- fread(fpath)
                          return(df)
                        })
  input2=rbindlist(input2, fill=TRUE)
  
  #Preprocess the input data (can be reduced when tuned in with real data inputs)
  input2 <- input2 %>% select(-conf.low, -conf.high, -std.error,-robust.se, -P, -covariates_removed, -cat_covars_collapsed)
  input2 <- input2 %>% filter(term == "days0_14" |
                                term == "days14_28" |
                                term == "days28_56" |
                                term == "days56_84" |
                                term == "days84_197"|
                                term == "days0_28"|
                                term == "days28_197")
  input1$strata[input1$strata =="sex_M"] <- "sex_Male"
  input1$strata[input1$strata =="sex_F"] <- "sex_Female"
  
  input1$strata[input1$strata =="ethnicity_1"] <- "ethnicity_White"
  input1$strata[input1$strata =="ethnicity_2"] <- "ethnicity_Black"
  input1$strata[input1$strata =="ethnicity_3"] <- "ethnicity_South_Asian"
  input1$strata[input1$strata =="ethnicity_4"] <- "ethnicity_Mixed"
  input1$strata[input1$strata =="ethnicity_5"] <- "ethnicity_Other"
  input1$strata[input1$strata =="ethnicity_6"] <- "ethnicity_Missing"
  
  input1$strata[input1$strata =="prior_history_true"] <- "prior_history_TRUE"
  input1$strata[input1$strata =="prior_history_false"] <- "prior_history_FALSE"
  #--------------------------------------
  # Step1: Extract the required variables
  #--------------------------------------
  #1. Person days
  fp_person_days <- input1[input1$event == outcome & input1$model == fit  &
                             input1$cohort == group & input1$strata == strata,]$person_days
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
  #--------------------------------------------------------------------
  #Step2.Calculate the average daily CVD incidence   - in the unexposed
  #--------------------------------------------------------------------
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
  
  #----------------------------------------
  #Step4. Calculate the daily CVD incidence
  #----------------------------------------
  #Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
  # for that day to derive the incidence on each day after COVID-19. 
  
  #1.assign the hr estimates
  lifetable$h <- ifelse(lifetable$days < 15, rep(hr_14),0)
  lifetable$h <- ifelse(lifetable$days > 14 & lifetable$days < 29, rep(hr_28),lifetable$h)
  lifetable$h <- ifelse(lifetable$days < 29 & is.na(lifetable$h), rep(hr0_28),lifetable$h)#alternative for 0-28 days
  
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 57, rep(hr_56),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 56 & lifetable$days < 85, rep(hr_84),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 84 & lifetable$days < 197, rep(hr_196),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 197 & is.na(lifetable$h), rep(hr28_196),lifetable$h)
  #alternative for 28-196 days
  
  #2.assign qh
  lifetable$qh <- lifetable$q*lifetable$h
  
  #3.assign 1-qh
  lifetable$'1-qh' <- 1 - lifetable$qh
  
  #4.assign sc
  lifetable$sc <- cumprod(lifetable$`1-qh`)
  #-----------------------------------------
  #Step5. Calculate the Absolute excess risk
  #-----------------------------------------
  #Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
  #compared with no COVID-19 diagnosis. 
  
  #1.AER =difference in absolute risk
  lifetable$'s-sc' <- lifetable$s - lifetable$sc
  
  #2.AER on day 196 
  AER_196 <- lifetable[nrow(lifetable),]$'s-sc' * total_cases
  print(AER_196) # 183.7275
  # 184 excess 'events' happens 196 days after 7558 total covid19 'cases'.
  

  # Return results
  results <- data.frame(event = outcome,
                        cohort = group,
                        subgroup = strata,
                        model = fit,
                        AER_196 = lifetable[nrow(lifetable),]$'s-sc' * total_cases,
                        stringsAsFactors = FALSE)
  
  return(results) 
  
  
}

#---------------------------------------------------------
#Step6. Run the function---FOR the active analyses----
#--------------------------------------------------------
#1. Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")
active <- active[active$active==TRUE,]
active$event <- gsub("out_date_","",active$outcome_variable)
active[,c("active","outcome","outcome_variable","prior_history_var","covariates")] <- NULL

active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event","model","cohort")), 
                              names_to = "strata")

active <- active[active$value==TRUE, c("event","model","cohort","strata")]

active$model <- ifelse(active$model=="all","mdl_agesex;mdl_max_adj",active$model)
active <- tidyr::separate_rows(active, model, sep = ";")

active$cohort <- ifelse(active$cohort=="all","vaccinated;electively_unvaccinated",active$cohort)
active <- tidyr::separate_rows(active, cohort, sep = ";")

#active$group <- gsub("_.*","",active$strata)
#active$group <- ifelse(active$group=="covid" & grepl("covid_history",active$strata), "covid_history", active$group)
#active$group <- ifelse(active$group=="covid" & grepl("covid_pheno",active$strata), "covid_pheno", active$group)
#active$group <- ifelse(active$group=="prior" & grepl("prior_history",active$strata), "prior_history", active$group)
#active <- unique(active[,c("event","model","cohort","group")])

#Preprocess to right outcomes, names and order
#active <- active[active$event %in% c("ate", "vte") & active$model %in% c("mdl_max_adj"),]

colnames(active)[colnames(active) == 'group'] <- 'strata'
colnames(active)[colnames(active) == 'cohort'] <- 'group'
colnames(active)[colnames(active) == 'model'] <- 'fit'
colnames(active)[colnames(active) == 'event'] <- 'outcome'

active <- active %>% select(-fit, everything())

#2. Loop to input the active analyses --- into the function.


for (i in 1:nrow(active)) {
  
  #files <- list.files(path = "output", pattern = "lifetable_*")
  #files <- files[grepl(active$strata[i])]
  
  excess_risk(active$outcome[i], active$group[i],active$strata[i], active$fit[i])
  AER_all_results <- results
  AER_all_results <- rbind(AER_all_results, results)
  rm(results)
  #write.csv(f4_compiled_lifetables, paste0("output/Figure4_compiled_lifetables.csv"), row.names = F)
}

#Delete file if it exists
if (file.exists(lt_files)) { file.remove(lt_files)}

69*196













#-------------------------------------------
#Step7. Output1 the csv
#-------------------------------------------

write.csv(AER_all_results, ("output/AER_all_results.csv"), row.names = F)



