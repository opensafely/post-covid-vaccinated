## =============================================================================
## Project:     Vaccinated delta wave population study
##
## Purpose:  
##  Apply stage 5. Absolute excess risk analysis
##  - Create a function to calculate absolute excess risks
## 
## Authors: Lucy Teece (adapted from RT, XJ, VW)
## Reviewer: Genevieve Cezard
## 
## TO RUN OUTSIDE OPENSAFELY PLATFORM
## Content: 
## 0. Clean input data and subset to relevant event / cohort
## 1. Average daily incidence of each outcome in unexposed age/sex subgroups
## 2. Life table approach to calculate cumulative risk over time
## 3. Carry over time-varying hazard ratios
## 4. Daily event incidence in exposed age/sex subgroups
## 5. Daily excess risk between exposed and unexposed age/sex subgroups
## 6. Absolute excess risk weighted by total number of COVID cases
## 7. total absolute excess risk as culmination of age/sex subgroups
## =============================================================================

# # CODE FOR SINGLE AER
# event_of_interest="ate"
# cohort_of_interest="pre_vaccination"
# model_of_interest="mdl_max_adj"     #Should always be maximum adjusted model
# subgroup_of_interest="aer_Female_40_59"

excess_risk <- function(event_of_interest, cohort_of_interest, model_of_interest,subgroup_of_interest, input) {
  
  
  #-------------------------Check structure the input---------------------------
  input <- input %>% mutate(across(c("estimate_main","estimate_subgroup","unexposed_person_days","unexposed_event_count","total_covid19_cases"), as.numeric))
  input <- as.data.frame(input)
  
  #---------------------------------Subset to relevant data---------------------
  input <- input[input$event == event_of_interest
                 & input$model == model_of_interest
                 & input$cohort == cohort_of_interest
                 & input$subgroup == subgroup_of_interest,]
  
  #----Add start and end days for time periods which are needed for lifetable---
  for(i in c("time_period_start","time_period_end")){
    input[,i] <- input$term
    input[,i] <- gsub("days", "",input[,i])                                     #Remove 'days'
  }
  
  input$time_period_start <- gsub("\\_.*", "",input[,i])                        #Remove everything after _
  input$time_period_end <- gsub(".*_", "",input[,i])                            #Remove everything before _
  input <- input %>% mutate(across(c("time_period_start", "time_period_end"), as.numeric))
  
  
  #---------------------------------------------------------------
  # LIFETABLE APPROACH
  #---------------------------------------------------------------
  lifetable <- data.frame(c(0:(max(input$time_period_end)-1)))
  
  colnames(lifetable) <- c("days")
  lifetable$event <- event_of_interest
  lifetable$cohort <- cohort_of_interest
  lifetable$model <- model_of_interest
  lifetable$subgroup <- subgroup_of_interest
  
  
  #Step1. Calculate average daily incidence of outcome in unexposed age/sex subgroups
  #As number of events (in the unexposed) divided by time (days)
  lifetable$incidence_unexp <- input$unexposed_event_count[1] / input$unexposed_person_days[1]
 
  #Step2. Use life table approach to calculate cumulative risk over time in unexposed age/sex subgroups
  lifetable$cumulative_survival_unexp <- cumprod(1 - lifetable$incidence_unexp) 
  
  #Step3. Carry over maximally adjusted hazard ratio estimates (time varying) for event after COVID exposure
  lifetable$hr_main <- NA
  
  for(i in 1:nrow(input)){
    tmp <- input[i,]
    lifetable$hr_main <- ifelse(lifetable$days >= tmp$time_period_start & lifetable$days < tmp$time_period_end, tmp$estimate_main, lifetable$hr_main)
  }
  
  lifetable$hr_subgroup <- NA
  
  for(i in 1:nrow(input)){
    tmp <- input[i,]
    lifetable$hr_subgroup <- ifelse(lifetable$days >= tmp$time_period_start & lifetable$days < tmp$time_period_end, tmp$estimate_subgroup, lifetable$hr_subgroup)
  }
  
  #Occasionally HR is redacted, is so set to null
  # These have been removed at an earlier stage
  
  #lifetable$hr <- ifelse(lifetable$hr=="[Redacted]",NA,lifetable$hr)
  #lifetable$hr <- as.numeric(lifetable$hr)
  
  #Step4. Calculate predicted incidence of outcome after COVID exposure in age/sex subgroups
  #Multiply daily incidence in unexposed by maximally adjusted hazard ratios (time varying) for event after COVID exposure
  lifetable$cumulative_survival_exp_main <- cumprod(1 - (lifetable$hr_main * lifetable$incidence_unexp))
  lifetable$cumulative_survival_exp_subgroup <- cumprod(1 - (lifetable$hr_subgroup * lifetable$incidence_unexp))
  
  #Step5. Calculate daily excess risk
  #As difference in cumulative survival unexposed and expected cumulative survival in unexposed
  lifetable$excess_risk_main <- lifetable$cumulative_survival_unexp - lifetable$cumulative_survival_exp_main
  lifetable$excess_risk_subgroup <- lifetable$cumulative_survival_unexp - lifetable$cumulative_survival_exp_subgroup
  
  #Step6. Carry over total number of COVID cases in age/sex subgroup, calculate subgroup-specific absolute excess risk
  lifetable$total_covid19_cases <- input$total_covid19_cases[1]
  lifetable$AER_main <- lifetable$excess_risk_main * lifetable$total_covid19_cases
  lifetable$AER_subgroup <- lifetable$excess_risk_subgroup * lifetable$total_covid19_cases

  #Step7. Total AER as total of all subgroups

  #lifetable$total_AER = rowSums(select(lifetable, ends_with("_AER")))
  
  # write output for AER figure
  write.csv(lifetable, file=paste0(aer_raw_output_dir, "/lifetable_" ,event_of_interest, "_" ,subgroup_of_interest, "_",cohort_of_interest, ".csv"), row.names = F)

  
  #AER_followup_end <- lifetable[nrow(lifetable),]$total_AER
  
  # results <- data.frame(event=event_of_interest,
  #                       cohort=cohort_of_interest,
  #                       model=model_of_interest,
  #                       AER_followup_end=AER_followup_end)
  
  #write.csv(results, paste0(aer_raw_results_dir, "/AER_raw_results_", cohort_of_interest, "_", model_of_interest, "_", event_of_interest,".csv"), row.names = F)
  #return(results)
}
