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
## 1. Extract required variables
## 2. Average daily incidence of each outcome in unexposed age/sex subgroups
## 3. Create life table to calculate cumulative risk over time
## 4. Daily event incidence after multiplying by adjusted hazard ratio
## 5. Absolute excess risk calculation (difference between risk with/without covid)
## =============================================================================

# # CODE FOR SINGLE AER
# event_of_interest="ate"
# cohort_of_interest="vaccinated"
# model_of_interest="mdl_max_adj"     #Should always be maximum adjusted model

excess_risk <- function(event_of_interest, cohort_of_interest, model_of_interest, input) {
  
  
  #-------------------------Check structure the input---------------------------
  input <- input %>% mutate(across(c("unexposed_person_days","unexposed_event_count","total_covid19_cases"), as.numeric))
  input <- as.data.frame(input)
  
  #---------------------------------Subset to relevant data---------------------
  input <- input[input$event == event_of_interest & 
                   input$model == model_of_interest  & 
                   input$cohort == cohort_of_interest,]
  
  #----Add start and end days for time periods which are needed for lifetable---
  for(i in c("time_period_start","time_period_end")){
    input[,i] <- input$term
    input[,i] <- gsub("days", "",input[,i])                                     #Remove 'days'
  }
  
  input$time_period_start <- gsub("\\_.*", "",input[,i])                        #Remove everything after _
  input$time_period_end <- gsub(".*_", "",input[,i])                            #Remove everything before _
  input <- input %>% mutate(across(c("time_period_start", "time_period_end"), as.numeric))
  
  
  #---------------------------------------------------------------
  # LIFETABLE APPROACH - loop through each age/sex subgroup
  #---------------------------------------------------------------
  lifetable <- data.frame(c(0:max(input$time_period_end)))
  
  colnames(lifetable) <- c("days")
  lifetable$event <- event_of_interest

  for(l in c("Female","Male")){
    for(m in agelabels){
      
      #Step1. Calculate average daily incidence of outcome in unexposed age/sex subgroups
      #As number of events (in the unexposed) divided by time (days)
      lifetable[,paste0(l,"_",m,"_incidence_unexp")] <- input$unexposed_event_count[min(which(input$subgroup == paste0("aer_",l,"_",m)))] / input$unexposed_person_days[min(which(input$subgroup == paste0("aer_",l,"_",m)))]
      
      #Step2. Use life table approach to calculate cumulative risk over time in unexposed age/sex subgroups
      lifetable[,paste0(l,"_",m,"_cumulative_survival_unexp")] <- cumprod(1 - lifetable[,paste0(l,"_",m,"_incidence_unexp")]) 
      
      #Step3. Carry over maximally adjusted hazard ratio estimates (time varying) for event after COVID exposure
      lifetable[,paste0(l,"_",m,"_hr")] <- NA
      tmp1 <- input %>%filter(subgroup == paste0("aer_",l,"_",m))
      for(i in 1:nrow(tmp1)){
        tmp2 <- tmp1[i,]
        lifetable[,paste0(l,"_",m,"_hr")] <- ifelse(lifetable$days >= tmp2$time_period_start & lifetable$days <= tmp2$time_period_end, tmp2$estimate, lifetable[,paste0(l,"_",m,"_hr")])
      }
      
      #Occasionally HR is redacted, is so set to null
      lifetable[,paste0(l,"_",m,"_hr")] <- ifelse(lifetable[,paste0(l,"_",m,"_hr")]=="[Redacted]",NA,lifetable[,paste0(l,"_",m,"_hr")])
      lifetable[,paste0(l,"_",m,"_hr")] <- as.numeric(lifetable[,paste0(l,"_",m,"_hr")])

      #Step4. Calculate predicted incidence of outcome after COVID exposure in age/sex subgroups
      #Multiply daily incidence in unexposed by maximally adjusted hazard ratios (time varying) for event after COVID exposure
      lifetable[,paste0(l,"_",m,"_cumulative_survival_exp")] <- cumprod(1 - (lifetable[,paste0(l,"_",m,"_hr")] * lifetable[,paste0(l,"_",m,"_incidence_unexp")])) 
      
      #Step5. Calculate daily excess risk
      #As difference in cumulative survival unexposed and expected cumulative survival in unexposed
      lifetable[,paste0(l,"_",m,"_excess_risk")] <- lifetable[,paste0(l,"_",m,"_cumulative_survival_exp")] -lifetable[,paste0(l,"_",m,"_cumulative_survival_unexp")]
      
      #Step6. Carry over total number of COVID cases in age/sex subgroup, calculate subgroup-specific absolute excess risk
      lifetable[,paste0(l,"_",m,"_total_covid19_cases")] <- input$total_covid19_cases[min(which(input$subgroup == paste0("aer_",l,"_",m)))]
      lifetable[,paste0(l,"_",m,"_AER")] <- lifetable[,paste0(l,"_",m,"_excess_risk")] * lifetable[,paste0(l,"_",m,"_total_covid19_cases")] 
    }
  }

  #Step7. Total AER as total of all subgroups

  lifetable$total_AER = rowSums(select(lifetable, ends_with("_AER")))
  
  # write output for AER figure
  write.csv(lifetable, file=paste0(aer_raw_results_dir, "/lifetable_" , cohort_of_interest, "_", model_of_interest, "_", event_of_interest,".csv"), row.names = F)

  
  AER_followup_end <- lifetable[nrow(lifetable),]$total_AER
  
  results <- data.frame(event=event_of_interest,
                        cohort=cohort_of_interest,
                        model=model_of_interest,
                        AER_followup_end=AER_followup_end)
  
  write.csv(results, paste0(aer_raw_results_dir, "/AER_raw_results_", cohort_of_interest, "_", model_of_interest, "_", event_of_interest,".csv"), row.names = F)
  return(results)
  #return(print(results)) 
}
