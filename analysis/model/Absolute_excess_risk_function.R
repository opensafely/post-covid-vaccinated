#CREATE A FUNCTION TO CALCULATE THE EXCESS RISK
excess_risk <- function(event_of_interest, cohort_of_interest, subgroup_of_interest, model_of_interest, input) {
  
  
  #-------------------------Check structure the input---------------------------
  input <- input %>% mutate(across(c("total_covid19_cases", "unexposed_person_days","unexposed_event_count"), as.numeric))
  input <- as.data.frame(input)
  
  #---------------------------------Subset to relevant data---------------------
  input <- input[input$event == event_of_interest & input$model == model_of_interest  & 
                   input$cohort == cohort_of_interest & input$subgroup == subgroup_of_interest,]
  
  #----Add start and end days for time periods which are needed for lifetable---
  for(i in c("time_period_start","time_period_end")){
    input[,i] <- input$term
    input[,i] <- gsub("days", "",input[,i])#Remove 'days'
  }
  
  input$time_period_start <- gsub("\\_.*", "",input[,i])#Remove everything after _
  input$time_period_end <- gsub(".*_", "",input[,i])#Remove everything before _
  input <- input %>% mutate(across(c("time_period_start", "time_period_end"), as.numeric))
  
  
  #--------------------------------------
  # Step1: Extract the required variables
  #--------------------------------------
  #1. Person days - 
  fp_person_days <- input$unexposed_person_days[1]
  
  #2.unexposed events
  unexposed_events <- input$unexposed_event_count[1]
  
  #3.Total cases
  total_cases <-  input$total_covid19_cases[1]
  
  
  #-----------------------------------------------------
  #Step2.Average daily CVD incidence - in the unexposed
  #-----------------------------------------------------
  #Number of new events / sum of person-time at risk
  incidence_rate <- unexposed_events/fp_person_days
  #-------------------------------------------------------------
  #Step3. Make life table to calculate cumulative risk over time
  #-------------------------------------------------------------
  #Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, - with and without COVID-19. 
  lifetable <- data.frame(c(0:196))
  
  colnames(lifetable) <- c("days")
  lifetable$event <- event_of_interest
  lifetable$model <- model_of_interest
  lifetable$cohort <- cohort_of_interest
  lifetable$subgroup <- subgroup_of_interest 
  lifetable$q <- incidence_rate 
  lifetable$'1-q' <- 1 - lifetable$q 
  lifetable$s <- cumprod(lifetable$`1-q`)
  #-------------------------
  #Step4.Daily CVD incidence
  #-------------------------
  #Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
  # for that day to derive the incidence on each day after COVID-19.
  
  #assign the hr estimates
  lifetable$h <- 0
  for(i in 1:nrow(input)){
    tmp <- input[i,]
    lifetable$h <- ifelse(lifetable$days >= tmp$time_period_start & lifetable$days < tmp$time_period_end, tmp$estimate,lifetable$h)
  }
  
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
  
  #2.CI of the AER
  #Confidence Interval = Attributable risk +/- 1.96 x Square Root of [p x q (1/n1+ 1/n2)]
  #Where, p = qh, q = 1-qh, n1= unexposed person days, n2 = exposed person days
  #https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
  
  lifetable$CI <- 1.96*lifetable$qh*lifetable$'1-qh'*(1/fp_person_days + 1/fp_person_days)
  
  #3.AER%
  lifetable$AERp <-lifetable$'s-sc'*100
  
  #CI of AER%
  #95% CI = ARP +/- ARP x (C.I. range from the attributable risk / the attributable risk)
  #Where, ARP=AERp, CI range= CI, attributable risk = s-sc
  #https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
  
  lifetable$CIp <- lifetable$AERp*(lifetable$CI / lifetable$`s-sc`)
  lifetable$CIp.low <- lifetable$AERp - lifetable$CIp
  lifetable$CIp.high <- lifetable$AERp + lifetable$CIp
  
  #Save life table for AER figure
  write.csv(lifetable, paste0(aer_raw_results_dir, "/lifetable_" , cohort_of_interest, "_", model_of_interest, "_", event_of_interest, "_", subgroup_of_interest,".csv"), row.names = F)
  
  
  AER_196 <- lifetable[nrow(lifetable),]$'s-sc' * total_cases
  
  results <- data.frame(event=event_of_interest,
                        cohort=cohort_of_interest,
                        subgroup=subgroup_of_interest,
                        model=model_of_interest,
                        AER_196=AER_196)
  
  write.csv(results, paste0(aer_raw_results_dir, "/AER_raw_results_", cohort_of_interest, "_", model_of_interest, "_", subgroup_of_interest, "_", event_of_interest,".csv"), row.names = F)
  return(results)
  #return(print(results)) 
}
