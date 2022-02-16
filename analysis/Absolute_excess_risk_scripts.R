#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang, Venexia Walker

#Preprocess the AER input data






excess_risk <- function(event, cohort, strata, model) {
  
  #Import data 
  input1 <- readr::read_csv("output/input1_aer.csv") #1.person days
  input2 <- readr::read_csv("output/input2_aer.csv") #2.unexposed events, 3.total cases, 4.hr
  
  #---------------------------------
  # Step1: Extract the required variables
  #---------------------------------
  #1. Person days
  fp_person_days <- input1[input1$event == event & input1$model == model &
                             input1$cohort == cohort & input1$strata == strata,]$person_days
  
  #2.unexposed events
  unexposed_events <-  subset(input2, input1$event == event & input1$model == model &
                                input1$cohort == cohort & input1$strata == strata)
  unexposed_events <-  as.numeric(unexposed_events$unexposed_events)#Indexing didn't work, but reconsider after real table.
  
  #3.Total cases
  total_cases <-  subset(input2, input1$event == event & input1$model == model &
                           input1$cohort == cohort & input1$strata == strata)
  total_cases <- as.numeric(total_cases$total_covid19_cases)
  
  #4.locate the estimates
  #0-14 days
  hr_14 <- input2[input2$event == event & 
                  input2$model == model & 
                  input2$cohort == cohort & 
                  input2$strata == strata & 
                  input2$term == "0_14 days",]$hr
  #14-28 days
  hr_28 <- input2[input2$event == event & 
                    input2$model == model & 
                    input2$cohort == cohort & 
                    input2$strata == strata & 
                    input2$term == "14_28 days",]$hr
  #28-56 days
  hr_56 <- input2[input2$event == event & 
                    input2$model == model & 
                    input2$cohort == cohort & 
                    input2$strata == strata & 
                    input2$term == "28_56 days",]$hr
  #56-84 days
  hr_84 <- input2[input2$event == event & 
                    input2$model == model & 
                    input2$cohort == cohort & 
                    input2$strata == strata & 
                    input2$term == "56_84 days",]$hr
  #84-196 days
  hr_196 <- input2[input2$event == event & 
                     input2$model == model & 
                     input2$cohort == cohort & 
                     input2$strata == strata & 
                     input2$term == "84_196 days",]$hr
  #Alternative 0-28 days
  hr0_28 <- input2[input2$event == event & 
                     input2$model == model & 
                     input2$cohort == cohort & 
                     input2$strata == strata & 
                     input2$term == "0_28 days",]$hr
  #Alternative 28 - 196 days
  hr28_196<- input2[input2$event == event & 
                      input2$model == model & 
                      input2$cohort == cohort & 
                      input2$strata == strata & 
                      input2$term == "28_196 days",]$hr
  #--------------------------------------------------------------------
  #Step2.Calculate the average daily CVD incidence   - in the unexposed
  #--------------------------------------------------------------------
  #Number of new events / sum of person-time at risk
  
  incidence_rate <- unexposed_events/fp_person_days
  
  #-------------------------------------------------------------
  #Step3. Make life table to calculate cumulative risk over time
  #-------------------------------------------------------------
  #Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, - with and without COVID-19. 
  lifetable <- data.frame(days = c(1:196),
                          q = incidence_rate,
                          stringsAsFactors = FALSE)
  
  lifetable$'1-q' <- 1 - lifetable$q 
  lifetable$s <- cumprod(lifetable$`1-q`)
  
  #----------------------------------------
  #Step4. Calculate the daily CVD incidence
  #----------------------------------------
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
  #-----------------------------------------
  #Step5. Calculate the Absolute excess risk
  #-----------------------------------------
  #Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
  #compared with no COVID-19 diagnosis. 
  
  #AER = Sc-S=difference in absolute risk
  lifetable$AER <- lifetable$sc - lifetable$s
  
  # Return results
  results <- data.frame(event = event,
                        cohort = cohort,
                        strata = strata,
                        model = model,
                        AER_196 = lifetable[nrow(lifetable),]$AER * total_cases,
                        stringsAsFactors = FALSE)
  
  return(results) 
  
}

# RT - add return the life table for - 'ate' and 'vte' for plotting
#Subgroups - 2. VACCINATED AND ELECTIVELY UNVACCINATED

#1.column1 - 196 days
#column2 - Cumulative AER(%)

              #2.AERp_ate_all

              #3.AERp_ate_male
              #4.AERp_ate_female 

              #5.AERp_ate_18_39
              #6.AERp_ate_40-59
              #7.AERp_ate_60-79
              #8.AERp_ate_80-110

              #9.AERp_ate_asian              ##additionals??
              #10.AERp_ate_black
              #11.AERp_ate_white
              #12.AERp_ate_mixed
              #13.AERp_ate_others

              #14.AERp_ate_history
              #15.AERp_ate_no_history

              #16.AERp_ate_hospitalised
              #17.AERp_ate_not_hospitalised

