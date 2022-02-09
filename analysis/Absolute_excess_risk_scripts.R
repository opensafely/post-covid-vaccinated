#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang, Venexia Walker

excess_risk <- function(cohort, event, agegp, sex, ethnicity, prior_history, hospitalisation) {
  
  #Import data 
  input1 <- readr::read_csv(paste0("output/Input1_AER_",cohort,".csv")) #1.person days
  input2 <- readr::read_csv(paste0("output/Input2_AER_",cohort,".csv")) #2.unexposed events, 3.total cases, 4.hr
  #rm(Input1_AER, Input2_AER)
  
  #---------------------------------
  # Step1: Extract the required variables
  #---------------------------------
  #1. Person days
  fp_person_days <- input1[input1$event == event & 
                             input1$agegp == agegp &
                             input1$sex == sex & 
                             input1$ethnicity == ethnicity &
                             input1$prior_history == prior_history & 
                             input1$hospitalisation == hospitalisation,]$person_days
  
  #2.unexposed events
  unexposed_events <-  input2[input2$event == event & 
                                input2$agegp == agegp &
                                input2$sex == sex & 
                                input2$ethnicity == ethnicity &
                                input2$prior_history == prior_history &
                                input2$hospitalisation == hospitalisation & 
                                input2$term == "0_14 days",]$unexposed_cvd_events
  
  #3.Total cases
  total_cases <-  input2[input2$event == event & 
                           input2$agegp == agegp &
                           input2$sex == sex & 
                           input2$ethnicity == ethnicity &
                           input2$prior_history == prior_history &
                           input2$hospitalisation == hospitalisation & 
                           input2$term == "0_14 days",]$total_covid19_cases
  
  #4.locate the estimates
  #0-14 days
  hr_14 <- input2[input2$event == event & 
                    input2$agegp == agegp &
                    input2$sex == sex & 
                    input2$ethnicity == ethnicity &
                    input2$prior_history == prior_history &
                    input2$hospitalisation == hospitalisation & 
                    input2$term == "0_14 days",]$hr_fully_adjusted
  
  #14-28 days
  hr_28 <- input2[input2$event == event & 
                    input2$agegp == agegp &
                    input2$sex == sex & 
                    input2$ethnicity == ethnicity &
                    input2$prior_history == prior_history &
                    input2$hospitalisation == hospitalisation & 
                    input2$term == "14_28 days",]$hr_fully_adjusted
  
  #28-56 days
  hr_56 <- input2[input2$event == event & 
                    input2$agegp == agegp &
                    input2$sex == sex & 
                    input2$ethnicity == ethnicity &
                    input2$prior_history == prior_history &
                    input2$hospitalisation == hospitalisation & 
                    input2$term == "28_56 days",]$hr_fully_adjusted
  
  #56-84 days
  hr_84 <- input2[input2$event == event & 
                    input2$agegp == agegp &
                    input2$sex == sex & 
                    input2$ethnicity == ethnicity &
                    input2$prior_history == prior_history &
                    input2$hospitalisation == hospitalisation & 
                    input2$term == "56_84 days",]$hr_fully_adjusted
  
  #84-196 days
  hr_196 <- input2[input2$event == event & 
                     input2$agegp == agegp &
                     input2$sex == sex & 
                     input2$ethnicity == ethnicity &
                     input2$prior_history == prior_history &
                     input2$hospitalisation == hospitalisation & 
                     input2$term == "84_196 days",]$hr_fully_adjusted
  
  #Alternative 0-28 days
  hr0_28 <- input2[input2$event == event & 
                     input2$agegp == agegp &
                     input2$sex == sex & 
                     input2$ethnicity == ethnicity &
                     input2$prior_history == prior_history &
                     input2$hospitalisation == hospitalisation & 
                     input2$term == "0_28 days",]$hr_fully_adjusted
  
  #Alternative 28 - 196 days
  hr28_196 <- input2[input2$event == event & 
                       input2$agegp == agegp &
                       input2$sex == sex & 
                       input2$ethnicity == ethnicity &
                       input2$prior_history == prior_history &
                       input2$hospitalisation == hospitalisation & 
                       input2$term == "28_196 days",]$hr_fully_adjusted
  
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
  results <- data.frame(cohort = cohort,
                        event = event,
                        agegp = agegp,
                        sex = sex,
                        ethnicity = ethnicity,
                        prior_history = prior_history,
                        hospitalisation = hospitalisation,
                        AER_196 = lifetable[nrow(lifetable),]$AER * total_cases,
                        stringsAsFactors = FALSE)
  
  return(results)
  
}