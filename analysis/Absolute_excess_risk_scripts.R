#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms

library(tidyverse)

data <- input_vaccinated

#-----------------------------------------------
#Step1.Calculate the average daily CVD incidence
#-----------------------------------------------
#Outcome1 - AMI
#Vaccinated cohort starts on 1/6/2021 and ends on 14/12/2021
#1.COHORT Dates
data$cohort_start <- as.Date("2021-06-01", format="%Y-%m-%d")
data$cohort_end <- as.Date("2021-12-14", format="%Y-%m-%d")

#2.Follow up period in vaccinated COHORT
#Index starts on the latest of: 1) cohort start date; 2)two weeks after the 2nd vaccination
data$fp_start <- pmax(data$cohort_start, data$vax_date_covid_2+15, na.rm = TRUE)
#Index ends on the earliest of: 1) Cohort end date; 2)CVD event; 3)death
data$fp_end <- pmin(data$out_date_ate+1, data$death_date+1, data$cohort_end, na.rm = TRUE)
#Index follow up period is the difference between follow up start and end.
data$fp_period <- data$fp_end - data$fp_start

#3.Person days or years
fp_person_days <- sum(data$fp_period)#  unit <- person days
print(fp_person_days) #Time difference of 210743 person-days
fp_person_years <- fp_person_days/365 #  unit <- person years
print(fp_person_years)#Time difference of 577.3781 person years

#4.Count events
sum(!is.na(data$death_date))# Total deaths
sum(!is.na(data$out_date_ate))# Total events
sum(!is.na(data$exp_date_covid19_confirmed))# Total COVID19 cases

#5.Incidence rate over the follow up period
#Number of new events / sum of person-time at risk

#5.1Numerator- Events in unexposed
ate_total <- sum(!is.na(data$out_date_ate))
print(ate_total)# Total events
ate_in_exposed <- sum(data$out_date_ate >= data$exp_date_covid19_confirmed, na.rm = T)
print(ate_in_exposed)#  Events in exposed
ate_in_unexposed <- ate_total - ate_in_exposed
print(ate_in_unexposed)#Events in unexposed

#5.2 Denominator- sum of person-time at risk
fp_person_days <- as.integer(fp_person_days)

#5.3 Incidence rate in unexposed
incidence_rate <- ate_in_unexposed/fp_person_days*100000
print(incidence_rate) # Incidence rate in unexposed per 100,000 person days follow up

#6. Method 2 (alternative) - Average daily incidence
#Event status in unexposed
data$ate_status_in_unexposed <- ifelse((data$out_date_ate < data$exp_date_covid19_confirmed) |
                                         (data$out_date_ate>0 & is.na(data$exp_date_covid19_confirmed)),1,0)
data$ate_status_in_unexposed[is.na(data$ate_status_in_unexposed)] <- 0
table(data$ate_status_in_unexposed, useNA = "ifany")

library(survival)
#Set the variables
fp_period <- data$fp_period # follw up time
ate_status_in_unexposed <- data$ate_status_in_unexposed #events in unexposed

#Null model
M0 <- survfit(Surv(fp_period, ate_status_in_unexposed)~1, data=data) 
M0

#Retrieve variables from the model
daily_incidence <- data.frame(time=M0$time, n_event=M0$n.event, n_risk=M0$n.risk )

#Daily incidence proportion and average daily incidence proportion
daily_incidence$incidence_proportion <- daily_incidence$n_event/daily_incidence$n_risk
mean(daily_incidence$incidence_proportion)*100000  # 90.4763 per 100,000 people 

#----------------------------------------
#Step2. Calculate the daily CVD incidence
#----------------------------------------
#Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
             # for that day to derive the incidence on each day after COVID-19. 

#-------------------------------------------------------------
#Step3. Make life table to calculate cumulative risk over time
#-------------------------------------------------------------
#Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, -
             # with and without COVID-19. 

#-----------------------------------------
#Step4. Calculate the Absolute excess risk
#-----------------------------------------
#Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
             #compared with no COVID-19 diagnosis. 

#------------------------------------------------
#Step5. Calculate the Overall Absolute excess risk
#-------------------------------------------------
#Description:Overall absolute excess risk is estimated from a weighted sum of the age and sex-specific excess risks, -
             #weighted by the proportions of people in age and sex strata within the COVID-19 infected population during the follow-up period.