#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang

library(tidyverse)
library(survival)

#OTHER ARTERIAL THROMBOTIC EVENTS
data <- (input_vaccinated[c("patient_id", "vax_date_covid_2",  "death_date", "out_date_ate", "exp_date_covid19_confirmed")])

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
#Index ends on the earliest of: 1) Cohort end date; 2)CVD event; 3)death; 4) COVID19 date
data$fp_end <- pmin(data$out_date_ate+1, data$death_date+1, data$exp_date_covid19_confirmed, data$cohort_end, na.rm = TRUE)#RT ELABORATE
#Index follow up period is the difference between follow up start and end.
data$fp_period <- data$fp_end - data$fp_start


#clean data(may not be required in actual data, but no harm anyway)
range(data$fp_period)#-239  to 196 days
data <- subset(data, data$fp_period < 197)
data <- subset(data, data$fp_period > 0)
#filter events after end of follow up period
table(data$out_date_ate > data$fp_end, useNA = "ifany")# 2,383 people had events after the end of follow up
table(data$out_date_ate == data$fp_end, useNA = "ifany") # 73 people had events at the end date of follow up
data <- subset(data, data$out_date_ate <= data$fp_end | is.na(data$out_date_ate))

#3.Person days or years
fp_person_days <- sum(data$fp_period)
print(fp_person_days) #Time difference in person-days
fp_person_years <- fp_person_days/365 
print(fp_person_years)#Time difference in person years

#4.Count events
sum(!is.na(data$death_date))# Total deaths
sum(!is.na(data$out_date_ate))# Total events
sum(!is.na(data$exp_date_covid19_confirmed))# Total COVID19 cases

#5.Incidence rate over the follow up period
#Number of new events / sum of person-time at risk

#Numerator- Events in unexposed
ate_total <- sum(!is.na(data$out_date_ate))
print(ate_total)# Total events
ate_in_exposed <- sum(data$out_date_ate >= data$exp_date_covid19_confirmed, na.rm = T)
print(ate_in_exposed)#  Events in exposed
ate_in_unexposed <- ate_total - ate_in_exposed
print(ate_in_unexposed)#Events in unexposed

#Denominator- sum of person-time at risk
fp_person_days <- as.integer(fp_person_days)

#Incidence rate in unexposed
incidence_rate <- ate_in_unexposed/fp_person_days*100000
print(incidence_rate) # Incidence rate in unexposed per 100,000 person days follow up

#6. Method 2 (alternative) - Average daily incidence


#Pre-process the variables
data$ate_in_unexposed <- ifelse((data$out_date_ate < data$exp_date_covid19_confirmed) |
                                         (data$out_date_ate>0 & is.na(data$exp_date_covid19_confirmed)),1,0)
data$ate_in_unexposed[is.na(data$ate_in_unexposed)] <- 0
table(data$ate_in_unexposed, useNA = "ifany")


#Assign the variables
fp_period <- data$fp_period # follow up time
ate_status_in_unexposed <- data$ate_in_unexposed #events in unexposed

#Null model
M0 <- survfit(Surv(fp_period, ate_status_in_unexposed)~1, data=data) 
M0

#Retrieve daily events from the model parameters
daily_incidence <- data.frame(time=M0$time, n_event=M0$n.event, n_risk=M0$n.risk )

#Daily incidence proportion and average daily incidence proportion
daily_incidence$incidence_proportion <- daily_incidence$n_event/daily_incidence$n_risk
#Mean= sum of events/sum of observations
mean <- sum(daily_incidence$incidence_proportion)/196
print(mean)#Average daily incidence - method 2
#mean(daily_incidence$incidence_proportion)*100000

#Create the life table
lifetable_ate <- as.data.frame(daily_incidence)

lifetable_ate$event <- "ARTERIAL THROMBOTIC EVENTS"
lifetable_ate$agegp <- "ALL"
lifetable_ate$sex <- "ALL"
lifetable_ate$days <- lifetable_ate$time
#q = estimated daily incidence of event
lifetable_ate$q <- 0.0009379283
lifetable_ate$'1-q' <- 1 - lifetable_ate$q 
lifetable_ate$s <- cumprod(lifetable_ate$`1-q`)
#h = maximally adjusted hazard ratios
hr <- compiled_hr_results_main_vaccinated_delta_mdl_max_adj_covid_history_false
hr_ate_28 <- subset(hr, hr$project == "vaccinated_delta" &  hr$event == "ate" &
                      hr$model == "mdl_max_adj" & hr$term == "days0_28")

hr_ate_196 <- subset(hr, hr$project == "vaccinated_delta" &  hr$event == "ate" &
                      hr$model == "mdl_max_adj" & hr$term == "days28_196")

lifetable_ate$h <- ifelse(lifetable_ate$days < 29, rep(hr_ate_28$estimate),0)
lifetable_ate$h <- ifelse(lifetable_ate$days > 28, rep(hr_ate_196$estimate),lifetable_ate$h)

lifetable_ate$qh <- lifetable_ate$q*lifetable_ate$h
lifetable_ate$'1-qh' <- 1 - lifetable_ate$qh
lifetable_ate$sc <- cumprod(lifetable_ate$`1-qh`)

#S-SC =difference in absolute risk

lifetable_ate$'sc-s' <- lifetable_ate$sc - lifetable_ate$s

#Sample results and interpretation.
9.887190e-02*100 #the changes in absolute risk of ATE after exposed to COVID19 on day 196 (28 weeks) would be 9.88%
9.887190e-02*188 # 18.58792 more ate events 28 weeks after 188 covid19 events - in the vaccinated delta population.


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

#plotting
######All-life table######
event<-rep('Arterial_event',342)
subgroup<-rep('All',342)
x_days_since_covid<-c(1:342)
weeks_since_covid<-c(rep('1',7), rep('2',7),rep('3-4',14),rep('5-8',28), rep('9-12',28),rep('13-28',98), rep('27-49',160))
q_estimated_average_incidence_rate<-rep(1.664217e-5, 342)
HR<-c(rep(21.7,7),rep(3.87, 7),rep(2.8,14),rep(2,28),rep(1.58,28),rep(1.34, 98), rep(1.34,160))

life_table_all<-data.frame(event,subgroup,x_days_since_covid,weeks_since_covid,q_estimated_average_incidence_rate,HR)
life_table_all$one_minus_q<-1-life_table_all$q_estimated_average_incidence_rate
life_table_all$S<-cumprod(life_table_all$one_minus_q)
life_table_all$qh<-life_table_all$q_estimated_average_incidence_rate*life_table_all$HR
life_table_all$one_minus_qh<-1- life_table_all$qh
life_table_all$SC<-cumprod(life_table_all$one_minus_qh)
life_table_all$AER<-life_table_all$S- life_table_all$SC
life_table_all$AER_percent<-life_table_all$AER*100

1367059*life_table_all[342,12] #7268.796

p_line<-ggplot(life_table_average,
               