#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang

library(tidyverse)
library(survival)

# Define empty results table

excess_risk <- data.frame(outcome = character(),
                          estimate = numeric(),
                          stringsAsFactors = FALSE)

# Specify data input

input <- "input_vaccinated"

# Load data

data <- readr::read_rds(paste0("output/",input,"_stage1.rds"))

# Identify outcomes

outcomes <- colnames(data)[substr(colnames(data),1,4)=="out_"]

# Specify outcome

for (outcome in outcomes) {


#Outcome- OTHER ARTERIAL THROMBOTIC EVENTS
data <- data[,c("patient_id", "vax_date_covid_2",  "death_date", outcome, "exp_date_covid19_confirmed")]
colnames(data) <- c("patient_id", "vax_date_covid_2",  "death_date", "outcome", "exp_date_covid19_confirmed")

#-----------------------------------------------
#Step1.Calculate the average daily CVD incidence
#-----------------------------------------------

#Vaccinated cohort starts on 1/6/2021 and ends on 14/12/2021
#1.COHORT Dates
data$cohort_start <- as.Date("2021-06-01", format="%Y-%m-%d")
data$cohort_end <- as.Date("2021-12-14", format="%Y-%m-%d")

#2.Follow up period in vaccinated COHORT
#Index starts on the latest of: 1) cohort start date; 2)two weeks after the 2nd vaccination
data$fp_start <- pmax(data$cohort_start, data$vax_date_covid_2+15, na.rm = TRUE)
#Index ends on the earliest of: 1) Cohort end date; 2)CVD event; 3)death; 4) COVID19 date (to get the unexposed)
data$fp_end <- pmin(data$outcome+1, data$death_date+1, data$exp_date_covid19_confirmed, data$cohort_end, na.rm = TRUE)#RT ELABORATE
#Index follow up period is the difference between follow up start and end.
data$fp_period <- data$fp_end - data$fp_start


#clean data(may not be required in actual data, but no harm anyway)
range(data$fp_period)#-239  to 196 days
data <- subset(data, data$fp_period < 197)
data <- subset(data, data$fp_period > 0)
#filter events after end of follow up period
table(data$outcome > data$fp_end, useNA = "ifany")# 2,383 people had events after the end x  of follow up
table(data$outcome == data$fp_end, useNA = "ifany") # 73 people had events at the end date of follow up
data <- subset(data, data$outcome <= data$fp_end | is.na(data$outcome))

#3.Person days or years
fp_person_days <- sum(data$fp_period)
print(fp_person_days) #Unit <- person-days
fp_person_years <- fp_person_days/365 
print(fp_person_years)#Unit <- person-years

#4.Count events
total_deaths <- sum(!is.na(data$death_date))# Total deaths
total_events <- sum(!is.na(data$outcome))# Total events
total_cases <- sum(!is.na(data$exp_date_covid19_confirmed))# Total COVID19 cases

#5.Incidence rate over the follow up period in the unexposed population
#Number of new events / sum of person-time at risk

#Numerator- Events in unexposed
ate_total <- sum(!is.na(data$outcome))
print(ate_total)# Total events
ate_in_exposed <- sum(data$outcome >= data$exp_date_covid19_confirmed, na.rm = T)
print(ate_in_exposed)#  Events in exposed
ate_in_unexposed <- ate_total - ate_in_exposed
print(ate_in_unexposed)#Events in unexposed

#Denominator- sum of person-time at risk
fp_person_days <- as.integer(fp_person_days)

#Incidence rate in unexposed
incidence_rate <- ate_in_unexposed/fp_person_days
print(incidence_rate) 
incidence_rate*100000 # Incidence rate in unexposed per 100,000 person days follow up

#----------------------------------------------------------------------------------------------------------
#Step1-Method 2 (alternative) - Average daily incidence
#----------------------------------------------------------------------------------------------------------
#Pre-process the variables
data$ate_in_unexposed <- ifelse((data$outcome < data$exp_date_covid19_confirmed) |
                                         (data$outcome>0 & is.na(data$exp_date_covid19_confirmed)),1,0)
data$ate_in_unexposed[is.na(data$ate_in_unexposed)] <- 0
table(data$ate_in_unexposed, useNA = "ifany")

#Assign the variables
fp_period <- data$fp_period # follow up time
ate_status_in_unexposed <- data$ate_in_unexposed #events in unexposed

#Null Survival model
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


#Create the life table
lifetable<- as.data.frame(daily_incidence)

#a.Unexposed population parameters
#Gather existing variables
lifetable$event <- gsub("out_date_","",outcome)
lifetable$agegp <- "ALL"
lifetable$sex <- "ALL"
lifetable$days <- lifetable$time
lifetable <- (lifetable[c("event", "agegp",  "sex", "days")])
#q = estimated daily incidence of event
lifetable$q <- incidence_rate # Method 1
lifetable$'1-q' <- 1 - lifetable$q 
lifetable$s <- cumprod(lifetable$`1-q`)

#b.Exposed population parameters
#h = maximally adjusted hazard ratios                                            #RT- to add output file path
hr <- compiled_hr_results_main_vaccinated_delta_mdl_max_adj_covid_history_false

#locate the estimates
hr_ate_28 <- subset(hr, hr$project == "vaccinated_delta" &  hr$event == "ate" &
                      hr$model == "mdl_max_adj" & hr$term == "days0_28")

hr_ate_196 <- subset(hr, hr$project == "vaccinated_delta" &  hr$event == "ate" &
                       hr$model == "mdl_max_adj" & hr$term == "days28_196")

#assign the estimates
lifetable$h <- ifelse(lifetable$days < 29, rep(hr_ate_28$estimate),0)
lifetable$h <- ifelse(lifetable$days > 28, rep(hr_ate_196$estimate),lifetable$h)
lifetable$qh <- lifetable$q*lifetable$h
lifetable$'1-qh' <- 1 - lifetable$qh
lifetable$sc <- cumprod(lifetable$`1-qh`)

#AER = S-SC =difference in absolute risk
lifetable$AER <- lifetable$sc - lifetable$s

#AER*100 = AER percentage
lifetable$AER_p <- lifetable$AER*100

#Sample results and interpretation.
9.887190e-02*100 #the changes in absolute risk of ATE after exposed to COVID19 on day 196 (28 weeks) would be 9.88%
9.887190e-02*188 # 18.58792 more ate events 28 weeks after 188 covid19 events - in the vaccinated delta population.

# Add to results dataframe

excess_risk[nrow(excess_risk)+1,] <- c(gsub("out_date_","",outcome), lifetable[nrow(lifetable),]$AER * total_cases)

}

##########################
#plotting
######All-life table######
plot(lifetable$days, lifetable$AER_p)



p_line<-ggplot(lifetable,
               aes(x=days,
                   y=AER_p,
                   group=1)) +
  #geom_errorbar(aes(ymin=incidence_rate_difference_LB, ymax=incidence_rate_difference_UB), width=.2,
  #              position=position_dodge(.9))+
  geom_line(size=1.5)+
  #geom_point()+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200),limits = c(0,200))+
  scale_y_continuous(limits = c(0,10))+
  labs(x='days since COVID-19 diagnosis',y='Cumulative difference in absolute risk  (%)',
       title = 'Arterial Thrombotic Events')+
  theme(plot.title = element_text(hjust = 0.5))
 
p_line
      #RT - to add plot saving file location         