#Project:Vaccinated delta wave population study
#Branch:Inclusion exclusion criteria 
#Scripts: Renin Toms

#1. Set the follow up date - 2 weeks(14 days) after the 2nd dose of vaccination
#Reason "Individuals may not have the best protection until 7–14 days after their second dose of the vaccine"
#Ref: https://www.health.gov.au/initiatives-and-programs/covid-19-vaccines/is-it-true/is-it-true-how-long-does-it-take-to-have-immunity-after-vaccination

#COHORT START DATE
#a.start date 2021-06-1 of the cohort if not vaccinated
data$delta_start <- as.Date("2021-06-01")
#b.15 days after the second vaccination
data$immune_start <- as.Date(data$covid19_vaccination_date2)+15
#c.latest of a,b as COHORT start date
data$vacc_coh_start_date <- pmax(data$delta_start, data$immune_start, na.rm = TRUE)

#COHORT END DATE
#a.End date 2021-12-31 of the cohort (temporary - but latter change to last date of data collection)
data$delta_end <- as.Date("2021-12-31")
#b.sample follow up ends on Death date, if any
data11$delta_end <- data11$death_date
data11$delta_end[is.na(data11$delta_end)] <- as.Date("2021-12-31")


#INCLUSION CRITERIA 1.Alive on the first day of follow up---------------------------------------------------------------
#a.Determine the death date
data$death_date <- as.Date(data$death_date)
#Adopted the earliest available death date

#b.determine the living status on start date
data$start_alive <- ifelse(data$death_date < data$vacc_coh_start_date, 0, 1)# 1- alive; 0 - died
data$start_alive[is.na(data$start_alive)] <- 1
table(data$start_alive, useNA = "ifany")# ~ 466 died before the start date

#subset data based on alive status on day 1 of follow up.
data1 <- subset(data, data$start_alive > 0) #~ 9534 samples retained

#INCLUSION CRITERIA 2.Known age between 18 and 110 inclusive on the first day of follow-up-------------------------------------- 
table(data$cov_age >=18, useNA = "ifany" )# ~ 2156 under 18 age group

#subset data based >=18 status on day 1 of follow up.
data2 <- subset(data1, data1$cov_age >= 18) #~ 7492 samples retained

#INCLUSION CRITERIA 3.Known sex-----------------------------------------------------------------------------------------
table(data2$cov_sex, useNA = "ifany")# nO 'NAs' found

#INCLUSION CRITERIA 4.Known deprivation--------------------------------------------------------------------------- 
table(data2$cov_deprivation, useNA = "ifany")# ~65 '0' found
data4 <- subset(data2, data2$cov_deprivation >= 1)#7427 samples retained

#INCLUSION CRITERIA 5.Registered in an English GP with TPP software for at least 6 months prior to the study start date--------------------------

#EXCLUSION CRITERIA 6.SARS-CoV-2 infection recorded prior to the start of follow-up---------------------------------------
#a.Determine the SARS-CoV-2 infection date
data4$exp_confirmed_covid19_date <- as.Date(data4$exp_confirmed_covid19_date)
#the earliest date adopted in the definition

#b.determine prior to start date infections
data4$prior_infections <- ifelse(data4$exp_confirmed_covid19_date < data4$vacc_coh_start_date, 1,0)#1-prior infection; 0 - No prior infection
data4$prior_infections[is.na(data4$prior_infections)] <- 0
table(data4$prior_infections, useNA = "ifany") #~183 prior infections

data6 <- subset(data4, data4$prior_infections < 1)#~7244 samples retain
#EXCLUSION CRITERIA 7.do not have a record of two vaccination doses prior to the study end date--------------------------------
#a.Determine the vaccination dates
data6$covid19_vaccination_date1 <- as.Date(data6$covid19_vaccination_date1)
data6$covid19_vaccination_date2 <- as.Date(data6$covid19_vaccination_date2)

#b.determine the vaccination gap in days
data6$vacc_gap <- data6$covid19_vaccination_date2 - data6$covid19_vaccination_date1

#b.Determine the 'fully' vaccinated status
data6$fully_vacc <- ifelse(data6$vacc_gap > 0,0,1) # 0- not fully vaccinated, 1- fully vaccinated
data6$fully_vacc <- ifelse()
table(data6$fully_vacc, useNA = "ifany")# ~65 '0' found
