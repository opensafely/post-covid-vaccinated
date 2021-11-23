#Project:Vaccinated delta wave population study
#Branch:Inclusion exclusion criteria 
#Scripts: Renin Toms

#1. Set the follow up date - 2 weeks(14 days) after the 2nd dose of vaccination
#Reason "Individuals may not have the best protection until 7–14 days after their second dose of the vaccine"
#Ref: https://www.health.gov.au/initiatives-and-programs/covid-19-vaccines/is-it-true/is-it-true-how-long-does-it-take-to-have-immunity-after-vaccination

#PRESET change dates to date format
data1$covid19_vaccination_date1 <- as.Date(data1$covid19_vaccination_date1,tryFormats = c("%Y-%m-%d"))
data1$covid19_vaccination_date2 <- as.Date(data1$covid19_vaccination_date2,tryFormats = c("%Y-%m-%d"))
data1$death_covid19_date <- as.Date(data1$death_covid19_date,tryFormats = c("%Y-%m-%d"))
data1$primary_care_death_date <- as.Date(data1$primary_care_death_date,tryFormats = c("%Y-%m-%d"))
data1$ons_died_from_any_cause_date <- as.Date(data1$ons_died_from_any_cause_date,tryFormats = c("%Y-%m-%d"))

#COHORT START DATE
#a.start date 2021-06-1 of the cohort if not vaccinated
data1$delta_start <- as.Date("2021-06-01")
#b.15 days after the second vaccination
data1$immune_start <- as.Date(data1$covid19_vaccination_date2)+15
#c.latest of a,b as COHORT start date
data1$vacc_coh_start_date <- pmax(data1$delta_start, data1$immune_start, na.rm = TRUE)

#INCLUSION CRITERIA 1.Alive on the first day of follow up
#a.Determine the death date
data1$vacc_coh_death_date <- pmin(data1$death_covid19_date, data1$primary_care_death_date, data1$ons_died_from_any_cause_date, na.rm = TRUE)
#Adopted the earliest available death date

#b.determine the living status on start date
data1$start_alive <- ifelse(data1$vacc_coh_death_date < data1$vacc_coh_start_date, 0, 1)# 1- alive; 0 - died
data1$start_alive[is.na(data1$start_alive)] <- 1
table(data1$start_alive, useNA = "ifany")# ~482 died before the start date

#subset data based on alive status on day 1 of follow up.
data1 <- subset(data1, data1$start_alive > 0) #~ 9518 samples retained

#INCLUSION CRITERIA 2.Known age between 18 and 110 inclusive on the first day of follow-up 
table(data$cov_age >=18, useNA = "ifany" )# ISSUE submitted to include DOB in study definition

#subset data based >=18 status on day 1 of follow up.
data2 <- subset(data1, data1$cov_age >= 18) #~ 7479 samples retained

#INCLUSION CRITERIA 3.Known sex
table(data2$cov_sex, useNA = "ifany")# nO 'NAs' found

#INCLUSION CRITERIA 4.Known deprivation 
table(data2$cov_deprivation, useNA = "ifany")# ~65 '0' found
data4 <- subset(data2, data2$cov_deprivation >= 1)#7414 samples retained

#INCLUSION CRITERIA 5.Registered in an English GP with TPP software for at least 6 months prior to the study start date
str(data)