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
#a.End date 2021-12-31 of the cohort (temporary - but change to last date of data collection)
data$delta_end <- as.Date("2021-12-31")
#b.sample follow up ends on Death date, if any
data$death_date <- as.Date(data$death_date)
#c.sample follow up ends on outcome event, if any
data$out_ami <- as.Date(data$out_ami)
data$out_stroke_isch <- as.Date(data$out_stroke_isch)
data$out_dvt <- as.Date(data$out_dvt)
data$out_pe <- as.Date(data$out_pe)
data$out_tia <- as.Date(data$out_tia)
data$out_stroke_sah_hs <- as.Date(data$out_stroke_sah_hs)
data$out_hf <- as.Date(data$out_hf)
data$out_angina <- as.Date(data$out_angina)
data$out_ate <- as.Date(data$out_ate)
data$out_vte <- as.Date(data$out_vte)
#d.earliest of a,b, c as COHORT END date
data$vacc_coh_end_date <- pmin(data$delta_end, data$death_date, 
                               data$out_ami, data$out_stroke_isch,
                               data$out_dvt, data$out_pe,
                               data$out_tia,data$out_stroke_sah_hs,
                               data$out_hf, data$out_angina,
                               data$out_ate, data$out_vte, na.rm = TRUE)

#INCLUSION CRITERIA 1.Alive on the first day of follow up---------------------------------------------------------------
#a.determine the living status on start date
data$start_alive <- ifelse(data$death_date < data$vacc_coh_start_date, 0, 1)# 1- alive; 0 - died
data$start_alive[is.na(data$start_alive)] <- 1
table(data$start_alive, useNA = "ifany")# ~ 466 died before the start date
#b.subset data based on alive status on day 1 of follow up.
data1 <- subset(data, data$start_alive > 0) #~ 9534 samples retained

#INCLUSION CRITERIA 2.Known age between 18 and 110 inclusive on the first day of follow-up-------------------------------------- 
table(data$cov_age >=18, useNA = "ifany" )# ~ 2156 under 18 age group 
data1 <- data1[!is.na(data1$cov_age),] # removes NAs, if any
#subset data based >=18 status on day 1 of follow up.
data2 <- subset(data1, data1$cov_age >= 18) #~ 7492 samples retained RECONFIRM

#INCLUSION CRITERIA 3.Known sex-----------------------------------------------------------------------------------------
table(data2$cov_sex, useNA = "ifany")# nO 'NAs' found
data3 <- data2[!is.na(data2$cov_sex),] # removes NAs, if any

#INCLUSION CRITERIA 4.Known deprivation--------------------------------------------------------------------------- 
table(data3$cov_deprivation, useNA = "ifany")# ~65 '0' found
data4 <- data3[!is.na(data3$cov_deprivation),] # removes NAs, if any
data4 <- subset(data3, data3$cov_deprivation >= 1)#7427 samples retained

#INCLUSION CRITERIA 5.Registered in an English GP with TPP software for at least 6 months prior to the study start date--------------------------
data5 <- data4 #RECONFIRM 

#EXCLUSION CRITERIA 6.SARS-CoV-2 infection recorded prior to the start of follow-up---------------------------------------
#a.Determine the SARS-CoV-2 infection date
data5$exp_confirmed_covid19_date <- as.Date(data5$exp_confirmed_covid19_date)
#the earliest date adopted in the definition
#b.determine prior to start date infections
data5$prior_infections <- ifelse(data5$exp_confirmed_covid19_date < data5$vacc_coh_start_date, 1,0)#1-prior infection; 0 - No prior infection
data5$prior_infections[is.na(data5$prior_infections)] <- 0
table(data5$prior_infections, useNA = "ifany") #~183 prior infections
data6 <- subset(data5, data5$prior_infections < 1)#~7244 samples retain

#EXCLUSION CRITERIA 7.do not have a record of two vaccination doses prior to the study end date --------------------------------
#a.Determine the vaccination dates
data6$covid19_vaccination_date1 <- as.Date(data6$covid19_vaccination_date1)
data6$covid19_vaccination_date2 <- as.Date(data6$covid19_vaccination_date2)
#b.determine the vaccination gap in days
data6$vacc_gap <- data6$covid19_vaccination_date2 - data6$covid19_vaccination_date1
#b.Subset the fully vaccinated group
# note vaccination gap will be an NA if both the vaccines are not received
table(data6$vacc_gap, useNA = "ifany") #~4342 Na
data7 <- data6[!is.na(data6$vacc_gap),] #2902 samples retain

#EXCLUSION CRITERIA 8.received a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)------------
data8 <- subset(data7, data7$covid19_vaccination_date1 >= as.Date("2020-12-08"))
data8 <- subset(data7, data7$covid19_vaccination_date2 >= as.Date("2020-12-08"))# none change

#EXCLUSION CRITERIA 9.received a second dose vaccination before their first dose vaccination------------
table(data8$vacc_gap < 0) # ~ 1475 -ve gaps
data9 <- subset(data8, data8$vacc_gap >= 0) # 1427 samples retain

#EXCLUSION CRITERIA 10.received a second dose vaccination less than three weeks after their first dose ------------
table(data9$vacc_gap < 21) # ~ 196
data10 <- subset(data9, data9$vacc_gap >= 21) # 1231 samples retain - RECONFIRM '21'

 



#They received a second dose vaccination less than three weeks after their first dose 

#They are recorded as having a situation (e.g., refused vaccination) 

#They received mixed vaccine products before 07-05-2021 
