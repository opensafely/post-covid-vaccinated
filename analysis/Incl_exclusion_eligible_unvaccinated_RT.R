#Project:Eligible but not vaccinated delta wave population study
#Branch:Inclusion exclusion criteria 
#Scripts: Renin Toms

data <- input

#1. Set the follow up date - latter of: 12 weeks (84 days) after they became eligible for vaccination/The start of the delta pandemic era
#Ref: https://docs.google.com/spreadsheets/d/1Epre2Cv_4UVTwHJ6ccJN7QwDRq9pGGyQoWJoWolODZQ/edit?usp=sharing

#COHORT START DATE
#a.start date 2021-06-1 of delta pandemic era
data$delta_start <- as.Date("2021-06-01")
#b.85 days after they become eligible for vaccination
#b1.determine the eligibility date for each age group
   #last eligibility group(22nd) was 18+ years group, whose vaccine eligibility started on 18-6-2021 
data$unvacc_start <- as.Date("2021-06-18")+85
#data$unvacc_start <- as.Date(data$elig_date)+85 - EXECUTE WHEN NEW DEFINITION DATE COME UP
#c.latest of a,b as COHORT start date
data$unvacc_coh_start_date <- pmax(data$delta_start, data$unvacc_start, na.rm = TRUE)

#COHORT END DATE
#The earliest of following:
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
#d.vaccination date, if any
str(data$covid_vax_disease_1_date)
#e.earliest of a,b, c as COHORT END date
data$vacc_coh_end_date <- pmin(data$delta_end, data$death_date, 
                               data$out_ami, data$out_stroke_isch,
                               data$out_dvt, data$out_pe,
                               data$out_tia,data$out_stroke_sah_hs,
                               data$out_hf, data$out_angina,
                               data$out_ate, data$out_vte, 
                               data$covid_vax_disease_1_date,
                               data$covid_vax_disease_2_date,
                               data$covid_vax_disease_3_date, na.rm = TRUE)

#INCLUSION CRITERIA 1.Alive on the first day of follow up---------------------------------------------------------------
#a.determine the living status on start date
data$start_alive <- ifelse(data$death_date < data$unvacc_coh_start_date, 0, 1)# 1- alive; 0 - died
data$start_alive[is.na(data$start_alive)] <- 1
table(data$start_alive, useNA = "ifany")# ~ 114 died before the start date
#b.subset data based on alive status on day 1 of follow up.
data1 <- subset(data, data$start_alive > 0) #~ 9886 samples retained

#INCLUSION CRITERIA 2.Known age between 18 and 110 on 01/06/2021-------------------------------------- 
table(data1$cov_num_age >=18, useNA = "ifany" )# ~ 2060under 18 age group 
table(data1$cov_num_age >110, useNA = "ifany" )# ~ 0 in dummy data
data1 <- data1[!is.na(data1$cov_num_age),] # removes NAs, if any
#subset data based >=18 status on day 1 of follow up.
data2 <- subset(data1, data1$cov_num_age >= 18 & data1$cov_num_age <= 110) #~ 7826 samples retained 

#INCLUSION CRITERIA 3.Known sex-----------------------------------------------------------------------------------------
table(data2$cov_cat_sex, useNA = "ifany")# nO 'NAs' found
data3 <- data2[!is.na(data2$cov_cat_sex),] # removes NAs, if any

#INCLUSION CRITERIA 4.Known deprivation--------------------------------------------------------------------------- 
table(data3$cov_cat_deprivation, useNA = "ifany")
data4 <- data3[!is.na(data3$cov_cat_deprivation),] # removes NAs, if any
#7427 samples retained

#INCLUSION CRITERIA 5.Registered in an English GP with TPP software for at least 6 months prior to the study start date--------------------------
data5 <- data4 # This criteria is met in study definition 

#EXCLUSION CRITERIA 6.SARS-CoV-2 infection recorded prior to the start of follow-up---------------------------------------
#a.Determine the SARS-CoV-2 infection date
data5$exp_confirmed_covid19_date <- as.Date(data5$exp_confirmed_covid19_date)
#the earliest date already adopted in the definition
#b.determine prior to start date infections
data5$prior_infections <- ifelse(data5$exp_confirmed_covid19_date < data5$unvacc_coh_start_date, 1,0)#1-prior infection; 0 - No prior infection
data5$prior_infections[is.na(data5$prior_infections)] <- 0
table(data5$prior_infections, useNA = "ifany") #~740 prior infections
data6 <- subset(data5, data5$prior_infections < 1)#~7086 samples retain

#EXCLUSION CRITERIA 7.have a record of one or more vaccination doses on the study start date---------------------------------------
#a.Determine the vacc status on study start date
data6$prior_vacc1 <- ifelse(data6$covid_vax_disease_1_date <= data6$unvacc_coh_start_date, 1,0)
data6$prior_vacc1[is.na(data6$prior_vacc1)] <- 0
data6$prior_vacc2 <- ifelse(data6$covid_vax_disease_2_date <= data6$unvacc_coh_start_date, 1,0)
data6$prior_vacc2[is.na(data6$prior_vacc2)] <- 0
data6$prior_vacc3 <- ifelse(data6$covid_vax_disease_3_date <= data6$unvacc_coh_start_date, 1,0)
data6$prior_vacc3[is.na(data6$prior_vacc3)] <- 0
data6$prior_vacc <- ifelse((data6$prior_vacc1==1 | data6$prior_vacc2==1 |data6$prior_vacc3==1), 1,0)
#Note NAs don't have any vaccination  date, hence move to '0' or unvaccinated category
data6$prior_vacc[is.na(data6$prior_vacc)] <- 0
table(data6$prior_vacc, useNA = "ifany")#5948 vaccinated ON/before cohort start date

#b.Exclude people with prior vaccination
data7 <- subset(data6, data6$prior_vacc < 1)#~1138 samples retain

#Define the cohort flow--------
cohort_flow <- data.frame(N = numeric(),
                          Description = character(),
                          stringsAsFactors = FALSE)

cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(data),"Study defined sample size")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(data1),"Inclusion1:Alive on the first day of follow up")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(data2),"Inclusion2:Known age between 18 and 110 on 01/06/2021")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(data3),"Inclusion3:Known sex")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(data4),"Inclusion4:Known deprivation")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(data5),"Inclusion5:Registered in an English GP with TPP software for at least 6 months prior to the study start date")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(data6),"Exclusion6: SARS-CoV-2 infection recorded prior to their index date")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(data7),"Exclusion7: Have a record of one or more vaccination doses on the study start date")

# Save data ----------------------------------------------------------------------------------------------
data.table::fwrite(data11,"data/Vaccinated_delta_cohort.rds")
data.table::fwrite(cohort_flow,"output/delta-cohort_flow.csv")
