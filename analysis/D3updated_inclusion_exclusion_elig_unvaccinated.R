#Project:Eligible but not vaccinated delta wave population study
#Branch:Inclusion exclusion criteria 
#Scripts: Renin Toms

#1. Set the follow up date - latter of: 12 weeks (84 days) after they became eligible for vaccination/The start of the delta pandemic era
#Ref: https://docs.google.com/spreadsheets/d/1Epre2Cv_4UVTwHJ6ccJN7QwDRq9pGGyQoWJoWolODZQ/edit?usp=sharing

#COHORT START DATE
#a.start date 2021-06-1 of delta pandemic era
input$delta_start <- as.Date("2021-06-01")
#b.12 weeks (84 days) after they become eligible for vaccination
input$unvacc_start <- as.Date(input$vax_date_eligible)+85 
#c.latest of a,b as COHORT start date
input$unvacc_coh_start_date <- pmax(input$delta_start, input$unvacc_start, na.rm = TRUE)

#COHORT END DATE
#The earliest of following:
#a.End date 2021-12-31 of the cohort (temporary - but change to last date of input collection)
input$delta_end <- as.Date("2021-12-31")
#b.sample follow up ends on Death date, if any
input$death_date <- as.Date(input$death_date)
#c.sample follow up ends on outcome event, if any
input$out_ami <- as.Date(input$out_date_ami)
input$out_stroke_isch <- as.Date(input$out_date_stroke_isch)
input$out_dvt <- as.Date(input$out_date_dvt)
input$out_pe <- as.Date(input$out_date_pe)
input$out_tia <- as.Date(input$out_date_tia)
input$out_stroke_sah_hs <- as.Date(input$out_date_stroke_sah_hs)
input$out_hf <- as.Date(input$out_date_hf)
input$out_angina <- as.Date(input$out_date_angina)
input$out_ate <- as.Date(input$out_date_ate)
input$out_vte <- as.Date(input$out_date_vte)
#d.vaccination date, if any
str(input$vax_date_covid_1)
#e.earliest of a,b, c as COHORT END date
input$vacc_coh_end_date <- pmin(input$delta_end, input$death_date, 
                                input$out_date_ami, input$out_date_stroke_isch,
                                input$out_date_dvt, input$out_date_pe,
                                input$out_date_tia,input$out_date_stroke_sah_hs,
                                input$out_date_hf, input$out_date_angina,
                                input$out_date_ate, input$out_date_vte, 
                                input$vax_date_covid_1,
                                input$vax_date_covid_2,
                                input$vax_date_covid_3, na.rm = TRUE)
#Define the cohort flow----------------------------------------------------------------------------------------------
cohort_flow <- data.frame(N = numeric(),
                          Description = character(),
                          stringsAsFactors = FALSE)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Study defined sample size")

#INCLUSION CRITERIA 1.Alive on the first day of follow up---------------------------------------------------------------
#a.determine the living status on start date
input$start_alive <- ifelse(input$death_date < input$unvacc_coh_start_date, 0, 1)# 1- alive; 0 - died
input$start_alive[is.na(input$start_alive)] <- 1
#b.subset input based on alive status on day 1 of follow up.
input <- subset(input, input$start_alive > 0)
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Inclusion1:Alive on the first day of follow up")

#INCLUSION CRITERIA 2.Known age between 18 and 110 on 01/06/2021-------------------------------------- 
input <- input[!is.na(input$cov_num_age),] # removes NAs, if any
#subset input based >=18 status on day 1 of follow up.
input <- subset(input, input$cov_num_age >= 18 & input$cov_num_age <= 110) 
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Inclusion2:Known age between 18 and 110 on 01/06/2021")

#INCLUSION CRITERIA 3.Known sex-----------------------------------------------------------------------------------------
input <- input[!is.na(input$cov_cat_sex),] # removes NAs, if any
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Inclusion3:Known sex")

#INCLUSION CRITERIA 4.Known deprivation--------------------------------------------------------------------------- 
input <- input[!is.na(input$cov_cat_deprivation),] # removes NAs, if any
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Inclusion4:Known deprivation")

#INCLUSION CRITERIA 5.Registered in an English GP with TPP software for at least 6 months prior to the study start date--------------------------
input <- input # This criteria is met in study definition 
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Inclusion5:Registered in an English GP with TPP software for at least 6 months prior to the study start date")

#EXCLUSION CRITERIA 6.SARS-CoV-2 infection recorded prior to the start of follow-up---------------------------------------
#a.Determine the SARS-CoV-2 infection date
input$exp_date_covid19_confirmed <- as.Date(input$exp_date_covid19_confirmed)
#the earliest date already adopted in the definition
#b.determine prior to start date infections
input$prior_infections <- ifelse(input$exp_date_covid19_confirmed < input$unvacc_coh_start_date, 1,0)#1-prior infection; 0 - No prior infection
input$prior_infections[is.na(input$prior_infections)] <- 0
input <- subset(input, input$prior_infections < 1)
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion6: SARS-CoV-2 infection recorded prior to their index date")

#EXCLUSION CRITERIA 7.have a record of one or more vaccination doses on the study start date---------------------------------------
#a.Determine the vacc status on study start date
input$prior_vacc1 <- ifelse(input$vax_date_covid_1 <= input$unvacc_coh_start_date, 1,0)
input$prior_vacc1[is.na(input$prior_vacc1)] <- 0
input$prior_vacc2 <- ifelse(input$vax_date_covid_2 <= input$unvacc_coh_start_date, 1,0)
input$prior_vacc2[is.na(input$prior_vacc2)] <- 0
input$prior_vacc3 <- ifelse(input$vax_date_covid_3 <= input$unvacc_coh_start_date, 1,0)
input$prior_vacc3[is.na(input$prior_vacc3)] <- 0
input$prior_vacc <- ifelse((input$prior_vacc1==1 | input$prior_vacc2==1 |input$prior_vacc3==1), 1,0)
#Note NAs don't have any vaccination  date, hence move to '0' or unvaccinated category
input$prior_vacc[is.na(input$prior_vacc)] <- 0
#b.Exclude people with prior vaccination
input <- subset(input, input$prior_vacc < 1)

#Define the cohort flow--------
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion7: Have a record of one or more vaccination doses on the study start date")
# Save input ----------------------------------------------------------------------------------------------
saveRDS(input, file = "output/IE_applied_unvaccinated_input.rds")
write.csv(cohort_flow,"output/delta-unvaccinated_cohort_flow.csv", row.names = FALSE)

#input.table::fwrite(input,"output/IE_applied_input.rds")
#input.table::fwrite(cohort_flow,"output/IE_delta-cohort_flow.csv")