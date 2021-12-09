#Project:Vaccinated delta wave population study
#Branch:Inclusion exclusion criteria 
#Scripts: Renin Toms

#1. Set the follow up date - 2 weeks(14 days) after the 2nd dose of vaccination
#Reason "Individuals may not have the best protection until 7–14 days after their second dose of the vaccine"
#Ref: https://www.health.gov.au/initiatives-and-programs/covid-19-vaccines/is-it-true/is-it-true-how-long-does-it-take-to-have-immunity-after-vaccination

#INDEX START DATE
#a.start date 2021-06-1 of the cohort 
input$delta_start <- as.Date("2021-06-01")
#b.15 days after the second vaccination
input$immune_start <- as.Date(input$vax_date_covid_2)+15
#c.latest of a,b as start date index
input$vacc_coh_start_date <- pmax(input$delta_start, input$immune_start, na.rm = TRUE)

#COHORT END DATE
#a.End date 2021-12-31 of the cohort (TEMPORARY - but change to last date of input collection)
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
#d.earliest of a,b, c as COHORT END date
input$vacc_coh_end_date <- pmin(input$delta_end, input$death_date, 
                               input$out_ami, input$out_stroke_isch,
                               input$out_dvt, input$out_pe,
                               input$out_tia,input$out_stroke_sah_hs,
                               input$out_hf, input$out_angina,
                               input$out_ate, input$out_vte, na.rm = TRUE)
#Define the cohort flow----------------------------------------------------------------------------------------------
cohort_flow <- data.frame(N = numeric(),
                          Description = character(),
                          stringsAsFactors = FALSE)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Study defined sample size")

#INCLUSION CRITERIA 1.Alive on the first day of follow up---------------------------------------------------------------
#a.determine the living status on start date
input$start_alive <- ifelse(input$death_date < input$vacc_coh_start_date, 0, 1)# 1- alive; 0 - died
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

#EXCLUSION CRITERIA 6.SARS-CoV-2 infection recorded prior to the start of follow-up------------------------------------------------------
#a.Determine the SARS-CoV-2 infection date
input$exp_date_covid19_confirmed <- as.Date(input$exp_date_covid19_confirmed)
#b.determine prior to start date infections
input$prior_infections <- ifelse(input$exp_date_covid19_confirmed < input$vacc_coh_start_date, 1,0)#1-prior infection; 0 - No prior infection
input$prior_infections[is.na(input$prior_infections)] <- 0
input <- subset(input, input$prior_infections < 1)
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion6: SARS-CoV-2 infection recorded prior to their index date")

#EXCLUSION CRITERIA 7.do not have a record of two vaccination doses prior to the study end date --------------------------------------------
#a.Determine the vaccination dates
input$vax_date_covid_1 <- as.Date(input$vax_date_covid_1)
input$vax_date_covid_2 <- as.Date(input$vax_date_covid_2)
#b.determine the vaccination gap in days
input$vacc_gap <- input$vax_date_covid_2 - input$vax_date_covid_1
# note vaccination gap will be an NA if 2nd/both the vaccines are not received
#b.Subset the fully vaccinated group
input <- input[!is.na(input$vacc_gap),] 
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion7:  No record of two vaccination doses prior to the study end date")

#EXCLUSION CRITERIA 8.received a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)------------
input <- subset(input, input$vax_date_covid_1 >= as.Date("2020-12-08"))
input <- subset(input, input$vax_date_covid_2 >= as.Date("2020-12-08"))
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion8: Recorded vaccination prior to the start date of vaccination program")

#EXCLUSION CRITERIA 9.received a second dose vaccination before their first dose vaccination------------
input <- subset(input, input$vacc_gap >= 0) 
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion9: Second dose vaccination recoreded  before their first dose vaccination")

#EXCLUSION CRITERIA 10.received a second dose vaccination less than three weeks after their first dose -------------------------------
input <- subset(input, input$vacc_gap >= 21) 
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion10: Second dose vaccination recorded less than three weeks after their first dose")

#EXCLUSION CRITERIA 11.They received mixed vaccine products before 07-05-2021 ---------------------------------------------------------------
#Determines mixed vaccination
input$vax_mixed <- ifelse((input$vax_cat_product_1!=input$vax_cat_product_2),1,0)
#Remove if vaccination products are mixed prior to "2021-05-07"
input$vax_prior_mixed <- ifelse(input$vax_mixed==1 & input$vax_date_covid_1 < as.Date ("2021-05-07"), 1,0)
input$vax_prior_mixed <- ifelse(input$vax_mixed==1 & input$vax_date_covid_2 < as.Date ("2021-05-07"), 1,0)
input$vax_prior_mixed[is.na(input$vax_prior_mixed)] <- 0
input <- subset(input, input$vax_prior_mixed==0)
#Removes if vaccination date is less than "2021-05-07" and the vaccine product name is not known!
input$vax_prior_unknown <- ifelse((input$vax_date_covid_1 < as.Date ("2021-05-07")) & is.na(input$vax_cat_product_1),1,0)
input$vax_prior_unknown <- ifelse((input$vax_date_covid_2 < as.Date ("2021-05-07")) & is.na(input$vax_cat_product_1),1,0)
input <- subset(input, input$vax_prior_unknown==0)
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion11: Received mixed vaccine products before 07-05-2021")

# Save input ---------------------------------------------------------------------------------------------------------------------
saveRDS(input, file = "output/IE_applied_vaccinated_input.rds")
write.csv(cohort_flow,"output/delta-vaccinated_cohort_flow.csv", row.names = FALSE)

#Alternatives-when required - un comment the below
#data.table::fwrite(input,"output/IE_applied_vaccinated_input.rds")
#data.table::fwrite(cohort_flow,"output/delta-vaccinated_cohort_flow.csv")
