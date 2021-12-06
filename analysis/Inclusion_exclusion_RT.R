#Project:Vaccinated delta wave population study
#Branch:Inclusion exclusion criteria 
#Scripts: Renin Toms

#1. Set the follow up date - 2 weeks(14 days) after the 2nd dose of vaccination
#Reason "Individuals may not have the best protection until 7–14 days after their second dose of the vaccine"
#Ref: https://www.health.gov.au/initiatives-and-programs/covid-19-vaccines/is-it-true/is-it-true-how-long-does-it-take-to-have-immunity-after-vaccination

#COHORT START DATE
#a.start date 2021-06-1 of the cohort 
input$delta_start <- as.Date("2021-06-01")
#b.if vaccinated 15 days after the second vaccination
input$immune_start <- as.Date(input$covid_vax_disease_2_date)+15
#c.latest of a,b as COHORT start date
input$vacc_coh_start_date <- pmax(input$delta_start, input$immune_start, na.rm = TRUE)

#COHORT END DATE
#a.End date 2021-12-31 of the cohort (TEMPORARY - but change to last date of input collection)
input$delta_end <- as.Date("2021-12-31")
#b.sample follow up ends on Death date, if any
input$death_date <- as.Date(input$death_date)
#c.sample follow up ends on outcome event, if any
input$out_ami <- as.Date(input$out_ami)
input$out_stroke_isch <- as.Date(input$out_stroke_isch)
input$out_dvt <- as.Date(input$out_dvt)
input$out_pe <- as.Date(input$out_pe)
input$out_tia <- as.Date(input$out_tia)
input$out_stroke_sah_hs <- as.Date(input$out_stroke_sah_hs)
input$out_hf <- as.Date(input$out_hf)
input$out_angina <- as.Date(input$out_angina)
input$out_ate <- as.Date(input$out_ate)
input$out_vte <- as.Date(input$out_vte)
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

#EXCLUSION CRITERIA 6.SARS-CoV-2 infection recorded prior to the start of follow-up---------------------------------------
#a.Determine the SARS-CoV-2 infection date
input$exp_confirmed_covid19_date <- as.Date(input$exp_confirmed_covid19_date)
#b.determine prior to start date infections
input$prior_infections <- ifelse(input$exp_confirmed_covid19_date < input$vacc_coh_start_date, 1,0)#1-prior infection; 0 - No prior infection
input$prior_infections[is.na(input$prior_infections)] <- 0
input <- subset(input, input$prior_infections < 1)
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion6: SARS-CoV-2 infection recorded prior to their index date")

#EXCLUSION CRITERIA 7.do not have a record of two vaccination doses prior to the study end date --------------------------------
#a.Determine the vaccination dates
input$covid19_vaccination_date1 <- as.Date(input$covid_vax_disease_1_date)
input$covid19_vaccination_date2 <- as.Date(input$covid_vax_disease_2_date)
#b.determine the vaccination gap in days
input$vacc_gap <- input$covid19_vaccination_date2 - input$covid19_vaccination_date1
# note vaccination gap will be an NA if 2nd/both the vaccines are not received
#b.Subset the fully vaccinated group
input <- input[!is.na(input$vacc_gap),] 
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion7:  No record of two vaccination doses prior to the study end date")

#EXCLUSION CRITERIA 8.received a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)------------
input <- subset(input, input$covid19_vaccination_date1 >= as.Date("2020-12-08"))
input <- subset(input, input$covid19_vaccination_date2 >= as.Date("2020-12-08"))
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion8: Recorded vaccination prior to the start date of vaccination program")

#EXCLUSION CRITERIA 9.received a second dose vaccination before their first dose vaccination------------
input <- subset(input, input$vacc_gap >= 0) 
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion9: Second dose vaccination recoreded  before their first dose vaccination")

#EXCLUSION CRITERIA 10.received a second dose vaccination less than three weeks after their first dose ------------
input <- subset(input, input$vacc_gap >= 21) 
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion10: Second dose vaccination recorded less than three weeks after their first dose")

#EXCLUSION CRITERIA 11.They received mixed vaccine products before 07-05-2021 ----------------------------------
#1.Determine the vaccine1 date and product for each dose
input$vacc_date_1 <- pmin(input$covid_vax_az_1_date,input$covid_vax_pfizer_1_date,input$covid_vax_moderna_1_date, na.rm = TRUE)#RT
input$vacc_product_1 <- ifelse(input$covid_vax_disease_1_date > 0 & input$covid_vax_az_1_date == input$vacc_date_1, 1,0)#1- Astro zeneca
input$vacc_product_1[is.na(input$vacc_product_1)] <- 0
input$vacc_product_1 <- ifelse(input$vacc_product_1==0 & (input$covid_vax_pfizer_1_date == input$vacc_date_1), 2,input$vacc_product_1)#2- pfizer
input$vacc_product_1[is.na(input$vacc_product_1)] <- 0
input$vacc_product_1 <- ifelse(input$vacc_product_1==0 & (input$covid_vax_moderna_1_date == input$vacc_date_1), 3,input$vacc_product_1)# 3- moderna
input$vacc_product_1[is.na(input$vacc_product_1)] <- 0 #  0 - no product info

#2.Determine the vaccine2 date and product for each dose
input$vacc_date_2 <- pmin(input$covid_vax_az_2_date,input$covid_vax_pfizer_2_date,input$covid_vax_moderna_2_date, na.rm = TRUE)#RT
input$vacc_product_2 <- ifelse(input$covid_vax_disease_2_date > 0 & input$covid_vax_az_2_date == input$vacc_date_2, 1,0)#1- Astro zeneca
input$vacc_product_2[is.na(input$vacc_product_2)] <- 0
input$vacc_product_2 <- ifelse(input$vacc_product_2==0 & (input$covid_vax_pfizer_2_date == input$vacc_date_2), 2,input$vacc_product_2)#2- pfizer
input$vacc_product_2[is.na(input$vacc_product_2)] <- 0
input$vacc_product_2 <- ifelse(input$vacc_product_2==0 & (input$covid_vax_moderna_2_date == input$vacc_date_2), 3,input$vacc_product_2)# 3- moderna
input$vacc_product_2[is.na(input$vacc_product_2)] <- 0 #  0 - no product info
table(input$vacc_product_2)

#Exclude samples with no vaccine product info
input <- subset(input, input$vacc_product_1 > 0)
input <- subset(input, input$vacc_product_2 > 0)
#Define the cohort flow
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Exclusion 11a: Vaccine product info not available")

#Exclude samples with mixed vaccine product before 7/5/2021



#This is a generalized code, which has to be modified latter based on the vaccine date & product variables which Yinghui(?) will derive
input10$mixed_vacc <- ifelse((input10$vaccine_1_product == "az" & 
                              input10$vaccine_2_product == "az") |
                              (input10$vaccine_1_product == "pfizer" & 
                              input10$vaccine_2_product == "pfizer") |
                              (input10$vaccine_1_product == "moderna" & 
                              input10$vaccine_2_product == "moderna"), 0, 1)#1- mixed

input10$mixed_vacc <- ifelse(input10$mixed_vacc == 1 & input10$covid_vax_disease_2_date < as.Date ("07/05/2021"),1,0) 
input11 <- subset(input10, input10$mixed_vacc == 0)

#Define the cohort flow--------
cohort_flow <- input.frame(N = numeric(),
                          Description = character(),
                          stringsAsFactors = FALSE)

cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Study defined sample size")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input1),"Inclusion1:Alive on the first day of follow up")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input2),"Inclusion2:Known age between 18 and 110 on 01/06/2021")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input3),"Inclusion3:Known sex")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input4),"Inclusion4:Known deprivation")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input5),"Inclusion5:Registered in an English GP with TPP software for at least 6 months prior to the study start date")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input6),"Exclusion6: SARS-CoV-2 infection recorded prior to their index date")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input7),"Exclusion7:Do not have a record of two vaccination doses prior to the study end date")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input8),"Exclusion8: Received a vaccination prior to 08-12-2020")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input9),"Exclusion9:Received a second dose vaccination before their first dose vaccination")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input10),"Exclusion10:Received a second dose vaccination less than three weeks after their first dose")
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input11),"Exclusion11:They received mixed vaccine products before 07-05-2021")

# Save input -------------------------------------------------------------------

input.table::fwrite(input11,"input/Vaccinated_delta_cohort.csv")
input.table::fwrite(cohort_flow,"output/delta-cohort_flow.csv")