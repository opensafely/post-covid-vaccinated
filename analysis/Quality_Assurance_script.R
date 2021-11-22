#Quality Assurance - *NOT FINISHED*
#Need to decide on the best way to read in data/sharing data between scripts.

library(dplyr)
library(data.table)
input <- fread("OpenSafely - unvaccinated analysis/input.csv" ,na.strings=c("","NA"))

#I'm assuming this will all be done in the preparing covariates script instead
outcome=colnames(input)[grepl("out_",colnames(input))]
qa_variables=colnames(input)[grepl("qa_",colnames(input))]
contain_date=colnames(input)[grepl("date",colnames(input))]
date_columns=c(outcome,qa_variables,contain_date)

for(date in date_columns){
  input[[date]]=as.Date(input[[date]], format="%Y-%m-%d")
}

#Rule 1:
#Year of birth is after year of death 
#Need to add year of birth
input$rule1=NA
input$rule1=(input$birth_date > input$death_date & is.na(input$birth_date)== "FALSE" & is.na(input$death_date) == "FALSE")


#Rule 2: Patient does not have mandatory fields completed (patient id, sex, age, index of multiple deprivation, ethnicity, region, n_disorder, smoking status, year of birth)
input$rule2=NA
input$rule2=(is.na(input$patient_id)|is.na(input$cov_age)|is.na(input$cov_sex)|is.na(input$cov_deprivation)|is.na(input$cov_ethnicity)|is.na(input$cov_region)|is.na(input$cov_n_disorder)|is.na(input$cov_smoking_status)|is.na(input$birth_date))
#input$rule2=(is.na(input$patient_id)|is.na(input$cov_age)|is.na(input$cov_sex)|is.na(input$cov_deprivation)|is.null(input$patient_id)|is.null(input$cov_age)|is.null(input$cov_sex)|is.null(input$cov_deprivation)|is.nan(input$patient_id)|is.nan(input$cov_age)|is.nan(input$cov_sex)|is.nan(input$cov_deprivation))
#I think is.na would probably be enough but not sure



#Rule 3: Year of birth predates NHS established year or year of birth exceeds current date
input$rule3=NA
input$rule3=((format(input$birth_date, format="%Y") <"1793" & is.na(input$birth_date) == "FALSE") |input$birth_date >format(Sys.Date(),"%Y-%m-%d") & is.na(input$birth_date) == "FALSE")


#Rule 4: Date of death is NULL or invalid (on or before 1/1/1900 or after current date)
input$rule4=NA
input$rule4=((input$death_date <="1900-01-01"|input$death_date > format(Sys.Date(),"%Y-%m-%d")) & is.na(input$death_date) == "FALSE")
#can death date be null as opposed to NA ?  


#Rule 5: Remove those where registered date of death is before actual date of death
#I'm not sure if this is going to be possible using OpenSafely - in TRE the deaths data table contains two different columns REG_DATE_OF_DEATH and REG_DATE which are used for this but not sure if this info is avaialable in OpenSafely


#Rule 6: Pregnancy/birth codes for men
input$rule6=NA
input$rule6=(is.na(input$qa_pregnancy) == FALSE & input$cov_sex=="M")


#Rule 7: Prostate cancer codes for women
input$rule7=NA
input$rule7=(is.na(input$qa_prostate_cancer) == FALSE & input$cov_sex=="F")

#Rule 8: Patients have all missing record dates and dates
#Not sure if this is possible? In NHS TRE this looks at gdppr data and removes people who have null values in the record_date or date column


#Rule 9: HRT or COCP meds for men
input$rule9=NA
input$rule9=((input$cov_sex=="M" & input$cov_hrt_meds==1)|(input$cov_sex=="M" & input$cov_cocp_meds==1))


#Remove rows that are TRUE for at least one rule
input=input[,(isFALSE(input$rule1) & isFALSE(input$rule2) & isFALSE(input$rule3) & isFALSE(input$rule4) & isFALSE(input$rule5) & isFALSE(input$rule6) & isFALSE(input$rule7) & isFALSE(input$rule8) & isFALSE(input$rule9))]


