#Quality Assurance
#Has various rules that must be met to be included in the study popultaion - checks that the data follows general common sense rules
#Need to decide on the best way to read in data/sharing data between scripts.

library(dplyr)
library(data.table)
input=readRDS("output/input.rds")
input$qa_birth_year=as.Date(input$qa_birth_year) #needs to be added to preprocessing script

#Rule 1:
#Year of birth is after year of death or patient only has year of death
input$rule1=NA
input$rule1=((input$qa_birth_year > as.integer((format(input$death_date, format="%Y"))) & is.na(input$qa_birth_year)== FALSE & is.na(input$death_date) == FALSE)|(is.na(input$qa_birth_year)== TRUE & is.na(input$death_date) == FALSE))


#Rule 2: Patient does not have mandatory fields completed (patient id, sex, age, index of multiple deprivation, ethnicity, region, n_disorder, smoking status, year of birth)
input$rule2=NA
input$rule2=(is.na(input$patient_id)|is.na(input$cov_age)|is.na(input$cov_sex)|is.na(input$cov_deprivation)|is.na(input$cov_ethnicity)|is.na(input$cov_region)|is.na(input$cov_n_disorder)|is.na(input$cov_smoking_status)|is.na(input$qa_birth_year))
#input$rule2=(is.na(input$patient_id)|is.na(input$cov_age)|is.na(input$cov_sex)|is.na(input$cov_deprivation)|is.null(input$patient_id)|is.null(input$cov_age)|is.null(input$cov_sex)|is.null(input$cov_deprivation)|is.nan(input$patient_id)|is.nan(input$cov_age)|is.nan(input$cov_sex)|is.nan(input$cov_deprivation))
#I think is.na would probably be enough but not sure


#Rule 3: Year of birth predates NHS established year or year of birth exceeds current date
input$rule3=NA
input$rule3=((input$qa_birth_year <1793 & is.na(input$qa_birth_year) == FALSE) |(input$qa_birth_year >as.integer(format(Sys.Date(),"%Y")) & is.na(input$qa_birth_year) == FALSE))


#Rule 4: Date of death is NULL or invalid (on or before 1/1/1900 or after current date)
input$rule4=NA
input$rule4=((input$death_date <="1900-01-01"|input$death_date > format(Sys.Date(),"%Y-%m-%d")) & is.na(input$death_date) == FALSE)
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
input$rule9=((input$cov_sex=="M" & input$cov_hormone_replacement_therapy==1)|(input$cov_sex=="M" & input$cov_combined_oral_contraceptive_pill==1))


#Remove rows that are TRUE for at least one rule
input_QA=input[rule1==F & rule2==F & rule3==F & rule4==F & rule6 & rule7==F & rule9==F,]
input_QA=input_QA %>% select(-c(rule1,rule2,rule3,rule4,rule6,rule7,rule9))


#QA summary
QA_summary <- data.frame(matrix(ncol = 2))
colnames(QA_summary) <- c('Rule', '# where rule true')
QA_summary[1,1]="Rule 1"
QA_summary[1,2]=nrow(input%>%filter(rule1==T))
QA_summary[2,1]="Rule 2"
QA_summary[2,2]=nrow(input%>%filter(rule2==T))
QA_summary[3,1]="Rule 3"
QA_summary[3,2]=nrow(input%>%filter(rule3==T))
QA_summary[4,1]="Rule 4"
QA_summary[4,2]=nrow(input%>%filter(rule4==T))
QA_summary[5,1]="Rule 6"
QA_summary[5,2]=nrow(input%>%filter(rule6==T))
QA_summary[6,1]="Rule 7"
QA_summary[6,2]=nrow(input%>%filter(rule7==T))
QA_summary[7,1]="Rule 9"
QA_summary[7,2]=nrow(input%>%filter(rule9==T))
QA_summary[8,1]="Total excluded from QA"
QA_summary[8,2]=nrow(input)-nrow(input_QA)

#write.csv(QA_summary, file = file.path("output", paste0("QA_summary", ".csv")) , row.names=F)





