## =============================================================================
## Project:     Post covid vaccinated project
##
##
## Purpose:  Apply stage 1. Data cleaning
##  - Prepare variables
##  - Apply QA rules
##  - Apply inclusion exclusion criteria
##  - Create cleaned datasets for the following sub-cohorts:
##    a. Vaccinated
##    b. Electively unvaccinated
## 
## Authors: Yinghui Wei, Renin Toms, Rochelle Knight, Genevieve Cezard
## Reviewer: Genevieve Cezard
## 
## Date combined: 13 December 2021
## by Genevieve Cezard
##
##
## Content: 
## 0. Load relevant libraries and read data/arguments
## 1. Prepare all variables (re-factoring, re-typing)
##    1.a. Set factor variables as factor
##    1.b. Set the group with the highest frequency as the reference group
##    1.c. Check that continuous variables are defined as numeric variables
##    1.d. Check and specify date format for date variables
##    1.e. Apply changes in the input dataset
## 2. Apply QA rules
## 3. Apply exclusion/inclusion criteria
##    (Differentiate criteria for the two sub-cohorts)
##    3.a. Apply the 6 common criteria applicable to both sub-cohort
##    3.b. Apply criteria specific to each sub-cohort
##    3.c. Create csv file 
## 4. Create the final stage 1 dataset 
## 
## NOTE: This code output are 3 .csv files and 1 R dataset
##       Output files have a specific name to reflect either the Vaccinated 
##       or Electively unvaccinated cohort
##
## =============================================================================


###############################################
# 0. Load relevant libraries and read in data #
###############################################
library(readr)
library(dplyr)
library(stringr)



# Get dataset for either the vaccinated or electively unvaccinated subcohort
# Specify command arguments ----------------------------------------------------
args = commandArgs(trailingOnly=TRUE)
input_filename = args[[1]] # tested with output/input.rds
cohort_name = args[[2]] # either "vaccinated" or "electively_unvaccinated"

input <-read_rds(input_filename)

# NOTE: Once study definition is updated, input_filename will be either 
# output/input_vaccinated.rds or output/input_electively_unvaccinated.rds

# Define general start date and end date
start_date = as.Date("2021-06-01")
end_date = as.Date("2021-12-14") # General End date: 2021-12-14 (Decision on Dec 20th 2021)

# NOTE: no censoring of end date for death/event at this stage

######################################################
# 1. Prepare all variables (re-factoring, re-typing) # 
######################################################

# Extract names of variables
variable_names <- tidyselect::vars_select(names(input), starts_with(c('sub_','cov_','qa_','vax_cat','exp_cat'), ignore.case = TRUE))

# Create a data frame for all relevant variables
covars <- input[,variable_names] #View(covars)

# Replace " " with "_"
covars$cov_cat_region <- gsub(" ", "_", covars$cov_cat_region)

#-------------------------------------#
# 1.a. Set factor variables as factor #
#-------------------------------------#
# Get the names of variables which are factors
factor_names <- tidyselect::vars_select(names(input), starts_with(c('sub_bin','sub_cat','cov_bin','cov_cat','qa_bin','vax_cat','exp_cat'), ignore.case = TRUE))

# Set the variables that should be factor variables as factor
covars[,factor_names] <- lapply(covars[,factor_names] , factor)

# Check the property of variables
#str(covars)

# Sort factor level alphabetically
mk_factor_orderlevels <- function(covars, colname)
{
  covars <- covars %>% mutate(
    !!sym(colname) := factor(!!sym(colname), levels = str_sort(unique(covars[[colname]]), numeric = TRUE)))
  return(covars)
}

for (colname in factor_names){
  #print(colname)
  covars <- mk_factor_orderlevels(covars, colname)
}


#----------------------------------------------------------------------#
# 1.b. Set the group with the highest frequency as the reference group #
#----------------------------------------------------------------------#
# Relevel

# Check the frequency for each factor level
#lapply(input[,factor_names], table)

# Find mode in a factor variable
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# For the following variables, the first level (reference level) is not the one with the highest frequency
# Set the most frequently occurred level as the reference for a factor variable
covars$cov_cat_ethnicity = relevel(covars$cov_cat_ethnicity, ref = as.character(calculate_mode(covars$cov_cat_ethnicity)))
covars$cov_cat_smoking_status = relevel(covars$cov_cat_smoking_status, ref = as.character(calculate_mode(covars$cov_cat_smoking_status)))
covars$cov_cat_region = relevel(covars$cov_cat_region, ref = as.character(calculate_mode(covars$cov_cat_region)))

covars$sub_cat_covid19_hospital = relevel(covars$sub_cat_covid19_hospital, ref = as.character(calculate_mode(covars$sub_cat_covid19_hospital)))

covars$vax_cat_jcvi_group = relevel(covars$vax_cat_jcvi_group, ref = as.character(calculate_mode(covars$vax_cat_jcvi_group)))
if (cohort_name == "vaccinated") {
  covars$vax_cat_product_1 = relevel(covars$vax_cat_product_1, ref = as.character(calculate_mode(covars$vax_cat_product_1)))
  covars$vax_cat_product_2 = relevel(covars$vax_cat_product_2, ref = as.character(calculate_mode(covars$vax_cat_product_2)))
  covars$vax_cat_product_3 = relevel(covars$vax_cat_product_3, ref = as.character(calculate_mode(covars$vax_cat_product_3)))
}

#combine groups in deprivation: First - most deprived; fifth -least deprived
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==1 | levels(covars$cov_cat_deprivation)==2] <-"1-2 (most deprived)"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==3 | levels(covars$cov_cat_deprivation)==4] <-"3-4"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==5 | levels(covars$cov_cat_deprivation)==6] <-"5-6"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==7 | levels(covars$cov_cat_deprivation)==8] <-"7-8"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==9 | levels(covars$cov_cat_deprivation)==10] <-"9-10 (least deprived)"
covars$cov_cat_deprivation = relevel(covars$cov_cat_deprivation, ref = as.character(calculate_mode(covars$cov_cat_deprivation))) # added

# A simple check if factor reference level has changed
#lapply(covars[,c("cov_cat_ethnicity", "cov_cat_smoking_status", "cov_cat_region","cov_cat_deprivation","exp_cat_covid19_hospital","vax_cat_jcvi_group","vax_cat_product_1","vax_cat_product_2","vax_cat_product_3")], table)

meta_data_factors <- lapply(covars[,factor_names], table)

# write.csv is not feasible to output list with uneven length
sink(file = file.path("output", paste0("meta_data_factors_",cohort_name, ".csv")))
print(meta_data_factors)
sink()

#----------------------------------------------------------------------#
# 1.c. Check that continuous variables are defined as numeric variable #
#----------------------------------------------------------------------#
# Notes: Age, number of GP consultations and year of birth are continuous variables

# Checking if continuous variables are set up as numeric variable correctly
#is.numeric(input$cov_num_age)
#is.numeric(input$cov_num_consulation_rate)
#is.numeric(input$qa_num_birth_year)
#str(covars)

#-------------------------------------------------------#
# 1.d. Check and specify date format for date variables #
#-------------------------------------------------------#
# Get the names of variables which are dates
date_names <- tidyselect::vars_select(names(input), starts_with(c('index_date','death_date','exp_date','out_date','vax_date'), ignore.case = TRUE))

# Set the variables that should be date variables as dates
for (colname in date_names){
  input[[colname]] <- as.Date(input[[colname]])
}


#-----------------------------------------#
# 1.e. Apply changes in the input dataset #
#-----------------------------------------#
input[,variable_names] <- covars
#str(input)


#####################
# 2. Apply QA rules #
#####################

#Rule 1: Year of birth is after year of death or patient only has year of death
input$rule1=NA
input$rule1=((input$qa_num_birth_year > (format(input$death_date, format="%Y")) & is.na(input$qa_num_birth_year)== FALSE & is.na(input$death_date) == FALSE)|(is.na(input$qa_num_birth_year)== TRUE & is.na(input$death_date) == FALSE))

#Rule 2: Year of birth predates NHS established year or year of birth exceeds current date
input$rule2=NA
input$rule2=((input$qa_num_birth_year <1793 |(input$qa_num_birth_year >format(Sys.Date(),"%Y"))) & is.na(input$qa_num_birth_year) == FALSE)

#Rule 3: Date of death is NULL or invalid (on or before 1/1/1900 or after current date)
input$rule3=NA
input$rule3=((input$death_date <="1900-01-01"|input$death_date > format(Sys.Date(),"%Y-%m-%d")) & is.na(input$death_date) == FALSE)

#Rule 4: Check index_date within expected range (between 2021-06-01 and 2021-12-14)
input$rule4=NA
input$rule4= (is.na(input$index_date)==TRUE | (is.na(input$index_date)==FALSE & (input$index_date < start_date | input$index_date > end_date)) )

#Rule 5: Pregnancy/birth codes for men
input$rule5=NA
input$rule5=(input$qa_bin_pregnancy == TRUE & input$cov_cat_sex=="M")

#Rule 6: HRT or COCP meds for men
input$rule6=NA
input$rule6=((input$cov_cat_sex=="M" & input$cov_bin_hormone_replacement_therapy==TRUE)|(input$cov_cat_sex=="M" & input$cov_bin_combined_oral_contraceptive_pill==TRUE))

#Rule 7: Prostate cancer codes for women
input$rule7=NA
input$rule7=(input$qa_bin_prostate_cancer == TRUE & input$cov_cat_sex=="F")



#Remove rows that are TRUE for at least one rule
input_QA=input%>%filter(rule1==FALSE & rule2==FALSE & rule3==FALSE & rule4==FALSE & rule5==FALSE & rule6==FALSE & rule7==FALSE)
input_QA=input_QA %>% select(-c(rule1,rule2,rule3,rule4,rule5,rule6,rule7))
# View(input_QA)

#Save QA'd input as .rds
#saveRDS(QA_summary,file = "output/QA_input.rds")

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
QA_summary[5,1]="Rule 5"
QA_summary[5,2]=nrow(input%>%filter(rule5==T))
QA_summary[6,1]="Rule 6"
QA_summary[6,2]=nrow(input%>%filter(rule6==T))
QA_summary[7,1]="Rule 7"
QA_summary[7,2]=nrow(input%>%filter(rule7==T))
QA_summary[8,1]="Total excluded from QA"
QA_summary[8,2]=nrow(input)-nrow(input_QA)


#Save Qa summary as .csv
write.csv(QA_summary, file = file.path("output", paste0("QA_summary_",cohort_name, ".csv")) , row.names=F)



#########################################
# 3. Apply exclusion/inclusion criteria #
#########################################
# (Differentiate criteria for the two sub-cohorts)

# Define the cohort flow
cohort_flow <- data.frame(N = numeric(),
                          Description = character(),
                          stringsAsFactors = FALSE)
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input_QA),"Study defined sample size")
input<-input_QA

#----------------------------------------------------------------#
# 3.a. Apply the 6 common criteria applicable to both sub-cohort #
#----------------------------------------------------------------#

#Inclusion criteria 1: Alive on the first day of follow up
input$start_alive <- ifelse(input$death_date < input$index_date, 0, 1) # Determine the living status on start date: 1- alive; 0 - died
input$start_alive[is.na(input$start_alive)] <- 1
input <- subset(input, input$start_alive == 1) # Subset input based on alive status on day 1 of follow up.
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 1 (Inclusion): Alive on the first day of follow up") # Feed into the cohort flow

#Inclusion criteria 2: Known age between 18 and 110 on 01/06/2021 
#input <- input[!is.na(input$cov_num_age),] # Commented out this code line since it should be dealt with in the next code line
input <- subset(input, input$cov_num_age >= 18 & input$cov_num_age <= 110) # Subset input if age between 18 and 110 on 01/06/2021.
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 2 (Inclusion): Known age between 18 and 110 on 01/06/2021") # Feed into the cohort flow

#Inclusion criteria 3: Known sex
input <- input[!is.na(input$cov_cat_sex),] # removes NAs, if any
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 3 (Inclusion): Known sex")

#Inclusion criteria 4: Known deprivation 
input <- input[!is.na(input$cov_cat_deprivation),] # removes NAs, if any
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 4 (Inclusion): Known deprivation")

#Inclusion criteria 5: Registered in an English GP with TPP software for at least 6 months prior to the study start date
# NOTE: Dealt with in Study definition
#input <- input # This criteria is met in study definition 
cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 5 (Inclusion): Registered in an English GP with TPP software for at least 6 months prior to the study start date")

#Exclusion criteria 6: SARS-CoV-2 infection recorded prior to the start of follow-up
# Removed for now as we need those with covid history for a sensitivity analysis
#input$prior_infections <- ifelse(input$exp_date_covid19_confirmed < input$index_date, 1,0)# Determine infections prior to start date : 1-prior infection; 0 - No prior infection
#input$prior_infections[is.na(input$prior_infections)] <- 0
#input <- subset(input, input$prior_infections ==0)
#cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 6 (Exclusion): SARS-CoV-2 infection recorded prior index date")


#-------------------------------------------------#
# 3.c. Apply criteria specific to each sub-cohort #
#-------------------------------------------------#

if (cohort_name == "vaccinated") {

  #Exclusion criteria 7: Do not have a record of two vaccination doses prior to the study end date
  input$vacc_gap <- input$vax_date_covid_2 - input$vax_date_covid_1 #Determine the vaccination gap in days : gap is NA if any vaccine date is missing
  input <- input[!is.na(input$vacc_gap),] # Subset the fully vaccinated group
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 7 (Exclusion): No record of two vaccination doses prior to the study end date") # Feed into the cohort flow
  
  #Exclusion criteria 8: Received a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)
  input <- subset(input, input$vax_date_covid_1 >= as.Date("2020-12-08")&input$vax_date_covid_2 >= as.Date("2020-12-08"))
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 8 (Exclusion): Recorded vaccination prior to the start date of vaccination program")
  
  #Exclusion criteria 9: Received a second dose vaccination before their first dose vaccination
  input <- subset(input, input$vacc_gap >= 0) # Keep those with positive vaccination gap
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 9 (Exclusion): Second dose vaccination recorded before the first dose vaccination")
  
  #Exclusion criteria 10: Received a second dose vaccination less than three weeks after their first dose
  input <- subset(input, input$vacc_gap >= 21) # Keep those with at least 3 weeks vaccination gap
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 10 (Exclusion): Second dose vaccination recorded less than three weeks after the first dose")
  
  #Exclusion criteria 11: Mixed vaccine products received before 07-05-2021
  # Trick to run the mixed vaccine code on dummy data with limited levels -> To ensure that the levels are the same in vax_cat_product variables
  levels(input$vax_cat_product_1) <- union(levels(input$vax_cat_product_1), levels(input$vax_cat_product_2))
  levels(input$vax_cat_product_2) <- levels(input$vax_cat_product_1)
  
  #Determines mixed vaccination before 7/5/2021
  input$vax_mixed <- ifelse((input$vax_cat_product_1!=input$vax_cat_product_2 & (is.na(input$vax_date_covid_2)==FALSE & input$vax_date_covid_2 < as.Date ("2021-05-07")) ),1,0)
  # Ensure vax_mixed is not NA but 0
  input$vax_mixed <- replace(input$vax_mixed, is.na(input$vax_mixed),0)
  #Determines unknown vaccine product before 7/5/2021
  input$vax_prior_unknown <- ifelse(is.na(input$vax_cat_product_1) | is.na(input$vax_cat_product_2), 1,0)# unknown products
  input$vax_prior_unknown <- ifelse(is.na(input$vax_date_covid_2), 1,input$vax_prior_unknown) #unknown vaccination 2 date
  input$vax_prior_unknown <- ifelse(input$vax_prior_unknown==1 & input$vax_date_covid_2 < as.Date ("2021-05-07"),1,0)#Remove if vaccination products are mixed or not known, prior to "2021-05-07"
  input <- subset(input, input$vax_mixed==0 | input$vax_prior_unknown==0)
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 11 (Exclusion): Received mixed vaccine products before 07-05-2021")

    
} else if (cohort_name == "electively_unvaccinated"){
  
  #Exclusion criteria 7: Have a record of one or more vaccination prior index date
  # i.e. Have a record of a first vaccination prior index date (no more vax 2 and 3 variables available in this dataset)
  #a.Determine the vaccination status on index start date
  input$prior_vacc1 <- ifelse(input$vax_date_covid_1 <= input$index_date, 1,0)
  input$prior_vacc1[is.na(input$prior_vacc1)] <- 0
  #input$prior_vacc2 <- ifelse(input$vax_date_covid_2 <= input$index_date, 1,0)
  #input$prior_vacc2[is.na(input$prior_vacc2)] <- 0
  #input$prior_vacc3 <- ifelse(input$vax_date_covid_3 <= input$index_date, 1,0)
  #input$prior_vacc3[is.na(input$prior_vacc3)] <- 0
  #input$prior_vacc <- ifelse((input$prior_vacc1==1 | input$prior_vacc2==1 |input$prior_vacc3==1), 1,0)
  #Note NAs don't have any vaccination date, hence move to '0' or unvaccinated category
  #input$prior_vacc[is.na(input$prior_vacc)] <- 0
  input <- subset(input, input$prior_vacc1 == 0) #Exclude people with prior vaccination
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 7 (Exclusion): Have a record of a first vaccination prior index date")
  
  #Exclusion criteria 8: Missing JCVI group
  input <- subset(input, is.na(input$vax_cat_jcvi_group)== FALSE)
  cohort_flow[nrow(cohort_flow)+1,] <- c(nrow(input),"Criteria 8 (Exclusion): Missing JCVI group")
  
}

#----------------------#
# 3.d. Create csv file #
#----------------------#
write.csv(cohort_flow, file = file.path("output", paste0("Cohort_flow_",cohort_name, ".csv")) , row.names=F)

#-------------------------------------#
# 4. Create the final stage 1 dataset #
#-------------------------------------#
saveRDS(input, file = file.path("output", paste0("input_",cohort_name, "_stage1.rds")))
