## =============================================================================
## Project:     Post covid vaccinated project
##
##
## Purpose:  Apply stage 1. Data cleaning
##  - Prepare variables
##  - Apply QA rules
##  - Apply inclusion exclusion criteria
##  - Create cleaned datasets for the following sub-cohorts:
##    a. Vaccinated cohort
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
## 0. Load relevant libraries and read data
## 1. Prepare all variables (re-factoring, re-typing)
##    1.a. Set factor variables as factor
##    1.b. Set the group with the highest frequency as the reference group
##    1.c. Check that continuous variables are defined as numeric variables
##    1.d. Check and specify date format for date variables
##    1.e. Apply changes in the input dataset
## 2. Apply QA rules
## 3. Apply exclusion/inclusion criteria
##    Differentiate criteria for the two sub-cohorts
##
## =============================================================================


###############################################
# 0. Load relevant libraries and read in data #
###############################################
library(readr)
library(dplyr)
library(stringr)

# Read a R dataset
input <-read_rds("output/input.rds") #View(input)


######################################################
# 1. Prepare all variables (re-factoring, re-typing) # 
######################################################

# Extract names of variables
variable_names <- tidyselect::vars_select(names(input), starts_with('cov_','qa_','vax_cat','exp_cat', ignore.case = TRUE))
# Create a data frame for all relevant variables
covars <- input[,variable_names] #View(covars)

# Replace " " with "_"
covars$cov_cat_region <- gsub(" ", "_", covars$cov_cat_region)

#-------------------------------------#
# 1.a. Set factor variables as factor #
#-------------------------------------#
# Get the names of variables which are factors
factor_names_bin <- tidyselect::vars_select(names(input), starts_with('cov_bin', ignore.case = TRUE))
factor_names_cat <- tidyselect::vars_select(names(input), starts_with('cov_cat', ignore.case = TRUE))
factor_names_qa_bin <- tidyselect::vars_select(names(input), starts_with('qa_bin', ignore.case = TRUE))
factor_names_vax_cat <- tidyselect::vars_select(names(input), starts_with('vax_cat', ignore.case = TRUE))
factor_names_exp_cat <- tidyselect::vars_select(names(input), starts_with('exp_cat', ignore.case = TRUE))
factor_names <- c(factor_names_bin, factor_names_cat, factor_names_qa_bin, factor_names_vax_cat, factor_names_exp_cat)

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

covars$exp_cat_covid19_hospital = relevel(covars$exp_cat_covid19_hospital, ref = as.character(calculate_mode(covars$exp_cat_covid19_hospital)))

covars$vax_cat_jcvi_group = relevel(covars$vax_cat_jcvi_group, ref = as.character(calculate_mode(covars$vax_cat_jcvi_group)))
covars$vax_cat_product_1 = relevel(covars$vax_cat_product_1, ref = as.character(calculate_mode(covars$vax_cat_product_1)))
covars$vax_cat_product_2 = relevel(covars$vax_cat_product_2, ref = as.character(calculate_mode(covars$vax_cat_product_2)))
covars$vax_cat_product_3 = relevel(covars$vax_cat_product_3, ref = as.character(calculate_mode(covars$vax_cat_product_3)))

#combine groups in deprivation: First - most deprived; fifth -least deprived
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==1 | levels(covars$cov_cat_deprivation)==2] <-"1-2 (most deprived)"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==3 | levels(covars$cov_cat_deprivation)==4] <-"3-4"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==5 | levels(covars$cov_cat_deprivation)==6] <-"5-6"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==7 | levels(covars$cov_cat_deprivation)==8] <-"7-8"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==9 | levels(covars$cov_cat_deprivation)==10] <-"9-10 (least deprived)"
covars$cov_cat_deprivation = relevel(covars$cov_cat_deprivation, ref = as.character(calculate_mode(covars$cov_cat_deprivation))) # added

# A simple check if factor reference level has changed
#lapply(covars[,c("cov_ethnicity", "cov_smoking_status", "cov_region")], table)
meta_data_factors <- lapply(covars[,factor_names], table)

# write.csv is not feasible to output list with uneven length
sink("output/meta_data_factors.csv")
print(meta_data_factors)
sink()

#----------------------------------------------------------------------#
# 1.c. Check that continuous variables are defined as numeric variable #
#----------------------------------------------------------------------#
# Notes: Age, number of GP consultations and year of birth are continuous variables

# Checking if continuous variables are set up as numeric variable correctly
#is.numeric(data$cov_num_age)
#is.numeric(data$cov_num_consulation_rate)
#is.numeric(data$qa_num_birth_year)
#str(covars)