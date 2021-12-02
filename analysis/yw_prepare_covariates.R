## =============================================================================
## Purpose:  Prepare covariates
## 
## Author:   Yinghui Wei
## 
## Date:     19 November 2021
##
## Data:     Post covid vaccinated project study population
##
## Content:  1. Set factor variables as factor; set the group with
##              the highest frequency as the reference group;
##           2. Check that continuous variables are defined as numeric variables.
##           3. check the number and percentage of missing values for covariates
## =============================================================================
#
# Notes:
# Assumption applied: age, deprivation and number of disorders are currently assumed to be continuous variables
#
# YW identified data queries:
#    1. table(data$cov_n_disorder) includes a negative value -1 (resolved in study definition 2)
#    2. table(data$cov_ethnicity) returns 17 ethnic groups, should merge some.
#       no names for ethnicity, all in numbers
#    3. cov_smoking_status = E, M, N, S, needs more detailed descriptions in the text about these labels (resolved)

# YW Notes:
#    1. No "NA" in covariates apart from smoking status, 50% "NA" in this covariate

# YW 24/Nov/2021 make deprivation as a categorical variable
#                deprivation: 11 unique integers, from 0 to 10. 
#                group 1-2 (most deprived) 3-4, 5-6, 7-8, 9-10 (least deprived), is 0 missing?
# YW updated 24/Nov/2021 make deprivation as a categorical variable


library(readr); library(dplyr); library(stringr)

# read in data
input <-read_rds("output/input.rds")

# extract names of covariates
covariate_names <- tidyselect::vars_select(names(input), starts_with('cov_', ignore.case = TRUE))

# create a data frame for covariates
covars <- input[,covariate_names]

#----------------------- REPLACE " " with "_" for glht's linfct-----------------
covars$cov_cat_region <- gsub(" ", "_", covars$cov_cat_region)

# names of variables which are factors
factor_names_bin <- tidyselect::vars_select(names(input), starts_with('cov_bin', ignore.case = TRUE))
factor_names_cat <- tidyselect::vars_select(names(input), starts_with('cov_cat', ignore.case = TRUE))

factor_names <- c(factor_names_bin, factor_names_cat)

# The variables that should be factor variables
covars[,factor_names] <- lapply(covars[,factor_names] , factor)

# check the property of variables
#str(covars)

# sort factor level alphabetically
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

##------Relevel to set the group which has the highest frequency as reference group -----------------------------------------

# check the frequency for each factor level
#lapply(input[,factor_names], table)

# Find mode in a factor variable
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# For the following variables, the first level (reference level) is not the one with the highest frequency
# set the most frequently occurred level as the reference reference for a factor variable
covars$cov_cat_ethnicity = relevel(covars$cov_cat_ethnicity, ref = as.character(calculate_mode(covars$cov_cat_ethnicity)))
covars$cov_cat_smoking_status = relevel(covars$cov_cat_smoking_status, ref = as.character(calculate_mode(covars$cov_cat_smoking_status)))
covars$cov_cat_region = relevel(covars$cov_cat_region, ref = as.character(calculate_mode(covars$cov_cat_region)))

#combine groups in deprivation: First - most deprived; fifth -least deprived
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==1 | levels(covars$cov_cat_deprivation)==2] <-"1-2 (most deprived)"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==3 | levels(covars$cov_cat_deprivation)==4] <-"3-4"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==5 | levels(covars$cov_cat_deprivation)==6] <-"5-6"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==7 | levels(covars$cov_cat_deprivation)==8] <-"7-8"
levels(covars$cov_cat_deprivation)[levels(covars$cov_cat_deprivation)==9 | levels(covars$cov_cat_deprivation)==10] <-"9-10 (least deprived)"

# a simple check if factor reference level has changed
#lapply(covars[,c("cov_ethnicity", "cov_smoking_status", "cov_region")], table)
meta_data_factors <- lapply(covars[,factor_names], table)

sink("output/meta_data_factors.csv")
print(meta_data_factors)
sink()
##------------------------------- NUMERICAL Variables --------------------------------------
# Checking if continuous covariates are set up as numeric variable correctly
#is.numeric(data$cov_num_age); is.numeric(data$cov_num_consulation_rate); 
#str(covars)

input[,covariate_names] <- covars
#str(input)
