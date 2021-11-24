## =============================================================================
## Purpose:  Check missing data and range
##
## Data:     Post Covid vaccinated project study population
##
## Content: (Descriptive statistics are presented in table in a wide format)
##          1. Create a table with missing data information (N,%) for covariates
##          2. Create a table with min and max for numerical covariates
##          3. Output table merging missing and range information for covariates
##          4. Create a table with min and max for date variables
##
## Created by:   Genevieve Cezard
## Updated by:
## 
## Created on:   22 November 2021, tables with information presented in a wide format
## Updated on: 
##
## =============================================================================

# libraries
library(dplyr)
library(readr)

# 0. Reading data
#input <- read_csv("../output/input.csv")

# Specify command arguments ----------------------------------------------------
args = commandArgs(trailingOnly=TRUE)
path_dataset  = args[[1]] # "../output/input.csv"

input <- read_csv(path_dataset)


#####################################################################
# Create tables with missing information and min-max in Wide format #
#####################################################################
N <- nrow(input)

# 1. Create a table with missing data information (N,%) for covariates
check_missing <- data.frame(variable = character(), N_missing = character(), Perc_missing = character())
check_missing[nrow(check_missing)+1,] <- c("N",N,"")
covariate_names <- tidyselect::vars_select(names(input), starts_with('cov_', ignore.case = TRUE))
for (i in covariate_names){
  check_missing[nrow(check_missing)+1,1] <- i
  check_missing[nrow(check_missing),2] <- nrow(input[is.na(input[,i]),])
  check_missing[nrow(check_missing),3] <- 100*(nrow(input[is.na(input[,i]),])/N)
}
#check_missing


# 2. Create a table with min and max for numerical covariates
check_range <- data.frame(variable = character(), Minimum_value = character(), Maximum_value = character())
input_numeric <- select_if(input, is.numeric)
numeric_var_names=colnames(input_numeric)
numeric_var_names = numeric_var_names [!numeric_var_names == "patient_id"]

for (i in numeric_var_names){
  num_var = input_numeric %>% dplyr::select(i)
  check_range[nrow(check_range)+1,1] <- i
  check_range[nrow(check_range),2] <- min(na.omit(num_var))
  check_range[nrow(check_range),3] <- max(na.omit(num_var))
}
#check_range


# 3. Output table merging missing and range information for covariates
check_both <- merge(x=check_missing, y=check_range, by = "variable",all.x=TRUE)
#check_both

data.table::fwrite(check_both,"../output/Check_missing_range_covariates.csv")


# 4. Create a table with min and max for date variables
check_dates <- data.frame(variable = character(), Ealiest_date = character(), Latest_date = character())
date_variables_names <- tidyselect::vars_select(names(input), ends_with(c('_date','_date1','_date2'), ignore.case = TRUE))
input_date <- input[,date_variables_names]

for (i in date_variables_names){
  date_var = input_date %>% pull(i)
  check_dates[nrow(check_dates)+1,1] <- i
  check_dates[nrow(check_dates),2] <- paste0("",min(na.omit(date_var)))
  check_dates[nrow(check_dates),3] <- paste0("",max(na.omit(date_var)))
}
#check_dates
data.table::fwrite(check_dates,"../output/Check_range_dates.csv")

