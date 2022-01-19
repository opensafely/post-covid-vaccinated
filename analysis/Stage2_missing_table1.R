## =============================================================================
## Project:     Post covid vaccinated project
##
##
## Purpose: 
##  Apply stage 2. Create missing data, range and descriptive statistics tables
##  - Table 0 - Missing data and range
##  - Table 1 - Descriptive statistics
## 
## Authors: Genevieve Cezard, Rochelle Knight
## Combined by: Genevieve Cezard
## Reviewer: Yinghui Wei
## 
## Content: 
## 0. Load relevant libraries and read data/arguments
## 1. Output missing data  and range check tables
## 2. Replace missing by a missing category
## 3. Output table 1
## 
## NOTE: This code outputs 3 .csv files and 1 R dataset
##       Output files have a specific name to reflect either the Vaccinated 
##       or Electively unvaccinated cohort
##
## =============================================================================

###############################################
# 0. Load relevant libraries and read in data #
###############################################
library(readr)
library(dplyr)


# Get dataset for either the vaccinated or electively unvaccinated subcohort
# Specify command arguments ----------------------------------------------------
args = commandArgs(trailingOnly=TRUE)
input_filename = args[[1]] # tested with output/input.rds
cohort_name = args[[2]] # either "vaccinated" or "electively_unvaccinated"

input <-read_rds(input_filename)

################################
# 1. Output missing data table #
################################
N <- nrow(input)

#-----------------------------------------------------------------------#
# 1.a. Create a table with missing data information (N,%) for variables #
#-----------------------------------------------------------------------#
# I have expanded here to more than just the covariates variable
check_missing <- data.frame(variable = character(), N_missing = character(), Perc_missing = character())
#check_missing[nrow(check_missing)+1,] <- c(" N",N,"")
covariate_names <- tidyselect::vars_select(names(input), starts_with(c('sub_','cov_','qa_','vax_cat','exp_cat'), ignore.case = TRUE))
for (i in covariate_names){
  check_missing[nrow(check_missing)+1,1] <- i
  check_missing[nrow(check_missing),2] <- nrow(input[is.na(input[,i]),])
  check_missing[nrow(check_missing),3] <- 100*(nrow(input[is.na(input[,i]),])/N)
}
#check_missing

#---------------------------------------------------------------#
# 1.b. Create a table with min and max for numerical covariates #
#---------------------------------------------------------------#
check_range <- data.frame(variable = character(), Minimum_value = character(), Maximum_value = character())
numeric_var_names=colnames(select_if(input, is.numeric))
#numeric_var_names = numeric_var_names [!numeric_var_names == "patient_id"]
numeric_var_names = numeric_var_names [!numeric_var_names %in% c("patient_id","start_alive","vax_mixed","vax_prior_unknown")]
# NOTE: change "start_alive","vax_mixed","vax_prior_unknown" to categorical/factor type in stage 1

for (i in numeric_var_names){
  check_range[nrow(check_range)+1,1] <- i
  check_range[nrow(check_range),2] <- min(na.omit(input %>% dplyr::select(i)))
  check_range[nrow(check_range),3] <- max(na.omit(input %>% dplyr::select(i)))
}
#check_range

#---------------------------------------------------------------------------#
# 1.a. c. Output table merging missing and range information for covariates #
#---------------------------------------------------------------------------#
check_both <- merge(x=check_missing, y=check_range, by = "variable",all.x=TRUE)
#check_both

write.csv(check_both, file = file.path("output", paste0("Check_missing_range_",cohort_name, ".csv")) , row.names=F)

#---------------------------------------------------------#
# 1.d. Create a table with min and max for date variables #
#---------------------------------------------------------#
check_dates <- data.frame(variable = character(), Ealiest_date = character(), Latest_date = character())
date_variables_names <- tidyselect::vars_select(names(input), starts_with(c('index_date','death_date','exp_date','out_date','vax_date'), ignore.case = TRUE))
input_date <- input[,date_variables_names]

for (i in date_variables_names){
  date_var = input_date %>% pull(i)
  check_dates[nrow(check_dates)+1,1] <- i
  check_dates[nrow(check_dates),2] <- paste0("",min(na.omit(date_var)))
  check_dates[nrow(check_dates),3] <- paste0("",max(na.omit(date_var)))
}
#check_dates
write.csv(check_dates, file = file.path("output", paste0("Check_dates_range_",cohort_name, ".csv")) , row.names=F)


############################################
# 2. Replace missing by a missing category #
############################################

# Change for smoking status variable
#table(input$cov_cat_smoking_status)
input$cov_cat_smoking_status <- replace(input$cov_cat_smoking_status, is.na(input$cov_cat_smoking_status),"M")

# Check ethnicity categories (NOTE: No NA for ethnicity in this dummy data)
# table(input$cov_cat_ethnicity)


#####################
# 3. Output table 1 #
#####################

#Define populations of interest
pop <- data.frame(rbind(c("Whole_population","!is.na(input$patient_id)"),
                        c("COVID_exposed","is.na(input$exp_date_covid19_confirmed)==F"),
                        c("COVID_hospitalised","input$exp_cat_covid19_hospital=='hospitalised'"),
                        c("COVID_non_hospitalised","input$exp_cat_covid19_hospital=='non_hospitalised'")
), stringsAsFactors = FALSE)

colnames(pop) <- c("name","condition")

#Create categories for age, deprivation and No. primary care consultationinput = input %>% 
input=input%>%mutate(cov_cat_age_group = 
                       case_when((cov_num_age>=18 & cov_num_age<=29) ~ "18-29",
                                 (cov_num_age>=30 & cov_num_age<=39) ~ "30-39",
                                 (cov_num_age>=40 & cov_num_age<=49) ~ "40-49",
                                 (cov_num_age>=50 & cov_num_age<=59) ~ "50-59",
                                 (cov_num_age>=60 & cov_num_age<=69) ~ "60-69",
                                 (cov_num_age>=70 & cov_num_age<=79) ~ "70-79",
                                 (cov_num_age>=80 & cov_num_age<=89) ~ "80-89",
                                 (cov_num_age>=90) ~ "90+",
                                 TRUE ~ "other"
                       ))
input = input %>% 
  mutate(cov_cat_consulation_rate_group = 
           case_when((cov_num_consulation_rate==0) ~ "0",
                     (cov_num_consulation_rate>=1 & cov_num_consulation_rate<=5) ~ "1-6",
                     (cov_num_consulation_rate>=6) ~ "6+",
                     TRUE ~ "other"
           ))



# Populate table 1 -------------------------------------------------------------
categorical_cov=colnames(input)[grep("cov_cat", colnames(input))]

numerical_cov=colnames(input)[grep("cov_num", colnames(input))]
numerical_cov=numerical_cov[!numerical_cov=="cov_num_age"]

binary_cov=colnames(input)[grep("cov_bin", colnames(input))]

#Base table
table1=input%>% dplyr::select(c(categorical_cov,numerical_cov,binary_cov))
table1= table1 %>% mutate_if(is.character,as.factor)
table1= table1 %>% mutate_if(is.logical,as.factor)
table1=as.data.frame(summary(table1,maxsum=50))
table1=rename(table1, Covariate_level = Freq, Covariate = Var2)
table1 <- table1 %>% filter(!str_detect(Covariate_level, "^FALSE"))

table1=table1%>% dplyr::select("Covariate","Covariate_level")
table1$Covariate=gsub("\\s","",table1$Covariate)#Remove spaces
table1$Covariate_level= sub('\\:.*', '', table1$Covariate_level)#Remove everything after :
table1=table1%>%filter(!(Covariate == "cov_num_consulation_rate" & !Covariate_level=="Mean   "))

table1_count_all= as.data.frame(matrix(nrow = 1, ncol = 2))
colnames(table1_count_all)=c("Covariate","Covariate_level")
table1_count_all[1,]=c("All","All")
table1=rbind(table1_count_all,table1)

for (j in 1:nrow(pop)) {
  
  population <- pop[j,]$name
  
  df <- subset(input, eval(parse(text = pop[j,]$condition)))
  
  #Count population size
  pop_summary=data.frame(matrix(ncol=3))
  colnames(pop_summary)=c("Covariate","Covariate_level",population)
  pop_summary[1,]=c("All","All",nrow(df))
  
  #Categorical covariates
  cat_pop=df %>% dplyr::select(categorical_cov)
  #cat_pop= cat_pop %>% mutate_if(is.character,as.factor)
  cat_summary=as.data.frame(summary(cat_pop,maxsum=50))
  cat_summary[,population]=cat_summary$Freq
  cat_summary=rename(cat_summary, Covariate_level = Freq, Covariate = Var2)
  cat_summary=cat_summary%>% dplyr::select("Covariate","Covariate_level",population)
  
  
  #Numerical covariates
  num_pop=df %>% dplyr::select(numerical_cov)
  num_summary=as.data.frame(summary(num_pop,maxsum=50))
  num_summary[,population]=num_summary$Freq
  num_summary=rename(num_summary, Covariate_level = Freq, Covariate = Var2)
  num_summary=num_summary%>% dplyr::select("Covariate","Covariate_level",population)
  num_summary=num_summary %>%filter(startsWith(num_summary$Covariate_level, "Mean")==T)
  
  #Binary covariates
  bin_pop=df %>% dplyr::select(binary_cov)
  #bin_pop= bin_pop %>% mutate_if(is.logical,as.factor)
  bin_summary=as.data.frame(summary(bin_pop,maxsum=50))
  bin_summary[,population]=bin_summary$Freq
  bin_summary=rename(bin_summary, Covariate_level = Freq, Covariate = Var2)
  bin_summary <- bin_summary %>% filter(str_detect(Covariate_level, "^TRUE"))
  bin_summary=bin_summary%>% dplyr::select("Covariate","Covariate_level",population)
  
  #Population summary
  pop_summary=rbind(pop_summary,cat_summary,num_summary,bin_summary)
  
  #Tidy summary table
  pop_summary$Covariate=gsub("\\s","",pop_summary$Covariate)#Remove spaces
  pop_summary$Covariate_level= sub('\\:.*', '', pop_summary$Covariate_level)#Remove everything after :
  #pop_summary$Covariate_level=gsub("\\s","",pop_summary$Covariate_level)#Remove spaces
  
  pop_summary[,population]=gsub(".*:", "",pop_summary[,population])#Remove everything before:
  pop_summary=pop_summary %>% drop_na(Covariate_level)#Remove rows with NA
  
  #Left join onto base table
  table1=left_join(table1,pop_summary, by=c("Covariate","Covariate_level"))
}


#Tidy table 1
table1$Covariate=gsub("cov_bin_", "History of ",table1$Covariate)
table1$Covariate=gsub("cov_\\D\\D\\D_", "",table1$Covariate)
table1$Covariate=gsub("_", " ",table1$Covariate)

write.csv(table1, file = file.path("output", paste0("Table_1_",cohort_name, ".csv")) , row.names=F)

