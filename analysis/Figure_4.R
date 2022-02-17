#Project:Vaccinated delta wave population study
#Branch:Figure4_graphical plotting of the estimated AER of ATE and VTE 
#Scripts: Renin Toms

library(purrr)
library(data.table)
library(tidyverse)

#Import data 
input1 <- readr::read_csv("output/input1_aer.csv") #1.person days
#input2 <- readr::read_csv("output/input2_aer.csv") #2.unexposed events, 3.total cases, 4.hr
#Import data
hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")
hr_files=paste0("output/",hr_files)
hr_file_paths <- purrr::pmap(list(hr_files),
                             function(fpath){
                               df <- fread(fpath)
                               return(df)
                             })
df=rbindlist(hr_file_paths, fill=TRUE)

#Preprocess the AER input data
input2 <- subset(df, df$term == "days0_14" |
                   df$term == "days14_28" |
                   df$term == "days28_56" |
                   df$term == "days56_84" |
                   df$term == "days84_197"|
                   df$term == "days0_28"|
                   df$term == "days28_197") # RT/RK check
input2 <- input2 %>% select(-conf.low, -conf.high, -std.error, -robust.se, -P, -covariates_removed, -cat_covars_collapsed)

#limit to ATE & VTE outcomes
input2 <- subset(input2, input2$event == "ate" | input2$event == "vte")
#---------------------------------
# Step1: Extract the required variables
#---------------------------------
#1. Person days
fp_person_days <- input1[input1$event == "ate" & input1$model == "mdl_max_adj" &
                           input1$cohort == "vaccinated" & input1$strata == "main",]$person_days#RT/RK/VW -check L

#2.unexposed events
unexposed_events <-  input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                              input2$cohort == "vaccinated" & input2$subgroup == "main" & 
                              input2$expo_week== "pre expo",]$events_total

#3.Total cases
total_cases <-  input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                         input2$cohort == "vaccinated" & input2$subgroup == "main" & 
                         input2$expo_week== "pre expo",]$total_covid19_cases

#4.locate the estimates
#0-14 days
hr_14 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                  input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days0_14",]$estimate
#14-28 days
hr_28 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                  input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days14_28",]$estimate
#28-56 days
hr_56 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                  input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days28_56",]$estimate
#56-84 days
hr_84 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                  input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days56_84",]$estimate
#84-196 days
hr_196 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                   input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days84_197",]$estimate
#Alternative 0-28 days
hr0_28 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                   input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days0_28",]$estimate
#Alternative 28 - 196 days
hr28_196<- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                    input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days28_196",]$estimate


