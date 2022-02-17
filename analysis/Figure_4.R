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