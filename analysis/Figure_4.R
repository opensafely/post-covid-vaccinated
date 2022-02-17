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

