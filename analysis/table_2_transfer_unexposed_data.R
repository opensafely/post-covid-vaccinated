# Transfer unexposed data from main to covid pheno type subgroups
# Author: Yinghui Wei

library(readr); library(dplyr); library(data.table); library(lubridate)
library(stringr);library(tidyverse)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  population <- "vaccinated"
  #population = "electively_unvaccinated"
  analyses <- "subgroups"
}else{
  analyses <- args[[1]]
  population <- args[[2]]
}

transfer_unexposed_data_from_main_to_covid_pheno_subgrp <- function(table2_main, table2_subgroups, population)
{
  index_subgrp <- grep("covid_pheno_", table2_subgroups$subgroup)
  event_names <- unique(table2_subgroups$event)
  for(i in index_subgrp){
    event <- table2_subgroups$event[i]
    index_main <- grep(event, table2_main$event)
    table2_subgroups$unexposed_event_count[i] <- table2_main$unexposed_event_count[index_main]
    table2_subgroups$unexposed_person_days[i] <- table2_main$unexposed_person_days[index_main]
    table2_subgroups$unexposed_ir[i] <- table2_main$unexposed_ir[index_main]
    table2_subgroups$unexposed_ir_lower[i] <- table2_main$unexposed_ir_lower[index_main]
    table2_subgroups$unexposed_ir_upper[i] <- table2_main$unexposed_ir_upper[index_main]
  }

  # extract input1_aer
  input1_aer <- table2_subgroups %>% select(c("event", "cohort_to_run", "subgroup", "strata", "unexposed_person_days"))
  names(input1_aer)[which(names(input1_aer) == "cohort_to_run")] = "cohort"
  input1_aer$event <- ifelse(startsWith(input1_aer$event,"out_"),gsub("out_date_","",input1_aer$event),input1_aer$event)
  
  # write output for table2 subgroups
  write.csv(table2_subgroups, file=paste0("output/table2_subgroups", "_", population, ".csv"), row.names = F)
  rmarkdown::render("analysis/compiled_table2_results.Rmd",
                    output_file=paste0("table2_subgroups","_", population),output_dir="output")
  
  #write output fir input1_aer subgroups
  write.csv(input1_aer, file=paste0("output/input1_aer_subgroups", "_", population, ".csv"), row.names=F)
  rmarkdown::render("analysis/compiled_input1_aer_results.Rmd",
                    output_file=paste0("input1_aer_subgroups","_", population),output_dir="output")
  
}

table_2_transfer <- function(population){
  table2_main <- read.csv(paste0("output/table2_main_", population, ".csv"))
  table2_subgroups <- read.csv(paste0("output/table2_subgroups_", population, ".csv"))
  transfer_unexposed_data_from_main_to_covid_pheno_subgrp(table2_main, table2_subgroups, population)
}

# Run function using specified commandArgs
if(population == "both"){
  table_2_transfer("vaccinated")
  table_2_transfer("electively_unvaccinated")
}else{
  table_2_transfer(population)
}
