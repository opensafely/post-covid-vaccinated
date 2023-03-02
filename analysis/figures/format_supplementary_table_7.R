library(dplyr)
library(data.table)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
rm(list = ls())

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/formatted_hr_tables/"
dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

#----------------------------Get CVD outcomes-----------------------------------
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

active_analyses <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", "")) %>%
  filter(outcome_variable %in% c("out_date_ate_unvax_sens","out_date_vte_unvax_sens",
                                 "out_date_ate","out_date_vte")) 


active_analyses$outcome <- gsub("Arterial thrombosis event","All arterial thromboses",active_analyses$outcome)
active_analyses$outcome <- gsub("Venous thrombosis event","All venous thromboses",active_analyses$outcome)


# Load all estimates
outcomes_to_plot <- active_analyses$outcome_name

estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

estimates <- estimates %>% filter(subgroup %in% c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised")
                                  & cohort == "electively_unvaccinated"
                                  & event %in% outcomes_to_plot
                                  & term %in% term[grepl("^days",term)]
                                  & model == "mdl_max_adj"
                                  & time_points == "reduced") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)
