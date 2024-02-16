library(dplyr)
library(data.table)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
rm(list = ls())

#results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
#output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/formatted_hr_tables/"

results_dir <- "C:/Users/gic30/OneDrive - University of Cambridge/2. Projects/Code/Post-covid-vaccinated - tables-figures formatting"
output_dir <- "C:/Users/gic30/OneDrive - University of Cambridge/2. Projects/Code/Post-covid-vaccinated - tables-figures formatting"

dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)


estimates <- read.csv(paste0(results_dir,"/hr_output_formatted_month1_split.csv"))

estimates <- estimates %>% filter(((subgroup %in% c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) 
                                  & term %in% term[grepl("^days",term)]) %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

#-------------------------------Specify estimate format ------------------------  
estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                                                            " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                                                            "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))

# Name outcomes ----------------------------------------------------------------
estimates$outcome <- ifelse(estimates$event == "ate","Arterial thrombosis event",(ifelse(estimates$event == "vte","Venous thrombosis event","")))

# Specify estimate order -----------------------------------------------------
estimates$subgroup <- ifelse(estimates$model == "mdl_max_adj" & estimates$subgroup == "main", "All, maximally adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_hospitalised", "Hospitalised COVID-19",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_non_hospitalised", "Non-hospitalised COVID-19",estimates$subgroup)

estimates$subgroup <- factor(estimates$subgroup, levels = c("All, maximally adjusted",
                                                            "Hospitalised COVID-19",
                                                            "Non-hospitalised COVID-19"))


# Format time periods
estimates <- estimates %>% mutate(term = case_when(term == "days0_1" ~ "Day 0",
                                                   term == "days1_7" ~ "1-6",
                                                   term == "days7_14" ~ "7-13",
                                                   term == "days14_21" ~ "14-20",
                                                   term == "days21_28" ~ "21-27",
                                                   term == "days28_197" ~ "28-196",
                                                   term == "days197_365" ~ "197-364",
                                                   term == "days365_714" ~ "365-714",
                                                   TRUE ~ term))

estimates$term <- factor(estimates$term, levels = c("Day 0","1-6","7-13","14-20","21-27","28-196","197-364","365-714"))

estimates[,c("event","estimate","conf_low","conf_high","model","time_points")] <- NULL

#Pivot table
estimates <- tidyr::pivot_wider(estimates, names_from = cohort, values_from = est)

# Order table
estimates <- estimates[order(estimates$outcome,estimates$subgroup,estimates$term),]

#Format table
estimates <- estimates %>% select("outcome","subgroup","term", "pre_vaccination","vaccinated","electively_unvaccinated")
estimates <- estimates %>% mutate(across(colnames(estimates),as.character))

estimates[nrow(estimates)+1,] <- "All arterial thromboses"
estimates[nrow(estimates)+1,] <- "All venous thromboses"

estimates$outcome <- factor(estimates$outcome, levels=c("All arterial thromboses",
                                          "Arterial thrombosis event",
                                          "All venous thromboses",
                                          "Venous thrombosis event"))

estimates <- estimates[order(estimates$outcome),]

write.csv(estimates, file = paste0(output_dir,"/supplementary_table6_m1split.csv"),row.names = F)
