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


# Load all estimates
outcomes_to_plot <- active_analyses$outcome_name

estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

estimates <- estimates %>% filter(((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                   | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) 
                                  & cohort == "electively_unvaccinated"
                                  & event %in% outcomes_to_plot
                                  & term %in% term[grepl("^days",term)]
                                  & time_points == "reduced") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                                                            " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                                                            "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))

# Specify estimate order -----------------------------------------------------

estimates$subgroup <- ifelse(estimates$model == "mdl_max_adj" & estimates$subgroup == "main", "All, maximally adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$model == "mdl_age_sex_region" & estimates$subgroup == "main", "All, age/sex/region adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_hospitalised", "Hospitalised COVID-19",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_non_hospitalised", "Non-hospitalised COVID-19",estimates$subgroup)

estimates$subgroup <- factor(estimates$subgroup, levels = c("All, age/sex/region adjusted",
                                                            "All, maximally adjusted",
                                                            "Hospitalised COVID-19",
                                                            "Non-hospitalised COVID-19"))


estimates$cohort <- ifelse(grepl("unvax_sens",estimates$event), "Unvaccinated cohort without censoring at vaccination", "Unvaccinated cohort")
estimates$cohort <- factor(estimates$cohort, levels=c("Unvaccinated cohort","Unvaccinated cohort without censoring at vaccination")) 

#------------------------------Tidy event names---------------------------------
estimates$event <- gsub("_unvax_sens","",estimates$event)
estimates <- estimates %>% left_join(active_analyses %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

estimates$event <- factor(estimates$event, levels = c("ate","vte"))

# Format time periods

estimates <- estimates %>% mutate(term = case_when(term == "days0_28" ~ "1-4",
                                                   term == "days28_197" ~ "5-28",
                                                   TRUE ~ term))

estimates$term <- factor(estimates$term, levels = c("1-4","5-28"))

estimates[,c("event","estimate","conf_low","conf_high","model","time_points")] <- NULL

#Pivot table
estimates <- tidyr::pivot_wider(estimates, names_from = cohort, values_from = est)

# Order table
estimates <- estimates[order(estimates$outcome,estimates$subgroup,estimates$term),]

#Format table
estimates <- estimates %>% select("outcome","subgroup","term", "Unvaccinated cohort","Unvaccinated cohort without censoring at vaccination")
estimates <- estimates %>% mutate(across(colnames(estimates),as.character))

estimates[nrow(estimates)+1,] <- "All arterial thromboses"
estimates[nrow(estimates)+1,] <- "All venous thromboses"

estimates$outcome <- factor(estimates$outcome, levels=c("All arterial thromboses",
                                          "Arterial thrombosis event",
                                          "All venous thromboses",
                                          "Venous thrombosis event"))

estimates <- estimates[order(estimates$outcome),]

write.csv(estimates, file = paste0(output_dir,"/supplementary_table5_unvaccinated_comparison.csv"),row.names = F)
