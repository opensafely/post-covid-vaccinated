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

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))


#---------------Focus on first 8 CVD outcomes (remove ate and vte)--------------
#outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name != c("ate","vte","ate_primary_position","vte_primary_position")]
outcomes_to_plot <- outcome_name_table$outcome_name

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))


estimates <- estimates %>% filter(((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                            | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) 
                                            & event %in% outcomes_to_plot 
                                            & term %in% term[grepl("^days",term)])%>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

#----------------------Add empty rows for missing results-----------------------
df1 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("pre_vaccination","vaccinated","electively_unvaccinated"),
               c("reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_28","days28_197","days197_535"))

colnames(df1) <- c("event","subgroup","cohort","time_points","model","term")

df2 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("pre_vaccination","vaccinated","electively_unvaccinated"),
                c("normal"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_535"))

colnames(df2) <- c("event","subgroup","cohort","time_points","model","term")

df<- rbind(df1,df2)

estimates <- df %>% left_join(estimates)

estimates <- estimates %>% filter((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                            | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) %>% 
  dplyr::mutate(across(c(estimate,conf_low,conf_high),as.numeric))

rm(df1,df2,df)
#------------------------------Tidy event names---------------------------------
estimates <- estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                  " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                  "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))


# Specify estimate order -----------------------------------------------------
estimates$outcome <- factor(estimates$outcome, levels=c("Arterial thrombosis event",
                                                                  "Acute myocardial infarction",
                                                                  "Ischaemic stroke",
                                                                  "Venous thrombosis event",
                                                                  "Pulmonary embolism",
                                                                  "Deep vein thrombosis",
                                                                  "Heart failure",
                                                                  "Angina",
                                                                  "Transient ischaemic attack",
                                                                  "Subarachnoid haemorrhage and haemorrhagic stroke",
                                                                  "Arterial thrombosis event - Primary position events",
                                                                  "Acute myocardial infarction - Primary position events",
                                                                  "Ischaemic stroke - Primary position events",
                                                                  "Venous thrombosis event - Primary position events",
                                                                  "Pulmonary embolism - Primary position events",
                                                                  "Deep vein thrombosis - Primary position events",
                                                                  "Heart failure - Primary position events",
                                                                  "Angina - Primary position events",
                                                                  "Transient ischaemic attack - Primary position events",
                                                                  "Subarachnoid haemorrhage and haemorrhagic stroke - Primary position events")) 


estimates$subgroup <- ifelse(estimates$model == "mdl_max_adj" & estimates$subgroup == "main", "All, maximally adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$model == "mdl_age_sex_region" & estimates$subgroup == "main", "All, age/sex/region adjusted",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_hospitalised", "Hospitalised COVID-19",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup == "covid_pheno_non_hospitalised", "Non-hospitalised COVID-19",estimates$subgroup)

estimates$subgroup <- factor(estimates$subgroup, levels = c("All, age/sex/region adjusted",
                                                            "All, maximally adjusted",
                                                            "Hospitalised COVID-19",
                                                            "Non-hospitalised COVID-19"))

estimates$cohort <- factor(estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 
levels(estimates$cohort) <- list("Pre-vaccination"="pre_vaccination", "Vaccinated"="vaccinated","Unvaccinated"="electively_unvaccinated")


# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model")] <- NULL

# Convert long to wide -------------------------------------------------------

format_hr_table <- function(df, time_periods,outcome_position){
  df$time_points <- NULL
  df <- tidyr::pivot_wider(df, names_from = cohort, values_from = est)
  df <- df %>% select("outcome","subgroup","term", "Pre-vaccination","Vaccinated","Unvaccinated")
  
  if(grepl("reduced", time_periods)){
    df$term <- factor(df$term, levels = c("days0_28",
                                          "days28_197",
                                          "days197_535"))
    
  }else{
    df$term <- factor(df$term, levels = c("days0_7",
                                          "days7_14",
                                          "days14_28",
                                          "days28_56",
                                          "days56_84",
                                          "days84_197",
                                          "days197_535"))
  }
  
  df <- df[order(df$outcome,df$subgroup,df$term),]
  tmp <- as.data.frame(matrix(ncol=ncol(df),nrow=0))
  colnames(tmp) <- colnames(df)
  
  for (i in unique(df$outcome)) {
    df1 <- df %>% filter(outcome == i)
    tmp[nrow(tmp)+1,] <- i
    tmp <- rbind(tmp,df1)
  }
  
  if(outcome_position == "any_position"){
    tmp[nrow(tmp)+1,] <- "Arterial thrombosis events"
    tmp[nrow(tmp)+1,] <- "Venous thromboembolism events"
    tmp[nrow(tmp)+1,] <- "Other vascular events"
    
    tmp$outcome <- factor(tmp$outcome, levels=c("Arterial thrombosis events",
                                                "Arterial thrombosis event",
                                                "Acute myocardial infarction",
                                                "Ischaemic stroke",
                                                "Venous thromboembolism events",
                                                "Venous thrombosis event",
                                                "Pulmonary embolism",
                                                "Deep vein thrombosis",
                                                "Other vascular events",
                                                "Heart failure",
                                                "Angina",
                                                "Transient ischaemic attack",
                                                "Subarachnoid haemorrhage and haemorrhagic stroke"))
  }else{
    tmp[nrow(tmp)+1,] <- "Arterial thrombosis events - Primary position events"
    tmp[nrow(tmp)+1,] <- "Venous thromboembolism events - Primary position events"
    tmp[nrow(tmp)+1,] <- "Other vascular events - Primary position events"
    
    tmp$outcome <- factor(tmp$outcome, levels=c("Arterial thrombosis events - Primary position events",
                                                "Arterial thrombosis event - Primary position events",
                                                "Acute myocardial infarction - Primary position events",
                                                "Ischaemic stroke - Primary position events",
                                                "Venous thromboembolism events - Primary position events",
                                                "Venous thrombosis event - Primary position events",
                                                "Pulmonary embolism - Primary position events",
                                                "Deep vein thrombosis - Primary position events",
                                                "Other vascular events - Primary position events",
                                                "Heart failure - Primary position events",
                                                "Angina - Primary position events",
                                                "Transient ischaemic attack - Primary position events",
                                                "Subarachnoid haemorrhage and haemorrhagic stroke - Primary position events"))
  }
  
  
  tmp <- tmp[order(tmp$outcome),]
  tmp$outcome <- NULL
  tmp <- tmp %>% dplyr::rename("Event" = "subgroup")
  
  write.csv(tmp, file = paste0(output_dir,"table3_main_formatted_hr_",outcome_position,"_",time_periods,"_time_periods.csv"),row.names = F)
}

estimates_reduced <- estimates %>% filter(time_points == "reduced" 
                                                    & !outcome %in% outcome[grepl("Primary position",outcome)])

estimates_reduced_primary_position <- estimates %>% filter(time_points == "reduced" 
                                                    & outcome %in% outcome[grepl("Primary position",outcome)])

estimates_normal <- estimates %>% filter(time_points == "normal" 
                                                    & !outcome %in% outcome[grepl("Primary position",outcome)])

estimates_normal_primary_position <- estimates %>% filter(time_points == "normal" 
                                                                     & outcome %in% outcome[grepl("Primary position",outcome)])


format_hr_table(estimates_reduced,"reduced","any_position")
format_hr_table(estimates_reduced_primary_position,"reduced","primary_position")
format_hr_table(estimates_normal,"normal","any_position")
format_hr_table(estimates_normal_primary_position,"normal","primary_position")
