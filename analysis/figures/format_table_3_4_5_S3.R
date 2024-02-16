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
output_dir <- "C:/Users/gic30/OneDrive - University of Cambridge/2. Projects/Code/Post-covid-vaccinated - tables-figures formatting/"
  
dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

#----------------------------Get CVD outcomes-----------------------------------
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

active_analyses <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))

active_analyses$outcome <- gsub("Arterial thrombosis event","All arterial thromboses",active_analyses$outcome)
active_analyses$outcome <- gsub("Venous thrombosis event","All venous thromboses",active_analyses$outcome)


active_analyses_pre_vax <- read_rds("lib/active_analyses_pre_vax.rds") %>% filter(active == "TRUE")

active_analyses_pre_vax <- active_analyses_pre_vax %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses_pre_vax$outcome_variable %>% str_replace("out_date_", ""))%>%
  filter(grepl("extended_follow_up",outcome_variable))

#---------------Focus on all outcomes--------------
#outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name != c("ate","vte","ate_primary_position","vte_primary_position")]
outcomes_to_plot <- active_analyses$outcome_name
outcomes_to_plot_pre_vax <- active_analyses_pre_vax$outcome_name

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))


estimates <- estimates %>% filter(((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                            | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) 
                                            & ((event %in% outcomes_to_plot & cohort %in% c("vaccinated","electively_unvaccinated")) | (event %in% outcomes_to_plot_pre_vax & cohort %in% c("pre_vaccination")))
                                            & term %in% term[grepl("^days",term)])%>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

#----------------------Add empty rows for missing results-----------------------
df1 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("vaccinated","electively_unvaccinated"),
               c("reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_28","days28_197"))

colnames(df1) <- c("event","subgroup","cohort","time_points","model","term")

df2 <- crossing(outcomes_to_plot_pre_vax,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("pre_vaccination"),
                c("reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_28","days28_197","days197_365","days365_714"))

colnames(df2) <- c("event","subgroup","cohort","time_points","model","term")

df3 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("vaccinated","electively_unvaccinated"),
                c("day_zero_reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_1","days1_28","days28_197"))

colnames(df3) <- c("event","subgroup","cohort","time_points","model","term")

df4 <- crossing(outcomes_to_plot_pre_vax,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("pre_vaccination"),
                c("day_zero_reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_1","days1_28","days28_197","days197_365","days365_714"))

colnames(df4) <- c("event","subgroup","cohort","time_points","model","term")


df5 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("vaccinated","electively_unvaccinated"),
                c("normal"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197"))

colnames(df5) <- c("event","subgroup","cohort","time_points","model","term")

df6 <- crossing(outcomes_to_plot_pre_vax,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("pre_vaccination"),
                c("normal"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_365","days365_714"))

colnames(df6) <- c("event","subgroup","cohort","time_points","model","term")


df<- rbind(df1,df2,df3,df4,df5,df6)

estimates <- df %>% left_join(estimates)

estimates <- estimates %>% filter((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                            | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) %>% 
  dplyr::mutate(across(c(estimate,conf_low,conf_high),as.numeric))

rm(df1,df2,df3,df4,df5,df6,df)

#------------------------------Tidy event names---------------------------------
#Remove extended follow up from outcome name as this will not be written in the final table
estimates$event <- gsub("_extended_follow_up","",estimates$event)

estimates <- estimates %>% left_join(active_analyses %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                  " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                  "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))

# Remove sensitivity analysis --------------------------------------------------
estimates <- estimates %>% filter(!outcome %in% outcome[grepl("sensitivity",outcome)])

# Specify estimate order -----------------------------------------------------
estimates$outcome <- factor(estimates$outcome, levels=c("All arterial thromboses",
                                                                  "Acute myocardial infarction",
                                                                  "Ischaemic stroke",
                                                                  "All venous thromboses",
                                                                  "Pulmonary embolism",
                                                                  "Deep vein thrombosis",
                                                                  "Heart failure",
                                                                  "Angina",
                                                                  "Transient ischaemic attack",
                                                                  "Subarachnoid haemorrhage and haemorrhagic stroke",
                                                                  "All arterial thromboses - Primary position events",
                                                                  "Acute myocardial infarction - Primary position events",
                                                                  "Ischaemic stroke - Primary position events",
                                                                  "All venous thromboses - Primary position events",
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

unique(estimates$term)
estimates <- estimates %>% mutate(term = case_when(term == "days0_1"~ "0",
                                                   term == "days1_28" ~ "1-4",
                                                   term == "days0_28" ~ "1-4",
                                                   term == "days28_197" ~ "5-28",
                                                   term == "days197_365" ~ "29-52",
                                                   term == "days365_714" ~ "53-102",
                                                   term == "days0_7" ~ "1",
                                                   term == "days7_14" ~ "2",
                                                   term == "days14_28" ~ "3-4",
                                                   term == "days28_56" ~ "5-8",
                                                   term == "days56_84" ~ "9-12",
                                                   term == "days84_197" ~ "13-28",
                                                   TRUE ~ term))
 

# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model")] <- NULL

# Create specific estimates datasets -------------------------------------------
estimates_reduced_main_paper <- estimates %>% filter(time_points == "reduced" 
                                                     & !outcome %in% outcome[grepl("Primary position",outcome)]
                                                     & (subgroup == "All, maximally adjusted" | (subgroup %in% c("All, age/sex/region adjusted","Hospitalised COVID-19","Non-hospitalised COVID-19") & outcome %in% c("All arterial thromboses" ,"All venous thromboses"))))

estimates_reduced_primary_position_main_paper <- estimates %>% filter(time_points == "reduced" 
                                                                      & outcome %in% outcome[grepl("Primary position",outcome)]
                                                                      & (subgroup == "All, maximally adjusted" | (subgroup %in% c("All, age/sex/region adjusted","Hospitalised COVID-19","Non-hospitalised COVID-19") & outcome %in% c("All arterial thromboses" ,"All venous thromboses"))))

estimates_day_zero_reduced_main_paper <- estimates %>% filter(time_points == "day_zero_reduced" 
                                                              & !outcome %in% outcome[grepl("Primary position",outcome)]
                                                              & (subgroup == "All, maximally adjusted" | (subgroup %in% c("All, age/sex/region adjusted","Hospitalised COVID-19","Non-hospitalised COVID-19") & outcome %in% c("All arterial thromboses" ,"All venous thromboses"))))

estimates_day_zero_reduced_primary_position_main_paper <- estimates %>% filter(time_points == "day_zero_reduced" 
                                                                               & outcome %in% outcome[grepl("Primary position",outcome)]
                                                                               & (subgroup == "All, maximally adjusted" | (subgroup %in% c("All, age/sex/region adjusted","Hospitalised COVID-19","Non-hospitalised COVID-19") & outcome %in% c("All arterial thromboses" ,"All venous thromboses"))))

estimates_normal_main_paper <- estimates %>% filter(time_points == "normal" 
                                                    & !outcome %in% outcome[grepl("Primary position",outcome)]
                                                    & (subgroup == "All, maximally adjusted" | (subgroup %in% c("All, age/sex/region adjusted","Hospitalised COVID-19","Non-hospitalised COVID-19") & outcome %in% c("All arterial thromboses" ,"All venous thromboses"))))

estimates_normal_primary_position_main_paper <- estimates %>% filter(time_points == "normal"
                                                                     & outcome %in% outcome[grepl("Primary position",outcome)]
                                                                     & (subgroup == "All, maximally adjusted" | (subgroup %in% c("All, age/sex/region adjusted","Hospitalised COVID-19","Non-hospitalised COVID-19") & outcome %in% c("All arterial thromboses" ,"All venous thromboses"))))


estimates_reduced_supplementary <- estimates %>% filter(time_points == "reduced" 
                                                        & !outcome %in% outcome[grepl("Primary position",outcome)]
                                                        & !outcome %in% c("All arterial thromboses" ,"All venous thromboses")
                                                        & subgroup != "All, maximally adjusted")

estimates_reduced_primary_position_supplementary <- estimates %>% filter(time_points == "reduced"
                                                                         & outcome %in% outcome[grepl("Primary position",outcome)]
                                                                         & !outcome %in% c("All arterial thromboses" ,"All venous thromboses")
                                                                         & subgroup != "All, maximally adjusted")

estimates_day_zero_reduced_supplementary <- estimates %>% filter(time_points == "day_zero_reduced" 
                                                                 & !outcome %in% outcome[grepl("Primary position",outcome)]
                                                                 & !outcome %in% c("All arterial thromboses" ,"All venous thromboses")
                                                                 & subgroup != "All, maximally adjusted")

estimates_day_zero_reduced_primary_position_supplementary <- estimates %>% filter(time_points == "day_zero_reduced"
                                                                                  & outcome %in% outcome[grepl("Primary position",outcome)]
                                                                                  & !outcome %in% c("All arterial thromboses" ,"All venous thromboses")
                                                                                  & subgroup != "All, maximally adjusted")

estimates_normal_supplementary <- estimates %>% filter(time_points == "normal" 
                                                       & !outcome %in% outcome[grepl("Primary position",outcome)]
                                                       & !outcome %in% c("All arterial thromboses" ,"All venous thromboses")
                                                       & subgroup != "All, maximally adjusted")

estimates_normal_primary_position_supplementary <- estimates %>% filter(time_points == "normal"
                                                                        & outcome %in% outcome[grepl("Primary position",outcome)]
                                                                        & !outcome %in% c("All arterial thromboses" ,"All venous thromboses")
                                                                        & subgroup != "All, maximally adjusted")

# Convert long to wide -------------------------------------------------------
#df = estimates_reduced_supplementary
format_hr_table <- function(df, time_periods,outcome_position, save_name){
  df$time_points <- NULL
  df <- tidyr::pivot_wider(df, names_from = cohort, values_from = est)
  df <- df %>% select("outcome","subgroup","term", "Pre-vaccination","Vaccinated","Unvaccinated")

  if(grepl("day_zero_reduced", time_periods)){
    df$term <- factor(df$term, levels = c("0",
                                          "1-4",
                                          "5-28",
                                          "29-52",
                                          "53-102"))
  }
  else if(grepl("reduced", time_periods)){
      df$term <- factor(df$term, levels = c("1-4",
                                            "5-28",
                                            "29-52",
                                            "53-102"))
      
    }else{
      df$term <- factor(df$term, levels = c("1",
                                            "2",
                                            "3-4",
                                            "5-8",
                                            "9-12",
                                            "13-28",
                                            "29-52",
                                            "53-102"))
    }
  
  df <- df[order(df$outcome,df$subgroup,df$term),]
  
  
  df <- df %>% mutate(across(c("outcome","subgroup","term"),as.character))
  
  if(outcome_position == "any_position"){
    df[nrow(df)+1,] <- "Arterial thrombotic events"
    df[nrow(df)+1,] <- "Venous thrombotic events"
    df[nrow(df)+1,] <- "Other cardiovascular events"
    
    df$outcome <- factor(df$outcome, levels=c("Arterial thrombotic events",
                                                "All arterial thromboses",
                                                "Acute myocardial infarction",
                                                "Ischaemic stroke",
                                                "Venous thrombotic events",
                                                "All venous thromboses",
                                                "Pulmonary embolism",
                                                "Deep vein thrombosis",
                                                "Other cardiovascular events",
                                                "Heart failure",
                                                "Angina",
                                                "Transient ischaemic attack",
                                                "Subarachnoid haemorrhage and haemorrhagic stroke"))
  }else{
    df[nrow(df)+1,] <- "Arterial thrombotic events - Primary position events"
    df[nrow(df)+1,] <- "Venous thrombotic events - Primary position events"
    df[nrow(df)+1,] <- "Other cardiovascular events - Primary position events"
    
    df$outcome <- factor(df$outcome, levels=c("Arterial thrombotic events - Primary position events",
                                                "All arterial thromboses - Primary position events",
                                                "Acute myocardial infarction - Primary position events",
                                                "Ischaemic stroke - Primary position events",
                                                "Venous thrombotic events - Primary position events",
                                                "All venous thromboses - Primary position events",
                                                "Pulmonary embolism - Primary position events",
                                                "Deep vein thrombosis - Primary position events",
                                                "Other cardiovascular events - Primary position events",
                                                "Heart failure - Primary position events",
                                                "Angina - Primary position events",
                                                "Transient ischaemic attack - Primary position events",
                                                "Subarachnoid haemorrhage and haemorrhagic stroke - Primary position events"))
  }
  
  
  df <- df[order(df$outcome),]
  
  write.csv(df, file = paste0(output_dir,save_name,outcome_position,"_",time_periods,"_time_periods.csv"),row.names = F)
}


format_hr_table(estimates_reduced_main_paper,"reduced","any_position","table3_main_formatted_hr_")
format_hr_table(estimates_reduced_primary_position_main_paper,"reduced","primary_position","table3_main_formatted_hr_")
format_hr_table(estimates_day_zero_reduced_main_paper,"day_zero_reduced","any_position","table3_main_formatted_hr_") # Tables 3-4-5
format_hr_table(estimates_day_zero_reduced_primary_position_main_paper,"day_zero_reduced","primary_position","table3_main_formatted_hr_")
format_hr_table(estimates_normal_main_paper,"normal","any_position","table3_main_formatted_hr_")
format_hr_table(estimates_normal_primary_position_main_paper,"normal","primary_position","table3_main_formatted_hr_")

format_hr_table(estimates_reduced_supplementary,"reduced","any_position","table3_supplementary_formatted_hr_")
format_hr_table(estimates_reduced_primary_position_supplementary,"reduced","primary_position","table3_supplementary_formatted_hr_")
format_hr_table(estimates_day_zero_reduced_supplementary,"day_zero_reduced","any_position","table3_supplementary_formatted_hr_") # Table S3
format_hr_table(estimates_day_zero_reduced_primary_position_supplementary,"day_zero_reduced","primary_position","table3_supplementary_formatted_hr_")
format_hr_table(estimates_normal_supplementary,"normal","any_position","table3_supplementary_formatted_hr_")
format_hr_table(estimates_normal_primary_position_supplementary,"normal","primary_position","table3_supplementary_formatted_hr_")
