library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(utils)

args = commandArgs(trailingOnly=TRUE)
project = args[[1]]#vaccinated, electively_unvaccinated

# Load fully adjusted main and COVID phenotype results------------------------------------
project="vaccinated"

main_max_adj_HR=read_csv(paste0("output/compiled_HR_results_main_",project,"_delta_mdl_max_adj_covid_history_false.csv"))
main_max_adj_HR=main_max_adj_HR%>%select(term,estimate,conf.low,conf.high,event,strata,model,covid_history)

main_age_sex_HR=read_csv(paste0("output/compiled_HR_results_main_",project,"_delta_mdl_agesex_covid_history_false.csv"))
main_age_sex_HR=main_age_sex_HR%>%select(term,estimate,conf.low,conf.high,event,strata,model,covid_history)

prior_covid_HR=read_csv(paste0("output/compiled_HR_results_main_",project,"_delta_mdl_max_adj_covid_history_true.csv"))
prior_covid_HR=prior_covid_HR%>%select(term,estimate,conf.low,conf.high,event,strata,model,covid_history)

subgroup_covid_pheno_HR=read_csv(paste0("output/compiled_HR_results_subgroup_covid_pheno_",project,"_delta_mdl_max_adj_covid_history_false.csv"))
subgroup_covid_pheno_HR=subgroup_covid_pheno_HR%>%select(term,estimate,conf.low,conf.high,event,strata,model,covid_history)



if(ncol(main_max_adj_HR)==8 & ncol(main_age_sex_HR)==8 & ncol(prior_covid_HR)==8 & ncol(subgroup_covid_pheno_HR)==8){
  #Combine results
  
  combined_HR=rbind(main_max_adj_HR,main_age_sex_HR,prior_covid_HR,subgroup_covid_pheno_HR)
  
  # Select HRs for time periods-------------------------------------------------
  
  combined_HR <- combined_HR %>% filter(str_detect(term, "^days"))
  
  # Make names 'nice' ----------------------------------------------------------
  
  combined_HR$event <- ifelse(combined_HR$event=="ami","Acute myocardial infarction",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="tia","Transient ischaemic attack",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="dvt","Deep vein thrombosis",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="hf","Heart failure",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="stroke_isch","Ischaemic stroke",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="angina","Angina",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="vte","Venous thromboembolism",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="pe","Pulmonary embolism",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="stroke_sah_hs","Subarachnoid haemorrhage and haemorrhagic stroke",combined_HR$event)
  combined_HR$event <- ifelse(combined_HR$event=="ate","Arterial thromboses",combined_HR$event)
  
  combined_HR$strata <- ifelse(combined_HR$strata=="main_main" & combined_HR$model=="mdl_max_adj" & combined_HR$covid_history=="covid_history_false","Maximally adjusted",combined_HR$strata )
  combined_HR$strata <- ifelse(combined_HR$strata=="main_main" & combined_HR$model=="mdl_agesex" & combined_HR$covid_history=="covid_history_false","Age/sex adjusted",combined_HR$strata )
  combined_HR$strata <- ifelse(combined_HR$strata=="main_main" & combined_HR$model=="mdl_max_adj" & combined_HR$covid_history=="covid_history_true","Prior history of COVID",combined_HR$strata )
  combined_HR$strata <- ifelse(combined_HR$strata=="expo_pheno_hospitalised","Hospitalised COVID-19",combined_HR$strata )
  combined_HR$strata <- ifelse(combined_HR$strata=="expo_pheno_non_hospitalised","Non-hospitalised COVID-19",combined_HR$strata )
  
  df=combined_HR
  # Specify estimate format ----------------------------------------------------  
  
  df$est <- paste0(ifelse(df$estimate>=10,sprintf("%.1f",df$estimate),sprintf("%.2f",df$estimate)),
                    " (",ifelse(df$conf.low>=10,sprintf("%.1f",df$conf.low),sprintf("%.2f",df$conf.low)),
                    "-",ifelse(df$conf.high>=10,sprintf("%.1f",df$conf.high),sprintf("%.2f",df$conf.high)),")")
  
  # Remove unnecessary variables -----------------------------------------------
  df <- df %>% select(term,event,strata,est)
  
  # Convert long to wide -------------------------------------------------------
  
  df <- tidyr::pivot_wider(df, names_from = term, values_from = est)
  
  # Specify estimate order -----------------------------------------------------
  
  df$event <- factor(df$event, levels=c("Acute myocardial infarction",
                                          "Ischaemic stroke",
                                          "Pulmonary embolism",
                                          "Deep vein thrombosis",
                                          "Transient ischaemic attack",
                                          "Subarachnoid haemorrhage and haemorrhagic stroke",
                                          "Heart failure",
                                          "Angina",
                                          "Arterial thromboses",
                                          "Venous thromboembolism")) 
  
  df$strata <- factor(df$strata, levels=c("Maximally adjusted",
                                             "Age/sex adjusted",
                                             "Hospitalised COVID-19",
                                             "Non-hospitalised COVID-19",
                                             "Prior history of COVID")) 
  
  df <- df[order(df$event,df$strata),]
  
  write.csv(df,paste0("output/supplementary_table_1_",project,"_.csv"))
  
}else{
  df=data.frame(ncol=1)
  colnames(df)="no_results"
  write.csv(df,paste0("output/supplementary_table_1_",project,"_.csv"))
}





