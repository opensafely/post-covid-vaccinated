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
main_max_adj_counts=main_max_adj_HR%>%select(expo_week,events_total,event,strata,model,covid_history)
main_max_adj_HR=main_max_adj_HR%>%select(term,estimate,conf.low,conf.high,event,strata,model,covid_history)

main_age_sex_HR=read_csv(paste0("output/compiled_HR_results_main_",project,"_delta_mdl_agesex_covid_history_false.csv"))
main_age_sex_counts=main_age_sex_HR%>%select(expo_week,events_total,event,strata,model,covid_history)
main_age_sex_HR=main_age_sex_HR%>%select(term,estimate,conf.low,conf.high,event,strata,model,covid_history)

prior_covid_HR=read_csv(paste0("output/compiled_HR_results_main_",project,"_delta_mdl_max_adj_covid_history_true.csv"))
prior_covid_counts=prior_covid_HR%>%select(expo_week,events_total,event,strata,model,covid_history)
prior_covid_HR=prior_covid_HR%>%select(term,estimate,conf.low,conf.high,event,strata,model,covid_history)

subgroup_covid_pheno_HR=read_csv(paste0("output/compiled_HR_results_subgroup_covid_pheno_",project,"_delta_mdl_max_adj_covid_history_false.csv"))
subgroup_covid_pheno_counts=subgroup_covid_pheno_HR%>%select(expo_week,events_total,event,strata,model,covid_history)
subgroup_covid_pheno_HR=subgroup_covid_pheno_HR%>%select(term,estimate,conf.low,conf.high,event,strata,model,covid_history)



if(ncol(main_max_adj_HR)==8 & ncol(main_age_sex_HR)==8 & ncol(prior_covid_HR)==8 & ncol(subgroup_covid_pheno_HR)==8){
  
  #Combine results
  
  combined_HR=rbind(main_max_adj_HR,main_age_sex_HR,prior_covid_HR,subgroup_covid_pheno_HR)
  combined_event_counts=rbind(main_max_adj_counts,main_age_sex_counts,prior_covid_counts,subgroup_covid_pheno_counts)
  combined_HR$type <- "HR"
  combined_event_counts$type <- "event_count"
  
  # Select relevant 'term' rows for post-COVID time periods---------------------
  
  combined_HR <- combined_HR %>% filter(str_detect(term, "^days"))
  combined_event_counts <- combined_event_counts %>% filter(str_detect(expo_week, "^days"))
  
  # Make names 'nice' ----------------------------------------------------------
 
   df=list(combined_HR,combined_event_counts)
  
  event_name=lapply(df, function(x) {
    x$event <- ifelse(x$event=="ami","Acute myocardial infarction",x$event)
    x$event <- ifelse(x$event=="tia","Transient ischaemic attack",x$event)
    x$event <- ifelse(x$event=="dvt","Deep vein thrombosis",x$event)
    x$event <- ifelse(x$event=="hf","Heart failure",x$event)
    x$event <- ifelse(x$event=="stroke_isch","Ischaemic stroke",x$event)
    x$event <- ifelse(x$event=="angina","Angina",x$event)
    x$event <- ifelse(x$event=="vte","Venous thromboembolism",x$event)
    x$event <- ifelse(x$event=="pe","Pulmonary embolism",x$event)
    x$event <- ifelse(x$event=="stroke_sah_hs","Subarachnoid haemorrhage and haemorrhagic stroke",x$event)
    x$event <- ifelse(x$event=="ate","Arterial thromboses",x$event)
    
   } )
  
  strata_name=lapply(df, function(x) {
    x$strata <- ifelse(x$strata=="main_main" & x$model=="mdl_max_adj" & x$covid_history=="covid_history_false","Maximally adjusted",x$strata )
    x$strata <- ifelse(x$strata=="main_main" & x$model=="mdl_agesex" & x$covid_history=="covid_history_false","Age/sex adjusted",x$strata )
    x$strata <- ifelse(x$strata=="main_main" & x$model=="mdl_max_adj" & x$covid_history=="covid_history_true","Prior history of COVID",x$strata )
    x$strata <- ifelse(x$strata=="expo_pheno_hospitalised","Hospitalised COVID-19",x$strata )
    x$strata <- ifelse(x$strata=="expo_pheno_non_hospitalised","Non-hospitalised COVID-19",x$strata )
  })
  
  combined_HR$tidy_event=event_name[[1]]
  combined_event_counts$tidy_event=event_name[[2]]
  
  combined_HR$tidy_strata=strata_name[[1]]
  combined_event_counts$tidy_strata=strata_name[[2]]
  
  df_HR=combined_HR
  
  # Specify estimate format ----------------------------------------------------  
  
  df_HR$est <- paste0(ifelse(df_HR$estimate>=10,sprintf("%.1f",df_HR$estimate),sprintf("%.2f",df_HR$estimate)),
                    " (",ifelse(df_HR$conf.low>=10,sprintf("%.1f",df_HR$conf.low),sprintf("%.2f",df_HR$conf.low)),
                    "-",ifelse(df_HR$conf.high>=10,sprintf("%.1f",df_HR$conf.high),sprintf("%.2f",df_HR$conf.high)),")")
  
  # Remove unnecessary variables -----------------------------------------------
  df_HR <- df_HR %>% select(term,tidy_event,tidy_strata,est,type)
  df_counts <- combined_event_counts %>% select(expo_week, events_total, tidy_event, tidy_strata,type)
  
  # Convert long to wide -------------------------------------------------------
  
  df_HR <- tidyr::pivot_wider(df_HR, names_from = term, values_from = est)
  df_counts <- tidyr::pivot_wider(df_counts, names_from = expo_week, values_from = events_total)
  
  # Combine hazard ratios and event counts
  df_HR_counts <- rbind(df_HR,df_counts)
  
  # Specify estimate order -----------------------------------------------------
  
  df_HR_counts$tidy_event <- factor(df_HR_counts$tidy_event, levels=c("Acute myocardial infarction",
                                          "Ischaemic stroke",
                                          "Pulmonary embolism",
                                          "Deep vein thrombosis",
                                          "Transient ischaemic attack",
                                          "Subarachnoid haemorrhage and haemorrhagic stroke",
                                          "Heart failure",
                                          "Angina",
                                          "Arterial thromboses",
                                          "Venous thromboembolism")) 
  
  df_HR_counts$tidy_strata <- factor(df_HR_counts$tidy_strata, levels=c("Maximally adjusted",
                                             "Age/sex adjusted",
                                             "Hospitalised COVID-19",
                                             "Non-hospitalised COVID-19",
                                             "Prior history of COVID")) 
  
  df_HR_counts$type <- factor(df_HR_counts$type, levels = c("HR",
                                                            "event_count"))
  
  df_HR_counts <- df_HR_counts[order(df_HR_counts$tidy_event, df_HR_counts$tidy_strata,df_HR_counts$type),]
  
  write.csv(df,paste0("output/supplementary_table_1_",project,"_.csv"))
  
}else{
  df=data.frame(ncol=1)
  colnames(df)="no_results"
  write.csv(df,paste0("output/supplementary_table_1_",project,"_.csv"))
}





