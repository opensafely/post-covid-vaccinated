library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(utils)
library(data.table)
library(purrr)

args = commandArgs(trailingOnly=TRUE)
project = args[[1]]

active_analyses <- read_rds("output/active_analyses.rds")

#--------Load fully adjusted main and COVID phenotype results-------------------
hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")

hr_files=paste0("output/",hr_files)

hr_file_paths <- pmap(list(hr_files), 
                          function(fpath){ 
                            df <- fread(fpath) 
                            return(df)
                          })
combined_hr <- rbindlist(hr_file_paths, fill=TRUE)
combined_event_counts <- combined_hr %>% select(expo_week,events_total,event,subgroup,model,cohort)

# Select relevant 'term' rows for post-COVID time periods---------------------

combined_hr <- combined_hr %>% filter(str_detect(term, "^days"))

# Make names 'nice' ----------------------------------------------------------

df=list(combined_hr,combined_event_counts)

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

write.csv(df_HR_counts,paste0("output/supplementary_table_1_",project,"_.csv")) <- combined_HR %>% filter(str_detect(term, "^days"))
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

write.csv(df_HR_counts,paste0("output/supplementary_table_1_",project,"_.csv"))



  
  
  
}else{
  df=data.frame(ncol=1)
  colnames(df)="no_results"
  write.csv(df,paste0("output/supplementary_table_1_",project,"_.csv"))
}





