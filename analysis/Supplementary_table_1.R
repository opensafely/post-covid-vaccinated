library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(utils)
library(data.table)
library(purrr)
library(markdown)


#-----------------------Determine active outcome events-------------------------
active_analyses <- read_rds("output/active_analyses.rds")
events <- active_analyses %>% filter(active=="TRUE")%>%select(outcome,outcome_variable)
events$outcome_variable <- gsub("out_date_","",events$outcome_variable)

#--------Load fully adjusted main and COVID phenotype results-------------------
hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")

hr_files=paste0("output/",hr_files)

hr_file_paths <- pmap(list(hr_files), 
                          function(fpath){ 
                            df <- fread(fpath) 
                            return(df)
                          })
combined_hr <- rbindlist(hr_file_paths, fill=TRUE)

#-------------------------Filter to active outcomes-----------------------------
combined_hr <- combined_hr %>% filter(event %in% events$outcome_variable)
combined_hr <- combined_hr %>% filter(subgroup %in% c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised","covid_history"))
combined_hr$type <- "Hazard ratio"
combined_event_counts <- combined_hr %>% select(expo_week,events_total,event,subgroup,model,cohort)
combined_event_counts$type <- "Event count"

#---------Select relevant 'term' rows for post-COVID time periods---------------

combined_hr <- combined_hr %>% filter(str_detect(term, "^days"))
combined_event_counts <- combined_event_counts %>% filter(!is.na(expo_week) & str_detect(expo_week, "^days"))

# Make names 'nice' ----------------------------------------------------------

combined_hr <- combined_hr %>% left_join(events, by=c("event"="outcome_variable"))
combined_event_counts <- combined_event_counts %>% left_join(events, by=c("event"="outcome_variable"))


df=list(combined_hr,combined_event_counts)

subgroup_name=lapply(df, function(x) {
  x$subgroup <- ifelse(x$subgroup=="main" & x$model=="mdl_max_adj","All",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="main" & x$model=="mdl_agesex","All, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_pheno_hospitalised" & x$model=="mdl_max_adj","Hospitalised COVID-19",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_pheno_hospitalised" & x$model=="mdl_agesex","Hospitalised COVID-19, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_pheno_non_hospitalised" & x$model=="mdl_max_adj","Non-hospitalised COVID-19",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_pheno_non_hospitalised" & x$model=="mdl_agesex","Non-hospitalised COVID-19, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_history" & x$model=="mdl_max_adj","Prior history of COVID-19",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_history" & x$model=="mdl_agesex","Prior history of COVID-19, age/sex adjusted",x$subgroup )
  
  
})

combined_hr$tidy_subgroup=subgroup_name[[1]]
combined_event_counts$tidy_subgroup=subgroup_name[[2]]


# Specify estimate format ----------------------------------------------------  

combined_hr$est <- paste0(ifelse(combined_hr$estimate>=10,sprintf("%.1f",combined_hr$estimate),sprintf("%.2f",combined_hr$estimate)),
                    " (",ifelse(combined_hr$conf.low>=10,sprintf("%.1f",combined_hr$conf.low),sprintf("%.2f",combined_hr$conf.low)),
                    "-",ifelse(combined_hr$conf.high>=10,sprintf("%.1f",combined_hr$conf.high),sprintf("%.2f",combined_hr$conf.high)),")")

# Remove unnecessary variables -----------------------------------------------
combined_hr <- combined_hr %>% select(term,outcome,tidy_subgroup,est,model,cohort, type)
combined_event_counts <- combined_event_counts %>% select(outcome, expo_week, events_total, tidy_subgroup,model,cohort,type)

# Convert long to wide -------------------------------------------------------

combined_hr <- tidyr::pivot_wider(combined_hr, names_from = term, values_from = est)
combined_event_counts <- tidyr::pivot_wider(combined_event_counts, names_from = expo_week, values_from = events_total)

# Combine hazard ratios and event counts ---------------------------------------
combined_hr_counts <- rbind(combined_hr,combined_event_counts)

# Specify column order ---------------------------------------------------------
if(length(colnames(combined_hr_counts)[grepl("^days",colnames(combined_hr_counts))]) ==5){
  combined_hr_counts <- combined_hr_counts %>% select(outcome,tidy_subgroup,cohort,model,type,days0_14,days14_28,days28_56,days56_84,days84_197)
}else{
  combined_hr_counts <- combined_hr_counts %>% select(outcome,tidy_subgroup,cohort,model,type,days0_14,days14_28,days28_56,days56_84,days84_197,days0_28,days28_197)
}

# Specify estimate order -------------------------------------------------------

combined_hr_counts$outcome <- factor(combined_hr_counts$outcome, levels=events$outcome) 

combined_hr_counts$tidy_subgroup <- factor(combined_hr_counts$tidy_subgroup, levels=c("All",
                                                                      "All, age/sex adjusted",
                                                                      "Hospitalised COVID-19",
                                                                      "Hospitalised COVID-19, age/sex adjusted",
                                                                      "Non-hospitalised COVID-19",
                                                                      "Non-hospitalised COVID-19, age/sex adjusted",
                                                                      "Prior history of COVID-19",
                                                                      "Prior history of COVID-19, age/sex adjusted")) 

combined_hr_counts$type <- factor(combined_hr_counts$type, levels = c("Hazard ratio",
                                                          "Event count"))

combined_hr_counts$cohort <- factor(combined_hr_counts$cohort, levels = c("vaccinated",
                                                                          "electively_unvaccinated"))

combined_hr_counts <- combined_hr_counts[order(combined_hr_counts$outcome,combined_hr_counts$cohort, combined_hr_counts$tidy_subgroup,combined_hr_counts$type),]

time_periods <- colnames(combined_hr_counts)[grepl("^days",colnames(combined_hr_counts))]
df <- combined_hr_counts %>% 
                          filter(type=="Event count") %>% 
                          mutate_at(vars(time_periods), ~as.numeric(.)) %>% 
                          filter_at(vars(time_periods), any_vars(.<5))


# Save as .csv------------------------------------------------------------------
write_csv(combined_hr_counts,paste0("output/supplementary_table_1.csv")) 
write.csv(df,paste0("output/HRs_less_than_5_supplementary_table_1.csv"),row.names=F) 

# Save a html of the results for easy reading in Level 4 server-----------------

rmarkdown::render("analysis/supplementary_table_1.Rmd",output_file="supplementary_table_1",output_dir="output")
rmarkdown::render("analysis/less_than_5_events_supplementary_table_1.Rmd",output_file="less_than_5_events_supplementary_table_1",output_dir="output")


