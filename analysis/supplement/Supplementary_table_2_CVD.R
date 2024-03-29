library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(utils)
library(data.table)
library(purrr)
library(markdown)

#-----------------------Determine active outcome events-------------------------
active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses$outcome_variable <- gsub("out_date_","",active_analyses$outcome_variable)

#--------Load HR results-------------------
hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")

hr_files=hr_files[endsWith(hr_files,".csv")]

hr_files=paste0("output/",hr_files)

hr_file_paths <- pmap(list(hr_files), 
                      function(fpath){ 
                        df <- fread(fpath) 
                        return(df)
                      })
combined_hr <- rbindlist(hr_file_paths, fill=TRUE)

#-------------------------Filter to active outcomes-----------------------------
combined_hr <- combined_hr %>% filter(event %in% c("ate","vte"))

#---------Select relevant 'term' rows for post-COVID time periods---------------

combined_hr <- combined_hr %>% filter(str_detect(term, "^days"))

# Make names 'nice' ----------------------------------------------------------

combined_hr <- combined_hr %>% left_join(active_analyses %>% select(outcome, outcome_variable), by=c("event"="outcome_variable"))

df=list(combined_hr)

subgroup_name=lapply(df, function(x) {
  x$subgroup <- ifelse(x$subgroup=="main" & x$model=="mdl_max_adj","All",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="main" & x$model=="mdl_agesex","All, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_pheno_hospitalised" & x$model=="mdl_max_adj","Hospitalised COVID-19",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_pheno_hospitalised" & x$model=="mdl_agesex","Hospitalised COVID-19, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_pheno_non_hospitalised" & x$model=="mdl_max_adj","Non-hospitalised COVID-19",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_pheno_non_hospitalised" & x$model=="mdl_agesex","Non-hospitalised COVID-19, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_history" & x$model=="mdl_max_adj","Prior history of COVID-19",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="covid_history" & x$model=="mdl_agesex","Prior history of COVID-19, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="agegp_18_39" & x$model=="mdl_max_adj","Age <40 years",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="agegp_18_39" & x$model=="mdl_agesex","Age <40 years, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="agegp_40_59" & x$model=="mdl_max_adj","Age 40-59 years",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="agegp_40_59" & x$model=="mdl_agesex","Age 40-59 years, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="agegp_60_79" & x$model=="mdl_max_adj","Age 60-79 years",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="agegp_60_79" & x$model=="mdl_agesex","Age 60-79 years, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="agegp_80_110" & x$model=="mdl_max_adj","Age 80+ years",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="agegp_80_110" & x$model=="mdl_agesex","Age 80+ years, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="sex_Female" & x$model=="mdl_max_adj","Females",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="sex_Female" & x$model=="mdl_agesex","Females, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="sex_Male" & x$model=="mdl_max_adj","Males",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="sex_Male" & x$model=="mdl_agesex","Males, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="prior_history_TRUE" & x$model=="mdl_max_adj","Prior history of event",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="prior_history_TRUE" & x$model=="mdl_agesex","Prior history of event, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="prior_history_FALSE" & x$model=="mdl_max_adj","No prior history of event",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="prior_history_FALSE" & x$model=="mdl_agesex","No prior history of event, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_White" & x$model=="mdl_max_adj","White ethnicity",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_White" & x$model=="mdl_agesex","White ethnicity, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_Black" & x$model=="mdl_max_adj","Black ethnicity",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_Black" & x$model=="mdl_agesex","Black ethnicity, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_Missing" & x$model=="mdl_max_adj","Missing ethnicity",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_Missing" & x$model=="mdl_agesex","Missing ethnicity, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_Mixed" & x$model=="mdl_max_adj","Mixed ethnicity",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_Mixed" & x$model=="mdl_agesex","Mixed ethnicity, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_Other" & x$model=="mdl_max_adj","Other ethnic groups",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_Other" & x$model=="mdl_agesex","Other ethnic groups, age/sex adjusted",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_South_Asian" & x$model=="mdl_max_adj","South Asian ethnicity",x$subgroup )
  x$subgroup <- ifelse(x$subgroup=="ethnicity_South_Asian" & x$model=="mdl_agesex","South Asian ethnicity, age/sex adjusted",x$subgroup )
})

combined_hr$tidy_subgroup=subgroup_name[[1]]

# Specify estimate format ----------------------------------------------------  

combined_hr$est <- paste0(ifelse(combined_hr$estimate>=10,sprintf("%.1f",combined_hr$estimate),sprintf("%.2f",combined_hr$estimate)),
                          " (",ifelse(combined_hr$conf.low>=10,sprintf("%.1f",combined_hr$conf.low),sprintf("%.2f",combined_hr$conf.low)),
                          "-",ifelse(combined_hr$conf.high>=10,sprintf("%.1f",combined_hr$conf.high),sprintf("%.2f",combined_hr$conf.high)),")")

# Remove unnecessary variables -----------------------------------------------
combined_hr <- combined_hr %>% select(term,outcome,tidy_subgroup,est,model,cohort)

# Convert long to wide -------------------------------------------------------

combined_hr <- tidyr::pivot_wider(combined_hr, names_from = term, values_from = est)

# Specify column order ---------------------------------------------------------
#if(length(colnames(combined_hr_counts)[grepl("^days",colnames(combined_hr_counts))]) ==5){
#  combined_hr_counts <- combined_hr_counts %>% select(outcome,tidy_subgroup,cohort,model,type,days0_14,days14_28,days28_56,days56_84,days84_197)
#}else{
#  combined_hr_counts <- combined_hr_counts %>% select(outcome,tidy_subgroup,cohort,model,type,days0_14,days14_28,days28_56,days56_84,days84_197,days0_28,days28_197)
#}

# Specify estimate order -------------------------------------------------------

combined_hr$outcome <- factor(combined_hr$outcome, levels=c("arterial thrombosis event","venous thrombosis event")) 

combined_hr$tidy_subgroup <- factor(combined_hr$tidy_subgroup, levels=c("All",
                                                                        "All, age/sex adjusted",
                                                                        "Prior history of COVID-19",
                                                                        "Prior history of COVID-19, age/sex adjusted",
                                                                        "Hospitalised COVID-19",
                                                                        "Hospitalised COVID-19, age/sex adjusted",
                                                                        "Non-hospitalised COVID-19",
                                                                        "Non-hospitalised COVID-19, age/sex adjusted",
                                                                        "Prior history of event",
                                                                        "Prior history of event, age/sex adjusted",
                                                                        "No prior history of event",
                                                                        "No prior history of event, age/sex adjusted",
                                                                        "Age <40 years",
                                                                        "Age <40 years, age/sex adjusted",
                                                                        "Age 40-59 years",
                                                                        "Age 40-59 years, age/sex adjusted",
                                                                        "Age 60-79 years",
                                                                        "Age 60-79 years, age/sex adjusted",
                                                                        "Age 80+ years",
                                                                        "Age 80+ years, age/sex adjusted",
                                                                        "Females",
                                                                        "Females, age/sex adjusted",
                                                                        "Males",
                                                                        "Males, age/sex adjusted",
                                                                        "White ethnicity",
                                                                        "White ethnicity, age/sex adjusted",
                                                                        "Black ethnicity",
                                                                        "Black ethnicity, age/sex adjusted",
                                                                        "South Asian ethnicity",
                                                                        "South Asian ethnicity, age/sex adjusted",
                                                                        "Mixed ethnicity",
                                                                        "Mixed ethnicity, age/sex adjusted",
                                                                        "Other ethnic groups",
                                                                        "Other ethnic groups, age/sex adjusted",
                                                                        "Missing ethnicity",
                                                                        "Missing ethnicity, age/sex adjusted"
                                                                        )) 

                                                                                      
combined_hr$cohort <- factor(combined_hr$cohort, levels = c("vaccinated",
                                                            "electively_unvaccinated"))

combined_hr <- combined_hr[order(combined_hr$cohort, combined_hr$outcome, combined_hr$tidy_subgroup),]

# Save as .csv------------------------------------------------------------------

for(i in c("vaccinated","electively_unvaccinated")){
  df <- combined_hr %>% filter(cohort == i)
  write_csv(df,paste0("output/supplementary_table_2_",i,".csv")) 
}
