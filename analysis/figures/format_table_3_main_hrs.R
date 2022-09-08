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
outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name != c("ate","vte","ate_primary_position","vte_primary_position")]

#------------------------------Load all estimates-------------------------------

hr_files=list.files(path = results_dir, pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

main_estimates <- estimates %>% filter(((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                            | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) 
                                            & event %in% outcomes_to_plot 
                                            & term %in% term[grepl("^days",term)]
                                            & results_fitted == "fitted_successfully")%>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

#----------------------Add empty rows for missing results-----------------------
df1 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("pre_vaccination","vaccinated","electively_unvaccinated"),
               c("reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_28","days28_197","days197_535"))

colnames(df1) <- c("event","subgroup","cohort","time_points","model","term")

df2 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("pre_vaccination","vaccinated","electively_unvaccinated"),
                c("normal"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_535"))

colnames(df2) <- c("event","subgroup","cohort","time_points","model","term")

df<- rbind(df1,df2)

main_estimates <- df %>% left_join(main_estimates)

main_estimates <- main_estimates %>% filter((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                            | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) %>% 
  mutate(across(c(estimate,conf_low,conf_high),as.numeric))

rm(df1,df2,df)
#------------------------------Tidy event names---------------------------------
main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#-------------------------------Specify estimate format ------------------------  

main_estimates$est <- ifelse(is.na(main_estimates$estimate),NA,paste0(ifelse(main_estimates$estimate>=10,sprintf("%.1f",main_estimates$estimate),sprintf("%.2f",main_estimates$estimate)),
                  " (",ifelse(main_estimates$conf_low>=10,sprintf("%.1f",main_estimates$conf_low),sprintf("%.2f",main_estimates$conf_low)),
                  "-",ifelse(main_estimates$conf_high>=10,sprintf("%.1f",main_estimates$conf_high),sprintf("%.2f",main_estimates$conf_high)),")"))


# Specify estimate order -----------------------------------------------------

main_estimates$outcome <- factor(main_estimates$outcome, levels=c("Acute myocardial infarction",
                                                                  "Ischaemic stroke",
                                                                  "Pulmonary embolism",
                                                                  "Deep vein thrombosis",
                                                                  "Heart failure",
                                                                  "Angina",
                                                                  "Transient ischaemic attack",
                                                                  "Subarachnoid haemorrhage and haemorrhagic stroke",
                                                                  "Acute myocardial infarction - Primary position events",
                                                                  "Ischaemic stroke - Primary position events",
                                                                  "Pulmonary embolism - Primary position events",
                                                                  "Deep vein thrombosis - Primary position events",
                                                                  "Heart failure - Primary position events",
                                                                  "Angina - Primary position events",
                                                                  "Transient ischaemic attack - Primary position events",
                                                                  "Subarachnoid haemorrhage and haemorrhagic stroke - Primary position events")) 


main_estimates$subgroup <- ifelse(main_estimates$model == "mdl_max_adj" & main_estimates$subgroup == "main", "All",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$model == "mdl_age_sex_region" & main_estimates$subgroup == "main", "All, age/sex/region adjusted",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup == "covid_pheno_hospitalised", "Hospitalised COVID-19",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup == "covid_pheno_non_hospitalised", "Non-hospitalised COVID-19",main_estimates$subgroup)

main_estimates$subgroup <- factor(main_estimates$subgroup, levels = c("All",
                                                                      "All, age/sex/region adjusted",
                                                                      "Hospitalised COVID-19",
                                                                      "Non-hospitalised COVID-19"))

main_estimates$cohort <- factor(main_estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 
levels(main_estimates$cohort) <- list("Pre-vaccinated"="pre_vaccination", "Vaccinated"="vaccinated","Electively unvaccinated"="electively_unvaccinated")


# Remove unnecessary variables -----------------------------------------------

main_estimates[,c("event","estimate","conf_low","conf_high","model")] <- NULL

# Convert long to wide -------------------------------------------------------
main_estimates_reduced <- main_estimates %>% filter(time_points == "reduced" 
                                                    & !outcome %in% outcome[grepl("Primary position",outcome)])

main_estimates_reduced_primary_position <- main_estimates %>% filter(time_points == "reduced" 
                                                    & outcome %in% outcome[grepl("Primary position",outcome)])

main_estimates_normal <- main_estimates %>% filter(time_points == "normal" 
                                                    & !outcome %in% outcome[grepl("Primary position",outcome)])

main_estimates_normal_primary_position <- main_estimates %>% filter(time_points == "normal" 
                                                                     & outcome %in% outcome[grepl("Primary position",outcome)])

format_hr_table <- function(df, time_periods){
  df$time_points <- NULL
  df <- tidyr::pivot_wider(df, names_from = term, values_from = est)
  df <- df[order(df$outcome,df$subgroup,df$cohort),]
  if(time_periods == "reduced"){
    df <- df %>% select("outcome","subgroup","cohort","days0_28","days28_197","days197_535")
  }else{
    df <- df %>% select("outcome","subgroup","cohort","days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_535")
  }
  return(df)
}

main_estimates_reduced <- format_hr_table(main_estimates_reduced,"reduced")
main_estimates_reduced_primary_position <- format_hr_table(main_estimates_reduced_primary_position,"reduced")
main_estimates_normal <- format_hr_table(main_estimates_normal,"normal")
main_estimates_normal_primary_position <- format_hr_table(main_estimates_normal_primary_position,"normal")


write.csv(main_estimates_reduced, file = paste0(output_dir,"table3_main_hr_formatted_reduced_time_periods.csv"),row.names = F)
write.csv(main_estimates_reduced_primary_position, file = paste0(output_dir,"table3_main_hr_formatted_reduced_time_periods_primary_position.csv"),row.names = F)
write.csv(main_estimates_normal, file = paste0(output_dir,"table3_main_hr_formatted_normal_time_periods.csv"),row.names = F)
write.csv(main_estimates_normal_primary_position, file = paste0(output_dir,"table3_main_hr_formatted_normal_time_periods_primary_position.csv"),row.names = F)


# main_estimates$stratum <- factor(main_estimates$stratum, levels=c("Extensive adjustment",
#                                             "Age/sex/region adjustment",
#                                             "Hospitalised COVID-19",
#                                             "Non-hospitalised COVID-19",
#                                             "Prior history of event",
#                                             "No prior history of event",
#                                             "Age group: <40",
#                                             "Age group: 40-59",
#                                             "Age group: 60-79",
#                                             "Age group: >=80",
#                                             "Sex: Female",
#                                             "Sex: Male",
#                                             "Ethnicity: White",
#                                             "Ethnicity: Black or Black British",
#                                             "Ethnicity: Asian or Asian British",
#                                             "Ethnicity: Other Ethnic Groups",
#                                             "Ethnicity: Mixed")) 



# Save -----------------------------------------------------------------------  

#data.table::fwrite(main_estimates, paste0("output/ccu002_01_suppl_table_4_",event,".csv"))
#data.table::fwrite(main_estimates, paste0("output/ccu002_01_suppl_table_4.csv"))

#}