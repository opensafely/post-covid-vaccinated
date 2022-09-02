rm(list = ls())

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"


#----------------------------Get CVD outcomes-----------------------------------

active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))


#---------------Focus on first 8 CVD outcomes (remove ate and vte)--------------
outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name != c("ate","vte")]

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

estimates <- estimates %>% filter(subgroup == "main" 
                                  & event %in% outcomes_to_plot 
                                  & term %in% term[grepl("^days",term)]
                                  & results_fitted == "fitted_successfully"
                                  & model %in% c("mdl_max_adj","mdl_age_sex_region" )) %>%
                                  select(term,estimate,conf.low,conf.high,event,subgroup,cohort,time_points,model)


#Can be removed once final results are out
main_estimates <- estimates[0,]

for(i in unique(estimates$event)){
  for(j in unique(estimates$subgroup)){
    for (k in unique(estimates$cohort)) {
      for (l in unique(estimates$time_points)) {
        tmp <- estimates %>% filter(event == i
                                    & subgroup == j
                                    & cohort == k
                                    & time_points == l)
        if(nrow(tmp)>0){
          tmp$time_points <- ifelse(any(tmp$term == "days0_7"),"normal","reduced")
          main_estimates <- rbind(main_estimates,tmp)
        }
      }
    }
  }
}

main_estimates <- main_estimates[!duplicated(main_estimates), ]
main_estimates <- main_estimates %>% mutate(across(c(estimate,conf.low,conf.high),as.numeric))

# We want to plot the figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points
main_estimates <- main_estimates %>%
  group_by(event,subgroup) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_points == "reduced") ~ "reduced",
    TRUE ~ "normal"))

main_estimates <- main_estimates %>% filter(time_points == "reduced")
#------------------------------Tidy event names---------------------------------
main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#-------------------------------Specify estimate format ------------------------  

main_estimates$est <- paste0(ifelse(main_estimates$estimate>=10,sprintf("%.1f",main_estimates$estimate),sprintf("%.2f",main_estimates$estimate)),
                  " (",ifelse(main_estimates$conf.low>=10,sprintf("%.1f",main_estimates$conf.low),sprintf("%.2f",main_estimates$conf.low)),
                  "-",ifelse(main_estimates$conf.high>=10,sprintf("%.1f",main_estimates$conf.high),sprintf("%.2f",main_estimates$conf.high)),")")

# Remove unnecessary variables -----------------------------------------------

main_estimates[,c("event","estimate","conf.low","conf.high","time_points","time_period_to_plot")] <- NULL

# Convert long to wide -------------------------------------------------------

main_estimates <- tidyr::pivot_wider(main_estimates, names_from = term, values_from = est)

# Specify estimate order -----------------------------------------------------

main_estimates$outcome <- factor(main_estimates$outcome, levels=c("Acute myocardial infarction",
                                        "Ischaemic stroke",
                                        "Pulmonary embolism",
                                        "Deep vein thrombosis",
                                        "Heart failure",
                                        "Angina",
                                        "Transient ischaemic attack",
                                        "Subarachnoid haemorrhage and haemorrhagic stroke",
                                        "Arterial thrombosis event",
                                        "Venous thrombosis event",
                                        
                                        "Acute myocardial infarction - Primary position events",
                                        "Ischaemic stroke - Primary position events",
                                        "Pulmonary embolism - Primary position events",
                                        "Deep vein thrombosis - Primary position events",
                                        "Heart failure - Primary position events",
                                        "Angina - Primary position events",
                                        "Transient ischaemic attack - Primary position events",
                                        "Subarachnoid haemorrhage and haemorrhagic stroke - Primary position events",
                                        "Arterial thrombosis event - Primary position events",
                                        "Venous thrombosis event - Primary position events"
                                        )) 


main_estimates$model <- ifelse(main_estimates$model == "mdl_max_adj", "Maximal adjustment","Age/sex/region adjustment")
main_estimates$model <- factor(main_estimates$model, levels = c("Maximal adjustment",
                                                                "Age/sex/region adjustment"))

main_estimates$cohort <- factor(main_estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 
levels(main_estimates$cohort) <- list("Pre-vaccinated (2020-01-01 - 2021-06-18)"="pre_vaccination", "Vaccinated (2021-06-01 - 2021-12-14)"="vaccinated","Electively unvaccinated (2021-06-01 - 2021-12-14)"="electively_unvaccinated")

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

main_estimates <- main_estimates[order(main_estimates$outcome,main_estimates$cohort,main_estimates$model),]

main_estimates_2 <- main_estimates %>% filter(model == "Maximal adjustment")

# Save -----------------------------------------------------------------------  

#data.table::fwrite(main_estimates, paste0("output/ccu002_01_suppl_table_4_",event,".csv"))
#data.table::fwrite(main_estimates, paste0("output/ccu002_01_suppl_table_4.csv"))

#}