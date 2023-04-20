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
  filter(outcome_variable %in% c("out_date_ate","out_date_vte","out_date_ate_primary_position","out_date_vte_primary_position")) 


active_analyses$outcome <- gsub("Arterial thrombosis event","All arterial thromboses",active_analyses$outcome)
active_analyses$outcome <- gsub("Venous thrombosis event","All venous thromboses",active_analyses$outcome)


active_analyses_pre_vax <- read_rds("lib/active_analyses_pre_vax.rds") %>% filter(active == "TRUE")

active_analyses_pre_vax <- active_analyses_pre_vax %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses_pre_vax$outcome_variable %>% str_replace("out_date_", ""))%>%
  filter(grepl("extended_follow_up",outcome_variable) 
         & outcome_variable %in% c("out_date_ate_extended_follow_up","out_date_vte_extended_follow_up",
                                   "out_date_ate_primary_position_extended_follow_up","out_date_vte_primary_position_extended_follow_up"))

#---------------Focus on all outcomes--------------
#outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name != c("ate","vte","ate_primary_position","vte_primary_position")]
outcomes_to_plot <- active_analyses$outcome_name
outcomes_to_plot_pre_vax <- active_analyses_pre_vax$outcome_name

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))
estimates <- estimates %>% filter(time_points == "day_zero_reduced")

estimates <- estimates %>% filter(((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                   | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) 
                                  & ((event %in% outcomes_to_plot & cohort %in% c("vaccinated","electively_unvaccinated")) | (event %in% outcomes_to_plot_pre_vax & cohort %in% c("pre_vaccination")))
                                  & term %in% term[grepl("^days",term)]
                                  & time_points == "day_zero_reduced")%>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

#----------------------Add empty rows for missing results-----------------------
df1 <- crossing(outcomes_to_plot,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("vaccinated","electively_unvaccinated"),
                c("day_zero_reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_1","days1_28","days28_197"))

colnames(df1) <- c("event","subgroup","cohort","time_points","model","term")

df2 <- crossing(outcomes_to_plot_pre_vax,c("main","covid_pheno_non_hospitalised","covid_pheno_hospitalised"),c("pre_vaccination"),
                c("day_zero_reduced"),c("mdl_max_adj","mdl_age_sex_region"),c("days0_1","days1_28","days28_197","days197_365","days365_714"))

colnames(df2) <- c("event","subgroup","cohort","time_points","model","term")

df<- rbind(df1,df2)

estimates <- df %>% left_join(estimates)

estimates <- estimates %>% filter((subgroup == "main" & model %in% c("mdl_max_adj","mdl_age_sex_region"))
                                  | (subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised") & model=="mdl_max_adj")) %>% 
  dplyr::mutate(across(c(estimate,conf_low,conf_high),as.numeric))

rm(df1,df2,df)

#---------------------------Left join event counts------------------------------
event_counts_pre_vax <- read.csv(paste0(results_dir, "/R_event_count_day_zero_output_pre_vax.csv"))
event_counts <- read.csv(paste0(results_dir, "/R_event_count_day_zero_output.csv"))

event_counts <- rbind(event_counts, event_counts_pre_vax)
rm(event_counts_pre_vax)

estimates <- estimates %>% left_join(event_counts, by= c("event"="event","subgroup"="subgroup","cohort"="cohort",
                                                         "term"="expo_week","time_points"="time_points"))
#------------------------------Tidy event names---------------------------------
#Remove extended follow up from outcome name as this will not be written in the final table
estimates$event <- gsub("_extended_follow_up","",estimates$event)

estimates <- estimates %>% left_join(active_analyses %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                                                            " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                                                            "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))


# Specify estimate order -----------------------------------------------------
estimates$outcome <- factor(estimates$outcome, levels=c("All arterial thromboses",
                                                        "All venous thromboses",
                                                        "All arterial thromboses - Primary position events",
                                                        "All venous thromboses - Primary position events"))


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
estimates <- estimates %>% mutate(term = case_when(term == "days0_1" ~ "Day 0",
                                                   term == "days1_28" ~ "1-4",
                                                   term == "days28_197" ~ "5-28",
                                                   term == "days197_365" ~ "29-52",
                                                   term == "days365_714" ~ "53-102",
                                                   TRUE ~ term))


# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model")] <- NULL

# Convert long to wide -------------------------------------------------------

format_hr_table <- function(df, time_periods,outcome_position, save_name){
  df$time_points <- NULL

  df <- tidyr::pivot_wider(df, names_from = cohort, values_from = c("est","events_total"))
  df <- df %>% select("outcome","subgroup","term","events_total_Pre-vaccination",
                      "est_Pre-vaccination","events_total_Vaccinated", "est_Vaccinated",
                      "events_total_Unvaccinated", "est_Unvaccinated")
  
  if(grepl("reduced", time_periods)){
    df$term <- factor(df$term, levels = c("Day 0",
                                          "1-4",
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
  
  
  df <- df %>% mutate(across(colnames(df),as.character))
  
  if(outcome_position == "any_position"){
    df[nrow(df)+1,] <- "Arterial thrombotic events"
    df[nrow(df)+1,] <- "Venous thrombotic events"
    
    df$outcome <- factor(df$outcome, levels=c("Arterial thrombotic events",
                                              "All arterial thromboses",
                                              "Venous thrombotic events",
                                              "All venous thromboses"))
  }else{
    df[nrow(df)+1,] <- "Arterial thrombotic events - Primary position events"
    df[nrow(df)+1,] <- "Venous thrombotic events - Primary position events"

    df$outcome <- factor(df$outcome, levels=c("Arterial thrombotic events - Primary position events",
                                              "All arterial thromboses - Primary position events",
                                              "Venous thrombotic events - Primary position events",
                                              "All venous thromboses - Primary position events"))
  }
  
  
  df <- df[order(df$outcome),]
  
  write.csv(df, file = paste0(output_dir,save_name,outcome_position,"_",time_periods,"_time_periods.csv"),row.names = F)
}

estimates_reduced <- estimates %>% filter(!outcome %in% outcome[grepl("Primary position",outcome)])


estimates_reduced_primary_position <- estimates %>% filter(outcome %in% outcome[grepl("Primary position",outcome)]) 


format_hr_table(estimates_reduced,"reduced","any_position","supplementary_table3_formatted_hr_")
format_hr_table(estimates_reduced_primary_position,"reduced","primary_position","supplementary_table3_formatted_hr_")
