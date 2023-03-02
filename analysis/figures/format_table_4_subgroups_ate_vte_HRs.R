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


#----------------------------Focus on ATE & VTE---------------------------------
outcomes_to_plot <- active_analyses$outcome_name
outcomes_to_plot_pre_vax <- active_analyses_pre_vax$outcome_name

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

estimates <- estimates %>% filter(!subgroup %in% c("covid_history","main","covid_pheno_hospitalised","covid_pheno_non_hospitalised","ethnicity_Missing")
                                  & ((event %in% outcomes_to_plot & cohort %in% c("vaccinated","electively_unvaccinated")) | (event %in% outcomes_to_plot_pre_vax & cohort %in% c("pre_vaccination")))
                                  & term %in% term[grepl("^days",term)]
                                  & model == "mdl_max_adj"
                                  & time_points == "reduced"
                                  & estimate != "[Redacted]") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

#----------------------Add empty rows for missing results-----------------------
df1 <- crossing(outcomes_to_plot,c("prior_history_TRUE", "prior_history_FALSE","agegp_18_39",
                                   "agegp_40_59","agegp_60_79","agegp_80_110","sex_Female","sex_Male","ethnicity_White",
                                   "ethnicity_Black","ethnicity_South_Asian","ethnicity_Other", "ethnicity_Mixed"),
                                  c("vaccinated","electively_unvaccinated"),
                                  c("reduced"),c("mdl_max_adj"),c("days0_28","days28_197"))

df2 <- crossing(outcomes_to_plot_pre_vax,c("prior_history_TRUE", "prior_history_FALSE","agegp_18_39",
                                  "agegp_40_59","agegp_60_79","agegp_80_110","sex_Female","sex_Male","ethnicity_White",
                                  "ethnicity_Black","ethnicity_South_Asian","ethnicity_Other", "ethnicity_Mixed"),
               c("pre_vaccination"),
               c("reduced"),c("mdl_max_adj"),c("days0_28","days28_197","days197_365","days365_714"))

colnames(df1) <- c("event","subgroup","cohort","time_points","model","term")
colnames(df2) <- c("event","subgroup","cohort","time_points","model","term")

df<- rbind(df1,df2)

estimates <- df %>% left_join(estimates) %>% 
  dplyr::mutate(across(c(estimate,conf_low,conf_high),as.numeric))

rm(df1,df2)
#------------------------------Tidy event names---------------------------------
estimates$event <- gsub("_extended_follow_up","",estimates$event)
estimates <- estimates %>% left_join(active_analyses %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#-------------------------------Specify estimate format ------------------------  

estimates$est <- ifelse(is.na(estimates$estimate),NA,paste0(ifelse(estimates$estimate>=10,sprintf("%.1f",estimates$estimate),sprintf("%.2f",estimates$estimate)),
                  " (",ifelse(estimates$conf_low>=10,sprintf("%.1f",estimates$conf_low),sprintf("%.2f",estimates$conf_low)),
                  "-",ifelse(estimates$conf_high>=10,sprintf("%.1f",estimates$conf_high),sprintf("%.2f",estimates$conf_high)),")"))


# Specify estimate order -----------------------------------------------------
estimates$subgroup <- ifelse(estimates$subgroup=="prior_history_FALSE","No prior history of event",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="prior_history_TRUE","Prior history of event",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_18_39","Age group: 18-39",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_40_59","Age group: 40-59",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_60_79","Age group: 60-79",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_80_110","Age group: 80-110",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="sex_Male","Sex: Male",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="sex_Female","Sex: Female",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_White","Ethnicity: White",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Mixed","Ethnicity: Mixed",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_South_Asian","Ethnicity: South Asian",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Black","Ethnicity: Black",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Other","Ethnicity: Other Ethnic Groups",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Missing","Ethnicity: Missing",estimates$subgroup)

estimates$subgroup <- factor(estimates$subgroup, levels = c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110","Sex: Female","Sex: Male",
                                                                      "Ethnicity: White","Ethnicity: Black","Ethnicity: South Asian","Ethnicity: Other Ethnic Groups", "Ethnicity: Mixed",
                                                                      "Prior history of event", "No prior history of event"))

estimates$cohort <- factor(estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 
levels(estimates$cohort) <- list("Pre-vaccination"="pre_vaccination", "Vaccinated"="vaccinated","Unvaccinated"="electively_unvaccinated")

estimates <- estimates %>% mutate(term = case_when(term == "days0_28" ~ "1-4",
                                                   term == "days28_197" ~ "5-28",
                                                   term == "days197_365" ~ "29-52",
                                                   term == "days365_714" ~ "53-102",
                                                   TRUE ~ term))

# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model")] <- NULL

# Convert long to wide -------------------------------------------------------

format_hr_table <- function(df, time_periods,event){
  df$time_points <- NULL
  df <- tidyr::pivot_wider(df, names_from = cohort, values_from = est)
  
  df <- df %>% select("outcome","subgroup","term", "Pre-vaccination","Vaccinated","Unvaccinated")
  
  # Order term
  df$term <- factor(df$term, levels = c("1-4",
                                        "5-28",
                                        "29-52",
                                        "53-102"))
  
  df <- df[order(df$subgroup,df$term),]
  
  df$subgroup_name <- df$subgroup
  df$subgroup_name <- sub('\\:.*', '', df$subgroup_name)
  df$subgroup <- sub('.*:', '', df$subgroup)
  df <- df %>% select("outcome","subgroup_name", "subgroup","term", "Pre-vaccination","Vaccinated","Unvaccinated")
  
  
  write.csv(df, file = paste0(output_dir,"supplementary_table_4_subgroups_hr_formatted_",event,"_",time_periods,"_time_periods.csv"),row.names = F)
}

for (outcome_name in unique(estimates$outcome)) {
  if(grepl("Primary position",outcome_name)){
    tmp_reduced <- estimates %>% filter(time_points == "reduced" 
                                     & outcome %in% outcome[grepl("Primary position",outcome)]
                                     & outcome == outcome_name)
    
  }else{
    tmp_reduced <- estimates %>% filter(time_points == "reduced" 
                                             & !outcome %in% outcome[grepl("Primary position",outcome)]
                                             & outcome == outcome_name)
    
  }

  format_hr_table(tmp_reduced,"reduced",outcome_name)
}
