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
outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name %in% c("ate","vte","ate_primary_position","vte_primary_position")]


# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

estimates <- estimates %>% filter(!subgroup %in% c("covid_history","main","covid_pheno_hospitalised","covid_pheno_non_hospitalised","ethnicity_Missing")
                                       & event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & model == "mdl_max_adj"
                                       & estimate != "[Redacted]") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model)

#----------------------Add empty rows for missing results-----------------------
df1 <- crossing(outcomes_to_plot,c("prior_history_TRUE", "prior_history_FALSE","agegp_18_39",
                                   "agegp_40_59","agegp_60_79","agegp_80_110","sex_Female","sex_Male","ethnicity_White",
                                   "ethnicity_Black","ethnicity_South_Asian","ethnicity_Other", "ethnicity_Mixed"),
                                  c("pre_vaccination","vaccinated","electively_unvaccinated"),
                                  c("reduced"),c("mdl_max_adj"),c("days0_28","days28_197","days197_535"))

colnames(df1) <- c("event","subgroup","cohort","time_points","model","term")

df2 <- crossing(outcomes_to_plot,c("prior_history_TRUE", "prior_history_FALSE","agegp_18_39",
                                   "agegp_40_59","agegp_60_79","agegp_80_110","sex_Female","sex_Male","ethnicity_White",
                                   "ethnicity_Black","ethnicity_South_Asian","ethnicity_Other", "ethnicity_Mixed"),
                                  c("pre_vaccination","vaccinated","electively_unvaccinated"),
                                  c("normal"),c("mdl_max_adj"),
                                  c("days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_535"))

colnames(df2) <- c("event","subgroup","cohort","time_points","model","term")

df<- rbind(df1,df2)

estimates <- df %>% left_join(estimates) %>% 
  dplyr::mutate(across(c(estimate,conf_low,conf_high),as.numeric))

rm(df1,df2,df)
#------------------------------Tidy event names---------------------------------
estimates <- estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

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


# Remove unnecessary variables -----------------------------------------------

estimates[,c("event","estimate","conf_low","conf_high","model")] <- NULL

# Convert long to wide -------------------------------------------------------

format_hr_table <- function(df, time_periods,event){
  df$time_points <- NULL
  df <- tidyr::pivot_wider(df, names_from = term, values_from = est)
  df <- df[order(df$subgroup,df$cohort),]
  if(time_periods == "reduced"){
    df <- df %>% select("outcome","subgroup","cohort","days0_28","days28_197","days197_535")
  }else{
    df <- df %>% select("outcome","subgroup","cohort","days0_7","days7_14","days14_28","days28_56","days56_84","days84_197","days197_535")
  }
  write.csv(df, file = paste0(output_dir,"table4_subgroups_hr_formatted_",event,"_",time_periods,"_time_periods.csv"),row.names = F)
}

for (outcome_name in unique(estimates$outcome)) {
  if(grepl("Primary position",outcome_name)){
    tmp_reduced <- estimates %>% filter(time_points == "reduced" 
                                     & outcome %in% outcome[grepl("Primary position",outcome)]
                                     & outcome == outcome_name)
    tmp_normal <- estimates %>% filter(time_points == "normal" 
                                            & outcome %in% outcome[grepl("Primary position",outcome)]
                                            & outcome == outcome_name)
  }else{
    tmp_reduced <- estimates %>% filter(time_points == "reduced" 
                                             & !outcome %in% outcome[grepl("Primary position",outcome)]
                                             & outcome == outcome_name)
    tmp_normal <- estimates %>% filter(time_points == "normal" 
                                            & !outcome %in% outcome[grepl("Primary position",outcome)]
                                            & outcome == outcome_name)
  }

  format_hr_table(tmp_reduced,"reduced",outcome_name)
  format_hr_table(tmp_normal,"normal",outcome_name)
}
