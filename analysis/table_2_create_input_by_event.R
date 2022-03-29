## ====================================================================================
## Purpose:  Table 2 extension to all subgroups + main (all eligible)
## 
## Author:   Yinghui Wei
##
## Reviewer: Rochelle Knight
##
## Date:     29 March 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  Create input data for table 2
##
## Output:   rds file: input_population_event_stage1.rds
## ====================================================================================

# read in data------------------------------------------------------------

input <- read_rds(paste0("output/input_",population,"_stage1.rds"))

# Define age groups
input$cov_cat_age_group <- ""
input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_110", input$cov_cat_age_group)

# rename variables to indicate them as subgroups
setnames(input,
         old = c("cov_cat_sex",
                 "cov_cat_age_group",
                 "cov_cat_ethnicity",
                 "cov_bin_vte"),
         new = c("sub_bin_sex",
                 "sub_cat_age_group",
                 "sub_cat_ethnicity",
                 "sub_bin_vte"))

levels(input$sub_cat_ethnicity) <- c("White", "Mixed", "South_Asian", "Black", "Other", "Missing")
event_dates_names <- active_analyses$outcome_variable
outcome_names <- tidyselect::vars_select(names(input), starts_with(c("out_"), ignore.case=TRUE))
outcome_names_not_active <- outcome_names[!outcome_names %in% event_dates_names]

input$sub_main <- "main"

sub_grp_names <- tidyselect::vars_select(names(input), starts_with(c('sub_'), ignore.case = TRUE))

vars_names <- tidyselect::vars_select(names(input), !starts_with(c('cov_','qa_','vax_cat'), ignore.case = TRUE))
vars_names <- vars_names[!vars_names %in% outcome_names_not_active]

survival_data <- input[,vars_names]

rm(list=c("input"))

survival_data <- survival_data %>% mutate(cohort_start_date = cohort_start,cohort_end_date = cohort_end)

data_active$event_date <- survival_data[,event]
# filter the population according to whether the subgroup is covid_history
if(subgrp == "covid_history"){
  data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==T)
}else{
  data_active <- data_active %>% filter(sub_bin_covid19_confirmed_history ==F)
}

# filter the population according to the subgrp level
data_active$active_subgrp_full_name <- data_active[,subgrp_full_name]
data_active <- data_active%>%filter(active_subgrp_full_name==subgrp_level)

# specify the cohort according to vaccination status
if(cohort=="vaccinated"){
  data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_unexposed=min(event_date, exp_date_covid19_confirmed, death_date, cohort_end_date,na.rm = TRUE))
  data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_exposed=min(event_date, death_date, cohort_end_date,na.rm = TRUE))
}else if(cohort=="electively_unvaccinated"){
  data_active <- data_active %>% left_join(data_active%>%dplyr::select(patient_id,vax_date_covid_1))
  data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_unexposed = min(vax_date_covid_1,event_date, exp_date_covid19_confirmed, death_date,cohort_end_date,na.rm = TRUE))
  data_active <- data_active %>% rowwise() %>% mutate(follow_up_end_exposed = min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
  data_active <- data_active %>% dplyr::select(!c(vax_date_covid_1))
}

event_dates_names <- active_analyses$outcome_variable
outcome_names <- tidyselect::vars_select(names(input), starts_with(c("out_"), ignore.case=TRUE))
outcome_names_not_active <- outcome_names[!outcome_names %in% event_dates_names]
