# specify study parameters
#For all analysis aside from age stratifed, analysis is performed across all ages 
agebreaks_all <- c(0, 110)
agelabels_all <- c("all")

#Age breaks and labels for age sub group analysis
agebreaks_strata <- c(0, 40, 60, 80, 111)
agelabels_strata <- c("18_39", "40_59", "60_79", "80_110")

cohort_start_date <- as.Date("2021-06-01")
cohort_end_date <- as.Date("2021-12-14")

#Used to split time since COVID exposure; when there are time periods with no events then
#a reduced number of time periods is used

cuts_days_since_expo <- c(14, 28, 56, 84, 197) 
cuts_days_since_expo_reduced <- c(28,197) 


active_analyses <- read_rds("output/active_analyses.rds")
active_analyses <- active_analyses %>%dplyr::filter(outcome_variable==paste0("out_date_",event_name) & active == "TRUE")

if(active_analyses$model=="all"){
    mdl=c("mdl_agesex","mdl_max_adj")
}else{
  mdl=active_analyses$model
}
  
if(active_analyses$cohort=="all"){
  cohort=c("vaccinated_delta", "electively_unvaccinated_delta")
}else{
  cohort=active_analyses$cohort
}  
  

analyses_to_run <- as.data.frame(t(active_analyses))
analyses_to_run$subgroup <- row.names(analyses_to_run)
colnames(analyses_to_run) <- c("run","subgroup")
analyses_to_run<- analyses_to_run %>% filter(run=="TRUE" & subgroup != "active" ) 
rownames(analyses_to_run) <- NULL
analyses_to_run <- analyses_to_run %>% select(!run)
analyses_to_run$event=event_name
analyses_to_run$strata <- NA

analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="main","main",analyses_to_run$strata)
analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="covid_history","TRUE",analyses_to_run$strata)

for(i in c("covid_pheno_","agegp_","sex_","ethnicity_","prior_history_")){
  analyses_to_run$strata <- ifelse(startsWith(analyses_to_run$subgroup,i),gsub(i,"",analyses_to_run$subgroup),analyses_to_run$strata)
  
}

analyses_to_run <- crossing(analyses_to_run,mdl,cohort)

if(strata == "main"){
  subgroup="main"
  which_strata="main"
  save_name="main"
}else if(strata == "covid_pheno_all"){
  subgroup="expo_pheno"
  which_strata=c("hospitalised","non_hospitalised")
  save_name="subgroup_covid_pheno"
}else if (startsWith(strata, "covid_pheno")==TRUE & strata != "covid_pheno_all"){
  subgroup="expo_pheno"
  which_strata=gsub("covid_pheno_","",strata)
  save_name="subgroup_covid_pheno"
}else if(strata == "agegp_all"){
  agebreaks=agebreaks_strata
  agelabels=agelabels_strata
  subgroup="agegp"
  which_strata=agelabels
  save_name="subgroup_agegp"
}else if(startsWith(strata,"agegp")==TRUE & strata !="agegp_all"){
  agebreaks=agebreaks_strata
  agelabels=agelabels_strata
  subgroup="agegp"
  which_strata=gsub("agegp_","",strata)
  save_name="subgroup_agegp"
}else if (strata == "sex_all"){
  subgroup="SEX"
  which_strata=c("F","M")
  save_name="subgroup_sex"
}else if (startsWith(strata,"sex")==TRUE & strata !="sex_all"){
  subgroup="SEX"
  which_strata=gsub("sex_","",strata)
  save_name="subgroup_sex"
}else if (strata == "ethnicity_all"){
  subgroup="cov_cat_ethnicity"
  which_strata = as.character(unique(input$cov_cat_ethnicity))
  save_name="subgroup_ethnicity"
}else if (startsWith(strata,"ethnicity")==TRUE & strata !="ethnicity_all"){
  subgroup="cov_cat_ethnicity"
  which_strata = gsub("ethnicity_","",strata)
  save_name="subgroup_ethnicity"
}else if (strata == "prior_history_all"){
  subgroup=NA
  which_strata = c("TRUE","FALSE")
  save_name="subgroup_prior_history"
}

#Create dataframe of all models that need to be run
ls_events_missing=crossing(event,subgroup,which_strata)

