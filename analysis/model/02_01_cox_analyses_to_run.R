##==============================================================================
## Reads in the active analyses table which specifies which 
## analysis to run for the outcome
##
## Creates a table of all the analyses to run for the outcome
## =============================================================================

## Read in active analyses table and filter to relevant outcome

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>%dplyr::filter(outcome_variable==paste0("out_date_",event_name) & active == "TRUE")

## Select covariates of interest 
covar_names<-str_split(active_analyses$covariates, ";")[[1]]
covar_names<-append(covar_names,"patient_id")
covar_names<-covar_names[!covar_names %in% c("cov_num_age","cov_cat_ethnicity","cov_cat_region","cov_cat_sex")]

##Set which models and cohorts are required

if(active_analyses$model=="all"){
  mdl=c("mdl_agesex","mdl_max_adj")
}else{
  mdl=active_analyses$model
}


## Transpose active_analyses to single column so can filter to analysis models to run

analyses_to_run <- as.data.frame(t(active_analyses))
analyses_to_run$subgroup <- row.names(analyses_to_run)
colnames(analyses_to_run) <- c("run","subgroup")
analyses_to_run<- analyses_to_run %>% filter(run=="TRUE" & subgroup != "active" ) 
rownames(analyses_to_run) <- NULL
analyses_to_run <- analyses_to_run %>% select(!run)
analyses_to_run$event=event_name

## Add in  all possible combinations of the subgroups, models and cohorts
analyses_to_run <- crossing(analyses_to_run,cohort)

## Add in which covariates to stratify by
analyses_to_run$stratify_by_subgroup=NA
for(i in c("ethnicity","sex")){
  analyses_to_run$stratify_by_subgroup <- ifelse(startsWith(analyses_to_run$subgroup,i),i,analyses_to_run$stratify_by_subgroup)
}
analyses_to_run$stratify_by_subgroup <- ifelse(startsWith(analyses_to_run$subgroup,"prior_history"),active_analyses$prior_history_var,analyses_to_run$stratify_by_subgroup)
analyses_to_run$stratify_by_subgroup <- ifelse(is.na(analyses_to_run$stratify_by_subgroup),analyses_to_run$subgroup,analyses_to_run$stratify_by_subgroup)


## Add in relevant subgroup levels to specify which stratum to run for
analyses_to_run$strata <- NA
analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="main","main",analyses_to_run$strata)
analyses_to_run$strata <- ifelse(analyses_to_run$subgroup=="covid_history","TRUE",analyses_to_run$strata)

for(i in c("covid_pheno_","agegp_","sex_","ethnicity_","prior_history_")){
  analyses_to_run$strata <- ifelse(startsWith(analyses_to_run$subgroup,i),gsub(i,"",analyses_to_run$subgroup),analyses_to_run$strata)
  
}

analyses_to_run$strata[analyses_to_run$strata=="South_Asian"]<- "South Asian"

# add subgroup category

analyses_to_run <- analyses_to_run %>% 
  dplyr::mutate(subgroup_cat = case_when(
    startsWith(subgroup, "agegp") ~ "age",
    startsWith(subgroup, "covid_history") ~ "covid_history",
    startsWith(subgroup, "covid_pheno") ~ "covid_pheno",
    startsWith(subgroup, "ethnicity") ~ "ethnicity",
    startsWith(subgroup, "main") ~ "main",
    startsWith(subgroup, "prior_history") ~ "prior_history",
    startsWith(subgroup, "sex") ~ "sex",
    TRUE ~ as.character(subgroup)))


## Separate into to dataframes as this will allow all the vaccinated/electively unvaccinated
## analyses to be run in one go to save having to read in the data for each individual analysis
## i.e can read it in once and run all the vaccinated analyses in one go

#for(i in cohort_to_run){
#  assign(paste0("analyses_to_run_",i),analyses_to_run %>% filter(cohort_to_run == i) )
#}
