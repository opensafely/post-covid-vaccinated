## =============================================================================
## 1.Calls the functions that formats the survival data into the relevant format
## to be used in the cox model
## 2.Defines the cox survival formula and fits the cox model
## 3.Format the results table
## =============================================================================
source(file.path(scripts_dir,"fit_get_data_surv_eventcountbasedtimecuts.R"))


#------------------FORMAT SURVIVAL DATASET AND RUN COX MODEL--------------------

fit_model_reducedcovariates <- function(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo,cuts_days_since_expo_reduced,covar_names,total_covid_cases){
  list_data_surv_noncase_ids_interval_names <- fit_get_data_surv(event,subgroup, stratify_by_subgroup, stratify_by,mdl, survival_data,cuts_days_since_expo)
  if(length(list_data_surv_noncase_ids_interval_names)==1){
    analyses_not_run <<- list_data_surv_noncase_ids_interval_names[[1]]
    return(fit_model_reducedcovariates)
  }
  
  data_surv <- list_data_surv_noncase_ids_interval_names[[1]]
  noncase_ids <- list_data_surv_noncase_ids_interval_names[[2]]
  interval_names <-list_data_surv_noncase_ids_interval_names[[3]]
  ind_any_zeroeventperiod <- list_data_surv_noncase_ids_interval_names[[4]]
  non_case_inverse_weight=list_data_surv_noncase_ids_interval_names[[5]]
  less_than_400_events=list_data_surv_noncase_ids_interval_names[[6]]
  if(less_than_400_events=="TRUE"){
    analyses_not_run[nrow(analyses_not_run)+1,]<<-c(event,subgroup,cohort,mdl,"TRUE","TRUE","TRUE","FALSE")
    return(fit_model_reducedcovariates)
  }

  if(ind_any_zeroeventperiod==TRUE){
    list_data_surv_noncase_ids_interval_names <- fit_get_data_surv(event,subgroup, stratify_by_subgroup, stratify_by,mdl,survival_data, cuts_days_since_expo=cuts_days_since_expo_reduced)
    data_surv <- list_data_surv_noncase_ids_interval_names[[1]]
    noncase_ids <- list_data_surv_noncase_ids_interval_names[[2]]
    interval_names <-list_data_surv_noncase_ids_interval_names[[3]]
    ind_any_zeroeventperiod <- list_data_surv_noncase_ids_interval_names[[4]]
    non_case_inverse_weight=list_data_surv_noncase_ids_interval_names[[5]]
  }
  
  #Select covariates if using model mdl_max_adj
  if(mdl=="mdl_max_adj"){
    covars=input%>%dplyr::select(all_of(covar_names))
    covar_names = names(covars)[ names(covars) != "patient_id"]
    data_surv <- data_surv %>% left_join(covars)
  }
 
  #Add inverse probablity weights for non-cases
  data_surv$cox_weights <- ifelse(data_surv$patient_id %in% noncase_ids, non_case_inverse_weight, 1)
  
  # Fit model and prep output csv
  fit_model <- coxfit(data_surv, interval_names, covar_names, subgroup, mdl)
  fit_model$subgroup <- subgroup
  fit_model$event <- event
  fit_model$cohort <- cohort
  fit_model$model <- mdl
  fit_model$total_covid19_cases <- total_covid_cases
  
  write.csv(fit_model, paste0(output_dir,"/tbl_hr_" , event, "_",subgroup,"_", cohort,"_",mdl, ".csv"), row.names = T)
  print(paste0("Hazard ratios saved: ", output_dir,"/tbl_hr_" , event, "_",subgroup,"_", cohort,"_",mdl, ".csv"))
}


#------------------------ GET SURV FORMULA & COXPH() ---------------------------
coxfit <- function(data_surv, interval_names, covar_names, subgroup, mdl){
  print("Working on cox model")
  
  if(mdl == "mdl_max_adj"){
    covars_to_remove <- rm_lowvar_covars(data_surv)[!is.na((rm_lowvar_covars(data_surv)))]
    print(paste0("Covariates removed: ", covars_to_remove))
    data_surv <- data_surv %>% dplyr::select(!all_of(covars_to_remove))
    collapse_covars_list=collapse_categorical_covars(data_surv)
    data_surv=collapse_covars_list[[1]]
    covars_collapsed=collapse_covars_list[[2]]
    covars_collapsed=unique(covars_collapsed[covars_collapsed %in% c("cov_cat_deprivation","cov_cat_smoking_status")])
  }
  
  print("Post Exposure event counts split by covariate levels")
  if(mdl=="mdl_agesex"){
    print(covariate_exploration(data_surv, c()))
  }else{
    covars_to_print <- covar_names[!covar_names %in% covars_to_remove]
    print(covariate_exploration(data_surv, append(covars_to_print,"ethnicity")))
  }

  covariates <- covar_names[covar_names %in% names(data_surv)] %>% sort()
  interval_names_withpre <- c("days_pre", interval_names)
  
  # get Survival formula ----
  
  covariates_excl_region_sex_age <- unique(c(interval_names, covariates))
  knot_placement=as.numeric(quantile(data_surv$age, probs=c(0.1,0.5,0.9)))
  
  #Base formula
  if(mdl=="mdl_agesex"){
    surv_formula <- paste0(
      "Surv(tstart, tstop, event) ~ ",
      paste(interval_names, collapse="+"), 
      "+ cluster(patient_id) + strat(region_name)")
  }else if (mdl=="mdl_max_adj"){
    surv_formula <- paste0(
      "Surv(tstart, tstop, event) ~ ",
      paste(covariates_excl_region_sex_age, collapse="+"), 
      "+ cluster(patient_id) + strat(region_name)")
  }
 
  #If subgroup is not sex then add sex into formula
  if ((startsWith(subgroup,"sex"))==F & (!"sex" %in% covariates_excl_region_sex_age)){
    surv_formula <- paste(surv_formula, "sex", sep="+")
  }
  
  #If subgroup is not ethnicity then add ethnicity into formula
  if ((startsWith(subgroup,"ethnicity"))==F & (!"ethnicity" %in% covariates_excl_region_sex_age) & mdl == "mdl_max_adj"){
    surv_formula <- paste(surv_formula, "ethnicity", sep="+")
  }
  
  #If subgroup is not age then add in age spline otherwise use age and age_sq
  if ((startsWith(subgroup,"agegp_"))==F){
    surv_formula <- paste(surv_formula, "rms::rcs(age,parms=knot_placement)", sep="+")
  }else if ((startsWith(subgroup,"agegp_"))==T){
    surv_formula <- paste(surv_formula, "age + age_sq", sep="+")
  }
  
  print(surv_formula)
  
  # fit cox model
  dd <<- datadist(data_surv)
  #options(datadist="dd")
  options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment"))
  print("Fitting cox model")
  fit_cox_model <-rms::cph(formula=as.formula(surv_formula),data=data_surv, weight=data_surv$cox_weights,surv = TRUE,x=TRUE,y=TRUE)
  # To get robust variance-covariance matrix so that robust standard errots can be used in CI's
  robust_fit_cox_model=rms::robcov(fit_cox_model, cluster = data_surv$patient_id)
  print("Finished fitting cox model")
  
  # Results ----
  results=as.data.frame(names(fit_cox_model$coefficients))
  colnames(results)="term"
  results$estimate=exp(fit_cox_model$coefficients)
  results$conf.low=exp(confint(robust_fit_cox_model,level=0.95)[,1]) #use robust standard errors to calculate CI
  results$conf.high=exp(confint(robust_fit_cox_model,level=0.95)[,2])
  results$std.error=exp(sqrt(diag(vcov(fit_cox_model))))
  results$robust.se=exp(sqrt(diag(vcov(robust_fit_cox_model))))
  if(mdl == "mdl_max_adj"){
    results$covariates_removed=paste0(covars_to_remove, collapse = ",")
    results$cat_covars_collapsed=paste0(covars_collapsed, collapse = ",")
    print(paste0("Categorical covariates collapsed: ", covars_collapsed))
  }
  
  #Add in P-values to results table
  #Can only get for covariate as a whole and not for each level so left join onto main covariate name
  results$covariate=results$term
  results$covariate=sub('\\=.*', '', results$covariate)
  results$P="NA"
  #anova_fit_cox_model=as.data.frame(anova(fit_cox_model))
  #anova_fit_cox_model$covariate=row.names(anova_fit_cox_model)
  #anova_fit_cox_model=anova_fit_cox_model%>%select("covariate","P")
  #results=results%>%left_join(anova_fit_cox_model,by="covariate")
  
  print("Finised working on cox model")
  return(results)
}




