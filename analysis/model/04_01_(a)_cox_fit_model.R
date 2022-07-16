## =============================================================================
## 1.Calls the functions that formats the survival data into the relevant format
## to be used in the cox model
## 2.Defines the cox survival formula and fits the cox model
## 3.Format the results table
## =============================================================================
source(file.path(scripts_dir,"04_01_(b)_cox_format_survival_data.R"))


#------------------FORMAT SURVIVAL DATASET AND RUN COX MODEL--------------------

fit_model_reducedcovariates <- function(event,subgroup,stratify_by_subgroup,stratify_by,mdl, survival_data,input,cuts_days_since_expo,cuts_days_since_expo_reduced,covar_names,reduced_covar_names,total_covid_cases,time_point){
  list_data_surv_noncase_ids_interval_names <- fit_get_data_surv(event,subgroup, stratify_by_subgroup, stratify_by,survival_data,cuts_days_since_expo,time_point)
  if(length(list_data_surv_noncase_ids_interval_names)==1){
    analyses_not_run <<- list_data_surv_noncase_ids_interval_names[[1]]
    return(fit_model_reducedcovariates)
  }
  
  data_surv <- list_data_surv_noncase_ids_interval_names[[1]]
  noncase_ids <- list_data_surv_noncase_ids_interval_names[[2]]
  interval_names <-list_data_surv_noncase_ids_interval_names[[3]]
  ind_any_zeroeventperiod <- list_data_surv_noncase_ids_interval_names[[4]]
  non_case_inverse_weight=list_data_surv_noncase_ids_interval_names[[5]]
  less_than_50_events=list_data_surv_noncase_ids_interval_names[[6]]
  sampled_data <- list_data_surv_noncase_ids_interval_names[[7]]
  
  if(less_than_50_events=="TRUE"){
    analyses_not_run[nrow(analyses_not_run)+1,]<<-c(event,subgroup,cohort,mdl,"TRUE","TRUE","TRUE","FALSE")
    return(fit_model_reducedcovariates)
  }

  if(ind_any_zeroeventperiod==TRUE){
    list_data_surv_noncase_ids_interval_names <- fit_get_data_surv(event,subgroup, stratify_by_subgroup, stratify_by,survival_data, cuts_days_since_expo=cuts_days_since_expo_reduced,time_point)
    data_surv <- list_data_surv_noncase_ids_interval_names[[1]]
    noncase_ids <- list_data_surv_noncase_ids_interval_names[[2]]
    interval_names <-list_data_surv_noncase_ids_interval_names[[3]]
    ind_any_zeroeventperiod <- list_data_surv_noncase_ids_interval_names[[4]]
    non_case_inverse_weight=list_data_surv_noncase_ids_interval_names[[5]]
    sampled_data <- list_data_surv_noncase_ids_interval_names[[7]]
  }
  
  #Select covariates if using model mdl_max_adj
  if("mdl_max_adj" %in% mdl){
    covars=input%>%dplyr::select(all_of(covar_names))
    covar_names = names(covars)[ names(covars) != "patient_id"]
    data_surv <- data_surv %>% left_join(covars)
    sampled_data <- sampled_data %>% left_join(covars)
  }
 
  if(subgroup=="covid_pheno_hospitalised" ){
    # Merge missing ethnicity into white ethnicity
    data_surv <- data_surv %>% mutate(ethnicity = as.character(ethnicity))%>%
      mutate(ethnicity = case_when(ethnicity=="White" ~ "White, including missing",
                                   ethnicity=="Mixed" ~ "Mixed",
                                   ethnicity=="South Asian" ~ "South Asian",
                                   ethnicity=="Black" ~ "Black",
                                   ethnicity=="Other" ~ "Other",
                                   ethnicity=="Missing" ~ "White, including missing"
      ))
    
    relevel_with <- get_mode(data_surv,"ethnicity")
    
    data_surv <- data_surv %>% mutate(ethnicity = as.factor(ethnicity))%>%
      mutate(ethnicity = relevel(ethnicity,ref=relevel_with))
    
    print(paste0("Ethnicity releveled with: ",relevel_with))
    print(unique(data_surv$ethnicity))
    
    sampled_data <- sampled_data %>% mutate(ethnicity = as.character(ethnicity))%>%
      mutate(ethnicity = case_when(ethnicity=="White" ~ "White, including missing",
                                   ethnicity=="Mixed" ~ "Mixed",
                                   ethnicity=="South Asian" ~ "South Asian",
                                   ethnicity=="Black" ~ "Black",
                                   ethnicity=="Other" ~ "Other",
                                   ethnicity=="Missing" ~ "White, including missing"
      ))
    
    relevel_with <- get_mode(sampled_data,"ethnicity")
    
    sampled_data <- sampled_data %>% mutate(ethnicity = as.factor(ethnicity))%>%
      mutate(ethnicity = relevel(ethnicity,ref=relevel_with))
    
  }
  
  if(subgroup=="covid_pheno_hospitalised"){
    # Merge missing smoking into ever smoker
    data_surv <- data_surv %>% mutate(cov_cat_smoking_status = as.character(cov_cat_smoking_status))%>%
      mutate(cov_cat_smoking_status = case_when(cov_cat_smoking_status=="Never smoker" ~ "Never smoker",
                                                cov_cat_smoking_status=="Ever smoker" ~ "Ever smoker",
                                                cov_cat_smoking_status=="Current smoker" ~ "Current smoker",
                                                cov_cat_smoking_status=="Missing" ~ "Ever smoker"
      ))
    
    
    relevel_with <- get_mode(data_surv,"cov_cat_smoking_status")
    
    data_surv <- data_surv %>% mutate(cov_cat_smoking_status = as.factor(cov_cat_smoking_status))%>%
      mutate(cov_cat_smoking_status = relevel(cov_cat_smoking_status,ref=relevel_with))
    
    print(paste0("Smoking status releveled with: ",relevel_with))
    print(unique(data_surv$cov_cat_smoking_status))
    
    sampled_data <- sampled_data %>% mutate(cov_cat_smoking_status = as.character(cov_cat_smoking_status))%>%
      mutate(cov_cat_smoking_status = case_when(cov_cat_smoking_status=="Never smoker" ~ "Never smoker",
                                                cov_cat_smoking_status=="Ever smoker" ~ "Ever smoker",
                                                cov_cat_smoking_status=="Current smoker" ~ "Current smoker",
                                                cov_cat_smoking_status=="Missing" ~ "Ever smoker"
      ))
    
    
    relevel_with <- get_mode(sampled_data,"cov_cat_smoking_status")
    
    sampled_data <- sampled_data %>% mutate(cov_cat_smoking_status = as.factor(cov_cat_smoking_status))%>%
      mutate(cov_cat_smoking_status = relevel(cov_cat_smoking_status,ref=relevel_with))
  }
    
  # Describe survival data
  sink(paste0("output/not-for-review/describe_data_surv_",event,"_", subgroup,"_",cohort,"_",time_point,"_time_periods.txt"))
  print(Hmisc::describe(data_surv))
  sink()
  
  # Save sampled data for Stata
  write.csv(sampled_data, paste0("output/input_sampled_data_",event,"_", subgroup,"_",cohort,"_",time_point,"_time_periods.csv") )
  
  
  if(event=="pe" & subgroup =="covid_pheno_hospitalised" & cohort == "electively_unvaccinated"){
    print("here")
    data.table::fwrite(data_surv, paste0("output/input_",event,"_", subgroup,"_",cohort,"_",time_point,"_time_periods.csv"))
    
  }else{
    data.table::fwrite(data_surv, paste0("output/input_",event,"_", subgroup,"_",cohort,"_",time_point,"_time_periods.csv"))
    
    #Fit model and prep output csv
    fit_model <- coxfit(data_surv, interval_names, covar_names,reduced_covar_names, mdl, subgroup)
    fit_model$subgroup <- subgroup
    fit_model$event <- event
    fit_model$cohort <- cohort
    fit_model$time_points <- time_point
    fit_model$total_covid19_cases <- total_covid_cases
    
    write.csv(fit_model, paste0(output_dir,"/tbl_hr_" , event, "_",subgroup,"_", cohort,"_",time_point, "_time_periods.csv"), row.names = T)
    print(paste0("Hazard ratios saved: ", output_dir,"/tbl_hr_" , event, "_",subgroup,"_", cohort,"_",time_point,  "_time_periods.csv"))
    
  }
}


#------------------------ GET SURV FORMULA & COXPH() ---------------------------
coxfit <- function(data_surv, interval_names, covar_names, reduced_covar_names, mdl, subgroup){
  print("Working on cox model")
  
  if("mdl_max_adj" %in% mdl){
    covars_to_remove <- rm_lowvar_covars(data_surv)[!is.na((rm_lowvar_covars(data_surv)))]
    print(paste0("Covariates removed: ", covars_to_remove))
    data_surv <- data_surv %>% dplyr::select(!all_of(covars_to_remove))
    collapse_covars_list=collapse_categorical_covars(data_surv,subgroup)
    data_surv=collapse_covars_list[[1]]
    covars_collapsed=collapse_covars_list[[2]]
    covars_collapsed=unique(covars_collapsed[covars_collapsed %in% c("cov_cat_deprivation","cov_cat_smoking_status")])
    print(paste0("Categorical covariates collapsed: ", covars_collapsed))
  }
  
  print("Post Exposure event counts split by covariate levels")
  if(!"mdl_max_adj" %in% mdl){
    print(covariate_exploration(data_surv, c()))
  }else if("mdl_max_adj" %in% mdl){
    covars_to_print <- covar_names[!covar_names %in% covars_to_remove]
    print(covariate_exploration(data_surv, append(covars_to_print,"ethnicity")))
  }
  
  covariates <- covar_names[covar_names %in% names(data_surv)] %>% sort()
  reduced_covariates <- intersect(covariates,reduced_covar_names)
  additional_covars_removed <- covariates[!covariates %in% reduced_covariates]
  print(paste0("Additional covariates removed for hospitalised analysis: ", additional_covars_removed))
  
  # get Survival formula ----
  
  covariates_excl_region_sex_age <- unique(c(interval_names, covariates))
  knot_placement=as.numeric(quantile(data_surv$age, probs=c(0.1,0.5,0.9)))
  
  combined_results <- as.data.frame(matrix(ncol=11,nrow=0))
  colnames(combined_results) <- c("term","estimate","conf.low","conf.high","std.error","robust.se","results_fitted","model","covariates_removed","cat_covars_collapsed","covariates_fitted")
  
  # For electively unvaccinated hospitalised ATE set the region reference as London
  if(subgroup == "covid_pheno_hospitalised" & ((event_name == "ate" & cohort == "electively_unvaccinated") | (event_name == "vte" & cohort == "vaccinated") | (event_name == "angina" & cohort == "vaccinated"))){
    data_surv$region_name <- relevel(data_surv$region_name, ref = "London")
    print("Region releveled with London before fitting cox")
  }
  
  for(model in mdl){
    #Base formula
    if(model %in% c("mdl_age_sex","mdl_age_sex_region")){
      surv_formula <- paste0(
        "Surv(tstart, tstop, event) ~ ",
        paste(interval_names, collapse="+"),
        "+ cluster(patient_id)")
    }else if (model =="mdl_max_adj"){
      surv_formula <- paste0(
        "Surv(tstart, tstop, event) ~ ",
        paste(covariates_excl_region_sex_age, collapse="+"),
        "+ cluster(patient_id)")
    }else if(model == "mdl_max_adj_reduced_covars"){
      surv_formula <- paste0(
        "Surv(tstart, tstop, event) ~ ",
        paste(c(interval_names,reduced_covariates), collapse="+"),
        "+ cluster(patient_id)")
    }
    
    # Add in region as either covariate or stratify by
    if (model %in% c("mdl_age_sex_region","mdl_max_adj","mdl_max_adj_reduced_covars")){
      if(subgroup=="covid_pheno_hospitalised"){
        surv_formula <- paste(surv_formula, "region_name", sep="+")
      }else{
        surv_formula <- paste(surv_formula, "strat(region_name)", sep="+")
      }
    }

    #If subgroup is not sex then add sex into formula
    if ((startsWith(subgroup,"sex"))==F & (!"sex" %in% covariates_excl_region_sex_age)){
      surv_formula <- paste(surv_formula, "sex", sep="+")
    }

    #If subgroup is not ethnicity then add ethnicity into formula
    if ((startsWith(subgroup,"ethnicity"))==F & (!"ethnicity" %in% covariates_excl_region_sex_age) & model %in% c("mdl_max_adj","mdl_max_adj_reduced_covars")){
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

    print("Cox output")
    print(fit_cox_model)
    print("Finished fitting cox model")

    # Results ----
    results=as.data.frame(names(fit_cox_model$coefficients))
    colnames(results)="term"
    results$estimate=exp(fit_cox_model$coefficients)
    results$conf.low=exp(confint(robust_fit_cox_model,level=0.95)[,1]) #use robust standard errors to calculate CI
    results$conf.high=exp(confint(robust_fit_cox_model,level=0.95)[,2])
    results$std.error=exp(sqrt(diag(vcov(fit_cox_model))))
    results$robust.se=exp(sqrt(diag(vcov(robust_fit_cox_model))))

    if(model == "mdl_max_adj"){
      results$covariates_removed=paste0(covars_to_remove, collapse = ",")
      results$cat_covars_collapsed=paste0(covars_collapsed, collapse = ",")
      results$covariates_fitted <- "normal"
    }else if(model =="mdl_max_adj_reduced_covars"){
      results$covariates_removed=paste0(c(covars_to_remove, additional_covars_removed), collapse = ",")
      results$cat_covars_collapsed=paste0(covars_collapsed, collapse = ",")
      results$covariates_fitted <- "reduced"
    }else{
      results$covariates_removed <- NA
      results$cat_covars_collapsed <- NA
      results$covariates_fitted <- NA
    }
    
    print("Print results")
    print(results)

    #Add in P-values to results table
    #Can only get for covariate as a whole and not for each level so left join onto main covariate name
    #results$covariate=results$term
    #results$covariate=sub('\\=.*', '', results$covariate)
    #results$P="NA"
    #anova_fit_cox_model=as.data.frame(anova(fit_cox_model))
    #anova_fit_cox_model$covariate=row.names(anova_fit_cox_model)
    #anova_fit_cox_model=anova_fit_cox_model%>%select("covariate","P")
    #results=results%>%left_join(anova_fit_cox_model,by="covariate")

    results$results_fitted <- ifelse(all(results$estimate < 200 & results$std.error <10 & results$robust.se <10),"fitted_successfully","fitted_unsuccessfully")

    results$model <- model
    combined_results <- rbind(combined_results,results)
  }
  
    print("Finised working on cox model")
    return(combined_results)
}
