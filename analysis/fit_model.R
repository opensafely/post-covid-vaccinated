## =============================================================================
## MODEL3B: FULLY ADJUSTED -- USING PRE-DEFINED OUTSOME-SPECIFIC FIXED COVARIATES 
## and AMI BACKWARD-SELECTED COVARIATES
##
## Author: Samantha Ip
## =============================================================================
source(file.path(scripts_dir,"fit_get_data_surv_eventcountbasedtimecuts.R"))

rm_lowvar_covars <- function(data_surv){
  cov_bin <- colnames(data_surv)[grepl("cov_bin", colnames(data_surv))]
  df <- data_surv %>% dplyr::select(c( "expo", "event", all_of(cov_bin))) %>% distinct() %>% filter((expo==1) & (event==1))
  df <- df %>%  dplyr::select(!c("expo", "event", 
                                 df %>%  dplyr::select_if(is.numeric) %>% names(),
                                 #df %>%  dplyr::select(all_of(cat_cov)) %>% names()
  ))
  summary <- as.data.frame(summary(df,maxsum=50))
  summary$Freq=gsub(".*:", "",summary$Freq)#Remove everything before:
  summary$Freq <- as.numeric(summary$Freq)
  return(as.character(summary$Var2[summary$Freq <=2]))
}

calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

collapse_categorical_covars <- function(data_surv){
  cov_cat <-colnames(data_surv)[grepl("cov_cat", colnames(data_surv))]
  df <- data_surv %>% dplyr::select(c( "expo", "event", all_of(cov_cat))) %>% distinct() %>% filter((expo==1) & (event==1))
  df <- df %>%  dplyr::select(!c("expo", "event", 
                                 df %>%  dplyr::select_if(is.numeric) %>% names(),
                                 #df %>%  dplyr::select_if(~n_distinct(.)==2) %>% names()
  ))
  summary <- as.data.frame(summary(df,maxsum=50))
  summary$Freq=gsub(".*:", "",summary$Freq)#Remove everything before:
  summary$Freq <- as.numeric(summary$Freq)
  cat_cov_to_remove=unique(as.character(summary$Var2[summary$Freq <=2]))
  if("cov_cat_deprivation" %in% cat_cov_to_remove){
    data_surv=data_surv %>% mutate(cov_cat_deprivation= 
                                     case_when(cov_cat_deprivation=="1"~"1",
                                               cov_cat_deprivation=="2"~"1",
                                               cov_cat_deprivation=="3"~"2",
                                               cov_cat_deprivation=="4"~"3",
                                               cov_cat_deprivation=="5"~"3"))
    data_surv$cov_cat_deprivation=as.factor(data_surv$cov_cat_deprivation)
    data_surv$cov_cat_deprivation = relevel(data_surv$cov_cat_deprivation, ref = as.character(calculate_mode(data_surv$cov_cat_deprivation)))
    
    
    
  }else if("cov_cat_smoking_status" %in% cat_cov_to_remove){
    data_surv=data_surv %>% mutate(cov_cat_smoking_status=
                                     case_when(cov_cat_smoking_status=="N"~"N",
                                               cov_cat_smoking_status=="E"~"S",
                                               cov_cat_smoking_status=="S"~"S",
                                               cov_cat_smoking_status=="M"~"M"))
    data_surv$cov_cat_smoking_status=as.factor(data_surv$cov_cat_smoking_status)
    data_surv$cov_cat_smoking_status = relevel(data_surv$cov_cat_smoking_status, ref = as.character(calculate_mode(data_surv$cov_cat_smoking_status)))
  }
  
  return(list(data_surv,cat_cov_to_remove))
}


#------------------------ GET SURV FORMULA & COXPH() ---------------------------
coxfit <- function(data_surv, interval_names, covar_names){
  
  if(mdl == "mdl_max_adj"){
    covars_to_remove <- rm_lowvar_covars(data_surv)[!is.na((rm_lowvar_covars(data_surv)))]
    data_surv <- data_surv %>% dplyr::select(!all_of(covars_to_remove))
    collapse_covars_list=collapse_categorical_covars(data_surv)
    data_surv=collapse_covars_list[[1]]
    covars_collapsed=collapse_covars_list[[2]]
    covars_collapsed=unique(covars_collapsed[covars_collapsed %in% c("cov_cat_deprivation","cov_cat_smoking_status")])
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
  if ((startsWith(strata,"sex_"))==F & (!"SEX" %in% covariates_excl_region_sex_age)){
    surv_formula <- paste(surv_formula, "SEX", sep="+")
  }

  #If subgroup is not age then add in age spline otherwise use age and age_sq
  if ((startsWith(strata,"agegp_"))==F){
    surv_formula <- paste(surv_formula, "rms::rcs(data_surv$age,knot_placement)", sep="+")
  }else if ((startsWith(strata,"agegp_"))==T){
    surv_formula <- paste(surv_formula, "age + age_sq", sep="+")
  }
  
  # fit cox model
  dd <<- datadist(data_surv)
  options(datadist="dd")
  fit_cox_model <-rms::cph(formula=as.formula(surv_formula),data=data_surv, weight=data_surv$cox_weights,surv = TRUE,x=TRUE,y=TRUE)
  robust_fit_cox_model=rms::robcov(fit_cox_model, cluster = data_surv$patient_id)##?for robust standard errors
  
  
  # Results ----
  results=as.data.frame(names(fit_cox_model$coefficients))
  colnames(results)="term"
  results$estimate=exp(fit_cox_model$coefficients)
  results$conf.low=exp(confint(robust_fit_cox_model,level=0.95)[,1])
  results$conf.high=exp(confint(robust_fit_cox_model,level=0.95)[,2])
  results$std.error=exp(sqrt(diag(vcov(fit_cox_model))))
  results$robust.se=exp(sqrt(diag(vcov(robust_fit_cox_model))))
  if(mdl == "mdl_max_adj"){
    results$covariates_removed=paste0(covars_to_remove, collapse = ",")
    results$cat_covars_collapsed=paste0(covars_collapsed, collapse = ",")
  }
  
  #Add in P-values to results table
  #Can only get for covariate as a whole and not for each level so left join onto main covariate name
  results$covariate=results$term
  results$covariate=sub('\\=.*', '', results$covariate)
  anova_fit_cox_model=as.data.frame(anova(fit_cox_model))
  anova_fit_cox_model$covariate=row.names(anova_fit_cox_model)
  anova_fit_cox_model=anova_fit_cox_model%>%select("covariate","P")
  results=results%>%left_join(anova_fit_cox_model,by="covariate")
  
  
  return(results)
}

fit_model_reducedcovariates <- function(event, stratify_by_subgroup, stratify_by, survival_data,covar_names,input){
  list_data_surv_noncase_ids_interval_names <- fit_get_data_surv(event, stratify_by_subgroup, stratify_by, survival_data,cuts_days_since_expo)
  if(length(list_data_surv_noncase_ids_interval_names)==1){
    analyses_not_run <<- list_data_surv_noncase_ids_interval_names[[1]]
    return(fit_model_reducedcovariates)
  }
  
  data_surv <- list_data_surv_noncase_ids_interval_names[[1]]
  noncase_ids <- list_data_surv_noncase_ids_interval_names[[2]]
  interval_names <-list_data_surv_noncase_ids_interval_names[[3]]
  ind_any_zeroeventperiod <- list_data_surv_noncase_ids_interval_names[[4]]
  non_case_weight=list_data_surv_noncase_ids_interval_names[[5]]
  less_than_400_events=list_data_surv_noncase_ids_interval_names[[6]]
  if(less_than_400_events=="TRUE"){
    analyses_not_run[nrow(analyses_not_run)+1,]<<-c(event,stratify_by_subgroup,stratify_by,"TRUE","TRUE","TRUE","FALSE")
    return(fit_model_reducedcovariates)
  }

  if (ind_any_zeroeventperiod==TRUE){
    list_data_surv_noncase_ids_interval_names <- fit_get_data_surv(event, stratify_by_subgroup, stratify_by, survival_data,cuts_days_since_expo=cuts_days_since_expo_reduced)
    data_surv <- list_data_surv_noncase_ids_interval_names[[1]]
    noncase_ids <- list_data_surv_noncase_ids_interval_names[[2]]
    interval_names <-list_data_surv_noncase_ids_interval_names[[3]]
    ind_any_zeroeventperiod <- list_data_surv_noncase_ids_interval_names[[4]]
    non_case_weight=list_data_surv_noncase_ids_interval_names[[5]]
  }
  
  #Select covariates if using model mdl_max_adj
  if(mdl=="mdl_max_adj"){
    covars=input%>%dplyr::select(all_of(covar_names))
    covar_names <- names(covars)[ names(covars) != "patient_id"]
    data_surv <- data_surv %>% left_join(covars)
  }
  
  #Add inverse probablity weights for non-cases
  data_surv$cox_weights <- ifelse(data_surv$patient_id %in% noncase_ids, non_case_weight, 1)
  
  # Fit model and prep output csv
  fit_model <- coxfit(data_surv, interval_names, covar_names)
  fit_model$strata <- paste0(stratify_by_subgroup,"_",stratify_by)
  fit_model$event <- event
  fit_model$project <- project
  fit_model$model <- mdl
  fit_model$covid_history <- covid_history
  
  write.csv(fit_model, paste0(output_dir,"/tbl_hr_" , save_name,"_",stratify_by, "_", event, "_",project,"_",mdl,"_",covid_history, ".csv"), row.names = T)
  
}




