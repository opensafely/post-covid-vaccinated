# Load excess risk function ----------------------------------------------------

source("analysis/excess_risk_function.R")

# Determine active analyses ----------------------------------------------------

active <- readr::read_rds("output/active_analyses.rds")
active <- active[active$active==TRUE,]
active$event <- gsub("out_date_","",active$outcome_variable)
active[,c("active","outcome","outcome_variable","prior_history_var","covariates")] <- NULL

active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event","model","cohort")), 
                              names_to = "strata")

active <- active[active$value==TRUE, c("event","model","cohort","strata")]
active$model <- ifelse(active$model=="all","mdl_agesex;mdl_max_adj",active$model)
active <- tidyr::separate_rows(active, model, sep = ";")
active$cohort <- ifelse(active$cohort=="all","vaccinated;electively_unvaccinated",active$cohort)
active <- tidyr::separate_rows(active, cohort, sep = ";")

# Run function -----------------------------------------------------------------

df <- NULL

for (i in 1:nrow(active)) {
  
  tmp <- excess_risk(event = active$event[i], 
                     model = active$model[i],
                     cohort = active$cohort[i],
                     strata = active$strata[i])
  
  df <- rbind(df,tmp)
  
}

# Save output ------------------------------------------------------------------

readr::write_csv(df, file = file.path("output", "excess_risk.csv"))