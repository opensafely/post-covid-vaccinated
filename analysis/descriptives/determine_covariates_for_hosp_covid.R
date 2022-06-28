# For hospitalsed COVID select covariates that have no 0 event counts in any time
# period or covariate level
library(dplyr)
library(readr)
library(stringr)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  cohort="both"
}else{
  cohort = args[[1]]
}

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>%dplyr::filter(active == "TRUE")

protected_covariates<-str_split(active_analyses$covariates, ";")[[1]]
protected_covariates <- protected_covariates[grepl("cov_num|cov_cat",protected_covariates)]

select_covariates_for_cox <- function(cohort){
  
  covariate_counts <- read_csv(paste0("output/not-for-review/hospitalised_event_counts_by_covariate_level_",cohort,".csv"))
  
  covariate_counts <- covariate_counts %>% filter(!Covariate %in% protected_covariates)
  covariate_counts$keep_covariate <- NA
  covariate_counts <- covariate_counts %>%
    group_by(event, Covariate) %>%
    dplyr::mutate(keep_covariate = case_when(
      any(unexposed_event_counts == 0 | days0_28_event_counts == 0 | days28_197_event_counts == 0) ~ "remove_covariate",
      TRUE ~ "keep_covariate"))
  
  covariates_to_adjust_for <- as.data.frame(matrix(ncol = 3, nrow = 0))
  colnames(covariates_to_adjust_for) <- c("outcome_event", "covariates", "subgroup")
  for(outcome_name in active_analyses$outcome_variable){
    df <- covariate_counts %>% filter(keep_covariate == "keep_covariate" & event == outcome_name)
    covariates_to_adjust_for[nrow(covariates_to_adjust_for)+1,] <- c(outcome_name,paste(c(unique(df$Covariate),protected_covariates), collapse = ";"),"covid_pheno_hospitalised")
  }
  
  write.csv(covariates_to_adjust_for, file = paste0("output/not-for-review/covariates_to_adjust_for_hosp_covid_",cohort,".csv"), row.names = F)
  
}

if(cohort == "both"){
  select_covariates_for_cox("vaccinated")
  select_covariates_for_cox("electively_unvaccinated")
}else{
  select_covariates_for_cox(cohort)
}
