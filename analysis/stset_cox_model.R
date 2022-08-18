library(survival)
library(magrittr)

# Parameters

origin <- as.numeric(as.Date("2021-06-01"))

# Read in stset data

data_surv_stata <- readr::read_csv("output/stset.csv")

# Rename _t0 and _t as R does not like varnames starting without letters

data_surv_stata <- dplyr::rename(data_surv_stata, 
                                 "t0" = "_t0",
                                 "t" = "_t")

# Update outcome status as R will exclude observations with missing values

data_surv_stata$outcome_status <- ifelse(is.na(data_surv_stata$outcome_status), 
                                         0, data_surv_stata$outcome_status)

# Make empty results data frame

results <- data.frame(term = character(),
                      hr = numeric(),
                      se = numeric(),
                      pval = numeric(),
                      n = numeric(),
                      n_event = numeric(),
                      call = character())


# Perform Cox regression using Stata data

for (i in c("", "+ region", "+ strata(region)")) {
  
  fit_stata <- survival::coxph(as.formula(paste0("Surv(t0, t, outcome_status) ~ days0_28 + days28_197 + sex + age_spline1 + age_spline2",i)), 
                               data = data_surv_stata, 
                               robust = FALSE, 
                               method = "efron", 
                               id = patient_id)
  
  summary_fit_stata <- summary(fit_stata)
  
  tmp <- data.frame(summary_fit_stata$coefficients)
  tmp$term <- row.names(tmp)
  row.names(tmp) <- NULL
  tmp$call <- paste0("age + sex ",i)
  tmp$n <- summary_fit_stata$n
  tmp$n_event <- summary_fit_stata$nevent
  tmp <- dplyr::rename(tmp,
                       "hr" = "exp.coef.",
                       "se" = "se.coef.",
                       "pval" = "Pr...z..")
  
  tmp <- tmp[,c("term","hr","se","pval","n","n_event","call")]
  
  results <- rbind(results, tmp)
  
}

readr::write_csv(results, "output/stset_cox_model.csv")