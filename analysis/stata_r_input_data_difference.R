library(dplyr)

data_surv_stata <- readr::read_csv("output/stset.csv")
data_surv_r <- readr::read_csv("output/input_ami_covid_pheno_hospitalised_electively_unvaccinated_reduced_time_periods.csv")

data_surv_stata <- dplyr::rename(data_surv_stata, 
                                 "tstart" = "_t0",
                                 "tstop" = "_t",
                                 "event" = "outcome_status")

data_surv_stata$event <- ifelse(is.na(data_surv_stata$event), 
                                         0, data_surv_stata$event)

data_surv_stata <- data_surv_stata %>% select(patient_id,event, tstart,tstop,days0_28,days28_197)

data_surv_r <- data_surv_r %>% select(patient_id, event, tstart, tstop,days0_28,days28_197)

setequal(data_surv_stata,data_surv_r)
print(setdiff(data_surv_stata,data_surv_r))
print(setdiff(data_surv_r,data_surv_stata))

df <- as.data.frame(matrix(ncol = 1,nrow = 1))
df[1,1] <- "empty df just for output, see logs"
write.csv(df, paste0("output/stata_r_input_difference.csv"))

