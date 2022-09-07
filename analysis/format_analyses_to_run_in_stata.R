# This script reads in the analyses_to_run_in_stata.csv file and adds on the file name for where the saved dataset is stored

fs::dir_create(here::here("lib"))
analyses_to_run_stata <- read.csv("lib/analyses_to_run_in_stata.csv")
analyses_to_run_stata$X <- NULL
analyses_to_run_stata$file_name <- paste0("output/input_sampled_data_",analyses_to_run_stata$outcome,"_", analyses_to_run_stata$subgroup,"_",analyses_to_run_stata$cohort,"_",analyses_to_run_stata$time_periods,"_time_periods.csv")
write.csv(analyses_to_run_stata,"lib/analyses_to_run_in_stata_formatted.csv",row.names = F)

