library(dplyr)
library(readr)
library(stringr)
library(tidyr)

#Read in AER
df <- readr::read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-cardiovascular/pre-vax & delta/OS output/AER/compiled_results_for_plotting/15_06_2023/AER_compiled_results.csv")

df <- df %>% filter(days == 196 & subgroup == "aer_overall" & time_points == "reduced")

# The column cumulative_difference_absolute_excess_risk is weighted by th prevax population size
# See lines 144 onwards in analysis/model/Absolute_excess_risk_function.R

df$time_points <- NULL

# Get excess events at 28 weeks per 100,000 COVID-19 diagnosis
df$cumulative_difference_absolute_excess_risk <- df$cumulative_difference_absolute_excess_risk * 100000
df <- df %>% select(event,cohort,cumulative_difference_absolute_excess_risk)
colnames(df) <- c("Event", "cohort","Excess events after 100,000 COVID-19 diagnosis")

write.csv(df,"C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-cardiovascular/pre-vax & delta/OS output/AER/Estimated excess events at 28 weeks.csv", row.names = F )


