#Format stata output ready for plotting
library(stringi)
library(readr)
library(dplyr)
library(stringr)

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
df <- read.csv(paste0(results_dir,"/stata_output.csv"))
df_prevax <- read.csv(paste0(results_dir,"/stata_output_prevax.csv"))
df <- rbind(df, df_prevax)
df$X <- NULL

rm(df_prevax)

active_analyses <- read_rds("lib/active_analyses.rds")

## Transpose active_analyses to single column so can filter to analysis models to run
subgroup <- as.data.frame(t(active_analyses[1,]))
subgroup$subgroup <- row.names(subgroup)
colnames(subgroup) <- c("run","subgroup")
subgroup<- subgroup %>% filter((run=="TRUE" | run == "FALSE") & subgroup != "active" ) 
rownames(subgroup) <- NULL
subgroup <- subgroup %>% select(!run)
subgroup$subgroup <- paste0("_",subgroup$subgroup)

# Get cohort
df$cohort <- ifelse(grepl("electively_unvaccinated",df$source),"electively_unvaccinated", ifelse(grepl("pre_vaccination",df$source),"pre_vaccination","vaccinated"))
unique(df$cohort)

# Get outcome event name
df$event <- df$source
df$event <- gsub("input_sampled_data_","",df$event)
df$event <- sub('\\_electively_unvaccinated.*', '', df$event)
df$event <- sub('\\_vaccinated.*', '', df$event)
df$event <- sub('\\_pre_vaccination.*', '', df$event)
df$event <- stri_replace_all_regex(df$event,
                       pattern=subgroup$subgroup,
                       replacement=c(""),
                       vectorize=FALSE)
unique(df$event)

# Get subgroup
df$subgroup <- df$source
df$subgroup <- str_replace(df$subgroup,paste0("input_sampled_data_", df$event,"_"),"")
df$subgroup <- sub('\\_electively_unvaccinated.*', '', df$subgroup)
df$subgroup <- sub('\\_vaccinated.*', '', df$subgroup)
df$subgroup <- sub('\\_pre_vaccination.*', '', df$subgroup)
unique(df$subgroup)

# Rename model
df$model <- ifelse(df$model == "max", "mdl_max_adj","mdl_age_sex_region")

#Fomat columns
df$time_points <- "reduced"
df$results_fitted <- "fitted_successfully"
df$source <- NULL
df$N_outcomes <- NULL

#Exponentiate results
df$estimate <- exp(df$estimate)
df$conf_low <- exp(df$conf_low)
df$conf_high <- exp(df$conf_high)

#Some results have been run twice (once in stata and once in R so remove duplicates)

stata_analyses <- read_csv("lib/analyses_to_run_in_stata.csv")
stata_analyses <- stata_analyses %>% dplyr::rename(time_points=time_periods,
                                                   event = outcome)

stata_analyses$subgroup <- ifelse(stata_analyses$subgroup=="hospitalised","covid_pheno_hospitalised",stata_analyses$subgroup)
stata_analyses$subgroup <- ifelse(stata_analyses$subgroup=="non_hospitalised","covid_pheno_non_hospitalised",stata_analyses$subgroup)

df <- merge(df,stata_analyses, by=c("event","subgroup","cohort","time_points"))

#Previous time period days have been added to the median which hasn't been done in the R HRs abd gets done
# in the figure scripts. Removing here so that everything is the same
df$remove_from_median <- NA
df$remove_from_median <- ifelse(grepl("days",df$term),df$term,df$remove_from_median)
df$remove_from_median <- sub("days","",df$remove_from_median)
df$remove_from_median <- as.numeric(sub("\\_.*","",df$remove_from_median))

df$median_follow_up <- df$median_follow_up - df$remove_from_median

#Remove duplicate rows
df <- df[!duplicated(df), ]

df <- df %>% filter(event != "pe" | subgroup != "main")
df <- df %>% filter(event != "hf_primary_position" | subgroup != "covid_pheno_non_hospitalised" | cohort != "electively_unvaccinated")

write.csv(df, file = paste0(results_dir,"/stata_output_formatted"),row.names = FALSE)
