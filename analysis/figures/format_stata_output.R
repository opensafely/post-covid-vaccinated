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

active_analyses <- read_rds("lib/active_analyses.rds")






write.csv(df, file = paste0(results_dir,"/stata_output_formatted"),row.names = FALSE)

df <- df %>% filter(cohort == "pre_vaccination" 
                    & event == "vte"
                    & subgroup == "main")

