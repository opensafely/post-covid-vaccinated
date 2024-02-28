# CREATE COMBINED (3 COHORT) TABLE 1A FOR POST-COVID MANUSCRIPTS

###############################################
# 0. Load relevant libraries and read in data #
###############################################

library(readr)
library(dplyr)
library(data.table)
library(tidyverse)
library(Hmisc)
library(broman)

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/table_1/"
dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

###############################################
# 1. CLEAN TABLE 1 FUNCTION
###############################################

clean_table_1 <- function(df) {
  df <- df %>% 
    mutate_at(c("Whole_population","COVID_exposed","COVID_hospitalised", "COVID_non_hospitalised"), as.numeric) %>%
    mutate(COVID_risk_per_100k = (COVID_exposed/Whole_population)*100000) %>%
    mutate_if(is.numeric, round, 0) %>%
    mutate("Number diagnosed with COVID-19 (risk per 100,000)" = paste0(add_commas(COVID_exposed), " (", COVID_risk_per_100k, ")"),
           "Covariate Level" = Covariate_level,
           "Whole Population" = add_commas(Whole_population)) %>%
    dplyr::select("Covariate", "Covariate Level", "Whole Population", "Number diagnosed with COVID-19 (risk per 100,000)")
}

table1_prevax <- read.csv(paste0(results_dir,"Table1_pre_vaccination_cvd.csv"))
table1_vax <- read.csv(paste0(results_dir,"Table1_vaccinated_without_covid_history_CVD.csv"))
table1_unvax <- read.csv(paste0(results_dir,"Table1_electively_unvaccinated_without_covid_history_CVD.csv"))

table1_prevax_format <- clean_table_1(table1_prevax)
table1_vax_format <- clean_table_1(table1_vax)
table1_unvax_format <- clean_table_1(table1_unvax)

# CONSTRUCT MAIN TABLE 1

colnames(table1_prevax_format)[3:4] <- paste(colnames(table1_prevax_format)[3:4], "prevax", sep = "_")
colnames(table1_vax_format)[3:4] <- paste(colnames(table1_vax_format)[3:4], "vax", sep = "_")
colnames(table1_unvax_format)[3:4] <- paste(colnames(table1_unvax_format)[3:4], "unvax", sep = "_")
  
table1_merged <- full_join(table1_prevax_format, table1_vax_format)
table1_merged <- full_join(table1_merged, table1_unvax_format)

table1_merged$Covariate <- capitalize(table1_merged$Covariate)
table1_merged$Covariate <- ifelse(table1_merged$Covariate == "Age group", "Age",table1_merged$Covariate)
table1_merged$Covariate <- ifelse(table1_merged$Covariate == "Deprivation", "Index of Multiple Deprivation",table1_merged$Covariate)
table1_merged$Covariate <- ifelse(table1_merged$Covariate == "Consulation rate group", "Consulation rate",table1_merged$Covariate)

table1_merged$`Covariate Level` <- ifelse(startsWith(table1_merged$Covariate, "History"),table1_merged$Covariate,table1_merged$`Covariate Level`)
table1_merged$`Covariate Level` <- sub("History of ","",table1_merged$`Covariate Level`)
table1_merged$`Covariate Level` <- capitalize(table1_merged$`Covariate Level`)
table1_merged$`Covariate Level` <- ifelse(table1_merged$`Covariate Level`=="Vte", "Venous thromboembolism",table1_merged$`Covariate Level`)
table1_merged$`Covariate Level` <- ifelse(table1_merged$`Covariate Level`=="Hf", "Heart failure",table1_merged$`Covariate Level`)
table1_merged$`Covariate Level` <- ifelse(table1_merged$`Covariate Level`=="Ami", "Acute myocardial infarction",table1_merged$`Covariate Level`)

table1_merged$Covariate <- ifelse(startsWith(table1_merged$Covariate,"History"),"Medical history",table1_merged$Covariate)
table1_merged <- table1_merged %>% filter(!startsWith(`Covariate Level`, "Mean"))
table1_merged[nrow(table1_merged)+1,] <- c("Cohort","","Pre-vaccinated","Pre-vaccinated","Vaccinated","Vaccinated","Electively unvaccinated","Electively unvaccinated")

table1_merged$Covariate <- factor(table1_merged$Covariate, levels = c("Cohort",
                                                                      "All",
                                                                      "Sex",
                                                                      "Age",
                                                                      "Ethnicity",
                                                                      "Index of Multiple Deprivation",
                                                                      "Smoking status",
                                                                      "Consulation rate",
                                                                      "Region",
                                                                      "Medical history"
                                                                      ))


table1_merged <- table1_merged[order(table1_merged$Covariate),]
table1_merged <- table1_merged %>% select(Covariate, `Covariate Level`, `Whole Population_prevax`, `Whole Population_vax`, 
                                          `Whole Population_unvax`,`Number diagnosed with COVID-19 (risk per 100,000)_prevax`,
                                          `Number diagnosed with COVID-19 (risk per 100,000)_vax`, `Number diagnosed with COVID-19 (risk per 100,000)_unvax`)

# SAVE TABLE 1
  
write.csv(table1_merged, paste0(output_dir,"Table1_formatted.csv"), row.names = FALSE) # Table 1 and S2

