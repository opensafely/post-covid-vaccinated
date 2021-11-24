# Load study definition output -------------------------------------------------

df <- arrow::read_feather(file = "output/input.feather") 

# Specify columns to keep ------------------------------------------------------

df <- df[,c("patient_id","death_date",
            colnames(df)[grepl("covid_vax_",colnames(df))], # Vaccinations
            colnames(df)[grepl("exp_",colnames(df))], # Exposures
            colnames(df)[grepl("out_",colnames(df))], # Outcomes
            colnames(df)[grepl("cov_",colnames(df))], # Covariates
            colnames(df)[grepl("qa_",colnames(df))])] # Quality assurance

# Convert dates to date format -------------------------------------------------

date_variables <-  c(colnames(df)[grepl("_date",colnames(df))], # Other date variables
                     colnames(df)[grepl("exp_",colnames(df))], # Exposures
                     colnames(df)[grepl("out_",colnames(df))]) # Outcomes

for (i in date_variables) {
  df[,i] <- as.Date(df[,i])
}

# Save file --------------------------------------------------------------------

saveRDS(df, file = "output/input.rds")