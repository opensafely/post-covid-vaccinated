# Load study definition output -------------------------------------------------

df <- arrow::read_feather(file = "output/input.feather") 

# Specify columns to keep ------------------------------------------------------

df <- df[,c("patient_id","death_date",
            colnames(df)[grepl("exp_",colnames(df))], # Exposures
            colnames(df)[grepl("out_",colnames(df))], # Outcomes
            colnames(df)[grepl("cov_",colnames(df))], # Covariates
            colnames(df)[grepl("qa_",colnames(df))], # Quality assurance
            colnames(df)[grepl("vax_",colnames(df))])] # Vaccinations

# Remove JCVI age variables ----------------------------------------------------

df[,c("vax_jcvi_age_1","vax_jcvi_age_2")] <- NULL

# Convert dates to date format -------------------------------------------------

for (i in colnames(df)[grepl("_date",colnames(df))]) {
  df[,i] <- as.Date(df[,i])
}

# Convert numbers to number format ---------------------------------------------

df$qa_num_birth_year <- format(df$qa_num_birth_year,"%Y")

for (i in colnames(df)[grepl("_num",colnames(df))]) {
  df[,i] <- as.numeric(df[,i])
}

# Convert categories to factor format ------------------------------------------

for (i in colnames(df)[grepl("_cat",colnames(df))]) {
  df[,i] <- as.factor(df[,i])
}

# Convert binaries to logical format -------------------------------------------

for (i in colnames(df)[grepl("_bin",colnames(df))]) {
  df[,i] <- as.logical(df[,i])
}

# Generate vaccine variables ---------------------------------------------------

for (i in 1:3) {
  
  # Restrict to relevant columns and rename

  tmp <- df[,c("patient_id",paste0(c("vax_date_covid_","vax_date_AstraZeneca_","vax_date_Pfizer_","vax_date_Moderna_"),i))]
  colnames(tmp) <- c("patient_id","vax_date_covid","vax_date_AstraZeneca","vax_date_Pfizer","vax_date_Moderna")
  
  # Determine vaccination product

  tmp$vax_cat_product <- NA
  
  tmp$vax_cat_product <- ifelse(!is.na(tmp$vax_date_covid) & 
                                   tmp$vax_date_covid==tmp$vax_date_AstraZeneca &
                                   tmp$vax_date_covid!=tmp$vax_date_Pfizer &
                                   tmp$vax_date_covid!=tmp$vax_date_Moderna,"AstraZeneca",tmp$vax_cat_product)
  
  tmp$vax_cat_product <- ifelse(!is.na(tmp$vax_date_covid) & 
                                   tmp$vax_date_covid!=tmp$vax_date_AstraZeneca &
                                   tmp$vax_date_covid==tmp$vax_date_Pfizer &
                                   tmp$vax_date_covid!=tmp$vax_date_Moderna,"Pfizer",tmp$vax_cat_product)
  
  tmp$vax_cat_product <- ifelse(!is.na(tmp$vax_date_covid) & 
                                   tmp$vax_date_covid!=tmp$vax_date_AstraZeneca &
                                   tmp$vax_date_covid!=tmp$vax_date_Pfizer &
                                   tmp$vax_date_covid==tmp$vax_date_Moderna,"Moderna",tmp$vax_cat_product)
  
  # Add information to main data

  tmp <- tmp[,c("patient_id","vax_cat_product")]
  colnames(tmp) <- c("patient_id",paste0("vax_cat_product_",i))
  df <- merge(df, tmp, by = "patient_id")
  
  # Remove unnecessary vaccination product information

  df[,paste0(c("vax_date_AstraZeneca_","vax_date_Pfizer_","vax_date_Moderna_"),i)] <- NULL
  
}

# Define severity --------------------------------------------------------------

df$exp_cat_covid19_hospital <- "no_infection"

df$exp_cat_covid19_hospital <- ifelse(!is.na(df$exp_date_covid19_confirmed),
                                      "non_hospitalised",df$exp_cat_covid19_hospital)

df$exp_cat_covid19_hospital <- ifelse(!is.na(df$exp_date_covid19_confirmed) & 
                                        !is.na(df$exp_date_covid19_hospital) &
                                        (df$exp_date_covid19_hospital-df$exp_date_covid19_confirmed>=0 &
                                           df$exp_date_covid19_hospital-df$exp_date_covid19_confirmed<29),
                                      "hospitalised_within28days",df$exp_cat_covid19_hospital)

df$exp_cat_covid19_hospital <- ifelse(!is.na(df$exp_date_covid19_confirmed) & 
                                        !is.na(df$exp_date_covid19_hospital) &
                                        (df$exp_date_covid19_hospital-df$exp_date_covid19_confirmed>=29),
                                      "hospitalised_after28days",df$exp_cat_covid19_hospital)

# Specify columns to keep ------------------------------------------------------

df <- df[,c("patient_id","death_date",
            colnames(df)[grepl("exp_",colnames(df))], # Exposures
            colnames(df)[grepl("out_",colnames(df))], # Outcomes
            colnames(df)[grepl("cov_",colnames(df))], # Covariates
            colnames(df)[grepl("qa_",colnames(df))], # Quality assurance
            colnames(df)[grepl("vax_",colnames(df))])] # Vaccination

# Save file --------------------------------------------------------------------

saveRDS(df, file = "output/input.rds")