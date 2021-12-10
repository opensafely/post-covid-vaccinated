cohort = "vaccinated"
study_start = "2021-06-01"

library(magrittr)

# Create spine dataset ---------------------------------------------------------

df <- arrow::read_feather(file = "output/input_index.feather",
                          col_select = c("patient_id","death_date"))

# Tidy input file and merge to dataset -----------------------------------------

for (i in c("index","other")) {

  ## Load study definition output
  
  tmp <- arrow::read_feather(file = paste0("output/input_",ifelse(i=="other",cohort,i),".feather"))
  
  ## Merge dynamic variables to main dataset
  
  keep <- c("patient_id",
            colnames(tmp)[grepl("exp_",colnames(tmp))], # Exposures
            colnames(tmp)[grepl("out_",colnames(tmp))], # Outcomes
            colnames(tmp)[grepl("cov_",colnames(tmp))]) # Covariates

  keep <- intersect(keep,colnames(tmp))
  
  tmp_dynamic <- tmp[,keep]
  
  colnames(tmp_dynamic) <- paste0(colnames(tmp_dynamic),"_",i)

  tmp_dynamic <- dplyr::rename(tmp_dynamic, patient_id = paste0("patient_id_",i))
  
  df <- merge(df,tmp_dynamic, by = "patient_id")

  ## Merge other specific variables if applicable
  
  keep <- c("patient_id",
            colnames(tmp)[grepl("qa_",colnames(tmp))], # Quality assurance
            colnames(tmp)[grepl("vax_",colnames(tmp))]) # Vaccinations
  
  keep <- intersect(keep,colnames(tmp))
  
  tmp_static <- tmp[,keep]
  
  if (ncol(tmp_static)>1) {
    df <- merge(df,tmp_static, by = "patient_id") 
  }
  
}

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

# Determine index date ---------------------------------------------------------

df$study_start_date <- as.Date(study_start)

df$vax_date_full <- as.Date(df$vax_date_covid_2)+14

df$use_date <- ifelse(df$study_start_date>df$vax_date_full & !is.na(),"index","other")

# Create sub cohorts -----------------------------------------------------------

index <- df[df$use_date=="index" & !is.na(df$use_date),]
index <- index[,!grepl("_other",colnames(index))]
colnames(index) <- gsub("_index","",colnames(index))
index$index_date <- index$study_start_date
index[,c("use_date")] <- NULL

other <- df[df$use_date=="other",]
other <- other[,!grepl("_index",colnames(other))]
colnames(other) <- gsub("_other","",colnames(other))
other$index_date <- other$vax_date_full
other[,c("use_date")] <- NULL
         
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