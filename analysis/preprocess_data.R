# Load libraries ---------------------------------------------------------------

library(magrittr)

# Define parameters ------------------------------------------------------------

## Study start date
study_start <- "2021-06-01"

# Create spine dataset ---------------------------------------------------------

df <- arrow::read_feather(file = "output/input_index.feather",
                          col_select = c("patient_id","death_date"))

# Merge each dataset to the spine dataset --------------------------------------

for (i in c("index","vaccinated","electively_unvaccinated")) {

  ## Load dataset
  
  tmp <- arrow::read_feather(file = paste0("output/input_",i,".feather"))
  
  ## Identify dynamic variables in dataset
  
  keep <- c("patient_id",
            colnames(tmp)[grepl("sub_",colnames(tmp))], # Subgroups
            colnames(tmp)[grepl("exp_",colnames(tmp))], # Exposures
            colnames(tmp)[grepl("out_",colnames(tmp))], # Outcomes
            colnames(tmp)[grepl("cov_",colnames(tmp))]) # Covariates
  
  keep <- keep[!grepl("tmp_exp_",keep)]
  keep <- keep[!grepl("tmp_sub_",keep)]
  keep <- keep[!grepl("tmp_cov_",keep)]
  
  keep <- intersect(keep,colnames(tmp))
  
  tmp_dynamic <- tmp[,keep]
  
  ## Rename dynamic variables to indicate source data
  
  colnames(tmp_dynamic) <- paste0(colnames(tmp_dynamic),"_",i)
  
  tmp_dynamic <- dplyr::rename(tmp_dynamic, patient_id = paste0("patient_id_",i))
  
  ## Merge dynamic variables to spine
  
  df <- merge(df,tmp_dynamic, by = "patient_id")

  ## Identify static variables
  
  keep <- c("patient_id",
            colnames(tmp)[grepl("qa_",colnames(tmp))], # Quality assurance
            colnames(tmp)[grepl("vax_",colnames(tmp))]) # Vaccinations
  
  keep <- intersect(keep,colnames(tmp))
  
  tmp_static <- tmp[,keep]
  
  ## Merge static variables to spine if applicable
  
  if (ncol(tmp_static)>1) {
    df <- merge(df,tmp_static, by = "patient_id") 
  }
  
}

# Remove temporary datasets ----------------------------------------------------

rm(tmp, tmp_dynamic, tmp_static)

# Rename universally defined variables -----------------------------------------
# NB: These appear in only one dataset so do not need dataset specific names

df <- dplyr::rename(df, 
                    "cov_cat_sex" = "cov_cat_sex_electively_unvaccinated",
                    "cov_num_consulation_rate" = "cov_num_consulation_rate_index",
                    "cov_bin_healthcare_worker" = "cov_bin_healthcare_worker_index")

# Remove JCVI age variables ----------------------------------------------------
# NB: These are used to determine JCVI category only

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

# Split into "vaccinated" and "electively_unvaccinated" cohorts ----------------

for (j in c("vaccinated","electively_unvaccinated")) {
  
  # Make temporary data frame --------------------------------------------------
  
  tmp <- df 
  
  # Perform cohort specific preprocessing steps --------------------------------
  
  source(paste0("analysis/preprocess_",j,".R"))
  
  # Define COVID-19 severity --------------------------------------------------------------
  
  tmp$sub_cat_covid19_hospital <- "no_infection"
  
  tmp$sub_cat_covid19_hospital <- ifelse(!is.na(tmp$exp_date_covid19_confirmed),
                                        "non_hospitalised",tmp$sub_cat_covid19_hospital)
  
  tmp$sub_cat_covid19_hospital <- ifelse(!is.na(tmp$exp_date_covid19_confirmed) & 
                                          !is.na(tmp$sub_date_covid19_hospital) &
                                          (tmp$sub_date_covid19_hospital-tmp$exp_date_covid19_confirmed>=0 &
                                             tmp$sub_date_covid19_hospital-tmp$exp_date_covid19_confirmed<29),
                                        "hospitalised",tmp$sub_cat_covid19_hospital)
  
  tmp$sub_cat_covid19_hospital <- as.factor(tmp$sub_cat_covid19_hospital)
  tmp[,c("sub_date_covid19_hospital")] <- NULL

  # Restrict columns and save analysis dataset ---------------------------------
  
  tmp1 <- tmp[,c("patient_id","death_date","index_date",
              colnames(tmp)[grepl("sub_",colnames(tmp))], # Subgroups
              colnames(tmp)[grepl("exp_",colnames(tmp))], # Exposures
              colnames(tmp)[grepl("out_",colnames(tmp))], # Outcomes
              colnames(tmp)[grepl("cov_",colnames(tmp))], # Covariates
              colnames(tmp)[grepl("qa_",colnames(tmp))], # Quality assurance
              colnames(tmp)[grepl("vax_",colnames(tmp))])] # Vaccination
  
  tmp1[,colnames(tmp)[grepl("tmp_out_",colnames(tmp))]] <- NULL
  
  saveRDS(tmp1, file = paste0("output/input_",j,".rds"))
  
  # Restrict columns and save Venn diagram input dataset -----------------------
  
  tmp2 <- tmp[,c("patient_id",colnames(tmp)[grepl("out_",colnames(tmp))])]
  
  saveRDS(tmp2, file = paste0("output/venn_",j,".rds"))
  
}