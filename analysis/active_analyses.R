# Create empty data frame ------------------------------------------------------

df <- data.frame(outcome = character(),
                 covariates = character(),
                 active = logical(),
                 stringsAsFactors = FALSE)

# Add cardiovascular outcomes --------------------------------------------------

for (i in c("ami","stroke_isch","dvt","pe","tia","stroke_sah_hs","hf","angina","ate","vte")) {
  df[nrow(df)+1,] <- c(paste0("out_date_",i),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       TRUE)
}

# Add diabetes outcomes --------------------------------------------------------

for (i in c("diabetes_type1","diabetes_type2","diabetes_other","diabetes_gestational")) {
  df[nrow(df)+1,] <- c(paste0("out_date_",i),
                       "",
                       FALSE)
}

# Save active analyses list ----------------------------------------------------

saveRDS(df, file = "output/active_analyses.rds")