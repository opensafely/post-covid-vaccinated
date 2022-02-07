# Create empty data frame ------------------------------------------------------

df <- data.frame(active = logical(),
                 outcome = character(),
                 outcome_variable = character(),
                 covariates = character(),
                 model = character(),
                 cohort	= character(),
                 covid_history = character(),
                 covid_pheno_hospitalised = character(),
                 covid_pheno_non_hospitalised = character(),
                 agegp_18_39 = character(),
                 agegp_40_59 = character(),
                 agegp_60_79 = character(),
                 agegp_80_110 = character(),
                 sex_M = character(),
                 sex_F = character(),
                 ethnicity_1 = character(),
                 ethnicity_2 = character(),
                 ethnicity_3 = character(),
                 ethnicity_4 = character(),
                 ethnicity_5 = character(),
                 ethnicity_6 = character(),
                 prior_history_true = character(),
                 prior_history_false = character(),
                 prior_history_var = character(),
                 stringsAsFactors = FALSE)

# Add cardiovascular outcomes --------------------------------------------------

outcomes <- c("acute myocardial infarction",
              "ischaemic stroke",
              "deep vein thrombosis",
              "pulmonary embolism",
              "transient ischaemic attack",
              "subarachnoid haemorrhage and haemorrhagic stroke",
              "heart failure",
              "angina")

outcomes_short <- c("ami","stroke_isch","dvt","pe","tia","stroke_sah_hs","hf","angina")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(TRUE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       rep("all",2),
                       rep(TRUE,3),
                       rep(FALSE,14),
                       "")
}


outcomes <- c("arterial thrombosis event",
              "venous thrombosis event")

outcomes_short <- c("ate","vte")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(TRUE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       rep("all",2),
                       rep(TRUE,17),
                       "")
}

df$prior_history_var <- ifelse(df$outcome=="arterial thrombosis event","sub_bin_ate",df$prior_history_var)
df$prior_history_var <- ifelse(df$outcome=="venous thrombosis event","cov_bin_vte",df$prior_history_var)

# Add diabetes outcomes --------------------------------------------------------

outcomes <- c("type 1 diabetes",
              "type 2 diabetes",
              "other or non-specific diabetes",
              "gestational diabetes")

outcomes_short <- c("diabetes_type1","diabetes_type2","diabetes_other","diabetes_gestational")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(FALSE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       rep("",21))
}

# Add diabetes outcomes --------------------------------------------------------

outcomes <- c("depression",
              "anxiety - general",
              "anxiety - obsessive compulsive disorder", 
              "anxiety - post traumatic stress disorder", 
              "eating disorders", 
              "serious mental illness",
              "self harm, aged >=10",
              "self harm, aged >=15",
              "suicide",
              "addiction")

outcomes_short <- c("out_date_depression",
                    "out_date_anxiety_general",
                    "out_date_anxiety_ocd", 
                    "out_date_anxiety_ptsd", 
                    "out_date_eating_disorders", 
                    "out_date_serious_mental_illness",
                    "out_date_self_harm_10plus",
                    "out_date_self_harm_15plus",
                    "out_date_suicide",
                    "out_date_addiction")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(FALSE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       rep("",21))
}

# Save active analyses list ----------------------------------------------------

saveRDS(df, file = "output/active_analyses.rds")