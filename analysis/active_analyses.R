# Create output directory ------------------------------------------------------

fs::dir_create(here::here("lib"))

# Create empty data frame ------------------------------------------------------

df <- data.frame(active = logical(),
                 outcome = character(),
                 outcome_variable = character(),
                 covariates = character(),
                 model = character(),
                 cohort	= character(),
                 main = character(),
                 covid_history = character(),
                 covid_pheno_hospitalised = character(),
                 covid_pheno_non_hospitalised = character(),
                 agegp_18_39 = character(),
                 agegp_40_59 = character(),
                 agegp_60_79 = character(),
                 agegp_80_110 = character(),
                 sex_Male = character(),
                 sex_Female = character(),
                 ethnicity_White = character(),
                 ethnicity_Mixed = character(),
                 ethnicity_South_Asian = character(),
                 ethnicity_Black = character(),
                 ethnicity_Other = character(),
                 ethnicity_Missing = character(),
                 prior_history_TRUE = character(),
                 prior_history_FALSE = character(),
                 prior_history_var = character(),
                 stringsAsFactors = FALSE)

# Add cardiovascular outcomes --------------------------------------------------

outcomes <- c("Acute myocardial infarction",
              "Ischaemic stroke",
              "Deep vein thrombosis",
              "Pulmonary embolism",
              "Transient ischaemic attack",
              "Subarachnoid haemorrhage and haemorrhagic stroke",
              "Heart failure",
              "Angina")

outcomes_short <- c("ami","stroke_isch","dvt","pe","tia","stroke_sah_hs","hf","angina")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(TRUE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       rep("all",2),
                       rep(TRUE,4),
                       rep(FALSE,14),
                       "")
}


outcomes <- c("Arterial thrombosis event",
              "Venous thrombosis event")

outcomes_short <- c("ate","vte")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(TRUE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       rep("all",2),
                       rep(TRUE,18),
                       "")
}

df$prior_history_var <- ifelse(df$outcome=="Arterial thrombosis event","sub_bin_ate",df$prior_history_var)
df$prior_history_var <- ifelse(df$outcome=="Venous thrombosis event","cov_bin_vte",df$prior_history_var)

# Add diabetes outcomes --------------------------------------------------------

outcomes <- c("Type 1 diabetes",
              "Type 2 diabetes",
              "Other or non-specific diabetes",
              "Gestational diabetes")

outcomes_short <- c("diabetes_type1","diabetes_type2","diabetes_other","diabetes_gestational")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(FALSE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       rep("",22))
}

# Add diabetes outcomes --------------------------------------------------------

outcomes <- c("Depression",
              "Anxiety - general",
              "Anxiety - obsessive compulsive disorder", 
              "Anxiety - post traumatic stress disorder", 
              "Eating disorders", 
              "Serious mental illness",
              "Self harm, aged >=10",
              "Self harm, aged >=15",
              "Suicide",
              "Addiction")

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
                       rep("",22))
}

# Save active analyses list ----------------------------------------------------

saveRDS(df, file = "lib/active_analyses.rds")