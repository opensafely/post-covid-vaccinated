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
                 outcome_group = character(),
                 stringsAsFactors = FALSE)

# Add cardiovascular outcomes (any position) -----------------------------------

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
  df[nrow(df)+1,] <- c(FALSE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       rep("all",2),
                       rep(FALSE,4),
                       rep(FALSE,14),
                       "",
                       "CVD")
}

# for local testing of one outcome (change everything else to FALSE)
# df[1,1] <- TRUE

outcomes <- c("Arterial thrombosis event",
              "Venous thrombosis event")

outcomes_short <- c("ate","vte")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(FALSE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       rep("all",2),
                       rep(FALSE,18),
                       "",
                       "CVD")
}

df$prior_history_var <- ifelse(df$outcome=="Arterial thrombosis event" ,"sub_bin_ate",df$prior_history_var)
df$prior_history_var <- ifelse(df$outcome=="Venous thrombosis event","cov_bin_vte",df$prior_history_var)

# Add cardiovascular outcomes (primary position) -----------------------------------

outcomes <- c("Acute myocardial infarction - Primary position events",
              "Ischaemic stroke - Primary position events",
              "Deep vein thrombosis - Primary position events",
              "Pulmonary embolism - Primary position events",
              "Transient ischaemic attack - Primary position events",
              "Subarachnoid haemorrhage and haemorrhagic stroke - Primary position events",
              "Heart failure - Primary position events",
              "Angina - Primary position events")

outcomes_short <- c("ami_primary_position","stroke_isch_primary_position","dvt_primary_position","pe_primary_position","tia_primary_position","stroke_sah_hs_primary_position","hf_primary_position","angina_primary_position")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(FALSE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       rep("all",2),
                       rep(FALSE,4),
                       rep(FALSE,14),
                       "",
                       "CVD")
}

# for local testing of one outcome (change everything else to FALSE)
# df[1,1] <- TRUE

outcomes <- c("Arterial thrombosis event - Primary position events",
              "Venous thrombosis event - Primary position events")

outcomes_short <- c("ate_primary_position","vte_primary_position")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(FALSE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                       rep("all",2),
                       rep(FALSE,18),
                       "",
                       "CVD")
}

df$prior_history_var <- ifelse(df$outcome=="Arterial thrombosis event - Primary position events","sub_bin_ate",df$prior_history_var)
df$prior_history_var <- ifelse(df$outcome=="Venous thrombosis event - Primary position events","cov_bin_vte",df$prior_history_var)

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
                       rep("",22),
                       "Diabetes")
}

# Add Mental Health outcomes --------------------------------------------------------

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

outcomes_short <- c("depression",
                    "anxiety_general",
                    "anxiety_ocd", 
                    "anxiety_ptsd", 
                    "eating_disorders", 
                    "serious_mental_illness",
                    "self_harm_10plus",
                    "self_harm_15plus",
                    "suicide",
                    "addiction")

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(TRUE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_num_age;cov_cat_sex;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_num_consulation_rate;cov_bin_healthcare_worker;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_ami;cov_bin_stroke_isch;cov_bin_recent_depression;cov_bin_history_depression;cov_bin_recent_anxiety;cov_bin_history_anxiety;cov_bin_recent_eating_disorders;cov_bin_history_eating_disorders;cov_bin_recent_serious_mental_illness;cov_bin_history_serious_mental_illness;cov_bin_recent_self_harm;cov_bin_history_self_harm",
                       rep("all",2),
                       rep(TRUE,1),
                       rep(FALSE,17),
                       "",
                       "Mental_health")
}

# Save active analyses list ----------------------------------------------------

saveRDS(df, file = "lib/active_analyses.rds")
