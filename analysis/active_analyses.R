library(tidyverse)

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

# ------------------------------------------------------------------------------
# Add diabetes outcomes --------------------------------------------------------
# ------------------------------------------------------------------------------

outcomes <- c("type 1 diabetes",
              "type 2 diabetes",
              "type 2 diabetes - pre diabetes",
              "type 2 diabetes - no pre diabetes",
              "type 2 diabetes - obesity",
              "type 2 diabetes - no obesity",
              "other or non-specific diabetes",
              "gestational diabetes")

outcome_group <- "diabetes"

outcomes_short <- c("t1dm","t2dm", "t2dm_pd","t2dm_pd_no", "t2dm_obes","t2dm_obes_no", "otherdm","gestationaldm")
outcome_venn <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

for (i in 1:length(outcomes)) {
  df[nrow(df)+1,] <- c(TRUE,
                       outcomes[i],
                       paste0("out_date_",outcomes_short[i]),
                       "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                       rep("all",2),
                       rep(TRUE,4),
                       rep(FALSE,14),
                       "",
                       "diabetes")
}

# change outcome group so that gestational diabetes has its own group

df <- df %>% mutate(outcome_group = case_when(outcome_variable == "out_date_gestationaldm" ~ "diabetes_gestational",
                                              TRUE ~ as.character(outcome_group)))

# change outcome group for pre diabetes and obesity analysis

df <- df %>% mutate(outcome_group = case_when(outcome == "type 2 diabetes - pre diabetes" ~ "diabetes_prediabetes",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - no pre diabetes" ~ "diabetes_no_prediabetes",
                                              TRUE ~ as.character(outcome_group)),
                    outcome_group = case_when(outcome == "type 2 diabetes - obesity" ~ "diabetes_obesity",
                                              TRUE ~ as.character(outcome_group)), 
                    outcome_group = case_when(outcome == "type 2 diabetes - no obesity" ~ "diabetes_no_obesity",
                                              TRUE ~ as.character(outcome_group)))

# turn on subgroups for main t2dm analyses

df[2,c(10:21)] <- TRUE

# turn on t2dm

# df[2,1] <- TRUE

# Remove sex as a covariate for gestational diabetes analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_gestationaldm" ~ "cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

# remove BMI for obesity subgroup analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_obes" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_obes_no" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_bin_prediabetes;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

# remove pre-diabetes for pre-diabetes subgroup analysis

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_pd" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

df <- df %>% mutate(covariates = case_when(outcome_variable == "out_date_t2dm_pd_no" ~ "cov_cat_sex;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_num_consulation_rate;cov_cat_smoking_status;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_healthcare_worker;cov_bin_carehome_status;cov_num_tc_hdl_ratio;cov_cat_bmi_groups;cov_bin_diabetes_gestational",
                                           TRUE ~ as.character(covariates)))

# add pre diabetes subgroup analysis
# 
# df$prior_history_var <- ifelse(df$outcome=="type 2 diabetes" ,"cov_bin_prediabetes",df$prior_history_var)
# df$prior_history_TRUE <- ifelse(df$outcome=="type 2 diabetes" ,TRUE,df$prior_history_TRUE)
# df$prior_history_FALSE <- ifelse(df$outcome=="type 2 diabetes" ,TRUE,df$prior_history_FALSE)
