# Load libraries ---------------------------------------------------------------

library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)

# Specify command arguments ----------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort <- "vaccinated"
} else {
  cohort <- args[[1]]
}

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review"))

# Define other parameters ------------------------------------------------------

study_start <- "2021-06-01"

# Create spine dataset ---------------------------------------------------------

df <- arrow::read_feather(file = "output/input_index.feather",
                          col_select = c("patient_id",
                                         "cov_num_consulation_rate",
                                         "cov_bin_healthcare_worker",
                                         "qa_bin_prostate_cancer",
                                         "qa_bin_pregnancy",
                                         "qa_num_birth_year",
                                         "death_date"))

# Load data --------------------------------------------------------------------

tmp1 <- arrow::read_feather(file = "output/input_electively_unvaccinated.feather",
                            col_select = c("patient_id",
                                           "cov_cat_sex",
                                           "vax_date_eligible",
                                           "vax_cat_jcvi_group"))

tmp2 <- arrow::read_feather(file = "output/input_vaccinated.feather",
                            col_select = c("patient_id",
                                           "vax_date_Pfizer_1",
                                           "vax_date_Pfizer_2",
                                           "vax_date_Pfizer_3",
                                           "vax_date_AstraZeneca_1",
                                           "vax_date_AstraZeneca_2",
                                           "vax_date_AstraZeneca_3",
                                           "vax_date_Moderna_1",
                                           "vax_date_Moderna_2",
                                           "vax_date_Moderna_3"))

# Overwrite patient IDs for dummy data only ------------------------------------

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")) {
  tmp1$patient_id <- df$patient_id
  tmp2$patient_id <- df$patient_id
}

# Make single spine dataset ----------------------------------------------------

df <- merge(df,tmp1, by = "patient_id")
df <- merge(df,tmp2, by = "patient_id")
rm(tmp1,tmp2)

print("Spine dataset created successfully")

# Covert all dates to date format ----------------------------------------------

for (i in colnames(df)[grepl("_date",colnames(df))]) {
  df[,i] <- as.Date(df[,i])
}

# Overwrite vaccination information for dummy data only ------------------------

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")) {
  source("analysis/modify_dummy_vax_data.R")
}

# Identify all vaccinations for a given product --------------------------------

for (cat_product in c("AstraZeneca","Pfizer","Moderna")) {
  
  tmp <- df %>%
    dplyr::select(patient_id, tidyr::matches(paste0("vax\\_date\\_",cat_product,"\\_\\d+"))) %>%
    tidyr::pivot_longer(
      cols = -patient_id,
      values_to = "date_covid",
      values_drop_na = TRUE
    ) %>%
    dplyr::select(-name) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::mutate(vax_index = order(date_covid))
  
  colnames(tmp) <- c("patient_id","date_covid",paste0("vax_index_",cat_product))
  
  assign(paste0("tmp_",cat_product), tmp)
  
}

# Combine vaccinations for each product into a wide format table ---------------

tmp <- df %>% 
  dplyr::filter_at(dplyr::vars(tidyr::starts_with("vax_date")), dplyr::all_vars(is.na(.))) %>%
  dplyr::select(patient_id) %>% 
  dplyr::full_join(
    tmp_Pfizer %>%
      dplyr::full_join(tmp_AstraZeneca, by=c("patient_id", "date_covid")) %>%
      dplyr::full_join(tmp_Moderna, by=c("patient_id", "date_covid")),
    by = "patient_id"
  ) 

# Determine product at each vaccination date -----------------------------------

tmp$cat_product <- ""

tmp$cat_product <- ifelse(!is.na(tmp$vax_index_AstraZeneca) & 
                            is.na(tmp$vax_index_Pfizer) & 
                            is.na(tmp$vax_index_Moderna),
                          "AstraZeneca",tmp$cat_product)

tmp$cat_product <- ifelse(is.na(tmp$vax_index_AstraZeneca) & 
                            !is.na(tmp$vax_index_Pfizer) & 
                            is.na(tmp$vax_index_Moderna),
                          "Pfizer",tmp$cat_product)

tmp$cat_product <- ifelse(is.na(tmp$vax_index_AstraZeneca) & 
                            is.na(tmp$vax_index_Pfizer) & 
                            !is.na(tmp$vax_index_Moderna),
                          "Moderna",tmp$cat_product)

tmp$cat_product <- ifelse((!is.na(tmp$vax_index_AstraZeneca)) + 
                            (!is.na(tmp$vax_index_Pfizer)) + 
                            (!is.na(tmp$vax_index_Moderna)) > 1,
                          "duplicate",tmp$cat_product)

# Determine vaccination order --------------------------------------------------

tmp <- tmp %>%
  dplyr::arrange(patient_id, date_covid) %>%
  dplyr::group_by(patient_id) %>%
  dplyr::mutate(vax_index=dplyr::row_number()) %>%
  dplyr::ungroup()

# Make summary variables for vaccination dates and products --------------------

tmp <- tmp %>%
  tidyr::pivot_wider(
    id_cols= patient_id,
    names_from = c("vax_index"),
    values_from = c("date_covid", "cat_product"),
    names_glue = "vax_{.value}_{vax_index}"
  )

# Add summary variables to main data -------------------------------------------

df <- merge(df, tmp, by = "patient_id", all.x = TRUE)
rm(tmp_AstraZeneca, tmp_Moderna, tmp_Pfizer)

print("Vaccination information recorded successfully")

# Tidy dataset -----------------------------------------------------------------

df <- df[,c("patient_id","death_date",
            colnames(df)[grepl("qa_",colnames(df))],
            colnames(df)[grepl("vax_date_eligible",colnames(df))], # Vaccination eligibility
            colnames(df)[grepl("vax_date_covid_",colnames(df))], # Vaccination dates
            colnames(df)[grepl("vax_cat_",colnames(df))], # Vaccination products
            colnames(df)[grepl("cov_",colnames(df))])] # Covariates

# Determine patient index date -----------------------------------------------

df$study_start_date <- as.Date(study_start)

if(cohort=="vaccinated"){
  df$pat_start_date <- as.Date(df$vax_date_covid_2)+14
}

if(cohort=="electively_unvaccinated"){
  df$pat_start_date <- as.Date(df$vax_date_eligible)+84
}

df$index_source <- ifelse(df$study_start_date>df$pat_start_date ,"study_start_date","pat_start_date")

df$index_date <- as.Date(ifelse(df$study_start_date>df$pat_start_date ,df$study_start_date,df$pat_start_date), origin = "1970-01-01")# Convert dates to date format -------------------------------------------------

print("Index date and source determined successfully")

# Load covariate data ----------------------------------------------------------

tmp_index <- arrow::read_feather(file = "output/input_index.feather")
tmp_other <- arrow::read_feather(file = paste0("output/input_",cohort,".feather"))

# Describe data --------------------------------------------------------------

sink(paste0("output/not-for-review/describe_tmp_index_",cohort,".txt"))
print(Hmisc::describe(tmp_index))
sink()

sink(paste0("output/not-for-review/describe_tmp_",cohort,".txt"))
print(Hmisc::describe(tmp_other))
sink()

# Overwrite patient IDs for dummy data only ------------------------------------

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")) {
  tmp_index$patient_id <- df$patient_id
  tmp_other$patient_id <- df$patient_id
}

# Make and merge covariate dataset ---------------------------------------------

tmp_index <- tmp_index[tmp_index$patient_id %in% df[df$index_source=="study_start_date",]$patient_id,
                       intersect(colnames(tmp_index),colnames(tmp_other))]

tmp_other <- tmp_other[tmp_other$patient_id %in% df[df$index_source=="pat_start_date",]$patient_id,
                       intersect(colnames(tmp_index),colnames(tmp_other))]

tmp <- rbind(tmp_index, tmp_other)

df <- merge(df, tmp, by = "patient_id")
rm(tmp, tmp_index,tmp_other)

print("Non-spine variables added to dataset successfully")

# Convert dates to date format -------------------------------------------------

df <- df %>%
  dplyr::rename(tmp_out_max_hba1c_mmol_mol_date = tmp_out_num_max_hba1c_date)

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

print("Variable formats updated successfully")

# Define COVID-19 severity ---------------------------------------------------

df$sub_cat_covid19_hospital <- "no_infection"

df$sub_cat_covid19_hospital <- ifelse(!is.na(df$exp_date_covid19_confirmed),
                                      "non_hospitalised",df$sub_cat_covid19_hospital)

df$sub_cat_covid19_hospital <- ifelse(!is.na(df$exp_date_covid19_confirmed) & 
                                        !is.na(df$sub_date_covid19_hospital) &
                                        (df$sub_date_covid19_hospital-df$exp_date_covid19_confirmed>=0 &
                                           df$sub_date_covid19_hospital-df$exp_date_covid19_confirmed<29),
                                      "hospitalised",df$sub_cat_covid19_hospital)

df$sub_cat_covid19_hospital <- as.factor(df$sub_cat_covid19_hospital)
df[,c("sub_date_covid19_hospital")] <- NULL
df <- df[!is.na(df$patient_id),]

print("COVID19 severity determined successfully")

# Create vars -------------------------------------------------------------

# vars could not be created in common vars file
df <- df %>% mutate(tmp_out_count_t2dm = tmp_out_count_t2dm_snomed + tmp_out_count_t2dm_hes,
                    tmp_out_count_t1dm = tmp_out_count_t1dm_snomed + tmp_out_count_t1dm_hes) %>%
  # cholesterol ratio              
  mutate(cov_num_tc_hdl_ratio = tmp_cov_num_cholesterol / tmp_cov_num_hdl_cholesterol) %>%
  # remove bmi date var
  dplyr::select(- cov_num_bmi_date_measured)

print("Diabetes count variables created successfully")

# define variables needed for diabetes algorithm 

df <- df %>% 
  mutate(tmp_out_year_first_diabetes_diag = format(tmp_out_date_first_diabetes_diag,"%Y")) %>%
  mutate(tmp_out_year_first_diabetes_diag = as.integer(tmp_out_year_first_diabetes_diag),
         age_1st_diag = tmp_out_year_first_diabetes_diag - qa_num_birth_year) %>%
  mutate(age_1st_diag = replace(age_1st_diag, which(age_1st_diag < 0), NA)) %>% # assign negative ages to NA)
  mutate(age_under_35_30_1st_diag = ifelse(!is.na(age_1st_diag) &
                                             (age_1st_diag < 35 & 
                                                (cov_cat_ethnicity == 1 | cov_cat_ethnicity == 2  | cov_cat_ethnicity == 5)) | 
                                             (age_1st_diag < 30), "Yes", "No")) %>%
  # HBA1C date var - earliest date for only those with >=47.5
  mutate(hba1c_date_step7 = as_date(case_when(tmp_out_num_max_hba1c_mmol_mol >= 47.5 ~ pmin(tmp_out_max_hba1c_mmol_mol_date, na.rm = TRUE))),
         # process codes - this is taking the first process code date in those individuals that have 5 or more process codes
         over5_pocc_step7 = as_date(case_when(tmp_out_count_poccdm_snomed >= 5 ~ pmin(out_date_poccdm, na.rm = TRUE))))

print("COVID-19 and diabetes variables needed for algorithm created successfully")

# Define diabetes outcome (using Sophie Eastwood algorithm) ----------------------------

scripts_dir <- "analysis/preprocess"
source(file.path(scripts_dir,"diabetes_algorithm.R"))
df <- diabetes_algo(df)
print("Diabetes algorithm run successfully")

# Restrict columns and save analysis dataset ---------------------------------

df1 <- df[,c("patient_id","death_date","index_date",
             colnames(df)[grepl("sub_",colnames(df))], # Subgroups
             colnames(df)[grepl("exp_",colnames(df))], # Exposures
             colnames(df)[grepl("out_",colnames(df))], # Outcomes
             colnames(df)[grepl("cov_",colnames(df))], # Covariates
             colnames(df)[grepl("qa_",colnames(df))], # Quality assurance
             colnames(df)[grepl("step",colnames(df))], # diabetes steps
             colnames(df)[grepl("vax_date_eligible",colnames(df))], # Vaccination eligibility
             colnames(df)[grepl("vax_date_covid_",colnames(df))], # Vaccination dates
             colnames(df)[grepl("vax_cat_",colnames(df))])] # Vaccination products

df1[,colnames(df)[grepl("tmp_",colnames(df))]] <- NULL

saveRDS(df1, file = paste0("output/input_",cohort,".rds"))

print("Input data saved successfully")

# Describe data --------------------------------------------------------------

sink(paste0("output/not-for-review/describe_input_",cohort,"_stage0.txt"))
print(Hmisc::describe(df1))
sink()

# Restrict columns and save Venn diagram input dataset -----------------------

#df2 <- df[,c("patient_id",colnames(df)[grepl("out_",colnames(df))])]

df2 <- df %>% select(starts_with(c("patient_id","tmp_out_date","out_date")))

# Describe data --------------------------------------------------------------

sink(paste0("output/not-for-review/describe_venn_",cohort,".txt"))
print(Hmisc::describe(df2))
sink()

saveRDS(df2, file = paste0("output/venn_",cohort,".rds"))

print("Venn diagram data saved successfully")