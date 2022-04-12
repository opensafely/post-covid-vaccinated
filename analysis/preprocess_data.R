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

# Define other parameters ------------------------------------------------------

study_start <- "2021-06-01"

# Load libraries ---------------------------------------------------------------

library(magrittr)

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

sink(paste0("output/describe_tmp_index_",cohort,".txt"))
print(Hmisc::describe(tmp_index))
sink()

sink(paste0("output/describe_tmp_",cohort,".txt"))
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

# tmp <- tmp[,!grepl("tmp_exp_",colnames(tmp))]
# tmp <- tmp[,!grepl("tmp_sub_",colnames(tmp))]
# tmp <- tmp[,!grepl("tmp_cov_",colnames(tmp))]

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
  mutate(cov_num_tc_hdl_ratio = tmp_cov_num_cholesterol / tmp_cov_num_hdl_cholesterol)

print("Diabetes count variables created successfully")

########################################################################################
# Define diabetes outcome (using Sophie Eastwood algorithm) ----------------------------
########################################################################################

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

# Diabetes adjudication algorithm

df <- df %>% 
  
  # Step 1. Any gestational diabetes code? 
  mutate(step_1 = ifelse(!is.na(out_date_gestationaldm), "Yes", "No")) %>% 
  
  # Step 1a. Any T1/ T2 diagnostic codes present? Denominator for step 1a is those with yes to step 1
  mutate(step_1a = ifelse(step_1 == "Yes" &
                            (!is.na(out_date_t1dm) | !is.na(out_date_t2dm)), "Yes",
                          ifelse(step_1 == "Yes" &                 
                                   is.na(out_date_t1dm) &          
                                   is.na(out_date_t2dm), "No", NA))) %>%
  
  # Step 2. Non-metformin antidiabetic denominator for step 2: no to step 1 or yes to step 1a
  mutate(step_2 = ifelse((step_1 == "No" | step_1a == "Yes" ) & 
                           !is.na(tmp_out_date_nonmetform_drugs_snomed), "Yes",
                         ifelse((step_1 == "No" | step_1a == "Yes") & 
                                  is.na(tmp_out_date_nonmetform_drugs_snomed), "No", NA))) %>%
  
  # Step 3. Type 1 code in the absence of type 2 code? denominator for step 3: no to step 2
  mutate(step_3 = ifelse(step_2=="No" &                   
                           !is.na(out_date_t1dm) &          
                           is.na(out_date_t2dm), "Yes", 
                         ifelse(step_2 == "No", "No", NA))) %>%
  
  # Step 4. Type 2 code in the absence of type 1 code denominator for step 3: no to step 3
  mutate(step_4 = ifelse(step_3 == "No" &                  
                           is.na(out_date_t1dm) &        
                           !is.na(out_date_t2dm), "Yes",
                         ifelse(step_3 == "No", "No", NA))) %>%
  
  # Step 5. Aged <35yrs (or <30 yrs for SAs and AFCS) at first diagnostic code? denominator for step 5: no to step 4
  mutate(step_5 = ifelse(step_4 == "No" &                          
                           age_under_35_30_1st_diag == "Yes", "Yes",
                         ifelse(step_4 == "No" &                            
                                  age_under_35_30_1st_diag == "No", "No", NA))) %>%
  mutate(step_5 = ifelse(step_5 == "No" |
                           is.na(step_5) & step_4 == "No", "No", "Yes")) %>%
  
  # Step 6. Type 1 and type 2 codes present? denominator for step 6: no to step 5
  mutate(step_6 = ifelse(step_5 == "No" &                            
                           !is.na(out_date_t1dm) &                  
                           !is.na(out_date_t2dm), "Yes", 
                         ifelse(step_5 == "No" &                           
                                  (is.na(out_date_t1dm) |                  
                                     is.na(out_date_t2dm)), "No", NA))) %>%
  
  # Step 6a. Type 1 only reported in primary care. denominator for step 6: no to step 6
  mutate(step_6a = ifelse(step_6 == "Yes" &                             
                            !is.na(tmp_out_date_t1dm_snomed) &        
                            is.na(tmp_out_date_t2dm_snomed), "Yes",
                          ifelse(step_6 == "Yes", "No", NA))) %>%
  
  # Step 6b. Type 2 only reported in primary care. denominator for step 6: no to step 6
  mutate(step_6b = ifelse(step_6a == "No" &                             
                            is.na(tmp_out_date_t1dm_snomed) &       
                            !is.na(tmp_out_date_t2dm_snomed), "Yes",
                          ifelse(step_6a == "No", "No", NA))) %>%
  
  # Step 6c. Number of type 1 codes>number of type 2 codes? denominator for step 6c: no to step 6b
  mutate(step_6c = ifelse(step_6b == "No" &
                            tmp_out_count_t1dm > tmp_out_count_t2dm, "Yes",
                          ifelse(step_6b == "No" &
                                   tmp_out_count_t1dm <= tmp_out_count_t2dm, "No", NA))) %>%
  # Step 6d. Number of type 2 codes>number of type 1 codes denominator for step 6d: no to step 6c
  mutate(step_6d = ifelse(step_6c == "No" &
                            tmp_out_count_t2dm > tmp_out_count_t1dm, "Yes",
                          ifelse(step_6c == "No" &
                                   tmp_out_count_t2dm <= tmp_out_count_t1dm, "No", NA))) %>%
  
  # Step 6e. Type 2 code most recent? denominator for step 6e: no to step 6d
  mutate(step_6e = ifelse(step_6d == "No" &                        
                            out_date_t2dm > out_date_t1dm, "Yes",
                          ifelse(step_6d == "No" &                         
                                   out_date_t2dm < out_date_t1dm, "No", NA))) %>%
  
  # Step 7. Diabetes medication or >5 process of care codes or HbA1c>=6.5? denominator for step 7: no to step 6
  mutate(step_7 = ifelse(step_6 == "No" &                          
                           ((!is.na(tmp_out_date_diabetes_medication)) |    
                              (tmp_out_num_max_hba1c_mmol_mol >= 47.5) |
                              (tmp_out_count_poccdm_snomed >= 5)), "Yes",
                         ifelse(step_6=="No" , "No", NA))) %>%
  
  # Create Diabetes Variable
  mutate(out_cat_diabetes = ifelse(step_1 == "No" & step_2 == "No" & step_3 == "No" & step_4 == "No" &
                                     step_5 == "No" & step_6 == "No" & step_7 == "No" |
                                     step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 == "No" & step_4 == "No" &
                                     step_5 == "No" & step_6 == "No" & step_7 == "No" , 
                                   "DM unlikely",
                                   ifelse(step_1 == "No" & step_2 == "No" & step_3 == "No" & step_4 == "No" &
                                            step_5 == "No" & step_6 == "No" & step_7 == "Yes" |
                                            step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 == "No" & step_4 == "No" &
                                            step_5 == "No" & step_6 == "No" & step_7 == "Yes", 
                                          "DM_other",
                                          ifelse(step_1 == "No" & step_2 == "Yes" |
                                                   step_1 == "Yes" & step_1a == "Yes" & step_2 == "Yes" |
                                                   step_1 == "No" & step_2 == "No" & step_3 == "No" & step_4 == "Yes" |
                                                   step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 == "No" & step_4 == "Yes" |
                                                   step_1 == "No" & step_2 == "No" & step_3 == "No" & step_4 == "No" & 
                                                   step_5 == "No" & step_6 == "Yes" & step_6a == "No" & step_6b=="Yes" |
                                                   step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 == "No" & step_4 == "No" & 
                                                   step_5 == "No" & step_6 == "Yes" & step_6a == "No" & step_6b=="Yes" |
                                                   step_1 == "No" & step_2 == "No" & step_3 == "No" & step_4 == "No" & 
                                                   step_5 == "No" & step_6 == "Yes" & step_6a == "No" & step_6b=="No" &
                                                   step_6c == "No" & step_6d == "Yes" |
                                                   step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 == "No" & step_4 == "No" & 
                                                   step_5 == "No" & step_6 == "Yes" & step_6a == "No" & step_6b=="No" &
                                                   step_6c == "No" & step_6d == "Yes" |  
                                                   step_1 == "No" & step_2 == "No" & step_3 == "No" & step_4 == "No" & 
                                                   step_5 == "No" & step_6 == "Yes" & step_6a == "No" & step_6b=="No" &
                                                   step_6c == "No" & step_6d == "No" & step_6e == "Yes" |
                                                   step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 == "No" & step_4 == "No" & 
                                                   step_5 == "No" & step_6 == "Yes" & step_6a == "No" & step_6b=="No" &
                                                   step_6c == "No" & step_6d == "No" & step_6e == "Yes", 
                                                 "T2DM",
                                                 ifelse(step_1 == "No" & step_2 == "No" & step_3=="Yes" |
                                                          step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3=="Yes" |
                                                          step_1 == "No" & step_2 == "No" & step_3 =="No" & step_4 == "No" & step_5 == "Yes" |
                                                          step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 =="No" & step_4 == "No" &
                                                          step_5 == "Yes" | 
                                                          step_1 == "No" & step_2 == "No" & step_3 =="No" & step_4 == "No" & step_5 == "No" &
                                                          step_6 == "Yes" & step_6a == "Yes" |
                                                          step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 =="No" & step_4 == "No" &
                                                          step_5 == "No" &
                                                          step_6 == "Yes" & step_6a == "Yes" |  
                                                          step_1 == "No" & step_2 == "No" & step_3 =="No" & step_4 == "No" & step_5 == "No" &
                                                          step_6 == "Yes" & step_6a == "No" & step_6b == "No" & step_6c == "Yes" |
                                                          step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 =="No" & step_4 == "No" &
                                                          step_5 == "No" &
                                                          step_6 == "Yes" & step_6a == "No" & step_6b == "No" & step_6c == "Yes" |  
                                                          step_1 == "No" & step_2 == "No" & step_3 =="No" & step_4 == "No" & step_5 == "No" &
                                                          step_6 == "Yes" & step_6a == "No" & step_6b == "No" & step_6c == "No" &
                                                          step_6d == "No" & step_6e == "No" |
                                                          step_1 == "Yes" & step_1a == "Yes" & step_2 == "No" & step_3 =="No" & step_4 == "No" & step_5 == "No" &
                                                          step_6 == "Yes" & step_6a == "No" & step_6b == "No" & step_6c == "No" &
                                                          step_6d == "No" & step_6e == "No",
                                                        "T1DM",
                                                        ifelse(step_1 == "Yes" & step_1a == "No", "GDM", NA)))))) %>%
  # replace NAs with None (no diabetes)
  mutate_at(vars(out_cat_diabetes), ~replace_na(., "None"))

print("Diabetes algorithm run successfully")

# Define incident diabetes date variables needed for cox analysis -------------------------
# Uses diabetes cateogory from algorithm above and date of first diabetes related code. 

df <- df %>%
  # remove old diabetes variables to avoid duplication / confusion - commented out for now 
  # dplyr::select(- out_date_t1dm, - out_date_t2dm, - out_date_otherdm, - out_date_gestationaldm) %>% 
  # GESTATIONAL
  mutate(out_date_gestationaldm = as_date(case_when(out_cat_diabetes == "GDM" ~ tmp_out_date_first_diabetes_diag)),
         # T2DM
         out_date_t2dm = as_date(case_when(out_cat_diabetes == "T2DM" ~ tmp_out_date_first_diabetes_diag)),
         # T1DM
         out_date_t1dm = as_date(case_when(out_cat_diabetes == "T1DM" ~ tmp_out_date_first_diabetes_diag)),
         # OTHER
         out_date_otherdm = as_date(case_when(out_cat_diabetes == "DM_other" ~ pmin(hba1c_date_step7, over5_pocc_step7, na.rm = TRUE))))

print("Diabetes date variables using algorithm created successfully")

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

df1[,colnames(df)[grepl("tmp_out_",colnames(df))]] <- NULL

saveRDS(df1, file = paste0("output/input_",cohort,".rds"))

print("Input data saved successfully")

# Describe data --------------------------------------------------------------

sink(paste0("output/describe_input_",cohort,"_stage0.txt"))
print(Hmisc::describe(df1))
sink()

# Restrict columns and save Venn diagram input dataset -----------------------

#df2 <- df[,c("patient_id",colnames(df)[grepl("out_",colnames(df))])]

df2 <- df %>% select(starts_with(c("patient_id","tmp_out_date","out_date")))

# Describe data --------------------------------------------------------------
sink(paste0("output/describe_venn_",cohort,".txt"))
print(Hmisc::describe(df2))
sink()

saveRDS(df2, file = paste0("output/venn_",cohort,".rds"))

print("Venn diagram data saved successfully")