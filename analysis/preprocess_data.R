# Load libraries ---------------------------------------------------------------

library(magrittr)

# Define parameters ------------------------------------------------------------

study_start <- "2021-06-01"

# Create spine dataset ---------------------------------------------------------

df <- arrow::read_feather(file = "output/input_index.feather",
                          col_select = c("patient_id","death_date"))

# Merge each dataset to the spine dataset --------------------------------------

for (i in c("index","vaccinated","electively_unvaccinated")) {

  ## Load dataset
  
  tmp <- arrow::read_feather(file = paste0("output/input_",i,".feather"))
  
   # Describe data --------------------------------------------------------------
  
  sink(paste0("output/describe_input_",i,"_studydefinition.txt"))
  print(Hmisc::describe(tmp))
  sink()

  # Overwrite patient IDs for dummy data only ----------------------------------
  
  if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")) {
    tmp$patient_id <- df$patient_id
  }
  
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

# Remove vax_date_covid_* variables --------------------------------------------
# NB: These will be removed from future study definitions

df[,c("vax_date_covid_1","vax_date_covid_2","vax_date_covid_3")] <- NULL

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

# Split into "vaccinated" and "electively_unvaccinated" cohorts ----------------

for (j in c("vaccinated","electively_unvaccinated")) {
  
  # Make temporary data frame --------------------------------------------------
  
  tmp <- df 
  
  # Remove dynamic variables from 'other' dataset --------------------------------
  
  tmp[,colnames(tmp)[grepl(paste0("_",ifelse(j=="vaccinated","electively_unvaccinated","vaccinated")),colnames(tmp))]] <- NULL
  
  # Determine patient index date -----------------------------------------------
  
  tmp$study_start_date <- as.Date(study_start)
  
  tmp$pat_start_date <- ifelse(j=="vaccinated",
                                as.Date(tmp$vax_date_covid_2)+14,
                                as.Date(tmp$vax_date_eligible)+84)
  
  tmp$use_date <- ifelse(tmp$study_start_date>tmp$pat_start_date ,"index","nonindex")
  
  # Identify variables for those using study start as index --------------------
  
  index <- tmp[tmp$use_date=="index",]
  index[,grepl("_vaccinated",colnames(index))] <- NULL
  colnames(index) <- gsub("_index","",colnames(index))
  index$index_date <- index$study_start_date
  index[,c("use_date")] <- NULL
  
  # Identify variables for those not using study start as index ----------------
  
  nonindex <- tmp[tmp$use_date=="nonindex",]
  nonindex[,grepl("_index",colnames(nonindex))] <- NULL
  colnames(nonindex) <- gsub("_vaccinated","",colnames(nonindex))
  nonindex$index_date <- nonindex$pat_start_date 
  nonindex[,c("use_date")] <- NULL
  
  # Combine index and nonindex back into a single dataset ----------------------
  
  tmp <- rbind(index,nonindex)
  
  # Define COVID-19 severity ---------------------------------------------------
  
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
              colnames(tmp)[grepl("vax_date_eligible",colnames(tmp))], # Vaccination eligbility
              colnames(tmp)[grepl("vax_date_covid_",colnames(tmp))], # Vaccination dates
              colnames(tmp)[grepl("vax_cat_",colnames(tmp))])] # Vaccination products and JCVI groupings
  
  tmp1[,colnames(tmp)[grepl("tmp_out_",colnames(tmp))]] <- NULL
  
  saveRDS(tmp1, file = paste0("output/input_",j,".rds"))
  
  # Describe data --------------------------------------------------------------
  
  sink(paste0("output/describe_input_",j,"_stage0.txt"))
  print(Hmisc::describe(tmp1))
  sink()
  
  # Restrict columns and save Venn diagram input dataset -----------------------
  
  tmp2 <- tmp[,c("patient_id",colnames(tmp)[grepl("out_",colnames(tmp))])]
  
  saveRDS(tmp2, file = paste0("output/venn_",j,".rds"))
  
}