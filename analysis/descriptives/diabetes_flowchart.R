## =============================================================================
## Project:     Post covid vaccinated project
##
## Purpose:  Build flowchart to illustrate the flow of diabetes diagnoses using the algorithm
## 
## Authors: Kurt Taylor
## Reviewer: 
## 
## Content: 
## 0. Load relevant libraries and read data/arguments
## 1. Construct flowchart
## 2. Save output as PNG: "diabetes_flow.png" (save output as CSV in OpenSafely and then run rest of script outside of OS environment)
## =============================================================================

###############################################
# 0. Load relevant libraries and read in data #
###############################################

# Libraries

libraries <- c("readr", "dplyr", "stringr", "tidyverse", "plyr")
# libraries <- c("readr", "dplyr", "stringr", "tidyverse", "DiagrammeR", "DiagrammeRsvg", "rsvg", "plyr")
lapply(libraries, require, character.only=T)

# command args 

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort_name <- "vaccinated"
  #cohort_name = "electively_unvaccinated"
}else{
  cohort_name <- args[[1]]
}

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "figure-data"))

diabetes_flow_function <- function(cohort_name, group) {
  # Load Stage 1 dataset
  
  diabetes_df <-read_rds(paste0("output/input_",cohort_name,"_stage1_",group,".rds"))
  
  ###############################################
  # 1. Construct flowchart -------------------- #
  ###############################################
  
  # Define boxes for flow chart within a list of values using algorithm steps created in preprocess_data.R.
  
  values <- list(
    # Diabetes diagnostic code / medication / care process code plus ethnicity code in primary or secondary care
    a = nrow(diabetes_df),
    # Step 1 any gestational diabetes code
    b = sum(diabetes_df$step_1 == "Yes", na.rm = T),
    b2 = sum(diabetes_df$step_1 == "No", na.rm = T),
    # Step 1a Any type 1 / type 2 code
    c = sum(diabetes_df$step_1a == "Yes", na.rm = T),
    c2 = sum(diabetes_df$step_1a == "No", na.rm = T),
    # GESTATIONAL DIABETES
    d = sum(diabetes_df$out_cat_diabetes == "GDM", na.rm = T),
    # Step 2 non-metformin oral antidiabetic 
    e = sum(diabetes_df$step_2 == "Yes", na.rm = T),
    e2 = sum(diabetes_df$step_2 == "No", na.rm = T),
    # TYPE 2 DIABETES
    f = sum(diabetes_df$out_cat_diabetes == "T2DM", na.rm = T),
    # Step 3 Type 1 code but no Type 2 code
    g = sum(diabetes_df$step_3 == "Yes", na.rm = T),
    g2 = sum(diabetes_df$step_3 == "No", na.rm = T),
    # TYPE 1 DIABETES 
    h = sum(diabetes_df$out_cat_diabetes == "T1DM", na.rm = T),
    # Step 4 Type 2 code and no type 1 code
    i = sum(diabetes_df$step_4 == "Yes", na.rm = T),
    i2 = sum(diabetes_df$step_4 == "No", na.rm = T),
    # Step 5 Aged <35 yrs (30 yrs for South Asians and African descent) at first diagnostic code)
    j = sum(diabetes_df$step_5 == "Yes", na.rm = T),
    j2 = sum(diabetes_df$step_5 == "No", na.rm = T),
    # Step 6 Type 1 and Type 2 codes present
    k = sum(diabetes_df$step_6 == "Yes", na.rm = T),
    k2 = sum(diabetes_df$step_6 == "No", na.rm = T),
    # Step 6a Type 1 only recorded in primary care
    l = sum(diabetes_df$step_6a == "Yes", na.rm = T),
    l2 = sum(diabetes_df$step_6a == "No", na.rm = T),
    # Step 6b Type 2 only recorded in primary care
    m = sum(diabetes_df$step_6b == "Yes", na.rm = T),
    m2 = sum(diabetes_df$step_6b == "No", na.rm = T),
    # Step 6c N Type 1 > N Type 2 codes
    n = sum(diabetes_df$step_6c == "Yes", na.rm = T),
    n2 = sum(diabetes_df$step_6c == "No", na.rm = T),
    # Step 6d N Type 2 > N Type 1 codes
    o = sum(diabetes_df$step_6d == "Yes", na.rm = T),
    o2 = sum(diabetes_df$step_6d == "No", na.rm = T),
    # Step 6e Type 2 code most recent
    p = sum(diabetes_df$step_6e == "Yes", na.rm = T),
    p2 = sum(diabetes_df$step_6e == "No", na.rm = T),
    # Step 7 Diabetes medication OR >= 5 care process codes OR HBA1c >= 47.5mmol)
    q = sum(diabetes_df$step_7 == "Yes", na.rm = T),
    q2 = sum(diabetes_df$step_7 == "No", na.rm = T),
    # DIABETES UNSPECIFIED TYPE
    r = sum(diabetes_df$out_cat_diabetes == "DM_other", na.rm = T),
    # DIABETES UNLIKELY
    s = sum(diabetes_df$out_cat_diabetes == "DM unlikely", na.rm = T))
  
  # REDACT <=5 TO NA --------------------------------------------------------------
  
  values <- lapply(values, function(x) replace(x, x <= 5, NA))
  
  # EXPORT RAW DATA FOR FLOWCHART -------------------------------------------
  # exporting data so that flow chart can easily be produced/tweaked outside of L4. 
  
  values_df <- ldply(values, data.frame) # convert list to df
  values_df_t <- data.table::transpose(values_df) # transpose df
  names(values_df_t) <- lapply(values_df_t[1, ], as.character) # make row 1 the column names
  values_df_t <- values_df_t[-1, ] 
  write.csv(values_df_t, file = paste0("output/review/figure-data/diabetes_flow_values_",cohort_name,"_",group,".csv")) # save
  
}

# Run function using specified commandArgs and active analyses for group

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active==TRUE)
group <- unique(active_analyses$outcome_group)


for(i in group){
  if (cohort_name == "both") {
    diabetes_flow_function("electively_unvaccinated", i)
    diabetes_flow_function("vaccinated", i)
  } else{
    diabetes_flow_function(cohort_name, i)
  }
}

# END