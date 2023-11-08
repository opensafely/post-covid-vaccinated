# Specify arguments ------------------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  outcome = "ate_extended_follow_up"
  subgroup = "covid_pheno_non_hospitalised"
  cohort = "pre_vaccination"
  day0 = FALSE
  extf = TRUE
  m1split = TRUE
} else {
  outcome <- args[[1]]
  subgroup <- args[[2]]
  cohort <- args[[3]]
  day0 <- args[[4]]
  extf <- args[[5]]
  m1split <- args[[6]]
}

# Load data --------------------------------------------------------------------
print('Load data')

df <- data.table::fread(paste0("output/input_sampled_data_",outcome,"_",subgroup,"_",cohort,"_reduced_time_periods.csv"))

# Save data --------------------------------------------------------------------
print('Save data')

data.table::fwrite(df, 
                   paste0("output/input_stata_",outcome,"_",subgroup,"_",cohort,"_day0",paste(day0),"_extf",paste(extf),"_m1split",paste(m1split),".csv.gz"),
                   compress = "gzip")