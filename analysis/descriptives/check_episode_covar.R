args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  filename <- "input_pe_electively_unvaccinated.csv"
  covariates <- "ethnicity;region_name"
  output <- "pe-ethnicity_region"
} else {
  filename <- args[[1]]
  covariates <- args[[2]]
  output <- args[[3]]
}

# Separate covariates ----------------------------------------------------------
print("Separate covariates")

covariates <- stringr::str_split(as.vector(covariates), ";")[[1]]

# Load data --------------------------------------------------------------------
print("Load data")

df <- read.csv(file=paste0("output/",filename))

# Aggregate by provided covariates ---------------------------------------------
print("Aggregate by provided covariates")

df <- df[,c("patient_id","days_cat",covariates)]
df$N <- 1

f <- paste0("N ~ days_cat + ", paste0(covariates, collapse = " + "))
df <- aggregate(as.formula(f), data = df, FUN = sum)

# Save output ------------------------------------------------------------------
print("Save output")

write.csv(df[order(df$N),], paste0("output/",output,".csv"))