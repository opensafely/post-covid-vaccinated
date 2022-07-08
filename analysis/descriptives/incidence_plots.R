args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  filename <- "input_pe_electively_unvaccinated.csv"
} else {
  filename <- args[[1]]
}

# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(file = paste0("output/",filename))

# Describe data ----------------------------------------------------------------
print("Describe data")

sink(paste0("output/not-for-review/describe_incidence_",filename,".txt"))
print(Hmisc::describe(df))
sink()

# Prepare exposure incidence plot -------------------------------------------------
print("Prepare exposure incidence plot")

df_exposure <- unique(df[df$expo==1 & !is.na(df$expo_date),c("patient_id","expo_date")])

# Describe exposure incidence data ---------------------------------------------
print("Describe exposure incidence data")

sink(paste0("output/not-for-review/describe_incidence_exposure_",filename,".txt"))
print(Hmisc::describe(df_exposure))
sink()

# Plot exposure incidence plot -------------------------------------------------
print("Plot exposure incidence data")

exposure <- incidence::incidence(df_exposure$expo_date, interval = 1)

jpeg(file=paste0("output/incidence_exposure-",gsub("\\..*","",filename),".jpeg"))
plot(exposure)
dev.off()

# Prepare outcome incidence plot -----------------------------------------------
print("Prepare outcome incidence plot")

df_outcome <- df[df$event==1 & !is.na(df$event_date),c("patient_id","event_date","expo")]
df_outcome$group <- ""
df_outcome$group <- ifelse(df_outcome$expo==0,"Unexposed",df_outcome$group)
df_outcome$group <- ifelse(df_outcome$expo==1,"Exposed",df_outcome$group)

# Describe outcome incidence data ---------------------------------------------
print("Describe outcome incidence data")

sink(paste0("output/not-for-review/describe_incidence_outcome_",filename,".txt"))
print(Hmisc::describe(df_outcome))
sink()

# Plot outcome incidence plot --------------------------------------------------
print("Plot outcome incidence plot")

outcome <- incidence::incidence(df_outcome$event_date, interval = 1, group = df_outcome$group)

jpeg(file=paste0("output/incidence_outcome-",gsub("\\..*","",filename),".jpeg"))
plot(outcome)
dev.off()
