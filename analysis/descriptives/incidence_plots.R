args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  file <- "input_pe_electively_unvaccinated.csv"
} else {
  file <- args[[1]]
}

# Load data --------------------------------------------------------------------

df <- readr::read_csv(paste0("output/",file))

# Plot exposure incidence plot -------------------------------------------------

df_exposure <- unique(df[df$expo==1,c("patient_id","expo_date")])
exposure <- incidence::incidence(df_exposure$expo_date, interval = 1)

jpeg(file=paste0("output/incidence_exposure-",gsub("\\..*","",file),".jpeg"))
plot(exposure)
dev.off()

# Plot outcome incidence plot --------------------------------------------------

df_outcome <- df[df$event==1,c("patient_id","event_date","expo")]
df_outcome$group <- ""
df_outcome$group <- ifelse(df_outcome$expo==0,"Unexposed",df_outcome$group)
df_outcome$group <- ifelse(df_outcome$expo==1,"Exposed",df_outcome$group)
outcome <- incidence::incidence(df_outcome$event_date, interval = 1, group = df_outcome$group)

jpeg(file=paste0("output/incidence_outcome-",gsub("\\..*","",file),".jpeg"))
plot(outcome)
dev.off()
