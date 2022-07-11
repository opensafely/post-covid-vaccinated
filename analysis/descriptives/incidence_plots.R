args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  filename <- "input_pe_electively_unvaccinated.csv"
} else {
  filename <- args[[1]]
}

# Load data --------------------------------------------------------------------
print("Load data")

df <- read.csv(file=paste0("output/",filename))

# Describe data ----------------------------------------------------------------
print("Describe data")

sink(paste0("output/not-for-review/describe_incidence_",gsub("\\..*","",filename),".txt"))
print(Hmisc::describe(df))
sink()

# Prepare exposure incidence plot -------------------------------------------------
print("Prepare exposure incidence plot")

df_exposure <- unique(df[df$expo==1 & !is.na(df$expo_date),c("patient_id","expo_date")])

# Describe exposure incidence data ---------------------------------------------
print("Describe exposure incidence data")

sink(paste0("output/not-for-review/describe_incidence_exposure_",gsub("\\..*","",filename),".txt"))
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

sink(paste0("output/not-for-review/describe_incidence_outcome_",gsub("\\..*","",filename),".txt"))
print(Hmisc::describe(df_outcome))
sink()

# Plot outcome incidence plot --------------------------------------------------
print("Plot outcome incidence plot")

outcome <- incidence::incidence(df_outcome$event_date, interval = 1, group = df_outcome$group)

jpeg(file=paste0("output/incidence_outcome-",gsub("\\..*","",filename),".jpeg"))
plot(outcome)
dev.off()

# Plot cumulative outcome incidence by days since exposure ---------------------
print("Plot cumulative outcome incidence by days since exposure")

df_cumulative <- df[df$expo==1 & df$event==1,
                    c("patient_id","expo_date","event_date")]

df_cumulative$days_since_exposure <- as.numeric(as.Date(df_cumulative$event_date) - as.Date(df_cumulative$expo_date))
df_cumulative <- df_cumulative[df_cumulative$days_since_exposure>=0,]

df_cumulative <- as.data.frame(table(df_cumulative$days_since_exposure))
df_cumulative <- dplyr::rename(df_cumulative, "days_since_exposure" = "Var1")

df_plot_cumulative <- data.frame(days_since_exposure = 0:196,
                                 stringsAsFactors = FALSE)

df_plot_cumulative <- merge(df_plot_cumulative, df_cumulative, by = "days_since_exposure", all.x = TRUE)

df_plot_cumulative$Freq <- ifelse(is.na(df_plot_cumulative$Freq), 0, df_plot_cumulative$Freq)
df_plot_cumulative$days_since_exposure <- as.numeric(df_plot_cumulative$days_since_exposure)
df_plot_cumulative$cumulative_freq <- cumsum(df_plot_cumulative$Freq)

ggplot2::ggplot(data = df_plot_cumulative, mapping = ggplot2::aes(x = days_since_exposure, y = cumulative_freq)) +
  ggplot2::geom_path() +
  ggplot2::scale_x_continuous(lim = c(0,196), breaks = seq(0,196,7), labels = seq(0,196,7)/7) +
  ggplot2::labs(x = "Weeks since COVID-19 diagnosis", y = "Cumulative frequency of outcome events") +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank())
  
ggplot2::ggsave(filename = paste0("output/cumulative_incidence_outcome-",gsub("\\..*","",filename),".jpeg"),
                height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)