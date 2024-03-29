args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  filename <- "input_pe_electively_unvaccinated.csv"
} else {
  filename <- args[[1]]
}

# Load data --------------------------------------------------------------------
print("Load data")

df <- read.csv(file=paste0("output/",filename))

# Restrict data to individuals with exposure and outcome -----------------------
print("Restrict data to individuals with exposure and outcome")

df <- df[df$expo==1 & df$event==1,
                    c("patient_id","expo_date","event_date")]

# Calculate days since exposure ------------------------------------------------
print("Calculate days since exposure")

df$days_since_exposure <- as.numeric(as.Date(df$event_date) - as.Date(df$expo_date))

# Restrict data to individuals with outcome after exposure ----------------------
print("Restrict data to individuals with outcome after exposure")

df <- df[df$days_since_exposure>=0,]

# Calculate events on each day since exposure ----------------------------------
print("Calculate events on each day since exposure")

df <- as.data.frame(table(df$days_since_exposure))
df <- dplyr::rename(df, "days_since_exposure" = "Var1", "events" = "Freq")

# Make table with all possible days since epxosure -----------------------------
print("Make table with all possible days since epxosure")

df_plot <- data.frame(days_since_exposure = 0:196, stringsAsFactors = FALSE)
df_plot <- merge(df_plot, df, by = "days_since_exposure", all.x = TRUE)
df_plot$events <- ifelse(is.na(df_plot$events), 0, df_plot$events)

# Calculate cumulative events --------------------------------------------------
print("Calculate cumulative events")

df_plot$days_since_exposure <- as.numeric(df_plot$days_since_exposure)
df_plot$cumulative_events <- cumsum(df_plot$events)

# Save data --------------------------------------------------------------------
print("Save data")

write.csv(df_plot, paste0("output/cumulative-",gsub("\\..*","",filename),".csv"))

# Make plot --------------------------------------------------------------------
print("Make plot")

ggplot2::ggplot(data = df_plot, mapping = ggplot2::aes(x = days_since_exposure, y = cumulative_events)) +
  ggplot2::geom_path() +
  ggplot2::scale_x_continuous(lim = c(0,196), breaks = seq(0,196,7), labels = seq(0,196,7)/7) +
  ggplot2::scale_y_continuous(lim = c(0,max(df_plot$cumulative_events))) + 
  ggplot2::labs(x = "Weeks since COVID-19 diagnosis", y = "Cumulative eventsuency of outcome events") +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank())

# Save plot --------------------------------------------------------------------
print("Save plot")

ggplot2::ggsave(filename = paste0("output/cumulative-",gsub("\\..*","",filename),".jpeg"),
                height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)