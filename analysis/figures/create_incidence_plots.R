library(outbreaks)
library(ggplot2)
library(incidence)

dat <- ebola_sim$linelist$date_of_onset
i <- incidence(dat)
plot(i)

i_df <- as.data.frame(i)

ggplot(i_df, aes(dates,counts)) + geom_col(width=1)

test <- incidence(rep(i_df$dates, i_df$counts))





  i_df_plot <- ggplot(i_df, aes(x = dates, y = counts)) + 
    geom_bar(stat="identity")
  

aes(x = dates, y = counts) + 
  labs(y = "Cumulative cases of COVID-19", x = "Months")

ggplot(i_df, aes(x=dates)) + geom_histogram(binwidth=30, colour="white") +
  # scale_x_date(labels = date_format("%Y-%b"),
               breaks = seq(min(i_df$dates)-5, max(i_df$dates)+5, 30),
               # limits = c(as.Date("2008-05-01"), as.Date("2012-04-01"))) +
  # ylab("Frequency") + xlab("Year and Month") +
  theme_bw() + opts(axis.text.x = theme_text(angle=90))