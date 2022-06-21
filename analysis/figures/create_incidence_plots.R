## =============================================================================
## Purpose:  Create plots from incident data (OUTSIDE OF OPENSAFELY)
## 
## Author:   Kurt Taylor
##
## Reviewer: 
##
## Date:     15th June 2022
##
## Data:     Post covid events study population
##
## Content: Create plots of the incidence and cumulative incidence of COVID by week from the beginning of follow up to the end of follow-up for each cohort.
## Output:  
## =============================================================================

library(outbreaks)
library(ggplot2)
library(incidence)
library(readr)
library(tidyverse)
library(cowplot)
library(scales)

cov_incidence_plot <- function(cohort_name, group) {
  
  ##------------------------------
  # WEEKLY INCIDENCE ---------------------------------------------------
  ##------------------------------
  
  # Read in the files outputted from OS
  
  weekly_all <- read.csv(paste0("output/review/figure-data/incidence/weekly_incidence_all_", cohort_name,"_", group,".csv"))
  weekly_age <- read.csv(paste0("output/review/figure-data/incidence/weekly_incidence_age_", cohort_name,"_", group,".csv"))
  weekly_sex <- read.csv(paste0("output/review/figure-data/incidence/weekly_incidence_sex_", cohort_name,"_", group,".csv"))
  
  # Turn data frames back into incidence objects 
  
  weekly_all_inc <- incidence(rep(weekly_all$dates, weekly_all$counts), interval = "1 saturday weeks")
  
  weekly_age_inc <- incidence(c(rep(weekly_age$dates, weekly_age$Age18_29),rep(weekly_age$dates, weekly_age$Age30_39), 
                                rep(weekly_age$dates, weekly_age$Age40_49),rep(weekly_age$dates, weekly_age$Age50_59), 
                                rep(weekly_age$dates, weekly_age$Age60_69),rep(weekly_age$dates, weekly_age$Age70_79), 
                                rep(weekly_age$dates, weekly_age$Age80_89),rep(weekly_age$dates, weekly_age$Age90_plus)),
                              groups = c(rep("Age18_29",sum(weekly_age$Age18_29)), rep("Age30_39",sum(weekly_age$Age30_39)),
                                         rep("Age40_49",sum(weekly_age$Age40_49)), rep("Age50_59",sum(weekly_age$Age50_59)),
                                         rep("Age_60_69",sum(weekly_age$Age60_69)), rep("Age70_79",sum(weekly_age$Age70_79)),
                                         rep("Age80_89",sum(weekly_age$Age80_89)), rep("Age90_plus",sum(weekly_age$Age90_plus))),
                              interval = "1 saturday weeks")
  
  weekly_sex_inc <- incidence(c(rep(weekly_sex$dates, weekly_sex$Female),rep(weekly_sex$dates, weekly_sex$Male)),
                             groups = c(rep("Female",sum(weekly_sex$Female)), rep("Male",sum(weekly_sex$Male))),
                             interval = "1 saturday weeks")
  
  weekly_all_plot <- plot(weekly_all_inc, labels_week = FALSE) + scale_x_date(labels = date_format("%b %Y"))
  weekly_age_plot <- plot(weekly_age_inc, labels_week = FALSE) + scale_x_date(labels = date_format("%b %Y"))
  weekly_sex_plot <- plot(weekly_sex_inc, labels_week = FALSE) + scale_x_date(labels = date_format("%b %Y"))
  
  ##------------------------------
  # CUMULATIVE INCIDENCE ---------
  ##------------------------------
  
  # Read in the files outputted from OS and change date var to date class
  
  cum_all <- read.csv(paste0("output/review/figure-data/incidence/cum_incidence_all_", cohort_name,"_", group,".csv"))
  cum_all$exp_date_covid19_confirmed <- as.Date(cum_all$exp_date_covid19_confirmed)
  cum_age <- read.csv(paste0("output/review/figure-data/incidence/cum_incidence_age_", cohort_name,"_", group,".csv"))
  cum_age$exp_date_covid19_confirmed <- as.Date(cum_age$exp_date_covid19_confirmed)
  cum_sex <- read.csv(paste0("output/review/figure-data/incidence/cum_incidence_sex_", cohort_name,"_", group,".csv"))
  cum_sex$exp_date_covid19_confirmed <- as.Date(cum_sex$exp_date_covid19_confirmed)
  
  # Plotting
  
  cumulative_all_plot <- cum_all %>% ggplot() + 
    geom_line(aes(x = exp_date_covid19_confirmed, y = Cumsum, linetype = All)) + 
    labs(y = "Cumulative cases of COVID-19", x = NULL)
  
  cumulative_age_plot <- cum_age %>% ggplot() + 
    geom_line(aes(x = exp_date_covid19_confirmed, y = Cumsum, linetype = Age_Group, col = Age_Group)) + 
    labs(y = "Cumulative cases of COVID-19", x = NULL)
  
  cumulative_sex_plot <- cum_sex %>% ggplot() + 
    geom_line(aes(x = exp_date_covid19_confirmed, y = Cumsum, linetype = Sex)) + 
    labs(y = "Cumulative cases of COVID-19", x = NULL)
  
  ##------------------------------
  # COMBINE PLOTS AND SAVE -------
  ##------------------------------
  
  # CREATE PLOT TITLES
  
  title_theme <- ggdraw() +
    draw_label("COVID-19 incidence without stratification", 
               x = 0, hjust = 0, size = 22, fontface = "bold")
  
  title_theme_age <- ggdraw() +
    draw_label("COVID-19 incidence stratified by age groups", 
               x = 0, hjust = 0, size = 22, fontface = "bold")
  
  title_theme_sex <- ggdraw() +
    draw_label("COVID-19 incidence stratified by sex", 
               x = 0, hjust = 0, size = 22, fontface = "bold")
  
  # PLOT WITHOUT STRATIFICATION
  
  all_plots <- plot_grid(weekly_all_plot, cumulative_all_plot, ncol=2, nrow = 1,
            labels = c("A: Weekly incidence of COVID-19 cases", "B: Cumulative incidence of COVID-19 cases"),
            label_x = -0.2,
            hjust = -0.5, scale = 0.93)
  png(paste0(local_file_output,"incidence_plot_", cohort_name, "_", group, ".png"),
      width = 15, height = 8, units = "in", res = 400)
  plot_grid(title_theme, all_plots, ncol = 1, rel_heights = c(0.1, 1))
  print(all_plots)
  dev.off()
  
  # PLOT STRATIFYING BY AGE
  
  age_plots <- plot_grid(weekly_age_plot, cumulative_age_plot, ncol=2, nrow = 1,
                         labels = c("A: Weekly incidence of COVID-19 cases", "B: Cumulative incidence of COVID-19 cases"),
                         label_x = -0.2,
                         hjust = -0.5, scale = 0.93)
  png(paste0(local_file_output,"incidence_plot_by_age_", cohort_name, "_", group, ".png"),
      width = 15, height = 8, units = "in", res = 400)
  plot_grid(title_theme_age, age_plots, ncol = 1, rel_heights = c(0.1, 1))
  print(age_plots)
  dev.off()
  
  # PLOT STRATIFYING BY SEX
  
  sex_plots <- plot_grid(weekly_sex_plot, cumulative_sex_plot, ncol=2, nrow = 1,
                         labels = c("A: Weekly incidence of COVID-19 cases", "B: Cumulative incidence of COVID-19 cases"),
                         label_x = -0.2,
                         hjust = -0.5, scale = 0.93)
  png(paste0(local_file_output,"incidence_plot_by_sex_", cohort_name, "_", group, ".png"),
      width = 15, height = 8, units = "in", res = 400)
  plot_grid(title_theme_sex, sex_plots, ncol = 1, rel_heights = c(0.1, 1))
  print(sex_plots)
  dev.off()
  
}

##--------------------
# RUN FUNCTION -------
##--------------------

# SPECIFICATIONS 

# change cohort name to "vaccinated", "unvaccinated" or "both"

cohort_name <- "vaccinated"

# use active analyses if you have more than one more "outcome group" to loop over
# active_analyses <- read_rds("lib/active_analyses.rds")
# active_analyses <- active_analyses %>% filter(active==TRUE)
# group <- unique(active_analyses$outcome_group)

# or manually change outcome group here: 
group <- "diabetes"

# change file path to where you want to save the figures
local_file_output <- "/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/incidence-plots/"

for(i in group){
  if (cohort_name == "both") {
    cov_incidence_plot("electively_unvaccinated", i)
    cov_incidence_plot("vaccinated", i)
  } else{
    cov_incidence_plot(cohort_name, i)
  }
}

### END