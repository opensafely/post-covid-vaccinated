#libraries
library(readr) 
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Using local path - for testing
#dir <- "C:/Users/gic30/OneDrive - University of Cambridge/2. Long Covid/Code/Post-covid-vaccinated - stage 6 - Figure 1 - 2022.02"
#setwd(dir)

#results_dir <- "/Users/gic30/OneDrive - University of Cambridge/2. Long Covid/Code/Post-covid-vaccinated - stage 6 - Figures CVD - 2021.08/output/"
#output_dir <- "/Users/gic30/OneDrive - University of Cambridge/2. Long Covid/Code/Post-covid-vaccinated - stage 6 - Figures CVD - 2021.08/figures/"

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))

# Focus on first 8 CVD outcomes (remove ate and vte)
#outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name != c("ate","vte","ate_primary_position","vte_primary_position")]
outcomes_to_plot <- outcome_name_table$outcome_name


# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

# Get estimates for main analyses and list of outcomes from active analyses
estimates <- estimates %>% filter(event %in% outcomes_to_plot 
                                  & term %in% term[grepl("^days",term)]
                                  & model == "mdl_max_adj"
                                  & time_points %in% time_points[grepl("^day_zero",time_points)]) %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up)


estimates <- estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

# We want to plot the figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points

# estimates <- estimates %>%
#   group_by(event,subgroup,cohort) %>%
#   dplyr::mutate(time_period_to_plot = case_when(
#     any(time_points == "normal") ~ "normal",
#     TRUE ~ "reduced"))
# 
# estimates <- estimates %>%
#   group_by(event,subgroup) %>%
#   dplyr::mutate(time_period_to_plot = case_when(
#     any(time_period_to_plot == "reduced") ~ "reduced",
#     TRUE ~ "normal"))

#------------------------------------------#
# 4. Specify groups and their line colours #
#------------------------------------------#
# Specify colours
estimates$colour <- ""
estimates$colour <- ifelse(estimates$cohort=="pre_vaccination","#d2ac47",estimates$colour)
estimates$colour <- ifelse(estimates$cohort=="vaccinated","#58764c",estimates$colour) 
estimates$colour <- ifelse(estimates$cohort=="electively_unvaccinated","#0018a8",estimates$colour)

# Factor variables for ordering
estimates$cohort <- factor(estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 
estimates$colour <- factor(estimates$colour, levels=c("#d2ac47","#58764c","#0018a8"))

# Rename adjustment groups
#levels(estimates$cohort) <- list("Vaccinated (1 Jun 2021 - 14 Dec 2021)"="vaccinated","Unvaccinated (1 Jun 2021 - 14 Dec 2021)"="electively_unvaccinated")
levels(estimates$cohort) <- list("Pre-vaccination (1 Jan 2020 - 18 Jun 2021)"="pre_vaccination", "Vaccinated (1 Jun 2021 - 14 Dec 2021)"="vaccinated","Unvaccinated (1 Jun 2021 - 14 Dec 2021)"="electively_unvaccinated")

# Order outcomes for plotting
# Use the nice names from active_analyses table i.e. outcome_name_table
estimates <- estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

estimates$outcome <- factor(estimates$outcome, levels=c("Arterial thrombosis event",
                                                        "Venous thrombosis event",
                                                        "Acute myocardial infarction",
                                                        "Ischaemic stroke",
                                                        "Pulmonary embolism",
                                                        "Deep vein thrombosis",
                                                        "Heart failure",
                                                        "Angina",
                                                        "Transient ischaemic attack",
                                                        "Subarachnoid haemorrhage and haemorrhagic stroke",
                                                        "Arterial thrombosis event - Primary position events",
                                                        "Venous thrombosis event - Primary position events",
                                                        "Acute myocardial infarction - Primary position events",
                                                        "Ischaemic stroke - Primary position events",
                                                        "Pulmonary embolism - Primary position events",
                                                        "Deep vein thrombosis - Primary position events",
                                                        "Heart failure - Primary position events",
                                                        "Angina - Primary position events",
                                                        "Transient ischaemic attack - Primary position events",
                                                        "Subarachnoid haemorrhage and haemorrhagic stroke - Primary position events"))
                                                                           
                                                                            
# Plot day zero main results
i="any_position"
for(i in c("any_position","primary_position")){
  if(i == "any_position"){
    df <- estimates %>% filter(!event %in% event[grepl("primary_position",event)] 
                               & subgroup == "main"
                               & time_points == "day_zero_reduced")
    ylim <- 256
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128,256)
  }else{
    df <- estimates %>% filter(event %in% event[grepl("primary_position",event)] 
                               & subgroup == "main"
                               & time_points == "day_zero_reduced")
    ylim <- 256
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128,256)
  }
  
  ggplot2::ggplot(data=df,
                  mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
    #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                  ymax = ifelse(conf_high>ylim,ylim,conf_high),  
                                                  width = 0)
                           #,position = ggplot2::position_dodge(width = 1)
    )+   
    #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
    ggplot2::geom_line() +
    #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
    ggplot2::scale_y_continuous(lim = c(0.5,ylim), breaks = ybreaks, trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,ceiling(max(df$median_follow_up, na.rm = T) / 4) * 4), breaks = seq(0,ceiling(max(df$median_follow_up, na.rm = T) / 4) * 4,4)) +
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
    ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
    #ggplot2::ggtitle(paste0(i," events, ",j," time periods")) +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   #legend.justification = "left",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   text=element_text(size=13)) +
    ggplot2::facet_wrap(outcome~., ncol = 2)
  
  ggplot2::ggsave(paste0(output_dir,"Figure_5_day_zero_main_",i,"_reduced_time_points.png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)
}

# Plot day zero hospitalised results
for(i in c("any_position","primary_position")){
  if(i == "any_position"){
    df <- estimates %>% filter(!event %in% event[grepl("primary_position",event)] 
                               & subgroup == "covid_pheno_hospitalised"
                               & time_points == "day_zero_reduced")
    ylim <- 256
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128,256)
  }else{
    df <- estimates %>% filter(event %in% event[grepl("primary_position",event)] 
                               & subgroup == "covid_pheno_hospitalised"
                               & time_points == "day_zero_reduced")
    ylim <- 256
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128,256)
  }
  
  ggplot2::ggplot(data=df,
                  mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
    #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                  ymax = ifelse(conf_high>ylim,ylim,conf_high),  
                                                  width = 0)
                           #,position = ggplot2::position_dodge(width = 1)
    )+   
    #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
    ggplot2::geom_line() +
    #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
    ggplot2::scale_y_continuous(lim = c(0.5,ylim), breaks = ybreaks, trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,ceiling(max(df$median_follow_up, na.rm = T) / 4) * 4), breaks = seq(0,ceiling(max(df$median_follow_up, na.rm = T) / 4) * 4,4)) +
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
    ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
    #ggplot2::ggtitle(paste0(i," events, ",j," time periods")) +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   #legend.justification = "left",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   text=element_text(size=13)) +
    ggplot2::facet_wrap(outcome~., ncol = 2)
  
  ggplot2::ggsave(paste0(output_dir,"Figure_5_day_zero_hospitalised_",i,"_reduced_time_points.png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)
}

# Plot day zero non-hospitalised results
for(i in c("any_position","primary_position")){
  if(i == "any_position"){
    df <- estimates %>% filter(!event %in% event[grepl("primary_position",event)] 
                               & subgroup == "covid_pheno_non_hospitalised"
                               & time_points == "day_zero_reduced")
    ylim <- 256
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128,256)
  }else{
    df <- estimates %>% filter(event %in% event[grepl("primary_position",event)] 
                               & subgroup == "covid_pheno_non_hospitalised"
                               & time_points == "day_zero_reduced")
    ylim <- 256
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128,256)
  }
  
  ggplot2::ggplot(data=df,
                  mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
    #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                  ymax = ifelse(conf_high>ylim,ylim,conf_high),  
                                                  width = 0)
                           #,position = ggplot2::position_dodge(width = 1)
    )+   
    #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
    ggplot2::geom_line() +
    #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
    ggplot2::scale_y_continuous(lim = c(0.5,ylim), breaks = ybreaks, trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,ceiling(max(df$median_follow_up, na.rm = T) / 4) * 4), breaks = seq(0,ceiling(max(df$median_follow_up, na.rm = T) / 4) * 4,4)) +
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
    ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
    #ggplot2::ggtitle(paste0(i," events, ",j," time periods")) +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   #legend.justification = "left",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   text=element_text(size=13)) +
    ggplot2::facet_wrap(outcome~., ncol = 2)
  
  ggplot2::ggsave(paste0(output_dir,"Figure_5_day_zero_non_hospitalised_",i,"_reduced_time_points.png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)
}
