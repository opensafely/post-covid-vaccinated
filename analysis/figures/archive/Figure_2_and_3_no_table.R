#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)


results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", "")) %>%
  filter(outcome_name %in% c("ate","vte","ate_primary_position","vte_primary_position"))

outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name %in% c("ate","vte","ate_primary_position","vte_primary_position")]

outcome_name_table$outcome <- gsub(" events","",outcome_name_table$outcome)

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

# Get estimates for main analyses and list of outcomes from active analyses
estimates <- estimates %>% filter(subgroup %in% c("covid_pheno_non_hospitalised","covid_pheno_hospitalised", "main") 
                                       & event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & model == "mdl_max_adj") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up)

estimates <- estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

# We want to plot the figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points
estimates <- estimates %>%
  group_by(event,subgroup,cohort) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_points == "normal") ~ "normal",
    TRUE ~ "reduced"))

estimates <- estimates %>%
  group_by(event,subgroup) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_period_to_plot == "reduced") ~ "reduced",
    TRUE ~ "normal"))



#------------------------------------------#
# Specify groups and their line colours #
#------------------------------------------#
# Specify colours
estimates$colour <- ""
estimates$colour <- ifelse(estimates$cohort=="pre_vaccination","#d2ac47",estimates$colour)
estimates$colour <- ifelse(estimates$cohort=="vaccinated","#58764c",estimates$colour) # Grey
estimates$colour <- ifelse(estimates$cohort=="electively_unvaccinated","#0018a8",estimates$colour) # Black

# Factor variables for ordering
estimates$cohort <- factor(estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 
estimates$colour <- factor(estimates$colour, levels=c("#d2ac47","#58764c","#0018a8"))

estimates$subgroup <- factor(estimates$subgroup,levels = c("main", "covid_pheno_hospitalised","covid_pheno_non_hospitalised"))

# Rename adjustment groups
levels(estimates$cohort) <- list("Pre-vaccination (1 Jan 2020 - 18 Jun 2021)"="pre_vaccination", "Vaccinated (1 Jun 2021 - 14 Dec 2021)"="vaccinated","Unvaccinated (1 Jun 2021 - 14 Dec 2021)"="electively_unvaccinated")
levels(estimates$subgroup) <- list("All COVID-19"="main", "Hospitalised COVID-19"="covid_pheno_hospitalised","Non-hospitalised COVID-19"="covid_pheno_non_hospitalised")

for(i in outcomes_to_plot){
  
  if(i=="vte"){
    ylim <- 512
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128,256)
  }else{
    ylim = 64
    ybreaks <- c(0.5,1,2,4,8,16,32,64)
  }
  df <- estimates %>% filter(event == i
                             & time_points == time_period_to_plot)
  
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
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 3, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   text=element_text(size=13)) +    
    ggplot2::facet_wrap(subgroup~., ncol = 3)
  
  ggplot2::ggsave(paste0(output_dir,"Figure_2_",i,".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
  
  
}

for(i in outcomes_to_plot){
  
  if(i=="vte"){
    ylim <- 512
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128,256)
  }else{
    ylim = 64
    ybreaks <- c(0.5,1,2,4,8,16,32,64)
  }
  df <- estimates %>% filter(event == i
                             & time_points == time_period_to_plot)
  
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
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 3, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   text=element_text(size=11)) +    
    ggplot2::facet_wrap(subgroup~., ncol = 3)
  
  ggplot2::ggsave(paste0(output_dir,"Figure_2_",i,"_half_size.png"), height = 105, width = 297, unit = "mm", dpi = 600, scale = 1)
  
  
}


