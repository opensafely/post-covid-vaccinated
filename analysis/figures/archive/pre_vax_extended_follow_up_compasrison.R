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

# Get outcomes to plot 

active_analyses <- read_rds("lib/active_analyses_pre_vax.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", "")) %>%
  filter(grepl("_ate|_vte",outcome_variable))

outcomes_to_plot <- outcome_name_table$outcome_name

outcome_name_table$outcome <- gsub(" events","",outcome_name_table$outcome)


# Load all estimates 
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

# Get estimates for main analyses and list of outcomes from active analyses

main_estimates <- estimates %>% filter(event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & model == "mdl_max_adj"
                                       & time_points == "reduced"
                                       & subgroup %in% c("main","covid_pheno_hospitalised","covid_pheno_non_hospitalised")
                                       & cohort == "pre_vaccination") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up) %>%
  dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))


#------------------------------------------#
# Specify groups and their line colours #
#------------------------------------------#
# Specify colours
main_estimates$colour <- "#d2ac47"
main_estimates$colour <- ifelse(grepl("extended_follow_up",main_estimates$event),"#009999",main_estimates$colour)

# Use the nice names from active_analyses table i.e. outcome_name_table
main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))
main_estimates$subgroup <- ifelse(main_estimates$subgroup == "main", "All COVID-19", ifelse(main_estimates$subgroup == "covid_pheno_hospitalised", "Hospitalised COVID-19","Non-Hospitalised COVID-19" ))

# Factor variables for ordering

main_estimates$outcome <- factor(main_estimates$outcome, levels=c("Arterial thrombosis event","Arterial thrombosis event - Extended follow up",
                                                              "Venous thrombosis event","Venous thrombosis event - Extended follow up" ,
                                                              "Arterial thrombosis event - Primary position","Arterial thrombosis event - Primary position, Extended follow up",
                                                              "Venous thrombosis event - Primary position","Venous thrombosis event - Primary position, Extended follow up"))

main_estimates$colour <- factor(main_estimates$colour, levels=c("#d2ac47","#009999"))
main_estimates$subgroup <- factor(main_estimates$subgroup,levels = c("All COVID-19", "Hospitalised COVID-19","Non-Hospitalised COVID-19"))

for (i in c("ate","vte","ate_primary_position","vte_primary_position")) {
  if(i == "vte"){
    ylim <- 256
    ybreaks <- c(0.5,1,2,4,8,16,32,64,128)
  }else{
    ylim <- 32
    ybreaks <- c(0.5,1,2,4,8,16,32)
  }
  df <- main_estimates %>% filter(event ==i | event == paste0(i,"_extended_follow_up"))
  
  if(i == "ate"){
    df$outcome <- factor(df$outcome, levels=c("Arterial thrombosis event","Arterial thrombosis event - Extended follow up"))
  }else if(i == "vte"){
    df$outcome <- factor(df$outcome, levels=c("Venous thrombosis event","Venous thrombosis event - Extended follow up"))
  }else if (i == "ate_primary_position"){
    df$outcome <- factor(df$outcome, levels=c("Arterial thrombosis event - Primary position","Arterial thrombosis event - Primary position, Extended follow up"))
  }else if(i == "vte_primary_position"){
    df$outcome <- factor(df$outcome, levels=c("Venous thrombosis event - Primary position","Venous thrombosis event - Primary position, Extended follow up"))
  }
  
  ggplot2::ggplot(data=df,
                  mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = outcome, shape= outcome, fill= outcome))+
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
    #ggplot2::scale_y_continuous(lim = c(0.5,32), breaks = c(0.5,1,2,4,8,16,32), trans = "log")+ 
    ggplot2::scale_y_continuous(lim = c(0.5,ylim), breaks = ybreaks, trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,ceiling(max(df$median_follow_up, na.rm = T) / 8) * 8), breaks = seq(0,ceiling(max(df$median_follow_up, na.rm = T) / 8) * 8,8))+ 
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$outcome))+
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$outcome)) +
    ggplot2::scale_shape_manual(values = c(21,21), labels = levels(df$outcome)) +
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
  
  ggplot2::ggsave(paste0(output_dir,"Pre_vax_compare_extended_follow_up_",i,".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
}
