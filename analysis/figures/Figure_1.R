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

active_analyses <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", "")) %>%
  filter(outcome_variable %in% c("out_date_ate","out_date_vte")) 

active_analyses_pre_vax <- read_rds("lib/active_analyses_pre_vax.rds") %>% filter(active == "TRUE")

active_analyses_pre_vax <- active_analyses_pre_vax %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses_pre_vax$outcome_variable %>% str_replace("out_date_", ""))%>%
  filter(grepl("extended_follow_up",outcome_variable) 
         & outcome_variable %in% c("out_date_ate_extended_follow_up","out_date_vte_extended_follow_up"))

#----------------------------Focus on ATE & VTE---------------------------------
outcomes_to_plot <- active_analyses$outcome_name
outcomes_to_plot_pre_vax <- active_analyses_pre_vax$outcome_name


# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

# Get estimates for main analyses and list of outcomes from active analyses
estimates <- estimates %>% filter(subgroup %in% c("covid_pheno_non_hospitalised","covid_pheno_hospitalised", "main") 
                                  & ((event %in% outcomes_to_plot & cohort %in% c("vaccinated","electively_unvaccinated")) | (event %in% outcomes_to_plot_pre_vax & cohort %in% c("pre_vaccination"))) 
                                  & term %in% term[grepl("^days",term)]
                                  & model == "mdl_max_adj"
                                  & time_points == "reduced") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up)

estimates <- estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

#------------------------------Tidy event names---------------------------------
estimates$event <- gsub("_extended_follow_up","",estimates$event)

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
levels(estimates$cohort) <- list("Pre-vaccination (Jan 1 2020 - Jun 18 2021)"="pre_vaccination", "Vaccinated (Jun 1 2021 - Dec 14 2021)"="vaccinated","Unvaccinated (Jun 1 2021 - Dec 14 2021)"="electively_unvaccinated")
levels(estimates$subgroup) <- list("All COVID-19"="main", "Hospitalised COVID-19"="covid_pheno_hospitalised","Non-hospitalised COVID-19"="covid_pheno_non_hospitalised")

estimates$grouping_name=""
estimates$grouping_name <- paste0(estimates$subgroup," - ", estimates$event)

unique(estimates$grouping_name)

#Set factor levels
estimates$grouping_name <- factor(estimates$grouping_name, levels = c("All COVID-19 - ate",
                                                                      "Hospitalised COVID-19 - ate",
                                                                      "Non-hospitalised COVID-19 - ate",
                                                                      "All COVID-19 - vte",
                                                                      "Hospitalised COVID-19 - vte",
                                                                      "Non-hospitalised COVID-19 - vte"))

names <- c(
  `All COVID-19 - ate` = "All COVID-19
  ",
  `Hospitalised COVID-19 - ate` = "Hospitalised COVID-19
  Arterial thrombotic events",
  `Non-hospitalised COVID-19 - ate` = "Non-hospitalised COVID-19
  ",
  `All COVID-19 - vte` = "",
  `Hospitalised COVID-19 - vte` = "Venous thrombotic events",
  `Non-hospitalised COVID-19 - vte` = ""
)

df <- estimates

min_plot <- 0.5

max_plot <- 512
y_lim <- c(0.5,512)
y_lim_breaks <- c(0.5,1,2,4,8,16,32,64,128,256,512)


ggplot2::ggplot(data = df, 
                mapping = ggplot2::aes(x = median_follow_up, y = estimate, color = cohort, shape = cohort, fill = cohort)) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1.5))+
  ggplot2::geom_point()+
  ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf_low<min_plot,min_plot,conf_low), 
                                                ymax = ifelse(conf_high>max_plot,max_plot,conf_high),  
                                                width = 0), 
                         #position = ggplot2::position_dodge(width = 1)
  )+
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1.5)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(lim = y_lim, breaks = y_lim_breaks, trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,ceiling(max(df$median_follow_up, na.rm = T) / 8) * 8), breaks = seq(0,ceiling(max(df$median_follow_up, na.rm = T) / 8) * 8,8)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
  ggplot2::scale_shape_manual(values = c(rep(21,length(unique(df$cohort)))),labels = levels(df$cohort)) +
  ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 6, byrow = TRUE)) +
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
  ggplot2::facet_wrap(grouping_name~.,labeller=as_labeller(names), ncol=3)

ggplot2::ggsave(paste0(output_dir,"Figure_1.png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
