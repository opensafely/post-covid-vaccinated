library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(plyr)

cohort=c("pre_vaccination", "vaccinated","electively_unvaccinated")
events_to_plot <- c("ate")

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"

active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))

# Focus on ATE and VTE
outcomes_to_plot <- "ate"


#--------Load fully adjusted main and COVID phenotype results-------------------
hr_files=list.files(path =results_dir , pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,hr_files)

hr_file_paths <- pmap(list(hr_files), 
                      function(fpath){ 
                        df <- fread(fpath) 
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

#-------------------------Filter to active outcomes-----------------------------
main_estimates <- estimates %>% filter(subgroup %in% subgroup[grepl("prior",subgroup)]
                                       & event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & results_fitted == "fitted_successfully"
                                       & model == "mdl_max_adj"
                                       & estimate != "[Redacted]") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up,model)

main_estimates <- main_estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))

#---------------------------Specify time to plot--------------------------------
main_estimates$add_to_median <- sub("days","",main_estimates$term)
main_estimates$add_to_median <- as.numeric(sub("\\_.*","",main_estimates$add_to_median))

main_estimates$median_follow_up <- ((main_estimates$median_follow_up + main_estimates$add_to_median)-1)/7

# Rename subgroup to 'nice' format------------------------------------------------

main_estimates$subgroup <- ifelse(main_estimates$subgroup=="prior_history_FALSE","No prior history of event",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="prior_history_TRUE","Prior history of event",main_estimates$subgroup)


# Specify line colours ---------------------------------------------------------
main_estimates$colour <- ""
main_estimates$colour <- ifelse(main_estimates$subgroup=="No prior history of event","#FFC20A",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Prior history of event","#0C7BDC",main_estimates$colour) 
 
# Factor variables for ordering
main_estimates$colour <- factor(main_estimates$colour, levels=c("#FFC20A","#0C7BDC"))
main_estimates$subgroup <- factor(main_estimates$subgroup, levels = c("No prior history of event","Prior history of event"))
main_estimates$cohort <- factor(main_estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 

# Rename adjustment groups
levels(main_estimates$cohort) <- list("Pre-vaccinated (2020-01-01 - 2021-06-18)"="pre_vaccination", "Vaccinated (2021-06-01 - 2021-12-14)"="vaccinated","Electively unvaccinated (2021-06-01 - 2021-12-14)"="electively_unvaccinated")


# Make event names 'nice' ------------------------------------------------------

main_estimates <- main_estimates %>% left_join(active_analyses %>% select(outcome, outcome_variable), by = c("event"="outcome_variable"))

# We want to plot the figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points
main_estimates <- main_estimates %>%
  group_by(event,subgroup,cohort) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_points == "normal") ~ "normal",
    TRUE ~ "reduced"))

main_estimates <- main_estimates %>%
  group_by(event,subgroup) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_period_to_plot == "reduced") ~ "reduced",
    TRUE ~ "normal"))



df <- main_estimates %>% filter(time_points == time_period_to_plot)

ggplot2::ggplot(data=df,
                mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = subgroup, shape= subgroup, fill= subgroup))+
  #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                ymax = ifelse(conf_high>64,64,conf_high),  
                                                width = 0), 
                         #position = ggplot2::position_dodge(width = 1)
                         )+   
  #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
  ggplot2::geom_line() +
  #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
  ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,round_any(max(df$median_follow_up, na.rm = T),4, f= ceiling)), breaks = seq(0,round_any(max(df$median_follow_up, na.rm = T),4, f= ceiling),4)) +
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup))+ 
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
  ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$subgroup)) +
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
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white")) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))+
  ggplot2::facet_wrap(cohort~., ncol = 3)

ggplot2::ggsave(paste0(output_dir,"convalescence_figure3_prior_history_ate.png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
  
  














