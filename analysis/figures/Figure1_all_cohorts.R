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
outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name != c("ate","vte")]

#---------------------------------------------#
# 3. Load and combine all estimates in 1 file #
#---------------------------------------------#
hr_files=list.files(path = results_dir, pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

# Get estimates for main analyses and list of outcomes from active analyses
estimates <- estimates %>% filter(subgroup == "main" 
                                       & event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & results_fitted == "fitted_successfully"
                                       & model == "mdl_max_adj") %>%
  select(term,estimate,conf.low,conf.high,event,subgroup,cohort,time_points)

main_estimates <- estimates[0,]

for(i in unique(estimates$event)){
  for(j in unique(estimates$subgroup)){
    for (k in unique(estimates$cohort)) {
      for (l in unique(estimates$time_points)) {
        tmp <- estimates %>% filter(event == i
                                         & subgroup == j
                                         & cohort == k
                                         & time_points == l)
        if(nrow(tmp)>0){
          tmp$time_points <- ifelse(any(tmp$term == "days0_7"),"normal","reduced")
          main_estimates <- rbind(main_estimates,tmp)
        }
      }
    }
  }
}

main_estimates <- main_estimates[!duplicated(main_estimates), ]
main_estimates <- main_estimates %>% mutate(across(c(estimate,conf.low,conf.high),as.numeric))

# We want to plot thhe figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points
main_estimates <- main_estimates %>%
  group_by(event,subgroup) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_points == "reduced") ~ "reduced",
    TRUE ~ "normal"))

#--------------------------------------#
# 4. Specify time in weeks (mid-point) #
#--------------------------------------#
term_to_time <- data.frame(term = c("days0_7","days7_14", "days14_28", "days28_56", "days56_84", "days84_197","days197_535", 
                                    "days0_28","days28_197","days28_535"),
                           time = c(0.5,1.5,3,6,10,20,52,
                                    2,16,40))
main_estimates <- merge(main_estimates, term_to_time, by = c("term"), all.x = TRUE)
#(28/7)+((535-28)/14)

#------------------------------------------#
# 4. Specify groups and their line colours #
#------------------------------------------#
# Specify colours
main_estimates$colour <- ""
main_estimates$colour <- ifelse(main_estimates$cohort=="pre_vaccination","#d2ac47",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$cohort=="vaccinated","#58764c",main_estimates$colour) # Grey
main_estimates$colour <- ifelse(main_estimates$cohort=="electively_unvaccinated","#94273c",main_estimates$colour) # Black

# Factor variables for ordering
main_estimates$cohort <- factor(main_estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 
main_estimates$colour <- factor(main_estimates$colour, levels=c("#d2ac47","#58764c","#94273c"))

# Rename adjustment groups
levels(main_estimates$cohort) <- list("Pre-vaccinated (2020-01-01 - 2021-06-18)"="pre_vaccination", "Vaccinated (2021-06-01 - 2021-12-14)"="vaccinated","Electively unvaccinated (2021-06-01 - 2021-12-14)"="electively_unvaccinated")

#-------------------------#
# 5. Specify outcome name #
#-------------------------#
# Use the nice names from active_analyses table i.e. outcome_name_table
main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

#---------#
# 6. Plot #
#---------#

for(i in c("any_position","primary_position")){
  if(i == "any_position"){
    df <- main_estimates %>% filter(!event %in% event[grepl("primary_position",event)]
                                    & time_points == time_period_to_plot)
  }else{
    df <- main_estimates %>% filter(event %in% event[grepl("primary_position",event)]
                                    & time_points == time_period_to_plot)
  }
  
  ggplot2::ggplot(data=df,
                  mapping = ggplot2::aes(x=time, y = estimate, color = cohort, shape= cohort, fill= cohort))+
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                                  ymax = ifelse(conf.high>64,64,conf.high),  
                                                  width = 0), 
                           position = ggplot2::position_dodge(width = 1))+   
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) +    
    #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
    ggplot2::scale_y_continuous(lim = c(0.5,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,56), breaks = seq(0,56,4)) +
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
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +    
    ggplot2::facet_wrap(outcome~., ncol = 2)
  
  ggplot2::ggsave(paste0(output_dir,"Figure1_all_cohorts_",i,".png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)
  
  
}

