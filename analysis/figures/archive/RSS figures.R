############
# Figure 1 #
############
#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Using local path - for testing
#dir <- "C:/Users/gic30/OneDrive - University of Cambridge/2. Long Covid/Code/Post-covid-vaccinated - stage 6 - Figure 1 - 2022.02"
#setwd(dir)

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses_table <- subset(active_analyses, active_analyses$active =="TRUE" & active_analyses$outcome_group=="CVD")
outcome_name_table <- active_analyses_table %>% 
  select(outcome, outcome_variable,outcome_group) %>% 
  mutate(outcome_name=active_analyses_table$outcome_variable %>% str_replace("out_date_", ""))

# Focus on first 8 CVD outcomes (remove ate and vte)
outcome_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name %in% c("ate","vte")]


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

#Get median follow-up time
median_follow_up <- estimates %>% select(expo_week, median_follow_up, event, subgroup, model, cohort, time_points) %>%
                                  filter(event %in% c("ate","vte") & median_follow_up != "[Redacted]" & !expo_week %in% c("days_pre","all post expo") )
median_follow_up <- median_follow_up %>% rename(term=expo_week)
median_follow_up$add_days <- sub("days","",median_follow_up$term)
median_follow_up$add_days <- sub('\\_.*', '', median_follow_up$add_days)
median_follow_up$add_days <- as.numeric(median_follow_up$add_days)
median_follow_up$median_follow_up <- as.numeric(median_follow_up$median_follow_up) +median_follow_up$add_days

# Get estimates for main analyses and list of outcomes from active analyses
main_estimates <- subset(estimates, subgroup == "main" & event %in% outcome_to_plot & term %in% term[grepl("^days",term)])

#--------------------------Format the results-----------------------------------
main_estimates <- main_estimates %>% mutate(across(c("estimate","conf.low","conf.high"), as.numeric))
main_estimates$model <- ifelse(main_estimates$model == "mdl_agesex", "mdl_age_sex",main_estimates$model)

main_estimates_ate <- main_estimates %>% filter(model %in% c("mdl_age_sex","mdl_max_adj")
                                                & event == "ate"
                                               & results_fitted != "fitted_unsuccessfully"
                                               & (event != "ate" | time_points != "normal"))

main_estimates_vte_vacc <- main_estimates %>% filter(model %in% c("mdl_age_sex","mdl_max_adj")
                                                & event == "vte"
                                                & (results_fitted == "fitted_successfully" | is.na(results_fitted))
                                                & cohort == "vaccinated")

main_estimates_vte_pre_vacc <- main_estimates %>% filter(model %in% c("mdl_age_sex","mdl_max_adj")
                                                     & event == "vte"
                                                     & (results_fitted == "fitted_successfully" | is.na(results_fitted))
                                                     & cohort == "pre_vaccination")

main_estimates_vte_unvax <- main_estimates %>% filter(model %in% c("mdl_age_sex","mdl_max_adj")
                                                         & event == "vte"
                                                         & (results_fitted == "fitted_successfully" | is.na(results_fitted))
                                                         & cohort == "electively_unvaccinated"
                                                      & term %in% c("days0_28","days28_197"))


main_estimates <- rbind(main_estimates_ate,main_estimates_vte_vacc,main_estimates_vte_pre_vacc,main_estimates_vte_unvax)                                        
main_estimates$expo_week <- NULL
main_estimates$median_follow_up <- NULL

#--------------------------------------#
# 4. Specify time in weeks (mid-point) #
#--------------------------------------#
term_to_time <- data.frame(term = c("days0_7","days7_14", "days14_28", "days28_56", "days56_84", "days84_197", 
                                    "days0_28","days28_197","days197_535"),
                           time = c(0.5,1.5,3,6,10,20,
                                    2,16,52))


main_estimates <- merge(main_estimates, term_to_time, by = c("term"), all.x = TRUE)

main_estimates <- main_estimates %>% left_join(median_follow_up)
main_estimates$median_follow_up <- as.numeric(main_estimates$median_follow_up) /7
main_estimates$time_to_plot <- ifelse(main_estimates$term == "days28_197" | main_estimates$term == "days84_197" & main_estimates$cohort !="pre_vaccination" ,main_estimates$median_follow_up,main_estimates$time)
main_estimates$time_to_plot <- ifelse(main_estimates$term == "days197_535" ,main_estimates$median_follow_up,main_estimates$time_to_plot)

#------------------------------------------#
# 4. Specify groups and their line colours #
#------------------------------------------#
# Specify colours
main_estimates$colour <- ""
main_estimates$colour <- ifelse(main_estimates$model=="mdl_age_sex","#bababa",main_estimates$colour) # Grey
main_estimates$colour <- ifelse(main_estimates$model=="mdl_max_adj","#000000",main_estimates$colour) # Black

# Factor variables for ordering
main_estimates$model <- factor(main_estimates$model, levels=c("mdl_age_sex", "mdl_max_adj")) 
main_estimates$colour <- factor(main_estimates$colour, levels=c("#bababa", "#000000"))
main_estimates$cohort <- factor(main_estimates$cohort, levels=c("pre_vaccination", "vaccinated","electively_unvaccinated"))

# Rename adjustment groups
levels(main_estimates$model) <- list("Age/sex/region adjustment"="mdl_age_sex", "Extensive adjustment"="mdl_max_adj")
levels(main_estimates$cohort) <- list("Pre-vaccinated (Jan 2020 - June 2021)"="pre_vaccination", "Vaccinated (June 2021 - Dec 2021)"="vaccinated", "Electively unvaccinated (June 2021 - Dec 2021)"="electively_unvaccinated")

#-------------------------#
# 5. Specify outcome name #
#-------------------------#
# Use the nice names from active_analyses table i.e. outcome_name_table
main_estimates <- main_estimates %>% left_join(outcome_name_table %>% select(outcome, outcome_name, outcome_group), by = c("event"="outcome_name"))

#---------#
# 6. Plot #
#---------#
event_name="vte"
for(event_name in outcome_to_plot){
  df=main_estimates %>% filter(event == event_name)
  
  ggplot2::ggplot(data=df,
                  mapping = ggplot2::aes(x=time_to_plot, y = estimate, color = model, shape=model, fill=model))+
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                                  ymax = ifelse(conf.high>64,64,conf.high),  
                                                  width = 0), 
                           position = ggplot2::position_dodge(width = 1))+   
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) +    
    #    ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
    ggplot2::scale_y_continuous(lim = c(0.5,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,36), breaks = seq(0,36,4)) +
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$model))+ 
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$model)) +
    ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$model)) +
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
    ggplot2::facet_wrap(cohort~., ncol = 3)+
    ggtitle(paste0(df$outcome)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplot2::ggsave(paste0(output_dir,"RSS_figure","_",event_name,".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
  
}


