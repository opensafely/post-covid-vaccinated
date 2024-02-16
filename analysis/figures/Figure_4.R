# Plots Figure 4 - AER

library(purrr)
library(data.table)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
#library(plyr)

#aer_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/compiled_results/"
#output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/Final/"
aer_output_dir <- "C:/Users/gic30/OneDrive - University of Cambridge/2. Projects/Code/Post-covid-vaccinated - tables-figures formatting"
output_dir <- "C:/Users/gic30/OneDrive - University of Cambridge/2. Projects/Code/Post-covid-vaccinated - tables-figures formatting/"

event_of_interest <- c("ate","vte")
cohort_name <- c("pre_vaccination", "vaccinated","electively_unvaccinated")

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses$outcome_variable <- gsub("out_date_","",active_analyses$outcome_variable)

#Load data
lifetables <- readr::read_csv(paste0(aer_output_dir,"/AER_compiled_results.csv"))

#???? Do we need to do this ?????
lifetables$excess_risk <- lifetables$cumulative_difference_absolute_excess_risk *100
#-------------------------Make event names 'nice' ------------------------------
lifetables <- lifetables %>% left_join(active_analyses %>% select(outcome, outcome_variable), by = c("event"="outcome_variable"))
lifetables$outcome <- factor(lifetables$outcome, levels = c("Arterial thrombosis event","Venous thrombosis event","Arterial thrombosis event - Primary position events","Venous thrombosis event - Primary position events"))
#-------------------------------Format group names--------------------------
lifetables$agegroup <- NA
lifetables$agegroup <- ifelse(grepl("18_39",lifetables$subgroup),"Age group: 18-39",lifetables$agegroup)
lifetables$agegroup <- ifelse(grepl("40_59",lifetables$subgroup),"Age group: 40-59",lifetables$agegroup)
lifetables$agegroup <- ifelse(grepl("60_79",lifetables$subgroup),"Age group: 60-79",lifetables$agegroup)
lifetables$agegroup <- ifelse(grepl("80_110",lifetables$subgroup),"Age group: 80-110",lifetables$agegroup)
lifetables$agegroup <- ifelse(grepl("aer_overall",lifetables$subgroup),"Combined",lifetables$agegroup)
unique(lifetables$agegroup)

lifetables$sex <- NA
lifetables$sex <- ifelse(grepl("Female",lifetables$subgroup),"Sex: Female",lifetables$sex)
lifetables$sex <- ifelse(grepl("Male",lifetables$subgroup),"Sex: Male",lifetables$sex)
lifetables$sex <- ifelse(grepl("aer_overall",lifetables$subgroup),"Sex: Male",lifetables$sex)

# Specify line colours ---------------------------------------------------------

lifetables$colour <- NA
lifetables$colour <- ifelse(lifetables$agegroup=="Age group: 18-39","#006d2c",lifetables$colour)
lifetables$colour <- ifelse(lifetables$agegroup=="Age group: 40-59","#31a354",lifetables$colour)
lifetables$colour <- ifelse(lifetables$agegroup=="Age group: 60-79","#74c476",lifetables$colour)
lifetables$colour <- ifelse(lifetables$agegroup=="Age group: 80-110","#bae4b3",lifetables$colour)
lifetables$colour <- ifelse(lifetables$agegroup=="Combined","#000000",lifetables$colour)

# Specify line types ---------------------------------------------------------
lifetables$linetype <- NA
lifetables$linetype <- ifelse(lifetables$sex=="Sex: Male","solid",lifetables$linetype)
lifetables$linetype <- ifelse(lifetables$sex=="Sex: Female","dotted",lifetables$linetype)

lifetables$grouping_name=""
lifetables$grouping_name <- ifelse(lifetables$cohort == "pre_vaccination", paste0(lifetables$outcome," - Pre-vaccination"),lifetables$grouping_name)
lifetables$grouping_name <- ifelse(lifetables$cohort == "vaccinated", paste0(lifetables$outcome," - Vaccinated"),lifetables$grouping_name)
lifetables$grouping_name <- ifelse(lifetables$cohort == "electively_unvaccinated", paste0(lifetables$outcome," - Unvaccinated"),lifetables$grouping_name)
unique(lifetables$grouping_name)

#Set factor levels
lifetables$grouping_name <- factor(lifetables$grouping_name, levels = c("Arterial thrombosis event - Pre-vaccination",
                                                                        "Arterial thrombosis event - Vaccinated",
                                                                        "Arterial thrombosis event - Unvaccinated",
                                                                        "Venous thrombosis event - Pre-vaccination",
                                                                        "Venous thrombosis event - Vaccinated",
                                                                        "Venous thrombosis event - Unvaccinated",
                                                                        "Arterial thrombosis event - Primary position events - Pre-vaccination",
                                                                        "Arterial thrombosis event - Primary position events - Vaccinated",
                                                                        "Arterial thrombosis event - Primary position events - Unvaccinated",
                                                                        "Venous thrombosis event - Primary position events - Pre-vaccination",
                                                                        "Venous thrombosis event - Primary position events - Vaccinated",
                                                                        "Venous thrombosis event - Primary position events - Unvaccinated"))




names <- c(
  "Arterial thrombosis event - Pre-vaccination" = "Pre-vaccination (Jan 1 2020 - Dec 14 2021)
  ",
  "Arterial thrombosis event - Vaccinated" = "Vaccinated (Jun 1 2021 - Dec 14 2021)
  Arterial thrombosis event",
  "Arterial thrombosis event - Unvaccinated" = "Unvaccinated (Jun 1 2021 - Dec 14 2021)
  ",
  "Venous thrombosis event - Pre-vaccination" = "",
  "Venous thrombosis event - Vaccinated" = "Venous thrombosis event",
  "Venous thrombosis event - Unvaccinated" = "",
  "Arterial thrombosis event - Primary position events - Pre-vaccination" = "Pre-vaccination (Jan 1 2020 - Dec 14 2021)
  ",
  "Arterial thrombosis event - Primary position events - Vaccinated" = "Vaccinated (Jun 1 2021 - Dec 14 2021)
  Arterial thrombosis event - Primary position events",
  "Arterial thrombosis event - Primary position events - Unvaccinated" = "Unvaccinated (Jun 1 2021 - Dec 14 2021)
  ",
  "Venous thrombosis event - Primary position events - Pre-vaccination" = "",
  "Venous thrombosis event - Primary position events - Vaccinated" = "Venous thrombosis event - Primary position events" ,
  "Venous thrombosis event - Primary position events - Unvaccinated" = "")

# Filter out days 197 and over
lifetables <- lifetables %>% filter(days <= 196)

# Change days to weeks
lifetables$weeks <- lifetables$days /7

#Plot for any position events using reduced time periods and overall HRs


df=lifetables %>% filter(!str_detect(event, "primary_position") & time_points == "reduced")


agegroup_levels <-c()
for(i in c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110","Combined")){
  levels_available <- unique(df$agegroup)
  if(i %in% levels_available){
    agegroup_levels <- append(agegroup_levels,i)
  }
}
  
df$agegroup <- factor(df$agegroup, levels=agegroup_levels)

#Set sex levels as factor
sex_levels <-c()
for(i in c("Sex: Male","Sex: Female")){
  levels_available <- unique(df$sex)
  if(i %in% levels_available){
    sex_levels <- append(sex_levels,i)
  }
}

df$sex <- factor(df$sex, levels=sex_levels)

#Set colour levels as factor
colour_levels <-c()
for(i in c("#006d2c","#31a354","#74c476","#bae4b3","#000000")){
  levels_available <- unique(df$colour)
  if(i %in% levels_available){
    colour_levels <- append(colour_levels,i)
  }
} 
df$colour <- factor(df$colour, levels=colour_levels)

#Set linetype levels as factor
linetype_levels <-c()
for(i in c("solid","dotted")){
  levels_available <- unique(df$linetype)
  if(i %in% levels_available){
    linetype_levels <- append(linetype_levels,i)
  }
} 
df$linetype <- factor(df$linetype, levels=linetype_levels)

  
#Test to see error bars as in dummy data the CI is too small so can't see it
#df$CIp.low<-df$AERp - 0.02
#df$CIp.high<-df$AERp + 0.02

df$x_min <- 0
df$x_max <- NA
df$x_max <- ifelse(df$cohort == "pre_vaccination",28,df$x_max)
df$x_max <- ifelse(df$cohort != "pre_vaccination",28,df$x_max)
  
ggplot2::ggplot(data = df, 
                mapping = ggplot2::aes(x = weeks, y = excess_risk, color = agegroup, shape = agegroup, fill = agegroup, linetype = sex)) +
  #ggplot2::geom_hline(colour = "#A9A9A9") +
  #geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
  ggplot2::geom_line() +
  #ggplot2::scale_x_continuous(lim = c(0,round_any(max(df$weeks, na.rm = T),10, f= ceiling)), breaks = seq(0,round_any(max(df$weeks, na.rm = T),10, f= ceiling),10))+ 
  ggplot2::scale_x_continuous(breaks=c(0,14,28))+ 
  ggplot2::scale_y_continuous(lim = c(0,ceiling(max(df$excess_risk, na.rm = T))), breaks = seq(0,ceiling(max(df$excess_risk, na.rm = T)),1))+ 
  ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
  ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
  ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$sex))+
  ggplot2::labs(x = "Weeks since COVID-19 diagnosis", y = "Cumulative difference in absolute risk  (%)") +
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
                 plot.title = element_text(hjust = 0.5),
                 text=element_text(size=13))+
  ggplot2::facet_wrap(~ grouping_name,labeller=as_labeller(names), ncol = 3, scales = "free_x") +
  geom_blank(aes(x = x_min)) +
  geom_blank(aes(x = x_max))

ggsave(paste0(output_dir, "/Figure_4.pdf"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)

  

#--------------Option 1: Indivdual plots for each outcome and cohort------------
# 
# outcome_position="any_position"
# time_points_of_interest="reduced"
# 
# 
# for(outcome_position in c("any_position","primary_position")){
#   for (time_points_of_interest in c("reduced","normal")) {
#     
#     
#     
#     if(nrow(df)>0){
#       #Set agegroup levels as factor
#       agegroup_levels <-c()
#       for(i in c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110")){
#         levels_available <- unique(df$agegroup)
#         if(i %in% levels_available){
#           agegroup_levels <- append(agegroup_levels,i)
#         }
#       }
#       
#       df$agegroup <- factor(df$agegroup, levels=agegroup_levels)
#       
#       #Set sex levels as factor
#       sex_levels <-c()
#       for(i in c("Sex: Male","Sex: Female")){
#         levels_available <- unique(df$sex)
#         if(i %in% levels_available){
#           sex_levels <- append(sex_levels,i)
#         }
#       }
#       
#       df$sex <- factor(df$sex, levels=sex_levels)
#       
#       #Set colour levels as factor
#       colour_levels <-c()
#       for(i in c("#006d2c","#31a354","#74c476","#bae4b3")){
#         levels_available <- unique(df$colour)
#         if(i %in% levels_available){
#           colour_levels <- append(colour_levels,i)
#         }
#       } 
#       df$colour <- factor(df$colour, levels=colour_levels)
#       
#       #Set linetype levels as factor
#       linetype_levels <-c()
#       for(i in c("solid","dotted")){
#         levels_available <- unique(df$linetype)
#         if(i %in% levels_available){
#           linetype_levels <- append(linetype_levels,i)
#         }
#       } 
#       df$linetype <- factor(df$linetype, levels=linetype_levels)
#       
#       
#       #Test to see error bars as in dummy data the CI is too small so can't see it
#       #df$CIp.low<-df$AERp - 0.02
#       #df$CIp.high<-df$AERp + 0.02
#       
#       df$x_min <- 0
#       df$x_max <- NA
#       df$x_max <- ifelse(df$cohort == "Pre-vaccination (2020-01-01 - 2021-06-18)",80,df$x_max)
#       df$x_max <- ifelse(df$cohort != "Pre-vaccination (2020-01-01 - 2021-06-18)",30,df$x_max)
#       
#       ggplot2::ggplot(data = df, 
#                       mapping = ggplot2::aes(x = weeks, y = excess_risk_main, color = agegroup, shape = agegroup, fill = agegroup, linetype = sex)) +
#         #ggplot2::geom_hline(colour = "#A9A9A9") +
#         #geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
#         ggplot2::geom_line() +
#         #ggplot2::scale_x_continuous(lim = c(0,round_any(max(df$weeks, na.rm = T),10, f= ceiling)), breaks = seq(0,round_any(max(df$weeks, na.rm = T),10, f= ceiling),10))+ 
#         ggplot2::scale_y_continuous(lim = c(0,round_any(max(df$excess_risk_main, na.rm = T),1, f= ceiling)), breaks = seq(0,round_any(max(df$excess_risk_main, na.rm = T),1, f= ceiling),1))+ 
#         ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
#         ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
#         ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$sex))+
#         ggplot2::labs(x = "Weeks since COVID-19 diagnosis", y = "Cumulative difference in absolute risk  (%)") +
#         ggplot2::ggtitle(paste0(outcome_position, " outcomes; ", time_points_of_interest, " time periods; overall HRs")) +
#         ggplot2::guides(fill=ggplot2::guide_legend(ncol = 6, byrow = TRUE)) +
#         ggplot2::theme_minimal() +
#         ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
#                        panel.grid.minor = ggplot2::element_blank(),
#                        panel.spacing.x = ggplot2::unit(0.5, "lines"),
#                        panel.spacing.y = ggplot2::unit(0, "lines"),
#                        legend.key = ggplot2::element_rect(colour = NA, fill = NA),
#                        legend.title = ggplot2::element_blank(),
#                        legend.position="bottom",
#                        plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
#                        plot.title = element_text(hjust = 0.5))+
#         ggplot2::facet_wrap(~ grouping_name,labeller=as_labeller(names), ncol = 3, scales = "free_x") +
#         geom_blank(aes(x = x_min)) +
#         geom_blank(aes(x = x_max))
#       
#       ggsave(paste0(aer_output_dir, "/figure_4_",outcome_position,"_", time_points_of_interest, "_time_periods_using_overall_HRs.png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
#       
#     }
#   }
# }
# 
# 
# for(outcome_position in c("any_position","primary_position")){
#   for (time_points_of_interest in c("reduced","normal")) {
#     
#     if(outcome_position == "any_position"){
#       df=lifetables_subgroup %>% filter(!str_detect(event, "primary_position") & time_points == time_points_of_interest )
#     }else{
#       df=lifetables_subgroup %>% filter(str_detect(event, "primary_position") & time_points == time_points_of_interest )
#     }
#     
#     if(nrow(df)>0){
#       #Set agegroup levels as factor
#       agegroup_levels <-c()
#       for(i in c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110")){
#         levels_available <- unique(df$agegroup)
#         if(i %in% levels_available){
#           agegroup_levels <- append(agegroup_levels,i)
#         }
#       }
#       
#       df$agegroup <- factor(df$agegroup, levels=agegroup_levels)
#       
#       #Set sex levels as factor
#       sex_levels <-c()
#       for(i in c("Sex: Male","Sex: Female")){
#         levels_available <- unique(df$sex)
#         if(i %in% levels_available){
#           sex_levels <- append(sex_levels,i)
#         }
#       }
#       
#       df$sex <- factor(df$sex, levels=sex_levels)
#       
#       #Set colour levels as factor
#       colour_levels <-c()
#       for(i in c("#006d2c","#31a354","#74c476","#bae4b3")){
#         levels_available <- unique(df$colour)
#         if(i %in% levels_available){
#           colour_levels <- append(colour_levels,i)
#         }
#       } 
#       df$colour <- factor(df$colour, levels=colour_levels)
#       
#       #Set linetype levels as factor
#       linetype_levels <-c()
#       for(i in c("solid","dotted")){
#         levels_available <- unique(df$linetype)
#         if(i %in% levels_available){
#           linetype_levels <- append(linetype_levels,i)
#         }
#       } 
#       df$linetype <- factor(df$linetype, levels=linetype_levels)
#       
#       
#       #Test to see error bars as in dummy data the CI is too small so can't see it
#       #df$CIp.low<-df$AERp - 0.02
#       #df$CIp.high<-df$AERp + 0.02
#       
#       df$x_min <- 0
#       df$x_max <- NA
#       df$x_max <- ifelse(df$cohort == "Pre-vaccination (2020-01-01 - 2021-06-18)",80,df$x_max)
#       df$x_max <- ifelse(df$cohort != "Pre-vaccination (2020-01-01 - 2021-06-18)",30,df$x_max)
#       
#       ggplot2::ggplot(data = df, 
#                       mapping = ggplot2::aes(x = weeks, y = excess_risk_subgroup, color = agegroup, shape = agegroup, fill = agegroup, linetype = sex)) +
#         #ggplot2::geom_hline(colour = "#A9A9A9") +
#         #geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
#         ggplot2::geom_line() +
#         #ggplot2::scale_x_continuous(lim = c(0,round_any(max(df$weeks, na.rm = T),10, f= ceiling)), breaks = seq(0,round_any(max(df$weeks, na.rm = T),10, f= ceiling),10))+ 
#         ggplot2::scale_y_continuous(lim = c(0,round_any(max(df$excess_risk_subgroup, na.rm = T),1, f= ceiling)), breaks = seq(0,round_any(max(df$excess_risk_subgroup, na.rm = T),1, f= ceiling),1))+ 
#         ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
#         ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
#         ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$sex))+
#         ggplot2::labs(x = "Weeks since COVID-19 diagnosis", y = "Cumulative difference in absolute risk  (%)") +
#         ggplot2::ggtitle(paste0(outcome_position, " outcomes; ", time_points_of_interest, " time periods; age/sex specific HRs")) +
#         ggplot2::guides(fill=ggplot2::guide_legend(ncol = 6, byrow = TRUE)) +
#         ggplot2::theme_minimal() +
#         ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
#                        panel.grid.minor = ggplot2::element_blank(),
#                        panel.spacing.x = ggplot2::unit(0.5, "lines"),
#                        panel.spacing.y = ggplot2::unit(0, "lines"),
#                        legend.key = ggplot2::element_rect(colour = NA, fill = NA),
#                        legend.title = ggplot2::element_blank(),
#                        legend.position="bottom",
#                        plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
#                        plot.title = element_text(hjust = 0.5))+
#         ggplot2::facet_wrap(~ outcome + cohort, ncol = 3, scales = "free_x") +
#         geom_blank(aes(x = x_min)) +
#         geom_blank(aes(x = x_max))
#       
#       ggsave(paste0(aer_output_dir, "/figure_4_",outcome_position,"_", time_points_of_interest, "_time_periods_using_age_sex_subgroup_HRs.png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
#     }
#   }
# }



