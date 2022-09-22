# Plots Figure 4 - AER

library(purrr)
library(data.table)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(plyr)

aer_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/compiled_results/"

event_of_interest <- c("ate","vte","ate_primary_position","vte_primary_position")
cohort_name <- c("pre_vaccination", "vaccinated","electively_unvaccinated")

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses$outcome_variable <- gsub("out_date_","",active_analyses$outcome_variable)

#Load data
lifetables <- readr::read_csv(paste0(aer_output_dir,"/AER_compiled_results.csv"))

#???? Do we need to do this ?????
lifetables$excess_risk_main <- lifetables$excess_risk_main *100
lifetables$excess_risk_subgroup <- lifetables$excess_risk_subgroup *100
#-------------------------Make event names 'nice' ------------------------------
lifetables <- lifetables %>% left_join(active_analyses %>% select(outcome, outcome_variable), by = c("event"="outcome_variable"))

#-------------------------------Format group names--------------------------
lifetables$agegroup <- NA
lifetables$agegroup <- ifelse(grepl("18_39",lifetables$subgroup),"Age group: 18-39",lifetables$agegroup)
lifetables$agegroup <- ifelse(grepl("40_59",lifetables$subgroup),"Age group: 40-59",lifetables$agegroup)
lifetables$agegroup <- ifelse(grepl("60_79",lifetables$subgroup),"Age group: 60-79",lifetables$agegroup)
lifetables$agegroup <- ifelse(grepl("80_110",lifetables$subgroup),"Age group: 80-110",lifetables$agegroup)

lifetables$sex <- NA
lifetables$sex <- ifelse(grepl("Female",lifetables$subgroup),"Sex: Female",lifetables$sex)
lifetables$sex <- ifelse(grepl("Male",lifetables$subgroup),"Sex: Male",lifetables$sex)

# Specify line colours ---------------------------------------------------------

lifetables$colour <- NA
lifetables$colour <- ifelse(lifetables$agegroup=="Age group: 18-39","#006d2c",lifetables$colour)
lifetables$colour <- ifelse(lifetables$agegroup=="Age group: 40-59","#31a354",lifetables$colour)
lifetables$colour <- ifelse(lifetables$agegroup=="Age group: 60-79","#74c476",lifetables$colour)
lifetables$colour <- ifelse(lifetables$agegroup=="Age group: 80-110","#bae4b3",lifetables$colour)

# Specify line types ---------------------------------------------------------
lifetables$linetype <- NA
lifetables$linetype <- ifelse(lifetables$sex=="Sex: Male","solid",lifetables$linetype)
lifetables$linetype <- ifelse(lifetables$sex=="Sex: Female","dotted",lifetables$linetype)

#Filter the lifatbles to non-NA results split by using the HRs from the overall results (hr_main)
# & using the age/sex HRs (hr_subgroup)
lifetables_main <- lifetables %>% filter(!is.na(hr_main))
lifetables_subgroup <- lifetables %>% filter(!is.na(hr_subgroup))

#--------------Option 1: Indivdual plots for each outcome and cohort------------

for(cohort_of_interest in cohort_name){
  for(outcome_position in c("any_position","primary_position")){
    
    if(outcome_position == "any_position"){
      df=lifetables_main %>% filter(cohort == cohort_of_interest & !str_detect(event, "primary_position") )
    }else{
      df=lifetables_main %>% filter(cohort == cohort_of_interest & str_detect(event, "primary_position") )
    }
    
    #Set agegroup levels as factor
    agegroup_levels <-c()
    for(i in c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110")){
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
    for(i in c("#006d2c","#31a354","#74c476","#bae4b3")){
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
    
    ggplot2::ggplot(data = df, 
                    mapping = ggplot2::aes(x = days, y = excess_risk_main, color = agegroup, shape = agegroup, fill = agegroup, linetype = sex)) +
      #ggplot2::geom_hline(colour = "#A9A9A9") +
      #geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(lim = c(0,round_any(max(df$days, na.rm = T),20, f= ceiling)), breaks = seq(0,round_any(max(df$days, na.rm = T),20, f= ceiling),20))+ 
      ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
      ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
      ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$sex))+
      ggplot2::labs(x = "Days since COVID-19 diagnosis", y = "Cumulative difference in absolute risk  (%)") +
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
                     plot.title = element_text(hjust = 0.5))+
      ggplot2::facet_wrap(outcome~., ncol = 2)
    
    ggsave(paste0(aer_output_dir, "/figure_4_",cohort_of_interest,"_",outcome_position, "_using_overall_HRs.png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
  }
}


for(cohort_of_interest in cohort_name){
  for(outcome_position in c("any_position","primary_position")){
    
    if(outcome_position == "any_position"){
      df=lifetables_subgroup %>% filter(cohort == cohort_of_interest & !str_detect(event, "primary_position") )
    }else{
      df=lifetables_subgroup %>% filter(cohort == cohort_of_interest & str_detect(event, "primary_position") )
    }
    
    #Set agegroup levels as factor
    agegroup_levels <-c()
    for(i in c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110")){
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
    for(i in c("#006d2c","#31a354","#74c476","#bae4b3")){
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
    
    ggplot2::ggplot(data = df, 
                    mapping = ggplot2::aes(x = days, y = excess_risk_main, color = agegroup, shape = agegroup, fill = agegroup, linetype = sex)) +
      #ggplot2::geom_hline(colour = "#A9A9A9") +
      #geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(lim = c(0,round_any(max(df$days, na.rm = T),20, f= ceiling)), breaks = seq(0,round_any(max(df$days, na.rm = T),20, f= ceiling),20)) +
      ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
      ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$agegroup)) +
      ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$sex))+
      ggplot2::labs(x = "Days since COVID-19 diagnosis", y = "Cumulative difference in absolute risk  (%)") +
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
                     plot.title = element_text(hjust = 0.5))+
      ggplot2::facet_wrap(outcome~., ncol = 2)
    
    ggsave(paste0(aer_output_dir, "/figure_4_",cohort_of_interest,"_",outcome_position, "_using_age_sex_subgroup_HRs.png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
  }
}


