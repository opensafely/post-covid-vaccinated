#Project: Vaccinated delta wave population study
#Scripts: Renin Toms, Venexia Walker
#Reviewer: Genevieve Cezard

#TO RUN OUTSIDE OPENSAFELY
# 1. load the right input data and make sure of the file names and variable structure
# 2. Cntrl+A run the whole script and find the .png graph files in working directory

library(purrr)
library(data.table)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

aer_results_dir <- "output/review/AER_results"

event_of_interest <- c("ate","vte")
model_of_interest <- c("mdl_max_adj")
cohort_name <- c("vaccinated","electively_unvaccinated")

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses$outcome_variable <- gsub("out_date_","",active_analyses$outcome_variable)

#Load data
lifetables <- readr::read_csv(paste0(aer_results_dir,"/Figure4_compiled_lifetables.csv"))

#------------------------Filter to outcomes of interest-------------------------
lifetables <- lifetables %>% filter(event %in% event_of_interest & model %in% model_of_interest)

#----------------------Filter to subgroups of interest--------------------------
lifetables <- lifetables %>% filter(str_detect(subgroup, c("^main","^sex","^age")))

#-------------------------Make event names 'nice' ------------------------------
lifetables <- lifetables %>% left_join(active_analyses %>% select(outcome, outcome_variable), by = c("event"="outcome_variable"))

#-------------------------------Format subgroups names--------------------------
lifetables$subgroup <- ifelse(lifetables$subgroup=="main","Combined",lifetables$subgroup)
lifetables$subgroup <- ifelse(lifetables$subgroup=="agegp_18_39","Age group: 18-39",lifetables$subgroup)
lifetables$subgroup <- ifelse(lifetables$subgroup=="agegp_40_59","Age group: 40-59",lifetables$subgroup)
lifetables$subgroup <- ifelse(lifetables$subgroup=="agegp_60_79","Age group: 60-79",lifetables$subgroup)
lifetables$subgroup <- ifelse(lifetables$subgroup=="agegp_80_110","Age group: 80-110",lifetables$subgroup)
lifetables$subgroup <- ifelse(lifetables$subgroup=="sex_Male","Sex: Male",lifetables$subgroup)
lifetables$subgroup <- ifelse(lifetables$subgroup=="sex_Female","Sex: Female",lifetables$subgroup)

# Specify line colours ---------------------------------------------------------

lifetables$colour <- ""
lifetables$colour <- ifelse(lifetables$subgroup=="Combined","#000000",lifetables$colour)
lifetables$colour <- ifelse(lifetables$subgroup=="Age group: 18-39","#006d2c",lifetables$colour)
lifetables$colour <- ifelse(lifetables$subgroup=="Age group: 40-59","#31a354",lifetables$colour)
lifetables$colour <- ifelse(lifetables$subgroup=="Age group: 60-79","#74c476",lifetables$colour)
lifetables$colour <- ifelse(lifetables$subgroup=="Age group: 80-110","#bae4b3",lifetables$colour)
lifetables$colour <- ifelse(lifetables$subgroup=="Sex: Male","#cab2d6",lifetables$colour)
lifetables$colour <- ifelse(lifetables$subgroup=="Sex: Female","#6a3d9a",lifetables$colour)

# Specify line types ---------------------------------------------------------

lifetables$line <- ""
lifetables$line <- ifelse(lifetables$subgroup=="Combined","solid",lifetables$line)
lifetables$line <- ifelse(lifetables$subgroup=="Age group: 18-39","dotted",lifetables$line)
lifetables$line <- ifelse(lifetables$subgroup=="Age group: 40-59","dotted",lifetables$line)
lifetables$line <- ifelse(lifetables$subgroup=="Age group: 60-79","dotted",lifetables$line)
lifetables$line <- ifelse(lifetables$subgroup=="Age group: 80-110","dotted",lifetables$line)
lifetables$line <- ifelse(lifetables$subgroup=="Sex: Male","longdash",lifetables$line)
lifetables$line <- ifelse(lifetables$subgroup=="Sex: Female","longdash",lifetables$line)

#--------------Option 1: Indivdual plots for each outcome and cohort------------

for(cohort_of_interest in cohort_name){
  for(outcome_name in event_of_interest){
    df=lifetables %>% filter(cohort == cohort_of_interest & event==outcome_name )
    
    sub_group_levels <-c()
    for(i in c("Combined","Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110",
               "Sex: Female","Sex: Male")){
      levels_available <- unique(df$subgroup)
      if(i %in% levels_available){
        sub_group_levels <- append(sub_group_levels,i)
      }
    }
    
    df$subgroup <- factor(df$subgroup, levels=sub_group_levels)
    
    colour_levels <-c()
    for(i in c("#000000","#006d2c","#31a354","#74c476","#bae4b3","#6a3d9a","#cab2d6")){
      levels_available <- unique(df$colour)
      if(i %in% levels_available){
        colour_levels <- append(colour_levels,i)
      }
    } 
    df$colour <- factor(df$colour, levels=colour_levels)
    
    #Test to see error bars as in dummy data the CI is too small so can't see it
    #df$CIp.low<-df$AERp - 0.02
    #df$CIp.high<-df$AERp + 0.02
    
    plot<-ggplot2::ggplot(data = df, 
                    mapping = ggplot2::aes(x = days, y = AERp, color = subgroup, shape = subgroup, fill = subgroup)) +
      #ggplot2::geom_hline(colour = "#A9A9A9") +
      geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200),limits = c(0,200))+
      ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
      ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
      ggplot2::labs(x = "Days since COVID-19 diagnosis", y = "Cumulative difference in absolute risk  (%)", title =df$outcome[1] ) +
      ggplot2::ggtitle(df$outcome[1])+
      ggplot2::guides(fill=ggplot2::guide_legend(ncol = length(sub_group_levels), byrow = TRUE)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.spacing.x = ggplot2::unit(0.5, "lines"),
                     panel.spacing.y = ggplot2::unit(0, "lines"),
                     legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                     legend.title = ggplot2::element_blank(),
                     legend.position="bottom",
                     plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                     plot.title = element_text(hjust = 0.5)) 

   
 
  #assign(paste0(outcome_name,"_",cohort_of_interest),plot)
  ggsave(paste0(aer_results_dir, "/figure_4_",outcome_name,"_",cohort_of_interest,".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
  }
}

#grid.arrange(ate_vaccinated, vte_vaccinated, nrow = 1)
#grid.arrange(ate_electively_unvaccinated, vte_electively_unvaccinated, nrow = 1)


#-----------Option 2: Plots for each cohort containing both outcomes------------

for(cohort_of_interest in cohort_name){
  df=lifetables %>% filter(cohort == cohort_of_interest)
  
  sub_group_levels <-c()
  for(i in c("Combined","Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110",
             "Sex: Female","Sex: Male")){
    levels_available <- unique(df$subgroup)
    if(i %in% levels_available){
      sub_group_levels <- append(sub_group_levels,i)
    }
  }
  
  df$subgroup <- factor(df$subgroup, levels=sub_group_levels)
  
  colour_levels <-c()
  for(i in c("#000000","#006d2c","#31a354","#74c476","#bae4b3","#6a3d9a","#cab2d6")){
    levels_available <- unique(df$colour)
    if(i %in% levels_available){
      colour_levels <- append(colour_levels,i)
    }
  } 
  df$colour <- factor(df$colour, levels=colour_levels)
  
  df$line <- factor(df$line, levels = c("solid","dotted","longdash"))
  
  #Test to see error bars as in dummy data the CI is too small so can't see it
  #df$CIp.low<-df$AERp - 0.02
  #df$CIp.high<-df$AERp + 0.02
  
  ggplot2::ggplot(data = df, 
                        mapping = ggplot2::aes(x = days, y = AERp, color = subgroup, shape = subgroup, fill = subgroup, linetype=subgroup)) +
    #ggplot2::geom_hline(colour = "#A9A9A9") +
    geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200),limits = c(0,200))+
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
    #Might need to have a think about line types as can't get it to work like colour
    #ggplot2::scale_linetype_manual(values = c("solid","dotted","dotdash","dashed",rep("solid",2)), labels = levels(df$subgroup)) +
    ggplot2::labs(x = "Days since COVID-19 diagnosis", y = "Cumulative difference in absolute risk  (%)") +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = length(sub_group_levels), byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"))+
    ggplot2::facet_wrap(outcome~.,ncol=2)
  
  ggsave(paste0(aer_results_dir, "/figure_4_",cohort_of_interest,".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
}






  

