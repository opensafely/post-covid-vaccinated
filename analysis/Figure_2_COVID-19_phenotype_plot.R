library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
project = args[[1]]#vaccinated, electively_unvaccinated

# Load fully adjusted COVID phenotype results------------------------------------
project="vaccinated"

if(project == "vaccinated"){
  subgroup_pheno_hr=read_csv("output/compiled_HR_results_subgroup_covid_pheno_vaccinated_delta_mdl_max_adj_covid_history_false.csv")
}else if(project == "electively_unvaccinated"){
  subgroup_pheno_hr=read_csv("output/compiled_HR_results_subgroup_covid_pheno_electively_vaccinated_delta_mdl_max_adj_covid_history_false.csv")
  
}
subgroup_pheno_hr=subgroup_pheno_hr%>%select(term,estimate,conf.low,conf.high,std.error,robust.se,expo_week,events_total,event,strata)
#subgroup_pheno_hr=subgroup_pheno_hr%>%filter(event %in% c("ami","pe"))
# Select HRs for time periods----------------------------------------------------

subgroup_pheno_hr <- subgroup_pheno_hr %>% filter(str_detect(term, "^days"))

# Where to plot the time period plots along the x-axis---------------------------

#In days since COVID
# term_to_time <- data.frame(term = c("days0_14","days14_28","days28_56","days56_84","days84_196",
#                                    "days0_28","days28_196"),
#                           time = c(7,21,42,70,140,
#                                    14,112))

#In weeks since COVID
#weeks0_2, weeks2_4, weeks4_8, weeks8_12, weeks12_28
#weeks0_4, weeks4_28
term_to_time <- data.frame(term = c("days0_14","days14_28","days28_56","days56_84","days84_196",
                                   "days0_28","days28_196"),
                          time = c(1,3,6,10,20,
                                   2,16))

subgroup_pheno_hr <- merge(subgroup_pheno_hr, term_to_time, by = c("term"), all.x = TRUE)

# Rename strata to 'nice' format------------------------------------------------

subgroup_pheno_hr$strata <- ifelse(subgroup_pheno_hr$strata=="expo_pheno_non_hospitalised","Non-hospitalised COVID-19",subgroup_pheno_hr$strata)
subgroup_pheno_hr$strata <- ifelse(subgroup_pheno_hr$strata=="expo_pheno_hospitalised","Hospitalised COVID-19",subgroup_pheno_hr$strata)

# Specify line colours----------------------------------------------------------

subgroup_pheno_hr$colour <- ""
subgroup_pheno_hr$colour <- ifelse(subgroup_pheno_hr$strata=="Non-hospitalised COVID-19","#fb9a99",subgroup_pheno_hr$colour)
subgroup_pheno_hr$colour <- ifelse(subgroup_pheno_hr$strata=="Hospitalised COVID-19","#e31a1c",subgroup_pheno_hr$colour)


# Make event names 'nice' ------------------------------------------------------

subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="ami","Acute myocardial infarction",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="tia","Transient ischaemic attack",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="dvt","Deep vein thrombosis",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="hf","Heart failure",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="stroke_isch","Ischaemic stroke",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="angina","Angina",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="vte","Venous thromboembolism",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="pe","Pulmonary embolism",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="stroke_sah_hs","Subarachnoid haemorrhage and haemorrhagic stroke",subgroup_pheno_hr$event)
subgroup_pheno_hr$event <- ifelse(subgroup_pheno_hr$event=="ate","Arterial thromboses",subgroup_pheno_hr$event)

# Which events to run
# Only run for events that have results for both hospitalised and non-hospitalised COVID

outcome_names=unique(subgroup_pheno_hr$event)
events_to_plot=c()
for(outcome in outcome_names){
  df=subgroup_pheno_hr%>%filter(event==outcome)
  number_of_subgroups=length(unique(df$strata))
  if(number_of_subgroups==2){
    events_to_plot=append(events_to_plot,outcome)
  }else{
    not_plot=append(not_plot,outcome)
  }
}

subgroup_pheno_hr=subgroup_pheno_hr%>%filter(event %in%events_to_plot)

# Factor variables for ordering-------------------------------------------------

subgroup_pheno_hr$strata <- factor(subgroup_pheno_hr$strata, levels=c("Hospitalised COVID-19",
                                                              "Non-hospitalised COVID-19")) 

subgroup_pheno_hr$colour <- factor(subgroup_pheno_hr$colour, levels=c("#e31a1c",
                                                              "#fb9a99"))


event_levels_order=c("Acute myocardial infarction", "Ischaemic stroke","Pulmonary embolism","Deep vein thrombosis",
                     "Transient ischaemic attack","Subarachnoid haemorrhage and haemorrhagic stroke","Heart failure",
                     "Angina","Arterial thromboses","Venous thromboembolism")

event_levels_order=event_levels_order[event_levels_order %in% events_to_plot]

subgroup_pheno_hr$event <- factor(subgroup_pheno_hr$event, levels=event_levels_order)

# Plot figures

min_plot=0.25
max_plot=64
if(length(events_to_plot)>0){
  ggplot2::ggplot(data =subgroup_pheno_hr,
                  mapping = ggplot2::aes(x = time, y = estimate, color = strata, shape=strata, fill=strata)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<min_plot,min_plot,conf.low), 
                                                  ymax = ifelse(conf.high>max_plot,max_plot,conf.high),  
                                                  width = 0), 
                           position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::scale_y_continuous(lim = c(0.01,64), breaks = c(0.25,0.5,1,2,4,8,16,32,64), trans = "log") +
    #ggplot2::scale_x_continuous(lim = c(0,196), breaks = seq(0,196,28)) +
    ggplot2::scale_x_continuous(lim = c(0,28), breaks = seq(0,28,4)) +
    ggplot2::scale_fill_manual(values = levels(subgroup_pheno_hr$colour), labels = levels(subgroup_pheno_hr$strata)) +
    ggplot2::scale_color_manual(values = levels(subgroup_pheno_hr$colour), labels = levels(subgroup_pheno_hr$strata)) +
    ggplot2::scale_shape_manual(values = c(21,22), labels = levels(subgroup_pheno_hr$strata)) +
    #ggplot2::labs(x = "\nDays since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 2, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   #legend.position = c(10, 0),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"))+ 
    ggplot2::facet_wrap(event~., ncol = 2)
  
  
  ggplot2::ggsave(paste0("output/figure2_COVID_phenotype_plot_y_lim.png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)
  
}else if(events_to_plot==0){
    df <- data.frame()
    ggplot2::ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
    ggplot2::ggsave(paste0("output/figure2_COVID_phenotype_weeks.png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)
    
}

