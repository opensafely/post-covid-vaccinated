library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
project = args[[1]]#vaccinated, electively_unvaccinated

# Load fully adjusted COVID phenotype results------------------------------------
project="vaccinated"

if(project == "vaccinated"){
  subgroup_age_HR=read_csv("output/compiled_HR_results_subgroup_agegp_vaccinated_delta_mdl_max_adj_covid_history_false.csv")
  subgroup_sex_HR=read_csv("output/compiled_HR_results_subgroup_sex_vaccinated_delta_mdl_max_adj_covid_history_false.csv")
  subgroup_ethnicity_HR=read_csv("output/compiled_HR_results_subgroup_ethnicity_vaccinated_delta_mdl_max_adj_covid_history_false.csv")
  subgroup_prior_history_HR=read_csv("output/compiled_HR_results_subgroup_prior_history_vaccinated_delta_mdl_max_adj_covid_history_false.csv")
}else if(project == "electively_unvaccinated"){
  subgroup_age_HR=read_csv("output/compiled_HR_results_subgroup_agegp_electively_unvaccinated_delta_mdl_max_adj_covid_history_false.csv")
  subgroup_sex_HR=read_csv("output/compiled_HR_results_subgroup_sex_electively_unvaccinated_delta_mdl_max_adj_covid_history_false.csv")
  subgroup_ethnicity_HR=read_csv("output/compiled_HR_results_subgroup_ethnicity_electively_unvaccinated_delta_mdl_max_adj_covid_history_false.csv")
  subgroup_prior_history_HR=read_csv("output/compiled_HR_results_subgroup_prior_history_electively_unvaccinated_delta_mdl_max_adj_covid_history_false.csv")
}

#Combine all results

combined_subgroup_HR=rbind(subgroup_age_HR,subgroup_sex_HR,subgroup_ethnicity_HR,subgroup_prior_history_HR)
combined_subgroup_HR=combined_subgroup_HR%>%select(term,estimate,conf.low,conf.high,std.error,robust.se,expo_week,events_total,event,strata)


# Select HRs for time periods----------------------------------------------------

combined_subgroup_HR <- combined_subgroup_HR %>% filter(str_detect(term, "^days"))

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

combined_subgroup_HR <- merge(combined_subgroup_HR, term_to_time, by = c("term"), all.x = TRUE)

# Rename strata to 'nice' format------------------------------------------------

combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="agegp_18_39","Age group: 18-39",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="agegp_40_59","Age group: 40-59",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="agegp_60_79","Age group: 60-79",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="agegp_80_110","Age group: 80-110",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="SEX_M","Sex: Male",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="SEX_F","Sex: Female",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="SEX_F","Sex: Female",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="cov_cat_ethnicity_1","Ethnicity: White",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="cov_cat_ethnicity_2","Ethnicity: Mixed",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="cov_cat_ethnicity_3","Ethnicity: South Asian",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="cov_cat_ethnicity_4","Ethnicity: Black",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(combined_subgroup_HR$strata=="cov_cat_ethnicity_5","Ethnicity: Other Ethnic Groups",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(endsWith(combined_subgroup_HR$strata,"FALSE")==T,"No prior history of event",combined_subgroup_HR$strata)
combined_subgroup_HR$strata <- ifelse(endsWith(combined_subgroup_HR$strata,"TRUE")==T,"Prior history of event",combined_subgroup_HR$strata)

# Give ethnicity estimates extra space -----------------------------------------

combined_subgroup_HR$time <- ifelse(combined_subgroup_HR$strata=="Ethnicity: South Asian", combined_subgroup_HR$time-0.5, combined_subgroup_HR$time)
combined_subgroup_HR$time <- ifelse(combined_subgroup_HR$strata=="Ethnicity: Other Ethnic Groups", combined_subgroup_HR$time-1.0, combined_subgroup_HR$time)
combined_subgroup_HR$time <- ifelse(combined_subgroup_HR$strata=="Ethnicity: Mixed", combined_subgroup_HR$time+0.5, combined_subgroup_HR$time)
combined_subgroup_HR$time <- ifelse(combined_subgroup_HR$strata=="Ethnicity: Black", combined_subgroup_HR$time+1.0, combined_subgroup_HR$time)

# Specify line colours ---------------------------------------------------------

combined_subgroup_HR$colour <- ""
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Age group: 18-39","#006d2c",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Age group: 40-59","#31a354",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Age group: 60-79","#74c476",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Age group: 80-110","#bae4b3",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Sex: Male","#cab2d6",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Sex: Female","#6a3d9a",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Ethnicity: White","#08519c",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Ethnicity: Black","#2171b5",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Ethnicity: South Asian","#4292c6",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Ethnicity: Other Ethnic Groups","#6baed6",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Ethnicity: Mixed","#9ecae1",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="Prior history of event","#ff7f00",combined_subgroup_HR$colour)
combined_subgroup_HR$colour <- ifelse(combined_subgroup_HR$strata=="No prior history of event","#fdbf6f",combined_subgroup_HR$colour)

# Make event names 'nice' ------------------------------------------------------

combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="ami","Acute myocardial infarction",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="tia","Transient ischaemic attack",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="dvt","Deep vein thrombosis",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="hf","Heart failure",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="stroke_isch","Ischaemic stroke",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="angina","Angina",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="vte","Venous thromboembolism",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="pe","Pulmonary embolism",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="stroke_sah_hs","Subarachnoid haemorrhage and haemorrhagic stroke",combined_subgroup_HR$event)
combined_subgroup_HR$event <- ifelse(combined_subgroup_HR$event=="ate","Arterial thromboses",combined_subgroup_HR$event)

#Add in subgroup name-----------------------------------------------------------

combined_subgroup_HR$subgroup=""
combined_subgroup_HR$subgroup=ifelse(startsWith(combined_subgroup_HR$strata,"Age")==T,"Age group",combined_subgroup_HR$subgroup)
combined_subgroup_HR$subgroup=ifelse(startsWith(combined_subgroup_HR$strata,"Sex")==T,"Sex",combined_subgroup_HR$subgroup)
combined_subgroup_HR$subgroup=ifelse(startsWith(combined_subgroup_HR$strata,"Ethnicity")==T,"Ethnicity",combined_subgroup_HR$subgroup)
combined_subgroup_HR$subgroup=ifelse(endsWith(combined_subgroup_HR$strata,"event")==T,"Prior history of event",combined_subgroup_HR$subgroup)


# Give age estimates extra space -----------------------------------------

combined_subgroup_HR$time <- ifelse(combined_subgroup_HR$strata=="Age group: 40-59", combined_subgroup_HR$time-0.25, combined_subgroup_HR$time)
combined_subgroup_HR$time <- ifelse(combined_subgroup_HR$strata=="Age group: 60-79", combined_subgroup_HR$time-0.5, combined_subgroup_HR$time)
combined_subgroup_HR$time <- ifelse(combined_subgroup_HR$strata=="Age group: 80-110", combined_subgroup_HR$time+0.25, combined_subgroup_HR$time)




# Factor variables for ordering-------------------------------------------------

combined_subgroup_HR$subgroup <- factor(combined_subgroup_HR$subgroup, levels=c("Prior history of event",
                                                                                "Age group",
                                                                                "Sex",
                                                                                "Ethnicity")) 


# combined_subgroup_HR$strata <- factor(combined_subgroup_HR$strata, levels=c("Prior history of event",
#                                           "No prior history of event",
#                                           "Age group: 18-39",
#                                           "Age group: 40-59",
#                                           "Age group: 60-79",
#                                           "Age group: 80-110",
#                                           "Sex: Female",
#                                           "Sex: Male",
#                                           "Ethnicity: White",
#                                           "Ethnicity: Black",
#                                           "Ethnicity: South Asian",
#                                           "Ethnicity: Other Ethnic Groups",
#                                           "Ethnicity: Mixed"))
# 
# combined_subgroup_HR$colour <- factor(combined_subgroup_HR$colour, levels=c("#ff7f00",
#                                         "#fdbf6f",
#                                         "#006d2c",
#                                         "#31a354",
#                                         "#74c476",
#                                         "#bae4b3",
#                                         "#6a3d9a",
#                                         "#cab2d6",
#                                         "#08519c",
#                                         "#2171b5",
#                                         "#4292c6",
#                                         "#6baed6",
#                                         "#9ecae1")) 
# 
combined_subgroup_HR$event <- factor(combined_subgroup_HR$event, levels=c("Acute myocardial infarction",
                                                                          "Ischaemic stroke",
                                                                          "Pulmonary embolism",
                                                                          "Deep vein thrombosis",
                                                                          "Transient ischaemic attack",
                                                                          "Subarachnoid haemorrhage and haemorrhagic stroke",
                                                                          "Heart failure",
                                                                          "Angina",
                                                                          "Arterial thromboses",
                                                                          "Venous thromboembolism"))

# Plot figures------------------------------------------------------------------
event_names=unique(combined_subgroup_HR$event)
event_names="Acute myocardial infarction"
event="Acute myocardial infarction"


df=combined_subgroup_HR[combined_subgroup_HR$event==event,]

subgroup_levels=c()
colour_levels=c()
event_strata=unique(df$strata)
if("Prior history of event" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Prior history of event")
  colour_levels=append(colour_levels,"#ff7f00")
}
if("No prior history of event" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"No prior history of event")
  colour_levels=append(colour_levels,"#fdbf6f")
}
if("Age group: 18-39" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Age group: 18-39")
  colour_levels=append(colour_levels,"#006d2c")
}
if("Age group: 40-59" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Age group: 40-59")
  colour_levels=append(colour_levels,"#31a354")
}
if("Age group: 60-79" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Age group: 60-79")
  colour_levels=append(colour_levels,"#74c476")
}
if("Age group: 80-110" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Age group: 80-110")
  colour_levels=append(colour_levels,"#bae4b3")
}
if("Sex: Female" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Sex: Female")
  colour_levels=append(colour_levels,"#6a3d9a")
}
if("Sex: Male" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Sex: Male")
  colour_levels=append(colour_levels,"#cab2d6")
}
if("Ethnicity: White" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Ethnicity: White")
  colour_levels=append(colour_levels,"#08519c")
}
if("Ethnicity: Black" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Ethnicity: Black")
  colour_levels=append(colour_levels,"#2171b5")
}
if("Ethnicity: South Asian" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Ethnicity: South Asian")
  colour_levels=append(colour_levels,"#4292c6")
}
if("Ethnicity: Other Ethnic Groups" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Ethnicity: Other Ethnic Groups")
  colour_levels=append(colour_levels,"#6baed6")
}
if("Ethnicity: Mixed" %in% event_strata){
  subgroup_levels=append(subgroup_levels,"Ethnicity: Mixed")
  colour_levels=append(colour_levels,"#9ecae1")
}

combined_subgroup_HR$strata <- factor(combined_subgroup_HR$strata, levels=subgroup_levels)
combined_subgroup_HR$colour <- factor(combined_subgroup_HR$colour, levels=colour_levels)

min_plot <- 0.25
max_plot <- 3

ggplot2::ggplot(data = combined_subgroup_HR[combined_subgroup_HR$event==event,], 
                mapping = ggplot2::aes(x = time, y = estimate, color = strata, shape = strata, fill = strata)) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5))+
  ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<min_plot,min_plot,conf.low), 
                                                ymax = ifelse(conf.high>max_plot,max_plot,conf.high),  
                                                width = 0), 
                         position = ggplot2::position_dodge(width = 1))+
  ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.5)) +
  #ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.25,0.5,1,2,4,8,16,32,64), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,28), breaks = seq(0,28,4)) +
  ggplot2::scale_fill_manual(values = levels(combined_subgroup_HR$colour), labels = levels(combined_subgroup_HR$strata))+ 
  ggplot2::scale_color_manual(values = levels(combined_subgroup_HR$colour), labels = levels(combined_subgroup_HR$strata)) +
  ggplot2::scale_shape_manual(values = c(rep(21,13)), labels = levels(combined_subgroup_HR$strata)) +
  #    ggplot2::scale_shape_manual(values = c(rep(c(21,22),4),23,24,rep(c(21,22),2),23,24,25), labels = levels(combined_subgroup_HR$strata)) + 
  ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 4, byrow = FALSE)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0.5, "lines"),
                 panel.spacing.y = ggplot2::unit(0, "lines"),
                 legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                 legend.title = ggplot2::element_blank(),
                 legend.position="bottom",
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +
  ggplot2::facet_wrap(subgroup~.,ncol=2)

ggplot2::ggsave(paste0("output/age_sex__ethnicity_prior_history_weeks_figures.png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)



for (event in event_names) {
  
  ggplot2::ggplot(data = combined_subgroup_HR[combined_subgroup_HR$event==event,], 
                  mapping = ggplot2::aes(x = time, y = estimate, color = strata, shape = strata, fill = strata)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<min_plot,min_plot,conf.low), 
                                                  ymax = ifelse(conf.high>max_plot,max_plot,conf.high),  
                                                  width = 0), 
                           position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.25,0.5,1,2,4,8,16,32,64), trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,44), breaks = seq(0,28,4)) +
    ggplot2::scale_fill_manual(values = levels(combined_subgroup_HR$colour), labels = levels(combined_subgroup_HR$strata)) +
    ggplot2::scale_color_manual(values = levels(combined_subgroup_HR$colour), labels = levels(combined_subgroup_HR$strata)) +
    ggplot2::scale_shape_manual(values = c(rep(c(21,22),4),23,24,rep(c(21,22),2),23,24,25), labels = levels(combined_subgroup_HR$strata)) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=2,byrow=FALSE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +
    ggplot2::facet_wrap(subgroup~.)
  
  #ggplot2::ggsave(paste0("output/ccu002_01_main_figures_2_3_",tolower(gsub("_event","",event)),".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
  
}

