#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"

#----------------------------Get CVD outcomes-----------------------------------
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

active_analyses <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", "")) %>%
  filter(outcome_variable %in% c("out_date_ate","out_date_vte","out_date_ate_primary_position","out_date_vte_primary_position")) 


active_analyses$outcome <- gsub("Arterial thrombosis event","All arterial thromboses",active_analyses$outcome)
active_analyses$outcome <- gsub("Venous thrombosis event","All venous thromboses",active_analyses$outcome)


active_analyses_pre_vax <- read_rds("lib/active_analyses_pre_vax.rds") %>% filter(active == "TRUE")

active_analyses_pre_vax <- active_analyses_pre_vax %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses_pre_vax$outcome_variable %>% str_replace("out_date_", ""))%>%
  filter(grepl("extended_follow_up",outcome_variable) 
         & outcome_variable %in% c("out_date_ate_extended_follow_up","out_date_vte_extended_follow_up",
                                   "out_date_ate_primary_position_extended_follow_up","out_date_vte_primary_position_extended_follow_up"))


#----------------------------Focus on ATE & VTE---------------------------------
outcomes_to_plot <- active_analyses$outcome_name
outcomes_to_plot_pre_vax <- active_analyses_pre_vax$outcome_name

# Load all estimates
estimates <- read.csv(paste0(results_dir,"/hr_output_formatted.csv"))

#-------------------------Filter to active outcomes-----------------------------
estimates <- estimates %>% filter(!subgroup %in% c("covid_history","main","covid_pheno_hospitalised","covid_pheno_non_hospitalised","ethnicity_Missing")
                                  & !grepl("aer_",subgroup)
                                  & ((event %in% outcomes_to_plot & cohort %in% c("vaccinated","electively_unvaccinated")) | (event %in% outcomes_to_plot_pre_vax & cohort %in% c("pre_vaccination")))
                                  & term %in% term[grepl("^days",term)]
                                  & model == "mdl_max_adj"
                                  & time_points == "reduced") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,model,median_follow_up)

#Second time period is redacted so remove single estimate
estimates <- estimates %>% filter(!(event == "vte" & subgroup == "ethnicity_South_Asian" & cohort == "electively_unvaccinated"))

#------------------------------Tidy event names---------------------------------
estimates$event <- gsub("_extended_follow_up","",estimates$event)
estimates <- estimates %>% left_join(active_analyses %>% select(outcome, outcome_name), by = c("event"="outcome_name"))

# Rename subgroup to 'nice' format------------------------------------------------
unique(estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="prior_history_FALSE","No prior history of event",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="prior_history_TRUE","Prior history of event",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_18_39","Age group: 18-39",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_40_59","Age group: 40-59",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_60_79","Age group: 60-79",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="agegp_80_110","Age group: 80-110",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="sex_Male","Sex: Male",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="sex_Female","Sex: Female",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_White","Ethnicity: White",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Mixed","Ethnicity: Mixed",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_South_Asian","Ethnicity: South Asian",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Black","Ethnicity: Black",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Other","Ethnicity: Other Ethnic Groups",estimates$subgroup)
estimates$subgroup <- ifelse(estimates$subgroup=="ethnicity_Missing","Ethnicity: Missing",estimates$subgroup)

# Give ethnicity estimates extra space -----------------------------------------

#estimates$time <- ifelse(estimates$subgroup=="Ethnicity: South Asian", estimates$time-0.25, estimates$time)
#estimates$time <- ifelse(estimates$subgroup=="Ethnicity: Other Ethnic Groups", estimates$time-0.5, estimates$time)
#estimates$time <- ifelse(estimates$subgroup=="Ethnicity: Mixed", estimates$time+0.25, estimates$time)
#estimates$time <- ifelse(estimates$subgroup=="Ethnicity: Black", estimates$time+0.5, estimates$time)

# Give age estimates extra space -----------------------------------------

#estimates$time <- ifelse(estimates$subgroup=="Age group: 40-59", estimates$time-0.25, estimates$time)
#estimates$time <- ifelse(estimates$subgroup=="Age group: 60-79", estimates$time-0.5, estimates$time)
#estimates$time <- ifelse(estimates$subgroup=="Age group: 80-110", estimates$time+0.25, estimates$time)

# Specify line colours ---------------------------------------------------------

estimates$colour <- ""
estimates$colour <- ifelse(estimates$subgroup=="Age group: 18-39","#0808c9",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Age group: 40-59","#0085ff",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Age group: 60-79","#00c9df",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Age group: 80-110","#73ffa6",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Sex: Male","#cab2d6",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Sex: Female","#6a3d9a",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Ethnicity: White","#444e86",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Ethnicity: Black","#ff126b",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Ethnicity: South Asian","#ff4fae",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Ethnicity: Other Ethnic Groups","#e97de1",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Ethnicity: Mixed","#c3a1ff",estimates$colour)
#estimates$colour <- ifelse(estimates$subgroup=="Ethnicity: Missing","#c5dfed",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="Prior history of event","#ff7f00",estimates$colour)
estimates$colour <- ifelse(estimates$subgroup=="No prior history of event","#fdbf6f",estimates$colour)


#Add in which subgroup stratified-----------------------------------------------------------

estimates$grouping=""
estimates$grouping=ifelse(endsWith(estimates$subgroup,"event")==T,"Prior history of event",estimates$grouping)
estimates$grouping=ifelse(startsWith(estimates$subgroup,"Age group")==T,"Age group",estimates$grouping)
estimates$grouping=ifelse(startsWith(estimates$subgroup,"Sex")==T,"Sex",estimates$grouping)
estimates$grouping=ifelse(startsWith(estimates$subgroup,"Ethnicity")==T,"Ethnicity",estimates$grouping)

estimates$grouping_name=""
estimates$grouping_name <- ifelse(estimates$cohort == "pre_vaccination", paste0(estimates$grouping," - Pre-vaccination"),estimates$grouping_name)
estimates$grouping_name <- ifelse(estimates$cohort == "vaccinated", paste0(estimates$grouping," - Vaccinated"),estimates$grouping_name)
estimates$grouping_name <- ifelse(estimates$cohort == "electively_unvaccinated", paste0(estimates$grouping," - Unvaccinated"),estimates$grouping_name)

#Set factor levels
estimates$grouping_name <- factor(estimates$grouping_name, levels = c("Age group - Pre-vaccination",
                                                                      "Age group - Vaccinated",
                                                                      "Age group - Unvaccinated",
                                                                      "Ethnicity - Pre-vaccination",
                                                                      "Ethnicity - Vaccinated"  ,
                                                                      "Ethnicity - Unvaccinated",
                                                                      "Prior history of event - Pre-vaccination",
                                                                      "Prior history of event - Vaccinated" ,
                                                                      "Prior history of event - Unvaccinated",
                                                                      "Sex - Pre-vaccination",
                                                                      "Sex - Vaccinated",
                                                                      "Sex - Unvaccinated"
                                                                      ))

names <- c(
  `Age group - Pre-vaccination` = "Pre-vaccination
  ",
  `Age group - Vaccinated` = "Vaccinated
  Age group",
  `Age group - Unvaccinated` = "Unvaccinated
  ",
    `Ethnicity - Pre-vaccination` = "",
  `Ethnicity - Vaccinated` = "Ethnicity",
  `Ethnicity - Unvaccinated` = "",
  `Prior history of event - Pre-vaccination` = "",
  `Prior history of event - Vaccinated` = "Prior history of event",
  `Prior history of event - Unvaccinated` = "",
  `Sex - Pre-vaccination` = "",
  `Sex - Vaccinated` = "Sex",
  `Sex - Unvaccinated` = ""
)


outcome_name="vte"

for(outcome_name in outcomes_to_plot){
  df=estimates %>% filter(event==outcome_name)
  
  sub_group_levels <-c()
  for(i in c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110",
             "Ethnicity: White","Ethnicity: Black","Ethnicity: South Asian","Ethnicity: Other Ethnic Groups", "Ethnicity: Mixed",
             "Prior history of event", "No prior history of event",
             "Sex: Female","Sex: Male")){
    levels_available <- unique(df$subgroup)
    if(i %in% levels_available){
      sub_group_levels <- append(sub_group_levels,i)
    }
  }

  df$subgroup <- factor(df$subgroup, levels=sub_group_levels)

  colour_levels <-c()
  for(i in c("#0808c9",
             "#0085ff",
             "#00c9df",
             "#73ffa6",
             
             "#444e86",
             "#ff126b",
             "#ff4fae",
             "#e97de1",
             "#c3a1ff",
             
             "#ff7f00",
             "#fdbf6f",
             "#6a3d9a",
             "#cab2d6")){
    levels_available <- unique(df$colour)
    if(i %in% levels_available){
      colour_levels <- append(colour_levels,i)
    }
  }

  df$colour <- factor(df$colour, levels=colour_levels)
  # df$subgroup <- factor(df$subgroup, levels = c("Age group: 18-39","Age group: 40-59","Age group: 60-79","Age group: 80-110",
  #                                               "Ethnicity: White","Ethnicity: Black","Ethnicity: South Asian","Ethnicity: Other Ethnic Groups", "Ethnicity: Mixed",
  #                                               "Prior history of event", "No prior history of event",
  #                                               "Sex: Female","Sex: Male"))
  # 
  # df$colour <- factor(df$colour, levels = c("#0808c9",
  #                                           "#0085ff",
  #                                           "#00c9df",
  #                                           "#73ffa6",
  #                                           "#444e86",
  #                                           "#ff126b",
  #                                           "#ff4fae",
  #                                           "#e97de1",
  #                                           "#c3a1ff",
  #                                           "#ff7f00",
  #                                           "#fdbf6f",
  #                                           "#6a3d9a"))
  # Plot figures------------------------------------------------------------------
  min_plot <- 0.25
  
  if(max(df$estimate, na.rm = T)<64){
    max_plot <- 64
    y_lim <- c(0.25,64)
    y_lim_breaks <- c(0.5,1,2,4,8,16,32,64)
  }else{
    max_plot <- 128
    y_lim <- c(0.25,128)
    y_lim_breaks <- c(0.5,1,2,4,8,16,32,64,128)
  }

  ggplot2::ggplot(data = df, 
                        mapping = ggplot2::aes(x = median_follow_up, y = estimate, color = subgroup, shape = subgroup, fill = subgroup)) +
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
    ggplot2::scale_x_continuous(lim = c(0,ceiling(max(df$median_follow_up, na.rm = T) / 4) * 4), breaks = seq(0,ceiling(max(df$median_follow_up, na.rm = T) / 4) * 4,4)) +
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup))+ 
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
    ggplot2::scale_shape_manual(values = c(rep(21,length(unique(df$subgroup)))),labels = levels(df$subgroup)) +
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
  
  ggplot2::ggsave(paste0(output_dir,"supplementary_figure_3_subgroups_",outcome_name,".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
}


