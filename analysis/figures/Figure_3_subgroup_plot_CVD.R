#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"

active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))

# Focus on ATE and VTE
outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name %in% c("ate","vte","ate_primary_position","vte_primary_position")]


#--------Load fully adjusted main and COVID phenotype results-------------------
hr_files=list.files(path = results_dir, pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

# Read in stata ouptut

tmp <- read.csv(paste0(results_dir, "/stata_output_formatted"))
tmp <- tmp %>% select(intersect(colnames(estimates),colnames(tmp)))
estimates <- rbind(estimates, tmp, fill = TRUE)
rm(tmp)

#-------------------------Filter to active outcomes-----------------------------
main_estimates <- estimates %>% filter(!subgroup %in% c("covid_history","main","covid_pheno_hospitalised","covid_pheno_non_hospitalised","ethnicity_Missing")
                                       & !subgroup %in% subgroup[grepl("aer_",subgroup)]
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

main_estimates$subgroup <- ifelse(main_estimates$subgroup=="main" & main_estimates$model== "mdl_max_adj","Maximal adjustment",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="main" & main_estimates$model== "mdl_age_sex_region","Age/sex/region adjustment",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="covid_history" ,"Prior history of COVID-19",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="covid_pheno_non_hospitalised","Non-hospitalised COVID-19",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="covid_pheno_hospitalised","Hospitalised COVID-19",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="prior_history_FALSE","No prior history of event",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="prior_history_TRUE","Prior history of event",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="agegp_18_39","Age group: 18-39",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="agegp_40_59","Age group: 40-59",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="agegp_60_79","Age group: 60-79",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="agegp_80_110","Age group: 80-110",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="sex_Male","Sex: Male",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="sex_Female","Sex: Female",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="ethnicity_White","Ethnicity: White",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="ethnicity_Mixed","Ethnicity: Mixed",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="ethnicity_South_Asian","Ethnicity: South Asian",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="ethnicity_Black","Ethnicity: Black",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="ethnicity_Other","Ethnicity: Other Ethnic Groups",main_estimates$subgroup)
main_estimates$subgroup <- ifelse(main_estimates$subgroup=="ethnicity_Missing","Ethnicity: Missing",main_estimates$subgroup)
unique(main_estimates$subgroup)
# Give ethnicity estimates extra space -----------------------------------------

#main_estimates$time <- ifelse(main_estimates$subgroup=="Ethnicity: South Asian", main_estimates$time-0.25, main_estimates$time)
#main_estimates$time <- ifelse(main_estimates$subgroup=="Ethnicity: Other Ethnic Groups", main_estimates$time-0.5, main_estimates$time)
#main_estimates$time <- ifelse(main_estimates$subgroup=="Ethnicity: Mixed", main_estimates$time+0.25, main_estimates$time)
#main_estimates$time <- ifelse(main_estimates$subgroup=="Ethnicity: Black", main_estimates$time+0.5, main_estimates$time)

# Give age estimates extra space -----------------------------------------

#main_estimates$time <- ifelse(main_estimates$subgroup=="Age group: 40-59", main_estimates$time-0.25, main_estimates$time)
#main_estimates$time <- ifelse(main_estimates$subgroup=="Age group: 60-79", main_estimates$time-0.5, main_estimates$time)
#main_estimates$time <- ifelse(main_estimates$subgroup=="Age group: 80-110", main_estimates$time+0.25, main_estimates$time)

# Specify line colours ---------------------------------------------------------

main_estimates$colour <- ""
main_estimates$colour <- ifelse(main_estimates$subgroup=="Maximal adjustment","#000000",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Age/sex adjustment","#bababa",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Age group: 18-39","#0808c9",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Age group: 40-59","#0085ff",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Age group: 60-79","#00c9df",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Age group: 80-110","#73ffa6",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Sex: Male","#cab2d6",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Sex: Female","#6a3d9a",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Ethnicity: White","#444e86",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Ethnicity: Black","#ff126b",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Ethnicity: South Asian","#ff4fae",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Ethnicity: Other Ethnic Groups","#e97de1",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Ethnicity: Mixed","#c3a1ff",main_estimates$colour)
#main_estimates$colour <- ifelse(main_estimates$subgroup=="Ethnicity: Missing","#c5dfed",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Prior history of event","#ff7f00",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="No prior history of event","#fdbf6f",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Non-hospitalised COVID-19","#fb9a99",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$subgroup=="Hospitalised COVID-19","#e31a1c",main_estimates$colour)
unique(main_estimates$colour)
# Make event names 'nice' ------------------------------------------------------

main_estimates <- main_estimates %>% left_join(active_analyses %>% select(outcome, outcome_variable), by = c("event"="outcome_variable"))

#Add in which subgroup stratified-----------------------------------------------------------

main_estimates$grouping=""
main_estimates$grouping=ifelse(main_estimates$subgroup=="Maximal adjustment","Overall",main_estimates$grouping)
main_estimates$grouping=ifelse(main_estimates$subgroup=="Age/sex adjustment","Overall",main_estimates$grouping)
main_estimates$grouping=ifelse(main_estimates$subgroup=="Hospitalised COVID-19","Hospitalised/Non-hospitalised COVID-19",main_estimates$grouping)
main_estimates$grouping=ifelse(main_estimates$subgroup=="Non-hospitalised COVID-19","Hospitalised/Non-hospitalised COVID-19",main_estimates$grouping)
main_estimates$grouping=ifelse(endsWith(main_estimates$subgroup,"event")==T,"Prior history of event",main_estimates$grouping)
main_estimates$grouping=ifelse(startsWith(main_estimates$subgroup,"Age group")==T,"Age group",main_estimates$grouping)
main_estimates$grouping=ifelse(startsWith(main_estimates$subgroup,"Sex")==T,"Sex",main_estimates$grouping)
main_estimates$grouping=ifelse(startsWith(main_estimates$subgroup,"Ethnicity")==T,"Ethnicity",main_estimates$grouping)

main_estimates$grouping_name=""
main_estimates$grouping_name <- ifelse(main_estimates$cohort == "pre_vaccination", paste0(main_estimates$grouping," - Pre-vaccination"),main_estimates$grouping_name)
main_estimates$grouping_name <- ifelse(main_estimates$cohort == "vaccinated", paste0(main_estimates$grouping," - Vaccinated"),main_estimates$grouping_name)
main_estimates$grouping_name <- ifelse(main_estimates$cohort == "electively_unvaccinated", paste0(main_estimates$grouping," - Unvaccinated"),main_estimates$grouping_name)

#Set factor levels
main_estimates$grouping_name <- factor(main_estimates$grouping_name, levels = c("Age group - Pre-vaccination",
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

# We want to plot the figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points
main_estimates <- main_estimates %>%
  group_by(event,subgroup,cohort) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_points == "normal") ~ "normal",
    TRUE ~ "reduced"))

main_estimates <- main_estimates %>%
  group_by(event,grouping) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_period_to_plot == "reduced") ~ "reduced",
    TRUE ~ "normal"))


for(outcome_name in outcomes_to_plot){
  df=main_estimates %>% filter(event==outcome_name & time_points == time_period_to_plot)
  
  sub_group_levels <-c()
  for(i in c("Maximal adjustment","Age/sex adjustment","Hospitalised COVID-19","Non-hospitalised COVID-19","Prior history of event", "No prior history of event","Age group: 18-39",
             "Age group: 40-59","Age group: 60-79","Age group: 80-110","Sex: Female","Sex: Male","Ethnicity: White",
             "Ethnicity: Black","Ethnicity: South Asian","Ethnicity: Other Ethnic Groups", "Ethnicity: Mixed","Ethnicity: Missing")){
    levels_available <- unique(df$subgroup)
    if(i %in% levels_available){
      sub_group_levels <- append(sub_group_levels,i)
    }
  }
  
  df$subgroup <- factor(df$subgroup, levels=sub_group_levels)
  
  colour_levels <-c()
  for(i in c("#000000",
             "#bababa",
             "#e31a1c",
             "#fb9a99",
             "#ff7f00",
             "#fdbf6f",
             "#0808c9",
             "#0085ff",
             "#00c9df",
             "#73ffa6",
             "#6a3d9a",
             "#cab2d6",
             "#444e86",
             "#ff126b",
             "#ff4fae",
             "#e97de1",
             "#c3a1ff",
             "#c5dfed")){
    levels_available <- unique(df$colour)
    if(i %in% levels_available){
      colour_levels <- append(colour_levels,i)
    }
  } 
  
  df$colour <- factor(df$colour, levels=colour_levels)
  
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
    ggplot2::scale_x_continuous(lim = c(0,round_any(max(df$median_follow_up, na.rm = T),4, f= ceiling)), breaks = seq(0,round_any(max(df$median_follow_up, na.rm = T),4, f= ceiling),4)) +
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup))+ 
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
    ggplot2::scale_shape_manual(values = c(rep(21,18))) +
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
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +
    ggplot2::facet_wrap(grouping_name~.,ncol=3)
  
  ggplot2::ggsave(paste0(output_dir,"Figure_3_subgroups_",outcome_name,".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
}


