library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)

cohort=c("vaccinated","electively_unvaccinated")
events_to_plot <- c("ate","vte")

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"

#-----------------------Determine active outcome events-------------------------
active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses$outcome_variable <- gsub("out_date_","",active_analyses$outcome_variable)

#--------Load fully adjusted main and COVID phenotype results-------------------
hr_files=list.files(path =results_dir , pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,hr_files)

hr_file_paths <- pmap(list(hr_files), 
                      function(fpath){ 
                        df <- fread(fpath) 
                        return(df)
                      })
combined_hr <- rbindlist(hr_file_paths, fill=TRUE)

#-------------------------Filter to active outcomes-----------------------------

combined_hr <- combined_hr %>% filter(event %in% c("ate","vte"))
combined_hr <- combined_hr %>% filter(model == "mdl_max_adj" | (model=="mdl_agesex" & subgroup=="main"))
combined_hr <- combined_hr %>% filter(subgroup != "covid_pheno_hospitalised" & subgroup !="covid_history")
combined_hr <- combined_hr %>% mutate(across(c("estimate","conf.low","conf.high"), as.numeric))

#combined_hr <- combined_hr %>% filter(model == "mdl_max_adj"| (model == "mdl_agesex" & subgroup == "main"))

# Select HRs for time periods----------------------------------------------------

combined_hr <- combined_hr %>% filter(str_detect(term, "^days"))


# Specify time points to plot HRs at -------------------------------------------

term_to_time <- data.frame(term = c("days0_7","days7_14","days14_28", "days28_56", "days56_84", "days84_197", 
                                    "days0_28","days28_197"),
                           time = c(0.5,1.5,3,6,10,20,
                                    2,16))
combined_hr <- merge(combined_hr, term_to_time, by = c("term"), all.x = TRUE)

# Rename subgroup to 'nice' format------------------------------------------------

combined_hr$subgroup <- ifelse(combined_hr$subgroup=="main" & combined_hr$model== "mdl_max_adj","Extensive adjustment",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="main" & combined_hr$model== "mdl_agesex","Age/sex adjustment",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="covid_history" ,"Prior history of COVID-19",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="covid_pheno_non_hospitalised","Non-hospitalised COVID-19",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="covid_pheno_hospitalised","Hospitalised COVID-19",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="prior_history_FALSE","No prior history of event",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="prior_history_TRUE","Prior history of event",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="agegp_18_39","Age group: 18-39",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="agegp_40_59","Age group: 40-59",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="agegp_60_79","Age group: 60-79",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="agegp_80_110","Age group: 80-110",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="sex_Male","Sex: Male",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="sex_Female","Sex: Female",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_White","Ethnicity: White",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_Mixed","Ethnicity: Mixed",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_South_Asian","Ethnicity: South Asian",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_Black","Ethnicity: Black",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_Other","Ethnicity: Other Ethnic Groups",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="ethnicity_Missing","Ethnicity: Missing",combined_hr$subgroup)


# Give ethnicity estimates extra space -----------------------------------------

#combined_hr$time <- ifelse(combined_hr$subgroup=="Ethnicity: South Asian", combined_hr$time-0.25, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Ethnicity: Other Ethnic Groups", combined_hr$time-0.5, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Ethnicity: Mixed", combined_hr$time+0.25, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Ethnicity: Black", combined_hr$time+0.5, combined_hr$time)

# Give age estimates extra space -----------------------------------------

#combined_hr$time <- ifelse(combined_hr$subgroup=="Age group: 40-59", combined_hr$time-0.25, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Age group: 60-79", combined_hr$time-0.5, combined_hr$time)
#combined_hr$time <- ifelse(combined_hr$subgroup=="Age group: 80-110", combined_hr$time+0.25, combined_hr$time)

# Specify line colours ---------------------------------------------------------

combined_hr$colour <- ""
combined_hr$colour <- ifelse(combined_hr$subgroup=="Extensive adjustment","#000000",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age/sex adjustment","#bababa",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age group: 18-39","#006d2c",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age group: 40-59","#31a354",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age group: 60-79","#74c476",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Age group: 80-110","#bae4b3",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Sex: Male","#cab2d6",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Sex: Female","#6a3d9a",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: White","#08519c",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: Black","#2171b5",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: South Asian","#4292c6",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: Other Ethnic Groups","#6baed6",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: Mixed","#9ecae1",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Ethnicity: Missing","#c5dfed",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Prior history of event","#ff7f00",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="No prior history of event","#fdbf6f",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Non-hospitalised COVID-19","#fb9a99",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Hospitalised COVID-19","#e31a1c",combined_hr$colour)

# Make event names 'nice' ------------------------------------------------------

combined_hr <- combined_hr %>% left_join(active_analyses %>% select(outcome, outcome_variable), by = c("event"="outcome_variable"))

#Add in which subgroup stratified-----------------------------------------------------------

combined_hr$grouping=""
combined_hr$grouping=ifelse(combined_hr$subgroup=="Extensive adjustment","Overall",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="Age/sex adjustment","Overall",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="Hospitalised COVID-19","Hospitalised/Non-hospitalised COVID-19",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="Non-hospitalised COVID-19","Hospitalised/Non-hospitalised COVID-19",combined_hr$grouping)
combined_hr$grouping=ifelse(endsWith(combined_hr$subgroup,"event")==T,"Prior history of event",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Age group")==T,"Age group",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Sex")==T,"Sex",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Ethnicity")==T,"Ethnicity",combined_hr$grouping)

c="vaccinated"
outcome_name="ate"
for(c in cohort){
  for(outcome_name in events_to_plot){
    df=combined_hr %>% filter(cohort == c & event==outcome_name)
    if(nrow(df)>0){
      group_levels <-c()
      for(i in c("Overall","Hospitalised/Non-hospitalised COVID-19", "Prior history of event","Age group","Sex","Ethnicity" )){
        levels_available <- unique(df$grouping)
        if(i %in% levels_available){
          group_levels <- append(group_levels,i)
        }
      }
      
      df$grouping <- factor(df$grouping, levels=group_levels)
      #combined_hr$grouping <- factor(combined_hr$grouping, levels=c("Overall",
      #                                                              "Hospitalised/Non-hospitalised COVID-19",
      #                                                              "Prior history of event",
      #                                                              "Age group",
      #                                                              "Sex",
      #                                                              "Ethnicity" )) 
      
      
      sub_group_levels <-c()
      for(i in c("Extensive adjustment","Age/sex adjustment","Hospitalised COVID-19","Non-hospitalised COVID-19","Prior history of event", "No prior history of event","Age group: 18-39",
                 "Age group: 40-59","Age group: 60-79","Age group: 80-110","Sex: Female","Sex: Male","Ethnicity: White",
                 "Ethnicity: Black","Ethnicity: South Asian","Ethnicity: Other Ethnic Groups", "Ethnicity: Mixed","Ethnicity: Missing")){
        levels_available <- unique(df$subgroup)
        if(i %in% levels_available){
          sub_group_levels <- append(sub_group_levels,i)
        }
      }
      
      df$subgroup <- factor(df$subgroup, levels=sub_group_levels)
      #combined_hr$subgroup <- factor(combined_hr$subgroup, levels=c("No prior history of COVID-19",
      #                                                              "Prior history of COVID-19",
      #                                                              "Hospitalised COVID-19",
      #                                                              "Non-hospitalised COVID-19",
      #                                                              "Prior history of event",
      #                                                              "No prior history of event",
      #                                                              "Age group: 18-39",
      #                                                              "Age group: 40-59",
      #                                                              "Age group: 60-79",
      #                                                              "Age group: 80-110",
      #                                                              "Sex: Female",
      #                                                              "Sex: Male",
      #                                                              "Ethnicity: White",
      #                                                              "Ethnicity: Black",
      #                                                              "Ethnicity: South Asian",
      #                                                              "Ethnicity: Other Ethnic Groups",
      #                                                              "Ethnicity: Mixed",
      #                                                              "Ethnicity: Missing"))
      
      colour_levels <-c()
      for(i in c("#000000",
                 "#bababa",
                 "#e31a1c",
                 "#fb9a99",
                 "#ff7f00",
                 "#fdbf6f",
                 "#006d2c",
                 "#31a354",
                 "#74c476",
                 "#bae4b3",
                 "#6a3d9a",
                 "#cab2d6",
                 "#08519c",
                 "#2171b5",
                 "#4292c6",
                 "#6baed6",
                 "#9ecae1",
                 "#c5dfed")){
        levels_available <- unique(df$colour)
        if(i %in% levels_available){
          colour_levels <- append(colour_levels,i)
        }
      } 
      
      df$colour <- factor(df$colour, levels=colour_levels)
      #combined_hr$colour <- factor(combined_hr$colour, levels=c("#000000",
      #                                                          "#bababa",
      #                                                          "#e31a1c",
      #                                                          "#fb9a99",
      #                                                          "#ff7f00",
      #                                                          "#fdbf6f",
      #                                                          "#006d2c",
      #                                                          "#31a354",
      #                                                          "#74c476",
      #                                                          "#bae4b3",
      #                                                          "#6a3d9a",
      #                                                          "#cab2d6",
      #                                                          "#08519c",
      #                                                          "#2171b5",
      #                                                          "#4292c6",
      #                                                          "#6baed6",
      #                                                          "#9ecae1",
      #                                                          "#c5dfed"))
      
      
      # Plot figures------------------------------------------------------------------
      min_plot <- 0.25
      max_plot <- 64
      
      ggplot2::ggplot(data = df, 
                      mapping = ggplot2::aes(x = time, y = estimate, color = subgroup, shape = subgroup, fill = subgroup)) +
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
        ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5))+
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<min_plot,min_plot,conf.low), 
                                                      ymax = ifelse(conf.high>max_plot,max_plot,conf.high),  
                                                      width = 0), 
                               position = ggplot2::position_dodge(width = 1))+
        ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
        ggplot2::scale_x_continuous(lim = c(0,28), breaks = seq(0,28,4)) +
        #ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup))+ 
        #ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
        ggplot2::scale_shape_manual(values = c(rep(21,18)), labels = levels(combined_hr$subgroup)) +
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
        ggplot2::facet_wrap(grouping~.,ncol=2)
      
      ggplot2::ggsave(paste0(output_dir,"figure_3_",outcome_name,"_",c, ".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
    }
  }
}
      
      
    
        
      
      
      
     
        
       
        
        
        
        
  