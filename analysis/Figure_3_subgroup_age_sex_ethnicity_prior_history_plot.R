library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  cohort="vaccinated"
}else{
  cohort = args[[1]]#both, vaccinated, electively_unvaccinated
}


if(cohort=="both"){
  cohort=c("vaccinated","electively_unvaccinated")
}

#-----------------------Determine active outcome events-------------------------
active_analyses <- read_rds("output/active_analyses.rds")
cols=colnames(active_analyses)[grepl("active|agegp|ethnicity|prior_history_TRUE|prior_history_FALSE",colnames(active_analyses))]
events <- active_analyses %>% filter_at(vars(cols), all_vars(.==TRUE))
events$outcome_variable <- gsub("out_date_","",events$outcome_variable)

#--------Load fully adjusted main and COVID phenotype results-------------------
hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0("output/",hr_files)

hr_file_paths <- pmap(list(hr_files), 
                      function(fpath){ 
                        df <- fread(fpath) 
                        return(df)
                      })
combined_hr <- rbindlist(hr_file_paths, fill=TRUE)

#-------------------------Filter to active outcomes-----------------------------

combined_hr <- combined_hr %>% filter(event %in% events$outcome_variable)
combined_hr <- combined_hr %>% filter(model == "mdl_max_adj")
#combined_hr <- combined_hr %>% filter(model == "mdl_max_adj"| (model == "mdl_agesex" & subgroup == "main"))

# Select HRs for time periods----------------------------------------------------

combined_hr <- combined_hr %>% filter(str_detect(term, "^days"))

# Select all time period names and remove all the numbers to find the time cut points for this project

term=unique(combined_hr$term)
cuts=c()
for(i in 1:length(term)){
  time_period=sub("days","",term[i])
  cuts=append(cuts,as.numeric(sub('\\_.*', '', time_period)))
  cuts=append(cuts,as.numeric(gsub(".*_", "",time_period)))
}
cuts=unique(cuts) 
cuts=sort(cuts, decreasing = F)

# Create a character vector of all the time period terms in increasing order with the reduced time periods at the end
term=as.character()
for(i in 1:(length(cuts)-1)){
  term=append(term,as.character(paste0("days",cuts[i],"_",cuts[i+1])))
}
term=append(term,as.character("days0_28"))
term=append(term,as.character(paste0("days28_",cuts[length(cuts)])))

# Find the mid point in weeks of the time periods
time=c()
for(i in 1:(length(cuts)-2)){
  time=append(time,(cuts[i+1]+cuts[i])/14)
}

#Have to -1 from the highest time point as have to +1 because cox model takes as
#[a,b) and need to include the final day eg [84,197) to include day 196 but only interest in
#days 0 to 196, not 0 to 197
time=append(time,((cuts[length(cuts)]-1)+cuts[length(cuts)-1])/14)

#For time period 0_28 in reduced time periods - currently always used but can be updated if we change this
time=append(time,2) 

# For time period 28 to final day
time=append(time,((cuts[length(cuts)]-1)+28)/14)


term_to_time <- data.frame(term = term,
                           time = time)

combined_hr <- merge(combined_hr, term_to_time, by = c("term"), all.x = TRUE)

# Rename subgroup to 'nice' format------------------------------------------------

combined_hr$subgroup <- ifelse(combined_hr$subgroup=="main","No prior history of COVID-19",combined_hr$subgroup)
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
combined_hr$colour <- ifelse(combined_hr$subgroup=="No prior history of COVID-19","#000000",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Prior history of COVID-19","#bababa",combined_hr$colour)
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

combined_hr <- combined_hr %>% left_join(events %>% select(outcome, outcome_variable), by = c("event"="outcome_variable"))

#Add in which subgroup stratified-----------------------------------------------------------

combined_hr$grouping=""
combined_hr$grouping=ifelse(combined_hr$subgroup=="No prior history of COVID-19","Overall",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="Prior history of COVID-19","Overall",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="Hospitalised COVID-19","Hospitalised/Non-hospitalised COVID-19",combined_hr$grouping)
combined_hr$grouping=ifelse(combined_hr$subgroup=="Non-hospitalised COVID-19","Hospitalised/Non-hospitalised COVID-19",combined_hr$grouping)
combined_hr$grouping=ifelse(endsWith(combined_hr$subgroup,"event")==T,"Prior history of event",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Age")==T,"Age group",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Sex")==T,"Sex",combined_hr$grouping)
combined_hr$grouping=ifelse(startsWith(combined_hr$subgroup,"Ethnicity")==T,"Ethnicity",combined_hr$grouping)

# Factor variables for ordering-------------------------------------------------

combined_hr$grouping <- factor(combined_hr$grouping, levels=c("Overall",
                                                              "Hospitalised/Non-hospitalised COVID-19",
                                                              "Prior history of event",
                                                              "Age group",
                                                              "Sex",
                                                              "Ethnicity" )) 
  
  
combined_hr$subgroup <- factor(combined_hr$subgroup, levels=c("No prior history of COVID-19",
                                                              "Prior history of COVID-19",
                                                              "Hospitalised COVID-19",
                                                              "Non-hospitalised COVID-19",
                                                              "Prior history of event",
                                                              "No prior history of event",
                                                              "Age group: 18-39",
                                                              "Age group: 40-59",
                                                              "Age group: 60-79",
                                                              "Age group: 80-110",
                                                              "Sex: Female",
                                                              "Sex: Male",
                                                              "Ethnicity: White",
                                                              "Ethnicity: Black",
                                                              "Ethnicity: South Asian",
                                                              "Ethnicity: Other Ethnic Groups",
                                                              "Ethnicity: Mixed",
                                                              "Ethnicity: Missing"))
                                                                              
  
combined_hr$colour <- factor(combined_hr$colour, levels=c("#000000",
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
                                                          "#c5dfed"))


# Plot figures------------------------------------------------------------------

events_to_plot <- events$outcome_variable

for(i in cohort){
  
  # Filter to cohort of interest
  
  df=combined_hr %>% filter(cohort ==i)
  
  # Plot -----------------------------------------------------------------------
  
  min_plot <- 0.25
  max_plot <- 64
  for(j in events_to_plot){
    ggplot2::ggplot(data = df[df$event==j,], 
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
      ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup))+ 
      ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
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
    
    ggplot2::ggsave(paste0("output/figure_3_",j,"_",i, ".png"), height = 210, width = 297, unit = "mm", dpi = 600, scale = 1)
  }      
}


    










