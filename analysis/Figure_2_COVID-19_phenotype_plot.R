library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)

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
events <- active_analyses %>% filter(active=="TRUE")%>%select(outcome,outcome_variable)
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
combined_hr <- combined_hr %>% filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
combined_hr <- combined_hr %>% filter(model == "mdl_max_adj")

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

# Rename strata to 'nice' format------------------------------------------------

combined_hr$subgroup <- ifelse(combined_hr$subgroup=="covid_pheno_non_hospitalised","Non-hospitalised COVID-19",combined_hr$subgroup)
combined_hr$subgroup <- ifelse(combined_hr$subgroup=="covid_pheno_hospitalised","Hospitalised COVID-19",combined_hr$subgroup)

# Specify line colours----------------------------------------------------------

combined_hr$colour <- ""
combined_hr$colour <- ifelse(combined_hr$subgroup=="Non-hospitalised COVID-19","#fb9a99",combined_hr$colour)
combined_hr$colour <- ifelse(combined_hr$subgroup=="Hospitalised COVID-19","#e31a1c",combined_hr$colour)

# Make event names 'nice' ------------------------------------------------------

combined_hr <- combined_hr %>% left_join(events, by = c("event"="outcome_variable"))

# Create figure-----------------------------------------------------------------

for(i in cohort){
  
  # Filter to cohort of interest
  
  df=combined_hr %>% filter(cohort ==i)
  
  # Which events to run
  # Only run for events that have results for both hospitalised and non-hospitalised COVID
  
  outcome_names=unique(df$event)
  events_to_plot=c()
  for(outcome in outcome_names){
    tmp=df%>%filter(event==outcome)
    number_of_subgroups=length(unique(df$subgroup))
    if(number_of_subgroups==2){
      events_to_plot=append(events_to_plot,outcome)
    }else{
      not_plot=append(not_plot,outcome)
    }
  }
  
  df=df%>%filter(event %in%events_to_plot)
  
  # Factor variables for ordering-------------------------------------------------
  
  df$subgroup <- factor(df$subgroup, levels=c("Hospitalised COVID-19",
                                              "Non-hospitalised COVID-19")) 
  
  df$colour <- factor(df$colour, levels=c("#e31a1c",
                                          "#fb9a99"))
  
  
  event_levels_order=events %>% filter(outcome_variable %in% events_to_plot)
  event_levels_order = event_levels_order$outcome_variable
  df$event <- factor(df$event, levels=event_levels_order)
  
  # Plot figures
  
  min_plot=0.25
  max_plot=64
  
  ggplot2::ggplot(data =df,
                  mapping = ggplot2::aes(x = time, y = estimate, color = subgroup, shape=subgroup, fill=subgroup)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<min_plot,min_plot,conf.low), 
                                                  ymax = ifelse(conf.high>max_plot,max_plot,conf.high),  
                                                  width = 0), 
                           position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.5,1,2,4,8,16,32,64), trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,28), breaks = seq(0,28,4)) +
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$subgroup)) +
    ggplot2::scale_shape_manual(values = c(21,22), labels = levels(df$subgroup)) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 2, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"))+ 
    ggplot2::facet_wrap(outcome~., ncol = 2)
  
  
  ggplot2::ggsave(paste0("output/figure2_COVID_phenotype_",i,".png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)

}

