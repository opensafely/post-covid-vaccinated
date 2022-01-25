library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
project = args[[1]]#vaccinated, electively_unvaccinated

# Load fully adjusted COVID phenotype results------------------------------------
project="vaccinated"

if(project == "vaccinated"){
  pheno_results=read_csv("output/compiled_HR_results_subgroup_covid_pheno_vaccinated_delta_mdl_max_adj_covid_history_false.csv")
}else if(project == "electively_unvaccinated"){
  pheno_results=read_csv("output/compiled_HR_results_subgroup_covid_pheno_electively_vaccinated_delta_mdl_max_adj_covid_history_false.csv")
  
}
pheno_results=pheno_results%>%select(term,estimate,conf.low,conf.high,std.error,robust.se,expo_week,events_total,event,strata)

# Select HRs for time periods----------------------------------------------------

pheno_results <- pheno_results %>% filter(str_detect(term, "^days"))

# Where to plot the time period plots along the x-axis---------------------------

#In days since COVID
term_to_time <- data.frame(term = c("days0_14","days14_28","days28_56","days56_84","days84_196",
                                   "days0_28","days28_196"),
                          time = c(7,21,42,70,140,
                                   14,112))

#In weeks since COVID
#weeks0_2, weeks2_4, weeks4_8, weeks8_12, weeks12_28
#weeks0_4, weeks4_28
# term_to_time <- data.frame(term = c("days0_14","days14_28","days28_56","days56_84","days84_196",
#                                    "days0_28","days28_196"),
#                           time = c(1,3,6,10,20,
#                                    2,16))

pheno_results <- merge(pheno_results, term_to_time, by = c("term"), all.x = TRUE)

# Rename strata to 'nice' format------------------------------------------------

pheno_results$strata <- ifelse(pheno_results$strata=="expo_pheno_non_hospitalised","Non-hospitalised COVID-19",pheno_results$strata)
pheno_results$strata <- ifelse(pheno_results$strata=="expo_pheno_hospitalised","Hospitalised COVID-19",pheno_results$strata)

# Specify line colours----------------------------------------------------------

pheno_results$colour <- ""
pheno_results$colour <- ifelse(pheno_results$strata=="Non-hospitalised COVID-19","#fb9a99",pheno_results$colour)
pheno_results$colour <- ifelse(pheno_results$strata=="Hospitalised COVID-19","#e31a1c",pheno_results$colour)


# Make event names 'nice' ------------------------------------------------------


pheno_results$event <- ifelse(pheno_results$event=="ami","Acute myocardial infarction",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="tia","Transient ischaemic attack",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="dvt","Deep vein thrombosis",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="hf","Heart failure",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="stroke_isch","Ischaemic stroke",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="angina","Angina",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="vte","Venous thromboembolism",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="pe","Pulmonary embolism",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="stroke_sah_hs","Subarachnoid haemorrhage and haemorrhagic stroke",pheno_results$event)
pheno_results$event <- ifelse(pheno_results$event=="ate","Arterial thromboses",pheno_results$event)

# Factor variables for ordering-------------------------------------------------

pheno_results$strata <- factor(pheno_results$strata, levels=c("Hospitalised COVID-19",
                                                              "Non-hospitalised COVID-19")) 

pheno_results$colour <- factor(pheno_results$colour, levels=c("#e31a1c",
                                                              "#fb9a99"))

pheno_results$event <- factor(pheno_results$event, levels=c("Acute myocardial infarction",
                                      "Ischaemic stroke",
                                      "Pulmonary embolism",
                                      "Deep vein thrombosis",
                                      "Transient ischaemic attack",
                                      "Subarachnoid haemorrhage and haemorrhagic stroke",
                                      "Heart failure",
                                      "Angina",
                                      "Arterial thromboses",
                                      "Venous thromboembolism"))

# Plot figures

ggplot2::ggplot(data =pheno_results,
                mapping = ggplot2::aes(x = time, y = estimate, color = strata, shape=strata, fill=strata)) +
  ggplot2::facet_wrap(event~., ncol = 2)+
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                                ymax = ifelse(conf.high>64,64,conf.high),  
                                                width = 0), 
                         position = ggplot2::position_dodge(width = 1)) +
  ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) +
  #ggplot2::scale_y_continuous(lim = c(0.25,64), breaks = c(0.5,0.5,1,2,4,8,16,32,64), trans = "log") +
  ggplot2::scale_x_continuous(lim = c(0,196), breaks = seq(0,196,28)) +
  #ggplot2::scale_x_continuous(lim = c(0,28), breaks = seq(0,28,4)) +
  ggplot2::scale_fill_manual(values = levels(pheno_results$colour)[1:2], labels = levels(pheno_results$strata)[1:2]) +
  ggplot2::scale_color_manual(values = levels(pheno_results$colour)[1:2], labels = levels(pheno_results$strata)[1:2]) +
  ggplot2::scale_shape_manual(values = c(rep(c(21,22),3),23,24)[1:2], labels = levels(pheno_results$strata)[1:1]) +
  ggplot2::labs(x = "\nDays since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
  #ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
  ggplot2::guides(fill=ggplot2::guide_legend(ncol = 4, byrow = TRUE)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0.5, "lines"),
                 panel.spacing.y = ggplot2::unit(0, "lines"),
                 legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                 legend.title = ggplot2::element_blank(),
                 legend.position="bottom",
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white")) 
  #ggplot2::facet_wrap(event~., ncol = 2)


ggplot2::ggsave(paste0("output/figure2_COVID_phenotype_days.png"), height = 297, width = 210, unit = "mm", dpi = 600, scale = 1)

