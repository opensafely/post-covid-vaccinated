#libraries
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/"

#-------------------------#
# 2. Get outcomes to plot #
#-------------------------#
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", "")) %>%
  filter(outcome_name %in% c("ate","vte","ate_primary_position","vte_primary_position"))

outcomes_to_plot <- outcome_name_table$outcome_name[outcome_name_table$outcome_name %in% c("ate","vte","ate_primary_position","vte_primary_position")]


#---------------------------------------------#
# 3. Load and combine all estimates in 1 file #
#---------------------------------------------#
hr_files=list.files(path = results_dir, pattern = "suppressed_compiled_HR_results_*")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(results_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

# Get estimates for main analyses and list of outcomes from active analyses
main_estimates <- estimates %>% filter(subgroup %in% c("covid_pheno_non_hospitalised","covid_pheno_hospitalised", "main") 
                                       & event %in% outcomes_to_plot 
                                       & term %in% term[grepl("^days",term)]
                                       & results_fitted == "fitted_successfully"
                                       & model == "mdl_max_adj") %>%
  select(term,estimate,conf_low,conf_high,event,subgroup,cohort,time_points,median_follow_up)


main_estimates <- main_estimates %>% dplyr::mutate(across(c(estimate,conf_low,conf_high,median_follow_up),as.numeric))


# We want to plot the figures using the same time-points across all cohorts so that they can be compared
# If any cohort uses reduced time points then all cohorts will be plotted with reduced time points
main_estimates <- main_estimates %>%
  group_by(event,subgroup,cohort) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_points == "normal") ~ "normal",
    TRUE ~ "reduced"))

main_estimates <- main_estimates %>%
  group_by(event,subgroup) %>%
  dplyr::mutate(time_period_to_plot = case_when(
    any(time_period_to_plot == "reduced") ~ "reduced",
    TRUE ~ "normal"))


#---------------------------Specify time to plot--------------------------------
main_estimates$add_to_median <- sub("days","",main_estimates$term)
main_estimates$add_to_median <- as.numeric(sub("\\_.*","",main_estimates$add_to_median))

main_estimates$median_follow_up <- ((main_estimates$median_follow_up + main_estimates$add_to_median)-1)/7
main_estimates$median_follow_up <- ifelse(main_estimates$median_follow_up == 0, 0.001,main_estimates$median_follow_up )


#------------------------------------------#
# 4. Specify groups and their line colours #
#------------------------------------------#
# Specify colours
main_estimates$colour <- ""
main_estimates$colour <- ifelse(main_estimates$cohort=="prevax","#d2ac47",main_estimates$colour)
main_estimates$colour <- ifelse(main_estimates$cohort=="vax","#58764c",main_estimates$colour) 
main_estimates$colour <- ifelse(main_estimates$cohort=="unvax","#0018a8",main_estimates$colour) 

#Specify lines
main_estimates$linetype <- ""
main_estimates$linetype <- ifelse(main_estimates$subgroup=="covid_pheno_hospitalised","solid",main_estimates$linetype)
main_estimates$linetype <- ifelse(main_estimates$subgroup=="covid_pheno_non_hospitalised","dashed",main_estimates$linetype)

# Factor variables for ordering
main_estimates$cohort <- factor(main_estimates$cohort, levels=c("pre_vaccination","vaccinated","electively_unvaccinated")) 
main_estimates$colour <- factor(main_estimates$colour, levels=c("#d2ac47","#58764c","#94273c"))

main_estimates$subgroup <- factor(main_estimates$subgroup,levels = c("main", "covid_pheno_hospitalised","covid_pheno_non_hospitalised"))

# Rename adjustment groups
levels(main_estimates$cohort) <- list("Pre-Vaccination (2020-01-01 - 2021-06-18)"="pre_vaccination", "Vaccinated (2021-06-01 - 2021-12-14)"="vaccinated","Unvaccinated (2021-06-01 - 2021-12-14)"="electively_unvaccinated")

# MAIN --------------------------------------------------------------------

for (i in unique(main_estimates$event)) {
  df <- main_estimates %>% filter(event ==i & time_points == time_period_to_plot & subgroup == "main")
  
  assign(paste0("main_",i), ggplot2::ggplot(data=df,
                  mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
    #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_point(aes(),size = 2) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                              ymax = ifelse(conf_high>64,64,conf_high),  
                                                              width = 0), 
                           position = ggplot2::position_dodge(width = 0.5))+   
    #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
    ggplot2::geom_line() +
    #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
    ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,56), breaks = seq(0,56,4)) +
    ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
    ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
    ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
    #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
    ggplot2::labs(x = "\n ", y = "Hazard ratio and 95% confidence interval") +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 3, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   plot.margin = margin(1,1,1,1, "cm")) +   
    theme(text = element_text(size = 12)) +
    theme(legend.text = element_blank()))
}

# HOSPITALISED ------------------------------------------------------------
for (i in unique(main_estimates$event)) {
  df <- main_estimates %>% filter(event ==i & time_points == time_period_to_plot & subgroup == "covid_pheno_hospitalised")
  
  assign(paste0("hospitalised_",i), ggplot2::ggplot(data=df,
                                            mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
           #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
           ggplot2::geom_point(aes(),size = 2) +
           ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
           ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                                     ymax = ifelse(conf_high>64,64,conf_high),  
                                                                     width = 0), 
                                  position = ggplot2::position_dodge(width = 0.5))+   
           #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
           ggplot2::geom_line() +
           #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
           ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
           ggplot2::scale_x_continuous(lim = c(0,56), breaks = seq(0,56,4)) +
           ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
           ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
           ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
           #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
           ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = NULL) +
           ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 3, byrow = TRUE)) +
           ggplot2::theme_minimal() +
           ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.spacing.x = ggplot2::unit(0.5, "lines"),
                          panel.spacing.y = ggplot2::unit(0, "lines"),
                          legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                          legend.title = ggplot2::element_blank(),
                          legend.position="bottom",
                          plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                          plot.margin = margin(1,1,1,1, "cm")) +   
           theme(text = element_text(size = 12)) +
           theme(legend.text = element_blank()))
  
  
}


# NON HOSPITALISED --------------------------------------------------------
for (i in unique(main_estimates$event)) {
  df <- main_estimates %>% filter(event ==i & time_points == time_period_to_plot & subgroup == "covid_pheno_non_hospitalised")
  
  assign(paste0("non_hospitalised_",i), ggplot2::ggplot(data=df,
                                                    mapping = ggplot2::aes(x=median_follow_up, y = estimate, color = cohort, shape= cohort, fill= cohort))+
           #ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
           ggplot2::geom_point(aes(), size = 2) +
           ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
           ggplot2::geom_errorbar(size = 1.2, mapping = ggplot2::aes(ymin = ifelse(conf_low<0.25,0.25,conf_low), 
                                                                     ymax = ifelse(conf_high>64,64,conf_high),  
                                                                     width = 0), 
                                  position = ggplot2::position_dodge(width = 0.5))+   
           #ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) + 
           ggplot2::geom_line() +
           #ggplot2::scale_y_continuous(lim = c(0.25,8), breaks = c(0.5,1,2,4,8), trans = "log") +
           ggplot2::scale_y_continuous(lim = c(0.25,32), breaks = c(0.25,0.5,1,2,4,8,16,32), trans = "log") +
           ggplot2::scale_x_continuous(lim = c(0,56), breaks = seq(0,56,4)) +
           ggplot2::scale_fill_manual(values = levels(df$colour), labels = levels(df$cohort))+ 
           ggplot2::scale_color_manual(values = levels(df$colour), labels = levels(df$cohort)) +
           ggplot2::scale_shape_manual(values = c(rep(21,22)), labels = levels(df$cohort)) +
           #ggplot2::scale_linetype_manual(values = levels(df$linetype), labels = levels(df$subgroup)) +
           ggplot2::labs(x = "\n ", y = NULL) +
           ggplot2::guides(fill=ggplot2::guide_legend(ncol = 1, nrow = 3, byrow = TRUE)) +
           ggplot2::theme_minimal() +
           ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.spacing.x = ggplot2::unit(0.5, "lines"),
                          panel.spacing.y = ggplot2::unit(0, "lines"),
                          legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                          legend.title = ggplot2::element_blank(),
                          legend.position="bottom",
                          legend.spacing.y = unit(0.01, 'cm'),
                          plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                          plot.margin = margin(1,1,1,1, "cm")) +   
           theme(text = element_text(size = 12)) +
           theme(legend.text = element_blank()))
}

# PLOT WITHOUT TABLE ------------------------------------------------------
# for(i in unique(main_estimates$event)){
#   main <- get(paste0("main_",i))
#   hospitalised <- get(paste0("hospitalised_",i))
#   non_hospitalised <- get(paste0("non_hospitalised_",i))
#   
#   ggpubr::ggarrange(main, non_hospitalised, ncol=2, nrow=1, common.legend = TRUE, legend="bottom",
#                     labels = c("A: All COVID-19", "B: Hospitalised-COVID-19", "C: Non-Hospitalised-COVID-19"),
#                     hjust = -0.1,
#                     font.label = list(size = 12)) +
#     theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
#   ggplot2::ggsave(paste0(output_dir,"Figure_1_",i,"_3_panel.png"), units = "mm", width=330, height=195)
#   
#   
#   
#   # png(paste0(output_dir,"Figure_1_",i,"_3_panel.png"),
#   #     units = "mm", width=330, height=195, res = 1000)
#   # ggpubr::ggarrange(main, non_hospitalised, ncol=2, nrow=1, common.legend = TRUE, legend="bottom",
#   #                   labels = c("A: All COVID-19", "B: Hospitalised-COVID-19", "C: Non-Hospitalised-COVID-19"),
#   #                   hjust = -0.1,
#   #                   font.label = list(size = 12)) +
#   #   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 
#   # dev.off()
# }


# ADD EVENT COUNTS TO PLOT TABLE  -------------------------------------------------------
table2 <- read.csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/table_2/formatted_table_2.csv", check.names = FALSE)
table2_primary_position <- read.csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/table_2/formatted_table_2_primary_position.csv", check.names = FALSE)
table2 <- rbind(table2, table2_primary_position)

table2$cohort <- ifelse(table2$cohort == "Pre-vaccinated", "Pre-Vaccination (2020-01-01 - 2021-06-18)", ifelse(table2$cohort == "Vaccinated","Vaccinated (2021-06-01 - 2021-12-14)", "Unvaccinated (2021-06-01 - 2021-12-14)") )

table2 <- table2 %>%
  dplyr::filter(Outcome %in% outcome_name_table$outcome) %>%
  dplyr::mutate(`Events after COVID-19` = `After hospitalised COVID-19` + `After non-hospitalised COVID-19`) %>%
  dplyr::rename(`Total events` = Total,
                Cohort = cohort) %>%
  dplyr::select(-c(`No COVID-19`)) %>%
  mutate(`Number of people` = ifelse(Cohort == "Pre-Vaccination (2020-01-01 - 2021-06-18)", 18210937,
                                     ifelse(Cohort == "Vaccinated (2021-06-01 - 2021-12-14)", 13572399,
                                            ifelse(Cohort == "Unvaccinated (2021-06-01 - 2021-12-14)",3161485, NA)))) %>%
  relocate(`Total events`, .after = `Cohort`) %>%
  relocate(`Number of people`, .after = `Cohort`) %>%
  relocate(`Events after COVID-19`, .after = `Total events`)
table2[,3:7] <- format(table2[,3:7], big.mark = ",", scientific = FALSE)

# COLOURED ROWS

# table.p <- ggtexttable(table2, rows = NULL,
#                        theme = ttheme(tbody.style = tbody_style(color = "white", fill = c("#d2ac47","#58764c","#0018a8")))) +
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 

# NO COLOURS

for(i in outcome_name_table$outcome){
  short_outcome <- outcome_name_table[outcome_name_table$outcome ==i,]$outcome_name
  main <- get(paste0("main_",short_outcome))
  hospitalised <- get(paste0("hospitalised_",short_outcome))
  non_hospitalised <- get(paste0("non_hospitalised_",short_outcome))
  
  tmp_table2 <- table2 %>% filter(Outcome == i)
  tmp_table2$Outcome <- NULL
  
  colnames(tmp_table2) <- c(colnames(tmp_table2)[1:2],paste0("Total ",i),colnames(tmp_table2)[4:6])
  
  table.p <- ggtexttable(tmp_table2, rows = NULL,
                         theme = ttheme(
                           tbody.style = tbody_style(hjust=0, x=0.01, fill = "white", size = 8),
                           colnames.style = colnames_style(hjust=0, x=0.01, fill = "white", size = 8))) 
  
  # PLOTTING ----------------------------------------------------------------
  
  # MAIN PLOT 
  
   p1 <- ggpubr::ggarrange(main, hospitalised, non_hospitalised, ncol=3, nrow=1, common.legend = FALSE, legend = "none",
                           labels = c("A: All COVID-19", "B: Hospitalised-COVID-19", "C: Non-Hospitalised-COVID-19"),
                           hjust = -0.1,
                           font.label = list(size = 12)) 
  
  # p1 <- ggpubr::ggarrange(main, non_hospitalised, ncol=2, nrow=1, common.legend = FALSE, legend = "none",
  #                         labels = c("A: All COVID-19", "C: Non-Hospitalised-COVID-19"),
  #                         hjust = -0.1,
  #                         font.label = list(size = 12)) 
  
  # EXTRACT LEGEND
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<- g_legend(non_hospitalised) 
  
  table.p <- table.p + theme(plot.margin = margin(0,3,0.7,0, "cm"))
  
  # ADD BLANK TO GET SPACING CORRECT
  
  blank <- grid.rect(gp=gpar(col="white"))
  p2 <- ggarrange(blank, mylegend, table.p, ncol = 3, widths = c(0.1,0.0005,1))
  
  # SAVE PLOT WITH TABLE
  
  png(paste0(output_dir,"Figure_1_3panel_with_table_",short_outcome,".png"),
      units = "mm", width=330, height=195, res = 1000)
  print(ggpubr::ggarrange(p1, 
                    p2,
                    nrow = 2,
                    heights = c(1, 0.15)) )
  # annotation_custom(ggplotGrob(table.p),
  #                   xmin = 5.5, ymin = 20,
  #                   xmax = 8)
  
  dev.off() 
  
}



# tab_add_footnote(text = "Pre-vaccinated (1 Jan 2020 to 18 June 2021), Vaccinated (1 Jun 2021 to 14 Dec 2021), Unvaccinated (1 Jun 2021 to 14 Dec 2021)", size = 7, face = "italic", hjust = 1.25)
# levels(table2_merged$Cohort) <- list("Pre-vaccinated (2020-01-01 - 2021-06-18)"="Pre-Vaccination", "Vaccinated (2021-06-01 - 2021-12-14)"="Vaccinated","Unvaccinated (2021-06-01 - 2021-12-14)"="Unvaccinated")

# PLOTTING ----------------------------------------------------------------

# MAIN PLOT 

p1 <- ggpubr::ggarrange(main_ate, hosp, non_hosp, ncol=3, nrow=1, common.legend = FALSE, legend = "none",
                        labels = c("A: All COVID-19", "B: Hospitalised-COVID-19", "C: Non-Hospitalised-COVID-19"),
                        hjust = -0.1,
                        font.label = list(size = 12)) 

p1 <- ggpubr::ggarrange(main_ate, non_hospitalised_ate, ncol=2, nrow=1, common.legend = FALSE, legend = "none",
                        labels = c("A: All COVID-19", "C: Non-Hospitalised-COVID-19"),
                        hjust = -0.1,
                        font.label = list(size = 12)) 

# EXTRACT LEGEND

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<- g_legend(non_hospitalised_ate) 

table.p <- table.p + theme(plot.margin = margin(0,3,0.7,0, "cm"))

# ADD BLANK TO GET SPACING CORRECT

blank <- grid.rect(gp=gpar(col="white"))
p2 <- ggarrange(blank, mylegend, table.p, ncol = 3, widths = c(0.1,0.0005,1))

# SAVE PLOT WITH TABLE

png(paste0(output_dir,"Figure_1_t2dm_3panel_with_table.png"),
    units = "mm", width=330, height=195, res = 1000)
ggpubr::ggarrange(p1, 
                  p2,
                  nrow = 2,
                  heights = c(1, 0.15)) 
# annotation_custom(ggplotGrob(table.p),
#                   xmin = 5.5, ymin = 20,
#                   xmax = 8)
dev.off() 
