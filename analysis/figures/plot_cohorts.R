# Define plot_cohorts function -------------------------------------------------

plot_cohorts <- function(outcome, prevax = NULL, vax = NULL, unvax = NULL) {
  
  ## Load and filter prevax data -----------------------------------------------
  
  if (!is.null(prevax)) {
    
    df_prevax <- data.table::fread(prevax,
                                   select = c("term","model","estimate","conf.low","conf.high","subgroup"),
                                   data.table = FALSE)
    
    df_prevax <- df_prevax[grepl("days",df_prevax$term) & df_prevax$subgroup=="main",]
    df_prevax$subgroup <- NULL
    df_prevax$cohort <- "Pre-vaccination cohort"
    
  } else {
    
    df_prevax <- data.frame(term = character(),
                            model = character(),
                            estimate = numeric(),
                            conf.low = numeric(),
                            conf.high = numeric(),
                            cohort = character())
    
  }
  
  ## Load and filter vax data --------------------------------------------------
  
  if (!is.null(vax)) {
    
    df_vax <- data.table::fread(vax,
                                select = c("term","model","estimate","conf.low","conf.high","subgroup"), 
                                data.table = FALSE)
    
    df_vax <- df_vax[grepl("days",df_vax$term) & df_vax$subgroup=="main",]
    df_vax$subgroup <- NULL
    df_vax$cohort <- "Vaccinated cohort"
    
  } else {
    
    df_vax <- data.frame(term = character(),
                         estimate = numeric(),
                         model = character(),
                         conf.low = numeric(),
                         conf.high = numeric(),
                         cohort = character())
    
  }
  
  ## Load and filter unvax data ------------------------------------------------
  
  if (!is.null(unvax)) {
    
    df_unvax <- data.table::fread(unvax, 
                                  select = c("term","model","estimate","conf.low","conf.high","subgroup"), 
                                  data.table = FALSE)
    
    df_unvax <- df_unvax[grepl("days",df_unvax$term)  & df_unvax$subgroup=="main",]
    df_unvax$subgroup <- NULL
    df_unvax$cohort <- "Unvaccinated cohort"
    
  } else {
    
    df_unvax <- data.frame(term = character(),
                           model = character(),
                           estimate = numeric(),
                           conf.low = numeric(),
                           conf.high = numeric(),
                           cohort = character())
    
  }
  
  ## Make single dataset for plotting ------------------------------------------
  
  df <- rbind(df_prevax, df_vax, df_unvax)
  
  ## Assign time for plotting --------------------------------------------------
  
  df$start <- as.numeric(gsub("days","",sub('\\_.*', '', df$term)))
  df$stop <- as.numeric(gsub(".*_", "",df$term))
  df$time <- df$start + ((df$stop-df$start)/2)
  
  ## Make variables numeric ----------------------------------------------------
  
  df$time <- as.numeric(df$time)
  df$estimate <- as.numeric(df$estimate)
  df$conf.low <- as.numeric(df$conf.low)
  df$conf.high <- as.numeric(df$conf.high)
  
  ## Plot ----------------------------------------------------------------------
  
  max_weeks <- max(df$stop)/7
  max_hr <- max(df$conf.high)
  
  ggplot2::ggplot(data=df, 
                  mapping = ggplot2::aes(x=time/7, y = estimate, color = model, fill=model)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                                  ymax = ifelse(conf.high>64,64,conf.high),  
                                                  width = 0), 
                           position = ggplot2::position_dodge(width = 1)) +   
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 1)) +    
    ggplot2::scale_y_continuous(lim = c(0.5,max_hr), breaks = 2^seq(-1,10), trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,max_weeks), breaks = seq(0,400,4)) +
    ggplot2::scale_fill_manual(values = c("#bababa","#000000"), 
                               breaks = c("mdl_agesex","mdl_max_adj"), 
                               labels = c("Age and sex adjustment","Extensive adjustment"))+ 
    ggplot2::scale_color_manual(values = c("#bababa","#000000"), 
                                breaks = c("mdl_agesex","mdl_max_adj"), 
                                labels = c("Age and sex adjustment","Extensive adjustment")) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +    
    ggplot2::facet_wrap(cohort~., nrow = 1)
  
  ## Save plot -----------------------------------------------------------------
  
  cohorts <- sum(!is.null(prevax), !is.null(vax), !is.null(unvax))
  
  ggplot2::ggsave(paste0("output/cohorts_",outcome,".png"), height = 210/2, width = 297*(cohorts/3), unit = "mm", dpi = 600, scale = 1)
  
}

# Make and save plots using plot_cohorts function ------------------------------

for (i in c("ami","angina","ate","dvt","hf","stroke_isch")) {
  
  plot_cohorts(outcome = i,
               prevax = paste0("~/OneDrive - University of Bristol/grp-EHR/Projects/CCU002_01/estimates-",i,".csv"), 
               vax = paste0("~/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-cardiovascular/OS output/Hazard ratios/suppressed_compiled_HR_results_",i,"_vaccinated_to_release.csv"), 
               unvax = paste0("~/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-cardiovascular/OS output/Hazard ratios/suppressed_compiled_HR_results_",i,"_electively_unvaccinated_to_release.csv"))
  
}

for (i in c("pe", "stroke_sah_hs", "tia","vte")) {
  
  plot_cohorts(outcome = i,
               prevax = paste0("~/OneDrive - University of Bristol/grp-EHR/Projects/CCU002_01/estimates-",i,".csv"), 
               vax = paste0("~/OneDrive - University of Bristol/grp-EHR/Projects/post-covid-cardiovascular/OS output/Hazard ratios/suppressed_compiled_HR_results_",i,"_vaccinated_to_release.csv"), 
               unvax = NULL)
  
}