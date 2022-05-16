## =============================================================================
## Purpose:  Create venn diagrams
## 
## Author:   Yinghui Wei
##
## Reviewer: Renin Toms, Venexia Walker
##
## Date:     6 December 2021; updated 10 January 2022; updated 27 January 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  to create a Venn diagram for each outcome outlining overlap in 
##           reporting from different data sources
## Output:   Venn diagrams in SVG files, venn_diagram_number_check.csv
## =============================================================================
library(data.table)
library(readr)
library(dplyr)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  population <- "vaccinated"
} else {
  population <- args[[1]]
}

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "venn-diagrams"))

venn_output <- function(population){
  
  # Identify active outcomes ---------------------------------------------------
  
  active_analyses <- readr::read_rds("lib/active_analyses.rds")
  outcomes <- active_analyses[active_analyses$active==TRUE,]$outcome_variable
  
  # Load data ------------------------------------------------------------------
  
  input <- readr::read_rds(paste0("output/venn_",population,".rds"))
  end_dates <- read_rds(paste0("output/follow_up_end_dates_",population,".rds"))
  
  input_stage1 <- readr::read_rds(paste0("output/input_", population,"_stage1.rds"))
  input_stage1 <- input_stage1[input_stage1$sub_bin_covid19_confirmed_history==FALSE,]
  
  input <- input[input$patient_id %in% input_stage1$patient_id,]
  input<- input %>% left_join(end_dates, by="patient_id")
  
  rm(input_stage1,end_dates)
  
  # Create empty table ---------------------------------------------------------
  
  df <- data.frame(outcome = character(),
                   only_snomed = numeric(),
                   only_hes = numeric(),
                   only_death = numeric(),
                   snomed_hes = numeric(),
                   snomed_death = numeric(),
                   hes_death = numeric(),
                   snomed_hes_death = numeric(),
                   total_snomed = numeric(),
                   total_hes = numeric(),
                   total_death = numeric(),
                   total = numeric(),
                   stringsAsFactors = FALSE)
  
  # Populate table and make Venn for each outcome ------------------------------

    for (outcome in outcomes) {
    # Restrict data to that relevant to the given outcome ----------------------
    tmp <- input[!is.na(input[,outcome]),c("patient_id","index_date",paste0(gsub("out_date_","", outcome),"_follow_up_end"), colnames(input)[grepl(outcome,colnames(input))])]
    colnames(tmp) <- gsub(paste0("tmp_",outcome,"_"),"",colnames(tmp))
    setnames(tmp,
             old=c(paste0(gsub("out_date_","", outcome),"_follow_up_end"),
                   outcome),
             new=c("follow_up_end",
                  "event_date"))
    
    tmp <- tmp %>% filter(follow_up_end >= index_date)
    
    # Impose follow-up start and end dates on events dates
    
    event_cols <- c("snomed","hes","death","event_date")
    for(colname in event_cols){
      if(colname %in% colnames(tmp)){
        tmp <- tmp %>% mutate(!!sym(colname) := replace(!!sym(colname), which(!!sym(colname)>follow_up_end | !!sym(colname)<index_date), NA))
        
      }
    }
    
    # Identify and add missing columns -----------------------------------------
    
    complete <- data.frame(patient_id = tmp$patient_id,
                           snomed = as.Date(NA),
                           hes = as.Date(NA),
                           death = as.Date(NA))
    
    #colnames(complete) <- c("patient_id",paste0("tmp_",outcome,c("_snomed","_hes","_death")))
    
    complete[,setdiff(colnames(tmp),"patient_id")] <- NULL
    notused <- NULL
    
    if (ncol(complete)>1) {
      tmp <- merge(tmp, complete, by = c("patient_id"))
      notused <- setdiff(colnames(complete),"patient_id")
    }
    
    # Calculate the number contributing to each source combo -------------------
    
    tmp$snomed_contributing <- !is.na(tmp$snomed) & 
      is.na(tmp$hes) & 
      is.na(tmp$death)
    
    tmp$hes_contributing <- is.na(tmp$snomed) & 
      !is.na(tmp$hes) & 
      is.na(tmp$death)
    
    tmp$death_contributing <- is.na(tmp$snomed) & 
      is.na(tmp$hes) & 
      !is.na(tmp$death)
    
    tmp$snomed_hes_contributing <- !is.na(tmp$snomed) & 
      !is.na(tmp$hes) & 
      is.na(tmp$death)
    
    tmp$hes_death_contributing <- is.na(tmp$snomed) & 
      !is.na(tmp$hes) & 
      !is.na(tmp$death)
    
    tmp$snomed_death_contributing <- !is.na(tmp$snomed) & 
      is.na(tmp$hes) & 
      !is.na(tmp$death)
    
    tmp$snomed_hes_death_contributing <- !is.na(tmp$snomed) & 
      !is.na(tmp$hes) & 
      !is.na(tmp$death)
    df[nrow(df)+1,] <- c(outcome,
                         only_snomed = nrow(tmp %>% filter(snomed_contributing==T)),
                         only_hes = nrow(tmp %>% filter(hes_contributing==T)),
                         only_death = nrow(tmp %>% filter(death_contributing==T)),
                         snomed_hes = nrow(tmp %>% filter(snomed_hes_contributing==T)),
                         snomed_death = nrow(tmp %>% filter(snomed_death_contributing==T)),
                         hes_death = nrow(tmp %>% filter(hes_death_contributing==T)),
                         snomed_hes_death = nrow(tmp %>% filter(snomed_hes_death_contributing==T)),
                         total_snomed = nrow(tmp %>% filter(!is.na(snomed))),
                         total_hes = nrow(tmp %>% filter(!is.na(hes))),
                         total_death = nrow(tmp %>% filter(!is.na(death))),
                         total = nrow(tmp %>% filter(!is.na(event_date))))
    
    # Remove sources not in study definition from Venn plots and summary -------
    
    source_combos <- c("only_snomed","only_hes","only_death","snomed_hes","snomed_death","hes_death","snomed_hes_death")
    source_consid <- source_combos
    
    if (!is.null(notused)) {
      for (i in notused) {
        
        # Add variables to consider for Venn plot to vector
        
        source_consid <- source_combos[!grepl(i,source_combos)]
        
        # Replace unused sources with NA in summary table
        
        for (j in setdiff(source_combos,source_consid)) {
          df[df$outcome==outcome,j] <- NA
        }
        
      }
    }

    # Proceed to create Venn diagram if all source combos exceed 5 -------------
    if (min(as.numeric(df[df$outcome==outcome,source_consid]))>5) {
      
      # Calculate contents of each Venn cell for plotting ----------------------
      
      index1 <- integer(0)
      index2 <- integer(0)
      index3 <- integer(0)
      
      if ("snomed" %in% source_consid) {
        index1 <- which(!is.na(tmp$snomed))
      }
      if ("hes" %in% source_consid) {
        index2 <- which(!is.na(tmp$hes))
      }
      if ("death" %in% source_consid) {
        index3 <- which(!is.na(tmp$death))
      }
      
      index <- list(index1, index2, index3)
      names(index) <- c("Primary care", "Secondary care", "Deaths")
      index <- Filter(length, index)
      
      # Fix colours --------------------------------------------------------------
      
      mycol <- c(ifelse("Primary care" %in% names(index),"thistle",""),
                 ifelse("Secondary care" %in% names(index),"lightcyan",""),
                 ifelse("Death record" %in% names(index),"lemonchiffon",""))
      
      mycol <- mycol[mycol!=""]
      
      # Make Venn diagram --------------------------------------------------------
      
      svglite::svglite(file = paste0("output/review/venn-diagrams/venn_diagram_",population,"_",gsub("out_date_","",outcome),".svg"))
      g <- ggvenn::ggvenn(
        index, 
        fill_color = mycol,
        stroke_color = "white",
        text_size = 5,
        set_name_size = 5, 
        fill_alpha = 0.9
      ) +  ggplot2::ggtitle(active_analyses[active_analyses$outcome_variable==outcome,]$outcome) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15, face = "bold"))
      print(g)
      dev.off()
      
    }
    
  }
  
  # Save summary file ----------------------------------------------------------
  
  write.csv(df, file = paste0("output/review/venn-diagrams/venn_diagram_number_check_", population,".csv"), row.names = F)
  
}

# Run function using specified commandArgs -------------------------------------

if(population == "both"){
  venn_output("electively_unvaccinated")
  venn_output("vaccinated")
} else{
  venn_output(population)
}
