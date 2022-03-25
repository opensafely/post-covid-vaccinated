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

library(readr); library("ggvenn"); library("svglite"); library("gridExtra")

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  population <- "electively_unvaccinated"
} else {
  population <- args[[1]]
}

venn_output <- function(population){
  
  # Identify active outcomes ---------------------------------------------------
  
  active_analyses <- readr::read_rds("lib/active_analyses.rds")
  outcomes <- active_analyses[active_analyses$active==TRUE,]$outcome_variable
  
  # Load data ------------------------------------------------------------------
  
  input <- readr::read_rds(paste0("output/venn_",population,".rds"))
  input_stage1 <- readr::read_rds(paste0("output/input_", population,"_stage1.rds"))
  input <- input[input$patient_id %in% input_stage1$patient_id,]
  
  # Create empty table ---------------------------------------------------------
  
  df <- data.frame(outcome = character(),
                   snomed = numeric(),
                   hes = numeric(),
                   death = numeric(),
                   snomed_hes = numeric(),
                   snomed_death = numeric(),
                   hes_death = numeric(),
                   snomed_hes_death = numeric(),
                   total = numeric(),
                   stringsAsFactors = FALSE)
  
  # Populate table and make Venn for each outcome ------------------------------
  
  for (outcome in outcomes) {
    
    # Restrict data to that relevant to the given outcome ----------------------
    
    tmp <- input[!is.na(input[,outcome]),c("patient_id",colnames(input)[grepl(outcome,colnames(input))])]
    
    # Identify and add missing columns -----------------------------------------
    
    complete <- data.frame(patient_id = tmp$patient_id,
                           snomed = as.Date(NA),
                           hes = as.Date(NA),
                           death = as.Date(NA))
    
    colnames(complete) <- c("patient_id",paste0("tmp_",outcome,c("_snomed","_hes","_death")))
    
    complete[,setdiff(colnames(tmp),"patient_id")] <- NULL
    notused <- NULL
    
    if (ncol(complete)>1) {
      tmp <- merge(tmp, complete, by = c("patient_id"))
      notused <- gsub(paste0("tmp_",outcome,"_"),"",setdiff(colnames(complete),"patient_id"))
    }
    
    # Calculate the number contributing to each source combo -------------------
    
    tmp$snomed <- !is.na(tmp[,paste0("tmp_",outcome,"_snomed")]) & 
      is.na(tmp[,paste0("tmp_",outcome,"_hes")]) & 
      is.na(tmp[,paste0("tmp_",outcome,"_death")])
    
    tmp$hes <- is.na(tmp[,paste0("tmp_",outcome,"_snomed")]) & 
      !is.na(tmp[,paste0("tmp_",outcome,"_hes")]) & 
      is.na(tmp[,paste0("tmp_",outcome,"_death")])
    
    tmp$death <- is.na(tmp[,paste0("tmp_",outcome,"_snomed")]) & 
      is.na(tmp[,paste0("tmp_",outcome,"_hes")]) & 
      !is.na(tmp[,paste0("tmp_",outcome,"_death")])
    
    tmp$snomed_hes <- !is.na(tmp[,paste0("tmp_",outcome,"_snomed")]) & 
      !is.na(tmp[,paste0("tmp_",outcome,"_hes")]) & 
      is.na(tmp[,paste0("tmp_",outcome,"_death")])
    
    tmp$hes_death <- is.na(tmp[,paste0("tmp_",outcome,"_snomed")]) & 
      !is.na(tmp[,paste0("tmp_",outcome,"_hes")]) & 
      !is.na(tmp[,paste0("tmp_",outcome,"_death")])
    
    tmp$snomed_death <- !is.na(tmp[,paste0("tmp_",outcome,"_snomed")]) & 
      is.na(tmp[,paste0("tmp_",outcome,"_hes")]) & 
      !is.na(tmp[,paste0("tmp_",outcome,"_death")])
    
    tmp$snomed_hes_death <- !is.na(tmp[,paste0("tmp_",outcome,"_snomed")]) & 
      !is.na(tmp[,paste0("tmp_",outcome,"_hes")]) & 
      !is.na(tmp[,paste0("tmp_",outcome,"_death")])
    
    df[nrow(df)+1,] <- c(outcome,
                         snomed = sum(tmp$snomed),
                         hes = sum(tmp$hes),
                         death = sum(tmp$death),
                         snomed_hes = sum(tmp$snomed_hes),
                         snomed_death = sum(tmp$snomed_death),
                         hes_death = sum(tmp$hes_death),
                         snomed_hes_death = sum(tmp$snomed_hes_death),
                         total = nrow(tmp))
    
    # Remove sources not in study definition from Venn plots -------------------
    
    consider <- c("snomed","hes","death","snomed_hes","snomed_death","hes_death","snomed_hes_death")
    
    if (!is.null(notused)) {
      for (i in notused) {
        consider <- consider[!grepl(i,consider)]
      }
    }

    # Proceed to create Venn diagram if all source combos exceed 5 -------------
    
    if (min(as.numeric(df[df$outcome==outcome,consider]))>5) {
      
      # Calculate contents of each Venn cell for plotting ----------------------
      
      index1 <- integer(0)
      index2 <- integer(0)
      index3 <- integer(0)
      
      if ("snomed" %in% consider) {
        index1 <- which(!is.na(tmp[,paste0("tmp_",outcome,"_snomed")]))
      }
      if ("hes" %in% consider) {
        index2 <- which(!is.na(tmp[,paste0("tmp_",outcome,"_hes")]))
      }
      if ("death" %in% consider) {
        index3 <- which(!is.na(tmp[,paste0("tmp_",outcome,"_death")]))
      }
      
      index <- list(index1, index2, index3)
      names(index) <- c("SNOMED", "Hospital Episodes", "Deaths")
      index <- Filter(length, index)
      
      # Fix colours --------------------------------------------------------------
      
      mycol <- c(ifelse("SNOMED" %in% names(index),"thistle",""),
                 ifelse("Hospital Episodes" %in% names(index),"lightcyan",""),
                 ifelse("Deaths" %in% names(index),"lemonchiffon",""))
      
      mycol <- mycol[mycol!=""]
      
      # Make Venn diagram --------------------------------------------------------
      
      svglite(file = paste0("output/venn_diagram_",population,"_",gsub("out_date_","",outcome),".svg"))
      g <- ggvenn(
        index, 
        fill_color = mycol,
        stroke_color = "white",
        text_size = 5,
        set_name_size = 5, 
        fill_alpha = 0.9
      ) +  ggtitle(active_analyses[active_analyses$outcome_variable==outcome,]$outcome) +
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
      print(g)
      dev.off()
      
    }
    
  }
  
  # Save summary file ----------------------------------------------------------
  
  write.csv(df, file = paste0("output/venn_diagram_number_check_", population,".csv"), row.names = F)
  
}

# Run function using specified commandArgs -------------------------------------

if(population == "both"){
  venn_output("electively_unvaccinated")
  venn_output("vaccinated")
} else{
  venn_output(population)
}