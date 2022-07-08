# Load packages ----------------------------------------------------------------

library(magrittr)
library(data.table)

# Specify arguments ------------------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  event_name <- "ami"
  cohort <- "vaccinated"
}else{
  event_name <- args[[1]]
  cohort <- args[[2]]
}

for(i in c("main_analysis","hospitalised_analysis" )){
  
  # Load data --------------------------------------------------------------------
  
  input <- readr::read_rds(paste0("output/input_",cohort,"_stage1.rds"))
  
  # Add end dates ----------------------------------------------------------------
  
  end_dates <- readr::read_rds(paste0("output/follow_up_end_dates_",cohort,".rds"))
  
  if(i == "main_analysis"){
    end_dates <- end_dates[,c("patient_id",
                              colnames(end_dates)[grepl(paste0(event_name,"_follow_up_end"),colnames(end_dates))])]
                             
    end_dates <- dplyr::rename(end_dates,
                              "follow_up_end" = paste0(event_name,"_follow_up_end")) 
    
  }else if(i == "hospitalised_analysis"){
    
    end_dates <- end_dates[,c("patient_id",
                              colnames(end_dates)[grepl(paste0(event_name,"_hospitalised_follow_up_end"),colnames(end_dates))],
                              colnames(end_dates)[grepl(paste0(event_name,"_hospitalised_date_expo_censor"),colnames(end_dates))])]
    
    end_dates <- dplyr::rename(end_dates,
                               "follow_up_end" = paste0(event_name,"_hospitalised_follow_up_end"),
                               "date_expo_censor" = paste0(event_name,"_hospitalised_date_expo_censor"))
  }
  
  
  input <- input %>% dplyr::left_join(end_dates, by = "patient_id")
  rm(end_dates)
  
  # Remove those with a history of COVID -----------------------------------------
  
  input <- input %>% dplyr::filter(sub_bin_covid19_confirmed_history == FALSE)
  
  # Adjust follow-up end date to take into account censor date
  # Not sure if there is a better way to do this - ifelse not great with dates but then creating
  # and uncreating a data table ?
  if(i == "hospitalised_analysis"){
    setDT(input)[follow_up_end == date_expo_censor, follow_up_end := follow_up_end-1]
    input <- as.data.frame(input)
  }
  
  # Filter variables -------------------------------------------------------------
  
  keep <- c("patient_id",
            "index_date",
            "follow_up_end",
            "exp_date_covid19_confirmed",
            paste0("out_date_",event_name), 
            colnames(input)[grepl("cov_",colnames(input))])
  
  input <- input[,keep]
  
  # Save reusable action input ---------------------------------------------------
  
  data.table::fwrite(input, paste0("output/reusableaction_input_",cohort,"_",event_name,"_",i,".csv"))
  
}

