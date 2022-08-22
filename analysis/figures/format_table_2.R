library(dplyr)
# Load data --------------------------------------------------------------------

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/table_2/"
dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

cohort <- c("vaccinated","electively_unvaccinated","pre_vaccination")

for(i in cohort){
  print(paste0("Working on table 2 ", i))
  table2_raw <- read.csv(paste0(results_dir,"table2_", i, ".csv"))
  
  table2_raw <- table2_raw %>% select(subgroup,event,unexposed_event_count, post_exposure_event_count)%>%
    filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
  
  table2_pre_expo <- table2_raw %>% select(subgroup, event, unexposed_event_count)
  table2_pre_expo$period <- "unexposed"
  table2_pre_expo <- table2_pre_expo %>% rename(event_count = unexposed_event_count)
  
  table2_post_expo <- table2_raw %>% select(subgroup, event, post_exposure_event_count)
  table2_post_expo$period <- "post_expo"
  table2_post_expo <- table2_post_expo %>% rename(event_count = post_exposure_event_count)
  
  table2 <- rbind(table2_pre_expo,table2_post_expo)
  
  rm(table2_pre_expo,table2_post_expo,table2_raw)
  
  
  table2$period <- ifelse(table2$period=="unexposed","No COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_hospitalised","After hospitalised COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_non_hospitalised","After non-hospitalised COVID-19",table2$period)
  
  table2[,"subgroup"] <- NULL
  table2 <- table2[!duplicated(table2), ]
  
  # Make columns for exposure time -----------------------------------------------
  
  table2 <- tidyr::pivot_wider(table2, names_from = "period", values_from = "event_count")
  
  #Add totals columm
  table2 <- table2 %>% mutate(across(c("No COVID-19","After hospitalised COVID-19","After non-hospitalised COVID-19"),as.numeric))
  table2$Total <- rowSums(table2[,c(2,3,4)])
  
  #Save table 2 for venn diagram numbers check
  data.table::fwrite(table2,paste0(results_dir,"table_2_venn_diagram_number_check_",i,".csv"))
  
  # Tidy event labels ------------------------------------------------------------
  active_analyses <- readr::read_rds("lib/active_analyses.RDS")
  
  table2 <- table2 %>% left_join(active_analyses %>% select(outcome_variable,outcome), by=c("event"="outcome_variable"))
  table2$event <- NULL

  #Split primary position event
  table2 <- table2  %>% rename(Outcome = outcome)
  table2 <- table2[,c(5,1,2,3,4)] 
  
  table2_primary_postion <- table2 %>% filter(grepl('Primary position',Outcome))
  table2 <- table2 %>% filter(!grepl('Primary position',Outcome))
  
  # Re-order rows and add empty rows ---------------------------------------------------------------
  
  table2 <- rbind(table2,c("Other vascular events",rep(NA,4)))
  table2 <- rbind(table2,c("Arterial thrombosis events",rep(NA,4)))
  table2 <- rbind(table2,c("Venous thromboembolism events",rep(NA,4)))
  
  table2_primary_postion <- rbind(table2_primary_postion,c("Other vascular events - Primary position events",rep(NA,4)))
  table2_primary_postion <- rbind(table2_primary_postion,c("Arterial thrombosis events - Primary position events",rep(NA,4)))
  table2_primary_postion <- rbind(table2_primary_postion,c("Venous thromboembolism events - Primary position events",rep(NA,4)))
  
  table2$Outcome <- factor(table2$Outcome, levels=c("Arterial thrombosis events",
                                                    "Arterial thrombosis event",
                                                    "Acute myocardial infarction",
                                                    "Ischaemic stroke",
                                                    "Venous thromboembolism events",
                                                    "Venous thrombosis event",
                                                    "Pulmonary embolism",
                                                    "Deep vein thrombosis",
                                                    "Other vascular events",
                                                    "Heart failure",
                                                    "Angina",
                                                    "Transient ischaemic attack",
                                                    "Subarachnoid haemorrhage and haemorrhagic stroke")) 
  
  table2 <- table2[order(table2$Outcome),]
  
  table2_primary_postion$Outcome <- factor(table2_primary_postion$Outcome, levels=c("Arterial thrombosis events - Primary position events",
                                                                                    "Arterial thrombosis event - Primary position events",
                                                                                    "Acute myocardial infarction - Primary position events",
                                                                                    "Ischaemic stroke - Primary position events",
                                                                                    "Venous thromboembolism events - Primary position events",
                                                                                    "Venous thrombosis event - Primary position events",
                                                                                    "Pulmonary embolism - Primary position events",
                                                                                    "Deep vein thrombosis - Primary position events",
                                                                                    "Other vascular events - Primary position events",
                                                                                    "Heart failure - Primary position events",
                                                                                    "Angina - Primary position events",
                                                                                    "Transient ischaemic attack - Primary position events",
                                                                                    "Subarachnoid haemorrhage and haemorrhagic stroke - Primary position events")) 
  

  table2_primary_postion <- table2_primary_postion[order(table2_primary_postion$Outcome),]
  
  
  # Save -------------------------------------------------------------------------
  
  data.table::fwrite(table2,paste0(output_dir,"formatted_main_table_2_",i,".csv"))
  data.table::fwrite(table2_primary_postion,paste0(output_dir,"formatted_main_table_2_primary_position_events_",i,".csv"))
  
  
}
