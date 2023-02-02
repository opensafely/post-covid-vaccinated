library(dplyr)
# Load data --------------------------------------------------------------------

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/table_2/"
dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)

format_table_2 <- function(df, cohort){
  table2_raw <- df
  
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
  write.csv(table2,paste0(results_dir,"table_2_venn_diagram_number_check_",cohort,".csv"),row.names = F)
  
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
  
  table2$cohort <- cohort
  
  table2_primary_postion$cohort <- cohort
  return(list(table2,table2_primary_postion))
}


# Combine across cohorts
#table2_pre_vax <- read.csv(paste0(results_dir,"table2_pre_vaccination.csv"))
table2_pre_vax_extFU <- read.csv(paste0(results_dir,"table2_pre_vaccination_extended_follow_up_outcomes_cvd.csv"))
table2_pre_vax_extFU <- table2_pre_vax_extFU %>% dplyr::rename(cohort_to_run = cohort_name)
table2_pre_vax_extFU$event <- gsub('_extended_follow_up', '', table2_pre_vax_extFU$event)

table2_vax <- read.csv(paste0(results_dir,"table2_vaccinated.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_electively_unvaccinated.csv"))

table2_pre_vax <- format_table_2(table2_pre_vax_extFU,"Pre-vaccinated")
table2_vax <- format_table_2(table2_vax,"Vaccinated")
table2_unvax <- format_table_2(table2_unvax,"Electively unvaccinated")

table2_pre_vax_primary_position <- table2_pre_vax[[2]]
table2_vax_primary_position <- table2_vax[[2]]
table2_unvax_primary_position <- table2_unvax[[2]]

table2_pre_vax <- table2_pre_vax[[1]]
table2_vax <- table2_vax[[1]]
table2_unvax <- table2_unvax[[1]]

table2_merged <- rbind(table2_pre_vax,table2_vax,table2_unvax)
table2_merged$cohort <- ifelse(table2_merged$Outcome %in% c("Arterial thrombosis events","Venous thromboembolism events","Other vascular events"),NA,table2_merged$cohort)
table2_merged <- table2_merged[!duplicated(table2_merged),]

table2_merged$Outcome <- factor(table2_merged$Outcome, levels=c("Arterial thrombosis events",
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


table2_merged$cohort <- factor(table2_merged$cohort, levels = c("Pre-vaccinated",
                                                                "Vaccinated",
                                                                "Electively unvaccinated"))

table2_merged <- table2_merged[order(table2_merged$Outcome, table2_merged$cohort),]
table2_merged <- table2_merged %>% select("Outcome","cohort","No COVID-19","After hospitalised COVID-19",
                                          "After non-hospitalised COVID-19","Total")


write.csv(table2_merged, paste0(output_dir,"formatted_table_2_extended_FU.csv"),row.names = F)

# Primary position events
table2_merged_primary_position <- rbind(table2_pre_vax_primary_position,table2_vax_primary_position,table2_unvax_primary_position)
table2_merged_primary_position$cohort <- ifelse(table2_merged_primary_position$Outcome %in% c("Arterial thrombosis events - Primary position events","Venous thromboembolism events - Primary position events","Other vascular events - Primary position events"),NA,table2_merged_primary_position$cohort)
table2_merged_primary_position <- table2_merged_primary_position[!duplicated(table2_merged_primary_position),]

table2_merged_primary_position$Outcome <- factor(table2_merged_primary_position$Outcome, levels=c("Arterial thrombosis events - Primary position events",
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


table2_merged_primary_position$cohort <- factor(table2_merged_primary_position$cohort, levels = c("Pre-vaccinated",
                                                                "Vaccinated",
                                                                "Electively unvaccinated"))

table2_merged_primary_position <- table2_merged_primary_position[order(table2_merged_primary_position$Outcome, table2_merged_primary_position$cohort),]
table2_merged_primary_position <- table2_merged_primary_position %>% select("Outcome","cohort","No COVID-19","After hospitalised COVID-19",
                                          "After non-hospitalised COVID-19","Total")

write.csv(table2_merged_primary_position, paste0(output_dir,"formatted_table_2_primary_position.csv"),row.names = F)


