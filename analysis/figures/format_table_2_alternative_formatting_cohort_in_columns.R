library(dplyr)
library(broman)
# Load data --------------------------------------------------------------------

results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"
output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/table_2/"
dir.create(file.path(output_dir), recursive =TRUE, showWarnings = FALSE)


#############################################################
# Alternative formatting of table 2 with cohorts as columns #
#############################################################

# Get active analysis table for labels -----------------------------------------
active_analyses <- readr::read_rds("lib/active_analyses.RDS")

# Get data from each cohort ----------------------------------------------------
table2_pre_vax <- read.csv(paste0(results_dir,"table2_pre_vaccination_cvd.csv"))
table2_vax <- read.csv(paste0(results_dir,"table2_vaccinated.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_electively_unvaccinated.csv"))


# Format data from each cohort -------------------------------------------------

format_table_2_short<- function(df, cohort){
  table2_raw <- df
  
  table2_raw <- table2_raw %>% select(subgroup,event,unexposed_event_count, post_exposure_event_count, unexposed_person_days, total_person_days)%>%
    filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))
  
  table2_raw$post_exposure_person_days <- table2_raw$total_person_days - table2_raw$unexposed_person_days
  
  table2_pre_expo <- table2_raw %>% select(subgroup, event, unexposed_event_count, unexposed_person_days)
  table2_pre_expo$period <- "unexposed"
  table2_pre_expo <- table2_pre_expo %>% rename(event_count = unexposed_event_count,
                                                person_days = unexposed_person_days)
  
  table2_post_expo <- table2_raw %>% select(subgroup, event, post_exposure_event_count, post_exposure_person_days)
  table2_post_expo$period <- "post_expo"
  table2_post_expo <- table2_post_expo %>% rename(event_count = post_exposure_event_count,
                                                  person_days = post_exposure_person_days )
  
  table2 <- rbind(table2_pre_expo,table2_post_expo)
  rm(table2_pre_expo,table2_post_expo,table2_raw)
  
  table2 <- table2 %>% mutate(across(c("event_count","person_days"), as.numeric))
  
  # Add in incidence rate
  table2[,"Incidence rate (per 100,000 person-years)"] <- add_commas(round((table2$event_count/(table2$person_days/365.25))*100000))
  table2[,"Event counts/100,000 person-years"] <- paste0(add_commas(table2$event_count), "/", add_commas(round((table2$person_days/365.25))))
  
  
  table2$period <- ifelse(table2$period=="unexposed","No COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_hospitalised","After hospitalised COVID-19",table2$period)
  table2$period <- ifelse(table2$period=="post_expo" & table2$subgroup == "covid_pheno_non_hospitalised","After non-hospitalised COVID-19",table2$period)
  
  table2[,c("subgroup","person_days")] <- NULL
  table2 <- table2[!duplicated(table2), ]


  # Tidy event labels ----------------------------------------------------------
  table2 <- table2 %>% left_join(active_analyses %>% select(outcome_variable,outcome), by=c("event"="outcome_variable"))
  table2$event <- NULL

  #Split primary position event
  table2 <- table2 %>% rename(Outcome = outcome)

  table2 <- table2 %>% filter(!grepl('Primary position',Outcome))
  
  # -> Not using primary position results for table 2 in CVD paper
  #table2_primary_position <- table2 %>% filter(grepl('Primary position',Outcome))
  
  # Re-order columns -----------------------------------------------------------
  table2 <- table2 %>% select("Outcome","period","Event counts/100,000 person-years","Incidence rate (per 100,000 person-years)")

  table2$cohort <- cohort

  return(table2)
}

table2_pre_vax_formatted <- format_table_2_short(table2_pre_vax,"Pre-vaccination")
table2_vax_formatted <- format_table_2_short(table2_vax,"Vaccinated")
table2_unvax_formatted <- format_table_2_short(table2_unvax,"Unvaccinated")

# Combine results for all cohorts ----------------------------------------------
table2_merged <- rbind(table2_pre_vax_formatted,table2_vax_formatted,table2_unvax_formatted)

# Make columns for cohort ------------------------------------------------------

#table2_transposed <- tidyr::pivot_wider(table2_merged, names_from = "cohort", values_from = "Event counts/100,000 person-years")
table2_transposed <- tidyr::pivot_wider(table2_merged, names_from = "cohort", values_from = c("Event counts/100,000 person-years","Incidence rate (per 100,000 person-years)"))

# Add labels and re-order rows -------------------------------------------------
table2_transposed <- table2_transposed[,c("Outcome","period","Event counts/100,000 person-years_Pre-vaccination","Incidence rate (per 100,000 person-years)_Pre-vaccination",
                               "Event counts/100,000 person-years_Vaccinated","Incidence rate (per 100,000 person-years)_Vaccinated",
                               "Event counts/100,000 person-years_Unvaccinated","Incidence rate (per 100,000 person-years)_Unvaccinated")]

table2_transposed <- rbind(table2_transposed,c("Other vascular events",rep(NA,7)))
table2_transposed <- rbind(table2_transposed,c("Arterial thrombosis events",rep(NA,7)))
table2_transposed <- rbind(table2_transposed,c("Venous thromboembolism events",rep(NA,7)))
table2_transposed <- rbind(table2_transposed,c(rep("table_sub_heading",2),sub('\\_.*', '', colnames(table2_transposed)[3:8])))

colnames(table2_transposed) <- c("Outcome","period","Pre-vaccination cohort (N=18,210,937)","Pre-vaccination cohort (N=18,210,937)",
                                 "Vaccinated cohort (N=13,572,399)","Vaccinated cohort (N=13,572,399)",
                                 "Unvaccinated cohort (N=3,161,485)","Unvaccinated cohort (N=3,161,485)") 
                                 
table2_transposed$Outcome <- factor(table2_transposed$Outcome, levels=c("table_sub_heading",
                                                                "Arterial thrombosis events",
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

table2_transposed$period <- factor(table2_transposed$period, levels = c("No COVID-19",
                                                                        "After hospitalised COVID-19",
                                                                        "After non-hospitalised COVID-19"))

table2_transposed <- table2_transposed[order(table2_transposed$Outcome, table2_transposed$period),]

write.csv(table2_transposed, paste0(output_dir,"formatted_table_2_cohorts_in_columns_updated.csv"),row.names = F)
