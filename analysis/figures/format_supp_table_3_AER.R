library(dplyr)
library(readr)
library(stringr)
library(tidyr)

aer_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/compiled_results/"
results_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/release/"

#Read in AER
df <- readr::read_csv(paste0(aer_output_dir,"/AER_compiled_results.csv"))

#Select max day of follow up (196 for vax/unvax, 534 for pre-vax)
# df <- df %>%
#   group_by(event, subgroup, time_points, cohort) %>%
#   filter(days == max(days) & subgroup != "aer_overall" & !is.na(AER_main) ) %>%
#   select(event, subgroup, cohort, subgroup, AER_main, time_points, days)%>% ungroup()

df <- df %>% filter(days == 196 & subgroup != "aer_overall" & !is.na(AER_main)) %>%
  select(event, subgroup, cohort, subgroup, AER_main, time_points, days) %>% distinct()

#Sum over all subgroups to get total
df <- df %>% 
  group_by(event, time_points, cohort) %>%
 summarise(total = sum(AER_main))

#Add person years of follow up
table2_pre_vax <- read.csv(paste0(results_dir,"table2_pre_vaccination_cvd.csv"))
table2_vax <- read.csv(paste0(results_dir,"table2_vaccinated.csv"))
table2_unvax <- read.csv(paste0(results_dir,"table2_electively_unvaccinated.csv"))
table2_pre_vax <- table2_pre_vax %>% rename(cohort_to_run = cohort_name)

table2 <- rbind(table2_pre_vax,table2_vax,table2_unvax)
rm(table2_pre_vax,table2_vax,table2_unvax)

#To get total follow up for main analysis need to sum unexposed person days of follow up & exposed person days
table2 <- table2 %>% select(event, subgroup, cohort_to_run, total_person_days, unexposed_person_days) %>% 
  filter(subgroup %in% c("covid_pheno_hospitalised","covid_pheno_non_hospitalised"))

table2$exposed_person_days <- table2$total_person_days - table2$unexposed_person_days
table2$total_person_days <- NULL
table2$unexposed_person_days <- NULL

#Sum follow up
table2 <- table2 %>% group_by(event, cohort_to_run) %>%
  summarise(total_exposed_follow_up = sum(exposed_person_days))

#Convert from days to years
table2$total_exposed_follow_up <- table2$total_exposed_follow_up/365.2
table2$event <- gsub("out_date_","",table2$event)

#Left join follow up onto AER table
df <- df %>% left_join(table2, by = c("event"="event", "cohort"="cohort_to_run"))
df$excess_events_per_1000_person_years <- df$total * (1000/df$total_exposed_follow_up)

df <- pivot_longer(df, cols = c(total, total_exposed_follow_up, excess_events_per_1000_person_years), names_to = "summary", values_to = "total")
df <- pivot_wider(df, names_from = cohort, values_from = total)

df <- df %>% rename("Pre-vaccination cohort"="pre_vaccination",
                    "Vaccinated cohort"="vaccinated",
                    "Unvaccinated cohort"="electively_unvaccinated")

df$summary <- ifelse(df$summary == "total", "Total excess events",df$summary)
df$summary <- ifelse(df$summary == "total_exposed_follow_up", "Total post exposure follow up (years)",df$summary)
df$summary <- ifelse(df$summary == "excess_events_per_1000_person_years", "Excess events per 1000 person years",df$summary)

#Get tidy names
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  mutate(outcome_name=active_analyses$outcome_variable %>% str_replace("out_date_", ""))%>%
  select(outcome, outcome_name)

df <- df %>% left_join(outcome_name_table, by = c("event"="outcome_name"))
df$event <- NULL
df <- df %>% select(outcome,summary,time_points, `Pre-vaccination cohort`, `Vaccinated cohort`, `Unvaccinated cohort`)

for(i in c("any_position","primary_position")){
  for (j in c("reduced", "normal")){
    
    if(i == "any_position"){
      tmp <- df %>% filter(!outcome %in% outcome[grepl("Primary position events",outcome)]
                                 & time_points == j)
    }else{
      tmp <- df %>% filter(outcome %in% outcome[grepl("Primary position events",outcome)]
                                   & time_points == j)
    }
    
    
    tmp$time_points <- NULL
    #tmp$`Pre-vaccination cohort`<- round(tmp$`Pre-vaccination cohort`, digits = 0)
    #tmp$`Vaccinated cohort`<- round(tmp$`Vaccinated cohort`, digits = 0)
    #tmp$`Unvaccinated cohort`<- round(tmp$`Unvaccinated cohort`, digits = 0)
    write.csv(tmp, paste0(aer_output_dir,"supp_table_3_",i,"_events_",j,"_time_periods.csv"),row.names = F)
    
  }
}


