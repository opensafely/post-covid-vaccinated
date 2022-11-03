aer_output_dir <- "C:/Users/zy21123/OneDrive - University of Bristol/Documents/OpenSAFELY/Outputs/Figures/AER/compiled_results/"

lifetables <- readr::read_csv(paste0(aer_output_dir,"/AER_compiled_results.csv"))

lifetables <- lifetables %>% 
  group_by(event, subgroup, time_points, cohort) %>%
  filter(days == max(days) & subgroup != "aer_overall" & !is.na(AER_main) ) %>%
  select(event, subgroup, cohort, subgroup, AER_main, time_points, days)%>% ungroup()

lifetables <- lifetables %>% 
  group_by(event, time_points, cohort) %>%
 summarise(total = sum(AER_main))

lifetables <- tidyr::pivot_wider(lifetables, names_from = "cohort", values_from = "total")

# Add tidy names
active_analyses <- read_rds("lib/active_analyses.rds") %>% filter(active == "TRUE")

outcome_name_table <- active_analyses %>% 
  select(outcome, outcome_variable) %>% 
  mutate(outcome_variable=active_analyses$outcome_variable %>% str_replace("out_date_", ""))

lifetables <- lifetables %>% left_join(outcome_name_table, by = c("event"="outcome_variable"))
lifetables$event <- NULL

lifetables <- lifetables %>% select(outcome,time_points, pre_vaccination, vaccinated, electively_unvaccinated)
colnames(lifetables) <- c("Outcome", "time_points", "Pre-vaccination cohort", "Vaccinated cohort", "Unvaccinated cohort")


for(i in c("any_position","primary_position")){
  for (j in c("reduced", "normal")){
    
    if(i == "any_position"){
      tmp <- lifetables %>% filter(!Outcome %in% Outcome[grepl("Primary position events",Outcome)]
                                 & time_points == j)
    }else{
      tmp <- lifetables %>% filter(Outcome %in% Outcome[grepl("Primary position events",Outcome)]
                                   & time_points == j)
    }
    
    
    tmp$time_points <- NULL
    #tmp$`Pre-vaccination cohort`<- round(tmp$`Pre-vaccination cohort`, digits = 0)
    #tmp$`Vaccinated cohort`<- round(tmp$`Vaccinated cohort`, digits = 0)
    #tmp$`Unvaccinated cohort`<- round(tmp$`Unvaccinated cohort`, digits = 0)
    write.csv(tmp, paste0(aer_output_dir,"supp_table_3_",i,"_events_",j,"_time_periods.csv"),row.names = F)
    
  }
}


