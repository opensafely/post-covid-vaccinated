# Check that table 2 event counts match cox model event counts
library(purrr)
library(data.table)
library(dplyr)

fs::dir_create(here::here("output", "review", "model"))
fs::dir_create(here::here("output", "not-for-review", "descriptives"))

output_dir <- "output/review/model"

# Read in table 2
table2_electively_unvaccinated <- read.csv("output/review/descriptives/table2_electively_unvaccinated.csv")
table2_vaccinated <- read.csv("output/review/descriptives/table2_electively_unvaccinated.csv")
table2 <- rbind(table2_electively_unvaccinated,table2_vaccinated)

table2 <- table2 %>% select(event,subgroup,cohort_to_run,unexposed_person_days,total_person_days,post_exposure_event_count)
table2$post_exposure_peson_days <- table2$total_person_days - table2$unexposed_person_days
table2[c("total_person_days","unexposed_person_days")] <- NULL
table2$event <- gsub("out_date_","",table2$event)
rm(table2_electively_unvaccinated,table2_vaccinated)

# Read in event count files

event_counts <- list.files(path = output_dir, pattern = "compiled_event_counts")
event_counts <- event_counts[!grepl("suppressed",event_counts)]
event_counts=event_counts[endsWith(event_counts,".csv")]
event_counts=paste0(output_dir,"/", event_counts)
event_counts_file_paths <- pmap(list(event_counts),
                                function(fpath){
                                  df <- fread(fpath)
                                  return(df)
                                })
event_counts_df <- rbindlist(event_counts_file_paths, fill=TRUE)
event_counts_df <- as.data.frame(event_counts_df)

event_counts_df <- event_counts_df %>% filter(expo_week=="all post expo" &
                                                time_points == "reduced") %>%
                                      select(events_total, event, subgroup,cohort,
                                              person_days_follow_up)

event_counts_df <- event_counts_df %>% rename(cox_events_total = events_total,
                                              cox_person_days_follow_up = person_days_follow_up)

# Left join cox event counts onto table 2
table2 <- table2 %>% left_join(event_counts_df, by = c("event" = "event", "subgroup"="subgroup",
                                                       "cohort_to_run" = "cohort"))

table2 <- table2 %>% mutate(across(c(post_exposure_event_count,post_exposure_peson_days,cox_events_total,cox_person_days_follow_up),as.numeric))

#Check if counts match
table2$event_counts_match <- table2$post_exposure_event_count == table2$cox_events_total
table2$person_days_match <- table2$post_exposure_peson_days == table2$cox_person_days_follow_up

write.csv(table2, "output/not-for-review/descriptives/table2_cox_model_event_counts_comparison.csv")
