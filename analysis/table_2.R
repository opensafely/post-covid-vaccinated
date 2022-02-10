## =============================================================================
## Purpose:  Create Table 2
## 
## Author:   Yinghui Wei
## 
## Date:     13 January 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  Number of outcome events;
##           person years of follow up and rates of events, for each outcome
## =============================================================================

library(readr); library(dplyr); library(data.table); library(lubridate)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  population <- "vaccinated"
  # population = "electively_unvaccinated"
} else {
  population <- args[[1]]
}

# read in data------------------------------------------------------------

if(population == "vaccinated"){
  input <- read_rds("output/input_vaccinated_stage1.rds")
}

if(population == "electively_unvaccinated"){
  input <- read_rds("output/input_electively_unvaccinated_stage1.rds")
}

event_date_names <- c("out_date_ami",  "out_date_stroke_isch", 
                   "out_date_pe",   "out_date_dvt",
                   "out_date_tia",  "out_date_stroke_sah_hs", 
                   "out_date_hf",   "out_date_angina",
                   "out_date_ate",  "out_date_vte")

# automation
event_names<- substr(event_date_names, start=10, stop=nchar(event_date_names))
event_names

col_headings <- c("event", "event_count", "pearson_years_follow_up", "incidence_rate")
table_2 <- data.frame(matrix(ncol=length(col_headings), nrow=length(event_date_names)))
colnames(table_2) <- col_headings
table_2$event <- event_names
table_2

n_events <- rep(0,length(event_date_names))

number_events <- function(outcome)
{
  count <- length(which(!is.na(outcome)))
  return(count)
}

n_events <- c(lapply(input[,event_date_names], number_events))

n_events

table_2$event_count <- as.numeric(n_events)

# record variable names for covariate, qa which are not used in calculating incidence rate
vars_names <- tidyselect::vars_select(names(input), !starts_with(c('sub_','cov_','qa_','vax_cat'), ignore.case = TRUE))

# Create a data frame for survival data: to avoid carrying covariates in the calculation
survival_data <- input[,vars_names] 

# cohort start date and end date
survival_data <- survival_data %>% 
                     mutate(cohort_start_date = as.Date("2021-06-01", format="%Y-%m-%d"),
                            cohort_end_date = as.Date("2021-12-14", format = "%Y-%m-%d"))

#View(survival_data)

#is.Date(survival_data$cohort_end_date)

#survival_data$event_date = survival_data$out_date_ami
# comment: patient_id = 6672, 2100-12-31
summary_stats <- function(population, survival_data, event_count, event_date_names, index)
{
  # take ami as an example, 17 Jan 2022
  survival_data$event_date <- survival_data[,event_date_names[index]]

  # if(population == "vaccinated"){
  #   #14 days after the second vaccination
  #   survival_data = survival_data %>% mutate(post_2vaccines_14days = as.Date(vax_date_covid_2)+14)
  #   survival_data = survival_data %>% rowwise() %>% mutate(follow_up_start= max(cohort_start_date,post_2vaccines_14days,na.rm = TRUE),
  #                                                          follow_up_end= min(event_date, death_date, cohort_end_date,na.rm = TRUE))
  #   }else if(population=="electively_unvaccinated"){
  #   survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
  #   survival_data = survival_data %>% mutate(post_vax_eligible_12wks = as.Date(vax_date_eligible)+84)
  #   survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_start = max(cohort_start_date,post_vax_eligible_12wks,na.rm = TRUE),
  #                                follow_up_end = min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
  #   survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
  # }
  if(population=="vaccinated"){
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(event_date, death_date, cohort_end_date,na.rm = TRUE))
  }else if(project=="electively_unvaccinated"){
    survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=  min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
    survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
  }
  # follow-up years
  survival_data = survival_data %>% mutate(follow_up_period = as.numeric((as.Date(follow_up_end) - as.Date(index_date))/365.2))
 # print(c(survival_data$follow_up_end, survival_data$follow_up_start))
   #number of person years follow up
  # pearson_years_follow_up  = round(sum(survival_data$follow_up_period, na.rm = TRUE),1)
  print(survival_data$follow_up_period)
  # incidence rate for ami
  incidence_rate = round(as.numeric(event_count[index])/pearson_years_follow_up, 4)
  
  return(c(pearson_years_follow_up, incidence_rate))
}

for(i in 1:length(event_date_names)){
  table_2[i,3:4] <- summary_stats(population, survival_data, table_2$event_count, event_date_names, i)
}

table_2

str(table_2)

write.csv(table_2, file="output/table2_summary_stats.csv", row.names=F)

# 
# # #----------------------------------------------------------------------------------------
# # writing code for a single outcome to start with: take ami as an example, 17 Jan 2022
# if(population == "vaccinated_delta"){
#   #14 days after the second vaccination
#   survival_data = survival_data %>% mutate(post_2vaccines_14days = as.Date(vax_date_covid_2)+14)
#   survival_data = survival_data %>% rowwise() %>% mutate(follow_up_start= min(max(cohort_start_date,post_2vaccines_14days,na.rm = TRUE), cohort_end_date, na.rm=TRUE),
#                                                          follow_up_end= min(out_date_ami, death_date, cohort_end_date,na.rm = TRUE))
# }else if(population=="electively_unvaccinated_delta"){
#   survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
#   survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(vax_date_covid_1,out_date_ami, death_date,cohort_end_date,na.rm = TRUE))
#   survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
# }
# 
# # follow-up years
# # some of the index_date were wrong? January 2022 after the end of the cohort?
# survival_data = survival_data %>% mutate(follow_up_period = as.numeric((as.Date(follow_up_end) - as.Date(follow_up_start))/365.2))
# 
# # checking ---------
# data<-survival_data %>% dplyr::select(c("out_date_ami", "death_date", "cohort_end_date","index_date" ,"follow_up_start", "follow_up_end", "follow_up_period"))
# View(data)
# View(data[which(data$index_date>"2021-12-04"),])
# names(survival_data)
# 
# #number of person years follow up
# table_2$pearson_years_follow_up[1]  = round(sum(survival_data$follow_up_period, na.rm = TRUE),1)
# table_2$pearson_years_follow_up[1]
# 
# 
# # incidence rate for ami
# table_2$incidence_rate[1] = round(as.numeric(table_2$event_counts[1])/table_2$pearson_years_follow_up[1], 4)
# 
# table_2


# what is "date_expo_censor"?
# else if (project == "unvaccinated"){
#   survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
#   survival_data$follow_up_start=cohort_start_date
#   survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(out_date_ami, vax_date_covid_1, death_date,cohort_end_date,date_expo_censor,na.rm = TRUE))
#   survival_data <- survival_data %>% dplyr::select(!vax_date_covid_1)
# }



# # read in event dates for outcome-of-interest
# event_names <- c("ami",  "stroke_isch", 
#                  "pe",   "dvt",
#                  "tia",  "stroke_sah_hs", 
#                  "hf",   "angina",
#                  "ate",  "vte"
# )
# # create a new data frame with columns: patient_id, out_date for each outcome
# event <- event_names[1]
# outcomes <-input%>%dplyr::select(c("patient_id", 
#                                    paste0("out_date_", event)))
# 
# # wrangle columns for naming convention 
# setnames(outcomes, 
#          old = c(paste0("out_date_", event)), 
#          new = c("event_date"))
# 
# outcomes$name <- event
# 
# head(outcomes)

# # outcome 1: ami
# n_events[1] <- number_events(input$out_date_ami)
# 
# # outcome 2: stroke
# n_events[2] <- number_events(input$out_date_stroke_isch)
# 
# # outcome 3: pe
# n_events[3] <- number_events(input$out_date_pe)
# 
# # outcome 4: dvt
# n_events[4] <- number_events(input$out_date_dvt)
# 
# # outcome 5: tia
# n_events[5] <- number_events(input$out_date_tia)
# 
# # outcome 6: stroke_sah_hs
# n_events[6] <- number_events(input$out_date_stroke_sah_hs)
# 
# # outcome 7: hf
# n_events[7] <- number_events(input$out_date_hf)
# 
# # outcome 8: angina
# n_events[8] <- number_events(input$out_date_angina)
# 
# # outcome 9: ate (Other Arterial Embolism)
# n_events[9] <- number_events(input$out_date_ate)
# 
# # outcome 10: vte
# n_events[10] <- number_events(input$out_date_vte)
# 
# 


