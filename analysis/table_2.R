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

# If working on local PC with dummy data, uncomment the following two lines
population = "vaccinated_delta" #commented out when using project yaml
#population = "electively_unvaccinated_delta" #commented out when using project yaml

# read in data------------------------------------------------------------

if(population == "vaccinated_delta"){
  input <- read_rds("output/input_vaccinated.rds")
}

if(population == "electively_unvaccinated"){
  input <- read_rds("output/input_electively_unvaccinated.rds")
}

n_events <- rep(0,10)

number_events <- function(outcome)
{
  count <- length(which(!is.na(outcome)))
  return(count)
}

outcome_names <- c("out_date_ami",  "out_date_stroke_isch", 
                   "out_date_pe",   "out_date_dvt",
                   "out_date_tia",  "out_date_stroke_sah_hs", 
                   "out_date_hf",   "out_date_angina",
                   "out_date_ate",  "out_date_vte"
                   )

n_events <- c(lapply(input[,outcome_names], number_events))

n_events



# record variable names for covariate, qa which are not used in calculating incidence rate
variable_names <- tidyselect::vars_select(names(input), !starts_with(c('sub_','cov_','qa_','vax_cat'), ignore.case = TRUE))


# Create a data frame for survival data 
survival_data <- input[,variable_names] #View(covars)


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

# follow-up start date: input$index_date
population= "vaccinated_delta"
#project = "electively_unvaccinated_delta"
#project = "unvaccinated"

# take ami as an example, 17 Jan 2022
# can't use index_date as the start date of follow up as some index dates were after the end of the cohort



survival_data <- survival_data %>% mutate(delta_start_date = as.Date("2021-06-01", format="%Y-%m-%d"),
                                          cohort_end_date = as.Date("2021-12-04", format = "%Y-%m-%d"))
View(survival_data)

is.Date(survival_data$cohort_end_date)
if(population == "vaccinated_delta"){
  #14 days after the second vaccination
  survival_data = survival_data %>% mutate(post_2vaccines_14days = as.Date(vax_date_covid_2)+14)
  survival_data = survival_data %>% rowwise() %>% mutate(follow_up_start= min(max(delta_start_date,post_2vaccines_14days,na.rm = TRUE), cohort_end_date, na.rm=TRUE),
                                                         follow_up_end= min(out_date_ami, death_date, cohort_end_date,na.rm = TRUE))
}else if(population=="electively_unvaccinated_delta"){
  survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
  survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(vax_date_covid_1,out_date_ami, death_date,cohort_end_date,na.rm = TRUE))
  survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
}


# follow-up years
# some of the index_date were wrong? January 2022 after the end of the cohort?
survival_data = survival_data %>% mutate(follow_up_period = (as.Date(follow_up_end) - as.Date(follow_up_start))/365.2)

data<-survival_data %>% dplyr::select(c("out_date_ami", "death_date", "cohort_end_date","index_date" ,"follow_up_start", "follow_up_end", "follow_up_period"))
View(data)
names(survival_data)

index_date <- survival_data$index_date
follow_up_end <- survival_data$follow_up_end

follow_up_period <- survival_data$follow_up_period
data <- data.frame(index_date, follow_up_end, follow_up_period)
View(data)

head(survival_data$follow_up_period)

number_person_years_follow_up  = sum(survival_data$follow_up_period, na.rm = TRUE)
number_person_years_follow_up

# incidence rate for ami
as.numeric(n_events[1])/as.numeric(number_person_years_follow_up)

# what is "date_expo_censor"?
# else if (project == "unvaccinated"){
#   survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
#   survival_data$follow_up_start=cohort_start_date
#   survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(out_date_ami, vax_date_covid_1, death_date,cohort_end_date,date_expo_censor,na.rm = TRUE))
#   survival_data <- survival_data %>% dplyr::select(!vax_date_covid_1)
# }




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


