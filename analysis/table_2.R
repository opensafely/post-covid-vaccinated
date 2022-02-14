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
  # population <- "vaccinated"
  population = "electively_unvaccinated"
} else {
  population <- args[[1]]
}

cohort_start = as.Date("2021-06-01", format="%Y-%m-%d")
cohort_end = as.Date("2021-12-14", format="%Y-%m-%d")

# read in data------------------------------------------------------------

if(population == "vaccinated"){
  input <- read_rds("output/input_vaccinated_stage1.rds")
}

if(population == "electively_unvaccinated"){
  input <- read_rds("output/input_electively_unvaccinated_stage1.rds")
}

# record variable names for covariate
vars_names <- tidyselect::vars_select(names(input), !starts_with(c('sub_','cov_','qa_','vax_cat'), ignore.case = TRUE))

# Create a data frame for survival data: to avoid carrying covariates in the calculation
survival_data <- input[,vars_names] 

# cohort start date and end date
survival_data <- survival_data %>% 
  mutate(cohort_start_date = cohort_start,
         cohort_end_date = cohort_end)

# automation
event_dates_names <- tidyselect::vars_select(names(input), starts_with('out_date', ignore.case = TRUE))
event_dates_names <- tidyselect::vars_select(event_dates_names, !contains('diabetes'))
event_dates_names

event_names<- substr(event_date_names, start=10, stop=nchar(event_date_names))
event_names

col_headings <- c("event", "event_count", "pearson_years_follow_up", "incidence_rate")
table_2 <- data.frame(matrix(ncol=length(col_headings), nrow=length(event_date_names)))
colnames(table_2) <- col_headings
table_2$event <- event_names
table_2

# comment: patient_id = 6672, 2100-12-31 - this indicates missing data
summary_stats <- function(population, survival_data, event_date_names, index)
{
  survival_data$event_date <- survival_data[,event_date_names[index]]
  if(population=="vaccinated"){
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=min(event_date, death_date, cohort_end_date,na.rm = TRUE))
  }else if(population=="electively_unvaccinated"){
    survival_data <- survival_data %>% left_join(input%>%dplyr::select(patient_id,vax_date_covid_1))
    survival_data <- survival_data %>% rowwise() %>% mutate(follow_up_end=  min(vax_date_covid_1,event_date, death_date,cohort_end_date,na.rm = TRUE))
    survival_data <- survival_data %>% dplyr::select(!c(vax_date_covid_1))
  }
  # follow-up days
  survival_data = survival_data %>% mutate(follow_up_period = as.numeric((as.Date(follow_up_end) - as.Date(index_date))))
  #hist(survival_data$follow_up_period)
  survival_data = survival_data %>% filter(follow_up_period >0 & follow_up_period < 197) # filter out follow up period 
  survival_data = survival_data %>% mutate(follow_up_years = follow_up_period / 365.2) # follow-up years
  event_count <- length(which(survival_data$out_date_ami >= survival_data$index_date  & survival_data$out_date_ami <= survival_data$follow_up_end))
  pearson_years_follow_up  = round(sum(survival_data$follow_up_years, na.rm = TRUE),1)
  incidence_rate = round(event_count/pearson_years_follow_up, 4)
  return(c(event_count, pearson_years_follow_up, incidence_rate))
}

for(i in 1:length(event_date_names)){
  table_2[i,2:4] <- summary_stats(population, survival_data, event_date_names, i)
}

table_2

str(table_2)

write.csv(table_2, file= paste0("output/", "table2_", population, ".csv"), row.names = F)
