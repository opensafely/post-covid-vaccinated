## =============================================================================
## Purpose:  Create incidence plots
## 
## Author:   Kurt Taylor
##
## Reviewer: 
##
## Date:     15th June 2022
##
## Data:     Post covid events study population
##
## Content: Create a plot of the incidence and cumulative incidence of COVID by week from the beginning of follow up to the end of follow-up for each cohort.
## Output:  
## =============================================================================

library(outbreaks)
library(ggplot2)
library(incidence)
library(readr)
library(dplyr)
library(cowplot)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort_name <- "vaccinated"
} else {
  cohort_name <- args[[1]]
}

group <- "diabetes"

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "review", "venn-diagrams"))

incidence_output <- function(cohort_name, group) {
  
  # Read input dataset 
  input_stage1 <- readr::read_rds(paste0("output/input_", cohort_name,"_stage1_", group,".rds"))
  
  end_date <- read_rds(paste0("output/follow_up_end_dates_electively_unvaccinated_diabetes.rds"))
    
  ##------------------------------
  # GENERATE DATA FOR WEEKLY INCIDENCE PLOTS --------------------------------------------------------------------
  ##------------------------------
  
  # 1. Confirmed COVID cases
  
  # Use date of confirmed COVID
  covid_cases <- input_stage1$exp_date_covid19_confirmed
  
  # Use incidence package generate an incidence object with weekly incidence 
  incidence_object <- incidence(covid_cases, interval = "1 week: saturday")
  # by sex
  incidence_object_sex <- incidence(covid_cases, interval = "1 week: saturday", group =  input_stage1$cov_cat_sex)
  # by age
  # Define age groups (taken from table 1 code)
  input_stage1$cov_cat_age_group <- ""
  input_stage1$cov_cat_age_group <- ifelse(input_stage1$cov_num_age>=18 & input_stage1$cov_num_age<=29, "18-29", input_stage1$cov_cat_age_group)
  input_stage1$cov_cat_age_group <- ifelse(input_stage1$cov_num_age>=30 & input_stage1$cov_num_age<=39, "30-39", input_stage1$cov_cat_age_group)
  input_stage1$cov_cat_age_group <- ifelse(input_stage1$cov_num_age>=40 & input_stage1$cov_num_age<=49, "40-49", input_stage1$cov_cat_age_group)
  input_stage1$cov_cat_age_group <- ifelse(input_stage1$cov_num_age>=50 & input_stage1$cov_num_age<=59, "50-59", input_stage1$cov_cat_age_group)
  input_stage1$cov_cat_age_group <- ifelse(input_stage1$cov_num_age>=60 & input_stage1$cov_num_age<=69, "60-69", input_stage1$cov_cat_age_group)
  input_stage1$cov_cat_age_group <- ifelse(input_stage1$cov_num_age>=70 & input_stage1$cov_num_age<=79, "70-79", input_stage1$cov_cat_age_group)
  input_stage1$cov_cat_age_group <- ifelse(input_stage1$cov_num_age>=80 & input_stage1$cov_num_age<=89, "80-89", input_stage1$cov_cat_age_group)
  input_stage1$cov_cat_age_group <- ifelse(input_stage1$cov_num_age>=90, "90+", input_stage1$cov_cat_age_group)
  
  incidence_object_age <- incidence(covid_cases, interval = "1 week: saturday", group =  input_stage1$cov_cat_sex)
  
  # convert incidence object to a dataframe so that it can be exported from OpenSAFELY
  incidence_object_df <- as.data.frame(incidence_object)
  incidence_object_age_df <- as.data.frame(incidence_object_sex)
  incidence_object_sex_df <- as.data.frame(incidence_object_age)

  ##------------------------------
  # GENERATE DATA FOR WEEKLY INCIDENCE PLOTS --------------------------------------------------------------------
  ##------------------------------
  
  # Make COVID event variable yes no
  
  input_stage1 <- input_stage1 %>% 
    mutate(covid_event = ifelse(is.na(exp_date_covid19_confirmed), 0, 1))
  
  # Prepare dataframes
  
  df <- input_stage1 %>%
    dplyr::select(exp_date_covid19_confirmed, covid_event, cov_cat_sex, cov_cat_age_group) %>%
    dplyr::rename(Sex = cov_cat_sex,
                  Age_Group = cov_cat_age_group) %>%
    mutate(All = "All")
  
  # ALL
  
  df_all <- df %>%
    dplyr::select(exp_date_covid19_confirmed, covid_event, All) %>%
    #removing NAs
    .[complete.cases(df),] %>%
    # Arrange by data
    arrange(exp_date_covid19_confirmed) %>%
    #wide format df with the count of each groups events at each time 
    #(some dates have more than on event)(NA of dates mismatch, replace by 0)
    pivot_wider(names_from = All,names_glue = "{All}", values_from = covid_event, values_fn = length, values_fill = 0) %>% 
    #changing groups event per date to cumsum
    mutate_at(-1,cumsum) %>%
    # long format 
    pivot_longer(cols = -1, names_to = "All", values_to = "Cumsum")
  
  # BY SEX
  
  df_sex <- df %>%
    dplyr::select(exp_date_covid19_confirmed, covid_event, Sex) %>%
    .[complete.cases(df),] %>%
    arrange(exp_date_covid19_confirmed) %>%
    pivot_wider(names_from = Sex,names_glue = "{Sex}", values_from = covid_event, values_fn = length, values_fill = 0) %>% 
    mutate_at(-1,cumsum) %>%
    pivot_longer(cols = -1, names_to = "Sex", values_to = "Cumsum")
  
  # BY AGE 
  
  df_age <- df %>%
    dplyr::select(exp_date_covid19_confirmed, covid_event, Age_Group) %>%
    .[complete.cases(df),] %>%
    arrange(exp_date_covid19_confirmed) %>%
    pivot_wider(names_from = Age_Group,names_glue = "{Age_Group}", values_from = covid_event, values_fn = length, values_fill = 0) %>% 
    mutate_at(-1,cumsum) %>%
    pivot_longer(cols = -1, names_to = "Age_Group", values_to = "Cumsum")
  
  ##------------------------------
  # SAVE OUTPUTS  --------------------------------------------------------------------
  ##------------------------------
  
  
  
}



##------------------------------
# PLOT WEEKLY INCIDENCE ---------------------------------------------------
##------------------------------

# ALL 


# i
# plot(i, color = "darkred")

i_df <- as.data.frame(i)
incidence::plot(i_df)

# SEX

i.sex <- incidence(onset, interval = "1 week: saturday", group =  input$cov_cat_age_group)
i.sex
incidence_plot <- plot(i.sex)

##------------------------------
# CUMULATIVE INCIDENCE ----------------------------------------------------
##------------------------------

# Make COVID event variable yes no

input_stage1 <- input_stage1 %>% 
  mutate(covid_event = ifelse(is.na(exp_date_covid19_confirmed), 0, 1))

# Prepare dataframe 

df <- input_stage1 %>%
  dplyr::select(exp_date_covid19_confirmed, covid_event, cov_cat_sex) %>%
  dplyr::rename(Sex = cov_cat_sex)

df$All <- "all"
df$Sex <- NULL

df <- df %>%
  #removing NAs
  .[complete.cases(df),] %>%
  # Arrange by data
  arrange(exp_date_covid19_confirmed) %>%
  #wide format df with the count of each groups events at each time 
  #(some dates have more than on event)(NA of dates mismatch, replace by 0)
  pivot_wider(names_from = All,names_glue = "{All}", values_from = covid_event, values_fn = length, values_fill = 0) %>% 
  #changing groups event per date to cumsum
  mutate_at(-1,cumsum) %>%
  # long format 
  pivot_longer(cols = -1, names_to = "All", values_to = "Cumsum")

df_no_strat <- df %>%
  dplyr::select(-Sex) %>%
  mutate(Cumsum = )
  
cumulative_plot <- df %>% ggplot() + 
geom_line(aes(x = exp_date_covid19_confirmed, y = Cumsum, linetype = Sex)) + 
labs(y = "Cumulative cases of COVID-19", x = "Months")

# GENERATE FINAL PLOT -----------------------------------------------------

png("output/review/incidence_plots.png",
    width = 15, height = 10, units = "in", res = 400)
plot_grid(incidence_plot, cumulative_plot, ncol=2, nrow = 1,
          labels = c("A: Weekly incidence of COVID-19 cases", "B: Cumulative incidence of COVID-19 cases"),
          label_x = -0.2,
          hjust = -0.5, scale = 0.93)
dev.off()
