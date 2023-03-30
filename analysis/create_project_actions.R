library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
library(dplyr)


###########################
# Load information to use #
###########################

## defaults ----
defaults_list <- list(
  version = "3.0",
  expectations= list(population_size=100000L)
)

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active =="TRUE") %>% select(outcome_variable, cohort)
active_analyses$outcome_variable <- gsub("out_date_","",active_analyses$outcome_variable)
cohort_to_run <- c("vaccinated", "electively_unvaccinated")

tmp <- crossing(active_analyses[active_analyses$cohort == "all",]$outcome_variable,cohort_to_run)
colnames(tmp) <- c("outcome_variable","cohort") 
active_analyses <- rbind(tmp, active_analyses)
active_analyses <- active_analyses %>% filter(cohort != "all")

analyses_to_run_stata <- read.csv("lib/analyses_to_run_in_stata.csv")
analyses_to_run_stata <- analyses_to_run_stata[,c("outcome","subgroup","cohort","time_periods")]
analyses_to_run_stata$subgroup <- ifelse(analyses_to_run_stata$subgroup=="hospitalised","covid_pheno_hospitalised",analyses_to_run_stata$subgroup)
analyses_to_run_stata$subgroup <- ifelse(analyses_to_run_stata$subgroup=="non_hospitalised","covid_pheno_non_hospitalised",analyses_to_run_stata$subgroup)

#The normal time period actions have been removed for now as the stata code is only set up to run the 
#reduced time periods
analyses_to_run_stata <- analyses_to_run_stata %>% filter(cohort %in% cohort_to_run
                                                          & time_periods == "reduced")


# Analyses to run in stata - day zero
analyses_to_run_stata_day_zero <- read.csv("lib/analyses_to_run_in_stata_day_zero.csv")
analyses_to_run_stata_day_zero <- analyses_to_run_stata_day_zero[,c("outcome","subgroup","cohort","time_periods")]
analyses_to_run_stata_day_zero$subgroup <- ifelse(analyses_to_run_stata_day_zero$subgroup=="hospitalised","covid_pheno_hospitalised",analyses_to_run_stata_day_zero$subgroup)
analyses_to_run_stata_day_zero$subgroup <- ifelse(analyses_to_run_stata_day_zero$subgroup=="non_hospitalised","covid_pheno_non_hospitalised",analyses_to_run_stata_day_zero$subgroup)
analyses_to_run_stata_day_zero$time_periods <- gsub("day_zero_","",analyses_to_run_stata_day_zero$time_periods)


# create action functions ----

############################
## generic action function #
############################
action <- function(
  name,
  run,
  dummy_data_file=NULL,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL
){
  
  outputs <- list(
    moderately_sensitive = moderately_sensitive,
    highly_sensitive = highly_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL
  
  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    dummy_data_file = dummy_data_file,
    needs = needs,
    outputs = outputs
  )
  action[sapply(action, is.null)] <- NULL
  
  action_list <- list(name = action)
  names(action_list) <- name
  
  action_list
}


## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}


## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


#################################################
## Function for typical actions to analyse data #
#################################################
# Updated to a typical action running Cox models for one outcome
apply_model_function <- function(outcome, cohort){
  splice(
    comment(glue("Cox model for {outcome} - {cohort}")),
    action(
      name = glue("Analysis_cox_{outcome}_{cohort}"),
      run = "r:latest analysis/model/01_cox_pipeline.R",
      arguments = c(outcome,cohort),
      needs = list("stage1_data_cleaning_both", glue("stage1_end_date_table_{cohort}")),
      moderately_sensitive = list(
        analyses_not_run = glue("output/review/model/analyses_not_run_{outcome}_{cohort}.csv"),
        compiled_hrs_csv = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_{cohort}.csv"),
        compiled_hrs_csv_to_release = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_{cohort}_to_release.csv"),
        compiled_event_counts_csv = glue("output/review/model/suppressed_compiled_event_counts_{outcome}_{cohort}.csv"),
        compiled_event_counts_csv_non_supressed = glue("output/review/model/compiled_event_counts_{outcome}_{cohort}.csv"),
        describe_data_surv = glue("output/not-for-review/describe_data_surv_{outcome}_*_{cohort}_*_time_periods.txt")
      ),
      highly_sensitive = list(
        dataset = glue("output/input_{outcome}_*_{cohort}_*_time_periods.csv"),
        sampled_dataset = glue("output/input_sampled_data_{outcome}_*_{cohort}_*_time_periods.csv")
      )
    )
  )
}

# Updated to a typical action running Cox models for one outcome
apply_model_function_covariate_testing <- function(outcome, cohort){
  splice(
    comment(glue("Cox model {outcome} - {cohort}, covariate_testing")),
    action(
      name = glue("Analysis_cox_{outcome}_{cohort}_covariate_testing"),
      run = "r:latest analysis/model/01_cox_pipeline.R",
      arguments = c(outcome,cohort,"test_all"),
      needs = list("stage1_data_cleaning_both", glue("stage1_end_date_table_{cohort}"),glue("stage_2_events_split_by_covariate_level_{cohort}")),
      moderately_sensitive = list(
        analyses_not_run = glue("output/review/model/analyses_not_run_{outcome}_{cohort}_covariate_testing_test_all.csv"),
        compiled_hrs_csv = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_{cohort}_covariate_testing_test_all.csv"),
        compiled_hrs_csv_to_release = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_{cohort}_covariate_testing_test_all_to_release.csv"),
        compiled_event_counts_csv = glue("output/review/model/suppressed_compiled_event_counts_{outcome}_{cohort}_covariate_testing_test_all.csv"),
        compiled_event_counts_csv_non_supressed = glue("output/review/model/compiled_event_counts_{outcome}_{cohort}_covariate_testing_test_all.csv"),
        describe_data_surv = glue("output/not-for-review/describe_data_surv_{outcome}_*_{cohort}_*_covariate_testing_test_all.txt")
      )
    )
  )
}

table2 <- function(cohort){
  splice(
    comment(glue("Stage 4 - Table 2 - {cohort} cohort")),
    action(
      name = glue("stage4_table_2_{cohort}"),
      run = "r:latest analysis/descriptives/table_2.R",
      arguments = c(cohort),
      needs = list("stage1_data_cleaning_both",glue("stage1_end_date_table_{cohort}")),
      moderately_sensitive = list(
        input_table_2 = glue("output/review/descriptives/table2_{cohort}.csv")
      )
    )
  )
}

days_to_event_histogram <- function(cohort){
  splice(
    comment(glue("Post-exposure days to event histogram data - {cohort}")),
    action(
      name = glue("days_to_event_histogram_{cohort}"),
      run = "r:latest analysis/descriptives/histogram_data_post_exposure_days_to_event.R",
      arguments = c(cohort),
      needs = list("stage1_data_cleaning_both",glue("stage1_end_date_table_{cohort}")),
      moderately_sensitive = list(
        input_table_2 = glue("output/review/descriptives/histogram_data_{cohort}.csv")
      )
    )
  )
}

stata_actions <- function(outcome, cohort, subgroup, time_periods){
  splice(
    #comment(glue("Stata cox {outcome} {subgroup} {cohort} {time_periods}")),
    action(
      name = glue("stata_cox_model_{outcome}_{subgroup}_{cohort}_{time_periods}"),
      run = "stata-mp:latest analysis/cox_model.do",
      arguments = c(glue("input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods")),
      needs = list(glue("Analysis_cox_{outcome}_{cohort}")),
      moderately_sensitive = list(
        medianfup = glue("output/input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods_stata_median_fup.csv"),
        stata_output = glue("output/input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods_cox_model.txt")
      )
    )
  )
}

stata_actions_day_zero <- function(outcome, cohort, subgroup, time_periods){
  splice(
    #comment(glue("Stata cox {outcome} {subgroup} {cohort} {time_periods}")),
    action(
      name = glue("stata_cox_day_zero_{outcome}_{subgroup}_{cohort}_{time_periods}"),
      run = "stata-mp:latest analysis/cox_model_day0.do",
      arguments = c(glue("input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods")),
      needs = list(glue("Analysis_cox_{outcome}_{cohort}")),
      moderately_sensitive = list(
        medianfup = glue("output/input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods_stata_median_fup_day_zero.csv"),
        stata_output = glue("output/input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods_cox_model_day_zero.txt")
      )
    )
  )
}


##########################################################
## Define and combine all actions into a list of actions #
##########################################################
actions_list <- splice(

  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create_project_actions.R",
          "Edit and run create_project_actions.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),
  
  #comment("Generate vaccination eligibility information"),
  action(
    name = glue("vax_eligibility_inputs"),
    run = "r:latest analysis/vax_eligibility_inputs.R",
    highly_sensitive = list(
      vax_study_dates_json = glue("output/vax_study_dates.json"),
      vax_jcvi_groups= glue("output/vax_jcvi_groups.csv"),
      vax_eligible_dates= ("output/vax_eligible_dates.csv")
    )
  ),

  #comment("Generate dummy data for study_definition - electively_unvaccinated"),
  action(
    name = "generate_study_population_electively_unvaccinated",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_electively_unvaccinated --output-format feather",
    needs = list("vax_eligibility_inputs"),
    highly_sensitive = list(
      cohort = glue("output/input_electively_unvaccinated.feather")
    )
  ),
  
  #comment("Generate dummy data for study_definition - vaccinated"),
  action(
    name = "generate_study_population_vaccinated",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_vaccinated --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input_vaccinated.feather")
    )
  ), 
  
  #comment("Generate dummy data for study_definition - index"),
  action(
    name = "generate_study_population_index",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_index --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input_index.feather")
    )
  ), 

  #comment("Preprocess data - vaccinated"),
  action(
    name = "preprocess_data_vaccinated",
    run = "r:latest analysis/preprocess/preprocess_data.R vaccinated",
    needs = list("generate_study_population_index", "generate_study_population_vaccinated", "generate_study_population_electively_unvaccinated"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_vaccinated_*.txt"),
      describe_index = glue("output/not-for-review/describe_tmp_index_vaccinated.txt"),
      describe_cohort = glue("output/not-for-review/describe_tmp_vaccinated.txt"),
      descrive_venn = glue("output/not-for-review/describe_venn_vaccinated.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_vaccinated.rds"),
      venn = glue("output/venn_vaccinated.rds")
    )
  ), 

  #comment("Preprocess data - electively_unvaccinated"),
  action(
    name = "preprocess_data_electively_unvaccinated",
    run = "r:latest analysis/preprocess/preprocess_data.R electively_unvaccinated",
    needs = list("generate_study_population_index", "generate_study_population_vaccinated", "generate_study_population_electively_unvaccinated"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_electively_unvaccinated_*.txt"),
      describe_index = glue("output/not-for-review/describe_tmp_index_electively_unvaccinated.txt"),
      describe_cohort = glue("output/not-for-review/describe_tmp_electively_unvaccinated.txt"),
      descrive_venn = glue("output/not-for-review/describe_venn_electively_unvaccinated.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_electively_unvaccinated.rds"),
      venn = glue("output/venn_electively_unvaccinated.rds")
    )
  ), 

  #comment("Stage 1 - Data cleaning"),
  action(
    name = "stage1_data_cleaning_both",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R both",
    needs = list("preprocess_data_vaccinated","preprocess_data_electively_unvaccinated"),
    moderately_sensitive = list(
      refactoring = glue("output/not-for-review/meta_data_factors_*.csv"),
      QA_rules = glue("output/review/descriptives/QA_summary_*.csv"),
      IE_criteria = glue("output/review/descriptives/Cohort_flow_*.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_*.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_*_stage1.rds")
    )
  ),
  
  #comment("Stage 1 - End date table"),
  action(
    name = "stage1_end_date_table_vaccinated",
    run = "r:latest analysis/preprocess/create_follow_up_end_date.R vaccinated",
    needs = list("preprocess_data_vaccinated","preprocess_data_electively_unvaccinated","stage1_data_cleaning_both"),
    highly_sensitive = list(
      end_date_table = glue("output/follow_up_end_dates_vaccinated.rds")
    )
  ),
  
  #comment("Stage 1 - End date table"),
  action(
    name = "stage1_end_date_table_electively_unvaccinated",
    run = "r:latest analysis/preprocess/create_follow_up_end_date.R electively_unvaccinated",
    needs = list("preprocess_data_vaccinated","preprocess_data_electively_unvaccinated","stage1_data_cleaning_both"),
    highly_sensitive = list(
      end_date_table = glue("output/follow_up_end_dates_electively_unvaccinated.rds")
    )
  ),
  
  #comment("Stage 2 - Missing - Table 1"),
  action(
    name = "stage2_missing_table1_both",
    run = "r:latest analysis/descriptives/Stage2_missing_table1.R both",
    needs = list("stage1_data_cleaning_both"),
    moderately_sensitive = list(
      Missing_RangeChecks = glue("output/not-for-review/Check_missing_range_*.csv"),
      DateChecks = glue("output/not-for-review/Check_dates_range_*.csv"),
      Descriptive_Table = glue("output/review/descriptives/Table1_*.csv")
    )
  ),
  
  #comment("Stage 4 - Create table2"),
  splice(
    # over cohort
    unlist(lapply(cohort_to_run, function(x) table2(cohort = x)), recursive = FALSE)
  ),
  
  #comment("create post exposure time to event histogram data"),
  splice(
    # over cohort
    unlist(lapply(cohort_to_run, function(x) days_to_event_histogram(cohort = x)), recursive = FALSE)
  ),
  
  #comment("Stage 4 - Venn diagrams"),
  action(
    name = "stage4_venn_diagram_both",
    run = "r:latest analysis/descriptives/venn_diagram.R both",
    needs = list("preprocess_data_vaccinated","preprocess_data_electively_unvaccinated","stage1_data_cleaning_both","stage1_end_date_table_vaccinated","stage1_end_date_table_electively_unvaccinated"),
    moderately_sensitive = list(
      venn_diagram = glue("output/review/venn-diagrams/venn_diagram_*"))
  ),
  
  #comment("Stage 5 - Apply models"),
  splice(unlist(lapply(1:nrow(active_analyses), 
                       function(i) apply_model_function(outcome = active_analyses[i, "outcome_variable"],
                                                 cohort = active_analyses[i, "cohort"])),
                recursive = FALSE)),
  
  
  #Stata reduced time periods analyses
  splice(unlist(lapply(1:nrow(analyses_to_run_stata), 
                       function(i) stata_actions(outcome = analyses_to_run_stata[i, "outcome"],
                                                 subgroup = analyses_to_run_stata[i, "subgroup"],
                                                 cohort = analyses_to_run_stata[i, "cohort"],
                                                 time_periods = analyses_to_run_stata[i, "time_periods"])),
                recursive = FALSE)),
  
  #Stata day zero analyses
  splice(unlist(lapply(1:nrow(analyses_to_run_stata_day_zero), 
                       function(i) stata_actions_day_zero(outcome = analyses_to_run_stata_day_zero[i, "outcome"],
                                                 subgroup = analyses_to_run_stata_day_zero[i, "subgroup"],
                                                 cohort = analyses_to_run_stata_day_zero[i, "cohort"],
                                                 time_periods = analyses_to_run_stata_day_zero[i, "time_periods"])),
                recursive = FALSE)),
  
  
  #comment("Format Stata output")
  action(
    name = "format_stata_output",
    run = "r:latest analysis/format_stata_output.R",
    needs = c(paste0("stata_cox_model_",analyses_to_run_stata$outcome,"_",analyses_to_run_stata$subgroup,"_",analyses_to_run_stata$cohort,"_",analyses_to_run_stata$time_periods),
              paste0("stata_cox_day_zero_",analyses_to_run_stata_day_zero$outcome,"_",analyses_to_run_stata_day_zero$subgroup,"_",analyses_to_run_stata_day_zero$cohort,"_",analyses_to_run_stata_day_zero$time_periods)),
    moderately_sensitive = list(
      stata_output = "output/stata_output.csv")
  ),
  
  action(
    name = "format_R_output",
    run = "r:latest analysis/model/07_combine_HRs_to_one_file.R",
    needs = c(paste0("Analysis_cox_",active_analyses$outcome_variable,"_",active_analyses$cohort)),
    moderately_sensitive = list(
      R_output = "output/review/model/R_HR_output.csv",
      R_event_counts = "output/review/model/R_event_count_output.csv",
      R_event_counts_day_zero = "output/review/model/R_event_count_day_zero_output.csv")
  ),
  
  action(
    name = "check_table2_matches_cox_event_counts",
    run = "r:latest analysis/descriptives/check_table2_matches_cox_event_counts.R",
    needs = c(paste0("Analysis_cox_",active_analyses$outcome_variable,"_",active_analyses$cohort),
                 "stage4_table_2_vaccinated", "stage4_table_2_electively_unvaccinated"),
    moderately_sensitive = list(
      check_table2_cox_event_counts = "output/not-for-review/descriptives/table2_cox_model_event_counts_comparison.csv"
    )
  )
  
)


## combine everything ----
project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)
  
#####################################################################################
## convert list to yaml, reformat comments and white space, and output a .yaml file #
#####################################################################################
x=as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")

