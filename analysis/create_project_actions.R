library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
#library(dplyr)



###########################
# Load information to use #
###########################

## defaults ----
defaults_list <- list(
  version = "3.0",
  expectations= list(population_size=400000L)
)

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses_table <- subset(active_analyses, active_analyses$active =="TRUE")
outcomes_model <- active_analyses_table$outcome_variable %>% str_replace("out_date_", "")


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
apply_model_function <- function(outcome){
  splice(
    comment(glue("Apply cox model for {outcome}")),
    action(
      name = glue("Analysis_cox_{outcome}"),
      run = "r:latest analysis/01_pipe.R",
      arguments = c(outcome),
      needs = list("stage1_data_cleaning_both"),
      moderately_sensitive = list(
        compiled_hrs_html = glue("output/suppressed_compiled_HR_results_{outcome}.html"),
        compiled_event_counts_html = glue("output/suppressed_compiled_event_counts_{outcome}.html"),
        analyses_not_run = glue("output/analyses_not_run_{outcome}.csv"),
        compiled_hrs_csv = glue("output/suppressed_compiled_HR_results_{outcome}.csv"),
        compiled_event_counts_csv = glue("output/suppressed_compiled_event_counts_{outcome}.csv")
      ),
      highly_sensitive = list(
        compiled_hrs = glue("output/compiled_HR_results_{outcome}.csv"),
        compiled_event_counts = glue("output/compiled_event_counts_{outcome}.csv")
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
  
  #comment("Generate dummy data for study_definition - unvaccinated"),
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

  #comment("Preprocess data"),
  action(
    name = "preprocess_data",
    run = "r:latest analysis/preprocess_data.R",
    needs = list("generate_study_population_index", "generate_study_population_vaccinated", "generate_study_population_electively_unvaccinated"),
    moderately_sensitive = list(
      describe = glue("output/describe_*.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_*.rds"),
      venn = glue("output/venn_*.rds")
    )
  ), 

  #comment("Stage 1 - Data cleaning"),
  action(
    name = "stage1_data_cleaning_both",
    run = "r:latest analysis/Stage1_data_cleaning.R both",
    needs = list("preprocess_data"),
    moderately_sensitive = list(
      refactoring = glue("output/meta_data_factors_*.csv"),
      QA_rules = glue("output/QA_summary_*.csv"),
      IE_criteria = glue("output/Cohort_flow_*.csv")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_*_stage1.rds")
    )
  ),

  #comment("Stage 2 - Missing - Table 1"),
  action(
    name = "stage2_missing_table1_both",
    run = "r:latest analysis/Stage2_missing_table1.R both",
    needs = list("stage1_data_cleaning_both"),
    moderately_sensitive = list(
      Missing_RangeChecks = glue("output/Check_missing_range_*.csv"),
      DateChecks = glue("output/Check_dates_range_*.csv"),
      Descriptive_Table = glue("output/Table1_*.csv")
    )
  ),

  #comment("Stage 3 - No action there for CVD outcomes"),  

  
  #comment("Stage 4 - Table 2"),
  action(
    name = "stage4_table2_both",
    run = "r:latest analysis/table_2.R both",
    needs = list("stage1_data_cleaning_both"),
    moderately_sensitive = list(
      table2 = glue("output/table2_*.csv")
    )
  ),
  
  #comment("Stage 4 - Venn diagrams"),
  action(
    name = "stage4_venn_diagram_both",
    run = "r:latest analysis/venn_diagram.R both",
    needs = list("preprocess_data","stage1_data_cleaning_both"),
    moderately_sensitive = list(
      venn_diagram = glue("output/venn_diagram_*.svg"),
      venn_diagram_number_check = glue("output/venn_diagram_number_check_*.csv")
    )
  ),

  #comment("Stage 4 - Table 2 subgroups"),
  action(
    name = "stage4_table_2_subgroups_both",
    run = "r:latest analysis/table_2_subgroups.R both",
    needs = list("preprocess_data","stage1_data_cleaning_both"),
    moderately_sensitive = list(
      table_2_subgroups = glue("output/table_2_subgroups_*.csv"),
      input_1_aer = glue("output/input1_aer_*.csv")
    )
  ),
  #comment("Stage 5 - Apply models"),
  splice(
    # over outcomes
    unlist(lapply(outcomes_model, function(x) apply_model_function(outcome = x)), recursive = FALSE)
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
as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")

