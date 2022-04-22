## =============================================================================
## Project:     Post covid vaccinated project
##
## Purpose:  Build flowchart to illustrate the flow of diabetes diagnoses using the algorithm
## 
## Authors: Kurt Taylor
## Reviewer: 
## 
## Content: 
## 0. Load relevant libraries and read data/arguments
## 1. Construct flowchart
## 2. Save output as PNG: "diabetes_flow.png" (save output as CSV in OpenSafely and then run rest of script outside of OS environment)
## =============================================================================

###############################################
# 0. Load relevant libraries and read in data #
###############################################

# Libraries

libraries <- c("readr", "dplyr", "stringr", "tidyverse", "plyr")
# libraries <- c("readr", "dplyr", "stringr", "tidyverse", "DiagrammeR", "DiagrammeRsvg", "rsvg", "plyr")
lapply(libraries, require, character.only=T)

# Specify command arguments ----------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort <- "vaccinated"
} else {
  cohort <- args[[1]]
}

fs::dir_create(here::here("output", "not-for-review"))
fs::dir_create(here::here("output", "for-review", "data-for-figures"))

# Load Stage 1 dataset

diabetes_df <-read_rds(paste0("output/not-for-review/input_",cohort,"_stage1.rds"))

###############################################
# 1. Construct flowchart -------------------- #
###############################################

# Define boxes for flow chart within a list of values using algorithm steps created in preprocess_data.R.

values <- list(
  # Diabetes diagnostic code / medication / care process code plus ethnicity code in primary or secondary care
  a = nrow(diabetes_df),
  # Step 1 any gestational diabetes code
  b = sum(diabetes_df$step_1 == "Yes", na.rm = T),
  b2 = sum(diabetes_df$step_1 == "No", na.rm = T),
  # Step 1a Any type 1 / type 2 code
  c = sum(diabetes_df$step_1a == "Yes", na.rm = T),
  c2 = sum(diabetes_df$step_1a == "No", na.rm = T),
  # GESTATIONAL DIABETES
  d = sum(diabetes_df$out_cat_diabetes == "GDM", na.rm = T),
  # Step 2 non-metformin oral antidiabetic 
  e = sum(diabetes_df$step_2 == "Yes", na.rm = T),
  e2 = sum(diabetes_df$step_2 == "No", na.rm = T),
  # TYPE 2 DIABETES
  f = sum(diabetes_df$out_cat_diabetes == "T2DM", na.rm = T),
  # Step 3 Type 1 code but no Type 2 code
  g = sum(diabetes_df$step_3 == "Yes", na.rm = T),
  g2 = sum(diabetes_df$step_3 == "No", na.rm = T),
  # TYPE 1 DIABETES 
  h = sum(diabetes_df$out_cat_diabetes == "T1DM", na.rm = T),
  # Step 4 Type 2 code and no type 1 code
  i = sum(diabetes_df$step_4 == "Yes", na.rm = T),
  i2 = sum(diabetes_df$step_4 == "No", na.rm = T),
  # Step 5 Aged <35 yrs (30 yrs for South Asians and African descent) at first diagnostic code)
  j = sum(diabetes_df$step_5 == "Yes", na.rm = T),
  j2 = sum(diabetes_df$step_5 == "No", na.rm = T),
  # Step 6 Type 1 and Type 2 codes present
  k = sum(diabetes_df$step_6 == "Yes", na.rm = T),
  k2 = sum(diabetes_df$step_6 == "No", na.rm = T),
  # Step 6a Type 1 only recorded in primary care
  l = sum(diabetes_df$step_6a == "Yes", na.rm = T),
  l2 = sum(diabetes_df$step_6a == "No", na.rm = T),
  # Step 6b Type 2 only recorded in primary care
  m = sum(diabetes_df$step_6b == "Yes", na.rm = T),
  m2 = sum(diabetes_df$step_6b == "No", na.rm = T),
  # Step 6c N Type 1 > N Type 2 codes
  n = sum(diabetes_df$step_6c == "Yes", na.rm = T),
  n2 = sum(diabetes_df$step_6c == "No", na.rm = T),
  # Step 6d N Type 2 > N Type 1 codes
  o = sum(diabetes_df$step_6d == "Yes", na.rm = T),
  o2 = sum(diabetes_df$step_6d == "No", na.rm = T),
  # Step 6e Type 2 code most recent
  p = sum(diabetes_df$step_6e == "Yes", na.rm = T),
  p2 = sum(diabetes_df$step_6e == "No", na.rm = T),
  # Step 7 Diabetes medication OR >= 5 care process codes OR HBA1c >= 47.5mmol)
  q = sum(diabetes_df$step_7 == "Yes", na.rm = T),
  q2 = sum(diabetes_df$step_7 == "No", na.rm = T),
  # DIABETES UNSPECIFIED TYPE
  r = sum(diabetes_df$out_cat_diabetes == "DM unspecified", na.rm = T),
  # DIABETES UNLIKELY
  s = sum(diabetes_df$out_cat_diabetes == "DM unlikely", na.rm = T))

# REDACT <=5 TO NA --------------------------------------------------------------

values <- lapply(values, function(x) replace(x, x <= 5, NA))

# EXPORT RAW DATA FOR FLOWCHART -------------------------------------------
# exporting data so that flow chart can easily be produced/tweaked outside of L4. 

values_df <- ldply(values, data.frame) # convert list to df
values_df_t <- data.table::transpose(values_df) # transpose df
names(values_df_t) <- lapply(values_df_t[1, ], as.character) # make row 1 the column names
values_df_t <- values_df_t[-1, ] 
write.csv(values_df_t, file = paste0("output/for-review/data-for-figures/diabetes_flow_values_",cohort,".csv")) # save
# I have checked and using the dataframe "values_df_t" gets the exact same flow chart as when using the list. 

# BUILD FLOW --------------------------------------------------------------

# flow <- DiagrammeR::grViz("
# digraph graph2 {
# 
# graph [layout = dot]
# 
# # NODE DEFINITIONS AND SUBSTITUTED TEXT
# 
# node [shape = rectangle, width = 4, fillcolor = Biege]
# a [label = '@@1', fontname = 'Arial Rounded MT']
# b [label = '@@2', fontname = 'Arial Rounded MT']
# c [label = '@@3', fontname = 'Arial Rounded MT']
# e [label = '@@5', fontname = 'Arial Rounded MT']
# g [label = '@@7', fontname = 'Arial Rounded MT']
# i [label = '@@9', fontname = 'Arial Rounded MT']
# j [label = '@@10', fontname = 'Arial Rounded MT']
# k [label = '@@11', fontname = 'Arial Rounded MT']
# l [label = '@@12', fontname = 'Arial Rounded MT']
# m [label = '@@13', fontname = 'Arial Rounded MT']
# n [label = '@@14', fontname = 'Arial Rounded MT']
# o [label = '@@15', fontname = 'Arial Rounded MT']
# p [label = '@@16', fontname = 'Arial Rounded MT']
# q [label = '@@17', fontname = 'Arial Rounded MT']
# 
# node [shape = oval, width = 4, style = filled]
# d [label = '@@4', fillcolor = red, fontname = 'Arial Rounded MT Bold']
# f [label = '@@6', fillcolor = red, fontname = 'Arial Rounded MT Bold']
# h [label = '@@8', fillcolor = red, fontname = 'Arial Rounded MT Bold']
# r [label = '@@18', fillcolor = red, fontname = 'Arial Rounded MT Bold']
# s [label = '@@19', fillcolor = red, fontname = 'Arial Rounded MT Bold']
# 
# # DRAW FLOW CHART AND LABELS
# 
# a -> b 
# b -> c [label = 'Yes']
# b -> e [label = 'No']
# c -> d [label = 'No']
# c -> e [label = 'Yes']
# e -> f [label = 'Yes']
# e -> g [label = 'No']
# g -> h [label = 'Yes']
# g -> i [label = 'No']
# i -> f [label = 'Yes']
# i -> j [label = 'No']
# j -> h [label = 'Yes']
# j -> k [label = 'No']
# k -> l [label = 'Yes']
# l -> h [label = 'Yes']
# l -> m [label = 'No']
# m -> f [label = 'Yes']
# m -> n [label = 'No']
# n -> h [label = 'Yes']
# n -> o [label = 'No']
# o -> f [label = 'Yes']
# o -> p [label = 'No']
# p -> f [label = 'Yes']
# p -> h [label = 'No']
# k -> q [label = 'No']
# q -> r [label = 'Yes']
# q -> s [label = 'No']
# 
# }
# 
# # WRITE LABELS TO BE USED ABOVE
# 
# [1]: paste0('Study population (N = ', values$a, ')')
# [2]: paste0('1. Any gestational diabetes code (N Yes = ', values$b, ', N No = ', values$b2, ')')
# [3]: paste0('1a. Any type 1 / type 2 codes (N Yes = ', values$c, ', N No = ', values$c2, ')')
# [4]: paste0('Gestational diabetes (N = ', values$d, ')')
# [5]: paste0('2. Non-metformin oral anti-diabetic (N Yes = ', values$e, ', N No = ', values$e2, ')')
# [6]: paste0('Type 2 Diabetes (N = ', values$f, ')')
# [7]: paste0('3. Type 1 code and no Type 2 code (N Yes = ', values$g, ', N No = ', values$g2, ')')
# [8]: paste0('Type 1 Diabetes (N = ', values$h, ')')
# [9]: paste0('4. Type 2 code and no Type 1 code (N Yes = ', values$i, ', N No = ', values$i2, ')')
# [10]: paste0('5. Aged < 35 yrs (<30 yrs if South Asian / African) at first diagnostic code (N Yes = ', values$j, ', N No = ', values$j2, ')')
# [11]: paste0('6. Type 1 and Type 2 codes present (N Yes = ', values$k, ', N No = ', values$k2, ')')
# [12]: paste0('6a. Type 1 only recorded in primary care (N Yes = ', values$l, ', N No = ', values$l2, ')')
# [13]: paste0('6b. Type 2 only recorded in primary care (N Yes = ', values$m, ', N No = ', values$m2, ')')
# [14]: paste0('6c. N Type 1 > N Type 2 codes (N Yes = ', values$n, ', N No = ', values$n2, ')')
# [15]: paste0('6d. N Type 2 > N Type 1 codes (N Yes = ', values$o, ', N No = ', values$o2, ')')
# [16]: paste0('6e. Type 2 code most recent (N Yes = ', values$p, ', N No = ', values$p2, ')')
# [17]: paste0('7. Diabetes medication OR >= 5 process codes OR HbA1c >= 47.5mmol (N Yes = ', values$q, ', N No = ', values$q2, ')')
# [18]: paste0('Diabetes unspecified type (N = ', values$r, ')')
# [19]: paste0('Diabetes unlikely (N = ', values$s, ')')
# ")
# 
# ###############################################
# # 2. Output  -------------------- #
# ###############################################
# 
# flow %>%
#   export_svg() %>%
#   charToRaw %>% 
#   rsvg_png(file.path("output", "diabetes_flow.png"))

# END