## =============================================================================
## Purpose:  Create venn diagrams
## 
## Author:   Yinghui Wei
## 
## Date:     6 December 2021; updated 10 January 2022; updated 27 January 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  to create a Venn diagram for each outcome outlining overlap in 
##           reporting from different data sources
## Output:   Venn diagrams in SVG files, venn_diagram_number_check.csv
## =============================================================================


library(readr); library("ggvenn"); library("svglite"); library("gridExtra")

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  #population <- "vaccinated"
  pouplation <- "electively_unvaccinated"
} else {
  population <- args[[1]]
}

# indicate active analyses -----------------------------------------------

active_analyses <- read_rds("output/active_analyses.rds")

# read in data------------------------------------------------------------
input <- read_rds(paste0("output/venn_",population,".rds"))
input_stage1 <- read_rds(paste0("output/input_", population,"_stage1.rds"))
input <- input %>% inner_join(input_stage1,by="patient_id")

variable_names <- tidyselect::vars_select(names(input), starts_with(c('tmp_out_date_','out_date'), ignore.case = TRUE))
input <- input[,variable_names]

#-- function to check number < 5 in the venn diagram ---------------------------

count_le5 <- function(outcome_names)
{
  print(outcome_names)
  
  #---"SNOMED", "Hospital Episodes", "Deaths"--------
  index1 <- which(!is.na(input[,outcome_names[1]]))
  index2 <- which(!is.na(input[,outcome_names[2]]))
  index3 <- which(!is.na(input[,outcome_names[3]]))
  
  inter12 <- intersect(index1, index2)
  inter23 <- intersect(index2, index3)
  inter13 <- intersect(index1, index3)
  inter123 <- intersect(inter12, index3)
  len_inter12_only <- length(inter12) - length(inter123)
  len_inter23_only <- length(inter23) - length(inter123)
  len_inter13_only <- length(inter13) - length(inter123)
  len_src1_only <- length(index1) - length(inter12) - len_inter13_only
  len_src2_only <- length(index2) - length(inter12) - len_inter23_only
  len_src3_only <- length(index3) - length(inter13) - len_inter23_only

  number_venn <- c(len_src1_only, len_src2_only, len_src3_only, len_inter12_only, len_inter13_only, len_inter23_only, length(inter123))
  #number_venn
  low_count <- length(which(number_venn <5))
  return(low_count)
}


#-- function to create venn diagram --------------------------------------------

venn_digram <- function(outcome_names, figure_name, figure_title)
{
  print(outcome_names)
  n_src = length(outcome_names)
  if(n_src ==3){
    index1 <- which(!is.na(input[,outcome_names[1]]))
    index2 <- which(!is.na(input[,outcome_names[2]]))
    index3 <- which(!is.na(input[,outcome_names[3]]))
    index = list(index1, index2, index3)
    names(index) <- c("SNOMED", "Hospital Episodes", "Deaths")
    mycol=c("thistle", "lightcyan", "lemonchiffon")
  }else{
    print("number of data sources != 3")
  }
  svglite(file= paste0("output/",figure_name, ".svg"))
  g <- ggvenn(
    index, 
    fill_color = mycol,
    stroke_color = "white",
    text_size = 5,
    set_name_size = 5, 
    fill_alpha = 0.9
  ) +  ggtitle(figure_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
  print(g)
  dev.off()
}

# replace this line with the outcome variable name in the active analyses table
#outcome_full_names_sources <- tidyselect::vars_select(names(input), starts_with('tmp_out_date_', ignore.case = TRUE))
outcome_variable_names <- active_analyses$outcome_variable[active_analyses$active==T]
outcome_full_names_sources <- NULL
for(i in 1: length(outcome_variable_names))
{
  outcome_full_names_sources <- c(outcome_full_names_sources,
                                   paste0("tmp_", outcome_variable_names[i], "_snomed"),
                                   paste0("tmp_", outcome_variable_names[i], "_hes"),
                                   paste0("tmp_", outcome_variable_names[i], "_death"))
}

outcome_names_sources <- gsub("tmp_out_date_","",outcome_full_names_sources) #delete the prefix
outcome_names <- gsub("_snomed","",outcome_names_sources) 
outcome_names <- gsub("_hes","",outcome_names) 
outcome_names <- gsub("_death","",outcome_names) 

unique_outcome_names <- unique(outcome_names)

count_less_than_5 <- rep("NA", length(unique_outcome_names))

index_lc = 1

#--10 separate svg files, one for each outcome----------------------------------
for (i in unique_outcome_names){
  print(i)
  index <- which(outcome_names == i)
  venn_outcome <- outcome_full_names_sources[index]
  print(venn_outcome)
  if(length(venn_outcome)!=3){print("number of data sources > 3!")}
  figure_name = figure_title <- paste0(population, "_", i)
  venn_digram(venn_outcome,figure_name, figure_title)
  count_less_than_5[index_lc] <- count_le5(venn_outcome)
  index_lc = index_lc +1
}

low_count_df <- data.frame(count_less_than_5, unique_outcome_names)
low_count_df <- low_count_df %>% mutate(count_less_than_5 = if_else(count_less_than_5 >0, "lower than 5", "No issue"))

names(low_count_df) <- c("any number < 5?", "outcome name")
write.csv(low_count_df, file= paste0("output/",population, "_venn_diagram_number_check.csv"), row.names = F)

