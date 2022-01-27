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
## =============================================================================


library(readr); library("ggvenn"); library("svglite"); library("gridExtra")

args = commandArgs(trailingOnly=TRUE)
population  = args[[1]] # vaccinated population or electively unvaccinated population

# If working on local PC with dummy data, uncomment the following two lines
#population="vaccinated" #this will need to be removed when using project yaml
#population="electively_unvaccinated" #this will need to be removed when using project yaml


# read in data------------------------------------------------------------

if(population == "vaccinated"){
   input <- read_rds("output/venn_vaccinated.rds")
 }

if(population == "electively_unvaccinated"){
  input <- read_rds("output/venn_electively_unvaccinated.rds")
}

# #------Testing Example with a Function ---------------------------------------
# # - outcome: ami
# 
#  index1 <- which(!is.na(input$ami_snomed))
#  index2 <- which(!is.na(input$ami_icd10_hes))
#  index3 <- which(!is.na(input$ami_icd10_death))
# 
#  # - Figure: has count and percentage---------------------------------------
#  y <- list(index1,index2, index3)
#  names(y) <- c("SNOMED", "Hospital Episodes", "Deaths")
# 
# svglite(file="output/venn_ami2.svg")
# par(mfrow=c(2,1))
#  g <- ggvenn(
#    y,
#    fill_color = c("thistle", "lightcyan", "lemonchiffon"),
#    stroke_color = "white",
#    text_size = 5,
#    set_name_size = 5,
#    fill_alpha = 0.9
#  ) +  ggtitle("Acute Myocardial Infarction2") +
#    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
#  g
#  dev.off()
#  
 

#-----------------------------------------------------------------------------

# -- write a function to create a venn diagram ------------------------------

# Rules: three data sources in the order of "SNOMED", "Hospital Episode", "Deaths"
venn_digram <- function(outcome_names, figure_title)
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
  g <- ggvenn(
    index, 
    fill_color = mycol,
    stroke_color = "white",
    text_size = 5,
    set_name_size = 5, 
    fill_alpha = 0.9
  ) +  ggtitle(figure_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
  return(g)
}


venn_digram2 <- function(outcome_names, figure_name, figure_title)
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
#-Apply the relevant function to each outcome-----------------------------------
# Rules: three data sources in the order of "SNOMED", "Hospital Episode", "Deaths"
# outcome 1: ami
ami_names <- c("tmp_out_date_ami_snomed", "tmp_out_date_ami_hes", "tmp_out_date_ami_death")
#figure_name <- paste0(population, "_", "f1_venn_ami", ".svg")
g1 <- venn_digram(ami_names, "Acute Myocardial Infarction")

# outcome 2: stroke
stroke_names <- c("tmp_out_date_stroke_isch_snomed", "tmp_out_date_stroke_isch_hes", "tmp_out_date_stroke_isch_death")
g2 <- venn_digram(stroke_names,"Ischaemic Stroke")

# outcome 3: pe
pe_names <- c("tmp_out_date_pe_snomed", "tmp_out_date_pe_hes", "tmp_out_date_pe_death")
g3 <- venn_digram(pe_names, "Pulmonary Embolism")

# outcome 4: dvt
# merge dvt_hes and dvt_pregnancy_hes
index = which(is.na(input$tmp_out_date_dvt_hes)==T)
input$tmp_out_date_dvt_hes[index]= input$tmp_out_date_dvt_pregnancy_hes[index]

index = which(is.na(input$tmp_out_date_dvt_death)==T)
input$tmp_out_date_dvt_death[index]= input$tmp_out_date_dvt_pregnancy_death[index]

dvt_names<-  c("tmp_out_date_dvt_snomed", "tmp_out_date_dvt_hes", "tmp_out_date_dvt_death")
g4 <- venn_digram(dvt_names, "Deep Vein Thrombosis")

# outcome 5: tia
tia_names <- c("tmp_out_date_tia_snomed", "tmp_out_date_tia_hes", "tmp_out_date_tia_death")
g5 <- venn_digram(tia_names, "Transient Ischaemic Attack")

# outcome 6: stroke_sah_hs
stroke_sah_hs_names <- c("tmp_out_date_stroke_sah_hs_snomed", "tmp_out_date_stroke_sah_hs_hes", "tmp_out_date_stroke_sah_hs_death")
g6 <- venn_digram(stroke_sah_hs_names, "Subarachnoid haemorrhage and haemorrhagic stroke")

# outcome 7: hf
hf_names <- c("tmp_out_date_hf_snomed", "tmp_out_date_hf_hes", "tmp_out_date_hf_death")
g7 <- venn_digram(hf_names, "Heart Failure")

# outcome 8: angina
angina_names <- c("tmp_out_date_angina_snomed", "tmp_out_date_angina_hes", "tmp_out_date_angina_death")
g8 <- venn_digram(angina_names,  "Angina Pectoris")

# outcome 9: ATE (Arterial thrombosis events)
ate_names <- c("tmp_out_date_ate_snomed", "tmp_out_date_ate_hes", "tmp_out_date_ate_death")
g9 <- venn_digram(ate_names, "All Arterial Thrombotic and Embolization Events")

# outcome 10: all_vte
vte_names <- c("tmp_out_date_vte_snomed", "tmp_out_date_vte_hes", "tmp_out_date_vte_death")
g10 <- venn_digram(vte_names, "All Venous Thromboembolism Events")

figure_name <- paste0("venn", "_", population, ".svg")
svglite(file=file.path("output", figure_name), width=25, height=15)
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, ncol=5)
dev.off()


#----------Approach 2: automation----------------#
# pitfall: lose figure titles

outcome_full_names_sources <- tidyselect::vars_select(names(input), starts_with('tmp_out_date_', ignore.case = TRUE))
outcome_names_sources <- gsub("tmp_out_date_","",outcome_full_names_sources) #delete the prefix
outcome_names <- gsub("_snomed","",outcome_names_sources) 
outcome_names <- gsub("_hes","",outcome_names) 
outcome_names <- gsub("_death","",outcome_names) 

outcome_names

length(unique(outcome_names))

unique_outcome_names <- unique(outcome_names)

#--10 separate svg files, one for each outcome-----------------------------
for (i in unique_outcome_names){
  print(i)
  index <- which(outcome_names == i)
  venn_outcome <- outcome_full_names_sources[index]
  if(length(venn_outcome)!=3){print("number of data sources > 3!")}
  figure_name = figure_title <- paste0(population, "_", i)
  venn_digram2(venn_outcome,figure_name, figure_title)
}

#index <- which(outcome_names == unique_outcome_names[1])

#venn_outcome <- outcome_full_names_sources[index]
#g1 <- venn_digram(venn_outcome, "Acute Myocardial Infarction")
#g1 <- venn_digram(venn_outcome, outcome_names[1])


# for (i in unique_outcome_names){
#   print(i)
#   index <- which(outcome_names == i)
#   venn_outcome <- outcome_full_names_sources[index]
#   if(length(venn_outcome)!=3){print("number of data sources > 3!")}
#   j = which(unique_outcome_names==i) 
#   g <- venn_digram(venn_outcome,i)
#   print(g)
# }

# approach two automation: one venn diagram in one svg file
# approach one: 10 venn diagrams in one svg file
