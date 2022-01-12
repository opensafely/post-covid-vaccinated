## =============================================================================
## Purpose:  Create venn diagrams
## 
## Author:   Yinghui Wei
## 
## Date:     6 December 2021; updated 10 January 2022
##
## Data:     Post covid vaccinated project study population
##
## Content:  to create a Venn diagram for each outcome outlining overlap in 
##           reporting from different data sources
## =============================================================================

# YW Alert 9/Dec/2021: Code for outcome 4 (dvt) and outcome 9 (oae) will need to 
#                      be updated once the study definition is updated with data from 
#                      SNOMED for these two outcomes

library(readr); library("ggvenn")

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
# - outcome: ami
# 
# index1 <- which(!is.na(input$ami_snomed))
# index2 <- which(!is.na(input$ami_icd10_hes))
# index3 <- which(!is.na(input$ami_icd10_death))
# 
# # - Figure: has count and percentage---------------------------------------
# y <- list(index1,index2, index3)
# names(y) <- c("SNOMED", "Hospital Episodes", "Deaths")

# png(file="output/venn_ami2.png")
# ggvenn(
#   y,
#   fill_color = c("thistle", "lightcyan", "lemonchiffon"),
#   stroke_color = "white",
#   text_size = 5,
#   set_name_size = 5,
#   fill_alpha = 0.9
# ) +  ggtitle("Acute Myocardial Infarction") +
#   theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
# dev.off()

#-----------------------------------------------------------------------------

# -- write a function to create a venn diagram ------------------------------

# Rules: three data sources in the order of "SNOMED", "Hospital Episode", "Deaths"
# Once the study definition is updated, n_src = 4 can be removed
venn_digram <- function(outcome_names, figure_name, figure_title)
{
  print(outcome_names)
  n_src = length(outcome_names)
  if(n_src ==2){
    index1 <- which(!is.na(input[,outcome_names[1]]))
    index2 <- which(!is.na(input[,outcome_names[2]]))
    index = list(index1, index2)
    names(index) <- c("Hospital Episodes", "Deaths")
    mycol=c("thistle", "lightcyan")
  }
  if(n_src ==3){
    index1 <- which(!is.na(input[,outcome_names[1]]))
    index2 <- which(!is.na(input[,outcome_names[2]]))
    index3 <- which(!is.na(input[,outcome_names[3]]))
    index = list(index1, index2, index3)
    names(index) <- c("SNOMED", "Hospital Episodes", "Deaths")
    mycol=c("thistle", "lightcyan", "lemonchiffon")
  }
  if(n_src ==4){
    index1 <- which(!is.na(input[,outcome_names[1]]))
    index2 <- which(!is.na(input[,outcome_names[2]]))
    index3 <- which(!is.na(input[,outcome_names[3]]))
    index4 <- which(!is.na(input[,outcome_names[4]]))
    index = list(index1, index2, index3, index4)
    names(index) <-  c("HE", "Preg HE", "Deaths", "Preg D")
    mycol=c("thistle", "lightcyan", "lemonchiffon", "orange")
  }
  #print(index)
  png(file=file.path("output", figure_name))
  print(ggvenn(
    index, 
    fill_color = mycol,
    stroke_color = "white",
    text_size = 5,
    set_name_size = 5, 
    fill_alpha = 0.9
  ) +  ggtitle(figure_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")))
  dev.off()
}


#-Apply the relevant function to each outcome-----------------------------------
# Rules: three data sources in the order of "SNOMED", "Hospital Episode", "Deaths"
#        four data sources in a particular order specified in the function
#        two data sources in a particular order specified in the function

# outcome 1: ami
ami_names <- c("tmp_out_date_ami_snomed", "tmp_out_date_ami_hes", "tmp_out_date_ami_death")
figure_name <- paste0(population, "_", "f1_venn_ami", ".png")
venn_digram(ami_names, figure_name, "Acute Myocardial Infarction")

# outcome 2: stroke
stroke_names <- c("tmp_out_date_stroke_isch_snomed", "tmp_out_date_stroke_isch_hes", "tmp_out_date_stroke_isch_death")
figure_name <- paste0(population, "_", "f2_venn_stroke", ".png")
venn_digram(stroke_names,figure_name, "Ischaemic Stroke")

# outcome 3: pe
pe_names <- c("tmp_out_date_pe_snomed", "tmp_out_date_pe_hes", "tmp_out_date_pe_death")
figure_name <- paste0(population, "_", "f3_venn_pe", ".png")
venn_digram(pe_names,figure_name, "Pulmonary Embolism")

# outcome 4: dvt
# comment: code for dvt will be updated once the study definition is updated
#dvt_names<-  c("tmp_out_date_dvt_hes", "tmp_out_date_dvt_pregnancy_hes", 
#               "tmp_out_date_dvt_death", "tmp_out_date_dvt_pregnancy_death")

# merge dvt_hes and dvt_pregnancy_hes
index = which(is.na(input$tmp_out_date_dvt_hes)==T)
input$tmp_out_date_dvt_hes[index]= input$tmp_out_date_dvt_pregnancy_hes[index]

index = which(is.na(input$tmp_out_date_dvt_death)==T)
input$tmp_out_date_dvt_death[index]= input$tmp_out_date_dvt_pregnancy_death[index]

dvt_names<-  c("tmp_out_date_dvt_snomed", "tmp_out_date_dvt_hes", "tmp_out_date_dvt_death")
figure_name <- paste0(population, "_", "f4_venn_dvt", ".png")
venn_digram(dvt_names, figure_name, "Deep Vein Thrombosis")

# outcome 5: tia
tia_names <- c("tmp_out_date_tia_snomed", "tmp_out_date_tia_hes", "tmp_out_date_tia_death")
figure_name <- paste0(population, "_", "f5_venn_tia", ".png")
venn_digram(tia_names, figure_name, "Transient Ischaemic Attack")

# outcome 6: stroke_sah_hs
stroke_sah_hs_names <- c("tmp_out_date_stroke_sah_hs_snomed", "tmp_out_date_stroke_sah_hs_hes", "tmp_out_date_stroke_sah_hs_death")
figure_name <- paste0(population, "_", "f6_venn_stroke_sah_hs", ".png")
venn_digram(stroke_sah_hs_names, figure_name, "Subarachnoid haemorrhage and haemorrhagic stroke")

# outcome 7: hf
hf_names <- c("tmp_out_date_hf_snomed", "tmp_out_date_hf_hes", "tmp_out_date_hf_death")
figure_name <- paste0(population, "_", "f7_venn_hf", ".png")
venn_digram(hf_names, figure_name, "Heart Failure")

# outcome 8: angina
angina_names <- c("tmp_out_date_angina_snomed", "tmp_out_date_angina_hes", "tmp_out_date_angina_death")
figure_name <- paste0(population, "_", "f8_venn_angina", ".png")
venn_digram(angina_names, figure_name, "Angina Pectoris")

# outcome 9: ATE (Arterial thrombosis events)
ate_names <- c("tmp_out_date_ate_snomed", "tmp_out_date_ate_hes", "tmp_out_date_ate_death")
figure_name <- paste0(population, "_", "f9_venn_ate", ".png")
venn_digram(ate_names, figure_name, "All Arterial Thrombotic and Embolization Events")

# outcome 10: all_vte
vte_names <- c("tmp_out_date_vte_snomed", "tmp_out_date_vte_hes", "tmp_out_date_vte_death")
figure_name <- paste0(population, "_", "f10_venn_vte", ".png")
venn_digram(vte_names, figure_name, "All Venous Thromboembolism Events")

