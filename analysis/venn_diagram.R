## =============================================================================
## Purpose:  Create venn diagrams
## 
## Author:   Yinghui Wei
## 
## Date:     6 December 2021
##
## Data:     Post covid vaccinated project study population
##
## Content:  to create a Venn diagram for each outcome outlining overlap in 
##           reporting from different data sources
## =============================================================================

library(arrow); library("ggvenn")

# read in data------------------------------------------------------------
input <- arrow::read_feather(file = file.path("output", "input.feather"))

# #------Testing Example with a Function ---------------------------------------
# # - outcome: ami
# 
# index1 <- which(!is.na(input$ami_snomed))
# index2 <- which(!is.na(input$ami_icd10_hes))
# index3 <- which(!is.na(input$ami_icd10_death))
# 
# # - Figure: has count and percentage---------------------------------------
# y <- list(index1,index2, index3)
# names(y) <- c("SNOMED", "Hospital Episode", "Deaths")
# 
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
venn_digram3 <- function(outocome_names, figure_name, figure_title)
{
  index1 <- which(!is.na(input[,outcome_names[1]]))
  index2 <- which(!is.na(input[,outcome_names[2]]))
  index3 <- which(!is.na(input[,outcome_names[3]]))
  index = list(index1, index2, index3)
  names(index) <- c("SNOMED", "Hospital Episode", "Deaths")
  print(index)
  png(file=file.path("output", figure_name))
  print(ggvenn(
    index, 
    fill_color = c("thistle", "lightcyan", "lemonchiffon"),
    stroke_color = "white",
    text_size = 5,
    set_name_size = 5, 
    fill_alpha = 0.9
  ) +  ggtitle(figure_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")))
  dev.off()
}

# venn diagram for data from two sources
venn_digram2 <- function(outocome_names, figure_name, figure_title)
{
  index1 <- which(!is.na(input[,outcome_names[1]]))
  index2 <- which(!is.na(input[,outcome_names[2]]))
  index = list(index1, index2)
  names(index) <- c("Hospital Episode", "Deaths")
  print(index)
  png(file=file.path("output", figure_name))
  print(ggvenn(
    index, 
    fill_color = c("thistle", "lightcyan"),
    stroke_color = "white",
    text_size = 5,
    set_name_size = 5, 
    fill_alpha = 0.9
  ) +  ggtitle(figure_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")))
  dev.off()
}

# Rules: four data sources with particlarly specified order
venn_digram4 <- function(index, figure_name, figure_title)
{
  #png(file="output/venn_ami2.png")
  print(index)
  png(file=file.path("output", figure_name), width = 600, height = 480)
  print(ggvenn(
    index, 
    fill_color = c("thistle", "lightcyan", "lemonchiffon", "orange"),
    stroke_color = "white",
    stroke_size = 0.8, # stroke size doesn't seem to have an effect
    text_size = 5,
    set_name_size = 5, 
    fill_alpha = 0.9
  ) +  ggtitle(figure_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")))
  dev.off()
}


#-Apply the relevant function to each outcome-----------------------------------
# Rules: three data sources in the order of "SNOMED", "Hospital Episode", "Deaths"
# Rules: four data sources with particlarly specified order

# outcome 1: ami
ami_names <- c("ami_snomed", "ami_icd10_hes", "ami_icd10_death")
venn_digram3(ami_names, "1 venn_ami.png", "Acute Myocardial Infarction")

# outcome 2: stroke

stroke_names <- c("stroke_isch_snomed", "stroke_isch_icd10_hes", "stroke_isch_icd10_death")
venn_digram3(stroke_names,"2 venn_stroke.png", "Ischaemic Stroke")

# outcome 3: pe
pe_names <- c("pe_snomed", "pe_icd10_hes", "pe_icd10_death")
venn_digram3(pe_names, "3 venn_pe.png", "Pulmonary Embolism")

# outcome 4: dvt
index1 <- which(!is.na(input$dvt_icd10_hes))
index2 <- which(!is.na(input$dvt_pregnancy_icd10_hes))
index3 <- which(!is.na(input$dvt_icd10_death))
index4 <- which(!is.na(input$dvt_pregnancy_icd10_death))

index <- list(index1, index2, index3, index4)
names(index) <-  c("HE", "Preg HE", "Deaths", "Preg D")
venn_digram4(index,"4 venn_dvt.png", "Deep Vein Thrombosis")

# outcome 5: tia
tia_names <- c("tia_snomed", "tia_icd10_hes", "tia_icd10_death")
venn_digram3(tia_names, "5 venn_tia.png", "Transient Ischaemic Attack")

# outcome 6: stroke_sah_hs
stroke_sah_hs_names <- c("stroke_sah_hs_snomed", "stroke_sah_hs_icd10_hes", "stroke_sah_hs_icd10_death")
venn_digram3(tia_names, "6 stroke_sah_hs.png", "Subarachnoid haemorrhage and haemorrhagic stroke")

# outcome 7: hf
hf_names <- c("hf_snomed", "hf_icd10_hes", "hf_icd10_death")
venn_digram3(hf_names, "7 venn_hf.png", "Heart Failure")

# outcome 8: angina
angina_names <- c("agina_snomed", "angina_icd10_hes", "angina_icd10_death")
venn_digram3(angina_names, "8 venn_angina.png", "Angina")

# outcome 9: oae (what is this? Arterial thrombosis events? 
#only two sources: deaths and hes, but no SONMED?
# there is a variable named "out_ate" - what is this?)

oae_names <- c("oae_icd10_hes", "oae_icd10_death")
venn_digram2(oae_names, "9 venn_oae.png", "Arterial Thrombosis Events")

# outcome 10: all_vte
vte_names <- c("all_vte_codes_snomed", "all_vte_codes_icd10_hes", "all_vte_codes_icd10_death")
venn_digram3(vte_names, "10 venn_vte.png", "Venous Thromboembolism Events")

