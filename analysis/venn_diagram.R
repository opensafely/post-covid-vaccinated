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

# YW Alert 9/Dec/2021: Code for outcome 4 (dvt) and outcome 9 (oae) will need to 
#                      be updated once the study definition is updated

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
# Once the study definition is updated, n_src = 4 can be removed
venn_digram <- function(outcome_names, figure_name, figure_title)
{
  print(outcome_names)
  n_src = length(outcome_names)
  if(n_src ==2){
    index1 <- which(!is.na(input[,outcome_names[1]]))
    index2 <- which(!is.na(input[,outcome_names[2]]))
    index = list(index1, index2)
    names(index) <- c("Hospital Episode", "Deaths")
    mycol=c("thistle", "lightcyan")
  }
  if(n_src ==3){
    index1 <- which(!is.na(input[,outcome_names[1]]))
    index2 <- which(!is.na(input[,outcome_names[2]]))
    index3 <- which(!is.na(input[,outcome_names[3]]))
    index = list(index1, index2, index3)
    names(index) <- c("SNOMED", "Hospital Episode", "Deaths")
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
  print(index)
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
ami_names <- c("ami_snomed", "ami_icd10_hes", "ami_icd10_death")
venn_digram(ami_names, "1 venn_ami.png", "Acute Myocardial Infarction")

# outcome 2: stroke
stroke_names <- c("stroke_isch_snomed", "stroke_isch_icd10_hes", "stroke_isch_icd10_death")
venn_digram(stroke_names,"2_venn_stroke.png", "Ischaemic Stroke")

# outcome 3: pe
pe_names <- c("pe_snomed", "pe_icd10_hes", "pe_icd10_death")
venn_digram(pe_names,"3_venn_pe.png", "Pulmonary Embolism")

# outcome 4: dvt
# comment: code for dvt will be updated once the study definition is updated
dvt_names<-  c("dvt_icd10_hes", "dvt_pregnancy_icd10_hes", 
               "dvt_icd10_death", "dvt_pregnancy_icd10_death")
venn_digram(dvt_names,"4_venn_dvt.png", "Deep Vein Thrombosis")

# outcome 5: tia
tia_names <- c("tia_snomed", "tia_icd10_hes", "tia_icd10_death")
venn_digram(tia_names, "5_venn_tia.png", "Transient Ischaemic Attack")

# outcome 6: stroke_sah_hs
stroke_sah_hs_names <- c("stroke_sah_hs_snomed", "stroke_sah_hs_icd10_hes", "stroke_sah_hs_icd10_death")
venn_digram(stroke_sah_hs_names, "6_stroke_sah_hs.png", "Subarachnoid haemorrhage and haemorrhagic stroke")

# outcome 7: hf
hf_names <- c("hf_snomed", "hf_icd10_hes", "hf_icd10_death")
venn_digram(hf_names, "7_venn_hf.png", "Heart Failure")

# outcome 8: angina
angina_names <- c("angina_snomed", "angina_icd10_hes", "angina_icd10_death")
venn_digram(angina_names, "8_venn_angina.png", "Angina")


# outcome 9: oae (what is this? Arterial thrombosis events? 
#only two sources: deaths and hes, but no SONMED?
# there is a variable named "out_ate" - what is this?)
# comment: code for outcome 9 will be updated once there are data from SNOMED
oae_names <- c("oae_icd10_hes", "oae_icd10_death")
venn_digram(oae_names, "9_venn_oae.png", "Arterial Thrombosis Events")

# outcome 10: all_vte
vte_names <- c("all_vte_codes_snomed", "all_vte_codes_icd10_hes", "all_vte_codes_icd10_death")
venn_digram(vte_names, "10_venn_vte.png", "Venous Thromboembolism Events")
