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

library(readr)

# If working on local PC with dummy data, uncomment the following two lines
population = "vaccinated" #commented out when using project yaml
population = "electively_unvaccinated" #commented out when using project yaml

# read in data------------------------------------------------------------

if(population == "vaccinated"){
  input <- read_rds("output/input_vaccinated.rds")
}

if(population == "electively_unvaccinated"){
  input <- read_rds("output/input_electively_unvaccinated.rds")
}

n_events <- rep(0,10)

number_events <- function(outcome)
{
  count <- length(which(!is.na(outcome)))
  return(count)
}

outcome_names <- c("out_date_ami",  "out_date_stroke_isch", 
                   "out_date_pe",   "out_date_dvt",
                   "out_date_tia",  "out_date_stroke_sah_hs", 
                   "out_date_hf",   "out_date_angina",
                   "out_date_ate",  "out_date_vte"
                   )

n_events <- c(lapply(input[,outcome_names], number_events))

n_events

# # outcome 1: ami
# n_events[1] <- number_events(input$out_date_ami)
# 
# # outcome 2: stroke
# n_events[2] <- number_events(input$out_date_stroke_isch)
# 
# # outcome 3: pe
# n_events[3] <- number_events(input$out_date_pe)
# 
# # outcome 4: dvt
# n_events[4] <- number_events(input$out_date_dvt)
# 
# # outcome 5: tia
# n_events[5] <- number_events(input$out_date_tia)
# 
# # outcome 6: stroke_sah_hs
# n_events[6] <- number_events(input$out_date_stroke_sah_hs)
# 
# # outcome 7: hf
# n_events[7] <- number_events(input$out_date_hf)
# 
# # outcome 8: angina
# n_events[8] <- number_events(input$out_date_angina)
# 
# # outcome 9: ate (Other Arterial Embolism)
# n_events[9] <- number_events(input$out_date_ate)
# 
# # outcome 10: vte
# n_events[10] <- number_events(input$out_date_vte)
# 
# 


