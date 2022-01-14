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

# outcome 1: ami
n_events[1] <- length(which(!is.na(input$out_date_ami)))
