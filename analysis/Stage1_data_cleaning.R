## =============================================================================
## Project:     Post covid vaccinated project
##
##
## Purpose:  Apply stage 1. Data cleaning
##  - Prepare variables
##  - Apply QA rules
##  - Apply inclusion exclusion criteria
##  - Create cleaned datasets for the following sub-cohorts:
##    a. Vaccinated cohort
##    b. Electively unvaccinated
## 
## Authors: Yinghui Wei, Renin Toms, Rochelle Knight, Genevieve Cezard
## Reviewer: Genevieve Cezard
## 
## Date combined: 13 December 2021
## by Genevieve Cezard
##
##
## Content: 
## 0. Load relevant libraries and read data
## 1. Prepare all variables (re-factoring, re-typing)
##    1.a. Set factor variables as factor
##    1.b. Set the group with the highest frequency as the reference group
##    1.c. Check that continuous variables are defined as numeric variables
##    1.d. Check and specify date format for date variables
##    1.e. Apply changes in the input dataset
## 2. Apply QA rules
## 3. Apply exclusion/inclusion criteria
##    Differentiate criteria for the two sub-cohorts
##
## =============================================================================