#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms

library(tidyverse)
library(lubridate)
library(zoo)
#-----------------------------------------------
#Step1.Calculate the average daily CVD incidence
#-----------------------------------------------
#Description: Calculate the average daily CVD incidence -
             # before or in the absence of COVID-19 across the whole follow up period -
             # separately in subgroups defined by age and sex (RT - time being, for whole sample)

#1.RISK START DATE
#a.Cohort start date 2021-06-1
input$delta_start <- as.Date("2021-06-01", format="%Y-%m-%d")
#b.if vaccinated, 14 days after the second vaccination
input$immune_start <- as.Date(input$vax_date_covid_2)+14
#c.At risk start time - latest of a,b as RISK - COHORT start date
input$risk_start_date <- pmax(input$delta_start, input$immune_start, na.rm = TRUE)

#2.RISK END DATE
#a.Cohort end date 2021-12-14 
input$delta_end <- as.Date("2021-12-14", format="%Y-%m-%d")
#b.sample follow up ends on Death date, if any
input$death_date <- as.Date(input$death_date, format="%Y-%m-%d")
#c.sample follow up ends on outcome event, if any
input$out_date_ami <- as.Date(input$out_date_ami, format="%Y-%m-%d")
#input$out_stroke_isch <- as.Date(input$out_date_stroke_isch, format="%Y-%m-%d")
#input$out_dvt <- as.Date(input$out_date_dvt, format="%Y-%m-%d")
#input$out_pe <- as.Date(input$out_date_pe, format="%Y-%m-%d")
#input$out_tia <- as.Date(input$out_date_tia, format="%Y-%m-%d")
#input$out_stroke_sah_hs <- as.Date(input$out_date_stroke_sah_hs, format="%Y-%m-%d")
#input$out_hf <- as.Date(input$out_date_hf, format="%Y-%m-%d")
#input$out_angina <- as.Date(input$out_date_angina, format="%Y-%m-%d")
#input$out_ate <- as.Date(input$out_date_ate, format="%Y-%m-%d")
#input$out_vte <- as.Date(input$out_date_vte, format="%Y-%m-%d")
#d.Infection date(for the uninfected group)
input$exp_date_covid19_confirmed <- as.Date(input$exp_date_covid19_confirmed,  format="%Y-%m-%d")
#e.Risk end date - earliest of a,b, c, d for the non-exposed/uninfected group
input$risk_end_date <- pmin(input$delta_end, 
                       input$death_date, 
                       input$out_date_ami,
                       input$exp_date_covid19_confirmed,  na.rm = TRUE)

table(input$risk_end_date)


#Step2. Calculate the daily CVD incidence----------------------------------------------
#Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
             # for that day to derive the incidence on each day after COVID-19. 

#Step3. Make life table to calculate cumulative risk over time---------------------------------------------
#Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, -
             # with and without COVID-19. 

#Step4. Calculate the Absolute excess risk-------------------------------------------------------------------
#Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
             #compared with no COVID-19 diagnosis. 

#Step5. Calculate the Overall Absolute excess risk------------------------------------------------------------
#Description:Overall absolute excess risk is estimated from a weighted sum of the age and sex-specific excess risks, -
             #weighted by the proportions of people in age and sex strata within the COVID-19 infected population during the follow-up period.