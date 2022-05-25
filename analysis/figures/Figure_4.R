#Project:Vaccinated delta wave population study
#Branch:Figure4_graphical plotting of the estimated AER of ATE and VTE 
#Scripts: Renin Toms, Venexia Walker
#Reviewer: Genevieve Cezard

#TO RUN OUTSIDE OPENSAFELY
# 1. load the right input data and make sure of the file names and variable structure
# 2. Cntrl+A run the whole script and find the .png graph files in working directory

#USE TO RUN A SINGLE PLOT
group <- "vaccinated"
fit <- "mdl_max_adj"
outcome <- "ate"
strata <- "sex_Female"

library(purrr)
library(data.table)
library(tidyverse)

#******************************************************
#I. Create a function to out put the figure 4_tables
#******************************************************

figure4_tbl <- function(group, fit, outcome, strata){
  
  #Load data 
  #1.Input1 - 1.unexposed person days
  input1.1 <- readr::read_csv("output/review/descriptives/input1_aer_main_vaccinated.csv")
  input1.2 <- readr::read_csv("output/review/descriptives/input1_aer_main_electively_unvaccinated.csv") 
  input1.3 <- readr::read_csv("output/review/descriptives/input1_aer_subgroups_vaccinated.csv")
  input1.4 <- readr::read_csv("output/review/descriptives/input1_aer_subgroups_electively_unvaccinated.csv") 
  
  #Preprocess input1                                                             #ADDS TWO MODEL FITS WITH SAME PERSON DAYS
  input1.5 <- rbind(input1.1,input1.2,input1.3,input1.4)                                           
  input1.5$fit <- "mdl_agesex"                                                   
  
  input1.6 <- input1.5                                                           
  input1.6$fit <- "mdl_max_adj"                                                  
  
  input1 <-rbind(input1.5,input1.6)                                              
  input1 <- input1 %>% select(-strata)                                           
  rm(input1.1, input1.2, input1.3, input1.4,input1.5, input1.6)
  
  #structure the input
  input1$unexposed_person_days <- as.numeric(input1$unexposed_person_days)
  
  #Input2 - 2.unexposed events, 3.total cases, 4.hr
  hr_files=list.files(path = "output/review/model", pattern = "compiled_HR_results_*")
  hr_files=hr_files[endsWith(hr_files,".csv")]
  hr_files=paste0("output/review/model/",hr_files)
  input2 <- purrr::pmap(list(hr_files),
                        function(fpath){
                          df <- fread(fpath)
                          return(df)})
  input2=rbindlist(input2, fill=TRUE)
  
  #Preprocess the Input2 
  input2 <- input2 %>% select(-conf.low, -conf.high, -std.error,-robust.se, -P, -covariates_removed, -cat_covars_collapsed)
  input2 <- input2 %>% filter(term == "days0_14" |
                                term == "days14_28" |
                                term == "days28_56" |
                                term == "days56_84" |
                                term == "days84_197"|
                                term == "days0_28"|
                                term == "days28_197")
  #--------------------------------------
  # Step1: Extract the required variables
  #--------------------------------------
  #1. Person days
  fp_person_days <- input1[input1$event == outcome & input1$fit == fit  &
                             input1$cohort == group & input1$subgroup == strata,]$unexposed_person_days
  #2.unexposed events
  unexposed_events <- input2[input2$event == outcome & input2$model == fit  & 
                               input2$cohort == group & input2$subgroup == strata & 
                               input2$expo_week== "pre expo",]$events_total
  #3.Total cases
  total_cases <-  input2[input2$event == outcome & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata & 
                           input2$expo_week== "pre expo",]$total_covid19_cases
  #4.locate the estimates
  #0-14 days
  hr_14 <- input2[input2$event == outcome  & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata & input2$term == "days0_14",]$estimate
  #14-28 days
  hr_28 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days14_28",]$estimate
  #28-56 days
  hr_56 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days28_56",]$estimate
  #56-84 days
  hr_84 <- input2[input2$event == outcome & input2$model == fit  & 
                    input2$cohort == group & input2$subgroup == strata& input2$term == "days56_84",]$estimate
  #84-196 days
  hr_196 <- input2[input2$event == outcome & input2$model == fit  & 
                     input2$cohort == group & input2$subgroup == strata& input2$term == "days84_197",]$estimate
  #Alternative 0-28 days
  hr0_28 <- input2[input2$event == outcome  & input2$model == fit  & 
                     input2$cohort == group & input2$subgroup == strata& input2$term == "days0_28",]$estimate
  #Alternative 28_196 days
  hr28_196 <- input2[input2$event == outcome  & input2$model == fit  & 
                       input2$cohort == group & input2$subgroup == strata& input2$term == "days28_197",]$estimate
  #--------------------------------------------------------------------
  #Step2.Calculate the average daily CVD incidence   - in the unexposed
  #--------------------------------------------------------------------
  #Number of new events / sum of person-time at risk
  incidence_rate <- unexposed_events/fp_person_days
  #-------------------------------------------------------------
  #Step3. Make life table to calculate cumulative risk over time
  #-------------------------------------------------------------
  #Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, - with and without COVID-19. 
  lifetable <- data.frame(c(1:196))
  colnames(lifetable) <- c("days")
  lifetable$event <- outcome 
  lifetable$model <- fit 
  lifetable$cohort <- group
  lifetable$subgroup <- strata
  
  lifetable$q <- incidence_rate 
  lifetable$'1-q' <- 1 - lifetable$q 
  lifetable$s <- cumprod(lifetable$`1-q`)
  #----------------------------------------
  #Step4. Calculate the daily CVD incidence
  #----------------------------------------
  #Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
  # for that day to derive the incidence on each day after COVID-19. 
  
  #1.assign the hr estimates
  lifetable$h <- ifelse(lifetable$days < 15, rep(hr_14),0)
  lifetable$h <- ifelse(lifetable$days > 14 & lifetable$days < 29, rep(hr_28),lifetable$h)
  lifetable$h <- ifelse(lifetable$days < 29 & is.na(lifetable$h), rep(hr0_28),lifetable$h)#alternative for 0-28 days
  
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 57, rep(hr_56),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 56 & lifetable$days < 85, rep(hr_84),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 84 & lifetable$days < 197, rep(hr_196),lifetable$h)
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 197 & is.na(lifetable$h), rep(hr28_196),lifetable$h)#alternative for 28-196 days
  #2.assign qh
  lifetable$qh <- lifetable$q*lifetable$h
  #3.assign 1-qh
  lifetable$'1-qh' <- 1 - lifetable$qh
  #4.assign sc
  lifetable$sc <- cumprod(lifetable$`1-qh`)
  #-------------------------------------------
  #Step5. Calculate the Absolute excess risk--
  #-------------------------------------------
  #Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
  #compared with no COVID-19 diagnosis. 
  
  #1.AER =difference in absolute risk
  lifetable$'s-sc' <- lifetable$s - lifetable$sc
  
  #2.CI of the AER
  #Confidence Interval = Attributable risk +/- 1.96 x Square Root of [p x q (1/n1+ 1/n2)]
  #Where, p = qh, q = 1-qh, n1= unexposed person days, n2 = exposed person days
  #https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
  
  lifetable$CI <- 1.96*lifetable$qh*lifetable$'1-qh'*(1/fp_person_days + 1/fp_person_days)#RT - input awaited for person days, currently works on dummy.
  
  #3.AER%
  lifetable$AERp <-lifetable$'s-sc'*100
  
  #CI of AER%
  #95% CI = ARP +/- ARP x (C.I. range from the attributable risk / the attributable risk)
  #Where, ARP=AERp, CI range= CI, attributable risk = s-sc
  #https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
  
  lifetable$CIp <- lifetable$AERp*(lifetable$CI / lifetable$`s-sc`)
  lifetable$CIp.low <- lifetable$AERp - lifetable$CIp
  lifetable$CIp.high <- lifetable$AERp + lifetable$CIp
  #-------------------------------------------
  #Step6. Output1 the csv
  #-------------------------------------------
  
  write.csv(lifetable, paste0("output/review/model/lifetable_delta_" , group, "_", fit, "_", outcome, "_", strata,".csv"), row.names = F)
  
  #-------------------------------------------
  #Step7. clear the environment
  #-------------------------------------------
  
  rm(list = ls())
}

#***************************************************************************************
#II. Run the function now----------------------FOR the active analyses------------------
#***************************************************************************************
#1. Define the active analyses
active <- readr::read_rds("lib/active_analyses.rds")
active <- active[active$active==TRUE,]
active$event <- gsub("out_date_","",active$outcome_variable)
active[,c("active","outcome","outcome_variable","prior_history_var","covariates")] <- NULL

active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event","model","cohort")), 
                              names_to = "strata")

active <- active[active$value==TRUE, c("event","model","cohort","strata")]

active$model <- ifelse(active$model=="all","mdl_agesex;mdl_max_adj",active$model)
active <- tidyr::separate_rows(active, model, sep = ";")

active$cohort <- ifelse(active$cohort=="all","vaccinated;electively_unvaccinated",active$cohort)
active <- tidyr::separate_rows(active, cohort, sep = ";")

#active$group <- gsub("_.*","",active$strata)
#active$group <- ifelse(active$group=="covid" & grepl("covid_history",active$strata), "covid_history", active$group)
#active$group <- ifelse(active$group=="covid" & grepl("covid_pheno",active$strata), "covid_pheno", active$group)
#active$group <- ifelse(active$group=="prior" & grepl("prior_history",active$strata), "prior_history", active$group)
#active <- unique(active[,c("event","model","cohort","group")])

#Preprocess to right outcomes, names and order
#select outcomes
active <- active[active$event %in% c("ate", "vte") & active$model %in% c("mdl_max_adj"),]

#align the column names
colnames(active)[colnames(active) == 'group'] <- 'strata'
colnames(active)[colnames(active) == 'cohort'] <- 'group'
colnames(active)[colnames(active) == 'model'] <- 'fit'
colnames(active)[colnames(active) == 'event'] <- 'outcome'

#order the column names
active <- active %>% select(-outcome,-fit, everything())
active <- active %>% select(-strata,-outcome, everything())
active <- active %>% select(-strata, everything())
active <- active %>% select(-strata, everything())

#remove non existing HR rows
active <- active[-32,]
active <- active[-66,]
active <- active[-66,]

#2.For loop the active analysis list to the figure4 table function
for (i in 1:nrow(active)) {
  #1.run the function
  figure4_tbl(active$group[i],active$fit[i],active$outcome[i],active$strata[i])
  
  #2.compile the results
  lt_files=list.files(path = "output/review/model", pattern = "lifetable_delta_*")
  lt_files=lt_files[endsWith(lt_files,".csv")]
  lt_files=paste0("output/review/model/",lt_files)
  f4_compiled_lifetables <- purrr::pmap(list(lt_files),
                                        function(fpath){
                                          df <- fread(fpath)
                                          return(df)
                                        })
  f4_compiled_lifetables=rbindlist(f4_compiled_lifetables, fill=TRUE)
  
  #3.output the csv
  write.csv(f4_compiled_lifetables, paste0("output/review/model/Figure4_compiled_lifetables.csv"), row.names = F)}

#4.Delete un used files
if (file.exists(lt_files)) { file.remove(lt_files)}
69*196

#****************************** 
#III. Figure4 - plotting
#******************************

library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#1.Load data
lifetables <- readr::read_csv("output/review/model/Figure4_compiled_lifetables.csv")

#2.Define plotting groups and subgroups
lifetables$group <- gsub("_.*","",lifetables$subgroup)
lifetables$group <- ifelse(lifetables$group=="covid" & grepl("covid_history",lifetables$subgroup), "covid_history", lifetables$group)
lifetables$group <- ifelse(lifetables$group=="covid" & grepl("covid_pheno",lifetables$subgroup), "covid_pheno", lifetables$group)
lifetables$group <- ifelse(lifetables$group=="prior" & grepl("prior_history",lifetables$subgroup), "prior_history", lifetables$group)

#3.Shape the subgroups
table(lifetables$subgroup)#18 subgroups
lifetables$subgroup<-factor(lifetables$subgroup,
                            levels = c('agegp_18_39','agegp_40_59', 'agegp_60_79', 'agegp_80_110',
                                       'sex_Male', 'sex_Female',
                                       'ethnicity_White','ethnicity_Black', 'ethnicity_South_Asian', 'ethnicity_Mixed', 'ethnicity_Other', 'ethnicity_Missing',
                                       'covid_pheno_hospitalised','covid_pheno_non_hospitalised',
                                       'prior_history_FALSE','prior_history_TRUE',
                                       'covid_history',
                                       'main'))
str(lifetables$subgroup)

#4. Define active plotting lines
active <- readr::read_rds("lib/active_analyses.rds")
active <- active[active$active==TRUE,]
active$event <- gsub("out_date_","",active$outcome_variable)
active[,c("active","outcome","outcome_variable","prior_history_var","covariates")] <- NULL

active <- tidyr::pivot_longer(active, 
                              cols = setdiff(colnames(active),c("event","model","cohort")), 
                              names_to = "strata")

active <- active[active$value==TRUE, c("event","model","cohort","strata")]

active$model <- ifelse(active$model=="all","mdl_agesex;mdl_max_adj",active$model)
active <- tidyr::separate_rows(active, model, sep = ";")

active$cohort <- ifelse(active$cohort=="all","vaccinated;electively_unvaccinated",active$cohort)
active <- tidyr::separate_rows(active, cohort, sep = ";")

active$group <- gsub("_.*","",active$strata)
active$group <- ifelse(active$group=="covid" & grepl("covid_history",active$strata), "covid_history", active$group)
active$group <- ifelse(active$group=="covid" & grepl("covid_pheno",active$strata), "covid_pheno", active$group)
active$group <- ifelse(active$group=="prior" & grepl("prior_history",active$strata), "prior_history", active$group)
active <- active[active$event %in% c("ate", "vte") & active$model %in% c("mdl_max_adj"),]
active <- unique(active[,c("event", "strata", "model","group")])

#5.For loop to create outputs ----------14 figures-------------------------

for (i in 1:nrow(active)) {
  plot <- subset(lifetables, lifetables$event == active$event[i] & lifetables$group == active$group[i] )

  p_line<-ggplot(plot,
                 aes(x=days,y=AERp,colour=subgroup)) + facet_grid(~cohort)+
    geom_line(aes(linetype=subgroup, colour=subgroup), size=1)+
    scale_linetype_manual(values = c(rep("solid", 18)))+
    #scale_color_manual(values = c(brewer.pal(18, "Set3")))+
    #scale_linetype_manual(values = c('solid','twodash','dotted','dashed','dotdash'))+
    geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
    scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200),limits = c(0,200))+
    #scale_y_continuous(limits = c(0,2))+
    labs(x='Weeks since COVID-19 diagnosis',y='Cumulative difference in absolute risk  (%)',
         title = active$event[i])+
    theme(plot.title = element_text(hjust = 0.5))+
    theme_bw()+
    theme(legend.key.size = unit(1.2,'cm'), legend.key = element_blank())+
    labs(color='Subgroup',linetype='Subgroup')
  p_line
  
  ggsave(p_line, filename = paste0("output/Figure4_delta_", active$group[i], "_", active$event[i],".png"), dpi=300,width = 10,height = 6)
  }
  
  #Find 14 .png figures in the output folder, each for Vaccinated vs Un-vaccinated panels of each subgroups of 'ate' and 'vte'.



