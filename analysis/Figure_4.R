#Project:Vaccinated delta wave population study
#Branch:Figure4_graphical plotting of the estimated AER of ATE and VTE 
#Scripts: Renin Toms

#group <- "vaccinated"
#fit <- "mdl_max_adj"
#outcome <- "ate"
#strata <- "sex_Female"

#******************************************************
#I. Create the function to out put the figure 4_tables
#******************************************************
#*
figure4_tbl <- function(group, fit, outcome, strata){
  
  library(purrr)
  library(data.table)
  library(tidyverse)
  
  #Import data 
  input1 <- readr::read_csv("output/input1_aer.csv") #1.person days
  #input2 <- readr::read_csv("output/input2_aer.csv") #2.unexposed events, 3.total cases, 4.hr
  #input2- Import data
  hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")
  hr_files=hr_files[endsWith(hr_files,".csv")]
  hr_files=paste0("output/",hr_files)
  input2 <- purrr::pmap(list(hr_files),
                        function(fpath){
                          df <- fread(fpath)
                          return(df)
                        })
  input2=rbindlist(input2, fill=TRUE)
  
  #Preprocess the input data
  input2 <- input2 %>% select(-conf.low, -conf.high, -std.error,-robust.se, -P, -covariates_removed, -cat_covars_collapsed)
  input2 <- input2 %>% filter(term == "days0_14" |
                                term == "days14_28" |
                                term == "days28_56" |
                                term == "days56_84" |
                                term == "days84_197"|
                                term == "days0_28"|
                                term == "days28_197")
  input1$strata[input1$strata =="sex_M"] <- "sex_Male"
  input1$strata[input1$strata =="sex_F"] <- "sex_Female"
  
  input1$strata[input1$strata =="ethnicity_1"] <- "ethnicity_White"
  input1$strata[input1$strata =="ethnicity_2"] <- "ethnicity_Black"
  input1$strata[input1$strata =="ethnicity_3"] <- "ethnicity_South_Asian"
  input1$strata[input1$strata =="ethnicity_4"] <- "ethnicity_Mixed"
  input1$strata[input1$strata =="ethnicity_5"] <- "ethnicity_Other"
  input1$strata[input1$strata =="ethnicity_6"] <- "ethnicity_Missing"
  
  input1$strata[input1$strata =="prior_history_true"] <- "prior_history_TRUE"
  input1$strata[input1$strata =="prior_history_false"] <- "prior_history_FALSE"
  
  #---------------------------------
  # Step1: Extract the required variables
  #---------------------------------
  #1. Person days
  fp_person_days <- input1[input1$event == outcome & input1$model == fit  &
                             input1$cohort == group & input1$strata == strata,]$person_days
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
  lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 197 & is.na(lifetable$h), rep(hr28_196),lifetable$h)
  #alternative for 28-196 days
  
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
  #reconfirm - RT
  lifetable$CI <- 1.96*lifetable$qh*lifetable$'1-qh'*(1/fp_person_days + 1/fp_person_days)
  
  #3.AER%
  lifetable$AERp <-lifetable$'s-sc'*100
  
  #CI of AER%
  #95% CI = ARP +/- ARP x (C.I. range from the attributable risk / the attributable risk)
  #Where, ARP=AERp, CI range= CI, attributable risk = s-sc
  lifetable$CIp <- lifetable$AERp*(lifetable$CI / lifetable$`s-sc`)
  lifetable$CIp.low <- lifetable$AERp - lifetable$CIp
  lifetable$CIp.high <- lifetable$AERp + lifetable$CIp
  #-------------------------------------------
  #Step6. Output1 the csv
  #-------------------------------------------
  
  write.csv(lifetable, paste0("output/lifetable_delta_" , group, "_", fit, "_", outcome, "_", strata,".csv"), row.names = F)
  
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

#For loop the active analysis list to the figure4 table function

for (i in 1:nrow(active)) {
  
  #1.run the function
  figure4_tbl(active$group[i],active$fit[i],active$outcome[i],active$strata[i])
  
  #2.compile the results
  lt_files=list.files(path = "output", pattern = "lifetable_delta_*")
  lt_files=lt_files[endsWith(lt_files,".csv")]
  lt_files=paste0("output/",lt_files)
  f4_compiled_lifetables <- purrr::pmap(list(lt_files),
                                        function(fpath){
                                          df <- fread(fpath)
                                          return(df)
                                        })
  f4_compiled_lifetables=rbindlist(f4_compiled_lifetables, fill=TRUE)
  
  #3.output the csv
  write.csv(f4_compiled_lifetables, paste0("output/Figure4_compiled_lifetables.csv"), row.names = F)
}

#4.Delete un using file if exists
if (file.exists(lt_files)) { file.remove(lt_files)}
69*196

#******************************START FROM HERE 
#III. Figure4 - plotting
#******************************
   figure4_plot <- function(group, fit, outcome, strata) {
    
  library(magrittr)
  library(dplyr)
  library(ggplot2)
    
  #Gather the lifetable CSV.s
  hr_files=list.files(path = "output", pattern = "lifetable_delta_*")
  hr_files=hr_files[endsWith(hr_files,".csv")]
  hr_files=paste0("output/",hr_files)
  lifetables <- purrr::pmap(list(hr_files),
                        function(fpath){ df <- fread(fpath)
                          return(df)})
  lifetables=rbindlist(lifetables, fill=TRUE)
  
  
  lifetables$subgroup <-factor(lifetables$subgroup,levels = c('sex_Male','sex_Female'))
  
  p_line<-ggplot(lifetable,
                 aes(x=days,
                     y=AERp,
                     group=1)) +
    #geom_errorbar(aes(ymin=incidence_rate_difference_LB, ymax=incidence_rate_difference_UB), width=.2,
    #              position=position_dodge(.9))+
    geom_line(size=1, col = 'red')+
    geom_ribbon(aes(ymin = CIp.low, ymax = CIp.high), alpha = 0.1)+
    #geom_point()+
    scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200),limits = c(0,200))+
    scale_y_continuous(breaks = c(-2. -1.5, -1, -0.5, 0, .5, 1, 1.5, 2))+
    labs(x='days since COVID-19 diagnosis',y='Cumulative difference in absolute risk  (%)',
         title = 'Arterial Thrombotic Events')+
    theme(plot.title = element_text(hjust = 0.5))
  
  p_line
  #RT - to add plot saving file location 
  
  }
  
  
}
#------------------RUN the function-------------------------------------------------------------------------------------
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
active <- unique(active[,c("event","model","cohort","group")])

active <- active[active$event %in% c("ate", "vte") & active$model %in% c("mdl_max_adj"),]
colnames(active)[colnames(active) == 'group'] <- 'strata'
colnames(active)[colnames(active) == 'cohort'] <- 'group'
colnames(active)[colnames(active) == 'model'] <- 'fit'
colnames(active)[colnames(active) == 'event'] <- 'outcome'

group <- "vaccinated"
fit <- "mdl_max_adj"
outcome <- "ate"
strata <- "sex_Female"


for (i in 1:nrow(active)) {

  files <- list.files(path = "output", pattern = "lifetable_*")
  files <- files[grepl(active$group[i])]
  
  figure4_tbl(active$cohort[i],active$model[i],active$event[i],active$group[i])
  
}

#group <- "vaccinated"
#fit <- "mdl_max_adj"
#outcome <- "vte"
#stratas <- c("main", 
             #"sex_Male", "sex_Female",
             #"agegp_18_39", "agegp_40_59", "agegp_60_79", "agegp_80_110",
            # "ethnicity_South_Asian", "ethnicity_Black", "ethnicity_White", "ethnicity_Mixed","ethnicity_Other", "ethnicity_Missing",
            # "prior_history_FALSE", "prior_history_TRUE", "covid_pheno_hospitalised", "covid_pheno_non_hospitalised",
             #"covid_history")

#Call the figure4 table function
figure4_tables(group, fit, outcome, strata)

#run for active analysis

figure4(group, fit, outcome, strata)

for (strata in stratas) { figure4("vaccinated","mdl_max_adj","ate", sex_Male)}

#output life tables
#subgroups = 18
figure4("vaccinated","mdl_max_adj","ate", stratas)

figure4("vaccinated","mdl_max_adj","ate", "sex_Male")
figure4("vaccinated","mdl_max_adj","ate", "sex_Female")

figure4("vaccinated","mdl_max_adj","ate", "agegp_18_39")
figure4("vaccinated","mdl_max_adj","ate", "agegp_40_59")
figure4("vaccinated","mdl_max_adj","ate", "agegp_60_79")
figure4("vaccinated","mdl_max_adj","ate", "agegp_80_110")

figure4("vaccinated","mdl_max_adj","ate", "ethnicity_South_Asian")
figure4("vaccinated","mdl_max_adj","ate", "ethnicity_Black")
figure4("vaccinated","mdl_max_adj","ate", "ethnicity_White")
figure4("vaccinated","mdl_max_adj","ate", "ethnicity_Mixed")
figure4("vaccinated","mdl_max_adj","ate", "ethnicity_Other")
figure4("vaccinated","mdl_max_adj","ate", "ethnicity_Missing")

figure4("vaccinated","mdl_max_adj","ate", "prior_history_FALSE")
figure4("vaccinated","mdl_max_adj","ate", "prior_history_TRUE")

figure4("vaccinated","mdl_max_adj","ate", "covid_pheno_hospitalised")
figure4("vaccinated","mdl_max_adj","ate", "covid_pheno_non_hospitalised")

figure4("vaccinated","mdl_max_adj","ate", "covid_history")

#----------------------outcomes = 2
figure4("vaccinated","mdl_max_adj","vte", "main")

figure4("vaccinated","mdl_max_adj","vte", "sex_Male")
figure4("vaccinated","mdl_max_adj","vte", "sex_Female")

figure4("vaccinated","mdl_max_adj","vte", "agegp_18_39")
figure4("vaccinated","mdl_max_adj","vte", "agegp_40_59")
figure4("vaccinated","mdl_max_adj","vte", "agegp_60_79")
figure4("vaccinated","mdl_max_adj","vte", "agegp_80_110")

figure4("vaccinated","mdl_max_adj","vte", "ethnicity_South_Asian")
figure4("vaccinated","mdl_max_adj","vte", "ethnicity_Black")
figure4("vaccinated","mdl_max_adj","vte", "ethnicity_White")
figure4("vaccinated","mdl_max_adj","vte", "ethnicity_Mixed")
figure4("vaccinated","mdl_max_adj","vte", "ethnicity_Other")
figure4("vaccinated","mdl_max_adj","vte", "ethnicity_Missing")

figure4("vaccinated","mdl_max_adj","vte", "prior_history_FALSE")
figure4("vaccinated","mdl_max_adj","vte", "prior_history_TRUE")

figure4("vaccinated","mdl_max_adj","vte", "covid_pheno_hospitalised")
figure4("vaccinated","mdl_max_adj","vte", "covid_pheno_non_hospitalised")

figure4("vaccinated","mdl_max_adj","vte", "covid_history")





table(input2$subgroup)

figure4("ate","vaccinated","main","mdl_agesex")

#subgroups =2
figure4("ate","vaccinated","main","mdl_max_adj")
figure4("ate","vaccinated","main","mdl_agesex")


figure4("ate","electively_unvaccinated","main","mdl_max_adj")

figure4("vte","vaccinated","main","mdl_max_adj")



##########################
#plotting
######life table##########
plot(Figure_4$days, Figure_4$AERp_main)

p_line<-ggplot(Figure_4,
               aes(x=days,
                   y=AERp_main,
                   group=1)) +
  #geom_errorbar(aes(ymin=incidence_rate_difference_LB, ymax=incidence_rate_difference_UB), width=.2,
  #              position=position_dodge(.9))+
  geom_line(size=1.5)+
  #geom_point()+
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200),limits = c(0,200))+
  scale_y_continuous(limits = c(0,2))+
  labs(x='days since COVID-19 diagnosis',y='Cumulative difference in absolute risk  (%)',
       title = 'ATE')+
  theme(plot.title = element_text(hjust = 0.5))

p_line
