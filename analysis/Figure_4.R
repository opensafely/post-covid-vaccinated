#Project:Vaccinated delta wave population study
#Branch:Figure4_graphical plotting of the estimated AER of ATE and VTE 
#Scripts: Renin Toms

group <- "vaccinated"
fit <- "mdl_max_adj"
outcome <- "vte"
strata <- "sex_Male"

figure4 <- function(group, fit, outcome, strata){
  
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
  
  #Preprocess the AER input data
  input2 <- input2 %>% select(-std.error, -robust.se, -P, -covariates_removed, -cat_covars_collapsed)
  input2 <- input2 %>% filter(term == "days0_14" |
                                term == "days14_28" |
                                term == "days28_56" |
                                term == "days56_84" |
                                term == "days84_197"|
                                term == "days0_28"|
                                term == "days28_197")
  input1$strata[input1$strata =="sex_M"] <- "sex_Male"
  input1$strata[input1$strata =="sex_F"] <- "sex_Female"
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
  
  #5.locate CI of the estimates
  #0-14 days
  hr_14_low <- input2[input2$event == outcome  & input2$model == fit  & 
                          input2$cohort == group & input2$subgroup == strata& input2$term == "days0_14",]$conf.low
  hr_14_high <- input2[input2$event == outcome  & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata& input2$term == "days0_14",]$conf.high
  #14-28 days
  hr_28_low <- input2[input2$event == outcome & input2$model == fit  & 
                          input2$cohort == group & input2$subgroup == strata& input2$term == "days14_28",]$conf.low
  hr_28_high <- input2[input2$event == outcome & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata& input2$term == "days14_28",]$conf.high
  #28-56 days
  hr_56_low <- input2[input2$event == outcome & input2$model == fit  & 
                          input2$cohort == group & input2$subgroup == strata& input2$term == "days28_56",]$conf.low
  hr_56_high <- input2[input2$event == outcome & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata& input2$term == "days28_56",]$conf.high
  #56-84 days
  hr_84_low <- input2[input2$event == outcome & input2$model == fit  & 
                          input2$cohort == group & input2$subgroup == strata& input2$term == "days56_84",]$conf.low
  hr_84_high <- input2[input2$event == outcome & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata& input2$term == "days56_84",]$conf.high
  #84-196 days
  hr_196_low <- input2[input2$event == outcome & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata& input2$term == "days84_197",]$conf.low
  hr_196_high <- input2[input2$event == outcome & input2$model == fit  & 
                            input2$cohort == group & input2$subgroup == strata& input2$term == "days84_197",]$conf.high
  #Alternative 0-28 days
  hr0_28_low <- input2[input2$event == outcome  & input2$model == fit  & 
                           input2$cohort == group & input2$subgroup == strata& input2$term == "days0_28",]$conf.low
  hr0_28_high <- input2[input2$event == outcome  & input2$model == fit  & 
                            input2$cohort == group & input2$subgroup == strata& input2$term == "days0_28",]$conf.high
  #Alternative 28 - 196 days
  hr28_196_low <- input2[input2$event == outcome  & input2$model == fit  & 
                             input2$cohort == group & input2$subgroup == strata& input2$term == "days28_197",]$conf.low
  hr28_196_high <- input2[input2$event == outcome  & input2$model == fit  & 
                              input2$cohort == group & input2$subgroup == strata& input2$term == "days28_197",]$conf.high
  #Alternative 28 - 196 days
  hr28_196<- input2[input2$event == outcome  & input2$model == fit  & 
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
  
  #2.assign the CI low
  lifetable$h.low <- ifelse(lifetable$days < 15, rep(hr_14_low),0)
  lifetable$h.low <- ifelse(lifetable$days > 14 & lifetable$days < 29, rep(hr_28_low),lifetable$h.low)
  lifetable$h.low <- ifelse(lifetable$days < 29 & is.na(lifetable$h.low), rep(hr0_28_low),lifetable$h.low)#alternative for 0-28 days
  
  lifetable$h.low <- ifelse(lifetable$days > 28 & lifetable$days < 57, rep(hr_56_low),lifetable$h.low)
  lifetable$h.low <- ifelse(lifetable$days > 56 & lifetable$days < 85, rep(hr_84_low),lifetable$h.low)
  lifetable$h.low <- ifelse(lifetable$days > 84 & lifetable$days < 197, rep(hr_196_low),lifetable$h.low)
  lifetable$h.low <- ifelse(lifetable$days > 28 & lifetable$days < 197 & is.na(lifetable$h.low), rep(hr28_196_low),lifetable$h.low)#alternative for 28-196 days
  
  #2.assign the CI high
  lifetable$h.high <- ifelse(lifetable$days < 15, rep(hr_14_high),0)
  lifetable$h.high <- ifelse(lifetable$days > 14 & lifetable$days < 29, rep(hr_28_high),lifetable$h.high)
  lifetable$h.high <- ifelse(lifetable$days < 29 & is.na(lifetable$h.high), rep(hr0_28_high),lifetable$h.high)#alternative for 0-28 days
  
  lifetable$h.high <- ifelse(lifetable$days > 28 & lifetable$days < 57, rep(hr_56_high),lifetable$h.high)
  lifetable$h.high <- ifelse(lifetable$days > 56 & lifetable$days < 85, rep(hr_84_high),lifetable$h.high)
  lifetable$h.high <- ifelse(lifetable$days > 84 & lifetable$days < 197, rep(hr_196_high),lifetable$h.high)
  lifetable$h.high <- ifelse(lifetable$days > 28 & lifetable$days < 197 & is.na(lifetable$h.high), rep(hr28_196_high),lifetable$h.high)#alternative for 28-196 days
  
  
  
  
  lifetable$qh <- lifetable$q*lifetable$h
  lifetable$'1-qh' <- 1 - lifetable$qh
  lifetable$sc <- cumprod(lifetable$`1-qh`)
  #-------------------------------------------
  #Step5. Calculate the Absolute excess risk--
  #-------------------------------------------
  #Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
  #compared with no COVID-19 diagnosis. 
  
  #AER = Sc-S=difference in absolute risk
  lifetable$AER <- lifetable$sc - lifetable$s # RT- reverse when real data
  lifetable$AERp <-lifetable$AER*100
  
  write.csv(lifetable, paste0("output/lifetable_" , group, "_", fit, "_", outcome, "_", strata,".csv"), row.names = F)
  
}
#------------------RUN the function-------------------------------------------------------------------------------------

#group <- "vaccinated"
#fit <- "mdl_max_adj"
#outcome <- "vte"
stratas <- c("main", 
             "sex_Male"), "sex_Female",
             "agegp_18_39", "agegp_40_59", "agegp_60_79", "agegp_80_110",
             "ethnicity_South_Asian", "ethnicity_Black", "ethnicity_White", "ethnicity_Mixed","ethnicity_Other", "ethnicity_Missing",
             "prior_history_FALSE", "prior_history_TRUE", "covid_pheno_hospitalised", "covid_pheno_non_hospitalised",
             "covid_history")


for (strata in stratas) { figure4("vaccinated","mdl_max_adj","ate", strata)}

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
