#Project:Vaccinated delta wave population study
#Branch:Figure4_graphical plotting of the estimated AER of ATE and VTE 
#Scripts: Renin Toms

library(purrr)
library(data.table)
library(tidyverse)

#Import data 
input1 <- readr::read_csv("output/input1_aer.csv") #1.person days
#input2 <- readr::read_csv("output/input2_aer.csv") #2.unexposed events, 3.total cases, 4.hr
#Import data
hr_files=list.files(path = "output", pattern = "compiled_HR_results_*")
hr_files=paste0("output/",hr_files)
hr_file_paths <- purrr::pmap(list(hr_files),
                             function(fpath){
                               df <- fread(fpath)
                               return(df)
                             })
df=rbindlist(hr_file_paths, fill=TRUE)

#Preprocess the AER input data
input2 <- subset(df, df$term == "days0_14" |
                   df$term == "days14_28" |
                   df$term == "days28_56" |
                   df$term == "days56_84" |
                   df$term == "days84_197"|
                   df$term == "days0_28"|
                   df$term == "days28_197") # RT/RK check
input2 <- input2 %>% select(-conf.low, -conf.high, -std.error, -robust.se, -P, -covariates_removed, -cat_covars_collapsed)

#limit to ATE & VTE outcomes
input2 <- subset(input2, input2$event == "ate" | input2$event == "vte")
#---------------------------------
# Step1: Extract the required variables
#---------------------------------
#1. Person days
fp_person_days <- input1[input1$event == "ate" & input1$model == "mdl_max_adj" &
                           input1$cohort == "vaccinated" & input1$strata == "main",]$person_days#RT/RK/VW -check L

#2.unexposed events
unexposed_events <-  input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                              input2$cohort == "vaccinated" & input2$subgroup == "main" & 
                              input2$expo_week== "pre expo",]$events_total

#3.Total cases
total_cases <-  input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                         input2$cohort == "vaccinated" & input2$subgroup == "main" & 
                         input2$expo_week== "pre expo",]$total_covid19_cases

#4.locate the estimates
#0-14 days
hr_14 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                  input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days0_14",]$estimate
#14-28 days
hr_28 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                  input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days14_28",]$estimate
#28-56 days
hr_56 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                  input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days28_56",]$estimate
#56-84 days
hr_84 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                  input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days56_84",]$estimate
#84-196 days
hr_196 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                   input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days84_197",]$estimate
#Alternative 0-28 days
hr0_28 <- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                   input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days0_28",]$estimate
#Alternative 28 - 196 days
hr28_196<- input2[input2$event == "ate" & input2$model == "mdl_max_adj" & 
                    input2$cohort == "vaccinated" & input2$subgroup == "main"& input2$term == "days28_196",]$estimate

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
lifetable$event <- "ate"
lifetable$model <- "mdl_max_adj"
lifetable$cohort <- "vaccinated"
lifetable$subgroup <- "main"

lifetable$q <- incidence_rate 
lifetable$'1-q' <- 1 - lifetable$q 
lifetable$s <- cumprod(lifetable$`1-q`)

#----------------------------------------
#Step4. Calculate the daily CVD incidence
#----------------------------------------
#Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
# for that day to derive the incidence on each day after COVID-19. 

#assign the hr estimates
lifetable$h <- ifelse(lifetable$days < 15, rep(hr_14),0)
lifetable$h <- ifelse(lifetable$days > 14 & lifetable$days < 29, rep(hr_28),lifetable$h)
lifetable$h <- ifelse(lifetable$days < 29 & is.na(lifetable$h), rep(hr0_28),lifetable$h)#alternative for 0-28 days

lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 57, rep(hr_56),lifetable$h)
lifetable$h <- ifelse(lifetable$days > 56 & lifetable$days < 85, rep(hr_84),lifetable$h)
lifetable$h <- ifelse(lifetable$days > 84 & lifetable$days < 197, rep(hr_196),lifetable$h)
lifetable$h <- ifelse(lifetable$days > 28 & lifetable$days < 197 & is.na(lifetable$h), rep(hr28_196),lifetable$h)#alternative for 28-196 days

lifetable$qh <- lifetable$q*lifetable$h
lifetable$'1-qh' <- 1 - lifetable$qh
lifetable$sc <- cumprod(lifetable$`1-qh`)
#-----------------------------------------
#Step5. Calculate the Absolute excess risk
#-----------------------------------------
#Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
#compared with no COVID-19 diagnosis. 

#AER = Sc-S=difference in absolute risk
lifetable$AER <- lifetable$sc - lifetable$s
#AER on day 196 
AER_196 <- lifetable[nrow(lifetable),]$AER * total_cases
print(AER_196) # 358.4324
# 358 excess 'events' happens 196 days after 20266 total covid19 'cases'.

##########################
#plotting
######life table##########
#convert to AER%
lifetable$AER_p <- lifetable$AER*100
plot(lifetable$days, lifetable$AER_p)

p_line<-ggplot(lifetable,
               aes(x=days,
                   y=AER_p,
                   group=1)) +
  #geom_errorbar(aes(ymin=incidence_rate_difference_LB, ymax=incidence_rate_difference_UB), width=.2,
  #              position=position_dodge(.9))+
  geom_line(size=1.5)+
  #geom_point()+
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200),limits = c(0,200))+
  scale_y_continuous(limits = c(0,2))+
  labs(x='days since COVID-19 diagnosis',y='Cumulative difference in absolute risk  (%)',
       title = 'AMI')+
  theme(plot.title = element_text(hjust = 0.5))

p_line
