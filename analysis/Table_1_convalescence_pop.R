#Table 1 - Covariate counts overall, and stratified by exposure status
#Currently cov_age in dummy data has ages below 18, deprivation =0 and ethnicity =0
library(dplyr)
library(data.table)


#Read in data
input <- fread("OpenSafely - unvaccinated analysis/input.csv" ,na.strings=c("","NA"))

#Remove space from cov_region - might already be done in preparing covariates script
input$cov_region= sub(' ', '_', input$cov_region)


#Create groups for age and deprivation 
input = input %>% 
  mutate(cov_age_group = 
           case_when((cov_age>=18 & cov_age<=29) ~ "18-29",
                     (cov_age>=30 & cov_age<=39) ~ "30-39",
                     (cov_age>=40 & cov_age<=49) ~ "40-49",
                     (cov_age>=50 & cov_age<=59) ~ "50-59",
                     (cov_age>=60 & cov_age<=69) ~ "60-69",
                     (cov_age>=70 & cov_age<=79) ~ "70-79",
                     (cov_age>=80 & cov_age<=89) ~ "80-89",
                     (cov_age>=90) ~ "90+"
                     ))

input = input %>% 
  mutate(cov_deprivation_group = 
           case_when((cov_deprivation==1 | cov_deprivation==2) ~ "1-2 (most deprived)",
                     (cov_deprivation==3 | cov_deprivation==4) ~ "3-4",
                     (cov_deprivation==5 | cov_deprivation==6) ~ "5-6",
                     (cov_deprivation==7 | cov_deprivation==8) ~ "7-8",
                     (cov_deprivation==9 | cov_deprivation==10) ~ "9-10 (least deprived)"
           ))



#Create vector of covariate names
covar_names <- input %>% dplyr::select(c(names(input)[grepl("cov_", names(input))])) %>% names()
covar_names=covar_names[! covar_names %in% c('cov_age','cov_deprivation')]

#Whole population dataset
whole_pop=input %>% dplyr::select(c(covar_names))#select relevant columns
whole_pop= whole_pop %>% mutate_if(is.numeric,as.factor)#convert all variables to factor
whole_pop= whole_pop %>% mutate_if(is.character,as.factor)#convert all variables to factor
whole_pop=as.data.frame(summary(whole_pop,maxsum=50))
whole_pop = rename(whole_pop, Covariate_level = Freq, Covariate = Var2)
whole_pop$Cov_frequency_all=whole_pop$Covariate_level


whole_pop$Covariate_level= sub('\\:.*', '', whole_pop$Covariate_level)#Remove everything after :
whole_pop$Covariate_level=gsub("\\s","",whole_pop$Covariate_level)#Remove spaces
whole_pop$Covariate=gsub("\\s","",whole_pop$Covariate)#Remove spaces
whole_pop$Cov_frequency_all=gsub(".*:", "",whole_pop$Cov_frequency_all)#Remove everything before:
whole_pop=whole_pop %>% drop_na(Covariate_level)#Remove rows with NA
whole_pop$Cov_frequency_all=as.numeric(whole_pop$Cov_frequency_all)
whole_pop=whole_pop%>%select(Covariate,Covariate_level,Cov_frequency_all)


#Covid exposed population
exposed_pop=input %>% filter(is.na(exp_confirmed_covid19_date)==F)%>%dplyr::select(c(covar_names))#select relevant columns
exposed_pop= exposed_pop %>% mutate_if(is.numeric,as.factor)#convert all variables to factor
exposed_pop= exposed_pop %>% mutate_if(is.character,as.factor)#convert all variables to factor
exposed_pop=as.data.frame(summary(exposed_pop,maxsum=50))
exposed_pop = rename(exposed_pop, Covariate_level = Freq, Covariate = Var2)
exposed_pop$Cov_frequency_covid=exposed_pop$Covariate_level


exposed_pop$Covariate_level= sub('\\:.*', '', exposed_pop$Covariate_level)#Remove everything after :
exposed_pop$Covariate_level=gsub("\\s","",exposed_pop$Covariate_level)#Remove spaces
exposed_pop$Covariate=gsub("\\s","",exposed_pop$Covariate)#Remove spaces
exposed_pop$Cov_frequency_covid=gsub(".*:", "",exposed_pop$Cov_frequency_covid)#Remove everything before:
exposed_pop=exposed_pop %>% drop_na(Covariate_level)#Remove rows with NA
exposed_pop$Cov_frequency_covid=as.numeric(exposed_pop$Cov_frequency_covid)
exposed_pop=exposed_pop%>%select(Covariate,Covariate_level,Cov_frequency_covid)

#Left join exposed population onto whole population
whole_pop=left_join(whole_pop,exposed_pop, by=c("Covariate","Covariate_level"))
whole_pop["Cov_frequency_all"][is.na(whole_pop["Cov_frequency_all"])] <- 0#change any NAs to 0 (shouldb't be any NAs with whole population)
whole_pop["Cov_frequency_covid"][is.na(whole_pop["Cov_frequency_covid"])] <- 0

#Add counts for all
count_all=whole_pop%>%filter(Covariate=="cov_sex")
count_all$Cov_frequency_all=sum(count_all$Cov_frequency_all)
count_all$Cov_frequency_covid=sum(count_all$Cov_frequency_covid)
count_all=count_all[1,]
count_all$Covariate="All"
count_all$Covariate_level="All"
whole_pop=rbind(count_all,whole_pop)

#write.csv(whole_pop, file = file.path("output", paste0("Table_1_general_pop", ".csv")) , row.names=F)



