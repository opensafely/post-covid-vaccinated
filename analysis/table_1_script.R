#Table 1 - Covariate counts overall, and stratified by exposure status
#Creates a table with the columns covariate, covariate level, cov_freq_all and cov_freq_covid. This table counts the number of people at each covariate level for both
#the whole population and those exposed to covid
#Currently cov_num_age in dummy data has ages below 18
#need to check for numeric covariates what values are wanted
library(dplyr)
library(data.table)
library(tidyverse)
library(readr)

#Read in data-------------------------------------------------------------------
input=read_rds(file = "output/input.rds")
input$cov_cat_region=gsub(" ","_",input$cov_cat_region)#This has been done in an earlier script so can be deleted once merged

input$exp_cat_covid19_hospital[input$exp_cat_covid19_hospital=="hospitalised_after28days"]="non_hospitalised"
input$exp_cat_covid19_hospital[input$exp_cat_covid19_hospital=="hospitalised_within28days"]="hospitalised"

#Define populations of interest
pop <- data.frame(rbind(c("Whole_population","!is.na(input$patient_id)"),
                        c("COVID_exposed","is.na(input$exp_date_covid19_confirmed)==F"),
                        c("COVID_hospitalised","input$exp_cat_covid19_hospital=='hospitalised'"),
                        c("COVID_non_hospitalised","input$exp_cat_covid19_hospital=='non_hospitalised'")
),
stringsAsFactors = FALSE)

colnames(pop) <- c("name","condition")


#Create categories for age, deprivation and No. primary care consultationinput = input %>% 
input=input%>%mutate(cov_cat_age_group = 
                       case_when((cov_num_age>=18 & cov_num_age<=29) ~ "18-29",
                                 (cov_num_age>=30 & cov_num_age<=39) ~ "30-39",
                                 (cov_num_age>=40 & cov_num_age<=49) ~ "40-49",
                                 (cov_num_age>=50 & cov_num_age<=59) ~ "50-59",
                                 (cov_num_age>=60 & cov_num_age<=69) ~ "60-69",
                                 (cov_num_age>=70 & cov_num_age<=79) ~ "70-79",
                                 (cov_num_age>=80 & cov_num_age<=89) ~ "80-89",
                                 (cov_num_age>=90) ~ "90+",
                                 TRUE ~ "other"
                       ))

#I believe this is done in an earlier script so can be deleted once merged
input = input %>% 
  mutate(cov_cat_deprivation_group = 
           case_when((cov_cat_deprivation==1 | cov_cat_deprivation==2) ~ "1-2 (most deprived)",
                     (cov_cat_deprivation==3 | cov_cat_deprivation==4) ~ "3-4",
                     (cov_cat_deprivation==5 | cov_cat_deprivation==6) ~ "5-6",
                     (cov_cat_deprivation==7 | cov_cat_deprivation==8) ~ "7-8",
                     (cov_cat_deprivation==9 | cov_cat_deprivation==10) ~ "9-10 (least deprived)",
                     TRUE ~ "other"
           ))

input = input %>% 
  mutate(cov_cat_consulation_rate_group = 
           case_when((cov_num_consulation_rate==0) ~ "0",
                     (cov_num_consulation_rate>=1 & cov_num_consulation_rate<=5) ~ "1-6",
                     (cov_num_consulation_rate>=6) ~ "6+",
                     TRUE ~ "other"
           ))




# Populate table 1 -------------------------------------------------------------
categorical_cov=colnames(input)[grep("cov_cat", colnames(input))]
categorical_cov=categorical_cov[!categorical_cov=="cov_cat_deprivation"]#will need to be removed if deprivation is grouped in an earlier script

numerical_cov=colnames(input)[grep("cov_num", colnames(input))]
numerical_cov=numerical_cov[!numerical_cov=="cov_num_age"]

binary_cov=colnames(input)[grep("cov_bin", colnames(input))]


#Base table
table1=input%>% dplyr::select(c(categorical_cov,numerical_cov,binary_cov))
table1= table1 %>% mutate_if(is.character,as.factor)
table1= table1 %>% mutate_if(is.logical,as.factor)
table1=as.data.frame(summary(table1,maxsum=50))
table1=rename(table1, Covariate_level = Freq, Covariate = Var2)
table1 <- table1 %>% filter(!str_detect(Covariate_level, "^FALSE"))

table1=table1%>% dplyr::select("Covariate","Covariate_level")
table1$Covariate=gsub("\\s","",table1$Covariate)#Remove spaces
table1$Covariate_level= sub('\\:.*', '', table1$Covariate_level)#Remove everything after :
table1=table1%>%filter(!(Covariate == "cov_num_consulation_rate" & !Covariate_level=="Mean   "))


for (j in 1:nrow(pop)) {
  
  population <- pop[j,]$name
  
  df <- subset(input, eval(parse(text = pop[j,]$condition)))
  
  #Count population size
  pop_summary=data.frame(matrix(ncol=3))
  colnames(pop_summary)=c("Covariate","Covariate_level",population)
  pop_summary[1,]=c("All","All",nrow(df))
  
  #Categorical covariates
  cat_pop=df %>% dplyr::select(categorical_cov)
  cat_pop= cat_pop %>% mutate_if(is.character,as.factor)
  cat_summary=as.data.frame(summary(cat_pop,maxsum=50))
  cat_summary[,population]=cat_summary$Freq
  cat_summary=rename(cat_summary, Covariate_level = Freq, Covariate = Var2)
  cat_summary=cat_summary%>% dplyr::select("Covariate","Covariate_level",population)
  
  
  #Numerical covariates
  num_pop=df %>% dplyr::select(numerical_cov)
  num_summary=as.data.frame(summary(num_pop,maxsum=50))
  num_summary[,population]=num_summary$Freq
  num_summary=rename(num_summary, Covariate_level = Freq, Covariate = Var2)
  num_summary=num_summary%>% dplyr::select("Covariate","Covariate_level",population)
  num_summary=num_summary %>%filter(startsWith(num_summary$Covariate_level, "Mean")==T)
  
  #Binary covariates
  bin_pop=df %>% dplyr::select(binary_cov)
  bin_pop= bin_pop %>% mutate_if(is.logical,as.factor)
  bin_summary=as.data.frame(summary(bin_pop,maxsum=50))
  bin_summary[,population]=bin_summary$Freq
  bin_summary=rename(bin_summary, Covariate_level = Freq, Covariate = Var2)
  bin_summary <- bin_summary %>% filter(str_detect(Covariate_level, "^TRUE"))
  bin_summary=bin_summary%>% dplyr::select("Covariate","Covariate_level",population)
  
  #Population summary
  pop_summary=rbind(pop_summary,cat_summary,num_summary,bin_summary)
  
  #Tidy summary table
  pop_summary$Covariate=gsub("\\s","",pop_summary$Covariate)#Remove spaces
  pop_summary$Covariate_level= sub('\\:.*', '', pop_summary$Covariate_level)#Remove everything after :
  #pop_summary$Covariate_level=gsub("\\s","",pop_summary$Covariate_level)#Remove spaces
  
  pop_summary[,population]=gsub(".*:", "",pop_summary[,population])#Remove everything before:
  pop_summary=pop_summary %>% drop_na(Covariate_level)#Remove rows with NA
  
  #Left join onto base table
  table1=left_join(table1,pop_summary, by=c("Covariate","Covariate_level"))
}


#Tidy table 1
table1$Covariate=gsub("cov_bin_", "History of ",table1$Covariate)
table1$Covariate=gsub("cov_\\D\\D\\D_", "",table1$Covariate)
table1$Covariate=gsub("_", " ",table1$Covariate)
table1=table1%>%filter(!Covariate_level %in% c("Min.","1st Qu.","Median"))

write.csv(table1, file = file.path("output", paste0("Table_1", ".csv")) , row.names=F)