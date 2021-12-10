#This script will create 
#Checking of covariates for critical numbers for separate vaccinated and unvaccinated population
library(dplyr)
library(data.table)
library(purrr)
library(readr)
library(tidyverse)

input <- read_rds("output/input.rds")
project="vaccinated_delta"
#unvaccinated_delta, unvaccinated

#done in previous script so can be deleted once merged
input$cov_cat_region=gsub(" ","_",input$cov_cat_region)
input$exp_cat_covid19_hospital[input$exp_cat_covid19_hospital=="hospitalised_after28days"]="non_hospitalised"
input$exp_cat_covid19_hospital[input$exp_cat_covid19_hospital=="hospitalised_within28days"]="hospitalised"

#Outcomes and covariates
outcome <- input %>% dplyr::select(c(names(input)[grepl("out_", names(input))])) %>% names()
covar_names=(c(colnames(input)[grepl("cov_", colnames(input))]))
covar_names <- covar_names[!covar_names %in% c("cov_num_age","cov_num_consulation_rate")]



#need to check whether all covariates are changed to factors in earlier script
factor_covars <- covar_names
mk_factor_orderlevels <- function(input, colname)
{
  input <- input %>% mutate(
    !!sym(colname) := factor(!!sym(colname), levels = str_sort(unique(input[[colname]]), numeric = TRUE)))
  return(input)
}

for (colname in factor_covars){
  print(colname)
  input <- mk_factor_orderlevels(input, colname)
}

#Set cohort start and end dates
if(endsWith(project,"_delta")==T){
  cohort_start_date=as.Date("2021-06-01")
  cohort_end_date=as.Date(Sys.Date(), format="%y-%m-%d")
}else{
  cohort_start_date=as.Date("2020-01-01")
  cohort_end_date=as.Date("2021-10-27")
}

#Populations of interest
pop <- data.frame(rbind(c("Whole_population","!is.na(input$patient_id)"),
                       c("COVID_hospitalised","input$exp_cat_covid19_hospital=='hospitalised'"),
                       c("COVID_non_hospitalised","input$exp_cat_covid19_hospital=='non_hospitalised'"),
                       c("agegp_18_39","input$cov_num_age>=18 & input$cov_num_age<40"),
                       c("agegp_40_59","input$cov_num_age>=40 & input$cov_num_age<60"),
                       c("agegp_60_79","input$cov_num_age>=60 & input$cov_num_age<80"),
                       c("agegp_80_110","input$cov_num_age>=80 & input$cov_num_age<111"),
                       c("male","input$cov_cat_sex=='M'"),
                       c("female","input$cov_cat_sex=='F'"),
                       c("ethnicity_1","input$cov_cat_ethnicity=='1'"),
                       c("ethnicity_2","input$cov_cat_ethnicity=='2'"),
                       c("ethnicity_3","input$cov_cat_ethnicity=='3'"),
                       c("ethnicity_4","input$cov_cat_ethnicity=='4'"),
                       c("ethnicity_5","input$cov_cat_ethnicity=='5'"),
                       c("ethnicity_6","input$cov_cat_ethnicity=='6'")
                       
),
stringsAsFactors = FALSE)

colnames(pop) <- c("name","condition")

#Create base table of covariate levels to join counts onto 
covar_levels=input %>% dplyr::select(c(covar_names))
covar_levels= covar_levels %>% mutate_if(is.numeric,as.factor)
covar_levels= covar_levels %>% mutate_if(is.character,as.factor)
covar_levels=as.data.frame(summary(covar_levels,maxsum=20))
covar_levels = rename(covar_levels, Covariate_level = Freq, Covariate = Var2)
covar_levels$Covariate_level= sub('\\:.*', '', covar_levels$Covariate_level)
covar_levels$Covariate_level=gsub("\\s","",covar_levels$Covariate_level)
covar_levels$Covariate=gsub("\\s","",covar_levels$Covariate)
covar_levels=covar_levels %>% drop_na(Covariate_level)
covar_levels=covar_levels%>%select(-Var1)

summary_final <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(summary_final) <- c('Covariate', 'Covariate_level', 'Pre_expo_freq', 'Post_expo_freq', 'Event','Strata','Project')




for (j in 1:nrow(pop)) {
  for(outcome_name in outcome){
    population <- pop[j,]$name
    df <- subset(input, eval(parse(text = pop[j,]$condition)))
    df=rename(df, outcome_event = outcome_name)

    if(project=="vaccinated_delta"){
      df=df %>% rowwise() %>% mutate(follow_up_start=max((vax_date_covid_2+14),cohort_start_date,na.rm = TRUE))
      df=df %>% rowwise() %>% mutate(follow_up_end=min(outcome_event, death_date,cohort_end_date,na.rm = TRUE))
    }else if (project=="unvaccinated_delta"){
      df=df %>% rowwise() %>% mutate(follow_up_start=max((vax_date_eligible+72),cohort_start_date,na.rm = TRUE))
      df=df %>% rowwise() %>% mutate(follow_up_end=min(vax_date_covid_1,outcome_event, death_date,cohort_end_date,na.rm = TRUE))
    }else if (project == "unvaccinated"){
      df$follow_up_start=cohort_start_date
      df=df %>% rowwise() %>% mutate(follow_up_end=min(outcome_event, vax_date_covid_1, death_date,cohort_end_date,na.rm = TRUE))
    }
    
    df_pre_expo= df %>% filter((follow_up_end == outcome_event & follow_up_end >= follow_up_start) & (follow_up_end < exp_date_covid19_confirmed | is.na(exp_date_covid19_confirmed)=="TRUE"))
    df_pre_expo <- df_pre_expo %>% dplyr::select(c(all_of(covar_names)))
    #df_pre_expo <- df_pre_expo %>%  dplyr::select(!c( df_pre_expo %>%  dplyr::select_if(~n_distinct(.)>2) %>% names()))
    #df_pre_expo= df_pre_expo %>% mutate_if(is.numeric,as.factor)
    #df_pre_expo= df_pre_expo %>% mutate_if(is.character,as.factor)
    
    summary_pre_expo <- as.data.frame(summary(df_pre_expo,maxsum=50))
    summary_pre_expo$Pre_expo_freq=summary_pre_expo$Freq
    summary_pre_expo = rename(summary_pre_expo, Covariate_level = Freq, Covariate = Var2)
    #summary_pre_expo$Pre_expo_freq <- gsub("0:","",summary_pre_expo$Pre_expo_freq)
    #summary_pre_expo$Pre_expo_freq <- gsub("1:","",summary_pre_expo$Pre_expo_freq)
    summary_pre_expo$Pre_expo_freq=gsub(".*:", "",summary_pre_expo$Pre_expo_freq) #remove everything before :
    
    summary_pre_expo$Covariate_level= sub('\\:.*', '', summary_pre_expo$Covariate_level)#remove everything after :
    summary_pre_expo$Covariate_level=gsub("\\s","",summary_pre_expo$Covariate_level)#remove any space
    summary_pre_expo$Covariate=gsub("\\s","",summary_pre_expo$Covariate)
    summary_pre_expo$Pre_expo_freq <- as.numeric(summary_pre_expo$Pre_expo_freq)
    summary_pre_expo=summary_pre_expo %>% drop_na(Covariate_level)
    summary_pre_expo=summary_pre_expo%>%select(-Var1)
    summary_pre_expo=left_join(covar_levels, summary_pre_expo, by = c('Covariate', 'Covariate_level')) 
    
    
    df_post_expo = df %>% filter(follow_up_end == outcome_event & follow_up_end >= exp_date_covid19_confirmed & follow_up_end >= follow_up_start)
    df_post_expo <- df_post_expo %>% dplyr::select(c(covar_names))
    #df_post_expo <- df_post_expo %>%  dplyr::select(!c( df_post_expo %>%  dplyr::select_if(~n_distinct(.)>2) %>% names()))
    #df_post_expo= df_post_expo %>% mutate_if(is.numeric,as.factor)
    #df_post_expo= df_post_expo %>% mutate_if(is.character,as.factor)
    
    summary_post_expo <- as.data.frame(summary(df_post_expo, maxsum=50))
    summary_post_expo$Post_expo_freq=summary_post_expo$Freq
    summary_post_expo = rename(summary_post_expo, Covariate_level = Freq, Covariate = Var2)
    summary_post_expo$Post_expo_freq=gsub(".*:", "",summary_post_expo$Post_expo_freq)
    
    summary_post_expo$Covariate_level= sub('\\:.*', '', summary_post_expo$Covariate_level)
    summary_post_expo$Covariate_level=gsub("\\s","",summary_post_expo$Covariate_level)
    summary_post_expo$Covariate=gsub("\\s","",summary_post_expo$Covariate)
    summary_post_expo$Post_expo_freq <- as.numeric(summary_post_expo$Post_expo_freq)
    summary_post_expo=summary_post_expo %>% drop_na(Covariate_level)
    summary_post_expo=summary_post_expo%>%select(-Var1)
    summary_post_expo=left_join(covar_levels, summary_post_expo, by = c('Covariate', 'Covariate_level')) 
    
    summary=left_join(summary_pre_expo, summary_post_expo, by = c('Covariate', 'Covariate_level'))  
    summary$Event=outcome_name
    summary$Strata=population
    summary$Project=project
    
    summary["Post_expo_freq"][is.na(summary["Post_expo_freq"])] <- 0
    summary_final=rbind(summary_final,summary)
  }
}


#write.csv(summary_final, file = file.path("output", paste0("covariates_counts_check", "_", vaccine_status, ".csv")) , row.names=F)
#write.csv(event_counts, file = file.path("output", paste0("event_counts", "_", vaccine_status, ".csv")) , row.names=F)

