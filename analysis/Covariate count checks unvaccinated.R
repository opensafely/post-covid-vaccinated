#Checking of covariates for critical numbers
library(dplyr)
library(data.table)
library(purrr)


input <- fread("OpenSafely - unvaccinated analysis/input.csv" ,na.strings=c("","NA"))
input=map_df(input, ~ gsub(" ", "_", .x))

date_columns=colnames(input)[grepl("\\d\\d\\d\\d-\\d\\d-\\d\\d",input)]

for(date in date_columns){
  input[[date]]=as.Date(input[[date]], format="%Y-%m-%d")
}


outcome <- input %>% dplyr::select(c(names(input)[grepl("out_", names(input))])) %>% names()
covar_names <- input %>% dplyr::select(c(names(input)[grepl("cov_", names(input))])) %>% names()
#covar_names=covar_names[! covar_names %in% c('cov_sex','cov_age','cov_n_disorder')]

covar_levels=input %>% dplyr::select(c(covar_names))
covar_levels <- covar_levels %>%  dplyr::select(!c(covar_levels %>%  dplyr::select_if(~n_distinct(.)>20) %>% names()))
#covar_levels=map_df(covar_levels, ~ gsub(" ", "_", .x))#To remove spaces in cov_region

covar_levels= covar_levels %>% mutate_if(is.numeric,as.factor)
covar_levels= covar_levels %>% mutate_if(is.character,as.factor)
covar_levels=as.data.frame(summary(covar_levels,maxsum=150))
covar_levels = rename(covar_levels, Covariate_level = Freq, Covariate = Var2)
covar_levels$Covariate_level= sub('\\:.*', '', covar_levels$Covariate_level)
covar_levels$Covariate_level=gsub("\\s","",covar_levels$Covariate_level)
covar_levels$Covariate=gsub("\\s","",covar_levels$Covariate)
covar_levels=covar_levels %>% drop_na(Covariate_level)
covar_levels=covar_levels%>%select(-Var1)
covar_names=unique(covar_levels$Covariate)

summary_final <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(summary_final) <- c('Covariate', 'Covariate_level', 'Pre_expo_freq', 'Post_expo_freq', 'Event')

outcome_name="out_ami"
for(outcome_name in outcome){
  df=rename(input, outcome_event = outcome_name)
  df=df %>% rowwise() %>% mutate(censor_date=min(outcome_event, covid19_vaccination_date1, death_date,as.Date("2021-10-27"),na.rm = TRUE))
  df_pre_expo= df %>% filter((censor_date == outcome_event & censor_date < exp_confirmed_covid19_date) | (censor_date == outcome_event & is.na(exp_confirmed_covid19_date)=="TRUE"))
  
  #covar_names=covar_names[! covar_names %in% c('cov_sex','cov_age','cov_deprivation','cov_n_disorder')]
  
  df_pre_expo <- df_pre_expo %>% dplyr::select(c(covar_names))
  #df_pre_expo <- df_pre_expo %>%  dplyr::select(!c( df_pre_expo %>%  dplyr::select_if(~n_distinct(.)>2) %>% names()))
  df_pre_expo= df_pre_expo %>% mutate_if(is.numeric,as.factor)
  df_pre_expo= df_pre_expo %>% mutate_if(is.character,as.factor)
  
  summary_pre_expo <- as.data.frame(summary(df_pre_expo,maxsum=50))
  summary_pre_expo$Pre_expo_freq=summary_pre_expo$Freq
  summary_pre_expo = rename(summary_pre_expo, Covariate_level = Freq, Covariate = Var2)
  #summary_pre_expo$Pre_expo_freq <- gsub("0:","",summary_pre_expo$Pre_expo_freq)
  #summary_pre_expo$Pre_expo_freq <- gsub("1:","",summary_pre_expo$Pre_expo_freq)
  summary_pre_expo$Pre_expo_freq=gsub(".*:", "",summary_pre_expo$Pre_expo_freq)
  
  summary_pre_expo$Covariate_level= sub('\\:.*', '', summary_pre_expo$Covariate_level)
  summary_pre_expo$Covariate_level=gsub("\\s","",summary_pre_expo$Covariate_level)
  summary_pre_expo$Covariate=gsub("\\s","",summary_pre_expo$Covariate)
  summary_pre_expo$Pre_expo_freq <- as.numeric(summary_pre_expo$Pre_expo_freq)
  summary_pre_expo=summary_pre_expo %>% drop_na(Covariate_level)
  summary_pre_expo=summary_pre_expo%>%select("Covariate","Covariate_level","Pre_expo_freq")
  summary_pre_expo=left_join(covar_levels, summary_pre_expo, by = c('Covariate', 'Covariate_level')) 
  
  df_post_expo = df %>% filter(censor_date == outcome_event & censor_date >= exp_confirmed_covid19_date)
  df_post_expo <- df_post_expo %>% dplyr::select(c(covar_names))
  #df_post_expo <- df_post_expo %>%  dplyr::select(!c( df_post_expo %>%  dplyr::select_if(~n_distinct(.)>2) %>% names()))
  df_post_expo= df_post_expo %>% mutate_if(is.numeric,as.factor)
  df_post_expo= df_post_expo %>% mutate_if(is.character,as.factor)
  
  summary_post_expo <- as.data.frame(summary(df_post_expo, maxsum=50))
  summary_post_expo$Post_expo_freq=summary_post_expo$Freq
  summary_post_expo = rename(summary_post_expo, Covariate_level = Freq, Covariate = Var2)
  #summary_post_expo$Post_expo_freq <- gsub("0:","",summary_post_expo$Post_expo_freq)
  #summary_post_expo$Post_expo_freq <- gsub("1:","",summary_post_expo$Post_expo_freq)
  summary_post_expo$Post_expo_freq=gsub(".*:", "",summary_post_expo$Post_expo_freq)
  
  summary_post_expo$Covariate_level= sub('\\:.*', '', summary_post_expo$Covariate_level)
  summary_post_expo$Covariate_level=gsub("\\s","",summary_post_expo$Covariate_level)
  summary_post_expo$Covariate=gsub("\\s","",summary_post_expo$Covariate)
  summary_post_expo$Post_expo_freq <- as.numeric(summary_post_expo$Post_expo_freq)
  summary_post_expo=summary_post_expo %>% drop_na(Covariate_level)
  summary_post_expo=summary_post_expo%>%select("Covariate","Covariate_level","Post_expo_freq")
  summary_post_expo=left_join(covar_levels, summary_post_expo, by = c('Covariate', 'Covariate_level')) 
  
  summary=left_join(summary_pre_expo, summary_post_expo, by = c('Covariate', 'Covariate_level'))  
  summary$Event=outcome_name
  summary["Post_expo_freq"][is.na(summary["Post_expo_freq"])] <- 0
  summary_final=rbind(summary_final,summary)
}


#Event counts
event_counts <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(event_counts) <- c('Pre_expo_event_count', 'Post_expo_event_count', 'Event')


for(outcome_name in outcome){
  count=summary_final%>%filter(Covariate=="cov_ami" & Event==outcome_name)
  count$Pre_expo_freq=sum(count$Pre_expo_freq)
  count$Post_expo_freq=sum(count$Post_expo_freq)
  count=count[1,]
  count=count%>%select(Pre_expo_freq,Post_expo_freq,Event)
  event_counts=rbind(event_counts,count)
}
