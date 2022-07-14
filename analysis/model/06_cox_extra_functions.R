#------------------------ SET DATES OUTSIDE RANGE AS NA ------------------------


set_dates_outofrange_na <- function(df, colname)
{
  df=df%>%rowwise()%>%mutate(!!sym(colname):=as.Date(ifelse(!!sym(colname)<follow_up_start | !!sym(colname) > follow_up_end, NA,!!sym(colname)),origin='1970-01-01'))
  return(df)
}


#----------------------- GET COVID PHENO-SPECIFIC DATASET ----------------------
# Adds in variable date_expo_censor which is the COVID exposure date for the phenotype  not of interest
# We want to be able to include follow up time prior to exposure for the pheno no of interest which uses date_expo_censor
# to find this time period

get_pheno_specific_dataset <- function(survival_data, pheno_of_interest){
  survival_data$date_expo_censor <- as.Date(ifelse(!(survival_data$expo_pheno %in% pheno_of_interest),
                                                   survival_data$expo_date, 
                                                   NA), origin='1970-01-01')
  
  
  survival_data$expo_date <- as.Date(ifelse((!is.na(survival_data$date_expo_censor)) & (survival_data$expo_date >= survival_data$date_expo_censor), NA, survival_data$expo_date), origin='1970-01-01')
  survival_data$event_date <- as.Date(ifelse((!is.na(survival_data$date_expo_censor)) & (survival_data$event_date >= survival_data$date_expo_censor), NA, survival_data$event_date), origin='1970-01-01')
  return(survival_data)
}


rm_lowvar_covars <- function(data_surv){
  cov_bin <- colnames(data_surv)[grepl("cov_bin", colnames(data_surv))]
  df <- data_surv %>% dplyr::select(c( "expo", "event", all_of(cov_bin),"patient_id")) %>% distinct() %>% filter((expo==1) & (event==1))
  df <- df %>%  dplyr::select(!c("expo", "event", "patient_id",
                                 df %>%  dplyr::select_if(is.numeric) %>% names()
                                 
  ))
  summary <- as.data.frame(summary(df,maxsum=100))
  summary <- summary %>% filter(startsWith(Freq,"Mode")==F)
  summary$Freq=gsub(".*:", "",summary$Freq)#Remove everything before:
  summary$Freq <- as.numeric(summary$Freq)
  covars_to_remove=as.character(summary$Var2[summary$Freq <=5])
  summary <- summary(summary$Var2)
  covars_to_remove <- append(covars_to_remove,names(summary)[summary==1])
  return(covars_to_remove)
}



collapse_categorical_covars <- function(data_surv,subgroup){
  cov_cat <-colnames(data_surv)[grepl("cov_cat", colnames(data_surv))]
  df <- data_surv %>% dplyr::select(c( "expo", "event", all_of(cov_cat), "patient_id")) %>% distinct() %>% filter((expo==1) & (event==1))
  df <- df %>%  dplyr::select(!c("expo", "event", "patient_id"))
  
  summary <- as.data.frame(summary(df,maxsum=50))
  summary$Freq=gsub(".*:", "",summary$Freq)#Remove everything before:
  summary$Var2 <- gsub("\\s","",summary$Var2)
  summary$Freq <- as.numeric(summary$Freq)
  
  cat_cov_to_remove=unique(as.character(summary$Var2[summary$Freq <=5]))
  
  if("cov_cat_deprivation" %in% cat_cov_to_remove){
    data_surv=data_surv %>% mutate(cov_cat_deprivation= 
                                     case_when(cov_cat_deprivation=="1-2 (most deprived)"~"1-4",
                                               cov_cat_deprivation=="3-4"~"1-4",
                                               cov_cat_deprivation=="5-6"~"5-6",
                                               cov_cat_deprivation=="7-8"~"7-10",
                                               cov_cat_deprivation=="9-10 (least deprived)"~"7-10"))
    
    data_surv$cov_cat_deprivation <- ordered(data_surv$cov_cat_deprivation, levels = c("1-4","5-6","7-10"))
  }
  
  if("cov_cat_smoking_status" %in% cat_cov_to_remove){
    if(subgroup != "covid_pheno_hospitalised"){
      data_surv=data_surv %>% mutate(cov_cat_smoking_status = as.character(cov_cat_smoking_status)) %>%
        mutate(cov_cat_smoking_status= case_when(cov_cat_smoking_status=="Never smoker"~"Never smoker",
                                                 cov_cat_smoking_status=="Ever smoker"~"Ever smoker",
                                                 cov_cat_smoking_status=="Current smoker"~"Ever smoker",
                                                 cov_cat_smoking_status=="Missing"~"Missing"))
      
      smoking_status_mode <- get_mode(data_surv,"cov_cat_smoking_status")
      data_surv <- data_surv %>% mutate(cov_cat_smoking_status = as.factor(cov_cat_smoking_status)) %>%
        mutate(cov_cat_smoking_status = relevel(cov_cat_smoking_status,ref=smoking_status_mode))
    }else if (subgroup == "covid_pheno_hospitalised"){
      data_surv=data_surv %>% mutate(cov_cat_smoking_status = as.character(cov_cat_smoking_status)) %>%
        mutate(cov_cat_smoking_status= case_when(cov_cat_smoking_status=="Never smoker"~"Never smoker",
                                                 cov_cat_smoking_status=="Ever smoker"~"Ever smoker",
                                                 cov_cat_smoking_status=="Current smoker"~"Ever smoker"))
      
      smoking_status_mode <- get_mode(data_surv,"cov_cat_smoking_status")
      data_surv <- data_surv %>% mutate(cov_cat_smoking_status = as.factor(cov_cat_smoking_status)) %>%
        mutate(cov_cat_smoking_status = relevel(cov_cat_smoking_status,ref=smoking_status_mode))
    }
    
  }
  return(list(data_surv,cat_cov_to_remove))
}


covariate_exploration <- function(data_surv, covars){
  df <- data_surv %>% dplyr::select(c( "expo", "event", all_of(covars), "region_name","sex","patient_id")) %>% distinct() %>% filter((expo==1) & (event==1))
  df <- df %>%  dplyr::select(!c("expo", "event", "patient_id",
                                 df %>%  dplyr::select_if(is.numeric) %>% names()
                                 
  ))
  summary <- as.data.frame(summary(df,maxsum=100))
  summary <- summary %>% filter(startsWith(Freq,"Mode")==F)
  summary$Count <- summary$Freq
  summary$Freq=gsub(":.*", "",summary$Freq)#Remove everything after:
  summary$Count=gsub(".*:", "",summary$Count)#Remove everything before
  summary$Var2 <- gsub("\\s","",summary$Var2)
  return(summary)
}

get_mode <- function(dataset, covariate){
  u <- unique(dataset[[covariate]])
  tab <- tabulate(match(dataset[[covariate]], u))
  relevel_with <- u[tab == max(tab)]
  return(relevel_with)
}