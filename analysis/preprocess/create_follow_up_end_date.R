#===============================================================================
#
# Create table of follow up end dates that can be used for venn diagrams, 
# Table 2 and Cox scripts to save repeatedly calculating them 
#
#===============================================================================
library(dplyr)
library(readr)
library(data.table)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort_name <- "vaccinated"
} else {
  cohort_name <- args[[1]]
}

#These are the study start and end dates for the Delta era
cohort_start_date <- as.Date("2021-06-01")
cohort_end_date <- as.Date("2021-12-14")

## Read in active analyses table and filter to relevant outcomes

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active == "TRUE") %>% select(outcome_variable)

follow_up_end_dates <- function(cohort_name, group){
  # Load relevant data
  input <- read_rds(paste0("output/input_",cohort_name,"_stage1_",group,".rds"))
  
  input <- input[,c("patient_id","death_date","index_date","sub_cat_covid19_hospital",active_analyses$outcome_variable,
                    colnames(input)[grepl("exp_",colnames(input))], 
                    colnames(input)[grepl("vax_date_covid_",colnames(input))])] 
  
  input$cohort_end_date<- cohort_end_date
  
  for(event in active_analyses$outcome_variable){
    print(paste0("Working on ",event))
    
    input <- input %>%rename(event_date = event)
    input$expo_date <- input$exp_date_covid19_confirmed
    input$expo_pheno <- input$sub_cat_covid19_hospital
    input <- input %>% mutate(expo_date = replace(expo_date, which(expo_date<index_date), NA))
    
    # Calculate follow up end dates based on cohort
    # follow_up_end_unexposed is required in Table 2 script and follow_up_end is 
    # the general follow up end date for each patient
    if(cohort_name=="vaccinated"){
      input$follow_up_end_unexposed <- apply(input[,c("event_date", "expo_date", "death_date", "cohort_end_date")],1, min,na.rm=TRUE)
      input$follow_up_end <- apply(input[,c("event_date", "death_date", "cohort_end_date")],1, min, na.rm=TRUE)
      
      input$follow_up_end_unexposed <- as.Date(input$follow_up_end_unexposed)
      input$follow_up_end <- as.Date(input$follow_up_end)
      
    }else if(cohort_name=="electively_unvaccinated"){
      input$follow_up_end_unexposed <- apply(input[,c("vax_date_covid_1","event_date", "expo_date", "death_date","cohort_end_date")],1, min,na.rm=TRUE)
      input$follow_up_end <- apply(input[,c("vax_date_covid_1","event_date", "death_date","cohort_end_date")],1, min, na.rm=TRUE)
      
      input$follow_up_end_unexposed <- as.Date(input$follow_up_end_unexposed)
      input$follow_up_end <- as.Date(input$follow_up_end)
    }
    
    
    # Calculate date_expo_censor which is the COVID exposure date for the phenotype  not of interest
    # in the phenotype analyses. This is needed to re-calculate follow-up end for pheno anlayses
    
    input <- input %>% mutate(event_date = replace(event_date, which(event_date>follow_up_end | event_date<index_date), NA))
    input <- input %>% mutate(expo_date = replace(expo_date, which(expo_date>follow_up_end | expo_date<index_date), NA))
    
    # Update COVID phenotypes after setting COVID exposure dates to NA that lie
    # outside follow up
    input$expo_pheno=as.character(input$expo_pheno)
    input=input%>%rowwise()%>%mutate(expo_pheno =ifelse(is.na(expo_date), "no_infection",expo_pheno))
    
    
    # Get COVID pheno specific dataset if necessary
    # Adds in variable date_expo_censor which is the COVID exposure date for the phenotype  not of interest
    # We want to be able to include follow up time prior to exposure for the pheno no of interest which uses date_expo_censor
    # to find this time period
    event_short <- gsub("out_date_","", event)
    
    for(pheno in c("hospitalised","non_hospitalised")){
      input$date_expo_censor <- as.Date(ifelse(!(input$expo_pheno %in% pheno),
                                               input$expo_date, 
                                               NA), origin='1970-01-01')
      setnames(input,
               old="date_expo_censor",
               new=paste0(pheno,"_date_expo_censor"))
    }
    
    input$hospitalised_follow_up_end <- apply(input[,c("follow_up_end","hospitalised_date_expo_censor")],1, min,na.rm=TRUE)
    input$non_hospitalised_follow_up_end <- apply(input[,c("follow_up_end","non_hospitalised_date_expo_censor")],1, min,na.rm=TRUE)
    
    input$hospitalised_follow_up_end <- as.Date(input$hospitalised_follow_up_end)
    input$non_hospitalised_follow_up_end <- as.Date(input$non_hospitalised_follow_up_end)
    
    
    setnames(input,
             old = c("event_date",
                     "follow_up_end_unexposed",
                     "follow_up_end",
                     "hospitalised_follow_up_end",
                     "non_hospitalised_follow_up_end",
                     "hospitalised_date_expo_censor",
                     "non_hospitalised_date_expo_censor"),
             new = c(paste0("out_date_",event_short),
                     paste0(event_short,"_follow_up_end_unexposed"),
                     paste0(event_short,"_follow_up_end"),
                     paste0(event_short,"_hospitalised_follow_up_end"),
                     paste0(event_short,"_non_hospitalised_follow_up_end"),
                     paste0(event_short,"_hospitalised_date_expo_censor"),
                     paste0(event_short,"_non_hospitalised_date_expo_censor")))
    
    print(paste0("Finished working on ",event))
  }
  
  input <- input[,c("patient_id","index_date",
                    colnames(input)[grepl("follow_up",colnames(input))],
                    colnames(input)[grepl("censor",colnames(input))])] 
  
  saveRDS(input, paste0("output/follow_up_end_dates_",cohort_name, "_",group, ".rds"))
}

# Run function using specified commandArgs and active analyses for group

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses %>% filter(active==TRUE)
group <- unique(active_analyses$outcome_group)

for(i in group){
  if (cohort_name == "both") {
    follow_up_end_dates("electively_unvaccinated", i)
    follow_up_end_dates("vaccinated", i)
  } else{
    follow_up_end_dates(cohort_name, i)
  }
}
