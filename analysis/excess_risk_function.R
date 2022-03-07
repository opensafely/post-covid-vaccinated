#Project:Vaccinated delta wave population study
#Branch:Absolute excess risk calculations
#Scripts: Renin Toms, Xiyun Jiang, Venexia Walker

excess_risk <- function(event, model, cohort, strata) {
  
  #---------------------------------
  # Step0: Import relevant data
  #---------------------------------
  
  # Load data
  input1 <- readr::read_csv(paste0("output/example_input1.csv")) #1.person days
  input2 <- readr::read_csv(paste0("output/example_input2.csv")) #2.unexposed events, 3.total cases, 4.hr

  # Make single input table
  df <- merge(input1, input2, by = c("event","model","cohort","strata"))
  
  # Restrict to relevant data
  df <- df[df$event==event & 
                   df$model == model &
                   df$cohort == cohort &
                   df$strata == strata,]
  
  #---------------------------------
  # Step1: Convert table to days
  #---------------------------------
  df$term <- gsub("days","",df$term)
  
  df <- tidyr::separate(data = df, 
                           col = term,
                           into = c("tstart","tstop"),
                           sep = "_",
                           remove = FALSE)
  
  df$tstart <- as.numeric(df$tstart)
  df$tstop <- as.numeric(df$tstop)
  df$diff <- df$tstop - df$tstart
  df <- df[order(df$tstart,df$tstop),]
  df <- as.data.frame(lapply(df, rep, df$diff))
  df$days <- row.names(df)
  
  #--------------------------------------------------------------------
  #Step2.Calculate the average daily incidence in the unexposed
  #--------------------------------------------------------------------
  #Number of new events / sum of person-time at risk
  
  df$q <- df$unexposed_events/df$person_days
  
  #-------------------------------------------------------------
  #Step3. Make life table to calculate cumulative risk over time
  #-------------------------------------------------------------
  #Description:Use a life table approach to calculate age- and sex specific cumulative risks over time, - with and without COVID-19. 
  
  df$'1-q' <- 1 - df$q 
  df$s <- cumprod(df$`1-q`)
  
  #----------------------------------------
  #Step4. Calculate the daily CVD incidence
  #----------------------------------------
  #Description: Multiply  the average daily incidence by the maximally adjusted age- and sex-specific HR, -
  # for that day to derive the incidence on each day after COVID-19. 
  
  df$qh <- df$q*df$hr
  df$'1-qh' <- 1 - df$qh
  df$sc <- cumprod(df$`1-qh`)
  
  #-----------------------------------------
  #Step5. Calculate the Absolute excess risk
  #-----------------------------------------
  #Description:Subtract the latter from the former to derive the absolute excess risks over time after COVID-19, -
  #compared with no COVID-19 diagnosis. 
  
  #AER = Sc-S=difference in absolute risk
  
  df$AER <- df$sc - df$s
  df$AER_cases <- df$ER * df$total_covid19_cases
  
  # Return results
  
  results <- data.frame(event = event,
                        model = model, 
                        cohort = cohort, 
                        strata = strata,
                        day = df[nrow(df),]$days,
                        AER = df[nrow(df),]$AER_cases,
                        stringsAsFactors = FALSE)
  
  return(results)
  
}