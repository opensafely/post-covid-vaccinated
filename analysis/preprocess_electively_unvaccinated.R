# Filter to individuals with less than 2 vaccinations --------------------------

tmp <- tmp[is.na(tmp$vax_date_covid_2),]

# Remove variables for vaccinated ----------------------------------------------

tmp[,colnames(tmp)[grepl("_vaccinated",colnames(tmp))]] <- NULL
tmp[,c(paste0(c("vax_date_AstraZeneca_","vax_date_Pfizer_","vax_date_Moderna_"),1))] <- NULL
tmp[,c(paste0(c("vax_date_covid_","vax_date_AstraZeneca_","vax_date_Pfizer_","vax_date_Moderna_"),2))] <- NULL
tmp[,c(paste0(c("vax_date_covid_","vax_date_AstraZeneca_","vax_date_Pfizer_","vax_date_Moderna_"),3))] <- NULL

# Determine index date ---------------------------------------------------------

tmp$study_start_date <- as.Date(study_start)

tmp$pat_start_date  <- as.Date(tmp$vax_date_eligible)+84

tmp$use_date <- ifelse(tmp$study_start_date>tmp$pat_start_date,"index","nonindex")

# Create sub cohorts -----------------------------------------------------------

index <- tmp[tmp$use_date=="index",]
index[,grepl("_electively_unvaccinated",colnames(index))] <- NULL
colnames(index) <- gsub("_index","",colnames(index))
index$index_date <- index$study_start_date
index[,c("use_date")] <- NULL

nonindex <- tmp[tmp$use_date=="nonindex",]
nonindex[,grepl("_index",colnames(nonindex))] <- NULL
colnames(nonindex) <- gsub("_electively_unvaccinated","",colnames(nonindex))
nonindex$index_date <- nonindex$pat_start_date 
nonindex[,c("use_date")] <- NULL

tmp <- rbind(index,nonindex)