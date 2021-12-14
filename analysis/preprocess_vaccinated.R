# Filter to individuals with known vaccinations 1 and 2 ------------------------

tmp <- tmp[!is.na(tmp$vax_date_covid_1) & !is.na(tmp$vax_date_covid_2),]

# Remove variables for electively unvaccinated ---------------------------------

tmp[,colnames(tmp)[grepl("_electively_unvaccinated",colnames(tmp))]] <- NULL

# Determine index date ---------------------------------------------------------

tmp$study_start_date <- as.Date(study_start)

tmp$pat_start_date  <- as.Date(tmp$vax_date_covid_2)+14

tmp$use_date <- ifelse(tmp$study_start_date>tmp$pat_start_date ,"index","nonindex")

# Create sub cohorts -----------------------------------------------------------

index <- tmp[tmp$use_date=="index",]
index[,grepl("_vaccinated",colnames(index))] <- NULL
colnames(index) <- gsub("_index","",colnames(index))
index$index_date <- index$study_start_date
index[,c("use_date")] <- NULL

nonindex <- tmp[tmp$use_date=="nonindex",]
nonindex <- nonindex[,!grepl("_index",colnames(nonindex))]
colnames(nonindex) <- gsub("_vaccinated","",colnames(nonindex))
nonindex$index_date <- nonindex$pat_start_date 
nonindex[,c("use_date")] <- NULL

tmp <- rbind(index,nonindex)

# Generate vaccine variables ---------------------------------------------------

for (i in 1:3) {
  
  # Restrict to relevant columns and rename
  
  vax <- tmp[,c("patient_id",paste0(c("vax_date_covid_","vax_date_AstraZeneca_","vax_date_Pfizer_","vax_date_Moderna_"),i))]
  colnames(vax) <- c("patient_id","vax_date_covid","vax_date_AstraZeneca","vax_date_Pfizer","vax_date_Moderna")
  
  # Determine vaccination product
  
  vax$vax_cat_product <- NA
  
  vax$vax_cat_product <- ifelse(!is.na(vax$vax_date_covid) & 
                                  vax$vax_date_covid==vax$vax_date_AstraZeneca &
                                  vax$vax_date_covid!=vax$vax_date_Pfizer &
                                  vax$vax_date_covid!=vax$vax_date_Moderna,"AstraZeneca",vax$vax_cat_product)
  
  vax$vax_cat_product <- ifelse(!is.na(vax$vax_date_covid) & 
                                  vax$vax_date_covid!=vax$vax_date_AstraZeneca &
                                  vax$vax_date_covid==vax$vax_date_Pfizer &
                                  vax$vax_date_covid!=vax$vax_date_Moderna,"Pfizer",vax$vax_cat_product)
  
  vax$vax_cat_product <- ifelse(!is.na(vax$vax_date_covid) & 
                                  vax$vax_date_covid!=vax$vax_date_AstraZeneca &
                                  vax$vax_date_covid!=vax$vax_date_Pfizer &
                                  vax$vax_date_covid==vax$vax_date_Moderna,"Moderna",vax$vax_cat_product)
  
  # Add information to main data
  
  vax <- vax[,c("patient_id","vax_cat_product")]
  colnames(vax) <- c("patient_id",paste0("vax_cat_product_",i))
  tmp <- merge(tmp, vax, by = "patient_id")
  
  # Remove unnecessary vaccination product information
  
  tmp[,paste0(c("vax_date_AstraZeneca_","vax_date_Pfizer_","vax_date_Moderna_"),i)] <- NULL
  
}