# Set seed ---------------------------------------------------------------------

set.seed(1)

# Make vax spine ---------------------------------------------------------------

tmp <- df[,c("patient_id","vax_date_eligible")]

# Assign product to each individual --------------------------------------------

tmp$product <- c("AstraZeneca","Pfizer","None","Moderna")[ceiling(runif(nrow(tmp),0,3.5))]

# Generate first vaccine information -------------------------------------------

tmp <- data.frame(patient_id = tmp$patient_id,
                  date = as.Date(tmp$vax_date_eligible + round(rnorm(nrow(tmp), mean = 10, sd = 3)), origin = '1970-01-01'),
                  seq = 1,
                  product = tmp$product,
                  stringsAsFactors = FALSE)

# Generate second vaccine information ------------------------------------------

tmp2 <- tmp
tmp2$date <- as.Date(tmp2$date + round(rnorm(nrow(tmp2), mean = 10*7, sd = 7)), origin = '1970-01-01')
tmp2$seq <- 2
tmp2$product <- ifelse(runif(nrow(tmp2),0,1)>0.95 & tmp2$product!="None",c("AstraZeneca","Pfizer","None","Moderna")[ceiling(runif(nrow(tmp2),0,3.5))],tmp2$product)
tmp <- rbind(tmp,tmp2)

# Generate third vaccine information -------------------------------------------

tmp3 <- tmp2
tmp3$date <- as.Date(tmp3$date + round(rnorm(nrow(tmp3), mean = 6*4*7, sd = 7)), origin = '1970-01-01')
tmp3$seq <- 3
tmp3$product <- ifelse(tmp3$product!="None",c("Pfizer","None","Moderna","AstraZeneca")[ceiling(runif(nrow(tmp3),0,3.01))],"None")
tmp <- rbind(tmp,tmp3)

# Remove sequence information by patient only ----------------------------------

tmp$seq <- NULL

# Remove records of no product -------------------------------------------------

tmp <- tmp[tmp$product!="None",]

# Add sequence information by patient and product ------------------------------

tmp <- tmp %>%
  dplyr::arrange(patient_id,product,date) %>%
  dplyr::group_by(patient_id,product) %>%
  dplyr::mutate(product_rank=rank(date))

# Make wide format table -------------------------------------------------------

tmp <- tidyr::pivot_wider(tmp, 
                   names_from = c("product","product_rank"), 
                   names_prefix = "vax_date_", 
                   values_from = "date")

# Order variables --------------------------------------------------------------

tmp <- tmp[,c("patient_id",
              "vax_date_AstraZeneca_1","vax_date_AstraZeneca_2","vax_date_AstraZeneca_3",
              "vax_date_Pfizer_1","vax_date_Pfizer_2","vax_date_Pfizer_3",
              "vax_date_Moderna_1","vax_date_Moderna_2","vax_date_Moderna_3")]

# Replace vax data in main dataset ---------------------------------------------

df[,c("vax_date_AstraZeneca_1","vax_date_AstraZeneca_2","vax_date_AstraZeneca_3",
      "vax_date_Pfizer_1","vax_date_Pfizer_2","vax_date_Pfizer_3",
      "vax_date_Moderna_1","vax_date_Moderna_2","vax_date_Moderna_3")] <- NULL

df <- merge(df, tmp, by = "patient_id", all.x = TRUE)

rm(tmp, tmp2, tmp3)