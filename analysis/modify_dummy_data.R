# Set seed

set.seed(1)

# Load vaccinated data

df <- readr::read_rds(file.path("output", "input_vaccinated.rds"))

# Assign a vaccine product for dose 1

df$vax_cat_product_1 <- ifelse(rbinom(nrow(df),1,0.5)==1,"AstraZeneca","Pfizer")
df$vax_cat_product_1 <- ifelse(runif(nrow(df), min = 0, max = 1)>0.9,"Moderna",df$vax_cat_product_1)

# Assign a vaccine product for dose 2

df$vax_cat_product_2 <- df$vax_cat_product_1
df$vax_cat_product_2 <- ifelse(runif(nrow(df), min = 0, max = 1)>0.99,"AstraZeneca",df$vax_cat_product_2)
df$vax_cat_product_2 <- ifelse(runif(nrow(df), min = 0, max = 1)>0.99,"Pfizer",df$vax_cat_product_2)
df$vax_cat_product_2 <- ifelse(runif(nrow(df), min = 0, max = 1)>0.99,"Moderna",df$vax_cat_product_2)

# Assign a vaccine product for dose 3

df$vax_cat_product_3 <- ifelse(!is.na(df$vax_date_covid_3),ifelse(rbinom(nrow(df),1,0.5)==1,"Moderna","Pfizer"),"")
df$vax_cat_product_3 <- ifelse(runif(nrow(df), min = 0, max = 1)>0.9,"",df$vax_cat_product_3)

# Save updated dummy data

saveRDS(df, file = "output/input_vaccinated.rds")