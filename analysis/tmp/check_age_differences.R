library(Hmisc)
library(dplyr)

fs::dir_create(here::here("output", "not-for-review", "tmp"))

index <- arrow::read_feather(file = "output/input_index.feather",
                          col_select = c("patient_id",
                                         "cov_num_age"))

index <- index[,c("patient_id","cov_num_age")]

electively_unvaccinated <- arrow::read_feather(file = "output/input_electively_unvaccinated.feather",
                            col_select = c("patient_id",
                                           "cov_num_age",
                                           "vax_date_eligible",
                                           "vax_cat_jcvi_group"))
electively_unvaccinated$vax_date_eligible <- as.Date(electively_unvaccinated$vax_date_eligible)

electively_unvaccinated <- electively_unvaccinated[,c("patient_id","cov_num_age","vax_date_eligible","vax_cat_jcvi_group")]

electively_unvaccinated <- electively_unvaccinated %>% rename("cov_num_age_electively_unvaccinated" = "cov_num_age")

index <- index %>% left_join(electively_unvaccinated, by = "patient_id")

index <- index %>% filter(abs(cov_num_age_electively_unvaccinated - cov_num_age) >= 2)

index <- index %>%
  dplyr::mutate(age_group_index = dplyr::case_when(
    cov_num_age < 0 ~ "<0",
    cov_num_age >= 0 & cov_num_age < 16 ~ "0-15",
    cov_num_age >= 16 & cov_num_age < 17 ~ "16-17",
    cov_num_age >= 18 & cov_num_age < 30 ~ "18-29",
    cov_num_age >= 30 & cov_num_age < 40 ~ "30-39",
    cov_num_age >= 40 & cov_num_age < 50 ~ "40-49",
    cov_num_age >= 50 & cov_num_age < 55 ~ "50-54",
    cov_num_age >= 55 & cov_num_age < 60 ~ "55-59",
    cov_num_age >= 60 & cov_num_age < 65 ~ "60-64",
    cov_num_age >= 65 & cov_num_age < 70 ~ "65-69",
    cov_num_age >= 70 & cov_num_age < 75 ~ "70-74",
    cov_num_age >= 75 & cov_num_age < 80 ~ "75-79",
    cov_num_age >= 80 & cov_num_age <= 110 ~ "80-110",
    cov_num_age > 110 ~ "<110"))

index <- index %>%
  dplyr::mutate(age_group_electively_unvaccinated = dplyr::case_when(
    cov_num_age_electively_unvaccinated < 0 ~ "<0",
    cov_num_age_electively_unvaccinated >= 0 & cov_num_age_electively_unvaccinated < 16 ~ "0-15",
    cov_num_age_electively_unvaccinated >= 16 & cov_num_age_electively_unvaccinated < 17 ~ "16-17",
    cov_num_age_electively_unvaccinated >= 18 & cov_num_age_electively_unvaccinated < 30 ~ "18-29",
    cov_num_age_electively_unvaccinated >= 30 & cov_num_age_electively_unvaccinated < 40 ~ "30-39",
    cov_num_age_electively_unvaccinated >= 40 & cov_num_age_electively_unvaccinated < 50 ~ "40-49",
    cov_num_age_electively_unvaccinated >= 50 & cov_num_age_electively_unvaccinated < 55 ~ "50-54",
    cov_num_age_electively_unvaccinated >= 55 & cov_num_age_electively_unvaccinated < 60 ~ "55-59",
    cov_num_age_electively_unvaccinated >= 60 & cov_num_age_electively_unvaccinated < 65 ~ "60-64",
    cov_num_age_electively_unvaccinated >= 65 & cov_num_age_electively_unvaccinated < 70 ~ "65-69",
    cov_num_age_electively_unvaccinated >= 70 & cov_num_age_electively_unvaccinated < 75 ~ "70-74",
    cov_num_age_electively_unvaccinated >= 75 & cov_num_age_electively_unvaccinated < 80 ~ "75-79",
    cov_num_age_electively_unvaccinated >= 80 & cov_num_age_electively_unvaccinated <= 110 ~ "80-110",
    cov_num_age_electively_unvaccinated > 110 ~ "<110"))

print("JCVI group by age_group_index")
print(table(index$vax_cat_jcvi_group,index$age_group_index))

print("JCVI group by age_group_electively_unvaccinated")
print(table(index$vax_cat_jcvi_group,index$age_group_electively_unvaccinated))

print("JCVI group")
print(table(index$vax_cat_jcvi_group))

sink(paste0("output/not-for-review/tmp/describe_age_diff.txt"))
print(Hmisc::describe(index))
sink()

