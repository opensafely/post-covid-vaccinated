library(purrr)
library(data.table)

fs::dir_create(here::here("output", "review", "model"))

output_dir <- "output/review/model"

#Read in R HRs
hr_files=list.files(path = output_dir, pattern = "to_release")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(output_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

estimates$redacted_results <- factor(estimates$redacted_results, levels = c("Redacted results",
                                                                            "No redacted results"))
estimates <- estimates[order(estimates$redacted_results),]

write.csv(estimates,paste0(output_dir,"/R_HR_output.csv") , row.names=F)

