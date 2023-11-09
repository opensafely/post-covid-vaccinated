# Define arguments

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  name="m1split"
}else{
  name  = args[[1]]
}

# List files to be combined

files <- list.files(path = "output/", pattern = "_cox_model_")

analyses_to_run_stata <- read.csv("lib/analyses_to_run_in_stata.csv", header=TRUE,
                                  col.names = c("outcome","cohort","subgroup","time_periods","day0","extf","m1split"),
                                  colClasses = c("character","character","character","character","character","character","character"))

analyses_to_run_stata$subgroup <- ifelse(analyses_to_run_stata$subgroup=="hospitalised","covid_pheno_hospitalised",analyses_to_run_stata$subgroup)
analyses_to_run_stata$subgroup <- ifelse(analyses_to_run_stata$subgroup=="non_hospitalised","covid_pheno_non_hospitalised",analyses_to_run_stata$subgroup)

tmp_files <- paste0("stata_cox_model_",analyses_to_run_stata$outcome,
                    "_",analyses_to_run_stata$subgroup,
                    "_",analyses_to_run_stata$cohort,
                    "_day0",analyses_to_run_stata$day0,
                    "_extf",analyses_to_run_stata$extf,
                    "_m1split",analyses_to_run_stata$m1split,".txt")

files <- intersect(files, tmp_files)

# Create empty master data frame

df <- NULL

# Append each file to master data frame

for (f in files) {
  
  ## Load data
  
  tmp <- readr::read_tsv(file = paste0("output/",f), skip = 2,
                         col_names = c("term",
                                       "b_min","se_min","t_min","lci_min","uci_min","p_min",
                                       "b_age_sex_obesity","se_age_sex_obesity","t_age_sex_obesity","lci_age_sex_obesity","uci_age_sex_obesity","p_age_sex_obesity",
                                       "b_max","se_max","t_max","lci_max","uci_max","p_max"))
  
  colnames(tmp) <- gsub("age_sex_obesity","AgeSexObesity",colnames(tmp))
  
  ## Make variables numeric
  
  tmp$b_min <- as.numeric(tmp$b_min)
  tmp$se_min <- as.numeric(tmp$se_min)
  tmp$t_min <- as.numeric(tmp$t_min)
  tmp$lci_min <- as.numeric(tmp$lci_min)
  tmp$uci_min <- as.numeric(tmp$uci_min)
  tmp$p_min <- as.numeric(tmp$p_min)
  tmp$b_max <- as.numeric(tmp$b_max)
  
  tmp$b_AgeSexObesity <- as.numeric(tmp$b_AgeSexObesity)
  tmp$se_AgeSexObesity <- as.numeric(tmp$se_AgeSexObesity)
  tmp$t_AgeSexObesity <- as.numeric(tmp$t_AgeSexObesity)
  tmp$lci_AgeSexObesity <- as.numeric(tmp$lci_AgeSexObesity)
  tmp$uci_AgeSexObesity <- as.numeric(tmp$uci_AgeSexObesity)
  tmp$p_AgeSexObesity <- as.numeric(tmp$p_AgeSexObesity)
  
  tmp$se_max <- as.numeric(tmp$se_max)
  tmp$t_max <- as.numeric(tmp$t_max)
  tmp$lci_max <- as.numeric(tmp$lci_max)
  tmp$uci_max <- as.numeric(tmp$uci_max)
  tmp$p_max <- as.numeric(tmp$p_max)
  
  ## Add source
  
  tmp$source <- f
  
  ## Separate info from estimates
  
  info_terms <- c("risk","N_fail","N_sub","N","N_clust")
  info <- tmp[tmp$term %in% info_terms,c("source","term","b_min","b_AgeSexObesity","b_max")]
  info <- dplyr::rename(info, "min" = "b_min","AgeSexObesity"="b_AgeSexObesity", "max" = "b_max")
  tmp <- tmp[!(tmp$term %in% info_terms),]
  
  ## Rename info
  
  info$term <- ifelse(info$term=="risk", "persondays", info$term)
  info$term <- ifelse(info$term=="N_fail", "outcomes", info$term)
  info$term <- ifelse(info$term=="N_sub", "subjects", info$term)
  info$term <- ifelse(info$term=="N", "observations", info$term)
  info$term <- ifelse(info$term=="N_clust", "clusters", info$term)
  
  ## Transpose info 
  
  info <- tidyr::pivot_wider(info, 
                             id_cols = "source", 
                             names_from = "term", 
                             values_from = c("min","AgeSexObesity", "max"),
                             names_glue = "{term}_{.value}")
  
  ## Merge info and estinates
  
  tmp <- merge(tmp, info, by = "source")
  
  ## Add median follow up

  f <- gsub("cox_model_","median_fup_",f)
  f <- gsub(".txt",".csv",f)
  print(f)
  fup <- readr::read_csv(file = paste0("output/",f))
  tmp <- merge(tmp, fup, by = "term", all.x = TRUE)
  
  ## Apend to master dataframe
    
  df <- rbind(df, tmp)
    
}

# Format master dataframe

df <- df[,c("source","term","medianfup",
            paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_min"),
            paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_AgeSexObesity"),
            paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_max"))]

df <- tidyr::pivot_longer(df, 
                          cols = c(paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_min"),
                                   paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_AgeSexObesity"),
                                   paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_max")),
                          names_to = c("stat","model"),
                          names_sep = "_",
                          names_prefix = "name",
                          values_to = "value")


df <- tidyr::pivot_wider(df, 
                         id_cols = c("source","term", "model","medianfup"),
                         names_from = "stat", 
                         values_from = "value")

# Make names match R output ----------------------------------------------------

df <- df[df$model=="max" | (df$model=="min" & df$term %in% c(unique(df$term[grepl("days",df$term)]),"1.sex","2.sex","age_spline1","age_spline2")) |
           (df$model == "AgeSexObesity" & df$term %in% c(unique(df$term[grepl("days",df$term)]),"1.sex","2.sex","age_spline1","age_spline2","cov_bin_obesity")),]

df <- df[order(df$source, df$model),
         c("source","term","model","b","lci","uci","se","medianfup","subjects","outcomes")]

df <- dplyr::rename(df,
                    "estimate" = "b",
                    "conf_low" = "lci",
                    "conf_high" = "uci",
                    "se_ln_hr" = "se",
                    "N_sample_size" = "subjects",
                    "median_follow_up" = "medianfup",
                    "N_outcomes" = "outcomes")

# Save output ------------------------------------------------------------------

readr::write_csv(df, paste0("output/stata_output_",name,"_pre_vax.csv"))