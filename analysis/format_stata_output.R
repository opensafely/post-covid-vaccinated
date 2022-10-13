# List files to be combined

files <- list.files(path = "output/", pattern = "_cox_model.txt")

# Create empty master data frame

df <- NULL

# Append each file to master data frame

for (f in files) {
  
  ## Load data
  
  tmp <- readr::read_tsv(file = paste0("output/",f), skip = 2,
                         col_names = c("term",
                                       "b_min","se_min","t_min","lci_min","uci_min","p_min",
                                       "b_max","se_max","t_max","lci_max","uci_max","p_max"))
  
  ## Make variables numeric
  
  tmp$b_min <- as.numeric(tmp$b_min)
  tmp$se_min <- as.numeric(tmp$se_min)
  tmp$t_min <- as.numeric(tmp$t_min)
  tmp$lci_min <- as.numeric(tmp$lci_min)
  tmp$uci_min <- as.numeric(tmp$uci_min)
  tmp$p_min <- as.numeric(tmp$p_min)
  tmp$b_max <- as.numeric(tmp$b_max)
  tmp$se_max <- as.numeric(tmp$se_max)
  tmp$t_max <- as.numeric(tmp$t_max)
  tmp$lci_max <- as.numeric(tmp$lci_max)
  tmp$uci_max <- as.numeric(tmp$uci_max)
  tmp$p_max <- as.numeric(tmp$p_max)
  
  ## Add source
  
  tmp$source <- f
  
  ## Seperate info from estimates
  
  info_terms <- c("risk","N_fail","N_sub","N","N_clust")
  info <- tmp[tmp$term %in% info_terms,c("source","term","b_min","b_max")]
  info <- dplyr::rename(info, "min" = "b_min", "max" = "b_max")
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
                             values_from = c("min","max"),
                             names_glue = "{term}_{.value}")
    
  ## Merge info and estinates
  
  tmp <- merge(tmp, info, by = "source")
  
  ## Add median follow up

  f <- gsub("_cox_model.txt","_stata_median_fup.csv",f)
  fup <- readr::read_csv(file = paste0("output/",f))
  tmp <- merge(tmp, fup, by = "term", all.x = TRUE)
  
  ## Apend to master dataframe
    
  df <- rbind(df, tmp)
    
}

# Format master dataframe

df <- df[,c("source","term","medianfup",
            paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_min"),
            paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_max"))]

df <- tidyr::pivot_longer(df, 
                          cols = c(paste0(c("b","se","t","lci","uci","p","persondays","outcomes","subjects","observations","clusters"),"_min"),
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

df <- df[df$model=="max" | (df$model=="min" & df$term %in% c("days0_28","days28_197","1.sex","2.sex","age_spline1","age_spline2")),]

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

write.csv(df, "output/stata_output.csv")