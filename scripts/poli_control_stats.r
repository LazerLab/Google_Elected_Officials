setwd("~/internal_websearch")
library(tidyverse)

labeled_folder = "data/qry_account_pairs_labeled/"

files = list.files(labeled_folder)

df_long = data.frame()
for(file in files){
  type = gsub("qry_account_pairs_|.csv", "", file)
  df <- read.csv(paste0(labeled_folder ,file))
    if ("total_counts" %in% colnames(df)) {
  colnames(df)[colnames(df) == "total_counts"] <- "counts"
}
    
    df <- df %>%
    group_by(poli_control) %>%
    summarise(total_counts = sum(counts),
              total_qrys = length(unique(qry)), total_qry_account_pairs = n()) %>%
    mutate(domain = type)
  
  df_long <- rbind(df_long, df)
}

df_long_sum <- df_long %>%
  group_by(domain) %>%
  mutate(total = sum(total_counts),
                     prop = total_counts/total)
df_long_sum <- df_long_sum[,c(5,1,2,3,4,6,7)]

write.csv(df_long_sum, "data/final_poli_control_stats.csv", row.names = FALSE)