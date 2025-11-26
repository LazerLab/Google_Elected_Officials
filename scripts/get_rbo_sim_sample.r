# create sampling framework for similarity across locations of search
setwd("~/internal_websearch")
library(tidyverse)
library(arrow)

get_final_samples <- function(big_df, date_list, n_df_samples, n_date_samples){
    sample_pairs <- sample_n(big_df, n_df_samples, replace = FALSE)
    
    sample_dates <- replicate(nrow(sample_pairs), 
                     sample(date_list, n_date_samples, replace = FALSE), simplify = "matrix")

    final_samples <- cbind(sample_pairs, as.data.frame(t(sample_dates)))%>%
    pivot_longer(starts_with("V"),names_to = "sample_index", values_to = "date_file")%>%
    select(-sample_index)

    return(final_samples)
}

qry_info <- read_csv("data/qry_info_house.csv") 

all_date_files <- list.files("/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files/house_parquet_no_na")

qry_info_prep <- qry_info%>%
mutate(district_str = ifelse(district == 0, "AT LARGE", as.character(district)),
      home_district = paste(state, district_str, sep = "-")) %>%
filter(!(state %in% c("AS", "GU", "MP", "PR", "VI"))) 

all_pairs <- as.data.frame(t(combn(unique(qry_info_prep$home_district), 2)))
colnames(all_pairs) <- c("loc_1", "loc_2")

all_pairs <- all_pairs %>%
mutate(state1 = substr(loc_1, start = 1, stop = 2),
      state2 = substr(loc_2, start = 1, stop = 2),
      same_state = ifelse(state1 == state2, 1, 0))%>%
filter(loc_1 != loc_2)

qry_list = qry_info_prep %>% filter(house_analysis_include == 1) %>%
                              pull(qry)

dif_state_pairs <- all_pairs %>% filter(same_state == 0)
same_state_pairs <- all_pairs %>% filter(same_state == 1)

print(paste0("total dif state pairs", as.character(nrow(dif_state_pairs))))
print(paste0("total same state pairs", as.character(nrow(same_state_pairs))))

big_samples_df <- data.frame()
for(q in qry_list){
    hp <- qry_info_prep %>% filter(qry == q) %>% pull(home_district)
    home_pairs <- all_pairs %>% filter(loc_1 == hp | loc_2 == hp)

    qry_samples <- rbind(get_final_samples(dif_state_pairs, all_date_files, 500, 20)%>%
                             mutate(type = "dif_state"),
                         
                         get_final_samples(same_state_pairs, all_date_files, 100, 20)%>%
                             mutate(type = "same_state"),
                         
                         get_final_samples(home_pairs, all_date_files, 100, 20)%>%mutate(type = "home_dif"))%>%
    mutate(qry = q)

    big_samples_df <- rbind(big_samples_df, qry_samples)
                              }

write_parquet(big_samples_df %>% arrange(date_file), "data/rbo_sim_sampling.parquet")