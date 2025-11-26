setwd("~/internal_websearch")
library(tidyverse)

# 6 random urls per qry from sample from "~/websearch/intermediate_files/preprocessing/sanity_check/unique_results2.csv"
relevance_raw <- read_csv("data/r_validation_1st_coder.csv") 
qry_info <- read_csv("data/qry_info_house_orig.csv")

relevance <- relevance_raw %>% group_by(qry) %>% summarise(relevance_score = sum(aw_relevance_score))

qry_info_final <- qry_info %>% left_join(relevance) %>%
mutate(house_analysis_include = ifelse(in_data == 0|

                                       # remove relevance score less than 3
                                      relevance_score <3 | 

                                       # remove territories
                                      state %in% c("AS", "GU", "MP", "PR", "VI") |
                                       
                                       # out of office by 9/1/2020
                                      qry %in% c('Duncan D. Hunter', 'John Ratcliffe', 'Mark Meadows'), 
                                     0,1))
write.csv(qry_info_final,"data/qry_info_house.csv", row.names = FALSE )

 