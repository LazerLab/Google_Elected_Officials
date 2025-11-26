setwd("~/internal_websearch")
library(tidyverse)
library(xtable)

INPUT_PATH = "/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files/merged_summary"

qry_domain_rank <- read_csv(paste0(INPUT_PATH, "/qry_domain_rank.csv"))

house_domain_final <- read_csv("data/house_domain_final.csv")

pas <-read.csv(gzfile("data/derived_party_leaning.csv.gz"), header = TRUE)%>%
rename("partisan_score" = leaning_score)

local_score <- read.csv(gzfile("data/derived_state_kl.csv.gz"), header = TRUE) %>%
rename("local_score" = kl_divergence)

newsguard_raw <- read_csv("data/newsguard_metadata_Sep21.csv")

qry_info <- read_csv("data/qry_info_house.csv") 

# Join all domain level classifications
newsguard <- newsguard_raw%>%
rename("mini_domain" = Domain, "parent_domain" = `Parent Domain`, "ng_score" = Score)%>%
select(mini_domain, parent_domain, Country, ng_score)%>%
pivot_longer(ends_with("domain"),names_to = "domain_type", values_to = "domain")%>%
drop_na()%>%
filter(!(domain == "vice.com" & Country %in% c("IT", "DE","FR", "ALL")))%>%
filter(!(domain == "msn.com" & Country %in% c("IT", "DE","FR", "ALL")))%>%
select(-domain_type, -Country)%>%
distinct() 

house_domain <- house_domain_final%>%
select(-categories_flagged, -title, -matched)%>%
    pivot_longer(c(news, education, other_third_party, campaign_personal, social, gov),
                names_to = "type", values_to = "value")%>%
filter(value == 1)%>% select(-value) %>%
left_join(newsguard)%>%
left_join(pas)%>%
left_join(local_score)%>%
mutate(poli_control = ifelse(type == "campaign_personal" |
                             domain %in% c("facebook.com", "house.gov", "instagram.com", "linkedin.com", "twitter.com"), 1,0),
      low_cred = ifelse(ng_score < 60, 1, 0),
       low_cred = ifelse(type == "news", low_cred, NA ),
       local_score_news = ifelse(type == "news", local_score, NA ),
       partisan_score_news = ifelse(type == "news", partisan_score, NA ),
      is_local_news = ifelse(local_score > 0.243, 1, 0 ),
      is_local_news = ifelse(type == "news", is_local_news, NA ))

qry_domain_rank_joined <- qry_domain_rank %>%
left_join(qry_info %>% select(qry, state, district, party, relevance_score)%>% distinct(), by = "qry")%>%
left_join(house_domain%>% select(-counts), by = "domain")

write.csv(qry_domain_rank_joined, "data/qry_domain_rank_joined.csv", row.names = FALSE)

# Cover rates table
dem_rep <- qry_domain_rank_joined %>% group_by(domain, party) %>%
summarise(counts = sum(counts))

get_coverage_summary <- function(var_name, news = TRUE){
    var <- rlang::sym(var_name)
    
    type_df_domain <- qry_domain_rank_joined %>% 
filter(type == "news")
    if(news == FALSE){
        type_df_domain <- qry_domain_rank_joined
        var_name = paste(var_name, "no_news", sep = "_")
    }
type_df_domain <- type_df_domain %>%
group_by(domain)%>% 
summarise(n_results = sum(counts),
          sum_rank = sum(serp_rank*counts),
          var_mean = mean(!!var)) %>%
mutate(covered = ifelse(is.na(var_mean)==TRUE, paste(var_name, "0", sep = "_"),
                        paste(var_name, "1", sep = "_"))) %>%
left_join( dem_rep %>%
pivot_wider(names_from = party, values_from = counts) %>%
replace_na(list(Republican = 0, Democrat = 0, Independent = 0))) %>%
group_by(covered)%>%
summarise(n_domains = n(),
          n_res = sum(n_results), 
          mean_counts = mean(n_results),
         sum_rank_total = sum(sum_rank),
         sum_dem = sum(Democrat),
        sum_rep = sum(Republican) ) %>%
mutate(domain_prop = n_domains/sum(n_domains),
       res_prop = n_res/sum(n_res),
       mean_counts = as.character(round(mean_counts, digits =0)),
      mean_rank = as.character(round(sum_rank_total/n_res, digits =1)),
      percent_dem =paste0(as.character(round((sum_dem/n_res)*100, digits = 1)),"%"),
       percent_rep =paste0(as.character(round((sum_rep/n_res)*100, digits = 1)),"%"),
      domain_n_prop = paste0(n_domains, " (",
                           as.character(round(domain_prop*100, digits = 1)),
                           "%)"),
      results_n_prop = paste0(as.character(round(n_res/1000000, digits =1)), " M (",
                           as.character(round(res_prop*100, digits = 1)),
                           "%)"))%>% 
select(-starts_with(c("sum_", "n_")), -domain_prop, -res_prop)

type_df_domain <-type_df_domain[, c( 1,6,7,2,3,4,5)]
    
return(type_df_domain)}


out_table <- rbind(get_coverage_summary("is_local_news"),
      get_coverage_summary("low_cred"),
      get_coverage_summary("partisan_score"),
     get_coverage_summary("partisan_score", news = FALSE))

colnames(out_table)<- c("","Num. Domains(%)", "Num. Results(%)", 
                       "Avg. Domain Freq.", "Avg. Rank", "% Dem. Results", "% Rep. Results")

print(xtable(out_table), 
      file= "figures/coverage_newsdom_tab.txt",digits = 1,
     include.rownames = FALSE)