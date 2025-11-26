# Script classifies domains with keyword matching

setwd("~/internal_websearch")

library(tidyverse)
library(rvest)
library(R.utils)

# we use the file with domains and counts, not provided here.
# to replicate, read in "house_domain_final_122024.csv" and select the first 2 columns, "domain" and "counts"
INPUT_PATH = "/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files/merged_summary"
house_domain_raw <- read_csv(paste0("/domain_counts.csv"))

# news domains from Robertson et al. (2018)
news_lists <- read_csv("data/news_domains_robertson.csv") 

# news domains from Clemm von Hohenberg et al. (2021)
news_lists2<- read_csv("data/news_domains_cvh.csv")
news_list <- unique(c(news_lists$domain, news_lists2$domain))

# NPR and Sinclair stations
m_st_df <- read_csv( "data/npr_m_st.csv")

news_vec <- c(m_st_df$domain, "news","weekly","magazine","sentinel","public ?radio", "gazette","observer",
              "public ?media","tribune","chronicle","herald","wire_","mag_","radio_","times_", "daily_",
              "enquirer", "dispatch", "countystar", "freepress","reporter", "bulletin", "dailymail",
             "businessreview", "publicpress", "digest_", "star_", "tv_", "forum", "post_", "insider_",
             "telegraph", "community ?journal", "community journal","voiceof", "banner", "broadcasting") 
news_string<- paste(news_vec,collapse = "|" )

third_party_vec <- c("wikipedia",
                     "funeral","realtor","real ?estate","law ?firm","wayfair","clinic","photo",
                     "music","lawyer","lawgroup","athletics","dentist","mortgage","mortuary","design",
                     "primary ?care","museum","violin","theatre","lawncare","hospital",
                     "tripadvisor", "law ?office", "consulting", "consultants?","medical ?group", "physician ?network","medical ?center", "llp", "llc","foundation","festival",
                    "nationalgalleries", "chiroprac", "hotdogs", "painting", "heating","flooring", "dealership","legal ?group",
                     "contractors?","hotel","physical_?ther","gallery", "pottery",
                     "realty", "familychiro", "cemetery", "construction", "peoplefinders", "sagaftra", "conference",
                     "uaw", "nasdaq", "dental", "attorney", "walmart", "legal", "obgyn", "architect",
                     "landscape", "banker", "counseling", "health_", "insurance", "farm ?and ?ranch",
                    "tavern", "property ?group", "healthalliance","artworks", "massage", "contracting", 
                     "company", "medgroup", "chirohealth", "theater", "senior ?living",
                    "ministries", "furniture", "orthopa?edic")
third_party_string<- paste(third_party_vec,collapse = "|" )

campaign_personal_string <- "for ?congress|4congress|4senate|for ?senate|for u.s. congress|for u.s. senate|for ?mayor"

social_media_string <- "twitter|facebook|instagram|youtube|linkedin|reddit"

house_domain <- house_domain_raw %>%
mutate(news = ifelse(domain %in% news_lists$domain == TRUE,1, 0))   

out1 <- paste("Matched from news datasets:",
      as.character(table(house_domain$news)[[2]], collapse = ","))

house_domain <- house_domain %>%
mutate(news = ifelse(news == 1 | grepl(news_string,domain) == TRUE,1, 0),
    education = ifelse(news == 1 | grepl("\\.edu$",domain) == FALSE, 0, 1),
    social = ifelse(news == 1 | education == 1 | grepl(social_media_string, domain) == FALSE, 0, 1),
    gov = ifelse(news == 1 |education == 1 | social == 1 | grepl("\\.gov$", domain) == FALSE, 0, 1),  
    campaign_personal = ifelse(
        news == 1 | education == 1 | social == 1 | gov == 1 |
        grepl(campaign_personal_string,domain) == FALSE, 0, 1),
    other_third_party = ifelse(
        news == 1 | education == 1 | social == 1 | gov == 1 | campaign_personal ==1 | 
        grepl(third_party_string,domain) == FALSE,0,1), 
       categories_flagged = news+education+campaign_personal+ social + gov+other_third_party,
      matched = ifelse( categories_flagged > 0, 1, 0))

out2 <- paste("Matched from keywords:",
      as.character(table(house_domain$matched)[[2]], collapse = ","))

house_domain_to_check <- house_domain %>% filter(matched == 0)
house_domain_labelled <- house_domain %>% filter(matched == 1) %>% mutate(title = NA)

domain_title_filename =  "data/domain_title_scraped.csv"
# this file is equivalent to the title column in "house_domain_final.csv"
if(file.exists(domain_title_filename) == TRUE){
    domain_title <- read.csv(domain_title_filename)
}else{
    i=1
    outlist <- list()
    for(domain in house_domain_to_check$domain){
        url <- paste("https://www.", domain, sep = "")
        print(i)
        html<- withTimeout(try(read_html(url)) , timeout = 20 )

        if(isTRUE(class(html)=="try-error")) {title<- NA }else{
        title <- html %>% html_node("head title") %>% html_text()}
        outlist[[length(outlist)+1]]<- c(domain, title)
        i=i+1
    }

domain_title <- as.data.frame(do.call(rbind, outlist))%>%
  distinct()
colnames(domain_title) <- c("domain", "title")
write.csv(domain_title,domain_title_filename, row.names = FALSE)}

house_domain_to_check <- house_domain_to_check %>% 
left_join(domain_title, by = "domain")%>%
mutate(title = tolower(title),
    news = ifelse(grepl(news_string,title) == FALSE,0, 1),
    education = ifelse(news == 1 | grepl("\\.edu",domain) == FALSE, 0, 1),
    social = ifelse(news == 1 | education == 1 | grepl(social_media_string, domain) == FALSE, 0, 1),
    gov = ifelse(news == 1 |education == 1 | social == 1 | grepl("\\.gov", domain) == FALSE, 0, 1),  
    campaign_personal = ifelse(
       news == 1 | 
        grepl(campaign_personal_string,title) == FALSE, 0, 1),
    other_third_party = ifelse(
        news == 1 | campaign_personal==1 |
        grepl(third_party_string,title) == FALSE,0,1), 
       categories_flagged = news+education+campaign_personal+ social + gov+other_third_party,
      matched = ifelse( categories_flagged> 0, 1, 0))


house_domain_pre_handcode <- rbind(house_domain_to_check, house_domain_labelled)

out3 <- paste("Matched from title scraping:",
      as.character(table(house_domain_pre_handcode$matched)[[2]], collapse = ","))

out_comb <- paste(out1, out2,out3, sep = ",")
cat(out_comb, 
    file = "data/house_domain_output.txt")

write.csv(house_domain_pre_handcode, "data/house_domain_pre_handcode.csv", row.names = FALSE)
