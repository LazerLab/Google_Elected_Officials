setwd("~/internal_websearch")

library(tidyverse)
library(httr)
library(jsonlite)

INPUT_PATH = "/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files/merged_summary"
OUTPUT_PATH = "data/qry_account_pairs_unlabeled/"

day_qry_url <- read_csv(paste0(INPUT_PATH, "/day_qry_url.csv"))

qry_url_sum <- day_qry_url %>% 
group_by(qry,url,domain, type)%>%
summarise(total_counts = sum(counts))

# Twitter
print("Twitter")
qry_info_twitter <- read_csv("data/qry_info_twitter.csv")%>% select(qry, Twitter)%>% 
rename("account" = Twitter)%>%
mutate(account = tolower(account), match = 1)

twitter_handles <- qry_url_sum %>% filter(domain == "twitter.com")%>%
separate_wider_delim(url,delim = regex("/status/|\\?ref|\\?lang") , 
                     names = c("url_clean", "junk"),too_few = "align_start",too_many = "drop")%>%
filter(grepl( "https://twitter.com/search", url_clean) == FALSE) %>%
mutate(url_clean = tolower(url_clean)) %>%
mutate(account = gsub("https://twitter.com/","", url_clean))%>%
group_by(qry, url_clean, account)%>% summarise(counts = sum(total_counts), n_unique_urls = n())%>%
left_join(qry_info_twitter, by = c("qry", "account")) %>% replace_na(list(match = 0))

write.csv(twitter_handles, paste0(OUTPUT_PATH,"qry_account_pairs_tw.csv"),
          row.names = FALSE)

# Facebook
print("Facebook")
facebook_accounts <- qry_url_sum %>% filter(domain == "facebook.com")%>%
mutate(account = gsub("https://.*facebook.com/","", tolower(url))) %>%
mutate(account = gsub("\\/.*", "", account))%>%
mutate(url_clean = paste0("https://www.facebook.com/", account))%>%
group_by(qry, url_clean, account)%>% summarise(counts = sum(total_counts), n_unique_urls = n())

write.csv(facebook_accounts, paste0(OUTPUT_PATH,"qry_account_pairs_fb.csv"),
          row.names = FALSE)

# house.gov
print("House.gov")
house_accounts <- qry_url_sum %>% filter(domain == "house.gov")%>%
mutate(url_clean = paste0(gsub("\\.gov.*", "", url),".gov"))%>%
mutate(url_clean = gsub("http.*//", "", url_clean))%>%
group_by(qry, url_clean)%>% summarise(counts = sum(total_counts), n_unique_urls = n())

write.csv(house_accounts, paste0(OUTPUT_PATH,"qry_account_pairs_hg.csv"),
          row.names = FALSE)

# Linkedin
print("Linkedin")
write.csv(qry_url_sum %>% filter(domain == "linkedin.com"),
          paste0(OUTPUT_PATH,"qry_account_pairs_linkedin.csv"),row.names = FALSE)

# Instagram
print("Instagram")
write.csv(qry_url_sum %>% filter(domain == "instagram.com"),
          paste0(OUTPUT_PATH,"qry_account_pairs_insta.csv"),row.names = FALSE)

# Youtube
print("Youtube")

# Scrape youtube channel names
KEY = "AIzaSyATfRVTla_Ws3t_XjlHaq06l15pwy8MCME"
BASE = "https://www.googleapis.com/youtube/v3/"

yt_data <- qry_url_sum %>% filter(domain == "youtube.com")

scraped_yt_filename = "data/scraped_yt_channel_ids.csv"
if(file.exists(scraped_yt_filename) == TRUE){
    video.df_long <- read.csv(scraped_yt_filename)
    yt_data_filter <- yt_data %>% filter(!(url %in% video.df_long$url))
}else{
    video.df_long <- data.frame(
        items.snippet.channelId = character(),
        items.snippet.title = character(),
        items.snippet.description = character(),
        items.snippet.channelTitle = character(),
        url = character(),
        stringsAsFactors = FALSE)
    
    yt_data_filter <- yt_data
}

start = 1
count = start
for(url in unique(yt_data_filter$url)[start: length(unique(yt_data_filter$url))]){
#for(url in unique(yt_data$url)[start:(start+5)]){
  result <- tryCatch({
  video_id <- gsub("https://www.youtube.com/watch\\?v=", "", url)
  print(paste0(count, ": ",video_id))
  
  api_params <- 
    paste(paste0("key=", KEY), 
          paste0("id=", video_id), 
          "part=snippet", sep = "&")
  
  api_call <- paste0(BASE, "videos", "?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
      
  parsed_json <- fromJSON(json_result, flatten = TRUE)

    if (length(parsed_json$items) > 0) {
      video.df <- as.data.frame(parsed_json) %>%
        select(items.snippet.channelId, items.snippet.title, 
               items.snippet.description, items.snippet.channelTitle) %>%
        mutate(url = url)
    } else {
      video.df <- data.frame(
        items.snippet.channelId = NA,
        items.snippet.title = NA,
        items.snippet.description = NA,
        items.snippet.channelTitle = NA,
        url = url
      )
    }
  
  video.df},
  
  error = function(e){
    message("Error with URL: ", url)
    message("Error message: ", e$message)
    NULL
    })
  
  if (!is.null(result)) {
    video.df_long <- rbind(video.df_long, result)
  }
  
  count  = count +1
}

write.csv(video.df_long, scraped_yt_filename, row.names = FALSE)

# keyword match some urls
third_party_vec <- c(" News", "NowThis", "MsMojo", "American Action Network",
                     "C-SPAN", "VEVO", "CBS", "NBC", "HBO", "CNN", "HuffPost", " Television")
third_party_string <- paste(third_party_vec, collapse ="|")

yt_data_final <- yt_data %>% left_join(video.df_long, by = "url") %>%
  mutate(channel_name = ifelse(is.na(items.snippet.channelTitle), url, items.snippet.channelTitle)) %>%
  group_by(qry, channel_name) %>%
  summarise(counts = sum(total_counts), n_unique_videos = n(),
            titles = as.character(list(items.snippet.title))) %>%
  mutate(poli_control = ifelse(grepl(third_party_string, channel_name ) == TRUE, 0, NA))
  
write.csv(yt_data_final, paste0(OUTPUT_PATH,"qry_account_pairs_yt.csv"), row.names = FALSE)

