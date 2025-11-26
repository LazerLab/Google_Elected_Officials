# Script scrapes weblinks on Wikipedia's "List of NPR stations" and " List of stations owned or operated by Sinclair Broadcast Group"


setwd("~/internal_websearch")

library(httr)
library(XML)
library(tidyverse)

url<-"https://en.wikipedia.org/wiki/List_of_NPR_stations"
  
# making http request 
resource<-GET(url)
parse <-htmlParse(resource)
links1<-xpathSApply(parse,path = "//a",xmlGetAttr,"href") 

url<-"https://en.wikipedia.org/wiki/List_of_stations_owned_or_operated_by_Sinclair_Broadcast_Group"
  
# making http request 
resource<-GET(url)
parse <-htmlParse(resource)
links2<-xpathSApply(parse,path = "//a",xmlGetAttr,"href") 

links_df <- data.frame("domain" = c(unlist(links1), unlist(links2))) %>%
filter(grepl("/wiki/", domain) == TRUE) %>%
mutate(domain = paste(tolower(gsub("/wiki/|_\\(AM\\)|_\\(FM\\)|-FM|-TV|-LD", "",domain )), ".", sep = ""))%>%
filter(nchar(domain)<6) %>% distinct()

write.csv(links_df, "data/npr_m_st.csv", row.names = F)
