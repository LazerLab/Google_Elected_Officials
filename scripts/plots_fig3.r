# Script produces Figure 3: Distribution of search result ranks by domain classifications.

setwd("~/internal_websearch")

library(tidyverse)
library(ggplot2)
library(cowplot)

ncol_facet = 1
qry_domain_rank_joined <- read_csv("data/qry_domain_rank_joined.csv")

type_df<- qry_domain_rank_joined %>%  
group_by(domain, serp_rank, type, poli_control, low_cred, is_local_news)%>%
summarise(counts = sum(counts))%>%
ungroup()

type_df_prop  <- rbind(type_df %>% 
select(counts, poli_control)%>%
pivot_longer(-counts, names_to = "cat", values_to = "part"),
type_df %>% 
filter(type == "news")%>%
select(counts, is_local_news, low_cred)%>%
pivot_longer(-counts, names_to = "cat", values_to = "part"))%>% 
group_by(cat, part)%>%
summarise(group_counts = sum(counts))%>%
group_by(cat) %>%
mutate(total_counts = sum(group_counts),
      prop = paste0(as.character(round((group_counts/total_counts) *100, digits = 1)), "%"),
      group_name = paste(cat, part, sep = "_"))

group_list <- type_df_prop$prop
names(group_list) <- type_df_prop$group_name

poli_control_vec <-  paste0("Politician Controlled\n(", 
                            group_list[["poli_control_1"]]," of All Results)")     
not_poli_control_vec <-  paste0("Not Politician Controlled\n(", 
                            group_list[["poli_control_0"]]," of All Results)")   

is_local_vec <-  paste0("Local News\n(", 
                            group_list[["is_local_news_1"]]," of News Results)")     
not_local_vec <-  paste0("National News\n(", 
                            group_list[["is_local_news_0"]]," of News Results)") 

low_cred_vec <-  paste0("Unreliable News\n(", 
                            group_list[["low_cred_1"]]," of News Results)")     
high_cred_vec <-  paste0("Reliable News\n(", 
                            group_list[["low_cred_0"]]," of News Results)") 
                      
type_sum <- type_df %>%
mutate(serp_rank = serp_rank+1,
       total_prop_news = (counts/sum(type_df%>%filter(type == "news") %>%pull(counts)))*100,
      total_prop = (counts/sum(type_df$counts))*100,
      poli_control = factor(ifelse(poli_control == TRUE,poli_control_vec, not_poli_control_vec),
                      levels = c( poli_control_vec, not_poli_control_vec)),
      classification_labelled = case_when(
          is_local_news == 1 ~ is_local_vec,
          is_local_news == 0 ~not_local_vec,
          TRUE ~ NA),
        low_cred = factor(ifelse(low_cred == 1, low_cred_vec, high_cred_vec),
                      levels = c( high_cred_vec, low_cred_vec)))

# A. Politician Controlled
plot1_df <- type_sum %>% 
       select(total_prop, counts, serp_rank,poli_control)%>%
       group_by(serp_rank,poli_control)%>%
       summarise(n = sum(total_prop, na.rm = TRUE),
                n_counts = sum(counts, na.rm = TRUE)) %>%
              group_by(poli_control)%>%
               mutate(group_counts = sum(n_counts),
                   mean_rank = sum(serp_rank*n_counts)/group_counts,
                     mean_label = ifelse(serp_rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA))

plot1<- ggplot(plot1_df, 
       aes(y = n, x = serp_rank))+
geom_col(fill = "gray50")+
       facet_wrap(~poli_control, ncol = ncol_facet)+ 
geom_vline(linetype = "dashed", linewidth = 1, aes(xintercept = mean_rank))+
geom_label(aes(label = mean_label, x = mean_rank +10), y = 5, label.size = 0.25, size = 4.5)+
labs(y = "Percentage of Total Results", x = "Result Rank")+
theme_bw(base_size = 18)+
theme(legend.position="none")

# A. Local or national news
plot2_df <- type_sum %>% select(total_prop_news,counts, serp_rank,classification_labelled)%>% 
       filter(is.na(classification_labelled) == FALSE)%>%
       group_by(serp_rank,classification_labelled)%>%
       summarise(n = sum(total_prop_news, na.rm = TRUE),
                 n_counts = sum(counts, na.rm = TRUE)) %>%
              group_by(classification_labelled)%>%
               mutate(group_counts = sum(n_counts),
                   mean_rank = sum(serp_rank*n_counts)/group_counts,
                     mean_label = ifelse(serp_rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA))

plot2<- ggplot(plot2_df,
       aes(y = n, x = serp_rank))+
geom_col(fill = "gray50")+
geom_vline(linetype = "dashed", linewidth = 1, aes(xintercept = mean_rank))+
geom_label(aes(label = mean_label, x = mean_rank +10), y = 10, label.size = 0.25, size = 4.5)+
scale_y_continuous(limits = c(0,12), breaks = c(0, 4, 8, 12))+
#scale_x_continuous(limits = c(0,15))+
facet_wrap(~classification_labelled, ncol = ncol_facet)+
labs(y = "Percentage of News Results", x = "Result Rank")+
theme_bw(base_size = 18)+
theme(legend.position="none")

# C. Reliable or unreliable news
plot3_df <- type_sum %>% 
select(total_prop_news,serp_rank,low_cred,counts )%>% 
       filter(is.na(low_cred) == FALSE )%>%
       group_by(serp_rank,low_cred)%>%
       summarise(n = sum(total_prop_news, na.rm = TRUE),
                 n_counts = sum(counts, na.rm = TRUE)) %>%
              group_by(low_cred)%>%
               mutate(group_counts = sum(n_counts),
                   mean_rank = sum(serp_rank*n_counts)/group_counts,
                     mean_label = ifelse(serp_rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA),
                     y_lab = ifelse(low_cred ==  low_cred_vec, 
                                    1, 10))

plot3<- ggplot(plot3_df,
       aes(y = n, x = serp_rank))+
geom_col(fill = "gray50")+
geom_vline(linetype = "dashed", linewidth = 1, aes(xintercept = mean_rank))+
geom_label(aes(label = mean_label, x = mean_rank +10, y = y_lab), label.size = 0.25, size = 4.5)+
facet_wrap(~low_cred, ncol = ncol_facet, scales = "free_y")+
labs(y = "Percentage of News Results", x = "Result Rank")+
theme_bw(base_size = 18)+
theme(legend.position="none")

plot_grid(plot1, plot2, plot3, ncol = 3,
          labels = c('A.', 'B.','C.'), 
          label_size = 20)
ggsave("figures/news_poli_local.pdf", width = 13, height = 8, units = "in")