# Script produces Figure 2: Composition, Concentration, and Ranking of Search Results

setwd("~/internal_websearch")

library(tidyverse)
library(ggplot2)
library(DescTools)
library(stats)
library(forcats)
library(xtable)
library(cowplot)

qry_domain_rank_joined <- read_csv("data/qry_domain_rank_joined.csv")%>% ungroup()

house_domain <- qry_domain_rank_joined %>%
group_by(domain,type) %>%
summarise(counts = sum(counts))%>%
ungroup()

domain_rank <- qry_domain_rank_joined %>% group_by(domain, serp_rank, type) %>%
summarise(counts = sum(counts))%>%
ungroup()

# formal names for each type
type<- c("campaign_personal","other_third_party","education","news","gov", "social" ,"total")
type_names <- c("Campaign/Personal", "Other Third Party", "Education","News", 
                "Government", "Social Media", "Total")
type_df <- data.frame(cbind(type, type_names))

# A. Percentage of unique domains and result types occupied by each result tye
house_domain2 <- house_domain%>%
group_by(type)%>%
summarise(t_count = sum(counts), n_unique = n())%>%
mutate(prop_count = t_count/sum(t_count),
      prop_unique = n_unique/sum(n_unique))%>%
pivot_longer(-type, names_to = "stat", values_to = "value")

plot1 <- ggplot(house_domain2 %>% left_join(type_df)%>%
       filter(stat %in% c("prop_unique", "prop_count"))%>%
      mutate(stat =factor(ifelse(stat == "prop_unique", "Percentage of\nTotal Unique Domains",
                             "Percentage of\nTotal Results"), ordered = TRUE,
                         levels = c("Percentage of\nTotal Results", "Percentage of\nTotal Unique Domains")),
            label = paste(round(value*100, digits = 1), "%")),
      aes(y = type_names, x = value*100, fill = type_names,label = label ))+
geom_col(width = 0.7, position = "dodge")+
geom_text(hjust = 0, nudge_x = 1, color = "gray30", size = 5)+
scale_fill_manual(values = c( "mediumpurple","plum2","mediumseagreen", "gold1","dodgerblue1","chocolate"))+
scale_y_discrete(limits=rev)+
scale_x_continuous(limits = c(0,105))+
theme_bw(base_size = 18)+
facet_wrap(~stat, strip.position = "bottom", )+
labs(y = "", x = "", fill = "Result Type")+
theme(legend.position="none",
      panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_blank(),
     strip.background = element_blank(),
      strip.text.x = element_text(size = 18),
           strip.placement = "outside")

# B. Cumulative percentage of total results
rank_cum_df <- house_domain%>%
ungroup()%>%
select(domain,counts) %>%
arrange(desc(counts))%>%
mutate(rank = rank(desc(counts)),
       prop = counts/sum(counts),
      cum_prop_counts = cumsum(counts)/sum(counts),
      rank_prop = rank/nrow(house_domain))

plot3 <- ggplot(rank_cum_df,aes(x = rank, y = cum_prop_counts*100))+geom_line()+
theme_bw(base_size = 18)+geom_abline(slope = 100/nrow(rank_cum_df), 
                       intercept = 0, 
                       linetype = "dashed")+
scale_y_continuous(limits = c(0,100))+
scale_x_continuous(breaks = seq(0,8000, 2000))+
coord_fixed(ratio = 40)+
labs(y = "Cumulative Percentage\nof Total Results", x = "Top X most frequent domains")

## C. Top 15 domains plot
top_15<- domain_rank %>%
mutate(weighted_rank = serp_rank*counts)%>%
group_by(domain,type)%>%
summarise(n_counts = sum(counts, na.rm = TRUE),
         sum_w_rank = sum(weighted_rank)) %>%
ungroup()%>%
arrange(desc(n_counts))%>%
mutate(rank = rank(desc(n_counts)),
       mean_rank = sum_w_rank/n_counts,
       prop = (n_counts/sum(n_counts))*100,
      domain_label = paste0(round(prop, digits = 1), "%, ", 
                    round(mean_rank, digits = 1)))

plot2 <- ggplot(top_15%>% left_join(type_df)%>%
       mutate(domain = paste(rank, ". ", domain, sep = ""))%>%
       filter(rank <16),
     aes(y = fct_reorder(domain, prop), x = prop, fill = type_names, 
         label = domain_label ))+
geom_col()+
scale_fill_manual(values = c("mediumseagreen", "gold1","dodgerblue1","chocolate"))+
geom_text(hjust = 0, nudge_x = 0.3, color = "gray30", size = 5)+
scale_x_continuous(limits = c(0,32))+
theme_bw(base_size = 18)+
  theme(legend.position = "inside",
    legend.justification.inside = c(0.95, 0.05),
        legend.title = element_text(size = 16), 
               legend.text = element_text(size = 14),
        panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_blank()
  )+
labs(y = "15 Most Frequent Domains", x = "Percentage of Total Results", fill = "Type")

## D. Rank by type plot
rank_df_sum <- domain_rank%>%
mutate(serp_rank= serp_rank+1)%>%
    group_by(serp_rank,type)%>%
    summarise(n_counts = sum(counts, na.rm = TRUE)) %>%
    group_by(type)%>%
    mutate(group_counts = sum(n_counts), 
        total_prop = (n_counts/group_counts)*100,
        mean_rank = sum(serp_rank*n_counts)/group_counts,
        mean_label = ifelse(serp_rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA))

plot4 <- ggplot(rank_df_sum %>%left_join(type_df), 
       aes(x = serp_rank, y = total_prop, fill = type_names))+
geom_col()+
geom_vline(aes(xintercept = mean_rank), color = "black", linetype = "dashed", linewidth = 1)+
geom_label(aes(label = mean_label, x = mean_rank +4), y = 25, label.size = 0.25, size = 4.5,fill="white"  )+
facet_wrap(~type_names, ncol = 2)+
scale_fill_manual(values = c( "mediumpurple","plum2","mediumseagreen", "gold1","dodgerblue1","chocolate"))+
theme_bw(base_size = 18)+theme(legend.position = "none")+
labs(x = "Result Rank", y = "Percentage of Results")

plot_grid1 <- plot_grid(plot1, plot3, ncol = 2,  rel_widths = c(1.6,1),
          labels = c('A.', 'B.'), 
          label_size = 18)

plot_grid2 <- plot_grid(plot2, plot4, ncol = 2, rel_widths = c(1.2,1),
          labels = c('C.', 'D.'), 
          label_size = 18)

plot_grid(plot_grid1, plot_grid2, ncol = 1, rel_heights = c(1,1.5),
          labels = c('', '','', ''), 
          label_size = 12)
ggsave("figures/fig2.pdf", width = 13, height = 10, units = "in")

# Table S2 in SI, Statistics for each type

## Gini Coefficient
house_gini_total <- house_domain %>%
arrange(desc(counts))%>%
mutate(rank = rank(desc(counts)),
       prop = counts/sum(counts),
      cum_prop_counts = cumsum(counts)/sum(counts),
      rank_prop = rank/nrow(house_domain),
      type = "total")

house_gini_total_mini <- tibble(
    domain = NA,
    type = "total",
    counts = 0,
    rank = 0,
    prop = 0,
    cum_prop_counts = 0,
    rank_prop = 0)

house_gini_total_mini <- house_gini_total_mini[, names(house_gini_total)]
house_gini_total <- rbind(house_gini_total, house_gini_total_mini)%>%
mutate(gini = Gini(house_domain$counts))

for(t in unique(house_domain$type)){
    house_gini_type <- house_domain %>%
    filter(type == t)
    
   house_gini_type2 <- house_gini_type %>%
arrange(desc(counts), .by_group = TRUE)%>%
mutate(rank = rank(desc(counts)),
       prop = counts/sum(counts),
      cum_prop_counts = cumsum(counts)/sum(counts),
      rank_prop = rank/nrow(house_gini_type)) 

    house_gini_type_mini <- tibble(
    domain = NA,
    type = t,
    counts = 0,
    rank = 0,
    prop = 0,
    cum_prop_counts = 0,
    rank_prop = 0)
    
    house_gini_type_mini <- house_gini_type_mini[, names(house_gini_type2)]
    
    house_gini_type2 <- rbind(house_gini_type_mini,house_gini_type2)%>%
    mutate(gini = Gini(house_gini_type$counts))
    
    house_gini_total  <- rbind(house_gini_total,house_gini_type2 )
}

house_gini_total <- house_gini_total%>% 
mutate_at(c("counts", "rank", "prop", "cum_prop_counts", "rank_prop"), as.numeric)

house_gini_type <- house_gini_total%>%select(type, gini) %>% distinct()

rank_df_type <- domain_rank %>%
mutate(serp_rank = serp_rank+1,
      weighted_sum = serp_rank*counts) %>%
group_by(type)%>%
summarise(n_domains = length(unique(domain)),
          weighted_sum_total = sum(weighted_sum), 
      n_results = sum(counts))%>%
ungroup()%>%
mutate(mean_rank = weighted_sum_total/ n_results,
       prop_domains= n_domains/sum(n_domains),
       prop_results = n_results/sum(n_results)
      ) %>% select(-weighted_sum_total)  

total_df_type <- as.data.frame(t(c("total", 
                                   sum(rank_df_type$n_domains), 
                                   sum(rank_df_type$n_results),
                                   sum(rank_df_type$n_results * rank_df_type$mean_rank)/ sum(rank_df_type$n_results),
                                   sum(rank_df_type$prop_domains), 
                                   sum(rank_df_type$prop_domains)))) %>%
mutate_at(c("V2", "V3", "V4", "V5", "V6"), as.numeric)

colnames(total_df_type) <-  names(rank_df_type)
                      
type_df_total <- rbind(rank_df_type,total_df_type) %>% 
mutate(domain_prop_count =paste(as.character(n_domains),
                                " (",
                                as.character(round(prop_domains*100, digits = 1)), 
                                "%)",sep = ""),
      result_prop_count =paste(as.character(n_results),
                                " (",
                                as.character(round(prop_results*100, digits = 1)), 
                                "%)",sep = ""),
      mean_rank_round = round(mean_rank, digits = 1)) %>%
select(-starts_with(c("n_", "prop")), -mean_rank) %>% 
left_join(house_gini_type)%>% 
mutate(gini = round(gini, digits = 2))%>%
left_join(type_df) %>% select(-type) 
type_df_total <- type_df_total[,c(5,2,1,3,4)]


print(xtable(type_df_total), 
      file= "figures/domain_type_tab.txt", 
      include.rownames = FALSE)
