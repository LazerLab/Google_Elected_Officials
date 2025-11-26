setwd("~/internal_websearch")
source("scripts/repo/get_weighted_ks.r")

library(tidyverse)
library(ggplot2)
library(cowplot)


INPUT_PATH = "/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files/cmpt_rank/merged_summary"
qry_domain_rank_si <- read.csv(paste0(INPUT_PATH, "/qry_domain_rank_si.csv"))

qry_domain_rank_joined <- read_csv("data/qry_domain_rank_joined.csv")%>% ungroup()

house_domain <- qry_domain_rank_joined %>%
group_by(domain,type) %>%
summarise(counts = sum(counts))%>%
ungroup()

domain_rank <- qry_domain_rank_joined %>% group_by(domain, serp_rank, type) %>%
summarise(counts = sum(counts))%>%
ungroup()

house_domain_si <- qry_domain_rank_si %>%
group_by(domain) %>%
summarise(counts_si = sum(counts))%>%
ungroup()%>% left_join(house_domain)%>%
pivot_longer(starts_with("counts"), names_to = "si", values_to = "counts")%>%
mutate(si = ifelse(si == "counts", "Main", "SI"))

domain_rank_si <- rbind(qry_domain_rank_si %>% group_by(domain, cmpt_rank) %>%
summarise(counts= sum(counts))%>%
rename("rank" = cmpt_rank)%>%  
                        select(domain, rank, counts)%>%
mutate(si = "SI")%>%
ungroup(),
                       domain_rank %>% rename("rank" = serp_rank)%>%  
                        select(domain, rank, counts) %>%
                       mutate(si = "Main")) %>%
left_join(qry_domain_rank_joined %>% 
          select(domain, type, partisan_score, local_score, low_cred, poli_control) %>% distinct())%>%
mutate( rank= rank+1, weighted_rank = rank*counts)

# formal names for each type
type<- c("campaign_personal","other_third_party","education","news","gov", "social" ,"total")
type_names <- c("Campaign/ Personal", "Other Third Party", "Education","News", 
                "Government", "Social Media", "Total")
type_df <- data.frame(cbind(type, type_names))

# A
house_domain2 <- house_domain_si%>%
group_by(type, si)%>%
summarise(t_count = sum(counts))%>%
group_by(si)%>%
mutate(prop_count = t_count/sum(t_count),
      label = paste(" ",round(prop_count*100, digits = 1), "%"))%>%
left_join(type_df)

plot1 <- ggplot(house_domain2 ,
      aes(y = type_names, x = prop_count*100, fill = type_names,label = label, group = si ))+
geom_col(aes(alpha = si),width = 0.7, position = "dodge")+
geom_text(hjust = 0, color = "gray30", size = 5, 
          position = position_dodge(width = .7))+
#geom_text(hjust = 0, nudge_x = 1, color = "gray30", size = 5)+
scale_fill_manual(values = c( "mediumpurple","plum2","mediumseagreen", "gold1","dodgerblue1","chocolate"))+
scale_alpha_manual(values = c(0.4,1))+
scale_y_discrete(limits=rev)+
scale_x_continuous(limits = c(0,105))+
theme_bw(base_size = 18)+
labs(x = "Percentage of Total Results", y = "", alpha = "")+
theme(panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_blank(),
      legend.position = "inside",
          legend.justification.inside = c(0.95, 0.05),
        legend.title = element_text(size = 16), 
               legend.text = element_text(size = 14)) + guides(fill = "none")

# B
rank_cum_df <- rbind(house_domain%>%
ungroup()%>%
select(domain,counts) %>%
arrange(desc(counts))%>%
mutate(rank = rank(desc(counts)),
       prop = counts/sum(counts),
      cum_prop_counts = cumsum(counts)/sum(counts),
      rank_prop = rank/nrow(house_domain),
      si = "Main"),
       house_domain_si%>%
ungroup()%>%
 filter(si == "SI")%>%
select(domain,counts) %>%
arrange(desc(counts))%>%
mutate(rank = rank(desc(counts)),
       prop = counts/sum(counts),
      cum_prop_counts = cumsum(counts)/sum(counts),
      rank_prop = rank/nrow(house_domain),
      si = "SI"))    

plot2 <- ggplot(rank_cum_df,aes(x = rank, y = cum_prop_counts*100, color = si))+
geom_line()+
theme_bw(base_size = 18)+geom_abline(slope = 100/nrow(rank_cum_df), 
                       intercept = 0, 
                       linetype = "dashed")+
scale_y_continuous(limits = c(0,100))+
scale_x_continuous(breaks = seq(0,8000, 2000))+
scale_color_manual(values = c("grey50","black"))+
coord_fixed(ratio = 40)+
labs(y = "Cumulative Percentage\nof Total Results", x = "Top X most frequent domains", color = "")+
    theme(legend.position = "inside",
          legend.justification.inside = c(0.95, 0.05),
        legend.title = element_text(size = 16), 
               legend.text = element_text(size = 14))

# C
top_15<- domain_rank_si %>%
mutate(weighted_rank = rank*counts)%>%
group_by(domain,type,si)%>%
summarise(n_counts = sum(counts, na.rm = TRUE),
         sum_w_rank = sum(weighted_rank)) %>%
group_by(si)%>%
arrange(desc(n_counts))%>%
mutate(rank = rank(desc(n_counts)),
       mean_rank = sum_w_rank/n_counts,
       prop = (n_counts/sum(n_counts))*100,
      domain_label = paste0(round(prop, digits = 1), "%"))

plot3 <- ggplot(top_15%>% left_join(type_df)%>%
       mutate(domain = paste(rank, ". ", domain, sep = ""))%>%
       filter(rank <16),
     aes(y = fct_reorder(domain, prop), x = prop, fill = type_names, 
         label = domain_label ))+
geom_col()+
scale_fill_manual(values = c("mediumseagreen", "gold1","dodgerblue1","chocolate"))+
geom_text(hjust = 0, nudge_x = 0.3, color = "gray30", size = 5)+
scale_x_continuous(limits = c(0,32))+
facet_wrap(~si, scales = "free_y")+
theme_bw(base_size = 18)+
  theme( legend.title = element_text(size = 16), 
               legend.text = element_text(size = 14),
        panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_blank()
  )+
labs(y = "15 Most Frequent Domains", x = "Percentage of Total Results", fill = "Type")

# D
rank_df_sum <- domain_rank_si%>%
    group_by(rank,type, si)%>%
    summarise(n_counts = sum(counts, na.rm = TRUE)) %>%
    group_by(type, si)%>%
    mutate(group_counts = sum(n_counts), 
        total_prop = (n_counts/group_counts)*100,
        mean_rank = sum(rank*n_counts)/group_counts,
        mean_label = ifelse(rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA))

plot4 <- ggplot(rank_df_sum %>%left_join(type_df), 
       aes(x = rank, y = total_prop, fill = type_names))+
geom_col()+
geom_vline(aes(xintercept = mean_rank), color = "black", linetype = "dashed", linewidth = 1)+
geom_label(aes(label = mean_label, x = mean_rank +4), y = 25, label.size = 0.25, size = 4.5,fill="white"  )+
facet_grid(rows = vars(type_names), cols = vars(si), scales = "free_x", labeller = label_wrap_gen(width = 15))+
scale_fill_manual(values = c( "mediumpurple","plum2","mediumseagreen", "gold1","dodgerblue1","chocolate"))+
theme_bw(base_size = 18)+theme(legend.position = "none")+
labs(x = "Result Rank", y = "Percentage of Results") 


plot_grid1 <- plot_grid(plot1, plot2, ncol = 1, align = 'v', axis = 'l', #rel_widths = c(1,1),
          labels = c('A.', 'B.'), 
          label_size = 18)

plot_grid2 <- plot_grid(plot_grid1, plot4, ncol = 2, rel_widths = c(0.8,1),
          labels = c('', 'D.'), 
          label_size = 18)

plot_grid(plot_grid2, plot3, ncol = 1, rel_heights = c(1.8,1),
          labels = c('', 'C.'), 
          label_size = 18)
ggsave("figures/si_cmpt_rank.pdf", width = 13, height = 15, units = "in")     

# Politician control
poli_control_df <- domain_rank_si %>%
group_by(poli_control)%>%
summarise(n_counts = sum(counts, na.rm = TRUE),
         sum_w_rank = sum(weighted_rank))%>%
mutate(mean_rank = sum_w_rank/n_counts)
print(poli_control_df)

# Partisanship
dem_rep <- qry_domain_rank_si%>%
left_join(qry_domain_rank_joined %>% 
          select(qry, domain, party, partisan_score, type) %>% distinct())%>%
    group_by(domain, party, partisan_score, type)%>%
    filter(party != "Independent")%>%
    summarise(counts = sum(counts))%>%
    ungroup()%>%
    mutate(total_counts = sum(counts))%>%
group_by(party)%>%
mutate(prop = counts/total_counts)%>% filter(is.na(partisan_score) == FALSE)

all_res_d <- dem_rep  %>% filter(party == "Democrat")
all_res_r <- dem_rep  %>% filter(party == "Republican")

w_ks_all <- weighted_ks_test(all_res_d$partisan_score, all_res_r$partisan_score,
                             w_x = all_res_d$prop, w_y = all_res_r$prop, B = 1000)

out_comb <- paste0("weighted ks: ",w_ks_all$statistic,", p=val: ",w_ks_all$p.value, sep = "")
print(out_comb)

