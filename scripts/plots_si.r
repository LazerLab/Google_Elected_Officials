# This script produces figures and tables in the SI

setwd("~/internal_websearch")

library(tidyverse)
library(ggplot2)
library(stats)
library(xtable)
library(DescTools)
library(cowplot)
library(estimatr)
library(scales)

### Functions
get_partisan_plot <- function(varname){
    plot <- ggplot(qry_sum_total_type %>% filter(group == varname), 
       aes(x = mean*100, y = type_names, fill = party,
                              label = paste0(round(mean*100, digits = 1), "%", sep = ""),
                        xmin = lci*100, xmax = uci*100))+
geom_col(position = "dodge", width = 0.7, alpha = 0.5)+
geom_text(aes(x = (uci*100)+1.5 ),hjust = 0, color = "gray30", size = 5, 
          position = position_dodge(width = .7))+
geom_errorbar(width = 0.3, position = position_dodge(0.7))+
    scale_y_discrete(limits=rev)+
scale_x_continuous(limits = c(0,112), breaks = c(0,25,50,75,100))+
scale_fill_manual(values = c( "dodgerblue4","red"))+ 
    theme_bw(base_size = 18)+
theme(legend.position = "top", 
      panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_blank())
    return(plot)
    }

get_mod_plot <- function(varname, sf){
    plot <-ggplot(df_mod_plot %>% filter(group == varname), 
       aes(x = estimate*sf, y =type_names, xmin = lci*sf, xmax = uci*sf))+
scale_y_discrete(limits=rev)+
geom_pointrange()+theme_bw(base_size = 18)+
geom_vline(xintercept = 0, linetype = "dashed")
    
    return(plot)
    }

### Nicely fomrmatted variable names for plots
type<- c('is_local_news_no','is_local_news','poli_control_no','poli_control',
         'low_cred_no','low_cred','local_na_no','local_na','low_cred_na_no',
         'low_cred_na','is_news_no','is_news',
         "campaign_personal","other_third_party","education","news",
         "gov", "social" ,"total","total_gini", "news_gini", "total_qry_counts", "total_news_qry_counts"
        )
type_names <- c("National News","Local News", "Not Politician\nControlled", "Politician\nControlled", 
                "Reliable News", "Unreliable News", "No\nClassification","No\nClassification","No\nClassification",
                "No\nClassification","News", "Not News",
                "Campaign/Personal", "Other Third Party", "Education","News", 
                "Government", "Social Media", "Total", "Source Concentration\nof All Results",
               "Source Concentration\nof News Results", "Total Number\nof Results",
                "Total Number\nof News Results")
type_names_df <- data.frame(cbind(type, type_names))

### Load files and pre-process


INPUT_PATH = "/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files/merged_summary"

qry_domain_rank_joined <- read_csv("data/qry_domain_rank_joined.csv")

# Google Trends Data
gtrends_df <- read_csv("data/gtrends_final_20200109_20201231.csv")

demand_gini <- gtrends_df%>%
rename("qry" = keyword) %>% 
filter(qry %in% qry_domain_rank_joined$qry) %>%
group_by(qry)%>%
  mutate(region_rank = rank(desc(hits), ties = "random"))%>%
  replace_na(list(hits = 0))%>%
  summarise(demand_gini = DescTools::Gini(hits))

qry_final <- qry_domain_rank_joined %>%
group_by(qry, domain, state, district, party, relevance_score, type, is_local_news, low_cred, poli_control,
        local_score, local_score_news, partisan_score, partisan_score_news)%>%
summarise(counts = sum(counts))%>%
group_by(qry)%>%
mutate(local_na = ifelse(is.na(is_local_news) == TRUE & type == "news", 1,0),
      low_cred_na = ifelse(is.na(low_cred) == TRUE& type == "news", 1,0),
      total_qry_counts = sum(counts),
      total_news_qry_counts = sum(counts[type == "news"], na.rm = TRUE))

qry_sum <- data.frame()
for(col in c("is_local_news", "poli_control", "low_cred", "local_na",
              "low_cred_na")){
    qry_sum_temp <- qry_final %>%
    select(qry, counts, party,total_qry_counts,total_news_qry_counts, !!sym(col)) %>%
    drop_na()%>%
    group_by(qry, party,total_qry_counts,total_news_qry_counts, !!sym(col))%>%
    summarise(sum_counts = sum(counts))%>%
    #filter(!!sym(col) ==1) %>%
    mutate(stat_type = ifelse( !!sym(col) == 1,col, paste(col, "_no", sep = "") ) )%>%
    select(-!!sym(col))
    
    qry_sum <- rbind(qry_sum, qry_sum_temp) 
              }

### Time variation

# Counts by query and domain and day
# this data is not provided publically at this time
qry_day_domain <- read.csv(paste0(INPUT_PATH, "/day_qry_url.csv"))

qry_day_domain_joined <- qry_day_domain %>%
select(-type)%>%
group_by(crawl_id, qry, domain) %>% 
summarise(counts  = sum(counts))%>%
left_join(qry_final %>% select(-counts)) %>%
group_by(crawl_id, qry) %>% 
mutate(total_day_qry_counts = sum(counts),
      crawl_id = gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",as.character(crawl_id)),
      crawl_id = as.Date(crawl_id)) %>% 
ungroup()

qry_day_domain_joined2 <- qry_day_domain_joined %>%
group_by(crawl_id, qry,party, type, total_day_qry_counts) %>%
summarise(sum_counts = sum(counts)) %>%
mutate(prop = sum_counts/total_day_qry_counts) %>%
left_join(type_names_df)

qry_day_domain_joined3 <- qry_day_domain_joined2 %>%
group_by(crawl_id, type_names) %>%
summarise(mean = mean(prop),
         lci = quantile(prop, 0.025),
         uci = quantile(prop, 0.975))

plot1 <- ggplot(qry_day_domain_joined2 %>% 
       filter(qry %in% c("Nancy Pelosi","Kelly Armstrong", "Eric Swalwell", "Abby Finkenauer" )) %>% 
       filter(type_names != "Education"),
       aes(x = crawl_id, y = prop*100, group = qry, color = type_names))+
geom_line()+
scale_color_manual(values = c( "mediumpurple","mediumseagreen", "gold3","dodgerblue1","chocolate"))+
facet_grid(cols = vars(type_names), rows = vars(qry))+
theme_bw(base_size = 18)+ theme(legend.position = "none")+
labs(y = "Percentage of Total Results", x = "Day")

plot2 <- ggplot(qry_day_domain_joined3, 
       aes(x = crawl_id, y = mean*100, ymin = lci*100, ymax = uci*100, 
           fill = type_names, color = type_names))+
geom_line()+
geom_vline(xintercept = as.Date("2020-11-03"), linetype = "dashed", alpha = 0.7)+
geom_ribbon(alpha = 0.2, color = NA)+
scale_color_manual(values = c( "mediumpurple","plum2","mediumseagreen", "gold3","dodgerblue1","chocolate"))+
scale_fill_manual(values = c( "mediumpurple","plum2","mediumseagreen", "gold1","dodgerblue1","chocolate"))+
scale_y_continuous(limits = c(0,60), breaks = c(0,20, 40, 60))+
facet_wrap(~type_names, ncol = 3)+
labs(y = "Avg. Percentage of Total Results", x = "Day")+
theme_bw(base_size = 18)+ theme(legend.position = "none")

plot_grid(plot2, plot1, ncol = 1, rel_heights = c(1, 1.5), labels = c("A.", "B."),
         label_size= 20, align = "v", axis = "lr")
ggsave("figures/si_time.pdf", 
      width = 13, height = 14, units = "in")

### Partisan Plot
qry_sum_type<- qry_sum %>% 
filter(grepl("na_no", stat_type) == FALSE)%>%
mutate(group = gsub("is_|_no|_na|_news", "", stat_type),
       prop = ifelse(group %in% c("local", "low_cred"),
                     sum_counts/total_news_qry_counts,
                     sum_counts/total_qry_counts)) %>% 
group_by(party, stat_type, group)%>%
summarise(mean = mean(prop), lci = quantile(prop, 0.025), uci = quantile(prop, 0.975))%>%
filter(party != "Independent") 

qry_sum_category <- qry_final %>%
    group_by(qry, party,total_qry_counts, type)%>%
    summarise(sum_counts = sum(counts)) %>% 
    mutate(prop = sum_counts/total_qry_counts)

qry_sum_total_type <- qry_sum_category %>% 
    group_by(party, type)%>%
    summarise(mean = mean(prop), lci = quantile(prop, 0.025), uci = quantile(prop, 0.975))%>%
    filter(party != "Independent") %>%
rename("stat_type" = type)%>%
mutate(group = "type")%>%
rbind(qry_sum_type)%>%
left_join(type_names_df %>% rename("stat_type" = type))%>%
mutate(type_names = factor(type_names, ordered = TRUE, 
                      levels = c("Campaign/Personal",  "Education", 
                "Government","News","Other Third Party", "Social Media",  "Politician\nControlled","Not Politician\nControlled", "Not News",
                                 "Local News", "National News", "Reliable News", "Unreliable News", 
                                "No\nClassification")),
      ci_dif = uci - lci)

sf = 2.5
plot_cat<- get_partisan_plot("type")+ 
labs(y = "", x = "Percentage of Total Results", fill = "")#+coord_fixed(6*sf+4)

plot_pc <- get_partisan_plot("poli_control")+ 
labs(y = "", x = "Percentage of Total Results", fill = "")#+coord_fixed(2*sf+4)

plot_lc <- get_partisan_plot("low_cred")+ 
labs(y = "", x = "Percentage of News Results", fill = "")#+coord_fixed(3*sf+4)

plot_local <- get_partisan_plot("local")+ 
labs(y = "", x = "Percentage of News Results", fill = "")#+coord_fixed(3*sf+4)

grobs <- ggplotGrob(plot_local)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
                                                        
left <- plot_grid(plot_cat+ theme(legend.position = "none"), 
                  plot_pc+ theme(legend.position = "none"), ncol = 1,
                  rel_heights = c(2,1),
                  align="v", axis = "l", labels = c("A.", "B."), label_size = 20 )
                              
right <- plot_grid( plot_local + theme(legend.position = "none"),
                   plot_lc+ theme(legend.position = "none"),legend, ncol = 1,
                  rel_heights = c(2.75,2.75, 1),
                  align="v", axis = "l", labels = c("C.", "D.", ""), label_size = 20)
                              
plot_grid(left, right, rel_widths = c(1,1.05))
ggsave("figures/si_partisan.pdf", 
      width = 13, height = 8, units = "in")

### Gini Plot    
gini_total = data.frame()
for(t in unique(qry_sum$stat_type)){
    qry_sum_type <- qry_sum %>%
    filter(stat_type == t)  %>% ungroup()
    
   qry_sum_type2 <- qry_sum_type %>%
arrange(desc(sum_counts), .by_group = TRUE)%>%
mutate(rank = rank(desc(sum_counts)),
       prop = sum_counts/sum(sum_counts),
      cum_prop_counts = cumsum(sum_counts)/sum(sum_counts)) %>%
    mutate(gini = Gini(qry_sum_type$sum_counts))
    
    gini_total  <- rbind(gini_total,qry_sum_type2 )
}
qry_sum_category_gini <- qry_final %>%
    group_by(qry, party,total_qry_counts,total_news_qry_counts, type)%>%
    summarise(sum_counts = sum(counts)) %>%
ungroup()%>% group_by(type)%>%
arrange(desc(sum_counts), .by_group = TRUE)%>%
mutate(rank = rank(desc(sum_counts)),
       prop = sum_counts/sum(sum_counts),
      cum_prop_counts = cumsum(sum_counts)/sum(sum_counts)) %>%
    mutate(gini = Gini(sum_counts)) %>%
rename("stat_type" = type)

qry_sum_category_gini <- qry_sum_category_gini[, c(1,2,3,4,6,5,7,8,9,10)]

gini_total_plot <- rbind(gini_total, qry_sum_category_gini)%>% 
filter(!(stat_type %in% c("is_news", "is_news_no", "local_na_no", "local_na",
                         "low_cred_na_no", "low_cred_na"))) %>%
mutate(res_type =ifelse(stat_type %in% c('is_local_news_no','is_local_news','low_cred_no','low_cred'),
                       "News Results", "All Results"))%>%
left_join(type_names_df %>% rename("stat_type" = type))%>%
       mutate(type_names = factor(type_names, ordered = TRUE, 
                     levels = c("Campaign/Personal",  "Education", 
                "Government","News","Other Third Party", "Social Media",  "Politician\nControlled","Not Politician\nControlled", "Not News",
                                 "Local News", "National News","Reliable News", "Unreliable News", 
                                "No\nClassification")),
             gini_text =ifelse(rank == 1, paste("Gini = ", signif(gini, 2), sep = ""), NA) )

plot1 <- ggplot(gini_total_plot %>% filter(res_type == "All Results"),# %>% left_join(type_df), 
      aes(x = rank, y = cum_prop_counts*100, label = gini_text))+
geom_label(x = 300, y = 10)+
geom_line()+
geom_abline(slope = 100/417, intercept = 0, linetype = "dashed", color = "gray50")+
facet_wrap(~type_names, ncol = 4)+
coord_fixed(4.17)+theme_bw(base_size = 18)+
labs(y = "Cumulative Percentage of Total Results", 
     x = "Top X Members")

plot2<- ggplot(gini_total_plot %>% filter(res_type == "News Results"),# %>% left_join(type_df), 
      aes(x = rank, y = cum_prop_counts*100, label = gini_text))+
geom_label(x = 300, y = 10)+
geom_line()+
geom_abline(slope = 100/417, intercept = 0, linetype = "dashed", color = "gray50")+
facet_wrap(~type_names, ncol = 2)+
coord_fixed(4.17)+theme_bw(base_size = 18)+
labs(y = "Cumulative Percentage of News Results", 
     x = "Top X Members")

plot_grid(plot1, plot2, ncol = 2, rel_widths = c(1.8,1), 
          labels = c("A.", "B."), label_size = 20 )

ggsave("figures/si_gini_member.pdf", 
      width = 13, height = 6, units = "in")

### Distribution of demand gini

gtrends_df_sum <- gtrends_df %>%
  replace_na(list(hits = 0))%>%
  group_by(keyword)%>%
  summarise(gini = Gini(hits))%>%
  rename("qry" = keyword) %>%
  left_join(qry_domain_rank_joined %>% select(qry, state, district, party) %>% distinct(), 
            by = "qry")%>%
  ungroup()%>% 
  mutate(keyword_state = paste0(qry, ", ", substring(party, 1, 1), 
                            " ", state, "-", as.character(district)),
         total_rank = rank(gini, ties.method = "first"),
         keyword_text1 = ifelse(total_rank %in% seq(1, 604, 5) & total_rank%%2 ==1, keyword_state, "" ),
         keyword_text2 = ifelse(total_rank %in% seq(1, 604, 5) & total_rank%%2 ==0, keyword_state, "" ))

ggplot(gtrends_df_sum, aes(x = gini, y= total_rank))+
  #scale_color_manual(values = c("gray30", "dodgerblue", "firebrick2"))+
  geom_point(size = 0.75, color = "gray50")+
  geom_text(aes(label = keyword_text1),
            hjust = 0, nudge_x = 0.03, size = 4.5 )+
  geom_text(aes(label = keyword_text2),
            hjust = 1, nudge_x = -0.03, size = 4.5 )+
  scale_x_continuous(limits = c(-0.25, 1.3), breaks = c(0, 0.25, 0.5, 0.75, 1))+
  theme_bw(base_size = 18)+
    theme(panel.grid.minor = element_blank())+
  labs(y = "House member (Ranked by Gini Coefficient)", x = "Gini Coefficient of Google Trends search volume across 50 states and DC")
ggsave("figures/si_gini_distribution.pdf", 
      width = 13, height = 15, units = "in")

### Regression Plot

qry_sum_joined <- rbind(qry_sum %>% 
                        mutate(group = gsub("is_|_no|_na|_news", "", stat_type),
                               prop = ifelse(group %in% c("local", "low_cred"),
                                              sum_counts/total_news_qry_counts,
                                              sum_counts/total_qry_counts)) %>%
                        ungroup()%>%
                              select(qry, stat_type, prop),
                       qry_sum_category %>% rename("stat_type" = type)%>%
                        ungroup()%>%
                              select(qry, stat_type, prop))%>% 
pivot_wider(names_from = stat_type, values_from = prop, values_fill = 0)
                              
total_gini <- qry_final %>%
group_by(qry) %>%
summarise(total_gini = DescTools::Gini(counts) ) 

stats_wide <- qry_final %>%
filter(type == "news")%>%
group_by(qry) %>%
summarise(news_gini = DescTools::Gini(counts),
          total_qry_counts = mean(total_qry_counts),
          total_news_qry_counts = mean(total_news_qry_counts),
         across(c(local_score, local_score_news, partisan_score, partisan_score_news), ~mean(.x, na.rm = TRUE)) ) %>%
left_join(total_gini)%>%left_join(demand_gini %>% select(-starts_with("region")))%>%
left_join(qry_sum_joined)  #%>%
#select(-ends_with(c("_no", "_na"))) %>%
#select(-low_cred, -is_news, -education)

col_list <- c("news_gini", "total_gini", "total_qry_counts", "total_news_qry_counts", "is_local_news",'is_local_news_no','low_cred', 'low_cred_no',"education",
             "poli_control", "campaign_personal", "gov", "news", "other_third_party", "social")


outlist <- list()
for(col in col_list){
mod<- lm_robust(reformulate("demand_gini", col), data = stats_wide)
outlist[[length(outlist)+1]]<-  c(col, summary(mod)$r.squared,
summary(mod)$adj.r.squared ,
summary(mod)$coefficients[2], #estimate
summary(mod)$coefficients[8], # pval
confint(mod)[2,1], #lci
confint(mod)[2,2])#uci
}
df_mod <- as.data.frame(do.call(rbind, outlist))
colnames(df_mod) <- c("var","r_sq","adj_r_sq","estimate", "pval", "lci", "uci")
df_mod <- df_mod %>%
mutate_at(c("r_sq","adj_r_sq","estimate", "pval", "lci", "uci"), as.numeric)%>% 
mutate(sig = ifelse(pval < 0.05, 1, 0))

df_mod_plot <- df_mod %>%  
left_join(type_names_df %>% rename("var"=type))%>%
mutate(type_names = factor(type_names, ordered = TRUE, 
                      levels = c("Campaign/Personal",  "Education",  "Government", "News", 
                                 "Other Third Party","Social Media",  "Politician\nControlled",
                                 "Local News", "National News", "Reliable News", "Unreliable News", 
                                 "Source Concentration\nof All Results",
               "Source Concentration\nof News Results", "Total Number\nof Results", "Total Number\nof News Results")),
      group = case_when(
          var %in% c("campaign_personal", "gov", "news", 
                     "other_third_party", "social", "education", "poli_control") ~ "A_Total Res",
          var %in% c("news_gini", "total_gini") ~ "C_gini",
          var %in%  c("total_qry_counts", "total_news_qry_counts") ~ "D_counts",
          TRUE ~"B_News"
      ))

write.csv(df_mod_plot, "figures/regression_df.csv", row.names = FALSE)  
write.csv(stats_wide, "data/stats_wide.csv", row.names = FALSE)                         

plot1 <- get_mod_plot("A_Total Res", 100) +
labs(y = "", x = "Diff. in Percentage of Total Results")

plot2 <- get_mod_plot("B_News", 100)+
labs(y = "", x = "Diff. in Percentage of News Results")#+
#scale_x_continuous(limits = c(-15, 15))

plot3 <- get_mod_plot("C_gini", 1)+
labs(y = "", x = "Diff. in Gini Coefficient")

plot4 <- get_mod_plot("D_counts", 1)+
labs(y = "", x = "Diff. in Total Number of Results")+
scale_x_continuous(labels = label_comma())

right <- plot_grid(plot2,plot3, plot4, ncol = 1 , rel_heights = c(3.5, 2.5, 2.5),
                  labels = c("B.", "C.", "D."),label_size = 20, align = "v", axis = "l")
plot_grid(plot1, right, ncol = 2, labels = c("A.", ""),label_size = 20, rel_widths = c(1, 1.2))

ggsave("figures/si_demand_gini_regression.pdf", 
      width = 13, height = 6, units = "in")

### Regression Table
                              
df_mod_plot_tab <-  df_mod_plot %>% 
mutate(type_order = as.numeric(type_names))%>%
arrange(group, type_order)%>% 
mutate_if(is.numeric, signif, 3) %>%
mutate_if(is.numeric, as.character) %>%
mutate(estimate = paste(estimate," (", lci, ", ", uci, ")", sep = ""),
      pval = ifelse(sig == 1, paste(pval, "*", sep = ""), pval))%>%
select(-r_sq,-var, -group, -lci, -uci, -sig, -type_order)
df_mod_plot_tab <- df_mod_plot_tab[,c(4, 2, 3, 1)]
colnames(df_mod_plot_tab) <- c("Outcome", "Estimate (95%CI)", "P-value", "Adj. R-Sq.")

print(xtable(df_mod_plot_tab), 
      file= "figures/demand_gini_tab.txt", 
      include.rownames = FALSE)

### Top 10 Members with largest share of education results

ed_domains <- qry_domain_rank_joined %>% filter(type == "education")

ed_qry <- gini_total_plot %>% filter(stat_type == "education") %>% filter(rank < 11) %>%
select(rank, qry, prop, cum_prop_counts) 
print("N unique ed members")                       
print(length(unique(gini_total_plot %>% filter(stat_type == "education") %>% pull(qry))))
                              
qry_domain_join <- qry_domain_rank_joined %>% filter(domain %in% ed_domains$domain) %>%
filter(qry %in% ed_qry$qry) %>% group_by(qry) %>% summarise(domains = paste(unique(domain), collapse = ", "))

ed_qry_sum <- ed_qry %>% left_join(qry_domain_join, by = "qry") %>%
left_join(qry_domain_rank_joined %>% select(qry, party, state, district) %>% distinct())%>%
mutate(keyword_state = paste0(qry, ", ", substring(party, 1, 1), 
                            " ", state, "-", as.character(district))) %>% 
mutate_at(c("prop", "cum_prop_counts"), round, 2)%>%
select(-qry, -party, -state, -district)

ed_qry_sum <- ed_qry_sum[, c(1,5,2,3,4)]
colnames(ed_qry_sum) <- c("Rank", "House Member", "Prop.", "Cum. Prop.", "Domains")

print(xtable(ed_qry_sum), 
      file= "figures/ed_domains_tab.txt", 
      include.rownames = FALSE)

### Top 10 Members with largest share of unreliable news results
                              
low_cred_qry <- gini_total_plot %>% filter(stat_type == "low_cred") %>% filter(rank < 11) %>%
select(rank, qry, prop, cum_prop_counts) 
print("N unique low_cred members")                       
print(length(unique(gini_total_plot %>% filter(stat_type == "low_cred") %>% pull(qry))))                       
                              
qry_domain_join <- qry_domain_rank_joined %>% filter(low_cred ==1) %>%
filter(qry %in% low_cred_qry$qry) %>% group_by(qry) %>% summarise(domains = paste(unique(domain), collapse = ", "))

low_cred_qry_sum <- low_cred_qry %>% left_join(qry_domain_join, by = "qry") %>%
left_join(qry_domain_rank_joined %>% select(qry, party, state, district) %>% distinct()) %>%
mutate(keyword_state = paste0(qry, ", ", substring(party, 1, 1), 
                            " ", state, "-", as.character(district))) %>% 
mutate_at(c("prop", "cum_prop_counts"), round, 2)%>%
select(-qry, -party, -state, -district)

low_cred_qry_sum <- low_cred_qry_sum[, c(1,5,2,3,4)]
colnames(low_cred_qry_sum) <- c("Rank", "House Member", "Prop.", "Cum. Prop.", "Domains")

print(xtable(low_cred_qry_sum), 
      file= "figures/low_cred_news_tab.txt", 
      include.rownames = FALSE)                              
