library(tidyverse)
library(arrow)
library(xtable)
library(DescTools)

setwd("~/internal_websearch")

rbo_result <- read_parquet("data/rbo_result_r.parquet")
qry_info <- read_csv("data/qry_info_house.csv")

rbo_result_len <- rbo_result %>%
mutate(list_len_dif = abs(list_len1 - list_len2),
      list_len_avg = (list_len1 + list_len2)/2,
      total_setdiff = setdiff_1_2 + setdiff_2_1,
      type = case_when(type == "dif_state" ~ "Different State Pair",
                      type == "same_state" ~ "Same State Pair",
                      TRUE ~ "Home District to Random Pair"),
         type = factor(type, ordered = TRUE, levels = c("Different State Pair", "Same State Pair", "Home District to Random Pair")))

rbo_sum1 <- rbo_result_len%>%
mutate(rbo_1 = ifelse(rbo ==1, 1, 0),
      rbo_under_jaccard_1 = ifelse(jaccard_similarity ==1 & rbo <1, 1, 0),
      setdiff_1 = ifelse(total_setdiff ==1 & list_len_dif==1, 1, 0),
      setdiff_1_1 = ifelse(setdiff_1_2 ==1 & setdiff_2_1==1 & list_len_dif == 0, 1, 0),
      high_sim = ifelse(rbo_1 + rbo_under_jaccard_1 + setdiff_1+ setdiff_1_1 > 0, 1, 0),
      loc_pair= paste(pmin(loc_1, loc_2), 
                            pmax(loc_1, loc_2), 
                            sep = "_"),
      low_sim = ifelse(rbo< 0.4, 1,0))

rbo_sum <- rbo_sum1 %>%
group_by(type)%>%
summarise(rbo_1 = sum(rbo_1),
         rbo_under_jaccard_1 = sum(rbo_under_jaccard_1),
          setdiff_1 = sum(setdiff_1),
          setdiff_1_1 = sum(setdiff_1_1),
         total = n())%>%
mutate(rbo_prop = rbo_1/total,
      rbo_jaccard_prop = rbo_under_jaccard_1/total,
      setdiff_prop = setdiff_1/total,
       setdiff_1_1_prop = setdiff_1_1/total,
      sum_prop = rbo_prop + rbo_jaccard_prop + setdiff_prop+ setdiff_1_1_prop)

# Main Plot

ggplot(rbo_result_len %>%
       mutate(type = factor(type, levels = c("Different State Pair",
                                            "Same State Pair", "Home District to Random Pair"))), aes(x = rbo)) +
  geom_histogram(
    aes(y = after_stat(count) / tapply(..count.., ..PANEL.., sum)[..PANEL..] * 100),
    fill = "gray50", color = "white", bins = 30
  )+
labs(x = "Rank-Biased Overlap", y = "Percentage of Pairs")+
coord_fixed(1/100)+
facet_wrap(~type)+theme_bw(base_size = 18)

ggsave("figures/loc_plot.pdf", 
      width = 13, height = 5, units = "in")

# Table
rbo_sum_tab<- rbo_sum  %>% select(-ends_with("_1"))%>%
mutate_at(c("rbo_prop", "rbo_jaccard_prop", "setdiff_prop", "setdiff_1_1_prop", "sum_prop"),
          ~(paste0(as.character(round(.*100, 2)), "%")))

colnames(rbo_sum_tab) <- c("Pair Type", "Number of pairs sampled", "% Identical", 
                           "% Same result set, different order", "% One location longer by 1 result", 
                           "% Same length, 1 different result in each location", "% Highly similar pairs (Total)")

rbo_sum_tab = data.frame(t(rbo_sum_tab))%>% rownames_to_column("col")%>%
 purrr::set_names(as.character(slice(., 1))) %>% # Set names from first row
  slice(-1)   
rbo_sum_tab <- rbo_sum_tab[,c(1,2,4,3)]

print(xtable(rbo_sum_tab), 
      file= "~/internal_websearch/figures/rbo_sim_tab.txt", 
      include.rownames = FALSE)

# SI day plot

day_sum <- rbo_result_len %>% group_by(date_file, type)%>%
mutate(date_file = as.Date(gsub(".\\parquet", "", date_file), format = "%Y%m%d"))%>%
summarise(mean_rbo= mean(rbo),
          median_rbo = median(rbo),
          lci_rbo = quantile(rbo, 0.025),
          uci_rbo = quantile(rbo, 0.975),
         #jaccard = mean(jaccard_similarity),
          #len_dif = mean(list_len_dif),
          #len_avg = mean(list_len_avg),
         n = n())

ggplot()+
geom_pointrange(data = day_sum, aes(x = date_file, y = median_rbo, ymin = lci_rbo, ymax = uci_rbo),
                size = 0.25)+
geom_point(data = day_sum, aes(x = date_file, y =mean_rbo ), color = "red", size = 0.5)+
labs(x = "Day", y = "Rank-Biased Overlap")+
facet_wrap(~type, ncol = 1)+theme_bw(base_size = 18)

ggsave("figures/si_loc_sim_day.pdf", width = 10, height = 6, units = "in")

# Query level table
rbo_low_sim_gini <- rbo_sum1 %>% 
group_by(qry, type)%>%
mutate(qry_counts = n())%>%
group_by(type)%>%
mutate(total_counts = sum(low_sim))%>% # low_sim counts
group_by(type,total_counts, qry,qry_counts)%>%
summarise(counts = sum(low_sim))%>%
group_by(type) %>%
arrange(desc(counts))%>%
mutate(prop_qry = counts/qry_counts,
      rank = rank(desc(counts)))

temp <- rbo_low_sim_gini %>% filter(rank < 11 ) %>% arrange(type, rank) %>%
left_join(qry_info %>% select(qry, state, district, party))%>%
mutate(prop_qry = paste0(as.character(round(prop_qry*100,2)), "%"),
      rank = as.character(round(rank)),
       counts = round(counts, digits = 0),
      qry_district = paste0(qry, ", ", substring(party, 1, 1), 
                            " ", state, "-", as.character(district)))%>%

select(type,rank, qry_district, counts,prop_qry)

colnames(temp) <- c("Type","Rank", "House Member", "Num. Low Sim. Pairs", "% Low Sim. Pairs")

print(xtable(temp), 
      file= "~/internal_websearch/figures/low_sim_qry.txt", 
      include.rownames = FALSE)

rbo_long <- rbind(
    rbo_sum1 %>%
    rename("loc" = loc_1) %>% select(-loc_2),
    rbo_sum1 %>%
    rename("loc" = loc_2) %>% select(-loc_1)
)%>% filter(type != "home_dif")

rbo_low_sim_qry <- rbo_sum1 %>% 
group_by(qry, type)%>%
mutate(total_counts = n())%>%
group_by(type, qry,total_counts)%>%
summarise(counts = sum(low_sim))%>%
mutate(prop = counts/total_counts)%>%
group_by(type) %>%
summarise(gini = Gini(prop, na.rm = TRUE),
         max = max(prop), 
         min = min(prop),
         lci = quantile(prop, 0.025),
         uci = quantile(prop, 0.975))%>%
mutate(group_type = "qry")

rbo_low_sim_day <- rbo_sum1 %>% 
group_by(date_file, type)%>%
mutate(total_counts = n())%>%
group_by(type, date_file,total_counts)%>%
summarise(counts = sum(low_sim))%>%
mutate(prop = counts/total_counts)%>%
group_by(type) %>%
summarise(gini = Gini(prop, na.rm = TRUE),
         max = max(prop), 
         min = min(prop),
         lci = quantile(prop, 0.025),
         uci = quantile(prop, 0.975))%>%
mutate(group_type = "day")

rbo_low_sim_loc <- rbo_long %>% 
group_by(loc, type)%>%
mutate(total_counts = n())%>%
group_by(type, loc,total_counts)%>%
summarise(counts = sum(low_sim))%>%
mutate(prop = counts/total_counts)%>%
group_by(type) %>%
summarise(gini = Gini(prop, na.rm = TRUE),
         max = max(prop), 
         min = min(prop),
         lci = quantile(prop, 0.025),
         uci = quantile(prop, 0.975))%>%
mutate(group_type = "loc")

temp <- rbind(rbo_low_sim_qry,rbo_low_sim_day,rbo_low_sim_loc )%>% 
mutate(max = paste0(as.character(round(max*100,2)), "%"),
      min = paste0(as.character(round(min*100,2)), "%"),
      ci = paste0("(",as.character(round(lci*100,2)), "%, ", as.character(round(uci*100,2)), "%)"),
      gini = round(gini, 2))%>%
select(group_type, type, gini, max, min, ci)

colnames(temp) <- c("Group Type", "Pair Type", "Gini Coeff.", "Max.", "Min", "95% CI")
print(xtable(temp), 
      file= "~/internal_websearch/figures/low_sim_gini.txt", 
      include.rownames = FALSE)