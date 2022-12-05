#Figure 4. Traditional measures of success by gender & offers

#CNS paper y/n

#4a----
fig4a_gen_table <- table(fig4_data$CNS_status, fig4_data$adjusted_gender)

fig4a_chi <- chisq.test(fig4a_gen_table)

fig4a_plot <- fig4_data %>% 
  count(adjusted_gender, CNS_status) %>% 
  spread(key = CNS_status, value = n) %>% 
  mutate(total = No + Yes,
         percent = get_percent(Yes, total)) %>% 
  ggplot(aes(x = adjusted_gender, y = percent))+
  geom_col(position = "dodge")+
  #facet_wrap(~gender, scales = "free")+
  labs(y = "Published in CNS (%)\n(n=156)", x = '',
       caption = "Pearson's Chi-squared test: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig4a_cns_gender.jpeg")

#4b----
fig4b_off_table <- table(fig4_data$CNS_status, fig4_data$faculty_offers)

fig4b_chi <- chisq.test(fig4b_off_table)

fig4b_plot <- fig4_data %>% 
  filter(!is.na(faculty_offers)) %>% 
  ggplot(aes(x = CNS_status))+
  geom_bar(position = "dodge")+
  facet_wrap(~faculty_offers)+
  labs(x = "Published in CNS\n(grouped by faculty offers)",
       y = "Number of responses\n(n=126)",
       caption = "Pearson's Chi-squared test: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig4b_cns_offers.jpeg")

#1st author CNS y/n
#4c----
fig4c_table <- table(fig4_data$CNS_status, fig4_data$adjusted_gender)

fig4c_chi <- chisq.test(fig4c_table)

fig4c_plot <- fig4_data %>% 
  filter(!is.na(faculty_offers)) %>% 
  filter(adjusted_gender %in% c("Woman", "Man")) %>% 
  #mutate(CNS_first_author = replace_na(CNS_first_author, "0")) %>%
  count(adjusted_gender, CNS_first_author) %>% 
  spread(key = CNS_first_author, value = n) %>% 
  mutate(total = `0`+`1`+`2`,
         `0` = get_percent(`0`, total),
         `1` = get_percent(`1`, total),
         `2` = get_percent(`2`, total)) %>% 
  gather(2:4, key = CNS_first_author, value = percent) %>% 
  ggplot(aes(x = CNS_first_author, y = percent))+
  geom_col(position = "dodge")+
  facet_wrap(~adjusted_gender)+
  labs(x = "Number of first-author CNS papers", y = "Percent of gender*\n(n=36)",
       caption = "Pearson's Chi-squared test: ns\n*N of Trans/GNC too low to visualize")+
  my_theme_horiz

ggsave("nafisa/figures/fig4c_cns_1st_gender.jpeg")

#4d----
fig4d_kwh <- kruskal.test(as.numeric(CNS_first_author) ~ faculty_offers, data = fig4_data)

fig4d_plot <- fig4_data %>% 
  filter(!is.na(faculty_offers)) %>%
  ggplot(aes(x = CNS_first_author))+
  geom_bar(position = "dodge")+
  facet_wrap(~faculty_offers)+
  labs(x = "Number of first-author CNS papers\n(grouped by faculty offers)", y = "Number of responses\n(n=126)",
       caption = "Kruskal-Wallis rank sum test: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig4d_cns_1st_offers.jpeg")

#compile stats----

chi_stats_list <- c("fig4a_chi", "fig4b_chi", "fig4c_chi")

plot_list <- c('4A', '4B', '4C')

fig4_chi_stats_tbl_raw <- map2_df(chi_stats_list, plot_list, get_wilcox_tbl) 

fig4_chi_stats_tbl <- fig4_chi_stats_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  select(figure, method, `statistic.X-squared`, p.value)

write_csv(fig4_chi_stats_tbl, "nafisa/figures/Fig4_Chi-sq_stats.csv")

#kw

fig4_kw_stats_tbl <- get_wilcox_tbl("fig4d_kwh", '4D') %>% 
  spread(key = attribute, value = value) %>% 
  select(-data.name)

write_csv(fig4_kw_stats_tbl, "nafisa/figures/Fig4_Kruskal-Wallis_stats.csv")

#generate plot----
Fig4ab <- plot_grid(fig4a_plot, fig4b_plot, 
                    labels = c('A', 'B'),
                    #rel_widths = c(.75, 1),
                    label_size = 18, nrow = 1)

Fig4cd <- plot_grid(fig4c_plot, fig4d_plot,
                    labels = c('C', 'D'),
                    label_size = 18, nrow = 1)

Fig4 <- plot_grid(Fig4ab, Fig4cd, nrow = 2)


ggsave("Figure_4.png", device = 'png', units = "in", scale = 1.75,
       path = 'nafisa/figures/', width = 6, height = 4)