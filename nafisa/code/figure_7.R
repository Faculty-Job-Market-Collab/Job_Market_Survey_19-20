
#career transition award: y/n
ct_data <- fig6_data %>% 
  filter(question == "transition_award") %>% distinct()

fig7_ct_wilcox <- wilcox.test(perc_offers ~ response,
                              data = ct_data,
                              na.rm=TRUE, paired=FALSE, 
                              exact=FALSE, conf.int=TRUE)

fig7a_plot <- ct_data %>% 
  ggplot(aes(x = response, y = perc_offers, 
             color = response))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Career Transition\nAward (n=189)", y="Percent offers",
       subtitle = "Mann-Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig7a.png")

#postdoc fellowship: y/n
fellow_data <- fig6_data %>% 
  filter(question == "grants_awarded") %>% distinct() %>% 
  select(-question) %>% 
  filter(response != "Transition to Independence Award ") %>% 
  mutate(status = "Yes",
         response = str_remove_all(response, "(?<=Grant).*")) %>% 
  distinct() %>% 
  spread(key = response, value = status) %>% 
  mutate(across(3:5, ~ if_else(is.na(.), "No", .)))

fig7_postfellow_wilcox <- wilcox.test(perc_offers ~ `Postdoctoral Fellowship`,
                                      data = fellow_data,
                                      na.rm=TRUE, paired=FALSE, 
                                      exact=FALSE, conf.int=TRUE)

fig7b_plot <- fellow_data %>% 
  ggplot(aes(x = `Postdoctoral Fellowship`, y = perc_offers, 
             color = `Postdoctoral Fellowship`))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Postdoctoral\nFellowship (n=155)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig7b.png")

#phd fellowship: y/n
fig7_phdfellow_wilcox <- wilcox.test(perc_offers ~ `Predoctoral Fellowship`,
                                     data = fellow_data,
                                     na.rm=TRUE, paired=FALSE, 
                                     exact=FALSE, conf.int=TRUE)

fig7c_plot <- fellow_data %>% 
  ggplot(aes(x = `Predoctoral Fellowship`, y = perc_offers, 
             color = `Predoctoral Fellowship`))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Predoctoral\nFellowship (n=155)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig7c.png")

#CNS status: y/n
CNS_data <- fig6_data %>% 
  filter(question == "CNS_status") %>% distinct()


fig7_CNS_wilcox <- wilcox.test(perc_offers ~ response,
                                 data = CNS_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig7d_plot <- CNS_data %>% 
  ggplot(aes(x = response, y = perc_offers, color = response))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nCNS Papers (n=163)", y="Percent of all applications that yielded offers",
       subtitle = "Mann-Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig7d.png")

#Fig 7 wilcox data----
wilcox_list <- c("fig7_ct_wilcox", "fig7_postfellow_wilcox",
                 "fig7_phdfellow_wilcox", "fig7_CNS_wilcox")

plot_list <- c('7A', '7B', '7C', '7D')

fig7_wilcox_tbl_raw <- map2_df(wilcox_list, plot_list, get_wilcox_tbl) 

fig7_wilcox_tbl <- fig7_wilcox_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  mutate(conf.int2 = round(as.numeric(conf.int2), digits = 2),
         conf.int1 = round(as.numeric(conf.int1), digits = 1),
         conf_int_95 = paste0("(", conf.int1, ", ", conf.int2, ")"),
         p.value = round(as.numeric(p.value), digits = 5)
  ) %>% 
  select(figure, method, statistic.W, p.value, conf_int_95,
         alternative, `estimate.difference in location`, 
         `null.value.location shift`)

write_csv(fig7_wilcox_tbl, "nafisa/figures/fig7_stats.csv")

#generate plot----
Fig7 <- plot_grid(fig7a_plot, fig7b_plot, fig7c_plot, fig7d_plot,
                  labels = c('A', 'B', 'C', 'D'),
                  label_size = 18, ncol = 2)

ggsave("Figure_7.png", device = 'png', units = "in", scale = 1.75,
       path = 'nafisa/figures/', width = 10, height = 11)