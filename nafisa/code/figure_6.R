#eLife fig 5B - metrics vs % offers


#total citations: above/below median
cites_data <- fig6_data %>% 
  filter(question == "scholar_citations_all") %>% 
  filter(response != 329) %>% 
  mutate(median = if_else(response >= 330, "Above the\nmedian (330+)", 
                          "Below the\nmedian (<329)")) %>% 
  distinct()
  

fig6_cites_wilcox <- wilcox.test(perc_offers ~ median,
                              data = cites_data,
                              na.rm=TRUE, paired=FALSE, 
                              exact=FALSE, conf.int=TRUE)

fig6a_plot <- cites_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Google Scholar\nCitations (n=148)", y="Percent offers",
       subtitle = "Mann-Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig6a.png")

#years on the job market: above/below median
years_data <- fig6_data %>% 
  filter(question == "application_cycles") %>% 
  mutate(median = if_else(response > 1.0, "Above the\nmedian (2+)", 
                          "At the\nmedian (1)")) %>% 
  distinct()


fig6_years_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = years_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig6b_plot <- years_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Application\nCycles (n=230)", 
       y="Percent offers",
       subtitle = "Mann Whitney U: p < 0.005")+
  my_theme_horiz

ggsave("nafisa/figures/fig6b.png")

#number of papers: above/below median
papers_data <- fig6_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  filter(response != 5) %>% 
  mutate(median = if_else(response >= 6, "Above the\nmedian (6+)", 
                          "Below the\nmedian (<5)")) %>% 
  distinct()


fig6_papers_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = papers_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig6c_plot <- papers_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Peer-reviewed\nPublications (n=152)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig6c.png")

#H-index: above/below median
h_data <- fig6_data %>% 
  filter(question == "scholar_hindex") %>% 
  filter(response != 9) %>% 
  mutate(median = if_else(response >= 10, "Above the\nmedian (10+)", 
                          "Below the\nmedian (<9)")) %>% 
  distinct()


fig6_h_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = h_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig6d_plot <- h_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Google Scholar\nH-index (n=133)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig6d.png")

#1st author papers: above/below median
fauth_data <- fig6_data %>% 
  filter(question == "first_author") %>% 
  filter(response != 6) %>% 
  mutate(median = if_else(response >= 7, "Above the\nmedian (7+)", 
                          "Below the\nmedian (<6)")) %>% 
  distinct()


fig6_fauth_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = fauth_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig6e_plot <- fauth_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="First-author\npublications (n=141)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig6e.png")

#corresponding author papers: above/below median
cauth_data <- fig6_data %>% 
  filter(question == "corresponding_author") %>% 
  filter(response != 1) %>% 
  mutate(median = if_else(response > 2, "Above the\nmedian (2+)", 
                          "Below the\nmedian (<1)")) %>% 
  distinct()


fig6_cauth_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = cauth_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig6f_plot <- cauth_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Corresponding-author\npublications (n=142)", 
       y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig6f.png")

#CNS 1st author papers: above/below median
CNS_fauth_data <- fig6_data %>% 
  filter(question == "CNS_first_author") %>% 
  filter(response != 1) %>% 
  mutate(median = if_else(response >= 2, "Above the\nmedian (2+)", 
                          "Below the\nmedian (<1)")) %>% 
  distinct()

fig6_CNS_fauth_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = CNS_fauth_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig6g_plot <- CNS_fauth_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="CNS First-author\npublications (n=22)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig6g.png")

#compile stats
wilcox_list <- c("fig6_cites_wilcox", "fig6_years_wilcox", "fig6_papers_wilcox",
                 "fig6_h_wilcox", "fig6_fauth_wilcox", "fig6_cauth_wilcox",
                 "fig6_CNS_fauth_wilcox")

plot_list <- c('6A', '6B', '6C', '6D', '6E', '6F', '6G')

fig6_wilcox_tbl_raw <- map2_df(wilcox_list, plot_list, get_wilcox_tbl) 

fig6_wilcox_tbl <- fig6_wilcox_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  mutate(conf.int2 = round(as.numeric(conf.int2), digits = 2),
         conf.int1 = round(as.numeric(conf.int1), digits = 1),
         conf_int_95 = paste0("(", conf.int1, ", ", conf.int2, ")"),
         p.value = round(as.numeric(p.value), digits = 5)
         ) %>% 
  select(figure, method, statistic.W, p.value, conf_int_95,
         alternative, `estimate.difference in location`, 
         `null.value.location shift`)

write_csv(fig6_wilcox_tbl, "nafisa/figures/fig6_stats.csv")

#generate plot----
Fig6 <- plot_grid(fig6a_plot, fig6b_plot, fig6c_plot, fig6d_plot,
                  fig6e_plot, fig6f_plot, fig6g_plot, blank_plot,
                  labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', ''),
                  label_size = 18, ncol = 2)

ggsave("Figure_6.png", device = 'png', units = "in", scale = 1.75,
       path = 'nafisa/figures/', width = 10, height = 11)