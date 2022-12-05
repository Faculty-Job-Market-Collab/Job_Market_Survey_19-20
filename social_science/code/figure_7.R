#eLife fig 5B - metrics vs % offers

#career transition award: y/n
ct_data <- fig7_data %>% 
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
  labs(x="Career Transition\nAward (n=154)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

#total citations: above/below median
cites_data <- fig7_data %>% 
  filter(question == "scholar_citations_all") %>% 
  mutate(median = if_else(response >= 42.0, "Above the\nmedian (42+)", 
                          "Below the\nmedian (<42)")) %>% 
  distinct()
  

fig7_cites_wilcox <- wilcox.test(perc_offers ~ median,
                              data = cites_data,
                              na.rm=TRUE, paired=FALSE, 
                              exact=FALSE, conf.int=TRUE)

fig7b_plot <- cites_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Google Scholar\nCitations (n=93)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz


#years on the job market: above/below median
years_data <- fig7_data %>% 
  filter(question == "application_cycles") %>% 
  mutate(median = if_else(response >= 2.0, "Above the\nmedian (2+)", 
                          "Below the\nmedian (1)")) %>% 
  distinct()


fig7_years_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = years_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig7c_plot <- years_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Application\nCycles (n=183)", 
       y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

#postdoc fellowship: y/n
fellow_data <- fig7_data %>% 
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

fig7d_plot <- fellow_data %>% 
  ggplot(aes(x = `Postdoctoral Fellowship`, y = perc_offers, 
             color = `Postdoctoral Fellowship`))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Postdoctoral\nFellowship (n=151)", y="Percent offers",
       subtitle = "Mann Whitney U: p<0.05")+
  my_theme_horiz

#number of papers: above/below median
papers_data <- fig7_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  mutate(median = if_else(response >= 5.0, "Above the\nmedian (5+)", 
                          "Below the\nmedian (<5)")) %>% 
  distinct()


fig7_papers_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = papers_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig7e_plot <- papers_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Peer-reviewed\nPublications (n=150)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

#H-index: above/below median
h_data <- fig7_data %>% 
  filter(question == "scholar_hindex") %>% 
  mutate(median = if_else(response >= 4.0, "Above the\nmedian (4+)", 
                          "Below the\nmedian (<4)")) %>% 
  distinct()


fig7_h_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = h_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig7f_plot <- h_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Google Scholar\nH-index (n=78)", y="Percent offers",
       subtitle = "Mann Whitney U: p<0.05")+
  my_theme_horiz

#1st author papers: above/below median
fauth_data <- fig7_data %>% 
  filter(question == "first_author") %>% 
  mutate(median = if_else(response >= 3.0, "Above the\nmedian (3+)", 
                          "Below the\nmedian (<3)")) %>% 
  distinct()


fig7_fauth_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = fauth_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig7g_plot <- fauth_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="First-author\npublications (n=145)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

#phd fellowship: y/n
fig7_phdfellow_wilcox <- wilcox.test(perc_offers ~ `Predoctoral Fellowship`,
                                      data = fellow_data,
                                      na.rm=TRUE, paired=FALSE, 
                                      exact=FALSE, conf.int=TRUE)

fig7h_plot <- fellow_data %>% 
  ggplot(aes(x = `Predoctoral Fellowship`, y = perc_offers, 
             color = `Predoctoral Fellowship`))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Predoctoral\nFellowship (n=151)", y="Percent offers",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

#research grant: y/n
#fig7_grant_wilcox <- wilcox.test(perc_offers ~ `Research Project Grant`,
#                                     data = fellow_data,
#                                     na.rm=TRUE, paired=FALSE, 
#                                     exact=FALSE, conf.int=TRUE)
#
#fellow_data %>% 
#  ggplot(aes(x = `Research Project Grant`, y = perc_offers))+
#  geom_boxplot()+
#  geom_jitter()+
#  coord_flip()+
#  scale_y_continuous(expand = c(0,0))+
#  labs(x="\nResearch Project Grant (n=151)", y="Percent of all applications that yielded offers",
#       subtitle = "Mann Whitney U: p>0.05")+
#  my_theme_horiz

#generate plot----
Fig7 <- plot_grid(fig7a_plot, fig7b_plot, fig7c_plot, fig7d_plot,
                  fig7e_plot, fig7f_plot, fig7g_plot, fig7h_plot,
                  labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
                  label_size = 18, ncol = 2)

ggsave("Figure_7.png", device = 'png', units = "in", scale = 1.75,
       path = 'social_science/figures/', width = 10, height = 11)