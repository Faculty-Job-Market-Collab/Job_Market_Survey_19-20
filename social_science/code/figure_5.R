# Traditional metrics & impact on search success (add teaching here)

# Fig 5A Career Transition Award 

# Fig 5B Total Citations (Above/Below Median) -- Does the median differ significantly by gender or race? 
plot_mwu_box_data(df = fig3_data, q = "scholar_citations_all", 
                  f = "peer", 
                  f_text = "PEER identity", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Number of Google Scholar citations",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_citations_box_peer.jpeg")

plot_mwu_box_data(df = fig3_data, q = "scholar_citations_all", 
                  f = "simple_gender", 
                  f_text = "\nGender", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Number of Google Scholar citations",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_citations_box_gender.jpeg")

# Fig 5C Total Publications (Above/Below Median) -- Does the median differ significantly by gender or race? 
plot_mwu_box_data(df = fig3_data, q = "peer-reviewed_papers", 
                  f = "peer", 
                  f_text = "PEER identity", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Number of peer-reviewed papers",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_peer-review_box_peer.jpeg")

plot_mwu_box_data(df = fig3_data, q = "peer-reviewed_papers", 
                  f = "simple_gender", 
                  f_text = "\nGender", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Number of peer-reviewed papers",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_peer-review_box_gender.jpeg")

# Fig 5D H-index (Above/Below Median) -- Does the median differ significantly by gender or race? 
plot_mwu_box_data(df = fig3_data, q = "scholar_hindex", 
                  f = "peer", 
                  f_text = "PEER identity", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Applicant Google Scholar H index",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_hindex_box_peer.jpeg")

plot_mwu_box_data(df = fig3_data, q = "scholar_hindex", 
                  f = "simple_gender", 
                  f_text = "Gender", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Applicant Google Scholar H index",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_hindex_box_gender.jpeg")

# Fig 5E 1st author papers (Above/Below Median) -- Does the median differ significantly by gender or race? 
plot_mwu_box_data(df = fig3_data, q = "first_author", f = "peer", 
                  f_text = "PEER identity", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Number of first author papers",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_first_auth_box_peer.jpeg")

plot_mwu_box_data(df = fig3_data, q = "first_author", 
                  f = "simple_gender", 
                  f_text = "Gender", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Number of first author papers",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_first_auth_box_gender.jpeg")

# Fig 5F PhD Fellowship (Fellowship/No Fellowship)
fig5f_peer_table <- table(fig5f_data$predoctoral, fig5f_data$peer)

fig5f_peer_chi <- chisq.test(fig5f_peer_table)

fig5f_data %>% 
  count(peer, predoctoral) %>% 
  spread(key = predoctoral, value = n) %>% 
  mutate(total = yes + no,
         percent = get_percent(yes, total),
         peer = paste0(peer, "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=peer, y = as.numeric(percent)))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="\nPercent of applicants that received a predoctoral fellowship", 
       x="PEER identity",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_percent_phdfellowships_peer.jpeg")

fig5f_gen_table <- table(fig5f_data$predoctoral, fig5f_data$simple_gender)

fig5f_gen_table <- fig5f_gen_table[,-2]

fig5f_gen_chi <- chisq.test(fig5f_gen_table)

fig5f_data %>% 
  count(simple_gender, predoctoral) %>% 
  spread(key = predoctoral, value = n) %>% 
  mutate(total = yes + no,
         percent = get_percent(yes, total),
         simple_gender = paste0(simple_gender, "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=simple_gender, y = as.numeric(percent)))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="\nPercent of applicants that received a predoctoral fellowship", 
       x="Gender",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_percent_phdfellowships_gender.jpeg")

# Fig 5G Years on the Job Market (Above/Below Median) -- Does the median differ significantly by gender or race? 

plot_mwu_box_data(df = fig5_data, q = "application_cycles", f = "peer", 
                  f_text = "PEER identity", mwu = "yes")+
  labs(x = "Number of application cycles",
       subtitle = "Mann Whitney U p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_years_app_box_peer.jpeg")

plot_mwu_box_data(df = fig5_data, q = "application_cycles", 
                  f = "simple_gender", 
                  f_text = "Gender", mwu = "yes")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Number of application cycles",
       subtitle = "Mann Whitney U p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_years_app_box_gender.jpeg")

# Fig 5H Years as post-doc (Above/Below Median) -- Does the median differ significantly by gender or race? 

# Fig 5I Percentage with Teaching Experience by Targeted Institution Type [mimic eLife Figure 6c] beyond TA experience/TA Experience
fig5i_wilcox <- wilcox.test(percent_PUI ~ teaching_status,
                            data = fig5i_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)
save_wilcox(fig5i_wilcox, 
            file = "social_science/figures/fig5_percent_PUI_mwu_test.csv")

fig5i_data %>% 
  ggplot(aes(x = teaching_status, y = as.numeric(percent_PUI)))+
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nTeaching experience", y="Percent of applications submitted to PUIs",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_percent_PUI_box.jpeg")

# Fig 5J Association between offer percentage and teaching experience [mimic eLife 6D] (No experience, TA Experience, Beyond TA Experience) -- Compare significant difference in offer % between groups
fig5j_wilcox <- wilcox.test(percent_offer ~ teaching_status,
                            data = fig5i_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)
save_wilcox(fig5j_wilcox, 
            file = "social_science/figures/fig5_percent_all_offers_mwu_test.csv")

fig5i_data %>% 
  ggplot(aes(x = teaching_status, y = percent_offer))+
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nTeaching experience", y="Percent of all applications that yielded offers",
       subtitle = "Mann Whitney U p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig5_percent_all_offers_box.jpeg")