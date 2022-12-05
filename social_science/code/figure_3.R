# Scholarly Metrics by Race/Ethnicity (Please use URM & Non-URM)

# Fig 3A 1st author pubs (Two-tailed Wilcoxon rank sum test)
fig3A_mwu_data <- fig3A_data %>% 
  filter(question == "first_author") %>% distinct()


fig3A_wilcox <- wilcox.test(as.numeric(response) ~ peer,
                            data = fig3A_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig3A_wilcox, 
            file = "social_science/figures/fig3_first_auth_peer_mwu_test.csv")

plot_mwu_bar_data(df = fig3A_data, q = "first_author", f = "peer", 
                  f_text = "PEER identity", mwu = "yes", l = "small", 
                  bin = "yes")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of first author papers",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig3_first_auth_peer.jpeg")

# Fig 3B Total Pubs (Two-tailed Wilcoxon rank sum test)
fig3B_mwu_data <-fig3_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  distinct() 

fig3B_wilcox <- wilcox.test(as.numeric(response) ~ peer,
                            data = fig3B_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig3B_wilcox, 
            file = "social_science/figures/fig3_peer-review_peer_mwu_test.csv")

plot_mwu_bar_data(df = fig3_data, q = "peer-reviewed_papers", 
                  f = "peer", 
                  f_text = "PEER identity", mwu = "yes", 
                  l = "small", bin = "yes")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of peer-reviewed papers",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig3_num_peer-review_peer.jpeg")

# Fig 3C All Citations (Two-tailed Wilcoxon rank sum test)
fig3C_mwu_data <-fig3_data %>% 
  filter(question == "scholar_citations_all") %>% 
  distinct() 

fig3C_wilcox <- wilcox.test(as.numeric(response) ~ peer,
                            data = fig3C_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig3C_wilcox, 
            file = "social_science/figures/fig3_scholar_citations_peer_mwu_test.csv")

plot_mwu_bar_data(df = fig3_data, q = "scholar_citations_all", 
                  f = "peer", 
                  f_text = "PEER identity", mwu = "yes", 
                  l = "big", bin = "yes")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of Google Scholar Citations",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig3_num_cites_peer.jpeg")

# Fig 3D H-index (Two-tailed Wilcoxon rank sum test)
fig3D_mwu_data <-fig3_data %>% 
  filter(question == "scholar_hindex") %>% 
  distinct() 

fig3D_wilcox <- wilcox.test(as.numeric(response) ~ peer,
                            data = fig3D_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig3D_wilcox, 
            file = "social_science/figures/fig3_hindex_peer_mwu_test.csv")

plot_mwu_bar_data(df = fig3_data, q = "scholar_hindex", 
                  f = "peer", 
                  f_text = "PEER identity", mwu = "yes", 
                  l = "small", bin = "yes")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Applicant Google Scholar H index",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig3_hindex_peer.jpeg")

# Fig 3E Percent of applicants w/ fellowships (chi-square)
fig3e_table <- table(fig3ef_data$fellowship, fig3ef_data$peer)

fig3e_chi <- chisq.test(fig3e_table)

fig3ef_data %>% 
  count(peer, fellowship) %>% 
  spread(key = fellowship, value = n) %>% 
  mutate(total = yes + no,
         percent = get_percent(yes, total),
         peer = paste0(peer, "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=peer, y = as.numeric(percent)))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses", 
       x="PEER identity of applicants that received a pre- or\npostdoctoral fellowship",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig3_percent_fellowships_peer.jpeg")

# Fig 3F Percent of applicants w/ grants (chi-square)
fig3f_table <- table(fig3ef_data$grant, fig3ef_data$peer)

fig3f_chi <- chisq.test(fig3f_table)

fig3ef_data %>% 
  count(peer, grant) %>% 
  spread(key = grant, value = n) %>% 
  mutate(total = yes + no,
         percent = get_percent(yes, total),
         peer = paste0(peer, "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=peer, y = as.numeric(percent)))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses", x="PEER identity of applicants that recieved a grant",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig3_percent_grants_peer.jpeg")