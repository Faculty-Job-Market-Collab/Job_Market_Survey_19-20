# Scholarly Metrics by Gender (Please use men and women instead of male/female)
# Publication Metrics

# Fig 2A 1st author pubs (Two-tailed Wilcoxon rank sum test)
fig2A_mwu_data <- fig2A_data %>% 
  filter(question == "first_author") %>% 
  filter(simple_gender != "No Response") %>% distinct()


fig2A_wilcox <- wilcox.test(as.numeric(response) ~ simple_gender,
                            data = fig2A_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig2A_wilcox, 
            file = "social_science/figures/fig2_first_auth_gender_mwu_test.csv")

plot_mwu_bar_data(df = fig2A_data, q = "first_author", f = "simple_gender", 
                  f_text = "Gender", mwu = "yes", l = "small", 
                  bin = "yes")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of first author papers",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig2_first_auth_gender.jpeg")

# Fig 2B Total Pubs (Two-tailed Wilcoxon rank sum test) 
fig2B_mwu_data <-fig2_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  filter(simple_gender != "No Response") %>% 
  distinct() 

fig2B_wilcox <- wilcox.test(as.numeric(response) ~ simple_gender,
                            data = fig2B_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig2B_wilcox, 
            file = "social_science/figures/fig2_peer-review_gender_mwu_test.csv")

plot_mwu_bar_data(df = fig2_data, q = "peer-reviewed_papers", 
                  f = "simple_gender", 
                  f_text = "Gender", mwu = "yes", 
                  l = "small", bin = "yes")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of peer-reviewed papers",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig2_num_peer-review_gender.jpeg")

# Fig 2C All Citations (Two-tailed Wilcoxon rank sum test)
fig2C_mwu_data <-fig2_data %>% 
  filter(question == "scholar_citations_all") %>% 
  filter(simple_gender != "No Response") %>% 
  distinct() 

fig2C_wilcox <- wilcox.test(as.numeric(response) ~ simple_gender,
                            data = fig2C_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig2C_wilcox, 
            file = "social_science/figures/fig2_scholar_citations_gender_mwu_test.csv")

plot_mwu_bar_data(df = fig2_data, q = "scholar_citations_all", 
                  f = "simple_gender", 
                  f_text = "Gender", mwu = "yes", 
                  l = "big", bin = "yes")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of Google Scholar Citations",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig2_num_cites_gender.jpeg")

# Fig 2D H-index (Two-tailed Wilcoxon rank sum test)
fig2D_mwu_data <-fig2_data %>% 
  filter(question == "scholar_hindex") %>% 
  filter(simple_gender != "No Response") %>% 
  distinct() 

fig2D_wilcox <- wilcox.test(as.numeric(response) ~ simple_gender,
                            data = fig2C_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig2D_wilcox, 
            file = "social_science/figures/fig2_hindex_gender_mwu_test.csv")

plot_mwu_bar_data(df = fig2_data, q = "scholar_hindex", 
                  f = "simple_gender", 
                  f_text = "Gender", mwu = "yes", 
                  l = "small", bin = "yes")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Applicant Google Scholar H index",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig2_hindex_gender.jpeg")

# Fig 2E Percent of applicants w/ fellowships (chi-square)
fig2e_tab_data <- fig2ef_data %>% 
  filter(simple_gender != "No Response") 

fig2e_table <- table(fig2e_tab_data$fellowship, 
                     fig2e_tab_data$simple_gender)

fig2e_table <- fig2e_table[,-3]

fig2e_chi <- chisq.test(fig2e_table)

fig2ef_data %>% 
  count(simple_gender, fellowship) %>% 
  spread(key = fellowship, value = n) %>% 
  mutate(total = yes + no,
         percent = get_percent(yes, total),
         simple_gender = paste0(simple_gender, "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=simple_gender, y = as.numeric(percent)))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses", 
       x="Gender of applicants that received a pre- or\npostdoctoral fellowship",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig2_percent_fellowships_gender.jpeg")

# Fig 2F Percent of applicants w/ grants (chi-square)
fig2f_tab_data <- fig2ef_data %>% 
  filter(simple_gender != "No Response") 
  
fig2f_table <- table(fig2f_tab_data$grant, 
                     fig2f_tab_data$simple_gender)

fig2f_table <- fig2f_table[,-3]

fig2f_chi <- chisq.test(fig2f_table)

fig2ef_data %>% 
  count(simple_gender, grant) %>% 
  spread(key = grant, value = n) %>% 
  mutate(total = yes + no,
         percent = get_percent(yes, total),
         simple_gender = paste0(simple_gender, "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=simple_gender, y = as.numeric(percent)))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses", x="Gender of applicants that recieved a grant",
       subtitle = paste0("Pearson's Chi-squared test with Yates' continuity correction\np>0.05"))+
  my_theme_horiz

ggsave("social_science/figures/fig2_percent_grants_gender.jpeg")
