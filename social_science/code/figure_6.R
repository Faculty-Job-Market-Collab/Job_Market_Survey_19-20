# Additional Search Influences & Preparation
# Fig 6A Association between PhD Institution Classification & Offers: If you provide me with the list of institutions, I’m happy to go through and provide the Carnegie Classification for them
fig6a_table <- table(fig6a_data$inst_value, fig6a_data$simple_faculty_offer)

fig6a_chi <- chisq.test(fig6a_table)

fig6a_data %>% 
  filter(!is.na(simple_faculty_offer)) %>% 
  filter(!is.na(inst_value)) %>% 
  count(simple_faculty_offer, inst_value) %>% 
  spread(key = simple_faculty_offer, value = n) %>% 
  mutate(Total = `0`+`1+`,
         Perc = get_percent(`1+`, Total),
         inst_value = paste0(inst_value, "\n(n=",
                           Total, ")")) %>% 
  ggplot(aes(x = inst_value, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "\nPhD institution type", y = "Recieved 1+ offers (%)\n",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\n p>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig6_phd_inst_offers.jpeg")

# Fig 6B Association between current advisor rank and offers
fig6b_table <- table(fig6b_data$response, fig6b_data$simple_faculty_offer)

fig6b_table <- fig6b_table[,-1]

fig6b_chi <- chisq.test(fig6b_table)

fig6b_data %>% 
  filter(!is.na(simple_faculty_offer)) %>% 
  filter(!is.na(response)) %>% 
  count(simple_faculty_offer, response) %>% 
  #spread(key = response, value = n) %>% 
  mutate(Total = n,
         Perc = get_percent(n, Total),
         response = paste0(response, "\n(n=",
                                Total, ")")) %>% 
  ggplot(aes(x = response, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "\nAdvisor rank", y = "Recieved 1+ offers (%)\n",
       subtitle = "Chi-squared test for given probabilities\np<0.005")+
  my_theme_horiz

ggsave("social_science/figures/fig6_phd_inst_offers.jpeg")

# Fig 6C Percentage solicited feedback on application materials -- Compare significant differences by race & gender
fig6c_data %>% 
  filter(!is.na(app_feedback)) %>% 
  count(simple_gender, app_feedback) %>% 
  spread(key = app_feedback, value = n) %>% 
  mutate(Total = Yes,
         Perc = get_percent(Yes, Total),
         simple_gender = paste0(simple_gender, "\n(n=",
                                Total, ")")) %>% 
  ggplot(aes(x = simple_gender, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Solicited feedback on application materials (%)\n", 
       x = "Gender",
       #caption = paste0("Chi-squared p-value = ", round(fig6b_chi[[3]], digits = 3))
       )+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_app_feedback_gender.jpeg")

fig6c_data %>% 
  filter(!is.na(app_feedback)) %>% 
  count(peer, app_feedback) %>% 
  spread(key = app_feedback, value = n) %>% 
  mutate(Total = Yes,
         Perc = get_percent(Yes, Total),
         peer = paste0(peer, "\n(n=",
                                Total, ")")) %>% 
  ggplot(aes(x = peer, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Solicited feedback on application materials (%)\n", x = "PEER identity",
       #caption = paste0("Chi-squared p-value = ", round(fig6b_chi[[3]], digits = 3))
  )+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_app_feedback_peer.jpeg")

# Fig 6D Percentage solicited feedback on interview materials -- Compare significant differences by race & gender
fig6d_data %>% 
  filter(!is.na(interview_feedback)) %>% 
  count(simple_gender, interview_feedback) %>% 
  spread(key = interview_feedback, value = n) %>% 
  mutate(Total = Yes,
         Perc = get_percent(Yes, Total),
         simple_gender = paste0(simple_gender, "\n(n=",
                       Total, ")")) %>% 
  ggplot(aes(x = simple_gender, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Solicited feedback on interview materials (%)\n", x = "Gender",
       #caption = paste0("Chi-squared p-value = ", round(fig6b_chi[[3]], digits = 3))
  )+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_interview_feedback_gender.jpeg")

fig6d_data %>% 
  filter(!is.na(interview_feedback)) %>% 
  count(peer, interview_feedback) %>% 
  spread(key = interview_feedback, value = n) %>% 
  mutate(Total = Yes,
         Perc = get_percent(Yes, Yes),
         peer = paste0(peer, "\n(n=",
                       Total, ")")) %>% 
  ggplot(aes(x = peer, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Solicited feedback on interview materials (%)\n", x = "PEER identity",
       #caption = paste0("Chi-squared p-value = ", round(fig6b_chi[[3]], digits = 3))
  )+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_interview_feedback_peer.jpeg")

# Fig 6E Percentage attended job market prep workshops at home institution -- Compare significant differences by race & gender
fig6e_peer_table <- table(fig6e_data$response, fig6e_data$peer)

fig6e_peer_chi <- chisq.test(fig6e_peer_table)

fig6e_data %>% 
  filter(!is.na(response)) %>% 
  count(peer, response) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         Perc = get_percent(Yes, Total),
         peer = paste0(peer, "\n(n=",
                                Total, ")")) %>% 
  ggplot(aes(x = peer, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Attended job market prep workshops at home institution (%)\n",
       x = "PEER identity",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_home_workshop_peer.jpeg")

fig6e_gen_table <- table(fig6e_data$response, fig6e_data$simple_gender)

fig6e_gen_table <- fig6e_gen_table[,-2]

fig6e_gen_chi <- chisq.test(fig6e_gen_table)

fig6e_data %>% 
  filter(!is.na(response)) %>% 
  count(simple_gender, response) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         Perc = get_percent(Yes, Total),
         simple_gender = paste0(simple_gender, "\n(n=",
                                    Total, ")")) %>% 
  ggplot(aes(x = simple_gender, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Attended job market prep workshops at home institution (%)\n", 
       x = "\nGender",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_home_workshop_gender.jpeg")

# Fig 6F Percentage attended job market prep workshops from third party sources -- Compare significant differences by race & gender
fig6f_peer_table <- table(fig6f_data$response, fig6f_data$peer)

fig6f_peer_chi <- chisq.test(fig6f_peer_table)

fig6f_data %>% 
  filter(!is.na(response)) %>% 
  count(peer, response) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         Perc = get_percent(Yes, Total),
         peer = paste0(peer, "\n(n=",
                                    Total, ")")) %>% 
  ggplot(aes(x = peer, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Attended third-party job market prep workshops (%)\n", x = "PEER identity",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05"
  )+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_home_workshop_peer.jpeg")

fig6f_gen_table <- table(fig6f_data$response, fig6f_data$simple_gender)

fig6f_gen_table <- fig6f_gen_table[,-2]

fig6f_gen_chi <- chisq.test(fig6f_gen_table)

fig6f_data %>% 
  filter(!is.na(response)) %>% 
  count(simple_gender, response) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         Perc = get_percent(Yes, Total),
         simple_gender = paste0(simple_gender, "\n(n=",
                                    Total, ")")) %>% 
  ggplot(aes(x = simple_gender, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Attended third-party job market prep workshops (%)\n", x = "\nGender",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05"
  )+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_home_workshop_gender.jpeg")
# Fig 6G Percentage with blog or website (divide social sciences & humanities)
fig6g_table <- table(fig6g_data$response, fig6g_data$research_category)

fig6g_chi <- chisq.test(fig6g_table)

fig6g_data %>% 
  filter(!is.na(response)) %>% 
  count(research_category, response) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         Perc = get_percent(Yes, Total),
         research_category = paste0(research_category, "\n(n=",
                                    Total, ")")) %>% 
  ggplot(aes(x = research_category, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Has a blog or website (%)", x = "Research category",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np<0.05"
  )+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_blog.jpeg")

# Fig 6H Percentage use social media (divide social sciences & humanities)
fig6h_table <- table(fig6h_data$response, fig6h_data$research_category)

fig6h_chi <- chisq.test(fig6h_table)

fig6h_data %>% 
  filter(!is.na(response)) %>% 
  count(research_category, response) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         Perc = get_percent(Yes, Total),
         research_category = paste0(research_category, "\n(n=",
                                    Total, ")")) %>% 
  ggplot(aes(x = research_category, y = Perc))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Has a social media presence (%)", x = "Research category",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np<0.05"
  )+
  my_theme_horiz

ggsave("social_science/figures/fig6_percent_social_media.jpeg")