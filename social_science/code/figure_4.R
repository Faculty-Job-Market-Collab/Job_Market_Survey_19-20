# Job Application benchmarks and their impact on success

# Fig 4A Subgroup Medians (application, Remote Interview, on-site interview, offer)
# All; Men & Women; URM & Non-URM; 1+ offer & no offer

fig4_tidy_data %>% 
  filter(question != "apps_submitted_binned") %>% 
  ggplot(aes(x = simple_gender, fill = simple_gender, y = as.numeric(response)))+
  geom_boxplot(position = "dodge")+
  facet_wrap(~ question, scales = "free_x")+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = cbPalette)+
  labs(y = "Number", x = "Gender")+
  my_theme_horiz

ggsave("social_science/figures/fig4_gender_outcome_boxplots.jpeg")


fig4_tidy_data %>% 
  filter(question != "apps_submitted_binned") %>% 
  ggplot(aes(x = peer, fill = peer, y = as.numeric(response)))+
  geom_boxplot(position = "dodge")+
  facet_wrap(~ question, scales = "free_x")+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  #scale_x_discrete(expand = c(0,0))+
  labs(y = "Number", x = "PEER identity")+
  my_theme_horiz

ggsave("social_science/figures/fig4_peer_outcome_boxplots.jpeg")

fig4_tidy_data %>% 
  filter(question != "apps_submitted_binned") %>% 
  ggplot(aes(x = simple_faculty_offer, fill = simple_faculty_offer, y = as.numeric(response)))+
  geom_boxplot(position = "dodge")+
  facet_wrap(~ question, scales = "free_x")+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  #scale_x_discrete(expand = c(0,0))+
  labs(y = "Number", x = "Faculty offers")+
  my_theme_horiz

ggsave("social_science/figures/fig4_offers_outcome_boxplots.jpeg")

# Fig 4B Correlations with Application Numbers (Remote, on-site, offers) -- Differences in correlations by gender & race

off_app_cor_coefs <- cor.test(as.numeric(fig4_data$apps_submitted), 
                            as.numeric(fig4_data$faculty_offers))

off_app_man_cor_coefs <- cor.test(as.numeric(fig4_data_men$apps_submitted), 
                              as.numeric(fig4_data_men$faculty_offers))

off_app_wom_cor_coefs <- cor.test(as.numeric(fig4_data_women$apps_submitted), 
                              as.numeric(fig4_data_women$faculty_offers))
      
ggplot(data = fig4_data_two, aes(x = as.numeric(fig4_data_two$apps_submitted), 
                             y = as.numeric(fig4_data_two$faculty_offers)))+
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE)+
  annotate("text", x = 100, y = 2.25, 
           label = paste0("R: ", round(off_app_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 2, 
           label = paste0("p-value: ", round(off_app_cor_coefs$p.value, 5)))+
  annotate("text", x = 80, y = .2, color = "#E69F00",
           label = paste0("R: ", round(off_app_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = .1, color = "#E69F00",
           label = paste0("p-value: ", round(off_app_man_cor_coefs$p.value, 5)))+
  annotate("text", x = 130, y = .2, color = "#009E73",
           label = paste0("R: ", round(off_app_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = .1, color = "#009E73",
           label = paste0("p-value: ", round(off_app_wom_cor_coefs$p.value, 5)))+
  labs(y = "Number of faculty offers", x = "Number of applications submitted",
       fill = "Gender:", color = "Gender:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_apps_offers_gen_corr.jpeg")

offsite_apps_cor_coefs <- cor.test(as.numeric(fig4_data$apps_submitted), 
                                    as.numeric(fig4_data$off_site_interviews))

offsite_apps_man_cor_coefs <- cor.test(as.numeric(fig4_data_men$apps_submitted), 
                                  as.numeric(fig4_data_men$off_site_interviews))

offsite_apps_wom_cor_coefs <- cor.test(as.numeric(fig4_data_women$apps_submitted), 
                                  as.numeric(fig4_data_women$off_site_interviews))


ggplot(data = fig4_data_two, aes(x = as.numeric(fig4_data_two$apps_submitted), 
                             y = as.numeric(fig4_data_two$off_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE) +
  annotate("text", x = 100, y = 16, 
           label = paste0("R: ", round(offsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 15, 
           label = paste0("p-value: ", round(offsite_apps_cor_coefs$p.value, 25)))+
  annotate("text", x = 80, y = 5, color = "#E69F00",
           label = paste0("R: ", round(offsite_apps_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = 4, color = "#E69F00",
           label = paste0("p-value: ", round(offsite_apps_man_cor_coefs$p.value, 10)))+
  annotate("text", x = 130, y = 11, color = "#009E73",
           label = paste0("R: ", round(offsite_apps_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = 10, color = "#009E73",
           label = paste0("p-value: ", round(offsite_apps_wom_cor_coefs$p.value, 20)))+
  labs(y = "Number of off-site interviews", x = "Number of applications submitted",
       fill = "Gender", color = "Gender")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_apps_off-site_gen_corr.jpeg")

onsite_apps_cor_coefs <- cor.test(as.numeric(fig4_data$apps_submitted), 
                                   as.numeric(fig4_data$on_site_interviews))

onsite_apps_man_cor_coefs <- cor.test(as.numeric(fig4_data_men$apps_submitted), 
                                       as.numeric(fig4_data_men$on_site_interviews))

onsite_apps_wom_cor_coefs <- cor.test(as.numeric(fig4_data_women$apps_submitted), 
                                       as.numeric(fig4_data_women$on_site_interviews))


ggplot(data = fig4_data_two, aes(x = as.numeric(fig4_data_two$apps_submitted), 
                             y = as.numeric(fig4_data_two$on_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE) +
  annotate("text", x = 130, y = 2.5, 
           label = paste0("R: ", round(onsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = 2.3, 
           label = paste0("p-value: ", round(onsite_apps_cor_coefs$p.value, 5)))+
  annotate("text", x = 80, y = .75, color = "#E69F00",
           label = paste0("R: ", round(onsite_apps_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = .25, color = "#E69F00",
           label = paste0("p-value: ", round(onsite_apps_man_cor_coefs$p.value, 5)))+
  annotate("text", x = 120, y = 6, color = "#009E73",
           label = paste0("R: ", round(onsite_apps_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 120, y = 5.7, color = "#009E73",
           label = paste0("p-value: ", round(onsite_apps_wom_cor_coefs$p.value, 5)))+
  labs(y = "Number of on-site interviews", x = "Number of applications submitted",
       fill = "Gender", color = "Gender")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_apps_on-site_gen_corr.jpeg")

#race
off_app_peer_cor_coefs <- cor.test(as.numeric(fig4_data_peer$apps_submitted), 
                                  as.numeric(fig4_data_peer$faculty_offers))

off_app_nonpeer_cor_coefs <- cor.test(as.numeric(fig4_data_nonpeer$apps_submitted), 
                                  as.numeric(fig4_data_nonpeer$faculty_offers))

ggplot(data = fig4_data, aes(x = as.numeric(fig4_data$apps_submitted), 
                             y = as.numeric(fig4_data$faculty_offers))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(values = peer_color)+
  scale_color_manual(values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE) +
  annotate("text", x = 100, y = 2, 
           label = paste0("R: ", round(off_app_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 1.75, 
           label = paste0("p-value: ", round(off_app_cor_coefs$p.value, 5)))+
  #peer data
  annotate("text", x = 80, y = 4.5, color = "#CC79A7",
           label = paste0("R: ", round(off_app_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = 4.25, color = "#CC79A7",
           label = paste0("p-value: ", round(off_app_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 130, y = 0.5, color = "#56B4E9",
           label = paste0("R: ", round(off_app_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = .2, color = "#56B4E9",
           label = paste0("p-value: ", round(off_app_nonpeer_cor_coefs$p.value, 5)))+
  labs(y = "Number of faculty offers", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_apps_offers_peer_corr.jpeg")

offsite_apps_peer_cor_coefs <- cor.test(as.numeric(fig4_data_peer$apps_submitted), 
                                       as.numeric(fig4_data_peer$off_site_interviews))

offsite_apps_nonpeer_cor_coefs <- cor.test(as.numeric(fig4_data_nonpeer$apps_submitted), 
                                       as.numeric(fig4_data_nonpeer$off_site_interviews))


ggplot(data = fig4_data, aes(x = as.numeric(fig4_data$apps_submitted), 
                             y = as.numeric(fig4_data$off_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(values = peer_color)+
  scale_color_manual(values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE) +
  annotate("text", x = 100, y = 16, 
           label = paste0("R: ", round(offsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 15, 
           label = paste0("p-value: ", round(offsite_apps_cor_coefs$p.value, 5)))+
  #peer data
  annotate("text", x = 100, y = 5, color = "#CC79A7",
           label = paste0("R: ", round(offsite_apps_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 4, color = "#CC79A7",
           label = paste0("p-value: ", round(offsite_apps_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 130, y = 10, color = "#56B4E9",
           label = paste0("R: ", round(offsite_apps_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = 9, color = "#56B4E9",
           label = paste0("p-value: ", round(offsite_apps_nonpeer_cor_coefs$p.value, 21)))+  
  labs(y = "Number of off-site interviews", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_apps_off-site_peer_corr.jpeg")

onsite_apps_peer_cor_coefs <- cor.test(as.numeric(fig4_data_peer$apps_submitted), 
                                      as.numeric(fig4_data_peer$on_site_interviews))

onsite_apps_nonpeer_cor_coefs <- cor.test(as.numeric(fig4_data_nonpeer$apps_submitted), 
                                      as.numeric(fig4_data_nonpeer$on_site_interviews))


ggplot(data = fig4_data, aes(x = as.numeric(apps_submitted), 
                             y = as.numeric(on_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(values = peer_color)+
  scale_color_manual(values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999")+
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE)+
  annotate("text", x = 125, y = 3, 
           label = paste0("R: ", round(onsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 125, y = 2.5, 
           label = paste0("p-value: ", round(onsite_apps_cor_coefs$p.value, 10)))+
  #peer data
  annotate("text", x = 95, y = 5, color = "#CC79A7",
           label = paste0("R: ", round(onsite_apps_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 95, y = 4.5, color = "#CC79A7",
           label = paste0("p-value: ", round(onsite_apps_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 130, y = 6, color = "#56B4E9",
           label = paste0("R: ", round(onsite_apps_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = 5.5, color = "#56B4E9",
           label = paste0("p-value: ", round(onsite_apps_nonpeer_cor_coefs$p.value, 10)))+  
  labs(y = "Number of on-site interviews", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_apps_on-site_peer_corr.jpeg")

# Fig 4C Correlations between interviews and offers (remote, off-site) -- Differences in correlations by gender & race

offsite_offs_cor_coefs <- cor.test(as.numeric(fig4_data$faculty_offers), 
                                   as.numeric(fig4_data$off_site_interviews))

offsite_offs_man_cor_coefs <- cor.test(as.numeric(fig4_data_men$faculty_offers), 
                                       as.numeric(fig4_data_men$off_site_interviews))

offsite_offs_wom_cor_coefs <- cor.test(as.numeric(fig4_data_women$faculty_offers), 
                                       as.numeric(fig4_data_women$off_site_interviews))


ggplot(data = fig4_data_two, aes(x = as.numeric(fig4_data_two$faculty_offers), 
                             y = as.numeric(fig4_data_two$off_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE) +
  annotate("text", x = 4.5, y = 16, 
           label = paste0("R: ", round(offsite_offs_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 15, 
           label = paste0("p-value: ", round(offsite_offs_cor_coefs$p.value, 5)))+
  annotate("text", x = 2, y = 12, color = "#E69F00",
           label = paste0("R: ", round(offsite_offs_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 2, y = 11, color = "#E69F00",
           label = paste0("p-value: ", round(offsite_offs_man_cor_coefs$p.value, 5)))+
  annotate("text", x = 4.5, y = 2, color = "#009E73",
           label = paste0("R: ", round(offsite_offs_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 1, color = "#009E73",
           label = paste0("p-value: ", round(offsite_offs_wom_cor_coefs$p.value, 5)))+
  labs(y = "Number of off-site interviews", x = "Number of faculty offers",
       fill = "Gender:", color = "Gender:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_offers_off-site_gen_corr.jpeg")

onsite_offers_cor_coefs <- cor.test(as.numeric(fig4_data$faculty_offers), 
                                  as.numeric(fig4_data$on_site_interviews))

onsite_offers_man_cor_coefs <- cor.test(as.numeric(fig4_data_men$faculty_offers), 
                                      as.numeric(fig4_data_men$on_site_interviews))

onsite_offers_wom_cor_coefs <- cor.test(as.numeric(fig4_data_women$faculty_offers), 
                                      as.numeric(fig4_data_women$on_site_interviews))


ggplot(data = fig4_data_two, aes(x = as.numeric(faculty_offers), 
                             y = as.numeric(on_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE) +
  annotate("text", x = 4.5, y = 4, 
           label = paste0("R: ", round(onsite_offers_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 3.5, 
           label = paste0("p-value: ", round(onsite_offers_cor_coefs$p.value, 25)))+
  annotate("text", x = 2, y = 1.5, color = "#E69F00",
           label = paste0("R: ", round(onsite_offers_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 2, y = 1.25, color = "#E69F00",
           label = paste0("p-value: ", round(onsite_offers_man_cor_coefs$p.value, 10)))+
  annotate("text", x = 3.5, y = 6.75, color = "#009E73",
           label = paste0("R: ", round(onsite_offers_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 3.5, y = 6.5, color = "#009E73",
           label = paste0("p-value: ", round(onsite_offers_wom_cor_coefs$p.value, 20)))+
  labs(y = "Number of on-site interviews", x = "Number of faculty offers",
       fill = "Gender:", color = "Gender:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_offers_on-site_gen_corr.jpeg")

#race
offsite_offers_peer_cor_coefs <- cor.test(as.numeric(fig4_data_peer$faculty_offers), 
                                        as.numeric(fig4_data_peer$off_site_interviews))

offsite_offers_nonpeer_cor_coefs <- cor.test(as.numeric(fig4_data_nonpeer$faculty_offers), 
                                           as.numeric(fig4_data_nonpeer$off_site_interviews))


ggplot(data = fig4_data, aes(x = as.numeric(faculty_offers), 
                             y = as.numeric(off_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(values = peer_color)+
  scale_color_manual(values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE) +
  annotate("text", x = 4.5, y = 13, 
           label = paste0("R: ", round(offsite_offs_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 12, 
           label = paste0("p-value: ", round(offsite_offs_cor_coefs$p.value, 5)))+
  #peer data
  annotate("text", x = .5, y = .5, color = "#CC79A7",
           label = paste0("R: ", round(offsite_offers_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = .5, y = -0.5, color = "#CC79A7",
           label = paste0("p-value: ", round(offsite_offers_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 4, y = 17, color = "#56B4E9",
           label = paste0("R: ", round(offsite_offers_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 4, y = 16, color = "#56B4E9",
           label = paste0("p-value: ", round(offsite_offers_nonpeer_cor_coefs$p.value, 7)))+  
  labs(y = "Number of off-site interviews", x = "Number of faculty offers",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_offers_off-site_peer_corr.jpeg")

onsite_offers_peer_cor_coefs <- cor.test(as.numeric(fig4_data_peer$faculty_offers), 
                                       as.numeric(fig4_data_peer$on_site_interviews))

onsite_offers_nonpeer_cor_coefs <- cor.test(as.numeric(fig4_data_nonpeer$faculty_offers), 
                                          as.numeric(fig4_data_nonpeer$on_site_interviews))


ggplot(data = fig4_data, aes(x = as.numeric(faculty_offers), 
                             y = as.numeric(on_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(values = peer_color)+
  scale_color_manual(values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999")+
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE)+
  annotate("text", x = 4.5, y = 7.5, 
           label = paste0("R: ", round(onsite_offers_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 7.2, 
           label = paste0("p-value: ", round(onsite_offers_cor_coefs$p.value, 25)))+
  #peer data
  annotate("text", x = 4.5, y = 2.5, color = "#CC79A7",
           label = paste0("R: ", round(onsite_offers_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 2.20, color = "#CC79A7",
           label = paste0("p-value: ", round(onsite_offers_peer_cor_coefs$p.value, 10)))+
  #nonpeer data
  annotate("text", x = 2.5, y = 5.5, color = "#56B4E9",
           label = paste0("R: ", round(onsite_offers_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 2.5, y = 5.2, color = "#56B4E9",
           label = paste0("p-value: ", round(onsite_offers_nonpeer_cor_coefs$p.value, 20)))+  
  labs(y = "Number of on-site interviews", x = "Number of faculty offers",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig4_offers_on-site_peer_corr.jpeg")

# Fig 4D Total Number of Interviews, Two groups Median # of applications -- Does the median differ significantly by gender or race? 

# Fig 4E Offer percentage (applied faculty only, other jobs)
fig4e_chi_table <- table(fig4_data$simple_faculty_offer, fig4_data$peer)

fig4e_chi <- chisq.test(fig4e_chi_table)

fig4_data %>% 
  count(simple_faculty_offer, peer) %>% 
  spread(key = simple_faculty_offer, value = n) %>% 
  mutate(total = `0` + `1+`,
         percent = get_percent(`1+`, total),
         peer = paste0(peer, "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=peer, y = as.numeric(percent)))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  labs(y="\nApplicants recieving 1+ faculty offer (%)", 
       x="PEER identity",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave("social_science/figures/fig4_percent_faculty_peer.jpeg")