#Figure:Who is a caregiver by race/ethnicity

fig4_data %>% 
  ggplot()+
  geom_bar(aes(x = primary_caregiver, fill = peer), 
           position = "dodge") +
  #facet_wrap(~peer, scales = "free_x", ncol = 1)+
  coord_flip()+
  labs(x = "The primary caregiver according to race/ethnicity", 
       y = "Number of respondents")+
  my_theme_horiz

ggsave("ari/figures/fig4_care_race-eth1.jpeg")

fig4_data %>% 
  select(-n) %>%
  filter(identity == "single") %>% 
  ggplot()+
  geom_bar(aes(x = primary_caregiver, fill = response), position = "dodge") +
  #facet_wrap(~response, ncol = 1)+
  coord_flip()+
  labs(x = "Primary caregiver", 
       y = "Number of respondents identifying as a single race/ethnicty")+
  my_theme_leg_horiz

ggsave("ari/figures/fig4_care_race-eth2.jpeg")

fig4_data %>% 
  select(-n) %>%
  filter(identity == "single") %>% 
  filter(primary_caregiver != "No dependents") %>% 
  ggplot()+
  geom_bar(aes(x = primary_caregiver, fill = response), position = "dodge") +
  #facet_wrap(~response, ncol = 1)+
  coord_flip()+
  labs(x = "Primary caregiver\n(excluding 'No dependents')", 
       y = "Number of respondents identifying as a single race/ethnicty")+
  my_theme_leg_horiz

ggsave("ari/figures/fig4_care_race-eth3.jpeg")

fig4_data %>% 
  select(-n, -response) %>%
  filter(identity == "multiple") %>% 
  distinct() %>%
  ggplot()+
  geom_bar(aes(x = primary_caregiver)) +
  #facet_wrap(~response, ncol = 1)+
  coord_flip()+
  labs(x = "Primary caregiver", 
       y = "Number of respondents identifying as multi-racial")+
  my_theme_horiz

ggsave("ari/figures/fig4_care_race-eth4.jpeg")
