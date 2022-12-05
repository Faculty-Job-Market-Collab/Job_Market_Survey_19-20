#Figure:Perception data of this group

grouped_percept_data %>% 
  filter(question != "covid_alter_research") %>% 
  filter(question != "IDP_status") %>% 
  filter(question != "commitment_impact") %>% 
  ggplot(aes(x = response, fill = caregiver))+
  geom_bar(position = "dodge")+
  coord_flip()+
  facet_wrap(~question, nrow = 2)+
  labs(x = "Applicant perception", y = "Number of responses",
       fill = "Has dependents")+
  my_theme_leg

ggsave("ari/figures/fig8_perceptions1.jpeg")

grouped_percept_data %>%
  filter(question == "covid_alter_research") %>%
  ggplot(aes(x = response, fill = caregiver))+
  geom_bar(position = "dodge")+
  coord_flip()+
  #facet_wrap(~question)+
  labs(y = "Number of responses", fill = "Has dependents")+
  my_theme_leg


grouped_percept_data %>% 
  filter(question == "IDP_status") %>% 
  ggplot(aes(x = response, fill = caregiver))+
  geom_bar(position = "dodge")+
  coord_flip()+
  #facet_wrap(~question)+
  labs(y = "Number of responses", x = "IDP",
       fill = "Has dependents")+
  my_theme_leg_horiz

ggsave("ari/figures/fig8_perceptions_idp.jpeg")

grouped_percept_data %>% 
  filter(question == "commitment_impact") %>% 
  ggplot(aes(x = response, fill = caregiver))+
  geom_bar(position = "dodge")+
  coord_flip()+
  #facet_wrap(~question)+
  labs(y = "Number of responses", x = "Impact to Commitment",
       fill = "Has dependents")+
  my_theme_leg_horiz

ggsave("ari/figures/fig8_perceptions_commitment.jpeg")