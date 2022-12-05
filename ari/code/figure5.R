#Figure:Economic stability index, overlay by race/ethnicity & caregiver vs non

fig5_data <- esi_data %>% select(id, esi) %>% 
  left_join(., fig4_data, by = "id") 

# ESI overlay by peer status

fig5_data %>% 
  filter(!is.na(peer)) %>% 
  ggplot(aes(x = peer, y = as.numeric(esi), fill = peer))+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.25, alpha = 0.5)+
  geom_boxplot(alpha = 0.5)+
  labs(x = "Respondent PEER* status (n=761)", y = "Economic Stability Index",
       caption = "*PEER = Persons excluded due to ethnicity/race")+
  my_theme_horiz

ggsave("ari/figures/fig5_identity.jpeg")

# ESI caregiver vs non

fig5_data %>% 
  ggplot(aes(x = fct_reorder(primary_caregiver, as.numeric(esi), median), 
             y = as.numeric(esi), fill = primary_caregiver))+
  geom_dotplot(binaxis = "y", stackdir = "center", 
               dotsize = 0.25, alpha = 0.5)+
  geom_boxplot(alpha = 0.5)+
  labs(x = "Primary caregiver (n=772)", y = "Economic Stability Index")+
  my_theme_horiz

ggsave("ari/figures/fig5_dependents.jpeg")

# ESI caregiver & peer

fig5_data %>% 
  filter(!is.na(peer)) %>% 
  ggplot(aes(x = peer, y = as.numeric(esi)))+
  geom_dotplot(binaxis = "y", stackdir = "center", 
               dotsize = 0.25, alpha = 0.5)+
  geom_boxplot(alpha = 0.5)+
  facet_wrap(~primary_caregiver)+
  labs(x = "Respondent PEER* Status by Primary Caregiver (n=761)", 
       y = "Economic Stability Index",
       caption = "*PEER = Persons excluded due to ethnicity/race")+
  my_theme_horiz

ggsave("ari/figures/fig5_peer_caregiver.jpeg")

# ESI offer status
esi_data %>% 
  select(id, esi) %>% 
  left_join(., outcome_data, by = "id") %>% 
  filter(question == "simple_offers") %>% 
  ggplot(aes(x = response, 
             y = as.numeric(esi), fill = response))+
  geom_dotplot(binaxis = "y", stackdir = "center", 
               dotsize = 0.25, alpha = 0.5)+
  geom_boxplot(alpha = 0.5)+
  labs(x = "Faculty offers (n = 677)", y = "Economic Stability Index")+
  my_theme_horiz

ggsave("ari/figures/fig5_offers.jpeg")