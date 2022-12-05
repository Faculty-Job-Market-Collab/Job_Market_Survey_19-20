#Figure:Proportion who are the primary caregiver

fig2a <- fig2_data %>% 
  ggplot()+
  geom_bar(aes(x = primary_caregiver))+
  coord_flip()+
  labs(x = "Primary caregiver", y = "Number of responses")+
  my_theme_horiz

fig2b <- fig2_data %>% 
  filter(primary_caregiver != "No dependents") %>% 
  ggplot()+
  geom_bar(aes(x = primary_caregiver))+
  coord_flip()+
  labs(x = "Primary caregiver\n(excludes 'No dependents')", 
       y = "Number of responses (n=207)")+
  my_theme_horiz

fig2c <- fig2_data %>% 
  filter(primary_caregiver != "No dependents") %>% 
  filter(!is.na(adjusted_gender)) %>% 
  ggplot()+
  geom_bar(aes(x = primary_caregiver, fill = adjusted_gender), 
           position = "dodge")+
  coord_flip()+
  labs(x = "Primary caregiver\n(excludes 'No dependents')", 
       y = "Number of responses (n=193)", fill = "Respondent\nGender")+
  my_theme_leg_horiz

plot_grid(fig2b, fig2c, labels = c("A", "B"), nrow = 2)

ggsave("ari/figures/fig2_caregiver.jpeg")