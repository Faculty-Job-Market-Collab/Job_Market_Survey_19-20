#Figure: Within respondents with dependents, split between child/adult dependents

#fig1a <- fig1_data %>% 
#  ggplot()+
#  geom_bar(aes(x = dependents))+
#  coord_flip()+
#  labs(x = "Dependent(s)", y = "Response")+
#  my_theme_horiz

fig1b <- fig1_data %>% 
  filter(dependents != "No dependents") %>% 
  ggplot()+
  geom_bar(aes(x = dependents))+
  coord_flip()+
  labs(x = "Dependent(s)\n(excluding 'No dependents')", 
       y = "Number of responses (n=197)")+
  my_theme_horiz

fig1c <- fig1_data %>% 
  filter(dependents != "No dependents") %>%  
  mutate(dependents = fct_collapse(dependents,
                                   "child(ren)" = c("Yes, multiple children", 
                                                    "Yes, one child"),
                                   "adult(s)" = c("Yes, adult(s) and child(ren)", 
                                                  "Yes, adult(s)"))) %>% 
  ggplot()+
  geom_bar(aes(x = dependents))+
  coord_flip()+
  labs(x = "Dependent types", y = "Number of responses (n=197)")+
  my_theme_horiz

fig1d <- fig1_data %>% 
  mutate(dependents = fct_collapse(dependents,
                                   "dependents" = c("Yes, multiple children", 
                                                    "Yes, one child",
                                                    "Yes, adult(s) and child(ren)",
                                                    "Yes, adult(s)"))) %>% 
  
  count(peer, dependents) %>% 
  spread(key = dependents, value = n) %>% 
  mutate(Total = `No dependents` + dependents,
         percent = round(get_percent(dependents, Total), 
                         digits = 2)) %>% 
  ggplot()+
  geom_col(aes(x = peer, y = percent))+
  coord_flip()+
  labs(x = "PEER* status\n", 
       y = "Percent respondents with dependents (n=769)",
       caption = "*PEER = Persons excluded due to ethnicity or race")+
  my_theme_horiz


fig1e <- fig1_data %>% 
  filter(dependents != "No dependents") %>%  
  count(peer, dependents) %>% 
  spread(key = peer, value = n) %>% 
  mutate(Yes = replace_na(Yes, 0), 
         Total = No + Yes,
         percent = round(get_percent(Yes, Total), digits = 2)) %>% 
  ggplot()+
  geom_col(aes(x = dependents, y = percent))+
  coord_flip()+
  labs(x = "Dependent types\n", y = "Percent PEER* respondents with dependents (n=197)",
       caption = "*PEER = Persons excluded due to ethnicity or race")+
  my_theme_leg_horiz

plot_grid(fig1b, fig1c, fig1d, fig1e, 
          labels = c("A", "B", "C", "D"), nrow = 4)

ggsave("ari/figures/fig1_dependents.jpeg")