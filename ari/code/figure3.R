#Figure:spouse/partner/co parent occupations

fig3_data %>% 
  ggplot()+
  geom_bar(aes(x = partner_occupation)) +
  facet_wrap(~primary_caregiver, scales = "free", ncol = 1)+
  coord_flip()+
  labs(x = "Partner occupation\n(separated by primary caregiver)", 
       y = "Number of responses (n=192)", 
       caption = "*SAHP = Stay-at home parent")+
  my_theme_horiz

ggsave("ari/figures/fig3_care_partner-occ.jpeg")