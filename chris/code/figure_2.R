#    PRESTIGE
#Group Ph.D. and/or postdoc training institutions by some metrics of “prestige” and see if offer percentage differed based on training institution “tier”?
#  Could use US News rankings, THE/World University Rankings, NSF research expenditures 
#Papers for inspiration: 
#  Six-fold over-representation of graduates from prestigious universities does not necessitate unmeritocratic selection in the faculty hiring process
# Systematic inequality and hierarchy in faculty hiring networks

#2A Analyze distribution of h-indexes of Ph.D. and postdoc advisors in our respondent pool and 
# look at effects of applicants training with advisors with h-indices in top 25%, 20%, etc…

hindex_rank_tidy %>% 
  filter(response <= 300) %>% 
  ggplot(aes(x = question, y = as.numeric(response)))+
  geom_boxplot()+
  geom_point(aes(color = hindex_rank), position = "jitter")+
  facet_wrap(~question, scales = "free")+
  labs(y = "H index", x = "", color = "Rank")+
  my_theme_leg_horiz

ggsave("chris/figures/fig2_hindex_distribution.jpeg")

fig2a_data %>% 
  filter(!is.na(scholar_hindex)) %>% 
  ggplot(aes(x = as.numeric(response), y = as.numeric(scholar_hindex)))+
  geom_point(aes(color = hindex_rank))+
  facet_wrap(~question, scales = "free")+
  #scale_fill_manual(values = cbPalette)+
  #scale_color_manual(values = cbbPalette)+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(color = hindex_rank), method=lm, se=TRUE)+
  labs(x = "Value", y = "Scholar H index", color = "Rank")

ggsave("chris/figures/fig2_scholar_hindex_outcomes_corr.jpeg")

fig2a_data %>% 
  filter(!is.na(phd_advisor_hindex)) %>% 
  ggplot(aes(x = as.numeric(response), y = as.numeric(phd_advisor_hindex)))+
  geom_point(aes(color = hindex_rank))+
  facet_wrap(~question, scales = "free")+
  coord_cartesian(ylim = c(0, 5000))+
  #scale_fill_manual(values = cbPalette)+
  #scale_color_manual(values = cbbPalette)+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(color = hindex_rank), method=lm, se=TRUE)+
  labs(y = "PhD advisor H index", x = "Value", color = "Rank")+
  my_theme_horiz

ggsave("chris/figures/fig2_phd_advisor_hindex_outcomes_corr.jpeg")

fig2a_data %>% 
  filter(!is.na(postdoc_advisor_hindex)) %>% 
  ggplot(aes(x = as.numeric(response), y = as.numeric(postdoc_advisor_hindex)))+
  geom_point(aes(color = hindex_rank))+
  facet_wrap(~question, scales = "free")+
  coord_cartesian(ylim = c(0, 1000))+
  #scale_fill_manual(values = cbPalette)+
  #scale_color_manual(values = cbbPalette)+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(color = hindex_rank), method=lm, se=TRUE)+
  labs(y = "Postdoc advisor H index", x = "Value", color = "Rank")+
  my_theme_horiz

ggsave("chris/figures/fig2_phd_advisor_hindex_outcomes_corr.jpeg")

#2B - From what institution did you receive your Ph.D. degree?

fig2b_data %>% 
  filter(inst_type == "phd_institution" | 
           inst_type == "postdoc_institution") %>% 
  ggplot(aes(x = inst_type, y = as.numeric(drdeg_rank)))+
  geom_boxplot()+
  geom_point(position = "jitter")+
  labs(y = "Doctoral degree rank", x = "Institution type")+
  my_theme_horiz

ggsave("chris/figures/fig2_drdeg_rank_phd_postdoc_inst.jpeg")

fig2b_data %>% 
  filter(inst_type == "phd_institution" | 
           inst_type == "postdoc_institution") %>% 
  ggplot(aes(x = inst_type, y = as.numeric(the_rank)))+
  geom_boxplot()+
  geom_point(position = "jitter")+
  labs(y = "THE World University rank", x = "Institution type")+
  my_theme_horiz

ggsave("chris/figures/fig2_THE_rank_phd_postdoc_inst.jpeg")

#CNS publications of applicants
#2c -  Q30 - Do you have any Cell, Nature, or Science publications?


#2d -  Look at offer percentage or presence of faculty offer by those who respond yes vs no

#2e -  Q31 & Q32 ask about # of CNS publications as author vs first-author
# Could investigate faculty offer status or offer percentage by # of CNS papers, first-author papers
