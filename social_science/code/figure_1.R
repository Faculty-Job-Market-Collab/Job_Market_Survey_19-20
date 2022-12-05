#Demographics

#Fig 1A Applicant field (by gender)

fig1_all_data %>% 
  ggplot(aes(x=research_category, fill=simple_gender))+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = cbPalette)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Number of responses", x="Research field",
       fill = "Gender")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig1_field.jpeg")

#Fig 1B Race/Ethnicity (include all categories)

fig1_all_data %>% 
  ggplot(aes(x=research_category, fill=peer))+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = cbPalette)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Number of responses", x="Research field\n",
       fill = "PEER*", caption = "*PEER=Persons excluded for ethnicity/race")+
  my_theme_leg_bottom

ggsave("social_science/figures/fig1_race-eth.jpeg")

#Fig 1C Target institution type (by gender)
fig1_inst_data %>% 
  ggplot(aes(x=factor(response, bin_levels_small), 
             fill = simple_gender))+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = cbPalette)+
  coord_flip()+
  facet_wrap(~question)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Number of responses", x="Number of applications submitted",
       fill = "Gender", caption = "GNC=gender non-conforming")+
  my_theme_leg_horiz

ggsave("social_science/figures/fig1_target_inst.jpeg")

#Fig 1D Applicant location (USA, Canada, UK) & where applied to jobs

ss_demo_data %>% 
  filter(question == "residence") %>% 
  distinct() %>% 
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Number of responses", x="Country of residence")+
  my_theme_horiz

ggsave("social_science/figures/fig1_location.jpeg")

#Fig 1E Current position
ss_demo_data %>% 
  filter(question == "position") %>% 
  distinct() %>% 
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Number of responses", x="Current position")+
  my_theme_horiz

ggsave("social_science/figures/fig1_position.jpeg")

#Fig 1F Total years as postdoc - Compare social sciences and humanities (Two-tailed Wilcoxon rank sum test)

#Fig 1G Number of postdoc positions

fig1g_data %>% 
  ggplot(aes(x=number_postdocs, 
             fill = research_category))+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = cbPalette)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Number of responses", x="Number of postdoctoral positions",
       fill = "Field:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig1_num_postdocs.jpeg")

#Fig 1H How many times have you applied for TT/TT-equivalent positions? - Compare social sciences and humanities (Two-tailed Wilcoxon rank sum test)
fig1h_wilcox <- wilcox.test(num_application_cycles ~ research_category, 
                            data = fig1h_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig1h_wilcox, 
            file = "social_science/figures/fig1_app_cycles_research_mwu_test.csv")

fig1h_data %>% 
  ggplot(aes(x=application_cycles, 
             fill = research_category))+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = cbPalette)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Number of responses", 
       x="Number of application cycles\n",
       fill = "Field:",
       caption = paste("Mann-Whitney U: p<0.01"))+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig1_app_cycles.jpeg")


#Fig 1I Mean Applicant Metrics (please provide separate for social sciences & humanities)
# 1st author pubs, total pubs, total citations, h-index, fellowships, career transition award 

metric_medians <- metric_data %>% 
  group_by(question, research_category) %>% 
  summarise(med = median(as.numeric(response), na.rm = TRUE),
            max = max(as.numeric(response), na.rm = TRUE))

metric_data %>% 
  group_by(question, research_category) %>% 
  ggplot(aes(x = question, y=as.numeric(response), fill = research_category))+
  #geom_dotplot()+
  geom_boxplot(position = "dodge")+
  scale_fill_manual(values = cbPalette)+
  facet_wrap(~question, scales = "free")+
  geom_text(data = metric_medians, aes(group = research_category, x = question, 
                                       y = max*0.8,
                                       label = paste("med=", med)), 
            position=position_dodge(width=1), size = 5)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "", y = "Response value",
       fill = "Research Category:")+
  my_theme_leg_bottom_horiz+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

ggsave("social_science/figures/fig1_metric_medians.jpeg")


grants_data %>% 
  count(question, research_category, response) %>%
  spread(key = response, value = n) %>% 
  mutate(Yes = if_else(is.na(Yes), 0, as.numeric(Yes)),
         Total = Yes + as.numeric(No),
    percent_awarded = get_percent(x = Yes, y = Total),
    question = if_else(question == "fellowship", "Fellowship", "Transition Award")) %>% 
  ggplot(aes(x = question, y = percent_awarded, fill = research_category))+
  #geom_dotplot()+
  geom_col(position = "dodge")+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0), limits = c(0,90))+
  geom_text(aes(x = question, group = research_category, y = percent_awarded + 5,
                label = paste0("(n = ", Total, ")")), 
            position=position_dodge(width=1), size = 4)+
  labs(x = "Applicant metric", y = "Percent Awarded",
       fill = "Field:")+
  my_theme_leg_bottom_horiz

ggsave("social_science/figures/fig1_awards.jpeg")

### teaching experience: TA experience, Visiting Assistant Professorship, 
# Total Adjunct Teaching positions, Teaching Certificate

teaching_data %>% 
  ggplot(aes(x = response, fill = research_category))+
  #geom_dotplot()+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = cbPalette)+
  facet_wrap(~question, scale = "free_y", ncol = 1)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Teaching experience", y = "Number of responses",
       fill = "Field:")+
  my_theme_leg_bottom_horiz


ggsave("social_science/figures/fig1_teaching.jpeg")