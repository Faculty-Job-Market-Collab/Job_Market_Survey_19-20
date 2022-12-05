#Figure:Average applicant metrics for this group?

# Number of postdoc positions----

grouped_network_data %>% 
  filter(question == "number_postdocs") %>% 
  distinct() %>% 
  mutate(response = factor(response, 
                           levels = c("1", "2", "3", ">3"))) %>% 
  ggplot(aes(x=response, fill=caregiver))+
  geom_bar(position = "dodge")+
  coord_flip()+
  labs(y="Number of respondents (n=199)", x="Number of postdocs",
       fill = "Dependents")+
  my_theme_leg_horiz

ggsave("ari/figures/fig6_num_postdocs.jpeg")

#Applicant metric medians

metric_data <- grouped_tidy_data %>% 
  spread(-id, -caregiver, -research_field, key = question, value = response) %>% 
  distinct()

grouped_metric_medians <- grouped_tidy_data %>% 
  filter(!is.na(research_field)) %>% 
  filter(question %in% c("peer-reviewed_papers", "first_author",
                         "scholar_citations_all", "scholar_hindex")) %>% 
  mutate(log_value = log(as.numeric(response))) %>% 
  group_by(question, research_field) %>% 
  summarise(med = median(as.numeric(response), na.rm = TRUE),
            min = min(as.numeric(response), na.rm = TRUE),
            max = max(as.numeric(response), na.rm = TRUE),
            med_log = median(as.numeric(log_value), na.rm = TRUE),
            min_log = min(as.numeric(log_value), na.rm = TRUE),
            max_log = max(as.numeric(log_value), na.rm = TRUE),
            n = n())

metric_medians <- grouped_tidy_data %>% 
  filter(!is.na(research_field)) %>% 
  filter(question %in% c("peer-reviewed_papers", "first_author",
                         "scholar_citations_all", "scholar_hindex")) %>% 
  group_by(question, caregiver) %>% 
  summarise(med = median(as.numeric(log_value), na.rm = TRUE),
            min = min(as.numeric(log_value), na.rm = TRUE),
            max = max(as.numeric(log_value), na.rm = TRUE),
            n = n())

care_met_med <- metric_medians %>% 
  filter(caregiver == "Yes")

non_care_met_med <- metric_medians %>% 
  filter(caregiver == "No")

grouped_tidy_data %>% 
  filter(!is.na(research_field)) %>% 
  filter(question %in% c("peer-reviewed_papers", "first_author",
                         "scholar_citations_all", "scholar_hindex")) %>% 
  #filter(caregiver == "Yes") %>% 
  group_by(caregiver) %>% 
  ggplot(aes(x = caregiver, y=as.numeric(log_value), fill = caregiver))+
  #geom_dotplot()+
  geom_boxplot()+
  facet_wrap(~question)+
  geom_text(data = metric_medians, 
              aes(x = caregiver, y = max+0.7, 
                  label = paste("median =", round(med, digits = 2))), 
            size = 5)+
  labs(x = "Scholarly metric by dependent status", 
       y = "Response value (log)", fill = "Caregiver")+
  my_theme_leg_bottom_horiz

ggsave("ari/figures/fig6_metric_medians_log.jpeg")

#grouped_tidy_data %>% 
#  filter(!is.na(research_field)) %>% 
#  filter(question %in% c("peer-reviewed_papers", "first_author",
#                         "scholar_citations_all", "scholar_hindex")) %>%  
#  filter(caregiver == "No") %>% 
#  ggplot(aes(x = question, y=as.numeric(log_value), fill = question))+
#  #geom_dotplot()+
#  geom_boxplot()+
#  facet_wrap(~question)+
#  geom_text(data = non_care_met_med, aes(x = question, 
#                                       y = max*0.7, 
#                                       label = paste("median =", 
#                                                     round(med, digits = 2))), 
#            size = 5)+
#  labs(x = "Scholarly metric (applicants without dependents)", y = "Response value")+
#  my_theme+
#  theme(axis.text.x = element_blank(), 
#        axis.ticks.x = element_blank())
#
#ggsave("ari/figures/fig6_metric_medians_no_dep.jpeg")

#F. 1st author papers----

#grouped_tidy_data %>% 
#  filter(question == "first_author_binned") %>% 
#  filter(!is.na(response)) %>% 
#  distinct() %>%
#  ggplot(aes(x=factor(response, levels = bin_levels_small), 
#             fill=caregiver))+
#  geom_bar(position = "dodge")+
#  coord_flip()+
#  labs(y="Number of responses (n=367)", 
#       x="Number of first author papers",
#       fill = "Has dependents")+
#  my_theme_leg_horiz
#
#ggsave("ari/figures/fig6_num_first_auth.jpeg")

#G. Total publications----

#grouped_qualif_data %>% 
#  filter(question == "peer-reviewed_papers") %>% 
#  filter(!is.na(response)) %>% 
#  distinct() %>% 
#  ggplot(aes(x=factor(response, levels = bin_levels_small), 
#             fill=caregiver))+
#  geom_bar(position = "dodge")+
#  coord_flip()+
#  labs(y="Number of responses", x="Number of papers",
#       fill = "Has dependents")+
#  my_theme_leg_horiz
#
#ggsave("ari/figures/fig6_num_peer-review.jpeg")

#H. Total citations----
#grouped_qualif_data %>% 
#  filter(question == "scholar_citations_all") %>% 
#  distinct() %>% 
#  ggplot(aes(x=factor(response, levels = bin_levels_big), 
#             fill=caregiver))+
#  geom_bar(position = "dodge")+
#  coord_flip()+
#  labs(y="Number of responses", x="Number of Google Scholar citations",
#       fill = "Has dependents")+
#  my_theme_leg_horiz
#
#ggsave("ari/figures/fig6_num_citations.jpeg")

#I. H-index----

#grouped_qualif_data %>% 
#  filter(question == "scholar_hindex") %>% 
#  distinct() %>% 
#  ggplot(aes(x=factor(response, levels = bin_levels_small), fill=caregiver))+
#  geom_bar(position = "dodge")+
#  coord_flip()+
#  labs(y="Number of responses", x="Google Scholar H-index",
#       fill = "Has dependents")+
#  my_theme_leg_horiz
#
#ggsave("ari/figures/fig6_hindex.jpeg")

#J. CNS papers y/n----

prop_yes_metrics_data %>% 
  filter(!is.na(research_field)) %>% 
  ggplot(aes(x=research_field, y=percent, fill=caregiver))+
  geom_col(position = "dodge")+
  facet_wrap(~question)+
  coord_flip()+
  labs(y="Percent responding 'yes' (n=373)", x="",
       fill = "Has dependents")+
  my_theme_leg_bottom_horiz

ggsave("ari/figures/fig6_status.jpeg")

#K. Fellowships y/n----
#grouped_qualif_data %>% 
#  filter(question == "fellowship") %>% 
#  distinct() %>% 
#  ggplot(aes(x=response, fill=caregiver))+
#  geom_bar(position = "dodge")+
#  coord_flip()+
#  labs(y="Number of responses", 
#       x="Received a pre- or\npostdoctoral fellowship",
#       fill = "Has dependents")+
#  my_theme_leg_horiz
#
#ggsave("ari/figures/fig6_fellowship.jpeg")

#L. Career transition award y/n----

#grouped_qualif_data %>% 
#  filter(question == "transition_award") %>% 
#  distinct() %>% 
#  ggplot(aes(x=response, fill=caregiver))+
#  geom_bar(position = "dodge")+
#  coord_flip()+
#  labs(y="Number of responses", x="Recieved a career transition award",
#       fill = "Has dependents")+
#  my_theme_leg_horiz
#
#ggsave("ari/figures/fig6_career_transition.jpeg")

#M. Posted preprints y/n----
#grouped_qualif_data %>% 
#  filter(question == "preprint_status") %>% 
#  distinct() %>% 
#  ggplot(aes(x=response, fill=caregiver))+
#  geom_bar(position = "dodge")+
#  coord_flip()+
#  labs(y="Number of responses", x="Posted a preprint",  
#       fill = "Has dependents")+
#  my_theme_leg_horiz
#
#ggsave("ari/figures/fig6_preprint.jpeg")

#N. PUI vs RI applicants----
grouped_app_outcomes %>% 
  filter(!is.na(R1_apps_submitted)) %>% 
  select(id, caregiver, R1_apps_submitted, PUI_apps_submitted) %>% 
  gather(-id, -caregiver, key = question, value = response) %>% 
  mutate(response = get_small_bins(as.numeric(response)),
         question = if_else(str_detect(question, "R1"), "RI", "PUI")) %>% 
  distinct() %>% 
  filter(!is.na(response)) %>% 
  ggplot(aes(x=factor(response, bin_levels_small), fill = caregiver))+
  geom_bar(position = "dodge")+
  coord_flip()+
  facet_wrap(~question)+
  labs(y="Number of responses", x="Number of applications submitted", 
       fill = "Has dependents")+
  my_theme_leg_horiz

ggsave("ari/figures/fig6_pui_ri.jpeg")