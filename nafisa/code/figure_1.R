#Figure 1 -- Applicant scholarly metrics

#Applicant fields
#1a----
fig1a_plot <- demo_data %>% 
  filter(question == "research_category") %>% 
  distinct() %>%
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=765)", x="Research field")+
  my_theme_horiz

ggsave("nafisa/figures/fig1a_field.jpeg")

#B. Bio applicant location

#1b----
fig1b_plot <- bio_demo_data %>% 
  filter(question == "residence") %>% 
  distinct() %>%
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=332)", x="Country of residence")+
  my_theme_horiz

ggsave("nafisa/figures/fig1b_location.jpeg")

#C. Bio applicant position
#1c----
fig1c_plot <- bio_demo_data %>% 
  filter(question == "position") %>% 
  distinct() %>%
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=295)", x="Position")+
  my_theme_horiz

ggsave("nafisa/figures/fig1c_position.jpeg")

#D. Number of postdoc positions
#1d----
fig1d_plot <- bio_network_data %>% 
  filter(question == "number_postdocs") %>% 
  distinct() %>% 
  mutate(response = factor(response, levels = c("1", "2", "3", ">3"))) %>% 
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=119)", x="Number of postdocs")+
  my_theme_horiz

ggsave("nafisa/figures/fig1d_num_postdocs.jpeg")

#E. Applicant metric medians
#1e----
metric_data <- bio_qualifications %>% select(id, 1:5, 7, 9:14, 16) %>% 
  gather(-id, key = question, value = response) %>% 
  distinct()

metric_medians <- metric_data %>% 
  group_by(question) %>% 
  summarise(med = median(as.numeric(response), na.rm = TRUE),
            max = max(as.numeric(response), na.rm = TRUE))

fig1e_plot <- metric_data %>% 
  ggplot(aes(x = question, y=as.numeric(response), fill = question))+
  #geom_dotplot()+
  geom_boxplot()+
  facet_wrap(~question, scales = "free")+
  geom_text(data = metric_medians, aes(x = question, 
                                       y = max*0.7, 
                                       label = paste("median =", med)), 
            size = 5)+
  labs(x = "Applicant metric", y = "Response value")+
  my_theme+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

ggsave("nafisa/figures/fig1e_metric_medians.jpeg")

#F. 1st author papers
#1f----
fig1f_plot <- bio_qualif_data %>% 
  filter(question == "first_author") %>% 
  filter(!is.na(response)) %>% 
  distinct() %>%
  ggplot(aes(x=as.numeric(response)
             ))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=164)", 
       x="Number of first-author papers")+
  my_theme_horiz

ggsave("nafisa/figures/fig1f_num_first_auth.jpeg")

#G. Total publications
#1g----
fig1g_plot <- bio_qualif_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  distinct() %>%
  ggplot(aes(x=as.numeric(response)))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=164)", x="Number of papers")+
  my_theme_horiz

ggsave("nafisa/figures/fig1g_num_peer-review.jpeg")

#H. Total citations
#1h----
fig1h_plot <- bio_qualif_data %>% 
  filter(question == "scholar_citations_all") %>% 
  distinct() %>%
  mutate(papers_bin = get_big_bins(as.numeric(response))) %>% 
  ggplot(aes(x=factor(papers_bin, 
                      levels = bin_levels_big)))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=148)", 
       x="Number of Google Scholar\ncitations")+
  my_theme_horiz

ggsave("nafisa/figures/fig1h_num_citations.jpeg")

#I. H-index
#1i----
fig1i_plot <- bio_qualif_data %>% 
  filter(question == "scholar_hindex") %>% 
  distinct() %>% 
  ggplot(aes(x=as.numeric(response)))+
  geom_bar()+
  coord_flip(xlim = c(0,30))+
  labs(y="Number of responses (n=148)", x="Google Scholar H-index")+
  my_theme_horiz

ggsave("nafisa/figures/fig1i_hindex.jpeg")

#J. CNS papers y/n
#1j----
fig1j_plot <- bio_qualif_data %>% 
  filter(question == "CNS_status") %>% 
  distinct() %>% 
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=163)", x="Published a CNS paper")+
  my_theme_horiz

ggsave("nafisa/figures/fig1j_cns.jpeg")

#K. Fellowships y/n
#1k----
fig1k_plot <- bio_qualif_data %>% 
  filter(question == "fellowship") %>% 
  distinct() %>%
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=202)", 
       x="Received a pre- or\npostdoctoral fellowship")+
  my_theme_horiz

ggsave("nafisa/figures/fig1k_fellowship.jpeg")

#L. Career transition award y/n
#1l----
fig1l_plot <- bio_qualif_data %>% 
  filter(question == "transition_award") %>% 
  distinct() %>% 
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=189)", 
       x="Recieved a career\ntransition award")+
  my_theme_horiz

ggsave("nafisa/figures/fig1l_career_transition.jpeg")

#M. Posted preprints y/n
#1m----
fig1m_plot <- bio_qualif_data %>% 
  filter(question == "preprint_status") %>% 
  distinct() %>% 
  ggplot(aes(x=response))+
  geom_bar()+
  coord_flip()+
  labs(y="Number of responses (n=164)", x="Posted a preprint")+
  my_theme_horiz

ggsave("nafisa/figures/fig1m_preprint.jpeg")

#N. PUI vs RI applicants
#1n----
fig1n_plot <- bio_app_outcomes %>% 
  filter(!is.na(R1_apps_submitted)) %>% 
  select(id, R1_apps_submitted, PUI_apps_submitted) %>% 
  gather(-id, key = question, value = response) %>% 
  mutate(response = get_small_bins(as.numeric(response)),
         question = if_else(str_detect(question, "R1"), "RI", "PUI")) %>% 
  distinct() %>% 
  filter(!is.na(response)) %>%
  ggplot(aes(x=factor(response, bin_levels_small)))+
  geom_bar()+
  coord_flip()+
  facet_wrap(~question)+
  labs(y="Number of responses (n=268)", x="Number of applications\nsubmitted")+
  my_theme_horiz

ggsave("nafisa/figures/fig1n_pui_ri.jpeg")

#generate plot 1----

fig1 <- plot_grid(fig1a_plot, fig1b_plot, fig1c_plot, fig1d_plot, 
                  fig1f_plot, fig1g_plot, fig1h_plot, fig1i_plot,
                  fig1j_plot, fig1k_plot, fig1l_plot, fig1m_plot,
                  fig1n_plot,
                  labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
                             'I', 'J', 'K', 'L', 'M'), ncol = 3)


ggsave("Figure_1.png", device = 'png', units = "in", scale = 1.75,
       path = 'nafisa/figures/', width = 12, height = 11)