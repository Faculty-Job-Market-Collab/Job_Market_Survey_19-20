
#dump all of the data into figures----

section_list <- tidy_data %>% pull(section) %>% unique()

question_list <- tidy_data %>% pull(question) %>% unique()


#each question by itself----
#all_plots <- map(section_list, function(s){
#  map(question_list, function(p){
#    plot_raw_data(tidy_data, s, p)})
#  })
#
#demo_plots <- compact(all_plots[[1]])
#
#qual_plots <- compact(all_plots[[2]])
#
#percept_plots <- compact(all_plots[[3]])
#
#prep_plots <- compact(all_plots[[4]])
#
#outcome_plots <- compact(all_plots[[5]])
#
#network_plots <- compact(all_plots[[6]])
#
#questions by gender----

tidy_gender <- tidy_data %>% 
  select(id, section, question, response) %>% 
  filter(section == "demographic") %>% 
  filter(question == "gender") %>% 
  select(-section) %>% 
  distinct() %>% 
  spread(question, response)

tidy_gender_data <- left_join(tidy_data, tidy_gender, by = "id") %>% 
  filter(question != "gender") %>% 
  distinct()
  
gender_plots <- map(section_list, function(s){
  map(question_list, function(p){
    plot_raw_data(tidy_gender_data, s, p, "gender")})
})

gender_demo_plots <- compact(gender_plots[[1]])

gender_qual_plots <- compact(gender_plots[[2]])

gender_percept_plots <- compact(gender_plots[[3]])

gender_prep_plots <- compact(gender_plots[[4]])

gender_outcome_plots <- compact(gender_plots[[5]])

gender_network_plots <- compact(gender_plots[[6]])

#questions by race/ethnicity----

tidy_eth <- tidy_data %>% 
  select(id, section, question, response) %>% 
  filter(section == "demographic") %>% 
  filter(question == "race_ethnicity") %>% 
  select(-section) %>% 
  distinct() %>% 
  rowid_to_column(., var = "dummy_id") %>% 
  spread(question, response)

tidy_eth_data <- left_join(tidy_data, tidy_eth, by = "id") %>% 
  select(-dummy_id) %>% 
  filter(question != "race_ethnicity") %>% 
  distinct()

eth_plots <- map(section_list, function(s){
  map(question_list, function(p){
    plot_raw_data(tidy_eth_data, s, p, "race_ethnicity")})
})

eth_demo_plots <- compact(eth_plots[[1]])

eth_qual_plots <- compact(eth_plots[[2]])

eth_percept_plots <- compact(eth_plots[[3]])

eth_prep_plots <- compact(eth_plots[[4]])

eth_outcome_plots <- compact(eth_plots[[5]])

eth_network_plots <- compact(eth_plots[[6]])

eth_leg <- plot_raw_data(tidy_eth_data, "demographic", "gender", "race_ethnicity")+
  theme(legend.position = "right")

#generate full figure----
eth_legend <- get_legend(eth_leg)

plot_legend <- plot_grid(eth_legend, nrow = 1)

# questions by Offer vs No offer----

tidy_offer <- tidy_data %>% 
  select(id, section, question, response) %>% 
  filter(section == "app_outcomes") %>% 
  filter(question == "faculty_offers") %>% 
  select(-section) %>% 
  distinct() %>% 
  mutate(
    response = str_trim(response, side = "both"),
    response = if_else(response >= 1, "Yes", "No")) %>% 
  rowid_to_column(., var = "dummy_id") %>% 
  spread(question, response)

tidy_offer_data <- left_join(tidy_data, tidy_offer, by = "id") %>% 
  select(id, section, question, response, faculty_offers) %>% 
  filter(question != "faculty_offers") %>% 
  filter(!is.na(faculty_offers)) %>% 
  distinct()

offer_plots <- map(section_list, function(s){
  map(question_list, function(p){
    plot_raw_data(tidy_offer_data, s, p, "faculty_offers")})
})

offer_demo_plots <- compact(offer_plots[[1]])

offer_qual_plots <- compact(offer_plots[[2]])

offer_percept_plots <- compact(offer_plots[[3]])

offer_prep_plots <- compact(offer_plots[[4]])

offer_outcome_plots <- compact(offer_plots[[5]])

offer_network_plots <- compact(offer_plots[[6]])

#offer by inst_value
tidy_offer_inst_data <- left_join(tidy_data, tidy_offer, by = "id") %>% 
  select(-question, -response, -section, -dummy_id) %>% distinct() %>% 
  filter(!is.na(faculty_offers)) %>% 
  filter(inst_type != "") %>% 
  distinct()

inst_key_list <- tidy_offer_inst_data %>% pull(inst_key) %>% unique()

inst_type_list <- c("on_site_institutions", "phd_institution", "postdoc_institution")

offer_inst_plots <- map(inst_type_list, function(t){
  map(inst_key_list, function(k){
    
    df <- tidy_offer_inst_data %>% 
      filter(inst_type == t) %>% 
      filter(inst_key == k) %>% 
      count(inst_value, faculty_offers, .drop = FALSE) %>% 
      mutate(percent = get_percent(n, sum(n)))
    
    sum_n <- sum(df$n)
    
    plot <- if(nrow(df)== 0){NULL}else{
      ggplot(df)+
          geom_col(aes_string(x = "inst_value", y = "percent", fill = "faculty_offers"))+
          coord_flip()+
          scale_x_discrete(drop = FALSE)+
          labs(title = paste0("% of ", sum_n, " institutions reported by the ", k))+
          theme(legend.position = "bottom")
      }
    })
})

onsite_plots <- compact(offer_inst_plots[[1]])

phd_plots <- compact(offer_inst_plots[[2]])
  
postdoc_plots <- compact(offer_inst_plots[[3]])
  
offsite_plots <- compact(offer_inst_plots[[1]])

#specific plots----

#first gen phd vs IDP

first_gen_data <- demo_data %>% 
  select(id, section, question, response) %>% 
  filter(question == "first_gen_phd") %>% 
  select(-section, -question) %>% 
  distinct() %>% 
  #spread(question, response) %>% 
  filter(!is.na(response))

idp_data <- perceptions %>% 
  select(id, IDP_status, IDP_helpful) %>% 
  filter(!is.na(IDP_helpful) | !is.na(IDP_status)) %>% 
  distinct() %>% 
  #spread(question, response) %>% 
    gather(-id, key = "IDP_question", value = "IDP_response")

phd_vs_idp_plot <- left_join(first_gen_data, idp_data, by = "id") %>% 
  distinct() %>% filter(!is.na(IDP_question)) %>% 
  filter(!is.na(IDP_response)) %>% 
  count(IDP_question, IDP_response, response) %>% 
  ggplot()+
  geom_col(aes(x = IDP_response, y = n, fill = response))+
  coord_flip()+
  facet_wrap(~IDP_question, scales = "free_y", ncol = 1)+
  labs(fill = "1st gen PhD")+
  theme(legend.position = "bottom")


#Teaching experience versus targeted institution
teach_exp_data <- qualif_data %>% 
  select(id, section, question, response) %>% 
  filter(question == "teaching_status") %>% 
  select(-section, -question) %>% 
  distinct() %>% 
  #spread(question, response) %>% 
  filter(!is.na(response))

app_data <- app_outcomes %>% 
  select(id, PUI_apps_submitted, R1_apps_submitted) %>% 
  filter(!is.na(PUI_apps_submitted) | !is.na(R1_apps_submitted)) %>% 
  distinct() %>% 
  gather(-id, key = "submitting_inst", value = "n")

exp_vs_inst_plot <- left_join(app_data, teach_exp_data, by = "id") %>% 
  distinct() %>% 
  count(id, submitting_inst, response) %>% 
  filter(!is.na(response)) %>% 
  count(submitting_inst, response) %>% 
  ggplot()+
    geom_col(aes(x = response, y = n, fill = submitting_inst))+
  coord_flip()+
  labs(x = "Teaching experience")+
  theme(legend.position = "bottom")

#Adjunct experience versus targeted institution
adjunct_exp_data <- qualif_data %>% 
  select(id, section, question, response) %>% 
  filter(question == "teaching_types") %>% 
  distinct() %>% 
  #spread(question, response) %>% 
  filter(str_detect(response, "adjunct") == TRUE) %>% 
  select(id, question) %>% distinct()
  

adj_vs_inst_plot <- left_join(app_data, adjunct_exp_data, by = "id") %>% 
  distinct() %>% 
  filter(n != "0") %>% 
  count(submitting_inst) %>% 
  ggplot()+
  geom_col(aes(x = submitting_inst, y = n))+
  coord_flip()+
  labs(x = "Adjunct experience")

#Data Summaries ----

#Network
network_summary <- network %>% 
  mutate(across(c(2:5, 13:16), as.numeric)) %>% 
  select(c(id, number_postdocs:num_institution_contacted, scholar_citations_all_2:scholar_hindex_2015_2)) %>% 
  gather(number_postdocs:scholar_hindex_2015_2, key = "Statistic", value = "response") %>% 
  distinct() %>% filter(!is.na(response)) %>% 
  group_by(Statistic) %>% 
  summarise(Number_of_Responses = n(), Minimum = min(response, na.rm = TRUE), IQR = IQR(response, na.rm = TRUE),
            Maximum = max(response, na.rm = TRUE), Mean = round(mean(response, na.rm = TRUE), digits = 2), 
            Median = median(response, na.rm = TRUE))

#Qualifications + preparation
qualif_summary <- preparation %>% select(id, offsite_interview_research_min) %>% 
  left_join(qualifications, .) %>% 
  mutate(across(c(1:5, 7, 9:14, 16, 21), as.numeric)) %>% 
  select(c(id, offsite_interview_research_min, `peer-reviewed_papers`:last_author, 
           preprint_number, CNS_number:scholar_hindex_2015, patent_number)) %>% 
  gather(offsite_interview_research_min:patent_number, key = "Statistic", value = "response") %>% 
  distinct() %>% filter(!is.na(response)) %>% 
  group_by(Statistic) %>% 
  summarise(Number_of_Responses = n(), Minimum = min(response, na.rm = TRUE), IQR = IQR(response, na.rm = TRUE),
            Maximum = max(response, na.rm = TRUE), Mean = round(mean(response, na.rm = TRUE), digits = 2), 
            Median = median(response, na.rm = TRUE))

#App Outcomes
outcome_summary <- app_outcomes %>% 
  mutate(across(c(1:8, 10, 11), as.numeric)) %>% 
  select(id, application_cycles, apps_submitted, R1_apps_submitted, 
         PUI_apps_submitted, off_site_interviews, on_site_interviews, 
         faculty_offers, rejections_recieved) %>% distinct() %>% 
  gather(application_cycles:rejections_recieved, key = "Statistic", value = "response") %>% 
  filter(!is.na(response)) %>% 
  group_by(Statistic) %>% 
  summarise(Number_of_Responses = n(), Minimum = min(response, na.rm = TRUE), IQR = IQR(response, na.rm = TRUE),
            Maximum = max(response, na.rm = TRUE), Mean = round(mean(response, na.rm = TRUE), digits = 2), 
            Median = median(response, na.rm = TRUE))

#income vs responses
income_data <- demo_data %>% 
  select(id, section, question, response) %>% 
  filter(question == "income"| question == "financial_support"| question == "extra_income") %>% 
  select(-section)

offer_response_data <- outcome_data %>%  
  select(id, section, question, response) %>% 
  select(-section) %>% 
  filter(question == "offer_responses") %>% 
  filter(str_detect(response, "Reject") == TRUE) %>% 
  select(id) %>% distinct()

income_plot <- left_join(offer_response_data, income_data, by = "id") %>% 
  count(question, response) %>% 
  filter(!is.na(response)) %>% 
  ggplot()+
  geom_col(aes(x = response, y = n, fill = question))+
  facet_wrap(~question, scales = "free_y", ncol = 1)+
  coord_flip()+
  labs(y = "Number of respondents that rejected at least 1 offer")+
  theme(legend.position = "none")
