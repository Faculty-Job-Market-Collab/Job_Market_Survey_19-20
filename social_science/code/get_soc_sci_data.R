source("code/load_data.R")

# Identify social science/humanities responses
ss_ids <- demo_data %>% 
  filter(response == "Social, Behavior, & Economic Sciences" |
           response == "Humanities") %>% 
  distinct() %>% select(id)

ss_tidy_data <- left_join(ss_ids, tidy_data, by = 'id') 

ss_demo_data <- left_join(ss_ids, demo_data, by = 'id') %>% distinct()

ss_network_data <- left_join(ss_ids, network_data, by = 'id') %>% distinct()

ss_network <- left_join(ss_ids, network, by = 'id') %>% distinct()

ss_qualifications <- left_join(ss_ids, qualifications, by = 'id') %>% distinct()

ss_qualif_data <- left_join(ss_ids, qualif_data, by = 'id') %>% distinct()

ss_app_outcomes <- left_join(ss_ids, app_outcomes, by = 'id') %>% distinct()

#Figure 1----
fig1_id <- demo_data %>% 
  filter(question == "peer") %>% 
  distinct() %>% 
  spread(key = question, value = response) 

fig1_gender <- demo_data %>% 
  filter(question == "simple_gender") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  mutate(simple_gender = fct_relevel(simple_gender, 
                                    c("Man", "Woman/Trans/GNC", "No Response")))

fig1_demo <- full_join(fig1_id, fig1_gender) %>% 
  distinct() %>% select(-section)

fig1_res <- demo_data %>% 
  filter(question == "research_category") %>% 
  distinct() %>% 
  spread(key = question, value = response)

fig1_all_data <- full_join(fig1_gender, fig1_id, by = "id") %>% 
  select(-section.x, -section.y) %>% 
  left_join(., fig1_res, by = "id") %>% 
  select(-section) %>% distinct() 

fig1_inst_data <- ss_tidy_data %>% 
  filter(str_detect(question, ".+_apps_submitted.+")) %>% 
  select(id, question, response) %>% 
  left_join(., fig1_gender, by = "id") %>% 
  select(-section) %>% 
  distinct() %>% 
  filter(!is.na(response))

fig1g_data <- ss_network %>% 
  select(id, number_postdocs) %>% 
  left_join(., fig1_res) %>% 
  select(-section) %>% distinct()

fig1h_data <- ss_app_outcomes %>% 
  select(id, application_cycles) %>% 
  left_join(., fig1_res) %>% 
  select(-section) %>% distinct() %>% 
  mutate(num_application_cycles = if_else(
    str_detect(application_cycles, ">5"), 
    5, as.numeric(application_cycles)))

metric_data <- ss_qualif_data %>%
  left_join(., fig1_res, by = "id") %>% 
  select(-section.x, - section.y) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("first_author",
                         "peer-reviewed_papers", "scholar_hindex",
                         "scholar_citations_all")) %>% 
  distinct()

grants_data <- ss_qualif_data %>%
  left_join(., fig1_res, by = "id") %>% 
  select(-section.x, - section.y) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("transition_award", "fellowship")) %>% 
  distinct()

teaching_data <- ss_qualif_data %>%
  left_join(., fig1_res, by = "id") %>% 
  select(-section.x, - section.y) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("teaching_status", "teaching_types")) %>% 
  filter(response %in% c("Teaching certificate", "Total adjunct positions", 
                         "Visiting assistant professor", "No",
                         "Yes, experience beyond TA", "Yes, TA")) %>% 
  distinct()
  
source("social_science/code/figure_1.R")

#Figure 2----

fig2_data <- left_join(ss_qualif_data, fig1_gender, by = "id") %>% 
  filter(!is.na(question)) %>% 
  select(-section.x, -section.y) %>% 
  distinct()

fig2A_data <- fig2_data %>% 
  filter(question == "first_author_binned" | 
           question == "first_author") %>% 
  distinct() 

fig2ef_data <- fig2_data %>% 
  filter(question == "grants_awarded") %>% 
  mutate(fellowship = if_else(str_detect(response, "Fellowship"), "yes", "no"),
         grant = if_else(str_detect(response, "Grant"), "yes", "no")) %>% 
  select(-question, -response) %>% distinct() %>% 
  left_join(ss_ids, ., by = "id") %>% 
  filter(!is.na(grant))

source("social_science/code/figure_2.R")


#Figure 3----
fig3_data <- left_join(ss_qualif_data, fig1_demo, by = "id") %>% 
  filter(!is.na(question)) %>% 
  select(-section) %>% 
  distinct()

fig3A_data <- fig3_data %>% 
  filter(question == "first_author_binned" | 
           question == "first_author") %>% 
  distinct() 

fig3ef_data <- fig3_data %>% 
  filter(question == "grants_awarded") %>% 
  mutate(fellowship = if_else(str_detect(response, "Fellowship"), "yes", "no"),
         grant = if_else(str_detect(response, "Grant"), "yes", "no")) %>% 
  select(-question, -response) %>% distinct() %>% 
  left_join(ss_ids, ., by = "id") %>% 
  filter(!is.na(grant))

source("social_science/code/figure_3.R")


#Figure 4----
fig4_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "simple_gender" | question == "off_site_interviews" |
           question == "on_site_interviews" | question == "peer" |
           question == "faculty_offers" | 
           question == "apps_submitted_binned" |
           question == "apps_submitted") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(apps_submitted)) %>% 
  mutate(simple_faculty_offer = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) >= 1 ~ "1+"
  ),
  simple_faculty_offer = factor(simple_faculty_offer, 
                                levels = c("0", "1+")))

ss_id_simple_offer <- select(fig4_data, id, simple_faculty_offer)

fig4_data_two <- fig4_data %>% 
  filter(simple_gender != "No Response")

fig4_data_women <- fig4_data %>% 
  filter(simple_gender == "Woman/Trans/GNC")

fig4_data_men <- fig4_data %>% 
  filter(simple_gender == "Man")

fig4_data_peer <- fig4_data %>% 
  filter(peer == "Yes")

fig4_data_nonpeer <- fig4_data %>% 
  filter(peer == "No")

fig4_tidy_data <- fig4_data %>% 
  gather(2:6, key = question, value = response)

source("social_science/code/figure_4.R")


#Figure 5----
fig5_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "application_cycles" | question == "faculty_offers" |
           question == "grants_awarded"| question == "teaching_status" |
           question == "apps_submitted" |
           question == "PUI_apps_submitted") %>% 
  left_join(., fig1_demo, by = "id") %>% 
  distinct()

fig5f_data <- fig5_data %>%  
  filter(question == "grants_awarded") %>% 
  filter(response == "Predoctoral Fellowship") %>% 
  #filter(!is.na(response)) %>% 
  select(-question) %>% 
  distinct() %>% 
  mutate(predoctoral = "yes") %>% 
  left_join(fig5_data, ., by = c("id", "peer", "simple_gender")) %>% 
  select(id, peer, simple_gender, predoctoral) %>% distinct() %>% 
  mutate(predoctoral = if_else(is.na(predoctoral), "no", predoctoral))

fig5i_data <- fig5_data %>% 
  filter(question == "teaching_status" | question == "faculty_offers" |
           question == "apps_submitted" |
           question == "PUI_apps_submitted") %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(apps_submitted)) %>% 
  mutate(percent_PUI = get_percent(PUI_apps_submitted, apps_submitted),
         percent_offer = get_percent(faculty_offers, apps_submitted))

source("social_science/code/figure_5.R")

#Figure 6----
fig6_data <- left_join(ss_tidy_data, ss_id_simple_offer, by = "id") %>% 
  left_join(., fig1_demo, by = "id") %>% distinct()

fig6a_data <- fig6_data %>% 
  filter(inst_type == "phd_institution" & inst_key == "PUI_RI") %>% 
  select(-question, -inst_type, -section, -inst_key, -response) %>% 
  distinct()

fig6b_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  distinct() %>% 
  filter(question == "advisor_rank") %>% 
  left_join(ss_id_simple_offer, ., by = "id") %>% 
  distinct()

fig6c_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "app_feedback") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  left_join(ss_ids, ., by = "id") %>% 
  left_join(., fig1_demo, by = "id")

fig6d_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "interview_feedback") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  left_join(ss_ids, ., by = "id") %>% 
  left_join(., fig1_demo, by = "id")

fig6e_data <- fig6_data %>% 
  select(id, question, response) %>% 
  filter(question == "workshops_home") %>% distinct() %>% 
  left_join(ss_ids, ., by = "id")%>% 
  left_join(., fig1_demo, by = "id")

fig6f_data <- fig6_data %>% 
  select(id, question, response) %>% 
  filter(question == "workshops_third_party") %>% distinct() %>% 
  left_join(ss_ids, ., by = "id") %>% 
  left_join(., fig1_demo, by = "id")

fig6g_data <- fig6_data %>% 
  select(id, question, response) %>% 
  filter(question == "website_hosted") %>% distinct() %>% 
  left_join(ss_ids, ., by = "id") %>% 
  left_join(., fig1_res, by = "id")

fig6h_data <- fig6_data %>% 
  select(id, question, response) %>% 
  filter(question == "social_media") %>% distinct() %>% 
  left_join(ss_ids, ., by = "id") %>% 
  left_join(., fig1_res, by = "id")

source("social_science/code/figure_6.R")

#Figure 7----
fig7_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("first_author",
                        "peer-reviewed_papers", "scholar_hindex",
                        "scholar_citations_all","transition_award", 
                        "fellowship", "application_cycles", "faculty_offers",
                        "apps_submitted", "grants_awarded")) %>% 
  distinct()

offer_percent_data <- fig7_data %>% 
  filter(question == "apps_submitted" | question == "faculty_offers") %>% 
  spread(key = question, value = response) %>% 
  mutate(perc_offers = get_percent(faculty_offers, apps_submitted)) %>% 
  select(id, perc_offers)

fig7_summary <- fig7_data %>% 
  filter(question %not_in% c("transition_award", "fellowship",
                             "grants_awarded")) %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE))

fig7_data <- left_join(fig7_data, offer_percent_data, by = "id") %>% 
  distinct()

source("social_science/code/figure_7.R")

#demo table
demo_table <- ss_demo_data %>% 
  filter(question %in% c("adjusted_gender", "age", "research_category", 
                         "residence", "peer", "dependents",
                         "position", "legal_status", "disability_status", 
                         "first_gen_undergrad", "first_gen_phd")) %>% 
  mutate(response = fct_collapse(response, 
                                 "Citizen/Resident" = c("Citizen", "Permanent resident"),
                                 "Visa" = c("Student visa", "Work visa"),
                                 "Other" = c("Choose not to disclose", "Outside the US or CA"),
                                 "Yes" = c("Yes, hidden", "Yes, visible"),
                                 "Yes, multiple children/adult(s)" = c("Yes, adult(s)", "Yes, multiple children"),
                                 "< 30 years old" = c("20 - 25 years old", "26 - 30 years old")
                                                      )) %>% 
  count(question, response) %>% 
  mutate(percent_total = get_percent(n, 276))

#write_csv(demo_table, "social_science/figures/demographics.csv")

#table of application metrics
metrics_table <- fig7_data %>% 
  filter(question %not_in% c("transition_award", "fellowship",
                             "grants_awarded")) %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE), 
            std_dev = round(sd(as.numeric(response, na.rm = TRUE)), 
                            digits = 2),
            min_val = min(as.numeric(response, na.rm = TRUE)),
            max_val = max(as.numeric(response, na.rm = TRUE))) %>% 
  mutate(range = paste0("(", min_val, ", ", max_val, ")")) %>% 
  select(-min_val, -max_val)

#write_csv(metrics_table, "social_science/figures/metrics.csv")

metrics_table2 <- fig7_data %>% 
  filter(question == "grants_awarded") %>% 
  count(question, response) %>% 
  mutate(percent = get_percent(n, 276))

#write_csv(metrics_table2, "social_science/figures/grants.csv")
