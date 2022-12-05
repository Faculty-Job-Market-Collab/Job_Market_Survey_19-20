
source("code/load_data.R")

library(ggsignif)
# Identify biological science responses
bio_ids <- demo_data %>% 
  filter(response == "Biological Sciences") %>% 
  distinct() %>% select(id)

bio_tidy_data <- left_join(bio_ids, tidy_data, by = 'id') 

#Figure 1.
bio_demo_data <- left_join(bio_ids, demo_data, by = 'id') %>% distinct()

bio_network_data <- left_join(bio_ids, network_data, by = 'id') %>% distinct()

bio_qualifications <- left_join(bio_ids, qualifications, by = 'id') %>% distinct()

bio_qualif_data <- left_join(bio_ids, qualif_data, by = 'id') %>% distinct()

bio_app_outcomes <- left_join(bio_ids, app_outcomes, by = 'id') %>% distinct()

source("nafisa/code/figure_1.R")

## Figure 2. 
fig2_data <- bio_tidy_data %>% 
  filter(question == "adjusted_gender" | 
           question == "residence" | question == "position" |
           question == "number_postdocs") %>% 
  select(id, question, response) %>% distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(adjusted_gender)) 

source("nafisa/code/figure_2.R")

# Fig 3.
fig3_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "adjusted_gender" | question == "off_site_interviews" |
           question == "on_site_interviews" | 
           question == "faculty_offers" | 
           question == "apps_submitted_binned" |
           question == "apps_submitted") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(adjusted_gender)) %>% 
  filter(!is.na(faculty_offers)) %>% 
  mutate(faculty_offers = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) == 1 ~ "1",
    as.numeric(faculty_offers) >= 1 ~ ">1"
  ),
  faculty_offers = factor(faculty_offers, levels = c("0", "1", ">1")))

source("nafisa/code/figure_3.R")

# Figure 4. 
fig4_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "adjusted_gender" | question == "CNS_status" |
           question == "CNS_first_author" | 
           question == "faculty_offers") %>% 
  distinct() %>% 
  spread(key = question, value = response)  %>% distinct() %>% 
  filter(!is.na(adjusted_gender)) %>% 
  filter(!is.na(CNS_status)) %>% 
  mutate(faculty_offers = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) == 1 ~ "1",
    as.numeric(faculty_offers) >= 1 ~ ">1"
  ),
  faculty_offers = factor(faculty_offers, levels = c("0", "1", ">1")),
  CNS_first_author = replace_na(CNS_first_author, "0"))

source("nafisa/code/figure_4.R")

#Figure 5. 

fig5_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "adjusted_gender" |
           question == "teaching_status" |
           question == "faculty_offers") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(adjusted_gender)) %>% 
  mutate(faculty_offers = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) == 1 ~ "1",
    as.numeric(faculty_offers) >= 1 ~ ">1"
  ),
  faculty_yn = if_else(as.numeric(faculty_offers) == 0, "no", "yes"), 
  faculty_offers = factor(faculty_offers, levels = c("0", "1", ">1")))

fig5_inst_bin_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "PUI_apps_submitted_binned" |
           question == "R1_apps_submitted_binned") %>% 
  distinct() %>% 
  rename(inst_type_bin = question, values_binned = response)

fig5_inst_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "PUI_apps_submitted" |
           question == "R1_apps_submitted") %>% 
  distinct() %>% 
  rename(inst_type = question, values = response)

fig5_teaching_types_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "teaching_types") %>% 
  select(-question) %>% 
  distinct() %>% rename(teaching_type = response)

fig5_data_join <- left_join(fig5_data, fig5_inst_data, by = "id") %>% 
  left_join(.,  fig5_teaching_types_data, by = "id") %>% 
  left_join(.,  fig5_inst_bin_data, by = "id") 

fig5_data <- fig5_data_join %>% 
  mutate(inst_type = str_remove_all(inst_type, "(?<=PUI|R1).*"),
         inst_type_bin = str_remove_all(inst_type_bin, "(?<=PUI|R1).*"),
         inst_type = str_replace_all(inst_type, "1", "I"),
         inst_type_bin = str_replace_all(inst_type_bin, "1", "I")) %>% 
  distinct()

source("nafisa/code/figure_5.R")

#fig 6----
fig6_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("first_author", "corresponding_author", "CNS_status",
                         "peer-reviewed_papers", "scholar_hindex", 
                         "CNS_first_author",
                         "scholar_citations_all","transition_award", 
                         "fellowship", "application_cycles", "faculty_offers",
                         "apps_submitted", "grants_awarded")) %>% 
  distinct()

offer_percent_data <- fig6_data %>% 
  filter(question == "apps_submitted" | question == "faculty_offers") %>% 
  spread(key = question, value = response) %>% 
  mutate(perc_offers = get_percent(faculty_offers, apps_submitted)) %>% 
  select(id, perc_offers)

fig6_summary <- fig6_data %>% 
  filter(question %not_in% c("transition_award", "fellowship",
                             "grants_awarded", "CNS_status")) %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE))

fig6_data <- left_join(fig6_data, offer_percent_data, by = "id") %>% 
  distinct()

source("nafisa/code/figure_6.R")

source("nafisa/code/figure_7.R")

#data tables----

#demo table
demo_table <- bio_demo_data %>% 
  filter(question %in% c("adjusted_gender", "age", "research_category", 
                         "residence", "peer", "dependents",
                         "position", "legal_status", "disability_status", 
                         "first_gen_undergrad", "first_gen_phd")) %>% 
  mutate(response = fct_collapse(response, 
                                 "Citizen/Resident" = c("Citizen", "Permanent resident"),
                                 "Visa" = c("Student visa", "Work visa"),
                                 "Other" = c("Choose not to disclose", "Outside the US or CA"),
                                 "Yes" = c("Yes, hidden", "Yes, visible"),
                                 "Yes, multiple children/adult(s)" = c("Yes, adult(s)", 
                                                                       "Yes, multiple children",
                                                                       "Yes, adult(s) and child(ren)")
  )) %>% 
  count(question, response) %>% 
  mutate(percent_total = get_percent(n, 332))

#write_csv(demo_table, "nafisa/figures/demographics.csv")

#table of application metrics
metrics_table <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("first_author", "corresponding_author",
                         "peer-reviewed_papers", "scholar_hindex",
                         "scholar_citations_all", "CNS_status",
                         "application_cycles", "faculty_offers",
                         "apps_submitted")) %>% 
  distinct() %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE), 
            std_dev = round(sd(as.numeric(response, na.rm = TRUE)), 
                            digits = 2),
            min_val = min(as.numeric(response, na.rm = TRUE)),
            max_val = max(as.numeric(response, na.rm = TRUE))) %>% 
  mutate(range = paste0("(", min_val, ", ", max_val, ")")) %>% 
  select(-min_val, -max_val)

#write_csv(metrics_table, "nafisa/figures/metrics.csv")

metrics_table2 <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>%  
  filter(question == "grants_awarded") %>% 
  filter(response != "Transition Award as PI") %>% 
  distinct() %>% 
  count(question, response) %>% 
  mutate(percent = get_percent(n, 332))

#write_csv(metrics_table2, "nafisa/figures/grants.csv")