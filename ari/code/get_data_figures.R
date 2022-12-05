source("code/load_data.R")

# Identify caregiver responses & group caregiver vs non
care_ids <- demo_data %>% 
  filter(question == "dependents") %>% 
  filter(response != "No dependents") %>% 
  distinct() %>% select(id)

non_care_ids <- demo_data %>% 
  filter(question == "dependents") %>% 
  filter(response == "No dependents") %>% 
  distinct() %>% select(id)

care_tidy_data <- left_join(care_ids, tidy_data, by = 'id') %>% 
  mutate(caregiver = "Yes")

non_care_tidy_data <- left_join(non_care_ids, tidy_data, by = 'id') %>% 
  mutate(caregiver = "No")

grouped_care_tidy_data <- rbind(care_tidy_data, non_care_tidy_data)

grouped_ids <- grouped_care_tidy_data %>% select(id, caregiver)

#Figure 1----
fig1_data <- tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "dependents" | question == "peer") %>% 
  distinct() %>% 
  spread(key = question, value = response) 

source("ari/code/figure1.R")

# Figure 2----
fig2_data <- tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "primary_caregiver" | question == "adjusted_gender") %>% 
  distinct() %>% 
  spread(key = question, value = response) 

source("ari/code/figure2.R")

# Figure 3----
fig3_data <- tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "primary_caregiver" | 
           question == "partner_occupation") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(primary_caregiver %in% 
           c("Myself, Partnered", "My partner", "Co-parents")) %>% 
  filter(!is.na(partner_occupation)) %>% 
  mutate(partner_occupation = if_else(partner_occupation == "Primary caregiver",
                                      "SAHP*", partner_occupation))

source("ari/code/figure3.R")

#Figure 4. ----
fig4_data <- tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "primary_caregiver" | question == "peer") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(primary_caregiver))

#source("ari/code/figure4.R") -- values provided in fig 1

#Figure 5.----
source("ari/code/figure5.R")

#Figure 6.----
field_ids <- tidy_data %>% 
  filter(question == "research_category") %>% 
  filter(!is.na(response)) %>% 
  select(id, response) %>% distinct() %>% 
  left_join(grouped_ids, ., by = 'id') %>% distinct() %>% 
  rename(research_field = "response")

grouped_tidy_data <- tidy_data %>% 
  select(id, question, response) %>% 
  filter(question %in% c("first_author", 
                         "peer-reviewed_papers", "scholar_citations_all",
                         "scholar_hindex", "CNS_status", "fellowship",
                         "transition_award", "preprint_status",
                         "first_author_binned", "peer-reviewed_papers_binned",
                         "scholar_citations_all_binned", "scholar_hindex_binned")) %>% 
  left_join(., field_ids, by = "id") %>% 
  distinct() %>% 
  filter(!is.na(caregiver)) %>% 
  mutate(log_value = log(as.numeric(response)))

prop_yes_metrics_data <- grouped_tidy_data %>% 
  filter(question %in% c("CNS_status", "fellowship",
                         "transition_award", "preprint_status")) %>% 
  mutate(research_field = fct_lump(research_field, n=1)) %>% 
  count(research_field, question, response, caregiver) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         percent = round(get_percent(Yes, Total), digits = 0))

grouped_app_outcomes <- left_join(grouped_ids, app_outcomes, by = 'id') %>% distinct()

#source("ari/code/figure6.R")

#Figure 7.----
fig7_data <- grouped_app_outcomes %>% 
  select(id, caregiver, faculty_offers, offer_responses, 
         rejections_recieved, application_cycles) %>% 
  mutate(faculty_offers = if_else(str_detect(faculty_offers, "one") == TRUE, "1", faculty_offers),
         faculty_offers = str_remove(faculty_offers, "(?<=[[:digit:]]).*"),
         faculty_offers = str_replace(faculty_offers, "[[:alpha:]]*", "0")) %>% 
  mutate(across(c(3, 5, 6), as.numeric)) %>% 
  mutate(across(c(3, 5, 6), get_small_bins)) %>% 
  gather(-id, -caregiver, key = "question", value = "response")

#source("ari/code/figure7.R")

#Figure 8.----
grouped_percept_data <- left_join(grouped_ids, percept_data, by = 'id') %>% distinct()

#source("ari/code/figure8.R")