source("code/load_data.R")

# Figure 1 - Network----

tidy_network <- tidy_data %>% 
  filter(section == "network") %>% distinct()

fig1b_data <- tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "num_institution_contacted" | 
           question == "faculty_offers") %>% distinct() 

fig1c_data <- tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "advisor_contacted_inst" #| 
           #question == "faculty_offers"
           ) %>% distinct() 

source("chris/code/figure_1.R")

# Figure 2 - Prestige----

source("code/tidy_ranking_data.R")

fig2_data <- left_join(all_rank_data, tidy_data, by = c("id", "inst_type", 
                                                        "NAME")) %>% 
  select(-inst_key, -inst_value) %>% distinct()

hindex_only_data <- fig2_data %>% 
  select(id, question, response) %>% distinct() %>% 
  filter(str_detect(question, "2015") == FALSE) %>% 
  filter(str_detect(question, "binned") == FALSE) %>% 
  mutate(question = case_when(
    question == "scholar_hindex_2" ~ "postdoc_advisor_hindex",
    question == "scholar_citations_all_2" ~ "phd_advisor_hindex",
    TRUE ~ question
    )) %>% 
  filter(str_detect(question, "hindex") == TRUE) %>% 
  spread(key = question, value = response)
  
scholar_hindex <- select(hindex_only_data, id, scholar_hindex) %>% 
  filter(!is.na(scholar_hindex)) %>% 
  mutate(scholar_hindex = as.numeric(scholar_hindex),
         hindex_rank = case_when(
           scholar_hindex >= 10.000 ~ "Top 25%",
           scholar_hindex >= 7.000 ~ "Top 50%",
           scholar_hindex < 7.000 ~ "Bottom 50%",
           scholar_hindex <= 4.000 ~ "Bottom 25 %"
         )) 

scholar_hindex_tidy <- scholar_hindex %>% 
  gather(-id, -hindex_rank, key = question, value = response)

phd_advisor_hindex <- select(hindex_only_data, id, phd_advisor_hindex) %>% 
  filter(!is.na(phd_advisor_hindex)) %>% 
  mutate(phd_advisor_hindex = as.numeric(phd_advisor_hindex),
         hindex_rank = case_when(
           phd_advisor_hindex >= 71.75 ~ "Top 25%",
           phd_advisor_hindex >= 45.5 ~ "Top 50%",
           phd_advisor_hindex < 45.5 ~ "Bottom 50%",
           phd_advisor_hindex <= 29.25 ~ "Bottom 25 %"
         ))

phd_advisor_hindex_tidy <- phd_advisor_hindex %>% 
  gather(-id, -hindex_rank, key = question, value = response)

postdoc_advisor_hindex <- select(hindex_only_data, id, 
                                 postdoc_advisor_hindex) %>% 
  filter(!is.na(postdoc_advisor_hindex)) %>% 
  mutate(postdoc_advisor_hindex = as.numeric(postdoc_advisor_hindex),
         hindex_rank = case_when(
           postdoc_advisor_hindex >= 71.75 ~ "Top 25%",
           postdoc_advisor_hindex >= 45.5 ~ "Top 50%",
           postdoc_advisor_hindex < 45.5 ~ "Bottom 50%",
           postdoc_advisor_hindex <= 29.25 ~ "Bottom 25 %"
         ))

postdoc_advisor_hindex_tidy <- postdoc_advisor_hindex %>% 
  gather(-id, -hindex_rank, key = question, value = response)

hindex_rank_tidy <- rbind(scholar_hindex_tidy, phd_advisor_hindex_tidy,
                          postdoc_advisor_hindex_tidy) %>% 
  mutate(response = as.character(response))

hindex_rank <- full_join(scholar_hindex, phd_advisor_hindex, 
                         by = c("id", "hindex_rank")) %>% 
  full_join(., postdoc_advisor_hindex) %>% 
  distinct()

fig2a_data <- tidy_data %>% 
  filter(section == "app_outcomes") %>% 
  select(id, question, response) %>% 
  filter(question == "application_cycles" | question == "apps_submitted" 
         | question == "faculty_offers" | question == "PUI_apps_submitted" |
           question == "R1_apps_submitted") %>% 
  left_join(., hindex_rank, by = "id") %>% 
  distinct() 

fig2b_data <- fig2_data %>% 
  select(id, inst_type, NAME, drdeg_rank, the_rank) %>% 
  distinct()

source("chris/code/figure_2.R")