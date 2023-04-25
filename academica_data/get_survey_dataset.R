#generate dataset of survey data for dashboard

#source("code/load_data.R") #load data

academica_df <- clean_data %>% 
  select(id, faculty_offers, research_category, peer, adjusted_gender, #Did receive / did not receive offer, field, gender, peer status
         apps_submitted_binned, off_site_interviews_binned, on_site_interviews,#Application process (number of applications, interviews number of remote and onsite)
         patent_status, conference_abstracts_binned, #Number of patents (field specific) & conference presentations (field specific)
         #Publication data (H-index, total papers, 1st author pubs, number of corresponding author, published preprints (Y/N) 
         scholar_hindex_binned, `peer-reviewed_papers_binned`,  
         first_author_binned, corresponding_author_binned, preprint_number_binned,
         #CNS publications (Y/N), 1st author CNS pubs, Total CNS pubs, 
         CNS_status, CNS_number, CNS_first_author, scholar_citations_all_binned, 
         #Funding (predoctoral, postdoctoral fellowship, transition, research project grant as COI or PI)
         transition_award, postdoctoral_fellow, predoctoral_fellow, grants_awarded, transition_award,
         teaching_status, application_cycles #Teaching/TA stats, Number of rounds on faculty job market
         ) %>% 
  distinct() %>% 
  mutate(grant_as_pi_or_copi = case_when(
    str_detect(grants_awarded, "Grant") ~ "Yes",
    grants_awarded == "NR" ~ "No response",
    TRUE ~ "No"),
    faculty_offer_status = if_else(as.numeric(faculty_offers) == 0, 
                                   "No", "Yes"),
    CNS_first_author_simple = case_when(
      as.numeric(CNS_first_author) == 0 ~ "0",
      as.numeric(CNS_first_author) == 1 ~ "1",
      as.numeric(CNS_first_author) >= 1 ~ ">1"
    ),
    CNS_number_binned = case_when(
      as.numeric(CNS_number) == 0 ~ "0",
      as.numeric(CNS_number) == 1 ~ "1",
      as.numeric(CNS_number) <= 5 ~ "2-5",
      as.numeric(CNS_number) <= 10 ~ "6-10"
    ),
    on_site_interviews_binned = get_small_bins(on_site_interviews)
    ) %>% 
  select(-grants_awarded, -faculty_offers, -CNS_first_author, 
         -CNS_number, -on_site_interviews)

academica_ids <- select(academica_df, id) %>% distinct() %>% 
  arrange(desc(id)) %>% rowid_to_column(var = "respondent_id")

academica_final <- full_join(academica_df, academica_ids, by = "id") %>% 
  select(-id) %>% 
  rename("gender" = adjusted_gender, "num_application_cycles" = application_cycles,
         "postdoctoral_fellowship" = postdoctoral_fellow,
         "predoctoral_fellowship" = predoctoral_fellow,
         "num_CNS_first_author" = CNS_first_author_simple)

write_csv(academica_final, paste0("academica_data/FJMC_survey_data_2019-2022_",
                                  Sys.Date(), ".csv"))

academica_cols <- colnames(academica_final) %>% as_tibble()


write_csv(academica_cols, "academica_data/FJMC_survey_data_README.csv")
