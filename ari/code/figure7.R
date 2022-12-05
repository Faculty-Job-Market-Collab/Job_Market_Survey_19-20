#Figure:Regional data/perhaps compared to regional average cost of childcare. 
#(did we ask if dependents are in school vs. childcare?)
# dependents vs offers/ caregiver vs offers
#is there an offer difference caregiver vs non
# what metrics differ?

fig7_data %>% 
  filter(question != "offer_responses") %>% distinct() %>% 
  filter(!is.na(response)) %>% 
  ggplot(aes(x = factor(response, 
                             levels = c("0", "1", "2", "3", "4", "5-9", "10-14",
                               "15-19", "20-29", "30-39", "40-49",
                               "50-99", "100-199")), 
             fill = caregiver))+
  geom_bar(position = "dodge")+
  coord_flip()+
  facet_wrap(~ question, scales = "free")+
  labs(y = "Number of responses (n=597)", x = "Response value",
       fill = "Has dependents")+
  my_theme_leg_bottom_horiz

ggsave("ari/figures/fig7_offers.jpeg")

fig7_data %>% 
  filter(question == "offer_responses") %>% distinct() %>%
  separate(response, sep = ",", c("1", "2", "3", "4", "5", "6")) %>% 
  gather(-id, -caregiver, -question, key = "dummy", value = "response") %>% 
  filter(!is.na(response)) %>% 
  mutate(response_type = case_when(
    str_detect(response, "rejected") ~ "Rejected",
    str_detect(response, "accepted") ~ "Accepted",
    TRUE ~ response)) %>% 
  mutate(response = str_remove_all(response, "I rejected offer\\(s\\) "),
    response = str_remove_all(response, "I accepted an offer at a "),
    response = str_to_sentence(response)) %>% 
  ggplot(aes(x = response, fill = caregiver))+
  geom_bar(position = "dodge")+
  coord_flip()+
  facet_wrap(~response_type, scales = "free", nrow = 2)+
  labs(y = "Number of responses", x = "Response to offer(s)",
       fill = "Has dependents")+
  my_theme_leg_horiz

ggsave("ari/figures/fig7_offer_responses.jpeg")