#streamline identity data
eth_data <- demo_data %>% 
  select(id, question, response) %>% 
  filter(question == "race_ethnicity")

eth_ids <- eth_data %>% 
  distinct() %>% count(id)

sing_id <- eth_ids %>% filter(n ==1) %>% 
  mutate(identity = "Single") %>% 
  left_join(., eth_data, by = "id") %>% 
  mutate(peer = if_else(response == "AA/Black/African" | 
                          response == "American/Islander Indigenous", 
                        "Yes", "No"))

peers_list <- c("101", "204", "208", "210", "241", "248", "30", "303", "348",
                "414", "416", "448", "453", "520", "557", "57", "620", "633",
                "636", "695", "70", "741", "774", "777", "780", "782", "804", 
                "814")

mult_id <- eth_ids %>% filter(n >=2) %>% 
  mutate(identity = "Multiple") %>% 
  left_join(., eth_data, by = "id") %>% 
  mutate(peer = if_else(id %in% peers_list, "Yes", "No")) 

eth_data_merge <- rbind(mult_id, sing_id) %>% 
  select(-n, -question, -response) %>% 
  distinct() %>% 
  gather(-id, key = question, value = response) %>% 
  mutate(section = "demographic")
  