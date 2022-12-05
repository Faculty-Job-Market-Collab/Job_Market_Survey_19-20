#NETWORK
#1A - Do individuals agreeing with the statement “I believe networking and/or connections ultimately helped me obtain an onsite interview.” - Q95, ultimately 
# receive more onsite interviews and job offers? -- only one respondent

#1B - Does the mean/median for Q97 (# of institutions where I received an offer and prev interacted with members of search committee) 
# differ from the mean/median # of offers in the full dataset? 

fig1b_data %>% 
  mutate(question = if_else(question == "faculty_offers", "All institutions", 
                            "From institutions where respondent\npreviously interacted with\ninstitution faculty")) %>% 
  ggplot(aes(x = as.numeric(response), y = question))+
  geom_boxplot()+
  #geom_point()+
  coord_flip()+
  labs(y = "", x = "Number of faculty offers")+
  my_theme_horiz

ggsave("chris/figures/fig1_network_offers.jpeg")

#1C - Are interview and offer metrics different for those individuals endorsing Q94 (advisor reached out to member of search committee to recommend them for the position)
# vs the group as a whole or those not endorsing Q94

#No responses -- display logic issues?
    
#1D - Perhaps create a composite metric of “search committee” connections w/ applicant’s advisors by adding up # of Yes responses to these questions:
#Q89 - Someone on the hiring committee knows your PhD advisor.
#Q90 - Someone on the search committee has collaborated with your PhD advisor. (maybe weight more as the “network” is stronger?)
#Q91 - Someone on the search committee knows your current postdoc advisor.
#Q92 - Someone on the search committee has collaborated with your current postdoc advisor.  (maybe weight more as the “network” is stronger?)
    
#No responses -- display logic issues?