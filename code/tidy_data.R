#ESI data ---- in progress 12/5/2022
#esi_data <- demographics %>% 
#  select(id, income, student_loan, financial_support, dependents,
#         extra_income, partner_occupation, legal_status) %>% 
#  mutate(income_esi = factor(income, 
#                             levels = c("More than $150,000", 
#                                        "$120,001 - 150,000",
#                                        "$90,001 - 120,000",
#                                        "$70,001 - 90,000",
#                                        "$50,001 - 70,000",
#                                        "$30,001 - 50,000",
#                                        "$25,000 - 30,000"),
#                             labels = c("7", "6", "5", "4", "3", "2", "1")),
#         loan_esi = factor(student_loan, 
#                           levels = c("No, I did not use financial aid",
#                                      "No, I have already paid them or received loan forgiveness",
#                                      "No, I used non-loan financial aid (e.g., grants, scholarships)",
#                                      "No, they are in deferment", "Yes"), 
#                           labels = c("5", "4", "3", "2", "1")),
#         support_esi = factor(financial_support, 
#                              levels = c("No", "Yes, but rarely",
#                                         "Yes, frequently but in amounts less than $500",
#                                         "Yes, frequently but in amounts greater than $500"), 
#                              labels = c("4", "3", "2", "1")),
#         extra_esi = factor(extra_income, 
#                            levels = c("Yes", "No"), 
#                            labels = c("1", "2")),
#         partner_esi = factor(partner_occupation, 
#                              levels = c("Tenured/Tenure-track faculty",
#                                         "Employed/Self-Employed (non-academic)",
#                                         "Pre-tenure or research/teaching faculty",
#                                         "Postdoc",
#                                         "Student (undergraduate or graduate)",
#                                         "Primary caregiver (stay at home parent)",
#                                         "I do not have a spouse/partner"), 
#                              labels = c("7", "6", "5", "4", "3", "2", "1")),
#         legal_esi = factor(legal_status, 
#                            levels = c("Citizen", "Applying from outside the country(ies)",
#                                       "Permanent resident", 
#                                       "Temporary work visa (e.g., H1B in U.S.)",
#                                       "Temporary student visa (e.g., F1, J1 in U.S.)",
#                                       "Choose not to disclose"), 
#                            labels = c("5", "4", "3", "2", "1", "0")),
#         dependent_esi = factor(dependents, 
#                                levels = c("No dependents", "Yes, one child",
#                                           "Yes, adult(s) I/we take care of", 
#                                           "Yes, multiple children",
#                                           "Yes, adult(s) and child(ren) I/we take care of"),
#                                labels = c("5", "4", "3", "2", "1"))
#  ) %>% 
#  mutate(across(contains("_esi"), as.numeric)) %>% 
#  mutate(across(contains("_esi"), 
#                function(x){replace_na(x, replace = 0)})) %>% 
#  rowwise() %>% 
#  mutate(esi = sum(c_across(9:15))) %>%
#  filter(esi >= 5) %>% #allows for two unanswered questions
#  select(id, esi)

#demographics data ----
demo_data <- demographics %>% 
  mutate(race_ethnicity = str_remove_all(race_ethnicity, "\\(.*\\)"),
         race_ethnicity = unlist(race_ethnicity)) %>% 
  separate(., race_ethnicity, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5")) %>% 
  gather(resp1:resp5, key = "dummy", value = "race_ethnicity", na.rm = TRUE) %>% 
  select(-dummy) %>% #drop dummy variable 
  gather(-id, key = "question", value = "response", na.rm = TRUE) %>% #pull into tidy format
  mutate(section = "demographic", #add question-type grouping
         response = map(response, function(x){case_when( #shorten and condense response values
           x == "Non-tenure track faculty (e.g. research associate…)" ~ "Non-tenure track faculty",
           x == "Yes, adult(s) and child(ren) I/we take care of" ~ "Yes, adult(s) and child(ren)",
           x == "Yes, adult(s) I/we take care of" ~ "Yes, adult(s)", 
           x == "No, I do not have a disability" ~ "No",
           x == "Yes, I have a hidden disability" ~ "Yes, hidden",
           x == "Yes, I have a visible disability" ~ "Yes, visible",
           x == "Yes, frequently but in amounts greater than $500" ~ "Yes, frequently > $500",
           x == "Yes, frequently but in amounts less than $500" ~ "Yes, frequently < $500",
           x == "Applying from outside the country(ies)" | x == "Somewhere else" ~ "Outside the US or CA",
           x == "I do not have a spouse/partner" ~ "Single",
           x == "Temporary student visa (e.g., F1, J1 in U.S.)" ~ "Student visa",
           x == "Temporary work visa (e.g., H1B in U.S.)" ~ "Work visa",
           x == "Employed/Self-Employed (non-academic)" ~ "Non-academic employment",
           x == "Pre-tenure or research/teaching faculty" ~ "Research/teaching faculty",
           x == "Primary caregiver (stay at home parent)" ~ "Primary caregiver",
           x == "Student (undergraduate or graduate)" ~ "Student",
           x == "Tenured/Tenure-track faculty" ~ "Tenured/TT faculty",
           x == "A live-in relative (e.g., grandparent) is" ~ "A live-in relative",
           x == "I am, and I am married or otherwise in a committed relationship" ~ "Myself, Partnered",
           x == "I am, but I am single" ~ "Myself, Single",
           x == "I have a partner(s) that equally shares in caregiving responsibilities" ~ "Co-parents",
           x == "I have no dependents" ~ "No dependents",
           x == "My spouse/partner is" ~ "My partner",
           x == "African-American/Black/African" ~ "AA/Black/African",
           x == "Asian-American/Asian" ~ "AA/Asian",
           x == "Caucasian-American/European" ~ "CA/European",
           x == "Caucasian-American/North African or Middle Eastern" ~ "CA/North Africa or Middle East",
           x == "The United States of America (U.S.)" ~ "USA",
           x %in% c("North American Hispanic/Latinx", "South/Central American",
                    "Caribbean Islander ", "North American Indigenous ",
                    "Oceania ", "Not Listed") ~ "American/Islander Indigenous", #condense rare identities
           x == "No, I did not use financial aid" ~ "No financial aid",
           x == "No, I have already paid them or received loan forgiveness" ~ "No, Paid",
           x == "No, I used non-loan financial aid (e.g., grants, scholarships)" ~ "No, Grants & scholarships",
           x == "No, they are in deferment" ~ "No, Deferred",
           TRUE ~ x
                              )}),
         response = unlist(response))

source("code/sort_identity_data.R") #code to identify yes/no PEER status

demo_data <- rbind(eth_data_merge, demo_data) #merge PEER status w/ demo

# qualifications data ----
qualif_data <- qualifications %>% 
  mutate(across(c(1:4, 7, 11:14), as.numeric)) %>% #prep columns for bins
  mutate(across(c(1:4, 7, 13, 14), get_small_bins, .names = "{.col}_binned"), #bin using bin_levels_small
         across(11:12, get_big_bins, .names = "{.col}_binned")) %>% #bin using bin_levels_big
  separate(teaching_types, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7", "resp8", "resp9", "resp10", "resp11", "resp12", "resp13")) %>% 
  gather(resp1:resp13, key = "dummy", value = "teaching_types", na.rm = TRUE) %>% 
  mutate(grants_awarded = str_remove_all(grants_awarded, "\\(.*\\)"),
         grants_awarded = str_remove_all(grants_awarded, " ,")) %>% 
  separate(grants_awarded, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7")) %>% 
  gather(resp1:resp7, key = "dummy2", value = "grants_awarded", na.rm = TRUE) %>% 
  select(-dummy, -dummy2) %>% #drop dummy variables
  mutate(fellowship = if_else(str_detect(grants_awarded, "Fellow"), "Yes", "No"),
         transition_award = if_else(str_detect(grants_awarded, "Transition"), "Yes", "No")) %>% 
  gather(-id, key = "question", value = "response", na.rm = TRUE) %>% #pull all qualif data into tidy format
  mutate(section = "qualifications", #question-type tag
         response = map(response, function(x){case_when( #abbreviate response values
           x == "Transition to Independence Award" ~ "Transition Award",
           x == "Transition to Independence Award  as PI" ~ "Transition Award as PI",
           x == "Yes, Teaching Assistant (TA) position" ~ "Yes, TA",
           x == "Adjunct Teaching Instructor for Undergraduate Courses at a Community College or PUI" ~ "Undergrad adjunct, community or PUI",
           x == "Adjunct Teaching Instructor for Undergraduate courses at an R1 University" ~ "Undergrad adjunct, RI",
           x == "Co-instructed graduate courses" ~ "Grad co-instructor",
           x == "Co-instructed undergraduate courses" ~ "Undergrad co-instructor",
           x == "Co-Instructor/Lecturer (type of course not specified)" ~ "Co-instructor/lecturer",
           x == "Guest lectured graduate courses" ~ "Grad guest lecturer",
           x == "Guest lectured undergraduate courses" ~ "Undergrad guest lecturer",
           x == "Guest-lecture (type of course not specified)" ~ "Guest lecturer",
           x == "Independent instructor/lecturer (type of course not specified)" ~ "Independent instructor",
           x == "Instructed graduate courses" ~ "Grad instructor",
           x == "Instructed undergraduate courses" ~ "Undergrad instructor",
           x == "Instructor for High School Courses" ~ "High school instructor",
           x == "Teaching Assistant for Undergraduate or Graduate Courses" ~ "Teaching assistant",
           x == "Total Adjunct teaching positions (i.e. all college level teaching counts)" ~ "Total adjunct positions",
           x == "Visiting Assistant Professorship" ~ "Visiting assistant professor",
           TRUE ~ x
         )}),
         response = unlist(response))

#perceptions data----
percept_data <- perceptions %>% 
  select(-covid_alter_research) %>% 
  gather(-id, key = "question", value = "response", na.rm = TRUE) %>% #pull into tidy data
  mutate(section = "perceptions", #add question-type tag
         response = case_when( #abbreviate responses
           response == "Yes, and it was required by my program/training" ~ "Yes, required",
           response == "Yes, but it was not a requirement of my program/training" ~ "Yes, not required",
           response == "Neither agree nor disagree" ~ "Neutral",
           response == "Somewhat agree" ~ "Agree",
           response == "Somewhat disagree" ~ "Disagree",
           response == "Strongly agree" ~ "Strongly Agree",
           response == "Strongly disagree" ~ "Strongly Disagree",
           TRUE ~ response
         ))

#preparation data----
prep_data <- preparation %>% 
  mutate(across(1:2, as.numeric),
         offsite_interview_research_min = get_big_bins(offsite_interview_research_min), #bin values using bin_levels_big
         app_feedback_source = str_remove_all(app_feedback_source, "(?<=dean),"), #remove commas that will interfere with splitting multiple responses
         interview_feedback_source = str_remove_all(interview_feedback_source, "(?<=dean),")) %>% 
  separate(., app_feedback_source, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7")) %>% 
  gather(resp1:resp7, key = "dummy", value = "app_feedback_source", na.rm = TRUE) %>% 
  separate(., interview_feedback_source, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7")) %>% 
  gather(resp1:resp7, key = "dummy2", value = "interview_feedback_source", na.rm = TRUE) %>% 
  select(-dummy, -dummy2) %>% #drop dummy variables
  gather(-id, key = "question", value = "response", na.rm = TRUE) %>% #pull into tidy data
  mutate(section = "preparation", #add question-type tag
         response = case_when( #abbreviate response values
           response == "Other faculty mentors (post-tenure)" ~ "Post-tenure faculty",
           response == "Other faculty mentors (pre-tenure)" ~ "Pre-tenure faculty",
           response == "Individuals in admin roles (dean etc.)" ~ "Admin (dean, etc.)",
           TRUE ~ response
         ))

#outcome data----
outcome_data <- app_outcomes %>% 
  mutate(faculty_offers = str_remove_all(faculty_offers, "\\(.*\\)"),
         across(c(1:4, 10, 11), as.numeric), #prep for binning
         across(c(1:4, 10, 11), get_small_bins, .names = "{.col}_binned")) %>% #bin values with bin_levels_small
  separate(., offer_responses, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6")) %>% 
  gather(resp1:resp6, key = "dummy", value = "offer_responses", na.rm = TRUE) %>% 
  select(-dummy, -covid_remote, -covid_offers_rescinded) %>% 
  mutate(faculty_offers = case_when( #replace with numeric value
    faculty_offers == "one rejection, waiting to hear from two others" ~ "1",
    faculty_offers == "0, though still waiting to hear from 1" ~ "0",
    faculty_offers == "7 offers " ~ "7",
    TRUE ~ faculty_offers
    ),
    faculty_offers = str_trim(faculty_offers), #drop extra spaces
    simple_offers = if_else(as.numeric(faculty_offers) >= 2, 
                            "2+", faculty_offers)) %>% 
  gather(-id, key = "question", value = "response", na.rm = TRUE) %>% #pull into tidy dataset
  mutate(section = "app_outcomes", #question-type tag
         response = map(response, function(x){case_when( #abbreviate response values
           x == "I accepted an offer at a primarily undergraduate institution" ~ "Accepted, PUI",
           x == "I accepted an offer at a research-intensive institution" ~ "Accepted, R1",
           x == "I did not recieve any offers" ~ "No offers",
           x == "I rejected offer(s) because of other family needs" ~ "Rejected, family needs",
           x == "I rejected offer(s) because of other reasons pertaining to the impact of Covid-19" ~ "Rejected, COVID-19 impacts",
           x == "I rejected offer(s) because of reduced resources directly related to the economic impact of Covid-19" ~ "Rejected, COVID-19 economic impacts",
           x == "I rejected offer(s) because of spouse/partner job prospects" ~ "Rejected, partner prospects",
           x == "I rejected offer(s) because the personal compensation/benefits were insufficient" ~ "Rejected, insufficient offer",
           x == "I rejected offer(s) because the position did not fit my career goals" ~ "Rejected, not a career fit",
           x == "I rejected offer(s) because the start up package was insufficient" ~ "Rejected, insufficient start-up",
           x == "I rejected offer(s) for geographic fit" ~ "Rejected, geographic fit",
           x == "I rejected offer(s) for hostile/unwelcome interviewing environments" ~ "Rejected, unwelcome environments",
           x == "I rejected offer(s) for reasons not listed above" ~ "Rejected, other reasons",
           TRUE ~ x
                        )}) %>% unlist()) 

#network_data----
network_data <- network %>% 
  mutate(faculty_contacted = get_small_bins(faculty_contacted), #group with bin_levels_small
         across(7:10, as.numeric), #prep for binning values
         across(7:10, get_big_bins, .names = "{.col}_binned")) %>% #group with bin_levels_big
  separate(., col = onsite_interview_ad_2, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4")) %>% 
  gather(key = "dummy", value = "onsite_interview_ad", 
         resp1, resp2, resp3, resp4, onsite_interview_ad_1, na.rm = TRUE) %>% 
  select(-dummy) %>% 
  gather(-id, key = "question", value = "response", na.rm = TRUE) %>% 
  mutate(section = "network", #question-type tag
         response = case_when( #abbreviate long responses
           response == "From an internal referral (the position was not advertised when I first learned about it)" ~ "Internal referral, unadvertised",
           response == "A contact at the hiring institution" ~ "Hiring inst contact",
           TRUE ~ response
         ))

#join tidy_data----
tidy_data <- rbind(demo_data, qualif_data, percept_data, 
                   prep_data, outcome_data, network_data) %>% 
  left_join(., carn_joined_inst, by = "id") %>% #add Carnegie data for provided institution values
  distinct()

