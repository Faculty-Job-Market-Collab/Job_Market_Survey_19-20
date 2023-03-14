#setup----
library(tidyverse) #for data wrangling

source("code/analysis_functions.R") #functions for binning numeric columns

raw_data <- read_csv("data/merged_data_19-22.csv") #load survey data

#remove incomplete entries----
clean_data <- raw_data %>%
  filter(previous_tenure_track == "No" | is.na(previous_tenure_track)) %>%  #drop respondents reporting a previous tenure track position
  select(-previous_tenure_track) %>% 
  mutate(apps_submitted_correct = as.numeric(RI_apps_submitted) + as.numeric(PUI_apps_submitted),
         apps_submitted = if (any(as.numeric(apps_submitted) < as.numeric(apps_submitted_correct))){ 
           apps_submitted_correct
           }else{
             apps_submitted
             }) %>% #repair mis-matches between total and additive apps submitted
  filter(as.numeric(apps_submitted) >= 1) %>% #drop respondents that didn't submit any applications
  filter(!is.na(apps_submitted)) %>% 
  select(-apps_submitted_correct,  
         -phd_advisor_committee_contact, -phd_advisor_committee_collab, #issue with qualtrics, these questions weren't answered
         -postdoc_advisor_committee_contact, -postdoc_advisor_committee_collab,
         -advisor_contacted_inst, -onsite_networking_impact,
         -phd_institution, -postdoc_institution, #institution names will be imported separately
         -on_site_institutions, -off_site_institutions, -offer_institutions)

clean_data_repaired <- clean_data %>% #shorten and condense response values
  mutate(position = if_else(position == "Non-tenure track faculty (e.g. research associate…)", 
                            "Non-tenure track faculty", position),
         disability_status = case_when(
           disability_status == "No, I do not have a disability" ~ "No",
           disability_status == "Yes, I have a hidden disability" ~ "Yes, hidden",
           disability_status == "Yes, I have a visible disability" ~ "Yes, visible",
           TRUE ~ disability_status
         ),
         residence = case_when(
           residence == "Somewhere else" ~ "Outside the US or CA",
           residence == "The United States of America (U.S.)" ~ "USA",
           TRUE ~ residence
         ),
         legal_status = case_when(
           legal_status == "Applying from outside the country(ies)" ~ "Outside the US or CA",
           legal_status == "Temporary student visa (e.g., F1, J1 in U.S.)" ~ "Student visa",
           legal_status == "Temporary student/scholar visa (e.g., F1, J1 in U.S.)" ~ "Student visa",
           legal_status == "Temporary work visa (e.g., H1B in U.S.)" ~ "Work visa",
           TRUE ~ legal_status
         ),
         dependents = case_when(
           dependents == "Yes, adult(s) and child(ren) I/we take care of" ~ "Yes, adult(s) and child(ren)",
           dependents == "Yes, adult(s) I/we take care of" ~ "Yes, adult(s)", 
           TRUE ~ dependents
         ),
         primary_caregiver = case_when(
           primary_caregiver == "A live-in relative (e.g., grandparent) is" ~ "A live-in relative",
           primary_caregiver == "I am, and I am married or otherwise in a committed relationship" ~ "Myself, Partnered",
           primary_caregiver == "I am, but I am single" ~ "Myself, Single",
           primary_caregiver == "I have a partner(s) that equally shares in caregiving responsibilities" ~ "Co-parents",
           primary_caregiver == "I have no dependents" ~ "No dependents",
           primary_caregiver == "My spouse/partner is" ~ "My partner",
           TRUE ~ primary_caregiver
         ),
         partner_occupation = case_when(
           partner_occupation == "I do not have a spouse/partner" ~ "Single",
           partner_occupation == "Employed/Self-Employed (non-academic)" ~ "Non-academic employment",
           partner_occupation == "Pre-tenure or research/teaching faculty" ~ "Research/teaching faculty",
           partner_occupation == "Primary caregiver (stay at home parent)" ~ "Primary caregiver",
           partner_occupation == "Student (undergraduate or graduate)" ~ "Student",
           partner_occupation == "Tenured/Tenure-track faculty" ~ "Tenured/TT faculty",
           TRUE ~ partner_occupation
         ),
         student_loan = case_when(
           student_loan == "No, I did not use financial aid" ~ "No financial aid",
           student_loan == "No, I have already paid them or received loan forgiveness" ~ "No, Paid",
           student_loan == "No, I used non-loan financial aid (e.g., grants, scholarships)" ~ "No, Grants & scholarships",
           student_loan == "No, they are in deferment" ~ "No, Deferred",
           TRUE ~ student_loan
         ), 
         financial_support = case_when(
           financial_support == "Yes, frequently but in amounts greater than $500" ~ "Yes, frequently > $500",
           financial_support == "Yes, frequently but in amounts less than $500" ~ "Yes, frequently < $500",
           TRUE ~ financial_support
         ),
         partner_education = case_when(
           partner_education == "Associates (2-year degree)" ~ "Associates",
           partner_education == "Bachelors (4-year degree)" ~ "Bachelors",
           partner_education == "High School (grade 12)" ~ "High School",
           partner_education == "I do not have a spouse/partner" ~ "Single",
           partner_education == "Middle School (grade 8) " ~ "Middle School",
           partner_education == "Professional Graduate Program (MD, JD, Physician assistant etc.)" ~ "Professional (MD, JD, etc.)",
           TRUE ~ partner_education
         )
  )

# collapse gender data categories----
set_lgbt_data <- clean_data %>% 
  select(id, gender, cis_trans_status, sexual_orientation) %>% 
  mutate(gender = if_else(is.na(gender), "NR", gender),
         cis_trans_status = if_else(is.na(cis_trans_status), "NR", cis_trans_status),
         sexual_orientation = if_else(is.na(sexual_orientation), "NR", sexual_orientation),
         gender = if_else(gender=="Non-binary"|gender=="Unlisted gender", 
                          "Gender minority", gender),
         cis_trans_status = case_when(
           cis_trans_status == "Cis" ~ "Cis",
           cis_trans_status == "NR" ~ "NR", 
           TRUE ~ "GNC"),
         tgnc = if_else(gender == "Gender minority" | cis_trans_status == "GNC", "Yes", "No"),
         lgb = case_when( #collapse sexual orientation options into y/n
           sexual_orientation == "NA"|is.na(sexual_orientation) ~ "NR",
           sexual_orientation == "NR" ~ "NR",
           str_detect(sexual_orientation, "heterosexual") ~ "No",
           TRUE ~ "Yes"),
         lgbtqia = if_else(lgb == "Yes" | tgnc == "Yes", "Yes", "No"),
         adjusted_gender = if_else(lgbtqia == "Yes", "LGB+/GNC", gender),
         simple_gender = case_when(
           adjusted_gender == "Man" ~ "Man",
           is.na(adjusted_gender) ~ "No Response",
           TRUE ~ "Woman/Trans/GNC")) %>% #collapse lgbt+ respondents across cols) #collapse lgbt+ group across cols
  select(id, gender, lgbtqia, adjusted_gender, simple_gender)

# collapse ethnicity data categories----
set_ethnicity_data <- clean_data %>% 
  select(id, race_ethnicity) %>% 
  mutate(race_ethnicity = if_else(is.na(race_ethnicity), "NR", race_ethnicity), 
         race_ethnicity = str_remove_all(race_ethnicity, "\\(.*\\)"),
         race_ethnicity = unlist(race_ethnicity)) %>% 
  separate(., race_ethnicity, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6")) %>% 
  gather(resp1:resp6, key = "dummy", value = "race_ethnicity", na.rm = TRUE) %>% 
  select(-dummy) %>% #drop dummy variable 
  mutate(simple_race_ethnicity = case_when(
    race_ethnicity == "African-American/Black/African" ~ "AA/Black/African",
    race_ethnicity == "Asian-American/Asian" ~ "AA/Asian",
    race_ethnicity == "Caucasian-American/European" ~ "CA/European",
    race_ethnicity == "Caucasian-American/North African or Middle Eastern" ~ "CA/NA or ME",
    race_ethnicity %in% c("North American Hispanic/Latinx", "South/Central American",
             "Caribbean Islander ", "North American Indigenous ",
             "Oceania ", "Not Listed") ~ "American/Islander Indigenous", #condense rare identities
    TRUE ~ race_ethnicity)
  )
#calculate number of ethnicities reported for each respondent
num_eths <- set_ethnicity_data %>% 
  distinct() %>% count(id, name = "num_ethnicities")

#identify respondents reporting a single race/ethnicity
single_eths <- num_eths %>% filter(num_ethnicities ==1) %>% #select ids of respondents reporting a single ethnic identity
  left_join(., set_ethnicity_data, by = "id") %>% #join with ethnicity identity data
  mutate(peer = case_when( #collapse ethnicity info into PEER y/n
    simple_race_ethnicity == "AA/Black/African" ~ "Yes",
    simple_race_ethnicity == "American/Islander Indigenous" ~ "Yes", 
    simple_race_ethnicity == "NR" ~ "NR", 
    TRUE ~ "No"))

#identify respondents reporting multiple identities that can be categorized as peers
multiple_eths <- num_eths %>% filter(num_ethnicities >=2) %>% #select ids of respondents reporting multiple ethnic identities
  left_join(., set_ethnicity_data, by = "id") #join with ethnicity identity data
  
#create a list of respondents w/ multiple ethnicities, at least one of which qualifies as PEER
peers_list <- multiple_eths %>% 
  mutate(peer_eth = case_when( #collapse ethnicity info into PEER y/n 
    simple_race_ethnicity == "AA/Black/African" ~ "Yes",
    simple_race_ethnicity == "American/Islander Indigenous" ~ "Yes",
    TRUE ~ "No")) %>% 
  filter(peer_eth == "Yes") %>% 
  pull(id) %>% unique()
  
multiple_eths <- multiple_eths %>% 
  mutate(peer = if_else(id %in% peers_list, "Yes", "No")) #collapse ethnicity info into PEER y/n

#merge ethnicity w/ peer data
ethnicity_data_merge <- rbind(single_eths, multiple_eths) %>% 
  select(-race_ethnicity) %>% 
  distinct()

#merge identity data
identity_data <- full_join(ethnicity_data_merge, set_lgbt_data, 
                           by = "id") %>% distinct()

#grants & fellowships: separate & repair----
separated_qual_data <- clean_data %>% 
  select(id, teaching_types, grants_awarded) %>% 
  mutate(teaching_types = if_else(is.na(teaching_types), "NR", teaching_types), #replace NA values with "NR" to prevent respondents being dropped during the gather
         grants_awarded = if_else(is.na(grants_awarded), "NR", grants_awarded)) %>% 
  separate(teaching_types, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6", 
                    "resp7", "resp8", "resp9", "resp10", "resp11", "resp12", 
                    "resp13", "resp14")) %>% 
  gather(resp1:resp14, key = "dummy", value = "teaching_types", na.rm = TRUE) %>% 
  mutate(grants_awarded = str_replace_all(grants_awarded, ", as co-PI", "as co-PI"),
         grants_awarded = str_remove_all(grants_awarded, 
                                         "\\(e\\.g\\., NIH R01\\),?|\\(e\\.g\\., R01\\),?|\\(e\\.g\\., K99\\)|\\(e\\.g\\., NIH K99\\)"),
         fellowship = if_else(str_detect(grants_awarded, "Fellow"), "Yes", "No"),
         transition_award = case_when(
           str_detect(grants_awarded, "Transition") ~ "Yes", 
           grants_awarded == "NR" ~ "NR",
           TRUE ~ "No"),
         postdoctoral_fellow = case_when(
           str_detect(grants_awarded, "Postdoctoral") ~ "Yes",
           grants_awarded == "NR" ~ "NR",
           TRUE ~ "No"),
         predoctoral_fellow = case_when(
           str_detect(grants_awarded, "Predoctoral") ~ "Yes",
           grants_awarded == "NR" ~ "NR",
           TRUE ~ "No"),
         grant_pi = case_when(
           str_detect(grants_awarded, "Grant as co-PI") ~ "Yes",
           grants_awarded == "NR" ~ "NR",
           TRUE ~ "No"),
         grant_copi = case_when(
           str_detect(grants_awarded, "Grant as PI") ~ "Yes",
           grants_awarded == "NR" ~ "NR",
           TRUE ~ "No")) %>% 
  separate(grants_awarded, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5")) %>% 
  gather(resp1:resp5, key = "dummy2", value = "grants_awarded", na.rm = TRUE) %>% 
  select(-dummy, -dummy2) #drop dummy variables

#calculate number of awards reported by each respondent
n_awards <- separated_qual_data %>% 
  select(id, grants_awarded) %>% 
  distinct() %>% count(id, name = "num_awards")

#calculate number of types of teaching experience reported by each respondent
n_teaching <- separated_qual_data %>% 
  select(id, teaching_types) %>% 
  distinct() %>% count(id, name = "num_teaching_xp")

#join separated qual data with calculated numbers
separated_qual_data <- left_join(separated_qual_data, 
                                 n_awards, by = "id") %>% 
  left_join(., n_teaching, by = "id")

#repair and abbreviate responses
repaired_qual_data <- separated_qual_data %>% 
  mutate(teaching_types = str_squish(teaching_types),
         grants_awarded = str_squish(grants_awarded),
         teaching_types = str_to_sentence(teaching_types)) %>% #drop extra spaces
  mutate(teaching_types = case_when(
    teaching_types == "Adjunct teaching.* at a community college.*" ~ "Undergrad adjunct, community or PUI",
    teaching_types == "Adjunct teaching.* at an r1.*" ~ "Undergrad adjunct, RI",
    teaching_types == "Co-instructed graduate courses" ~ "Grad co-instructor",
    teaching_types == "Co-instructed undergraduate courses" ~ "Undergrad co-instructor",
    teaching_types == "Co-instructor/lecturer (type of course not specified)" ~ "Co-instructor/lecturer",
    teaching_types == "Guest lectured graduate courses" ~ "Grad guest lecturer",
    teaching_types == "Guest lectured undergraduate courses" ~ "Undergrad guest lecturer",
    teaching_types == "Guest-lecture (type of course not specified)" ~ "Guest lecturer",
    teaching_types == "Independent instructor/lecturer (type of course not specified)" ~ "Independent instructor",
    teaching_types == "Instructed graduate courses" ~ "Grad instructor",
    teaching_types == "Instructed undergraduate courses" ~ "Undergrad instructor",
    teaching_types == "Instructor for high school courses" ~ "High school instructor",
    teaching_types == "Teaching assistant for undergraduate or graduate courses" ~ "Teaching assistant",
    teaching_types == "Total adjunct teaching positions (i.e. all college level teaching counts)" ~ "Total adjunct positions",
    teaching_types == "Visiting assistant professorship" ~ "Visiting assistant professor",
    TRUE ~ teaching_types
  ))
  
#feedback sources: separate & repair----
separated_prep_data <- clean_data %>% 
  select(id, offsite_interview_research_min, app_feedback_source, interview_feedback_source) %>% 
  mutate(offsite_interview_research_min = get_big_bins(offsite_interview_research_min), #bin values using bin_levels_big
         app_feedback_source = str_remove_all(app_feedback_source, "(?<=dean),"), #remove commas that will interfere with splitting multiple responses
         interview_feedback_source = str_remove_all(interview_feedback_source, "(?<=dean),"),
         app_feedback_source = if_else(is.na(app_feedback_source), "NR", app_feedback_source),#replace NA values with "NR" to prevent respondents being dropped during the gather
         interview_feedback_source = if_else(is.na(interview_feedback_source), "NR", interview_feedback_source)) %>% 
  separate(., app_feedback_source, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7")) %>% 
  gather(resp1:resp7, key = "dummy", value = "app_feedback_source", na.rm = TRUE) %>% 
  separate(., interview_feedback_source, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7")) %>% 
  gather(resp1:resp7, key = "dummy2", value = "interview_feedback_source", na.rm = TRUE) %>% 
  select(-dummy, -dummy2) #drop dummy variables

#calculate number of application feedback sources reported by each respondent
n_app_feedback <- separated_prep_data %>% 
  select(id, app_feedback_source) %>% 
  distinct() %>% count(id, name = "num_app_feedback")

#calculate number of interview feedback sources reported by each respondent
n_interview_feedback <- separated_prep_data %>% 
  select(id, interview_feedback_source) %>% 
  distinct() %>% count(id, name = "num_interview_feedback")

#add counts and abbreviate responses
repaired_prep_data <- separated_prep_data %>% 
  left_join(., n_app_feedback, by = "id") %>% 
  left_join(., n_interview_feedback, by = "id") %>% 
  mutate(across(3:4, function(x){
    case_when( #abbreviate feedback sources
           x == "Other faculty mentors (post-tenure)" ~ "Post-tenure faculty",
           x == "Other faculty mentors (pre-tenure)" ~ "Pre-tenure faculty",
           x == "Individuals in admin roles (dean etc.)" ~ "Admin (dean, etc.)",
           TRUE ~ x
         )}))

#perception data: collapse responses----
collapse_perception_data <- clean_data %>% 
  select(id, IDP_helpful, adequate_preparation, advisor_helpful, 
         research_product_satisfaction, transparent_process, outcome_satisfaction,
         family_impeded_search, citizenship_impeded_search, mental_health_impact,
         search_affected_other_goals, commitment_impact) %>% 
  mutate(across(2:12 , function(x){
         case_when( #abbreviate responses
           x == "Yes, and it was required by my program/training" ~ "Yes, required",
           x == "Yes, but it was not a requirement of my program/training" ~ "Yes, not required",
           x == "Neither agree nor disagree" ~ "Neutral",
           TRUE ~ x
         )}))

#outcomes: separate, repair, collapse, & bin data----
separated_outcome_data <- clean_data %>% 
  select(id, faculty_offers, offer_responses) %>% 
  mutate(faculty_offers = str_remove_all(faculty_offers, "\\(.*\\)"),
         faculty_offers = str_remove(faculty_offers, ",.*"),
         faculty_offers = str_remove(faculty_offers, "(?<=\\d).*"),
         faculty_offers = if_else(is.na(faculty_offers)|str_detect(faculty_offers, "N/A"), 
                                  "NR", faculty_offers),
         faculty_offers = if_else(str_detect(faculty_offers, "[^NR][[:alpha:]]+"), "0", faculty_offers),
         offer_responses = if_else(is.na(offer_responses), "NR", offer_responses)) %>% #replace NA values with "NR" to prevent respondents being dropped during the gather
  separate(., offer_responses, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6",
                    "resp7", "resp8", "resp9")) %>% 
  gather(resp1:resp9, key = "dummy", value = "offer_responses", na.rm = TRUE) %>% 
  select(-dummy)

#calculate number of reasons reported for rejecting offers
calc_num_reject <- separated_outcome_data %>% 
  select(id, offer_responses) %>% distinct() %>% 
  mutate(reject = str_detect(offer_responses, "rejected")) %>% 
  filter(reject == TRUE) %>% 
  count(id, reject, name = "num_rejection_reasons") %>% 
  select(-reject)

#add counts and abbreviate responses
repaired_outcome_data <- separated_outcome_data %>%
  left_join(., calc_num_reject, by = "id") %>% 
  mutate(faculty_offers = case_when( #replace with numeric value
    faculty_offers == "one rejection, waiting to hear from two others" ~ "0",
    faculty_offers == "0, though still waiting to hear from 1" ~ "0",
    faculty_offers == "7 offers " ~ "7",
    TRUE ~ faculty_offers
  ),
  faculty_offers = str_trim(faculty_offers), #drop extra spaces
  simple_offers = if_else(as.numeric(faculty_offers) >= 2, 
                          "2+", faculty_offers)) %>% 
  mutate(offer_responses = case_when( #abbreviate response values
           offer_responses == "I accepted an offer at a primarily undergraduate institution" ~ "Accepted, PUI",
           offer_responses == "I accepted an offer at a research-intensive institution" ~ "Accepted, R1",
           offer_responses == "I did not recieve any offers" ~ "No offers",
           offer_responses == "I rejected offer(s) because of other family needs" ~ "Rejected, family needs",
           offer_responses == "I rejected offer(s) because of other reasons pertaining to the impact of Covid-19" ~ "Rejected, COVID-19 impacts",
           offer_responses == "I rejected offer(s) because of reduced resources directly related to the economic impact of Covid-19" ~ "Rejected, COVID-19 economic impacts",
           offer_responses == "I rejected offer(s) because of spouse/partner job prospects" ~ "Rejected, partner prospects",
           offer_responses == "I rejected offer(s) because the personal compensation/benefits were insufficient" ~ "Rejected, insufficient offer",
           offer_responses == "I rejected offer(s) because the position did not fit my career goals" ~ "Rejected, not a career fit",
           offer_responses == "I rejected offer(s) because the start up package was insufficient" ~ "Rejected, insufficient start-up",
           offer_responses == "I rejected offer(s) for geographic fit" ~ "Rejected, geographic fit",
           offer_responses == "I rejected offer(s) for hostile/unwelcome interviewing environments" ~ "Rejected, unwelcome environments",
           offer_responses == "I rejected offer(s) for reasons not listed above" ~ "Rejected, other reasons",
           TRUE ~ offer_responses
           ))

#job ad data: separate, clean, abbreviate----
separated_network_data <- select(clean_data, id, onsite_interview_ad_1, onsite_interview_ad_2) %>% 
  mutate(onsite_interview_ad_1 = if_else(is.na(onsite_interview_ad_1), "NR", onsite_interview_ad_1), #replace NA values with "NR" to prevent respondents being dropped during the gather
         onsite_interview_ad_2 = if_else(is.na(onsite_interview_ad_2), "NR", onsite_interview_ad_2)) %>% 
  separate(., onsite_interview_ad_2, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6")) %>% 
  gather(resp1:resp6, key = "dummy", value = "onsite_interview_ad_2", na.rm = TRUE) %>% 
  separate(., onsite_interview_ad_1, sep = ",", #respondents allowed to select all that apply, split responses into multiple values
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6")) %>% 
  gather(key = "dummy1", value = "onsite_interview_ad_1", 
         resp1:resp6, na.rm = TRUE) %>% 
  select(-dummy, -dummy1) %>% 
  mutate(across(2:3, function(x){ 
    case_when( #abbreviate long responses
           x == "From an internal referral (the position was not advertised when I first learned about it)" ~ "Internal referral, unadvertised",
           x == "A contact at the hiring institution" ~ "Hiring inst contact",
           TRUE ~ x
         )}))

#bin numerical responses----
binned_columns <- clean_data %>% 
  select(id, apps_submitted, RI_apps_submitted, PUI_apps_submitted, #select columns requiring binned values
         off_site_interviews, rejections_received,
         `peer-reviewed_papers`, conference_abstracts,
         corresponding_author, first_author, preprint_number, 
         faculty_contacted, contains("scholar")) %>% 
  mutate(across(c(2:12, 15:16), get_small_bins, .names = "{.col}_binned"),
         across(c(13:14, 17:20), get_big_bins, .names = "{.col}_binned")) %>% 
  select(id, contains("_binned")) #drop original columns


#final data merge----
merged_cleaned_data <- clean_data_repaired %>% 
  select(-race_ethnicity, -cis_trans_status, -teaching_types, -grants_awarded, #drop repaired, redundant columns
         -offsite_interview_research_min, -app_feedback_source, -interview_feedback_source,
         -IDP_helpful, -adequate_preparation, -advisor_helpful, -gender,
         -research_product_satisfaction, -transparent_process, -outcome_satisfaction,
         -family_impeded_search, -citizenship_impeded_search, -mental_health_impact,
         -search_affected_other_goals, -commitment_impact, -offer_responses,
         -faculty_offers, -onsite_interview_ad_1, -onsite_interview_ad_2) %>% 
  left_join(., identity_data, by = "id") %>% 
  left_join(., repaired_qual_data, by = "id") %>% 
  left_join(., repaired_prep_data, by = "id") %>% 
  left_join(., collapse_perception_data, by = "id") %>% 
  left_join(., repaired_outcome_data, by = "id") %>% 
  left_join(., separated_network_data, by = "id") %>% 
  left_join(., binned_columns) %>% distinct()


#write data to csv file----
write_csv(merged_cleaned_data, paste0("data/cleaned_data_19-22_", Sys.Date(), ".csv")) #optional local save of fully cleaned data set