library(tidyverse) #for data wrangling
library(data.table) #for setnames()
library(car) #for anova

source("code/analysis_functions.R") #functions for generating plots and binning b
source("code/get_plot_options.R")

raw_data <- read_csv("data/raw_job_survey_data.csv") #load survey data

q_list <- read_csv("data/question_legend.csv") #csv of q numbers, full q, & column names

q_num <- q_list %>% pull(Q_number)#list of q numbers

q_data <- q_list %>% pull(Data) #list of column names

data <- raw_data[-c(1,2,3),] %>% #drop non-data rows
  filter(as.numeric(Progress) >= 33) %>% #drop entries w/ less than 33% completion
  select(-(1:17)) #drop unnecessary data columns

setnames(data, old = q_num, new = q_data) #rename columns

#arrange data and split into data sets
clean_data <- mutate(data, id = rownames(data)) %>% #generate unique ids
  filter(previous_tenure_track == "No" | is.na(previous_tenure_track)) %>%  #drop responders reporting a previous tenure track postion
  select(-previous_tenure_track) %>% 
  mutate(apps_submitted_correct = as.numeric(R1_apps_submitted) + as.numeric(PUI_apps_submitted),
         apps_submitted = if (any(as.numeric(apps_submitted) < as.numeric(apps_submitted_correct))){ 
           apps_submitted_correct
           }else{
             apps_submitted
             }) %>% 
  filter(as.numeric(apps_submitted) >= 1) %>% 
  filter(!is.na(apps_submitted)) %>% 
  select(-apps_submitted_correct)

#dataset for each institution listed w/ pui, ri status, region, etc
carn_data <- read_csv("data/full_survey_inst_data.csv") %>% 
  mutate(id = as.character(id), 
         State_Providence = fct_collapse(State_Providence,
                                         California = c("California", "CA"),
                                         Connecticut = c("Connecticut", "CT"),
                                         Alabama = c("Alabama", "AL")))

carn_joined_inst <- carn_data %>% 
  select(id, inst_type, NAME, State_Providence, US_region, PUI_RI, 
         Country, world_region, Other_inst_type) %>% 
  gather(-id, -inst_type, -NAME, key = "inst_key", value = "inst_value")

## question-based datasets----
demographics <- select(clean_data, position:biomedical, id) %>% #individual descriptives
  mutate(gender = if_else(gender=="Non-binary"|gender=="Unlisted gender"|is.na(gender), 
                          "Gender minority", gender),
         cis_trans_status = if_else(cis_trans_status != "Cis", "gnc", cis_trans_status),
         tgnc = if_else(gender == "Gender minority" | cis_trans_status == "gnc", "yes", "no"),
         adjusted_gender = if_else(tgnc == "yes", "Trans/GNC", gender),
         simple_gender = if_else(adjusted_gender == "Man", "Man", "Woman/Trans/GNC"),
         simple_gender = if_else(is.na(simple_gender), "No Response", simple_gender))

covid_only <- select(clean_data, contains("covid"), id) #select covid-related questions

qualifications <- select(clean_data, 'peer-reviewed_papers':teaching_types, id) #questions related to applicant metrics (e.g., num/type papers, citations)

app_outcomes <- select(clean_data, apps_submitted:application_cycles, id) %>% #questions related to application packets (e.g., num/type apps submitted, feedback)
  select(-on_site_institutions, -off_site_insitutions, -offer_institutions) 

network <- select(clean_data, advisor_rank:scholar_hindex_2015_2, id, -contains("research_min")) %>% #questions related to applicant pedigree & advisors
  select(-phd_institution, -postdoc_institution,  
         -phd_advisor_committee_contact, -phd_advisor_committee_collab, #issue with qualtrics, these questions weren't answered
         -postdoc_advisor_committee_contact, -postdoc_advisor_committee_collab,
         -advisor_contacted_inst, -onsite_networking_impact) 

preparation <- select(clean_data, id, 
                      contains("research_min"), app_feedback:workshop_data) #questions about applicant preparation for the job market

perceptions <- select(clean_data, 
                      id, covid_alter_research:commitment_impact) #questions about how the application process affected applicant feelings

free_resp <- select(clean_data, id, comments)

bin_levels_small <- c("0", "1", "2", "3", "4", "5-9", "10-14", "15-19", 
                      "20-29", "30-39", "40-49", "50-99", "100-199", 
                      "200-299", "300+")

bin_levels_big <- c("< 10", "10-19", "20-49", "50-99", "100-149", 
                    "150-199", "200-299", "300-399", "400-499", "500-999", 
                    "1000-1499", "1500-1999", "2000-2999", 
                    "3000-3999", "4000+")

#get tidied and fully coded data
source("code/tidy_data.R")

#write_csv(tidy_data, "data/tidy_data") #optional local save of fully cleaned data set