library(tidyverse) #for data wrangling
library(car) #for anova

source("code/analysis_functions.R") #functions for generating plots
source("code/get_plot_options.R")

#arrange data and split into data sets
clean_data <- read_csv("data/cleaned_data_19-22_2023-03-10.csv") #load survey data

#generate ffi data from clean_data & join with cleaned survey data
source("code/get_ffi_data.R") 

survey_data <- left_join(clean_data, ffi_data, by = "id")

#dataset for each institution listed w/ pui, ri status, region, etc
carn_data <- read_csv("data/full_survey_inst_data_19-20_only.csv") %>% 
  mutate(id = paste0(id, "_2019-2020"),
         State_Providence = fct_collapse(State_Providence,
                                         California = c("California", "CA"),
                                         Connecticut = c("Connecticut", "CT"),
                                         Alabama = c("Alabama", "AL")))

inst_data <- carn_data %>% 
  select(id, inst_type, NAME, State_Providence, US_region, PUI_RI, 
         Country, world_region, Other_inst_type) %>% 
  gather(-id, -inst_type, -NAME, key = "inst_key", value = "inst_value")

## question-based datasets----
#demographics <- select(clean_data, position:biomedical, id) #individual descriptives
#
#covid_only <- select(clean_data, contains("covid"), id) #select covid-related questions
#
#qualifications <- select(clean_data, 'peer-reviewed_papers':teaching_types, id) #questions related to applicant metrics (e.g., num/type papers, citations)
#
#app_outcomes <- select(clean_data, apps_submitted:application_cycles, id) %>% #questions related to application packets (e.g., num/type apps submitted, feedback)
#  select(-on_site_institutions, -off_site_insitutions, -offer_institutions) 
#
#network <- select(clean_data, advisor_rank:scholar_hindex_2015_2, id, -contains("research_min")) #questions related to applicant pedigree & advisors
#
#preparation <- select(clean_data, id, 
#                      contains("research_min"), app_feedback:workshop_data) #questions about applicant preparation for the job market
#
#perceptions <- select(clean_data, 
#                      id, covid_alter_research:commitment_impact) #questions about how the application process affected applicant feelings
#
#free_resp <- select(clean_data, id, comments)


#convert to tidied dataset
tidy_data <- clean_data %>% 
  gather(-id, key = "question", value = "response") %>% 
  left_join(., inst_data, by = "id") %>% #add Carnegie data for provided institution values
  distinct()

#write_csv(tidy_data, "data/tidy_data") #optional local save of fully cleaned data set