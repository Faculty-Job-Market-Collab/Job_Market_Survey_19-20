library(tidyverse) #for data wrangling
library(data.table) #for setnames()

#load full data files
data_files <- c("data/2019-2020_raw_job_survey_data.csv",
                "data/2020-2021_raw_job_survey_data.csv",
                "data/2021-2022_raw_job_survey_data.csv")

raw_data <- map(data_files, read_csv) #load data files

#prep 2019-2020 data ---- 

#get questions & column names
q_list_19 <- read_csv("data/question_legend_19.csv") #csv of q numbers, full q, & column names for 2019-2020 data

q_num_19 <- q_list_19 %>% pull(Q_number)#list of q numbers for 2019-2020

q_data_19 <- q_list_19 %>% pull(Data) #list of column names for 2019-2020

#drop extra columns & fix data column names
data_19 <- raw_data[[1]][-c(1,2),] %>% #drop non-data rows
  filter(as.numeric(Progress) >= 33) %>% #drop entries w/ less than 33% completion
  select(-(1:17)) #drop unnecessary data columns

setnames(data_19, old = q_num_19, new = q_data_19) #rename columns

#set unique ids
data_19_ids <- data_19 %>% 
  mutate(survey_year = "2019-2020",
         sexual_orientation = "NA",
         partner_education = "NA") %>% 
  rowid_to_column("id")

#prep 2020-2022 data ----

#get questions & column names
q_list_20 <- read_csv("data/question_legend_20-22.csv") #csv of q numbers, full q, & column names for 2020-2022 data

q_num_20 <- q_list_20 %>% pull(Q_number)#list of q numbers for 2020-2022

q_data_20 <- q_list_20 %>% pull(Data) #list of column names for 2020-2022

#drop extra columns & fix data column names
data_20 <- raw_data[[2]][-c(1,2),] %>% #drop non-data rows
  filter(as.numeric(Progress) >= 33) %>% #drop entries w/ less than 33% completion
  select(-(1:17)) #drop unnecessary data columns

setnames(data_20, old = q_num_20, new = q_data_20) #rename columns

data_21 <- raw_data[[3]][-c(1,2),] %>% #drop non-data rows
  filter(as.numeric(Progress) >= 33) %>% #drop entries w/ less than 33% completion
  select(-(1:17)) #drop unnecessary data columns

setnames(data_21, old = q_num_20, new = q_data_20) #rename columns

#set unique ids
data_20_ids <- data_20 %>% 
  mutate(survey_year = "2020-2021") %>% 
  rowid_to_column("id")

data_21_ids <- data_21 %>% 
  mutate(survey_year = "2021-2022") %>% 
  rowid_to_column("id")

#merge datasets
merged_data <- rbind(data_19_ids, data_20_ids, data_21_ids) %>% 
  mutate(id = paste0(id, "_", survey_year))

write_csv(merged_data, "data/merged_data_19-22.csv") #save full data set with ids