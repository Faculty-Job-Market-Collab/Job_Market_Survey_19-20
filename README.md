# Job_Market_Survey_19-20

This repository contains the data cleaning, wrangling, and functions for manuscripts generated using the data from Job Market Surveys from 2019-2022 as well as the R markdown files used to curate/visualize compiled data. 

Manuscript-specfic code and data are held as submodules and presently include:
- `Kozik_Caregivers`
- `Jadavji_Biology`
- `Mollet_SocialSci`
- `Smith_Prestige`

## `surveys/`

Contains `pdf` files of each survey processed within this repository. The `pdf` file for each survey contains a full list of the questions and response options presented to survey respondents.

## `data_dumps/`

Contains R markdown files used to compile, visualize, and report large amounts of data for internal review.

## `code/`

Code files used to process data in preparation for analysis. Ensures standard data inputs and treatment of the data across analyses until their completion.

### `merge_raw_data.R`

Code to import data from all three survey years, 2019 to 2022, generate unique ids for each respondent, and merge into a single data file for prep and analysis.

*Data Required:*

- `data/2019-2020_raw_job_survey_data.csv`

- `data/2020-2021_raw_job_survey_data.csv`

- `data/2021-2022_raw_job_survey_data.csv`

- `data/question_legend_19.csv` (File to match questions with appropriate column names for 2019-2020 survey data.)

- `data/question_legend_20-22.csv` (File to match questions with appropriate column names for 2020-2022 survey data.)

*Output:*

- `data/merged_data_19-22.csv` (CSV file with all three survey years combined.)

### `clean_merged_data.R`

Clean and prep the merged, raw data file for import and analysis.

- Remove incomplete entries: respondents reporting a previous tenure track position or who did not report submitting any applications were dropped from all subsequent analyses.

- Collapse gender data categories: responses to questions regarding gender and sexual orientation were collapsed to group respondents by cis/straight men, cis/straight women, and LGBTQIA+ respondents.

- Collapse ethnicity data categories: respondents were allowed to select multiple race/ethnicity categories. These responses were collapsed to group respondents according to whether they belonged to a PEER (persons excluded due to ethnicity or race) group. A column was also added to quantify the number of responses selected.

- Separate & repair: several questions allowed respondents to select multiple options that needed to be separated into individual responses. Columns were also added to quantify the number of responses selected where appropriate.

  - Grants & fellowships

  - Feedback sources
  
  - Job ad data

  - Application outcomes

- Bin numerical responses: Free entry numerical values (e.g., number of Google Scholar citations, number of applications submitted) were sorted into pre-determined ranges for categorical analyses.

*Data Required:*

- `data/merged_data_19-22.csv` (CSV file with all three survey years combined.)

*Output:*

- `data/cleaned_data_19-22_`date generated`.csv`

### `analysis_functions.R`

### `get_plot_options.R`

### `get_ffi_data.R`

Use income-related data to generate a Financial Flexibility Index (FFI), a conservative estimate of financial resources for each applicant. Responses for income, student loan status, number and type of dependents, residual income, familial financial assistance, and gender were assigned values between 1 and 3 (details below), then added together to calculate the FFI value. Respondent FFIs were grouped into three categories (low, average, and high) based on the interquartile range of the group.

- Annual income (USD):

  - More than 120,001 == 3
  
  - 70,001 - 120,000 == 2
  
  - 25,000 - 70,000 == 1

- Student loan payment status:

  - "No financial aid needed" == 3 (suggests substantial extended-family financial flexibility)
  
  - "No, paid off" or "No, used grants & scholarships" == 2
  
  - "No, deferred" & reported status as a "PhD Candidate (ABD)" == 2, (loans are deferred due to student status)
  
  - "No deferred" & did not report being a student == 1 (suggests financial obligations incompatible with current income)
  
  - "Yes" == 1
  
- Familial financial support:

  - No financial support == income FFI value
  
  - Yes, but rarely & income range was "2" == 2 (occasional financial assistance is not unexpected)
  
  - Yes, frequently & income range was "1" == 2 (frequent financial assistance indicates good extended-family financial flexibility)
  
  - All other "Yes" responses == income FFI value
  
- Sources of residual income (rental properties, dividends, etc.):

  - Yes == 3
  
  - No == 2
  
- Dependents:

  - No dependents == 3
  
  - One child == 2
  
  - Multiple adult and/or children dependents == 1
  
- Gender: 

  - Men == 2
  
  - Women or LGB+/GNC == 1
  
- Childcare:

  - No dependents == 3
  
  - Dual-income/live-in relative == 2
  
  - Single parents == 1
  
  - Stay-at-home spouse == 1

*Data Required:*

- `data/cleaned_data_19-22_`date generated`.csv`

### `load_data.R`

*Data Required:*

- `data/cleaned_data_19-22_`date generated`.csv` (generated by `merge_raw_data.R` and `clean_merged_data.R`)

- `data/full_survey_inst_data.csv` (data for respondent-reported institution names; output from wrangle_carnegie_lists repo)

*Code Required:*

- `code/analysis_functions.R`

- `code/get_plot_options.R`

- `code/get_ffi_data.R`

### `tidy_ranking_data.R`

### `data_dump.R`
