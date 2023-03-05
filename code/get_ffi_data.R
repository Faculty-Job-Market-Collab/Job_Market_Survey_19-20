#Financial Flexibility Index calculations
ffi_raw_data <- clean_data %>% 
  select(id, income, student_loan, financial_support, dependents, 
         primary_caregiver, adjusted_gender,
         extra_income, partner_occupation, position) %>% 
  distinct() %>% 
  mutate(income_ffi = fct_collapse(income, 
                             "3" = c("More than $150,000", "$120,001 - 150,000"),
                             "2" = c("$90,001 - 120,000", "$70,001 - 90,000"),
                             "1" = c("$50,001 - 70,000", "$30,001 - 50,000",
                                     "$25,000 - 30,000")),
         loan_ffi = case_when(
           student_loan == "No financial aid" ~ "3", #(suggests substantial extended-family financial flexibility)
           student_loan == "Yes" ~ "1",
           str_detect(student_loan, "No, Paid|Grants & scholarships") ~ "2",
           student_loan == "No, Deferred" & position == "PhD Candidate (ABD)" ~ "2", #(loans are deferred due to student status)
           student_loan == "No, Deferred" & position != "PhD Candidate (ABD)" ~ "1", #(suggests financial obligations incompatible with current income)
           ),
         support_ffi = case_when(
           financial_support == "No" & income_ffi == "3" ~ "3",
           financial_support == "No" & income_ffi == "2" ~ "2",
           financial_support == "No" & income_ffi == "1" ~ "1",
           financial_support == "Yes, but rarely" & income_ffi == "2" ~ "2", #(occasional financial assistance is not unexpected)
           str_detect(financial_support, "Yes, frequently") & income_ffi == "1" ~ "2", #(frequent financial assistance indicates good extended-family financial flexibility)
           str_detect(financial_support, "Yes") & income_ffi == "2" ~ "2",
           str_detect(financial_support, "Yes") & income_ffi == "3" ~ "3",
           str_detect(financial_support, "Yes") & income_ffi == "1" ~ "1"
             ),
         extra_ffi = if_else(extra_income == "Yes", "3", "2"),
         dependent_ffi = fct_collapse(dependents,
                                      "3" = "No dependents", 
                                      "2" = "Yes, one child",
                                      "1" = c("Yes, adult(s)", "Yes, multiple children",
                                              "Yes, adult(s) and child(ren)")),
         gender_ffi = fct_collapse(adjusted_gender,
                                   "2" = "Man",
                                   "1" = c("Woman", "LGB+/GNC")),
         childcare_ffi = case_when(
           primary_caregiver == "No dependents" ~ "3",
           primary_caregiver == "Myself, Single" ~ "1",
           primary_caregiver == "My partner" & partner_occupation == "Primary caregiver (stay at home parent)" ~ "1",
           primary_caregiver %in% c("A live-in relative", "Co-parents",
                                    "Myself, Partnered", "My partner") ~ "2"
         )) %>% 
  mutate(across(contains("_ffi"), as.numeric)) %>% 
  mutate(across(contains("_ffi"), 
                function(x){replace_na(x, replace = 0)})) %>% 
  rowwise() %>% 
  mutate(ffi_value = sum(c_across(11:17))) %>%
  filter(ffi_value >= 6) %>% #allows for two unanswered questions
  select(id, ffi_value)

ffi_summary <- summary(ffi_raw_data$ffi_value) #identify ffi min, max, and IQR values to create categories

ffi_data <- ffi_raw_data %>% 
  mutate(ffi_bins = case_when( #bin ffi values into 3 categories based on IQR values
    ffi_value < 13 ~ "Low",
    ffi_value >= 13 & ffi_value <= 15 ~ "Average",
    ffi_value > 15 ~ "High"
  ))