#get world regions----
AFR <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
         "Cape Verde", "Central African Republic", "Chad", "Comoros", "Ivory Coast", 
         "Democratic Republic of the Congo", "Equatorial Guinea", "Eritrea", "Ethiopia", 
         "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", 
         "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", 
         "Niger", "Nigeria", "Republic of the Congo", "Rwanda", "Sao Tome and Principe", "Senegal", 
         "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Eswatini", "Togo", 
         "Uganda", "Tanzania", "Zambia", "Zimbabwe", "Cote de Ivoire", "Eswatini")

AMR <- c("Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", 
         "Chile", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", 
         "El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", 
         "Nicaragua", "Panama", "Paraguay", "Peru", "Saint Kitts and Nevis", "Saint Lucia", 
         "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela")

SEAR <- c("Bangladesh", "Bhutan", "North Korea", "India", "Indonesia", "Maldives", "Myanmar", "Nepal", 
          "Sri Lanka", "Thailand", "Timor-Leste")

EUR <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", 
         "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Russian Federation", "Georgia", 
         "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Kazakhstan", 
         "Kyrgyzstan", "Latvia", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", 
         "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", 
         "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", 
         "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan")

EMR <- c("Afghanistan", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", "Jordan", "Kuwait", "Lebanon", 
         "Libya", "Morocco", "Oman", "Pakistan", "Palestine", "Qatar", "Saudi Arabia", "Somalia", 
         "Sudan", "Syria", "Tunisia", "United Arab Emirates", "Yemen", "Northern Cyprus")

WPR <- c("Australia", "Brunei", "Cambodia", "China", "Cook Islands", "Fiji", "Japan", "Kiribati", 
         "Laos", "Malaysia", "Marshall Islands", "Micronesia", "Mongolia", "Nauru", "New Zealand", 
         "Niue", "Palau", "Papua New Guinea", "Philippines", "Samoa", "Singapore", "Solomon Islands", 
         "South Korea", "Taiwan", "Tonga", "Tuvalu", "Vanuatu", "Vietnam", "Hong Kong", "Macao",
         "Brunei Darussalam")

get_world_region <- function(x, y = NULL){
  region <- case_when(
    x %in% AFR ~ "African",
    x %in% AMR ~ "The Americas",
    x %in% SEAR ~ "South-East Asian",
    x %in% EUR ~ "European",
    x %in% EMR ~ "Eastern Mediterranean",
    x %in% WPR ~ "Western Pacific",
    x == "Canada" | x == "canada" ~ "Canada",
    x == "USA" | x == "United States" | x == "Puerto Rico" ~ "USA"
  )
  
  if(is.null(y)){
    
    return(region)
    
  }else{
    
    check_region <- if_else(is.na(region), y, region)
    
    return(check_region)
  }
  
}

the_rank_data_raw <- read_csv("data/THE-World-Uni-Rankings.csv")

the_rank_data <- the_rank_data_raw %>% 
  mutate(the_rank = str_remove_all(Rank, "–.*"),
         the_top_five = if_else((as.numeric(the_rank) <= (5*2112)/100), "yes", "no"),
         the_top_ten = if_else((as.numeric(the_rank) <= (10*2112)/100), "yes", "no"),
         the_top_fifteen = if_else((as.numeric(the_rank) <= (15*2112)/100), "yes", "no"),
         the_top_twenty = if_else((as.numeric(the_rank) <= (20*2112)/100), "yes", "no"),
         the_top_twenty_five = if_else((as.numeric(the_rank) <= (25*2112)/100), "yes", "no"),
         the_rank_binned = case_when(
           as.numeric(Rank) >= 1 & as.numeric(Rank) <= 10 ~ "1–10",
           as.numeric(Rank) >= 11 & as.numeric(Rank) <= 20 ~ "11–20",
           as.numeric(Rank) >= 21 & as.numeric(Rank) <= 30 ~ "21–30",
           as.numeric(Rank) >= 31 & as.numeric(Rank) <= 40 ~ "31–40",
           as.numeric(Rank) >= 41 & as.numeric(Rank) <= 50 ~ "41–50",
           as.numeric(Rank) >= 51 & as.numeric(Rank) <= 100 ~ "51–100",
           as.numeric(Rank) >= 101 & as.numeric(Rank) <= 150 ~ "101–150",
           as.numeric(Rank) >= 151 & as.numeric(Rank) <= 200 ~ "151–200",
           TRUE ~ Rank),
         world_region = map(Country, get_world_region),
         world_region = unlist(world_region)) %>% 
  mutate(Name = str_to_title(Name),
         Name = str_replace(Name, "Saint |^St ", "St "),
         Name = str_replace_all(Name, "&", "And"),
         Name = str_replace_all(Name, "Aandm|AAndM", "A And M"),
         Name = str_replace_all(Name, "Aandt|AAndT", "A And T"),
         Name = str_replace(Name, " At | In |/", " "),
         Name = str_replace_all(Name, "-", " "),
         #Name = str_remove(Name, "Penn State "),
         Name = str_remove_all(Name, "The |Campus|,"),
         #Name = map(Name, replace_uny),
         #Name = unlist(Name),
         Name = str_squish(Name),
         Name = str_replace_all(Name, ",|'|\\.| –|\\(|\\)|’", ""))

rank_carn_data <- carn_data %>% select(DOCRSDEG) %>% 
  distinct() %>% arrange(desc(DOCRSDEG)) %>% 
  filter(!is.na(DOCRSDEG)) %>% 
  rowid_to_column("drdeg_rank") %>% 
  left_join(carn_data, ., by = "DOCRSDEG") %>% 
  select(id, inst_type, NAME, drdeg_rank, PUI_RI, US_region,
         world_region) %>% 
  mutate(drdeg_top_five = if_else((as.numeric(drdeg_rank) <= (5*190)/100), "yes", "no"),
         drdeg_top_ten = if_else((as.numeric(drdeg_rank) <= (10*190)/100), "yes", "no"),
         drdeg_top_fifteen = if_else((as.numeric(drdeg_rank) <= (15*190)/100), "yes", "no"),
         drdeg_top_twenty = if_else((as.numeric(drdeg_rank) <= (20*190)/100), "yes", "no"),
         drdeg_top_twenty_five = if_else((as.numeric(drdeg_rank) <= (25*190)/100), "yes", "no")#,
         #drdeg_rank_binned = case_when(
         #  as.numeric(drdeg_rank) >= 1 & as.numeric(drdeg_rank) <= 10 ~ "1–10",
         #  as.numeric(drdeg_rank) >= 11 & as.numeric(drdeg_rank) <= 20 ~ "11–20",
         #  as.numeric(drdeg_rank) >= 21 & as.numeric(drdeg_rank) <= 30 ~ "21–30",
         #  as.numeric(drdeg_rank) >= 31 & as.numeric(drdeg_rank) <= 40 ~ "31–40",
         #  as.numeric(drdeg_rank) >= 41 & as.numeric(drdeg_rank) <= 50 ~ "41–50",
         #  as.numeric(drdeg_rank) >= 51 & as.numeric(drdeg_rank) <= 100 ~ "51–100",
         #  as.numeric(drdeg_rank) >= 101 & as.numeric(drdeg_rank) <= 150 ~ "101–150",
         #  as.numeric(drdeg_rank) >= 151 & as.numeric(drdeg_rank) <= 200 ~ "151–200",
         #  TRUE ~ drdeg_rank)
         )

all_rank_data <- select(the_rank_data, -Rank, -Country) %>% 
  left_join(rank_carn_data, ., by = c("NAME" = "Name", "world_region"))

