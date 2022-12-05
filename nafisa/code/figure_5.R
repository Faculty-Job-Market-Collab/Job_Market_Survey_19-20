#Figure 5. Teaching and application submissions by gender & offers

#PUI vs RI
#5a----
fig5_gen_aov_data <- fig5_data %>% 
  filter(!is.na(inst_type)) %>% 
  filter(!is.na(adjusted_gender)) %>% 
  select(id, adjusted_gender, inst_type, values) %>% 
  distinct()

fig5_gen_aov_mean <- fig5_gen_aov_data %>% group_by(adjusted_gender, inst_type) %>% 
  summarise(avg = mean(as.numeric(values), na.rm = TRUE),
            std = sd(as.numeric(values), na.rm = TRUE))

fig5a_aov <- aov(values ~ inst_type * adjusted_gender, data = fig5_gen_aov_data)

fig5a_aov_summary <- summary(fig5a_aov)

fig5a_tukey <- TukeyHSD(fig5_gen_aov)

#PUI:Woman-RI:Man
#RI:Man-PUI:Man --
#RI:Woman-PUI:Woman --
#RI:Woman-PUI:Man --

fig5a_plot <- fig5_gen_aov_data %>% 
  ggplot(aes(x = adjusted_gender, y = as.numeric(values),
             fill = inst_type))+
  geom_boxplot()+
  geom_signif(annotations = "***", color = "gray62",
              y_position = c(40, 40, 50, 60), 
              xmin = c(.75, 2.75, .75, 1.25), 
              xmax = c(1.25, 3.25, 3.25, 2.75))+
  labs(x = "Gender (n=235)", y = "Number of applications submitted",
       fill = "Institution type",
       caption = "*** = p<0.001; ns = not significant")+
  my_theme_leg_bottom_horiz

ggsave("nafisa/figures/fig5a_pui-ri_gender_box.jpeg")

#5b----
fig5b_plot <- fig5_data %>% 
  filter(!is.na(inst_type)) %>% 
  filter(!is.na(adjusted_gender)) %>% 
  select(id, adjusted_gender, inst_type_bin, values_binned) %>% 
  distinct() %>%
  count(adjusted_gender, inst_type_bin, values_binned) %>% 
  spread(key = values_binned, value = n) %>% 
  mutate(across(3:15, ~ replace_na(.x, replace = 0))) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(3:15)), na.rm = TRUE),
         across(3:15, ~ get_percent(.x, total))) %>% 
  gather(3:15, key = values_binned, value = percent) %>% 
  select(-total) %>% distinct() %>% 
  ggplot(aes(x = factor(values_binned, 
                                 levels = bin_levels_small), 
             y = percent, fill = adjusted_gender))+
  geom_col(position = "dodge")+
  coord_flip()+
  facet_wrap(~inst_type_bin)+
  labs(x = "Number of applications submitted", 
       y = "Percent of gender (n=235)\n(grouped by institution type)",
       fill = "Gender")+
  my_theme_leg_bottom_horiz

ggsave("nafisa/figures/fig5b_pui-ri_gender_spread.jpeg")

#5c----
fig5_offer_aov_data <- fig5_data %>% 
  filter(!is.na(faculty_offers)) %>% 
  filter(!is.na(inst_type)) %>% 
  select(id, faculty_offers, values, inst_type) %>% 
  distinct()

fig5c_aov <- aov(values ~ inst_type * faculty_offers, data = fig5_offer_aov_data)

fig5c_aov_summary <- summary(fig5c_aov)

fig5c_tukey <- TukeyHSD(fig5_offer_aov)

fig5c_plot <- fig5_offer_aov_data %>% 
  select(id, inst_type, faculty_offers, values) %>% distinct() %>% 
  ggplot(aes(x = faculty_offers, y= as.numeric(values), 
             fill = inst_type))+
  geom_boxplot()+
  geom_signif(annotations = "***", color = "#00C19F",
              y_position = c(65, 70), 
              xmin = c(2.25, 1.25), 
              xmax = c(3.25, 3.25))+
  geom_signif(annotations = "***", color = "gray62",
              y_position = c(77, 85, 55, 77, 45, 77, 85, 55), 
              xmin = c(.75, 1.75, 2.75, .75, 2.25, 0.75, 1.75, 1.25), 
              xmax = c(1.25, 2.25, 3.25, 2.25, 2.75, 3.25, 3.25, 2.75))+
  geom_signif(annotations = "ns", color = "#F8766D",
              y_position = c(30, 30), 
              xmin = c(.75, 1.75), 
              xmax = c(1.75, 2.75))+
  geom_signif(annotations = "*", 
              y_position = c(50), xmin = c(1.25), xmax = c(1.75))+
  geom_signif(annotations = "ns", color = "#00C19F",
              y_position = c(40), xmin = c(1.25), xmax = c(2.25))+
  labs(y = "Applications submitted", x = "Number of faculty offers\n(n=232)",
       fill = "Institution type",
       caption = "*** = p<0.001; * = p<0.01; ns = not significant")+
  my_theme_leg_bottom_horiz

ggsave("nafisa/figures/fig5c_pui-ri_offers_box.jpeg")

#5d----
fig5d_plot <- fig5_data %>% 
  filter(!is.na(inst_type)) %>% 
  filter(!is.na(faculty_offers)) %>% 
  select(id, values_binned, inst_type_bin, faculty_offers) %>% 
  distinct() %>% 
  ggplot(aes(x = factor(values_binned, 
                        levels = bin_levels_small), 
             fill = faculty_offers))+
  geom_bar(position = "dodge")+
  coord_flip()+
  facet_wrap(~inst_type_bin)+
  labs(x = "Applications submitted\n(grouped by institution type)", y = "Number of responses (n=232)",
       fill = "Faculty offers")+
  my_theme_leg_bottom_horiz

ggsave("nafisa/figures/fig5d_pui-ri_offers_hist.jpeg")

#Teaching experience
#5e----
teach_gen_chi <- fisher.test(table(fig5_data$adjusted_gender, 
                                   fig5_data$teaching_type), 
                             simulate.p.value = TRUE)

teach_list <- fig5_data %>% 
  filter(!is.na(teaching_type)) %>% 
  filter(teaching_type != "Total adjunct positions") %>% 
  pull(teaching_type) %>% unique()

mult_teach_gen_chi <- map_df(teach_list, function(x){
  
  type_df <- map_df(teach_list, function(y){
    
    print(c(x, y))
    
    df <- fig5_data %>% 
      filter(teaching_type %in% c(x, y)) %>% distinct()
    
    if(x != y){
    
    test <- fisher.test(table(df$adjusted_gender, df$teaching_type))
    
    p <- tibble(type1 = x, type2 = y, p = as.character(test[[1]]))
    
    }else{
      p <- tibble(type1 = x, type2 = y, p = "N/A")
    }
    
    return(p)
  })
  
  return(type_df)
})

sig_teach_gen <- mult_teach_gen_chi %>% 
  filter(p != "N/A") %>% 
  filter(as.numeric(p) <= 0.05) %>% 
  distinct(p, .keep_all = TRUE) %>% 
  mutate(p = round(as.numeric(p), digits = 3))

write_csv(sig_teach_gen, "nafisa/figures/5e_p-values_teaching_genders.csv")
# men =50; women = 94
fig5e_plot <- fig5_data %>% 
  filter(!is.na(teaching_type)) %>% 
  filter(adjusted_gender %in% c("Man", "Woman")) %>% 
  filter(teaching_type != "Total adjunct positions") %>% 
  select(id, adjusted_gender, teaching_type) %>% distinct() %>% 
  count(adjusted_gender, teaching_type) %>% 
  mutate(total = if_else(adjusted_gender == "Man", 50, 94),
         percent = get_percent(n, total)) %>% 
  ggplot(aes(x = teaching_type, y = percent,
             fill = adjusted_gender))+
  geom_col(position = "dodge")+
  coord_flip() +
  facet_grid(~adjusted_gender)+
  labs(x = "Teaching type", y = "Percent of responses by gender* (n=144)",
       fill = "Gender*",
       caption = paste("Fisher's Exact Test p-value = ", 
                       round(teach_gen_chi[[1]], digits = 5), 
                       "\n*N too low to visualize Trans/NGC"))+
  my_theme_horiz

ggsave("nafisa/figures/fig5e_teach_gender.jpeg")

#offers vs no offers
#5f----
teach_offer_chi_yn <- fisher.test(table(fig5_data$faculty_yn, 
                                   fig5_data$teaching_type), 
                             simulate.p.value = TRUE)

mult_teach_offer_chi_yn <- map_df(teach_list, function(x){
  
  type_df <- map_df(teach_list, function(y){
    
    df <- fig5_data %>% 
      filter(teaching_type == x | teaching_type == y) %>% distinct()
    
    if(x != y){
      
      print(c(x, y))
      
      test <- fisher.test(table(df$faculty_yn, df$teaching_type))
      
      p <- tibble(type1 = x, type2 = y, p = as.character(test[[1]]))
      
    }else{
      p <- tibble(type1 = x, type2 = y, p = "N/A")
    }
    
    return(p)
  })
  
  return(type_df)
})

sig_teach_offer_yn <- mult_teach_offer_chi_yn %>% 
  filter(p != "N/A") %>% 
  filter(as.numeric(p) <= 0.05) %>% 
  distinct(p, .keep_all = TRUE) %>% 
  mutate(p = round(as.numeric(p), digits = 3))

write_csv(sig_teach_offer_yn, "nafisa/figures/fig5f_p-values_teaching_yes_vs_no_offers.csv")

#one offer vs multiple offers
teach_offer_chi <- fisher.test(table(fig5_data$faculty_offers, 
                                     fig5_data$teaching_type), 
                               simulate.p.value = TRUE)

offer_teach_list <- c("Teaching assistant", "Grad guest lecturer",                
                      "Undergrad co-instructor", "Guest lecturer",                     
                      "Lecturing for Workshops", "Teaching certificate",               
                      "Undergrad guest lecturer", "Lab Course Instructor",              
                      "Undergrad instructor", "High school instructor",             
                      "Undergrad adjunct, community or PUI",
                      "Grad instructor", "Co-instructor/lecturer",             
                      "Grad co-instructor", "Independent instructor",             
                      "Visiting assistant professor")

mult_teach_offer_chi <- map_df(offer_teach_list, function(x){
  
  type_df <- map_df(offer_teach_list, function(y){
    
    df <- fig5_data %>% 
      filter(faculty_offers != "0") %>% 
      filter(teaching_type == x | teaching_type == y) %>% distinct()
    
    if(x != y){
      
      print(c(x, y))
      
      test <- fisher.test(table(df$faculty_offers, df$teaching_type))
      
      p <- tibble(type1 = x, type2 = y, p = as.character(test[[1]]))
      
    }else{
      p <- tibble(type1 = x, type2 = y, p = "N/A")
    }
    
    return(p)
  })
  
  return(type_df)
})

sig_teach_offer_mult <- mult_teach_offer_chi %>% 
  filter(p != "N/A") %>% 
  filter(as.numeric(p) <= 0.05) %>% 
  distinct(p, .keep_all = TRUE) %>% 
  mutate(p = round(as.numeric(p), digits = 3))

write_csv(sig_teach_offer_mult, "nafisa/figures/fig5f_p-values_teaching_1_vs_mult_offers.csv")

fig5f_plot <- fig5_data %>% 
  filter(!is.na(teaching_type)) %>% 
  filter(!is.na(faculty_offers)) %>% 
  filter(teaching_type != "Total adjunct positions") %>%
  ggplot(aes(x = teaching_type, fill = faculty_offers))+
  geom_bar()+
  coord_flip()+
  facet_grid(~faculty_offers)+
  labs(x = "Teaching type", y = "Number of responses (n=130)",
       caption = paste("Fisher's Exact Test p-value = ", round(teach_offer_chi[[1]], digits = 5))
       )+
  my_theme_horiz

ggsave("nafisa/figures/fig5f_teach_offers.jpeg")

#compile stats----

aov_list <- c("fig5a_aov_summary", "fig5c_aov_summary")

plot_list <- c('5A', '5C')

fig5_aov_tbl_raw <- map2_df(aov_list, plot_list, get_wilcox_tbl) 

fig5_aov_tbl <- fig5_aov_tbl_raw %>% 
  mutate(compare = case_when(
    str_detect(attribute, "1") ~ "inst_type",
    str_detect(attribute, "2") ~ "adjusted_gender",
    str_detect(attribute, "3") ~ "inst_type:adjusted_gender",
    str_detect(attribute, "4") ~ "Residuals",
  ),
  attribute = str_remove_all(attribute, "[[:digit:]]")) %>% 
  spread(key = attribute, value = value) 

write_csv(fig5_aov_tbl, "nafisa/figures/fig5ac_aov_stats.csv")

#5a tukey
fig5a_t1 <-fig5a_tukey[[1]] %>% as_tibble(rownames = "comparison") %>% 
  mutate(variables = "institution type",
         figure = "5A")

fig5a_t2 <-fig5a_tukey[[2]] %>% as_tibble(rownames = "comparison") %>% 
  mutate(variables = "gender", 
         figure = "5A")

fig5a_t3 <-fig5a_tukey[[3]] %>% as_tibble(rownames = "comparison") %>% 
  mutate(variables = "institution type: gender", 
         figure = "5A")

#5c tukey
fig5c_t1 <-fig5c_tukey[[1]] %>% as_tibble(rownames = "comparison") %>% 
  mutate(variables = "institution type",
         figure = "5C")

fig5c_t2 <-fig5c_tukey[[2]] %>% as_tibble(rownames = "comparison") %>% 
  mutate(variables = "faculty offers",
         figure = "5C")

fig5c_t3 <-fig5c_tukey[[3]] %>% as_tibble(rownames = "comparison") %>% 
  mutate(variables = "institution type:faculty offers",
         figure = "5C")

fig5_tukey_data <- rbind(fig5a_t1, fig5a_t2, fig5a_t3,
                         fig5c_t1, fig5c_t2, fig5c_t3) %>% 
  mutate(method = "Tukey multiple comparisons of means", 
         confidence = "95") %>% 
  rename("difference in observed means" = diff, 
         "lower end point of the interval" = lwr, 
         "upper end point of the interval" = upr, 
         "adjusted p-value" = `p adj`)
write_csv(fig5_tukey_data, "nafisa/figures/fig5ac_tukey_stats.csv")

#generate plot----
Fig5ab <- plot_grid(fig5a_plot, fig5b_plot, 
                    labels = c('A', 'B'),
                    label_size = 18, nrow = 1)

Fig5cd <- plot_grid(fig5c_plot, fig5d_plot,
                    labels = c('C', 'D'),
                    label_size = 18, nrow = 1)

Fig5ef <- plot_grid(fig5e_plot, fig5f_plot,
                    labels = c('E', 'F'),
                    label_size = 18, nrow = 2)

Fig5 <- plot_grid(Fig5ab, Fig5cd, Fig5ef, ncol = 1,
                  rel_heights = c(.75, .75, 1))


ggsave("Figure_5.png", device = 'png', units = "in", scale = 1.75,
       path = 'nafisa/figures/', width = 8, height = 14)