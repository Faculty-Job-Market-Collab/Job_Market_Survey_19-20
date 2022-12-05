#Figure 2 Applicant Scholarly Metrics by Gender


#A. Applicant field

fig2a_plot <- bio_demo_data %>% 
  filter(question == "adjusted_gender") %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(adjusted_gender)) %>% 
  select(id, adjusted_gender) %>% 
  ggplot(aes(x = adjusted_gender, fill = adjusted_gender)) + 
  geom_bar()+
  coord_flip()+
  labs(x = "Gender", y = "Number of responses (n=319)",
       caption = '')+
  my_theme_horiz

ggsave("nafisa/figures/fig2a_field.jpeg")
  
#B. Applicant location

fig2b_plot <- fig2_data %>% select(id, adjusted_gender, residence) %>% 
  filter(adjusted_gender != "Trans/GNC") %>% 
  ggplot(aes(x = adjusted_gender, fill = adjusted_gender)) + 
  geom_bar()+
  facet_wrap(~residence, scales = "free_x")+
  coord_flip()+
  labs(x = "Gender*", y = "Number of responses (n=319)",
       caption = "*N of Trans/GNC too low to report")+
  my_theme_horiz

ggsave("nafisa/figures/fig2b_location.jpeg")

#C. Applicant position----

fig2c_table <- table(fig2_data$position, fig2_data$adjusted_gender)

fig2c_chi <- chisq.test(fig2c_table)

fig2c_plot <- fig2_data %>% select(id, adjusted_gender, position) %>% 
  filter(!is.na(position)) %>%
  filter(adjusted_gender != "Trans/GNC") %>% 
  ggplot(aes(x = adjusted_gender, fill = adjusted_gender)) + 
  geom_bar()+
  facet_wrap(~position, scales = "free_x")+
  #coord_flip()+
  labs(x = "Gender*", y = "Number of responses\n(n=285)",
      caption = "Pearson's Chi-squared test: ns\n*N of Trans/GNC too low to visualize")+
  my_theme_horiz

ggsave("nafisa/figures/fig2c_position.jpeg")

#D. Number of postdoc positions----

fig2d_data <- fig2_data %>% 
  select(id, adjusted_gender, number_postdocs) %>% distinct() %>% 
  filter(!is.na(number_postdocs))

fig2d_table <- table(fig2d_data$number_postdocs, fig2d_data$adjusted_gender)

fig2d_chi <- chisq.test(fig2d_table)

#Perform the Mann-Whitney U test -- changed to chisq b/c 3 gender cats
#fig2d_mw<-wilcox.test(as.numeric(number_postdocs) ~ adjusted_gender, data=fig2d_data, 
#                na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

fig2d_plot <- fig2d_data %>% 
  filter(!is.na(number_postdocs)) %>% 
  ggplot(aes(x = factor(number_postdocs, 
                        levels = c("1", "2", "3", ">3")), 
             fill = adjusted_gender)) + 
  geom_bar()+
  facet_wrap(~adjusted_gender, scales = "free_x")+
  coord_flip()+
  labs(x = "Number of postdoctoral\npositions",
       y = "Number of responses (n=117)",
       caption = "Pearson's Chi-squared test: ns")+
  my_theme_horiz

ggsave("nafisa/figures/fig2d_num_postdoc.jpeg")

#compile stats----

stats_list <- c("fig2c_chi", "fig2d_chi")

plot_list <- c('2A', '2B')

fig2_stats_tbl_raw <- map2_df(stats_list, plot_list, get_wilcox_tbl) 

fig2_stats_tbl <- fig2_stats_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  select(figure, method, `statistic.X-squared`, p.value, parameter.df)
  
write_csv(fig2_stats_tbl, "nafisa/figures/fig2_stats.csv")

#generate plot----
Fig2ab <- plot_grid(fig2a_plot, fig2b_plot, 
                  labels = c('A', 'B'),
                  rel_widths = c(.75, 1),
                  label_size = 18, nrow = 1)

Fig2cd <- plot_grid(fig2c_plot, fig2d_plot,
                    labels = c('C', 'D'),
                    label_size = 18, nrow = 1)

Fig2 <- plot_grid(Fig2ab, Fig2cd, nrow = 2)


ggsave("Figure_2.png", device = 'png', units = "in", scale = 1.75,
       path = 'nafisa/figures/', width = 6, height = 4)