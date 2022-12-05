#Figure 3. Job application benchmarks by gender and offers (0, 1, >1 offers)


#Number of applications submitted
#a----
#Perform the Mann-Whitney U test
#fig3a_mw <- wilcox.test(as.numeric(apps_submitted) ~ adjusted_gender, data=fig3_data, 
#                      na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

#Need chi-sq b/c of 3 genders
fig3a_table <- table(fig3_data$apps_submitted, fig3_data$adjusted_gender)

fig3a_chi <- chisq.test(fig3a_table)

fig3a_data <- fig3_data %>% 
  filter(!is.na(apps_submitted_binned)) %>%
  count(adjusted_gender, apps_submitted_binned) %>% 
  spread(key = apps_submitted_binned, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:14)), na.rm = TRUE)) %>% 
  gather(2:14, key = "apps_submitted_binned", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_gender = get_percent(n, total))

fig3a_plot_leg <- fig3a_data %>% 
  ggplot(aes(x = factor(apps_submitted_binned, 
                        levels = bin_levels_small), 
             y = percent_gender,
             fill = adjusted_gender))+
  geom_col(position = "dodge")+
  #facet_wrap(~apps_submitted_binned, scales = "free_x")+
  labs(x = "Number of applications submitted", 
       y = "Percent of responses by gender\n(n=228)",
       caption = "Pearson's Chi-squared test: ns", 
       fill = "Gender")+
  my_theme_leg

fig3a_plot <- fig3a_plot_leg+
  my_theme

ggsave("nafisa/figures/fig3a_apps_gender.jpeg")

#b----
fig3b_kwh <- kruskal.test(apps_submitted ~ faculty_offers, data = fig3_data)

fig3b_pw <- pairwise.wilcox.test(as.numeric(fig3_data$apps_submitted), 
                                 fig3_data$faculty_offers,
                     p.adjust.method = "BH")

fig3b_plot <- fig3_data %>% 
  ggplot(aes(y = as.numeric(apps_submitted), x = faculty_offers
             ))+
  geom_boxplot()+
  geom_signif(annotations = "***", color = "gray62",
              y_position = c(65, 75, 85), 
              xmin = c(1, 1, 2), 
              xmax = c(2, 3, 3))+
  labs(y = "Number of applications submitted", x = "Faculty offers",
       caption = "*** = p-value<0.005; Kruskal-Wallis and Pairwise Wilcox")+
  my_theme

ggsave("nafisa/figures/fig3b_apps_offers_box.jpeg")

#c----
fig3c_plot <- fig3_data %>% 
  ggplot(aes(x = factor(apps_submitted_binned, 
                        levels = bin_levels_small)))+
  geom_bar(position = "dodge")+
  facet_wrap(~faculty_offers, scales = "free_y")+
  labs(x = "Number of applications submitted", y = "Number of responses")+
  my_theme

ggsave("nafisa/figures/fig3c_apps_offers_hist.jpeg")

#Remote interviews
#d ----

#fig3d_mw<-wilcox.test(as.numeric(off_site_interviews) ~ adjusted_gender, data=fig3_data, 
#                      na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

fig3d_table <- table(fig3_data$off_site_interviews, fig3_data$adjusted_gender)

fig3d_chi <- chisq.test(fig3d_table)

fig3d_data <- fig3_data %>% 
  filter(!is.na(off_site_interviews)) %>%
  count(adjusted_gender, off_site_interviews) %>% 
  spread(key = off_site_interviews, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:18)), na.rm = TRUE)) %>% 
  gather(2:18, key = "off_site_interviews", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_gender = get_percent(n, total))

fig3d_plot <- fig3d_data %>% 
  ggplot(aes(x = as.numeric(off_site_interviews), 
             y = percent_gender,
             fill = adjusted_gender))+
  geom_col(position = "dodge")+
  facet_wrap(~adjusted_gender, scales = "free_x")+
  labs(x = "Number of off-site interviews", 
       y = "Percent of responses by gender\n(n=229)",
       caption = "Pearson's Chi-squared test: ns")+
  my_theme

ggsave("nafisa/figures/fig3d_remote_int_gender.jpeg")

#e----
fig3e_kwh <- kruskal.test(as.numeric(off_site_interviews) ~ faculty_offers, data = fig3_data)

fig3e_pw <- pairwise.wilcox.test(as.numeric(fig3_data$off_site_interviews), 
                                 fig3_data$faculty_offers,
                                 p.adjust.method = "BH")

fig3e_plot <- fig3_data %>% 
  ggplot(aes(y = as.numeric(off_site_interviews), 
             x = faculty_offers))+
  geom_boxplot()+
  geom_signif(annotations = "***", color = "gray62",
              y_position = c(15, 20, 25), 
              xmin = c(1, 1, 2), 
              xmax = c(2, 3, 3))+
  labs(y = "Number of off-site interviews", x = "Faculty offers",
       caption = "*** = p-value<0.005; Kruskal-Wallis and Pairwise Wilcox")+
  my_theme_horiz

ggsave("nafisa/figures/fig3e_off-site_offers_box.jpeg")

#f----
fig3f_plot <- fig3_data %>% 
  ggplot(aes(x = as.numeric(off_site_interviews)))+
  geom_bar(position = "dodge")+
  facet_wrap(~faculty_offers, scales = "free_y")+
  labs(x = "Number of off-site interviews", y = "Number of responses")+
  my_theme

ggsave("nafisa/figures/fig3f_off-site_offers_hist.jpeg")

#Onsite interviews

#g----
#fig3g_mw<-wilcox.test(as.numeric(on_site_interviews) ~ adjusted_gender, data=fig3_data, 
#                      na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

fig3g_table <- table(fig3_data$on_site_interviews, fig3_data$adjusted_gender)

fig3g_chi <- chisq.test(fig3g_table)

fig3g_data <- fig3_data %>% 
  filter(!is.na(on_site_interviews)) %>%
  count(adjusted_gender, on_site_interviews) %>% 
  spread(key = on_site_interviews, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:20)), na.rm = TRUE)) %>% 
  gather(2:20, key = "on_site_interviews", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_gender = get_percent(n, total))

fig3g_plot <- fig3g_data %>% 
  ggplot(aes(x = as.numeric(on_site_interviews), 
             y = percent_gender,
             fill = adjusted_gender))+
  geom_col()+
  facet_wrap(~adjusted_gender)+
  labs(x = "Number of on-site interviews", 
       y = "Percent of responses by gender\n(n=228)",
       caption = "Pearson's Chi-squared test: ns")+
  my_theme

ggsave("nafisa/figures/fig3g_onsite_int_gender.jpeg")

#h----
fig3h_kwh <- kruskal.test(as.numeric(on_site_interviews) ~ faculty_offers, data = fig3_data)

fig3h_pw <- pairwise.wilcox.test(as.numeric(fig3_data$on_site_interviews), 
                                 fig3_data$faculty_offers,
                                 p.adjust.method = "BH")

fig3h_plot <- fig3_data %>% 
  ggplot(aes(y = as.numeric(on_site_interviews), 
             x = faculty_offers))+
  geom_boxplot()+
  geom_signif(annotations = "***", color = "gray62",
              y_position = c(10, 15, 20), 
              xmin = c(1, 1, 2), 
              xmax = c(2, 3, 3))+
  labs(y = "Number of on-site interviews", x = "Faculty offers",
       caption = "*** = p-value<0.005; Kruskal-Wallis and Pairwise Wilcox")+
  my_theme_horiz

ggsave("nafisa/figures/fig3h_onsite_offers_box.jpeg")

#3i----
fig3i_plot <- fig3_data %>% 
  ggplot(aes(x = as.numeric(on_site_interviews)))+
  geom_bar(position = "dodge")+
  facet_wrap(~faculty_offers, scales = "free_y")+
  labs(x = "Number of on-site interviews", y = "Number of responses")+
  my_theme

ggsave("nafisa/figures/fig3i_onsite_offers_hist.jpeg")

#Number of offers
#3j----

fig3j_table <- table(fig3_data$faculty_offers, fig3_data$adjusted_gender)

fig3j_chi <- chisq.test(fig3j_table)

fig3j_data <- fig3_data %>% 
  filter(!is.na(faculty_offers)) %>%
  count(adjusted_gender, faculty_offers) %>% 
  spread(key = faculty_offers, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:4)), na.rm = TRUE)) %>% 
  gather(2:4, key = "faculty_offers", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_gender = get_percent(n, total),
         faculty_offers = factor(faculty_offers,
                                 levels = c("0", "1", ">1")))

fig3j_plot <- fig3j_data %>% 
  ggplot(aes(x = faculty_offers, 
             y = percent_gender,
             fill = adjusted_gender))+
  geom_col(position = "dodge")+
  coord_flip()+
  #facet_wrap(~faculty_offers, scales = "free_y")+
  labs(x = "Number of faculty offers", 
       y = "Percent of responses by gender (n=229)",
       fill = "Gender",
       caption = "Pearson's Chi-squared test: ns")+
  my_theme

ggsave("nafisa/figures/fig3j_offers_gender.jpeg")

#compile chi stats (a,d,g,j)----
chi_list <- c("fig3a_chi", "fig3d_chi", 
                 "fig3g_chi", "fig3j_chi")

plot_list <- c('3A', '3D', '3G', '3J')

fig3_chi_tbl_raw <- map2_df(chi_list, plot_list, get_wilcox_tbl) 

fig3_chi_tbl <- fig3_chi_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  select(figure, method, `statistic.X-squared`, p.value, parameter.df)

write_csv(fig3_chi_tbl, "nafisa/figures/fig3_chi_stats.csv")

#compile kw stats (b,e,h) ----
kw_list <- c("fig3b_kwh", "fig3e_kwh", "fig3h_kwh")

pw_list <- c("fig3b_pw", "fig3e_pw", "fig3h_pw")

plot_list <- c('3B', '3E', '3H')

fig3_kw_tbl_raw <- map2_df(kw_list, plot_list, get_wilcox_tbl) 

fig3_kw_tbl <- fig3_kw_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  select(-data.name)

write_csv(fig3_kw_tbl, "nafisa/figures/fig3_kruskal-wallis_stats.csv")

fig3_pw1_tbl_raw <- map2_df(pw_list, plot_list, get_wilcox_tbl)

fig3_pw_tbl <- fig3_pw_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  mutate(p.adjust.method = "Benjamini & Hochberg Correction (1995)") %>% 
  select(-data.name, -p.value3) %>% 
  rename(`p.value 0 vs. 1` = p.value1, `p.value 0 vs. >1` = p.value2,
         `p.value 1 vs. >1` = p.value4)

write_csv(fig3_pw_tbl, "nafisa/figures/fig3_pairwise-wilcox_stats.csv")

#generate plot----

fig3_gen_leg <- get_legend_plot(fig3a_plot_leg)

fig3a_leg <- plot_grid(fig3j_plot, fig3_gen_leg,
                       labels = c('A', ''), 
                       rel_heights = c(.5, 2),
                       label_size = 18)

fig3bcd <- plot_grid(fig3a_plot, fig3b_plot, fig3c_plot, 
                     labels = c('B', 'C', 'D'),
                     rel_widths = c(.6, .6, 1),
                     label_size = 18, ncol = 3)

fig3efg <- plot_grid(fig3d_plot, fig3e_plot, fig3f_plot, 
          labels = c('E', 'F', 'G'),
          label_size = 18, ncol = 3)

fig3hij <- plot_grid(fig3g_plot, fig3h_plot,
          fig3i_plot,
          labels = c('H', 'I', 'J'),
          label_size = 18, ncol = 3)

Fig3 <- plot_grid(fig3a_leg, fig3bcd, fig3efg, fig3hij, ncol = 1)

ggsave("Figure_3.png", device = 'png', units = "in", scale = 1.75,
       path = 'nafisa/figures/', width = 12, height = 11)