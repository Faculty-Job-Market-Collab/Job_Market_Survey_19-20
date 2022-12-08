library(scales)
library(RColorBrewer)
library(cowplot)
library(grid)
#library(patchwork)

#preferred themes----
base_theme <- theme_classic() + #basic plot settings for horizontal x-axis labels w/ legend
  theme(axis.text=element_text(size=16), #font size
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=20,face="bold"), 
        plot.caption=element_text(size=16, hjust=.5),
        plot.subtitle = element_text(size = 16, hjust = .5),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 18))

my_theme <- base_theme + #no legend, angled x-axis labels
  theme(legend.position = "none",  
        axis.text.x = element_text(angle = 45, hjust = 1))

my_theme_horiz <- base_theme+ #no legend, horizontal x-axis labels
  theme(legend.position = "none")

my_theme_leg <- base_theme + #angled x-axis labels w/ legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

my_theme_leg_left <- base_theme + #horizontal x-axis labels w/ legend on left
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left")

my_theme_leg_bottom <- base_theme + #angled x-axis labels w/ legend below plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

my_theme_leg_bottom_horiz <- base_theme + #horizontal x-axis labels w/ legend below plot
  theme(legend.position = "bottom")

right_margin <- theme(plot.margin = margin(t = 5.5, r = 15, b = 5.5, l = 5.5, 
                                           unit = "pt")) #add padding

#to remove padding between axis and data: scale_x/y_discrete/continuous(expand = c(0,0))+

#adding proportion/count labels to barchart
prop_lab_low <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = 1)

count_lab_low <- geom_text(stat = "count", aes(label=..count..), vjust = 1)

prop_lab_high <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = -1)

count_lab_high <- geom_text(stat = "count", aes(label=..count..), vjust = -1)

#colorblind palettes----
cbPalette <- c("#000000", #black
               "#E69F00", #goldenrod
               "#56B4E9", #light blue
               "#009E73", #dark green
               "#F0E442", #gold
               "#0072B2", #blue
               "#D55E00", #orange rust
               "#CC79A7", #dusty pink
               "#999999") #grey

#adjusted_gender palette
gender_breaks <- c("Man", "Woman", "Trans/GNC")

gender_color <- c("#009E73", "#D55E00", "#E69F00")

#simple_gender
gender_simple_breaks <- c("Man", "Woman/Trans/GNC", "No Response")

#peer palette
peer_breaks <- c("Yes", "No")

peer_color <- c("#56B4E9", "#F0E442")

#research palette
research_breaks <- c("Biological Sciences", "Chemistry",
                     "Computer & Information Sciences",    
                     "Engineering", "Geosciences", "Humanities",
                     "Integrated Sciences", 
                     "Mathematical & Physical Sciences", 
                     "Social, Behavior, &\nEconomic Sciences")

#plotting placeholder----
blank_plot <- ggplot()+
  theme_void()

blank <- plot_grid(blank_plot)

#plotting functions----

#plot feature weights
feature_box_plot <- function(df){
  plot <- ggplot(df)+
    geom_boxplot(aes(x = clean_feat, y = weight))+
    coord_flip()+
    labs(x = "\nLogistic Regression Variables", 
         y = "Weight (Absolute value)")+
    my_theme_horiz
  
  return(plot)
}

#generate legend-only plot
get_legend_plot <- function(p){
  
  leg <- get_legend(p)
  
  plot_grid(leg, greedy = FALSE) 
}
