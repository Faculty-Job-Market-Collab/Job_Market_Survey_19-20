library(scales)
library(RColorBrewer)
library(cowplot)
library(grid)
#library(patchwork)

#preferred themes----
my_theme <- theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        #plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_horiz <- theme_classic()+ 
  theme(legend.position = "none", axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"),
        #plot.caption = element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5),
        #panel.border = element_blank()
        )

my_theme_leg <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        #plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_leg_left <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left", plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_leg_bottom <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_leg_bottom_horiz <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"),
        legend.position = "bottom", plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_leg_horiz <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"),
        plot.subtitle = element_text(size = 12, hjust = .5))

right_margin <- theme(plot.margin = margin(t = 5.5, r = 15, b = 5.5, l = 5.5, unit = "pt"))

#removing padding between axis and data
#scale_x/y_discrete/continuous(expand = c(0,0))+

#adding proportion/count labels to barchart
prop_lab_low <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = 1)

count_lab_low <- geom_text(stat = "count", aes(label=..count..), vjust = 1)

prop_lab_high <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = -1)

count_lab_high <- geom_text(stat = "count", aes(label=..count..), vjust = -1)

#colorblind palettes
#The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999") #grey, goldenrod, light blue, dark green, gold, navy blue, orange rust, pink

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #black

#simple_gender palette
gender_color <- c("#E69F00", "#009E73", "#F0E442")

peer_color <- c("#56B4E9", "#CC79A7")

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
  
  plot_grid(leg) 
}
